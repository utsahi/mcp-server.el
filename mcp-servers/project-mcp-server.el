;;; project-mcp-server.el --- Project aware mcp server -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Kishor Datar
;; Author: Kishor Datar <kishordatar at gmail>
;; Version: 0.1
;; Package-Requires: ((emacs "30.0"))
;; Keywords: mcp, server, llm ;

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

(require 'eieio)
(require 'mcp-server)
(require 'filesys-mcp-server)
(require 'project)

(defvar project-mcp-server-last-buffer-project nil)
(defvar project-mcp-server-set-window-project-idle-timer-duration 2)
(defvar project-mcp-server-set-window-project-timer nil)
(defvar project-mcp-server-ripgrep-default-types "csharp,lisp,cpp,config"
  "Default comma-separated list of file types for ripgrep searches.")

(defun project-mcp-server-set-window-project-timer-fn ()
  (setq project-mcp-server-last-buffer-project
        (let* ((pr (project-current)))
          (when pr (expand-file-name (project-root pr))))))

(setq project-mcp-server-set-window-project-timer
      (run-with-idle-timer
       project-mcp-server-set-window-project-idle-timer-duration
       t
       'project-mcp-server-set-window-project-timer-fn))

(defclass project-mcp-server (mcp-server)
  (()))

(defun project-mcp-server-validate-path (path)
  (unless (file-exists-p path)
    (error "Don't know about path: %s" path))
  path)

(defun project-mcp-server-maybe-windows-path (path)
  (if (eq system-type 'windows-nt)
      (string-replace "/" "\\" path)
    path))

(defun project-mcp-server-collect-process-output (command cb &rest args)
  "Execute a shell COMMAND asynchronously and collect its output into a temporary buffer.

COMMAND is a list of strings representing the shell command and its arguments.

CB is a callback function that gets invoked upon process completion. It receives the process EVENT
and any additional arguments provided in ARGS.

ARGS is a plist of additional arguments to pass to the callback function. Use the key `:args` in 
ARGS to pass specific additional data to the callback function.

CB can use `current-buffer` to access the command's output if needed.

Example usage:
(project-mcp-server-collect-process-output
 (list \"ls\" \"-l\")
 (lambda (event args)
 (when (string= event \"finished\n\")
 (let ((output (buffer-string))) ;; Read the output here
 (message \"Process output: %s\" output)
 (when args
 (message \"Additional args: %s\" args)))))
 :args '(\"some additional data\"))

This executes the \"ls -l\" command asynchronously. The callback accesses and processes the command's
output from the current buffer, and it can also make use of any additional arguments provided (e.g., 
'(\"some additional data\'))."
  (let* ((buffer-name (generate-new-buffer-name "*process-output*"))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (erase-buffer)
      (make-process
       :name "process-collector"
       :buffer buffer
       :command command
       :noquery t
       :sentinel (lambda (proc event)
                   (unless (process-live-p proc)
                     (with-current-buffer buffer
                       (funcall cb event (plist-get args :args)))
                     (kill-buffer buffer)))))))

(cl-defmethod mcp-server-enumerate-tools ((this project-mcp-server))
  `(
    (:name "project-get-last-active-project" :description "Returns the root of the last active project if any for the current context. When calling other project related tools, if the context doesn't already contain a reference to the working directory, this tool should be used to determine the working directory."
	   :properties ((:name directory :type "string" :required t :description "Project root directory. Must specify full path."))
	   :async-lambda (lambda (request arguments cb-response)
                           (mcp-server-write-tool-call-text-result
                            request
                            (if project-mcp-server-last-buffer-project (json-encode `(:root ,project-mcp-server-last-buffer-project)) (error "No project was active!"))
                            cb-response)))

    (:name "write-file-content" :description "Overwrites the contents of the file."
	   :properties ((:name project-root :type "string" :required t :description "Project root.")
			(:name file-path :type "string" :required t :description "If relative path, it is calculated relative to project-root.")
                        (:name content :type "string" :required t :description "UTF-8 encoded File content."))
	   :async-lambda (lambda (request arguments cb-response)
			   (let* ((fp-arg (gethash "file-path" arguments))
                                  (path (if (file-exists-p fp-arg) fp-arg
                                          (file-name-concat (gethash "project-root" arguments) (gethash "file-path" arguments))))) 
                             (with-temp-buffer
                               (set-buffer-file-coding-system 'utf-8)
                               (insert (gethash "content" arguments))
                               (write-file path))
			     (mcp-server-write-tool-call-text-result
                              request
                              "Success"
                              cb-response))))
    
    (:name "project-find-files" :description "Returns the project files."
	   :properties ((:name directory :type "string" :required t :description "Project root directory. Must specify full path.")
                        (:name file-names :type "array" :required t :description "One more file name substrings." :items (:type . "string")))
	   :async-lambda (lambda (request arguments cb-response)
                           (let* ((directory (gethash "directory" arguments))
                                  (file-names-arg (gethash "file-names" arguments))
                                  (file-patterns (seq-map 'identity file-names-arg))
                                  (default-directory directory)
                                  (all-files (project-files (project-current)))
                                  (matching-files (seq-filter 
                                                   (lambda (file)
                                                     (seq-some (lambda (pattern)
                                                                 (string-match-p (regexp-quote pattern) (file-name-nondirectory file)))
                                                               file-patterns))
                                                   all-files)))
                             (mcp-server-write-tool-call-text-result
                              request
                              (json-encode `(:files ,matching-files))
                              cb-response))))

    (:name "project-git-pull-current-branch" :description "Update the repo using git pull on the current branch."
	   :properties ((:name directory :type "string" :required t :description "Root directory of the project. Must specify full path.")
                        (:name remote :type "string" :required nil :description "remote name. 'origin' by default."))
	   :async-lambda (lambda (request arguments cb-response)
                           (let* ((directory (project-mcp-server-validate-path (gethash "directory" arguments)))
                                  (default-directory directory)
                                  (current-branch (string-trim (shell-command-to-string "git branch --show-current")))
                                  (command (list "git" "pull" (or (gethash "remote" arguments) "origin") current-branch "--no-tags" "--no-stat")))
                             (project-mcp-server-collect-process-output
                              command
                              (lambda (event args)
                                (mcp-server-write-tool-call-text-result
                                 (plist-get args :request)
                                 (buffer-string)
                                 (plist-get args :cb-response)))
                              :args (list :request request :cb-response cb-response)))))

    (:name "project-git-fetch" :description "runs git fetch."
	   :properties ((:name directory :type "string" :required t :description "Root directory of the project. Must specify full path.")
                        (:name branch :type "string" :required t :description "branch name")
                        (:name remote :type "string" :required nil :description "remote name. 'origin' by default."))
	   :async-lambda (lambda (request arguments cb-response)
                           (let* ((directory (project-mcp-server-validate-path (gethash "directory" arguments)))
                                  (default-directory directory)
                                  (command (list "git" "fetch" (or (gethash "remote" arguments) "origin") (gethash "branch" arguments))))
                             (project-mcp-server-collect-process-output
                              command
                              (lambda (event args)
                                (mcp-server-write-tool-call-text-result
                                 (plist-get args :request)
                                 (buffer-string)
                                 (plist-get args :cb-response)))
                              :args (list :request request :cb-response cb-response)))))

    (:name "project-git-log" :description "runs 'git --no-pager log [arguments]'."
	   :properties ((:name directory :type "string" :required t :description "Root directory of the project. Must specify full path.")
                        (:name args :type "array" :required t :description "list of arguments." :items (:type . "string")))
	   :async-lambda (lambda (request arguments cb-response) 
                           (let* ((directory (project-mcp-server-validate-path (gethash "directory" arguments)))
                                  (input-args (gethash "args" arguments))
                                  (default-directory directory)
                                  (command (append (list "git" "--no-pager" "log") (seq-map 'identity input-args))))
                             (project-mcp-server-collect-process-output
                              command
                              (lambda (event args)
                                (mcp-server-write-tool-call-text-result
                                 (plist-get args :request)
                                 (buffer-string)
                                 (plist-get args :cb-response)))
                              :args (list :request request :cb-response cb-response)))))
    
    (:name "project-git-diff" :description "runs 'git --no-pager diff [arguments]'."
	   :properties ((:name directory :type "string" :required t :description "Root directory of the project. Must specify full path.")
                        (:name args :type "array" :required t :description "list of arguments." :items (:type . "string")))
	   :async-lambda (lambda (request arguments cb-response) 
                           (let* ((directory (project-mcp-server-validate-path (gethash "directory" arguments)))
                                  (input-args (gethash "args" arguments))
                                  (default-directory directory)
                                  (command (append (list "git" "--no-pager" "diff") (seq-map 'identity input-args))))
                             (project-mcp-server-collect-process-output
                              command
                              (lambda (event args)
                                (mcp-server-write-tool-call-text-result
                                 (plist-get args :request)
                                 (buffer-string)
                                 (plist-get args :cb-response)))
                              :args (list :request request :cb-response cb-response)))))

    (:name "project-diff-pull-request" :description "Calculates merge-diff with the current branch."
	   :properties ((:name directory :type "string" :required t :description "Root directory of the project. Must specify full path.")
                        (:name remote :type "string" :required nil :description "remote name. 'origin' by default.")
                        (:name branch :type "string" :required nil :description "'FETCH_HEAD' by default."))
	   :async-lambda (lambda (request arguments cb-response)
                           (let* ((directory (project-mcp-server-validate-path (gethash "directory" arguments)))
                                  (default-directory directory)
                                  (current-branch (string-trim (shell-command-to-string "git branch --show-current")))
                                  (target-branch (or (gethash "branch" arguments) "FETCH_HEAD"))
                                  (merge-base (string-trim (shell-command-to-string
                                                            (format "git merge-base %s %s" 
                                                                    (shell-quote-argument current-branch)
                                                                    (shell-quote-argument target-branch)))))
                                  (command (list "git"
                                                 "merge-tree"
                                                 merge-base
                                                 current-branch
                                                 target-branch)))
                             (project-mcp-server-collect-process-output
                              command
                              (lambda (event args)
                                (mcp-server-write-tool-call-text-result
                                 (plist-get args :request)
                                 (buffer-string)
                                 (plist-get args :cb-response)))
                              :args (list :request request :cb-response cb-response)))))

    (:name "project-ripgrep"
           :description "invokes 'rg' to grep the files. Do NOT shell-escape the arguments."
           :properties ((:name directory :type "string" :required t :description "Root search directory. Must specify full path.")
                        (:name search-pattern :type "string" :required t :description "Escaped search regex.")
                        (:name types :type "string" :required nil :description ,(format "Optional comma-separated list of types (default: %s)" project-mcp-server-ripgrep-default-types)))
           :async-lambda
           (lambda (request arguments cb-response)
             (let* ((directory (project-mcp-server-validate-path (gethash "directory" arguments)))
                    (search-pattern (gethash "search-pattern" arguments))
                    (types-str (or (gethash "types" arguments) project-mcp-server-ripgrep-default-types))
                    (types (split-string types-str "," t "[ \t\n\r]+"))
                    (type-args (apply #'append (mapcar (lambda (type) (list "--type" type)) types)))
                    (command (append (list "rg")
                                     type-args
                                     (list "--iglob" "!out|obj"
                                           search-pattern
                                           (project-mcp-server-maybe-windows-path directory)))))
               (project-mcp-server-collect-process-output
                command
                (lambda (event args)
                  (mcp-server-write-tool-call-text-result
                   (plist-get args :request)
                   (buffer-string)
                   (plist-get args :cb-response)))
                :args (list :request request :cb-response cb-response)))))

    (:name "project-has-index" :description "Determines if the project is indexed."
           :properties ((:name directory :type "string" :required t :description "Root directory to index. Must specify full path."))
           :async-lambda (lambda (request arguments cb-response)
                           (let* ((indexed 
                                   (file-exists-p (file-name-concat
                                                   (project-mcp-server-validate-path (project-mcp-server-maybe-windows-path (gethash "directory" arguments)))
                                                   "GTAGS"))))
                             (mcp-server-write-tool-call-text-result
                              request
                              (if indexed "Indexed!" "Index not found!")
                              cb-response))))
    
    (:name "project-gtags-reindex" :description "reindex files using gtags. EXPENSIVE. call only if requested EXPLICITLY!"
           :properties ((:name directory :type "string" :required t :description "Root directory to index. Must specify full path."))
           :async-lambda (lambda (request arguments cb-response)
                           (let* ((file-extensions '("csx" "cs" "cpp" "h" "c" "hpp" "ts" "js" "xml" "el"))
                                  (file-extensions-regex (concat "\\(" (mapconcat (lambda (ext) (concat "\\." ext "$")) file-extensions "\\)\\|\\(") "\\)"))
                                  (working-dir (project-mcp-server-validate-path (project-mcp-server-maybe-windows-path (gethash "directory" arguments))))
                                  (files (directory-files-recursively working-dir file-extensions-regex))
                                  (filtered-files (seq-filter (lambda (f) (not (string-match-p "\\\\obj\\\\" f))) files))
                                  (proc
                                   (project-mcp-server-collect-process-output
                                    `("gtags" "-f" "-" "--statistics" "-C" ,working-dir)
                                    (lambda (event args)
                                      (mcp-server-write-tool-call-text-result
                                       (plist-get args :request)
                                       (buffer-string)
                                       (plist-get args :cb-response)))
                                    :args (list :request request :cb-response cb-response))))
                             (mapc (lambda (f)
                                     (process-send-string proc (project-mcp-server-maybe-windows-path f))
                                     (process-send-string proc "\n"))
                                   filtered-files)
                             (process-send-eof proc))))

    (:name "project-global-find-definition" :description "Find definition of symbol using GNU global. Use if the project has index. Produces grep style output."
	   :properties ((:name directory :type "string" :required t :description "Root directory to index. Must specify full path.")
                        (:name symbol :type "string" :required t :description "Symbol to lookup."))
	   :async-lambda (lambda (request arguments cb-response)
                           (let* ((working-dir (project-mcp-server-validate-path (project-mcp-server-maybe-windows-path (gethash "directory" arguments))))
                                  (command `("global" "-v" "-a" "--result=grep" "-C" ,working-dir "--"
                                             ,(gethash "symbol" arguments))))
                             (project-mcp-server-collect-process-output
                              command
                              (lambda (event args)
                                (mcp-server-write-tool-call-text-result
                                 (plist-get args :request)
                                 (buffer-string)
                                 (plist-get args :cb-response)))
                              :args (list :request request :cb-response cb-response)))))

    (:name "project-global-find-symbols-with-prefix" :description "Find symbols that start with the prefix using GNU global. Use if the project has index. Produces grep style output."
	   :properties ((:name directory :type "string" :required t :description "Root directory to index. Must specify full path.")
                        (:name symbol :type "string" :required t :description "Symbol to lookup."))
	   :async-lambda (lambda (request arguments cb-response)
                           (let* ((working-dir (project-mcp-server-validate-path (project-mcp-server-maybe-windows-path (gethash "directory" arguments))))
                                  (command `("global" "-v" "-a" "--result=grep" "-c" ,(gethash "symbol" arguments) "-C" ,working-dir)))
                             (project-mcp-server-collect-process-output
                              command
                              (lambda (event args)
                                (mcp-server-write-tool-call-text-result
                                 (plist-get args :request)
                                 (buffer-string)
                                 (plist-get args :cb-response)))
                              :args (list :request request :cb-response cb-response)))))

    (:name "project-global-find-references" :description "Find reference to the symbol using GNU global. Use if the project has index. Produces grep style output."
	   :properties ((:name directory :type "string" :required t :description "Root directory to index. Must specify full path.")
                        (:name symbol :type "string" :required t :description "Symbol to lookup."))
	   :async-lambda (lambda (request arguments cb-response)
                           (let* ((working-dir (project-mcp-server-validate-path (project-mcp-server-maybe-windows-path (gethash "directory" arguments))))
                                  (command `("global" "-v" "-a" "--result=grep" "-C" ,working-dir "--reference" "--"
                                             ,(gethash "symbol" arguments))))
                             (project-mcp-server-collect-process-output
                              command
                              (lambda (event args)
                                (mcp-server-write-tool-call-text-result
                                 (plist-get args :request)
                                 (buffer-string)
                                 (plist-get args :cb-response)))
                              :args (list :request request :cb-response cb-response)))))

    (:name "project-global-grep"
           :description "Grep for the pattern using GNU global. Use if the index is available. with -g option. Produces grep style output."
           :properties ((:name directory :type "string" :required t :description "Root directory to index. Must specify full path.")
                        (:name pattern :type "string" :required t :description "Pattern to grep for."))
           :async-lambda (lambda (request arguments cb-response)
                           (let* ((working-dir (project-mcp-server-validate-path (project-mcp-server-maybe-windows-path (gethash "directory" arguments))))
                                  (command `("global" "-v" "-a" "--result=grep" "-C" ,working-dir "-g" ,(gethash "pattern" arguments))))
                             (project-mcp-server-collect-process-output
                              command
                              (lambda (event args)
                                (mcp-server-write-tool-call-text-result
                                 (plist-get args :request)
                                 (buffer-string)
                                 (plist-get args :cb-response)))
                              :args (list :request request :cb-response cb-response)))))

    (:name "project-global-find-references-from" :description "Find reference to the symbol from the given line in the reference file using GNU global. Use if the project has index. Produces grep style output.
E.g. if token foo appears on line 10 in file.txt, this method can find all references to foo with that line and file as the starting point."
	   :properties ((:name directory :type "string" :required t :description "Root directory to index. Must specify full path.")
                        (:name symbol :type "string" :required t :description "Symbol to lookup.")
                        (:name file :type "string" :required t :description "File name.")
                        (:name line :type number :required t :description "Line number."))
	   :async-lambda (lambda (request arguments cb-response)
                           (let* ((working-dir (project-mcp-server-validate-path (project-mcp-server-maybe-windows-path (gethash "directory" arguments))))
                                  (from-here-arg (concat "--from-here=" (number-to-string (gethash "line" arguments)) ":" (project-mcp-server-maybe-windows-path (gethash "file" arguments))))
                                  (command `("global" "-v" "-a" "--result=grep"
                                             ,from-here-arg
                                             "--"
                                             ,(gethash "symbol" arguments)
                                             "-C" ,working-dir)))
                             (project-mcp-server-collect-process-output
                              command
                              (lambda (event args)
                                (mcp-server-write-tool-call-text-result
                                 (plist-get args :request)
                                 (buffer-string)
                                 (plist-get args :cb-response)))
                              :args (list :request request :cb-response cb-response))))) 
    )
  )

(provide 'project-mcp-server)
