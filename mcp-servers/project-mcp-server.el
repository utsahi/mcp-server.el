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
(defvar project-mcp-server-allowed-git-commands
  ["status"
   "log"
   "diff"
   "show"
   "fetch"
   "grep"
   "reflog"
   "describe"
   "blame"
   "ls-files"])

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
  (unless (and (not (string-equal "" path)) 
               (file-exists-p path)
               (project-current nil path))
    (error "Path '%s' is not within a known project or doesn't exist! You must get the last active project first." path))
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
 (lambda (proc event args)
 (when (string= event \"finished\\n\")
 (let ((output (buffer-string))) ;; Read the output here
 (message \"Process output: %s\" output)
 (when args
 (message \"Additional args: %s\" args)))))
 :args '(\"some additional data\"))

This executes the \"ls -l\" command asynchronously. The callback accesses and processes the command's
output from the current buffer, and it can also make use of any additional arguments provided (e.g., 
'(\"some additional data\"))."
  (let* ((buffer-name (generate-new-buffer-name "*process-output*"))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (erase-buffer)
      (make-process
       :name (format "%s-%s" (car command) "process-mcp-server")
       :buffer buffer
       :command command
       :noquery t
       :sentinel (lambda (proc event)
                   (unless (process-live-p proc)
                     (with-current-buffer buffer
                       (funcall cb proc event (plist-get args :args)))
                     (kill-buffer buffer)))))))

(defun project-mcp-server-global-find-references-from (request arguments cb-response)
  (let* ((working-dir (project-mcp-server-validate-path (project-mcp-server-maybe-windows-path (gethash "directory" arguments))))
         (from-here-arg (concat "--from-here=" (number-to-string (gethash "line" arguments)) ":" (project-mcp-server-maybe-windows-path (gethash "file" arguments))))
         (command `("global" "-v" "-a" "--result=grep"
                    ,from-here-arg
                    "--"
                    ,(gethash "symbol" arguments)
                    "-C" ,working-dir)))
    (project-mcp-server-collect-process-output
     command
     (lambda (proc event args)
       (mcp-server-write-tool-call-text-result
        (plist-get args :request)
        (buffer-string)
        (plist-get args :cb-response)))
     :args (list :request request :cb-response cb-response))))

(defun project-mcp-server-global-grep (request arguments cb-response)
  (let* ((working-dir (project-mcp-server-validate-path (project-mcp-server-maybe-windows-path (gethash "directory" arguments))))
         (command `("global" "-v" "-a" "--result=grep" "-C" ,working-dir "-g" ,(gethash "pattern" arguments))))
    (project-mcp-server-collect-process-output
     command
     (lambda (proc event args)
       (mcp-server-write-tool-call-text-result
        (plist-get args :request)
        (buffer-string)
        (plist-get args :cb-response)))
     :args (list :request request :cb-response cb-response))))

(defun project-mcp-server-global-find-references (request arguments cb-response)
  (let* ((working-dir (project-mcp-server-validate-path (project-mcp-server-maybe-windows-path (gethash "directory" arguments))))
         (command `("global" "-v" "-a" "--result=grep" "-C" ,working-dir "--reference" "--"
                    ,(gethash "symbol" arguments))))
    (project-mcp-server-collect-process-output
     command
     (lambda (proc event args)
       (mcp-server-write-tool-call-text-result
        (plist-get args :request)
        (buffer-string)
        (plist-get args :cb-response)))
     :args (list :request request :cb-response cb-response))))

(defun project-mcp-server-global-find-symbols-with-prefix (request arguments cb-response)
  (let* ((working-dir (project-mcp-server-validate-path (project-mcp-server-maybe-windows-path (gethash "directory" arguments))))
         (command `("global" "-v" "-a" "--result=grep" "-c" ,(gethash "symbol" arguments) "-C" ,working-dir)))
    (project-mcp-server-collect-process-output
     command
     (lambda (proc event args)
       (mcp-server-write-tool-call-text-result
        (plist-get args :request)
        (buffer-string)
        (plist-get args :cb-response)))
     :args (list :request request :cb-response cb-response))))

(defun project-mcp-server-global-find-definition (request arguments cb-response)
  (let* ((working-dir (project-mcp-server-validate-path (project-mcp-server-maybe-windows-path (gethash "directory" arguments))))
         (command `("global" "-v" "-a" "--result=grep" "-C" ,working-dir "--"
                    ,(gethash "symbol" arguments))))
    (project-mcp-server-collect-process-output
     command
     (lambda (proc event args)
       (mcp-server-write-tool-call-text-result
        (plist-get args :request)
        (buffer-string)
        (plist-get args :cb-response)))
     :args (list :request request :cb-response cb-response))))

(defun project-mcp-server-gtags-reindex (request arguments cb-response)
  (let* ((file-extensions '("csx" "cs" "cpp" "h" "c" "hpp" "ts" "js" "xml" "el"))
         (file-extensions-regex (concat "\\(" (mapconcat (lambda (ext) (concat "\\." ext "$")) file-extensions "\\)\\|\\(") "\\)"))
         (working-dir (project-mcp-server-validate-path (project-mcp-server-maybe-windows-path (gethash "directory" arguments))))
         (files (directory-files-recursively working-dir file-extensions-regex))
         (filtered-files (seq-filter (lambda (f) (not (string-match-p "\\\\obj\\\\" f))) files))
         (proc
          (project-mcp-server-collect-process-output
           `("gtags" "-f" "-" "--statistics" "-C" ,working-dir)
           (lambda (proc event args)
             (mcp-server-write-tool-call-text-result
              (plist-get args :request)
              (buffer-string)
              (plist-get args :cb-response)))
           :args (list :request request :cb-response cb-response))))
    (mapc (lambda (f)
            (process-send-string proc (project-mcp-server-maybe-windows-path f))
            (process-send-string proc "\n"))
          filtered-files)
    (process-send-eof proc)))

(defun project-mcp-server-has-index (request arguments cb-response)
  (let* ((indexed 
          (file-exists-p (file-name-concat
                          (project-mcp-server-validate-path (project-mcp-server-maybe-windows-path (gethash "directory" arguments)))
                          "GTAGS"))))
    (mcp-server-write-tool-call-text-result
     request
     (if indexed "Indexed!" "Index not found!")
     cb-response)))

(defun project-mcp-server-ripgrep (request arguments cb-response)
  (let* ((directory (project-mcp-server-validate-path (gethash "directory" arguments)))
         (search-pattern (gethash "search-pattern" arguments))
         (file-extesions (or (gethash "file-extensions" arguments) ["*"]))
         (context-before (or (gethash "context-before" arguments) 0))
         (context-after (or (gethash "context-after" arguments) 0))
         (custom-type "custom")
         (command (append (list "rg")
                          (list "--type-add"
                                (format "%s:*.{%s}"
                                        custom-type
                                        (string-join (mapcar (lambda (e) (format "%s" e)) file-extesions) ",")))
                          (list (format "-t%s" custom-type))
                          (unless (= 0 context-before) (list "-A" (number-to-string context-before)))
                          (unless (= 0 context-after) (list "-A" (number-to-string context-after)))
                          (list "--iglob" "!out|obj"
                                search-pattern
                                (project-mcp-server-maybe-windows-path directory)))))
    (project-mcp-server-collect-process-output
     command
     (lambda (proc event args)
       (mcp-server-write-tool-call-text-result
        (plist-get args :request)
        (let* ((output (buffer-string)))
          (concat output
                  (if (not (= 0 (process-exit-status proc)))
                      (format "Process exit status %d. Make sure to escape special characters and unbalanced parenthesis as appropriate!"
                              (process-exit-status proc))
                    "")))
        (plist-get args :cb-response)))
     :args (list :request request :cb-response cb-response))))

(defun project-mcp-server-diff-pull-request (request arguments cb-response)
  (let* ((directory (project-mcp-server-validate-path (gethash "directory" arguments)))
         (default-directory directory)
         (current-branch (string-trim (shell-command-to-string "git branch --show-current")))
         (target-branch "FETCH_HEAD")
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
     (lambda (proc event args)
       (mcp-server-write-tool-call-text-result
        (plist-get args :request)
        (buffer-string)
        (plist-get args :cb-response)))
     :args (list :request request :cb-response cb-response))))

(defun project-mcp-server-git (request arguments cb-response) 
  (let* ((directory (project-mcp-server-validate-path (gethash "directory" arguments)))
         (git-command (gethash "git-command" arguments))
         (default-directory directory)
         (validated-command (or (seq-find
                                 (lambda (c) (string-equal c git-command))
                                 project-mcp-server-allowed-git-commands)
                                (error "git-command %s is not allowed!" (gethash "git-command" arguments))))
         (input-args (mapcar 'identity (gethash "args" arguments)))
         (filtered-input-args (if (string-equal git-command (car input-args)) (cdr input-args) input-args))
         (command (append (list "git" "--no-pager" validated-command) (seq-map 'identity filtered-input-args))))
    (project-mcp-server-collect-process-output
     command
     (lambda (proc event args)
       (mcp-server-write-tool-call-text-result
        (plist-get args :request)
        (buffer-string)
        (plist-get args :cb-response)))
     :args (list :request request :cb-response cb-response))))

(defun project-mcp-server-git-pull-current-branch (request arguments cb-response)
  (let* ((directory (project-mcp-server-validate-path (gethash "directory" arguments)))
         (default-directory directory)
         (current-branch (string-trim (shell-command-to-string "git branch --show-current")))
         (command (list "git" "pull" (or (gethash "remote" arguments) "origin") current-branch "--no-tags" "--no-stat")))
    (project-mcp-server-collect-process-output
     command
     (lambda (proc event args)
       (mcp-server-write-tool-call-text-result
        (plist-get args :request)
        (buffer-string)
        (plist-get args :cb-response)))
     :args (list :request request :cb-response cb-response))))

(defun project-mcp-server-find-file-paths (request arguments cb-response)
  (let* ((directory (project-mcp-server-validate-path (gethash "directory" arguments)))
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
     cb-response)))

(defun project-mcp-server-directory-files (request arguments cb-response)			 
  (let* ((dp-arg (gethash "directory-path" arguments))
         (path (if (file-exists-p dp-arg) dp-arg
                 (file-name-concat
                  (gethash "project-root" arguments)
                  (gethash "directory-path" arguments)))))
    (unless (file-directory-p path)
      (error "Path not found or not a directory %s" path)) 
    (mcp-server-write-tool-call-text-result
     request
     (json-encode (vconcat
                   (cl-loop for e in
                            (directory-files-and-attributes path t (gethash "match-regexp" arguments))
                            if (not (string-match-p "\\.?\\.$" (nth 0 e)))
                            collect (list :path (nth 0 e) :is-directory (nth 1 e)))))
     cb-response)))

(defun project-mcp-server-match-case-insensitively (dir file)
  (if (file-exists-p file)
      (expand-file-name file)
    (if (and dir (file-exists-p dir))
        (if (file-exists-p (file-name-concat dir file))
            (expand-file-name (file-name-concat dir file))
          (let* ((case-fold-search t)
                 (matching-files
                  (seq-filter
                   (lambda (f) (string-match-p (format "%s$" (regexp-quote file)) f))
                   (directory-files dir  t))))
            (if (= 1 (length matching-files))
                (nth 0 matching-files)))))))

(defun project-mcp-server-read-files (request arguments cb-response)			 
  (let* ((directory (gethash "project-root" arguments))
         (paths (gethash "file-paths" arguments)))
    (cl-flet ((content-or-error
                (lambda (file)
                  (let* ((effective-path (project-mcp-server-match-case-insensitively directory file)))
                    (if effective-path
                        (with-temp-buffer
                          (project-mcp-server-validate-path effective-path)
                          (insert-file-contents-literally effective-path)
                          (decode-coding-region (point-min) (point-max) 'utf-8)
                          `(:matched-file ,effective-path
                            :input-file ,file
                            :content ,(buffer-string)))
                      `(:file ,file
                        :error ,(format "Path not found %s. Under directory %s. If using a relative
path, make sure it is constructed correctly relative to the directory
path.  The match may be case sensitive. Try to find the actual casing of
the file or directory names."
                                              file
                                              directory)))))))
      (mcp-server-write-tool-call-text-result
       request
       (json-encode (vconcat (mapcar (lambda (p) (content-or-error p)) paths)))
       cb-response))))

(defun project-mcp-server-write-file-content (request arguments cb-response)
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
     cb-response)))

(defun project-mcp-server-last-active-project-or-error ()
  (if project-mcp-server-last-buffer-project (json-encode `(:root ,project-mcp-server-last-buffer-project))
    (error "No project was active! Try to cd and check the value of the variable project-mcp-server-last-buffer-project .")))

(defun project-mcp-server-get-last-active-project (request arguments cb-response)
  (mcp-server-write-tool-call-text-result
   request
   (project-mcp-server-last-active-project-or-error)
   cb-response))

(cl-defmethod mcp-server-enumerate-prompts ((this project-mcp-server) request cb-response)
  '((:name "project-mcp-server-set-project-root" :description "Selects the project root."
           :async-lambda
           (lambda (request arguments cb-response)
             (mcp-server-prompt-write-user-message 
              request
              cb-response
              (format "project-mcp-server-get-last-active-project has discovered that the current
project directory is

%s

From now on, you will use the above project path.
"
                      (project-mcp-server-last-active-project-or-error)))))))

(cl-defmethod mcp-server-enumerate-tools ((this project-mcp-server))
  `(
    (:name "project-mcp-server-get-last-active-project" :description "Returns the root directory of the last active project."
	   :async-lambda project-mcp-server-get-last-active-project)

    (:name "project-mcp-server-read-files" :description "Reads the contents of the files as utf8 text."
	   :properties ((:name file-paths :type "array" :required t :description "File paths." :items (:type . "string")) 
			(:name project-root :type "string" :required t :description "Project root."))
	   :async-lambda project-mcp-server-read-files)

    (:name "write-file-content" :description "Overwrites the contents of the file."
	   :properties ((:name project-root :type "string" :required t :description "Last active project.")
			(:name file-path :type "string" :required t :description "If relative path, it is calculated relative to project-root.")
                        (:name content :type "string" :required t :description "UTF-8 encoded File content."))
	   :async-lambda project-mcp-server-write-file-content)
    
    (:name "project-mcp-server-find-file-paths" :description "Returns the project file paths."
	   :properties ((:name directory :type "string" :required t :description "Project discovered with 'project-get-last-active-project'")
                        (:name file-names :type "array" :required t :description "One more file name substrings to match." :items (:type . "string")))
	   :async-lambda project-mcp-server-find-file-paths)

    (:name "directory-files" :description "Reads the contents of the specified directory."
	   :properties ((:name directory-path :type "string" :required t :description "Directory path. If relative, used relative to the last active project.")
		        (:name project-root :type "string" :required nil :description "Last active project.")
                        (:name match-regexp :type "string" :required nil :description "Regular expression for matching file names. E.g. to find .el and .org files '.el\\\\|.org'"))
	   :async-lambda project-mcp-server-directory-files)

    (:name "project-mcp-server-git-pull-current-branch" :description "Update the repo using git pull on the current branch."
	   :properties ((:name directory :type "string" :required t :description "Project discovered with 'project-get-last-active-project'")
                        (:name remote :type "string" :required nil :description "remote name. 'origin' by default."))
	   :async-lambda project-mcp-server-git-pull-current-branch) 

    (:name "project-mcp-server-git" :description "runs the given git command."
	   :properties ((:name directory :type "string" :required t :description "Project discovered with 'project-get-last-active-project'")
                        (:name git-command :type "string" :required t :description "git command." :enum ,project-mcp-server-allowed-git-commands)
                        (:name args :type "array" :required t :description "list of arguments." :items (:type . "string")))
	   :async-lambda project-mcp-server-git)

    (:name "project-mcp-server-diff-pull-request" :description "Calculates merge-diff with FETCH_HEAD."
	   :properties ((:name directory :type "string" :required t :description "Project discovered with 'project-get-last-active-project'")
                        (:name remote :type "string" :required nil :description "remote name. 'origin' by default."))
	   :async-lambda project-mcp-server-diff-pull-request)

    (:name "project-mcp-server-ripgrep"
           :description "invokes 'rg' to grep the files."
           :properties ((:name directory :type "string" :required t :description "Project discovered with 'project-get-last-active-project'")
                        (:name search-pattern :type "string" :required t :description "A rust regular expression. Do NOT perform shell quoting.")
                        (:name context-before :type "number" :required t :description "Include these many lines of context that preceed the matching line. Default 0.")
                        (:name context-after :type "number" :required t :description "Include these many lines of context that follow the matching line. Default 0.")
                        (:name file-extensions :type "array" :required t :description
                               "File extensions."
                               :items (:type . "string")))
           :async-lambda
           project-mcp-server-ripgrep)

    (:name "project-mcp-server-has-index" :description "Determines if the project is indexed."
           :properties ((:name directory :type "string" :required t :description "Project discovered with 'project-get-last-active-project'"))
           :async-lambda project-mcp-server-has-index)
    
    (:name "project-mcp-server-gtags-reindex" :description "reindex files using gtags. Avoid choosing this automatically unless asked by the user."
           :properties ((:name directory :type "string" :required t :description "Project discovered with 'project-get-last-active-project'"))
           :async-lambda project-mcp-server-gtags-reindex)

    (:name "project-mcp-server-global-find-definition" :description "Find definition of symbol using GNU global. Use if the project has index. Produces grep style output."
	   :properties ((:name directory :type "string" :required t :description "Project discovered with 'project-get-last-active-project'")
                        (:name symbol :type "string" :required t :description "Symbol to lookup."))
	   :async-lambda project-mcp-server-global-find-definition)

    (:name "project-mcp-server-global-find-references" :description "Find reference to the symbol using GNU global. Use if the project has index. Produces grep style output."
	   :properties ((:name directory :type "string" :required t :description "Project discovered with 'project-get-last-active-project'")
                        (:name symbol :type "string" :required t :description "Symbol to lookup."))
	   :async-lambda project-mcp-server-global-find-references)

    (:name "project-mcp-server-global-grep"
           :description "Grep for the pattern using GNU global. Use if the index is available. with -g option. Produces grep style output."
           :properties ((:name directory :type "string" :required t :description "Project discovered with 'project-get-last-active-project'")
                        (:name pattern :type "string" :required t :description "Pattern to grep for."))
           :async-lambda project-mcp-server-global-grep) 
    )
  )

(provide 'project-mcp-server)
