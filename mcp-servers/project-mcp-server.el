;;; project-mcp-server.el --- An in memory cache -*- lexical-binding: t; -*-

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
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(require 'eieio)
(require 'mcp-server)
(require 'filesys-mcp-server)

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

(cl-defmethod mcp-server-enumerate-tools ((this project-mcp-server))
  `(
    (:name "project-get-last-active-project" :description "Returns the root of the last active project if any for the current context. When calling other project related tools, if the context doesn't already contain a reference to the working directory, this tool should be used to determine the working directory."
	   :properties ()
	   :async-lambda (lambda (request arguments cb-response)
                           (mcp-server-write-tool-call-text-result
                            request
                            (if project-mcp-server-last-buffer-project (json-encode `(:root ,project-mcp-server-last-buffer-project)) (error "No project was active!"))
                            cb-response)))

    (:name "project-ripgrep"
           :description "invokes 'rg' to grep the files. Does not invoke in a shell but instead invokes the process directly."
           :properties ((:name directory :type "string" :required t :description "Root search directory. Must specify full path.")
                        (:name search-pattern :type "string" :required t :description "ripgrep regex search pattern passed to the rg executable.")
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
               (filesys-mcp-server-collect-process-output
                command
                (lambda (event args)
                  (mcp-server-write-tool-call-text-result
                   (plist-get args :request)
                   (buffer-string)
                   (plist-get args :cb-response)))
                :args (list :request request :cb-response cb-response)))))

    (:name "project-gtags-reindex" :description "reindex files using gtags"
 :properties ((:name directory :type "string" :required t :description "Root directory to index. Must specify full path."))
 :async-lambda (lambda (request arguments cb-response)
                 (let* ((file-extensions '("csx" "cs" "cpp" "h" "c" "hpp" "ts" "js" "xml" "el"))
                        (file-extensions-regex (concat "\\(" (mapconcat (lambda (ext) (concat "\\." ext "$")) file-extensions "\\)\\|\\(") "\\)"))
                        (working-dir (project-mcp-server-validate-path (project-mcp-server-maybe-windows-path (gethash "directory" arguments))))
                        (files (directory-files-recursively working-dir file-extensions-regex))
                        (filtered-files (seq-filter (lambda (f) (not (string-match-p "\\\\obj\\\\" f))) files))
                        (proc
                         (filesys-mcp-server-collect-process-output
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

    (:name "project-global-find-definition" :description "Find definition of symbol using GNU global. Produces grep style output."
	   :properties ((:name directory :type "string" :required t :description "Root directory to index. Must specify full path.")
                        (:name symbol :type "string" :required t :description "Symbol to lookup."))
	   :async-lambda (lambda (request arguments cb-response)
                           (let* ((working-dir (project-mcp-server-validate-path (project-mcp-server-maybe-windows-path (gethash "directory" arguments))))
                                  (command `("global" "-v" "-a" "--result=grep" "-C" ,working-dir "--"
                                             ,(gethash "symbol" arguments))))
                             (filesys-mcp-server-collect-process-output
                              command
                              (lambda (event args)
                                (mcp-server-write-tool-call-text-result
                                 (plist-get args :request)
                                 (buffer-string)
                                 (plist-get args :cb-response)))
                              :args (list :request request :cb-response cb-response)))))

    (:name "project-global-find-symbols-with-prefix" :description "Find symbols that start with the prefix using GNU global. Produces grep style output."
	   :properties ((:name directory :type "string" :required t :description "Root directory to index. Must specify full path.")
                        (:name symbol :type "string" :required t :description "Symbol to lookup."))
	   :async-lambda (lambda (request arguments cb-response)
                           (let* ((working-dir (project-mcp-server-validate-path (project-mcp-server-maybe-windows-path (gethash "directory" arguments))))
                                  (command `("global" "-v" "-a" "--result=grep" "-c" ,(gethash "symbol" arguments) "-C" ,working-dir)))
                             (filesys-mcp-server-collect-process-output
                              command
                              (lambda (event args)
                                (mcp-server-write-tool-call-text-result
                                 (plist-get args :request)
                                 (buffer-string)
                                 (plist-get args :cb-response)))
                              :args (list :request request :cb-response cb-response)))))

    (:name "project-global-find-references" :description "Find reference to the symbol using GNU global. Produces grep style output."
	   :properties ((:name directory :type "string" :required t :description "Root directory to index. Must specify full path.")
                        (:name symbol :type "string" :required t :description "Symbol to lookup."))
	   :async-lambda (lambda (request arguments cb-response)
                           (let* ((working-dir (project-mcp-server-validate-path (project-mcp-server-maybe-windows-path (gethash "directory" arguments))))
                                  (command `("global" "-v" "-a" "--result=grep" "-C" ,working-dir "--reference" "--"
                                             ,(gethash "symbol" arguments))))
                             (filesys-mcp-server-collect-process-output
                              command
                              (lambda (event args)
                                (mcp-server-write-tool-call-text-result
                                 (plist-get args :request)
                                 (buffer-string)
                                 (plist-get args :cb-response)))
                              :args (list :request request :cb-response cb-response)))))

    (:name "project-global-grep"
           :description "Grep for the pattern using GNU global with -g option. Produces grep style output."
           :properties ((:name directory :type "string" :required t :description "Root directory to index. Must specify full path.")
                        (:name pattern :type "string" :required t :description "Pattern to grep for."))
           :async-lambda (lambda (request arguments cb-response)
                           (let* ((working-dir (project-mcp-server-validate-path (project-mcp-server-maybe-windows-path (gethash "directory" arguments))))
                                  (command `("global" "-v" "-a" "--result=grep" "-C" ,working-dir "-g" ,(gethash "pattern" arguments))))
                             (filesys-mcp-server-collect-process-output
                              command
                              (lambda (event args)
                                (mcp-server-write-tool-call-text-result
                                 (plist-get args :request)
                                 (buffer-string)
                                 (plist-get args :cb-response)))
                              :args (list :request request :cb-response cb-response)))))

    (:name "project-global-find-references-from" :description "Find reference to the symbol from the given line in the reference file using GNU global. Produces grep style output.
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
                             (filesys-mcp-server-collect-process-output
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
