;;; gnu-global-mcp-server.el --- GNU Global/Gtags MCP Server -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Kishor Datar
;; Author: Kishor Datar <kishordatar at gmail>
;; Version: 0.1
;; Package-Requires: ((emacs "30.0"))
;; Keywords: mcp, server, llm, gtags, global ;

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

;;; Code:

(require 'mcp-server)

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

;; MCP Server class for GNU Global

(defclass gnu-global-mcp-server (mcp-server)
  (()))

(cl-defmethod mcp-server-enumerate-tools ((this gnu-global-mcp-server))
  '(
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

    (:name "project-mcp-server-global-find-symbols-with-prefix"
           :description "Find symbols with prefix using GNU global. Use if the project has index. Produces grep style output."
           :properties ((:name directory :type "string" :required t :description "Project discovered with 'project-get-last-active-project'")
                        (:name symbol :type "string" :required t :description "Symbol prefix to lookup."))
           :async-lambda project-mcp-server-global-find-symbols-with-prefix)

    (:name "project-mcp-server-global-find-references-from"
           :description "Find references from a specific location using GNU global. Use if the project has index. Produces grep style output."
           :properties ((:name directory :type "string" :required t :description "Project discovered with 'project-get-last-active-project'")
                        (:name symbol :type "string" :required t :description "Symbol to lookup.")
                        (:name file :type "string" :required t :description "File path.")
                        (:name line :type "number" :required t :description "Line number."))
           :async-lambda project-mcp-server-global-find-references-from)
    ))

(provide 'gnu-global-mcp-server)

;;; gnu-global-mcp-server.el ends here
