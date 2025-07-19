;;; filesys-mcp-server.el --- file system operations -*- lexical-binding: t; -*-

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

(defclass filesys-mcp-server (mcp-server)
  (()))

(cl-defmethod mcp-server-enumerate-tools ((this filesys-mcp-server))
  '(
    (:name "read-file" :description "Reads the contents of the file as utf8 text."
	   :properties ((:name file-path :type "string" :required t :description "If relative path, it is calculated relative to project-root.")
			(:name project-root :type "string" :required nil :description "Project root, required if the file path is relative."))
	   :async-lambda (lambda (request arguments cb-response)
			   (let* ((fp-arg (gethash "file-path" arguments))
                                  (path (if (file-exists-p fp-arg) fp-arg
                                          (file-name-concat (gethash "project-root" arguments) (gethash "file-path" arguments)))))
                             (unless (file-exists-p path)
                               (error "path not found %s" path)) 
			     (mcp-server-write-tool-call-text-result
                              request
                              (with-temp-buffer
                                (insert-file-contents-literally path)
                                (decode-coding-region (point-min) (point-max) 'utf-8)
                                (buffer-string))
                              cb-response)))) 
    (:name "directory-files" :description "Reads the contents of the specified directory."
	   :properties ((:name directory-path :type "string" :required t :description "If relative path, it is calculated relative to project-root.")
			(:name project-root :type "string" :required nil :description "Project root, required if the directory path is relative.")
                        (:name match-regexp :type "string" :required nil :description "Regular expression for matching file names. E.g. to find .el and .org files '.el\\\\|.org'"))
	   :async-lambda (lambda (request arguments cb-response)
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
                              cb-response)))) 
    )
  )

(provide 'filesys-mcp-server)
