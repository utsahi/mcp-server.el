;;; web-mcp-server.el --- Project aware mcp server -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Kishor Datar
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

(require 'mcp-server)

(defclass web-mcp-server (mcp-server)
  (()))

(defun web-mcp-server-url-retrieve (request arguments cb-response)
  (let* ((url (gethash "url" arguments)))
    (url-retrieve
     url
     (lambda (status)
       (if (plist-get status :error)
           (progn
             (mcp-server-write-tool-call-error-result
              request
              (format "Failed to fetch URL: %s" (plist-get status :error))
              cb-response))

         (mcp-server-write-tool-call-text-result
          request
          (buffer-substring-no-properties (point-min) (point-max))
          cb-response)
         

         
         ;; (goto-char (point-min))
         ;; (when (search-forward-regexp "\r?\n\r?\n" nil t)
         ;;   (let ((body (buffer-substring-no-properties (point) (point-max))))
         ;;     (with-current-buffer (get-buffer-create "*web-mcp-last-render-buffer*")
         ;;       (erase-buffer)
         ;;       (insert body)
         ;;       (shr-render-buffer (current-buffer))
         ;;       (let* ((result (buffer-string)))
         ;;         result))
         ;;     ))
         ))
     nil
     nil
     t)
    ))

(cl-defmethod mcp-server-enumerate-tools ((this web-mcp-server))
  `(
    (:name "web-mcp-server-url-retrieve" :description "Retreves a URL. Returns the RAW response including headers."
           :properties ((:name url :type "string" :required t :description "URL to fetch."))
           :async-lambda web-mcp-server-url-retrieve)
    ))

(provide 'web-mcp-server)
