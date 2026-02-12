;;; mcp-server-test-util.el --- Utility methods for debugging MCP server implementations.  -*- lexical-binding: t; -*-

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
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(defun mcp-server-debug-tool (fn fn-args)
  (let* ((args-ht (make-hash-table :test 'equal))
         (req (make-hash-table :test 'equal))
         (fn-args-copy (copy-sequence fn-args))
         )
    (puthash "id" 1 req)
    (while fn-args-copy      
      (puthash (symbol-name (car fn-args-copy)) (cadr fn-args-copy) args-ht)
      (setq fn-args-copy (cddr fn-args-copy)))
    (apply fn (list req args-ht (lambda (ar) (message "Call completed"))))))

; (mcp-server-debug-tool 'web-mcp-server-render-web-page '(url "https://www.google.com/"))


