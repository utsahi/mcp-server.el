;;; emacs-mcp-server.el --- Emacs mcp server -*- lexical-binding: t; -*-

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

(defclass emacs-mcp-server (mcp-server)
  (()))

(defun emacs-mcp-server-function-documentation (request arguments cb-response)
  (let* ((name (intern (gethash "name" arguments))))
    (mcp-server-write-tool-call-text-result
          request
          (documentation name t)
          cb-response)))

(defun emacs-mcp-server-variable-documentation (request arguments cb-response)
  (let* ((name (intern (gethash "name" arguments))))
    (mcp-server-write-tool-call-text-result
          request
          (documentation-property name 'variable-documentation t)
          cb-response)))

(cl-defmethod mcp-server-enumerate-tools ((this emacs-mcp-server))
  `(
    (:name "emacs-mcp-server-function-documentation" :description "Returns the documentation string for a given Emacs Lisp function symbol. Input is the function name as a string. Output is the full docstring, or an error if not found. Useful for code understanding, introspection, or LLMs that need to explain Emacs functions."
           :properties ((:name name :type "string" :required t :description "Name of the function."))
           :async-lambda emacs-mcp-server-function-documentation)
    (:name "emacs-mcp-server-variable-documentation" :description "Returns the documentation string for a given Emacs Lisp variable symbol. Input is the variable name as a string. Output is the full docstring, or an error if not found. Useful for code understanding, introspection, or LLMs that need to explain Emacs variables."
           :properties ((:name name :type "string" :required t :description "Name of the variable."))
           :async-lambda emacs-mcp-server-variable-documentation)
    ))

(provide 'emacs-mcp-server)
