;;; prompts-mcp-server.el --- Prompts -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Kishor Datar
;; Author: Kishor Datar <kishordatar at gmail>
;; Version: 0.1
;; Package-Requires: ((emacs "30.0"))
;; Keywords: mcp, server, llm, doctor ;

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

(defclass prompts-mcp-server (mcp-server)
  (()))

(cl-defmethod mcp-server-enumerate-prompts ((this prompts-mcp-server) request cb-response)
  '((:name "plan" :description "prompt to generate a plan." :arguments ((:name "unused" :description "unused" :required nil))
           :async-lambda (lambda (request arguments cb-response)
                           (mcp-server-prompt-write-user-message 
                            request
                            cb-response
                            "Generate a detailed plan. Ask for clarifying questions if necessary before proceeding.")))
    (:name  "use-cases" :description  "prompt to generate a use-cases scenarios."
            :async-lambda (lambda (request arguments cb-response)
                           (mcp-server-prompt-write-user-message 
                            request
                            cb-response
                            "Generate a detailed use cases. Ask for clarifying questions if necessary before proceeding."))))
  )

(provide 'prompts-mcp-server)
