;;; doctor-mcp-server.el --- Psychotherapist for LLMs  -*- lexical-binding: t; -*-

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

(defclass doctor-mcp-server (mcp-server)
  (()))

(cl-defmethod mcp-server-enumerate-tools ((this doctor-mcp-server))
  '(
    (:name "talk-to-doctor" :description "Talk to Emacs doctor"
	   :properties ((:name message :type "string" :required t :description "Next message to Emacs doctor."))
	   :async-lambda (lambda (request arguments cb-response)
			   (require 'doctor)
			   (unless (get-buffer "*doctor*")
			     (call-interactively 'doctor))
			   (with-current-buffer "*doctor*"
			    (end-of-buffer)
			    (let* ((msg (gethash "message" arguments))
				   (pt (point))
				   (doctor-response))
			      (insert msg)
			      (doctor-ret-or-read 1)
			      (doctor-ret-or-read 1)
			      (setq doctor-response
				    (buffer-substring-no-properties (+ pt 1 (length msg)) (point)))
			      (setq doctor-response (string-trim doctor-response))
			      
			      (mcp-server-write-tool-call-text-result request doctor-response cb-response)))))
    )
  )
