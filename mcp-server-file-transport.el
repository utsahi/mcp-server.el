;;; mcp-server-file-transport.el --- MCP request dispatcher -*- lexical-binding: t; -*-
;;;    reads/writes requests from/to the specified files

;; Copyright (C) 2025 Kishor Datar
;; Author: Kishor Datar <kishordatar at gmail>
;; Version: 0.1
;; Package-Requires: ((emacs "30.0"))
;; Keywords: mcp, server, network ;

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

(require 'promising-future)
(require 'mcp-server)

(defclass file-promising-future (promising-future)
  ((-request-file :initarg :request-file :initform (error "Must provide :request-file"))
   (-response-file :initarg :response-file :initform (error "Must provide :response-file"))
   (-timeout-sec :initarg :timeout-sec :initform (error "Must provide :timeout-sec"))
   (-timeout-timer :initform nil))
  :documentation "Request is read from request-file and response popped into the response-file.")

(cl-defmethod promising-future-on-completion ((this file-promising-future) result)
  (unless (eq (oref this -state) 'dispatched)
    (error "Invalid state while writing response %s." (oref this -state)))
  (when (oref this -timeout-timer)
    (cancel-timer (oref this -timeout-timer)))
  (with-temp-buffer
    (setq-local require-final-newline nil)
    (set-buffer-file-coding-system 'utf-8)
    (insert result)
    (let ((save-silently t))
      (write-file (oref this -response-file) nil)))
  (cl-call-next-method this result))

(cl-defmethod promising-future-schedule ((this file-promising-future) worker)
  (let* ((request (with-temp-buffer
		    (set-buffer-file-coding-system 'utf-8)
		    (insert-file-contents-literally (oref this -request-file))
		    (string-trim-right (buffer-string)))))    

    (cl-call-next-method
     this
     (lambda ()
       (funcall worker request)))
    
    (if (and (oref this -timeout-sec) (not (eq 0 (oref this -timeout-sec))))
	(oset this -timeout-timer
	      (run-with-timer
	       (oref this -timeout-sec)
	       nil
	       (lambda (promising-future request)
		 (let* ((parsed)
			(req-id))
		   (condition-case ex
		       (setq parsed (json-parse-string request))
		     (error (setq parsed (make-hash-table))))
		   (setq req-id (or (gethash "id" parsed) 0))
		   (message "Request with id %d timed out. Sending a timeout error response." req-id)
		   (promising-future-on-completion
		    this
		    (json-encode
		     `((jsonrpc . "2.0")
		       (id . ,req-id)
		       (error . ((code . -32603)
				 (message . "Timeout.  Server did not respond in a timely manner."))))))))
	       this
	       request)))))

(defvar file-promising-future-sessions (make-hash-table :test 'equal))

(defun mcp-server-file-transport-dispatch-request (session mcp-server request-file response-file timeout-sec)
  (let* ((promising-future
	  (or (gethash session file-promising-future-sessions)
 	      (puthash session (file-promising-future :request-file request-file :response-file response-file :timeout-sec timeout-sec)
		       file-promising-future-sessions)))
	 (server (make-instance mcp-server)))    
    (promising-future-schedule
     promising-future
     (lambda (request)
       (mcp-server-process-request
	server
	request
	(lambda (result)
	  (promising-future-on-completion promising-future result)))))
    
    "Request dispatched."))

(defun mcp-server-file-transport-pop-response-if-ready (session)
  (let* ((promising-future
	  (or (gethash session file-promising-future-sessions)
	      (error (format "Session %s not found" session)))))
    (if (not (promising-future-result-ready-p promising-future))
	0
      (promising-future-pop-result promising-future)
      (remhash session file-promising-future-sessions)
      1)))

(provide 'mcp-server-file-transport)
