;;; mcp-server-headless-stdio-transport.el --- MCP request dispatcher run with emacs --script -*- lexical-binding: t; -*-

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

;; run as:
;;   emacs --script mcp-server-headless-stdio-transport.el doctor-mcp-server.el doctor-mcp-server 3

(message "Adding to load-path %s" (file-name-parent-directory load-file-name))
(add-to-list 'load-path (file-name-parent-directory load-file-name))
(message "Adding to load-path %s" (file-name-concat (file-name-parent-directory load-file-name) "mcp-servers"))
(add-to-list 'load-path (file-name-concat (file-name-parent-directory load-file-name) "mcp-servers"))

(require 'promising-future)
(require 'mcp-server)
(load (or (nth 0 argv) (error "Must specify the .el file containing the mcp-server implementation as the first argument.")) nil t)

(defclass stdio-promising-future (promising-future)
  ((-timeout-sec :initarg :timeout-sec :initform (error "Must provide :timeout-sec"))
   (-timeout-timer :initform nil))
  :documentation "Request is read from the console.")

(cl-defmethod promising-future-on-completion ((this stdio-promising-future) result)
  (unless (eq (oref this -state) 'dispatched)
    (error "Invalid state while writing response %s." (oref this -state)))
  (when (oref this -timeout-timer)
    (cancel-timer (oref this -timeout-timer)))
  (princ (format "%s\n" result))
  (cl-call-next-method this result))

(cl-defmethod promising-future-schedule ((this stdio-promising-future) worker)
  (let* ((request (read-from-minibuffer "")))    
    
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

(let* ((server-class (intern (or (nth 1 argv) (error "Must specify the mcp-server class name as the second argument."))))
       (timeout (string-to-number (or (nth 2 argv) "0"))))
  (while t
    (message "Reading the next request")
    (let* ((promising-future (stdio-promising-future :timeout-sec timeout))
	   (server
	    (make-instance server-class)))
      (promising-future-schedule
       promising-future
       (lambda (request)
	 (mcp-server-process-request
	  server
	  request
	  (lambda (result)
	    (promising-future-on-completion promising-future result)))))

      (while (not (promising-future-result-ready-p promising-future))
	(sleep-for 0.1)
	(message "Waiting for completion. Timeout %f sec (0=no timeout)." timeout))
      )))

(provide 'mcp-server-headless-stdio-transport)
