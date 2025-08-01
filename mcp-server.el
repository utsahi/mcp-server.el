;;; mcp-server.el --- Base class for implementing MCP servers in elisp  -*- lexical-binding: t; -*-

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

(require 'json)

(defclass mcp-server ()
  (()))

(cl-defgeneric mcp-server-process-request (obj request cb-response))
(cl-defmethod mcp-server-process-request ((this mcp-server) request cb-response)
  (condition-case outer-ex
      (let* ((parsed-request (json-parse-string request))
	     (id (gethash "id" parsed-request))
	     (method (gethash "method" parsed-request)))
	(condition-case ex
	    (cond ((string-equal method "initialize")
		   (mcp-server-process-initialize-request this parsed-request cb-response))
		  ((string-equal method "notifications/initialized")
		   (mcp-server-on-notifications-initialized this parsed-request cb-response))
		  ((string-equal method "notifications/cancelled")
		   (mcp-server-on-notifications-cancelled this parsed-request cb-response))
		  ((string-equal method "tools/list")
		   (mcp-server-process-tools-list-request this parsed-request cb-response))
		  ((string-equal method "resources/list")
		   (mcp-server-process-resources-list-request this parsed-request cb-response))
		  ((string-equal method "resources/templates/list")
		   (mcp-server-process-resources-templates-list-request this parsed-request cb-response))
		  ((string-equal method "prompts/list")
		   (mcp-server-process-prompts-list-request this parsed-request cb-response))
                  ((string-equal method "prompts/get")
                   (mcp-server-process-prompts-get-request this parsed-request cb-response))
		  ((string-equal method "ping")
		   (mcp-server-process-ping-request this parsed-request cb-response))
		  ((string-equal method "logging/setLevel")
		   (mcp-server-process-logging-setlevel-request this parsed-request cb-response))
		  ((string-equal method "tools/call")
		   (condition-case ex
		       (mcp-server-process-tools-call-request this parsed-request cb-response)
		     (error
		      (mcp-server-write-json-line
		       cb-response
		       (mcp-server-compose-tool-call-error id ex)
		       t))
		     ))
		  ((error "Unknown method %s" method)))
	  (error (mcp-server-write-json-line
		  cb-response
		  (mcp-server-compose-rpc-server-error id method ex)
		  t))))
    (error (mcp-server-write-json-line
	    cb-response
	    (mcp-server-compose-rpc-server-error 0 "" outer-ex)
	    t))))

(cl-defgeneric mcp-server-process-initialize-request (obj request cb-response))
(cl-defmethod mcp-server-process-initialize-request ((this mcp-server) request cb-response)
  (let* ((default-initialize-response
	  `((jsonrpc . "2.0")
	    (id . ,(gethash "id" request))
	    (result
	     (protocolVersion . "2025-03-26")
	     (capabilities
	      (logging . ,(make-hash-table))
	      (prompts (listChanged . :json-false))
	      (resources (subscribe . :json-false) (listChanged . :json-false))
	      (tools (listChanged . :json-false)))
	     (serverInfo
	      (name . ,(format "%s" (type-of this)))
	      (version . "1.0.0"))
	     (instructions . "")))))
    (mcp-server-write-json-line
     cb-response
     (json-encode default-initialize-response))))

(cl-defgeneric mcp-server-on-notifications-initialized (obj request cb-response))
(cl-defmethod mcp-server-on-notifications-initialized ((this mcp-server) request cb-response)
  (mcp-server-write-json-line cb-response ""))

(cl-defgeneric mcp-server-on-notifications-cancelled (obj request cb-response))
(cl-defmethod mcp-server-on-notifications-cancelled ((this mcp-server) request cb-response)
  (mcp-server-write-json-line cb-response ""))

(cl-defgeneric mcp-server-enumerate-tools (obj))
(cl-defmethod mcp-server-enumerate-tools ((this mcp-server))
  )

(cl-defgeneric mcp-server-process-tools-list-request (obj request cb-response))
(cl-defmethod mcp-server-process-tools-list-request ((this mcp-server) request cb-response)
  (let* ((tools
	  (cl-flet ((required-pr (lambda (l p &optional allow-nil)
				   (or (plist-member l p) (error "%s not specified." p))
				   (if allow-nil
				     (plist-get l p)
				     (or (plist-get l p) (error "%s canot be null." p)))))
		    (optional-pr (lambda (l p &optional d) (or (plist-get l p) d))))
	    (mapcar (lambda (tl)
		      (let* ((properties
			      (mapcar (lambda (p)
					(append (list (required-pr p :name)
                                                      `(type . ,(required-pr p :type))
		                                      `(description . ,(or (optional-pr p :description) "")))
                                                (if (string-equal "array" (required-pr p :type)) `((items ,(required-pr p :items))))
                                                (if (optional-pr p :enum) `(
                                                                            (enum . ,(required-pr p :enum))))))
				      (optional-pr tl :properties)))
			     (required
			      (cl-loop for p in (optional-pr tl :properties)
				       if (required-pr p :required t)
				       collect (required-pr p :name))))
			`((name . ,(required-pr tl :name))
			  (description . ,(required-pr tl :description))
			  (inputSchema (type . "object")
				       (properties . ,(or properties (make-hash-table)))
				       (required . ,(vconcat required)))))
		      )
		    (mcp-server-enumerate-tools this)))))
    (mcp-server-write-json-line
     cb-response
     (mcp-server-compose-result request 'tools (vconcat tools))
     )))

(cl-defgeneric mcp-server-process-tools-call-request (obj request cb-response))
(cl-defmethod mcp-server-process-tools-call-request ((this mcp-server) request cb-response)
  (let* ((params (gethash "params" request))
	 (tool-name (gethash "name" params))
	 (arguments (gethash "arguments" params))
	 (tool (or (seq-find (lambda (i) (string-equal tool-name (plist-get i :name)))
			     (mcp-server-enumerate-tools this))
		   (error "tool %s not found" tool-name)))
	 (async-lambda (or (plist-get tool :async-lambda) (error "tool %s does not have an implementation." tool-name))))
    (funcall async-lambda request arguments cb-response)
      ))

(cl-defgeneric mcp-server-process-resources-list-request (obj request cb-response))
(cl-defmethod mcp-server-process-resources-list-request ((this mcp-server) request cb-response)
  (mcp-server-write-json-line
   cb-response
   (mcp-server-compose-result request 'resources [])
   ))

(cl-defgeneric mcp-server-process-resources-templates-list-request (obj request cb-response))
(cl-defmethod mcp-server-process-resources-templates-list-request ((this mcp-server) request cb-response)
  (mcp-server-write-json-line
   cb-response
   (mcp-server-compose-result request 'resourceTemplates [])
   ))

(cl-defgeneric mcp-server-enumerate-prompts (obj))
(cl-defmethod mcp-server-enumerate-prompts ((this mcp-server) request cb-response))

(cl-defgeneric mcp-server-process-prompts-list-request (obj request cb-response))
(cl-defmethod mcp-server-process-prompts-list-request ((this mcp-server) request cb-response)
  (mcp-server-write-json-line
   cb-response
   (mcp-server-compose-result request 'prompts (vconcat (mcp-server-enumerate-prompts this request cb-response)))
   ))

(cl-defgeneric mcp-server-process-prompts-get-request (obj request cb-response))
(cl-defmethod mcp-server-process-prompts-get-request ((this mcp-server) request cb-response)
  (let* ((prompts (mcp-server-enumerate-prompts this request cb-response)))
    (unless prompts
      (error "prompts/get not implemented by %s" (type-of this)))
    (let* ((params (or (gethash "params" request) (error "params not found in the request.")))
           (name (or (gethash "name" params) (error "name not found in the request.")))
           (arguments (or (gethash "arguments" params) (error "arguments not found in the request.")))
           (prompt (or (seq-find (lambda (p) (string-equal (plist-get p :name) name)) prompts) (error "%s not implemented" name)))
           (impl (or (plist-get prompt :async-lambda) (error "%s lambda not implemented" name))))
      (funcall impl request arguments cb-response))))

(cl-defgeneric mcp-server-process-ping-request (obj request cb-response))
(cl-defmethod mcp-server-process-ping-request ((this mcp-server) request cb-response)
  (message "Ping result: %s" (mcp-server-compose-result request nil nil))
  (mcp-server-write-json-line
   cb-response
   (mcp-server-compose-result request nil nil)
   ))

(cl-defgeneric mcp-server-process-logging-setlevel-request (obj request cb-response))
(cl-defmethod mcp-server-process-logging-setlevel-request ((this mcp-server) request cb-response)
  (mcp-server-write-json-line
   cb-response
   ""
   ))

(defun mcp-server-write-json-line (cb-response json &optional trace)
  (when trace (message "Response: %s" json))
  (funcall
   cb-response
   (if (or (string-equal "" json) (not (string-match-p "[\r\n]" json))) json (json-serialize (json-parse-string json)))))

(defun mcp-server-compose-rpc-server-error (id method error)
  (json-encode
   `((jsonrpc . "2.0")
    (id . ,id)
    (error .
	   ((code . -32603)
	    (message . ,(format "%s" error)))))))

(defun mcp-server-compose-result (request label value)
  (json-encode
      `((jsonrpc . "2.0")
	(id . ,(gethash "id" request))
	(result .
		,(if label (list (cons label value)) (make-hash-table))))))

(defun mcp-server-compose-tool-call-error (id error)
  (json-encode
   `((jsonrpc . "2.0")
     (id . ,id)
     (result .
	     ((isError . t)
	      (content .
		       [
			((type . text)
			 (text . ,(format "%s" error)))
			]))))))

(defun mcp-server-compose-tool-call-text-result (request text)
  (json-encode
   `((jsonrpc . "2.0")
     (id . ,(gethash "id" request))
     (result .
	     ((content .
		       [
			((type . text)
			 (text . ,text))
			]))))))

(defun mcp-server-write-tool-call-error-result (request text cb-response)
  (mcp-server-write-json-line
   cb-response
   (mcp-server-compose-tool-call-error (gethash "id" request) text)))

(defun mcp-server-write-tool-call-text-result (request text cb-response)
  (mcp-server-write-json-line
   cb-response
   (mcp-server-compose-tool-call-text-result request text)))

(defun mcp-server-prompt-write-user-message (request cb-response message)
  (mcp-server-write-json-line
   cb-response
   (mcp-server-compose-result
    request
    'messages
    `[((role . user)
       (content . ((type . text)
		   (text . ,message))))])
   ))

(provide 'mcp-server)
