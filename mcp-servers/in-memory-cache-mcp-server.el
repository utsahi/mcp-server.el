;;; in-memory-cache-mcp-server.el --- An in memory cache -*- lexical-binding: t; -*-

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

(defclass in-memory-cache-mcp-server (mcp-server)
  (()))

(defvar in-memory-cache-mcp-server-partitions (make-hash-table :test 'equal))

(defun in-memory-cache-mcp-server-ensure-partition (request-arguments)
  (let* ((partition (or (gethash "partition" arguments) "default")))
    (or (gethash partition in-memory-cache-mcp-server-partitions)
	(puthash partition (make-hash-table :test 'equal) in-memory-cache-mcp-server-partitions))))

(cl-defmethod mcp-server-enumerate-tools ((this in-memory-cache-mcp-server))
  '(
    (:name "add-replace-cache-entry" :description "Add a new entry to the in-memory cache or replace an existing entry."
	   :properties ((:name key :type "string" :required t :description "The key.")
			(:name value :type "string" :required t :description "The value.")
			(:name partition :type "string" :required nil :description "The partition under which the key-value pair is stored. Default partition is used if unspecified."))
	   :async-lambda (lambda (request arguments cb-response)
			   (let* ((ht (in-memory-cache-mcp-server-ensure-partition arguments)))
			     (puthash (gethash "key" arguments) (gethash "value" arguments) ht)
			     )
			   (mcp-server-write-tool-call-text-result request "0" cb-response)))
    (:name "get-keys" :description "Enumerate keys in the in-memory cache for the given partition."
	   :properties ((:name partition :type "string" :required nil :description "The partition under which the key-value pair is stored. Default partition is used if unspecified."))
	   :async-lambda (lambda (request arguments cb-response)
			   (let* ((ht (in-memory-cache-mcp-server-ensure-partition arguments)))
			     (mcp-server-write-tool-call-text-result
			      request
			      (json-encode (vconcat (hash-table-keys ht)))
			      cb-response))
			   ))
    (:name "get-partitions" :description "Enumerate partitions in the in-memory cache."
	   :properties ()
	   :async-lambda (lambda (request arguments cb-response)
			   (mcp-server-write-tool-call-text-result
			      request
			      (json-encode (vconcat (hash-table-keys in-memory-cache-mcp-server-partitions)))
			      cb-response)
			   ))
    )
  )
