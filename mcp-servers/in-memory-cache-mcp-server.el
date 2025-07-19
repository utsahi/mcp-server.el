;;; in-memory-cache-mcp-server.el --- An in memory cache -*- lexical-binding: t; -*-

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

(require 'eieio)
(require 'mcp-server)

(defclass in-memory-cache-mcp-server (mcp-server)
  (()))

(defvar in-memory-cache-mcp-server-partitions (make-hash-table :test 'equal))

(defun in-memory-cache-mcp-server-ensure-partition (request-arguments)
  (let* ((partition (or (gethash "partition" arguments) "default")))
    (or (gethash partition in-memory-cache-mcp-server-partitions)
	(puthash partition
		 (list :kvstore (make-hash-table :test 'equal) :queue nil :stack nil)
		 in-memory-cache-mcp-server-partitions))))

(cl-defmethod mcp-server-enumerate-tools ((this in-memory-cache-mcp-server))
  '(
    (:name "add-replace-cache-entry" :description "Add a new entry to the in-memory cache or replace an existing entry."
	   :properties ((:name key :type "string" :required t :description "The key.")
			(:name value :type "string" :required t :description "The value.")
			(:name partition :type "string" :required nil :description "'default' if unspecified."))
	   :async-lambda (lambda (request arguments cb-response)
			   (let* ((ht (plist-get (in-memory-cache-mcp-server-ensure-partition arguments) :kvstore)))
			     (puthash (gethash "key" arguments) (gethash "value" arguments) ht))
			   (mcp-server-write-tool-call-text-result request "Success" cb-response)))

    (:name "get-cache-entry" :description "Get the entry from the in-memory cache."
	   :properties ((:name key :type "string" :required t :description "The key.")
			(:name partition :type "string" :required nil :description "'default' if unspecified."))
	   :async-lambda (lambda (request arguments cb-response)
			   (let* ((ht (plist-get (in-memory-cache-mcp-server-ensure-partition arguments) :kvstore)))
			     (mcp-server-write-tool-call-text-result
			      request
			      (or (gethash (gethash "key" arguments) ht)
				  (error "Cache entry for the key '%s' not found" (gethash "key" arguments)))
			      cb-response))))

    (:name "get-keys" :description "Enumerate keys in the in-memory cache for the given partition."
	   :properties ((:name partition :type "string" :required nil :description "'default' if unspecified."))
	   :async-lambda (lambda (request arguments cb-response)
			   (let* ((ht (plist-get (in-memory-cache-mcp-server-ensure-partition arguments) :kvstore)))
			     (mcp-server-write-tool-call-text-result
			      request
			      (json-encode (vconcat (when ht (hash-table-keys ht))))
			      cb-response))
			   ))

    (:name "list-partitions" :description "Enumerate partitions in the in-memory cache."
	   :properties ()
	   :async-lambda (lambda (request arguments cb-response)
			   (mcp-server-write-tool-call-text-result
			    request
			    (json-encode (vconcat (hash-table-keys in-memory-cache-mcp-server-partitions)))
			    cb-response)
			   ))

    (:name "drop-partitions" :description "Drop a partition from the in-memory cache."
          :properties ((:name partition :type "string" :required nil :description "name of the partition to drop."))
          :async-lambda (lambda (request arguments cb-response)
                          (unless (gethash (gethash "partition" arguments) in-memory-cache-mcp-server-partitions)
                            (error "Partition %s not found" (gethash "partition" arguments)))
                          (remhash (gethash "partition" arguments) in-memory-cache-mcp-server-partitions)
                          (mcp-server-write-tool-call-text-result
                           request
                           "Success"
                           cb-response)
                          ))

    (:name "stack-push" :description "Add a new entry on top of the stack."
	   :properties ((:name value :type "string" :required t :description "The value.")
			(:name partition :type "string" :required nil :description "'default' if unspecified."))
	   :async-lambda (lambda (request arguments cb-response)
			   (let* ((hte (in-memory-cache-mcp-server-ensure-partition arguments)))
			     (plist-put hte :stack (cons (gethash "value" arguments) (plist-get hte :stack)))
			     )
			   (mcp-server-write-tool-call-text-result
			    request
			    "Success"
			    cb-response)))

    (:name "stack-peek" :description "Peek one or more entries from the top of the stack."
	   :properties ((:name count :type "number" :required nil :description "Number of entries to peek. 1 if unspecified.")
			(:name partition :type "string" :required nil :description "'default' if unspecified."))
	   :async-lambda (lambda (request arguments cb-response)
			   (let* ((hte (in-memory-cache-mcp-server-ensure-partition arguments)))
			     (mcp-server-write-tool-call-text-result
			      request
			      (json-encode (vconcat (seq-take (plist-get hte :stack) (or (gethash "count" arguments) 1))))
			      cb-response))))

    (:name "stack-pop" :description "Pop one or more entries from the top of the stack."
	   :properties ((:name count :type "number" :required nil :description "Number of entries to pop. 1 if unspecified.")
			(:name partition :type "string" :required nil :description "'default' if unspecified."))
	   :async-lambda (lambda (request arguments cb-response)
			   (let* ((count (or (gethash "count" arguments) 1))
				  (hte (in-memory-cache-mcp-server-ensure-partition arguments))
				  (popped (seq-take (plist-get hte :stack) count)))
			     (plist-put hte :stack (seq-drop (plist-get hte :stack) count))
			     (mcp-server-write-tool-call-text-result
			      request
			      (json-encode (vconcat popped))
			      cb-response))))

    (:name "enqueue" :description "Add a new entry to queue."
	   :properties ((:name value :type "string" :required t :description "The value.")
			(:name partition :type "string" :required nil :description "'default' if unspecified."))
	   :async-lambda (lambda (request arguments cb-response)
			   (let* ((hte (in-memory-cache-mcp-server-ensure-partition arguments)))
			     (plist-put hte :queue (append (plist-get hte :queue) (list (gethash "value" arguments)))))
			   (mcp-server-write-tool-call-text-result
			    request
			    "Success"
			    cb-response)))

    (:name "queue-peek" :description "Peek one or more entries at the head of the queue."
	   :properties ((:name count :type "number" :required nil :description "Number of entries to peek. 1 if unspecified.")
			(:name partition :type "string" :required nil :description "'default' if unspecified."))
	   :async-lambda (lambda (request arguments cb-response)
			   (let* ((hte (in-memory-cache-mcp-server-ensure-partition arguments)))
			     (mcp-server-write-tool-call-text-result
			      request
			      (json-encode (vconcat (seq-take (plist-get hte :queue) (or (gethash "count" arguments) 1))))
			      cb-response))))

    (:name "dequeue" :description "Dequeue one or more entries from the queue."
	   :properties ((:name count :type "number" :required nil :description "Number of entries to pop. 1 if unspecified.")
			(:name partition :type "string" :required nil :description "'default' if unspecified."))
	   :async-lambda (lambda (request arguments cb-response)
			   (let* ((count (or (gethash "count" arguments) 1))
				  (hte (in-memory-cache-mcp-server-ensure-partition arguments))
				  (popped (seq-take (plist-get hte :queue) count)))
			     (plist-put hte :queue (seq-drop (plist-get hte :queue) count))
			     (mcp-server-write-tool-call-text-result
			      request
			      (json-encode (vconcat popped))
			      cb-response))))

    (:name "save-cache" :description "Saves/takes snapshot of/persists cache to disk."
	   :properties ((:name filepath :type "string" :required t :description "Target file."))
	   :async-lambda (lambda (request arguments cb-response)
			   (let* ()
			     (with-temp-buffer
			       (insert (json-encode in-memory-cache-mcp-server-partitions))
			       (let ((save-silently t))
				 (set-buffer-file-coding-system 'utf-8)
				 (write-file (gethash "filepath" arguments) nil)))
			     (mcp-server-write-tool-call-text-result
			      request
			      "Success"
			      cb-response))))

    (:name "load-cache" :description "Loads or hydrates saved cache snapshot previously persisted to disk."
	   :properties ((:name filepath :type "string" :required t :description "Snapshot file path."))
	   :async-lambda (lambda (request arguments cb-response)
			   (let* ((filecontent
				   (with-temp-buffer
				     (set-buffer-file-coding-system 'utf-8)
				     (insert-file-contents (gethash "filepath" arguments))
				     (buffer-string))))
			     (let* ((ht (make-hash-table :test 'equal))
				    (ht1 (json-parse-string filecontent :array-type 'list)))
			       (mapc (lambda (k1)
				       (let* ((ht2 (gethash k1 ht1))
					      (l (list)))
					 (cl-flet ((val-or-nil (k ht) (if (eq :null (gethash k ht2)) nil (gethash k ht2))))
					   (setq l (plist-put l :queue (val-or-nil "queue" ht2)))
					   (plist-put l :stack (val-or-nil "stack" ht2))
					   (plist-put l :kvstore (val-or-nil "kvstore" ht2))
					   (puthash k1 l ht)
					   )
					 ))
				     (hash-table-keys ht1))
			       (setq in-memory-cache-mcp-server-partitions ht)))

			   (mcp-server-write-tool-call-text-result
			    request
			    "Success"
			    cb-response)))
    )
  )

(provide 'in-memory-cache-mcp-server)
