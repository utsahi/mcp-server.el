;;; web-mcp-server.el --- Web mcp server -*- lexical-binding: t; -*-

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

(defvar web-mcp-server-url-retrieve-timeout 20)
(defvar web-mcp-server-url-retrieve-max-length 250000)

(defclass web-mcp-server (mcp-server)
  (()))

(defun web-mcp-server-write-result (request result cb-response)
  (if (> (length result) web-mcp-server-url-retrieve-max-length)
      (mcp-server-write-tool-call-error-result
       request
       (format "Length of the response string (%d) exceeds the configured max length %d. Try alternative tool. E.g., try retrieving rendered web page." (length result) web-mcp-server-url-retrieve-max-length)
       cb-response)
    (mcp-server-write-tool-call-text-result
     request
     result
     cb-response)))

(defun web-mcp-server-url-retrieve-internal (url callback)
  (let* ((callback-args (list
			 (list :callback callback
			       :done nil
			       :cancellation-timer nil))))
    (let* ((killed)
	   (url-buffer
	    (url-retrieve url callback callback-args t t)))
      (plist-put (nth 0 callback-args)
		 :cancellation-timer
		 (run-with-timer
		  web-mcp-server-url-retrieve-timeout
		  nil
		  (lambda ()
		    (when (and (not (plist-get callback-args :done))
			       (process-live-p (get-buffer-process url-buffer)))
		      (message "web-mcp-server request timeout. Killing the process.")
		      (set-process-query-on-exit-flag
		       (get-buffer-process url-buffer)
		       nil)
		      (kill-buffer url-buffer))))))))

(defun web-mcp-server-url-retrieve (request arguments cb-response)
  (let* ((url (gethash "url" arguments)))
    (web-mcp-server-url-retrieve-internal
     url
     (lambda (status args)
       (if (plist-get status :error)
           (progn
             (mcp-server-write-tool-call-error-result
              request
              (format "Failed to fetch URL: %s" (plist-get status :error))
              cb-response))

         (web-mcp-server-write-result
          request
          (buffer-substring-no-properties (point-min) (point-max))
          cb-response))))))

(defun web-mcp-server-render-web-page (request arguments cb-response)
  (let* ((url (gethash "url" arguments)))
    (web-mcp-server-url-retrieve-internal
     url
     (lambda (status args)
       (if (plist-get status :error)
           (progn
             (mcp-server-write-tool-call-error-result
              request
              (format "Failed to fetch URL: %s" (plist-get status :error))
              cb-response))
         (goto-char (point-min))
         (when (search-forward-regexp "\r?\n\r?\n" nil t)
           (let ((body (buffer-substring-no-properties (point) (point-max))))
             (with-current-buffer (get-buffer-create "*web-mcp-last-render-buffer*")
               (erase-buffer)
               (insert body)
               (with-silent-modifications (shr-render-region (point-min) (point-max)))
               (let* ((result (buffer-substring-no-properties (point-min) (point-max))))
                 (web-mcp-server-write-result
                  request
                  result
                  cb-response)))
             )))))))

(defun web-mcp-server-collect-process-output (command cb &rest args)
  (let* ((buffer-name (generate-new-buffer-name "*web-mcp-server-process-output*"))
         (buffer (get-buffer-create buffer-name))
         (err-buf (get-buffer-create (generate-new-buffer-name "*web-mcp-server-process-error*"))))
    (with-current-buffer buffer
      (erase-buffer)
      (make-process
       :name (format "%s-%s" (car command) "web-mcp-server")
       :buffer buffer
       :command command
       :noquery t
       :stderr err-buf
       :filter (plist-get args :filter)
       :sentinel (lambda (proc event)
                   (unless (process-live-p proc)
                     (with-current-buffer buffer
                       (funcall cb proc event (plist-get args :args)))
                     (kill-buffer buffer)
                     (kill-buffer err-buf)))))))

(defun web-mcp-server-yt-dlp-minimize-json-info (info)
  (let* ((ht (make-hash-table))
         (parsed (json-parse-string info)))
    (maphash
     (lambda (key val)
       (if (string-equal "automatic_captions" key)
           (let* ((caph (make-hash-table)))
             (maphash
              (lambda (capk capv)
                (when (or (string-equal "en" capk) (string-equal "en-orig" capk))
                  (puthash capk capv caph)))
              val)
             (puthash key caph ht))
         (puthash key val ht))
       )
     parsed)
    (json-encode ht)))

(defun web-mcp-server-yt-dlp-video-json-info (request arguments cb-response)
  (let* ((command
          (append
           (list "yt-dlp" "-j" (or (gethash "url" arguments) (user-error "URL not specified."))))))
    (web-mcp-server-collect-process-output
     command
     (lambda (proc event args)
       (let* ((output (buffer-string)))
         (if (= 0 (process-exit-status proc))
             (mcp-server-write-tool-call-text-result
              (plist-get args :request)
              (web-mcp-server-yt-dlp-minimize-json-info output)
              (plist-get args :cb-response))
           (mcp-server-write-tool-call-error-result
            (plist-get args :request)
            (format "Process exit status %d.\n\n%s"
                    (process-exit-status proc)
                    output)
            (plist-get args :cb-response)))))
     :args (list :request request :cb-response cb-response))))

(cl-defmethod mcp-server-enumerate-tools ((this web-mcp-server))
  `(
    (:name "web-mcp-server-url-retrieve" :description "Retreves a URL. Returns the RAW response including headers. To search
the web for topics, news, quotes, weather etc., use https://html.duckduckgo.com/html/?q=<URL-ESCAPED-SEARCH-QUERY>."
           :properties ((:name url :type "string" :required t :description "URL to fetch."))
           :async-lambda web-mcp-server-url-retrieve)

    (:name "web-mcp-render-web-page" :description "Returns the rendered html content of the URL. Response does not include links, markup etc. "
           :properties ((:name url :type "string" :required t :description "URL to render."))
           :async-lambda web-mcp-server-render-web-page)

    (:name "web-mcp-server-yt-dlp-video-json-info" :description "Returns video info using yt-dlp."
           :properties ((:name url :type "string" :required t :description "URL of the video."))
           :async-lambda web-mcp-server-yt-dlp-video-json-info)
    
    ))

(provide 'web-mcp-server)
