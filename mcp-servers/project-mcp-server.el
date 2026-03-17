;;; project-mcp-server.el --- Project aware mcp server -*- lexical-binding: t; -*-

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
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

(require 'eieio)
(require 'mcp-server)
(require 'project)

(defconst project-mcp-server-max-file-length 250000)
(defvar project-mcp-server-last-buffer-project nil)
(defvar project-mcp-server-set-window-project-idle-timer-duration 2)
(defvar project-mcp-server-set-window-project-timer nil)
(defvar project-mcp-server-allowed-git-commands
  ["blame"
   "branch"
   "describe"
   "diff"
   "fetch"
   "grep"
   "log"
   "ls-files"
   "merge"
   "merge-base"
   "merge-tree"
   "pull"
   "reflog"
   "show"
   "status"])

(defgroup project-mcp-server nil
  "Project MCP server."
  :group 'project-mcp-server)

(defcustom project-mcp-server-project-relations nil
  "Alist mapping project root directories to lists of related project root directories.
Each element is of the form (PROJECT-ROOT . (RELATED-ROOT ...)).
"
  :type '(alist :key-type (string :tag "Project root")
                :value-type (repeat string :tag "Related project"))
  :group 'project-mcp-server)

(defun project-mcp-server-add-project-relation ()
  (interactive)
  (let* ((current (or (project-current) (user-error "Not inside a project.")))
         (new (project-prompt-project-dir)))
    (when new
      (let* ((curr-proj-dir (expand-file-name (project-root current)))
             (new-proj-dir (expand-file-name new))
             (curr-relations (alist-get curr-proj-dir project-mcp-server-project-relations nil nil 'string-equal)))
        (if curr-relations
            (setf (alist-get curr-proj-dir project-mcp-server-project-relations nil nil 'string-equal) (list (cons new-proj-dir  (car curr-relations))))
          (customize-save-variable 'project-mcp-server-project-relations (cons (list curr-proj-dir (list new-proj-dir)) project-mcp-server-project-relations)))))))

(defun project-mcp-server-set-window-project-timer-fn ()
  (setq project-mcp-server-last-buffer-project
        (let* ((pr (project-current)))
          (when pr (expand-file-name (project-root pr))))))

(setq project-mcp-server-set-window-project-timer
      (run-with-idle-timer
       project-mcp-server-set-window-project-idle-timer-duration
       t
       'project-mcp-server-set-window-project-timer-fn))

(defclass project-mcp-server (mcp-server)
  (()))

(defun project-mcp-server-validate-path (path)
  (unless (and (not (string-equal "" path))
               (file-exists-p path)
               (let* ((default-directory (file-name-directory path)))
                 (project-current nil path)))
    (error "Path '%s' is not within a known project or doesn't exist! You must get the last active project first." path))
  path)

(defun project-mcp-server-process-output-length-limit-filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (unless (boundp 'project-mcp-server-output-length)
        (setq-local project-mcp-server-output-length 0))
      (let* ((moving (= (point) (process-mark proc)))
             (count (length string))
             (output-too-long (> (+ count project-mcp-server-output-length)
                                 project-mcp-server-max-file-length)))
        (save-excursion
          ;; Insert the text, advancing the process marker.
          (goto-char (process-mark proc))
          (if output-too-long
              (progn
                (erase-buffer)
                (insert (format "\n\n\nProcess was killed and output was truncated as the total output length exceeded the set limit of %d. Please adjust the command parameters.\n\n"
                                project-mcp-server-max-file-length)))
            (insert string)
            (setq project-mcp-server-output-length (+ count project-mcp-server-output-length)))
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc)))
        (when output-too-long
          (delete-process proc))))))

(defun project-mcp-server-collect-process-output (command cb &rest args)
  "Execute a shell COMMAND asynchronously and collect its output into a temporary buffer.

COMMAND is a list of strings representing the shell command and its arguments.

CB is a callback function that gets invoked upon process completion. It receives the process EVENT
and any additional arguments provided in ARGS.

ARGS is a plist of additional arguments to pass to the callback function. Use the key `:args` in
ARGS to pass specific additional data to the callback function.

CB can use `current-buffer` to access the command's output if needed.

Example usage:
(project-mcp-server-collect-process-output
 (list \"ls\" \"-l\")
 (lambda (proc event args)
 (when (string= event \"finished\\n\")
 (let ((output (buffer-string))) ;; Read the output here
 (message \"Process output: %s\" output)
 (when args
 (message \"Additional args: %s\" args)))))
 :args '(\"some additional data\"))

This executes the \"ls -l\" command asynchronously. The callback accesses and processes the command's
output from the current buffer, and it can also make use of any additional arguments provided (e.g.,
'(\"some additional data\"))."
  (let* ((buffer-name (generate-new-buffer-name "*process-output*"))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (erase-buffer)
      (make-process
       :name (format "%s-%s" (car command) "process-mcp-server")
       :buffer buffer
       :command command
       :noquery t
       :filter (plist-get args :filter)
       :sentinel (lambda (proc event)
                   (unless (process-live-p proc)
                     (with-current-buffer buffer
                       (funcall cb proc event (plist-get args :args)))
                     (kill-buffer buffer)))))))

(defun project-mcp-server-ripgrep (request arguments cb-response)
  (let* ((directory (project-mcp-server-validate-path (gethash "directory" arguments)))
         (search-pattern (gethash "search-pattern" arguments))
         (file-extensions (mapcar 'identity (gethash "file-extensions" arguments)))
         (file-paths (mapcar 'identity (gethash "file-paths" arguments)))
         (context-before (or (gethash "context-before" arguments) 0))
         (context-after (or (gethash "context-after" arguments) 0))
         (fixed-strings (gethash "fixed-strings" arguments))
         (custom-type "custom")
         (default-directory directory)
         (command (append (list "rg" "--color=never")
                          (when file-extensions
                            (list "--type-add"
                                  (format "%s:*.{%s}"
                                          custom-type
                                          (string-join (mapcar (lambda (e) (format "%s" e)) file-extensions) ","))))
                          (when file-extensions
                            (list (format "-t%s" custom-type)))
                          ;; If fixed-strings flag is provided, run rg in fixed-strings mode.
                          (when fixed-strings (list "-F"))
                          (unless (= 0 context-before) (list "-A" (number-to-string context-before)))
                          (unless (= 0 context-after) (list "-B" (number-to-string context-after)))
                          (list  "--iglob" "!*~" "--iglob" "!*#" "--iglob" "!**/obj/**" "--iglob" "!**/objd/**"
                                 search-pattern)
                          (or file-paths (list directory)))))
    (project-mcp-server-collect-process-output
     command
     (lambda (proc event args)
       (let* ((output (buffer-string)))
         (if (= 0 (process-exit-status proc))
             (mcp-server-write-tool-call-text-result
              (plist-get args :request)
              output
              (plist-get args :cb-response))
           (if (= 1 (process-exit-status proc))
               (mcp-server-write-tool-call-error-result
                (plist-get args :request)
                "No matches found!"
                (plist-get args :cb-response))
             (mcp-server-write-tool-call-error-result
                (plist-get args :request)
                (format "Process exit status %d.\n\n%s"
                     (process-exit-status proc)
                     output)
                (plist-get args :cb-response))))))
     :args (list :request request :cb-response cb-response)
     :filter 'project-mcp-server-process-output-length-limit-filter)))

(defun project-mcp-server-fd (request arguments cb-response)
  (let* ((directory (project-mcp-server-validate-path (gethash "directory-path" arguments)))
         (search-pattern (gethash "match-regexp" arguments))
         (use-glob (gethash "use-glob" arguments))
         (types (gethash "types" arguments))
         (default-directory directory)
         (command (append '("fd"
                           "--color=never" "--absolute-path")
                          (if types (seq-mapcat (lambda (t) `("--type" ,t)) types))
                          ;; If use-glob is truthy, pass --glob PATTERN; otherwise pass the pattern as-is (regex)
                          (if use-glob
                              `(,(concat "--glob" ) ,search-pattern ,directory)
                            `(,search-pattern ,directory)))))
    (project-mcp-server-collect-process-output
     command
     (lambda (proc event args)
       (let* ((output (buffer-string)))
         (if (= 0 (process-exit-status proc))
             (mcp-server-write-tool-call-text-result
              (plist-get args :request)
              output
              (plist-get args :cb-response))
           (mcp-server-write-tool-call-error-result
            (plist-get args :request)
            (format "Process exit status %d.\n\n%s"
                    (process-exit-status proc)
                    output)
            (plist-get args :cb-response)))))
     :args (list :request request :cb-response cb-response)
     :filter 'project-mcp-server-process-output-length-limit-filter)))

(defun project-mcp-server-git (request arguments cb-response)
  (let* ((directory (project-mcp-server-validate-path (gethash "directory" arguments)))
         (git-command (gethash "git-command" arguments))
         (default-directory directory)
         (validated-command (or (seq-find
                                 (lambda (c) (string-equal c git-command))
                                 project-mcp-server-allowed-git-commands)
                                (error "git-command %s is not allowed!" (gethash "git-command" arguments))))
         (input-args (mapcar 'identity (gethash "args" arguments)))
         (filtered-input-args (if (string-equal git-command (car input-args)) (cdr input-args) input-args))
         (command (append (list "git" "--no-pager" "-c" "color.ui=false" validated-command) (seq-map 'identity filtered-input-args))))
    (project-mcp-server-collect-process-output
     command
     (lambda (proc event args)
       (mcp-server-write-tool-call-text-result
        (plist-get args :request)
        (buffer-string)
        (plist-get args :cb-response)))
     :args (list :request request :cb-response cb-response)
     :filter 'project-mcp-server-process-output-length-limit-filter)))

(defun project-mcp-server-match-case-insensitively (dir file)
  (if (file-exists-p file)
      (expand-file-name file)
    (if (and dir (file-exists-p dir))
        (if (file-exists-p (file-name-concat dir file))
            (expand-file-name (file-name-concat dir file))
          (let* ((case-fold-search t)
                 (matching-files
                  (seq-filter
                   (lambda (f) (string-match-p (format "%s$" (regexp-quote file)) f))
                   (directory-files dir  t))))
            (if (= 1 (length matching-files))
                (nth 0 matching-files)))))))

(defun project-mcp-server-read-file-lines-or-error (directory file range)
  (if (and range (> (car range) (cdr range)))
      (error "Invalid range %d:%d."
             (car range)
             (cdr range)))
  (let* ((effective-path (project-mcp-server-match-case-insensitively directory file)))
    (if effective-path
        (with-temp-buffer
          (project-mcp-server-validate-path effective-path)
          (insert-file-contents-literally effective-path)
          (let* ((full (buffer-string))
                 (content))
            (if (not range)
                (setq content full)
              (let* ((all-lines (string-split full "\n"))
                     (some-lines
                      (seq-take (seq-drop all-lines (- (car range) 1))
                                (- (cdr range) (car range) -1))))
                (setq content (string-join some-lines "\n"))))
            (if (> (length content) project-mcp-server-max-file-length)
                (error "File %s too large. Length %d. Line count %d. Narrow down the line ranges. Maximum allowed length %d."
                       file
                       (length content)
                       (length (string-split content "\n"))
                       project-mcp-server-max-file-length)
              content)))
      (error "Path not found %s. Under directory '%s'. If using a relative
path, make sure it is constructed correctly relative to the directory
path.  The match may be case sensitive. Try to find the actual casing of
the file or directory names."
              file
              directory))))

(defun project-mcp-server-read-file (request arguments cb-response)
  (let* ((directory (gethash "project-root" arguments))
         (path (gethash "file-path" arguments)))
    (mcp-server-write-tool-call-text-result
     request
     (project-mcp-server-read-file-lines-or-error directory path nil)
     cb-response)))

(defun project-mcp-server-read-file-lines (request arguments cb-response)
  (let* ((directory (gethash "project-root" arguments))
         (path (gethash "file-path" arguments))
         (line-range-start (gethash "line-range-start" arguments))
         (line-range-end (gethash "line-range-end" arguments)))
    (mcp-server-write-tool-call-text-result
     request
     (json-encode (project-mcp-server-read-file-lines-or-error directory path (cons line-range-start line-range-end)))
     cb-response)))

(defun project-mcp-server-replace-string-in-file (request arguments cb-response)
  (let* ((path (project-mcp-server-validate-path
                (project-mcp-server-match-case-insensitively
                 (gethash "project-root" arguments)
                 (gethash "file-path" arguments))))
         (replacement (gethash "replacement" arguments))         
         (search-string (gethash "search-string" arguments))         
         (replacement-count 0))
    (with-temp-buffer
      (insert-file-contents-literally path)            
      (goto-char (point-min))
      (while (search-forward search-string nil t)
        (replace-match replacement nil t)
        (cl-incf replacement-count))
      (write-file path))
    (mcp-server-write-tool-call-text-result
     request
     (format "Replaced %d occurrences. %s"
             replacement-count
             (if (= 0 replacement-count)
                 "Try recomputing the replacement by reading the line range."
               ""))
     cb-response)))

(defun project-mcp-server-write-file-content (request arguments cb-response)
  (let* ((fp-arg (gethash "file-path" arguments))
         (path (if (file-exists-p fp-arg) fp-arg
                 (file-name-concat (gethash "project-root" arguments) (gethash "file-path" arguments)))))
    (write-region (gethash "content" arguments) nil path nil 'novisit)
    (mcp-server-write-tool-call-text-result
     request
     "Success"
     cb-response)))

(defun project-mcp-server-last-active-project-or-error ()
  (if project-mcp-server-last-buffer-project (json-encode `(:root ,project-mcp-server-last-buffer-project))
    (error "No project was active! Try to cd and check the value of the variable project-mcp-server-last-buffer-project .")))

(defun project-mcp-server-get-last-active-project (request arguments cb-response)
  (mcp-server-write-tool-call-text-result
   request
   (project-mcp-server-last-active-project-or-error)
   cb-response))

(cl-defmethod mcp-server-enumerate-prompts ((this project-mcp-server) request cb-response)
  '((:name "project-mcp-server-set-project-root" :description "Selects the project root."
           :async-lambda
           (lambda (request arguments cb-response)
             (mcp-server-prompt-write-user-message
              request
              cb-response
              (format "Please use the following project directory %s."
                      (project-mcp-server-last-active-project-or-error)))))))

(cl-defmethod mcp-server-enumerate-tools ((this project-mcp-server))
  `(
    (:name "project-mcp-server-get-last-active-project" :description "Returns the root directory of the last active project as a JSON object. Useful for LLMs to discover the current project context before performing file or code operations."
           :async-lambda project-mcp-server-get-last-active-project)

    (:name "project-mcp-server-read-file" :description ,(format "Reads the entire contents of a file. Fails with an error message if the file is too large.\n- Limit: max file length is %d characters. Use project-mcp-server-read-file-lines for large files."
      project-mcp-server-max-file-length)
           :properties ((:name file-path :type "string" :required t :description "File path.")
                        (:name project-root :type "string" :required t :description "Project root."))
           :async-lambda project-mcp-server-read-file)

    (:name "project-mcp-server-read-file-lines" :description "Reads a specific range of lines from a file. Input: file path, project root, start and end line numbers. Output: JSON-encoded string with the requested lines. Use for large files or when only a section is needed. LLMs should prefer this for efficiency."
           :properties ((:name file-path :type "string" :required t :description "File path.")
                        (:name project-root :type "string" :required t :description "Project root.")
                        (:name line-range-start :type "number" :required t :description "Line range start.")
                        (:name line-range-end :type "number" :required t :description "Line range end"))
           :async-lambda project-mcp-server-read-file-lines)

    (:name "project-mcp-server-write-file-content" :description "Overwrites the entire content of a file. Input: project root, file path, and new content. Use with care—this replaces all existing content. LLMs should validate file paths and content before calling."
           :properties ((:name project-root :type "string" :required t :description "Last active project.")
                        (:name file-path :type "string" :required t :description "If relative path, it is calculated relative to project-root.")
                        (:name content :type "string" :required t :description "File content."))
           :async-lambda project-mcp-server-write-file-content)

    (:name "project-mcp-server-replace-string-in-file" :description "Replaces all occurrences of a string in a file. Input: project root, file path, search string, and replacement. Useful for LLMs updating code, refactoring, or making bulk changes. Returns the number of replacements."
           :properties ((:name project-root :type "string" :required t :description "Last active project.")
                        (:name file-path :type "string" :required t :description "If relative path, it is calculated relative to project-root.")
                        (:name search-string :type "string" :required t :description "String to replace.")
                        (:name replacement :type "string" :required t :description "Replacement string."))
           :async-lambda project-mcp-server-replace-string-in-file)    

    (:name "project-mcp-server-fd" :description "Finds file or directory paths using the 'fd' command. Input: directory path, match regexp, and optional types. Returns a list of matching paths. LLMs can use this for fast file discovery in large projects. IMPORTANT: the match-regexp is passed directly to fd (no shell expansion). The 'match-regexp' is passed directly to fd (no shell expansion). When 'use-glob' is true, the value must be a glob pattern (for example "*.el" or "**/*.el") and not a regular expression. Patterns beginning with a dot (for example ".rc" or patterns that start with a '.') may match hidden files; fd filters hidden files by default. To include hidden files when running fd directly, use the --hidden (-H) flag (not exposed by this tool). Avoid shell quoting characters in the pattern; they are treated literally by fd.

Exit codes: 0 = matches found, 1 = no matches, >1 = fd error. Examples (Linux/Unix):\n\n  -- Regex search for files containing 'TODO':\n     {"directory-path": "/path/to/project", "match-regexp": "TODO"}\n\n  -- Glob search for .md files (use fd --glob):\n     {"directory-path": "/path/to/project", "use-glob": true, "match-regexp": "*.md"}\n\nWindows example (PowerShell/CMD):\n\n  -- Literal search for 'README.md':\n     {"directory-path": "C:\\path\\to\\project", "match-regexp": "README.md"}\n\nNote: this tool invokes fd directly; shell quoting/expansion happens outside this function if callers run a shell before invoking it."
           :properties ((:name directory-path :type "string" :required t :description "Directory path within the project.")
                        (:name match-regexp :type "string" :required t :description "Regular expression or glob passed to the 'fd' command.")
                        (:name use-glob :type "boolean" :required nil :description "If non-nil, treat match-regexp as a glob and pass --glob to fd.")
                        (:name types :type "array" :required nil :description "types one or more of [\"file\" \"directory\" \"executable\" \"empty\"]" :items (:type . "string")))
           :async-lambda project-mcp-server-fd)

    (:name "project-mcp-server-git" :description "Runs a git command in the context of the last active project. Input: directory, git command (from allowed list), and arguments. Output: command result or error. LLMs should use this for version control operations. IMPORTANT: the args are passed directly to git (no shell expansion). Do NOT rely on shell globbing or variable expansion in args. Examples (Linux/Unix):\n\n  -- Get status:\n     {"directory": "/path/to/project", "git-command": "status", "args": []}\n\n  -- Show blame for a file:\n     {"directory": "/path/to/project", "git-command": "blame", "args": ["--", "src/main.c"]}\n\nWindows example (PowerShell/CMD):\n\n  -- Example using a Windows path:\n     {"directory": "C:\\path\\to\\project", "git-command": "status", "args": []}\n\nNote: this tool invokes git directly; shell quoting/expansion happens outside this function if callers run a shell before invoking it."
           :properties ((:name directory :type "string" :required t :description "Project discovered with 'project-get-last-active-project'")
                        (:name git-command :type "string" :required t :description "git command." :enum ,project-mcp-server-allowed-git-commands)
                        (:name args :type "array" :required t :description "list of arguments." :items (:type . "string")))
           :async-lambda project-mcp-server-git)

    (:name "project-mcp-server-ripgrep"
           :description "Runs ripgrep ('rg') in the project. Input: directory, search pattern, context lines, file paths/extensions. Output: matching lines with context. LLMs should use this for fast, large-scale code search. IMPORTANT: the search-pattern is passed directly to rg (no shell expansion) and is interpreted by ripgrep's PCRE2 engine. Do NOT include additional shell quoting characters as they will be treated literally.

Common pitfalls and tips:

- Word-boundary \b: PCRE2 defines word characters as [A-Za-z0-9_]. Tokens containing non-word characters (for example hyphens like 'mcp-server') will not be matched by \b anchors. Use plain 'mcp-server' or explicit lookarounds such as (?<![A-Za-z0-9_])mcp-server(?![A-Za-z0-9_]).
- JSON / escaping: when sending patterns in JSON, backslashes must be escaped. Example: to send \b you must provide "\\b" in the JSON payload.
- Literal matches: to match characters like '*', '?', '$', or patterns that resemble shell expansions, set 'fixed-strings' true to run rg with -F (fixed-strings) for literal matching.

Exit codes: 0 = matches found, 1 = no matches, >1 = rg error (e.g., invalid regex). Examples (Linux/Unix):

  -- Literal search for asterisk (*) using fixed-strings:
     {"directory": "/path/to/project", "search-pattern": "*", "fixed-strings": true}

  -- Regex search for any lowercase letter [a-z]:
     {"directory": "/path/to/project", "search-pattern": "[a-z]"}

  -- Literal search for a string that looks like a shell expansion (e.g. $(echo hi)):
     {"directory": "/path/to/project", "search-pattern": "$(echo hi)", "fixed-strings": true}

Windows examples (PowerShell/CMD):

  -- Literal search for asterisk (*) (PowerShell/CMD):
     {"directory": "C:\\path\\to\\project", "search-pattern": "*", "fixed-strings": true}

  -- Literal search for an environment-variable-like token (CMD):
     {"directory": "C:\\path\\to\\project", "search-pattern": "%USERNAME%", "fixed-strings": true}

  -- PowerShell: to avoid PowerShell interpolation you would normally single-quote the argument; however this tool invokes rg directly so quoting is not applied here. Use fixed-strings for literal matches:
     {"directory": "C:\\path\\to\\project", "search-pattern": "$(echo hi)", "fixed-strings": true}

Note: examples show platform-specific shell characters. The same principle applies across platforms: this tool invokes rg directly (no shell expansion), so patterns are interpreted by ripgrep/PCRE2. If callers run a shell before invoking this tool, shell-specific quoting/expansion may apply there; prefer using the fixed-strings flag when you need literal matching."
           :properties ((:name directory :type "string" :required t :description "Project discovered with 'project-get-last-active-project'")
                        (:name search-pattern :type "string" :required t :description "A rust regular expression. Do NOT perform shell quoting.")
                        (:name context-before :type "number" :required t :description "Include these many lines of context that preceed the matching line. Default 0.")
                        (:name context-after :type "number" :required t :description "Include these many lines of context that follow the matching line. Default 0.")
                        (:name file-paths :type "array" :required nil :description "File paths." :items (:type . "string"))
                        (:name file-extensions :type "array" :required nil :description "File extensions." :items (:type . "string"))
                        (:name fixed-strings :type "boolean" :required nil :description "If non-nil, run rg in fixed-strings mode (-F) to match the search-pattern literally rather than as a regex.") )
           :async-lambda
           project-mcp-server-ripgrep)
    )
  )

(provide 'project-mcp-server)
