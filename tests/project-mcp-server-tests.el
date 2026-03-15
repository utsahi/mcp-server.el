;;; project-mcp-server-tests.el --- Tests for project-mcp-server -*- lexical-binding: t; -*-

(require 'ert)
(require 'project-mcp-server)
(require 'mcp-server-test-util)

(defvar project-mcp-server-tests-project-root
  (let* ((this-file (or load-file-name (buffer-file-name)))
         (tests-dir (and this-file (file-name-directory this-file))))
    (when tests-dir
      (expand-file-name ".." tests-dir))))

(defun project-mcp-server-tests-make-temp-file (&optional content)
  "Create a temp file under tests/.temp-files with optional CONTENT and return its path." 
  (let* ((root (or project-mcp-server-tests-project-root (error "tests root not found")))
         (temp-dir (expand-file-name ".temp-files" root)))
    (unless (file-directory-p temp-dir) (make-directory temp-dir t))
    (let ((fname (expand-file-name (format "pmcp-test-%d-%d.txt" (abs (random)) (truncate (float-time))) temp-dir)))
      (with-temp-file fname
        (when content (insert content)))
      fname)))

(ert-deftest project-mcp-server-get-last-active-project-test ()
  (let ((project-mcp-server-last-buffer-project project-mcp-server-tests-project-root))
    (mcp-server-test-call-tool 'project-mcp-server "project-mcp-server-get-last-active-project")))

(ert-deftest project-mcp-server-read-file-test ()
  (let ((project-mcp-server-last-buffer-project project-mcp-server-tests-project-root)
        (tmp (project-mcp-server-tests-make-temp-file "New content!")))    
    (mcp-server-test-call-tool 'project-mcp-server "project-mcp-server-read-file"
                              'file-path tmp
                              'project-root project-mcp-server-tests-project-root)))

(ert-deftest project-mcp-server-write-file-content-test ()
  (let ((project-mcp-server-last-buffer-project project-mcp-server-tests-project-root)
        (tmp (project-mcp-server-tests-make-temp-file nil)))
    (mcp-server-test-call-tool 'project-mcp-server "project-mcp-server-write-file-content"
                              'project-root project-mcp-server-tests-project-root
                              'file-path tmp
                              'content "New content!")))

(ert-deftest project-mcp-server-replace-string-in-file-test ()
  (let ((project-mcp-server-last-buffer-project project-mcp-server-tests-project-root)
        (tmp (project-mcp-server-tests-make-temp-file "foo bar foo")))
    (mcp-server-test-call-tool 'project-mcp-server "project-mcp-server-replace-string-in-file"
                              'project-root project-mcp-server-tests-project-root
                              'file-path tmp
                              'search-string "foo"
                              'replacement "baz")))

(ert-deftest project-mcp-server-fd-test ()
  (let ((project-mcp-server-last-buffer-project project-mcp-server-tests-project-root))
    (mcp-server-test-call-tool 'project-mcp-server "project-mcp-server-fd"
                              'directory-path project-mcp-server-tests-project-root
                              'match-regexp ".*test.*"
                              'types ["file"])))

(ert-deftest project-mcp-server-git-status-test ()
  (let ((project-mcp-server-last-buffer-project project-mcp-server-tests-project-root))
    (mcp-server-test-call-tool 'project-mcp-server "project-mcp-server-git"
                              'directory project-mcp-server-tests-project-root
                              'git-command "status"
                              'args [])))

(ert-deftest project-mcp-server-ripgrep-test ()
  (let ((project-mcp-server-last-buffer-project project-mcp-server-tests-project-root)
        (tmp (project-mcp-server-tests-make-temp-file "Hello")))
    (mcp-server-test-call-tool 'project-mcp-server "project-mcp-server-ripgrep"
                              'directory project-mcp-server-tests-project-root
                              'search-pattern "Hello"
                              'context-before 0
                              'context-after 0
                              'file-paths (vector tmp)
                              'file-extensions (vector "txt"))))

(ert-deftest project-mcp-server-replace-preserve-lf ()
  "Ensure replacing text in a LF file preserves LF line endings and performs replacement."
  (let* ((project-mcp-server-last-buffer-project project-mcp-server-tests-project-root)
         (tmp (project-mcp-server-tests-make-temp-file "foo\nfoo\n")))
    (mcp-server-test-call-tool 'project-mcp-server "project-mcp-server-replace-string-in-file"
                              'project-root project-mcp-server-tests-project-root
                              'file-path tmp
                              'search-string "foo"
                              'replacement "bar")
    (with-temp-buffer
      (insert-file-contents tmp)
      (let ((content (buffer-string)))
        (should (string-match-p "bar" content))
        (should-not (string-match-p "\r\n" content))))))

(ert-deftest project-mcp-server-replace-preserve-crlf ()
  "Ensure replacing text in a CRLF file preserves CRLF line endings and performs replacement."
  (let* ((project-mcp-server-last-buffer-project project-mcp-server-tests-project-root)
         ;; create content with explicit CRLF sequences
         (tmp (project-mcp-server-tests-make-temp-file "foo\r\nfoo\r\n")))
    (mcp-server-test-call-tool 'project-mcp-server "project-mcp-server-replace-string-in-file"
                              'project-root project-mcp-server-tests-project-root
                              'file-path tmp
                              'search-string "foo"
                              'replacement "bar")
    (with-temp-buffer
      (insert-file-contents-literally tmp)
      (let ((raw (buffer-string)))
        (should (string-match-p "\r\n" raw))
        (should (string-match-p "bar" raw))))))

(ert-deftest project-mcp-server-replace-lf-file-with-crlf-replacement ()
  "Replacing in an LF file with a CRLF-containing replacement inserts CRLF sequences from the replacement."
  (let* ((project-mcp-server-last-buffer-project project-mcp-server-tests-project-root)
         (tmp (project-mcp-server-tests-make-temp-file "start\nsearch\nend\n")))
    (mcp-server-test-call-tool 'project-mcp-server "project-mcp-server-replace-string-in-file"
                              'project-root project-mcp-server-tests-project-root
                              'file-path tmp
                              'search-string "search"
                              'replacement "line1\r\nline2\r\n")
    (with-temp-buffer
      (insert-file-contents-literally tmp)
      (let ((raw (buffer-string)))
        (should (string-match-p "line1\r\nline2\r\n" raw))
        (should (string-match-p "line1" raw))))))

(ert-deftest project-mcp-server-replace-crlf-file-with-lf-replacement ()
  "Replacing in a CRLF file with an LF-only replacement inserts LF sequences from the replacement."
  (let* ((project-mcp-server-last-buffer-project project-mcp-server-tests-project-root)
         (tmp (project-mcp-server-tests-make-temp-file "start\r\nsearch\r\nend\r\n")))
    (mcp-server-test-call-tool 'project-mcp-server "project-mcp-server-replace-string-in-file"
                              'project-root project-mcp-server-tests-project-root
                              'file-path tmp
                              'search-string "search"
                              'replacement "lineA\nlineB\n")
    (with-temp-buffer
      (insert-file-contents-literally tmp)
      (let ((raw (buffer-string)))
        ;; replacement contains LF; check that the literal replacement sequence exists
        (should (string-match-p "lineA\nlineB\n" raw))
        (should (string-match-p "lineA" raw))))))

(provide 'project-mcp-server-tests)


