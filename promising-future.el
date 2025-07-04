;;; promising-future.el --- to schedule work and poll for completion.   -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Kishor Datar
;; Author: Kishor Datar <kishordatar at gmail>
;; Version: 0.1
;; Package-Requires: ((emacs "30.0"))
;; Keywords: promise, future, worker, async

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

(defclass promising-future ()
  ((-state :initform 'initial)
   (-promise :initform nil)))

(cl-defgeneric promising-future-schedule (obj worker))
(cl-defmethod promising-future-schedule ((this promising-future) worker)
  (when (not (eq (oref this -state) 'initial))
    (error "Invalid state %s" (oref this -state)))
  (oset this -state 'dispatched)
  (run-with-timer
   0
   nil
   worker))

(cl-defgeneric promising-future-on-completion (obj result))
(cl-defmethod promising-future-on-completion ((this promising-future) result)
  (unless (eq (oref this -state) 'dispatched)
      (error "Invalid state %s" (oref this -state)))
  (oset this -promise result)
  (oset this -state 'done))

(cl-defmethod promising-future-result-ready-p ((this promising-future))
  (if (eq 'done (oref this -state)) t nil))

(cl-defmethod promising-future-pop-result ((this promising-future))
  (let* ()
    (unless (promising-future-result-ready-p this)
	(error "No result available.")      
      (oref this -promise))))

(provide 'promising-future)
