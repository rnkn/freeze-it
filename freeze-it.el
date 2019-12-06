;;; freeze-it.el --- Minor mode to make your previous writing read-only  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Paul W. Rankin

;; Author: Paul W. Rankin <pwr@x2510.com>
;; Keywords: wp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; # Freeze It #

;; An Emacs minor mode to kill your inner editor! After an idle amount of
;; time, all text before point will be made read-only.

;; Option freeze-it-go-back will go back this far before making everything
;; prior read-only. This can be nil, word, line, visible-line, line, or
;; paragraph.

;; Option freeze-it-delay is the number of seconds to wait before freezing
;; text.

;;; Code:

(defgroup freeze-it
  nil
  "Options for freeze minor mode."
  :prefix "freeze-it-"
  :group 'text)

(defcustom freeze-it-delay
  5
  "Number of seconds in idle time before running `freeze-it-now'."
  :type 'number
  :safe 'numberp
  :group 'freeze-it)

(defcustom freeze-it-go-back
  'line
  "Amount of text before point to go back before freezing."
  :type '(choice (const :tag "None" nil)
                 (const :tag "Word" word)
                 (const :tag "Line" line)
                 (const :tag "Visible Line" visible-line)
                 (const :tag "Paragraph" paragraph))
  :safe 'symbolp
  :group 'freeze-it)

(defcustom freeze-it-lighter
  " FREEZE!"
  "Mode-line indicator for `freeze-it-mode'."
  :type '(choice (const :tag "No lighter" "") string)
  :safe 'stringp
  :group 'freeze-it)

(defun freeze-it-now ()
  "Make text before point read-only, first going back by `freeze-it-go-back'."
  (when freeze-it-mode
    (save-excursion
      (save-restriction
        (widen)
        (let ((go-back
               (intern-soft (concat "forward-"
                                    (symbol-name freeze-it-go-back)))))
          (when go-back (funcall go-back -1)))
        (put-text-property (point-min) (point) 'read-only t)))))

(define-minor-mode freeze-it-mode
  "When enabled, after idle timer `freeze-it-delay', text in the current
buffer before point is made read-only.

The option `freeze-it-go-back' controls how far back before point to
leave alone."
  :init-value nil
  :lighter freeze-it-lighter
  (if freeze-it-mode
      (run-with-idle-timer freeze-it-delay t #'freeze-it-now)
    (cancel-function-timers #'freeze-it-now)))

(provide 'freeze-it)
;;; freeze-it.el ends here
