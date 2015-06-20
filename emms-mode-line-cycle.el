;;; emms-mode-line-cycle.el --- An extension of emms-mode-line.el for displaying `emms-mode-line-string' cyclically -*- lexical-binding: t -*-

;; Copyright (C) 2015 momomo5717

;; Keywords: emms mode-line
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4") (emms "4.0"))
;; URL: https://github.com/momomo5717/emms-mode-line-cycle

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by

;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;  This is a minor mode for updating `emms-mode-line-string' cyclically within specified width
;;  with `emms-playing-time-display'.

;; Setup:
;;
;; (add-to-list 'load-path "/path/to/emms-mode-line-cycle")
;; (require 'emms-mode-line-cycle)
;;
;; (emms-mode-line 1)
;; (emms-playing-time 1)
;;
;; (emms-mode-line-cycle 1)
;;
;; User Option:
;;
;;  + `emms-mode-line-cycle-max-width'
;;  + `emms-mode-line-cycle-current-title-function'
;;

;;; Code:
(require 'emms-mode-line)
(require 'emms-playing-time)

(defgroup emms-mode-line-cycle nil
  "Update `emms-mode-line-string' cyclically with `emms-playing-time-display'."
  :prefix "emms-mode-line-cycle-"
  :group 'emms-mode-line)

(defcustom emms-mode-line-cycle-max-width 16
  "Max width of the track title."
  :type 'integer
  :group 'emms-mode-line-cycle)

(defcustom emms-mode-line-cycle-current-title-function
  (lambda () (emms-track-description
          (emms-playlist-current-selected-track)))
  "Getter function for the current track title."
  :type 'function
  :group 'emms-mode-line-cycle)

(defvar emms-mode-line-cycle) ; Suppress a warning message

(defvar emms-mode-line-cycle--title "")
(defvar emms-mode-line-cycle--title-width 0)
(defvar emms-mode-line-cycle--title-chars '())
(defvar emms-mode-line-cycle--title-chars-last '())

(defun emms-mode-line-cycle--substring (str &optional width)
  "Substring STR with `emms-mode-line-cycle-max-width'.
WIDTH is string width."
  (truncate-string-to-width
   str (or width emms-mode-line-cycle-max-width) 0 ? ))

(defun emms-mode-line-cycle--playlist-current ()
  "Format the current track title."
  (setq emms-mode-line-cycle--title
        (funcall emms-mode-line-cycle-current-title-function)
        emms-mode-line-cycle--title-width
        (string-width emms-mode-line-cycle--title)
        emms-mode-line-cycle--title-chars
        (cons ?  (string-to-list emms-mode-line-cycle--title))
        emms-mode-line-cycle--title-chars-last
        (last emms-mode-line-cycle--title-chars))
  (format emms-mode-line-format
          (if (<= emms-mode-line-cycle--title-width emms-mode-line-cycle-max-width)
              emms-mode-line-cycle--title
            (emms-mode-line-cycle--substring emms-mode-line-cycle--title))))

(defun emms-mode-line-cycle--update-title-chars ()
  "Update `emms-mode-line-cycle--title-chars'."
  (let ((new-last (list (car emms-mode-line-cycle--title-chars))))
    (setcdr emms-mode-line-cycle--title-chars-last new-last)
    (setq emms-mode-line-cycle--title-chars-last new-last
          emms-mode-line-cycle--title-chars
          (cdr emms-mode-line-cycle--title-chars))))

(defun emms-mode-line-cycle--update-mode-line-string ()
  "Update `emms-mode-line-string'."
  (when (and emms-mode-line-cycle
             (> emms-mode-line-cycle--title-width emms-mode-line-cycle-max-width))
    (setq emms-mode-line-string
          (format
           emms-mode-line-format
           (emms-mode-line-cycle--substring
            (apply #'string (emms-mode-line-cycle--update-title-chars)))))))

;;;###autoload
(define-minor-mode emms-mode-line-cycle
  "Update `emms-mode-line-string' cyclically with `emms-playing-time-display'."
  :global t
  (if emms-mode-line-cycle
      (unless (eq emms-mode-line-mode-line-function
                  'emms-mode-line-cycle--playlist-current)
        (put 'emms-mode-line-cycle--playlist-current
             :default-mode-line-function
             emms-mode-line-mode-line-function)
        (setq emms-mode-line-mode-line-function
              'emms-mode-line-cycle--playlist-current)
        (advice-add 'emms-playing-time-display
                    :before #'emms-mode-line-cycle--update-mode-line-string))
    
    (when (eq emms-mode-line-mode-line-function
              'emms-mode-line-cycle--playlist-current)
      (setq emms-mode-line-mode-line-function
            (get 'emms-mode-line-cycle--playlist-current
                 :default-mode-line-function))
      (put 'emms-mode-line-cycle--playlist-current :default-mode-line-function
           nil))
    (advice-remove 'emms-playing-time-display
                   #'emms-mode-line-cycle--update-mode-line-string)))

(provide 'emms-mode-line-cycle)
;;; emms-mode-line-cycle.el ends here
