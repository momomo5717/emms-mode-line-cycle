;;; emms-mode-line-cycle.el --- An extension of emms-mode-line.el for displaying `emms-mode-line-string' cyclically -*- lexical-binding: t -*-

;; Copyright (C) 2015 momomo5717

;; Keywords: emms, mode-line
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
;;
;;  It is useful for long track titles.
;;
;; Further information is available from:
;; https://github.com/momomo5717/emms-mode-line-cycle  (README.org)
;;

;; Setup:
;;
;; (add-to-list 'load-path "/path/to/emms-mode-line-cycle")
;; (require 'emms-mode-line-cycle)
;;
;; (emms-mode-line 1)
;; (emms-playing-time 1)
;;
;; If you use this package like `emms-mode-line-icon', you need to load it.
;; (require 'emms-mode-line-icon)
;; (custom-set-variables '(emms-mode-line-cycle-use-icon-p t))
;;
;; (emms-mode-line-cycle 1)
;;
;; User Option:
;;
;;  + `emms-mode-line-cycle-max-width'
;;  + `emms-mode-line-cycle-additional-space-num'
;;  + `emms-mode-line-cycle-use-icon-p'
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
  "Max width of display title."
  :type 'integer)

(defcustom emms-mode-line-cycle-additional-space-num 2
  "The number of space characters to add them to the current title."
  :type 'integer)

(defcustom emms-mode-line-cycle-use-icon-p nil
  "Use icon like `emms-mode-line-icon-function'.
If you use it, you need to load `emms-mode-line-icon'."
  :type 'boolean)

(defcustom emms-mode-line-cycle-current-title-function
  (lambda () (emms-track-description
          (emms-playlist-current-selected-track)))
  "Getter function for the current track title.
Its function returns a stirng."
  :type 'function)

(defvar emms-mode-line-cycle) ; Suppress a warning message
(defvar emms-mode-line-icon-before-format)
(defvar emms-mode-line-icon-image-cache)

(defvar emms-mode-line-cycle--title ""
  "The current track title.")

(defvar emms-mode-line-cycle--title-width 0
  "Width of the current track title via `string-width'.")

(defvar emms-mode-line-cycle--title-queue nil
  "Queue for the current track title.")

(defun emms-mode-line-cycle--substring (str &optional width)
  "Substring STR with `emms-mode-line-cycle-max-width'.
WIDTH is string width."
  (truncate-string-to-width
   str (or width emms-mode-line-cycle-max-width) 0 ? ))

(defun emms-mode-line-cycle--make-title-queue (title)
  "Return a queue of TITLE."
  (let* ((len (string-width title))
         (char-ls
          (if (<= len emms-mode-line-cycle-max-width)
              (string-to-list title)
            (let ((emms-mode-line-cycle-additional-space-num
                   (if (< emms-mode-line-cycle-additional-space-num 1)
                       1 emms-mode-line-cycle-additional-space-num)))
             (cons ? (nconc (string-to-list title)
                            (make-list (1- emms-mode-line-cycle-additional-space-num) ? ))))))
         (queue (cons nil nil)))
    (setcar queue (last (setcdr queue char-ls)))
    queue))

(defun emms-mode-line-cycle--rotate-queue (queue)
  "Rotate QUEUE."
  (let ((head (cadr queue)))
    (setcdr queue (cddr queue))
    (when (null (cdr queue)) (setcar queue queue))
    (setcar queue (setcdr (car queue) (cons head nil)))
    queue))

(defun emms-mode-line-cycle--set-title-queue (title)
  "Set TITLE to `emms-mode-line-cycle--title-queue'."
  (setq emms-mode-line-cycle--title-queue
        (emms-mode-line-cycle--make-title-queue title)))

(defun emms-mode-line-cycle--rotate-title-queue ()
  "Rotate `emms-mode-line-cycle--title-queue'."
  (setq emms-mode-line-cycle--title-queue
        (emms-mode-line-cycle--rotate-queue emms-mode-line-cycle--title-queue)))

(defun emms-mode-line-cycle--initialize (title)
  "Initialize emms-mode-line-cycle's global variables to the TITLE."
  (setq emms-mode-line-cycle--title title
        emms-mode-line-cycle--title-width (string-width title))
  (emms-mode-line-cycle--set-title-queue title))

(defun emms-mode-line-cycle--playlist-current (&optional title initialp)
  "Format the current track TITLE like `emms-mode-line-playlist-current'.
If INITIALP is no-nil, initialized."
  (when initialp
    (emms-mode-line-cycle--initialize
     (or title (funcall emms-mode-line-cycle-current-title-function))))
  (format emms-mode-line-format
          (if (> emms-mode-line-cycle--title-width emms-mode-line-cycle-max-width)
              (emms-mode-line-cycle--substring
               (if initialp  emms-mode-line-cycle--title
                 (apply #'string (cdr emms-mode-line-cycle--title-queue))))
            emms-mode-line-cycle--title)))

(defun emms-mode-line-cycle--icon-function (&optional title initialp)
  "Format the current track TITLE like `emms-mode-line-icon-function'.
If INITIALP is no-nil, initialized."
  (concat " "
          emms-mode-line-icon-before-format
          (emms-propertize "NP:" 'display emms-mode-line-icon-image-cache)
          (emms-mode-line-cycle--playlist-current title initialp)))

(defun emms-mode-line-cycle-mode-line-function (&optional title)
  "This is used as `emms-mode-line-mode-line-function'.
If TITLE is no-nil, it is set to emms-mode-line-cycle's global variables."
  (if emms-mode-line-cycle-use-icon-p
      (emms-mode-line-cycle--icon-function title t)
    (emms-mode-line-cycle--playlist-current title t)))

(defun emms-mode-line-cycle-update-mode-line-string ()
  "Update `emms-mode-line-string', if `emms-mode-line-cycle' is non-nil."
  (when (and emms-mode-line-cycle
             (> emms-mode-line-cycle--title-width emms-mode-line-cycle-max-width))
    (emms-mode-line-cycle--rotate-title-queue)
    (setq emms-mode-line-string
          (if emms-mode-line-cycle-use-icon-p
              (emms-mode-line-cycle--icon-function)
            (emms-mode-line-cycle--playlist-current)))))

;;;###autoload
(define-minor-mode emms-mode-line-cycle
  "Update `emms-mode-line-string' cyclically with `emms-playing-time-display'."
  :global t
  (if emms-mode-line-cycle
      (progn
        (unless (eq emms-mode-line-mode-line-function
                    'emms-mode-line-cycle-mode-line-function)
          (put 'emms-mode-line-cycle-mode-line-function
               :default-mode-line-function
               emms-mode-line-mode-line-function)
          (setq emms-mode-line-mode-line-function
                'emms-mode-line-cycle-mode-line-function))
        (advice-add 'emms-playing-time-display :before
                    #'emms-mode-line-cycle-update-mode-line-string))
    (when (eq emms-mode-line-mode-line-function
              'emms-mode-line-cycle-mode-line-function)
      (setq emms-mode-line-mode-line-function
            (get 'emms-mode-line-cycle-mode-line-function
                 :default-mode-line-function))
      (put 'emms-mode-line-cycle-mode-line-function :default-mode-line-function
           nil))
    (advice-remove 'emms-playing-time-display
                   #'emms-mode-line-cycle-update-mode-line-string)))

(provide 'emms-mode-line-cycle)
;;; emms-mode-line-cycle.el ends here
