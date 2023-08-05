;;; nerd-svg-icons-dired.el --- Dired theme with nerd-svg-icons  -*- lexical-binding: t; -*-

;; Author: Shihao Liu
;; Keywords: icon
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.3"))

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; A Dired theme with svg icons that look better with perfect alignment and
;; size.  It renders SVG icons in GUI and nerd icons in TUI.
;; --------------------------------------

;;; Usage:
;;
;; --------------------------------------

;;; Code:
(require 'nerd-svg-icons)
(require 'dired)

(defun nerd-svg-icons-dired--add-overlay (pos string)
  "Add overlay to display STRING at POS."
  (let ((ov (make-overlay (1- pos) pos)))
    (overlay-put ov 'nerd-svg-icons-dired-overlay t)
    (overlay-put ov 'after-string string)))

(defun nerd-svg-icons-dired--overlays-in (beg end)
  "Get all nerd-svg-icons-dired overlays between BEG to END."
  (cl-remove-if-not
   (lambda (ov)
     (overlay-get ov 'nerd-svg-icons-dired-overlay))
   (overlays-in beg end)))

(defun nerd-svg-icons-dired--overlays-at (pos)
  "Get nerd-svg-icons-dired overlays at POS."
  (apply #'nerd-svg-icons-dired--overlays-in `(,pos ,pos)))

(defun nerd-svg-icons-dired--remove-all-overlays ()
  "Remove all `nerd-svg-icons-dired' overlays."
  (save-restriction
    (widen)
    (mapc #'delete-overlay
          (nerd-svg-icons-dired--overlays-in (point-min) (point-max)))))

(defun nerd-svg-icons-dired--refresh (&rest _)
  "Display the icons of files in a Dired buffer."
  (nerd-svg-icons-dired--remove-all-overlays)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when (dired-move-to-filename nil)
        (let ((file (dired-get-filename 'relative 'noerror)))
          (when file
            (let ((icon (if (file-directory-p file)
                            (nerd-svg-icons-icon-for-dir file)
                          (nerd-svg-icons-icon-for-file file))))
              (if (member file '("." ".."))
                  (nerd-svg-icons-dired--add-overlay
                   (point) (concat (make-string nerd-svg-icons-icon-width ?\s) " "))
                (nerd-svg-icons-dired--add-overlay (point) (concat icon " ")))))))
      (forward-line 1))))

(defvar nerd-svg-icons-dired-mode)

;;;###autoload
(define-minor-mode nerd-svg-icons-dired-mode
  "Display nerd-svg-icons icon for each files in a Dired buffer."
  :global nil
  (when (derived-mode-p 'dired-mode)
    (if nerd-svg-icons-dired-mode
        (progn
          (advice-add #'dired-readin :after #'nerd-svg-icons-dired--refresh)
          (advice-add #'dired-revert :after #'nerd-svg-icons-dired--refresh)
          (advice-add #'dired-internal-do-deletions :after #'nerd-svg-icons-dired--refresh)
          (advice-add #'dired-insert-subdir :after #'nerd-svg-icons-dired--refresh)
          (advice-add #'dired-do-kill-lines :after #'nerd-svg-icons-dired--refresh)
          (nerd-svg-icons-dired--refresh))
      (advice-remove #'dired-readin #'nerd-svg-icons-dired--refresh)
      (advice-remove #'dired-revert #'nerd-svg-icons-dired--refresh)
      (advice-remove #'dired-internal-do-deletions #'nerd-svg-icons-dired--refresh)
      (advice-remove #'dired-insert-subdir #'nerd-svg-icons-dired--refresh)
      (advice-remove #'dired-do-kill-lines #'nerd-svg-icons-dired--refresh)
      (nerd-svg-icons-dired--remove-all-overlays))))

(provide 'nerd-svg-icons-dired)

;;; nerd-svg-icons-dired.el ends here
