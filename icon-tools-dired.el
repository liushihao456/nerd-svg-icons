;;; icon-tools-dired.el --- Dired theme with icon-tools  -*- lexical-binding: t; -*-

;; Author: Shihao Liu
;; Keywords: treemacs svg icon
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.3") (treemacs "2.9.5"))

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
;; A treemacs theme with svg icons that look better with perfect alignment and
;; size.  It renders SVG icons in GUI and nerd icons in TUI.
;; --------------------------------------

;;; Usage:
;;
;; --------------------------------------

;;; Code:
(require 'icon-tools)
(require 'dired)

(defun icon-tools-dired--add-overlay (pos string)
  "Add overlay to display STRING at POS."
  (let ((ov (make-overlay (1- pos) pos)))
    (overlay-put ov 'icon-tools-dired-overlay t)
    (overlay-put ov 'after-string string)))

(defun icon-tools-dired--overlays-in (beg end)
  "Get all icon-tools-dired overlays between BEG to END."
  (cl-remove-if-not
   (lambda (ov)
     (overlay-get ov 'icon-tools-dired-overlay))
   (overlays-in beg end)))

(defun icon-tools-dired--overlays-at (pos)
  "Get icon-tools-dired overlays at POS."
  (apply #'icon-tools-dired--overlays-in `(,pos ,pos)))

(defun icon-tools-dired--remove-all-overlays ()
  "Remove all `icon-tools-dired' overlays."
  (save-restriction
    (widen)
    (mapc #'delete-overlay
          (icon-tools-dired--overlays-in (point-min) (point-max)))))

(defun icon-tools-dired--refresh ()
  "Display the icons of files in a Dired buffer."
  (icon-tools-dired--remove-all-overlays)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when (dired-move-to-filename nil)
        (let ((file (dired-get-filename 'relative 'noerror)))
          (when file
            (let ((icon (if (file-directory-p file)
                            (icon-tools-icon-for-dir file)
                          (icon-tools-icon-for-file file))))
              (if (member file '("." ".."))
                  (icon-tools-dired--add-overlay
                   (point) (concat (make-string icon-tools-icon-width ?\s) "\t"))
                (icon-tools-dired--add-overlay (point) (concat icon "\t")))))))
      (forward-line 1))))

(defvar icon-tools-dired-mode)

;;;###autoload
(define-minor-mode icon-tools-dired-mode
  "Display icon-tools icon for each files in a Dired buffer."
  :lighter " icon-tools-dired-mode"
  (when (and (derived-mode-p 'dired-mode))
    (if icon-tools-dired-mode
        (progn
          (setq-local tab-width 1)
          (advice-add #'dired-readin :after #'icon-tools-dired--refresh)
          (advice-add #'dired-revert :after #'icon-tools-dired--refresh)
          (advice-add #'dired-internal-do-deletions :after #'icon-tools-dired--refresh)
          (advice-add #'dired-insert-subdir :after #'icon-tools-dired--refresh)
          (advice-add #'dired-do-kill-lines :after #'icon-tools-dired--refresh)
          (icon-tools-dired--refresh))
      (kill-local-variable 'tab-width)
      (advice-remove #'dired-readin #'icon-tools-dired--refresh)
      (advice-remove #'dired-revert #'icon-tools-dired--refresh)
      (advice-remove #'dired-internal-do-deletions #'icon-tools-dired--refresh)
      (advice-remove #'dired-insert-subdir #'icon-tools-dired--refresh)
      (advice-remove #'dired-do-kill-lines #'icon-tools-dired--refresh)
      (icon-tools-dired--remove-all-overlays))))

(provide 'icon-tools-dired)

;;; icon-tools-dired.el ends here
