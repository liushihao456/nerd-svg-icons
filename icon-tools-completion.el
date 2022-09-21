;;; icon-tools-completion.el --- Completion with icon-tools  -*- lexical-binding: t; -*-

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
;; Add icons to minibuffer candidates.  Compared to all-the-icons, it look
;; better with perfect alignment and size.  It renders SVG icons in GUI and nerd
;; icons in TUI.
;; --------------------------------------

;;; Usage:
;;
;; --------------------------------------

;;; Code:
(require 'icon-tools)

(defgroup all-the-icons-completion nil
  "Add icons to completion candidates."
  :group 'appearance
  :prefix "all-the-icons-completion")

(defvar icon-tools-completion-icon-right-padding 1
  "Padding added to the right of completion icons.")

(defun icon-tools-completion-get-icon (cand cat)
  "Return the icon for the candidate CAND of completion category CAT."
  (cl-case cat
    (file (icon-tools-completion-get-file-icon cand))
    (project-file (icon-tools-completion-get-file-icon cand))
    (buffer (icon-tools-completion-get-buffer-icon cand))
    (face (icon-tools-completion-get-face-icon cand))
    (t "")))

(defun icon-tools-completion-get-file-icon (cand)
  "Return the icon for the candidate CAND of completion category file."
  (cond ((string-match-p "\\/$" cand)
         (concat (icon-tools-icon-for-dir cand)
                 (make-string icon-tools-completion-icon-right-padding ?\s)))
        (t (concat (icon-tools-icon-for-file cand)
                   (make-string icon-tools-completion-icon-right-padding ?\s)))))

(defun icon-tools-completion-get-buffer-icon (cand)
  "Return the icon for the candidate CAND of completion category buffer."
  (concat
   (or
    (icon-tools-icon-for-str cand)
    (icon-tools-icon-for-mode (buffer-local-value 'major-mode (get-buffer cand))))
   (make-string icon-tools-completion-icon-right-padding ?\s)))

(defun icon-tools-completion-get-face-icon (cand)
  "Return the icon for the candidate CAND of completion category face."
  (concat
   (icon-tools-icon-str "color" (if (stringp cand) (intern cand) cand))
   (make-string icon-tools-completion-icon-right-padding ?\s)))

(defun icon-tools-completion-completion-metadata-get (orig metadata prop)
  "Meant as :around advice for `completion-metadata-get', Add icons as prefix.
ORIG should be `completion-metadata-get'
METADATA is the metadata.
PROP is the property which is looked up."
  (if (eq prop 'affixation-function)
      (let ((cat (funcall orig metadata 'category))
            (aff (or (funcall orig metadata 'affixation-function)
                     (when-let ((ann (funcall orig metadata 'annotation-function)))
                       (lambda (cands)
                         (mapcar (lambda (x) (list x "" (funcall ann x))) cands))))))
        (cond
         ((and (eq cat 'multi-category) aff)
          (lambda (cands)
            (mapcar (lambda (x)
                      (pcase-exhaustive x
                        (`(,cand ,prefix ,suffix)
                         (let ((orig (get-text-property 0 'multi-category cand)))
                           (list cand
                                 (concat (icon-tools-completion-get-icon (cdr orig) (car orig))
                                         prefix)
                                 suffix)))))
                    (funcall aff cands))))
         ((and cat aff)
          (lambda (cands)
            (mapcar (lambda (x)
                      (pcase-exhaustive x
                        (`(,cand ,prefix ,suffix)
                         (list cand
                               (concat (icon-tools-completion-get-icon cand cat)
                                       prefix)
                               suffix))))
                    (funcall aff cands))))
         ((eq cat 'multi-category)
          (lambda (cands)
            (mapcar (lambda (x)
                      (let ((orig (get-text-property 0 'multi-category x)))
                        (list x (icon-tools-completion-get-icon (cdr orig) (car orig)) "")))
                    cands)))
         (cat
          (lambda (cands)
            (mapcar (lambda (x)
                      (list x (icon-tools-completion-get-icon x cat) ""))
                    cands)))
         (aff)))
    (funcall orig metadata prop)))

(defvar icon-tools-completion--marginalia-old-offset 0)

;;;###autoload
(define-minor-mode icon-tools-completion-mode
  "Add icons to completion candidates."
  :global t
  (if icon-tools-completion-mode
      (progn
        (when (boundp 'marginalia-align-offset)
          (setq icon-tools-completion--marginalia-old-offset marginalia-align-offset)
          (setq marginalia-align-offset
                (+ icon-tools-completion--marginalia-old-offset
                   (+ icon-tools-icon-width icon-tools-completion-icon-right-padding))))
        (advice-add #'completion-metadata-get :around #'icon-tools-completion-completion-metadata-get))
    (advice-remove #'completion-metadata-get #'icon-tools-completion-completion-metadata-get)
    (when (boundp 'marginalia-align-offset)
      (setq marginalia-align-offset icon-tools-completion--marginalia-old-offset))))

;; For the byte compiler
(defvar marginalia-mode)
;;;###autoload
(defun icon-tools-completion-marginalia-setup ()
  "Hook to `marginalia-mode-hook' to bind `icon-tools-completion-mode' to it."
  (icon-tools-completion-mode (if marginalia-mode 1 -1)))

(provide 'icon-tools-completion)

;;; icon-tools-completion.el ends here
