;;; nerd-svg-icons-completion.el --- Completion with nerd-svg-icons  -*- lexical-binding: t; -*-

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
(require 'nerd-svg-icons)
(require 'bookmark)

(defgroup nerd-svg-icons-completion nil
  "Add icons to completion candidates."
  :group 'appearance
  :prefix "nerd-svg-icons-completion")

(defvar nerd-svg-icons-completion-icon-right-padding 1
  "Padding added to the right of completion icons.")

(defvar nerd-svg-icons-completion-category-icon-alist
  '((file . nerd-svg-icons-completion-get-file-icon)
    (command . nerd-svg-icons-completion-get-command-icon)
    (project-file . nerd-svg-icons-completion-get-file-icon)
    (buffer . nerd-svg-icons-completion-get-buffer-icon)
    (face . nerd-svg-icons-completion-get-face-icon)
    (bookmark . nerd-svg-icons-completion-get-bookmark-icon)
    (symbol . nerd-svg-icons-completion-get-symbol-icon)
    (function . nerd-svg-icons-completion-get-symbol-icon)
    (variable . nerd-svg-icons-completion-get-symbol-icon)
    (imenu . nerd-svg-icons-completion-get-imenu-icon)
    (library . nerd-svg-icons-completion-get-package-icon)
    (package . nerd-svg-icons-completion-get-package-icon)
    (embark-keybinding . nerd-svg-icons-completion-get-embark-keybinding-icon)
    (customize-group . nerd-svg-icons-completion-get-customize-group-icon)
    (minor-mode . nerd-svg-icons-completion-get-minor-mode-icon)))

(defun nerd-svg-icons-completion-get-icon (cand cat)
  "Return the icon for the candidate CAND of completion category CAT."
  (if-let (fun (alist-get cat nerd-svg-icons-completion-category-icon-alist))
      (funcall fun cand)
    ""))

(defun nerd-svg-icons-completion-get-file-icon (cand)
  "Return the icon for the candidate CAND of completion category file."
  (concat
   (cond ((string-match-p "\\/$" cand)
          (nerd-svg-icons-icon-for-dir cand))
         (t (or
             (nerd-svg-icons-icon-for-file cand)
             (make-string nerd-svg-icons-icon-width ?\s))))
   (make-string nerd-svg-icons-completion-icon-right-padding ?\s)))

(defun nerd-svg-icons-completion-get-command-icon (cand)
  "Return the icon for the candidate CAND of completion category command."
  (concat
   (nerd-svg-icons-icon-for-symbol-kind "command")
   (make-string nerd-svg-icons-completion-icon-right-padding ?\s)))

(defun nerd-svg-icons-completion-get-buffer-icon (cand)
  "Return the icon for the candidate CAND of completion category buffer."
  (concat
   (or
    (nerd-svg-icons-icon-for-str cand)
    (nerd-svg-icons-icon-for-mode (buffer-local-value 'major-mode (get-buffer cand))))
   (make-string nerd-svg-icons-completion-icon-right-padding ?\s)))

(defun nerd-svg-icons-completion-get-face-icon (cand)
  "Return the icon for the candidate CAND of completion category face."
  (concat
   (nerd-svg-icons-icon-for-symbol-kind "face" :face (intern-soft cand))
   (make-string nerd-svg-icons-completion-icon-right-padding ?\s)))

(defun nerd-svg-icons-completion-get-bookmark-icon (cand)
  "Return the icon for the candidate CAND of completion category bookmark."
  (if-let ((bm (assoc cand (bound-and-true-p bookmark-alist))))
      (nerd-svg-icons-completion-get-file-icon (bookmark-get-filename bm))
    (nerd-svg-icons-icon-str "fa-bookmark" :face 'nerd-svg-icons-orange)))

(defun nerd-svg-icons-completion-get-symbol-icon (cand)
  "Return the icon for the candidate CAND of completion category symbol."
  (let* ((s (intern-soft cand))
         (kind (cond
                ((commandp s) "command")
                ((macrop (symbol-function s)) "macro")
                ((fboundp s) "function")
                ((facep s) "face")
                ((and (boundp s) (custom-variable-p s)) "custom")
                ((and (boundp s) (local-variable-if-set-p s)) "local")
                ((boundp s) "variable")
                (t "unknown"))))
    (concat
     (nerd-svg-icons-icon-for-symbol-kind kind)
     (make-string nerd-svg-icons-completion-icon-right-padding ?\s))))

(defun nerd-svg-icons-completion-get-imenu-icon (cand)
  "Return the icon for the candidate CAND of completion category imenu."
  (concat
   (nerd-svg-icons-icon-for-symbol-kind (get-text-property 0 'kind cand))
   (make-string nerd-svg-icons-completion-icon-right-padding ?\s)))

(defun nerd-svg-icons-completion-get-package-icon (cand)
  "Return the icon for the candidate CAND of completion category package."
  (concat
   (nerd-svg-icons-icon-str "cod-package" :face 'nerd-svg-icons-lpurple)
   (make-string nerd-svg-icons-completion-icon-right-padding ?\s)))

(defun nerd-svg-icons-completion-get-embark-keybinding-icon (cand)
  "Return the icon for the candidate CAND of completion category embark-keybinding."
  (concat
   (nerd-svg-icons-icon-str "cod-key" :face 'nerd-svg-icons-cyan)
   (make-string nerd-svg-icons-completion-icon-right-padding ?\s)))

(defun nerd-svg-icons-completion-get-customize-group-icon (cand)
  "Return the icon for the candidate CAND of completion category `customize-group'."
  (concat
   (nerd-svg-icons-icon-str "seti-settings" :face 'nerd-svg-icons-orange)
   (make-string nerd-svg-icons-completion-icon-right-padding ?\s)))

(defun nerd-svg-icons-completion-get-minor-mode-icon (cand)
  "Return the icon for the candidate CAND of completion category minor-mode."
  (concat
   (nerd-svg-icons-icon-str "md-cogs" :face 'nerd-svg-icons-dcyan)
   (make-string nerd-svg-icons-completion-icon-right-padding ?\s)))

(defun nerd-svg-icons-completion-completion-metadata-get (orig metadata prop)
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
                                 (concat (nerd-svg-icons-completion-get-icon (cdr orig) (car orig))
                                         prefix)
                                 suffix)))))
                    (funcall aff cands))))
         ((and cat aff)
          (lambda (cands)
            (mapcar (lambda (x)
                      (pcase-exhaustive x
                        (`(,cand ,prefix ,suffix)
                         (list cand
                               (concat (nerd-svg-icons-completion-get-icon cand cat)
                                       prefix)
                               suffix))))
                    (funcall aff cands))))
         ((eq cat 'multi-category)
          (lambda (cands)
            (mapcar (lambda (x)
                      (let ((orig (get-text-property 0 'multi-category x)))
                        (list x (nerd-svg-icons-completion-get-icon (cdr orig) (car orig)) "")))
                    cands)))
         (cat
          (lambda (cands)
            (mapcar (lambda (x)
                      (list x (nerd-svg-icons-completion-get-icon x cat) ""))
                    cands)))
         (aff)))
    (funcall orig metadata prop)))

(defvar nerd-svg-icons-completion--marginalia-old-offset 0)

;;;###autoload
(define-minor-mode nerd-svg-icons-completion-mode
  "Add icons to completion candidates."
  :global t
  (if nerd-svg-icons-completion-mode
      (progn
        (when (boundp 'marginalia-align-offset)
          (setq nerd-svg-icons-completion--marginalia-old-offset marginalia-align-offset)
          (setq marginalia-align-offset
                (+ nerd-svg-icons-completion--marginalia-old-offset
                   (+ nerd-svg-icons-icon-width nerd-svg-icons-completion-icon-right-padding))))
        (advice-add #'completion-metadata-get :around #'nerd-svg-icons-completion-completion-metadata-get))
    (advice-remove #'completion-metadata-get #'nerd-svg-icons-completion-completion-metadata-get)
    (when (boundp 'marginalia-align-offset)
      (setq marginalia-align-offset nerd-svg-icons-completion--marginalia-old-offset))))

;; For the byte compiler
(defvar marginalia-mode)
;;;###autoload
(defun nerd-svg-icons-completion-marginalia-setup ()
  "Hook to `marginalia-mode-hook' to bind `nerd-svg-icons-completion-mode' to it."
  (nerd-svg-icons-completion-mode (if marginalia-mode 1 -1)))

(provide 'nerd-svg-icons-completion)

;;; nerd-svg-icons-completion.el ends here
