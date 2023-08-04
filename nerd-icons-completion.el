;;; nerd-icons-completion.el --- Completion with nerd-icons  -*- lexical-binding: t; -*-

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
(require 'nerd-icons)
(require 'bookmark)

(defgroup nerd-icons-completion nil
  "Add icons to completion candidates."
  :group 'appearance
  :prefix "nerd-icons-completion")

(defvar nerd-icons-completion-icon-right-padding 1
  "Padding added to the right of completion icons.")

(defvar nerd-icons-completion-category-icon-alist
  '((file . nerd-icons-completion-get-file-icon)
    (command . nerd-icons-completion-get-command-icon)
    (project-file . nerd-icons-completion-get-file-icon)
    (buffer . nerd-icons-completion-get-buffer-icon)
    (face . nerd-icons-completion-get-face-icon)
    (bookmark . nerd-icons-completion-get-bookmark-icon)
    (symbol . nerd-icons-completion-get-symbol-icon)
    (function . nerd-icons-completion-get-symbol-icon)
    (variable . nerd-icons-completion-get-variable-icon)
    (imenu . nerd-icons-completion-get-imenu-icon)
    (library . nerd-icons-completion-get-package-icon)
    (package . nerd-icons-completion-get-package-icon)
    (embark-keybinding . nerd-icons-completion-get-embark-keybinding-icon)
    (customize-group . nerd-icons-completion-get-customize-group-icon)
    (minor-mode . nerd-icons-completion-get-minor-mode-icon)))

(defun nerd-icons-completion-get-icon (cand cat)
  "Return the icon for the candidate CAND of completion category CAT."
  (if-let (fun (alist-get cat nerd-icons-completion-category-icon-alist))
      (funcall fun cand)
    ""))

(defun nerd-icons-completion-get-file-icon (cand)
  "Return the icon for the candidate CAND of completion category file."
  (concat
   (cond ((string-match-p "\\/$" cand)
          (nerd-icons-icon-for-dir cand))
         (t (or
             (nerd-icons-icon-for-file cand)
             (make-string nerd-icons-icon-width ?\s))))
   (make-string nerd-icons-completion-icon-right-padding ?\s)))

(defun nerd-icons-completion-get-command-icon (cand)
  "Return the icon for the candidate CAND of completion category command."
  (concat
   (nerd-icons-icon-for-symbol-kind "command")
   (make-string nerd-icons-completion-icon-right-padding ?\s)))

(defun nerd-icons-completion-get-buffer-icon (cand)
  "Return the icon for the candidate CAND of completion category buffer."
  (concat
   (or
    (nerd-icons-icon-for-str cand)
    (nerd-icons-icon-for-mode (buffer-local-value 'major-mode (get-buffer cand))))
   (make-string nerd-icons-completion-icon-right-padding ?\s)))

(defun nerd-icons-completion-get-face-icon (cand)
  "Return the icon for the candidate CAND of completion category face."
  (concat
   (nerd-icons-icon-for-symbol-kind "face" :face (intern-soft cand))
   (make-string nerd-icons-completion-icon-right-padding ?\s)))

(defun nerd-icons-completion-get-bookmark-icon (cand)
  "Return the icon for the candidate CAND of completion category bookmark."
  (if-let ((bm (assoc cand (bound-and-true-p bookmark-alist))))
      (nerd-icons-completion-get-file-icon (bookmark-get-filename bm))
    (nerd-icons-icon-str "fa-bookmark" :face 'nerd-icons-orange)))

(defun nerd-icons-completion-get-symbol-icon (cand)
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
     (nerd-icons-icon-for-symbol-kind kind)
     (make-string nerd-icons-completion-icon-right-padding ?\s))))

(defun nerd-icons-completion-get-variable-icon (cand)
  "Return the icon for the candidate CAND of completion category variable."
  (let ((s (intern-soft cand)))
    (concat
     (cond
      ((and (boundp s) (custom-variable-p s))
       (nerd-icons-icon-str "wrench" :face 'nerd-icons-orange))
      ((and (boundp s) (local-variable-if-set-p s))
       (nerd-icons-icon-str "variable-local" :face 'nerd-icons-blue))
      (t
       (nerd-icons-icon-str "variable" :face 'nerd-icons-blue)))
     (make-string nerd-icons-completion-icon-right-padding ?\s))))

(defun nerd-icons-completion-get-imenu-icon (cand)
  "Return the icon for the candidate CAND of completion category imenu."
  (concat
   (if-let (kind (get-text-property 0 'kind cand))
       (nerd-icons-icon-for-symbol-kind kind)
     (nerd-icons-icon-str "tag" :face 'nerd-icons-lpurple))
   (make-string nerd-icons-completion-icon-right-padding ?\s)))

(defun nerd-icons-completion-get-package-icon (cand)
  "Return the icon for the candidate CAND of completion category package."
  (concat
   (nerd-icons-icon-str "package" :face 'nerd-icons-lpurple)
   (make-string nerd-icons-completion-icon-right-padding ?\s)))

(defun nerd-icons-completion-get-embark-keybinding-icon (cand)
  "Return the icon for the candidate CAND of completion category embark-keybinding."
  (concat
   (nerd-icons-icon-str "key" :face 'nerd-icons-cyan)
   (make-string nerd-icons-completion-icon-right-padding ?\s)))

(defun nerd-icons-completion-get-customize-group-icon (cand)
  "Return the icon for the candidate CAND of completion category `customize-group'."
  (concat
   (nerd-icons-icon-str "wrench" :face 'nerd-icons-orange)
   (make-string nerd-icons-completion-icon-right-padding ?\s)))

(defun nerd-icons-completion-get-minor-mode-icon (cand)
  "Return the icon for the candidate CAND of completion category minor-mode."
  (concat
   (nerd-icons-icon-str "gear" :face 'nerd-icons-dcyan)
   (make-string nerd-icons-completion-icon-right-padding ?\s)))

(defun nerd-icons-completion-completion-metadata-get (orig metadata prop)
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
                                 (concat (nerd-icons-completion-get-icon (cdr orig) (car orig))
                                         prefix)
                                 suffix)))))
                    (funcall aff cands))))
         ((and cat aff)
          (lambda (cands)
            (mapcar (lambda (x)
                      (pcase-exhaustive x
                        (`(,cand ,prefix ,suffix)
                         (list cand
                               (concat (nerd-icons-completion-get-icon cand cat)
                                       prefix)
                               suffix))))
                    (funcall aff cands))))
         ((eq cat 'multi-category)
          (lambda (cands)
            (mapcar (lambda (x)
                      (let ((orig (get-text-property 0 'multi-category x)))
                        (list x (nerd-icons-completion-get-icon (cdr orig) (car orig)) "")))
                    cands)))
         (cat
          (lambda (cands)
            (mapcar (lambda (x)
                      (list x (nerd-icons-completion-get-icon x cat) ""))
                    cands)))
         (aff)))
    (funcall orig metadata prop)))

(defvar nerd-icons-completion--marginalia-old-offset 0)

;;;###autoload
(define-minor-mode nerd-icons-completion-mode
  "Add icons to completion candidates."
  :global t
  (if nerd-icons-completion-mode
      (progn
        (when (boundp 'marginalia-align-offset)
          (setq nerd-icons-completion--marginalia-old-offset marginalia-align-offset)
          (setq marginalia-align-offset
                (+ nerd-icons-completion--marginalia-old-offset
                   (+ nerd-icons-icon-width nerd-icons-completion-icon-right-padding))))
        (advice-add #'completion-metadata-get :around #'nerd-icons-completion-completion-metadata-get))
    (advice-remove #'completion-metadata-get #'nerd-icons-completion-completion-metadata-get)
    (when (boundp 'marginalia-align-offset)
      (setq marginalia-align-offset nerd-icons-completion--marginalia-old-offset))))

;; For the byte compiler
(defvar marginalia-mode)
;;;###autoload
(defun nerd-icons-completion-marginalia-setup ()
  "Hook to `marginalia-mode-hook' to bind `nerd-icons-completion-mode' to it."
  (nerd-icons-completion-mode (if marginalia-mode 1 -1)))

(provide 'nerd-icons-completion)

;;; nerd-icons-completion.el ends here
