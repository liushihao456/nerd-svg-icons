;;; nerd-svg-icons-treemacs-icons.el --- Treemacs theme with nerd-svg-icons  -*- lexical-binding: t; -*-

;; Author: Shihao Liu
;; Keywords: treemacs icon
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
;; A treemacs theme with svg icons that looks better with perfect alignment and
;; size.  It renders SVG icons in GUI and nerd icons in TUI.
;; --------------------------------------

;;; Usage:
;;
;; --------------------------------------

;;; Code:
(require 'nerd-svg-icons)
(require 'treemacs)

(with-eval-after-load 'treemacs
  (treemacs-create-theme "nerd-svg-icons-treemacs-icons"
    :config
    ;; The leading spaces must be propertized, as treemacs will wrap the icon string
    ;; as property around a blank string, shadowing the second-level svg property.
    ;;
    ;; See `treemacs-create-icon'.
    (let ((indent-str (propertize
                       (make-string nerd-svg-icons-icon-width ?\s)
                       'display (make-string nerd-svg-icons-icon-width ?\s))))
      ;; repo
      (treemacs-create-icon
       :icon (format "%s\t" (nerd-svg-icons-icon-str "oct-repo" :face 'treemacs-term-node-face :scale 1.3))
       :fallback 'same-as-icon
       :extensions (root-open))
      (treemacs-create-icon
       :icon (format "%s\t" (nerd-svg-icons-icon-str "oct-repo" :face 'treemacs-term-node-face :scale 1.3))
       :fallback 'same-as-icon
       :extensions (root-closed))

      ;; folder
      (treemacs-create-icon
       :icon (format "%s%s\t"
                     (nerd-svg-icons-icon-str "oct-chevron_down" :face 'font-lock-doc-face)
                     (nerd-svg-icons-icon-str "md-folder_open" :face 'font-lock-doc-face))
       :fallback 'same-as-icon
       :extensions (dir-open))
      (treemacs-create-icon
       :icon (format "%s%s\t"
                     (nerd-svg-icons-icon-str "oct-chevron_right" :face 'font-lock-doc-face)
                     (nerd-svg-icons-icon-str "md-folder" :face 'font-lock-doc-face))
       :fallback 'same-as-icon
       :extensions (dir-closed))

      (treemacs-create-icon
       :icon (format "%s%s\t"
                     (nerd-svg-icons-icon-str "oct-chevron_down" :face 'font-lock-function-name-face)
                     (nerd-svg-icons-icon-str "cod-package" :face 'font-lock-function-name-face))
       :fallback 'same-as-icon
       :extensions (tag-open))
      (treemacs-create-icon
       :icon (format "%s%s\t"
                     (nerd-svg-icons-icon-str "oct-chevron_right" :face 'font-lock-function-name-face)
                     (nerd-svg-icons-icon-str "cod-package" :face 'font-lock-function-name-face))
       :fallback 'same-as-icon
       :extensions (tag-closed))

      ;; tag
      (treemacs-create-icon
       :icon (format "%s%s\t"
                     indent-str
                     (nerd-svg-icons-icon-str "fa-tag" :face 'nerd-svg-icons-purple))
       :fallback 'same-as-icon
       :extensions (tag-leaf))

      ;; errors
      (treemacs-create-icon
       :icon (format "%s\t" (nerd-svg-icons-icon-str "cod-error" :face 'font-lock-warning-face))
       :fallback 'same-as-icon
       :extensions (error))
      (treemacs-create-icon
       :icon (format "%s\t" (nerd-svg-icons-icon-str "cod-warning" :face 'nerd-svg-icons-yellow))
       :fallback 'same-as-icon
       :extensions (warning))
      (treemacs-create-icon
       :icon (format "%s\t" (nerd-svg-icons-icon-str "cod-info" :face 'nerd-svg-icons-green))
       :fallback 'same-as-icon
       :extensions (info))

      (dolist (item nerd-svg-icons-extension-icon-alist)
        (let* ((extension (car item))
               (icon-name (cadr item))
               (face (caddr item))
               (gui-icon (format "%s%s\t"
                                 indent-str
                                 (nerd-svg-icons-icon-str icon-name :face face)))
               (tui-icon (format "%s%s\t"
                                 indent-str
                                 (nerd-svg-icons-nerd-icon-str icon-name :face face))))
          (let* ((icon-pair (cons gui-icon tui-icon))
                 (gui-icons (treemacs-theme->gui-icons treemacs--current-theme))
                 (tui-icons (treemacs-theme->tui-icons treemacs--current-theme))
                 (gui-icon  (car icon-pair))
                 (tui-icon  (cdr icon-pair)))
            (ht-set! gui-icons extension gui-icon)
            (ht-set! tui-icons extension tui-icon))))

      ;; fallback
      (treemacs-create-icon
       :icon (format "%s%s\t"
                     indent-str
                     (nerd-svg-icons-icon-str "oct-file" :face 'nerd-svg-icons-cyan))
       :fallback 'same-as-icon
       :extensions (fallback))))

  (treemacs-load-theme "nerd-svg-icons-treemacs-icons"))

;;;###autoload
(defun nerd-svg-icons-treemacs-icons-config ()
  "Install nerd-svg-icons-treemacs-icons theme configuration.")

(provide 'nerd-svg-icons-treemacs-icons)

;;; nerd-svg-icons-treemacs-icons.el ends here
