;;; icon-tools-treemacs-icons.el --- Treemacs theme with icon-tools  -*- lexical-binding: t; -*-

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
(require 'treemacs)

(with-eval-after-load 'treemacs
  (treemacs-create-theme "icon-tools-treemacs-icons"
    :config
    ;; The leading spaces must be propertized, as treemacs will wrap the icon string
    ;; as property around a blank string, shadowing the second-level svg property.
    ;;
    ;; See `treemacs-create-icon'.
    (let ((indent-str (propertize
                       (make-string icon-tools-icon-width ?\s)
                       'display (make-string icon-tools-icon-width ?\s))))
      ;; repo
      (treemacs-create-icon
       :icon (format "%s\t" (icon-tools-icon-str "repo" 'treemacs-term-node-face))
       :fallback 'same-as-icon
       :extensions (root-open))
      (treemacs-create-icon
       :icon (format "%s\t" (icon-tools-icon-str "repo" 'treemacs-term-node-face))
       :fallback 'same-as-icon
       :extensions (root-closed))

      ;; folder
      (treemacs-create-icon
       :icon (format "%s%s\t"
                     (icon-tools-icon-str "chevron-down" 'font-lock-doc-face)
                     (icon-tools-icon-str "file-directory" 'font-lock-doc-face))
       :fallback 'same-as-icon
       :extensions (dir-open))
      (treemacs-create-icon
       :icon (format "%s%s\t"
                     (icon-tools-icon-str "chevron-right" 'font-lock-doc-face)
                     (icon-tools-icon-str "file-directory" 'font-lock-doc-face))
       :fallback 'same-as-icon
       :extensions (dir-closed))

      (treemacs-create-icon
       :icon (format "%s%s\t"
                     (icon-tools-icon-str "chevron-down" 'font-lock-function-name-face)
                     (icon-tools-icon-str "package" 'font-lock-function-name-face))
       :fallback 'same-as-icon
       :extensions (tag-open))
      (treemacs-create-icon
       :icon (format "%s%s\t"
                     (icon-tools-icon-str "chevron-right" 'font-lock-function-name-face)
                     (icon-tools-icon-str "package" 'font-lock-function-name-face))
       :fallback 'same-as-icon
       :extensions (tag-closed))

      ;; tag
      (treemacs-create-icon
       :icon (format "%s%s\t"
                     indent-str
                     (icon-tools-icon-str "tag" 'icon-tools-purple))
       :fallback 'same-as-icon
       :extensions (tag-leaf))

      ;; errors
      (treemacs-create-icon
       :icon (format "%s\t" (icon-tools-icon-str "error" 'font-lock-warning-face))
       :fallback 'same-as-icon
       :extensions (error))
      (treemacs-create-icon
       :icon (format "%s\t" (icon-tools-icon-str "warning" 'icon-tools-yellow))
       :fallback 'same-as-icon
       :extensions (warning))
      (treemacs-create-icon
       :icon (format "%s\t" (icon-tools-icon-str "info" 'icon-tools-green))
       :fallback 'same-as-icon
       :extensions (info))

      (dolist (item icon-tools-extension-icon-alist)
        (let* ((extension (car item))
               (icon-name (cadr item))
               (face (caddr item))
               (gui-icon (format "%s%s\t"
                                 indent-str
                                 (icon-tools-icon-str icon-name face)))
               (tui-icon (format "%s%s\t"
                                 indent-str
                                 (icon-tools-nerd-icon-str icon-name face))))
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
                     (icon-tools-icon-str "file" 'icon-tools-cyan))
       :fallback 'same-as-icon
       :extensions (fallback))))

  (treemacs-load-theme "icon-tools-treemacs-icons"))

;;;###autoload
(defun icon-tools-treemacs-icons-config ()
  "Install icon-tools-treemacs-icons theme configuration.")

(provide 'icon-tools-treemacs-icons)

;;; icon-tools-treemacs-icons.el ends here
