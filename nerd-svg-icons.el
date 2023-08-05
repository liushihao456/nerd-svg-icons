;;; nerd-svg-icons.el --- Tools for icons in Emacs  -*- lexical-binding: t; -*-

;; Author: Shihao Liu
;; Keywords: icon svg nerd
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
;; Tools for creating icons in Emacs that supports both GUI and TUI.
;; -----------------------------------------------------------------

;;; Usage:
;;
;; --------------------------------------

;;; Code:
(require 'xml)
(require 'svg)
(require 'color)
(require 'nerd-svg-icons-faces)
(require 'nerd-svg-icons-data-nerd)

(defgroup nerd-svg-icons nil
  "Group for nerd-svg-icons."
  :group 'nerd-svg-icons)

(defvar nerd-svg-icons-svg-icon-dir
  (expand-file-name "svg" (file-name-directory load-file-name)))

(defvar nerd-svg-icons-icon-width 2)

(defvar nerd-svg-icons-svg-icon-cache
  (make-hash-table :test 'equal :size 250))

(defun nerd-svg-icons-svg-icon-cache-add (icon icon-name &rest args)
  (puthash (format "%s-%s-%s-%d-%d"
                   icon-name
                   (symbol-name (or (plist-get args :face) 'default))
                   (or (plist-get args :scale) 1.0)
                   (window-font-width)
                   (window-font-height))
           icon nerd-svg-icons-svg-icon-cache))

(defun nerd-svg-icons-svg-icon-cache-get (icon-name &rest args)
  (gethash (format "%s-%s-%s-%d-%d"
                   icon-name
                   (symbol-name (or (plist-get args :face) 'default))
                   (or (plist-get args :scale) 1.0)
                   (window-font-width)
                   (window-font-height))
           nerd-svg-icons-svg-icon-cache))

(defun nerd-svg-icons-svg-icon-filepath (icon-name)
  (concat (file-name-as-directory nerd-svg-icons-svg-icon-dir)
          icon-name ".svg"))

(defun nerd-svg-icons-svg-icon-parse (icon-name)
  (with-temp-buffer
    (insert-file-contents (nerd-svg-icons-svg-icon-filepath icon-name))
    (xml-parse-region (point-min) (point-max))))

(defun nerd-svg-icons--svg-icon-emacs-color-to-svg-color (color-name)
  "Convert Emacs COLOR-NAME to #rrggbb form.
If COLOR-NAME is unknown to Emacs, then return COLOR-NAME as-is."
  (let ((rgb-color (color-name-to-rgb color-name)))
    (if rgb-color
        (apply #'color-rgb-to-hex (append rgb-color '(2)))
      color-name)))

(defun nerd-svg-icons--svg-icon-alist-to-keyword-plist (alist)
  (cl-loop for (head . tail) in alist
           nconc (list (intern (concat ":" (symbol-name head))) tail)))

(defun nerd-svg-icons--svg-icon-recursively-copy-children (node1 node2 fg-color)
  (let ((children (xml-node-children node2)))
    (when (and node1 children)
      (dolist (child children)
        (when (listp child)
          (let ((attrs (xml-node-attributes child))
                (node1-child))
            (dolist (attr attrs)
              (when (string-equal (car attr) "fill")
                (setcdr attr fg-color))
              ;; (when (color-defined-p (cdr attr))
              ;;   (setcdr attr fg-color))
              )
            (setq node1-child
                  (apply 'svg-node
                         (append (list node1 (xml-node-name child))
                                 (nerd-svg-icons--svg-icon-alist-to-keyword-plist attrs))))
            (nerd-svg-icons--svg-icon-recursively-copy-children node1-child child fg-color)))))))

(defvar nerd-svg-icons-svg-icon-scale-alist
  '(("tag" . 0.8)
    ("key" . 0.8)
    ("tools" . 0.85)
    ("tex" . 1.2)
    ("java" . 1.2)
    ("database" . 0.8)
    ("typescript-badge" . 0.85)
    ("javascript-badge" . 0.92)
    ("jsx-alt" . 0.85)
    ("tsx-alt" . 0.85)
    ("file-directory" . 1.05)
    ("visualstudio" . 0.85)
    ("wrench" . 0.85)
    ("emacs" . 1.05)
    ("file" . 1.1)
    ("file-zip" . 1.05)
    ("film" . 0.9)
    ("symbol-parameter" . 1.15)
    ("closed_caption" . 1.15)
    ("variable-local" . 1.05)
    ("repo" . 1.1)))

(defvar nerd-svg-icons-svg-icon-base-scale 0.9)

(defun nerd-svg-icons--svg-icon-get-viewbox-multiplier (icon-name)
  (let ((cell (assoc icon-name nerd-svg-icons-svg-icon-scale-alist)))
    (if cell
        (/ 1 (* (cdr cell) nerd-svg-icons-svg-icon-base-scale))
      (/ 1 nerd-svg-icons-svg-icon-base-scale))))

(defun nerd-svg-icons--svg-icon-get-face-attribute-deep (face attribute)
  (when (facep face)
    (let ((face0 (face-attribute face :inherit))
          (val (face-attribute face attribute)))
      (while (and (facep face0) (eq val 'unspecified))
        (setq val (face-attribute face0 attribute))
        (setq face0 (face-attribute face0 :inherit)))
      val)))

(defun nerd-svg-icons-svg-icon (icon-name &rest args)
  "Build the icon ICON-NAME.

ARGS are additional plist arguments where properties FACE and
SCALE are supported.

Icon is drawn with the foreground of FACE and scaled with SCALE."

  (let ((cache-item (apply #'nerd-svg-icons-svg-icon-cache-get icon-name args)))
    (if cache-item
    ;; (if nil
        cache-item
      (let* ((face (plist-get args :face))
             (scale (plist-get args :scale))

             (root (nerd-svg-icons-svg-icon-parse icon-name))

             ;; Read original viewbox
             (viewbox-str (cdr (assq 'viewBox (xml-node-attributes (car root)))))
             (viewbox (when viewbox-str (mapcar 'string-to-number (split-string viewbox-str))))
             (view-x (if viewbox (nth 0 viewbox) 0))
             (view-y (if viewbox (nth 1 viewbox) 0))
             (view-width (if viewbox
                             (nth 2 viewbox)
                           (string-to-number (cdr (assq 'width (xml-node-attributes (car root)))))))
             (view-height (if viewbox
                              (nth 3 viewbox)
                            (string-to-number (cdr (assq 'height (xml-node-attributes (car root)))))))

             ;; Set icon size (in pixels) to `nerd-svg-icons-icon-width'x1 characters
             (svg-width  (* (window-font-width) nerd-svg-icons-icon-width))

             ;; Use 2 * (`window-font-width') instead, because on Windows, if
             ;; `window-font-height' returns value larger than 2 *
             ;; (`window-font-width'), the icon's height will actually be higher
             ;; than the original line height (which seems to be 2 *
             ;; (`window-font-width') no matter what `window-font-height'
             ;; returns).
             ;; ;; (svg-height (window-font-height)
             (svg-height (* (window-font-width) 2))

             ;; Scale by zooming in/out the svg viewbox
             (multiplier (if scale
                             (* (/ 1 scale)
                                (nerd-svg-icons--svg-icon-get-viewbox-multiplier icon-name))
                           (nerd-svg-icons--svg-icon-get-viewbox-multiplier icon-name)))
             (d-view-width (* (- multiplier 1) view-width))
             (view-x (- view-x (/ d-view-width 2)))
             (view-width (+ view-width d-view-width))
             (d-view-height (* (- multiplier 1) view-height))
             (view-y (- view-y (/ d-view-height 2)))
             (view-height (+ view-height d-view-height))

             (svg-viewbox (format "%f %f %f %f" view-x view-y view-width view-height))

             ;; Foreground and background
             (fg-color (nerd-svg-icons--svg-icon-get-face-attribute-deep face :foreground))
             (fg-color (nerd-svg-icons--svg-icon-emacs-color-to-svg-color
                        (or (when (facep fg-color)
                              (face-foreground fg-color nil t))
                            (when (not (eq fg-color 'unspecified)) fg-color)
                            (face-attribute 'default :foreground))))
             ;; Use only transparent background for now
             (bg-color "transparent")
             ;; (bg-color (nerd-svg-icons--svg-icon-get-face-attribute-deep face :background))
             ;; (bg-color (nerd-svg-icons--svg-icon-emacs-color-to-svg-color
             ;;            (or (when (facep bg-color)
             ;;                  (face-background bg-color nil t))
             ;;                (when (not (eq bg-color 'unspecified)) bg-color)
             ;;                "transparent")))

             (svg (svg-create svg-width svg-height
                              :viewBox svg-viewbox
                              :stroke-width 0
                              :fill fg-color)))

        (unless (equal bg-color "transparent")
          (svg-rectangle svg view-x view-y view-width view-height
                         :fill bg-color))

        ;; Insert all parsed nodes, replacing colors with fg-color
        (nerd-svg-icons--svg-icon-recursively-copy-children svg (car root) fg-color)

        (apply #'nerd-svg-icons-svg-icon-cache-add (svg-image svg :ascent 'center :scale 1)
               icon-name args)))))

(defun nerd-svg-icons--get-nerd-icon-glyph (icon-name)
  "Return the glyph of nerd icon ICON-NAME.

ICON-NAME is a string in the form of FAMILY-ICON, e.g. fa-book."
  (let* ((splits (split-string icon-name "-"))
         (family (car splits))
         (name (concat "nf-" family "-" (cadr splits)))
         (alist (pcase family
                  ("cod" nerd-svg-icons/codicon-alist)
                  ("dev" nerd-svg-icons/devicon-alist)
                  ("fa" nerd-svg-icons/faicon-alist)
                  ("fae" nerd-svg-icons/faicon-alist)
                  ("linux" nerd-svg-icons/flicon-alist)
                  ("iec" nerd-svg-icons/ipsicon-alist)
                  ("md" nerd-svg-icons/mdicon-alist)
                  ("oct" nerd-svg-icons/octicon-alist)
                  ("pom" nerd-svg-icons/pomicon-alist)
                  ("pl" nerd-svg-icons/powerline-alist)
                  ("ple" nerd-svg-icons/powerline-alist)
                  ("custom" nerd-svg-icons/sucicon-alist)
                  ("seti" nerd-svg-icons/sucicon-alist)
                  ("weather" nerd-svg-icons/wicon-alist))))
    (cdr (assoc name alist))))

(defun nerd-svg-icons-svg-icon-str (icon-name &rest args)
  "Return the svg icon as string.

ICON-NAME is a string in the form of FAMILY-ICON, e.g. fa-book.

ARGS are additional plist arguments where properties FACE and SCALE are
supported."
  (if-let* ((glyph (nerd-svg-icons--get-nerd-icon-glyph icon-name))
            (codepoint (format "%x" (string-to-char glyph))))
      (propertize
       (make-string nerd-svg-icons-icon-width ?\-)
       'display (apply #'nerd-svg-icons-svg-icon codepoint args))))

(defun nerd-svg-icons-nerd-icon-str (icon-name &rest args)
  "Return the nerd icon as string.

ICON-NAME is a string in the form of FAMILY-ICON, e.g. fa-book.

ARGS are additional plist arguments where properties FACE and SCALE are
supported."
  (when-let ((glyph (nerd-svg-icons--get-nerd-icon-glyph icon-name)))
    (propertize (concat glyph " ") 'face `(:foreground
                              ,(face-attribute
                                (or (plist-get args :face) 'default)
                                :foreground)))
    ))

(defun nerd-svg-icons-icon-str (icon-name &rest args)
  (if (display-graphic-p)
      (apply #'nerd-svg-icons-svg-icon-str icon-name args)
    (apply #'nerd-svg-icons-nerd-icon-str icon-name args)))

;; Icon alists --------------------------------------------------------------- ;

(defvar nerd-svg-icons-extension-icon-alist
  '(
    ("fish"                 "oct-terminal"              nerd-svg-icons-lpink)
    ("zsh"                  "oct-terminal"              nerd-svg-icons-lcyan)
    ("sh"                   "oct-terminal"              nerd-svg-icons-purple)
    ("terminal"             "oct-terminal"              nerd-svg-icons-purple)
    ("bat"                  "oct-terminal"              nerd-svg-icons-purple)

    ;; Meta
    ("tags"                 "fa-tag"                   nerd-svg-icons-blue)
    ("tag"                  "fa-tag"                   nerd-svg-icons-blue)
    ("log"                  "fa-bug"                   nerd-svg-icons-maroon)
    ("aux"                  "fa-bug"                   nerd-svg-icons-maroon)
    ("nav"                  "fa-bug"                   nerd-svg-icons-maroon)
    ("snm"                  "fa-bug"                   nerd-svg-icons-maroon)
    ("toc"                  "fa-bug"                   nerd-svg-icons-maroon)
    ("vrb"                  "fa-bug"                   nerd-svg-icons-maroon)

    ;; binary
    ("exe"                  "oct-file_binary"           nerd-svg-icons-dsilver)
    ("dll"                  "md-cogs"                 nerd-svg-icons-dsilver)
    ("lib"                  "oct-file_binary"           nerd-svg-icons-dsilver)
    ("class"                "oct-file_binary"           nerd-svg-icons-dsilver)
    ("obj"                  "oct-file_binary"           nerd-svg-icons-dsilver)
    ("so"                   "oct-file_binary"           nerd-svg-icons-dsilver)
    ("o"                    "oct-file_binary"           nerd-svg-icons-dsilver)
    ("d"                    "oct-file_binary"           nerd-svg-icons-dsilver)
    ("out"                  "oct-file_binary"           nerd-svg-icons-dsilver)
    ("elc"                  "oct-file_binary"           nerd-svg-icons-dsilver)
    ("eln"                  "oct-file_binary"           nerd-svg-icons-dsilver)
    ("cmake-cache"          "oct-file_binary"           nerd-svg-icons-dsilver)
    ("csr"                  "oct-file_binary"           nerd-svg-icons-dsilver)
    ("eslintcache"          "oct-file_binary"           nerd-svg-icons-dsilver)
    ("cer"                  "oct-file_binary"           nerd-svg-icons-dsilver)
    ("der"                  "oct-file_binary"           nerd-svg-icons-dsilver)
    ("pfx"                  "oct-file_binary"           nerd-svg-icons-dsilver)
    ("p7b"                  "oct-file_binary"           nerd-svg-icons-dsilver)
    ("p7r"                  "oct-file_binary"           nerd-svg-icons-dsilver)
    ("DS_STORE"             "oct-file_binary"           nerd-svg-icons-dsilver)
    ("src"                  "oct-file_binary"           nerd-svg-icons-dsilver)
    ("crl"                  "oct-file_binary"           nerd-svg-icons-dsilver)
    ("sst"                  "oct-file_binary"           nerd-svg-icons-dsilver)
    ("stl"                  "oct-file_binary"           nerd-svg-icons-dsilver)
    ("pyc"                  "oct-file_binary"           nerd-svg-icons-dsilver)
    ("bin"                  "oct-file_binary"           nerd-svg-icons-dsilver)

    ;; torrent
    ("torrent"              "oct-download"       nerd-svg-icons-dsilver)
    ("aria2"                "oct-download"       nerd-svg-icons-dsilver)
    ("ed2k"                 "oct-download"       nerd-svg-icons-dsilver)

    ;; book
    ("azw"                  "cod-book"                  nerd-svg-icons-dsilver)
    ("azw3"                 "cod-book"                  nerd-svg-icons-dsilver)
    ("mobi"                 "cod-book"                  nerd-svg-icons-dsilver)
    ("epub"                 "cod-book"                  nerd-svg-icons-dsilver)

    ;; ?
    ("pkg"                  "cod-package"               nerd-svg-icons-dsilver)
    ("rpm"                  "cod-package"               nerd-svg-icons-dsilver)
    ("tar"                  "oct-file_zip"              nerd-svg-icons-lmaroon)
    ("rar"                  "oct-file_zip"              nerd-svg-icons-lmaroon)
    ("gz"                   "oct-file_zip"              nerd-svg-icons-lmaroon)
    ("zip"                  "oct-file_zip"              nerd-svg-icons-lmaroon)
    ("7z"                   "oct-file_zip"              nerd-svg-icons-lmaroon)
    ("xz"                   "oct-file_zip"              nerd-svg-icons-lmaroon)
    ("tgz"                  "oct-file_zip"              nerd-svg-icons-lmaroon)
    ("dat"                  "md-chart_bar"             nerd-svg-icons-cyan)
    ("edn"                  "md-chart_bar"             nerd-svg-icons-cyan)
    ("dmg"                  "cod-package"                 nerd-svg-icons-lsilver)
    ;; Source Codesode
    ("scpt"                 "md-apple"                 nerd-svg-icons-pink)
    ("aup"                  "oct-file_code"              nerd-svg-icons-yellow)
    ("elm"                  "seti-elm"                   nerd-svg-icons-blue)
    ("erl"                  "dev-erlang"                nerd-svg-icons-red)
    ("hrl"                  "dev-erlang"                nerd-svg-icons-dred)
    ("eex"                  "seti-elixir"                nerd-svg-icons-lorange)
    ("leex"                 "seti-elixir"                nerd-svg-icons-lorange)
    ("heex"                 "seti-elixir"                nerd-svg-icons-lorange)
    ("ex"                   "seti-elixir"                nerd-svg-icons-lpurple)
    ("exs"                  "seti-elixir"                nerd-svg-icons-lred)
    ("java"                 "fae-java"                  nerd-svg-icons-purple)
    ("jar"                  "fae-java"                  nerd-svg-icons-purple)
    ("gradle"               "seti-gradle"                nerd-svg-icons-silver)
    ("ebuild"               "md-gentoo"                nerd-svg-icons-cyan)
    ("eclass"               "md-gentoo"                nerd-svg-icons-blue)
    ("go"                   "custom-go"                    nerd-svg-icons-blue)
    ("jl"                   "seti-julia"                 nerd-svg-icons-purple)
    ("magik"                "fa-magic"   nerd-svg-icons-blue)
    ("matlab"               "md-math_compass"                nerd-svg-icons-orange)
    ("nix"                  "md-nix"                   nerd-svg-icons-blue)
    ("pl"                   "dev-perl"                  nerd-svg-icons-lorange)
    ("pm"                   "dev-perl"                  nerd-svg-icons-lorange)
    ("pod"                  "dev-perl"              nerd-svg-icons-lgreen)
    ("php"                  "seti-php"                   nerd-svg-icons-lsilver)
    ("pony"                 "oct-file_code"                  nerd-svg-icons-maroon)
    ("ps1"                  "md-powershell"            nerd-svg-icons-blue)
    ("pro"                  "dev-prolog"                nerd-svg-icons-lmaroon)
    ("proog"                "dev-prolog"                nerd-svg-icons-lmaroon)
    ("py"                   "seti-python"                nerd-svg-icons-dblue)
    ("py.typed"             "seti-python"                nerd-svg-icons-pink)
    ("idr"                  "oct-file_code"                 nerd-svg-icons-red)
    ("ipynb"                "oct-file_code"               nerd-svg-icons-dorange)
    ("gem"                  "cod-ruby"              nerd-svg-icons-red)
    ("rb"                   "oct-ruby"                  nerd-svg-icons-lred)
    ("rs"                   "seti-rust"                  nerd-svg-icons-maroon)
    ("rlib"                 "seti-rust"                  nerd-svg-icons-dmaroon)
    ("r"                    "seti-R"                     nerd-svg-icons-purple)
    ("rd"                   "seti-R"                     nerd-svg-icons-purple)
    ("rdx"                  "seti-R"                     nerd-svg-icons-purple)
    ("rsx"                  "seti-R"                     nerd-svg-icons-purple)
    ("svelte"               "seti-svelte"                nerd-svg-icons-red)
    ("gql"                  "seti-graphql"               nerd-svg-icons-dpink)
    ("graphql"              "seti-graphql"               nerd-svg-icons-dpink)
    ("c"                    "seti-c"                nerd-svg-icons-blue)
    ("h"                    "seti-c"                nerd-svg-icons-purple)
    ("h.in"                 "seti-c"                nerd-svg-icons-lblue)
    ("cc"                   "md-language_cpp"        nerd-svg-icons-blue)
    ("cpp"                  "md-language_cpp"        nerd-svg-icons-blue)
    ("cxx"                  "md-language_cpp"        nerd-svg-icons-blue)
    ("hh"                   "md-language_cpp"        nerd-svg-icons-purple)
    ("hpp"                  "md-language_cpp"        nerd-svg-icons-purple)
    ("hpp.in"               "md-language_cpp"        nerd-svg-icons-lblue)
    ("hxx"                  "md-language_cpp"        nerd-svg-icons-purple)
    ("m"                    "md-apple")
    ("mm"                   "md-apple")

    ;; Lisps
    ("cl"                   "oct-file_code"           nerd-svg-icons-lorange)
    ("l"                    "oct-file_code"                  nerd-svg-icons-orange)
    ("lisp"                 "oct-file_code"                  nerd-svg-icons-orange)
    ("hy"                   "oct-file_code"                    nerd-svg-icons-blue)
    ("el"                   "custom-emacs"                 nerd-svg-icons-purple)
    ("clj"                  "seti-clojure"           nerd-svg-icons-blue)
    ("cljc"                 "seti-clojure"           nerd-svg-icons-blue)
    ("cljs"                 "seti-clojure"             nerd-svg-icons-dblue)
    ("coffee"               "dev-coffeescript"          nerd-svg-icons-maroon)
    ("iced"                 "dev-coffeescript"          nerd-svg-icons-lmaroon)
    ("dart"                 "dev-dart"                  nerd-svg-icons-blue)
    ("rkt"                  "oct-file_code"                nerd-svg-icons-red)
    ("scrbl"                "oct-file_code"                nerd-svg-icons-blue)
    ;; Stylesheeting
    ("css"                  "md-language_css3"                  nerd-svg-icons-yellow)
    ("scss"                 "dev-sass"                  nerd-svg-icons-pink)
    ("sass"                 "dev-sass"                  nerd-svg-icons-dpink)
    ("less"                 "dev-sass"                  nerd-svg-icons-dyellow)
    ("postcss"              "md-language_css3"               nerd-svg-icons-dred)
    ("sss"                  "md-language_css3"               nerd-svg-icons-dred)
    ("styl"                 "dev-stylus"                nerd-svg-icons-lgreen)
    ("csv"                  "cod-graph_line"                 nerd-svg-icons-dblue)
    ;; haskell
    ("hs"                   "seti-haskell"               nerd-svg-icons-red)
    ("chs"                  "seti-haskell"               nerd-svg-icons-red)
    ("lhs"                  "seti-haskell"               nerd-svg-icons-red)
    ("hsc"                  "seti-haskell"               nerd-svg-icons-red)
    ;; Web modes
    ("inky-haml"            "fa-html5"                  nerd-svg-icons-lyellow)
    ("haml"                 "fa-html5"                  nerd-svg-icons-lyellow)
    ("htm"                  "fa-html5"                 nerd-svg-icons-orange)
    ("html"                 "fa-html5"                 nerd-svg-icons-orange)
    ("inky-er"              "fa-html5"                 nerd-svg-icons-lred)
    ("inky-erb"             "fa-html5"                 nerd-svg-icons-lred)
    ("erb"                  "fa-html5"                 nerd-svg-icons-lred)
    ("hbs"                  "oct-file_code"             nerd-svg-icons-green)
    ("inky-slim"            "cod-dashboard"             nerd-svg-icons-yellow)
    ("slim"                 "cod-dashboard"             nerd-svg-icons-yellow)
    ("jade"                 "seti-jade"                  nerd-svg-icons-red)
    ("pug"                  "seti-pug"               nerd-svg-icons-red)
    ;; Javascript
    ("d3js"                 "oct-file_code"                    nerd-svg-icons-lgreen)
    ("re"                   "seti-reasonml"                nerd-svg-icons-red-alt)
    ("rei"                  "seti-reasonml"                nerd-svg-icons-dred)
    ("ml"                   "seti-ocaml"                 nerd-svg-icons-lpink)
    ("mli"                  "seti-ocaml"                 nerd-svg-icons-dpink)
    ("react"                "md-react"                 nerd-svg-icons-lblue)
    ("ts"                   "seti-typescript"      nerd-svg-icons-blue-alt)
    ("js"                   "seti-javascript"      nerd-svg-icons-yellow)
    ("es"                   "seti-javascript"      nerd-svg-icons-yellow)
    ("jsx"                  "seti-javascript"               nerd-svg-icons-yellow)
    ("tsx"                  "seti-typescript"               nerd-svg-icons-blue-alt)
    ("njs"                  "md-nodejs"                nerd-svg-icons-lgreen)
    ("vue"                  "seti-vue"                   nerd-svg-icons-lgreen)

    ("sbt"                  "seti-sbt"                   nerd-svg-icons-red)
    ("scala"                "seti-scala"                 nerd-svg-icons-red)
    ("scm"                  "oct-file_code"                nerd-svg-icons-red)
    ("swift"                "seti-swift"                 nerd-svg-icons-green)

    ("tcl"                  "oct-file_code"                   nerd-svg-icons-dred)

    ("tf"                   "seti-terraform"             nerd-svg-icons-purple-alt)
    ("tfvars"               "seti-terraform"             nerd-svg-icons-purple-alt)
    ("tfstate"              "seti-terraform"             nerd-svg-icons-purple-alt)

    ("asm"                  "oct-file_code"      nerd-svg-icons-blue)
    ;; Verilog(-AMS) and SystemVerilog(-AMS)
    ("v"                    "oct-file_code"               nerd-svg-icons-red)
    ("vams"                 "oct-file_code"               nerd-svg-icons-red)
    ("sv"                   "oct-file_code"               nerd-svg-icons-red)
    ("sva"                  "oct-file_code"               nerd-svg-icons-red)
    ("svh"                  "oct-file_code"               nerd-svg-icons-red)
    ("svams"                "oct-file_code"               nerd-svg-icons-red)
    ;; VHDL(-AMS)
    ("vhd"                  "oct-file_code"                  nerd-svg-icons-blue)
    ("vhdl"                 "oct-file_code"                  nerd-svg-icons-blue)
    ("vhms"                 "oct-file_code"                  nerd-svg-icons-blue)
    ;; Cabal
    ("cabal"                "oct-file_code"                 nerd-svg-icons-lblue)
    ;; Kotlin
    ("kt"                   "seti-kotlin"                nerd-svg-icons-orange)
    ("kts"                  "seti-kotlin"                nerd-svg-icons-orange)
    ;; Nimrod
    ("nim"                  "seti-nim"                nerd-svg-icons-yellow)
    ("nims"                 "seti-nim"                nerd-svg-icons-yellow)
    ;; SQL
    ("sql"                  "fa-database"              nerd-svg-icons-silver)
    ("db"                   "fa-database"              nerd-svg-icons-silver)
    ("cache"                "fa-database"              nerd-svg-icons-green)
    ;; Styles
    ("styles"               "oct-file_code"                 nerd-svg-icons-red)
    ;; Lua
    ("lua"                  "seti-lua"                   nerd-svg-icons-dblue)
    ;; ASCII doc
    ("adoc"                 "oct-file_code"              nerd-svg-icons-lblue)
    ("asciidoc"             "oct-file_code"              nerd-svg-icons-lblue)
    ;; Puppet
    ("pp"                   "seti-puppet"                nerd-svg-icons-yellow)
    ;; Jinja
    ("j2"                   "seti-jinja"                 nerd-svg-icons-silver)
    ("jinja2"               "seti-jinja"                 nerd-svg-icons-silver)
    ;; Docker
    ("dockerfile"           "seti-docker"                nerd-svg-icons-cyan)
    ;; Vagrant
    ("vagrantfile"          "oct-file_code"               nerd-svg-icons-blue)
    ;; GLSL
    ("glsl"                 "oct-file_code"          nerd-svg-icons-blue)
    ("vert"                 "oct-file_code"          nerd-svg-icons-blue)
    ("tesc"                 "oct-file_code"          nerd-svg-icons-purple)
    ("tese"                 "oct-file_code"          nerd-svg-icons-dpurple)
    ("geom"                 "oct-file_code"          nerd-svg-icons-green)
    ("frag"                 "oct-file_code"          nerd-svg-icons-red)
    ("comp"                 "oct-file_code"          nerd-svg-icons-dblue)
    ;; CUDA
    ("cu"                   "oct-file_code"          nerd-svg-icons-green)
    ("cuh"                  "oct-file_code"          nerd-svg-icons-green)
    ;; Fortran
    ("f90"                  "md-language_fortran"               nerd-svg-icons-purple)
    ;; C#
    ("cs"                   "md-language_csharp"           nerd-svg-icons-dblue)
    ("csx"                  "md-language_csharp"           nerd-svg-icons-dblue)
    ;; F#
    ("fs"                   "dev-fsharp"                nerd-svg-icons-blue-alt)
    ("fsi"                  "dev-fsharp"                nerd-svg-icons-blue-alt)
    ("fsx"                  "dev-fsharp"                nerd-svg-icons-blue-alt)
    ("fsscript"             "dev-fsharp"                nerd-svg-icons-blue-alt)
    ;; zig
    ("zig"                  "seti-zig"                   nerd-svg-icons-orange)
    ;; odin
    ("odin"                 "oct-file_code"                  nerd-svg-icons-lblue)
    ;; File Types
    ("ico"                  "seti-image"            nerd-svg-icons-blue)
    ("png"                  "seti-image"            nerd-svg-icons-orange)
    ("gif"                  "seti-image"            nerd-svg-icons-green)
    ("jpeg"                 "seti-image"            nerd-svg-icons-dblue)
    ("jpg"                  "seti-image"            nerd-svg-icons-dblue)
    ("webp"                 "seti-image"            nerd-svg-icons-dblue)
    ("svg"                  "seti-image"            nerd-svg-icons-lgreen)
    ("eps"                  "seti-image"            nerd-svg-icons-lgreen)
    ;; Audio
    ("mp3"                  "md-music"                 nerd-svg-icons-dred)
    ("wav"                  "md-music"                 nerd-svg-icons-dred)
    ("m4a"                  "md-music"                 nerd-svg-icons-dred)
    ("ogg"                  "md-music"                 nerd-svg-icons-dred)
    ("flac"                 "md-music"                 nerd-svg-icons-dred)
    ("opus"                 "md-music"                 nerd-svg-icons-dred)
    ("au"                   "md-music"                 nerd-svg-icons-dred)
    ("aif"                  "md-music"                 nerd-svg-icons-dred)
    ("aifc"                 "md-music"                 nerd-svg-icons-dred)
    ("aiff"                 "md-music"                 nerd-svg-icons-dred)
    ("ly"                   "md-music"                 nerd-svg-icons-green)
    ;; Video
    ("mov"                  "md-movie_open_outline"                  nerd-svg-icons-blue)
    ("mp4"                  "md-movie_open_outline"                  nerd-svg-icons-blue)
    ("ogv"                  "md-movie_open_outline"                  nerd-svg-icons-dblue)
    ("mpg"                  "md-movie_open_outline"                  nerd-svg-icons-blue)
    ("mpeg"                 "md-movie_open_outline"                  nerd-svg-icons-blue)
    ("flv"                  "md-movie_open_outline"                  nerd-svg-icons-blue)
    ("ogv"                  "md-movie_open_outline"                  nerd-svg-icons-dblue)
    ("mkv"                  "md-movie_open_outline"                  nerd-svg-icons-blue)
    ("webm"                 "md-movie_open_outline"                  nerd-svg-icons-blue)
    ;; Subtitle
    ("srt"                  "md-closed_caption_outline"        nerd-svg-icons-dblue)
    ;; Fonts
    ("ttf"                  "seti-font"                  nerd-svg-icons-dcyan)
    ("otf"                  "seti-font"                  nerd-svg-icons-dcyan)
    ("woff"                 "seti-font"                  nerd-svg-icons-cyan)
    ("woff2"                "seti-font"                  nerd-svg-icons-cyan)
    ("ttc"                  "seti-font"                  nerd-svg-icons-cyan)
    ;; Doc
    ("pdf"                  "seti-pdf"              nerd-svg-icons-dred)
    ("text"                 "md-file_document_multiple"             nerd-svg-icons-cyan)
    ("txt"                  "fa-edit"                  nerd-svg-icons-cyan)
    ("rst"                  "md-text_box_edit"      nerd-svg-icons-green)
    ("doc"                  "md-microsoft_word"        nerd-svg-icons-blue)
    ("docx"                 "md-microsoft_word"        nerd-svg-icons-blue)
    ("docm"                 "md-microsoft_word"        nerd-svg-icons-blue)
    ("texi"                 "seti-tex"                   nerd-svg-icons-lred)
    ("tex"                  "seti-tex"                   nerd-svg-icons-lred)
    ("sty"                  "seti-tex"                   nerd-svg-icons-lred)
    ("pygtex"               "seti-tex"                   nerd-svg-icons-pink)
    ("pygstyle"             "seti-tex"                   nerd-svg-icons-pink)
    ("md"                   "md-language_markdown"              nerd-svg-icons-lblue)
    ("bib"                  "seti-tex"                nerd-svg-icons-maroon)
    ("org"                  "custom-orgmode"              nerd-svg-icons-lgreen)
    ("pps"                  "md-microsoft_powerpoint"  nerd-svg-icons-orange)
    ("ppt"                  "md-microsoft_powerpoint"  nerd-svg-icons-orange)
    ("pptsx"                "md-microsoft_powerpoint"  nerd-svg-icons-orange)
    ("ppttx"                "md-microsoft_powerpoint"  nerd-svg-icons-orange)
    ("knt"                  "md-microsoft_powerpoint"  nerd-svg-icons-cyan)
    ("xlsx"                 "md-microsoft_excel"       nerd-svg-icons-dgreen)
    ("xlsm"                 "md-microsoft_excel"       nerd-svg-icons-dgreen)
    ("xlsb"                 "md-microsoft_excel"       nerd-svg-icons-dgreen)
    ("xltx"                 "md-microsoft_excel"       nerd-svg-icons-dgreen)
    ("xltm"                 "md-microsoft_excel"       nerd-svg-icons-dgreen)
    ;; key and licence
    ("key"                  "cod-key"                   nerd-svg-icons-lblue)
    ("pem"                  "cod-key"                   nerd-svg-icons-orange)
    ("p12"                  "cod-key"                   nerd-svg-icons-dorange)
    ("crt"                  "cod-key"                   nerd-svg-icons-lblue)
    ("pub"                  "cod-key"                   nerd-svg-icons-blue)
    ("gpg"                  "cod-key"                   nerd-svg-icons-lblue)
    ("license.md"           "cod-key"                   nerd-svg-icons-purple)
    ("license"              "cod-key"                   nerd-svg-icons-purple)
    ("lic"                  "cod-key"                   nerd-svg-icons-dblue)
    ("gemfile"              "cod-key"                   nerd-svg-icons-dblue)
    ("bookmarks"            "fa-bookmark"             nerd-svg-icons-orange)

    ;; Config
    ("node"                 "md-nodejs"                nerd-svg-icons-green)
    ("babelrc"              "seti-babel"                 nerd-svg-icons-yellow)
    ("bashrc"               "md-script_text"                nerd-svg-icons-dpink)
    ("bowerrc"              "seti-bower"                 nerd-svg-icons-silver)
    ("cr"                   "seti-crystal"               nerd-svg-icons-yellow)
    ("ecr"                  "seti-crystal"               nerd-svg-icons-yellow)
    ("ini"                  "md-cogs"                  nerd-svg-icons-yellow)
    ("eslintignore"         "seti-eslint"                nerd-svg-icons-purple)
    ("eslint"               "seti-eslint"                nerd-svg-icons-lpurple)
    ("git"                  "seti-git"                   nerd-svg-icons-lred)
    ("mk"                   "dev-gnu"                   nerd-svg-icons-dorange)
    ("clang"                "md-cogs"                  nerd-svg-icons-dpurple)
    ("llvm"                 "md-cogs"                  nerd-svg-icons-dpurple)
    ("clangd"               "md-cogs"                  nerd-svg-icons-dpurple)
    ("cmake"                "md-cogs"                 nerd-svg-icons-red)
    ("cmakelists.txt"       "md-cogs"                 nerd-svg-icons-red)
    ("ninja"                "md-ninja")
    ("makefile"             "seti-makefile"              nerd-svg-icons-cyan)
    ("dockerignore"         "seti-docker"                nerd-svg-icons-dblue)
    ("xml"                  "cod-code"                  nerd-svg-icons-lorange)
    ("json"                 "seti-settings"              nerd-svg-icons-yellow)
    ("clang-format"         "seti-settings"              nerd-svg-icons-yellow)
    ("cson"                 "md-cogs"                  nerd-svg-icons-yellow)
    ("yml"                  "md-cogs"                  nerd-svg-icons-dyellow)
    ("yaml"                 "md-cogs"                  nerd-svg-icons-dyellow)
    ("toml"                 "md-cogs"                  nerd-svg-icons-pink)
    ("cfg"                  "md-cogs"                  nerd-svg-icons-dblue)
    ("terminfo"             "md-cogs"                  nerd-svg-icons-dblue)
    ("settings.json"        "md-cogs"                  nerd-svg-icons-dblue)
    ("Vagrantfile"          "md-cogs"                  nerd-svg-icons-silver)
    ("babel.config.js"      "md-cogs"                  nerd-svg-icons-silver)
    ("babelignore"          "md-cogs"                  nerd-svg-icons-silver)
    ("babelrc"              "md-cogs"                  nerd-svg-icons-silver)
    ("babelrc.js"           "md-cogs"                  nerd-svg-icons-silver)
    ("babelrc.json"         "md-cogs"                  nerd-svg-icons-silver)
    ("bashrc"               "md-cogs"                  nerd-svg-icons-silver)
    ("bazel"                "md-cogs"                  nerd-svg-icons-silver)
    ("bazelrc"              "md-cogs"                  nerd-svg-icons-silver)
    ("bower.json"           "md-cogs"                  nerd-svg-icons-silver)
    ("bowerrc"              "md-cogs"                  nerd-svg-icons-silver)
    ("cabal"                "md-cogs"                  nerd-svg-icons-silver)
    ("cfg"                  "md-cogs"                  nerd-svg-icons-silver)
    ("conf"                 "md-cogs"                  nerd-svg-icons-silver)
    ("config"               "md-cogs"                  nerd-svg-icons-silver)
    ("cson"                 "md-cogs"                  nerd-svg-icons-silver)
    ("editorconfig"         "md-cogs"                  nerd-svg-icons-silver)
    ("envrc"                "md-cogs"                  nerd-svg-icons-silver)
    ("eslintignore"         "md-cogs"                  nerd-svg-icons-silver)
    ("eslintrc"             "md-cogs"                  nerd-svg-icons-silver)
    ("feature"              "md-cogs"                  nerd-svg-icons-silver)
    ("gemfile"              "md-cogs"                  nerd-svg-icons-silver)
    ("gitattributes"        "md-cogs"                  nerd-svg-icons-silver)
    ("gitconfig"            "md-cogs"                  nerd-svg-icons-silver)
    ("gitignore"            "md-cogs"                  nerd-svg-icons-silver)
    ("gitmodules"           "md-cogs"                  nerd-svg-icons-silver)
    ("ideavimrc"            "md-cogs"                  nerd-svg-icons-silver)
    ("iml"                  "md-cogs"                  nerd-svg-icons-silver)
    ("ini"                  "md-cogs"                  nerd-svg-icons-silver)
    ("inputrc"              "md-cogs"                  nerd-svg-icons-silver)
    ("ledgerrc"             "md-cogs"                  nerd-svg-icons-silver)
    ("lock"                 "md-cogs"                  nerd-svg-icons-silver)
    ("nginx"                "md-cogs"                  nerd-svg-icons-silver)
    ("npm-shrinkwrap.json"  "md-cogs"                  nerd-svg-icons-silver)
    ("npmignore"            "md-cogs"                  nerd-svg-icons-silver)
    ("npmrc"                "md-cogs"                  nerd-svg-icons-silver)
    ("package-lock.json"    "md-cogs"                  nerd-svg-icons-silver)
    ("package.json"         "md-cogs"                  nerd-svg-icons-silver)
    ("phpunit"              "md-cogs"                  nerd-svg-icons-silver)
    ("pkg"                  "md-cogs"                  nerd-svg-icons-silver)
    ("plist"                "md-cogs"                  nerd-svg-icons-silver)
    ("properties"           "md-cogs"                  nerd-svg-icons-silver)
    ("terminalrc"           "md-cogs"                  nerd-svg-icons-silver)
    ("tridactylrc"          "md-cogs"                  nerd-svg-icons-silver)
    ("vimperatorrc"         "md-cogs"                  nerd-svg-icons-silver)
    ("vimrc"                "md-cogs"                  nerd-svg-icons-silver)
    ("vrapperrc"            "md-cogs"                  nerd-svg-icons-silver)
    ("xdefaults"            "md-cogs"                  nerd-svg-icons-silver)
    ("xml"                  "md-cogs"                  nerd-svg-icons-silver)
    ("xresources"           "md-cogs"                  nerd-svg-icons-silver)
    ("yaml"                 "md-cogs"                  nerd-svg-icons-silver)
    ("yarn-integrity"       "md-cogs"                  nerd-svg-icons-silver)
    ("yarnclean"            "md-cogs"                  nerd-svg-icons-silver)
    ("yarnignore"           "md-cogs"                  nerd-svg-icons-silver)
    ("yarnrc"               "md-cogs"                  nerd-svg-icons-silver)
    ("rc"                   "md-cogs"                  nerd-svg-icons-silver)
    ("project"              "md-cogs"                  nerd-svg-icons-silver)
    ("prefs"                "md-cogs"                  nerd-svg-icons-silver)
    ("sln"                  "md-microsoft_visual_studio"          nerd-svg-icons-blue)
    ("vcxproj"              "md-microsoft_visual_studio"          nerd-svg-icons-blue)
    ("vcproj"               "md-microsoft_visual_studio"          nerd-svg-icons-blue)

    ;; model
    ("pth"                  "cod-package"               nerd-svg-icons-dsilver)
    ("ckpt"                 "cod-package"               nerd-svg-icons-dsilver)
    ("model"                "cod-package"               nerd-svg-icons-dsilver)

    ;; whl
    ("whl"                  "cod-package"               nerd-svg-icons-purple-alt)
    ))

(defvar nerd-svg-icons-regexp-icon-alist
  '(
    ;;
    ("^TAGS$"                   "fa-tag"             nerd-svg-icons-blue)
    ("^TAG$"                    "fa-tag"             nerd-svg-icons-blue)
    ("^TODO$"                   "oct-checklist"       nerd-svg-icons-lyellow)
    ("^LICENSE$"                "cod-book"            nerd-svg-icons-blue)
    ("^readme.md$"              "md-language_markdown"        nerd-svg-icons-lblue)
    ("^readme"                  "cod-book"            nerd-svg-icons-lcyan)
    ("help"                     "md-information_outline"            nerd-svg-icons-purple)
    ("info"                     "md-information_outline"            nerd-svg-icons-pink)

    ;; Config
    ("nginx$"                   "dev-nginx"           nerd-svg-icons-dgreen)
    ("apache$"                  "md-apache_kafka"          nerd-svg-icons-dgreen)

    ;; C
    ("^Makefile$"               "seti-makefile"        nerd-svg-icons-dorange)
    ("^CMakeLists.txt$"         "md-cogs"           nerd-svg-icons-red)
    ("^CMakeCache.txt$"         "md-cogs"           nerd-svg-icons-blue)
    ("cmake"                    "md-cogs"           nerd-svg-icons-red)

    ;; Visual Studio
    ("vcxproj"                  "md-microsoft_visual_studio"    nerd-svg-icons-blue)
    ("vcproj"                   "md-microsoft_visual_studio"    nerd-svg-icons-blue)

    ;; Docker
    ("^\\.?Dockerfile"          "seti-docker"          nerd-svg-icons-blue)

    ;; Homebrew
    ("^Brewfile$"               "fa-beer"        nerd-svg-icons-lsilver)

    ;; ;; AWS
    ("^stack.*.json$"           "md-aws"             nerd-svg-icons-orange)
    ("^serverless\\.yml$"       "fa-bolt"            nerd-svg-icons-yellow)

    ;; lock files
    ("~$"                       "md-lock"            nerd-svg-icons-maroon)

    ;; Source Codes
    ("^mix.lock$"               "seti-elixir"          nerd-svg-icons-lyellow)

    ;; Ruby
    ("^Gemfile\\(\\.lock\\)?$"  "cod-ruby"     nerd-svg-icons-red)
    ("_?test\\.rb$"             "oct-ruby"       nerd-svg-icons-red)
    ("_?test_helper\\.rb$"      "oct-ruby"       nerd-svg-icons-dred)
    ("_?spec\\.rb$"             "oct-ruby"       nerd-svg-icons-red)
    ("_?spec_helper\\.rb$"      "oct-ruby"       nerd-svg-icons-dred)

    ("-?spec\\.ts$"             "seti-typescript" nerd-svg-icons-blue)
    ("-?test\\.ts$"             "seti-typescript" nerd-svg-icons-blue)
    ("-?spec\\.js$"             "seti-javascript"         nerd-svg-icons-lpurple)
    ("-?test\\.js$"             "seti-javascript"         nerd-svg-icons-lpurple)
    ("-?spec\\.jsx$"            "md-react"      nerd-svg-icons-blue-alt)
    ("-?test\\.jsx$"            "md-react"      nerd-svg-icons-blue-alt)

    ;; Git
    ("^MERGE_"                  "oct-git_merge"       nerd-svg-icons-red)
    ("^COMMIT_EDITMSG"          "oct-git_commit"      nerd-svg-icons-red)

    ;; Stylesheeting
    ("stylelint"                "seti-stylelint"       nerd-svg-icons-lyellow)

    ;; JavaScript
    ("^package.json$"           "seti-npm"             nerd-svg-icons-red)
    ("^package.lock.json$"      "seti-npm"             nerd-svg-icons-dred)
    ("^yarn\\.lock"             "seti-yarn"            nerd-svg-icons-blue-alt)
    ("\\.npmignore$"            "seti-npm"             nerd-svg-icons-dred)
    ("^bower.json$"             "seti-bower"           nerd-svg-icons-lorange)
    ("^gulpfile"                "seti-gulp"            nerd-svg-icons-lred)
    ("^gruntfile"               "seti-grunt"           nerd-svg-icons-lyellow)
    ("^webpack"                 "seti-webpack"         nerd-svg-icons-lblue)

    ;; Go
    ("^go.mod$"                 "seti-go"       nerd-svg-icons-blue-alt)
    ("^go.work$"                "seti-go"       nerd-svg-icons-blue-alt)

    ;; Emacs
    ("bookmarks"                "fa-bookmark"       nerd-svg-icons-orange)
    ("bookmark"                 "fa-bookmark"        nerd-svg-icons-orange)

    ("^\\*scratch\\*$"          "fa-sticky_note"     nerd-svg-icons-lyellow)
    ("^\\*scratch.*"            "fa-sticky_note"     nerd-svg-icons-yellow)
    ("^\\*new-tab\\*$"          "fa-star"            nerd-svg-icons-cyan)

    ("\\.git"                   "seti-git"             nerd-svg-icons-yellow)

    ("^\\."                     "md-cogs")
    ))

(defvar nerd-svg-icons-default-file-icon
  '("oct-file" nerd-svg-icons-dsilver))

(defvar nerd-svg-icons-dir-regexp-icon-alist
  '(
    ("trash"            "fa-trash")
    ("dropbox"          "fa-dropbox")
    ("google[ _-]drive" "md-google_drive")
    ("^atom$"           "md-atom")
    ("documents"        "md-folder_file")
    ("download"         "md-folder_download")
    ("desktop"          "md-desktop_mac")
    ("pictures"         "md-folder_image")
    ("photos"           "md-folder_image")
    ("music"            "md-folder_music")
    ("movies"           "md-folder_play")
    ("code"             "md-folder_edit")
    ("workspace"        "cod-multiple_windows")
    ("test"             "md-folder_cog")
    ("config"           "md-folder_cog")
    ("history"          "md-folder_clock")
    ("\\.git"           "seti-git")
    ))

(defvar nerd-svg-icons-default-dir-icon
  '("md-folder" nerd-svg-icons-dsilver))

(defvar nerd-svg-icons-weather-icon-alist
  '(
    ("tornado"               "md-tornado")
    ("hurricane"             "md-hurricane")
    ("thunderstorms"         "weather-thunderstorm")
    ("sunny"                 "weather-day_sunny")
    ("rain.*snow"            "weather-rain_mix")
    ("rain.*hail"            "weather-rain_mix")
    ("sleet"                 "weather-sleet")
    ("hail"                  "weather-hail")
    ("drizzle"               "weather-sprinkle")
    ("rain"                  "weather-showers")
    ("showers"               "weather-showers")
    ("blowing.*snow"         "weather-snow_wind")
    ("snow"                  "weather-snow")
    ("dust"                  "weather-dust")
    ("fog"                   "weather-fog")
    ("haze"                  "weather-day_haze")
    ("smoky"                 "weather-smoke")
    ("blustery"              "weather-cloudy_windy")
    ("windy"                 "weather-cloudy_gusts")
    ("cold"                  "weather-snowflake_cold")
    ("partly.*cloudy.*night" "weather-night_alt_partly_cloudy")
    ("partly.*cloudy"        "weather-day_cloudy_high")
    ("cloudy.*night"         "weather-night_alt_cloudy")
    ("cxloudy.*day"          "weather-day_cloudy")
    ("cloudy"                "weather-cloudy")
    ("clear.*night"          "weather-night_clear")
    ("fair.*night"           "weather-stars")
    ("fair.*day"             "weather-horizon")
    ("hot"                   "weather-hot")
    ("not.*available"        "weather-na")
    ))

(defvar nerd-svg-icons-mode-icon-alist
  '(
    (emacs-lisp-mode                    "custom-emacs"             nerd-svg-icons-purple)
    (circe-server-mode                  "fa-commenting_o")
    (circe-channel-mode                 "fa-commenting_o")
    (crystal-mode                       "seti-crystal"           nerd-svg-icons-yellow)
    (erc-mode                           "fa-commenting_o")
    (inferior-emacs-lisp-mode           "custom-emacs"             nerd-svg-icons-lblue)
    (dired-mode                         "md-folder_multiple")
    (lisp-interaction-mode              "oct-file_code"              nerd-svg-icons-orange)
    (sly-mrepl-mode                     "oct-file_code"       nerd-svg-icons-orange)
    (slime-repl-mode                    "oct-file_code"       nerd-svg-icons-orange)
    (org-mode                           "custom-orgmode"          nerd-svg-icons-lgreen)
    (typescript-mode                    "seti-typescript"  nerd-svg-icons-blue-alt)
    (react-mode                         "md-react"             nerd-svg-icons-lblue)
    (js-mode                            "seti-javascript"  nerd-svg-icons-yellow)
    (js-jsx-mode                        "md-react"             nerd-svg-icons-yellow)
    (js2-mode                           "seti-javascript"  nerd-svg-icons-yellow)
    (js3-mode                           "seti-javascript"  nerd-svg-icons-yellow)
    (rjsx-mode                          "md-react"             nerd-svg-icons-cyan-alt)
    (term-mode                          "oct-terminal")
    (vterm-mode                         "oct-terminal")
    (eshell-mode                        "oct-terminal"          nerd-svg-icons-purple)
    (magit-refs-mode                    "oct-git_branch"        nerd-svg-icons-red)
    (magit-process-mode                 "seti-github")
    (magit-diff-mode                    "oct-git_compare"       nerd-svg-icons-lblue)
    (ediff-mode                         "oct-git_compare"       nerd-svg-icons-red)
    (diff-mode                          "oct-git_compare"       nerd-svg-icons-purple)
    (comint-mode                        "oct-terminal"          nerd-svg-icons-lblue)
    (eww-mode                           "md-firefox"           nerd-svg-icons-red)
    (org-agenda-mode                    "oct-checklist"         nerd-svg-icons-lgreen)
    (cfw:calendar-mode                  "md-calendar_check")
    (ibuffer-mode                       "fa-files_o"             nerd-svg-icons-dsilver)
    (messages-buffer-mode               "md-message_text_outline"           nerd-svg-icons-dsilver)
    (help-mode                          "md-information_outline"              nerd-svg-icons-purple)
    (Info-mode                          "md-information_outline"              nerd-svg-icons-pink)
    (benchmark-init/tree-mode           "cod-dashboard")
    (jenkins-mode                       "seti-jenkins"           nerd-svg-icons-blue)
    (magit-popup-mode                   "seti-git"               nerd-svg-icons-red)
    (magit-status-mode                  "seti-git"               nerd-svg-icons-lred)
    (magit-log-mode                     "seti-git"               nerd-svg-icons-green)
    (mu4e-compose-mode                  "md-pencil")
    (mu4e-headers-mode                  "cod-mail")
    (mu4e-main-mode                     "cod-mail")
    (mu4e-view-mode                     "cod-mail_read")
    (package-menu-mode                  "md-package_variant_plus"           nerd-svg-icons-silver)
    (paradox-menu-mode                  "md-archive"           nerd-svg-icons-silver)
    (Custom-mode                        "seti-settings")
    (web-mode                           "seti-webpack"       nerd-svg-icons-purple)
    (fundamental-mode                   "md-file_document_multiple"         nerd-svg-icons-dsilver)
    (special-mode                       "md-information_outline"              nerd-svg-icons-yellow)
    (text-mode                          "md-file_document_multiple"         nerd-svg-icons-cyan)
    (enh-ruby-mode                      "oct-ruby"              nerd-svg-icons-lred)
    (ruby-mode                          "oct-ruby"              nerd-svg-icons-lred)
    (inf-ruby-mode                      "oct-ruby"              nerd-svg-icons-red)
    (projectile-rails-compilation-mode  "oct-ruby"              nerd-svg-icons-red)
    (rspec-compilation-mode             "oct-ruby"              nerd-svg-icons-red)
    (rake-compilation-mode              "oct-ruby"              nerd-svg-icons-red)
    (sh-mode                            "oct-terminal"          nerd-svg-icons-purple)
    (shell-mode                         "oct-terminal"          nerd-svg-icons-purple)
    (fish-mode                          "oct-terminal"          nerd-svg-icons-lpink)
    (nginx-mode                         "dev-nginx"             nerd-svg-icons-dgreen)
    (apache-mode                        "md-apache_kafka"            nerd-svg-icons-dgreen)
    (makefile-mode                      "seti-makefile"          nerd-svg-icons-dorange)
    (cmake-mode                         "md-cogs"             nerd-svg-icons-red)
    (dockerfile-mode                    "seti-docker"            nerd-svg-icons-blue)
    (docker-compose-mode                "seti-docker"            nerd-svg-icons-lblue)
    (nxml-mode                          "cod-code"              nerd-svg-icons-lorange)
    (json-mode                          "seti-settings"          nerd-svg-icons-yellow)
    (jsonian-mode                       "seti-settings"          nerd-svg-icons-yellow)
    (yaml-mode                          "seti-settings"          nerd-svg-icons-dyellow)
    (elisp-byte-code-mode               "oct-file_binary"            nerd-svg-icons-dsilver)
    (archive-mode                       "oct-archive"          nerd-svg-icons-lmaroon)
    (elm-mode                           "seti-elm"               nerd-svg-icons-blue)
    (erlang-mode                        "dev-erlang"            nerd-svg-icons-red)
    (elixir-mode                        "seti-elixir"            nerd-svg-icons-lorange)
    (java-mode                          "fae-java"              nerd-svg-icons-purple)
    (go-mode                            "custom-go"                nerd-svg-icons-blue)
    (go-dot-mod-mode                    "seti-go"         nerd-svg-icons-blue-alt)
    (go-dot-work-mode                   "seti-go"         nerd-svg-icons-blue-alt)
    (graphql-mode                       "seti-graphql"           nerd-svg-icons-dpink)
    (matlab-mode                        "md-math_compass"            nerd-svg-icons-orange)
    (nix-mode                           "md-nix"               nerd-svg-icons-blue)
    (perl-mode                          "dev-perl"              nerd-svg-icons-lorange)
    (cperl-mode                         "dev-perl"              nerd-svg-icons-lorange)
    (php-mode                           "seti-php"               nerd-svg-icons-lsilver)
    (prolog-mode                        "dev-prolog"            nerd-svg-icons-lmaroon)
    (python-mode                        "seti-python"            nerd-svg-icons-dblue)
    (inferior-python-mode               "seti-python"            nerd-svg-icons-dblue)
    (racket-mode                        "oct-file_code"            nerd-svg-icons-red)
    (rust-mode                          "seti-rust"              nerd-svg-icons-maroon)
    (scala-mode                         "seti-scala"             nerd-svg-icons-red)
    (scheme-mode                        "oct-file_code"            nerd-svg-icons-red)
    (swift-mode                         "seti-swift"             nerd-svg-icons-green)
    (svelte-mode                        "seti-svelte"            nerd-svg-icons-red)
    (c-mode                             "seti-c"            nerd-svg-icons-blue)
    (c++-mode                           "md-language_cpp"    nerd-svg-icons-blue)
    (csharp-mode                        "md-language_csharp"       nerd-svg-icons-dblue)
    (clojure-mode                       "seti-clojure"       nerd-svg-icons-blue)
    (cider-repl-mode                    "seti-clojure"       nerd-svg-icons-green)
    (clojurescript-mode                 "seti-clojure"         nerd-svg-icons-dblue)
    (coffee-mode                        "dev-coffeescript"      nerd-svg-icons-maroon)
    (lisp-mode                          "oct-file_code"              nerd-svg-icons-orange)
    (css-mode                           "md-language_css3"              nerd-svg-icons-yellow)
    (scss-mode                          "dev-sass"              nerd-svg-icons-pink)
    (sass-mode                          "dev-sass"              nerd-svg-icons-dpink)
    (less-css-mode                      "dev-sass"              nerd-svg-icons-dyellow)
    (stylus-mode                        "dev-stylus"            nerd-svg-icons-lgreen)
    (csv-mode                           "cod-graph_line"             nerd-svg-icons-dblue)
    (haskell-mode                       "seti-haskell"           nerd-svg-icons-red)
    (haskell-c2hs-mode                  "seti-haskell"           nerd-svg-icons-red)
    (literate-haskell-mode              "seti-haskell"           nerd-svg-icons-red)
    (haml-mode                          "fa-html5"              nerd-svg-icons-lyellow)
    (html-mode                          "fa-html5"             nerd-svg-icons-orange)
    (rhtml-mode                         "fa-html5"             nerd-svg-icons-lred)
    (mustache-mode                      "oct-file_code"         nerd-svg-icons-green)
    (slim-mode                          "cod-dashboard"         nerd-svg-icons-yellow)
    (jade-mode                          "seti-jade"              nerd-svg-icons-red)
    (pug-mode                           "seti-pug"           nerd-svg-icons-red)
    (image-mode                         "seti-image"             nerd-svg-icons-blue)
    (texinfo-mode                       "seti-tex"               nerd-svg-icons-lred)
    (markdown-mode                      "md-language_markdown"          nerd-svg-icons-lblue)
    (bibtex-mode                        "seti-tex"            nerd-svg-icons-maroon)
    (org-mode                           "custom-orgmode"               nerd-svg-icons-lgreen)
    (compilation-mode                   "md-cogs")
    (objc-mode                          "md-apple")
    (tuareg-mode                        "seti-ocaml")
    (purescript-mode                    "seti-purescript")
    (verilog-mode                       "oct-file_code"           nerd-svg-icons-red)
    (vhdl-mode                          "oct-file_code"              nerd-svg-icons-blue)
    (haskell-cabal-mode                 "oct-file_code"             nerd-svg-icons-lblue)
    (kotlin-mode                        "seti-kotlin"            nerd-svg-icons-orange)
    (nim-mode                           "oct-file_code"            nerd-svg-icons-yellow)
    (sql-mode                           "fa-database"          nerd-svg-icons-silver)
    (lua-mode                           "seti-lua"               nerd-svg-icons-dblue)
    (adoc-mode                          "oct-file_code"          nerd-svg-icons-lblue)
    (puppet-mode                        "seti-puppet"            nerd-svg-icons-yellow)
    (jinja2-mode                        "seti-jinja"             nerd-svg-icons-silver)
    (powershell-mode                    "md-powershell"        nerd-svg-icons-blue)
    (tex-mode                           "seti-tex"               nerd-svg-icons-lred)
    (latex-mode                         "seti-tex"               nerd-svg-icons-lred)
    (dart-mode                          "dev-dart"              nerd-svg-icons-blue)
    (fsharp-mode                        "dev-fsharp"            nerd-svg-icons-blue)
    (asm-mode                           "oct-file_code"  nerd-svg-icons-blue)
    (nasm-mode                          "oct-file_code"  nerd-svg-icons-blue)
    (tcl-mode                           "oct-file_code"               nerd-svg-icons-dred)
    (cuda-mode                          "oct-file_code"            nerd-svg-icons-green)
    (f90-mode                           "md-language_fortran"           nerd-svg-icons-purple)
    (hy-mode                            "oct-file_code"                nerd-svg-icons-blue)
    (glsl-mode                          "oct-file_code"      nerd-svg-icons-green)
    (zig-mode                           "oct-file_code"               nerd-svg-icons-orange)
    (odin-mode                          "oct-file_code"              nerd-svg-icons-lblue)
    (pdf-view-mode                      "seti-pdf"          nerd-svg-icons-dred)
    (elfeed-search-mode                 "fa-rss_square"        nerd-svg-icons-orange)
    (elfeed-show-mode                   "fa-rss"               nerd-svg-icons-orange)
    (lilypond-mode                      "md-music"             nerd-svg-icons-green)
    (magik-session-mode                 "oct-terminal"          nerd-svg-icons-blue)
    (magik-cb-mode                      "cod-book"              nerd-svg-icons-blue)
    (dashboard-mode                     "cod-dashboard"         nerd-svg-icons-orange)
    ))

(defvar nerd-svg-icons-symbol-kind-icon-alist
  '(
    ;; C, C++, java, python
    ("file"           "cod-symbol-file"               nerd-svg-icons-lpurple 0.95)
    ("function"       "cod-symbol_method"                  nerd-svg-icons-purple 0.95)
    ("method"         "cod-symbol_method"                  nerd-svg-icons-purple 0.95)
    ("prototype"      "cod-symbol_method"                  nerd-svg-icons-purple 0.95)
    ("annotation"     "cod-symbol_method"                  nerd-svg-icons-purple 0.95)
    ("constructor"    "cod-symbol_method"                  nerd-svg-icons-orange 0.95)
    ("class"          "cod-symbol_class"              nerd-svg-icons-lorange)
    ("struct"         "cod-symbol_class"              nerd-svg-icons-lorange)
    ("interface"      "cod-symbol_class"              nerd-svg-icons-lorange)
    ("union"          "cod-symbol_misc"               nerd-svg-icons-lorange 0.95)
    ("enum"           "cod-symbol_enum"         nerd-svg-icons-lorange)
    ("enumerator"     "cod-symbol_enum_member"  nerd-svg-icons-lblue 0.9)
    ("enummember"     "cod-symbol_enum_member"  nerd-svg-icons-lblue 0.9)
    ("using"          "cod-symbol_namespace"          nerd-svg-icons-dyellow)
    ("namespace"      "cod-symbol_namespace"          nerd-svg-icons-dyellow)
    ("variable"       "cod-symbol_field"                  nerd-svg-icons-lblue 0.95)
    ("member"         "cod-symbol_field"                  nerd-svg-icons-lblue 0.95)
    ("field"          "cod-symbol_field"                  nerd-svg-icons-lblue 0.95)
    ("externvar"      "cod-symbol_field"                  nerd-svg-icons-dorange 0.95)
    ("local"          "cod-symbol_variable"            nerd-svg-icons-dblue 1.1)
    ("macro"          "md-arrow_expand"                     nerd-svg-icons-purple 0.85)
    ("string"         "cod-symbol_string"             nerd-svg-icons-blue 0.9)
    ("boolean"        "cod-symbol_boolean"            nerd-svg-icons-lpurple 0.9)
    ("array"          "cod-symbol_array"              nerd-svg-icons-maroon 0.85)
    ("number"         "cod-symbol_numeric"            nerd-svg-icons-lgreen 0.85)
    ("object"         "cod-symbol_namespace"          nerd-svg-icons-lgreen 0.95)
    ("misc"           "cod-symbol_misc"               nerd-svg-icons-lgreen 0.95)
    ("operator"       "cod-symbol_operator"           nerd-svg-icons-orange 0.9)
    ("parameter"      "cod-symbol_parameter"          nerd-svg-icons-dpurple 1.1)
    ("macroparam"     "cod-symbol_parameter"          nerd-svg-icons-purple 1.1)
    ("typeparameter"  "cod-symbol_parameter"          nerd-svg-icons-lmaroon 1.1)
    ("tparam"         "cod-symbol_parameter"          nerd-svg-icons-lmaroon 1.1)
    ("event"          "cod-symbol_event"              nerd-svg-icons-yellow 0.95)
    ("typedef"        "cod-references"                nerd-svg-icons-lmaroon 0.8)
    ("package"        "cod-package"                   nerd-svg-icons-lblue 0.9)
    ("module"         "cod-package"                   nerd-svg-icons-lblue 0.9)
    ("key"            "cod-symbol_key"                nerd-svg-icons-dblue 1.05)
    ("null"           "weather-na"                     nerd-svg-icons-lmaroon 1.5)

    ;; Elisp
    ("derivedMode"  "md-cogs"                     nerd-svg-icons-purple 0.9)
    ("majorMode"    "md-cogs"                     nerd-svg-icons-purple 0.9)
    ("command"      "md-apple_keyboard_command"     nerd-svg-icons-purple 0.9)
    ("minorMode"    "md-cogs"                     nerd-svg-icons-purple 0.9)
    ("inline"       "cod-symbol_method"                  nerd-svg-icons-purple 0.95)
    ("subst"        "cod-symbol_method"                  nerd-svg-icons-purple 0.95)
    ("group"        "cod-package"                   nerd-svg-icons-lblue 0.9)
    ("error"        "cod-error"                     nerd-svg-icons-lblue)
    ("custom"       "seti-settings"                  nerd-svg-icons-orange)
    ("face"         "fae-palette_color"                     nerd-svg-icons-red)
    ("const"        "cod-symbol_constant"           nerd-svg-icons-lgreen)
    ("symbol"       "cod-symbol_key"             nerd-svg-icons-dyellow 0.9)
    ("alias"        "cod-references"                nerd-svg-icons-lmaroon 0.8)
    ("unknown"      "cod-question"           nerd-svg-icons-dyellow 0.9)

    ;; JavaScript, TypeScript
    ("constant"     "cod-symbol_constant"           nerd-svg-icons-lgreen)
    ("property"     "cod-symbol_property"           nerd-svg-icons-blue)

    ;; Markdown
    ("chapter"      "fa-sticky_note"               nerd-svg-icons-yellow)
    ("section"      "md-format_section"                   nerd-svg-icons-lorange 0.9)
    ("subsection"   "md-format_section"                   nerd-svg-icons-orange 0.8)

    ;; Org
    ("part"         "fa-pagelines"                 nerd-svg-icons-lmaroon)
    ))

(defvar nerd-svg-icons-default-mode-icon
  '("md-cogs" nerd-svg-icons-dsilver))

;; Function start ------------------------------------------------------------ ;

(defun nerd-svg-icons--match-to-alist (file alist)
  "Match FILE against an entry in ALIST using `string-match'."
  (assoc file alist (lambda (a b) (string-match a b))))

(defun nerd-svg-icons-dir-is-submodule (dir)
  "Checker whether or not DIR is a git submodule."
  (let* ((gitmodule-dir (locate-dominating-file dir ".gitmodules"))
         (modules-file  (expand-file-name (format "%s.gitmodules" gitmodule-dir)))
         (module-search (format "submodule \".*?%s\"" (file-name-base dir))))
    (when (and gitmodule-dir (file-exists-p (format "%s/.git" dir)))
      (with-temp-buffer
        (insert-file-contents modules-file)
        (search-forward-regexp module-search (point-max) t)))))

;;;###autoload
(defun nerd-svg-icons-icon-for-dir (dir &rest args)
  "Get the formatted icon for DIR.

ARGS should be a plist containining `:face' or `:scale'."
  (let ((path (expand-file-name dir)))
    (cond
     ((file-remote-p path)
      (apply #'nerd-svg-icons-icon-str "oct-terminal"
             (append args '(:face nerd-svg-icons-blue))))
     ((file-symlink-p path)
      (apply #'nerd-svg-icons-icon-str "md-folder_move"
             (append args '(:face nerd-svg-icons-blue))))
     ((nerd-svg-icons-dir-is-submodule path)
      (apply #'nerd-svg-icons-icon-str "md-folder_move"
             (append args '(:face nerd-svg-icons-blue))))
     ((file-exists-p (format "%s/.git" path))
      (apply #'nerd-svg-icons-icon-str "oct-repo"
             (append args '(:face nerd-svg-icons-blue))))
     (t
      (let* ((dir-name (file-name-base (directory-file-name dir)))
             (match (or (cdr (nerd-svg-icons--match-to-alist
                              dir-name
                              nerd-svg-icons-dir-regexp-icon-alist))
                        nerd-svg-icons-default-dir-icon))
             (icon-name (car match))
             (face (cadr match)))
        (apply #'nerd-svg-icons-icon-str icon-name
               (append args `(:face ,(or face 'nerd-svg-icons-blue)))))))))

;;;###autoload
(defun nerd-svg-icons-icon-for-str (str &rest args)
  "Get the formatted icon for STR.

ARGS should be a plist containining `:face' or `:scale'."
  (when-let ((match (nerd-svg-icons--match-to-alist
                     str nerd-svg-icons-regexp-icon-alist)))
    (apply #'nerd-svg-icons-icon-str (cadr match)
           (append args `(:face ,(caddr match))))))

;;;###autoload
(defun nerd-svg-icons-icon-for-file (file &rest args)
  "Get the formatted icon for FILE.

ARGS should be a plist containining `:face' or `:scale'."
  (let* ((ext (file-name-extension file))
         (match (or (cdr (nerd-svg-icons--match-to-alist
                          file nerd-svg-icons-regexp-icon-alist))
                    (and ext (cdr
                              (assoc (downcase ext)
                                     nerd-svg-icons-extension-icon-alist)))
                    nerd-svg-icons-default-file-icon)))
    (apply #'nerd-svg-icons-icon-str (car match)
           (append args `(:face ,(cadr match))))))

;;;###autoload
(defun nerd-svg-icons-icon-for-mode (mode &rest args)
  "Get the formatted icon for MODE.

ARGS should be a plist containining `:face' or `:scale'."
  (let* ((mode0 mode)
         (match (assoc mode0 nerd-svg-icons-mode-icon-alist)))
    (while (and mode0 (not match))
      (setq mode0 (get mode0 'derived-mode-parent))
      (setq match (assoc mode0 nerd-svg-icons-mode-icon-alist)))
    (if match
        (apply #'nerd-svg-icons-icon-str (cadr match)
               (append args `(:face ,(caddr match))))
      (apply #'nerd-svg-icons-icon-str "md-cogs"
             (append args '(:face nerd-svg-icons-purple))))))

;;;###autoload
(defun nerd-svg-icons-icon-for-symbol-kind (kind &rest args)
  "Get the formatted icon for symbol KIND.

ARGS should be a plist containining `:face' or `:scale'."
  (if-let* ((spec (cdr (assoc kind nerd-svg-icons-symbol-kind-icon-alist)))
            (icon-str (apply #'nerd-svg-icons-icon-str (car spec)
                             (append args
                                     `(:face ,(cadr spec))
                                     `(:scale ,(caddr spec)))))
            ((not (string-empty-p icon-str))))
      icon-str
    (nerd-svg-icons-icon-str "fa-tag" :face 'nerd-svg-icons-pink)))

;; Overriding all-the-icons -------------------------------------------------- ;

;;;###autoload
(define-minor-mode nerd-svg-icons-override-mode
  "Override `all-the-icons' functions with `nerd-svg-icons` ones."
  :global t
  (if nerd-svg-icons-override-mode
      (progn
        (require 'all-the-icons)
        (advice-add #'all-the-icons-alltheicon :override #'nerd-svg-icons-icon-str)
        (advice-add #'all-the-icons-fileicon :override #'nerd-svg-icons-icon-str)
        (advice-add #'all-the-icons-octicon :override #'nerd-svg-icons-icon-str)
        (advice-add #'all-the-icons-material :override #'nerd-svg-icons-icon-str)
        (advice-add #'all-the-icons-faicon :override #'nerd-svg-icons-icon-str)
        (advice-add #'all-the-icons-wicon :override #'nerd-svg-icons-icon-str))
    (advice-remove #'all-the-icons-alltheicon #'nerd-svg-icons-icon-str)
    (advice-remove #'all-the-icons-fileicon #'nerd-svg-icons-icon-str)
    (advice-remove #'all-the-icons-octicon #'nerd-svg-icons-icon-str)
    (advice-remove #'all-the-icons-material #'nerd-svg-icons-icon-str)
    (advice-remove #'all-the-icons-faicon #'nerd-svg-icons-icon-str)
    (advice-remove #'all-the-icons-wicon #'nerd-svg-icons-icon-str)))

(provide 'nerd-svg-icons)

;;; nerd-svg-icons.el ends here
