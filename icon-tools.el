;;; icon-tools.el --- Tools for icons in Emacs  -*- lexical-binding: t; -*-

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
(require 'icon-tools-faces)
(require 'icon-tools-data-nerd)

(defgroup icon-tools nil
  "Group for icon-tools."
  :group 'icon-tools)

(defvar icon-tools-svg-icon-dir
  (expand-file-name "svg" (file-name-directory load-file-name)))

(defvar icon-tools-icon-width 2)

(defvar icon-tools-svg-icon-cache
  (make-hash-table :test 'equal :size 250))

(defun icon-tools-svg-icon-cache-add (icon icon-name &rest args)
  (puthash (format "%s-%s-%s-%d-%d"
                   icon-name
                   (symbol-name (or (plist-get args :face) 'default))
                   (or (plist-get args :scale) 1.0)
                   (window-font-width)
                   (window-font-height))
           icon icon-tools-svg-icon-cache))

(defun icon-tools-svg-icon-cache-get (icon-name &rest args)
  (gethash (format "%s-%s-%s-%d-%d"
                   icon-name
                   (symbol-name (or (plist-get args :face) 'default))
                   (or (plist-get args :scale) 1.0)
                   (window-font-width)
                   (window-font-height))
           icon-tools-svg-icon-cache))

(defun icon-tools-svg-icon-filepath (icon-name)
  (concat (file-name-as-directory icon-tools-svg-icon-dir)
          icon-name ".svg"))

(defun icon-tools-svg-icon-parse (icon-name)
  (with-temp-buffer
    (insert-file-contents (icon-tools-svg-icon-filepath icon-name))
    (xml-parse-region (point-min) (point-max))))

(defun icon-tools--svg-icon-emacs-color-to-svg-color (color-name)
  "Convert Emacs COLOR-NAME to #rrggbb form.
If COLOR-NAME is unknown to Emacs, then return COLOR-NAME as-is."
  (let ((rgb-color (color-name-to-rgb color-name)))
    (if rgb-color
        (apply #'color-rgb-to-hex (append rgb-color '(2)))
      color-name)))

(defun icon-tools--svg-icon-alist-to-keyword-plist (alist)
  (cl-loop for (head . tail) in alist
           nconc (list (intern (concat ":" (symbol-name head))) tail)))

(defun icon-tools--svg-icon-recursively-copy-children (node1 node2 fg-color)
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
                                 (icon-tools--svg-icon-alist-to-keyword-plist attrs))))
            (icon-tools--svg-icon-recursively-copy-children node1-child child fg-color)))))))

(defvar icon-tools-svg-icon-scale-alist
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

(defvar icon-tools-svg-icon-base-scale 0.9)

(defun icon-tools--svg-icon-get-viewbox-multiplier (icon-name)
  (let ((cell (assoc icon-name icon-tools-svg-icon-scale-alist)))
    (if cell
        (/ 1 (* (cdr cell) icon-tools-svg-icon-base-scale))
      (/ 1 icon-tools-svg-icon-base-scale))))

(defun icon-tools--svg-icon-get-face-attribute-deep (face attribute)
  (when (facep face)
    (let ((face0 (face-attribute face :inherit))
          (val (face-attribute face attribute)))
      (while (and (facep face0) (eq val 'unspecified))
        (setq val (face-attribute face0 attribute))
        (setq face0 (face-attribute face0 :inherit)))
      val)))

(defun icon-tools-svg-icon (icon-name &rest args)
  "Build the icon ICON-NAME.

ARGS are additional plist arguments where properties FACE and
SCALE are supported.

Icon is drawn with the foreground of FACE and scaled with SCALE."

  (let ((cache-item (apply #'icon-tools-svg-icon-cache-get icon-name args)))
    (if cache-item
    ;; (if nil
        cache-item
      (let* ((face (plist-get args :face))
             (scale (plist-get args :scale))

             (root (icon-tools-svg-icon-parse icon-name))

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

             ;; Set icon size (in pixels) to `icon-tools-icon-width'x1 characters
             (svg-width  (* (window-font-width) icon-tools-icon-width))

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
                                (icon-tools--svg-icon-get-viewbox-multiplier icon-name))
                           (icon-tools--svg-icon-get-viewbox-multiplier icon-name)))
             (d-view-width (* (- multiplier 1) view-width))
             (view-x (- view-x (/ d-view-width 2)))
             (view-width (+ view-width d-view-width))
             (d-view-height (* (- multiplier 1) view-height))
             (view-y (- view-y (/ d-view-height 2)))
             (view-height (+ view-height d-view-height))

             (svg-viewbox (format "%f %f %f %f" view-x view-y view-width view-height))

             ;; Foreground and background
             (fg-color (icon-tools--svg-icon-get-face-attribute-deep face :foreground))
             (fg-color (icon-tools--svg-icon-emacs-color-to-svg-color
                        (or (when (facep fg-color)
                              (face-foreground fg-color nil t))
                            (when (not (eq fg-color 'unspecified)) fg-color)
                            (face-attribute 'default :foreground))))
             ;; Use only transparent background for now
             (bg-color "transparent")
             ;; (bg-color (icon-tools--svg-icon-get-face-attribute-deep face :background))
             ;; (bg-color (icon-tools--svg-icon-emacs-color-to-svg-color
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
        (icon-tools--svg-icon-recursively-copy-children svg (car root) fg-color)

        (apply #'icon-tools-svg-icon-cache-add (svg-image svg :ascent 'center :scale 1)
               icon-name args)))))

(defun icon-tools--get-nerd-icon-glyph (icon-name)
  "Return the glyph of nerd icon ICON-NAME.

ICON-NAME is a string in the form of FAMILY-ICON, e.g. fa-book."
  (let* ((splits (split-string icon-name "-"))
         (family (car splits))
         (name (concat "nf-" family "-" (cadr splits)))
         (alist (pcase family
                  ("cod" nerd-icons/codicon-alist)
                  ("dev" nerd-icons/devicon-alist)
                  ("fa" nerd-icons/faicon-alist)
                  ("fae" nerd-icons/faicon-alist)
                  ("linux" nerd-icons/flicon-alist)
                  ("iec" nerd-icons/ipsicon-alist)
                  ("md" nerd-icons/mdicon-alist)
                  ("oct" nerd-icons/octicon-alist)
                  ("pom" nerd-icons/pomicon-alist)
                  ("pl" nerd-icons/powerline-alist)
                  ("ple" nerd-icons/powerline-alist)
                  ("custom" nerd-icons/sucicon-alist)
                  ("seti" nerd-icons/sucicon-alist)
                  ("weather" nerd-icons/wicon-alist))))
    (cdr (assoc name alist))))

(defun icon-tools-svg-icon-str (icon-name &rest args)
  "Return the svg icon as string.

ICON-NAME is a string in the form of FAMILY-ICON, e.g. fa-book.

ARGS are additional plist arguments where properties FACE and SCALE are
supported."
  (if-let* ((glyph (icon-tools--get-nerd-icon-glyph icon-name))
            (codepoint (format "%x" (string-to-char glyph))))
      (propertize
       (make-string icon-tools-icon-width ?\-)
       'display (apply #'icon-tools-svg-icon codepoint args))))

(defun icon-tools-nerd-icon-str (icon-name &rest args)
  "Return the nerd icon as string.

ICON-NAME is a string in the form of FAMILY-ICON, e.g. fa-book.

ARGS are additional plist arguments where properties FACE and SCALE are
supported."
  (when-let ((glyph (icon-tools--get-nerd-icon-glyph icon-name)))
    (propertize (concat glyph " ") 'face `(:foreground
                              ,(face-attribute
                                (or (plist-get args :face) 'default)
                                :foreground)))
    ))

(defun icon-tools-icon-str (icon-name &rest args)
  (if (display-graphic-p)
      (apply #'icon-tools-svg-icon-str icon-name args)
    (apply #'icon-tools-nerd-icon-str icon-name args)))

;; Icon alists --------------------------------------------------------------- ;

(defvar icon-tools-extension-icon-alist
  '(
    ("fish"                 "oct-terminal"              icon-tools-lpink)
    ("zsh"                  "oct-terminal"              icon-tools-lcyan)
    ("sh"                   "oct-terminal"              icon-tools-purple)
    ("terminal"             "oct-terminal"              icon-tools-purple)
    ("bat"                  "oct-terminal"              icon-tools-purple)

    ;; Meta
    ("tags"                 "fa-tag"                   icon-tools-blue)
    ("tag"                  "fa-tag"                   icon-tools-blue)
    ("log"                  "fa-bug"                   icon-tools-maroon)
    ("aux"                  "fa-bug"                   icon-tools-maroon)
    ("nav"                  "fa-bug"                   icon-tools-maroon)
    ("snm"                  "fa-bug"                   icon-tools-maroon)
    ("toc"                  "fa-bug"                   icon-tools-maroon)
    ("vrb"                  "fa-bug"                   icon-tools-maroon)

    ;; binary
    ("exe"                  "oct-file_binary"           icon-tools-dsilver)
    ("dll"                  "md-cogs"                 icon-tools-dsilver)
    ("lib"                  "oct-file_binary"           icon-tools-dsilver)
    ("class"                "oct-file_binary"           icon-tools-dsilver)
    ("obj"                  "oct-file_binary"           icon-tools-dsilver)
    ("so"                   "oct-file_binary"           icon-tools-dsilver)
    ("o"                    "oct-file_binary"           icon-tools-dsilver)
    ("d"                    "oct-file_binary"           icon-tools-dsilver)
    ("out"                  "oct-file_binary"           icon-tools-dsilver)
    ("elc"                  "oct-file_binary"           icon-tools-dsilver)
    ("eln"                  "oct-file_binary"           icon-tools-dsilver)
    ("cmake-cache"          "oct-file_binary"           icon-tools-dsilver)
    ("csr"                  "oct-file_binary"           icon-tools-dsilver)
    ("eslintcache"          "oct-file_binary"           icon-tools-dsilver)
    ("cer"                  "oct-file_binary"           icon-tools-dsilver)
    ("der"                  "oct-file_binary"           icon-tools-dsilver)
    ("pfx"                  "oct-file_binary"           icon-tools-dsilver)
    ("p7b"                  "oct-file_binary"           icon-tools-dsilver)
    ("p7r"                  "oct-file_binary"           icon-tools-dsilver)
    ("DS_STORE"             "oct-file_binary"           icon-tools-dsilver)
    ("src"                  "oct-file_binary"           icon-tools-dsilver)
    ("crl"                  "oct-file_binary"           icon-tools-dsilver)
    ("sst"                  "oct-file_binary"           icon-tools-dsilver)
    ("stl"                  "oct-file_binary"           icon-tools-dsilver)
    ("pyc"                  "oct-file_binary"           icon-tools-dsilver)
    ("bin"                  "oct-file_binary"           icon-tools-dsilver)

    ;; torrent
    ("torrent"              "oct-download"       icon-tools-dsilver)
    ("aria2"                "oct-download"       icon-tools-dsilver)
    ("ed2k"                 "oct-download"       icon-tools-dsilver)

    ;; book
    ("azw"                  "cod-book"                  icon-tools-dsilver)
    ("azw3"                 "cod-book"                  icon-tools-dsilver)
    ("mobi"                 "cod-book"                  icon-tools-dsilver)
    ("epub"                 "cod-book"                  icon-tools-dsilver)

    ;; ?
    ("pkg"                  "cod-package"               icon-tools-dsilver)
    ("rpm"                  "cod-package"               icon-tools-dsilver)
    ("tar"                  "oct-file_zip"              icon-tools-lmaroon)
    ("rar"                  "oct-file_zip"              icon-tools-lmaroon)
    ("gz"                   "oct-file_zip"              icon-tools-lmaroon)
    ("zip"                  "oct-file_zip"              icon-tools-lmaroon)
    ("7z"                   "oct-file_zip"              icon-tools-lmaroon)
    ("xz"                   "oct-file_zip"              icon-tools-lmaroon)
    ("tgz"                  "oct-file_zip"              icon-tools-lmaroon)
    ("dat"                  "md-chart_bar"             icon-tools-cyan)
    ("edn"                  "md-chart_bar"             icon-tools-cyan)
    ("dmg"                  "cod-package"                 icon-tools-lsilver)
    ;; Source Codesode
    ("scpt"                 "md-apple"                 icon-tools-pink)
    ("aup"                  "oct-file_code"              icon-tools-yellow)
    ("elm"                  "seti-elm"                   icon-tools-blue)
    ("erl"                  "dev-erlang"                icon-tools-red)
    ("hrl"                  "dev-erlang"                icon-tools-dred)
    ("eex"                  "seti-elixir"                icon-tools-lorange)
    ("leex"                 "seti-elixir"                icon-tools-lorange)
    ("heex"                 "seti-elixir"                icon-tools-lorange)
    ("ex"                   "seti-elixir"                icon-tools-lpurple)
    ("exs"                  "seti-elixir"                icon-tools-lred)
    ("java"                 "fae-java"                  icon-tools-purple)
    ("jar"                  "fae-java"                  icon-tools-purple)
    ("gradle"               "seti-gradle"                icon-tools-silver)
    ("ebuild"               "md-gentoo"                icon-tools-cyan)
    ("eclass"               "md-gentoo"                icon-tools-blue)
    ("go"                   "custom-go"                    icon-tools-blue)
    ("jl"                   "seti-julia"                 icon-tools-purple)
    ("magik"                "fa-magic"   icon-tools-blue)
    ("matlab"               "md-math_compass"                icon-tools-orange)
    ("nix"                  "md-nix"                   icon-tools-blue)
    ("pl"                   "dev-perl"                  icon-tools-lorange)
    ("pm"                   "dev-perl"                  icon-tools-lorange)
    ("pod"                  "dev-perl"              icon-tools-lgreen)
    ("php"                  "seti-php"                   icon-tools-lsilver)
    ("pony"                 "oct-file_code"                  icon-tools-maroon)
    ("ps1"                  "md-powershell"            icon-tools-blue)
    ("pro"                  "dev-prolog"                icon-tools-lmaroon)
    ("proog"                "dev-prolog"                icon-tools-lmaroon)
    ("py"                   "seti-python"                icon-tools-dblue)
    ("py.typed"             "seti-python"                icon-tools-pink)
    ("idr"                  "oct-file_code"                 icon-tools-red)
    ("ipynb"                "oct-file_code"               icon-tools-dorange)
    ("gem"                  "cod-ruby"              icon-tools-red)
    ("rb"                   "oct-ruby"                  icon-tools-lred)
    ("rs"                   "seti-rust"                  icon-tools-maroon)
    ("rlib"                 "seti-rust"                  icon-tools-dmaroon)
    ("r"                    "seti-R"                     icon-tools-purple)
    ("rd"                   "seti-R"                     icon-tools-purple)
    ("rdx"                  "seti-R"                     icon-tools-purple)
    ("rsx"                  "seti-R"                     icon-tools-purple)
    ("svelte"               "seti-svelte"                icon-tools-red)
    ("gql"                  "seti-graphql"               icon-tools-dpink)
    ("graphql"              "seti-graphql"               icon-tools-dpink)
    ("c"                    "seti-c"                icon-tools-blue)
    ("h"                    "seti-c"                icon-tools-purple)
    ("h.in"                 "seti-c"                icon-tools-lblue)
    ("cc"                   "md-language_cpp"        icon-tools-blue)
    ("cpp"                  "md-language_cpp"        icon-tools-blue)
    ("cxx"                  "md-language_cpp"        icon-tools-blue)
    ("hh"                   "md-language_cpp"        icon-tools-purple)
    ("hpp"                  "md-language_cpp"        icon-tools-purple)
    ("hpp.in"               "md-language_cpp"        icon-tools-lblue)
    ("hxx"                  "md-language_cpp"        icon-tools-purple)
    ("m"                    "md-apple")
    ("mm"                   "md-apple")

    ;; Lisps
    ("cl"                   "oct-file_code"           icon-tools-lorange)
    ("l"                    "oct-file_code"                  icon-tools-orange)
    ("lisp"                 "oct-file_code"                  icon-tools-orange)
    ("hy"                   "oct-file_code"                    icon-tools-blue)
    ("el"                   "custom-emacs"                 icon-tools-purple)
    ("clj"                  "seti-clojure"           icon-tools-blue)
    ("cljc"                 "seti-clojure"           icon-tools-blue)
    ("cljs"                 "seti-clojure"             icon-tools-dblue)
    ("coffee"               "dev-coffeescript"          icon-tools-maroon)
    ("iced"                 "dev-coffeescript"          icon-tools-lmaroon)
    ("dart"                 "dev-dart"                  icon-tools-blue)
    ("rkt"                  "oct-file_code"                icon-tools-red)
    ("scrbl"                "oct-file_code"                icon-tools-blue)
    ;; Stylesheeting
    ("css"                  "md-language_css3"                  icon-tools-yellow)
    ("scss"                 "dev-sass"                  icon-tools-pink)
    ("sass"                 "dev-sass"                  icon-tools-dpink)
    ("less"                 "dev-sass"                  icon-tools-dyellow)
    ("postcss"              "md-language_css3"               icon-tools-dred)
    ("sss"                  "md-language_css3"               icon-tools-dred)
    ("styl"                 "dev-stylus"                icon-tools-lgreen)
    ("csv"                  "cod-graph_line"                 icon-tools-dblue)
    ;; haskell
    ("hs"                   "seti-haskell"               icon-tools-red)
    ("chs"                  "seti-haskell"               icon-tools-red)
    ("lhs"                  "seti-haskell"               icon-tools-red)
    ("hsc"                  "seti-haskell"               icon-tools-red)
    ;; Web modes
    ("inky-haml"            "fa-html5"                  icon-tools-lyellow)
    ("haml"                 "fa-html5"                  icon-tools-lyellow)
    ("htm"                  "fa-html5"                 icon-tools-orange)
    ("html"                 "fa-html5"                 icon-tools-orange)
    ("inky-er"              "fa-html5"                 icon-tools-lred)
    ("inky-erb"             "fa-html5"                 icon-tools-lred)
    ("erb"                  "fa-html5"                 icon-tools-lred)
    ("hbs"                  "oct-file_code"             icon-tools-green)
    ("inky-slim"            "cod-dashboard"             icon-tools-yellow)
    ("slim"                 "cod-dashboard"             icon-tools-yellow)
    ("jade"                 "seti-jade"                  icon-tools-red)
    ("pug"                  "seti-pug"               icon-tools-red)
    ;; Javascript
    ("d3js"                 "oct-file_code"                    icon-tools-lgreen)
    ("re"                   "seti-reasonml"                icon-tools-red-alt)
    ("rei"                  "seti-reasonml"                icon-tools-dred)
    ("ml"                   "seti-ocaml"                 icon-tools-lpink)
    ("mli"                  "seti-ocaml"                 icon-tools-dpink)
    ("react"                "md-react"                 icon-tools-lblue)
    ("ts"                   "seti-typescript"      icon-tools-blue-alt)
    ("js"                   "seti-javascript"      icon-tools-yellow)
    ("es"                   "seti-javascript"      icon-tools-yellow)
    ("jsx"                  "seti-javascript"               icon-tools-yellow)
    ("tsx"                  "seti-typescript"               icon-tools-blue-alt)
    ("njs"                  "md-nodejs"                icon-tools-lgreen)
    ("vue"                  "seti-vue"                   icon-tools-lgreen)

    ("sbt"                  "seti-sbt"                   icon-tools-red)
    ("scala"                "seti-scala"                 icon-tools-red)
    ("scm"                  "oct-file_code"                icon-tools-red)
    ("swift"                "seti-swift"                 icon-tools-green)

    ("tcl"                  "oct-file_code"                   icon-tools-dred)

    ("tf"                   "seti-terraform"             icon-tools-purple-alt)
    ("tfvars"               "seti-terraform"             icon-tools-purple-alt)
    ("tfstate"              "seti-terraform"             icon-tools-purple-alt)

    ("asm"                  "oct-file_code"      icon-tools-blue)
    ;; Verilog(-AMS) and SystemVerilog(-AMS)
    ("v"                    "oct-file_code"               icon-tools-red)
    ("vams"                 "oct-file_code"               icon-tools-red)
    ("sv"                   "oct-file_code"               icon-tools-red)
    ("sva"                  "oct-file_code"               icon-tools-red)
    ("svh"                  "oct-file_code"               icon-tools-red)
    ("svams"                "oct-file_code"               icon-tools-red)
    ;; VHDL(-AMS)
    ("vhd"                  "oct-file_code"                  icon-tools-blue)
    ("vhdl"                 "oct-file_code"                  icon-tools-blue)
    ("vhms"                 "oct-file_code"                  icon-tools-blue)
    ;; Cabal
    ("cabal"                "oct-file_code"                 icon-tools-lblue)
    ;; Kotlin
    ("kt"                   "seti-kotlin"                icon-tools-orange)
    ("kts"                  "seti-kotlin"                icon-tools-orange)
    ;; Nimrod
    ("nim"                  "seti-nim"                icon-tools-yellow)
    ("nims"                 "seti-nim"                icon-tools-yellow)
    ;; SQL
    ("sql"                  "fa-database"              icon-tools-silver)
    ("db"                   "fa-database"              icon-tools-silver)
    ("cache"                "fa-database"              icon-tools-green)
    ;; Styles
    ("styles"               "oct-file_code"                 icon-tools-red)
    ;; Lua
    ("lua"                  "seti-lua"                   icon-tools-dblue)
    ;; ASCII doc
    ("adoc"                 "oct-file_code"              icon-tools-lblue)
    ("asciidoc"             "oct-file_code"              icon-tools-lblue)
    ;; Puppet
    ("pp"                   "seti-puppet"                icon-tools-yellow)
    ;; Jinja
    ("j2"                   "seti-jinja"                 icon-tools-silver)
    ("jinja2"               "seti-jinja"                 icon-tools-silver)
    ;; Docker
    ("dockerfile"           "seti-docker"                icon-tools-cyan)
    ;; Vagrant
    ("vagrantfile"          "oct-file_code"               icon-tools-blue)
    ;; GLSL
    ("glsl"                 "oct-file_code"          icon-tools-blue)
    ("vert"                 "oct-file_code"          icon-tools-blue)
    ("tesc"                 "oct-file_code"          icon-tools-purple)
    ("tese"                 "oct-file_code"          icon-tools-dpurple)
    ("geom"                 "oct-file_code"          icon-tools-green)
    ("frag"                 "oct-file_code"          icon-tools-red)
    ("comp"                 "oct-file_code"          icon-tools-dblue)
    ;; CUDA
    ("cu"                   "oct-file_code"          icon-tools-green)
    ("cuh"                  "oct-file_code"          icon-tools-green)
    ;; Fortran
    ("f90"                  "md-language_fortran"               icon-tools-purple)
    ;; C#
    ("cs"                   "md-language_csharp"           icon-tools-dblue)
    ("csx"                  "md-language_csharp"           icon-tools-dblue)
    ;; F#
    ("fs"                   "dev-fsharp"                icon-tools-blue-alt)
    ("fsi"                  "dev-fsharp"                icon-tools-blue-alt)
    ("fsx"                  "dev-fsharp"                icon-tools-blue-alt)
    ("fsscript"             "dev-fsharp"                icon-tools-blue-alt)
    ;; zig
    ("zig"                  "seti-zig"                   icon-tools-orange)
    ;; odin
    ("odin"                 "oct-file_code"                  icon-tools-lblue)
    ;; File Types
    ("ico"                  "seti-image"            icon-tools-blue)
    ("png"                  "seti-image"            icon-tools-orange)
    ("gif"                  "seti-image"            icon-tools-green)
    ("jpeg"                 "seti-image"            icon-tools-dblue)
    ("jpg"                  "seti-image"            icon-tools-dblue)
    ("webp"                 "seti-image"            icon-tools-dblue)
    ("svg"                  "seti-image"            icon-tools-lgreen)
    ("eps"                  "seti-image"            icon-tools-lgreen)
    ;; Audio
    ("mp3"                  "md-music"                 icon-tools-dred)
    ("wav"                  "md-music"                 icon-tools-dred)
    ("m4a"                  "md-music"                 icon-tools-dred)
    ("ogg"                  "md-music"                 icon-tools-dred)
    ("flac"                 "md-music"                 icon-tools-dred)
    ("opus"                 "md-music"                 icon-tools-dred)
    ("au"                   "md-music"                 icon-tools-dred)
    ("aif"                  "md-music"                 icon-tools-dred)
    ("aifc"                 "md-music"                 icon-tools-dred)
    ("aiff"                 "md-music"                 icon-tools-dred)
    ("ly"                   "md-music"                 icon-tools-green)
    ;; Video
    ("mov"                  "md-movie_open_outline"                  icon-tools-blue)
    ("mp4"                  "md-movie_open_outline"                  icon-tools-blue)
    ("ogv"                  "md-movie_open_outline"                  icon-tools-dblue)
    ("mpg"                  "md-movie_open_outline"                  icon-tools-blue)
    ("mpeg"                 "md-movie_open_outline"                  icon-tools-blue)
    ("flv"                  "md-movie_open_outline"                  icon-tools-blue)
    ("ogv"                  "md-movie_open_outline"                  icon-tools-dblue)
    ("mkv"                  "md-movie_open_outline"                  icon-tools-blue)
    ("webm"                 "md-movie_open_outline"                  icon-tools-blue)
    ;; Subtitle
    ("srt"                  "md-closed_caption_outline"        icon-tools-dblue)
    ;; Fonts
    ("ttf"                  "seti-font"                  icon-tools-dcyan)
    ("otf"                  "seti-font"                  icon-tools-dcyan)
    ("woff"                 "seti-font"                  icon-tools-cyan)
    ("woff2"                "seti-font"                  icon-tools-cyan)
    ("ttc"                  "seti-font"                  icon-tools-cyan)
    ;; Doc
    ("pdf"                  "seti-pdf"              icon-tools-dred)
    ("text"                 "md-file_document"             icon-tools-cyan)
    ("txt"                  "fa-edit"                  icon-tools-cyan)
    ("rst"                  "md-text_box_edit"      icon-tools-green)
    ("doc"                  "md-microsoft_word"        icon-tools-blue)
    ("docx"                 "md-microsoft_word"        icon-tools-blue)
    ("docm"                 "md-microsoft_word"        icon-tools-blue)
    ("texi"                 "seti-tex"                   icon-tools-lred)
    ("tex"                  "seti-tex"                   icon-tools-lred)
    ("sty"                  "seti-tex"                   icon-tools-lred)
    ("pygtex"               "seti-tex"                   icon-tools-pink)
    ("pygstyle"             "seti-tex"                   icon-tools-pink)
    ("md"                   "md-language_markdown"              icon-tools-lblue)
    ("bib"                  "seti-tex"                icon-tools-maroon)
    ("org"                  "custom-orgmode"              icon-tools-lgreen)
    ("pps"                  "md-microsoft_powerpoint"  icon-tools-orange)
    ("ppt"                  "md-microsoft_powerpoint"  icon-tools-orange)
    ("pptsx"                "md-microsoft_powerpoint"  icon-tools-orange)
    ("ppttx"                "md-microsoft_powerpoint"  icon-tools-orange)
    ("knt"                  "md-microsoft_powerpoint"  icon-tools-cyan)
    ("xlsx"                 "md-microsoft_excel"       icon-tools-dgreen)
    ("xlsm"                 "md-microsoft_excel"       icon-tools-dgreen)
    ("xlsb"                 "md-microsoft_excel"       icon-tools-dgreen)
    ("xltx"                 "md-microsoft_excel"       icon-tools-dgreen)
    ("xltm"                 "md-microsoft_excel"       icon-tools-dgreen)
    ;; key and licence
    ("key"                  "cod-key"                   icon-tools-lblue)
    ("pem"                  "cod-key"                   icon-tools-orange)
    ("p12"                  "cod-key"                   icon-tools-dorange)
    ("crt"                  "cod-key"                   icon-tools-lblue)
    ("pub"                  "cod-key"                   icon-tools-blue)
    ("gpg"                  "cod-key"                   icon-tools-lblue)
    ("license.md"           "cod-key"                   icon-tools-purple)
    ("license"              "cod-key"                   icon-tools-purple)
    ("lic"                  "cod-key"                   icon-tools-dblue)
    ("gemfile"              "cod-key"                   icon-tools-dblue)
    ("bookmarks"            "fa-bookmark"             icon-tools-orange)

    ;; Config
    ("node"                 "md-nodejs"                icon-tools-green)
    ("babelrc"              "seti-babel"                 icon-tools-yellow)
    ("bashrc"               "md-script_text"                icon-tools-dpink)
    ("bowerrc"              "seti-bower"                 icon-tools-silver)
    ("cr"                   "seti-crystal"               icon-tools-yellow)
    ("ecr"                  "seti-crystal"               icon-tools-yellow)
    ("ini"                  "md-cogs"                  icon-tools-yellow)
    ("eslintignore"         "seti-eslint"                icon-tools-purple)
    ("eslint"               "seti-eslint"                icon-tools-lpurple)
    ("git"                  "seti-git"                   icon-tools-lred)
    ("mk"                   "dev-gnu"                   icon-tools-dorange)
    ("clang"                "md-cogs"                  icon-tools-dpurple)
    ("llvm"                 "md-cogs"                  icon-tools-dpurple)
    ("clangd"               "md-cogs"                  icon-tools-dpurple)
    ("cmake"                "md-cogs"                 icon-tools-red)
    ("cmakelists.txt"       "md-cogs"                 icon-tools-red)
    ("ninja"                "md-ninja")
    ("makefile"             "seti-makefile"              icon-tools-cyan)
    ("dockerignore"         "seti-docker"                icon-tools-dblue)
    ("xml"                  "cod-code"                  icon-tools-lorange)
    ("json"                 "seti-settings"              icon-tools-yellow)
    ("clang-format"         "seti-settings"              icon-tools-yellow)
    ("cson"                 "md-cogs"                  icon-tools-yellow)
    ("yml"                  "md-cogs"                  icon-tools-dyellow)
    ("yaml"                 "md-cogs"                  icon-tools-dyellow)
    ("toml"                 "md-cogs"                  icon-tools-pink)
    ("cfg"                  "md-cogs"                  icon-tools-dblue)
    ("terminfo"             "md-cogs"                  icon-tools-dblue)
    ("settings.json"        "md-cogs"                  icon-tools-dblue)
    ("Vagrantfile"          "md-cogs"                  icon-tools-silver)
    ("babel.config.js"      "md-cogs"                  icon-tools-silver)
    ("babelignore"          "md-cogs"                  icon-tools-silver)
    ("babelrc"              "md-cogs"                  icon-tools-silver)
    ("babelrc.js"           "md-cogs"                  icon-tools-silver)
    ("babelrc.json"         "md-cogs"                  icon-tools-silver)
    ("bashrc"               "md-cogs"                  icon-tools-silver)
    ("bazel"                "md-cogs"                  icon-tools-silver)
    ("bazelrc"              "md-cogs"                  icon-tools-silver)
    ("bower.json"           "md-cogs"                  icon-tools-silver)
    ("bowerrc"              "md-cogs"                  icon-tools-silver)
    ("cabal"                "md-cogs"                  icon-tools-silver)
    ("cfg"                  "md-cogs"                  icon-tools-silver)
    ("conf"                 "md-cogs"                  icon-tools-silver)
    ("config"               "md-cogs"                  icon-tools-silver)
    ("cson"                 "md-cogs"                  icon-tools-silver)
    ("editorconfig"         "md-cogs"                  icon-tools-silver)
    ("envrc"                "md-cogs"                  icon-tools-silver)
    ("eslintignore"         "md-cogs"                  icon-tools-silver)
    ("eslintrc"             "md-cogs"                  icon-tools-silver)
    ("feature"              "md-cogs"                  icon-tools-silver)
    ("gemfile"              "md-cogs"                  icon-tools-silver)
    ("gitattributes"        "md-cogs"                  icon-tools-silver)
    ("gitconfig"            "md-cogs"                  icon-tools-silver)
    ("gitignore"            "md-cogs"                  icon-tools-silver)
    ("gitmodules"           "md-cogs"                  icon-tools-silver)
    ("ideavimrc"            "md-cogs"                  icon-tools-silver)
    ("iml"                  "md-cogs"                  icon-tools-silver)
    ("ini"                  "md-cogs"                  icon-tools-silver)
    ("inputrc"              "md-cogs"                  icon-tools-silver)
    ("ledgerrc"             "md-cogs"                  icon-tools-silver)
    ("lock"                 "md-cogs"                  icon-tools-silver)
    ("nginx"                "md-cogs"                  icon-tools-silver)
    ("npm-shrinkwrap.json"  "md-cogs"                  icon-tools-silver)
    ("npmignore"            "md-cogs"                  icon-tools-silver)
    ("npmrc"                "md-cogs"                  icon-tools-silver)
    ("package-lock.json"    "md-cogs"                  icon-tools-silver)
    ("package.json"         "md-cogs"                  icon-tools-silver)
    ("phpunit"              "md-cogs"                  icon-tools-silver)
    ("pkg"                  "md-cogs"                  icon-tools-silver)
    ("plist"                "md-cogs"                  icon-tools-silver)
    ("properties"           "md-cogs"                  icon-tools-silver)
    ("terminalrc"           "md-cogs"                  icon-tools-silver)
    ("tridactylrc"          "md-cogs"                  icon-tools-silver)
    ("vimperatorrc"         "md-cogs"                  icon-tools-silver)
    ("vimrc"                "md-cogs"                  icon-tools-silver)
    ("vrapperrc"            "md-cogs"                  icon-tools-silver)
    ("xdefaults"            "md-cogs"                  icon-tools-silver)
    ("xml"                  "md-cogs"                  icon-tools-silver)
    ("xresources"           "md-cogs"                  icon-tools-silver)
    ("yaml"                 "md-cogs"                  icon-tools-silver)
    ("yarn-integrity"       "md-cogs"                  icon-tools-silver)
    ("yarnclean"            "md-cogs"                  icon-tools-silver)
    ("yarnignore"           "md-cogs"                  icon-tools-silver)
    ("yarnrc"               "md-cogs"                  icon-tools-silver)
    ("rc"                   "md-cogs"                  icon-tools-silver)
    ("project"              "md-cogs"                  icon-tools-silver)
    ("prefs"                "md-cogs"                  icon-tools-silver)
    ("sln"                  "md-microsoft_visual_studio"          icon-tools-blue)
    ("vcxproj"              "md-microsoft_visual_studio"          icon-tools-blue)
    ("vcproj"               "md-microsoft_visual_studio"          icon-tools-blue)

    ;; model
    ("pth"                  "cod-package"               icon-tools-dsilver)
    ("ckpt"                 "cod-package"               icon-tools-dsilver)
    ("model"                "cod-package"               icon-tools-dsilver)

    ;; whl
    ("whl"                  "cod-package"               icon-tools-purple-alt)
    ))

(defvar icon-tools-regexp-icon-alist
  '(
    ;;
    ("^TAGS$"                   "fa-tag"             icon-tools-blue)
    ("^TAG$"                    "fa-tag"             icon-tools-blue)
    ("^TODO$"                   "oct-checklist"       icon-tools-lyellow)
    ("^LICENSE$"                "cod-book"            icon-tools-blue)
    ("^readme.md$"              "md-language_markdown"        icon-tools-lblue)
    ("^readme"                  "cod-book"            icon-tools-lcyan)
    ("help"                     "md-information_outline"            icon-tools-purple)
    ("info"                     "md-information_outline"            icon-tools-pink)

    ;; Config
    ("nginx$"                   "dev-nginx"           icon-tools-dgreen)
    ("apache$"                  "md-apache_kafka"          icon-tools-dgreen)

    ;; C
    ("^Makefile$"               "seti-makefile"        icon-tools-dorange)
    ("^CMakeLists.txt$"         "md-cogs"           icon-tools-red)
    ("^CMakeCache.txt$"         "md-cogs"           icon-tools-blue)
    ("cmake"                    "md-cogs"           icon-tools-red)

    ;; Visual Studio
    ("vcxproj"                  "md-microsoft_visual_studio"    icon-tools-blue)
    ("vcproj"                   "md-microsoft_visual_studio"    icon-tools-blue)

    ;; Docker
    ("^\\.?Dockerfile"          "seti-docker"          icon-tools-blue)

    ;; Homebrew
    ("^Brewfile$"               "fa-beer"        icon-tools-lsilver)

    ;; ;; AWS
    ("^stack.*.json$"           "md-aws"             icon-tools-orange)
    ("^serverless\\.yml$"       "fa-bolt"            icon-tools-yellow)

    ;; lock files
    ("~$"                       "md-file_lock"            icon-tools-maroon)

    ;; Source Codes
    ("^mix.lock$"               "seti-elixir"          icon-tools-lyellow)

    ;; Ruby
    ("^Gemfile\\(\\.lock\\)?$"  "cod-ruby"     icon-tools-red)
    ("_?test\\.rb$"             "oct-ruby"       icon-tools-red)
    ("_?test_helper\\.rb$"      "oct-ruby"       icon-tools-dred)
    ("_?spec\\.rb$"             "oct-ruby"       icon-tools-red)
    ("_?spec_helper\\.rb$"      "oct-ruby"       icon-tools-dred)

    ("-?spec\\.ts$"             "seti-typescript" icon-tools-blue)
    ("-?test\\.ts$"             "seti-typescript" icon-tools-blue)
    ("-?spec\\.js$"             "seti-javascript"         icon-tools-lpurple)
    ("-?test\\.js$"             "seti-javascript"         icon-tools-lpurple)
    ("-?spec\\.jsx$"            "md-react"      icon-tools-blue-alt)
    ("-?test\\.jsx$"            "md-react"      icon-tools-blue-alt)

    ;; Git
    ("^MERGE_"                  "oct-git_merge"       icon-tools-red)
    ("^COMMIT_EDITMSG"          "oct-git_commit"      icon-tools-red)

    ;; Stylesheeting
    ("stylelint"                "seti-stylelint"       icon-tools-lyellow)

    ;; JavaScript
    ("^package.json$"           "seti-npm"             icon-tools-red)
    ("^package.lock.json$"      "seti-npm"             icon-tools-dred)
    ("^yarn\\.lock"             "seti-yarn"            icon-tools-blue-alt)
    ("\\.npmignore$"            "seti-npm"             icon-tools-dred)
    ("^bower.json$"             "seti-bower"           icon-tools-lorange)
    ("^gulpfile"                "seti-gulp"            icon-tools-lred)
    ("^gruntfile"               "seti-grunt"           icon-tools-lyellow)
    ("^webpack"                 "seti-webpack"         icon-tools-lblue)

    ;; Go
    ("^go.mod$"                 "seti-go"       icon-tools-blue-alt)
    ("^go.work$"                "seti-go"       icon-tools-blue-alt)

    ;; Emacs
    ("bookmarks"                "fa-bookmark"       icon-tools-orange)
    ("bookmark"                 "fa-bookmark"        icon-tools-orange)

    ("^\\*scratch\\*$"          "fa-sticky_note"     icon-tools-lyellow)
    ("^\\*scratch.*"            "fa-sticky_note"     icon-tools-yellow)
    ("^\\*new-tab\\*$"          "fa-star"            icon-tools-cyan)

    ("\\.git"                   "seti-git"             icon-tools-yellow)

    ("^\\."                     "md-file_hidden")
    ))

(defvar icon-tools-default-file-icon
  '("md-file" icon-tools-dsilver))

(defvar icon-tools-dir-regexp-icon-alist
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

(defvar icon-tools-default-dir-icon
  '("md-folder" icon-tools-dsilver))

(defvar icon-tools-weather-icon-alist
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

(defvar icon-tools-mode-icon-alist
  '(
    (emacs-lisp-mode                    "custom-emacs"             icon-tools-purple)
    (circe-server-mode                  "fa-commenting_o")
    (circe-channel-mode                 "fa-commenting_o")
    (crystal-mode                       "seti-crystal"           icon-tools-yellow)
    (erc-mode                           "fa-commenting_o")
    (inferior-emacs-lisp-mode           "custom-emacs"             icon-tools-lblue)
    (dired-mode                         "md-folder_multiple")
    (lisp-interaction-mode              "oct-file_code"              icon-tools-orange)
    (sly-mrepl-mode                     "oct-file_code"       icon-tools-orange)
    (slime-repl-mode                    "oct-file_code"       icon-tools-orange)
    (org-mode                           "custom-orgmode"          icon-tools-lgreen)
    (typescript-mode                    "seti-typescript"  icon-tools-blue-alt)
    (react-mode                         "md-react"             icon-tools-lblue)
    (js-mode                            "seti-javascript"  icon-tools-yellow)
    (js-jsx-mode                        "md-react"             icon-tools-yellow)
    (js2-mode                           "seti-javascript"  icon-tools-yellow)
    (js3-mode                           "seti-javascript"  icon-tools-yellow)
    (rjsx-mode                          "md-react"             icon-tools-cyan-alt)
    (term-mode                          "oct-terminal")
    (vterm-mode                         "oct-terminal")
    (eshell-mode                        "oct-terminal"          icon-tools-purple)
    (magit-refs-mode                    "oct-git_branch"        icon-tools-red)
    (magit-process-mode                 "seti-github")
    (magit-diff-mode                    "oct-git_compare"       icon-tools-lblue)
    (ediff-mode                         "oct-git_compare"       icon-tools-red)
    (diff-mode                          "oct-git_compare"       icon-tools-purple)
    (comint-mode                        "oct-terminal"          icon-tools-lblue)
    (eww-mode                           "md-firefox"           icon-tools-red)
    (org-agenda-mode                    "oct-checklist"         icon-tools-lgreen)
    (cfw:calendar-mode                  "md-calendar_check")
    (ibuffer-mode                       "fa-files_o"             icon-tools-dsilver)
    (messages-buffer-mode               "md-message_text_outline"           icon-tools-dsilver)
    (help-mode                          "md-information_outline"              icon-tools-purple)
    (Info-mode                          "md-information_outline"              icon-tools-pink)
    (benchmark-init/tree-mode           "cod-dashboard")
    (jenkins-mode                       "seti-jenkins"           icon-tools-blue)
    (magit-popup-mode                   "seti-git"               icon-tools-red)
    (magit-status-mode                  "seti-git"               icon-tools-lred)
    (magit-log-mode                     "seti-git"               icon-tools-green)
    (mu4e-compose-mode                  "md-pencil")
    (mu4e-headers-mode                  "cod-mail")
    (mu4e-main-mode                     "cod-mail")
    (mu4e-view-mode                     "cod-mail_read")
    (package-menu-mode                  "md-package_variant_plus"           icon-tools-silver)
    (paradox-menu-mode                  "md-archive"           icon-tools-silver)
    (Custom-mode                        "seti-settings")
    (web-mode                           "seti-webpack"       icon-tools-purple)
    (fundamental-mode                   "md-file_document"         icon-tools-dsilver)
    (special-mode                       "md-information_outline"              icon-tools-yellow)
    (text-mode                          "md-file_document"         icon-tools-cyan)
    (enh-ruby-mode                      "oct-ruby"              icon-tools-lred)
    (ruby-mode                          "oct-ruby"              icon-tools-lred)
    (inf-ruby-mode                      "oct-ruby"              icon-tools-red)
    (projectile-rails-compilation-mode  "oct-ruby"              icon-tools-red)
    (rspec-compilation-mode             "oct-ruby"              icon-tools-red)
    (rake-compilation-mode              "oct-ruby"              icon-tools-red)
    (sh-mode                            "oct-terminal"          icon-tools-purple)
    (shell-mode                         "oct-terminal"          icon-tools-purple)
    (fish-mode                          "oct-terminal"          icon-tools-lpink)
    (nginx-mode                         "dev-nginx"             icon-tools-dgreen)
    (apache-mode                        "md-apache_kafka"            icon-tools-dgreen)
    (makefile-mode                      "seti-makefile"          icon-tools-dorange)
    (cmake-mode                         "md-cogs"             icon-tools-red)
    (dockerfile-mode                    "seti-docker"            icon-tools-blue)
    (docker-compose-mode                "seti-docker"            icon-tools-lblue)
    (nxml-mode                          "cod-code"              icon-tools-lorange)
    (json-mode                          "seti-settings"          icon-tools-yellow)
    (jsonian-mode                       "seti-settings"          icon-tools-yellow)
    (yaml-mode                          "seti-settings"          icon-tools-dyellow)
    (elisp-byte-code-mode               "oct-file_binary"            icon-tools-dsilver)
    (archive-mode                       "oct-archive"          icon-tools-lmaroon)
    (elm-mode                           "seti-elm"               icon-tools-blue)
    (erlang-mode                        "dev-erlang"            icon-tools-red)
    (elixir-mode                        "seti-elixir"            icon-tools-lorange)
    (java-mode                          "fae-java"              icon-tools-purple)
    (go-mode                            "custom-go"                icon-tools-blue)
    (go-dot-mod-mode                    "seti-go"         icon-tools-blue-alt)
    (go-dot-work-mode                   "seti-go"         icon-tools-blue-alt)
    (graphql-mode                       "seti-graphql"           icon-tools-dpink)
    (matlab-mode                        "md-math_compass"            icon-tools-orange)
    (nix-mode                           "md-nix"               icon-tools-blue)
    (perl-mode                          "dev-perl"              icon-tools-lorange)
    (cperl-mode                         "dev-perl"              icon-tools-lorange)
    (php-mode                           "seti-php"               icon-tools-lsilver)
    (prolog-mode                        "dev-prolog"            icon-tools-lmaroon)
    (python-mode                        "seti-python"            icon-tools-dblue)
    (inferior-python-mode               "seti-python"            icon-tools-dblue)
    (racket-mode                        "oct-file_code"            icon-tools-red)
    (rust-mode                          "seti-rust"              icon-tools-maroon)
    (scala-mode                         "seti-scala"             icon-tools-red)
    (scheme-mode                        "oct-file_code"            icon-tools-red)
    (swift-mode                         "seti-swift"             icon-tools-green)
    (svelte-mode                        "seti-svelte"            icon-tools-red)
    (c-mode                             "seti-c"            icon-tools-blue)
    (c++-mode                           "md-language_cpp"    icon-tools-blue)
    (csharp-mode                        "md-language_csharp"       icon-tools-dblue)
    (clojure-mode                       "seti-clojure"       icon-tools-blue)
    (cider-repl-mode                    "seti-clojure"       icon-tools-green)
    (clojurescript-mode                 "seti-clojure"         icon-tools-dblue)
    (coffee-mode                        "dev-coffeescript"      icon-tools-maroon)
    (lisp-mode                          "oct-file_code"              icon-tools-orange)
    (css-mode                           "md-language_css3"              icon-tools-yellow)
    (scss-mode                          "dev-sass"              icon-tools-pink)
    (sass-mode                          "dev-sass"              icon-tools-dpink)
    (less-css-mode                      "dev-sass"              icon-tools-dyellow)
    (stylus-mode                        "dev-stylus"            icon-tools-lgreen)
    (csv-mode                           "cod-graph_line"             icon-tools-dblue)
    (haskell-mode                       "seti-haskell"           icon-tools-red)
    (haskell-c2hs-mode                  "seti-haskell"           icon-tools-red)
    (literate-haskell-mode              "seti-haskell"           icon-tools-red)
    (haml-mode                          "fa-html5"              icon-tools-lyellow)
    (html-mode                          "fa-html5"             icon-tools-orange)
    (rhtml-mode                         "fa-html5"             icon-tools-lred)
    (mustache-mode                      "oct-file_code"         icon-tools-green)
    (slim-mode                          "cod-dashboard"         icon-tools-yellow)
    (jade-mode                          "seti-jade"              icon-tools-red)
    (pug-mode                           "seti-pug"           icon-tools-red)
    (image-mode                         "seti-image"             icon-tools-blue)
    (texinfo-mode                       "seti-tex"               icon-tools-lred)
    (markdown-mode                      "md-language_markdown"          icon-tools-lblue)
    (bibtex-mode                        "seti-tex"            icon-tools-maroon)
    (org-mode                           "custom-orgmode"               icon-tools-lgreen)
    (compilation-mode                   "md-cogs")
    (objc-mode                          "md-apple")
    (tuareg-mode                        "seti-ocaml")
    (purescript-mode                    "seti-purescript")
    (verilog-mode                       "oct-file_code"           icon-tools-red)
    (vhdl-mode                          "oct-file_code"              icon-tools-blue)
    (haskell-cabal-mode                 "oct-file_code"             icon-tools-lblue)
    (kotlin-mode                        "seti-kotlin"            icon-tools-orange)
    (nim-mode                           "oct-file_code"            icon-tools-yellow)
    (sql-mode                           "fa-database"          icon-tools-silver)
    (lua-mode                           "seti-lua"               icon-tools-dblue)
    (adoc-mode                          "oct-file_code"          icon-tools-lblue)
    (puppet-mode                        "seti-puppet"            icon-tools-yellow)
    (jinja2-mode                        "seti-jinja"             icon-tools-silver)
    (powershell-mode                    "md-powershell"        icon-tools-blue)
    (tex-mode                           "seti-tex"               icon-tools-lred)
    (latex-mode                         "seti-tex"               icon-tools-lred)
    (dart-mode                          "dev-dart"              icon-tools-blue)
    (fsharp-mode                        "dev-fsharp"            icon-tools-blue)
    (asm-mode                           "oct-file_code"  icon-tools-blue)
    (nasm-mode                          "oct-file_code"  icon-tools-blue)
    (tcl-mode                           "oct-file_code"               icon-tools-dred)
    (cuda-mode                          "oct-file_code"            icon-tools-green)
    (f90-mode                           "md-language_fortran"           icon-tools-purple)
    (hy-mode                            "oct-file_code"                icon-tools-blue)
    (glsl-mode                          "oct-file_code"      icon-tools-green)
    (zig-mode                           "oct-file_code"               icon-tools-orange)
    (odin-mode                          "oct-file_code"              icon-tools-lblue)
    (pdf-view-mode                      "seti-pdf"          icon-tools-dred)
    (elfeed-search-mode                 "fa-rss_square"        icon-tools-orange)
    (elfeed-show-mode                   "fa-rss"               icon-tools-orange)
    (lilypond-mode                      "md-music"             icon-tools-green)
    (magik-session-mode                 "oct-terminal"          icon-tools-blue)
    (magik-cb-mode                      "cod-book"              icon-tools-blue)
    (dashboard-mode                     "cod-dashboard"         icon-tools-orange)
    ))

(defvar icon-tools-symbol-kind-icon-alist
  '(
    ;; C, C++, java, python
    ("file"           "cod-symbol-file"               icon-tools-lpurple 0.95)
    ("function"       "cod-symbol_method"                  icon-tools-purple 0.95)
    ("method"         "cod-symbol_method"                  icon-tools-purple 0.95)
    ("prototype"      "cod-symbol_method"                  icon-tools-purple 0.95)
    ("annotation"     "cod-symbol_method"                  icon-tools-purple 0.95)
    ("constructor"    "cod-symbol_method"                  icon-tools-orange 0.95)
    ("class"          "cod-symbol_class"              icon-tools-lorange)
    ("struct"         "cod-symbol_class"              icon-tools-lorange)
    ("interface"      "cod-symbol_class"              icon-tools-lorange)
    ("union"          "cod-symbol_misc"               icon-tools-lorange 0.95)
    ("enum"           "cod-symbol_enum"         icon-tools-lorange)
    ("enumerator"     "cod-symbol_enum_member"  icon-tools-lblue 0.9)
    ("enummember"     "cod-symbol_enum_member"  icon-tools-lblue 0.9)
    ("using"          "cod-symbol_namespace"          icon-tools-dyellow)
    ("namespace"      "cod-symbol_namespace"          icon-tools-dyellow)
    ("variable"       "cod-symbol_field"                  icon-tools-lblue 0.95)
    ("member"         "cod-symbol_field"                  icon-tools-lblue 0.95)
    ("field"          "cod-symbol_field"                  icon-tools-lblue 0.95)
    ("externvar"      "cod-symbol_field"                  icon-tools-dorange 0.95)
    ("local"          "cod-symbol_variable"            icon-tools-dblue 1.1)
    ("macro"          "md-arrow_expand"                     icon-tools-purple 0.85)
    ("string"         "cod-symbol_string"             icon-tools-blue 0.9)
    ("boolean"        "cod-symbol_boolean"            icon-tools-lpurple 0.9)
    ("array"          "cod-symbol_array"              icon-tools-maroon 0.85)
    ("number"         "cod-symbol_numeric"            icon-tools-lgreen 0.85)
    ("object"         "cod-symbol_namespace"          icon-tools-lgreen 0.95)
    ("misc"           "cod-symbol_misc"               icon-tools-lgreen 0.95)
    ("operator"       "cod-symbol_operator"           icon-tools-orange 0.9)
    ("parameter"      "cod-symbol_parameter"          icon-tools-dpurple 1.1)
    ("macroparam"     "cod-symbol_parameter"          icon-tools-purple 1.1)
    ("typeparameter"  "cod-symbol_parameter"          icon-tools-lmaroon 1.1)
    ("tparam"         "cod-symbol_parameter"          icon-tools-lmaroon 1.1)
    ("event"          "cod-symbol_event"              icon-tools-yellow 0.95)
    ("typedef"        "cod-references"                icon-tools-lmaroon 0.8)
    ("package"        "cod-package"                   icon-tools-lblue 0.9)
    ("module"         "cod-package"                   icon-tools-lblue 0.9)
    ("key"            "cod-symbol_key"                icon-tools-dblue 1.05)
    ("null"           "weather-na"                     icon-tools-lmaroon 1.5)

    ;; Elisp
    ("derivedMode"  "md-cogs"                     icon-tools-purple 0.9)
    ("majorMode"    "md-cogs"                     icon-tools-purple 0.9)
    ("command"      "md-apple_keyboard_command"     icon-tools-purple 0.9)
    ("minorMode"    "md-cogs"                     icon-tools-purple 0.9)
    ("inline"       "cod-symbol_method"                  icon-tools-purple 0.95)
    ("subst"        "cod-symbol_method"                  icon-tools-purple 0.95)
    ("group"        "cod-package"                   icon-tools-lblue 0.9)
    ("error"        "cod-error"                     icon-tools-lblue)
    ("custom"       "seti-settings"                  icon-tools-orange)
    ("face"         "fae-palette_color"                     icon-tools-red)
    ("const"        "cod-symbol_constant"           icon-tools-lgreen)
    ("symbol"       "cod-symbol_key"             icon-tools-dyellow 0.9)
    ("alias"        "cod-references"                icon-tools-lmaroon 0.8)
    ("unknown"      "cod-question"           icon-tools-dyellow 0.9)

    ;; JavaScript, TypeScript
    ("constant"     "cod-symbol_constant"           icon-tools-lgreen)
    ("property"     "cod-symbol_property"           icon-tools-blue)

    ;; Markdown
    ("chapter"      "fa-sticky_note"               icon-tools-yellow)
    ("section"      "md-format_section"                   icon-tools-lorange 0.9)
    ("subsection"   "md-format_section"                   icon-tools-orange 0.8)

    ;; Org
    ("part"         "fa-pagelines"                 icon-tools-lmaroon)
    ))

(defvar icon-tools-default-mode-icon
  '("md-cogs" icon-tools-dsilver))

;; Function start ------------------------------------------------------------ ;

(defun icon-tools--match-to-alist (file alist)
  "Match FILE against an entry in ALIST using `string-match'."
  (assoc file alist (lambda (a b) (string-match a b))))

(defun icon-tools-dir-is-submodule (dir)
  "Checker whether or not DIR is a git submodule."
  (let* ((gitmodule-dir (locate-dominating-file dir ".gitmodules"))
         (modules-file  (expand-file-name (format "%s.gitmodules" gitmodule-dir)))
         (module-search (format "submodule \".*?%s\"" (file-name-base dir))))
    (when (and gitmodule-dir (file-exists-p (format "%s/.git" dir)))
      (with-temp-buffer
        (insert-file-contents modules-file)
        (search-forward-regexp module-search (point-max) t)))))

;;;###autoload
(defun icon-tools-icon-for-dir (dir &rest args)
  "Get the formatted icon for DIR.

ARGS should be a plist containining `:face' or `:scale'."
  (let ((path (expand-file-name dir)))
    (cond
     ((file-remote-p path)
      (apply #'icon-tools-icon-str "oct-terminal"
             (append args '(:face icon-tools-blue))))
     ((file-symlink-p path)
      (apply #'icon-tools-icon-str "md-folder_move"
             (append args '(:face icon-tools-blue))))
     ((icon-tools-dir-is-submodule path)
      (apply #'icon-tools-icon-str "md-folder_move"
             (append args '(:face icon-tools-blue))))
     ((file-exists-p (format "%s/.git" path))
      (apply #'icon-tools-icon-str "oct-repo"
             (append args '(:face icon-tools-blue))))
     (t
      (let* ((dir-name (file-name-base (directory-file-name dir)))
             (match (or (cdr (icon-tools--match-to-alist
                              dir-name
                              icon-tools-dir-regexp-icon-alist))
                        icon-tools-default-dir-icon))
             (icon-name (car match))
             (face (cadr match)))
        (apply #'icon-tools-icon-str icon-name
               (append args `(:face ,(or face 'icon-tools-blue)))))))))

;;;###autoload
(defun icon-tools-icon-for-str (str &rest args)
  "Get the formatted icon for STR.

ARGS should be a plist containining `:face' or `:scale'."
  (when-let ((match (icon-tools--match-to-alist
                     str icon-tools-regexp-icon-alist)))
    (apply #'icon-tools-icon-str (cadr match)
           (append args `(:face ,(caddr match))))))

;;;###autoload
(defun icon-tools-icon-for-file (file &rest args)
  "Get the formatted icon for FILE.

ARGS should be a plist containining `:face' or `:scale'."
  (let* ((ext (file-name-extension file))
         (match (or (cdr (icon-tools--match-to-alist
                          file icon-tools-regexp-icon-alist))
                    (and ext (cdr
                              (assoc (downcase ext)
                                     icon-tools-extension-icon-alist)))
                    icon-tools-default-file-icon)))
    (apply #'icon-tools-icon-str (car match)
           (append args `(:face ,(cadr match))))))

;;;###autoload
(defun icon-tools-icon-for-mode (mode &rest args)
  "Get the formatted icon for MODE.

ARGS should be a plist containining `:face' or `:scale'."
  (let* ((mode0 mode)
         (match (assoc mode0 icon-tools-mode-icon-alist)))
    (while (and mode0 (not match))
      (setq mode0 (get mode0 'derived-mode-parent))
      (setq match (assoc mode0 icon-tools-mode-icon-alist)))
    (if match
        (apply #'icon-tools-icon-str (cadr match)
               (append args `(:face ,(caddr match))))
      (apply #'icon-tools-icon-str "md-text_box_edit"
             (append args '(:face icon-tools-purple))))))

;;;###autoload
(defun icon-tools-icon-for-symbol-kind (kind &rest args)
  "Get the formatted icon for symbol KIND.

ARGS should be a plist containining `:face' or `:scale'."
  (if-let* ((spec (cdr (assoc kind icon-tools-symbol-kind-icon-alist)))
            (icon-str (apply #'icon-tools-icon-str (car spec)
                             (append args
                                     `(:face ,(cadr spec))
                                     `(:scale ,(caddr spec)))))
            ((not (string-empty-p icon-str))))
      icon-str
    (icon-tools-icon-str "fa-tag" :face 'icon-tools-pink)))

;; Overriding all-the-icons -------------------------------------------------- ;

;;;###autoload
(define-minor-mode icon-tools-override-mode
  "Override `all-the-icons' functions with `icon-tools` ones."
  :global t
  (if icon-tools-override-mode
      (progn
        (require 'all-the-icons)
        (advice-add #'all-the-icons-alltheicon :override #'icon-tools-icon-str)
        (advice-add #'all-the-icons-fileicon :override #'icon-tools-icon-str)
        (advice-add #'all-the-icons-octicon :override #'icon-tools-icon-str)
        (advice-add #'all-the-icons-material :override #'icon-tools-icon-str)
        (advice-add #'all-the-icons-faicon :override #'icon-tools-icon-str)
        (advice-add #'all-the-icons-wicon :override #'icon-tools-icon-str))
    (advice-remove #'all-the-icons-alltheicon #'icon-tools-icon-str)
    (advice-remove #'all-the-icons-fileicon #'icon-tools-icon-str)
    (advice-remove #'all-the-icons-octicon #'icon-tools-icon-str)
    (advice-remove #'all-the-icons-material #'icon-tools-icon-str)
    (advice-remove #'all-the-icons-faicon #'icon-tools-icon-str)
    (advice-remove #'all-the-icons-wicon #'icon-tools-icon-str)))

(provide 'icon-tools)

;;; icon-tools.el ends here
