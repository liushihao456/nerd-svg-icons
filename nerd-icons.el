;;; nerd-icons.el --- Tools for icons in Emacs  -*- lexical-binding: t; -*-

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
(require 'nerd-icons-faces)
(require 'nerd-icons-data-nerd)

(defgroup nerd-icons nil
  "Group for nerd-icons."
  :group 'nerd-icons)

(defvar nerd-icons-svg-icon-dir
  (expand-file-name "svg" (file-name-directory load-file-name)))

(defvar nerd-icons-icon-width 2)

(defvar nerd-icons-svg-icon-cache
  (make-hash-table :test 'equal :size 250))

(defun nerd-icons-svg-icon-cache-add (icon icon-name &rest args)
  (puthash (format "%s-%s-%s-%d-%d"
                   icon-name
                   (symbol-name (or (plist-get args :face) 'default))
                   (or (plist-get args :scale) 1.0)
                   (window-font-width)
                   (window-font-height))
           icon nerd-icons-svg-icon-cache))

(defun nerd-icons-svg-icon-cache-get (icon-name &rest args)
  (gethash (format "%s-%s-%s-%d-%d"
                   icon-name
                   (symbol-name (or (plist-get args :face) 'default))
                   (or (plist-get args :scale) 1.0)
                   (window-font-width)
                   (window-font-height))
           nerd-icons-svg-icon-cache))

(defun nerd-icons-svg-icon-filepath (icon-name)
  (concat (file-name-as-directory nerd-icons-svg-icon-dir)
          icon-name ".svg"))

(defun nerd-icons-svg-icon-parse (icon-name)
  (with-temp-buffer
    (insert-file-contents (nerd-icons-svg-icon-filepath icon-name))
    (xml-parse-region (point-min) (point-max))))

(defun nerd-icons--svg-icon-emacs-color-to-svg-color (color-name)
  "Convert Emacs COLOR-NAME to #rrggbb form.
If COLOR-NAME is unknown to Emacs, then return COLOR-NAME as-is."
  (let ((rgb-color (color-name-to-rgb color-name)))
    (if rgb-color
        (apply #'color-rgb-to-hex (append rgb-color '(2)))
      color-name)))

(defun nerd-icons--svg-icon-alist-to-keyword-plist (alist)
  (cl-loop for (head . tail) in alist
           nconc (list (intern (concat ":" (symbol-name head))) tail)))

(defun nerd-icons--svg-icon-recursively-copy-children (node1 node2 fg-color)
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
                                 (nerd-icons--svg-icon-alist-to-keyword-plist attrs))))
            (nerd-icons--svg-icon-recursively-copy-children node1-child child fg-color)))))))

(defvar nerd-icons-svg-icon-scale-alist
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

(defvar nerd-icons-svg-icon-base-scale 0.9)

(defun nerd-icons--svg-icon-get-viewbox-multiplier (icon-name)
  (let ((cell (assoc icon-name nerd-icons-svg-icon-scale-alist)))
    (if cell
        (/ 1 (* (cdr cell) nerd-icons-svg-icon-base-scale))
      (/ 1 nerd-icons-svg-icon-base-scale))))

(defun nerd-icons--svg-icon-get-face-attribute-deep (face attribute)
  (when (facep face)
    (let ((face0 (face-attribute face :inherit))
          (val (face-attribute face attribute)))
      (while (and (facep face0) (eq val 'unspecified))
        (setq val (face-attribute face0 attribute))
        (setq face0 (face-attribute face0 :inherit)))
      val)))

(defun nerd-icons-svg-icon (icon-name &rest args)
  "Build the icon ICON-NAME.

ARGS are additional plist arguments where properties FACE and
SCALE are supported.

Icon is drawn with the foreground of FACE and scaled with SCALE."

  (let ((cache-item (apply #'nerd-icons-svg-icon-cache-get icon-name args)))
    (if cache-item
    ;; (if nil
        cache-item
      (let* ((face (plist-get args :face))
             (scale (plist-get args :scale))

             (root (nerd-icons-svg-icon-parse icon-name))

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

             ;; Set icon size (in pixels) to `nerd-icons-icon-width'x1 characters
             (svg-width  (* (window-font-width) nerd-icons-icon-width))

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
                                (nerd-icons--svg-icon-get-viewbox-multiplier icon-name))
                           (nerd-icons--svg-icon-get-viewbox-multiplier icon-name)))
             (d-view-width (* (- multiplier 1) view-width))
             (view-x (- view-x (/ d-view-width 2)))
             (view-width (+ view-width d-view-width))
             (d-view-height (* (- multiplier 1) view-height))
             (view-y (- view-y (/ d-view-height 2)))
             (view-height (+ view-height d-view-height))

             (svg-viewbox (format "%f %f %f %f" view-x view-y view-width view-height))

             ;; Foreground and background
             (fg-color (nerd-icons--svg-icon-get-face-attribute-deep face :foreground))
             (fg-color (nerd-icons--svg-icon-emacs-color-to-svg-color
                        (or (when (facep fg-color)
                              (face-foreground fg-color nil t))
                            (when (not (eq fg-color 'unspecified)) fg-color)
                            (face-attribute 'default :foreground))))
             ;; Use only transparent background for now
             (bg-color "transparent")
             ;; (bg-color (nerd-icons--svg-icon-get-face-attribute-deep face :background))
             ;; (bg-color (nerd-icons--svg-icon-emacs-color-to-svg-color
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
        (nerd-icons--svg-icon-recursively-copy-children svg (car root) fg-color)

        (apply #'nerd-icons-svg-icon-cache-add (svg-image svg :ascent 'center :scale 1)
               icon-name args)))))

(defun nerd-icons--get-nerd-icon-glyph (icon-name)
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

(defun nerd-icons-svg-icon-str (icon-name &rest args)
  "Return the svg icon as string.

ICON-NAME is a string in the form of FAMILY-ICON, e.g. fa-book.

ARGS are additional plist arguments where properties FACE and SCALE are
supported."
  (if-let* ((glyph (nerd-icons--get-nerd-icon-glyph icon-name))
            (codepoint (format "%x" (string-to-char glyph))))
      (propertize
       (make-string nerd-icons-icon-width ?\-)
       'display (apply #'nerd-icons-svg-icon codepoint args))))

(defun nerd-icons-nerd-icon-str (icon-name &rest args)
  "Return the nerd icon as string.

ICON-NAME is a string in the form of FAMILY-ICON, e.g. fa-book.

ARGS are additional plist arguments where properties FACE and SCALE are
supported."
  (when-let ((glyph (nerd-icons--get-nerd-icon-glyph icon-name)))
    (propertize (concat glyph " ") 'face `(:foreground
                              ,(face-attribute
                                (or (plist-get args :face) 'default)
                                :foreground)))
    ))

(defun nerd-icons-icon-str (icon-name &rest args)
  (if (display-graphic-p)
      (apply #'nerd-icons-svg-icon-str icon-name args)
    (apply #'nerd-icons-nerd-icon-str icon-name args)))

;; Icon alists --------------------------------------------------------------- ;

(defvar nerd-icons-extension-icon-alist
  '(
    ("fish"                 "oct-terminal"              nerd-icons-lpink)
    ("zsh"                  "oct-terminal"              nerd-icons-lcyan)
    ("sh"                   "oct-terminal"              nerd-icons-purple)
    ("terminal"             "oct-terminal"              nerd-icons-purple)
    ("bat"                  "oct-terminal"              nerd-icons-purple)

    ;; Meta
    ("tags"                 "fa-tag"                   nerd-icons-blue)
    ("tag"                  "fa-tag"                   nerd-icons-blue)
    ("log"                  "fa-bug"                   nerd-icons-maroon)
    ("aux"                  "fa-bug"                   nerd-icons-maroon)
    ("nav"                  "fa-bug"                   nerd-icons-maroon)
    ("snm"                  "fa-bug"                   nerd-icons-maroon)
    ("toc"                  "fa-bug"                   nerd-icons-maroon)
    ("vrb"                  "fa-bug"                   nerd-icons-maroon)

    ;; binary
    ("exe"                  "oct-file_binary"           nerd-icons-dsilver)
    ("dll"                  "md-cogs"                 nerd-icons-dsilver)
    ("lib"                  "oct-file_binary"           nerd-icons-dsilver)
    ("class"                "oct-file_binary"           nerd-icons-dsilver)
    ("obj"                  "oct-file_binary"           nerd-icons-dsilver)
    ("so"                   "oct-file_binary"           nerd-icons-dsilver)
    ("o"                    "oct-file_binary"           nerd-icons-dsilver)
    ("d"                    "oct-file_binary"           nerd-icons-dsilver)
    ("out"                  "oct-file_binary"           nerd-icons-dsilver)
    ("elc"                  "oct-file_binary"           nerd-icons-dsilver)
    ("eln"                  "oct-file_binary"           nerd-icons-dsilver)
    ("cmake-cache"          "oct-file_binary"           nerd-icons-dsilver)
    ("csr"                  "oct-file_binary"           nerd-icons-dsilver)
    ("eslintcache"          "oct-file_binary"           nerd-icons-dsilver)
    ("cer"                  "oct-file_binary"           nerd-icons-dsilver)
    ("der"                  "oct-file_binary"           nerd-icons-dsilver)
    ("pfx"                  "oct-file_binary"           nerd-icons-dsilver)
    ("p7b"                  "oct-file_binary"           nerd-icons-dsilver)
    ("p7r"                  "oct-file_binary"           nerd-icons-dsilver)
    ("DS_STORE"             "oct-file_binary"           nerd-icons-dsilver)
    ("src"                  "oct-file_binary"           nerd-icons-dsilver)
    ("crl"                  "oct-file_binary"           nerd-icons-dsilver)
    ("sst"                  "oct-file_binary"           nerd-icons-dsilver)
    ("stl"                  "oct-file_binary"           nerd-icons-dsilver)
    ("pyc"                  "oct-file_binary"           nerd-icons-dsilver)
    ("bin"                  "oct-file_binary"           nerd-icons-dsilver)

    ;; torrent
    ("torrent"              "oct-download"       nerd-icons-dsilver)
    ("aria2"                "oct-download"       nerd-icons-dsilver)
    ("ed2k"                 "oct-download"       nerd-icons-dsilver)

    ;; book
    ("azw"                  "cod-book"                  nerd-icons-dsilver)
    ("azw3"                 "cod-book"                  nerd-icons-dsilver)
    ("mobi"                 "cod-book"                  nerd-icons-dsilver)
    ("epub"                 "cod-book"                  nerd-icons-dsilver)

    ;; ?
    ("pkg"                  "cod-package"               nerd-icons-dsilver)
    ("rpm"                  "cod-package"               nerd-icons-dsilver)
    ("tar"                  "oct-file_zip"              nerd-icons-lmaroon)
    ("rar"                  "oct-file_zip"              nerd-icons-lmaroon)
    ("gz"                   "oct-file_zip"              nerd-icons-lmaroon)
    ("zip"                  "oct-file_zip"              nerd-icons-lmaroon)
    ("7z"                   "oct-file_zip"              nerd-icons-lmaroon)
    ("xz"                   "oct-file_zip"              nerd-icons-lmaroon)
    ("tgz"                  "oct-file_zip"              nerd-icons-lmaroon)
    ("dat"                  "md-chart_bar"             nerd-icons-cyan)
    ("edn"                  "md-chart_bar"             nerd-icons-cyan)
    ("dmg"                  "cod-package"                 nerd-icons-lsilver)
    ;; Source Codesode
    ("scpt"                 "md-apple"                 nerd-icons-pink)
    ("aup"                  "oct-file_code"              nerd-icons-yellow)
    ("elm"                  "seti-elm"                   nerd-icons-blue)
    ("erl"                  "dev-erlang"                nerd-icons-red)
    ("hrl"                  "dev-erlang"                nerd-icons-dred)
    ("eex"                  "seti-elixir"                nerd-icons-lorange)
    ("leex"                 "seti-elixir"                nerd-icons-lorange)
    ("heex"                 "seti-elixir"                nerd-icons-lorange)
    ("ex"                   "seti-elixir"                nerd-icons-lpurple)
    ("exs"                  "seti-elixir"                nerd-icons-lred)
    ("java"                 "fae-java"                  nerd-icons-purple)
    ("jar"                  "fae-java"                  nerd-icons-purple)
    ("gradle"               "seti-gradle"                nerd-icons-silver)
    ("ebuild"               "md-gentoo"                nerd-icons-cyan)
    ("eclass"               "md-gentoo"                nerd-icons-blue)
    ("go"                   "custom-go"                    nerd-icons-blue)
    ("jl"                   "seti-julia"                 nerd-icons-purple)
    ("magik"                "fa-magic"   nerd-icons-blue)
    ("matlab"               "md-math_compass"                nerd-icons-orange)
    ("nix"                  "md-nix"                   nerd-icons-blue)
    ("pl"                   "dev-perl"                  nerd-icons-lorange)
    ("pm"                   "dev-perl"                  nerd-icons-lorange)
    ("pod"                  "dev-perl"              nerd-icons-lgreen)
    ("php"                  "seti-php"                   nerd-icons-lsilver)
    ("pony"                 "oct-file_code"                  nerd-icons-maroon)
    ("ps1"                  "md-powershell"            nerd-icons-blue)
    ("pro"                  "dev-prolog"                nerd-icons-lmaroon)
    ("proog"                "dev-prolog"                nerd-icons-lmaroon)
    ("py"                   "seti-python"                nerd-icons-dblue)
    ("py.typed"             "seti-python"                nerd-icons-pink)
    ("idr"                  "oct-file_code"                 nerd-icons-red)
    ("ipynb"                "oct-file_code"               nerd-icons-dorange)
    ("gem"                  "cod-ruby"              nerd-icons-red)
    ("rb"                   "oct-ruby"                  nerd-icons-lred)
    ("rs"                   "seti-rust"                  nerd-icons-maroon)
    ("rlib"                 "seti-rust"                  nerd-icons-dmaroon)
    ("r"                    "seti-R"                     nerd-icons-purple)
    ("rd"                   "seti-R"                     nerd-icons-purple)
    ("rdx"                  "seti-R"                     nerd-icons-purple)
    ("rsx"                  "seti-R"                     nerd-icons-purple)
    ("svelte"               "seti-svelte"                nerd-icons-red)
    ("gql"                  "seti-graphql"               nerd-icons-dpink)
    ("graphql"              "seti-graphql"               nerd-icons-dpink)
    ("c"                    "seti-c"                nerd-icons-blue)
    ("h"                    "seti-c"                nerd-icons-purple)
    ("h.in"                 "seti-c"                nerd-icons-lblue)
    ("cc"                   "md-language_cpp"        nerd-icons-blue)
    ("cpp"                  "md-language_cpp"        nerd-icons-blue)
    ("cxx"                  "md-language_cpp"        nerd-icons-blue)
    ("hh"                   "md-language_cpp"        nerd-icons-purple)
    ("hpp"                  "md-language_cpp"        nerd-icons-purple)
    ("hpp.in"               "md-language_cpp"        nerd-icons-lblue)
    ("hxx"                  "md-language_cpp"        nerd-icons-purple)
    ("m"                    "md-apple")
    ("mm"                   "md-apple")

    ;; Lisps
    ("cl"                   "oct-file_code"           nerd-icons-lorange)
    ("l"                    "oct-file_code"                  nerd-icons-orange)
    ("lisp"                 "oct-file_code"                  nerd-icons-orange)
    ("hy"                   "oct-file_code"                    nerd-icons-blue)
    ("el"                   "custom-emacs"                 nerd-icons-purple)
    ("clj"                  "seti-clojure"           nerd-icons-blue)
    ("cljc"                 "seti-clojure"           nerd-icons-blue)
    ("cljs"                 "seti-clojure"             nerd-icons-dblue)
    ("coffee"               "dev-coffeescript"          nerd-icons-maroon)
    ("iced"                 "dev-coffeescript"          nerd-icons-lmaroon)
    ("dart"                 "dev-dart"                  nerd-icons-blue)
    ("rkt"                  "oct-file_code"                nerd-icons-red)
    ("scrbl"                "oct-file_code"                nerd-icons-blue)
    ;; Stylesheeting
    ("css"                  "md-language_css3"                  nerd-icons-yellow)
    ("scss"                 "dev-sass"                  nerd-icons-pink)
    ("sass"                 "dev-sass"                  nerd-icons-dpink)
    ("less"                 "dev-sass"                  nerd-icons-dyellow)
    ("postcss"              "md-language_css3"               nerd-icons-dred)
    ("sss"                  "md-language_css3"               nerd-icons-dred)
    ("styl"                 "dev-stylus"                nerd-icons-lgreen)
    ("csv"                  "cod-graph_line"                 nerd-icons-dblue)
    ;; haskell
    ("hs"                   "seti-haskell"               nerd-icons-red)
    ("chs"                  "seti-haskell"               nerd-icons-red)
    ("lhs"                  "seti-haskell"               nerd-icons-red)
    ("hsc"                  "seti-haskell"               nerd-icons-red)
    ;; Web modes
    ("inky-haml"            "fa-html5"                  nerd-icons-lyellow)
    ("haml"                 "fa-html5"                  nerd-icons-lyellow)
    ("htm"                  "fa-html5"                 nerd-icons-orange)
    ("html"                 "fa-html5"                 nerd-icons-orange)
    ("inky-er"              "fa-html5"                 nerd-icons-lred)
    ("inky-erb"             "fa-html5"                 nerd-icons-lred)
    ("erb"                  "fa-html5"                 nerd-icons-lred)
    ("hbs"                  "oct-file_code"             nerd-icons-green)
    ("inky-slim"            "cod-dashboard"             nerd-icons-yellow)
    ("slim"                 "cod-dashboard"             nerd-icons-yellow)
    ("jade"                 "seti-jade"                  nerd-icons-red)
    ("pug"                  "seti-pug"               nerd-icons-red)
    ;; Javascript
    ("d3js"                 "oct-file_code"                    nerd-icons-lgreen)
    ("re"                   "seti-reasonml"                nerd-icons-red-alt)
    ("rei"                  "seti-reasonml"                nerd-icons-dred)
    ("ml"                   "seti-ocaml"                 nerd-icons-lpink)
    ("mli"                  "seti-ocaml"                 nerd-icons-dpink)
    ("react"                "md-react"                 nerd-icons-lblue)
    ("ts"                   "seti-typescript"      nerd-icons-blue-alt)
    ("js"                   "seti-javascript"      nerd-icons-yellow)
    ("es"                   "seti-javascript"      nerd-icons-yellow)
    ("jsx"                  "seti-javascript"               nerd-icons-yellow)
    ("tsx"                  "seti-typescript"               nerd-icons-blue-alt)
    ("njs"                  "md-nodejs"                nerd-icons-lgreen)
    ("vue"                  "seti-vue"                   nerd-icons-lgreen)

    ("sbt"                  "seti-sbt"                   nerd-icons-red)
    ("scala"                "seti-scala"                 nerd-icons-red)
    ("scm"                  "oct-file_code"                nerd-icons-red)
    ("swift"                "seti-swift"                 nerd-icons-green)

    ("tcl"                  "oct-file_code"                   nerd-icons-dred)

    ("tf"                   "seti-terraform"             nerd-icons-purple-alt)
    ("tfvars"               "seti-terraform"             nerd-icons-purple-alt)
    ("tfstate"              "seti-terraform"             nerd-icons-purple-alt)

    ("asm"                  "oct-file_code"      nerd-icons-blue)
    ;; Verilog(-AMS) and SystemVerilog(-AMS)
    ("v"                    "oct-file_code"               nerd-icons-red)
    ("vams"                 "oct-file_code"               nerd-icons-red)
    ("sv"                   "oct-file_code"               nerd-icons-red)
    ("sva"                  "oct-file_code"               nerd-icons-red)
    ("svh"                  "oct-file_code"               nerd-icons-red)
    ("svams"                "oct-file_code"               nerd-icons-red)
    ;; VHDL(-AMS)
    ("vhd"                  "oct-file_code"                  nerd-icons-blue)
    ("vhdl"                 "oct-file_code"                  nerd-icons-blue)
    ("vhms"                 "oct-file_code"                  nerd-icons-blue)
    ;; Cabal
    ("cabal"                "oct-file_code"                 nerd-icons-lblue)
    ;; Kotlin
    ("kt"                   "seti-kotlin"                nerd-icons-orange)
    ("kts"                  "seti-kotlin"                nerd-icons-orange)
    ;; Nimrod
    ("nim"                  "seti-nim"                nerd-icons-yellow)
    ("nims"                 "seti-nim"                nerd-icons-yellow)
    ;; SQL
    ("sql"                  "fa-database"              nerd-icons-silver)
    ("db"                   "fa-database"              nerd-icons-silver)
    ("cache"                "fa-database"              nerd-icons-green)
    ;; Styles
    ("styles"               "oct-file_code"                 nerd-icons-red)
    ;; Lua
    ("lua"                  "seti-lua"                   nerd-icons-dblue)
    ;; ASCII doc
    ("adoc"                 "oct-file_code"              nerd-icons-lblue)
    ("asciidoc"             "oct-file_code"              nerd-icons-lblue)
    ;; Puppet
    ("pp"                   "seti-puppet"                nerd-icons-yellow)
    ;; Jinja
    ("j2"                   "seti-jinja"                 nerd-icons-silver)
    ("jinja2"               "seti-jinja"                 nerd-icons-silver)
    ;; Docker
    ("dockerfile"           "seti-docker"                nerd-icons-cyan)
    ;; Vagrant
    ("vagrantfile"          "oct-file_code"               nerd-icons-blue)
    ;; GLSL
    ("glsl"                 "oct-file_code"          nerd-icons-blue)
    ("vert"                 "oct-file_code"          nerd-icons-blue)
    ("tesc"                 "oct-file_code"          nerd-icons-purple)
    ("tese"                 "oct-file_code"          nerd-icons-dpurple)
    ("geom"                 "oct-file_code"          nerd-icons-green)
    ("frag"                 "oct-file_code"          nerd-icons-red)
    ("comp"                 "oct-file_code"          nerd-icons-dblue)
    ;; CUDA
    ("cu"                   "oct-file_code"          nerd-icons-green)
    ("cuh"                  "oct-file_code"          nerd-icons-green)
    ;; Fortran
    ("f90"                  "md-language_fortran"               nerd-icons-purple)
    ;; C#
    ("cs"                   "md-language_csharp"           nerd-icons-dblue)
    ("csx"                  "md-language_csharp"           nerd-icons-dblue)
    ;; F#
    ("fs"                   "dev-fsharp"                nerd-icons-blue-alt)
    ("fsi"                  "dev-fsharp"                nerd-icons-blue-alt)
    ("fsx"                  "dev-fsharp"                nerd-icons-blue-alt)
    ("fsscript"             "dev-fsharp"                nerd-icons-blue-alt)
    ;; zig
    ("zig"                  "seti-zig"                   nerd-icons-orange)
    ;; odin
    ("odin"                 "oct-file_code"                  nerd-icons-lblue)
    ;; File Types
    ("ico"                  "seti-image"            nerd-icons-blue)
    ("png"                  "seti-image"            nerd-icons-orange)
    ("gif"                  "seti-image"            nerd-icons-green)
    ("jpeg"                 "seti-image"            nerd-icons-dblue)
    ("jpg"                  "seti-image"            nerd-icons-dblue)
    ("webp"                 "seti-image"            nerd-icons-dblue)
    ("svg"                  "seti-image"            nerd-icons-lgreen)
    ("eps"                  "seti-image"            nerd-icons-lgreen)
    ;; Audio
    ("mp3"                  "md-music"                 nerd-icons-dred)
    ("wav"                  "md-music"                 nerd-icons-dred)
    ("m4a"                  "md-music"                 nerd-icons-dred)
    ("ogg"                  "md-music"                 nerd-icons-dred)
    ("flac"                 "md-music"                 nerd-icons-dred)
    ("opus"                 "md-music"                 nerd-icons-dred)
    ("au"                   "md-music"                 nerd-icons-dred)
    ("aif"                  "md-music"                 nerd-icons-dred)
    ("aifc"                 "md-music"                 nerd-icons-dred)
    ("aiff"                 "md-music"                 nerd-icons-dred)
    ("ly"                   "md-music"                 nerd-icons-green)
    ;; Video
    ("mov"                  "md-movie_open_outline"                  nerd-icons-blue)
    ("mp4"                  "md-movie_open_outline"                  nerd-icons-blue)
    ("ogv"                  "md-movie_open_outline"                  nerd-icons-dblue)
    ("mpg"                  "md-movie_open_outline"                  nerd-icons-blue)
    ("mpeg"                 "md-movie_open_outline"                  nerd-icons-blue)
    ("flv"                  "md-movie_open_outline"                  nerd-icons-blue)
    ("ogv"                  "md-movie_open_outline"                  nerd-icons-dblue)
    ("mkv"                  "md-movie_open_outline"                  nerd-icons-blue)
    ("webm"                 "md-movie_open_outline"                  nerd-icons-blue)
    ;; Subtitle
    ("srt"                  "md-closed_caption_outline"        nerd-icons-dblue)
    ;; Fonts
    ("ttf"                  "seti-font"                  nerd-icons-dcyan)
    ("otf"                  "seti-font"                  nerd-icons-dcyan)
    ("woff"                 "seti-font"                  nerd-icons-cyan)
    ("woff2"                "seti-font"                  nerd-icons-cyan)
    ("ttc"                  "seti-font"                  nerd-icons-cyan)
    ;; Doc
    ("pdf"                  "seti-pdf"              nerd-icons-dred)
    ("text"                 "md-file_document_multiple"             nerd-icons-cyan)
    ("txt"                  "fa-edit"                  nerd-icons-cyan)
    ("rst"                  "md-text_box_edit"      nerd-icons-green)
    ("doc"                  "md-microsoft_word"        nerd-icons-blue)
    ("docx"                 "md-microsoft_word"        nerd-icons-blue)
    ("docm"                 "md-microsoft_word"        nerd-icons-blue)
    ("texi"                 "seti-tex"                   nerd-icons-lred)
    ("tex"                  "seti-tex"                   nerd-icons-lred)
    ("sty"                  "seti-tex"                   nerd-icons-lred)
    ("pygtex"               "seti-tex"                   nerd-icons-pink)
    ("pygstyle"             "seti-tex"                   nerd-icons-pink)
    ("md"                   "md-language_markdown"              nerd-icons-lblue)
    ("bib"                  "seti-tex"                nerd-icons-maroon)
    ("org"                  "custom-orgmode"              nerd-icons-lgreen)
    ("pps"                  "md-microsoft_powerpoint"  nerd-icons-orange)
    ("ppt"                  "md-microsoft_powerpoint"  nerd-icons-orange)
    ("pptsx"                "md-microsoft_powerpoint"  nerd-icons-orange)
    ("ppttx"                "md-microsoft_powerpoint"  nerd-icons-orange)
    ("knt"                  "md-microsoft_powerpoint"  nerd-icons-cyan)
    ("xlsx"                 "md-microsoft_excel"       nerd-icons-dgreen)
    ("xlsm"                 "md-microsoft_excel"       nerd-icons-dgreen)
    ("xlsb"                 "md-microsoft_excel"       nerd-icons-dgreen)
    ("xltx"                 "md-microsoft_excel"       nerd-icons-dgreen)
    ("xltm"                 "md-microsoft_excel"       nerd-icons-dgreen)
    ;; key and licence
    ("key"                  "cod-key"                   nerd-icons-lblue)
    ("pem"                  "cod-key"                   nerd-icons-orange)
    ("p12"                  "cod-key"                   nerd-icons-dorange)
    ("crt"                  "cod-key"                   nerd-icons-lblue)
    ("pub"                  "cod-key"                   nerd-icons-blue)
    ("gpg"                  "cod-key"                   nerd-icons-lblue)
    ("license.md"           "cod-key"                   nerd-icons-purple)
    ("license"              "cod-key"                   nerd-icons-purple)
    ("lic"                  "cod-key"                   nerd-icons-dblue)
    ("gemfile"              "cod-key"                   nerd-icons-dblue)
    ("bookmarks"            "fa-bookmark"             nerd-icons-orange)

    ;; Config
    ("node"                 "md-nodejs"                nerd-icons-green)
    ("babelrc"              "seti-babel"                 nerd-icons-yellow)
    ("bashrc"               "md-script_text"                nerd-icons-dpink)
    ("bowerrc"              "seti-bower"                 nerd-icons-silver)
    ("cr"                   "seti-crystal"               nerd-icons-yellow)
    ("ecr"                  "seti-crystal"               nerd-icons-yellow)
    ("ini"                  "md-cogs"                  nerd-icons-yellow)
    ("eslintignore"         "seti-eslint"                nerd-icons-purple)
    ("eslint"               "seti-eslint"                nerd-icons-lpurple)
    ("git"                  "seti-git"                   nerd-icons-lred)
    ("mk"                   "dev-gnu"                   nerd-icons-dorange)
    ("clang"                "md-cogs"                  nerd-icons-dpurple)
    ("llvm"                 "md-cogs"                  nerd-icons-dpurple)
    ("clangd"               "md-cogs"                  nerd-icons-dpurple)
    ("cmake"                "md-cogs"                 nerd-icons-red)
    ("cmakelists.txt"       "md-cogs"                 nerd-icons-red)
    ("ninja"                "md-ninja")
    ("makefile"             "seti-makefile"              nerd-icons-cyan)
    ("dockerignore"         "seti-docker"                nerd-icons-dblue)
    ("xml"                  "cod-code"                  nerd-icons-lorange)
    ("json"                 "seti-settings"              nerd-icons-yellow)
    ("clang-format"         "seti-settings"              nerd-icons-yellow)
    ("cson"                 "md-cogs"                  nerd-icons-yellow)
    ("yml"                  "md-cogs"                  nerd-icons-dyellow)
    ("yaml"                 "md-cogs"                  nerd-icons-dyellow)
    ("toml"                 "md-cogs"                  nerd-icons-pink)
    ("cfg"                  "md-cogs"                  nerd-icons-dblue)
    ("terminfo"             "md-cogs"                  nerd-icons-dblue)
    ("settings.json"        "md-cogs"                  nerd-icons-dblue)
    ("Vagrantfile"          "md-cogs"                  nerd-icons-silver)
    ("babel.config.js"      "md-cogs"                  nerd-icons-silver)
    ("babelignore"          "md-cogs"                  nerd-icons-silver)
    ("babelrc"              "md-cogs"                  nerd-icons-silver)
    ("babelrc.js"           "md-cogs"                  nerd-icons-silver)
    ("babelrc.json"         "md-cogs"                  nerd-icons-silver)
    ("bashrc"               "md-cogs"                  nerd-icons-silver)
    ("bazel"                "md-cogs"                  nerd-icons-silver)
    ("bazelrc"              "md-cogs"                  nerd-icons-silver)
    ("bower.json"           "md-cogs"                  nerd-icons-silver)
    ("bowerrc"              "md-cogs"                  nerd-icons-silver)
    ("cabal"                "md-cogs"                  nerd-icons-silver)
    ("cfg"                  "md-cogs"                  nerd-icons-silver)
    ("conf"                 "md-cogs"                  nerd-icons-silver)
    ("config"               "md-cogs"                  nerd-icons-silver)
    ("cson"                 "md-cogs"                  nerd-icons-silver)
    ("editorconfig"         "md-cogs"                  nerd-icons-silver)
    ("envrc"                "md-cogs"                  nerd-icons-silver)
    ("eslintignore"         "md-cogs"                  nerd-icons-silver)
    ("eslintrc"             "md-cogs"                  nerd-icons-silver)
    ("feature"              "md-cogs"                  nerd-icons-silver)
    ("gemfile"              "md-cogs"                  nerd-icons-silver)
    ("gitattributes"        "md-cogs"                  nerd-icons-silver)
    ("gitconfig"            "md-cogs"                  nerd-icons-silver)
    ("gitignore"            "md-cogs"                  nerd-icons-silver)
    ("gitmodules"           "md-cogs"                  nerd-icons-silver)
    ("ideavimrc"            "md-cogs"                  nerd-icons-silver)
    ("iml"                  "md-cogs"                  nerd-icons-silver)
    ("ini"                  "md-cogs"                  nerd-icons-silver)
    ("inputrc"              "md-cogs"                  nerd-icons-silver)
    ("ledgerrc"             "md-cogs"                  nerd-icons-silver)
    ("lock"                 "md-cogs"                  nerd-icons-silver)
    ("nginx"                "md-cogs"                  nerd-icons-silver)
    ("npm-shrinkwrap.json"  "md-cogs"                  nerd-icons-silver)
    ("npmignore"            "md-cogs"                  nerd-icons-silver)
    ("npmrc"                "md-cogs"                  nerd-icons-silver)
    ("package-lock.json"    "md-cogs"                  nerd-icons-silver)
    ("package.json"         "md-cogs"                  nerd-icons-silver)
    ("phpunit"              "md-cogs"                  nerd-icons-silver)
    ("pkg"                  "md-cogs"                  nerd-icons-silver)
    ("plist"                "md-cogs"                  nerd-icons-silver)
    ("properties"           "md-cogs"                  nerd-icons-silver)
    ("terminalrc"           "md-cogs"                  nerd-icons-silver)
    ("tridactylrc"          "md-cogs"                  nerd-icons-silver)
    ("vimperatorrc"         "md-cogs"                  nerd-icons-silver)
    ("vimrc"                "md-cogs"                  nerd-icons-silver)
    ("vrapperrc"            "md-cogs"                  nerd-icons-silver)
    ("xdefaults"            "md-cogs"                  nerd-icons-silver)
    ("xml"                  "md-cogs"                  nerd-icons-silver)
    ("xresources"           "md-cogs"                  nerd-icons-silver)
    ("yaml"                 "md-cogs"                  nerd-icons-silver)
    ("yarn-integrity"       "md-cogs"                  nerd-icons-silver)
    ("yarnclean"            "md-cogs"                  nerd-icons-silver)
    ("yarnignore"           "md-cogs"                  nerd-icons-silver)
    ("yarnrc"               "md-cogs"                  nerd-icons-silver)
    ("rc"                   "md-cogs"                  nerd-icons-silver)
    ("project"              "md-cogs"                  nerd-icons-silver)
    ("prefs"                "md-cogs"                  nerd-icons-silver)
    ("sln"                  "md-microsoft_visual_studio"          nerd-icons-blue)
    ("vcxproj"              "md-microsoft_visual_studio"          nerd-icons-blue)
    ("vcproj"               "md-microsoft_visual_studio"          nerd-icons-blue)

    ;; model
    ("pth"                  "cod-package"               nerd-icons-dsilver)
    ("ckpt"                 "cod-package"               nerd-icons-dsilver)
    ("model"                "cod-package"               nerd-icons-dsilver)

    ;; whl
    ("whl"                  "cod-package"               nerd-icons-purple-alt)
    ))

(defvar nerd-icons-regexp-icon-alist
  '(
    ;;
    ("^TAGS$"                   "fa-tag"             nerd-icons-blue)
    ("^TAG$"                    "fa-tag"             nerd-icons-blue)
    ("^TODO$"                   "oct-checklist"       nerd-icons-lyellow)
    ("^LICENSE$"                "cod-book"            nerd-icons-blue)
    ("^readme.md$"              "md-language_markdown"        nerd-icons-lblue)
    ("^readme"                  "cod-book"            nerd-icons-lcyan)
    ("help"                     "md-information_outline"            nerd-icons-purple)
    ("info"                     "md-information_outline"            nerd-icons-pink)

    ;; Config
    ("nginx$"                   "dev-nginx"           nerd-icons-dgreen)
    ("apache$"                  "md-apache_kafka"          nerd-icons-dgreen)

    ;; C
    ("^Makefile$"               "seti-makefile"        nerd-icons-dorange)
    ("^CMakeLists.txt$"         "md-cogs"           nerd-icons-red)
    ("^CMakeCache.txt$"         "md-cogs"           nerd-icons-blue)
    ("cmake"                    "md-cogs"           nerd-icons-red)

    ;; Visual Studio
    ("vcxproj"                  "md-microsoft_visual_studio"    nerd-icons-blue)
    ("vcproj"                   "md-microsoft_visual_studio"    nerd-icons-blue)

    ;; Docker
    ("^\\.?Dockerfile"          "seti-docker"          nerd-icons-blue)

    ;; Homebrew
    ("^Brewfile$"               "fa-beer"        nerd-icons-lsilver)

    ;; ;; AWS
    ("^stack.*.json$"           "md-aws"             nerd-icons-orange)
    ("^serverless\\.yml$"       "fa-bolt"            nerd-icons-yellow)

    ;; lock files
    ("~$"                       "md-lock"            nerd-icons-maroon)

    ;; Source Codes
    ("^mix.lock$"               "seti-elixir"          nerd-icons-lyellow)

    ;; Ruby
    ("^Gemfile\\(\\.lock\\)?$"  "cod-ruby"     nerd-icons-red)
    ("_?test\\.rb$"             "oct-ruby"       nerd-icons-red)
    ("_?test_helper\\.rb$"      "oct-ruby"       nerd-icons-dred)
    ("_?spec\\.rb$"             "oct-ruby"       nerd-icons-red)
    ("_?spec_helper\\.rb$"      "oct-ruby"       nerd-icons-dred)

    ("-?spec\\.ts$"             "seti-typescript" nerd-icons-blue)
    ("-?test\\.ts$"             "seti-typescript" nerd-icons-blue)
    ("-?spec\\.js$"             "seti-javascript"         nerd-icons-lpurple)
    ("-?test\\.js$"             "seti-javascript"         nerd-icons-lpurple)
    ("-?spec\\.jsx$"            "md-react"      nerd-icons-blue-alt)
    ("-?test\\.jsx$"            "md-react"      nerd-icons-blue-alt)

    ;; Git
    ("^MERGE_"                  "oct-git_merge"       nerd-icons-red)
    ("^COMMIT_EDITMSG"          "oct-git_commit"      nerd-icons-red)

    ;; Stylesheeting
    ("stylelint"                "seti-stylelint"       nerd-icons-lyellow)

    ;; JavaScript
    ("^package.json$"           "seti-npm"             nerd-icons-red)
    ("^package.lock.json$"      "seti-npm"             nerd-icons-dred)
    ("^yarn\\.lock"             "seti-yarn"            nerd-icons-blue-alt)
    ("\\.npmignore$"            "seti-npm"             nerd-icons-dred)
    ("^bower.json$"             "seti-bower"           nerd-icons-lorange)
    ("^gulpfile"                "seti-gulp"            nerd-icons-lred)
    ("^gruntfile"               "seti-grunt"           nerd-icons-lyellow)
    ("^webpack"                 "seti-webpack"         nerd-icons-lblue)

    ;; Go
    ("^go.mod$"                 "seti-go"       nerd-icons-blue-alt)
    ("^go.work$"                "seti-go"       nerd-icons-blue-alt)

    ;; Emacs
    ("bookmarks"                "fa-bookmark"       nerd-icons-orange)
    ("bookmark"                 "fa-bookmark"        nerd-icons-orange)

    ("^\\*scratch\\*$"          "fa-sticky_note"     nerd-icons-lyellow)
    ("^\\*scratch.*"            "fa-sticky_note"     nerd-icons-yellow)
    ("^\\*new-tab\\*$"          "fa-star"            nerd-icons-cyan)

    ("\\.git"                   "seti-git"             nerd-icons-yellow)

    ("^\\."                     "md-cogs")
    ))

(defvar nerd-icons-default-file-icon
  '("oct-file" nerd-icons-dsilver))

(defvar nerd-icons-dir-regexp-icon-alist
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

(defvar nerd-icons-default-dir-icon
  '("md-folder" nerd-icons-dsilver))

(defvar nerd-icons-weather-icon-alist
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

(defvar nerd-icons-mode-icon-alist
  '(
    (emacs-lisp-mode                    "custom-emacs"             nerd-icons-purple)
    (circe-server-mode                  "fa-commenting_o")
    (circe-channel-mode                 "fa-commenting_o")
    (crystal-mode                       "seti-crystal"           nerd-icons-yellow)
    (erc-mode                           "fa-commenting_o")
    (inferior-emacs-lisp-mode           "custom-emacs"             nerd-icons-lblue)
    (dired-mode                         "md-folder_multiple")
    (lisp-interaction-mode              "oct-file_code"              nerd-icons-orange)
    (sly-mrepl-mode                     "oct-file_code"       nerd-icons-orange)
    (slime-repl-mode                    "oct-file_code"       nerd-icons-orange)
    (org-mode                           "custom-orgmode"          nerd-icons-lgreen)
    (typescript-mode                    "seti-typescript"  nerd-icons-blue-alt)
    (react-mode                         "md-react"             nerd-icons-lblue)
    (js-mode                            "seti-javascript"  nerd-icons-yellow)
    (js-jsx-mode                        "md-react"             nerd-icons-yellow)
    (js2-mode                           "seti-javascript"  nerd-icons-yellow)
    (js3-mode                           "seti-javascript"  nerd-icons-yellow)
    (rjsx-mode                          "md-react"             nerd-icons-cyan-alt)
    (term-mode                          "oct-terminal")
    (vterm-mode                         "oct-terminal")
    (eshell-mode                        "oct-terminal"          nerd-icons-purple)
    (magit-refs-mode                    "oct-git_branch"        nerd-icons-red)
    (magit-process-mode                 "seti-github")
    (magit-diff-mode                    "oct-git_compare"       nerd-icons-lblue)
    (ediff-mode                         "oct-git_compare"       nerd-icons-red)
    (diff-mode                          "oct-git_compare"       nerd-icons-purple)
    (comint-mode                        "oct-terminal"          nerd-icons-lblue)
    (eww-mode                           "md-firefox"           nerd-icons-red)
    (org-agenda-mode                    "oct-checklist"         nerd-icons-lgreen)
    (cfw:calendar-mode                  "md-calendar_check")
    (ibuffer-mode                       "fa-files_o"             nerd-icons-dsilver)
    (messages-buffer-mode               "md-message_text_outline"           nerd-icons-dsilver)
    (help-mode                          "md-information_outline"              nerd-icons-purple)
    (Info-mode                          "md-information_outline"              nerd-icons-pink)
    (benchmark-init/tree-mode           "cod-dashboard")
    (jenkins-mode                       "seti-jenkins"           nerd-icons-blue)
    (magit-popup-mode                   "seti-git"               nerd-icons-red)
    (magit-status-mode                  "seti-git"               nerd-icons-lred)
    (magit-log-mode                     "seti-git"               nerd-icons-green)
    (mu4e-compose-mode                  "md-pencil")
    (mu4e-headers-mode                  "cod-mail")
    (mu4e-main-mode                     "cod-mail")
    (mu4e-view-mode                     "cod-mail_read")
    (package-menu-mode                  "md-package_variant_plus"           nerd-icons-silver)
    (paradox-menu-mode                  "md-archive"           nerd-icons-silver)
    (Custom-mode                        "seti-settings")
    (web-mode                           "seti-webpack"       nerd-icons-purple)
    (fundamental-mode                   "md-file_document_multiple"         nerd-icons-dsilver)
    (special-mode                       "md-information_outline"              nerd-icons-yellow)
    (text-mode                          "md-file_document_multiple"         nerd-icons-cyan)
    (enh-ruby-mode                      "oct-ruby"              nerd-icons-lred)
    (ruby-mode                          "oct-ruby"              nerd-icons-lred)
    (inf-ruby-mode                      "oct-ruby"              nerd-icons-red)
    (projectile-rails-compilation-mode  "oct-ruby"              nerd-icons-red)
    (rspec-compilation-mode             "oct-ruby"              nerd-icons-red)
    (rake-compilation-mode              "oct-ruby"              nerd-icons-red)
    (sh-mode                            "oct-terminal"          nerd-icons-purple)
    (shell-mode                         "oct-terminal"          nerd-icons-purple)
    (fish-mode                          "oct-terminal"          nerd-icons-lpink)
    (nginx-mode                         "dev-nginx"             nerd-icons-dgreen)
    (apache-mode                        "md-apache_kafka"            nerd-icons-dgreen)
    (makefile-mode                      "seti-makefile"          nerd-icons-dorange)
    (cmake-mode                         "md-cogs"             nerd-icons-red)
    (dockerfile-mode                    "seti-docker"            nerd-icons-blue)
    (docker-compose-mode                "seti-docker"            nerd-icons-lblue)
    (nxml-mode                          "cod-code"              nerd-icons-lorange)
    (json-mode                          "seti-settings"          nerd-icons-yellow)
    (jsonian-mode                       "seti-settings"          nerd-icons-yellow)
    (yaml-mode                          "seti-settings"          nerd-icons-dyellow)
    (elisp-byte-code-mode               "oct-file_binary"            nerd-icons-dsilver)
    (archive-mode                       "oct-archive"          nerd-icons-lmaroon)
    (elm-mode                           "seti-elm"               nerd-icons-blue)
    (erlang-mode                        "dev-erlang"            nerd-icons-red)
    (elixir-mode                        "seti-elixir"            nerd-icons-lorange)
    (java-mode                          "fae-java"              nerd-icons-purple)
    (go-mode                            "custom-go"                nerd-icons-blue)
    (go-dot-mod-mode                    "seti-go"         nerd-icons-blue-alt)
    (go-dot-work-mode                   "seti-go"         nerd-icons-blue-alt)
    (graphql-mode                       "seti-graphql"           nerd-icons-dpink)
    (matlab-mode                        "md-math_compass"            nerd-icons-orange)
    (nix-mode                           "md-nix"               nerd-icons-blue)
    (perl-mode                          "dev-perl"              nerd-icons-lorange)
    (cperl-mode                         "dev-perl"              nerd-icons-lorange)
    (php-mode                           "seti-php"               nerd-icons-lsilver)
    (prolog-mode                        "dev-prolog"            nerd-icons-lmaroon)
    (python-mode                        "seti-python"            nerd-icons-dblue)
    (inferior-python-mode               "seti-python"            nerd-icons-dblue)
    (racket-mode                        "oct-file_code"            nerd-icons-red)
    (rust-mode                          "seti-rust"              nerd-icons-maroon)
    (scala-mode                         "seti-scala"             nerd-icons-red)
    (scheme-mode                        "oct-file_code"            nerd-icons-red)
    (swift-mode                         "seti-swift"             nerd-icons-green)
    (svelte-mode                        "seti-svelte"            nerd-icons-red)
    (c-mode                             "seti-c"            nerd-icons-blue)
    (c++-mode                           "md-language_cpp"    nerd-icons-blue)
    (csharp-mode                        "md-language_csharp"       nerd-icons-dblue)
    (clojure-mode                       "seti-clojure"       nerd-icons-blue)
    (cider-repl-mode                    "seti-clojure"       nerd-icons-green)
    (clojurescript-mode                 "seti-clojure"         nerd-icons-dblue)
    (coffee-mode                        "dev-coffeescript"      nerd-icons-maroon)
    (lisp-mode                          "oct-file_code"              nerd-icons-orange)
    (css-mode                           "md-language_css3"              nerd-icons-yellow)
    (scss-mode                          "dev-sass"              nerd-icons-pink)
    (sass-mode                          "dev-sass"              nerd-icons-dpink)
    (less-css-mode                      "dev-sass"              nerd-icons-dyellow)
    (stylus-mode                        "dev-stylus"            nerd-icons-lgreen)
    (csv-mode                           "cod-graph_line"             nerd-icons-dblue)
    (haskell-mode                       "seti-haskell"           nerd-icons-red)
    (haskell-c2hs-mode                  "seti-haskell"           nerd-icons-red)
    (literate-haskell-mode              "seti-haskell"           nerd-icons-red)
    (haml-mode                          "fa-html5"              nerd-icons-lyellow)
    (html-mode                          "fa-html5"             nerd-icons-orange)
    (rhtml-mode                         "fa-html5"             nerd-icons-lred)
    (mustache-mode                      "oct-file_code"         nerd-icons-green)
    (slim-mode                          "cod-dashboard"         nerd-icons-yellow)
    (jade-mode                          "seti-jade"              nerd-icons-red)
    (pug-mode                           "seti-pug"           nerd-icons-red)
    (image-mode                         "seti-image"             nerd-icons-blue)
    (texinfo-mode                       "seti-tex"               nerd-icons-lred)
    (markdown-mode                      "md-language_markdown"          nerd-icons-lblue)
    (bibtex-mode                        "seti-tex"            nerd-icons-maroon)
    (org-mode                           "custom-orgmode"               nerd-icons-lgreen)
    (compilation-mode                   "md-cogs")
    (objc-mode                          "md-apple")
    (tuareg-mode                        "seti-ocaml")
    (purescript-mode                    "seti-purescript")
    (verilog-mode                       "oct-file_code"           nerd-icons-red)
    (vhdl-mode                          "oct-file_code"              nerd-icons-blue)
    (haskell-cabal-mode                 "oct-file_code"             nerd-icons-lblue)
    (kotlin-mode                        "seti-kotlin"            nerd-icons-orange)
    (nim-mode                           "oct-file_code"            nerd-icons-yellow)
    (sql-mode                           "fa-database"          nerd-icons-silver)
    (lua-mode                           "seti-lua"               nerd-icons-dblue)
    (adoc-mode                          "oct-file_code"          nerd-icons-lblue)
    (puppet-mode                        "seti-puppet"            nerd-icons-yellow)
    (jinja2-mode                        "seti-jinja"             nerd-icons-silver)
    (powershell-mode                    "md-powershell"        nerd-icons-blue)
    (tex-mode                           "seti-tex"               nerd-icons-lred)
    (latex-mode                         "seti-tex"               nerd-icons-lred)
    (dart-mode                          "dev-dart"              nerd-icons-blue)
    (fsharp-mode                        "dev-fsharp"            nerd-icons-blue)
    (asm-mode                           "oct-file_code"  nerd-icons-blue)
    (nasm-mode                          "oct-file_code"  nerd-icons-blue)
    (tcl-mode                           "oct-file_code"               nerd-icons-dred)
    (cuda-mode                          "oct-file_code"            nerd-icons-green)
    (f90-mode                           "md-language_fortran"           nerd-icons-purple)
    (hy-mode                            "oct-file_code"                nerd-icons-blue)
    (glsl-mode                          "oct-file_code"      nerd-icons-green)
    (zig-mode                           "oct-file_code"               nerd-icons-orange)
    (odin-mode                          "oct-file_code"              nerd-icons-lblue)
    (pdf-view-mode                      "seti-pdf"          nerd-icons-dred)
    (elfeed-search-mode                 "fa-rss_square"        nerd-icons-orange)
    (elfeed-show-mode                   "fa-rss"               nerd-icons-orange)
    (lilypond-mode                      "md-music"             nerd-icons-green)
    (magik-session-mode                 "oct-terminal"          nerd-icons-blue)
    (magik-cb-mode                      "cod-book"              nerd-icons-blue)
    (dashboard-mode                     "cod-dashboard"         nerd-icons-orange)
    ))

(defvar nerd-icons-symbol-kind-icon-alist
  '(
    ;; C, C++, java, python
    ("file"           "cod-symbol-file"               nerd-icons-lpurple 0.95)
    ("function"       "cod-symbol_method"                  nerd-icons-purple 0.95)
    ("method"         "cod-symbol_method"                  nerd-icons-purple 0.95)
    ("prototype"      "cod-symbol_method"                  nerd-icons-purple 0.95)
    ("annotation"     "cod-symbol_method"                  nerd-icons-purple 0.95)
    ("constructor"    "cod-symbol_method"                  nerd-icons-orange 0.95)
    ("class"          "cod-symbol_class"              nerd-icons-lorange)
    ("struct"         "cod-symbol_class"              nerd-icons-lorange)
    ("interface"      "cod-symbol_class"              nerd-icons-lorange)
    ("union"          "cod-symbol_misc"               nerd-icons-lorange 0.95)
    ("enum"           "cod-symbol_enum"         nerd-icons-lorange)
    ("enumerator"     "cod-symbol_enum_member"  nerd-icons-lblue 0.9)
    ("enummember"     "cod-symbol_enum_member"  nerd-icons-lblue 0.9)
    ("using"          "cod-symbol_namespace"          nerd-icons-dyellow)
    ("namespace"      "cod-symbol_namespace"          nerd-icons-dyellow)
    ("variable"       "cod-symbol_field"                  nerd-icons-lblue 0.95)
    ("member"         "cod-symbol_field"                  nerd-icons-lblue 0.95)
    ("field"          "cod-symbol_field"                  nerd-icons-lblue 0.95)
    ("externvar"      "cod-symbol_field"                  nerd-icons-dorange 0.95)
    ("local"          "cod-symbol_variable"            nerd-icons-dblue 1.1)
    ("macro"          "md-arrow_expand"                     nerd-icons-purple 0.85)
    ("string"         "cod-symbol_string"             nerd-icons-blue 0.9)
    ("boolean"        "cod-symbol_boolean"            nerd-icons-lpurple 0.9)
    ("array"          "cod-symbol_array"              nerd-icons-maroon 0.85)
    ("number"         "cod-symbol_numeric"            nerd-icons-lgreen 0.85)
    ("object"         "cod-symbol_namespace"          nerd-icons-lgreen 0.95)
    ("misc"           "cod-symbol_misc"               nerd-icons-lgreen 0.95)
    ("operator"       "cod-symbol_operator"           nerd-icons-orange 0.9)
    ("parameter"      "cod-symbol_parameter"          nerd-icons-dpurple 1.1)
    ("macroparam"     "cod-symbol_parameter"          nerd-icons-purple 1.1)
    ("typeparameter"  "cod-symbol_parameter"          nerd-icons-lmaroon 1.1)
    ("tparam"         "cod-symbol_parameter"          nerd-icons-lmaroon 1.1)
    ("event"          "cod-symbol_event"              nerd-icons-yellow 0.95)
    ("typedef"        "cod-references"                nerd-icons-lmaroon 0.8)
    ("package"        "cod-package"                   nerd-icons-lblue 0.9)
    ("module"         "cod-package"                   nerd-icons-lblue 0.9)
    ("key"            "cod-symbol_key"                nerd-icons-dblue 1.05)
    ("null"           "weather-na"                     nerd-icons-lmaroon 1.5)

    ;; Elisp
    ("derivedMode"  "md-cogs"                     nerd-icons-purple 0.9)
    ("majorMode"    "md-cogs"                     nerd-icons-purple 0.9)
    ("command"      "md-apple_keyboard_command"     nerd-icons-purple 0.9)
    ("minorMode"    "md-cogs"                     nerd-icons-purple 0.9)
    ("inline"       "cod-symbol_method"                  nerd-icons-purple 0.95)
    ("subst"        "cod-symbol_method"                  nerd-icons-purple 0.95)
    ("group"        "cod-package"                   nerd-icons-lblue 0.9)
    ("error"        "cod-error"                     nerd-icons-lblue)
    ("custom"       "seti-settings"                  nerd-icons-orange)
    ("face"         "fae-palette_color"                     nerd-icons-red)
    ("const"        "cod-symbol_constant"           nerd-icons-lgreen)
    ("symbol"       "cod-symbol_key"             nerd-icons-dyellow 0.9)
    ("alias"        "cod-references"                nerd-icons-lmaroon 0.8)
    ("unknown"      "cod-question"           nerd-icons-dyellow 0.9)

    ;; JavaScript, TypeScript
    ("constant"     "cod-symbol_constant"           nerd-icons-lgreen)
    ("property"     "cod-symbol_property"           nerd-icons-blue)

    ;; Markdown
    ("chapter"      "fa-sticky_note"               nerd-icons-yellow)
    ("section"      "md-format_section"                   nerd-icons-lorange 0.9)
    ("subsection"   "md-format_section"                   nerd-icons-orange 0.8)

    ;; Org
    ("part"         "fa-pagelines"                 nerd-icons-lmaroon)
    ))

(defvar nerd-icons-default-mode-icon
  '("md-cogs" nerd-icons-dsilver))

;; Function start ------------------------------------------------------------ ;

(defun nerd-icons--match-to-alist (file alist)
  "Match FILE against an entry in ALIST using `string-match'."
  (assoc file alist (lambda (a b) (string-match a b))))

(defun nerd-icons-dir-is-submodule (dir)
  "Checker whether or not DIR is a git submodule."
  (let* ((gitmodule-dir (locate-dominating-file dir ".gitmodules"))
         (modules-file  (expand-file-name (format "%s.gitmodules" gitmodule-dir)))
         (module-search (format "submodule \".*?%s\"" (file-name-base dir))))
    (when (and gitmodule-dir (file-exists-p (format "%s/.git" dir)))
      (with-temp-buffer
        (insert-file-contents modules-file)
        (search-forward-regexp module-search (point-max) t)))))

;;;###autoload
(defun nerd-icons-icon-for-dir (dir &rest args)
  "Get the formatted icon for DIR.

ARGS should be a plist containining `:face' or `:scale'."
  (let ((path (expand-file-name dir)))
    (cond
     ((file-remote-p path)
      (apply #'nerd-icons-icon-str "oct-terminal"
             (append args '(:face nerd-icons-blue))))
     ((file-symlink-p path)
      (apply #'nerd-icons-icon-str "md-folder_move"
             (append args '(:face nerd-icons-blue))))
     ((nerd-icons-dir-is-submodule path)
      (apply #'nerd-icons-icon-str "md-folder_move"
             (append args '(:face nerd-icons-blue))))
     ((file-exists-p (format "%s/.git" path))
      (apply #'nerd-icons-icon-str "oct-repo"
             (append args '(:face nerd-icons-blue))))
     (t
      (let* ((dir-name (file-name-base (directory-file-name dir)))
             (match (or (cdr (nerd-icons--match-to-alist
                              dir-name
                              nerd-icons-dir-regexp-icon-alist))
                        nerd-icons-default-dir-icon))
             (icon-name (car match))
             (face (cadr match)))
        (apply #'nerd-icons-icon-str icon-name
               (append args `(:face ,(or face 'nerd-icons-blue)))))))))

;;;###autoload
(defun nerd-icons-icon-for-str (str &rest args)
  "Get the formatted icon for STR.

ARGS should be a plist containining `:face' or `:scale'."
  (when-let ((match (nerd-icons--match-to-alist
                     str nerd-icons-regexp-icon-alist)))
    (apply #'nerd-icons-icon-str (cadr match)
           (append args `(:face ,(caddr match))))))

;;;###autoload
(defun nerd-icons-icon-for-file (file &rest args)
  "Get the formatted icon for FILE.

ARGS should be a plist containining `:face' or `:scale'."
  (let* ((ext (file-name-extension file))
         (match (or (cdr (nerd-icons--match-to-alist
                          file nerd-icons-regexp-icon-alist))
                    (and ext (cdr
                              (assoc (downcase ext)
                                     nerd-icons-extension-icon-alist)))
                    nerd-icons-default-file-icon)))
    (apply #'nerd-icons-icon-str (car match)
           (append args `(:face ,(cadr match))))))

;;;###autoload
(defun nerd-icons-icon-for-mode (mode &rest args)
  "Get the formatted icon for MODE.

ARGS should be a plist containining `:face' or `:scale'."
  (let* ((mode0 mode)
         (match (assoc mode0 nerd-icons-mode-icon-alist)))
    (while (and mode0 (not match))
      (setq mode0 (get mode0 'derived-mode-parent))
      (setq match (assoc mode0 nerd-icons-mode-icon-alist)))
    (if match
        (apply #'nerd-icons-icon-str (cadr match)
               (append args `(:face ,(caddr match))))
      (apply #'nerd-icons-icon-str "md-cogs"
             (append args '(:face nerd-icons-purple))))))

;;;###autoload
(defun nerd-icons-icon-for-symbol-kind (kind &rest args)
  "Get the formatted icon for symbol KIND.

ARGS should be a plist containining `:face' or `:scale'."
  (if-let* ((spec (cdr (assoc kind nerd-icons-symbol-kind-icon-alist)))
            (icon-str (apply #'nerd-icons-icon-str (car spec)
                             (append args
                                     `(:face ,(cadr spec))
                                     `(:scale ,(caddr spec)))))
            ((not (string-empty-p icon-str))))
      icon-str
    (nerd-icons-icon-str "fa-tag" :face 'nerd-icons-pink)))

;; Overriding all-the-icons -------------------------------------------------- ;

;;;###autoload
(define-minor-mode nerd-icons-override-mode
  "Override `all-the-icons' functions with `nerd-icons` ones."
  :global t
  (if nerd-icons-override-mode
      (progn
        (require 'all-the-icons)
        (advice-add #'all-the-icons-alltheicon :override #'nerd-icons-icon-str)
        (advice-add #'all-the-icons-fileicon :override #'nerd-icons-icon-str)
        (advice-add #'all-the-icons-octicon :override #'nerd-icons-icon-str)
        (advice-add #'all-the-icons-material :override #'nerd-icons-icon-str)
        (advice-add #'all-the-icons-faicon :override #'nerd-icons-icon-str)
        (advice-add #'all-the-icons-wicon :override #'nerd-icons-icon-str))
    (advice-remove #'all-the-icons-alltheicon #'nerd-icons-icon-str)
    (advice-remove #'all-the-icons-fileicon #'nerd-icons-icon-str)
    (advice-remove #'all-the-icons-octicon #'nerd-icons-icon-str)
    (advice-remove #'all-the-icons-material #'nerd-icons-icon-str)
    (advice-remove #'all-the-icons-faicon #'nerd-icons-icon-str)
    (advice-remove #'all-the-icons-wicon #'nerd-icons-icon-str)))

(provide 'nerd-icons)

;;; nerd-icons.el ends here
