;;; icon-tools.el --- Tools for icons in Emacs  -*- lexical-binding: t; -*-

;; Author: Shihao Liu
;; Keywords: treemacs nerd icon
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

(defun icon-tools-svg-icon-cache-add (icon icon-name &optional face)
  (puthash (format "%s-%s-%d-%d" icon-name (symbol-name (or face 'default))
                   (window-font-width) (window-font-height))
           icon icon-tools-svg-icon-cache))

(defun icon-tools-svg-icon-cache-get (icon-name &optional face)
  (gethash (format "%s-%s-%d-%d" icon-name (symbol-name (or face 'default))
                   (window-font-width) (window-font-height))
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
              (when (color-defined-p (cdr attr))
                (setcdr attr fg-color)))
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
    ("database" . 0.9)
    ("file-directory" . 1.05)
    ("emacs" . 1.05)
    ("file" . 1.1)
    ;; ("file-media" . 1.05)
    ;; ("file-pdf" . 1.05)
    ;; ("file-binary" . 1.05)
    ("file-xls" . 1.05)
    ("file-doc" . 1.05)
    ("file-ppt" . 1.05)
    ("file-zip" . 1.05)
    ("film" . 0.9)
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

(defun icon-tools-svg-icon (icon-name &optional face scale)
  "Build the icon ICON-NAME.

Icon is drawn with the foreground of FACE.

The third argument SCALE, if provided, scales the icon."

  (let ((cache-item (icon-tools-svg-icon-cache-get icon-name face)))
    ;; (if cache-item
    (if nil
        cache-item
      (let* ((root (icon-tools-svg-icon-parse icon-name))

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

             ;; Scale
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

        (icon-tools-svg-icon-cache-add (svg-image svg :ascent 'center :scale 1)
                                       icon-name face)))))

(defun icon-tools-svg-icon-str (icon-name &optional face scale)
  (if (image-type-available-p 'svg)
      (propertize
       (make-string icon-tools-icon-width ?\-)
       'display (icon-tools-svg-icon icon-name face scale))
    ""))

(defun icon-tools-nerd-icon-str (icon-name &optional face)
  (propertize (or (cdr (assoc icon-name icon-tools-data-nerd-alist)) "")
              'face `(:foreground ,(face-attribute (or face 'default) :foreground))))

(defun icon-tools-icon-str (icon-name &optional face scale)
  (if (display-graphic-p)
      (icon-tools-svg-icon-str icon-name face scale)
    (icon-tools-nerd-icon-str icon-name face)))

;; Icon alists --------------------------------------------------------------- ;

(defvar icon-tools-extension-icon-alist
  '(
    ("fish"                 "terminal"              icon-tools-lpink)
    ("zsh"                  "terminal"              icon-tools-lcyan)
    ("sh"                   "terminal"              icon-tools-purple)
    ("terminal"             "terminal"              icon-tools-purple)
    ("bat"                  "terminal"              icon-tools-purple)
    ;; Meta
    ("tags"                 "tag"                   icon-tools-blue)
    ("log"                  "bug"                   icon-tools-maroon)
    ("aux"                  "bug"                   icon-tools-maroon)
    ("nav"                  "bug"                   icon-tools-maroon)
    ("snm"                  "bug"                   icon-tools-maroon)
    ("toc"                  "bug"                   icon-tools-maroon)
    ("vrb"                  "bug"                   icon-tools-maroon)

    ;; binary
    ("exe"                  "file-binary"           icon-tools-dsilver)
    ("dll"                  "gears"                 icon-tools-dsilver)
    ("lib"                  "file-binary"           icon-tools-dsilver)
    ("obj"                  "file-binary"           icon-tools-dsilver)
    ("so"                   "file-binary"           icon-tools-dsilver)
    ("o"                    "file-binary"           icon-tools-dsilver)
    ("d"                    "file-binary"           icon-tools-dsilver)
    ("out"                  "file-binary"           icon-tools-dsilver)
    ("elc"                  "file-binary"           icon-tools-dsilver)
    ("eln"                  "file-binary"           icon-tools-dsilver)
    ("cmake-cache"          "file-binary"           icon-tools-dsilver)
    ("csr"                  "file-binary"           icon-tools-dsilver)
    ("eslintcache"          "file-binary"           icon-tools-dsilver)
    ("cer"                  "file-binary"           icon-tools-dsilver)
    ("der"                  "file-binary"           icon-tools-dsilver)
    ("pfx"                  "file-binary"           icon-tools-dsilver)
    ("p7b"                  "file-binary"           icon-tools-dsilver)
    ("p7r"                  "file-binary"           icon-tools-dsilver)
    ("DS_STORE"             "file-binary"           icon-tools-dsilver)
    ("src"                  "file-binary"           icon-tools-dsilver)
    ("crl"                  "file-binary"           icon-tools-dsilver)
    ("sst"                  "file-binary"           icon-tools-dsilver)
    ("stl"                  "file-binary"           icon-tools-dsilver)
    ("pyc"                  "file-binary"           icon-tools-dsilver)
    ("bin"                  "file-binary"           icon-tools-dsilver)

    ;; torrent
    ("torrent"              "file-arrow-down"       icon-tools-dsilver)
    ("aria2"                "file-arrow-down"       icon-tools-dsilver)
    ("ed2k"                 "file-arrow-down"       icon-tools-dsilver)
    ;; book
    ("azw"                  "book"                  icon-tools-dsilver)
    ("azw3"                 "book"                  icon-tools-dsilver)
    ("mobi"                 "book"                  icon-tools-dsilver)
    ("epub"                 "book"                  icon-tools-dsilver)
    ;; ?
    ("pkg"                  "package"               icon-tools-dsilver)
    ("rpm"                  "package"               icon-tools-dsilver)
    ("tar"                  "file-zip"              icon-tools-lmaroon)
    ("rar"                  "file-zip"              icon-tools-lmaroon)
    ("gz"                   "file-zip"              icon-tools-lmaroon)
    ("zip"                  "file-zip"              icon-tools-lmaroon)
    ("7z"                   "file-zip"              icon-tools-lmaroon)
    ("xz"                   "file-zip"              icon-tools-lmaroon)
    ("tgz"                  "file-zip"              icon-tools-lmaroon)
    ("dat"                  "chart-bar"             icon-tools-cyan)
    ("edn"                  "chart-bar"             icon-tools-cyan)
    ("dmg"                  "tools"                 icon-tools-lsilver)
    ;; Source Codes
    ("scpt"                 "apple"                 icon-tools-pink)
    ("aup"                  "audacity"              icon-tools-yellow)
    ("elm"                  "elm"                   icon-tools-blue)
    ("erl"                  "erlang"                icon-tools-red)
    ("hrl"                  "erlang"                icon-tools-dred)
    ("eex"                  "elixir"                icon-tools-lorange)
    ("leex"                 "elixir"                icon-tools-lorange)
    ("heex"                 "elixir"                icon-tools-lorange)
    ("ex"                   "elixir"                icon-tools-lpurple)
    ("exs"                  "elixir"                icon-tools-lred)
    ("java"                 "java"                  icon-tools-purple)
    ("jar"                  "java"                  icon-tools-purple)
    ("gradle"               "gradle"                icon-tools-silver)
    ("ebuild"               "gentoo"                icon-tools-cyan)
    ("eclass"               "gentoo"                icon-tools-blue)
    ("go"                   "go"                    icon-tools-blue)
    ("jl"                   "julia"                 icon-tools-purple)
    ("magik"                "wand-magic-sparkles"   icon-tools-blue)
    ("matlab"               "matlab"                icon-tools-orange)
    ("nix"                  "nix"                   icon-tools-blue)
    ("pl"                   "perl"                  icon-tools-lorange)
    ("pm"                   "perl"                  icon-tools-lorange)
    ("pod"                  "perldocs"              icon-tools-lgreen)
    ("php"                  "php"                   icon-tools-lsilver)
    ("pony"                 "pony"                  icon-tools-maroon)
    ("ps1"                  "powershell"            icon-tools-blue)
    ("pro"                  "prolog"                icon-tools-lmaroon)
    ("proog"                "prolog"                icon-tools-lmaroon)
    ("py"                   "python"                icon-tools-dblue)
    ("py.typed"             "python"                icon-tools-pink)
    ("idr"                  "idris"                 icon-tools-red)
    ("ipynb"                "jupyter"               icon-tools-dorange)
    ("gem"                  "ruby-alt"              icon-tools-red)
    ("rb"                   "ruby"                  icon-tools-lred)
    ("rs"                   "rust"                  icon-tools-maroon)
    ("rlib"                 "rust"                  icon-tools-dmaroon)
    ("r"                    "R"                     icon-tools-purple)
    ("rd"                   "R"                     icon-tools-purple)
    ("rdx"                  "R"                     icon-tools-purple)
    ("rsx"                  "R"                     icon-tools-purple)
    ("svelte"               "svelte"                icon-tools-red)
    ("gql"                  "graphql"               icon-tools-dpink)
    ("graphql"              "graphql"               icon-tools-dpink)
    ("c"                    "c-line"                icon-tools-blue)
    ("h"                    "c-line"                icon-tools-purple)
    ("h.in"                 "c-line"                icon-tools-lblue)
    ("m"                    "apple")
    ("mm"                   "apple")
    ;;
    ("cc"                   "cplusplus-line"        icon-tools-blue)
    ("cpp"                  "cplusplus-line"        icon-tools-blue)
    ("cxx"                  "cplusplus-line"        icon-tools-blue)
    ("hh"                   "cplusplus-line"        icon-tools-purple)
    ("hpp"                  "cplusplus-line"        icon-tools-purple)
    ("hpp.in"               "cplusplus-line"        icon-tools-lblue)
    ("hxx"                  "cplusplus-line"        icon-tools-purple)
    ;; Lisps
    ("cl"                   "common-lisp"           icon-tools-lorange)
    ("l"                    "lisp"                  icon-tools-orange)
    ("lisp"                 "lisp"                  icon-tools-orange)
    ("hy"                   "hy"                    icon-tools-blue)
    ("el"                   "emacs"                 icon-tools-purple)
    ("clj"                  "clojure-alt"           icon-tools-blue)
    ("cljc"                 "clojure-alt"           icon-tools-blue)
    ("cljs"                 "clojurejs"             icon-tools-dblue)
    ("coffee"               "coffeescript"          icon-tools-maroon)
    ("iced"                 "coffeescript"          icon-tools-lmaroon)
    ("dart"                 "dart"                  icon-tools-blue)
    ("rkt"                  "racket"                icon-tools-red)
    ("scrbl"                "racket"                icon-tools-blue)
    ;; Stylesheeting
    ("css"                  "css3"                  icon-tools-yellow)
    ("scss"                 "sass"                  icon-tools-pink)
    ("sass"                 "sass"                  icon-tools-dpink)
    ("less"                 "less"                  icon-tools-dyellow)
    ("postcss"              "postcss"               icon-tools-dred)
    ("sss"                  "postcss"               icon-tools-dred)
    ("styl"                 "stylus"                icon-tools-lgreen)
    ("csv"                  "graph"                 icon-tools-dblue)
    ;; haskell
    ("hs"                   "haskell"               icon-tools-red)
    ("chs"                  "haskell"               icon-tools-red)
    ("lhs"                  "haskell"               icon-tools-red)
    ("hsc"                  "haskell"               icon-tools-red)
    ;; Web modes
    ("inky-haml"            "haml"                  icon-tools-lyellow)
    ("haml"                 "haml"                  icon-tools-lyellow)
    ("htm"                  "html5"                 icon-tools-orange)
    ("html"                 "html5"                 icon-tools-orange)
    ("inky-er"              "html5"                 icon-tools-lred)
    ("inky-erb"             "html5"                 icon-tools-lred)
    ("erb"                  "html5"                 icon-tools-lred)
    ("hbs"                  "moustache"             icon-tools-green)
    ("inky-slim"            "dashboard"             icon-tools-yellow)
    ("slim"                 "dashboard"             icon-tools-yellow)
    ("jade"                 "jade"                  icon-tools-red)
    ("pug"                  "pug-old"               icon-tools-red)
    ;; Javascript
    ("d3js"                 "d3"                    icon-tools-lgreen)
    ("re"                   "reason"                icon-tools-red-alt)
    ("rei"                  "reason"                icon-tools-dred)
    ("ml"                   "ocaml"                 icon-tools-lpink)
    ("mli"                  "ocaml"                 icon-tools-dpink)
    ("react"                "react"                 icon-tools-lblue)
    ("ts"                   "typescript-alt"        icon-tools-blue-alt)
    ("js"                   "javascript-badge"      icon-tools-yellow)
    ("es"                   "javascript-badge"      icon-tools-yellow)
    ("jsx"                  "jsx-alt"               icon-tools-yellow)
    ("tsx"                  "tsx-alt"               icon-tools-blue-alt)
    ("njs"                  "nodejs"                icon-tools-lgreen)
    ("vue"                  "vue"                   icon-tools-lgreen)

    ("sbt"                  "sbt"                   icon-tools-red)
    ("scala"                "scala"                 icon-tools-red)
    ("scm"                  "scheme"                icon-tools-red)
    ("swift"                "swift"                 icon-tools-green)

    ("tcl"                  "tcl"                   icon-tools-dred)

    ("tf"                   "terraform"             icon-tools-purple-alt)
    ("tfvars"               "terraform"             icon-tools-purple-alt)
    ("tfstate"              "terraform"             icon-tools-purple-alt)

    ("asm"                  "assembly-generic"      icon-tools-blue)
    ;; Verilog(-AMS) and SystemVerilog(-AMS)
    ("v"                    "verilog"               icon-tools-red)
    ("vams"                 "verilog"               icon-tools-red)
    ("sv"                   "verilog"               icon-tools-red)
    ("sva"                  "verilog"               icon-tools-red)
    ("svh"                  "verilog"               icon-tools-red)
    ("svams"                "verilog"               icon-tools-red)
    ;; VHDL(-AMS)
    ("vhd"                  "vhdl"                  icon-tools-blue)
    ("vhdl"                 "vhdl"                  icon-tools-blue)
    ("vhms"                 "vhdl"                  icon-tools-blue)
    ;; Cabal
    ("cabal"                "cabal"                 icon-tools-lblue)
    ;; Kotlin
    ("kt"                   "kotlin"                icon-tools-orange)
    ("kts"                  "kotlin"                icon-tools-orange)
    ;; Nimrod
    ("nim"                  "nimrod"                icon-tools-yellow)
    ("nims"                 "nimrod"                icon-tools-yellow)
    ;; SQL
    ("sql"                  "database"              icon-tools-silver)
    ("db"                   "database"              icon-tools-silver)
    ("cache"                "database"              icon-tools-green)
    ;; Styles
    ("styles"               "style"                 icon-tools-red)
    ;; Lua
    ("lua"                  "lua"                   icon-tools-dblue)
    ;; ASCII doc
    ("adoc"                 "asciidoc"              icon-tools-lblue)
    ("asciidoc"             "asciidoc"              icon-tools-lblue)
    ;; Puppet
    ("pp"                   "puppet"                icon-tools-yellow)
    ;; Jinja
    ("j2"                   "jinja"                 icon-tools-silver)
    ("jinja2"               "jinja"                 icon-tools-silver)
    ;; Docker
    ("dockerfile"           "docker"                icon-tools-cyan)
    ;; Vagrant
    ("vagrantfile"          "vagrant"               icon-tools-blue)
    ;; GLSL
    ("glsl"                 "vertexshader"          icon-tools-blue)
    ("vert"                 "vertexshader"          icon-tools-blue)
    ("tesc"                 "vertexshader"          icon-tools-purple)
    ("tese"                 "vertexshader"          icon-tools-dpurple)
    ("geom"                 "vertexshader"          icon-tools-green)
    ("frag"                 "vertexshader"          icon-tools-red)
    ("comp"                 "vertexshader"          icon-tools-dblue)
    ;; CUDA
    ("cu"                   "nvidia"                icon-tools-green)
    ("cuh"                  "nvidia"                icon-tools-green)
    ;; Fortran
    ("f90"                  "fortran"               icon-tools-purple)
    ;; C#
    ("cs"                   "csharp-line"           icon-tools-dblue)
    ("csx"                  "csharp-line"           icon-tools-dblue)
    ;; F#
    ("fs"                   "fsharp"                icon-tools-blue-alt)
    ("fsi"                  "fsharp"                icon-tools-blue-alt)
    ("fsx"                  "fsharp"                icon-tools-blue-alt)
    ("fsscript"             "fsharp"                icon-tools-blue-alt)
    ;; zig
    ("zig"                  "zig"                   icon-tools-orange)
    ;; odin
    ("odin"                 "odin"                  icon-tools-lblue)
    ;; File Types
    ("ico"                  "file-media"            icon-tools-blue)
    ("png"                  "file-media"            icon-tools-orange)
    ("gif"                  "file-media"            icon-tools-green)
    ("jpeg"                 "file-media"            icon-tools-dblue)
    ("jpg"                  "file-media"            icon-tools-dblue)
    ("webp"                 "file-media"            icon-tools-dblue)
    ("svg"                  "file-media"            icon-tools-lgreen)
    ("eps"                  "file-media"            icon-tools-lgreen)
    ;; Audio
    ("mp3"                  "music"                 icon-tools-dred)
    ("wav"                  "music"                 icon-tools-dred)
    ("m4a"                  "music"                 icon-tools-dred)
    ("ogg"                  "music"                 icon-tools-dred)
    ("flac"                 "music"                 icon-tools-dred)
    ("opus"                 "music"                 icon-tools-dred)
    ("au"                   "music"                 icon-tools-dred)
    ("aif"                  "music"                 icon-tools-dred)
    ("aifc"                 "music"                 icon-tools-dred)
    ("aiff"                 "music"                 icon-tools-dred)
    ("ly"                   "music"                 icon-tools-green)
    ;; Video
    ("mov"                  "film"                  icon-tools-blue)
    ("mp4"                  "film"                  icon-tools-blue)
    ("ogv"                  "film"                  icon-tools-dblue)
    ("mpg"                  "film"                  icon-tools-blue)
    ("mpeg"                 "film"                  icon-tools-blue)
    ("flv"                  "film"                  icon-tools-blue)
    ("ogv"                  "film"                  icon-tools-dblue)
    ("mkv"                  "film"                  icon-tools-blue)
    ("webm"                 "film"                  icon-tools-blue)
    ;; Subtitle
    ("srt"                  "closed_caption"        icon-tools-dblue)
    ;; Fonts
    ("ttf"                  "font"                  icon-tools-dcyan)
    ("otf"                  "font"                  icon-tools-dcyan)
    ("woff"                 "font"                  icon-tools-cyan)
    ("woff2"                "font"                  icon-tools-cyan)
    ("ttc"                  "font"                  icon-tools-cyan)
    ;; Doc
    ("pdf"                  "file-pdf"              icon-tools-dred)
    ("text"                 "file-text"             icon-tools-cyan)
    ("txt"                  "edit"                  icon-tools-cyan)
    ("rst"                  "restructuredtext"      icon-tools-green)
    ("doc"                  "microsoft-word"        icon-tools-blue)
    ("docx"                 "microsoft-word"        icon-tools-blue)
    ("docm"                 "microsoft-word"        icon-tools-blue)
    ("texi"                 "tex"                   icon-tools-lred)
    ("tex"                  "tex"                   icon-tools-lred)
    ("sty"                  "tex"                   icon-tools-lred)
    ("pygtex"               "tex"                   icon-tools-pink)
    ("pygstyle"             "tex"                   icon-tools-pink)
    ("md"                   "markdown"              icon-tools-lblue)
    ("bib"                  "bibtex"                icon-tools-maroon)
    ("org"                  "org-mode"              icon-tools-lgreen)
    ("pps"                  "microsoft-powerpoint"  icon-tools-orange)
    ("ppt"                  "microsoft-powerpoint"  icon-tools-orange)
    ("pptsx"                "microsoft-powerpoint"  icon-tools-orange)
    ("ppttx"                "microsoft-powerpoint"  icon-tools-orange)
    ("knt"                  "microsoft-powerpoint"  icon-tools-cyan)
    ("xlsx"                 "microsoft-excel"       icon-tools-dgreen)
    ("xlsm"                 "microsoft-excel"       icon-tools-dgreen)
    ("xlsb"                 "microsoft-excel"       icon-tools-dgreen)
    ("xltx"                 "microsoft-excel"       icon-tools-dgreen)
    ("xltm"                 "microsoft-excel"       icon-tools-dgreen)
    ;; key and licence
    ("key"                  "key"                   icon-tools-lblue)
    ("pem"                  "key"                   icon-tools-orange)
    ("p12"                  "key"                   icon-tools-dorange)
    ("crt"                  "key"                   icon-tools-lblue)
    ("pub"                  "key"                   icon-tools-blue)
    ("gpg"                  "key"                   icon-tools-lblue)
    ("license.md"           "key"                   icon-tools-purple)
    ("license"              "key"                   icon-tools-purple)
    ("lic"                  "key"                   icon-tools-dblue)
    ("gemfile"              "key"                   icon-tools-dblue)
    ("bookmarks"            "bookmarks"             icon-tools-orange)

    ;; Config
    ("node"                 "nodejs"                icon-tools-green)
    ("babelrc"              "babel"                 icon-tools-yellow)
    ("bashrc"               "script"                icon-tools-dpink)
    ("bowerrc"              "bower"                 icon-tools-silver)
    ("cr"                   "crystal"               icon-tools-yellow)
    ("ecr"                  "crystal"               icon-tools-yellow)
    ("ini"                  "cogs"                  icon-tools-yellow)
    ("eslintignore"         "eslint"                icon-tools-purple)
    ("eslint"               "eslint"                icon-tools-lpurple)
    ("git"                  "git"                   icon-tools-lred)
    ("mk"                   "gnu"                   icon-tools-dorange)
    ("clang"                "llvm"                  icon-tools-dpurple)
    ("llvm"                 "llvm"                  icon-tools-dpurple)
    ("clangd"               "llvm"                  icon-tools-dpurple)
    ("cmake"                "cmake"                 icon-tools-red)
    ("cmakelists.txt"       "cmake"                 icon-tools-red)
    ("ninja"                "ninja")
    ("makefile"             "file-text"             icon-tools-cyan)
    ("dockerignore"         "docker"                icon-tools-dblue)
    ("xml"                  "file-code"             icon-tools-lorange)
    ("json"                 "settings"              icon-tools-yellow)
    ("clang-format"         "settings"              icon-tools-yellow)
    ("cson"                 "cogs"                  icon-tools-yellow)
    ("yml"                  "cogs"                  icon-tools-dyellow)
    ("yaml"                 "cogs"                  icon-tools-dyellow)
    ("toml"                 "cogs"                  icon-tools-pink)
    ("cfg"                  "cogs"                  icon-tools-dblue)
    ("terminfo"             "cogs"                  icon-tools-dblue)
    ("settings.json"        "cogs"                  icon-tools-dblue)
    ("Vagrantfile"          "cogs"                  icon-tools-silver)
    ("babel.config.js"      "cogs"                  icon-tools-silver)
    ("babelignore"          "cogs"                  icon-tools-silver)
    ("babelrc"              "cogs"                  icon-tools-silver)
    ("babelrc.js"           "cogs"                  icon-tools-silver)
    ("babelrc.json"         "cogs"                  icon-tools-silver)
    ("bashrc"               "cogs"                  icon-tools-silver)
    ("bazel"                "cogs"                  icon-tools-silver)
    ("bazelrc"              "cogs"                  icon-tools-silver)
    ("bower.json"           "cogs"                  icon-tools-silver)
    ("bowerrc"              "cogs"                  icon-tools-silver)
    ("cabal"                "cogs"                  icon-tools-silver)
    ("cfg"                  "cogs"                  icon-tools-silver)
    ("conf"                 "cogs"                  icon-tools-silver)
    ("config"               "cogs"                  icon-tools-silver)
    ("cson"                 "cogs"                  icon-tools-silver)
    ("editorconfig"         "cogs"                  icon-tools-silver)
    ("envrc"                "cogs"                  icon-tools-silver)
    ("eslintignore"         "cogs"                  icon-tools-silver)
    ("eslintrc"             "cogs"                  icon-tools-silver)
    ("feature"              "cogs"                  icon-tools-silver)
    ("gemfile"              "cogs"                  icon-tools-silver)
    ("gitattributes"        "cogs"                  icon-tools-silver)
    ("gitconfig"            "cogs"                  icon-tools-silver)
    ("gitignore"            "cogs"                  icon-tools-silver)
    ("gitmodules"           "cogs"                  icon-tools-silver)
    ("ideavimrc"            "cogs"                  icon-tools-silver)
    ("iml"                  "cogs"                  icon-tools-silver)
    ("ini"                  "cogs"                  icon-tools-silver)
    ("inputrc"              "cogs"                  icon-tools-silver)
    ("ledgerrc"             "cogs"                  icon-tools-silver)
    ("lock"                 "cogs"                  icon-tools-silver)
    ("nginx"                "cogs"                  icon-tools-silver)
    ("npm-shrinkwrap.json"  "cogs"                  icon-tools-silver)
    ("npmignore"            "cogs"                  icon-tools-silver)
    ("npmrc"                "cogs"                  icon-tools-silver)
    ("package-lock.json"    "cogs"                  icon-tools-silver)
    ("package.json"         "cogs"                  icon-tools-silver)
    ("phpunit"              "cogs"                  icon-tools-silver)
    ("pkg"                  "cogs"                  icon-tools-silver)
    ("plist"                "cogs"                  icon-tools-silver)
    ("properties"           "cogs"                  icon-tools-silver)
    ("terminalrc"           "cogs"                  icon-tools-silver)
    ("tridactylrc"          "cogs"                  icon-tools-silver)
    ("vimperatorrc"         "cogs"                  icon-tools-silver)
    ("vimrc"                "cogs"                  icon-tools-silver)
    ("vrapperrc"            "cogs"                  icon-tools-silver)
    ("xdefaults"            "cogs"                  icon-tools-silver)
    ("xml"                  "cogs"                  icon-tools-silver)
    ("xresources"           "cogs"                  icon-tools-silver)
    ("yaml"                 "cogs"                  icon-tools-silver)
    ("yarn-integrity"       "cogs"                  icon-tools-silver)
    ("yarnclean"            "cogs"                  icon-tools-silver)
    ("yarnignore"           "cogs"                  icon-tools-silver)
    ("yarnrc"               "cogs"                  icon-tools-silver)
    ("rc"                   "cogs"                  icon-tools-silver)
    ("project"              "cogs"                  icon-tools-silver)
    ("prefs"                "cogs"                  icon-tools-silver)
    ("sln"                  "visualstudio"          icon-tools-blue)
    ("vcxproj"              "visualstudio"          icon-tools-blue)
    ("vcproj"               "visualstudio"          icon-tools-blue)

    ;; model
    ("pth"                  "package"               icon-tools-dsilver)
    ("ckpt"                 "package"               icon-tools-dsilver)
    ("model"                "package"               icon-tools-dsilver)

    ;; whl
    ("whl"                  "package"               icon-tools-purple-alt)
    ))

(defvar icon-tools-regexp-icon-alist
  '(
    ;;
    ("^TAGS$"                   "tag"             icon-tools-blue)
    ("^TODO$"                   "checklist"       icon-tools-lyellow)
    ("^LICENSE$"                "book"            icon-tools-blue)
    ("^readme.md$"              "markdown"        icon-tools-lblue)
    ("^readme"                  "book"            icon-tools-lcyan)
    ("help"                     "info"            icon-tools-purple)
    ("info"                     "info"            icon-tools-pink)

    ;; Config
    ("nginx$"                   "nginx"           icon-tools-dgreen)
    ("apache$"                  "apache"          icon-tools-dgreen)

    ;; C
    ("^Makefile$"               "gnu"             icon-tools-dorange)
    ("^CMakeLists.txt$"         "cmake"           icon-tools-red)
    ("^CMakeCache.txt$"         "cmake"           icon-tools-blue)
    ("cmake"                    "cmake"           icon-tools-red)

    ;; Visual Studio
    ("vcxproj"                  "visualstudio"    icon-tools-blue)
    ("vcproj"                   "visualstudio"    icon-tools-blue)

    ;; Docker
    ("^\\.?Dockerfile"          "docker"          icon-tools-blue)

    ;; Homebrew
    ("^Brewfile$"               "homebrew"        icon-tools-lsilver)

    ;; ;; AWS
    ("^stack.*.json$"           "aws"             icon-tools-orange)
    ("^serverless\\.yml$"       "bolt"            icon-tools-yellow)

    ;; lock files
    ("~$"                       "lock"            icon-tools-maroon)

    ;; Source Codes
    ("^mix.lock$"               "elixir"          icon-tools-lyellow)

    ;; Ruby
    ("^Gemfile\\(\\.lock\\)?$"  "config-ruby"     icon-tools-red)
    ("_?test\\.rb$"             "test-ruby"       icon-tools-red)
    ("_?test_helper\\.rb$"      "test-ruby"       icon-tools-dred)
    ("_?spec\\.rb$"             "test-ruby"       icon-tools-red)
    ("_?spec_helper\\.rb$"      "test-ruby"       icon-tools-dred)

    ("-?spec\\.ts$"             "test-typescript" icon-tools-blue)
    ("-?test\\.ts$"             "test-typescript" icon-tools-blue)
    ("-?spec\\.js$"             "test-js"         icon-tools-lpurple)
    ("-?test\\.js$"             "test-js"         icon-tools-lpurple)
    ("-?spec\\.jsx$"            "test-react"      icon-tools-blue-alt)
    ("-?test\\.jsx$"            "test-react"      icon-tools-blue-alt)

    ;; Git
    ("^MERGE_"                  "git-merge"       icon-tools-red)
    ("^COMMIT_EDITMSG"          "git-commit"      icon-tools-red)

    ;; Stylesheeting
    ("stylelint"                "stylelint"       icon-tools-lyellow)

    ;; JavaScript
    ("^package.json$"           "npm"             icon-tools-red)
    ("^package.lock.json$"      "npm"             icon-tools-dred)
    ("^yarn\\.lock"             "yarn"            icon-tools-blue-alt)
    ("\\.npmignore$"            "npm"             icon-tools-dred)
    ("^bower.json$"             "bower"           icon-tools-lorange)
    ("^gulpfile"                "gulp"            icon-tools-lred)
    ("^gruntfile"               "grunt"           icon-tools-lyellow)
    ("^webpack"                 "webpack"         icon-tools-lblue)

    ;; Go
    ("^go.mod$"                 "config-go"       icon-tools-blue-alt)
    ("^go.work$"                "config-go"       icon-tools-blue-alt)

    ;; Emacs
    ("bookmarks"                "bookmarks"       icon-tools-orange)
    ("bookmark"                 "bookmark"        icon-tools-orange)

    ("^\\*scratch\\*$"          "sticky_note"     icon-tools-lyellow)
    ("^\\*scratch.*"            "sticky_note"     icon-tools-yellow)
    ("^\\*new-tab\\*$"          "star"            icon-tools-cyan)

    ("\\.git"                   "git"             icon-tools-yellow)

    ("^\\."                     "gears")
    ))

(defvar icon-tools-default-file-icon
  '("file" icon-tools-dsilver))

(defvar icon-tools-dir-regexp-icon-alist
  '(
    ("trash"            "trash")
    ("dropbox"          "dropbox")
    ("google[ _-]drive" "google-drive")
    ("^atom$"           "atom")
    ("documents"        "book")
    ("download"         "cloud-download")
    ("desktop"          "device-desktop")
    ("pictures"         "images-alt")
    ("photos"           "camera-retro")
    ("music"            "music")
    ("movies"           "film")
    ("code"             "code")
    ("workspace"        "workspace")
    ("test"             "test-directory")
    ("\\.git"           "git")
    ))

(defvar icon-tools-default-dir-icon
  '("file-directory" icon-tools-dsilver))

(defvar icon-tools-weather-icon-alist
  '(
    ("tornado"               "tornado")
    ("hurricane"             "hurricane")
    ("thunderstorms"         "thunderstorm")
    ("sunny"                 "day-sunny")
    ("rain.*snow"            "rain-mix")
    ("rain.*hail"            "rain-mix")
    ("sleet"                 "sleet")
    ("hail"                  "hail")
    ("drizzle"               "sprinkle")
    ("rain"                  "showers")
    ("showers"               "showers")
    ("blowing.*snow"         "snow-wind")
    ("snow"                  "snow")
    ("dust"                  "dust")
    ("fog"                   "fog")
    ("haze"                  "day-haze")
    ("smoky"                 "smoke")
    ("blustery"              "cloudy-windy")
    ("windy"                 "cloudy-gusts")
    ("cold"                  "snowflake-cold")
    ("partly.*cloudy.*night" "night-alt-partly-cloudy")
    ("partly.*cloudy"        "day-cloudy-high")
    ("cloudy.*night"         "night-alt-cloudy")
    ("cxloudy.*day"          "day-cloudy")
    ("cloudy"                "cloudy")
    ("clear.*night"          "night-clear")
    ("fair.*night"           "stars")
    ("fair.*day"             "horizon")
    ("hot"                   "hot")
    ("not.*available"        "na")
    ))

(defvar icon-tools-mode-icon-alist
  '(
    (emacs-lisp-mode                    "emacs"             icon-tools-purple)
    (circe-server-mode                  "commenting-dots")
    (circe-channel-mode                 "commenting-dots")
    (crystal-mode                       "crystal"           icon-tools-yellow)
    (erc-mode                           "commenting-o")
    (inferior-emacs-lisp-mode           "emacs"             icon-tools-lblue)
    (dired-mode                         "workspace")
    (lisp-interaction-mode              "lisp"              icon-tools-orange)
    (sly-mrepl-mode                     "common-lisp"       icon-tools-orange)
    (slime-repl-mode                    "common-lisp"       icon-tools-orange)
    (org-mode                           "org-mode"          icon-tools-lgreen)
    (typescript-mode                    "typescript-alt"    icon-tools-blue-alt)
    (react-mode                         "react"             icon-tools-lblue)
    (js-mode                            "javascript-badge"  icon-tools-yellow)
    (js-jsx-mode                        "react"             icon-tools-yellow)
    (js2-mode                           "javascript-badge"  icon-tools-yellow)
    (js3-mode                           "javascript-badge"  icon-tools-yellow)
    (rjsx-mode                          "react"             icon-tools-cyan-alt)
    (term-mode                          "terminal")
    (vterm-mode                         "terminal")
    (eshell-mode                        "terminal"          icon-tools-purple)
    (magit-refs-mode                    "git-branch"        icon-tools-red)
    (magit-process-mode                 "mark-github")
    (magit-diff-mode                    "git-compare"       icon-tools-lblue)
    (ediff-mode                         "git-compare"       icon-tools-red)
    (comint-mode                        "terminal"          icon-tools-lblue)
    (eww-mode                           "firefox"           icon-tools-red)
    (org-agenda-mode                    "checklist"         icon-tools-lgreen)
    (cfw:calendar-mode                  "calendar-check")
    (ibuffer-mode                       "files"             icon-tools-dsilver)
    (messages-buffer-mode               "message"           icon-tools-dsilver)
    (help-mode                          "info"              icon-tools-purple)
    (Info-mode                          "info"              icon-tools-pink)
    (benchmark-init/tree-mode           "dashboard")
    (jenkins-mode                       "jenkins"           icon-tools-blue)
    (magit-popup-mode                   "git"               icon-tools-red)
    (magit-status-mode                  "git"               icon-tools-lred)
    (magit-log-mode                     "git"               icon-tools-green)
    (mu4e-compose-mode                  "pencil")
    (mu4e-headers-mode                  "mail")
    (mu4e-main-mode                     "mail")
    (mu4e-view-mode                     "mail-read")
    (package-menu-mode                  "archive"           icon-tools-silver)
    (paradox-menu-mode                  "archive"           icon-tools-silver)
    (Custom-mode                        "settings")
    (web-mode                           "webpack-old"       icon-tools-purple)
    (fundamental-mode                   "file-text"         icon-tools-dsilver)
    (special-mode                       "info"              icon-tools-yellow)
    (text-mode                          "file-text"         icon-tools-cyan)
    (enh-ruby-mode                      "ruby"              icon-tools-lred)
    (ruby-mode                          "ruby"              icon-tools-lred)
    (inf-ruby-mode                      "ruby"              icon-tools-red)
    (projectile-rails-compilation-mode  "ruby"              icon-tools-red)
    (rspec-compilation-mode             "ruby"              icon-tools-red)
    (rake-compilation-mode              "ruby"              icon-tools-red)
    (sh-mode                            "terminal"          icon-tools-purple)
    (shell-mode                         "terminal"          icon-tools-purple)
    (fish-mode                          "terminal"          icon-tools-lpink)
    (nginx-mode                         "nginx"             icon-tools-dgreen)
    (apache-mode                        "apache"            icon-tools-dgreen)
    (makefile-mode                      "gnu"               icon-tools-dorange)
    (cmake-mode                         "cmake"             icon-tools-red)
    (dockerfile-mode                    "docker"            icon-tools-blue)
    (docker-compose-mode                "docker"            icon-tools-lblue)
    (nxml-mode                          "code"              icon-tools-lorange)
    (json-mode                          "settings"          icon-tools-yellow)
    (jsonian-mode                       "settings"          icon-tools-yellow)
    (yaml-mode                          "settings"          icon-tools-dyellow)
    (elisp-byte-code-mode               "binary"            icon-tools-dsilver)
    (archive-mode                       "file-zip"          icon-tools-lmaroon)
    (elm-mode                           "elm"               icon-tools-blue)
    (erlang-mode                        "erlang"            icon-tools-red)
    (elixir-mode                        "elixir"            icon-tools-lorange)
    (java-mode                          "java"              icon-tools-purple)
    (go-mode                            "go"                icon-tools-blue)
    (go-dot-mod-mode                    "config-go"         icon-tools-blue-alt)
    (go-dot-work-mode                   "config-go"         icon-tools-blue-alt)
    (graphql-mode                       "graphql"           icon-tools-dpink)
    (matlab-mode                        "matlab"            icon-tools-orange)
    (nix-mode                           "nix"               icon-tools-blue)
    (perl-mode                          "perl"              icon-tools-lorange)
    (cperl-mode                         "perl"              icon-tools-lorange)
    (php-mode                           "php"               icon-tools-lsilver)
    (prolog-mode                        "prolog"            icon-tools-lmaroon)
    (python-mode                        "python"            icon-tools-dblue)
    (inferior-python-mode               "python"            icon-tools-dblue)
    (racket-mode                        "racket"            icon-tools-red)
    (rust-mode                          "rust"              icon-tools-maroon)
    (scala-mode                         "scala"             icon-tools-red)
    (scheme-mode                        "scheme"            icon-tools-red)
    (swift-mode                         "swift"             icon-tools-green)
    (svelte-mode                        "svelte"            icon-tools-red)
    (c-mode                             "c-line"            icon-tools-blue)
    (c++-mode                           "cplusplus-line"    icon-tools-blue)
    (csharp-mode                        "csharp-line"       icon-tools-dblue)
    (clojure-mode                       "clojure-alt"       icon-tools-blue)
    (cider-repl-mode                    "clojure-alt"       icon-tools-green)
    (clojurescript-mode                 "clojurejs"         icon-tools-dblue)
    (coffee-mode                        "coffeescript"      icon-tools-maroon)
    (lisp-mode                          "lisp"              icon-tools-orange)
    (css-mode                           "css3"              icon-tools-yellow)
    (scss-mode                          "sass"              icon-tools-pink)
    (sass-mode                          "sass"              icon-tools-dpink)
    (less-css-mode                      "less"              icon-tools-dyellow)
    (stylus-mode                        "stylus"            icon-tools-lgreen)
    (csv-mode                           "graph"             icon-tools-dblue)
    (haskell-mode                       "haskell"           icon-tools-red)
    (haskell-c2hs-mode                  "haskell"           icon-tools-red)
    (literate-haskell-mode              "haskell"           icon-tools-red)
    (haml-mode                          "haml"              icon-tools-lyellow)
    (html-mode                          "html5"             icon-tools-orange)
    (rhtml-mode                         "html5"             icon-tools-lred)
    (mustache-mode                      "moustache"         icon-tools-green)
    (slim-mode                          "dashboard"         icon-tools-yellow)
    (jade-mode                          "jade"              icon-tools-red)
    (pug-mode                           "pug-old"           icon-tools-red)
    (image-mode                         "image"             icon-tools-blue)
    (texinfo-mode                       "tex"               icon-tools-lred)
    (markdown-mode                      "markdown"          icon-tools-lblue)
    (bibtex-mode                        "bibtex"            icon-tools-maroon)
    (org-mode                           "org"               icon-tools-lgreen)
    (compilation-mode                   "cogs")
    (objc-mode                          "apple")
    (tuareg-mode                        "ocaml")
    (purescript-mode                    "purescript")
    (verilog-mode                       "verilog"           icon-tools-red)
    (vhdl-mode                          "vhdl"              icon-tools-blue)
    (haskell-cabal-mode                 "cabal"             icon-tools-lblue)
    (kotlin-mode                        "kotlin"            icon-tools-orange)
    (nim-mode                           "nimrod"            icon-tools-yellow)
    (sql-mode                           "database"          icon-tools-silver)
    (lua-mode                           "lua"               icon-tools-dblue)
    (adoc-mode                          "asciidoc"          icon-tools-lblue)
    (puppet-mode                        "puppet"            icon-tools-yellow)
    (jinja2-mode                        "jinja"             icon-tools-silver)
    (powershell-mode                    "powershell"        icon-tools-blue)
    (tex-mode                           "tex"               icon-tools-lred)
    (latex-mode                         "tex"               icon-tools-lred)
    (dart-mode                          "dart"              icon-tools-blue)
    (fsharp-mode                        "fsharp"            icon-tools-blue)
    (asm-mode                           "assembly-generic"  icon-tools-blue)
    (nasm-mode                          "assembly-generic"  icon-tools-blue)
    (tcl-mode                           "tcl"               icon-tools-dred)
    (cuda-mode                          "nvidia"            icon-tools-green)
    (f90-mode                           "fortran"           icon-tools-purple)
    (hy-mode                            "hy"                icon-tools-blue)
    (glsl-mode                          "vertexshader"      icon-tools-green)
    (zig-mode                           "zig"               icon-tools-orange)
    (odin-mode                          "odin"              icon-tools-lblue)
    (pdf-view-mode                      "file-pdf"          icon-tools-dred)
    (elfeed-search-mode                 "square-rss"        icon-tools-orange)
    (elfeed-show-mode                   "rss"               icon-tools-orange)
    (lilypond-mode                      "music"             icon-tools-green)
    (magik-session-mode                 "terminal"          icon-tools-blue)
    (magik-cb-mode                      "book"              icon-tools-blue)
    ))

(defvar icon-tools-default-mode-icon
  '("file-directory" icon-tools-dsilver))

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
(defun icon-tools-icon-for-dir (dir &optional face)
  "Get the formatted icon for DIR with FACE, if provided.

Note: You want chevron, please use `icon-tools-icon-for-dir-with-chevron'."
  (let ((path (expand-file-name dir))
        (face1 (or face 'icon-tools-blue)))
    (cond
     ((file-remote-p path)
      (icon-tools-icon-str "terminal" face1))
     ((file-symlink-p path)
      (icon-tools-icon-str "file-symlink-directory" face1))
     ((icon-tools-dir-is-submodule path)
      (icon-tools-icon-str "file-submodule" face1))
     ((file-exists-p (format "%s/.git" path))
      (icon-tools-icon-str "repo" face1))
     (t
      (let* ((dir-name (file-name-base (directory-file-name dir)))
             (match (or (cdr (icon-tools--match-to-alist
                              dir-name
                              icon-tools-dir-regexp-icon-alist))
                        icon-tools-default-dir-icon))
             (icon-name (car match))
             (face1 (or face (cadr match))))
        (icon-tools-icon-str icon-name face1))))))

;;;###autoload
(defun icon-tools-icon-for-str (str &optional face)
  "Get the formatted icon for STR with FACE, if provided."
  (when-let ((match (icon-tools--match-to-alist
                     str icon-tools-regexp-icon-alist)))
    (icon-tools-icon-str
     (cadr match) (or face (caddr match)))))

;;;###autoload
(defun icon-tools-icon-for-file (file &optional face)
  "Get the formatted icon for FILE with FACE, if provided."
  (let* ((ext (file-name-extension file))
         (match (or (cdr (icon-tools--match-to-alist
                          file icon-tools-regexp-icon-alist))
                    (and ext (cdr
                              (assoc (downcase ext)
                                     icon-tools-extension-icon-alist)))
                    icon-tools-default-file-icon))
         (icon-name (car match))
         (face1 (or face (cadr match))))
    (icon-tools-icon-str icon-name face1)))

;;;###autoload
(defun icon-tools-icon-for-mode (mode &optional face)
  "Get the formatted icon for MODE with FACE, if provided.
ARG-OVERRIDES should be a plist containining `:height',
`:v-adjust' or `:face' properties like in the normal icon
inserting functions."
  (let* ((mode0 mode)
         (match (assoc mode0 icon-tools-mode-icon-alist)))
    (while (and mode0 (not match))
      (setq mode0 (get mode0 'derived-mode-parent))
      (setq match (assoc mode0 icon-tools-mode-icon-alist)))
    (if match
        (icon-tools-icon-str
         (cadr match) (or face (caddr match)))
      (make-string icon-tools-icon-width ?\s))))

(provide 'icon-tools)

;;; icon-tools.el ends here
