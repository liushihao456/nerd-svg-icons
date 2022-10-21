;;; icon-tools-faces.el --- A module of faces for icon-tools

;; Author: Shihao Liu
;; Keywords: icon
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.3"))

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains all of the faces used by the package for
;; colouring icons

;;; Code:

(defgroup icon-tools-faces nil
  "Manage how All The Icons icons are coloured and themed."
  :prefix "icon-tools-"
  :group 'icon-tools)

;; red
(defface icon-tools-red
  '((((background dark)) :foreground "#AC4142")
    (((background light)) :foreground "#AC4142"))
  "Face for red icons"
  :group 'icon-tools-faces)
(defface icon-tools-lred
  '((((background dark)) :foreground "#EB595A")
    (((background light)) :foreground "#EB595A"))
  "Face for lred icons"
  :group 'icon-tools-faces)
(defface icon-tools-dred
  '((((background dark)) :foreground "#843031")
    (((background light)) :foreground "#843031"))
  "Face for dred icons"
  :group 'icon-tools-faces)
(defface icon-tools-red-alt
  '((((background dark)) :foreground "#ce5643")
    (((background light)) :foreground "#843031"))
  "Face for dred icons"
  :group 'icon-tools-faces)

;; green
(defface icon-tools-green
  '((((background dark)) :foreground "#90A959")
    (((background light)) :foreground "#90A959"))
  "Face for green icons"
  :group 'icon-tools-faces)
(defface icon-tools-lgreen
  '((((background dark)) :foreground "#C6E87A")
    (((background light)) :foreground "#3D6837"))
  "Face for lgreen icons"
  :group 'icon-tools-faces)
(defface icon-tools-dgreen
  '((((background dark)) :foreground "#6D8143")
    (((background light)) :foreground "#6D8143"))
  "Face for dgreen icons"
  :group 'icon-tools-faces)

;; yellow
(defface icon-tools-yellow
  '((((background dark)) :foreground "#FFD446")
    (((background light)) :foreground "#FFCC0E"))
  "Face for yellow icons"
  :group 'icon-tools-faces)
(defface icon-tools-lyellow
  '((((background dark)) :foreground "#FFC16D")
    (((background light)) :foreground "#FF9300"))
  "Face for lyellow icons"
  :group 'icon-tools-faces)
(defface icon-tools-dyellow
  '((((background dark)) :foreground "#B48D56")
    (((background light)) :foreground "#B48D56"))
  "Face for dyellow icons"
  :group 'icon-tools-faces)

;; blue
(defface icon-tools-blue
  '((((background dark)) :foreground "#6A9FB5")
    (((background light)) :foreground "#6A9FB5"))
  "Face for blue icons"
  :group 'icon-tools-faces)
(defface icon-tools-blue-alt
  '((((background dark)) :foreground "#2188b6")
    (((background light)) :foreground "#2188b6"))
  "Face for blue icons"
  :group 'icon-tools-faces)
(defface icon-tools-lblue
  '((((background dark)) :foreground "#8FD7F4")
    (((background light)) :foreground "#677174"))
  "Face for lblue icons"
  :group 'icon-tools-faces)
(defface icon-tools-dblue
  '((((background dark)) :foreground "#446674")
    (((background light)) :foreground "#446674"))
  "Face for dblue icons"
  :group 'icon-tools-faces)

;; maroon
(defface icon-tools-maroon
  '((((background dark)) :foreground "#8F5536")
    (((background light)) :foreground "#8F5536"))
  "Face for maroon icons"
  :group 'icon-tools-faces)
(defface icon-tools-lmaroon
  '((((background dark)) :foreground "#CE7A4E")
    (((background light)) :foreground "#CE7A4E"))
  "Face for lmaroon icons"
  :group 'icon-tools-faces)
(defface icon-tools-dmaroon
  '((((background dark)) :foreground "#72584B")
    (((background light)) :foreground "#72584B"))
  "Face for dmaroon icons"
  :group 'icon-tools-faces)

;; purple
(defface icon-tools-purple
  '((((background dark)) :foreground "#AA759F")
    (((background light)) :foreground "#68295B"))
  "Face for purple icons"
  :group 'icon-tools-faces)
(defface icon-tools-purple-alt
  '((((background dark)) :foreground "#5D54E1")
    (((background light)) :foreground "#5D54E1"))
  "Face for purple icons"
  :group 'icon-tools-faces)
(defface icon-tools-lpurple
  '((((background dark)) :foreground "#E69DD6")
    (((background light)) :foreground "#E69DD6"))
  "Face for lpurple icons"
  :group 'icon-tools-faces)
(defface icon-tools-dpurple
  '((((background dark)) :foreground "#694863")
    (((background light)) :foreground "#694863"))
  "Face for dpurple icons"
  :group 'icon-tools-faces)

;; orange
(defface icon-tools-orange
  '((((background dark)) :foreground "#D4843E")
    (((background light)) :foreground "#D4843E"))
  "Face for orange icons"
  :group 'icon-tools-faces)
(defface icon-tools-lorange
  '((((background dark)) :foreground "#FFA500")
    (((background light)) :foreground "#FFA500"))
  "Face for lorange icons"
  :group 'icon-tools-faces)
(defface icon-tools-dorange
  '((((background dark)) :foreground "#915B2D")
    (((background light)) :foreground "#915B2D"))
  "Face for dorange icons"
  :group 'icon-tools-faces)

;; cyan
(defface icon-tools-cyan
  '((((background dark)) :foreground "#75B5AA")
    (((background light)) :foreground "#75B5AA"))
  "Face for cyan icons"
  :group 'icon-tools-faces)
(defface icon-tools-cyan-alt
  '((((background dark)) :foreground "#61dafb")
    (((background light)) :foreground "#0595bd"))
  "Face for cyan icons"
  :group 'icon-tools-faces)
(defface icon-tools-lcyan
  '((((background dark)) :foreground "#A5FDEC")
    (((background light)) :foreground "#2C7D6E"))
  "Face for lcyan icons"
  :group 'icon-tools-faces)
(defface icon-tools-dcyan
  '((((background dark)) :foreground "#48746D")
    (((background light)) :foreground "#48746D"))
  "Face for dcyan icons"
  :group 'icon-tools-faces)

;; pink
(defface icon-tools-pink
  '((((background dark)) :foreground "#F2B4B8")
    (((background light)) :foreground "#FC505B"))
  "Face for pink icons"
  :group 'icon-tools-faces)
(defface icon-tools-lpink
  '((((background dark)) :foreground "#FFBDC1")
    (((background light)) :foreground "#FF505B"))
  "Face for lpink icons"
  :group 'icon-tools-faces)
(defface icon-tools-dpink
  '((((background dark)) :foreground "#B18286")
    (((background light)) :foreground "#7E5D5F"))
  "Face for dpink icons"
  :group 'icon-tools-faces)

;; silver
(defface icon-tools-silver
  '((((background dark)) :foreground "#716E68")
    (((background light)) :foreground "#716E68"))
  "Face for silver icons"
  :group 'icon-tools-faces)
(defface icon-tools-lsilver
  '((((background dark)) :foreground "#B9B6AA")
    (((background light)) :foreground "#7F7869"))
  "Face for lsilver icons"
  :group 'icon-tools-faces)
(defface icon-tools-dsilver
  '((((background dark)) :foreground "#838484")
    (((background light)) :foreground "#838484"))
  "Face for dsilver icons"
  :group 'icon-tools-faces)


(provide 'icon-tools-faces)
;;; icon-tools-faces.el ends here
