;;; block-at-point-test.el --- summary -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Tests the resolution of block at point

;;; Code:

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))

(require 'ert)
(require 'heed-mode)
(require 'heed-test-fixtures)


(ert-deftest heed--block-at-point--single-block ()
  (with-temp-buffer-content
   (":: html"
    "@id=frumious-bandersnatch"
    "<h1>Bandersnatches be crazy</h1>"
    "--"
    "")
   (heed--scan-and-mark-blocks)
   (let ((locations (mapcar (lambda (pt)
                              (goto-char pt)
                              (cons pt (heed--block-at-point)))
                            '(1 4 8 9 21 68))))
     (mapc
      (lambda (loc)
        (should (equal (heed-test--to-num-block-bounds (cdr loc))
                       (list :type :block
                             :bounds '(1 . 70)
                             :content-bounds '(35 . 68)
                             :overlay '(35 . 68)))))
      locations))))

(ert-deftest heed--block-at-point--block-with-nested-children ()
  (with-temp-buffer-content
   (":: column-layout"
    "  :: column"
    "  @id=left-column"
    "    :: html"
    "    <h1>Left Column</h1>"
    "    --"
    "  --"
    "  :: column"
    "  @id=right-column"
    "    :: html"
    "    <h1>Right Column</h1>"
    "    --"
    "  --"
    "--"
    "")
   (heed--scan-and-mark-blocks)
   (let ((column-layout (mapcar #'heed-test--get-block-at-point '(1 4 8 17 178)))
         (left-column (mapcar #'heed-test--get-block-at-point '(20 25 29 94)))
         (left-html (mapcar #'heed-test--get-block-at-point '(52 59 79 90)))
         (right-column (mapcar #'heed-test--get-block-at-point '(99 105 108 176)))
         (right-html (mapcar #'heed-test--get-block-at-point '(132 139 151 171))))

     (heed-test--all-cdr-should-eq
      column-layout
      (list :type :block
            :bounds '(1 . 180)
            :content-bounds '(178 . 178)))

     (heed-test--all-cdr-should-eq
      left-column
      (list :type :block
            :bounds '(18 . 96)
            :content-bounds '(92 . 92)))

     (heed-test--all-cdr-should-eq
      left-html
      (list :type :block
            :bounds '(48 . 91)
            :content-bounds '(60 . 85)))

     (heed-test--all-cdr-should-eq
      right-column
      (list :type :block
            :bounds '(97 . 177)
            :content-bounds '(173 . 173)))

     (heed-test--all-cdr-should-eq
      right-html
      (list :type :block
            :bounds '(128 . 172)
            :content-bounds '(140 . 166))))))

;;; block-at-point-test.el ends here
