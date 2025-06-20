;;; block-parse-test.el --- summary -*- lexical-binding: t -*-

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

;; Tests veriyfing the scanning and parsing of heed blocks

;;; Code:

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))

(require 'ert)
(require 'heed-mode)
(require 'heed-test-fixtures)

(ert-deftest heed--parse-basic-html-block ()
  "Should correctly parse a basic :: block with content and closing --."
  (with-temp-buffer-content
   (":: html"
    "@id=frumious-bandersnatch"
    "<h1>Bandersnatches be crazy</h1>"
    "--"
    "")
   (let ((block (heed--parse-block-bounds (point-min))))
     (should (equal
              (heed-test--to-num-block-bounds block)
              (list :type :block
                    :bounds '(1 . 70)
                    :content-bounds '(35 . 68)))))))

(ert-deftest heed--parse-block-with-empty-content-lines ()
  "Should correctly parse a block with several empty content lines."
  (with-temp-buffer-content
   (":: html"
    "@id=frumious-bandersnatch"
    ""
    ""
    ""
    "--"
    "")
   (goto-char (point-min))
   (let ((block (heed--parse-block-bounds (point-min))))
     (should (equal
              (heed-test--to-num-block-bounds block)
              (list :type :block
                    :bounds '(1 . 40)
                    :content-bounds '(35 . 38)))))))

(ert-deftest heed--parse-nested-child-block ()
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
   (goto-char 128) ;; Line start at html-block in #right-column
   (let ((block (heed--parse-block-bounds (point))))
     (should (equal
              (heed-test--to-num-block-bounds block)
              (list :type :block
                    :bounds '(128 . 172)
                    :content-bounds '(140 . 166)))))))

(ert-deftest heed--parse-nested-child-block-with-one-child ()
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
   (goto-char 97) ;; Line start at html-block in #right-column
   (let ((block (heed--parse-block-bounds (point))))
     (should (equal
              (heed-test--to-num-block-bounds block)
              (list :type :block
                    :bounds '(97 . 177)
                    :content-bounds '(173 . 173)
                    :children
                    (list
                     (list :type :block
                           :bounds '(128 . 172)
                           :content-bounds '(140 . 166)))))))))

(ert-deftest heed--parse-block-with-child-blocks ()
  "Should correctly parse a block with several empty content lines."
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
   (goto-char (point-min))
   (let ((block (heed--parse-block-bounds (point-min))))
     (should (equal
              (heed-test--to-num-block-bounds block)
              (list :type :block
                    :bounds '(1 . 180)
                    :content-bounds '(178 . 178)
                    :children (list
                               (list :type :block
                                     :bounds '(18 . 96)
                                     :content-bounds '(92 . 92)
                                     :children (list
                                                (list :type :block
                                                      :bounds '(48 . 91)
                                                      :content-bounds '(60 . 85))))
                               (list :type :block
                                     :bounds '(97 . 177)
                                     :content-bounds '(173 . 173)
                                     :children (list
                                                (list :type :block
                                                      :bounds '(128 . 172)
                                                      :content-bounds '(140 . 166)))))))))))

;;; block-parse-test.el ends here
