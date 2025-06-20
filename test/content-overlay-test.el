;;; content-overlay-test.el --- summary -*- lexical-binding: t -*-

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

;; Test the management of content overlays and the synchronization with
;; content bounds

;;; Code:

(require 'ert)
(require 'heed-mode)
(require 'heed-test-fixtures)

(ert-deftest heed--basic-html-block--overlay-bounds=content-bounds ()
  (with-temp-buffer-content
   (":: html"
    "@id=frumious-bandersnatch"
    "<h1>Bandersnatches be crazy</h1>"
    "--"
    "")
   (heed--scan-and-mark-blocks)
   (should (equal (length heed--block-boundaries) 1))
   (should (equal (length (overlays-in (point-min) (point-max))) 1))
   (let ((block (car heed--block-boundaries))
         (overlay (car (overlays-in (point-min) (point-max)))))
     (should
      (equal (cons (overlay-start overlay) (overlay-end overlay))
             (heed--block-bounds block :content-bounds))))))

(ert-deftest heed--adjust-overlay!--content-bounds=overlay-bounds ()
  (with-temp-buffer-of-size
   200
   (let ((block (list :type :block
                      :bounds (cons (copy-marker 25) (copy-marker 50))
                      :content-bounds (cons (copy-marker 35) (copy-marker 48))
                      :overlay (let ((ov (make-overlay 35 48)))
                                 ov))))
     (heed--adjust-block-overlay! block)
     (let ((num-block (heed-test--to-num-block-bounds block)))
       (should
        (equal (plist-get num-block :content-bounds)
               (plist-get num-block :overlay)))))))

(ert-deftest heed--adjust-overlay!--content-bounds!=overlay-bounds ()
  (with-temp-buffer-of-size
   200
   (let ((block (list :type :block
                      :bounds (cons (copy-marker 25) (copy-marker 50))
                      :content-bounds (cons (copy-marker 35) (copy-marker 48))
                      :overlay (let ((ov (make-overlay 30 49)))
                                 ov))))
     (heed--adjust-block-overlay! block)
     (let ((num-block (heed-test--to-num-block-bounds block)))
       (should
        (equal (plist-get num-block :content-bounds)
               (plist-get num-block :overlay)))
       (should
        (equal (cons 35 48)
               (plist-get num-block :overlay)))))))

(ert-deftest heed--adjust-overlay!--content-bounds-not-a-range ()
  (with-temp-buffer-of-size
   200
   (let ((block (list :type :block
                      :bounds (cons (copy-marker 25) (copy-marker 50))
                      :content-bounds (cons (copy-marker 35) (copy-marker 35))
                      :overlay (let ((ov (make-overlay 30 49)))
                                 ov))))
     (heed--adjust-block-overlay! block)
     (let ((num-block (heed-test--to-num-block-bounds block)))
       (should
        (equal (cons 35 35)
               (plist-get num-block :content-bounds)))
       (should
        (equal (plist-get num-block :overlay)
               nil))))))

(ert-deftest heed--adjust-overlay!--content-bounds-is-nil ()
  (with-temp-buffer-of-size
   200
   (let ((block (list :type :block
                      :bounds (cons (copy-marker 25) (copy-marker 50))
                      :overlay (let ((ov (make-overlay 30 49)))
                                 ov))))
     (heed--adjust-block-overlay! block)
     (let ((num-block (heed-test--to-num-block-bounds block)))
       (should
        (equal (plist-get num-block :content-bounds)
               nil))
       (should
        (equal (plist-get num-block :overlay)
               nil))))))

(ert-deftest heed--basic-html-block--kill-char-backwards-at-content-start--overlay-bounds=content-bounds ()
  (with-temp-buffer-content
   (":: html"
    "@id=frumious-bandersnatch"
    "<h1>Bandersnatches be crazy</h1>"
    "--"
    "")
   (heed-mode)
   (let ((block (car heed--block-boundaries)))
     (goto-char (heed--block-bounds-begin block :content-bounds))
     (kill-backward-chars 1)
     (let ((overlay (car (overlays-in (point-min) (point-max)))))
       (should (equal overlay nil))
       (should (equal (length heed--block-boundaries) 1))
       (should (equal (length (overlays-in (point-min) (point-max))) 0))
       (should (equal (heed-test--to-num-block-bounds block)
                      (list :type :block
                            :bounds '(1 . 69)
                            :content-bounds '(67 . 67))))))))


;;; content-overlay-test.el ends here
