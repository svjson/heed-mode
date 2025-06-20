;;; heed-test-fixtures.el --- summary -*- lexical-binding: t -*-

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

;; Contains macros and utilities that support the heed-mode test suite.

;;; Code:

(require 'heed-mode)
(require 'ert)

(defmacro with-temp-buffer-content (content &rest body)
  "Create temp buffer, insert CONTENT as literal string, then run BODY."
  `(with-temp-buffer
     (insert ,(string-join content "\n"))
     (goto-char (point-min))
     ,@body))

(defmacro with-temp-buffer-of-size (size &rest body)
  "Create a temp buffer with SIZE characters of dummy content, then run BODY.

The buffer will contain repeating 'x' characters and have point at position 1."
  `(with-temp-buffer
     (insert-char ?x ,size)
     (goto-char 1)
     ,@body))

(defun heed-test--to-num-block-bounds (block)
  "Construct a copy of BLOCK with integer bounds instead of marker bounds."
  (let* ((bounds (plist-get block :bounds))
         (content-bounds (plist-get block :content-bounds))
         (type (plist-get block :type))
         (overlay (plist-get block :overlay))
         (result nil))
    (message "to-num-block-bounds input: %s" block)
    (message "%s" overlay)
    (when type
      (setq result (plist-put result :type type)))
    (when (markerp (car bounds))
      (setq result (plist-put result :bounds (cons (marker-position (car bounds))
                                                   (marker-position (cdr bounds))))))
    (when (markerp (car content-bounds))
      (setq result (plist-put result :content-bounds (cons (marker-position (car content-bounds))
                                                           (marker-position (cdr content-bounds))))))
    (when overlay
      (setq result (plist-put result :overlay (cons (overlay-start overlay)
                                                    (overlay-end overlay)))))
    (message "to-num-block-bounds output: %s" result)
    result))

(provide 'heed-test-fixtures)

;;; heed-test-fixtures.el ends here
