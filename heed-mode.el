;;; heed-mode.el --- summary -*- lexical-binding: t -*-

;; Author: Sven Johansson
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Homepage: https://www.github.com/svjson/heed-mode.el
;; Keywords: tools, convenience, heed, presentation, slide

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

;; Major mode for editing .heed presentation files

;; commentary

;;; Code:

(require 'color)


;; Custom Faces

(defface heed-frontmatter-delimiter-face
  '((t :inherit font-lock-comment-face :weight bold))
  "Face for the dashed lines delimiting frontmatter.")

(defface heed-frontmatter-key-face
  '((t :inherit font-lock-constant-face))
  "Face for frontmatter keys.")

(defface heed-frontmatter-value-face
  '((t :inherit font-lock-string-face))
  "Face for frontmatter values.")

(defface heed-block-open-face
  '((t :weight bold :inherit font-lock-preprocessor-face))
  "Face for the :: delimiter at the start of Heed block contents.")

(defface heed-block-close-face
  '((t :inherit heed-block-open-face))
  "Face for the -- delimiter at the end of Heed block contents.")

(defface heed-block-content-overlay-face
  '((t :background unspecified :extend t))
  "Face for the content block content overlay.")



;; Content tracking variables

(defvar-local heed--frontmatter-range nil
  "Cons cell of (start . end) buffer positions of frontmatter delimiter lines.")

(defvar-local heed--block-boundaries nil
  "List of (start-marker . end-marker) pairs for Heed content blocks.")

(defvar-local heed--block-content-overlays nil
  "List of overlays highlighting Heed block content regions.")



;; Content tracking

(defun heed--scan-and-mark-blocks ()
  "Scan buffer for :: blocks and record start/end markers and content overlays."
  (heed--clear-block-overlays)
  (setq heed--block-boundaries nil)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^::\\s-*\\([a-zA-Z0-9_-]+\\)" nil t)
      (let ((start (match-beginning 0)))
        (forward-line 1)
        ;; Skip any @attr lines
        (while (and (not (eobp))
                    (looking-at "^@"))
          (forward-line 1))
        (let ((content-start (point)))
          ;; Find closing --
          (let ((end (or (save-excursion
                           (while (and (not (eobp))
                                       (not (looking-at "^\\s-*--\\s-*$")))
                             (forward-line 1))
                           (point))
                         (point-max))))
            ;; Record the block marker range
            (let ((start-marker (copy-marker start))
                  (end-marker (copy-marker end t)))
              (push (cons start-marker end-marker) heed--block-boundaries))
            ;; Add the overlay if there's visible content
            (when (< content-start end)
              (let ((start content-start)
                    (finish (save-excursion
                              (goto-char end)
                              (if (and (not (eobp)) (= (char-after) ?\n))
                                  (1+ end) ; include newline
                                end))))
                (let ((ov (make-overlay start finish)))
                  (overlay-put ov 'face 'heed-block-content-overlay-face)
                  (overlay-put ov 'evaporate t)
                  (push ov heed-block-content-overlays)))))))))
  (font-lock-ensure))

(defun heed--detect-frontmatter ()
  "Detect and record the frontmatter region at the top of the buffer, if present."
  (setq heed--frontmatter-range nil)
  (save-excursion
    (goto-char (point-min))
    (when (looking-at "^[-]+\\s-*$")
      (let ((start (match-beginning 0)))
        (forward-line 1)
        (while (and (not (eobp))
                    (not (looking-at "^[-]+\\s-*$")))
          (forward-line 1))
        (when (looking-at "^[-]+\\s-*$")
          (setq heed--frontmatter-range
                (cons start (match-end 0))))))))



;; Font locking and styling overlays

(defun heed--clear-block-overlays ()
  "Clear all existing block content overlays."
  (mapc #'delete-overlay heed--block-content-overlays)
  (setq heed--block-content-overlays nil))

(defvar heed-font-lock-keywords
  (list '("^\\(::\\)\\s-*\\([a-zA-Z0-9_-]+\\)\\(?:\\s-*{[^}]*}\\)?"
          (1 'heed-block-open-face)
          (2 font-lock-keyword-face))
        (list #'heed--match-frontmatter-delimiter
              '(0 'heed-frontmatter-delimiter-face))
        (list #'heed--match-frontmatter-prop-line
              '(1 'heed-frontmatter-key-face)
              '(2 'heed-frontmatter-value-face))
        (list #'heed--match-block-close
              '(0 'heed-block-close-face))
        '("^\\(@\\)\\([a-zA-Z0-9_-]+\\)\\(\\[[^]=]+=[^]]+\\]\\)?\\(=\\)\\(.*\\)?"
          (1 font-lock-keyword-face)              ; @
          (2 font-lock-constant-face)             ; prop
          (3 font-lock-preprocessor-face nil t)   ; [n=2] (optional)
          (4 font-lock-comment-face)              ; =
          (5 font-lock-variable-name-face nil t)) ; value (optional)
        '("\\(^\\|[^\\]\\)\"[^\"]*\"" . font-lock-string-face)
        '("\\_<\\(true\\|false\\|::\\)\\_>" . font-lock-constant-face))
  "Basic font-lock keywords for `heed-mode`.")

(defun heed--match-frontmatter-prop-line (limit)
  "Match key: value lines inside frontmatter only, within LIMIT."
  (when heed--frontmatter-range
    (let ((start (car heed--frontmatter-range))
          (end (cdr heed--frontmatter-range))
          matched)
      (while (and (not matched)
                  (re-search-forward "^\\([a-zA-Z0-9_-]+\\):\\s-*\\(.*\\)$" limit t))
        (let ((pos (match-beginning 0)))
          (when (and (>= pos start) (< pos end))
            (setq matched t))))
      matched)))

(defun heed--match-block-close (limit)
  "Match block-closing `--` lines that are real block terminators.

LIMIT ensures matching is not overly greedy."
  (let (matched)
    (while (and (not matched)
                (re-search-forward "^\\s-*--\\s-*$" limit t))
      ;; Is this position between any known block markers?
      (let ((pos (match-beginning 0)))
        (setq matched
              (seq-some (lambda (range)
                          (and (<= (car range) pos)
                               (<= pos (cdr range))))
                        heed--block-boundaries))))
    matched))

(defun heed--match-frontmatter-delimiter (limit)
  "Match frontmatter delimiter lines only if part of valid header, within LIMIT."
  (when heed--frontmatter-range
    (let ((start (car heed--frontmatter-range))
          (end (cdr heed--frontmatter-range)))
      (when (and (>= (point) start)
                 (< (point) end))
        (when (re-search-forward "^[-]+\\s-*$" limit t)
          (let ((pos (match-beginning 0)))
            (when (and (>= pos start) (< pos end))
              t)))))))

(defun heed--adjust-color-brightness (color percent)
  "Lighten or darken COLOR (hex string like \"#112233\") by PERCENT.

A positive value will lighten and a negative value will darken."
  (let* ((rgb (color-name-to-rgb color))
         (scale (+ 1.0 (/ percent 100.0))))
    (apply #'color-rgb-to-hex
           (mapcar (lambda (c) (min 1.0 (max 0.0 (* c scale)))) rgb))))

(defun heed--init-content-background-face ()
  "Dynamically set the background for `heed-block-content-overlay-face`.

Does not affect the face if has been customized."
  (unless (face-user-default-spec 'heed-block-content-overlay-face)
    (let* ((bg (face-attribute 'default :background nil t))
           (adjusted
            (if (color-dark-p (color-name-to-rgb bg))
                (heed--adjust-color-brightness bg 20)
              (heed--adjust-color-brightness bg -20))))
      (set-face-attribute 'heed-block-content-overlay-face nil
                          :background adjusted))))


;; Heed mode

(defun heed--after-change (_beg _end _len)
  "Rescan blocks on buffer changes."
  ;; Could and perhaps should be made this incremental, but this works for now
  (heed--detect-frontmatter)
  (heed--scan-and-mark-blocks))

;;;###autoload
(define-derived-mode heed-mode prog-mode "Heed"
  "Major mode for editing Heed presentation files."
  (heed--init-content-background-face)
  (setq font-lock-defaults '(heed-font-lock-keywords))
  (heed--detect-frontmatter)
  (heed--scan-and-mark-blocks)
  (add-hook 'after-change-functions #'heed--after-change nil t))

;;; heed-mode.el ends here
