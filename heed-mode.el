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

(require 'cl-lib)
(require 'dash)
(require 'color)
(require 'json)


;; Custom variables

(defcustom heed-cli-command "heed-cli"
  "Command to run the Heed CLI tool."
  :type 'string
  :group 'heed)



;; Global variables

(defvar heed--presentations-hash (make-hash-table :test #'equal)
  "Global stash where information about known presentations are stored.

The absolute path to the root folder of a presentation is used as key.")



;; Buffer-local content tracking variables

(defvar-local heed--frontmatter-range nil
  "Cons cell of (start . end) buffer positions of frontmatter delimiter lines.")

(defvar-local heed--block-boundaries nil
  "List of block boundaries and metadata for Heed content blocks and aside blocks.
Stored as (:type <type>
           :bounds (start-marker . end-marker)
           :content-bounds (start-marker . end-marker) ;; Optional
           :overlay <overlay>) ;; Optional")



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

(defface heed-macro-sigil-face
  '((t :weight bold :inherit font-lock-type-face))
  "Face for the % macro sigil.")

(defface heed-block-open-face
  '((t :weight bold :inherit font-lock-preprocessor-face))
  "Face for the :: delimiter at the start of Heed block contents.")

(defface heed-block-type-face
  '((t :weight bold :inherit font-lock-keyword-face))
  "Face for the block type id, immediately following :: in definitions.")

(defface heed-block-close-face
  '((t :inherit heed-block-open-face))
  "Face for the -- delimiter at the end of Heed block contents.")

(defface heed-block-content-overlay-face
  '((t :background unspecified :extend t))
  "Face for the content block content overlay.")

(defface heed-aside-open-face
  '((t :weight bold :inherit font-lock-keyword-face))
  "Face for the == delimiter at the start of Heed aside block contents.")

(defface heed-aside-id-face
  '((t :weight bold :inherit font-lock-preprocessor-face))
  "Face for the aside block id, immediately following == in definitions.")

(defface heed-aside-close-face
  '((t :weight bold :inherit font-lock-keyword-face))
  "Face for the -- delimiter at the end of Heed aside block contents.")

(defface heed-phase-open-face
  '((t :inherit font-lock-preprocessor-face))
  "Face for the !! delimiter at the start of Heed phase block contents.")

(defface heed-phase-id-face
  '((t :weight bold :inherit font-lock-constant-face))
  "Face for the phase block id, immediately following !! in definitions.")



;; font-lock-keywords

(defvar heed-font-lock-keywords
  (list '("^[ \t]*\\(::\\)\\s-*\\(%\\)\\([a-zA-Z:0-9_-]+\\)\\(?:\\s-*{[^}]*?}\\)?"
          (1 'heed-block-open-face)
          (2 'heed-macro-sigil-face)
          (3 'heed-block-type-face))
        '("^[ \t]*\\(::\\)\\s-*\\([a-zA-Z:0-9_-]+\\)\\(?:\\s-*{[^}]*}\\)?"
          (1 'heed-block-open-face)
          (2 'heed-block-type-face))
        '("^\\(==\\)\\s-*\\([a-zA-Z:0-9_-]+\\)\\(?:\\s-*{[^}]*}\\)?"
          (1 'heed-aside-open-face)
          (2 'heed-aside-id-face))
        (list #'heed--match-frontmatter-delimiter
              '(0 'heed-frontmatter-delimiter-face))
        (list #'heed--match-frontmatter-prop-line
              '(1 'heed-frontmatter-key-face)
              '(2 'heed-frontmatter-value-face))
        `((lambda (limit)
            (heed--match-block-close limit :block))
          (0 'heed-block-close-face))
        `((lambda (limit)
            (heed--match-block-close limit :aside))
          (0 'heed-aside-close-face))
        '("^\\(!!\\)\\s-*\\([a-zA-Z:0-9_-]+\\)\\(?:\\s-*{[^}]*}\\)?"
          (1 'heed-phase-open-face)
          (2 'heed-phase-id-face))
        (list #'heed--match-phase-transition-line
              '(1 font-lock-keyword-face nil t)
              '(2 font-lock-function-name-face)
              '(3 font-lock-builtin-face nil t)
              '(4 font-lock-string-face nil t)
              '(5 font-lock-constant-face nil t))
        '("^[ \t]*\\(@\\)\\([a-zA-Z0-9_-]+\\)\\(\\[[^]=]+=[^]]+\\]\\)?\\(=\\)\\(.*?\\)$"
          (1 font-lock-keyword-face)
          (2 font-lock-constant-face)
          (3 font-lock-preprocessor-face nil t)
          (4 font-lock-comment-face nil t)
          (5 font-lock-variable-name-face nil t))

        (apply #'list
               (concat "^[ \t]*\\(%\\)\\([a-zA-Z0-9_.-]+\\)"
                       "\\(?:\\([[{][^]= \t}]+[]}]\\)\\)?"
                       "\\(\\.[a-zA-Z0-9_-]+\\)?"
                       "\\(?::\\([a-zA-Z0-9_-]+\\)\\)?"
                       "[ \t]*\\(=\\)[ \t]*"
                       "\\([^|]*?\\)"
                       "\\(?:|[ \t]*\\(.*?\\)\\)?$")
               '((1 'heed-macro-sigil-face)
                 (2 font-lock-preprocessor-face)
                 (3 font-lock-type-face nil t)
                 (4 font-lock-preprocessor-face nil t)
                 (5 font-lock-constant-face nil t)
                 (6 font-lock-comment-face nil t)
                 (7 font-lock-variable-name-face nil t)
                 (8 font-lock-variable-name-face nil t)))

        '("\\_<\\(true\\|false\\|::\\)\\_>" . font-lock-constant-face))
  "Basic font-lock keywords for `heed-mode`.")



;; Content tracking

(defun heed--scan-and-mark-blocks ()
  "Scan buffer for :: and == blocks and annotate them."
  (heed--purge-block-boundaries)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\(::\\|==\\)\\s-*\\(%?\\)\\([a-zA-Z0-9_-]+\\)" nil t)
      (when-let* ((type (pcase (match-string 1)
                          ("::" :block)
                          ("==" :aside)))
                  (start (match-beginning 0))
                  (block-bounds (heed--parse-block-bounds start type)))
        (let ((block (append (list :type type) block-bounds)))
          (when (and (eq type :block)
                     (heed--block-bounds-range-p block :content-bounds))
            (setq block (plist-put block :overlay (heed--apply-block-overlay block))))
          (push block heed--block-boundaries)
          (goto-char (-> block (plist-get :bounds) (cdr)))))))
  (font-lock-flush)
  (font-lock-ensure))

(defun heed--parse-block-type (begin)
  "Parse the block type from a block open header at BEGIN."
  (save-excursion
    (goto-char begin)
    (when (looking-at "^\\s-*\\(::\\|==\\)\\s-*\\(%?\\)\\([a-zA-Z0-9_-]+\\)")
      (pcase (match-string 1)
        ("::" :block)
        ("==" :aside)))))

(defun heed--parse-block-bounds (begin &optional type)
  "Determine the bounds and content bounds for a block starting at BEGIN.

Block TYPE may be provided, if already determined.  It will otherwise be parsed
from the block opening line."
  (save-excursion
    (goto-char begin)
    (let ((type (or type (heed--parse-block-type begin)))
          (stack '("block"))
          (children nil)
          (content-begin nil)
          (line-end nil)
          (end-of-block nil))
      (while (and (not (eobp))
                  (not end-of-block)
                  (or
                   (looking-at "^\\s-*[@%].*$")
                   (looking-at "^[ \t]*::.*")
                   stack))
        (forward-line 1)
        (beginning-of-line)
        (cond
         ((looking-at "^[ \t]*::.*")
          (when-let ((child (heed--parse-block-bounds (point))))
            (setq children (append children (list child)))
            (goto-char (-> child (plist-get :bounds) (cdr)))
            (setq content-begin nil)))

         ((and stack (looking-at "^[ \t]*--[\t]*"))
          (progn
            (if (null (cdr stack))
                (setq end-of-block (line-end-position))
              (progn
                (setq stack (cdr stack))
                (when (null (cdr stack))
                  (setq content-begin (1+ (line-end-position))))))))

         ((not (looking-at "^\\s-*[@%].*$"))
          (when (not content-begin)
            (setq content-begin (point))))))
      (beginning-of-line)
      (when-let ((content-begin (or content-begin (point)))
                 (end (or end-of-block (heed--find-block-end)))
                 (block (list :type type
                              :bounds (cons (copy-marker begin)
                                            (copy-marker end))
                              :content-bounds (cons (copy-marker content-begin)
                                                    (copy-marker (progn
                                                                   (goto-char end)
                                                                   (line-beginning-position)))))))
        (when children
          (plist-put block :children children))
        block))))

(defun heed--apply-block-overlay (block)
  "Apply overlay to content-bounds of BLOCK."
  (when-let* ((content-bounds (plist-get block :content-bounds))
              (ov (make-overlay (car content-bounds) (cdr content-bounds))))
    (overlay-put ov 'heed-overlay t)
    (overlay-put ov 'face 'heed-block-content-overlay-face)
    (overlay-put ov 'evaporate t)
    ov))

(defun heed--find-block-end ()
  "Return point just after a valid '--', or nil."
  (save-excursion
    (let ((block-end nil))
      (while (and (not (eobp))
                  (not block-end))
        (if (string-match-p "^\\s-*--\\s-*$" (thing-at-point 'line t))
            (setq block-end (line-end-position))
          (forward-line 1)))
      block-end)))

(defun heed--ensure-valid-block! (block)
  "Return non-nil if BLOCK is still structurally valid — same type and bounds."
  (save-excursion
    (when-let ((parsed (heed--parse-block-bounds
                        (marker-position (car (plist-get block :bounds))))))
      (when (heed--bounds-equal-p :bounds block parsed)
        (let ((old-bounds (plist-get block :content-bounds))
              (new-bounds (plist-get parsed :content-bounds)))
          (when (and (markerp (car old-bounds))
                     (markerp (car new-bounds)))
            (set-marker (car old-bounds) (marker-position (car new-bounds)))
            (set-marker (cdr old-bounds) (marker-position (cdr new-bounds)))
            block))))))

(defun heed--stale-content-overlay-p (block)
  "Determine if content overlay bounds and content-bounds of BLOCK have diverged."
  (when-let ((ov (plist-get block :overlay)))
    (not (equal
          (cons (overlay-start ov) (overlay-end ov))
          (heed--block-bounds block :content-overlay)))))

(defun heed--bounds-equal-p (bound-type a b)
  "Return t if marker cons cells A and B of BOUND-TYPE contain equal points."
  (heed--bounds-cons-equal-p (plist-get a bound-type)
                             (plist-get b bound-type)))

(defun heed--bounds-cons-equal-p (a b)
  "Return t if marker cons cells A and B point to the same positions."
  (and (= (marker-position (car a)) (marker-position (car b)))
       (= (marker-position (cdr a)) (marker-position (cdr b)))))

(defun heed--block-bounds-range-p (block bounds-type)
  "Determine if the bounds of BOUNDS-TYPE in BLOCK constitute an actual range."
  (let ((bounds (heed--block-bounds block bounds-type)))
    (not (equal (car bounds) (cdr bounds)))))

(defun heed--block-bounds (block bounds-type)
  "Return the bounds of BOUNDS-TYPE in BLOCK, as integer cons."
  (when (plist-get block bounds-type)
    (cons (heed--block-bounds-begin block bounds-type)
          (heed--block-bounds-end block bounds-type))))

(defun heed--block-bounds-begin (block bounds-type)
  "Return the bounds left marker of BOUNDS-TYPE in BLOCK, as integer."
  (-> block
      (plist-get bounds-type)
      (car)
      (marker-position)))

(defun heed--block-bounds-end (block bounds-type)
  "Return the bounds right marker of BOUNDS-TYPE in BLOCK, as integer."
  (-> block
      (plist-get bounds-type)
      (cdr)
      (marker-position)))

(defun heed--block-at-point (&optional block-context)
  "Return the block info at point, if any, from `heed--block-boundaries`.

Optionally provide BLOCK-CONTEXT to limit search to a set of blocks.
Used to pass the children of a block to recursively determine if point
is at nested child or outer block.
Defaults to `heed--block-boundaries`"
  (let* ((blocks (or block-context heed--block-boundaries))
         (match (seq-find (lambda (block)
                      (let ((range (plist-get block :bounds)))
                        (and (<= (marker-position (car range)) (point))
                             (< (point) (marker-position (cdr range))))))
                          blocks)))
    (when match
      (if-let ((children (plist-get match :children)))
          (let ((child-at-point (heed--block-at-point children)))
            (or child-at-point match))
        match))))

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



;; Font lock-functions and styling overlays

(defun heed--extend-region ()
  "Force font-lock to always highlight the full line."
  (cons (line-beginning-position) (line-end-position)))

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

(defun heed--match-block-close (limit type)
  "Match block-closing `--` lines that are real block terminators.

TYPE specifies the block type, :block or :aside.
LIMIT ensures matching is not overly greedy."
  (let (matched)
    (while (and (not matched)
                (re-search-forward "^[ \t]*--\\s-*$" limit t))
      ;; Is this position between any known block markers?
      (let ((pos (match-beginning 0)))
        (setq matched
              (seq-some (lambda (block)
                          (let ((range (plist-get block :bounds)))
                            (and (eq type (plist-get block :type))
                                 (<= (car range) pos)
                                 (<= pos (cdr range)))))
                        heed--block-boundaries))))
    matched))

(defun heed--purge-block-boundaries ()
  "Clear block boundary data and remove/clean up any overlays they may contain."
  (mapc (lambda (block)
          (when-let ((ov (plist-get block :overlay)))
            (delete-overlay ov)))
        heed--block-boundaries)
  (setq heed--block-boundaries nil))

(defun heed--adjust-block-overlay! (block)
  "Synchronizes :overlay with :content-bounds in BLOCK.

If :content-bounds is nil or does not describe a range, the overlay is killed."
  (when-let ((ov (plist-get block :overlay)))
    (if-let* ((content-bounds (heed--block-bounds block :content-bounds))
              (_ (not (= (car content-bounds) (cdr content-bounds)))))
        (move-overlay
         ov
         (heed--block-bounds-begin block :content-bounds)
         (heed--block-bounds-end block :content-bounds))
      (progn
        (delete-overlay ov)
        (cl-remf block :overlay)))))

(defun heed--clear-block-overlays-between (start end)
  "Remove any heed block overlays between START and END."
  (mapc #'delete-overlay
        (cl-remove-if-not
         (lambda (ov) (overlay-get ov 'heed-overlay))
         (overlays-in start end))))

(defun heed--clear-block-overlays ()
  "Clear all existing block content overlays."
  (heed--clear-block-overlays-between (point-min) (point-max)))

(defun heed--match-phase-transition-line (limit)
  "Match a phase-transition line within LIMIT."
  (when (save-excursion (re-search-forward "^\\(#\\)?\\([a-zA-Z:0-9_-]+\\)\\s-*\\(-->\\|<--\\)\\s-*\\(.*\\)?" limit t))
    (re-search-forward "^\\(#\\)?\\([a-zA-Z:0-9_-]+\\)\\s-*\\(-->\\)?\\(<--\\)?\\s-*\\(.*\\)?")))



;; Color adjustment

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



;; Presentation navigation

(defun heed--presentation-root ()
  "Get the absolute path the the presentation root."
  (when-let ((absolute (alist-get 'absoluteRoot (heed-cli--get-root))))
    (if (eq :null absolute)
        nil
      absolute)))

(defun heed--slide-file (root index-entry)
  "Get the path to the slide file describe by INDEX-ENTRY on disk.

ROOT is the presentation root directory."
  (let* ((path (alist-get 'path index-entry))
         (slide-id (alist-get 'slide index-entry))
         (type (alist-get 'type index-entry))
         (filename (concat slide-id "." type)))
    (expand-file-name (format "%s/%s" path filename) root)))

(defun heed--open-slide-file (path)
  "Open a slide file at PATH and ensure heed-slide-mode is enabled.

This is a deviation from standard Emacs hooks and auto-modes for the reason
that we don't want to interfere with json-mode or other modes that are more
commonly used for editing non-Heed JSON files."
  (let ((buffer (find-file path)))
    (with-current-buffer buffer
      (heed-slide-mode 1))))

(defun heed--jump-to-slide-number (slide-number)
  "Jump to slide with number SLIDE-NUMBER within the current presentation."
  (if-let* ((root (heed--presentation-root))
            (index (plist-get (gethash root heed--presentations-hash) :index)))
      (when-let ((in-range (and (< slide-number (length index))
                                (>= slide-number 0)))
                 (slide (nth slide-number index)))
        (let* ((fullpath (heed--slide-file root slide)))
          (heed--open-slide-file fullpath)
          (message "%s / %s" (1+ slide-number) (length index))))
    (message "The current buffer is not part of a Heed presentation.")))

(defun with-slide-num (op)
  "Invoke function OP with the current slide number as an argument.

If the current buffer is not part of a presentation, the operation is
aborted."
  (if-let ((current (heed--slide-num)))
      (funcall op current)
    (message "The current buffer is not part of a Heed presentation.")))

(defun heed-next-slide ()
  "Open the next slide in the presentation."
  (interactive)
  (with-slide-num (lambda (n)
                    (unless (heed--jump-to-slide-number (1+ n))
                      (message "No further slides in this presentation.")))))

(defun heed-previous-slide ()
  "Open the previous slide in the presentation."
  (interactive)
  (with-slide-num (lambda (n)
                    (unless (heed--jump-to-slide-number (1- n))
                      (message "This is the first slide in this presentation.")))))

(defun heed-open-slide ()
  "Open a slide from the current presentation."
  (interactive)
  (let* ((root (heed--presentation-root))
         (index (plist-get (gethash root heed--presentations-hash) :index))
         (options (mapcar (lambda (slide)
                            (cons (concat (alist-get 'section slide)
                                          " / "
                                          (alist-get 'slide slide))
                                  (heed--slide-file root slide)))
                          index))
         (slide (completing-read "Open slide:" (mapcar
                                                #'car
                                                options))))
    (heed--open-slide-file (alist-get slide options nil nil #'string-equal))))

(defun heed--ensure-presentation-entry (root)
  "Ensure an entry for ROOT is present in `heed--presentations-hash`."
  (if-let ((pres (gethash root heed--presentations-hash '())))
      pres
    (let ((new-pres '(:root root)))
      (puthash root new-pres heed--presentations-hash)
      new-pres)))

(defun heed-close-presentation-other-files ()
  "Close all other files from this presentation, keeping the current file open."
  (interactive)
  (let ((current-root (heed--presentation-root))
        (counter 0))
    (if current-root
        (dolist (buf (buffer-list))
          (let ((this-file (buffer-file-name))
                (buf-file (buffer-file-name buf)))
            (when (and (not (string-equal this-file buf-file))
                       (string-prefix-p current-root buf-file))
              (when buf
                (kill-buffer buf)
                (setq counter (1+ counter)))
              (message "Closed %s other presentation files." counter))))
      (message "The current buffer is not part of a Heed presentation."))))

(defun heed--store-presentation-data (root key value)
  "Store VALUE for KEY for presentation ROOT in the global presentations hash."
  (let ((pres (heed--ensure-presentation-entry root)))
    (plist-put pres key value)))

(defun heed--refresh-index ()
  "Refresh the cached presentation index for the current file."
  (when-let* ((root (heed-cli--get-root))
              (absolute-root (alist-get 'absoluteRoot root))
              (value-p (not (eq absolute-root :null)))
              (index (heed-cli--load-index)))
    (heed--store-presentation-data absolute-root :index index)
    index))

(defun heed--slide-num ()
  "Get the position of the current slide in the presentation index."
  (let ((index (heed--refresh-index)))
    (cl-position-if
     (lambda (entry)
       (string-equal (alist-get 'slide entry)
                     (file-name-base (buffer-file-name))))
     index)))



;; heed-cli interaction

(defun heed-cli--available-p ()
  "Return non-nil if `heed-cli` is available in PATH."
  (or (executable-find heed-cli-command)
      (and (file-exists-p heed-cli-command)
           (file-executable-p heed-cli-command))))

(defun heed-cli--execute-command (command &optional plain-output)
  "Execute a heed-cli command and return the command output.

COMMAND is a list of string arguments, ie `(\"show\" \"index\").

By default, the output will be parsed with `json-parse-string'.
Any non-nil value for PLAIN-OUTPUT will return the raw output as a string.

The \"--json\" flag must still be provided in the COMMAND by the caller
even if plain-output is nil."
  (if (not (heed-cli--available-p))
      (message "Cannot locate heed-cli executable. (Custom variable `heed-cli-command` has value: '%s'" heed-cli-command)
    (let* ((proc-args (append (list heed-cli-command nil t nil) command))
           (result (with-temp-buffer
                     (apply #'call-process proc-args)
                     (buffer-string))))
      (if plain-output
          result
        (json-parse-string result :object-type 'alist)))))

(defun heed-cli--build-command (base-command path json-p)
  "Build a heed-cli command-list from BASE-COMMAND.

Optionally applies PATH as a final base argument, if non-nil.
Optionally adds \"--json\" to the end of the command for non-nil values
of JSON-P."
  (let ((with-path (if path (append base-command (list path)) base-command)))
    (if json-p
        (append with-path '("--json"))
      with-path)))

(defun heed-cli--load-index (&optional path)
  "Load presentation slide index for a  presentation, if applicable.

The index is loaded for the current buffer file/path, unless overridden
by PATH."
  (cl-map 'list
          #'identity
          (alist-get 'slides (heed-cli--execute-command
                              (heed-cli--build-command '("show" "index") path t)))))

(defun heed-cli--get-root (&optional path)
  "Find presentation root directory using heed-cli.

Resolves the path from the `default-directory` of the current buffer, unless
overridden by PATH."
  (heed-cli--execute-command
   (heed-cli--build-command '("show" "root") path t)))



;; heed-slide-mode

(defvar heed-slide-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-n") #'heed-next-slide)
    (define-key map (kbd "M-p") #'heed-previous-slide)
    (define-key map (kbd "C-c k") #'heed-close-presentation-other-files)
    (define-key map (kbd "C-c n") #'heed-open-slide)
    map)
  "Keymap for `heed-slide-mode'.")

;;;###autoload
(define-minor-mode heed-slide-mode
  "Minor mode for navigating within a Heed presentation."
  :lighter " HeedSlide"
  :keymap heed-slide-mode-map)



;; heed-mode

(defun heed--after-change (_beg _end _len)
  "Check if we're inside a valid block and refresh it, otherwise rescan all."
  (heed--detect-frontmatter)
  (if-let ((block (-> (heed--block-at-point)
                      (heed--ensure-valid-block!))))
      (when (heed--stale-content-overlay-p block)
        (heed--adjust-block-overlay! block))
    (heed--scan-and-mark-blocks)))

;;;###autoload
(define-derived-mode heed-mode prog-mode "Heed"
  "Major mode for editing Heed presentation files."
  (setq-local font-lock-extend-region-functions '(heed--extend-region))
  (heed--init-content-background-face)
  (heed--clear-block-overlays)
  (setq font-lock-defaults '(heed-font-lock-keywords))
  (heed--detect-frontmatter)
  (heed--scan-and-mark-blocks)
  (add-hook 'after-change-functions #'heed--after-change nil t)
  (heed-slide-mode 1))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.heed\\'" . heed-mode))

(provide 'heed-mode)
;;; heed-mode.el ends here
