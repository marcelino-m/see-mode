;;; see.el --- Edit string  in a separate buffer with appropriate major mode enabled in it

;; Author: Marcelo Muñoz <ma.munoz.araya@gmail.com>
;; Keywords: convenience
;; Version: 0.0.1
;; Package-Requires: ((ivy "0.10.0") (language-detection "0.1.0"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This package is realy awesome!.

;;; Code:

(require 'language-detection)
(require 'ivy)

;;;###autoload
(defcustom see-window-setup 'other-window
  "How the source code edit buffer should be displayed.
Possible values for this option are:

current-window    Show edit buffer in the current window, keeping all other
                  windows.
other-window      Use `switch-to-buffer-other-window' to display edit buffer."
  :group 'see-mode
  :type '(choice
          (const current-window)
          (const other-window)))

;;;###autoload
(defcustom see-use-align-quotes nil
  "Control if quotes are aligned vertically"
  :group 'see-mode
  :type '(boolean))

(defvar see-language-detection-alist
  '((ada         . ada-mode)
    (awk         . awk-mode)
    (c           . c-mode)
    (cpp         . c++-mode)
    (clojure     . clojure-mode)
    (csharp      . csharp-mode)
    (css         . css-mode)
    (dart        . dart-mode)
    (delphi      . delphi-mode)
    (emacslisp   . emacs-lisp-mode)
    (erlang      . erlang-mode)
    (fortran     . fortran-mode)
    (fsharp      . fsharp-mode)
    (go          . go-mode)
    (groovy      . groovy-mode)
    (haskell     . haskell-mode)
    (html        . html-mode)
    (java        . java-mode)
    (javascript  . javascript-mode)
    (json        . json-mode)
    (latex       . latex-mode)
    (lisp        . lisp-mode)
    (lua         . lua-mode)
    (matlab      . octave-mode)
    (objc        . objc-mode)
    (perl        . perl-mode)
    (php         . php-mode)
    (prolog      . prolog-mode)
    (python      . python-mode)
    (r           . r-mode)
    (ruby        . ruby-mode)
    (rust        . rust-mode)
    (scala       . scala-mode)
    (shell       . shell-script-mode)
    (smalltalk   . smalltalk-mode)
    (sql         . sql-mode)
    (swift       . swift-mode)
    (visualbasic . visual-basic-mode)
    (xml         . sgml-mode)))



(defvar see-regx-str-literal-c "\"\\(\\\\.\\|[^\"\\]\\)*\""
  "This regex match c and c++ string literal")

(define-minor-mode see-mode
  "Minor mode for  editing string in buffer with apropiate mode enabled."
  :lighter " see"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map "\C-c'" 'see-exit)
            (define-key map "\C-c\C-k" 'see-abort)
            (define-key map "\C-x\C-s" 'see-save)
            map))


(defun see-set-ov (beg end)
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'face 'secondary-selection)
    ov))


(defun see-set-region-as-read-only (beg end)
  "Mark region as read only"
  (with-silent-modifications
    (let ((inhibit-read-only t))
      (put-text-property beg end 'read-only t))))


(defun see-unset-region-as-read-only (beg end)
  "Mark region as read only"
  (interactive "r")
  (with-silent-modifications
    (let ((inhibit-read-only t))
          (put-text-property beg end 'read-only nil))))


(defun see-cleanup-before-copy-back (code)
  "Remove empty  lines from upper  and bottom of code  and remove
trailing whitespace."
  (with-temp-buffer
    (insert code)
    (beginning-of-buffer)
    (let ((point (point)))
      (skip-syntax-forward "-")
      (beginning-of-line)
      (delete-region point (point))
      (end-of-buffer)
      (setq point (point))
      (skip-syntax-backward "-")
      (end-of-line)
      (delete-region (point) point)
      (delete-trailing-whitespace (point-min) (point-max))
      (buffer-string))))


(defun see-construct-datum (beg end)
  (list
   :beg    beg
   :end    end
   :buffer (current-buffer)
   :mode   nil))


(defun see-try-determine-lang-mode (string)
  "Try to determine mode based on content of STRING, if fail."
  (cdr
   (assoc
    (language-detection-string string) see-language-detection-alist)))

(defun see-generate-buffer-name (mode)
  "Return a string that is the name of no existing buffer based on mode"
  (generate-new-buffer-name (format "[see: %s]" mode)))

(defun see-switch-to-edit-buffer (buffer)
  (pcase see-window-setup
    (`current-window (pop-to-buffer-same-window buffer))
    (`other-window
     (switch-to-buffer-other-window buffer))))


(defun see-edit-snipet (begr endr)
  (let* ((datum        (see-construct-datum begr endr))
         (beg          (plist-get datum :beg))
         (end          (plist-get datum :end))
         (mode         (plist-get datum :mode))
         (raw-str      (buffer-substring-no-properties beg end))
         (code         (see-unquote-lines raw-str))
         (ov           (see-set-ov beg end))
         (inhibit-quit t)
         (win-conf     (current-window-configuration))
         (return-flag   nil))

    (see-set-region-as-read-only beg end)
    (setq mark-active nil)
    (unless mode
      (setq mode (see-try-determine-lang-mode code))
      (unless
          (with-local-quit
            (unless (y-or-n-p (format "%s was dectect, it's correct" mode))
              (see-select-major-mode
               (lambda (m)
                 (setq mode (intern m)))))
            t)
        (see-unset-region-as-read-only beg end)
        (delete-overlay ov)
        (setq return-flag t)))

    (unless return-flag
      (see-switch-to-edit-buffer (see-generate-buffer-name mode))
      (insert code)
      (funcall mode)
      (indent-region (point-min) (point-max))
      (see-mode)
      (setq-local see-ov ov)
      (setq-local see-original-snipet raw-str)
      (setq-local see-saved-win-conf  win-conf))))


(defun see-select-major-mode (fn)
  (interactive)
  (ivy-read "Select mode: "
            (mapcar 'cdr see-language-detection-alist)
            :action fn))

(defun see-kill-edit-session ()
  (let ((source-buffer (overlay-buffer see-ov))
        (edit-buffer (current-buffer))
        (beg (overlay-start see-ov))
        (end (overlay-end see-ov))
        (ov see-ov))
    (with-current-buffer source-buffer
      (see-unset-region-as-read-only beg end)
      (delete-overlay ov))
    (pcase see-window-setup
      (`current-window
       (pop-to-buffer-same-window source-buffer)
       (kill-buffer edit-buffer))
      (`other-window
       (switch-to-buffer-other-window source-buffer)
       (kill-buffer edit-buffer)))))


(defun see-save ()
  (interactive)
  (let ((code (see-cleanup-before-copy-back
               (buffer-substring-no-properties (point-min) (point-max))))
        (beg  (overlay-start see-ov))
        (end  (overlay-end see-ov))
        (ov   see-ov))
    (with-current-buffer (overlay-buffer see-ov)
      (let ((inhibit-read-only t))
        (delete-region beg end)
        (goto-char beg)
        (insert (see-quote-lines code))
        (move-overlay ov beg (point))
        (let ((end (point)))
          (goto-char beg)
          (indent-region-line-by-line beg end)
          (see-set-region-as-read-only beg end))))))

(defun see-restore-original-snipet ()
  "Discard any modification on original buffer."
  (interactive)
  (let ((beg    (overlay-start see-ov))
        (end    (overlay-end   see-ov))
        (ov     see-ov)
        (snipet see-original-snipet))
    (with-current-buffer (overlay-buffer see-ov)
      (let ((inhibit-read-only t))
        (delete-region beg end)
        (goto-char beg)
        (insert snipet)
        (move-overlay ov beg (point))
        (see-set-region-as-read-only beg (point))))))

(defun see-abort ()
  "Discard any modification on original buffer and kill edit session."
  (interactive)
  (see-restore-original-snipet)
  (see-kill-edit-session))

(defun see-exit ()
  (interactive)
  (see-save)
  (see-kill-edit-session))

;;;###autoload
(defun see-edit-src-at-point ()
  (interactive)
  (let ((region (see-find-snipet-at-point)))
    (if region
        (see-edit-snipet (car region) (cdr region))
      (message "Nothing to edit here."))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  FUNCTION THAT DEPEND ON CUSTOM
;;          BACKEND FUNCTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun see-quote-lines (code)
  (cond ((derived-mode-p 'c++-mode 'c-mode)
         (see-quote-lines-cc code))))


(defun see-unquote-lines (code)
  (cond ((derived-mode-p 'c++-mode 'c-mode)
         (see-unquote-lines-cc code))))


(defun see-find-snipet-at-point ()
  (cond ((derived-mode-p 'c++-mode 'c-mode)
         (see-find-snipet-at-point-cc))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;     CUSTOM BACKEND FUNCTION
;;         FOR EACH MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; c++-mode
(defun see-unquote-lines-cc (code)
  (with-temp-buffer
    (insert code)
    (beginning-of-buffer)
    (while (re-search-forward see-regx-str-literal-c nil t)
      (goto-char (match-beginning 0))
      (delete-char 1)
      (goto-char (1- (match-end 0)))
      (delete-char -1))
    (beginning-of-buffer)
    (while (re-search-forward "\\(\\\\\\)*\"" nil t)
      ;; handle escape quotes
      (let ((len (- (match-end 0) (match-beginning 0))))
        (if (= len 2)
            (replace-match "\"" nil t)
          (replace-match
           (concat (make-string (- len 3) ?\\) "\"") nil t))))
    (buffer-string)))


(defun see-quote-lines-cc (code)
  (with-temp-buffer
    (save-excursion
      ;; handle escape quoted
      (insert code)
      (beginning-of-buffer)
      (while (re-search-forward "\\(\\\\\\)*\"" nil t)
        (let ((len (- (match-end 0) (match-beginning 0))))
          (if (= len 1)
              (replace-match "\\\"" nil t)
            (replace-match
             (concat (make-string (1+ len) ?\\) "\"") nil t)))))
    (let ((max-col 0))
      ;; calcule column of longest line
      (if see-use-align-quotes
          (save-excursion
            (while
                (progn
                  (end-of-line)
                  (when (> (current-column) max-col)
                    (setq max-col (current-column)))
                  (zerop (forward-line 1))))))
      (while
          (progn
            (insert "\"")
            (end-of-line)
            (if (>= max-col (current-column))
                (insert
                 (concat
                  (apply 'concat
                         (make-list  (- (+ max-col 2) (current-column))  " ")) "\""))
              (insert " \""))
            (zerop (forward-line 1)))))
    (buffer-substring-no-properties (point-min) (point-max))))


(defun see-find-snipet-at-point-cc ()
  (let ((point (point))
        (beg   nil)
        (end   nil)
        (regx  see-regx-str-literal-c))
    (save-excursion
      (goto-char (point-min))
      (while
          (and
           (re-search-forward (format "%s\\(%s*%s\\)*" regx "[\n[:blank:]]" regx) nil t)
           (not (and (<= (match-beginning 0) point (match-end 0))
                     (setq beg (match-beginning 0)
                           end (match-end 0))))))
      (and beg end `(,beg . ,end)))))

(provide 'see)

;;; see.el ends here
