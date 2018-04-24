;;; see.el --- Edit string  in a separate buffer with appropriate major mode enabled in it

;; Author: Marcelo Muñoz <ma.munoz.araya@gmail.com>
;; Keywords: convenience
;; Version: 0.0.1

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

;; TODO: write commentary


;;; Code:

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



(define-minor-mode see-mode
  "Minor mode for  editing string in buffer with apropiate mode enabled."
  :lighter " see"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map "\C-c'" 'see-exit)
            (define-key map "\C-c\C-k" 'see-abort)
            (define-key map "\C-x\C-s" 'see-save)
            map))


(defun see-set-ov (beg end)
  "TODO"
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
   :str    (buffer-substring-no-properties beg end)))


(defun see-try-determine-lang-mode (string)
  "Try to determine mode based on content of STRING, if fail."
  'sql-mode)

(defun see-generate-buffer-name (mode)
  "Return a string that is the name of no existing buffer based on mode"
  (generate-new-buffer-name (format "[see: %s]" mode)))


(defun see-switch-to-edit-buffer (buffer)
  (pcase see-window-setup
    (`current-window (pop-to-buffer-same-window buffer))
    (`other-window
     (switch-to-buffer-other-window buffer))))


(defun see-edit-snipet (begr endr)
  "TODO: make doc"
  (let* ((datum (see-construct-datum begr endr))
         (beg (plist-get datum :beg))
         (end (plist-get datum :end))
         (code (see-unquote-lines (plist-get datum :str)))
         (mode (see-try-determine-lang-mode code))
         (buffer (generate-new-buffer (see-generate-buffer-name mode)))
         (ov (see-set-ov beg end)))
    (see-set-region-as-read-only beg end)
    (setq mark-active nil)
    (see-switch-to-edit-buffer buffer)
    (insert code)
    (funcall mode)
    (indent-region (point-min) (point-max))
    (see-mode)
    (setq-local see-ov ov)
    (setq-local see-original-snipet (plist-get datum :str))))


(defun see-kill-edit-session ()
  "TODO"
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
  "TODO: make doc"
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
  "TODO"
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
  "TODO: make doc"
  (cond ((derived-mode-p 'c++-mode)
         (see-quote-lines-c++ code))))


(defun see-unquote-lines (code)
  "TODO: make doc"
  (cond ((derived-mode-p 'c++-mode)
         (see-unquote-lines-c++ code))))


(defun see-find-snipet-at-point ()
  (cond ((derived-mode-p 'c++-mode)
         (see-find-snipet-at-point-c++))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;     CUSTOM BACKEND FUNCTION
;;         FOR EACH MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; c++-mode
(defun see-unquote-lines-c++ (code)
  (with-temp-buffer
    (insert code)
    (beginning-of-buffer)
    (while (re-search-forward "\"\\(\\\\.\\|[^\"\\]\\)*\"" nil t)
      (goto-char (match-beginning 0))
      (delete-char 1)
      (goto-char (1- (match-end 0)))
      (delete-char -1))
    (buffer-string)))


(defun see-quote-lines-c++ (code)
  (with-temp-buffer
    (save-excursion
      (insert code))
    (let ((max-col 0))
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


(defun see-find-snipet-at-point-c++ ()
  (let ((point (point))
        (beg   nil)
        (end   nil))
    (save-excursion
      (goto-char (point-min))
      (while
          (and
           (re-search-forward "\"\\(\\\\.\\|[^\"\\]\\)*\"\\([\n[:blank:]]*\"\\(\\\\.\\|[^\"\\]\\)*\"\\)*" nil t)
           (not (and (<= (match-beginning 0) point (match-end 0))
                     (setq beg (match-beginning 0)
                           end (match-end 0))))))
      (and beg end `(,beg . ,end)))))

(provide 'see)

;;; see.el ends here
