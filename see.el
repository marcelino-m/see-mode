;;; see.el --- Edit string  in a separate file with appropriate major mode enabled in it

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
  :group 'org-edit-structure
  :type '(choice
          (const current-window)
          (const other-window)))



(define-minor-mode see-mode
  "Minor mode for  editing string in buffer with apropiate mode enabled."
  :lighter " see"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map "\C-c'" 'see-exit)
            ;; (define-key map "\C-c\C-k" 'see-abort)
            (define-key map "\C-x\C-s" 'see-save)
            map))


(defun see-set-ov (beg end)
  "TODO"
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'face 'secondary-selection)
    (overlay-put ov 'evaporate t)
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


(defun see-find-string-literal-in-region (beg end)
  "Find string literal in region"
  (cond ((derived-mode-p 'c++-mode)
         (see-find-string-literal-in-region-c++-mode beg end))))


(defun see-trimmed-empty-lines (code)
  "Remove empty lines from begining and end."
  (with-temp-buffer
    (insert code)
    (beginning-of-buffer)
    (let ((point (point)))
      (while (looking-at "^[[:space:]]*$")
        (forward-line))
      (delete-region point (point)))
    (end-of-buffer)
    (let ((end (point)))
      (re-search-backward "[[:graph:]]")
      (forward-char)
      (delete-region (point) end)
      (delete-trailing-whitespace (point-min) (point-max))
      (buffer-string))))


(defun see-quote-lines (code)
  "TODO: make doc"
  (cond ((derived-mode-p 'c++-mode)
         (see-quote-lines-c++ code))))


(defun see-unquote-lines (code)
  "TODO: make doc"
  (cond ((derived-mode-p 'c++-mode)
         (see-unquote-lines-c++ code))))


(defun see-switch-to-edit-buffer (buffer)
  (pcase see-window-setup
    (`current-window (pop-to-buffer-same-window buffer))
    (`other-window
     (switch-to-buffer-other-window buffer))))

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


(defun see-try-determine-lang-mode (code)
  "Try to determine mode based on content of string CODE, if fail, ask user for mode"
  (interactive)
  'sql-mode)


;;;###autoload
(defun see-edit-find-in-region (begr endr)
  "TODO: make doc"
  (interactive "r")
  (let* ((datum (see-find-string-literal-in-region begr endr))
         (beg (plist-get datum :beg))
         (end (plist-get datum :end))
         (code (see-unquote-lines (plist-get datum :str)))
         (mode (see-try-determine-lang-mode code))
         (buffer (generate-new-buffer (generate-new-buffer-name "see-[SQL]")))
         (ov (see-set-ov beg end)))
    (see-set-region-as-read-only beg end)
    (setq mark-active nil)
    (see-switch-to-edit-buffer buffer)
    (insert code)
    (funcall mode)
    (indent-region (point-min) (point-max))
    (see-mode)
    (setq-local see-ov ov)))


(defun see-save ()
  "TODO: make doc"
  (interactive)
  (let ((code (see-trimmed-empty-lines
               (buffer-substring-no-properties (point-min) (point-max))))
        (beg  (overlay-start see-ov))
        (end  (overlay-end see-ov))
        (ov   nil))
    (with-current-buffer (overlay-buffer see-ov)
      (let ((inhibit-read-only t))
        (delete-region beg end)
        (goto-char beg)
        (insert (see-quote-lines code))
        (let ((end (point)))
          (setq ov (see-set-ov beg end))
          (goto-char beg)
          (indent-region-line-by-line beg end)
          (see-set-region-as-read-only beg end))))
    (setq see-ov ov)))

;;;###autoload
(defun see-exit ()
  "TODO"
  (interactive)
  (see-save)
  (see-kill-edit-session))

;;;###autoload
(defun see-edit-src-at-point ()
  (interactive)
  (cond ((derived-mode-p 'c++-mode)
         (see-edit-src-at-point-c++))))


;;; custom function for each mode
;; c++-mode

(defun see-find-string-literal-in-region-c++-mode (beg end)
  (save-excursion
    (goto-char beg)
    (if (re-search-forward "\"\\(.\\|\\.\\)*\"\\([\n[:blank:]]*\"\\(.\\|\\.\\)*\"\\)*" end t)
        (let ((beg (match-beginning 0))
              (end (point)))
          (list
           :beg    beg
           :end    end
           :buffer (current-buffer)
           :str    (buffer-substring-no-properties beg end))))))

(defun see-unquote-lines-c++ (code)
  (with-temp-buffer
    (insert code)
    (beginning-of-buffer)
    (while (re-search-forward "\"\\(.\\|\\.\\)*\"" nil t)
      (goto-char (match-end 0))
      (delete-char -1)
      (goto-char (match-beginning 0))
      (delete-char 1))
    (buffer-string)))

(defun see-quote-lines-c++ (code)
  (with-temp-buffer
    (insert code)
    (beginning-of-buffer)
    (while
        (progn
          (insert "\"")
          (end-of-line)
          (insert " \"")
          (zerop (forward-line 1))))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun see-edit-src-at-point-c++ ()
  (let ((point (point))
        (beg   nil)
        (end   nil))
    (save-excursion
      (goto-char (point-min))
      (while (and (re-search-forward "\"\\(.\\|\\.\\)*\"\\([\n[:blank:]]*\"\\(.\\|\\.\\)*\"\\)*" nil t)
                  (not (and (<= (match-beginning 0) point (match-end 0))
                            (setq beg (match-beginning 0)
                                  end (match-end 0))))))
      (and beg end (see-edit-find-in-region beg end)))))

(provide 'see)

;;; see.el ends here
