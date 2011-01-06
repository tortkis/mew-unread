;;; mew-unread (folder list mode for mew)

;;; Copyright (C) 2011, Toru Takaishi
;;; All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;  * Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.

;;;  * Redistributions in binary form must reproduce the above
;;;    copyright notice, this list of conditions and the following
;;;    disclaimer in the documentation and/or other materials provided
;;;    with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
;;; CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
;;; INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS
;;; BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;; TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
;;; ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
;;; TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
;;; THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.

;; Updated: January 2, 2011
;; - fixed a bug of not updating the unread diff number in some cases.
;; - fixed a bug that visiting +queue or +draft is redirected to *Mew unread* buffer.

(require 'mew)

;; variables

(defvar *mew-unread-mode-hook* nil
  "*List of functions to call when entering mew-unread mode.")

(defvar *mew-unread-mode-map* nil
  "Keymap for mew-unread major mode.")

(defvar *mew-unread-check-list* nil
  "*List of mew folders to be listed by mew-unread-check.")

(defvar *mew-unread-position* 0)
(defvar *mew-unread-check-exclude* '("+queue" "+draft"))

(defvar *mew-unread-counts* (make-hash-table :test 'equal))
(defvar *mew-unread-diff* (make-hash-table :test 'equal))

;; mew unread major mode

(defun mew-unread-mode ()
  "Major mode for mew folder list with the numbers of unread & marked messages."
  (interactive)
  (kill-all-local-variables)
  (if *mew-unread-mode-map*
      nil
    (setq *mew-unread-mode-map* (make-sparse-keymap))
    (define-key *mew-unread-mode-map* "p" 'mew-unread-move-up)
    (define-key *mew-unread-mode-map* "n" 'mew-unread-move-down)
    (define-key *mew-unread-mode-map* "q" 'mew-unread-quit)
    (define-key *mew-unread-mode-map* "i" 'mew-unread-retrieve)
    (define-key *mew-unread-mode-map* "g" 'mew-unread-goto-folder)
    (define-key *mew-unread-mode-map* " " 'mew-unread-visit-folder)
    (define-key *mew-unread-mode-map* "\r" 'mew-unread-visit-folder))
  (setq major-mode 'mew-unread-mode)
  (setq mode-name "Mew-Unread")
  (use-local-map *mew-unread-mode-map*)
  (hl-line-mode)
  (font-lock-mode t)
  (run-hooks '*mew-unread-mode-hook*))

(defun mew-unread-move-up ()
  (interactive)
  (if (>= (line-number-at-pos) 4)
      (forward-line -1)
    (goto-char (point-min))
    (forward-line 2)))

(defun mew-unread-move-down ()
  (interactive)
  (when (< (line-number-at-pos) (+ (length *mew-unread-check-list*) 2))
    (forward-line 1))
  (when (< (line-number-at-pos) 3)
    (goto-char (point-min))
    (forward-line 2)))

(defun mew-unread-retrieve ()
  (interactive)
  (mew-summary-switch-to-folder "+inbox")
  (mew-summary-retrieve))

(defun mew-unread-goto-folder ()
  (interactive)
  (setq w0 (selected-window))
  (setq w1 (split-window-vertically))
  (select-window w1)
  (mew-summary-switch-to-folder "+inbox")
  (mew-summary-goto-folder)
  (delete-window w0))

(defun mew-unread-quit ()
  (interactive)
  (kill-buffer nil))

(defun mew-unread-visit-folder (&optional go-to-unread)
  (interactive)
  (if (< (line-number-at-pos) 3)
      (progn
        (goto-char (point-min))
        (forward-line 2))
    (beginning-of-line)
    (let* ((sp (1- (search-forward ":+")))
           (ep (re-search-forward "$"))
           (fname (buffer-substring sp ep)))
      (beginning-of-line)
      (setq *mew-unread-position* (point))
      (mew-summary-switch-to-folder fname)
      (when go-to-unread
        (beginning-of-buffer)
        (unless (re-search-forward "^U" nil t nil)
          (goto-char (point-max)))))))

;; Check unread mails

(defun mew-unread-check-count (regexp)
  (save-excursion
    (goto-char (point-min))
    (let ((cnt 0))
      (while (re-search-forward regexp nil t nil)
        (setq cnt (1+ cnt)))
      cnt)))

(defun mew-unread-display ()
  (get-buffer-create "*Mew unread*")
  (switch-to-buffer "*Mew unread*")
  (mew-unread-mode)
  (setq buffer-read-only nil)
  (kill-region (point-min) (point-max))
  (insert " total  unread(diff)  marked(diff)  :folder\n")
  (insert "-----------------------------------------------------\n")
  (mapc #'(lambda (folder)
            (let ((unreadnum (or (car (gethash folder *mew-unread-counts*)) 0))
                  (marknum (or (cadr (gethash folder *mew-unread-counts*)) 0))
                  (totalnum (or (caddr (gethash folder *mew-unread-counts*)) 0))
                  (unreadnum-diff (or (car (gethash folder *mew-unread-diff*)) 0))
                  (marknum-diff (or (cadr (gethash folder *mew-unread-diff*)) 0)))
              (goto-char (point-max))
              (let (p0 p1)
                (insert (format "%6d" totalnum))
                (insert (format "  %6d" unreadnum))
                (setq p0 (point))
                (insert (format "%-6s" (format "(%+d)" unreadnum-diff)))
                (setq p1 (point))
                (unless (= unreadnum-diff 0)
                  (put-text-property p0 p1 'font-lock-face
                                     `(foreground-color . ,(if (> unreadnum-diff 0) "red" "blue"))))
                (insert (format "  %6d" marknum))
                (setq p0 (point))
                (insert (format "%-6s" (format "(%+d)" marknum-diff)))
                (setq p1 (point))
                (unless (= marknum-diff 0)
                  (put-text-property p0 p1 'font-lock-face
                                     `(foreground-color . ,(if (> marknum-diff 0) "red" "blue"))))
                (insert (format "  :%s\n" folder)))))
        *mew-unread-check-list*)
  (goto-char *mew-unread-position*)
  (when (< (line-number-at-pos) 3)
    (goto-char (point-min))
    (forward-line 2))
  (setq buffer-read-only t)
  (redraw-display))

(defun mew-unread-check-folder (folder &optional clear-diff)
  (let ((unreadnum 0) (marknum 0) (totalnum 0)
        (unreadnum-diff 0) (marknum-diff 0))
    (mew-summary-switch-to-folder folder)
    (setq unreadnum (mew-unread-check-count "^U"))
    (setq marknum (mew-unread-check-count "^*"))
    (setq totalnum (count-lines (point-min) (point-max)))
    (if (or clear-diff
            (not (gethash folder *mew-unread-counts*)))
        (setf (gethash folder *mew-unread-diff*) (list 0 0))
      (setf (gethash folder *mew-unread-diff*)
            (list (- unreadnum (car (gethash folder *mew-unread-counts*)))
                  (- marknum (cadr (gethash folder *mew-unread-counts*))))))
    (setf (gethash folder *mew-unread-counts*) (list unreadnum marknum totalnum))))

(defun mew-unread-check-all (&optional cfolder clear-diff)
  (mapc #'(lambda (folder)
            (when (or (and cfolder (string-equal folder cfolder))
                      (null (gethash folder *mew-unread-counts*)))
              (mew-unread-check-folder folder clear-diff)))
        *mew-unread-check-list*)
  (mew-unread-display))

(defun mew-unread-check ()
  (interactive)
  (let ((cbuf-name (buffer-name)))
    (mew-unread-check-all)
    (if (find cbuf-name *mew-unread-check-exclude* :test 'equal)
        (mew-unread-display)
      (mew-summary-switch-to-folder cbuf-name)
      (save-excursion
        (mew-summary-ls t nil t))
      ;; after mew-summary-ls, mew-unread-check-folder will be called.
      )))

(defun mew-unread-summary-ls-and-retrieve (&optional no-flush)
  (interactive "P")
  (mew-unread-check-folder (buffer-name) t)
  (mew-summary-retrieve no-flush))

;; retrieve mail

(setq mew-pop-sentinel-non-biff-hook
      '(lambda ()
         (set-buffer "+inbox")
         ;; record received emails in *received-email* buffer
         (when (> (point-max) (point-min))
           (let ((rcved (replace-regexp-in-string
                         "^" "==> "
                         (replace-regexp-in-string
                          "[ ]+\r.*$" ""
                          (buffer-substring (point) (- (point-max) 1))))))
             (get-buffer-create "*received-email*")
             (set-buffer "*received-email*")
             (setq buffer-read-only nil)
             (goto-char (point-max))
             (insert rcved)
             (insert "\n")
             (setq buffer-read-only t)))
         ;;
         (mew-summary-switch-to-folder "+inbox")
         (mew-summary-auto-refile)
         (mew-summary-exec)
         ;; run mew-summary-exec-hook
         (mew-unread-check-all)
         (mew-unread-display)
         t))

(add-hook 'mew-summary-exec-hook
          '(lambda ()
             (let ((cbuf-name (buffer-name)))
               (mapc #'(lambda (folder)
                         (mew-summary-switch-to-folder folder)
                         (save-excursion
                           (mew-summary-ls t nil nil)))
                     *mew-unread-check-list*)
               (mew-summary-switch-to-folder cbuf-name))))

(add-hook 'mew-scan-sentinel-hook
          '(lambda ()
             (let ((cbuf-name (buffer-name)))
               (unless (find cbuf-name *mew-unread-check-exclude* :test 'equal)
                 (mew-unread-check-all cbuf-name)))))

(add-hook 'mew-summary-mode-hook
	  '(lambda ()
	     (local-set-key "b" 'mew-unread-check)
             (local-set-key "i" 'mew-unread-summary-ls-and-retrieve)))

(provide 'mew-unread)



;; [mew] summary mode
;;          | "i"
;;          | mew-pop-sentinel-non-biff-hook
;;          | switch to +inbox
;; [mew] mew-summary-auto-refile
;; [mew] mew-summary-exec
;;          | mew-summary-exec-hook
;; [mew] mew-summary-ls for all folders
;;          | mew-scan-sentinel-hook
;;       mew-unread-check-all (count for all folders)
;;          |
;;       mew-unread-display

;; [mew] summary mode
;;          | "b"
;;       mew-unread-check
;;          |
;;       mew-unread-check-all (initialization)
;;          |
;; [mew] mew-summary-ls for current folders
;;          | mew-scan-sentinel-hook
;;       mew-unread-check-all (count for current folder)
;;          |
;;       mew-unread-display

