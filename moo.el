;;; moo.el --- comint-based emacs interface for MOOs.

;;; Copyright (C) 1993 Noah S. Friedman

;;; Author: Noah Friedman <friedman@prep.ai.mit.edu>
;;; Maintainer: Noah Friedman <friedman@prep.ai.mit.edu>
;;; Keywords: games, extensions

;;; $Id: moo.el,v 1.0 1993/11/15 05:26:49 friedman Exp $

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, you can either send email to this
;;; program's author (see below) or write to: The Free Software Foundation,
;;; Inc.; 675 Massachusetts Avenue; Cambridge, MA 02139, USA.

;;; Code:

;; Require comint "softly".
(or (featurep 'comint)
    (load "comint" t))

;;;###autoload
(defvar moo-buffer-name-format "*moo-%s*"
  "*Basic buffer name for MOO sessions.")

;;;###autoload
(defvar moo-mode-hook nil
  "*Hook to run at the end of moo-mode.")

;;;###autoload
(defvar moo-host "microworld.media.mit.edu"
  "*A string specifying the internet name or IP address of the MOO host.")

;;;###autoload
(defvar moo-port 8888
  "*An integer specifying the TCP port of the MOO on moo-host.")

;;;###autoload
(defun moo-mode ()
  "Major mode for MOO sessions.

If the `comint' library is available, `comint-mode' is called to
implement a command history, etc.  Otherwise, `text-mode' is called.
This means either `comint-mode-hook' or `text-mode-hook' may be run, but
almost certainly not both.

It is best to put MOO Mode--specific hooks on `moo-mode-hook', which is run
last of all."
  (interactive)
  (if (not (featurep 'comint))
      (text-mode)
    (comint-mode)
    (make-local-variable 'comint-prompt-regexp)
    (setq comint-prompt-regexp "^"))
  ;; This must be done after calling comint-mode since that function may
  ;; set its own process filter and sentinel for this process.
  (let ((proc (get-buffer-process (current-buffer))))
    (set-process-filter proc 'moo-filter)
    (set-process-sentinel proc 'moo-sentinel))
  (setq mode-name "MOO")
  (setq major-mode 'moo-mode)
  (setq mode-line-process '(": %s"))
  (make-local-variable 'case-fold-search)
  (setq case-fold-search t)
  (run-hooks 'moo-mode-hook))

;;;###autoload
(defun moo (host &optional port &optional newp)
  "Connect to a MOO.
Emacs prompts for the host and port number.  The default for each is
inserted in the minibuffer to be edited if desired.

With a prefix argument, always create a new MOO session even if there is
already an existing connection to that MOO.  Otherwise, try to switch to an
existing session on that host."
  (interactive (list (read-from-minibuffer "MOO host: " moo-host)
                     (read-from-minibuffer "MOO port: "
                                           (number-to-string moo-port))
                     current-prefix-arg))
  (setq port (string-to-number port)
        moo-port port
        moo-host host)
  (let* ((buf-fun (if newp 'generate-new-buffer 'get-buffer-create))
         (buffer (funcall buf-fun (format moo-buffer-name-format host)))
         (proc (get-buffer-process buffer)))
    (switch-to-buffer buffer)
    (or (and proc (memq (process-status proc) '(run stop open)))
        (progn
          (goto-char (point-max))
          (setq proc (open-network-stream "moo" buffer host port))
          (set-marker (process-mark proc) (point-max))
          (moo-mode)
          (cond
           ((string-lessp "19" emacs-version)
            ;; Done for Emacs 19 only.
            (make-local-variable 'kill-buffer-hook)
            (add-hook 'kill-buffer-hook 'moo-delete-process)))))))

(defun moo-filter (proc string)
  (let* ((process-buffer (process-buffer proc))
         (proc-mark (process-mark proc))
         (old-proc-mark-pos (marker-position proc-mark))
         (original-buffer (current-buffer))
         user-point
         user-point-offset)
    (unwind-protect
        (progn
          (set-buffer process-buffer)
          (setq user-point (point))
          (setq user-point-offset (- user-point proc-mark))
          (goto-char proc-mark)
          (insert string)
          (set-marker proc-mark (point))
          (goto-char old-proc-mark-pos)
          (while (search-forward "\C-m" proc-mark 'goto-end)
            (delete-char -1))
          (if (>= user-point-offset 0)
              (goto-char (+ proc-mark user-point-offset))
            (goto-char user-point)))
      (set-buffer original-buffer))))

(defun moo-sentinel (proc event)
  (let ((orig-buffer (current-buffer)))
    (unwind-protect
        (progn
          (set-buffer (process-buffer proc))
          (insert (format "Process %s %s\n" proc event))
          (goto-char (point-max)))
      (set-buffer orig-buffer)))
  (moo-delete-process proc))

(defun moo-delete-process (&optional proc)
  (or proc
      (setq proc (get-buffer-process (current-buffer))))
  (and proc
       (delete-process proc)))

(provide 'moo)

;; End of file moo.el
