;;; nicomment.el --- Flow comment on your Emacs -*- lexical-binding:t -*-

;; Author: Wataru MIYAGUNI <gonngo@gmail.com>
;; URL: https://github.com/gongo/emacs-nicomment
;; Version: 0.0.1
;; Keywords: posframe, niconico
;; Package-Requires: ((posframe "0.5.0"))

;;; Commentary:

;; 'nicomment.el' is a library that reproduces the flowing comment of niconico
;; [https://www.nicovideo.jp]

;; This library is based on the following libraries.
;; [https://github.com/tumashu/posframe]
;; [https://github.com/kusabashira/vim-nicomment]

(eval-when-compile (require 'cl))
(require 'posframe)

;;; Code:

(defstruct nicomment--comment
  (id (nicomment--generate-comment-id))
  (x nil)
  (y nil)
  (size 'normal)
  (color nil)
  (speed (+ 1 (random 3)))
  (expire-at nil)
  (message ""))

(defvar nicomment--timeout-second-fixed-comment 3)
(defvar nicomment--timer nil)
(defvar nicomment--comments nil)

;;
;; see https://nullprogram.com/blog/2010/05/11/
;;
(defun nicomment--generate-comment-id ()
  (let ((s (md5 (format "%s%s%s%s%s%s%s%s%s%s"
                        (user-uid)
                        (emacs-pid)
                        (system-name)
                        (user-full-name)
                        user-mail-address
                        (current-time)
                        (emacs-uptime)
                        (garbage-collect)
                        (random)
                        (recent-keys)))))
    (format "%s-%s-3%s-%s-%s"
            (substring s 0 8)
            (substring s 8 12)
            (substring s 13 16)
            (substring s 16 20)
            (substring s 20 32))))

(defun nicomment--comment-tick (comment)
  (decf (nicomment--comment-x comment) (nicomment--comment-speed comment)))

(defun nicomment--comment-deletable-p (comment)
  (or (< (nicomment--comment-x comment) 0)         ;; Outside the left of posframe parent (= current) frame.
      (when (nicomment--comment-expire-at comment) ;; Timeout of fixed comment
        (time-less-p (nicomment--comment-expire-at comment) (current-time)))
      ))

(cl-defun nicomment--comment-display (comment &key position poshandler)
  (when (and (null position) (null poshandler))
    (error "Nicomment: Specify either :position (int . int) or :poshandler (function)"))
  (let ((font-size (case (nicomment--comment-size comment)
                     (large
                      (* (frame-char-size) 5))
                     (small
                      (* (frame-char-size) 2))
                     (t
                      (* (frame-char-size) 3))))
        )
    (posframe-show
     (nicomment--comment-id comment)
     :string (nicomment--comment-message comment)
     :font (font-spec :size font-size)
     :foreground-color (nicomment--comment-color comment)
     :override-parameters '((alpha 100 80))
     :position position
     :poshandler poshandler)))

(defun nicomment--comment-start-at-right (comment)
  (nicomment--comment-display
   comment :poshandler (lambda (info)
                         (cons (setf (nicomment--comment-x comment) (- (plist-get info :parent-frame-width)
                                                                       (plist-get info :posframe-width)))
                               (setf (nicomment--comment-y comment) (random (- (plist-get info :parent-frame-height)
                                                                               (plist-get info :posframe-height)
                                                                               (plist-get info :mode-line-height)
                                                                               (plist-get info :minibuffer-height))))))))

(defun nicomment--comment-start-at-top-center (comment)
  (setf (nicomment--comment-speed comment) 0)
  (setf (nicomment--comment-expire-at comment)
        (time-add (current-time) (seconds-to-time nicomment--timeout-second-fixed-comment)))
  (nicomment--comment-display
   comment :poshandler (lambda (info)
                         (let ((pos (posframe-poshandler-frame-top-center info)))
                           (cons (setf (nicomment--comment-x comment) (car pos))
                                 (setf (nicomment--comment-y comment) (cdr pos)))))))

(defun nicomment--comment-start-at-bottom-center (comment)
  (setf (nicomment--comment-speed comment) 0)
  (setf (nicomment--comment-expire-at comment)
        (time-add (current-time) (seconds-to-time nicomment--timeout-second-fixed-comment)))
  (nicomment--comment-display
   comment :poshandler (lambda (info)
                         (let ((pos (posframe-poshandler-frame-top-center info)))
                           (cons (setf (nicomment--comment-x comment) (car pos))
                                 (setf (nicomment--comment-y comment) (- (plist-get info :parent-frame-height)
                                                                         (plist-get info :posframe-height)
                                                                         (plist-get info :mode-line-height)
                                                                         (plist-get info :minibuffer-height))))))))

(defun nicomment--comment-show (comment)
  (nicomment--comment-display
   comment :position (cons (nicomment--comment-x comment)
                           (nicomment--comment-y comment))))

(defun nicomment--comment-delete (comment)
  (posframe-delete (nicomment--comment-id comment)))

(defun nicomment--update-comments ()
  (mapc 'nicomment--comment-tick nicomment--comments)
  (setq nicomment--comments (loop for c in nicomment--comments
                                  if (nicomment--comment-deletable-p c) do (nicomment--comment-delete c)
                                  else collect c into showable
                                  finally return showable))
  (mapc 'nicomment--comment-show nicomment--comments))


(defun nicomment--mode-start ()
  (setq nicomment--comments nil)
  (setq nicomment--timer (run-at-time 1 0.010 #'nicomment--update-comments)))

(defun nicomment--mode-stop ()
  (cancel-timer nicomment--timer)
  (setq nicomment--comments nil)
  (setq nicomment--timer nil)
  (posframe-delete-all))

(define-minor-mode nicomment-mode
  "Toggle nicomment mode"
  :lighter " NICOMMENT"
  :global t
  (if nicomment-mode (nicomment--mode-start)
    (nicomment--mode-stop)))

(cl-defun nicomment-add-comment (message &key size color position)
  (when nicomment-mode
    (let ((comment (make-nicomment--comment :message message :size size :color color)))
      (case position
        (ue
         (nicomment--comment-start-at-top-center comment))
        (shita
         (nicomment--comment-start-at-bottom-center comment))
        (t
         (nicomment--comment-start-at-right comment)))
      (add-to-list 'nicomment--comments comment))))

;;; nicomment.el ends here
