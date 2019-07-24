;;; nicomment-demo.el --- Flow comment on your Emacs -*- lexical-binding:t -*-

;; This library is the test code of nicomment.el
;;
;; 1st. M-x load-library RET nicomment
;; 2nd. M-x nicomment-mode (to enable)
;; 3rd. Execute `nicomment-demo' that following code.
;; 4th. M-x nicomment-mode (to disable)

(defun nicomment-demo ()
  (interactive)
  (let ((messages '(
                    "偉業"
                    "わこつ"
                    "888888"
                    "www"
                    "初見"
                    "いきがい"
                    "神回"
                    "ん？"
                    "なぜ浮上した"
                    "かわいい"
                    "かっこいい"
                    "わかる"
                    "時報"
                    ))
        (colors '("red" "#00FF00" "#0000FF" nil nil nil nil nil)) ;; nil means foreground color current frame
        (sizes '(large small normal nil nil nil nil nil)) ;; nil means 'normal
        (positions '(ue shita naka nil nil nil nil nil)) ;; nil means 'naka
        )
    (dotimes (_i 30)
      (lexical-let ((at (random 20))
                    (msg (nth (random (length messages)) messages))
                    (color (nth (random (length colors)) colors))
                    (size (nth (random (length sizes)) sizes))
                    (position (nth (random (length positions)) positions))
                    )
        (run-at-time at nil (lambda () (nicomment-add-comment msg :size size :color color :position position)))))))

;;; nicomment-demo.el ends here
