;; 2015-01-24
;;  2015-08-26: resume work on v3
;; same as 'vt-textoids.el', but with glyph support
;; Real-world usage:
;; 1) eval-buffer on this buffer.
;; 2) split screen and go into 'vt-textoids-canvas' buffer (it will get created if it
;; doesn't exist
;; 3) Press C-c z .  'vt-textoid-canvas' does not need to have focus
;;
;; Note: press C-c x to kill a running game.
;; Note: this game assumes you have the color 'vt-face-blue-\#0000ff' et al, previously
;;  set, by issuing 'customize-faces'.  This should be done as part of your .emacs startup

(require 'gamegrid)
;; Note: fix this by having an override library, that will only override the methods
(require 'vt-gamegrid-override)

(global-set-key "\C-cz" 'vt-textoids-run-game)
(global-set-key "\C-cx" 'vt-textoids-kill-timer)

;; set global vars here
(defun vt-textoids-init ()
  (setq vt-textoids-bg-char ?C)
  (setq vt-textoids-bg-char ?\s)
  (setq vt-textoids-border-char ?D)
  (setq vt-textoids-canvas-width 30)
  (setq vt-textoids-canvas-height 20)
  (setq vt-textoids-draw-count 0)
  (setq vt-textoids-draw-throttle 20) 
  (setq vt-textoids-tick-period 1.0)
  (setq vt-textoids-brick-char ?B)
  (setq vt-textoids-brick-length 4)  
  ;; see also vt-textoids-init-ship
  (let ((i 0))
    (setq vt-textoids-bg-row "")
    (while (< i (- vt-textoids-canvas-width 1))       
      (setq vt-textoids-bg-row (cl-concatenate 'string vt-textoids-bg-row (string vt-textoids-bg-char)))
      (setq i (1+ i)))
    (setq vt-textoids-bg-row (cl-concatenate 'string vt-textoids-bg-row "\n"))
    )
)

(defun vt-textoids-init-canvas ()
  (let ((first-time-flag nil))
    (when (equal (length (window-list)) 1)
      (split-window-right)
      (setq first-time-flag t)
      ;; switch to game buffer
      ;;(other-window 1)
      )
    (setq vt-textoids-buf (get-buffer-create "vt-textoids-canvas"))

    (when (not (equal (current-buffer) vt-textoids-buf))
      (other-window 1))

    ;;(switch-to-buffer vt-textoids-buf)
    ;;(set-buffer "vt-textoids-canvas")
    (set-buffer vt-textoids-buf)
    (erase-buffer)
    (let ((i 0))
      (while (< i vt-textoids-canvas-height)
        (insert vt-textoids-bg-row)
        (setq i (+ i 1))))
    (vt-textoids-init-canvas-color)
    ;;(switch-to-buffer "vt-textoids-canvas")
    ;; (when (not (null first-time-flag))
    ;;   (other-window 1)
    ;;   )
    (switch-to-buffer vt-textoids-buf))
  
)

(defun vt-textoids-init-canvas-color ()
  ;;(goto-char 30)
  ;;(let ((i vt-textoids-canvas-width))
  (cl-loop for i from 1 to (- vt-textoids-canvas-height 2) do
    ;; note: the following has to start at 2, otherwise get a big box to the right
    (cl-loop for j from 2 to (- vt-textoids-canvas-width 0) do
      (goto-char (+ (* i vt-textoids-canvas-width) j))
      (put-text-property (1- (point))
                     (point)
                     'face
                     "vt-face-black-#000000")
      )
  )
)

(defun vt-textoids-draw-border ()
  (set-buffer "vt-textoids-canvas")
  (let ((point 0))
    ;; upper line
    (let ((i 0))
      (while (< i vt-textoids-canvas-width)
        (setq point i)
        (goto-char point)
        (delete-char 1)
        (insert vt-textoids-border-char)
        (put-text-property (1- (point))
		       (point)
		       'face
               "vt-face-grey-#7f7f7f")
        (setq i (1+ i))))
    ;; the sides
    (cl-loop for i from 1 to (1- vt-textoids-canvas-height) do
             (+ 1 1)
      ;; ;; left side
      ;; (setq point (1+ (* i vt-textoids-canvas-width)))
      ;; (goto-char point)
      ;; (delete-char 1)
      ;; (insert vt-textoids-border-char)
      ;; (put-text-property (1- (point))
      ;;                    (point)
      ;;                    'face
      ;;                    "vt-face-grey-#7f7f7f")
      ;; right side
      ;; (setq point (+ point (- vt-textoids-canvas-width 2)))
      ;; (goto-char point)
      ;; (delete-char 1)
      ;; (insert vt-textoids-border-char)
      ;; (put-text-property (1- (point))
      ;;                    (point)
      ;;                    'face
      ;;                    "vt-face-grey-#7f7f7f")
     )
    ;; lower line
    (let ((i 0))
      (while (< i (- vt-textoids-canvas-width 1))
        (setq point (+ (* vt-textoids-canvas-width (- vt-textoids-canvas-height 2)) i 1))
        (goto-char point)
        (delete-char 1)
        (insert vt-textoids-border-char)
        (put-text-property (1- (point))
                         (point)
                         'face
                         "vt-face-grey-#7f7f7f")
        (setq i (1+ i))))
)) 

;; (defun vt-textoids-init-bricks ()
;;   (setq vt-textoids-brick-char ?B)
;; )

(defun vt-textoids-draw-bricks ()
  (let ((i 0) )
    (while (< i 20)
      ;; no points in first two row or last row
      (setq point (+ (random (* (- vt-textoids-canvas-width 6) vt-textoids-canvas-height)) (* 2 vt-textoids-canvas-width)))
      
      ;;(setq point 130)
      
      (let ((i 0) (done nil))
        (while (and (< i vt-textoids-brick-length) (not done))
          ;; don't put bricks in last slot of row, or wrap bricks
          (if (equal  (% (+ point i) (- vt-textoids-canvas-width 0)) 0)
              (setq done t)
            (goto-char (+  point i))
            (delete-char 1)
            (insert vt-textoids-brick-char)
            (put-text-property (1- (point))
                               (point)
                               'face
                               "vt-face-red-\#ff0000"))
          (setq i (1+ i))
          ))
      (setq i (1+ i)))
    )
  )

;;(vt-textoids-init-canvas)

(defun vt-textoids-init-ship ()
  (setq vt-textoids-ship-char ?A)
  (setq vt-textoids-ship-x 1)
  (setq vt-textoids-ship-y (/ vt-textoids-canvas-width 2))
  ;;Note: x is horizontal, y is vertical
  (setq vt-textoids-ship-vx 1)
  (setq vt-textoids-ship-vy 0)
  (setq vt-textoids-ship-length 5)
  (setq vt-textoids-ship nil )
  (setq vt-textoids-ship-head 0)
  )

;; bump the ship history matrix, so each element of the snake gets passed
;; down the line
;; (defun vt-textoids-bump-ship ()
;;   (message "vt-textoids-bump-ship: entered")
;;   (let ((tail (aref vt-textoids-ship i)))
;;     (cl-loop for i from (1- vt-textoids-ship-length) downto 1 do 
;;              (aset vt-textoids-ship (1- i) (aref vt-textoids-ship i)) 
;;     )
;;     ;; return tail to caller so he can erase
;;     tail
;;     )
;; )
;; determine the next position for the ship
;; (defun vt-textoids-get-ship-pos ()
;;   (condition-case err
;; (message "vt-textoids-get-ship-pos: entered")
;;     (let (
;;           (cand-point (+ (* vt-textoids-ship-x vt-textoids-canvas-width) vt-textoids-ship-y)          )
;;          )
;;       (message "fuck")
;;       ;;(message "buffer-substring=%s" (buffer-substring  cand-pos cand-pos))
;;       (if (equal (buffer-substring-no-properties   cand-pos cand-pos) vt-textoids-brick-char)
;;           (message "found brick at %d" cand-point)
;;         )

;;       (message "vt-textoids-get-ship-pos: cand-point=%d" cand-point)
;;       cand-point
;;       )
;; (error (princ (format "vt-textoids-get-ship-pos: The error was: %s" err)))
;;     )
;;   )

;; return buffers point given an x and y pos
(defun vt-textoids-get-point (x y)
  (+ (* x vt-textoids-canvas-width) y)
  )

;; return true if brick is found at point
(defun vt-textoids-brick-at-point(p)
  (let ((found nil))
    (when (string-equal (buffer-substring-no-properties  p (1+  p)) (char-to-string vt-textoids-brick-char))
        (setq found t)
      )
    found
  )
)
;; TODO: refactor it so cand-point is not a global
(defun vt-textoids-get-ship-pos ()
  ;; (condition-case err 
  (message "vt-textoids-get-ship-pos: entered")
  ;;(setq vt-textoids-cand-point (+ (* vt-textoids-ship-x vt-textoids-canvas-width) vt-textoids-ship-y))
  (setq vt-textoids-cand-point (vt-textoids-get-point vt-textoids-ship-x  vt-textoids-ship-y))
  ;;(message "fuck")
  (message "buffer-substring-no-properties=%s" (buffer-substring-no-properties  vt-textoids-cand-point (1+  vt-textoids-cand-point)))
  ;; (when (string-equal (buffer-substring-no-properties  vt-textoids-cand-point (1+ vt-textoids-cand-point) (char-to-string vt-textoids-brick-char)))
  ;; (message "found brick at %d" cand-point)
  ;; (when (string-equal "B" (char-to-string vt-textoids-brick-char))
  ;;   (message "fuck 3")
  ;;   )
  ;; (when (string-equal (buffer-substring-no-properties  vt-textoids-cand-point (1+  vt-textoids-cand-point)) (char-to-string vt-textoids-brick-char))
  ;;   (message "found brick at %d" vt-textoids-cand-point)
  (when (vt-textoids-brick-at-point vt-textoids-cand-point)
    (message "found brick at %d" vt-textoids-cand-point)
    ;; go back to prior point in ship
    (setq vt-textoids-cand-point (car vt-textoids-ship))
    ;; try a new cand point
    ;; (if (not (equal vt-textoids-ship-vx 0))
    ;;     (setq vt-textoids-ship-vx))
    ;; (cond (
    ;;   (not (equal vt-textoids-ship-vx 0)) (setq vt-textoids-ship-vx)(setq vt-textoids-ship-vy 1))

    ;; )
    ;; TODO: can probably go recursive here and call vt-textoids-get-ship-pos again
    (cond ((not (equal vt-textoids-ship-vx 0)) (setq vt-textoids-ship-vx 0)(setq vt-textoids-ship-vy (if (equal (random 2) 0) 1 -1)))
    ((not (equal vt-textoids-ship-vy 0)) (setq vt-textoids-ship-vx (if (equal (random 2) 0) 1 -1))(setq vt-textoids-ship-vy 0))
       (t "dont know"))    
    )
  
;;     )
  ;;vt-textoids-cand-point
  ;;  (error (princ (format "vt-textoids-get-ship-pos: The error was: %s" err)) 2)
  ;; ) 
)

(defun vt-textoids-update-game ()
  ;; we seem to have to set the buffer each time
  (set-buffer "vt-textoids-canvas")
  ;;(message "vt-textoids-update-game: entered, draw-cnt=%s" vt-textoids-draw-count)
  (let ((point 0) (tail nil))
    ;;(setq point (+ (* vt-textoids-ship-x vt-textoids-canvas-width) vt-textoids-ship-y)) 
    (vt-textoids-get-ship-pos)
    ;;(setq point (vt-textoids-get-ship-pos))
    (setq point vt-textoids-cand-point)

    ;; push onto ship history
    ;; (when (>= vt-textoids-ship-head (1-  vt-textoids-ship-length))
    ;;   (setq tail (vt-textoids-bump-ship)))
    (when (> (length vt-textoids-ship) vt-textoids-ship-length)
      (let ((rev-ship (reverse vt-textoids-ship)))
        (setq tail (car rev-ship))
        (setq vt-textoids-ship (reverse (cdr rev-ship)))
      )
    )

    (message "vt-textoids-update-game: tail=%s" tail )

    (condition-case err
        ;;(aset vt-textoids-ship vt-textoids-ship-head point)
        (setq vt-textoids-ship (cons point vt-textoids-ship))
      (error (princ (format "The error was: %s" err))
        2
    ))

    (setq vt-textoids-ship-head (% (1+ vt-textoids-ship-head) vt-textoids-ship-length))
    ;;(message "***vt-textoids-update-game: ship-head=%s" vt-textoids-ship-head)
    
    (goto-char point)
    (delete-char 1)
    (insert vt-textoids-ship-char)
    (put-text-property (1- (point))
                       (point)
                       'face
                       "vt-face-blue-\#0000ff")

    (setq vt-textoids-ship-x (+ vt-textoids-ship-x vt-textoids-ship-vx))
    (setq vt-textoids-ship-y (+ vt-textoids-ship-y vt-textoids-ship-vy))
    
    (setq vt-textoids-draw-count (+ vt-textoids-draw-count 1))

    ;; erase the tail, if any
    (when tail
      (goto-char tail)
      (delete-char 1)
      (insert vt-textoids-bg-char)
      (put-text-property (1- (point))
                         (point)
                         'face
                         "vt-face-black-\#000000"))
    
    (if (> vt-textoids-draw-count vt-textoids-draw-throttle)
        ;; Note: these do not reliably kill the timer.  use 'cancel-function-timers'
        ;; as below
;;      (gamegrid-kill-timer))
;;      (cancel-timer vt-textoids-timer)
      (progn
      (message "vt-textoids-update-game: about to kill timer")
      ;;(cancel-function-timers 'vt-textoids-update-game)
      (vt-textoids-kill-timer)
      )
      )
))

;; separate kill timer
;; Note: this has to be marked interactive
(defun vt-textoids-kill-timer ()
  (interactive)
  (message "vt-textoids-update-game: about to kill timer")
  (cancel-function-timers 'vt-textoids-update-game))

(defun vt-textoids-run-game ()
  (interactive)
  (vt-textoids-init)
  (vt-textoids-init-canvas)
  (vt-textoids-draw-border)
  ;;(vt-textoids-init-bricks)
  (vt-textoids-draw-bricks)
  (vt-textoids-init-ship)

  ;(vt-gamegrid-override-start-timer vt-textoids-tick-period 'vt-textoids-update-game)
  ;;(run-with-timer vt-textoids-tick-period vt-textoids-draw-throttle 'vt-textoids-update-game)
  (setq vt-textoids-timer (run-with-timer 0.5 0.5 'vt-textoids-update-game))
  ;; (let ((i 0))
  ;;   (while (< i 10)
  ;;     ;;(gamegrid-start-timer vt-textoids-tick-period 'vt-cascades-update-game)
  ;;     (vt-textoids-update-game)
  ;;     ;;(sleep-for 1)
  ;;     (setq vt-textoids-timer (run-with-timer 7 7 'vt-textoids-update-game))
  ;;     (setq i (+ i 1))
  ;;     ))
  ;;(gamegrid-kill-timer)
)

;;(vt-textoids-run-game)
