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
(setq VT-TEXTOIDS-CELL-DONE-FLAG -100)
(setq VT-TEXTOIDS-SNAKE-DONE-FLAG -200)

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
  ;; see also vt-textoids-init-snake
  (let ((i 0))
    (setq vt-textoids-bg-row "")
    (while (< i (- vt-textoids-canvas-width 1))       
      (setq vt-textoids-bg-row (cl-concatenate 'string vt-textoids-bg-row (string vt-textoids-bg-char)))
      (setq i (1+ i)))
    (setq vt-textoids-bg-row (cl-concatenate 'string vt-textoids-bg-row "\n"))
    )
)

(defun vt-textoids-init-snake ()
  (setq vt-textoids-snake-char ?A)
  ;; (setq vt-textoids-snake-x 1)
  ;; (setq vt-textoids-snake-y (/ vt-textoids-canvas-width 2))
  (setq vt-textoids-snake-x (/ vt-textoids-canvas-width 2))
  (setq vt-textoids-snake-y 1)
  ;;Note: x is horizontal, y is vertical...nope I fixed this
  (setq vt-textoids-snake-vx 0)
  (setq vt-textoids-snake-vy 1)
  (setq vt-textoids-snake-length 5)
  (setq vt-textoids-snake nil )
  (setq vt-textoids-snake-head 0)
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


;; bump the snake history matrix, so each element of the snake gets passed
;; down the line
;; (defun vt-textoids-bump-snake ()
;;   (message "vt-textoids-bump-snake: entered")
;;   (let ((tail (aref vt-textoids-snake i)))
;;     (cl-loop for i from (1- vt-textoids-snake-length) downto 1 do 
;;              (aset vt-textoids-snake (1- i) (aref vt-textoids-snake i)) 
;;     )
;;     ;; return tail to caller so he can erase
;;     tail
;;     )
;; )
;; determine the next position for the snake
;; (defun vt-textoids-get-snake-pos ()
;;   (condition-case err
;; (message "vt-textoids-get-snake-pos: entered")
;;     (let (
;;           (cand-point (+ (* vt-textoids-snake-x vt-textoids-canvas-width) vt-textoids-snake-y)          )
;;          )
;;       (message "fuck")
;;       ;;(message "buffer-substring=%s" (buffer-substring  cand-pos cand-pos))
;;       (if (equal (buffer-substring-no-properties   cand-pos cand-pos) vt-textoids-brick-char)
;;           (message "found brick at %d" cand-point)
;;         )

;;       (message "vt-textoids-get-snake-pos: cand-point=%d" cand-point)
;;       cand-point
;;       )
;; (error (princ (format "vt-textoids-get-snake-pos: The error was: %s" err)))
;;     )
;;   )

;; return the x-coord of point
(defun vt-textoids-get-point-x (point)
  (% point vt-textoids-canvas-width)
  )

;; return the y-coord of point
(defun vt-textoids-get-point-y (point)
  (/ point vt-textoids-canvas-width)
  )

;; return buffers point given an x and y pos
(defun vt-textoids-get-point (x y)
  ;;(+ (* x vt-textoids-canvas-width) y)
  (+ (* y vt-textoids-canvas-width) x)
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

(defun vt-textoids-has-brick (x y)
  (vt-textoids-brick-at-point (vt-textoids-get-point x y))
  )

;; TODO: refactor it so cand-point is not a global
(defun vt-textoids-get-snake-pos ()
  ;; (condition-case err 
  (message "vt-textoids-get-snake-pos: entered")
  ;;(setq vt-textoids-cand-point (+ (* vt-textoids-snake-x vt-textoids-canvas-width) vt-textoids-snake-y))
  (setq vt-textoids-cand-point (vt-textoids-get-point vt-textoids-snake-x  vt-textoids-snake-y))
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
    ;; go back to prior point in snake
    (setq vt-textoids-cand-point (car vt-textoids-snake))
    ;; try a new cand point
    ;; (if (not (equal vt-textoids-snake-vx 0))
    ;;     (setq vt-textoids-snake-vx))
    ;; (cond (
    ;;   (not (equal vt-textoids-snake-vx 0)) (setq vt-textoids-snake-vx)(setq vt-textoids-snake-vy 1))

    ;; )
    ;; TODO: can probably go recursive here and call vt-textoids-get-snake-pos again
    (cond ((not (equal vt-textoids-snake-vx 0)) (setq vt-textoids-snake-vx 0)(setq vt-textoids-snake-vy (if (equal (random 2) 0) 1 -1)))
    ((not (equal vt-textoids-snake-vy 0)) (setq vt-textoids-snake-vx (if (equal (random 2) 0) 1 -1))(setq vt-textoids-snake-vy 0))
       (t "dont know"))    
    )
  
;;     )
  ;;vt-textoids-cand-point
  ;;  (error (princ (format "vt-textoids-get-snake-pos: The error was: %s" err)) 2)
  ;; ) 
)

;; get the next cell position.  We try to move down first, then left/right (based on coin
;; toss), and then right/left (opposite direction of the prior attempt).  If all of these
;; fail we return a value of VT-TEXTOIDS-CELL-DONE-FLAG in the rc to indicate this cell can move
;; any more.  It is then up to the client to recurse over the remaing cells in the snake
;; until they too can no longer move.
(defun vt-textoids-get-next-cell-pos (cur-pos-x cur-pos-y vx vy &optional try-num)
;;(defun vt-textoids-get-next-cell-pos (cur-pos-x cur-pos-y vx vy try-num)
  (when (null try-num)
    (setq try-num 0)
    )
  ;;(message "vt-textoids-get-next-cell-pos: entered, try-num=%s" try-num)
  (message "vt-textoids-get-next-cell-pos: entered, cur-pos-x=%s,cur-pos-y=%s,vx=%s,vy=%s,try-num=%s" cur-pos-x cur-pos-y vx vy try-num)
  (message "vt-textoids-get-next-cell-pos: entered, has-brick=%s" (vt-textoids-has-brick (+ cur-pos-x vx) (+ cur-pos-y vy)))
  (let ((result nil))
    (catch 'get-next-cell-error
      (cond
       ((< try-num 0)
        ;;(message "vt-textoids-get-next-cell-pos: invalid try-num.  Returning" )
        (throw 'get-next-cell-error "vt-textoids-get-next-cell-pos: invalid try-num."))
       
       ((>= try-num 2)
        ;; cell is at the end of the line
        ;; this is the recursive "bottoming-out" condition
        (setq result (append '(0 0) (list  VT-TEXTOIDS-CELL-DONE-FLAG)))
        )

       ;; no brick found path
       ((not (vt-textoids-has-brick (+ cur-pos-x vx) (+ cur-pos-y vy)))
        ;;(setq result (+ cur-pos-x vx) (+ cur-pos-y vy))
        ;; return result with rc=0
        (setq result (list (+ cur-pos-x vx) (+ cur-pos-y vy) 0))
        )

       ;; brick found path.  Try left/right.
       ((= try-num 0)
        (message "try-num=0 path")
        (vt-textoids-get-next-cell-pos cur-pos-x cur-pos-y (if (equal (random 2) 0) 1 -1) 0 (1+ try-num)))
       
       ((= try-num 1)
        (vt-textoids-get-next-cell-pos cur-pos-x cur-pos-y (* vx -1) 0 (1+ try-num))
        )
       
      )

       ;; return our result
       result
       )
    )
)

(defun vt-textoids-update-game ()
  ;; call new function to debug
  (vt-textoids-get-next-cell-pos (vt-textoids-get-point-x (car (vt-snake))) (vt-textoids-get-point-y (car (vt-snake))) vt-textoids-snake-vx vt-textoids-snake-vy)
  ;; we seem to have to set the buffer each time
  (set-buffer "vt-textoids-canvas")
  ;;(message "vt-textoids-update-game: entered, draw-cnt=%s" vt-textoids-draw-count)
  (let ((point 0) (tail nil))
    ;;(setq point (+ (* vt-textoids-snake-x vt-textoids-canvas-width) vt-textoids-snake-y)) 
    (vt-textoids-get-snake-pos)
    ;;(setq point (vt-textoids-get-snake-pos))
    (setq point vt-textoids-cand-point)

    ;; push onto snake history
    ;; (when (>= vt-textoids-snake-head (1-  vt-textoids-snake-length))
    ;;   (setq tail (vt-textoids-bump-snake)))
    (when (> (length vt-textoids-snake) vt-textoids-snake-length)
      (let ((rev-snake (reverse vt-textoids-snake)))
        (setq tail (car rev-snake))
        (setq vt-textoids-snake (reverse (cdr rev-snake)))
      )
    )

    (message "vt-textoids-update-game: tail=%s" tail )

    (condition-case err
        ;;(aset vt-textoids-snake vt-textoids-snake-head point)
        (setq vt-textoids-snake (cons point vt-textoids-snake))
      (error (princ (format "The error was: %s" err))
        2
    ))

    (setq vt-textoids-snake-head (% (1+ vt-textoids-snake-head) vt-textoids-snake-length))
    ;;(message "***vt-textoids-update-game: snake-head=%s" vt-textoids-snake-head)
    
    (goto-char point)
    (delete-char 1)
    (insert vt-textoids-snake-char)
    (put-text-property (1- (point))
                       (point)
                       'face
                       "vt-face-blue-\#0000ff")

    (setq vt-textoids-snake-x (+ vt-textoids-snake-x vt-textoids-snake-vx))
    (setq vt-textoids-snake-y (+ vt-textoids-snake-y vt-textoids-snake-vy))
    
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
  (vt-textoids-init-snake)

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

;; this is just a dummy function to aid in unit testing
(defun vt-textoids-do-something ()
  7
  )
;;(vt-textoids-run-game)
