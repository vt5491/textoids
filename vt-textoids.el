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

;; set this to nil to see pure ascii charactars (which is cool in its own way)
(setq vt-textoids-use-glyphs t)
(setq VT-TEXTOIDS-CELL-DONE-FLAG -100)
(setq VT-TEXTOIDS-SNAKE-DONE-FLAG -200)

(setq VT-TEXTOIDS-FACE-COLORS '(
  "vt-face-green-\#00ff00"
  "vt-face-grey-\#7f7f7f"
  "vt-face-blue-\#0000ff"
  "vt-face-orange-\#ffa500"
  "vt-face-white-\#ffffff"
  "vt-face-yellow-\#ffff00"
))

;; set global vars here
(defun vt-textoids-init ()
  (setq vt-textoids-bg-char ?C)
  (setq vt-textoids-bg-char ?\s)
  (setq vt-textoids-border-char ?D)
  (setq vt-textoids-canvas-width 30)
  (setq vt-textoids-canvas-height 20)
  (setq vt-textoids-draw-count 0)
  (setq vt-textoids-draw-throttle 50) 
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
  (setq vt-textoids-snake-x (/ vt-textoids-canvas-width 2))
  (setq vt-textoids-snake-y 1)
  (setq vt-textoids-snake-vx 0)
  (setq vt-textoids-snake-vy 1)
  (setq vt-textoids-snake-length 5)
  (setq vt-textoids-snake nil )
  (setq vt-textoids-snake-head 0)
  (setq vt-textoids-snake-color (vt-textoids-get-rnd-face-color))
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

    (set-buffer vt-textoids-buf)
    (erase-buffer)
    (let ((i 0))
      (while (< i vt-textoids-canvas-height)
        (insert vt-textoids-bg-row)
        (setq i (+ i 1))))
    (vt-textoids-init-canvas-color)

    (switch-to-buffer vt-textoids-buf))
    ;; "hide" the cursor by making it black.  Unfortunatley, this the cursor
    ;; for the entire frame not the buffer
    (set-cursor-color "#000000")
    ;; use a skinny cursor-type because sometimes the cursor is over
    ;; a char like a brick-char and then you see a "B" there
    (setq cursor-type 'bar)
)

(defun vt-textoids-init-canvas-color ()
  (cl-loop for i from 1 to (- vt-textoids-canvas-height 2) do
    ;; note: the following has to start at 2, otherwise get a big box to the right
    (cl-loop for j from 2 to (- vt-textoids-canvas-width 0) do
      (goto-char (+ (* i vt-textoids-canvas-width) j))
      (when vt-textoids-use-glyphs
        (put-text-property (1- (point))
                           (point)
                           'face
                           "vt-face-black-#000000"))
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
        (when vt-textoids-use-glyphs
          (put-text-property (1- (point))
                             (point)
                             'face
                             "vt-face-grey-#7f7f7f"))
        (setq i (1+ i))))
    ;; the sides
    (cl-loop for i from 1 to (- vt-textoids-canvas-height 2) do
             ;; (+ 1 1)                    
      ;; left side
      (setq point (1+ (* i vt-textoids-canvas-width)))
      (goto-char point)
      (delete-char 1)
      (insert vt-textoids-border-char)
      (when vt-textoids-use-glyphs
        (put-text-property (1- (point))
                           (point)
                           'face
                           "vt-face-grey-#7f7f7f"))
      ;;right side
      (setq point (+ point (- vt-textoids-canvas-width 2)))
      (goto-char point)
      (delete-char 1)
      (insert vt-textoids-border-char)
      (when vt-textoids-use-glyphs
        (put-text-property (1- (point))
                           (point)
                           'face
                           "vt-face-grey-#7f7f7f"))
     )
    ;; lower line
    (let ((i 0))
      (while (< i (- vt-textoids-canvas-width 1))
        (setq point (+ (* vt-textoids-canvas-width (- vt-textoids-canvas-height 2)) i 1))
        (goto-char point)
        (delete-char 1)
        (insert vt-textoids-border-char)
        (when vt-textoids-use-glyphs
          (put-text-property (1- (point))
                             (point)
                             'face
                             "vt-face-grey-#7f7f7f"
                             ;;"vt-face-black-\#000000"
                             ))
        (setq i (1+ i))))
)) 

(defun vt-textoids-draw-bricks ()
  (let ((i 0) )
    (while (< i 25)
      ;; no points in first two row or last row
      (setq point (+ (random (* (- vt-textoids-canvas-width 6) vt-textoids-canvas-height)) (* 2 vt-textoids-canvas-width)))
      
      (let ((i 0) (done nil))
        (while (and (< i vt-textoids-brick-length) (not done))
          ;; don't put bricks in last slot of row, or wrap bricks
          (if (equal  (% (+ point i) (- vt-textoids-canvas-width 0)) 0)
              (setq done t)
            (goto-char (+  point i))
            (delete-char 1)
            (insert vt-textoids-brick-char)
            (when vt-textoids-use-glyphs
              (put-text-property (1- (point))
                                 (point)
                                 'face
                                 "vt-face-red-\#ff0000")))
          (setq i (1+ i))
          ))
      (setq i (1+ i)))
    )
  )

;; return a random face color
(defun vt-textoids-get-rnd-face-color ()
  (nth (random (length VT-TEXTOIDS-FACE-COLORS)) VT-TEXTOIDS-FACE-COLORS)
)

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
  (+ (* y vt-textoids-canvas-width) x)
  )

;; return true if brick is found at point
;; In this case a "brick" is a logicial "brick".  Anything that obstructs the
;; flow of the snake is considered a "brick", including bricks, borders, and snake
;; cells themselves.  Yes, all the brick test functions should be renamed to
;; use "blockage" or "impediment" instead of brick.
(defun vt-textoids-brick-at-point(p)
  (let ((found nil))
    (when (or (string-equal (buffer-substring-no-properties  p (1+  p)) (char-to-string vt-textoids-brick-char)) (string-equal (buffer-substring-no-properties  p (1+  p)) (char-to-string vt-textoids-border-char)) (string-equal (buffer-substring-no-properties  p (1+  p)) (char-to-string vt-textoids-snake-char)))
        (setq found t)
      )
    found
  )
)

(defun vt-textoids-has-brick (x y)
  (vt-textoids-brick-at-point (vt-textoids-get-point x y))
  )

;; get the next cell position.  We try to move down first, then left/right (based on coin
;; toss), and then right/left (opposite direction of the prior attempt).  If all of these
;; fail we return a value of VT-TEXTOIDS-CELL-DONE-FLAG in the rc to indicate this cell cannot move
;; any more.  It is then up to the client to recurse over the remaing cells in the snake
;; until they too can no longer move.
(defun vt-textoids-get-next-cell-pos (cur-pos-x cur-pos-y vx vy &optional try-num)
  (when (null try-num)
    (setq try-num 0)
    )

  (let ((result nil))
    (catch 'get-next-cell-error
      (cond
       ((< try-num 0)
        (throw 'get-next-cell-error "vt-textoids-get-next-cell-pos: invalid try-num."))
       
       ((> try-num 2)
        ;; cell is at the end of the line
        ;; this is the recursive "bottoming-out" condition
        (setq result (append '(0 0) (list  VT-TEXTOIDS-CELL-DONE-FLAG)))
        )

       ;; no brick found path
       ((not (vt-textoids-has-brick (+ cur-pos-x vx) (+ cur-pos-y vy)))
        ;; return result with rc=0
        (setq result (list (+ cur-pos-x vx) (+ cur-pos-y vy) 0))
        )

       ;; brick found path and going down.  Try left/right random.
       ((and (= try-num 0) (= vy 1))
        (setq result (vt-textoids-get-next-cell-pos cur-pos-x cur-pos-y (if (equal (random 2) 0) -1 1) 0 (1+ try-num))))

       ;; brick found path, but were moving sideways.  Want to keep moving in same dir
       ((and (= try-num 0) (not (= vy 1)))
        (setq result (vt-textoids-get-next-cell-pos cur-pos-x cur-pos-y vx 0 (1+ try-num))))
       
       ((= try-num 1)
        (setq result (vt-textoids-get-next-cell-pos cur-pos-x cur-pos-y (* vx -1) 0 (1+ try-num)))
        )

       ;; if we get to here were out of movement options
       (t        
        ;; cell is at the end of the line
        ;; this is the recursive "bottoming-out" condition
        (setq result (append '(0 0) (list  VT-TEXTOIDS-CELL-DONE-FLAG)))
        )

      )

       ;; return our result
       result
       )
    )
)

(defun vt-textoids-update-game ()  
  ;; we seem to have to set the buffer each time
  (set-buffer "vt-textoids-canvas")
  
  (let ((point 0) (tail nil) (result nil))
    (setq result (vt-textoids-get-next-cell-pos vt-textoids-snake-x vt-textoids-snake-y vt-textoids-snake-vx vt-textoids-snake-vy))
    (cond
          ;; snake cell is out of room
          ((= (nth 2 result) VT-TEXTOIDS-CELL-DONE-FLAG)
           (vt-textoids-init-snake)
           (setq vt-textoids-draw-count 0)
           ;; check if we're totally done
           (when (vt-textoids-brick-at-point (vt-textoids-get-point vt-textoids-snake-x vt-textoids-snake-y))
             (message "game over")
             (vt-textoids-kill-timer)
   )
          )
          ;; snake cell still has places to go
          ((= (nth 2 result) 0)
            (setq vt-textoids-snake-x (nth 0 result))
            (setq vt-textoids-snake-y (nth 1 result)))
    )

    (setq point (vt-textoids-get-point vt-textoids-snake-x vt-textoids-snake-y))

    ;; push onto snake history
    (when (> (length vt-textoids-snake) vt-textoids-snake-length)
      (let ((rev-snake (reverse vt-textoids-snake)))
        (setq tail (car rev-snake))
        (setq vt-textoids-snake (reverse (cdr rev-snake)))
      )
    )
    
    (condition-case err        
        (setq vt-textoids-snake (cons point vt-textoids-snake))
      (error (princ (format "The error was: %s" err))
        2
    ))

    (setq vt-textoids-snake-head (% (1+ vt-textoids-snake-head) vt-textoids-snake-length))    
    (goto-char point)
    (delete-char 1)
    (insert vt-textoids-snake-char)
    (when vt-textoids-use-glyphs
      (put-text-property (1- (point))
                         (point)
                         'face                       
                         vt-textoids-snake-color
                         ))
    
    (setq vt-textoids-draw-count (+ vt-textoids-draw-count 1))

    ;; erase the tail, if any
    (when tail
      (goto-char tail)
      (delete-char 1)
      (insert vt-textoids-bg-char)
      (when vt-textoids-use-glyphs
        (put-text-property (1- (point))
                           (point)
                           'face
                           "vt-face-black-\#000000"                         
                           )))
    
    (if (> vt-textoids-draw-count vt-textoids-draw-throttle)
        ;; Note: these do not reliably kill the timer.  use 'cancel-function-timers'
        ;; as below

      (progn
        (message "vt-textoids-update-game: about to kill timer")        
        (vt-textoids-kill-timer)
      )
      )
))

;; separate kill timer
;; Note: this has to be marked interactive
(defun vt-textoids-kill-timer ()
  (interactive)
  (message "vt-textoids-update-game: about to kill timer")
  (cancel-function-timers 'vt-textoids-update-game)
  ;; make cursor visible again
  (set-cursor-color "#7f7f7f")
  ;; and the default type again
  (setq cursor-type 'box)
  )

(defun vt-textoids-run-game ()
  (interactive)
  (vt-textoids-init)
  (vt-textoids-init-canvas)
  (vt-textoids-draw-border)  
  (vt-textoids-draw-bricks)
  (vt-textoids-init-snake)

  (setq vt-textoids-timer (run-with-timer 0.15 0.15 'vt-textoids-update-game))
)

;; this is just a dummy function to aid in unit testing
(defun vt-textoids-do-something ()
  7
  )

;;(vt-textoids-run-game)
