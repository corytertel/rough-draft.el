;;; package -- Summary
;;; Commentary:
;;; A package for editing suggestions.
;;; Code:

;; TODO
;; - cutting and pasting
;; - record video demo for good readme
;; - delete word, sentence, sexp, etc
;; - delete selection

(require 'cl-seq)

(defface rough-draft-insertion
  `((t (:foreground "forest green" :background "#cceecc")))
  "The face used for suggested inserts."
  :group 'rough-draft)

(defface rough-draft-deletion
  `((t (:foreground "#aa2222" :background "#ffdddd" :strike-through t)))
  "The face used for suggested deletes."
  :group 'rough-draft)

(defface rough-draft-approve-insertion
  `((t (:foreground "forest green" :background "#cceecc" :box (:line-width 2 :color "black"))))
  "The face used for suggested inserts."
  :group 'rough-draft)

(defface rough-draft-approve-deletion
  `((t (:foreground "#aa2222" :background "#ffdddd" :strike-through t :box (:line-width 2 :color "black"))))
  "The face used for suggested deletes."
  :group 'rough-draft)

(defvar rough-draft--insertion-overlays '())
(make-variable-buffer-local 'rough-draft--insertion-overlays)
(defvar rough-draft--deletion-overlays '())
(make-variable-buffer-local 'rough-draft--deletion-overlays)

(defvar rough-draft--last-yank nil)

;; FIXME remove recursion
(defun rough-draft--insert-into-sorted-list (lst ov)
  (cond ((not lst) (cons ov '()))
	((< (overlay-start ov) (overlay-start (car lst)))
	 (cons ov lst))
	(t (cons (car lst) (rough-draft--insert-into-sorted-list (cdr lst) ov)))))

;; FIXME remove recursion
(defun rough-draft--remove-from-list (lst point)
  (cond ((not lst) lst)
	((= point (overlay-start (car lst)))
	 (progn (delete-overlay (car lst))
		(cdr lst)))
	(t (cons (car lst) (rough-draft--remove-from-list (cdr lst) point)))))

(defun rough-draft--insert-insertion (ov)
  (setq rough-draft--insertion-overlays
	(rough-draft--insert-into-sorted-list rough-draft--insertion-overlays ov)))

(defun rough-draft--insert-deletion (ov)
  (setq rough-draft--deletion-overlays
	(rough-draft--insert-into-sorted-list rough-draft--deletion-overlays ov)))

(defun rough-draft--insertion-p (point)
  (named-let f ((lst rough-draft--insertion-overlays))
    (cond ((not lst) nil)
	  ((= point (overlay-start (car lst)))
	   (car lst))
	  (t (f (cdr lst))))))

(defun rough-draft--deletion-p (point)
  (named-let f ((lst rough-draft--deletion-overlays))
    (cond ((not lst) nil)
	  ((and (>= point (overlay-start (car lst))) (<= point (overlay-end (car lst))))
	   (car lst))
	  (t (f (cdr lst))))))

(defun rough-draft--remove-insertion (point)
  (setq rough-draft--insertion-overlays
	(rough-draft--remove-from-list rough-draft--insertion-overlays point)))

(defun rough-draft--remove-deletion (point)
  (setq rough-draft--deletion-overlays
	(rough-draft--remove-from-list rough-draft--deletion-overlays point)))

(defun rough-draft--overlay-text (ov)
  (cadr (member 'after-string (overlay-properties ov))))

(defun rough-draft-insert (str)
  (let* ((pt (point))
	 (cur-ov (rough-draft--insertion-p pt))
	 (text (concat (if cur-ov (rough-draft--overlay-text cur-ov) "") str)))
    (when cur-ov
      (rough-draft--remove-insertion pt))
    (let ((ov (make-overlay pt pt)))
      (overlay-put ov 'category 'rough-draft)
      (overlay-put ov 'after-string (propertize (substring-no-properties text) 'face 'rough-draft-insertion))
      (rough-draft--insert-insertion ov))))

(defun rough-draft-insert-char (&optional char count)
  (interactive (list last-command-event
		     (prefix-numeric-value current-prefix-arg)))
  (let ((char (or char last-command-event)))
    (rough-draft-insert (make-string (or count 1)
				     (cond ((= char ?\S-\ ) ?\s)
					   ((= char ?\C-m) ?\C-j)
					   (t char))))))

(defun rough-draft-yank-undo ()
  (interactive)
  (when rough-draft--last-yank
    (when-let* ((pt (point))
		(cur-ov (rough-draft--insertion-p pt))
		(text (substring-no-properties
		       (string-remove-suffix
			rough-draft--last-yank
			(rough-draft--overlay-text cur-ov)))))
      (rough-draft--remove-insertion pt)
      (if (length> text 0)
	  (let ((ov (make-overlay pt pt)))
	    (overlay-put ov 'category 'rough-draft)
	    (overlay-put ov 'after-string
			 (propertize text 'face 'rough-draft-insertion))
	    (rough-draft--insert-insertion ov))))
    (setq rough-draft--last-yank nil)))

;; slight modification of `insert-for-yank-1'
(defun rough-draft--insert-for-yank-1 (string)
  "Helper for `rough-draft--insert-for-yank', which see."
  (let* ((handler (and (stringp string)
		     (get-text-property 0 'yank-handler string)))
	 (param (or (nth 1 handler) string))
	 (opoint (point))
	 (inhibit-read-only inhibit-read-only)
	 end)

    ;; FIXME: This throws away any yank-undo-function set by previous calls
    ;; to insert-for-yank-1 within the loop of insert-for-yank!
    (setq yank-undo-function t)
    (if (nth 0 handler) ; FUNCTION
	(funcall (car handler) param)
      (rough-draft-insert param))
    (setq end (point))

    ;; Prevent read-only properties from interfering with the
    ;; following text property changes.
    (setq inhibit-read-only t)

    (unless (nth 2 handler) ; NOEXCLUDE
      (remove-yank-excluded-properties opoint end))

    ;; If last inserted char has properties, mark them as rear-nonsticky.
    (if (and (> end opoint)
	   (text-properties-at (1- end)))
	(put-text-property (1- end) end 'rear-nonsticky t))

    (if (eq yank-undo-function t)		   ; not set by FUNCTION
	(setq yank-undo-function (nth 3 handler))) ; UNDO
    (if (nth 4 handler)				   ; COMMAND
	(setq this-command (nth 4 handler)))))

;; slight modification of `insert-for-yank'
(defun rough-draft--insert-for-yank (string)
  ;; Allow altering the yank string.
  (run-hook-wrapped 'yank-transform-functions
                    (lambda (f) (setq string (funcall f string)) nil))
  (let (to)
    (while (setq to (next-single-property-change 0 'yank-handler string))
      (rough-draft--insert-for-yank-1 (substring string 0 to))
      (setq string (substring string to))))
  (rough-draft--insert-for-yank-1 string)
  (setq rough-draft--last-yank (substring-no-properties string)))

;; slight modification of `yank'
(defun rough-draft-yank (&optional arg)
  (interactive "*P")
  (setq yank-window-start (window-start))
  ;; If we don't get all the way thru, make last-command indicate that
  ;; for the following command.
  (setq this-command t)
  (push-mark)
  (rough-draft--insert-for-yank (current-kill (cond
					       ((listp arg) 0)
					       ((eq arg '-) -2)
					       (t (1- arg)))))
  (if (consp arg)
      ;; This is like exchange-point-and-mark, but doesn't activate the mark.
      ;; It is cleaner to avoid activation, even though the command
      ;; loop would deactivate the mark because we inserted text.
      (goto-char (prog1 (mark t)
		   (set-marker (mark-marker) (point) (current-buffer)))))
  ;; If we do get all the way thru, make this-command indicate that.
  (if (eq this-command t)
      (setq this-command 'yank))
  nil)

;; slight modification of `yank-from-kill-ring'
(defun rough-draft-yank-from-kill-ring (string &optional arg)
  (interactive (list (read-from-kill-ring "Yank from kill-ring: ")
                     current-prefix-arg))
  (setq yank-window-start (window-start))
  (push-mark)
  (rough-draft--insert-for-yank string)
  (when yank-from-kill-ring-rotate
    (let ((pos (seq-position kill-ring string)))
      (if pos
          (setq kill-ring-yank-pointer (nthcdr pos kill-ring))
        (kill-new string))))
  (if (consp arg)
      ;; Swap point and mark like in `yank' and `yank-pop'.
      (goto-char (prog1 (mark t)
                   (set-marker (mark-marker) (point) (current-buffer))))))

;; slight modification of `yank-pop'
;; FIXME doesn't work
(defun rough-draft-yank-pop (&optional arg)
  (interactive "p")
  (if (not (eq last-command 'rough-draft-yank))
      (rough-draft-yank-from-kill-ring (read-from-kill-ring "Yank from kill-ring: ")
				       current-prefix-arg)
    (setq this-command 'yank)
    (unless arg (setq arg 1))
    (let ((inhibit-read-only t)
          (before (< (point) (mark t))))
      ;; (if before
      ;;     (funcall (or yank-undo-function 'delete-region) (point) (mark t))
      ;;   (funcall (or yank-undo-function 'delete-region) (mark t) (point)))
      (rough-draft-yank-undo)
      (setq yank-undo-function nil)
      (set-marker (mark-marker) (point) (current-buffer))
      (rough-draft--insert-for-yank (current-kill arg))
      ;; Set the window start back where it was in the yank command,
      ;; if possible.
      (set-window-start (selected-window) yank-window-start t)
      (if before
          ;; This is like exchange-point-and-mark, but doesn't activate the mark.
          ;; It is cleaner to avoid activation, even though the command
          ;; loop would deactivate the mark because we inserted text.
          (goto-char (prog1 (mark t)
		       (set-marker (mark-marker) (point) (current-buffer))))))
    nil))

(defun rough-draft-backward-delete-char (arg)
  (interactive "p")
  (if (and mark-active (not (eq (mark) (point))))
      (progn (rough-draft-delete (- (mark) (point)))
	     (deactivate-mark))
    (rough-draft-delete (- (or arg 1)))))

(defun rough-draft-forward-delete-char (arg)
  (interactive "p")
  (if (and mark-active (not (eq (mark) (point))))
      (progn (rough-draft-delete (- (mark) (point)))
	     (deactivate-mark))
    (rough-draft-delete (or arg 1))))

(defun rough-draft-delete (arg)
  (let ((pt (point)))
    (if (< arg 0)
	;; If there exists an insertion behind the current point
	(if-let* ((ins-ov (rough-draft--insertion-p pt))
		  (str (rough-draft--overlay-text ins-ov)))
	    (if (= 1 (length str))
		(rough-draft--remove-insertion pt)
	      (let ((text (substring str 0 (- (length str) 1)))
		    (ov (make-overlay pt pt)))
		(rough-draft--remove-insertion pt)
		(overlay-put ov 'category 'rough-draft)
		(overlay-put ov 'after-string (propertize (substring-no-properties text) 'face 'rough-draft-insertion))
		(rough-draft--insert-insertion ov)))
	  (let ((cur-ov (rough-draft--deletion-p pt)))
	    (cond
	     ;; No overlay exists.
	     ((not cur-ov)
	      (let* ((next-ov (rough-draft--deletion-p (- pt 1)))
		     (ov (make-overlay (if next-ov (overlay-start next-ov) (- pt 1)) pt)))
		(when next-ov (rough-draft--remove-deletion (overlay-start next-ov)))
		(overlay-put ov 'category 'rough-draft)
		(overlay-put ov 'face 'rough-draft-deletion)
		(rough-draft--insert-deletion ov)
		(forward-char arg)))
	     ;; You are at the start of an overlay.
	     ((= pt (overlay-start cur-ov))
	      (let* ((next-ov (rough-draft--deletion-p (- pt 1)))
		     (ov (make-overlay (if next-ov (overlay-start next-ov) (- pt 1)) (overlay-end cur-ov))))
		;; There's another overlay next. Combine the two.
		(when next-ov (rough-draft--remove-deletion (overlay-start next-ov)))
		(rough-draft--remove-deletion pt)
		(overlay-put ov 'category 'rough-draft)
		(overlay-put ov 'face 'rough-draft-deletion)
		(rough-draft--insert-deletion ov)
		(forward-char arg)))
	     ;; You are at the end of an overlay. Shrink the overlay.
	     ((= pt (overlay-end cur-ov))
	      (progn (when (> (- pt 1) (overlay-start cur-ov))
		       (let ((ov (make-overlay (overlay-start cur-ov) (- pt 1))))
			 (overlay-put ov 'category 'rough-draft)
			 (overlay-put ov 'face 'rough-draft-deletion)
			 (rough-draft--insert-deletion ov)))
		     (rough-draft--remove-deletion (overlay-start cur-ov))
		     (forward-char arg)))
	     ;; You are in the middle of an overlay. Split the overlay.
	     (t
	      (let* ((start (overlay-start cur-ov))
		     (end (overlay-end cur-ov))
		     (ov1 (make-overlay pt end)))
		(rough-draft--remove-deletion start)
		(overlay-put ov1 'category 'rough-draft)
		(overlay-put ov1 'face 'rough-draft-deletion)
		(rough-draft--insert-deletion ov1)
		(if (< start (- pt 1))
		    (let ((ov2 (make-overlay start (- pt 1))))
		      (overlay-put ov2 'category 'rough-draft)
		      (overlay-put ov2 'face 'rough-draft-deletion)
		      (rough-draft--insert-deletion ov2)))
		(forward-char arg))))))
      (let ((cur-ov (rough-draft--deletion-p pt)))
	(cond
	 ;; No overlay exists.
	 ((not cur-ov)
	  (let* ((next-ov (rough-draft--deletion-p (+ pt 1)))
		 (ov (make-overlay pt (if next-ov (overlay-end next-ov) (+ pt 1)))))
	    (when next-ov (rough-draft--remove-deletion (+ pt 1)))
	    (overlay-put ov 'category 'rough-draft)
	    (overlay-put ov 'face 'rough-draft-deletion)
	    (rough-draft--insert-deletion ov)
	    (forward-char arg)))
	 ;; You are at the end of an overlay.
	 ((= pt (overlay-end cur-ov))
	  (let* ((next-ov (rough-draft--deletion-p (+ pt 1)))
		 (ov (make-overlay (overlay-start cur-ov) (if next-ov (overlay-end next-ov) (+ pt 1)))))
	    (when next-ov (rough-draft--remove-deletion (overlay-start next-ov)))
	    (rough-draft--remove-deletion (overlay-start cur-ov))
	    (overlay-put ov 'category 'rough-draft)
	    (overlay-put ov 'face 'rough-draft-deletion)
	    (rough-draft--insert-deletion ov)
	    (forward-char arg)))
	 ;; You are at the start of an overlay. Shrink the overlay.
	 ((= pt (overlay-start cur-ov))
	  (let ((end (overlay-end cur-ov)))
	    (rough-draft--remove-deletion (overlay-start cur-ov))
	    (when (< pt (- end 1))
	      (let ((ov (make-overlay (+ pt 1) end)))
		(overlay-put ov 'category 'rough-draft)
		(overlay-put ov 'face 'rough-draft-deletion)
		(rough-draft--insert-deletion ov)))
	    (forward-char arg)))
	 ;; You are in the middle of an overlay. Split the overlay.
	 (t
	  (let* ((start (overlay-start cur-ov))
		 (end (overlay-end cur-ov))
		 (ov1 (make-overlay start pt)))
	    (rough-draft--remove-deletion start)
	    (overlay-put ov1 'category 'rough-draft)
	    (overlay-put ov1 'face 'rough-draft-deletion)
	    (rough-draft--insert-deletion ov1)
	    (if (> end (+ pt 1))
		(let ((ov2 (make-overlay (+ pt 1) end)))
		  (overlay-put ov2 'category 'rough-draft)
		  (overlay-put ov2 'face 'rough-draft-deletion)
		  (rough-draft--insert-deletion ov2)))
	    (forward-char arg))))))))

(defun rough-draft-delete-word ()
  (interactive)
  )

(defun rough-draft--approve-insertion (ov)
  (goto-char (overlay-start ov))
  (overlay-put ov 'face 'rough-draft-approve-insertion)
  (when (yes-or-no-p "Insert selection?")
    (insert (rough-draft--overlay-text ov)))
  (rough-draft--remove-insertion (overlay-start ov)))

(defun rough-draft--approve-deletion (ov)
  (goto-char (overlay-start ov))
  (overlay-put ov 'face 'rough-draft-approve-deletion)
  (when (yes-or-no-p "Delete selection?")
    (delete-region (overlay-start ov) (overlay-end ov)))
  (rough-draft--remove-deletion (overlay-start ov)))

;; FIXME if an insertion is before a deletion, the insertion will get added to the deletion overlay and get deleted aswell.
;; (defun rough-draft-query-approve ()
;;   (interactive)
;;   (defun f (ins del)
;;     (cond ((not (or del ins)) nil)
;; 	  ((not del)
;; 	   (let ((ov (car ins)))
;; 	     (rough-draft--approve-insertion ov)
;; 	     (f (cdr ins) del)))
;; 	  ((not ins)
;; 	   (let ((ov (car del)))
;; 	     (rough-draft--approve-deletion ov)
;; 	     (f ins (cdr del))))
;; 	  ((<= (overlay-start (car ins)) (overlay-start (car del)))
;; 	   (let ((ov (car ins)))
;; 	     (rough-draft--approve-insertion ov)
;; 	     (f (cdr ins) del)))
;; 	  (t
;; 	   (let ((ov (car del)))
;; 	     (rough-draft--approve-deletion ov)
;; 	     (f ins (cdr del))))))
;;   (f rough-draft--insertion-overlays rough-draft--deletion-overlays))

(defun rough-draft-query-approve ()
  (interactive)
  (dolist (ov rough-draft--deletion-overlays)
    (rough-draft--approve-deletion ov))
  (dolist (ov rough-draft--insertion-overlays)
    (rough-draft--approve-insertion ov)))

(defun rough-draft-remove-suggestions ()
  (interactive)
  (dolist (ov rough-draft--insertion-overlays)
    (delete-overlay ov))
  (dolist (ov rough-draft--deletion-overlays)
    (delete-overlay ov))
  (setq rough-draft--insertion-overlays '())
  (setq rough-draft--deletion-overlays '()))

(define-minor-mode rough-draft-mode
  "Turns on an editing suggestion mode."
  :lighter "rough-draft"
  :keymap
  (let ((i 0)
	(map (make-keymap)))
    (or (char-table-p (nth 1 map))
       (error "The initialization of rough-draft-mode-map must be updated"))
    ;; Make all multibyte characters search for themselves.
    (set-char-table-range (nth 1 map) (cons #x100 (max-char))
			  #'rough-draft-insert-char)
    (setq i ?\s)
    (while (< i 256)
      (define-key map (vector i) #'rough-draft-insert-char)
      (setq i (1+ i)))
    (define-key map "\t" #'rough-draft-insert-char)
    (define-key map (kbd "RET") #'rough-draft-insert-char)
    (define-key map (kbd "C-j") #'rough-draft-insert-char)

    (define-key map (kbd "DEL") #'rough-draft-backward-delete-char)
    (define-key map (kbd "C-d") #'rough-draft-forward-delete-char)
    (define-key map (kbd "<delete>") #'rough-draft-forward-delete-char)

    ;; FIXME to abide by emacs standard
    (define-key map (kbd "C-v") #'rough-draft-yank)
    (define-key map (kbd "M-v") #'rough-draft-yank-pop)
    map))

(add-hook 'rough-draft-mode-hook #'rough-draft-query-approve)

(global-set-key (kbd "C-c r") #'rough-draft-mode)

hello world, goodbye cruel world
rrrtfrrrtfratasrtfastf


;;; rough-draft.el ends here
