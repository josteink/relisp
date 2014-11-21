
;; utility macros

(defmacro with-region (start end &rest body)
  `(save-restriction
     (narrow-to-region ,start ,end)
     (goto-char (point-min))
     ,@body))

(defmacro with-defun (&rest body)
  `(progn
     (beginning-of-defun)
     (mark-sexp)
     (with-region (region-beginning) (region-end)
                  ,@body)))

;; active s-exp detection

(defun relisp-char-at-point ()
  "Returns the character at the current point, or an empty string if at buffer-end."
  (let* ((start (point))
         (end   (min (point-max) (+ 1 start))))
    (buffer-substring-no-properties start end)))

(defun relisp-mark-sexp-dwim ()
  "Function which attempts to auto-select the \"current\" s-expression the user is working with."
  (interactive)
  (if (not (equal "(" (relisp-char-at-point)))
      (search-backward "("))
  (mark-sexp)
  ;; make it easier to select subsequent sibling s-expressions
  (exchange-point-and-mark))

;; region utility functions

(defun relisp-get-sexpr-beginning ()
  (save-excursion
    (when (not mark-active)
      (relisp-mark-sexp-dwim))
    (region-beginning)))

(defun relisp-get-sexpr-end ()
  (save-excursion
    (when (not mark-active)
      (relisp-mark-sexp-dwim))
    (region-end)))

(defun relisp-select-region (start end)
  "Sets the current region based on the provided bounderies."
  ;; implementation based on this post;
  ;; http://stackoverflow.com/questions/11689948/programatically-selecting-a-region
  (interactive)
  (goto-char start)
  (set-mark-command nil)
  (goto-char end)
  (setq deactivate-mark nil))

;; (defun relisp-rename-symbol (old-symbol new-symbol)
;;   "Renames a symbol within a buffer."
;;   )

(defun relisp-elisp-insert-function (name body)
  "Inserts a ELISP function definition an dbody."
  (let ((doc-start)
	(doc-end))
    (insert "(defun " name " ()" )
    (newline-and-indent)
    (setq doc-start (+ 1 (point)))
    (insert "\"TODO: Document function.\"")
    (setq doc-end (- (point) 1))
    (newline-and-indent)
    (insert body ")")
    (newline)
    (newline)
    (list doc-start doc-end)))

(defun relisp-elisp-invoke-function (name)
  "Inserts a ELISP function invocation."
  (insert "(" name ")"))

(defun relisp-extract-function ()
  "Extracts a function based on the currently active s-expression."
  (interactive)

  (let* ((start         (relisp-get-sexpr-beginning))
         (end           (relisp-get-sexpr-end))
         (function-body (buffer-substring-no-properties start end))
         (function-name (read-string "Function-name: "))
         (doc-points))
    (save-excursion
      ;; remove function to be extracted
      (kill-region start end)
      (relisp-elisp-invoke-function function-name)

      ;; we want to reindent the parts we modify/add. to make this easier
      ;; we narrow our buffer down to our current function only.
      (with-defun
       (setq doc-points
             (relisp-elisp-insert-function function-name function-body))
       (indent-region (point-min) (point-max) nil)))
    ;; highlight the doc-string so that it is ready for editing
    (relisp-select-region (car doc-points) (car (cdr doc-points)))))


;; minor-mode tweaks

(defvar relisp-mode-map
  (let* ((map (make-sparse-keymap)))
    ;;(define-key map (kbd "C-' ?") 'relisp-show-help)
    ;;(define-key map (kbd "C-' r") 'relisp-rename-symbol)
    ;;(define-key map (kbd "C-' i") 'relisp-inline-symbol)
    (define-key map (kbd "C-' C-x") 'relisp-extract-function)
    (define-key map (kbd "C-M-<SPC>") 'relisp-mark-sexp-dwim)
    map))

(define-minor-mode relisp-mode
  "Minor-mode which enables elisp refactorings."
  :init-value nil
  :lighter " Relisp"
  :keymap 'relisp-mode-map
  :group 'relisp
  )

(provide 'relisp)
