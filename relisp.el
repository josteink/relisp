
;; active s-exp detection

(defun relisp-char-at-point ()
  "Returns the character at the current point, or an empty string if at buffer-end."
  (let* ((start (point))
	 (end   (min (point-max) (+ 1 start))))
    (buffer-substring-no-properties start end)))

(defun relisp-select-sexp ()
  "Function which attempts to auto-select the \"current\" s-expression the user is working with."
  (interactive)
  (if (not (equal "(" (relisp-char-at-point)))
      (search-backward "("))
  (mark-sexp))

;; region utility method

(defun relisp-get-sexpr-beginning ()
  (save-excursion
    (when (not mark-active)
      (relisp-select-sexp))
    (region-beginning)))

(defun relisp-get-sexpr-end ()
  (save-excursion
    (when (not mark-active)
      (relisp-select-sexp))
    (region-end)))

;; (defun relisp-rename-symbol (old-symbol new-symbol)
;;   "Renames a symbol within a buffer."
;;   )

(defun relisp-extract-function ()
  "Extracts a function based on the currently active s-expression."
  (interactive)

  (let* ((start         (relisp-get-sexpr-beginning))
         (end           (relisp-get-sexpr-end))
         (function-body (buffer-substring-no-properties start end))
         (function-name (read-string "Function-name: ")))
    (save-excursion
      (kill-region start end)
      (insert "(" function-name ")")

      (beginning-of-defun)
      (mark-sexp)
      (let* ((f-start (region-beginning))
             (f-end   (region-end)))
        (save-restriction
          (narrow-to-region f-start f-end)
          (insert "(defun " function-name " ()" )
          (newline-and-indent)
          (insert function-body ")")
          (newline)
          (newline)
          (indent-region (point-min) (point-max) nil))))))

;; minor-mode tweaks

(defvar relisp-mode-map
  (let* ((map (make-sparse-keymap)))
    ;;(define-key map (kbd "C-' ?") 'relisp-show-help)
    ;;(define-key map (kbd "C-' r") 'relisp-rename-symbol)
    ;;(define-key map (kbd "C-' i") 'relisp-inline-symbol)
    (define-key map (kbd "C-' C-x") 'relisp-extract-function)
    (define-key map (kbd "C-M-<SPC>") 'relisp-select-sexp)
    map))

(define-minor-mode relisp-mode
  "Minor-mode which enables elisp refactorings."
  :init-value nil
  :lighter " Region"
  :keymap 'relisp-mode-map
  :group 'relisp
  )

(provide 'relisp)
