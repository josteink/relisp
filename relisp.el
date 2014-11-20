
;; region-activity detection
(setq relisp-region-active nil)

(defun relisp-region-active-on ()
  (setq relisp-region-active t))

(defun relisp-region-active-off ()
  (setq relisp-region-active nil))

(add-hook 'activate-mark-hook 'relisp-region-active-on)
(add-hook 'deactivate-mark-hook 'relisp-region-active-off)

;; active s-exp detection

(defun relisp-glyphs-at-cursor ()
  "Return the glyph at the current cursor-position.

  Based on the code found here for unit-at-cursor: http://ergoemacs.org/emacs/elisp_get-selection-or-unit.html"
  (let (p1 p2)
    (save-excursion
      (progn
        (skip-chars-backward "[:graph:]")
        (setq p1 (point))
        (skip-chars-forward "[:graph:]")
        (setq p2 (point))))
    (vector (buffer-substring-no-properties p1 p2) p1 p2 )) )

(defun relisp-select-sexp ()
  "Function which attempts to auto-select the \"current\" s-expression the user is working with."
  (interactive)
  (let* ((glyphs      (relisp-glyphs-at-cursor))
	 (first-glyph (substring (aref glyphs 0) 0 1)))
    (when (not (equal "(" first-glyph))
      (goto-char (search-backward "("))))
  (mark-sexp))

;; region utility methods

(defun relisp-get-sexpr-beginning ()
  (save-excursion
    (when (not relisp-region-active)
      (relisp-select-sexp))
    (region-beginning)))

(defun relisp-get-sexpr-end ()
  (save-excursion
    (when (not relisp-region-active)
      (relisp-select-sexp))
    (region-end)))


(defun relisp-rename-symbol (old-symbol new-symbol)
  "Renames a symbol within a buffer."
  )

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
      (insert "(defun " function-name "()" )
      (newline-and-indent)
      (insert function-body ")")
      (newline)
      (newline)
      (indent-whole-buffer))))

(defun relisp-initialize-debug ()
  (local-set-key (kbd "C-' C-x") 'relisp-extract-function)
  (local-set-key (kbd "C-' x") 'relisp-extract-function)
  (local-set-key (kbd "C-M-<SPC>") 'relisp-select-sexp))


;; setup as a minor-mode


;; (defvar relisp-mode-map (make-sparse-keymap))
;; (let* ((map relisp-mode-map))
;;   (define-key map (kbd "?") 'relisp-show-help)
;;   (define-key map (kbd "r") 'relisp-rename-symbol)
;;   (define-key map (kbd "x") 'relisp-extract-function)
;;   (define-key map (kbd "i") 'relisp-inline-symbol))

;; (define-minor-mode relisp-mode
;;   "Minor-mode which enables elisp refactorings."
;;   :init-value nil
;;   :lighter " Region"
;;   :keymap 'relisp-mode-map
;;   :group 'relisp
;;   )


;; (defun relisp-mode-activate ()
;;   (relisp-mode t))

(defun relisp-elisp-mode-hook ()
  (relisp-initialize-debug)
  ;;(local-set-key (kbd "C-'" 'relisp-mode-activate))
  )

(add-hook 'elisp-mode-hook 'relisp-elisp-mode-hook)


(provide 'relisp)
