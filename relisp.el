
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
  (let ((glyphs (relisp-glyphs-at-cursor)))
    (prin1 glyphs)
    (when (not (equal "(" (substring glyphs 1 1)))
      (goto-char (search-backward "("))))
  (mark-sexp))

;;; for debugging. remove when done.
(local-s (kbd "C-' C-s") 'relisp-select-sexp)

(defun relisp-get-region-beginning ()
  (save-excursion
    (when (not relisp-region-active)
      (relisp-select-sexp))
    (region-beginning)))

(defun relisp-get-region-end ()
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

  ;; get "points/marks" for the current s-expression
  ;; get string for this region
  ;; kill this region
  ;; navigate to the "top" codeblock
  ;; insert (defun function-name () "Insert documentation here"
  ;; insert body
  ;; insert )

  (let* ((start         (region-beginning))
         (end           (region-end))
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

;; (defun relisp-elisp-mode-hook ()
;;   (local-set-key (kbd "C-'" 'relisp-mode-activate)))

;; (add-hook 'elisp-mode-hook 'relisp-elisp-mode-hook)
