(require 'font-lock)

(defvar flyspell-generic-check-word-predicate)

(defgroup markless nil
  "Markless settings")

(defgroup markless-faces nil
  "Faces used in Markless mode"
  :group 'markless
  :group 'faces)

(defmacro --markless-defface (name prop &optional doc)
  `(defface ,name
       '((t ,prop))
     ,(or doc "")
     :group 'markless-faces))

(--markless-defface markless-italic-face (:inherit italic))
(--markless-defface markless-bold-face (:inherit bold))
(--markless-defface markless-underline-face (:underline t))
(--markless-defface markless-strikethrough-face (:strike-through t))
(--markless-defface markless-literal-face (:inherit (fixed-pitch font-lock-constant-face)))
(--markless-defface markless-keyword-face (:inherit font-lock-type-face))
(--markless-defface markless-url-face (:inherit link))
(--markless-defface markless-comment-face (:inherit font-lock-comment-face))
(--markless-defface markless-warning-face (:inherit font-lock-warning-face))
(--markless-defface markless-error-face (:inherit font-lock-warning-face))
(--markless-defface markless-instruction-face (:inherit font-lock-function-name-face))
(--markless-defface markless-embed-face (:inherit shadow :slant normal :weight normal))
(--markless-defface markless-highlight-face (:inherit highlight))
(--markless-defface markless-markup-face (:inherit shadow))

(defconst markless-url-regex "[[:alpha:]][[:alnum:]+\\-.]*://[[:alnum:]$\\-_.+!*'()&,/:;=?@%#\\\\]+")

(defun markless-at-block-p (point)
  )

(defun markless-fontify-url (last)
  (when (re-search-forward markless-url-regex last t)
    (goto-char (1+ (match-end 0)))
    (let* ((start (match-beginning 0))
           (end (match-end 0))
           (props (list 'keymap markless-mode-mouse-map
                        'face 'markless-url-face
                        'mouse-face 'markless-highlight-face
                        'rear-nonsticky t
                        'font-lock-multiline t)))
      (add-text-properties start end props)
      t)))

(defun markless-fontify-sup/sub (last)
  (when (re-search-forward "[v^](.*?)" last t)
    (let* ((start (match-beginning 0))
           (end (match-end 0))
           (props `(display ((raise ,(if (eql ?v (char-after start)) -0.2 +0.2)) (height 0.8)))))
      (add-face-text-property start (+ start 2) 'markless-markup-face)
      (add-face-text-property (- end 1) end 'markless-markup-face)
      (add-text-properties (+ start 2) (- end 1) props)
      t)))

(defun markless-fontify-compound (last))

(setq markless-font-lock-keywords
      `(("\\(//\\)\\(.*?\\)\\(//\\)"
         (1 'markless-markup-face prepend)
         (2 'markless-italic-face append)
         (3 'markless-markup-face prepend))
        ("\\(\\*\\*\\)\\(.*?\\)\\(\\*\\*\\)"
         (1 'markless-markup-face prepend)
         (2 'markless-bold-face append)
         (3 'markless-markup-face prepend))
        ("\\(__\\)\\(.*?\\)\\(__\\)"
         (1 'markless-markup-face prepend)
         (2 'markless-underline-face append)
         (3 'markless-markup-face prepend))
        ("\\(<-\\)\\(.*?\\)\\(->\\)"
         (1 'markless-markup-face prepend)
         (2 'markless-strikethrough-face append)
         (3 'markless-markup-face prepend))
        ("\\(``\\)\\(.*?\\)\\(``\\)"
         (1 'markless-markup-face prepend)
         (2 'markless-literal-face append)
         (3 'markless-markup-face prepend))
        (markless-fontify-sup/sub)
        (markless-fontify-url)
        (markless-fontify-compound)))

(defun markless-syntactic-face (state)
  (let ((in-comment (nth 4 state)))
    (cond
      (in-comment 'markless-comment-face)
      (t nil))))

(defun markless-at-word-p ()
  (not (let ((faces (get-text-property (point) 'face)))
         (unless (listp faces) (setq faces (list faces)))
         (or (memq 'markless-url-face faces)
             (memq 'markless-literal-face faces)
             (memq 'markless-keyword-face faces)))))

(defun markless-follow-link-at-point ()
  (interactive)
  (if (thing-at-point-looking-at markless-url-regex)
      (let* ((url (message (match-string 0)))
             (struct (url-generic-parse-url url)))
        (if (url-fullness struct)
            (browse-url url)
            (let ((file (car (url-path-and-query struct))))
              (when (and file (< 0 (length file))) (find-file file)))))
      (user-error "Point is not at a link or URL")))

(defvar markless-mode-hook nil)

(defvar markless-mode-map
  (let ((map (make-keymap)))
    map))

(defvar markless-mode-mouse-map
  (let ((map (make-sparse-keymap)))
    (define-key map [follow-link] 'mouse-face)
    (define-key map [mouse-2] 'markless-follow-link-at-point)
    map))

(define-derived-mode markless-mode text-mode "Markless"
  "Major mode for Markless documents."
  (setq font-lock-defaults
        '(markless-font-lock-keywords
          nil nil nil nil
          (font-lock-multiline . t)))

  (setq-local flyspell-generic-check-word-predicate
              #'markless-at-word-p))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mess" . markless-mode))

(provide 'markless)
