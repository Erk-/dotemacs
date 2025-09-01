;;; lex-mode.el --- major mode for lex  -*- lexical-binding: t; -*-

(defvar lex-mode-abbrev-table nil
  "Abbreviation table used in `lex-mode' buffers.")

(define-abbrev-table 'lex-mode-abbrev-table
  '())

;;;###autoload
(define-derived-mode lex-mode fundamental-mode "lex"
  "Major mode for lex files."
  :abbrev-table lex-mode-abbrev-table
  (setq-local comment-start "#")
  (setq-local comment-start-skip "#+[\t ]*")
  (setq-local indent-tabs-mode t))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.lex" . lex-mode))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(lex-mode . ("/Users/erk/dev/ordnet/dannet_lsp/target/release/dannet_lsp"))))

(provide 'lex-mode)
