(defun hotfix-kmacro-hack (&rest _)
  (interactive
   (list (intern (completing-read
                  "Insert kbd macro (name): "
                  obarray
                  (lambda (elt)
                    (and (fboundp elt)
                         (or (stringp (symbol-function elt))
                             (vectorp (symbol-function elt))
                             (kmacro-extract-lambda (symbol-function elt)))))
                  t))
         current-prefix-arg))
  nil)
(advice-add 'insert-kbd-macro :before #'hotfix-kmacro-hack)
