;;; lang/javascript/config.el -*- lexical-binding: t; -*-

(after! projectile
  (pushnew! projectile-project-root-files "package.json")
  (pushnew! projectile-globally-ignored-directories "node_modules" "flow-typed"))


;;
;;; Major modes

(dolist (feature '(typescript-ts-mode
                   tsx-ts-mode
                   (nodejs-repl-mode . nodejs-repl)))
  (let ((pkg  (or (cdr-safe feature) feature))
        (mode (or (car-safe feature) feature)))
    (with-eval-after-load pkg
      (set-docsets! mode "JavaScript"
        "AngularJS" "Backbone" "BackboneJS" "Bootstrap" "D3JS" "EmberJS" "Express"
        "ExtJS" "JQuery" "JQuery_Mobile" "JQuery_UI" "KnockoutJS" "Lo-Dash"
        "MarionetteJS" "MomentJS" "NodeJS" "PrototypeJS" "React" "RequireJS"
        "SailsJS" "UnderscoreJS" "VueJS" "ZeptoJS")
      (set-ligatures! mode
        ;; Functional
        :def "function"
        :lambda "() =>"
        :composition "compose"
        ;; Types
        :null "null"
        :true "true" :false "false"
        ;; Flow
        :not "!"
        :and "&&" :or "||"
        :for "for"
        :return "return"
        ;; Other
        :yield "import"))))

(defun +javascript-common-config (mode &optional ts-symbol)
  (let ((mode-vars-hook (intern (format "%s-local-vars-hook" mode))))
    (when (modulep! +lsp)
      (add-hook mode-vars-hook #'lsp! 'append))
    (set-repl-handler! mode #'+javascript/open-repl)
    (when ts-symbol
      (treesit-ensure-installed ts-symbol))
    ))

(use-package! js-mode
  :defer t
  :init
  (+javascript-common-config 'js-mode))

(use-package! js-ts-mode
  :defer t
  :when (modulep! +tree-sitter)
  :init
  (set-tree-sitter!
      'js-mode
      'js-ts-mode
    '((javascript :url "https://github.com/tree-sitter/tree-sitter-javascript"
       :rev "master"
       :source-dir "src")))
  (+javascript-common-config 'js-ts-mode))

(use-package! typescript-ts-mode
  :defer t
  :when (modulep! +tree-sitter)
  :init
  (cl-pushnew '(typescript
                "https://github.com/tree-sitter/tree-sitter-typescript"
                "master"
                "typescript/src"
                nil
                nil)
              treesit-language-source-alist :test #'eq :key #'car)
  (+javascript-common-config 'typescript-ts-mode 'typescript))

(use-package! tsx-ts-mode
  :defer t
  :when (modulep! +tree-sitter)
  :init
  ;; This hook is not defined automatically so we define it here.
  (message "Hello world from tsx")
  (cl-pushnew '(tsx
                "https://github.com/tree-sitter/tree-sitter-typescript"
                "master"
                "tsx/src"
                nil
                nil)
              treesit-language-source-alist :test #'eq :key #'car)
  (+javascript-common-config 'tsx-ts-mode 'tsx))
