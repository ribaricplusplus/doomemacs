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

;; TODO: Use js-ts-mode for regular JavaScript and tsx-ts-mode for JSX

;; Make sure that treesitter grammar gets installed with set-tree-sitter! and that fallbacks are provided.

(use-package! typescript-ts-mode
  :defer t
  :hook (typescript-ts-mode . rainbow-delimiters-mode)
  :config
  (set-repl-handler! 'typescript-ts-mode #'+javascript/open-repl)
  (set-tree-sitter! 'typescript-mode 'typescript-ts-mode 'typescript)
  )

(use-package! tsx-ts-mode
  :defer t
  :hook (tsx-ts-mode . rainbow-delimiters-mode)
  :config
  (set-repl-handler! 'tsx-ts-mode #'+javascript/open-repl)
  )

(when (modulep! +lsp)
  (add-hook! '(typescript-ts-mode-hook tsx-ts-mode-hook js-ts-mode-hook) #'lsp!))

;;
;;; Tools

;; TODO: Configure support for npm mode
