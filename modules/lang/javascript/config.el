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

(use-package! typescript-ts-mode
  :defer t
  :hook (typescript-ts-mode . rainbow-delimiters-mode)
  :config
  (set-repl-handler! 'typescript-ts-mode #'+javascript/open-repl)
  (set-electric! 'typescript-ts-mode :chars '(?\} ?\) ?. ?:))
  )

(use-package! tsx-ts-mode
  :defer t
  :hook (tsx-ts-mode . rainbow-delimiters-mode)
  :config
  (set-repl-handler! 'tsx-ts-mode #'+javascript/open-repl)
  (set-electric! 'tsx-ts-mode :chars '(?\} ?\) ?. ?:))
  )

(when (modulep! +lsp)
  (add-hook! '(typescript-ts-mode-hook tsx-ts-mode-hook) #'lsp!)

;;
;;; Tools

;;;###package skewer-mode
(map! :localleader
      (:after js2-mode
        :map js2-mode-map
        "S" #'+javascript/skewer-this-buffer
        :prefix ("s" . "skewer"))
      :prefix "s"
      (:after skewer-mode
        :map skewer-mode-map
        "E" #'skewer-eval-last-expression
        "e" #'skewer-eval-defun
        "f" #'skewer-load-buffer)

      (:after skewer-css
        :map skewer-css-mode-map
        "e" #'skewer-css-eval-current-declaration
        "r" #'skewer-css-eval-current-rule
        "b" #'skewer-css-eval-buffer
        "c" #'skewer-css-clear-all)

      (:after skewer-html
        :map skewer-html-mode-map
        "e" #'skewer-html-eval-tag))


(use-package! npm-mode
  :hook ((js-mode typescript-mode) . npm-mode)
  :config
  (map! :localleader
        (:map npm-mode-keymap
          "n" npm-mode-command-keymap)
        (:after js2-mode
          :map js2-mode-map
          :prefix ("n" . "npm"))))


;;
;;; Projects

(def-project-mode! +javascript-npm-mode
  :modes '(html-mode
           css-mode
           web-mode
           markdown-mode
           typescript-ts-mode
           tsx-ts-mode
           json-mode
           solidity-mode)
  :when (locate-dominating-file default-directory "package.json")
  :add-hooks '(+javascript-add-npm-path-h npm-mode))

(def-project-mode! +javascript-gulp-mode
  :when (locate-dominating-file default-directory "gulpfile.js"))
