;; ~/.emacs.d/init.el

;;; ----------------------------
;;; ローカルファイル読み込み
;;; ----------------------------
(if (file-exists-p "~/.local/dotfiles/init.el")
    (progn
      (print "Load local file")
      (load "~/.local/dotfiles/init.el")))

;;; ----------------------------
;;; General
;;; ----------------------------
;; yes or no -> y or n
(defalias 'yes-or-no-p 'y-or-n-p)

;; バックアップファイルをまとめる
(setq backup-directory-alist '((".*" . "~/.emacs.d/backups")))
;; 番号付けによる複数保存
(setq version-control     t)  ;; 実行の有無
(setq kept-new-versions   5)  ;; 最新の保持数
(setq kept-old-versions   1)  ;; 最古の保持数
(setq delete-old-versions t)  ;; 範囲外を削除

;; 自動保存ファイルをまとめる
(setq auto-save-file-name-transforms   '((".*" "~/.emacs.d/temp" t)))
;; 保存の間隔
(setq auto-save-timeout 10)     ;; 秒   (デフォルト : 30)
(setq auto-save-interval 100)   ;; 打鍵 (デフォルト : 300)

;; カスタムファイルの指定
(setq custom-file "~/.emacs.d/custom.el")
(if (file-exists-p (expand-file-name custom-file))
    (load-file (expand-file-name custom-file)))

;; 起動時の画面を表示しない
(setq inhibit-startup-message t)

;; インデントTAB文字を使わない（空白文字を使う）
(setq-default indent-tabs-mode nil)

;; C-kで改行文字も削除
(setq kill-whole-line t)

;; 保存時に自動で行末にある無駄な空白を削除
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; iswitchbを使う（C-xbによるバッファ選択が進化）
;;(iswitchb-mode 1)
;; iswitchbの際に無視するバッファリスト
;;(setq iswitchb-buffer-ignore (append iswitchb-buffer-ignore '("*Messages*" "*scratch*" "*Completions*" "*Kill Ring*")))

;; C-aで行の本当に先頭ではなく，行の非空白文字の先頭へ
(defun back-to-indentation-or-beginning ()
  (interactive)
  (if (= (point) (save-excursion (back-to-indentation) (point)))
    (beginning-of-line) (back-to-indentation)))
(global-set-key "\C-a" 'back-to-indentation-or-beginning)

;; 自動カッコ入力
(electric-pair-mode 1)

;; クリップボード使用
(setq x-select-enable-clipboard t)

;; display line numbers
(if (version<= "26.0.50" emacs-version)
    (progn
      (global-display-line-numbers-mode)
      (defun display-line-numbers-color-on-after-init (frame)
	"Hook function executed after FRAME is generated."
        (unless (display-graphic-p frame)
          (set-face-background
           'line-number
           (plist-get base16-solarized-dark-colors :base01))))
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (display-line-numbers-color-on-after-init frame)))
      )
  (progn
    (global-linum-mode t)
    (setq linum-format "%4d|")))



;;; ----------------------------
;;; Package
;;; ----------------------------
(require 'package)
;; インストールするパッケージ
(setq package-list
      '(zenburn-theme
	rainbow-delimiters
        ;;highlight-indent-guides
        auto-complete
        neotree
        all-the-icons
        yatex
        ))
;; package-archivesを上書き
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ;;("melpa-stable" . "https://stable.melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("milk" . "http://melpa.milkbox.net/")))
;; 初期化
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


;; zenburn Theme
(load-theme 'zenburn t)
(if (null window-system)
  (set-face-background 'default "unspecified-bg")
)

;;; rainbow-delimiters
(require 'rainbow-delimiters)
;;(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; 抵抗のカラーコード風
(require 'cl-lib)
(require 'color)

(rainbow-delimiters-mode 1)
(setq rainbow-delimiters-outermost-only-face-count 1)

(set-face-foreground 'rainbow-delimiters-depth-1-face "#9a4040")
(set-face-foreground 'rainbow-delimiters-depth-2-face "#ff5e5e")
(set-face-foreground 'rainbow-delimiters-depth-3-face "#ffaa77")
(set-face-foreground 'rainbow-delimiters-depth-4-face "#dddd77")
(set-face-foreground 'rainbow-delimiters-depth-5-face "#80ee80")
(set-face-foreground 'rainbow-delimiters-depth-6-face "#66bbff")
(set-face-foreground 'rainbow-delimiters-depth-7-face "#da6bda")
(set-face-foreground 'rainbow-delimiters-depth-8-face "#afafaf")
(set-face-foreground 'rainbow-delimiters-depth-9-face "#f0f0f0")

;; prog-mode で有効にする
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;; インデントを強調表示する
;;(require 'highlight-indent-guides)
;;(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
;;(add-hook 'yaml-mode-hook 'highlight-indent-guides-mode)
;;(add-hook 'text-mode-hook 'highlight-indent-guides-mode)

;;(setq highlight-indent-guides-auto-enabled nil)
;;(setq highlight-indent-guides-method 'character)
;;(setq highlight-indent-guides-responsive t)
;;(setq highlight-indent-guides-character ?\|)


;;auto-complete
(require 'auto-complete)
;; (add-to-list 'load-path "~/.emacs.d/popup-el")
;; (load "popup")
;; (add-to-list 'load-path "~/.emacs.d/auto-complete")
;; (load "auto-complete")
;; (require 'auto-complete-config)
(ac-config-default)
(ac-set-trigger-key "TAB")
(setq ac-use-menu-map t)
(setq ac-delay 0.05)
(setq ac-auto-show-menu 0.05)

;;; インテリセンス
;; (require 'company)
;; (global-company-mode) ; 全バッファで有効にする
;; (setq company-transformers '(company-sort-by-backend-importance)) ;; ソート順
;; (setq company-idle-delay 0) ; デフォルトは0.5
;; (setq company-minimum-prefix-length 3) ; デフォルトは4
;; (setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る
;; (setq completion-ignore-case t)
;; (setq company-dabbrev-downcase nil)
;; (global-set-key (kbd "C-M-i") 'company-complete)
;; (define-key company-active-map (kbd "C-n") 'company-select-next) ;; C-n, C-pで補完候補を次/前の候補を選択
;; (define-key company-active-map (kbd "C-p") 'company-select-previous)
;; (define-key company-search-map (kbd "C-n") 'company-select-next)
;; (define-key company-search-map (kbd "C-p") 'company-select-previous)
;; (define-key company-active-map (kbd "C-s") 'company-filter-candidates) ;; C-sで絞り込む
;; (define-key company-active-map (kbd "C-i") 'company-complete-selection) ;; TABで候補を設定
;; (define-key company-active-map [tab] 'company-complete-selection) ;; TABで候補を設定
;; (define-key company-active-map (kbd "C-f") 'company-complete-selection) ;; C-fで候補を設定
;; (define-key emacs-lisp-mode-map (kbd "C-M-i") 'company-complete) ;; 各種メジャーモードでも C-M-iで company-modeの補完を使う

;;; neotree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

;;; yatex
(require 'yatex)
(setq auto-mode-alist
      (append '(("\\.tex$" . yatex-mode)
                ("\\.ltx$" . yatex-mode)
                ("\\.cls$" . yatex-mode)
                ("\\.sty$" . yatex-mode)
                ("\\.clo$" . yatex-mode)
                ("\\.bbl$" . yatex-mode)) auto-mode-alist))
(setq YaTeX-kanji-code 4)
(setq tex-command "platex")
(setq YaTeX-dvipdf-command "dvipdfmx")
(add-hook 'yatex-mode-hook
          '(lambda ()
             (auto-fill-mode -1)))

;;;
;;; その他
;;;

;; 1行上下に移動
(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))
(global-set-key [(meta up)] 'move-line-up)
(global-set-key [(meta p)] 'move-line-up)


(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))
(global-set-key [(meta down)] 'move-line-down)
(global-set-key [(meta n)] 'move-line-down)
