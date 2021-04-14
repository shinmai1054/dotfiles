;; ~/.emacs.d/init.el

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(display-time-mode t)
 '(package-selected-packages
   (quote
    (company-go go-mode highlight-indent-guides zenburn-theme rainbow-delimiters)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 105 :width normal)))))


(require 'package)

;; インストールするパッケージ
(setq package-list
      '(zenburn-theme
	rainbow-delimiters
        ;;highlight-indent-guides
        ))

;; package-archivesを上書き
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ;;("melpa-stable" . "https://stable.melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("gnu" . "https://elpa.gnu.org/packages/")))

;; 初期化
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


;; Theme
(load-theme 'zenburn t)
(if (null window-system)
  (set-face-background 'default "unspecified-bg")
)


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
      ))


;; rainbow-delimiters を使うための設定
(require 'rainbow-delimiters)
;(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; 括弧の色を強調する設定
;(require 'cl-lib)
;(require 'color)
;(defun rainbow-delimiters-using-stronger-colors ()
;  (interactive)
;  (cl-loop
;   for index from 1 to rainbow-delimiters-max-face-count
;   do
;   (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
;    (cl-callf color-saturate-name (face-foreground face) 30))))
;(add-hook 'emacs-startup-hook 'rainbow-delimiters-using-stronger-colors)

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


;; インデントを強調表示する
;;(require 'highlight-indent-guides)
;;(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
;;(add-hook 'yaml-mode-hook 'highlight-indent-guides-mode)
;;(add-hook 'text-mode-hook 'highlight-indent-guides-mode)

;;(setq highlight-indent-guides-auto-enabled nil)
;;(setq highlight-indent-guides-method 'character)
;;(setq highlight-indent-guides-responsive t)
;;(setq highlight-indent-guides-character ?\|)

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

;; line number
(global-linum-mode t)
(setq linum-format "%4d|")

;; kakko auto nyuryoku
(electric-pair-mode 1)

;; auto-complete
(add-to-list 'load-path "~/.emacs.d/popup-el")
(load "popup")
(add-to-list 'load-path "~/.emacs.d/auto-complete")
(load "auto-complete")
(require 'auto-complete-config)
(ac-config-default)
(ac-set-trigger-key "TAB")
(setq ac-use-menu-map t)
(setq ac-delay 0.05)
(setq ac-auto-show-menu 0.05)

;; yes or no -> y or n
(defalias 'yes-or-no-p 'y-or-n-p)

;;;
;;; General
;;;
(setq inhibit-startup-message t)
(mouse-wheel-mode)
(tool-bar-mode 0)
(if (not window-system) (menu-bar-mode 0))
;;;
;;; Anthy on Emacs
;;;
(setq default-input-method "japanese-egg-anthy")
(global-set-key "\C-o" 'toggle-input-method)

(prefer-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)
