"
" ~/.vimrc
"
"   Vim Configuration
"

" Environment variables. {{{
" Set environment variables.
let $XDG_CACHE_HOME = empty($XDG_CACHE_HOME) ? expand('$HOME/.cache') : $XDG_CACHE_HOME
let $XDG_CONFIG_HOME = empty($XDG_CONFIG_HOME) ? expand('$HOME/.config') : $XDG_CONFIG_HOME
let $XDG_DATA_HOME = empty($XDG_DATA_HOME) ? expand('$HOME/.local/share') : $XDG_DATA_HOME
if has('nvim')
  let $VIM_CACHE_HOME = $XDG_CACHE_HOME . '/nvim'
  let $VIM_CONFIG_HOME = $XDG_CONFIG_HOME . '/nvim'
else
  let $VIM_CACHE_HOME = $XDG_CACHE_HOME . '/vim'
  let $VIM_CONFIG_HOME = $XDG_CONFIG_HOME . '/vim'
endif
let $VIM_PLUGIN_HOME = $VIM_CONFIG_HOME . '/plugins'
let $VIM_DATA_HOME = $VIM_CONFIG_HOME . '/datas'

" Set up runtimepath.
set runtimepath+=$VIM_CONFIG_HOME
set runtimepath+=$VIM_PLUGIN_HOME
set runtimepath+=$VIM_DATA_HOME
" }}}

" Cursor {{{
let &t_SI .= "\e[6 q"
let &t_EI .= "\e[2 q"
let &t_SR .= "\e[4 q"
" }}}

" Basis {{{
" 編集中でもファイルを開けるように
set hidden

" 他で編集したファイルを自動で再読み込み
set autoread

" 補完時の挙動を指定
set completeopt=menuone,noselect

" スクロール時の余白行数
set scrolloff=5
set sidescrolloff=6

" バッファスクロール
set mouse=a

" バックアップを作成しない
set nobackup
set nowritebackup

" バックスペースでなんでも消せるように
set backspace=indent,eol,start

" ビープ無効
set visualbell t_vb=
set vb t_vb=
set belloff=all

" エラーメッセージ表示時にビープを鳴らさない
set noerrorbells

" 現在のディレクトリから開始
set browsedir=buffer

" カーソルを行をまたいで移動
"set whichwrap=b,s,h,l,<,>,[,],~

" Windowsでパスの区切り文字をスラッシュで扱う
set shellslash

" コマンドをステータスに表示
set showcmd

" viminfoの設定
set viminfo='50,<1000,s100,\"50

" モードラインを無効
"set modelines=0

" タイトルを変更させない
set notitle

" ヤンクでクリップボードを使用
set clipboard=unnamedplus

" コマンドモードで補完を使用
set wildmode=longest:full,full
set wildmenu
set wildignorecase

" スワップファイルを作らない
set noswapfile

" 折り返さない
set nowrap
" 折り返しを設定したとき、単語内で折り返さない
"set linebreak

" 列幅
"set columns

" 行幅
"set lines

" ルーラーを表示
set ruler

" 省略されずに表示
set display=lastline

" 不可視文字を表示
set list

" 不可視文字の設定
set listchars=tab:▹\ ,trail:-,extends:»,precedes:«,eol:\ ,nbsp:%

" 開始時の挨拶を表示しない
set shortmess+=I

" 検索ループ時のメッセージを表示しない
set shortmess+=s

" カレント行のハイライト
set cursorline

" 対応する括弧をハイライト表示する
set showmatch

" 括弧のハイライト表示の秒数を設定
set matchtime=3

" インデント方法の変更
set cinoptions+=:0

" メッセージ表示欄を2行確保
"set cmdheight=2
" メッセージ表示欄を1行確保
set cmdheight=1

" ステータス行を常に表示
set laststatus=2

" コマンドラインの履歴を10000件保存
set history=10000

" 行番号を表示
set number

" 行番号を相対値で表示
"set relativenumber

" スペルチェック(コメントも適用されるので、指定しない)
"set spell

" コマンド実行中は再描画しない
set lazyredraw

" tabの幅
set tabstop=2

" tabをスペースにする
set expandtab

" Cインデント
"set cindent
" スマートインデント
set smartindent
" オートインデント
"set autoindent

" 全ての数を10進数として扱う
set nrformats=

" キーボードから入力した場合のtabの幅
set softtabstop=2

" 自動で挿入/削除されるインデントの量
set shiftwidth=2

" ツールバーを非表示
set guioptions-=T

" yでコピーしたとき、クリップボードに入る
set guioptions+=ambiwidth

" メニューバーを非表示
set guioptions-=m

" 右スクロールバーを非表示
"set guioptions+=R

" 折りたたみ
set foldmethod=marker
set foldlevel=1
set foldcolumn=0

" 検索にマッチした行以外折りたたむ
set nofoldenable

" 末尾まで検索後、ファイル先頭にループさせる
set wrapscan

" 検索で大文字小文字を区別しない
set ignorecase

" 検索文字に大文字があるときは大文字小文字を区別する
set smartcase

" インクリメンタルサーチ
set incsearch

" 検索文字列をハイライト表示
set hlsearch

" ウィンドウサイズを均等にする
set equalalways
"set noequalalways

" 全角文字専用の設定
set ambiwidth=single
"set ambiwidth=double

" タグファイルを指定
set tags+=.tags,./.tags

" 補完時に1行まるごと補完
set showfulltag

" タグから補完リストに追加
set wildoptions=tagfile

" 文字コード関係
set encoding=utf-8
set fileencoding=utf-8
set fileformat=unix
set fileencodings=utf-8,cp932,euc-jp,iso-20220-jp,default,latin,sjis
set fileformats=unix,dos,mac

" undoを保存するファイルを生成
set undofile
" undoを保存するディレクトリを指定
set undodir=$HOME/.cache/vim/undodir

" vimの矩形選択で文字が無くても右へ進める
set virtualedit=block
" }}}

" Functions {{{
" 背景を透明にする
augroup AutoColorscheme
  autocmd!
  autocmd Colorscheme * highlight Normal ctermbg=NONE guibg=NONE
  autocmd Colorscheme * highlight NonText ctermbg=NONE guibg=NONE
  autocmd Colorscheme * highlight LineNr ctermbg=NONE guibg=NONE
  autocmd Colorscheme * highlight Folded ctermbg=NONE guibg=NONE
  autocmd Colorscheme * highlight EndOfBuffer ctermbg=NONE guibg=NONE
augroup END
" }}}

" Keybindings {{{
" Escを2回押してハイライト消去
nnoremap <Esc><Esc> :nohlsearch<CR><ESC>
" }}}

" NeoBundle (Plugin Manager) {{{
" Note: Skip initialization for vim-tiny or vim-small.
if 0 | endif

if &compatible
  set nocompatible               " Be iMproved
endif

" NeoBundle installation directory
let s:neobundle_cache_home = $VIM_CACHE_HOME . '/bundle'
let s:neobundle_repos_home = s:neobundle_cache_home . '/neobundle.vim'
execute 'set runtimepath+=' . s:neobundle_cache_home
" Install NeoBundle
if !isdirectory(s:neobundle_repos_home)
  echo "Install NeoBundle ..."
  execute '!git clone https://github.com/Shougo/neobundle.vim' s:neobundle_repos_home
endif
"let g:neobundle_default_git_protocol='https'

" Required:
execute 'set runtimepath+=' . s:neobundle_repos_home

" Required:
call neobundle#begin(s:neobundle_cache_home)

" Let NeoBundle manage NeoBundle
" Required:
NeoBundleFetch 'Shougo/neobundle.vim'

" My Bundles here:
" Refer to |:NeoBundle-examples|.
" Note: You don't set neobundle setting in .gvimrc!
"NeoBundle 'itchyny/lightline.vim'
"NeoBundle 'morhetz/gruvbox'

call neobundle#end()

" Required:
filetype plugin indent on

" If there are uninstalled bundles found on startup,
" this will conveniently prompt you to install them.
"NeoBundleCheck
" }}}

" True Color
set t_Co=256
" Syntax highlight
syntax enable
" Color scheme
set background=dark
colorscheme darkblue
"colorscheme gruvbox

