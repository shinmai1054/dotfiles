# config.fish

alias l 'ls -a'
alias ll 'ls -l'
alias la 'ls -la'
alias ltr 'ls -ltr'

alias grep 'grep --color=auto'

alias mkdir 'mkdir -p'
alias rm 'rm -i'
alias cp 'cp -i'
alias mv 'mv -i'

alias c 'clear'
alias h 'history'

alias cx 'chmod +x'
alias 'c-x' 'chmod -x'

alias cd.. 'cd ..'
alias .. 'cd ..'
alias ... 'cd ../..'
alias .... 'cd ../../..'
alias ..... 'cd ../../../..'

alias b 'bat'
alias e 'emacs -nw'
alias o 'open'
alias py 'python'

alias g 'git'
alias ga 'git add'
alias gd 'git diff'
alias gs 'git status'
alias gps 'git push'
alias gpu 'git pull'
alias gb 'git branch'
alias gf 'git fetch'
alias gcm 'git commit'
alias gcl 'git clone'
alias emacs 'emacs -nw'
alias sudoedit 'SUDO_EDITOR=emacs sudo -e'
if [ (uname) = 'Darwin' ]
   source ~/.config/fish/macos.fish
else if [ (uname) = 'Linux' ]
   source ~/.config/fish/linux.fish
end

if type "code" > /dev/null 2>&1
   set EDITOR code
   alias v 'code .'
else
   set EDITOR 'emacs -nw'
end


function es
   emacs /sudo::$argv
end

function reload
   source ~/.config/fish/config.fish
end

if test -f ~/.config/fish/tmux.fish
   source ~/.config/fish/tmux.fish
end

# pyenv
if test -d ~/.pyenv
   status is-login; and pyenv init --path | source
   status is-interactive; and pyenv init - | source
end

# cd and ls
functions -c cd cd_fish
function cd
   cd_fish $argv; and pwd -P; and ll
end

# prompt color
set -g hydro_color_pwd blue
set -g hydro_color_git green
set -g hydro_color_prompt brpurple
set -g hydro_color_duration 6272a4
set -g hydro_multiline true
set -g hydro_fetch true

# fzf
set -g FZF_COMPLETE 2
set -g FZF_LEGACY_KEYBINDINGS 0
set -g FZF_DEFAULT_OPTS "--height 40% --layout=reverse --border --inline-info"

if type "fd" > /dev/null 2>&1
   set -g FZF_ALT_C_COMMAND 'fd --type d'
   set -g FZF_CD_COMMAND 'fd --type d'
   set -g FZF_CD_WITH_HIDDEN_COMMAND 'fd --type d --hidden'
   set -g FZF_OPEN_COMMAND 'fd --type f --hidden'
   set -g FZF_FIND_FILE_COMMAND 'fd --type f --hidden'
end

if type "bat" > /dev/null 2>&1
   set -g FZF_PREVIEW_FILE_CMD 'bat --color=always --style="numbers,changes"'
end

if test -f ~/.config/fish/local.fish
   source ~/.config/fish/local.fish
end
