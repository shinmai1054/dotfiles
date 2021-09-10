# config.fish

if type "code" > /dev/null 2>&1                                                                                                                                                                 09/10 22:20:35
   set EDITOR code
   alias v 'code .'
else
   set EDITOR emacs
end


if [ (uname) = 'Darwin' ]
   source ~/.config/fish/macos.fish
else if [ (uname) = 'Linux' ]
   source ~/.config/fish/linux.fish
end

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
alias p 'python3'
alias t 'tmux'

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

function es
   emacs /sudo::$argv
end

function reload
   source ~/.config/fish/config.fish
end

# pyenv
if test -d ~/.pyenv
   status is-interactive; and pyenv init --path | source
end

# cd and ls
function cd
   builtin cd $argv; and pwd -P; and ll
end

# bobthefish
set -g theme_show_exit_status yes
set -g theme_title_display_process yes
set -g theme_project_dir_length 1
set -g theme_newline_cursor yes
set -g theme_date_format "+%m/%d %H:%M:%S"


if test -f ~/.config/fish/local.fish
   source ~/.config/fish/local.fish
end

set -U fish_user_paths (echo $fish_user_paths | tr ' ' '\n' | sort -u)
