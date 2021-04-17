# config.fish

set EDITOR emacs

if [ (uname) = 'Darwin' ]
   alias ls 'ls -G -F'
else if [ (uname) = 'Linux' ]
   alias ls 'ls -F --color=auto'
end

alias l 'ls -a'
alias ll 'ls -ltr'
alias la 'ls -la'
alias ltr 'ls -ltr'

alias grep 'grep --color'

alias mkdir 'mkdir -p'
alias rm 'rm -i'
alias cp 'cp -i'
alias mv 'mv -i'

alias cot 'open -a coteditor'
alias c 'clear'
alias h 'history'

alias cx 'chmod +x'
alias 'c-x' 'chmod -x'

alias cd.. 'cd ..'
alias .. 'cd ..'
alias ... 'cd ../..'
alias .... 'cd ../../..'
alias ..... 'cd ../../../..'

alias oc 'sudo openconnect --config=$HOME/openconnect/config v-conn.nagasaki-u.ac.jp'

function reload
  source ~/.config/fish/config.fish
end

# pyenv
if test -d ~/.pyenv/bin
   fish_add_path $HOME/.pyenv/bin
end

type -q pyenv; and eval (pyenv init - | source)

# cd and ls
function cd
  builtin cd $argv; and pwd -P; and ll
end

# bobthefish
set -g theme_show_exit_status yes
set -g theme_title_display_process yes
set -g theme_date_format "+%m/%d %H:%M:%S"

# fisher
if ! type fisher > /dev/null 2>&1
   curl -sL https://git.io/fisher | source && fisher install jorgebucaran/fisher
   fisher update
end

if test -f ~/.config/fish/local.fish
   source ~/.config/fish/local.fish
end

set -U fish_user_paths (echo $fish_user_paths | tr ' ' '\n' | sort -u)
