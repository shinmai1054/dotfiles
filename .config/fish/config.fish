# config.fish

set -x EDITOR nano

alias l 'ls -a'
alias ll 'ls -ltr'
alias la 'ls -la'
alias ls 'ls -G -F'
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

function reload
  source ~/.config/fish/config.fish
end

# pyenv
# set PATH $HOME/.pyenv/shims $PATH
eval (pyenv init - | source)

# cd and ls
function cd
  builtin cd $argv; and pwd -P; and ll
end

# bobthefish
set -g theme_show_exit_status yes
set -g theme_title_display_process yes
set -g theme_date_format "+%m/%d %H:%M:%S"
#set -g fish_user_paths "/usr/local/opt/openssl@1.1/bin" $fish_user_paths

export LDFLAGS="-L/usr/local/opt/zlib/lib"
export CPPFLAGS="-I/usr/local/opt/zlib/include"

#set -g fish_user_paths "/usr/local/opt/bzip2/bin" $fish_user_paths

#export LDFLAGS="-L/usr/local/opt/bzip2/lib"
#export CPPFLAGS="-I/usr/local/opt/bzip2/include"
