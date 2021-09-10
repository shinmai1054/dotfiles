export LDFLAGS="-L/usr/local/opt/zlib/lib"
export CPPFLAGS="-I/usr/local/opt/zlib/include"

#export LDFLAGS="-L/usr/local/opt/bzip2/lib"
#export CPPFLAGS="-I/usr/local/opt/bzip2/include"

fish_add_path "$HOME/.nodebrew/current/bin"

fish_add_path ~/bin

alias sleepon='sudo pmset -a disablesleep 0'
alias sleepoff='sudo pmset -a disablesleep 1'
