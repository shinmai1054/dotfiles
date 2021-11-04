# linux.fish

alias ls 'ls -FX --color=auto'
alias i 'sudo apt install -y'
alias ug 'sudo apt update && sudo apt upgrade -y'
alias copy 'xsel -bi'

if type "batcat" > /dev/null 2>&1
    alias bat 'batcat'
end
if type "fdfind" > /dev/null 2>&1
alias fd 'fdfind'
end
