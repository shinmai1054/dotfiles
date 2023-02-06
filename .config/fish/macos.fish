# macos.fish

if test -f /opt/homebrew/bin/brew
    /opt/homebrew/bin/brew shellenv | source
end

test -e {$HOME}/.iterm2_shell_integration.fish ; and source {$HOME}/.iterm2_shell_integration.fish

alias ls 'ls -GF'
alias f 'open -a Finder ./'
alias i 'brew install'
alias ug 'brew upgrade'
alias copy 'pbcopy'
alias kb 'open /System/Library/CoreServices/KeyboardSetupAssistant.app'

if type trash > /dev/null 2>&1
    alias rm 'trash -F'
end
