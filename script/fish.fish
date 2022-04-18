#!/usr/bin/env fish

# fisher
if not type "fisher" > /dev/null 2>&1
    set packages jorgebucaran/fisher oh-my-fish/theme-bobthefish jethrokuan/z jethrokuan/fzf jorgebucaran/autopair.fish
    curl -sL https://git.io/fisher | source && fisher install $packages
end
