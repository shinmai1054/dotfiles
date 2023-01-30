#!/usr/bin/env fish

# fisher
if not type "fisher" > /dev/null 2>&1
    set packages jorgebucaran/fisher jethrokuan/z jethrokuan/fzf jorgebucaran/autopair.fish dracula/fish jorgebucaran/hydro
    curl -sL https://git.io/fisher | source && fisher install $packages
end
