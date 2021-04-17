# fisher
if ! type fisher > /dev/null 2>&1
    curl -sL https://git.io/fisher | source && fisher install jorgebucaran/fisher && fisher update
end
