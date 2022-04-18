#!/bin/bash

set -ue

DOTPATH="$HOME/.dotfiles"
BACKUP_PATH="$DOTPATH/bak"

github_url="https://github.com/shinmai1054/dotfiles.git"
tar_url="https://github.com/machan1054/dotfiles/archive/master.tar.gz"

has() {
    type "$1" > /dev/null 2>&1
    return $?
}

if [ ! -d $DOTPATH ]; then
    # git が使えるなら git
    if has "git"; then
        git clone "$github_url" "$DOTPATH"

    # 使えない場合は curl か wget を使用する
    elif has "curl" || has "wget"; then
        if has "curl"; then
            curl -L "$tar_url"
        elif has "wget"; then
            wget -O - "$tar_url"
        fi | tar zxv
        mv -f dotfiles-master "$DOTPATH"
    else
        echo "curl or wget required"
        exit 1
    fi
else
    if has "git"; then
        git -C $DOTPATH pull
    fi
fi

cd $DOTPATH
for f in .??*; do
    [[ $(basename $f) == ".git" ]] && continue
    [[ $(basename $f) == ".gitignore" ]] && continue

    echo "$f"
    if [[ -L "$HOME/$(basename $f)" ]]; then
        rm -f "$HOME/$(basename $f)"
    fi
    if [[ -e "$HOME/$(basename $f)" ]]; then
        mkdir -p $BACKUP_PATH
        mv "$HOME/$(basename $f)" "$BACKUP_PATH"
    fi
    ln -snf "$DOTPATH/$f" "$HOME/$f"
done

echo "Install completed!"
