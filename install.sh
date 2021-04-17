#!/bin/bash

set -ue

DOTPATH="$HOME/.dotfiles"
BACKUP_PATH="$HOME/.dotbackup"

GITHUB_URL="https://github.com/machan1054/dotfiles.git"

has() {
    type "$1" > /dev/null 2>&1
    return $?
}

if [ ! -d $DOTPATH ]; then
    # git が使えるなら git
    if has "git"; then
        git clone "$GITHUB_URL" "$DOTPATH"

    # 使えない場合は curl か wget を使用する
    elif has "curl" || has "wget"; then
        tarball="https://github.com/machan1054/dotfiles/archive/master.tar.gz"

        # どっちかでダウンロードして，tar に流す
        if has "curl"; then
            curl -L "$tarball"

        elif has "wget"; then
            wget -O - "$tarball"

        fi | tar zxv

        # 解凍したら，DOTPATH に置く
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

cd "$DOTPATH"
if [ $? -ne 0 ]; then
    echo "not found: $DOTPATH"
    exit 2
fi

# バックアップとリンク
if [ ! -d $BACKUP_PATH ];then
    echo "$BACKUP_PATH not found. Auto Make it"
    mkdir "$BACKUP_PATH"
fi

#local script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
#local dotdir=$(dirname ${script_dir})
#if [[ "$HOME" != "$dotdir" ]];then
#for f in $dotdir/.??*; do
for f in .??*
do
    [[ `basename $f` == ".git" ]] && continue
    [[ `basename $f` == ".gitignore" ]] && continue

    echo "$f"
    if [[ -L "$HOME/`basename $f`" ]]; then
        rm -f "$HOME/`basename $f`"
    fi
    if [[ -e "$HOME/`basename $f`" ]]; then
        mv "$HOME/`basename $f`" "$HOME/.dotbackup"
    fi
    ln -snf "$DOTPATH/$f" "$HOME/$f"
done

echo "Install completed!"
