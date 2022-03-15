#!/usr/bin/env fish


function attach_tmux
    # TMUX
    if status is-interactive
        if not type "tmux" > /dev/null 2>&1
            echo 'Error: tmux command not found' 2>&1
        else if test -n "$TMUX"
            echo "TMUX"
        else if tmux has-session >/dev/null 2>&1; and tmux list-sessions | grep -qv 'attached'
            set create_new_session "Create New Session"
            set slist $create_new_session\n(tmux list-sessions | grep -v 'attached')

            set reply (echo $slist | fzf)
            set num (echo $reply | cut -d: -f1)

            if test $reply = $create_new_session
                exec tmux new-session
            else if test (string match -r '[0-9]+' $num)
                exec tmux attach -t "$num"
            else
                return 1
            end
        else
            exec tmux new-session
        end
    end
end

alias t attach_tmux
