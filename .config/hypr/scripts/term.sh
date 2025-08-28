#!/bin/bash
TEXT=$(hyprctl clients -j | jq '.[] | select(.workspace.id == 11) | select(.class = "emacs")')

hyprctl dispatch workspace 11

echo "$TEXT"
if [ -z "$TEXT" ]; then
    hyprctl dispatch exec "emacsclient -c -e \"(vterm)\""
fi
    
