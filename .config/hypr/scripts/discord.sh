#!/bin/bash
TEXT=$(hyprctl clients -j | jq '.[] | select(.workspace.id == 12) | select(.class = "discord")')

hyprctl dispatch workspace 12

if [ -z "$TEXT" ]; then
    hyprctl dispatch exec "vesktop"
fi
    
