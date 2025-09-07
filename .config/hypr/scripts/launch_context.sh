#!/bin/bash

options="dev\nee\ndefault"
choice=$(echo -e "$options" | rofi -dmenu \
    -p "Context:" \
    -lines 3 \
    -theme-str 'window { height: 240; }')

if [ -n "$choice" ]; then
    script="$HOME/.config/hypr/scripts/launch_${choice}_ws.sh"
    echo $script
    echo [ -x "$script" ] && "$script"
fi
