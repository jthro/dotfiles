#!/bin/bash

key="$1"
movetows="$2"

context=$(cat ~/.config/hypr/current_context 2>/dev/null)

[ -z "$ctx" ] && ctx="default"

if [ -n "$movetows" ]; then
    hyprctl dispatch movetoworkspace name:"${context}${key}"
else
    hyprctl dispatch workspace name:"${context}${key}"
fi
