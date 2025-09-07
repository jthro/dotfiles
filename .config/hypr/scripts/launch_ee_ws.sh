#!/bin/bash

echo "ee" > ~/.config/hypr/current_context

hyprctl dispatch workspace name:ee1
firefox &
sleep 0.2

hyprctl dispatch workspace name:ee2
emacsclient -c &
sleep 0.2

hyprctl dispatch workspace name:ee3
kicad
