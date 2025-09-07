#!/bin/bash

echo "dev" > ~/.config/hypr/current_context

hyprctl dispatch workspace name:dev1
firefox &
sleep 0.2

hyprctl dispatch workspace name:dev2
emacsclient -c &
