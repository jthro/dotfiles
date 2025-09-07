#!/bin/bash

rm ~/.config/hypr/current_context

sleep 0.3

hyprctl dispatch workspace 1
