#!/bin/zsh

hyprctl dispatch workspace $1
hyprctl dispatch workspace $(( $1 + 10 ))
