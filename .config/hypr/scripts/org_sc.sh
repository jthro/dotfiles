#!/bin/bash

save_dir="$HOME/org-roam/_media/"

filename=$(wofi --dmenu --prompt "Filename for screenshot:")

[ -z "$filename" ] && exit 1

grim -g "$(slurp)" "$save_dir/$filename.png"
