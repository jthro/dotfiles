#!/bin/bash

save_dir="$HOME/org-roam/_media/"

filename=$(rofi -dmenu -p "Filename for screenshot:")

[ -z "$filename" ] && exit 1

grim -g "$(slurp)" "$save_dir/$filename.png"

link="#+attr_org: :width 400
[[file:../../_media/$filename.png]]"

echo -n "$link" | wl-copy
