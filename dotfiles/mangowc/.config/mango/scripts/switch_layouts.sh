#!/usr/bin/env bash

layout_file="/tmp/current-layout"
current_layout=$(cat "$layout_file" 2>/dev/null || echo "qwerty")

if [[ "$current_layout" == "qwerty" ]]; then
    mmsg -d "setoption,xkb_rules_variant,colemak,"
    echo "colemak" > /tmp/current-layout
    echo "Switched to Colemak"
else
    mmsg -d "setoption,xkb_rules_variant,"
    echo "qwerty" > /tmp/current-layout
    echo "Switched to Qwerty"
fi
