#!/usr/bin/env bash

TARGET_APPS=(
    "\.exe$"
    "^steam_app_"
)

declare -A APPID_BY_MONITOR
CURRENT_LAYOUT="default"
LOCKFILE="/tmp/layout_switch.lock"

is_target_app() {
    local appid="$1"
    local target

    for target in "${TARGET_APPS[@]}"; do
        if [[ "$appid" =~ "$target" ]]; then
            return 0
        fi
    done

    return 1
}

handle_focus_change() {
    local monitor="$1"
    local appid="${APPID_BY_MONITOR[$monitor]}"

    if is_target_app "$appid"; then
        if [ "$CURRENT_LAYOUT" != "game" ]; then
            mmsg -d "setoption,xkb_rules_variant,"
            CURRENT_LAYOUT="game"
        fi
    elif [ "$CURRENT_LAYOUT" = "game" ]; then
        mmsg -d "setoption,xkb_rules_variant,colemak,"
        CURRENT_LAYOUT="default"
    fi
}

exec 204>"$LOCKFILE"
flock -n 204 || { echo "Another instance is running"; exit 1; }

mmsg -w | while read -r monitor event value; do
    if [ "$event" = "appid" ]; then
        APPID_BY_MONITOR["$monitor"]="$value"
    elif [ "$event" = "selmon" ] && [ "$value" = "1" ]; then
        handle_focus_change "$monitor"
    fi
done
