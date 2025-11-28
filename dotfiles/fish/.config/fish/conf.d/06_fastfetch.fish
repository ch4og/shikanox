if status is-interactive
    if string match -q "*ghostty*" $TERM || string match -q "*kitty*" $TERM
        if test "$SHLVL" -eq 1
            command -v fastfetch >/dev/null 2>&1 && fastfetch
        end
    end
end
