# xset s off
# xset -dpms
# picom -b
# xplanet &
# picom -bcCGf -i 0.8 -e 0.8 --no-fading-openclose

if [[ -r /etc/default/keyboard ]]; then
  . /etc/default/keyboard
  setxkbmap -model "$XKBMODEL" -layout "$XKBLAYOUT" -variant "$XKBVARIANT" -option "$XKBOPTIONS"
fi
