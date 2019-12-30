#!/bin/sh

xsetroot -cursor_name left_ptr
xsetroot -name "xmonad"
hsetroot -solid "#1d1f21"

xrdb -merge ~/.Xresources
xmodmap ~/.Xmodmap

xbanish -i mod4 &
dunst -config ~/.config/dunst/dunstrc &
sxhkd &
picom -b
fcitx -d

udiskie &

exec xmonad
