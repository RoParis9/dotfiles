#!/bin/bash

# Start awww-daemon if not already running
pgrep -x awww-daemon > /dev/null || awww-daemon &

# Wait a bit longer for Wayland to initialize
sleep 1

# Pick a random wallpaper
WALLPAPER=$(find "$HOME/Media/wallpapers/" -type f \( -iname "*.jpg" -o -iname "*.png" -o -iname "*.jpeg" \) | shuf -n 1)

# Set wallpaper with transition
awww img "$WALLPAPER" \
  --transition-type any \
  --transition-fps 180 \
  --transition-duration 2 \
  --transition-pos "$(hyprctl cursorpos)"

