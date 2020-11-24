#!/bin/bash

while [ $(echo $(pidof alacritty) | awk '{split($0,a); print length(a);}') -ne 1 ]; do
	echo "Waiting for kitty before launching picom.."
	sleep 0.1
done

picom -b --config ~/.config/picom/picom.conf
echo "Launched picom as background daemon.."

