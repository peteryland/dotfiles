#!/bin/bash

if ! command -v xplanet &> /dev/null; then
  exit 1
fi

sleep 1

# There can be only one
if [[ $(pgrep -c xplanet.sh) -gt 1 ]]; then
  killall -eq -u "$USER" -o 2s xplanet.sh
fi

firstscreen="$(xrandr --current | grep current | head -1)"
xres="$(sed 's/^.*current \([0-9]\+\) \?x.*$/\1/' <<< "$firstscreen")"
yres="$(sed 's/^.*current [0-9]\+ \?x \?\([0-9]\+\),.*$/\1/' <<< "$firstscreen")"

cd "$(dirname "$0")"

outfile="$HOME"/.xplanet/out.png
haveclouds=

if [[ $haveclouds ]]; then
  cloudsfile="$HOME"/.xplanet/images/clouds.jpg
  clouds4k="${cloudsfile%.jpg}".4k.jpg
  cloudconfig='cloudmap=$clouds4k'
  if [[ -r $clouds4k ]]; then
    if [[ -z $(find "$clouds4k" -mmin -60 2> /dev/null) ]]; then
      localsha="$(sha256sum "$cloudsfile" | awk '{print $1;}')"
      originsha="$(wget https://raw.githubusercontent.com/apollo-ng/cloudmap/master/global.sha256 -q -T 5 --no-cache -O - | awk '{print $1;}')"
      if [[ $originsha != $localsha ]]; then
        wget https://raw.githubusercontent.com/apollo-ng/cloudmap/master/global.jpg?${originsha} --no-cache -q -T 5 -O "$cloudsfile".new
        newsha="$(sha256sum "$cloudsfile".new | awk '{print $1;}')"
        if [[ $newsha = $originsha ]]; then
          mv -f "$cloudsfile".new "$cloudsfile"
          convert -resize "$xres"x"$yres" -quality 95 "$cloudsfile" "$clouds4k"
        else
          rm -f "$cloudsfile".new
        fi
      fi
    fi
  else
    wget https://raw.githubusercontent.com/apollo-ng/cloudmap/master/global.jpg?${originsha} --no-cache -q -T 5 -O "$cloudsfile"
    convert -resize "$xres"x"$yres" -quality 95 "$cloudsfile" "$clouds4k"
  fi
fi

markers/getdata.sh

echo "[default]
marker_font=/usr/share/fonts/truetype/firacodenerd/FiraCodeNerd-Retina.ttf

[earth]
night_map=night.4k.jpg
$cloudconfig
bump_map=bump.4k.jpg
bump_scale=1
marker_color={255,255,255}" > ~/.xplanet/xplanet.config
echo "map=$(date +%m).4k.jpg" >> ~/.xplanet/xplanet.config
for i in markers/*.marker; do
  echo "marker_file=$i" >> ~/.xplanet/xplanet.config
done
# cat ~/.xplanet/satellites/config >> ~/.xplanet/xplanet.config

nice xplanet -config ~/.xplanet/xplanet.config -projection rect -verbosity 0 -output "$outfile" -num_times 1 -geometry "$xres"x"$yres" -longitude 11
hsetroot -fill "$outfile" > /dev/null
if [[ $? -eq 123 ]]; then exit 123; fi
sleep 600

exec "./$(basename $0)"
