#!/bin/bash

cd "$(dirname "$0")"

f1="${1:-6}"
f2="${2:-$((f1+4))}"
f3="${3:-$((f2+2))}"
f4="${4:-$((f3+2))}"

if [[ -z $(find volcanos.marker -mmin -60 2> /dev/null) ]]; then
  wget https://volcano.si.edu/news/WeeklyVolcanoRSS.xml -qO - | grep -E ^\<title\|georss:point | cut -d\> -f2 | cut -d\< -f1 | while read x; do
    if [[ $title ]]; then
      coords="$x"
      echo "$coords \"\" color=Orange symbolsize=0 opacity=80 align=center"
      echo "$coords \" $title  \" color=Tan symbolsize=0 fontsize=$f2 opacity=80"
      unset title
    else
      title="${x% (*}"
    fi
  done > volcanos.marker
fi

if [[ -z $(find quakes.marker -mmin -60 2> /dev/null) ]]; then
  wget https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/4.5_week.geojson -qO - \
    | jq -r '.features[] | {properties, geometry}
                         | "\(.geometry.coordinates[1]) \(.geometry.coordinates[0])" as $coords
                         | "color=\(if .properties.mag >= 5 then if .properties.mag > 6 then "Red" else "Orange" end else "Yellow" end)" as $color
                         | (('"$(echo -n $(date +%s))"' - .properties.time / 1000) / 86400) as $days
                         | "opacity=\(if .properties.mag >= 5 or $days <= 2 then if .properties.mag > 6 or $days <= 1 then 90 else 70 end else 50 end)" as $opacity
                         | "fontsize=\(if $days > 4 and .properties.mag <= 6 then '"$f1"' else if $days > 2 then '"$f2"' else if $days > 1 then '"$f3"' else '"$f4"' end end end)" as $size
                         | "\($coords) \"\" \($color) symbolsize=0 \($size) \($opacity) align=center\n" +
                           "\($coords) \" \(if $days <= 1 and .properties.mag >= 6 then .properties.title else .properties.mag end)  \" \($color) symbolsize=0 \($size) \($opacity)"' > quakes.marker
fi
