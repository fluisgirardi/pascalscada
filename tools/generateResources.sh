#!/bin/sh

if [ -x /usr/bin/inkscape ] || [ -x /usr/local/bin/inkscape ]; then
  inkcmd='inkscape';
else
  if [ -x /usr/bin/inkscape-devel ] || [ -x /usr/local/bin/inkscape-devel ]; then
    inkcmd='inkscape-devel';
  else
    echo "Inkscape or inkscape-devel not found!"
    return 1;
  fi
fi;

if [ -x /usr/bin/convert ] || [ -x /usr/local/bin/convert ]; then
  convertcmd='convert';
else
  echo "ImageMagick not found!"
  return 1;
fi


for pkgfolder in `find ../src/ -maxdepth 1 -mindepth 1 -type d -printf '%f\n'`; do
  ShouldRemoveIconsFolder=0;
  echo -n "Looking for scalable icons in src/$pkgfolder: ";
  if [ -d "../src/$pkgfolder/scalable" ] ; then
    echo "FOUND! Starting the conversion to PNG";
    if [ ! -d "../src/$pkgfolder/icons" ] ; then
      mkdir "../src/$pkgfolder/icons";
      ShouldRemoveIconsFolder=1;
    fi

    for svgfile in `find ../src/$pkgfolder/scalable -maxdepth 1 -mindepth 1 -type f -iname 't*.svg' -printf '%f\n'`; do
      outfile=`echo $svgfile | cut -d"/" -f3 | cut -d"." -f1`;
      echo " --> (Re)Creating file ../src/$pkgfolder/icons/$outfile.png";
      $inkcmd --file=../src/$pkgfolder/scalable/$svgfile --export-png=../src/$pkgfolder/icons/$outfile.png --export-dpi=45 > /dev/null
    done;
  else
    echo "NOT FOUND...";
  fi

  echo -n "Looking for PNG ICONS      in src/$pkgfolder: ";
  if [ -d "../src/$pkgfolder/icons" ] ; then
    echo "FOUND! Starting the Lazarus Resource file (.lrs) creation procedure...";
    linha="";
    CreateLRS=0;
    for l in `find ../src/$pkgfolder/icons -maxdepth 1 -mindepth 1 -type f -iname "t*.png"`; do
      CreateLRS=1;
      linha="$linha $l";
      echo " --> found file $l";
    done;
    
    if [ $CreateLRS -eq 1 ]; then
      if [ -f "../src/$pkgfolder/$pkgfolder.lrs" ]; then
        echo " --> Updating LRS file in src/$pkgfolder/$pkgfolder.lrs"        
      else
        echo " --> Creating LRS file in src/$pkgfolder/$pkgfolder.lrs"
      fi
      ./lazres_linux_x86_64 ../src/$pkgfolder/$pkgfolder.lrs $linha > /dev/null    
    fi
  else
    echo "NOT FOUND...";
  fi

  if [ $ShouldRemoveIconsFolder -eq 1 ]; then
    echo "Removing icons folder...";
    rm -rf "../src/$pkgfolder/icons";
  fi
done;

