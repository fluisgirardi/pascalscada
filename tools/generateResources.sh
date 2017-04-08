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
      $inkcmd --file=../src/$pkgfolder/scalable/$svgfile --export-png=../src/$pkgfolder/icons/$outfile.png --export-dpi=48 > /dev/null
    done;
  else
    echo "NOT FOUND...";
  fi

  echo -n "Looking for PNG ICONS      in src/$pkgfolder: ";
  if [ -d "../src/$pkgfolder/icons" ] ; then
    echo "FOUND! Starting the Lazarus Resource file (.lrs) creation procedure...";
    if [ ! -d "../src/$pkgfolder/.tmp" ] ; then
      mkdir "../src/$pkgfolder/.tmp";
    fi;
    
    if [ -f "../src/$pkgfolder/icons/overlay.png" ]; then
      echo " --> Overlay icon found.";
      for png in `find ../src/$pkgfolder/icons -maxdepth 1 -mindepth 1 -type f -iname "t*.png" -printf '%f\n'`; do
        echo " --> Adding overlay icon to src/$pkgfolder/img/$png and saving the result in src/$pkgfolder/.tmp/$png";
        $convertcmd ../src/$pkgfolder/icons/$png ../src/$pkgfolder/icons/overlay.png +composite ../src/$pkgfolder/.tmp/$png
      done;      
    else
      echo " --> No overlay icon found, copying ../src/$pkgfolder/icons/* => ../src/$pkgfolder/.tmp";
      cp -rf ../src/$pkgfolder/icons/* ../src/$pkgfolder/.tmp 2> /dev/null
    fi

    linha="";
    CreateLRS=0;
    for l in `find ../src/$pkgfolder/.tmp -maxdepth 1 -mindepth 1 -type f -iname "t*.png"`; do
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
    else
      echo " --> No PNG files found!";
    fi

    echo " --> Clean up: rm -rf ../src/$pkgfolder/.tmp";
    rm -rf ../src/$pkgfolder/.tmp
    
  else
    echo "NOT FOUND...";
  fi

  if [ $ShouldRemoveIconsFolder -eq 1 ]; then
    echo "Removing icons folder...";
    rm -rf "../src/$pkgfolder/icons";
  fi
  echo "";
done;
