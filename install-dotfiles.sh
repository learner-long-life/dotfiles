#!/bin/sh

dir=$(pwd)
for file in .*;
do echo $file
if [ -e ~/$file ];
then
  echo "$file exists, not linking."
else
  ln -s $dir/$file ~/$file
fi
done;
