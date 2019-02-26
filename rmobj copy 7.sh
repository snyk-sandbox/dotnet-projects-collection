#/bin/bash

dirlist=$(find . -mindepth 1 -maxdepth 1 -type d | grep -v "./.git")
for dir in $dirlist; do
	cd $dir
	if [ -d obj ]; then
		rm -rf obj/
	fi
	cd ..
done

