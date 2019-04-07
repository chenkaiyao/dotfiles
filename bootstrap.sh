#!/usr/bin/env bash

cd "$(dirname "${BASH_SOURCE}")";

function doIt() {
	rsync -avh --no-perms ./move ~;
	source ~/.bash_profile;
}

if [ "$1" == "--force" -o "$1" == "-f" ]; then
	doIt;
else
	read -p "This may overwrite existing files in your home directory. Are you sure? (y/n) " -n 1;
	echo "";
	if [[ $REPLY =~ ^[Yy]$ ]]; then
		doIt;
	fi;
fi;
unset doIt;

echo ""

echo -e "\033[40;32m start to install command line tools for your system ...\033[0m"

sysType=`uname -s`

echo -e "\033[40;32m Your system is $sysType \033[0m"

if [ $sysType = "Linux" ]; then
    source ./apt.sh;
elif [ $sysType = "Darwin" ]; then
    source ./brew.sh;
else
    echo -e "\033[40;32m unsupported system, exit \033[0m"
fi

echo ""
echo -e "\033[40;32m All done, HAPPY HACKING :-) \033[0m"
echo ""

