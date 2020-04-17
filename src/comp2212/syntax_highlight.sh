#!/bin/bash

cwd=$(pwd)
vim_syntax_directory="$HOME/.vim/syntax"
vim_ftdetect_directory="$HOME/.vim/ftdetect"

if [ -d "$vim_syntax_directory" ]; then
	vim_syntax_directory+="/spl.vim"
	cp spl.vim "$vim_syntax_directory"
else
	vim_syntax_directory+="/spl.vim"
	cd ~/.vim; mkdir syntax; cd "$cwd"; cp spl.vim "$vim_syntax_directory"
fi

if [ -d "$vim_ftdetect_directory" ]; then
	vim_ftdetect_directory+="/spl.vim"
	cp ftdetect.vim "$vim_ftdetect_directory"
else
	vim_ftdetect_directory+="/spl.vim"
	cd $HOME/.vim; mkdir ftdetect; cd "$cwd"; cp ftdetect.vim "$vim_ftdetect_directory"
fi
