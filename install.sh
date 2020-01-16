#!/usr/bin/env bash

CURR_DIR=$(pwd)

mkdir -p ~/.config
mkdir -p ~/.xmonad

ln -s $CURR_DIR/conky ~/.config/conky
ln -s $CURR_DIR/kitty ~/.config/kitty
ln -s $CURR_DIR/todo-sh ~/.todo
ln -s $CURR_DIR/xmonad/xmonad.hs ~/.xmonad/xmonad.hs
ln -s $CURR_DIR/xmonad/lib ~/.xmonad/lib
