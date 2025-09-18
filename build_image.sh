#!/bin/sh

# args: kernel out

dd if=/dev/zero of=$2 bs=1024 count=1440
dd if=$1 of=$2 conv=notrunc
