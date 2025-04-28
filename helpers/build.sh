#!/bin/sh

clang -march=native -O3 reshape.c -o reshape -lzstd
