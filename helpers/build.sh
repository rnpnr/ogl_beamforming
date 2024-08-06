#!/bin/sh

clang -march=native -O2 send_ornot.c ogl_beamformer_lib.c -o helper -lzstd
#clang -c send_ornot.c -fms-extensions -Wno-ignored-attributes -o /dev/null
