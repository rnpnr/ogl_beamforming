#!/bin/sh

clang -march=native -O2 send_params.c ogl_beamformer_lib.c -o helper -lzstd
#clang -c send_params.c -fms-extensions -Wno-ignored-attributes -o /dev/null
