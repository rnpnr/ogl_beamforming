#!/bin/sh

clang -march=native -O2 send_ornot.c ogl_beamformer_lib.c -o helper -lzstd

#clang -march=native -O3 -fsanitize=address,undefined comp_paper.c ogl_beamformer_lib.c -o comp_paper -lzstd -lfftw3f -lpthread
clang -march=native -O3 comp_paper.c ogl_beamformer_lib.c -o comp_paper -lzstd -lfftw3f -lpthread
#clang -march=native -O0 -ggdb comp_paper.c ogl_beamformer_lib.c -o comp_paper -lzstd -lfftw3f

#clang -c send_ornot.c -fms-extensions -Wno-ignored-attributes -o /dev/null
