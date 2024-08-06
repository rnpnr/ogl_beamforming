#!/bin/sh

clang -march=native -O3 send_params.c -o helper -lzstd
clang -march=native -O3 export.c -o export_frame -lzstd
#clang -c send_params.c -fms-extensions -Wno-ignored-attributes -o /dev/null
