# ogl beamforming

# Building

Bootstrap the build tool once and it will rebuild itself as
needed:
```sh
cc -march=native -O3 build.c -o build
```
or:
```bat
md out & cl -nologo -std:c11 -O2 -Fo:out\ build.c
```

Then run the build tool:
```sh
./build
```

## Debug Builds
Pass the build tool the `--debug` flag to get a build suitable for
development/debugging:
```
./build --debug
```

Debug builds enable dynamic reloading of almost the entire program
and you can make changes to most code and recompile without
exiting the application.

## MSVC Support

MSVC is not the target compiler for this application. While some
attempt is made to keep an exe building with MSVC no effort is
spent testing that it works beyond launching and running. Some
previous attempts at testing it have indicated that MSVC is
miscompiling parts of the code. Additionally there have been valid
(as far as other C compilers are concerned) lines of code in this
project that have caused MSVC to **crash** during compilation. If
your compiler is so poorly written that it crashes on **ANY**
input is it really worth our effort to support?
