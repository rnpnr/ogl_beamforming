# ogl beamforming

# Building

The beamformer requires a compiler with support for `_Float16`.
This means that GCC 12.1 or Clang 15 are the mininum supported
compiler versions. Testing on compilers this old has been limited
so you may run into bugs which do not occur with newer compilers.
It is highly recommended that you use a more modern compiler (GCC
15+ or Clang 21+).

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

# Troubleshooting

## Missing Vulkan Support

If the beamformer fails to start with a message like:
```sh
./ogl
[vulkan]  selecting device: llvmpipe (LLVM 15.0.7, 256 bits)
[vulkan]  fatal error: missing required device extensions:
[vulkan]      VK_KHR_external_memory
[vulkan]      VK_KHR_external_memory_win32
```
you may need to use the legacy branch:

```sh
git checkout legacy
./build
```

The `legacy` branch will be supported until the need for those
extensions is removed but will not see feature or performance
updates.

## `llvmpipe`

If the beamformer starts with the `llvmpipe` device it means that
your system is missing vulkan libraries. On Ubuntu this can be
resolved with:

```sh
sudo apt install libvulkan1
```

# Publication

This project has a paper covering some of the design and
optimization up until commit 295b9c4. It is currently available as
a preprint: [arXiv:2512.11086]. If you wish to refer to this
project in any published work you can cite that article. The link
will be updated once a full publication is available.

[arXiv:2512.11086]: https://arxiv.org/abs/2512.11086
