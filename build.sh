#!/bin/sh
set -e

cflags="-march=native -std=c11 -O3 -Wall -I./external/include"
#cflags="${cflags} -fproc-stat-report"
#cflags="${cflags} -Rpass-missed=.*"
libcflags="$cflags -fPIC -shared -Wno-unused-variable"
ldflags="-lm"

debug=${DEBUG}

cc=${CC:-cc}

case $(uname -sm) in
MINGW64*)
	ldflags="$ldflags -lgdi32 -lwinmm"
	if [ ! ${NO_MATLAB} ] && [ -d "C:/Program Files/MATLAB/R2022a/extern/lib/win64/microsoft" ]; then
		libcflags="$libcflags -DMATLAB_CONSOLE"
		extra_ldflags="-llibmat -llibmex"
	fi
	libname="beamformer.dll"
	main="main_w32.c"
	${cc} $libcflags helpers/ogl_beamformer_lib.c -o helpers/ogl_beamformer_lib.dll \
		-L'C:/Program Files/MATLAB/R2022a/extern/lib/win64/microsoft' \
		$extra_ldflags
	;;
Linux*)
	cflags="$cflags -D_DEFAULT_SOURCE"
	libname="beamformer.so"
	main="main_linux.c"
	${cc} $libcflags helpers/ogl_beamformer_lib.c -o helpers/ogl_beamformer_lib.so
	;;
esac

if [ ! -f external/lib/libraylib.a ]; then
	git submodule update --init --depth=1 external/raylib
	cmake --install-prefix="${PWD}/external" \
		-G "Ninja" -B external/raylib/build_static -S external/raylib \
		-D CMAKE_INSTALL_LIBDIR=lib -D CMAKE_BUILD_TYPE="Release" \
		-D BUILD_SHARED_LIBS=OFF \
		-DCUSTOMIZE_BUILD=ON -DBUILD_EXAMPLES=OFF -DWITH_PIC=ON \
		-DOPENGL_VERSION=4.3 -DUSE_AUDIO=OFF -DSUPPORT_MODULE_RAUDIO=OFF
	cmake --build   external/raylib/build_static
	cmake --install external/raylib/build_static

	# NOTE: we also build the dynamic lib for debug purposes
	cmake --install-prefix="${PWD}/external" \
		-G "Ninja" -B external/raylib/build_shared -S external/raylib \
		-D BUILD_SHARED_LIBS=ON \
		-D CMAKE_INSTALL_LIBDIR=lib -D CMAKE_BUILD_TYPE="Release" \
		-DCUSTOMIZE_BUILD=ON -DBUILD_EXAMPLES=OFF -DWITH_PIC=ON \
		-DOPENGL_VERSION=4.3 -DUSE_AUDIO=OFF -DSUPPORT_MODULE_RAUDIO=OFF
	cmake --build   external/raylib/build_shared
	cmake --install external/raylib/build_shared
fi

# Hot Reloading/Debugging
if [ "$debug" ]; then
	cflags="$cflags -O0 -ggdb -D_DEBUG -Wno-unused-function"
	#cflags="$cflags -fsanitize=address,undefined"
	ldflags="-L./external/lib -lraylib -Wl,-rpath,external/lib/ $ldflags"
	libcflags="$cflags -fPIC -shared"
	${cc} $libcflags beamformer.c -o $libname $ldflags
else
	ldflags="./external/lib/libraylib.a ${ldflags}"
fi

${cc} ${cflags} -o ogl ${main} ${ldflags}
