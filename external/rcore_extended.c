#include "raylib/src/rcore.c"

// NOTE(rnp): hacky function to get the GLFWwindow handle
RLAPI void *
GetPlatformWindowHandle(void)
{
	return (void *)platform.handle;
}
