/* See LICENSE for license details. */

// NOTE(rnp): functions which require platform layer support but
// otherwise share implementation
// TODO(rnp): replace all this with platform specific functions

void *GetPlatformWindowHandle(void);

// see: external/raylib/src/external/glfw/include/GLFW/glfw3.h
typedef void glfw_window_resize_fn(void *window, i32 width, i32 height);

glfw_window_resize_fn *glfwSetWindowSizeCallback(void *window, glfw_window_resize_fn *callback);
void *glfwSetCharCallback(void *window, void (*callback)(void *window, u32 codepoint));
void *glfwSetKeyCallback(void *window, void (*callback)(void *window, i32 key, i32 scancode, i32 action, i32 mods));
void *glfwSetMouseButtonCallback(void *window, void (*callback)(void *window, i32 button, i32 action, i32 mods));
void *glfwSetScrollCallback(void *window, void (*callback)(void *window, f64 x_delta, f64 y_delta));

global BeamformerInput       *beamformer_input;
global glfw_window_resize_fn *raylib_window_resize;

function void
os_push_input_event(BeamformerInput *input, BeamformerInputEvent event)
{
	assert(input->event_count < countof(input->event_queue));
	if (input->event_count < countof(input->event_queue))
		input->event_queue[input->event_count++] = event;
}

function BeamformerInputModifiers
os_modifiers_from_glfw(i32 modifiers)
{
	BeamformerInputModifiers result = 0;
	result |= ((modifiers & 0x1) != 0) * BeamformerInputModifier_Shift;
	result |= ((modifiers & 0x2) != 0) * BeamformerInputModifier_Control;
	result |= ((modifiers & 0x4) != 0) * BeamformerInputModifier_Alt;
	result |= ((modifiers & 0x8) != 0) * BeamformerInputModifier_Meta;
	return result;
}

function void
os_scroll_callback(void *window, f64 xoff, f64 yoff)
{
	// TODO(rnp): GLFW doesn't pass modifiers through but the os event would have them
	os_push_input_event(beamformer_input, (BeamformerInputEvent){
		.kind = BeamformerInputEventKind_MouseScroll,
		.scroll = {.x = xoff, .y = yoff},
	});
}

function void
os_key_callback(void *window, i32 key, i32 scancode, i32 action, i32 modifiers)
{
	BeamformerInputEvent input_event = {
		.kind      = action == 0 ? BeamformerInputEventKind_ButtonRelease : BeamformerInputEventKind_ButtonPress,
		.modifiers = os_modifiers_from_glfw(modifiers),
	};

	switch (key) {
	default:{}break;
	case BeamformerButtonID_Space:
	case BeamformerButtonID_Apostrophe:
	case BeamformerButtonID_Comma:
	case BeamformerButtonID_Minus:
	case BeamformerButtonID_Period:
	case BeamformerButtonID_Slash:
	case BeamformerButtonID_0:
	case BeamformerButtonID_1:
	case BeamformerButtonID_2:
	case BeamformerButtonID_3:
	case BeamformerButtonID_4:
	case BeamformerButtonID_5:
	case BeamformerButtonID_6:
	case BeamformerButtonID_7:
	case BeamformerButtonID_8:
	case BeamformerButtonID_9:
	case BeamformerButtonID_Semicolon:
	case BeamformerButtonID_Equal:
	case BeamformerButtonID_A:
	case BeamformerButtonID_B:
	case BeamformerButtonID_C:
	case BeamformerButtonID_D:
	case BeamformerButtonID_E:
	case BeamformerButtonID_F:
	case BeamformerButtonID_G:
	case BeamformerButtonID_H:
	case BeamformerButtonID_I:
	case BeamformerButtonID_J:
	case BeamformerButtonID_K:
	case BeamformerButtonID_L:
	case BeamformerButtonID_M:
	case BeamformerButtonID_N:
	case BeamformerButtonID_O:
	case BeamformerButtonID_P:
	case BeamformerButtonID_Q:
	case BeamformerButtonID_R:
	case BeamformerButtonID_S:
	case BeamformerButtonID_T:
	case BeamformerButtonID_U:
	case BeamformerButtonID_V:
	case BeamformerButtonID_W:
	case BeamformerButtonID_X:
	case BeamformerButtonID_Y:
	case BeamformerButtonID_Z:
	case BeamformerButtonID_LeftBracket:
	case BeamformerButtonID_Backslash:
	case BeamformerButtonID_RightBracket:
	case BeamformerButtonID_Grave:
	{
		input_event.button_id = key;
		input_event.codepoint = key;
	}break;

	case 256:{input_event.button_id = BeamformerButtonID_Escape;      }break;
	case 257:{input_event.button_id = BeamformerButtonID_Enter;       }break;
	case 258:{input_event.button_id = BeamformerButtonID_Tab;         }break;
	case 259:{input_event.button_id = BeamformerButtonID_Backspace;   }break;
	case 260:{input_event.button_id = BeamformerButtonID_Insert;      }break;
	case 261:{input_event.button_id = BeamformerButtonID_Delete;      }break;
	case 262:{input_event.button_id = BeamformerButtonID_Right;       }break;
	case 263:{input_event.button_id = BeamformerButtonID_Left;        }break;
	case 264:{input_event.button_id = BeamformerButtonID_Down;        }break;
	case 265:{input_event.button_id = BeamformerButtonID_Up;          }break;
	case 266:{input_event.button_id = BeamformerButtonID_PageUp;      }break;
	case 267:{input_event.button_id = BeamformerButtonID_PageDown;    }break;
	case 268:{input_event.button_id = BeamformerButtonID_Home;        }break;
	case 269:{input_event.button_id = BeamformerButtonID_End;         }break;
	case 280:{input_event.button_id = BeamformerButtonID_CapsLock;    }break;
	case 281:{input_event.button_id = BeamformerButtonID_ScrollLock;  }break;
	case 282:{input_event.button_id = BeamformerButtonID_NumLock;     }break;
	case 283:{input_event.button_id = BeamformerButtonID_PrintScreen; }break;
	case 284:{input_event.button_id = BeamformerButtonID_Pause;       }break;
	case 290:{input_event.button_id = BeamformerButtonID_F1;          }break;
	case 291:{input_event.button_id = BeamformerButtonID_F2;          }break;
	case 292:{input_event.button_id = BeamformerButtonID_F3;          }break;
	case 293:{input_event.button_id = BeamformerButtonID_F4;          }break;
	case 294:{input_event.button_id = BeamformerButtonID_F5;          }break;
	case 295:{input_event.button_id = BeamformerButtonID_F6;          }break;
	case 296:{input_event.button_id = BeamformerButtonID_F7;          }break;
	case 297:{input_event.button_id = BeamformerButtonID_F8;          }break;
	case 298:{input_event.button_id = BeamformerButtonID_F9;          }break;
	case 299:{input_event.button_id = BeamformerButtonID_F10;         }break;
	case 300:{input_event.button_id = BeamformerButtonID_F11;         }break;
	case 301:{input_event.button_id = BeamformerButtonID_F12;         }break;
	case 302:{input_event.button_id = BeamformerButtonID_F13;         }break;
	case 303:{input_event.button_id = BeamformerButtonID_F14;         }break;
	case 304:{input_event.button_id = BeamformerButtonID_F15;         }break;
	case 305:{input_event.button_id = BeamformerButtonID_F16;         }break;
	case 306:{input_event.button_id = BeamformerButtonID_F17;         }break;
	case 307:{input_event.button_id = BeamformerButtonID_F18;         }break;
	case 308:{input_event.button_id = BeamformerButtonID_F19;         }break;
	case 309:{input_event.button_id = BeamformerButtonID_F20;         }break;
	case 310:{input_event.button_id = BeamformerButtonID_F21;         }break;
	case 311:{input_event.button_id = BeamformerButtonID_F22;         }break;
	case 312:{input_event.button_id = BeamformerButtonID_F23;         }break;
	case 313:{input_event.button_id = BeamformerButtonID_F24;         }break;
	case 314:{input_event.button_id = BeamformerButtonID_F25;         }break;
	case 320:{input_event.button_id = BeamformerButtonID_KP0;         }break;
	case 321:{input_event.button_id = BeamformerButtonID_KP1;         }break;
	case 322:{input_event.button_id = BeamformerButtonID_KP2;         }break;
	case 323:{input_event.button_id = BeamformerButtonID_KP3;         }break;
	case 324:{input_event.button_id = BeamformerButtonID_KP4;         }break;
	case 325:{input_event.button_id = BeamformerButtonID_KP5;         }break;
	case 326:{input_event.button_id = BeamformerButtonID_KP6;         }break;
	case 327:{input_event.button_id = BeamformerButtonID_KP7;         }break;
	case 328:{input_event.button_id = BeamformerButtonID_KP8;         }break;
	case 329:{input_event.button_id = BeamformerButtonID_KP9;         }break;
	case 330:{input_event.button_id = BeamformerButtonID_KPDecimal;   }break;
	case 331:{input_event.button_id = BeamformerButtonID_KPDivide;    }break;
	case 332:{input_event.button_id = BeamformerButtonID_KPMultiply;  }break;
	case 333:{input_event.button_id = BeamformerButtonID_KPSubtract;  }break;
	case 334:{input_event.button_id = BeamformerButtonID_KPAdd;       }break;
	case 335:{input_event.button_id = BeamformerButtonID_KPEnter;     }break;
	case 336:{input_event.button_id = BeamformerButtonID_KPEqual;     }break;
	case 340:{input_event.button_id = BeamformerButtonID_LeftShift;   }break;
	case 341:{input_event.button_id = BeamformerButtonID_LeftControl; }break;
	case 342:{input_event.button_id = BeamformerButtonID_LeftAlt;     }break;
	case 343:{input_event.button_id = BeamformerButtonID_LeftSuper;   }break;
	case 344:{input_event.button_id = BeamformerButtonID_RightShift;  }break;
	case 345:{input_event.button_id = BeamformerButtonID_RightControl;}break;
	case 346:{input_event.button_id = BeamformerButtonID_RightAlt;    }break;
	case 347:{input_event.button_id = BeamformerButtonID_RightSuper;  }break;
	case 348:{input_event.button_id = BeamformerButtonID_Menu;        }break;
	}

	os_push_input_event(beamformer_input, input_event);
}

function void
os_mouse_button_callback(void *window, i32 button, i32 action, i32 modifiers)
{
	BeamformerButtonID button_id = BeamformerButtonID_MouseLeft;
	switch (button) {
	default:{}break;
	case 0:{button_id = BeamformerButtonID_MouseLeft;  }break;
	case 1:{button_id = BeamformerButtonID_MouseRight; }break;
	case 2:{button_id = BeamformerButtonID_MouseMiddle;}break;
	}

	os_push_input_event(beamformer_input, (BeamformerInputEvent){
		.kind      = action == 0 ? BeamformerInputEventKind_ButtonRelease : BeamformerInputEventKind_ButtonPress,
		.modifiers = os_modifiers_from_glfw(modifiers),
		.button_id = button_id,
	});
}

function void
os_window_equip_common(BeamformerInput *input, void *window)
{
	beamformer_input = input;

	glfwSetCharCallback(window,        0);
	glfwSetKeyCallback(window,         os_key_callback);
	glfwSetMouseButtonCallback(window, os_mouse_button_callback);
	glfwSetScrollCallback(window,      os_scroll_callback);
}

function void
os_build_frame_input(BeamformerInput *input)
{
	Vector2 new_mouse = {-1, -1};
	if (IsWindowFocused()) new_mouse = GetMousePosition();
	input->mouse_x = new_mouse.x;
	input->mouse_y = new_mouse.y;
}
