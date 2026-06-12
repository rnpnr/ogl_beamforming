/* See LICENSE for license details. */

// NOTE(rnp): functions which require platform layer support but
// otherwise share implementation

function void
os_push_input_event(BeamformerInput *input, BeamformerInputEvent event)
{
	assert(input->event_count < countof(input->event_queue));
	if (input->event_count < countof(input->event_queue))
		input->event_queue[input->event_count++] = event;
}

function void
os_build_frame_input(BeamformerInput *input)
{
	Vector2 new_mouse = {-1, -1};
	if (IsWindowFocused()) new_mouse = GetMousePosition();
	input->mouse_x = new_mouse.x;
	input->mouse_y = new_mouse.y;

	input->input_modifiers  = 0;
	input->input_modifiers |= BeamformerInputModifier_LeftAlt      * IsKeyDown(KEY_LEFT_ALT);
	input->input_modifiers |= BeamformerInputModifier_RightAlt     * IsKeyDown(KEY_RIGHT_ALT);
	input->input_modifiers |= BeamformerInputModifier_LeftControl  * IsKeyDown(KEY_LEFT_CONTROL);
	input->input_modifiers |= BeamformerInputModifier_RightControl * IsKeyDown(KEY_RIGHT_CONTROL);
	input->input_modifiers |= BeamformerInputModifier_LeftShift    * IsKeyDown(KEY_LEFT_SHIFT);
	input->input_modifiers |= BeamformerInputModifier_RightShift   * IsKeyDown(KEY_RIGHT_SHIFT);
	input->input_modifiers |= BeamformerInputModifier_LeftMeta     * IsKeyDown(KEY_LEFT_SUPER);
	input->input_modifiers |= BeamformerInputModifier_RightMeta    * IsKeyDown(KEY_RIGHT_SUPER);
}
