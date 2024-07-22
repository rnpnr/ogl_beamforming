# Usage

## ogl

## ogl_beamformer_pipe.c

Make sure `ogl_beamformer_pipe.mexa64` is in your MATLAB path and call

```matlab
ogl_beamformer_pipe(pipe_name, data)
```

* `pipe_name`: must be a character array, not a MATLAB string, and
  must correspond to the name provided in the main program.
* `data`: must be `int16`.

### Caveats

If you `clear` the mex function on Win32 you will not be able to
send more data until the main program is closed and reopened. This
is due to how pipes are implemented in Windows.

