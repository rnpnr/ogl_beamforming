clear all;

openProject("../others/Ultrasound-Beamforming/Ultrasound-Beamforming.prj");
openProject("../others/Verasonics-Biasing-Imaging/Verasonics_Biasing_Imaging.prj");

vrs_path   = "/tmp/downloads";
vrs_prefix = "240723_ATS539_Contrast_uFORCES-32-TxRow";
%vrs_prefix = "240723_ATS539_Resolution_uFORCES-32-TxRow";
%vrs_prefix = "240723_ATS539_Resolution_FORCES-TxRow";
%vrs_prefix = "240723_ATS539_Contrast_FORCES-TxRow";
vrs_num    = "_Intensity_06";
vrs_name   = vrs_prefix + vrs_num + ".vrs";

pipe_name = '/tmp/beamformer_data_fifo';
smem_name = '/ogl_beamformer_parameters';

vrs  = VRSFile(fullfile(vrs_path, vrs_name));
data = vrs.GetData();

load(fullfile(vrs_path, "postEnv.mat"), "Trans", "Receive", "Resource", "TW");
load(fullfile(vrs_path, "preEnv.mat"), "sparseElements", "scan");

receive             = Receive([Receive.bufnum] == 1);
receive_orientation = scan.TransmitEvents(1).ImagingPattern.ReceiveOrientation;
recieve_elements    = scan.TransmitEvents(1).ImagingPattern.ReceiveOrientation.GetElementCount(die);

bp.output_min_coordinate = struct('x', -45e-3, 'y', 0, 'z', 5e-3,   'w', 0);
bp.output_max_coordinate = struct('x',  45e-3, 'y', 0, 'z', 165e-3, 'w', 0);

bp.output_points.x = 256;
bp.output_points.y = 1;
bp.output_points.z = 1024;
bp.output_points.w = 1; % Number of frames for averaging

bp.rf_raw_dim     = struct('x', size(data, 1), 'y', size(data, 2));
bp.dec_data_dim.x = max(1 + [receive.endSample] - [receive.startSample], [], "all");
bp.dec_data_dim.y = recieve_elements;
bp.dec_data_dim.z = max([receive.acqNum]);
bp.dec_data_dim.w = 0;

bp.sampling_frequency = receive(1).samplesPerWave * Trans.frequency(1) * 1e6;
bp.center_frequency   = Trans.frequency * 1e6;
bp.speed_of_sound     = Resource.Parameters.speedOfSound;

bp.time_offset = TW(1).Parameters(3) / bp.center_frequency;

bp.channel_mapping  = Trans.ConnectorES - 1;
if (exist('sparseElements'))
	bp.uforces_channels = sparseElements - 1;
else
	bp.uforces_channels = 0:(recieve_elements - 1);
end

bp.channel_offset = 0 + recieve_elements * receive_orientation.Contains(tobe.Orientation.Column);

die_size       = scan.Die.GetSize();
bp.xdc_origin  = [-die_size(1) / 2, -die_size(2) / 2, 0, 0];
bp.xdc_corner1 = [ die_size(1) / 2, -die_size(2) / 2, 0, 0];
bp.xdc_corner2 = [-die_size(1) / 2,  die_size(2) / 2, 0, 0];
bp.xdc_count   = length(bp.xdc_origin) / 4;

bp.focal_depth  = 0;

% NOTE: plane and position along plane normal for beamforming 2D HERCULES
bp.beamform_plane = 0;
bp.off_axis_pos = 0;

fc = bp.center_frequency;
fs = bp.sampling_frequency;

%bp.lpf_order = 63;
%Rp  = 0.00057565; % Corresponds to 0.01 dB peak-to-peak ripple
%Rst = 1e-4;       % Corresponds to 80 dB stopband attenuation
%bp.lpf_coefficients = firceqrip(bp.lpf_order, (fc/2)/(fs/2), [Rp Rst], 'passedge');

% NOTE: No low pass filtering/demod
%bp.lpf_order = 0;
%cs_stages = uint8([
%	OGLShaderStage.HADAMARD, ...
%	OGLShaderStage.UFORCES, ...
%	OGLShaderStage.MIN_MAX, ...
%]);

cs_stages = uint8([
	OGLShaderStage.HADAMARD, ...
	OGLShaderStage.UFORCES, ...
	OGLShaderStage.MIN_MAX, ...
]);

%loadlibrary('ogl_beamformer_lib')
%calllib('ogl_beamformer_lib', 'set_beamformer_parameters', smem_name, bp);
%calllib('ogl_beamformer_lib', 'set_beamformer_pipeline', smem_name, cs_stages, numel(cs_stages));
%%while (true)
%%tic()
%calllib('ogl_beamformer_lib', 'send_data', pipe_name, smem_name, data, bp.rf_raw_dim);
%%toc()
%%end
%%unloadlibrary('ogl_beamformer_lib')

export_bp(fullfile(vrs_path, vrs_prefix + "_bp_inc.h"), bp);
fd = fopen(fullfile(vrs_path, vrs_prefix + vrs_num + "_rf_raw.bin"), "w");
fwrite(fd, data, 'int16');
fclose(fd);
