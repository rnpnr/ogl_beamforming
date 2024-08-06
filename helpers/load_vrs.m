clear all;

openProject("../others/Ultrasound-Beamforming/Ultrasound-Beamforming.prj");

vrs_path   = "/tmp/downloads";
vrs_prefix = "240723_ATS539_Resolution_uFORCES-32-TxRow";
%vrs_prefix = "240723_ATS539_Resolution_FORCES-TxRow";
%vrs_prefix = "240723_ATS539_Contrast_FORCES-TxRow";
vrs_name   = vrs_prefix + "_Intensity_07.vrs";

pipe_name = '/tmp/beamformer_data_fifo';
smem_name = '/ogl_beamformer_parameters';

vrs  = VRSFile(fullfile(vrs_path, vrs_name));
data = vrs.GetData();

load(fullfile(vrs_path, "postEnv.mat"), "Trans", "Receive", "Resource", "TW");
load(fullfile(vrs_path, "preEnv.mat"), "sparseElements", "scan");

receive             = Receive([Receive.bufnum] == 1);
receive_orientation = scan.TransmitEvents(1).ImagingPattern.ReceiveOrientation;

bp.output_min_xz = struct('x', -12e-3, 'y', 20e-3);
bp.output_max_xz = struct('x',  12e-3, 'y', 90e-3);

bp.output_points.x = 256;
bp.output_points.y = 512;
bp.output_points.z = 1;
bp.output_points.w = 0;

bp.rf_raw_dim     = struct('x', size(data, 1), 'y', size(data, 2));
bp.dec_data_dim.x = max(1 + [receive.endSample] - [receive.startSample], [], "all");
bp.dec_data_dim.y = 128;
bp.dec_data_dim.z = max([receive.acqNum]);
bp.dec_data_dim.w = 0;

bp.sampling_frequency = receive(1).samplesPerWave * Trans.frequency(1) * 1e6;
bp.center_frequency   = Trans.frequency * 1e6;
bp.speed_of_sound     = Resource.Parameters.speedOfSound;

bp.time_offset = TW(1).Parameters(3) / bp.center_frequency;

bp.channel_mapping  = Trans.ConnectorES - 1;
if (exist('sparseElements'))
	bp.uforces_channels = sparseElements(1:length(sparseElements)/2) - 1;
else
	bp.uforces_channels = 1:128 - 1;
end

bp.channel_offset   = 0 + 128 * receive_orientation.Contains(tobe.Orientation.Column);

die_size      = scan.Die.GetSize();
bp.xdc_min_xy = struct('x', -die_size(1) / 2, 'y', -die_size(2) / 2);
bp.xdc_max_xy = struct('x',  die_size(1) / 2, 'y',  die_size(2) / 2);

bp.focal_depth = 0;

% NOTE: effectively disables lpf
%bp.lpf_order = 0;
%bp.lpf_coefficients = 1;
%bp.center_frequency = 0;

bp.lpf_coefficients = [0.00150425278148001, 0.00663627704153171, 0.0183467992553614,  0.0386288031294497, ...
                       0.0668063601697744,  0.0985254555140267,  0.126486796857306,   0.142954931338469,  ...
                       0.142954931338469,   0.126486796857306,   0.0985254555140267,  0.0668063601697744, ...
                       0.0386288031294497,  0.0183467992553614,  0.00663627704153171, 0.00150425278148001];
bp.lpf_order = numel(bp.lpf_coefficients) - 1;

loadlibrary('ogl_beamformer_lib')
calllib('ogl_beamformer_lib', 'set_beamformer_parameters', smem_name, bp)
while (true)
tic()
calllib('ogl_beamformer_lib', 'send_data', pipe_name, smem_name, data, bp.rf_raw_dim)
toc()
end
unloadlibrary('ogl_beamformer_lib')
