% NOTE(rnp): extracts all vrs files in a saved dataset and saves them as binary data along
% with a C-header appropriate for use with send_params.c

openProject("../others/Verasonics-Biasing-Imaging/Verasonics_Biasing_Imaging.prj");

study                = "241210_ATS539_Resolution_FORCES-TxRow";
input_folder_prefix  = "/tmp/downloads/raw";
output_folder_prefix = "/tmp/downloads/extracted";
output_fmt_string    = study + "_%02d.bin";
vrs_match_pattern    = "*.vrs";

% NOTE(rnp): exported default parameters
bp.output_points         = make_vec4([2048, 1, 2048]);
bp.output_min_coordinate = make_vec4([-45e-3, 0, 0]);
bp.output_max_coordinate = make_vec4([ 45e-3, 0, 165e-3]);

cs_stages = [
	OGLShaderStage.CUDA_DECODE, ...
	OGLShaderStage.CUDA_HILBERT, ...
	OGLShaderStage.DAS, ...
];

%% Extract/Save Data
dir_listing = dir(fullfile(input_folder_prefix, vrs_match_pattern));
count = 0;
for i = 1:length(dir_listing)
	vrs  = VRSFile(fullfile(input_folder_prefix, dir_listing(i).name));
	data = vrs.GetData();
	write_cleansed_data(data, fullfile(output_folder_prefix, sprintf(output_fmt_string, count)))
	delete(fullfile(input_folder_prefix, dir_listing(i).name));
	count = count + 1;
	rf_raw_dim = size(data);
end

%% Fill Header
load(fullfile(input_folder_prefix, "postEnv.mat"), "Trans", "Receive", "Resource", "TW");
load(fullfile(input_folder_prefix, "preEnv.mat"), "mode", "scan");

receive             = Receive([Receive.bufnum] == 1);
receive_orientation = scan.TransmitEvents(1).ImagingPattern.ReceiveOrientation;
receive_elements    = receive_orientation.GetElementCount(scan.Die);

if (isempty(scan.TransmitsPerFrame))
	transmit_count = scan.AcquisitionCount;
else
	transmit_count = scan.TransmitsPerFrame;
end

bp.rf_raw_dim     = struct('x', rf_raw_dim(1), 'y', rf_raw_dim(2));
bp.dec_data_dim.x = 1 + Receive(1).endSample - Receive(1).startSample;
bp.dec_data_dim.y = receive_elements;
bp.dec_data_dim.z = transmit_count;

if (isfield(TW(1), "Parameters"))
    bp.center_frequency = TW(1).Parameters(1) * 1e6;
    bp.time_offset      = TW(1).Parameters(3) / bp.center_frequency;
else
    bp.center_frequency = die.CenterFrequency;
    bp.time_offset      = 0;
end

bp.sampling_frequency = receive(1).samplesPerWave * Trans.frequency(1) * 1e6;
bp.speed_of_sound     = Resource.Parameters.speedOfSound;

if (mode == "uforces")
	load(fullfile(input_folder_prefix, "preEnv.mat"), "sparseElements");
	bp.uforces_channels = sparseElements - 1;
else
	bp.uforces_channels = 0:(receive_elements - 1);
end

bp.channel_mapping = Trans.ConnectorES - 1;
bp.channel_offset  = 0 + receive_elements * double(receive_orientation.Contains(tobe.Orientation.Column));

% NOTE(rnp): only valid for single xdc
die_size       = scan.Die.GetSize();
bp.xdc_origin  = [-die_size(1) / 2, -die_size(2) / 2, 0, 0];
bp.xdc_corner1 = [ die_size(1) / 2, -die_size(2) / 2, 0, 0];
bp.xdc_corner2 = [-die_size(1) / 2,  die_size(2) / 2, 0, 0];
bp.xdc_count   = length(bp.xdc_origin) / 4;

bp.focal_depths    = [scan.TransmitEvents.FocalDepth];
bp.transmit_angles = [scan.TransmitEvents.SteeringAngle] * pi / 180;

% TODO: is mode saved anywhere besides the pre env?
switch (mode)
case {'forces', 'uforces'}
	bp.decode        = 1;
	bp.das_shader_id = uint8(OGLDasIDs.DAS_UFORCES);
case 'hercules'
	bp.decode        = 1;
	bp.das_shader_id = uint8(OGLDasIDs.DAS_HERCULES);
case {'vls', 'tpw'}
	bp.decode        = 0;
	bp.das_shader_id = uint8(OGLDasIDs.DAS_RCA);
end

bp.beamform_plane = 0;
bp.off_axis_pos   = 0;
bp.f_number       = 1;

%% Save Header
% NOTE(rnp): this will order the fields in the same order as the
% BeamformerParameters struct and perform validation
bp = struct(libstruct('BeamformerParameters', bp));

out_name = study;
fd = fopen(fullfile(output_folder_prefix, out_name + "_bp_inc.h"), "w");
fprintf(fd, "static i32 shader_stages[] = {\n");
fprintf(fd, "\t%s,\n", "CS_" + string(cs_stages));
fprintf(fd, "};\n\n");
fprintf(fd, "static BeamformerParameters bp = {\n");
fprintf(fd, "%s", sprint_struct(bp, 2, ".", ",", "__builtin_inf()"));
fprintf(fd, "};\n");
fclose(fd);

clear fd;

function [] = write_cleansed_data(data, output_filename)
	fd = fopen(output_filename, "w");
	fwrite(fd, int16(data), 'int16');
	fclose(fd);
end

function vec4 = make_vec4(array)
	if (numel(array) == 3)
		array = [array, 0];
	end
	vec4 = struct('x', array(1), 'y', array(2), 'z', array(3), 'w', array(4));
end
