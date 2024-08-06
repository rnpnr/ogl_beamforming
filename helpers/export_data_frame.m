vrs_path   = "/tmp/downloads";
vrs_prefix = "240905_ATS539_Resolution_uFORCES-16-TxRow";
vrs_num    = "_Intensity_06";

% vrs_path   = "C:\Vantage\Data\241203_ATS539_Resolution_FORCES-TxRow";
% vrs_prefix = "241203_ATS539_Resolution_FORCES-TxRow";
% vrs_num    = "_Intensity_06";

output_points            = struct('x', 2048, 'y', 1, 'z', 2048);
bp.output_min_coordinate = struct('x', -45e-3, 'y', 0, 'z', 5e-3,   'w', 0);
bp.output_max_coordinate = struct('x',  45e-3, 'y', 0, 'z', 165e-3, 'w', 0);

bp.das_shader_id = uint8(OGLDasIDs.DAS_UFORCES);
cs_stages = [
	OGLShaderStage.CUDA_DECODE, ...
	OGLShaderStage.CUDA_HILBERT, ...
	OGLShaderStage.DAS, ...
];

if ispc
	pipe_name = '\\.\pipe\beamformer_data_fifo';
	smem_name = 'Local\ogl_beamformer_parameters';
elseif isunix
	pipe_name = '/tmp/beamformer_data_fifo';
	smem_name = '/ogl_beamformer_parameters';
else
	assert(1, "Invalid Platform!")
end

vrs_name  = vrs_prefix + vrs_num + ".vrs";

vrs  = VRSFile(fullfile(vrs_path, vrs_name));
data = vrs.GetData();

load(fullfile(vrs_path, "postEnv.mat"), "Trans", "Receive", "Resource", "TW");
load(fullfile(vrs_path, "preEnv.mat"), "sparseElements", "scan");

receive             = Receive([Receive.bufnum] == 1);
receive_orientation = scan.TransmitEvents(1).ImagingPattern.ReceiveOrientation;
recieve_elements    = scan.TransmitEvents(1).ImagingPattern.ReceiveOrientation.GetElementCount(scan.Die);

bp.output_points.x = 256;
bp.output_points.y = 1;
bp.output_points.z = 1024;
bp.output_points.w = 0; % Number of frames for averaging

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

bp.channel_offset = 0 + recieve_elements * double(receive_orientation.Contains(tobe.Orientation.Column));

die_size       = scan.Die.GetSize();
bp.xdc_origin  = [-die_size(1) / 2, -die_size(2) / 2, 0, 0];
bp.xdc_corner1 = [ die_size(1) / 2, -die_size(2) / 2, 0, 0];
bp.xdc_corner2 = [-die_size(1) / 2,  die_size(2) / 2, 0, 0];
bp.xdc_count   = length(bp.xdc_origin) / 4;

bp.focal_depth  = 0;

% NOTE: plane and position along plane normal for beamforming 2D HERCULES
bp.beamform_plane = 0;
bp.off_axis_pos = 0;
bp.f_number = 1;

fc = bp.center_frequency;
fs = bp.sampling_frequency;

loadlibrary('ogl_beamformer_lib')
calllib('ogl_beamformer_lib', 'set_beamformer_parameters', smem_name, bp);
calllib('ogl_beamformer_lib', 'set_beamformer_pipeline', smem_name, uint8(cs_stages), numel(cs_stages));

% NOTE: matlab must be the one to allocate this if we want to get the data out (unless we want to
% use libmex alloc functions).
output_count  = output_points.x * output_points.y * output_points.z * 2; % (complex singles)
output_data   = libpointer('singlePtr', single(zeros(1, output_count)));
calllib('ogl_beamformer_lib', 'beamform_data_synchronized', ...
        pipe_name, smem_name, data, bp.rf_raw_dim, ...
        output_points, output_data);

beamformed = complex(output_data.Value(1:2:end), output_data.Value(2:2:end));
figure(); imagesc(20 * log10(abs(reshape(beamformed, [output_points.x, output_points.z])')))

out_name = vrs_prefix + vrs_num + "_beamformed";
fd = fopen(fullfile(vrs_path, out_name + ".bin"), "w");
fwrite(fd, beamformed, "single");
fclose(fd);

out_name = vrs_prefix;
bp.output_points = output_points;
% NOTE(rnp): this will order the fields in the same order as the
% BeamformerParameters struct and perform validation
bp = struct(libstruct('BeamformerParameters', bp));

fd = fopen(fullfile(vrs_path, out_name + "_params.txt"), "w");
fprintf(fd, "stages = {");
fprintf(fd, "%s, ", string(cs_stages(1:(numel(cs_stages) - 1))));
fprintf(fd, "%s}\n\n",  string(cs_stages(end)));
fprintf(fd, "%s", sprint_struct(bp));
fclose(fd);

fd = fopen(fullfile(vrs_path, out_name + "_bp_inc.h"), "w");
fprintf(fd, "static i32 shader_stages[] = {\n");
fprintf(fd, "\t%s,\n", "CS_" + string(cs_stages));
fprintf(fd, "};\n\n");
fprintf(fd, "static BeamformerParameters bp = {\n");
fprintf(fd, "%s", sprint_struct(bp, 2, ".", ","));
fprintf(fd, "};\n");
fclose(fd);

clear fd;
