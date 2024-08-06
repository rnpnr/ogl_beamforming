function [] = export_bp(output, bp)

fd = fopen(output, "w");

print_u32 = @(fd, u32) fprintf(fd, "%d", u32);
print_f32 = @(fd, f32) fprintf(fd, "%e", f32);
print_v2  = @(fd, v2)  fprintf(fd, "{.x = %0.03f, .y = %0.03f}", v2.x, v2.y);
print_v4  = @(fd, v4)  fprintf(fd, "{.x = %0.03f, .y = %0.03f, .z = %0.03f, .w = %0.03f}", v4.x, v4.y, v4.z, v4.w);
print_uv2 = @(fd, uv2) fprintf(fd, "{.x = %d, .y = %d}", uv2.x, uv2.y);
print_uv4 = @(fd, uv4) fprintf(fd, "{.x = %d, .y = %d, .z = %d, .w = %d}", uv4.x, uv4.y, uv4.z, uv4.w);
%print_arr = @(fd, arr) fprintf(fd, "{"); fprintf(fd, " %d,", arr); fprintf(fd, "}");

fprintf(fd, "static BeamformerParameters bp = {\n");
fprintf(fd, ".channel_mapping = {");      fprintf(fd, " %d,", bp.channel_mapping);  fprintf(fd, "},\n");
fprintf(fd, ".uforces_channels = {");     fprintf(fd, " %d,", bp.uforces_channels); fprintf(fd, "},\n");
fprintf(fd, ".lpf_coefficients = {");     fprintf(fd, " %e,", bp.lpf_coefficients); fprintf(fd, "},\n");
fprintf(fd, ".xdc_origin = {");           fprintf(fd, " %0.04f,", bp.xdc_origin);   fprintf(fd, "},\n");
fprintf(fd, ".xdc_corner1 = {");          fprintf(fd, " %0.04f,", bp.xdc_corner1);  fprintf(fd, "},\n");
fprintf(fd, ".xdc_corner2 = {");          fprintf(fd, " %0.04f,", bp.xdc_corner2);  fprintf(fd, "},\n");
fprintf(fd, ".dec_data_dim = ");          print_uv4(fd, bp.dec_data_dim);           fprintf(fd, ",\n");
fprintf(fd, ".output_points = ");         print_uv4(fd, bp.output_points);          fprintf(fd, ",\n");
fprintf(fd, ".output_min_coordinate = "); print_v4(fd, bp.output_min_coordinate);   fprintf(fd, ",\n");
fprintf(fd, ".output_max_coordinate = "); print_v4(fd, bp.output_max_coordinate);   fprintf(fd, ",\n");
fprintf(fd, ".rf_raw_dim = ");            print_uv2(fd, bp.rf_raw_dim);             fprintf(fd, ",\n");
fprintf(fd, ".xdc_count = ");             print_u32(fd, bp.xdc_count);              fprintf(fd, ",\n");
fprintf(fd, ".channel_offset = ");        print_u32(fd, bp.channel_offset);         fprintf(fd, ",\n");
fprintf(fd, ".lpf_order = ");             print_u32(fd, bp.lpf_order);              fprintf(fd, ",\n");
fprintf(fd, ".speed_of_sound = ");        print_f32(fd, bp.speed_of_sound);         fprintf(fd, ",\n");
fprintf(fd, ".sampling_frequency = ");    print_f32(fd, bp.sampling_frequency);     fprintf(fd, ",\n");
fprintf(fd, ".center_frequency = ");      print_f32(fd, bp.center_frequency);       fprintf(fd, ",\n");
fprintf(fd, ".focal_depth = ");           print_f32(fd, bp.focal_depth);            fprintf(fd, ",\n");
fprintf(fd, ".time_offset = ");           print_f32(fd, bp.time_offset);            fprintf(fd, ",\n");
fprintf(fd, ".uforces = ");               print_u32(fd, bp.uforces);                fprintf(fd, ",\n");
fprintf(fd, ".off_axis_pos = ");          print_f32(fd, bp.off_axis_pos);           fprintf(fd, ",\n");
fprintf(fd, ".beamform_plane= ");         print_u32(fd, bp.beamform_plane);         fprintf(fd, ",\n");
fprintf(fd, "};\n");

fclose(fd);

end
