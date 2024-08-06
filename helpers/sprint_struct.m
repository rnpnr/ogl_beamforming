function str = sprint_struct(s, indentation, element_prefix, element_suffix, inf_string)
    % SPRINT_STRUCT  stringize a struct

    arguments
        s              {mustBeA(s, "struct")}
        indentation    {mustBeNumeric, mustBeFinite} = 0
        element_prefix {mustBeText}                  = ""
        element_suffix {mustBeText}                  = ""
        inf_string     {mustBeText}                  = "inf"
    end

    fields = fieldnames(s);
    values = struct2cell(s);
    max_field_name_len = max(strlength(convertCharsToStrings(fields)));
    str = "";
    for k = 1:numel(fields)
        % NOTE(rnp): fix matlab's name munging
        if strncmp("m_", fields{k}, 2)
            fields{k} = fields{k}(2:end);
        end
        pad_len = max_field_name_len - length(fields{k});
        str = str + string(repelem(' ', indentation));
        str = str + element_prefix + fields{k} + string(repelem(' ', pad_len));
        str = str + " = ";

        if isstruct(values{k})
            value_pad_len = max_field_name_len + indentation + 5;
            str = str + sprintf("{\n");
            str = str + sprint_struct(values{k}, value_pad_len, ".", ",");
            str = str + string(repelem(' ', value_pad_len - 1)) + "}";
        elseif isnumeric(values{k})
            if isscalar(values{k})
                if isinf(values{k})
                    if values{k} < 0
                        str = str + "-";
                    end
                    str = str + inf_string;
                elseif round(values{k}) == values{k}
                    str = str + sprintf("%d", values{k});
                else
                    str = str + sprintf("%e", values{k});
                end
            else
                len = length(values{k});
                str = str + "{";
                str = str + sprintf("%d, ", values{k}(1:(len - 1)));
                str = str + sprintf("%d}",  values{k}(len));
            end
        else
            assert(1, "unhandled class: " + class(values{k}));
        end
        str = sprintf("%s%s\n", str, element_suffix);
    end
end
