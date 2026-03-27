module FillCol6FromPanelSourceNoMinorityTools

export fill_col6_from_panel_source_no_minority

function fill_col6_from_panel_source_no_minority(targetfile::AbstractString,
                                                 sourcefile::AbstractString,
                                                 top_source_panel::Int,
                                                 top_source_col::Int,
                                                 bottom_source_panel::Union{Int,Nothing},
                                                 bottom_source_col::Union{Int,Nothing},
                                                 outfile::AbstractString)

    src_lines = readlines(sourcefile)
    tgt_lines = readlines(targetfile)

    source_panels = extract_source_panels(src_lines)
    isempty(source_panels) && error("No source panels found in source file.")

    top_source = extract_source_column_from_panel(source_panels, top_source_panel, top_source_col)

    bottom_source = if isnothing(bottom_source_panel)
        nothing
    else
        isnothing(bottom_source_col) && error("bottom_source_col must be provided when bottom_source_panel is not nothing.")
        extract_source_column_from_panel(source_panels, bottom_source_panel, bottom_source_col)
    end

    out = String[]

    panel_idx = 0
    active_source = top_source
    current_label = nothing
    continuation_index = 0
    saw_trials_row = false

    for line in tgt_lines
        stripped = strip(line)

        if is_panel_marker(line)
            panel_idx += 1
            current_label = nothing
            continuation_index = 0
            saw_trials_row = false

            if panel_idx == 1
                active_source = top_source
            elseif panel_idx == 2
                active_source = bottom_source
            else
                active_source = nothing
            end

            push!(out, line)
            continue
        end

        if stripped == raw"\bottomrule"
            if active_source !== nothing && !saw_trials_row
                trials_val = get_scalar_value_for_target(active_source, "Number of Trials")
                if trials_val !== nothing
                    push!(out, make_trials_row(String(trials_val)))
                end
            end
            push!(out, line)
            current_label = nothing
            continuation_index = 0
            continue
        end

        new_line = line

        if active_source !== nothing && looks_like_table_row(line)
            cells, trail = split_latex_row(line)

            # target table: stub + 6 displayed columns = 7 cells
            if length(cells) == 7
                label_raw = normalize_label(cells[1])
                label_key = canonical_label(label_raw)

                if !isempty(label_raw)
                    current_label = label_key
                    continuation_index = 1

                    if label_key == "Number of Trials"
                        saw_trials_row = true
                        val = get_scalar_value_for_target(active_source, label_raw)
                        if val !== nothing
                            set_first_five_to_dash!(cells)
                            cells[7] = padded(val)
                            new_line = join_latex_row(cells, trail)
                        end

                    elseif label_key == "Number of Cities"
                        cells[7] = " - "
                        new_line = join_latex_row(cells, trail)

                    elseif label_key == "Racial Minority"
                        cells[7] = " - "
                        new_line = join_latex_row(cells, trail)

                    elseif label_key == "Adjusted R2 (Minority)"
                        cells[7] = " - "
                        new_line = join_latex_row(cells, trail)

                    else
                        scalar_val = get_scalar_value_for_target(active_source, label_raw)

                        if scalar_val !== nothing
                            cells[7] = padded(scalar_val)
                            new_line = join_latex_row(cells, trail)

                        elseif haskey(active_source.row_blocks, label_key)
                            vals = active_source.row_blocks[label_key]
                            if !isempty(vals)
                                cells[7] = padded(vals[1])
                                new_line = join_latex_row(cells, trail)
                            end
                        end
                    end

                else
                    if current_label !== nothing && haskey(active_source.row_blocks, current_label)
                        vals = active_source.row_blocks[current_label]
                        continuation_index += 1
                        if continuation_index <= length(vals)
                            cells[7] = padded(vals[continuation_index])
                            new_line = join_latex_row(cells, trail)
                        end
                    elseif current_label == "Racial Minority"
                        cells[7] = " - "
                        new_line = join_latex_row(cells, trail)
                    end
                end
            end
        end

        push!(out, new_line)
    end

    open(outfile, "w") do io
        for l in out
            println(io, l)
        end
    end

    return outfile
end

struct SourceColumnData
    row_blocks::Dict{String, Vector{String}}
    scalar_rows::Dict{String, String}
end

function extract_source_panels(lines::Vector{String})
    panel_ranges = Vector{Tuple{Int,Int}}()
    starts = Int[]

    for (i, line) in enumerate(lines)
        if is_panel_marker(line)
            push!(starts, i)
        end
    end

    if !isempty(starts)
        for j in 1:length(starts)
            s = starts[j]
            e = (j < length(starts)) ? starts[j + 1] - 1 : length(lines)
            push!(panel_ranges, (s, e))
        end
        return [lines[s:e] for (s, e) in panel_ranges]
    end

    blocks = Vector{Vector{String}}()
    current = String[]
    inside = false

    for line in lines
        if occursin(raw"\begin{tabular}", line)
            inside = true
            current = String[line]
            continue
        end

        if inside
            push!(current, line)
            if occursin(raw"\end{tabular}", line)
                push!(blocks, current)
                current = String[]
                inside = false
            end
        end
    end

    return blocks
end

function extract_source_column_from_panel(panels::Vector{Vector{String}},
                                          source_panel::Int,
                                          source_col::Int)
    source_panel >= 1 || error("source_panel must be >= 1, got $source_panel")
    source_col >= 1 || error("source_col must be >= 1, got $source_col")
    source_panel <= length(panels) || error("Requested source_panel=$source_panel, but source file has only $(length(panels)) panel(s).")

    lines = panels[source_panel]

    row_blocks = Dict{String, Vector{String}}()
    scalar_rows = Dict{String, String}()

    current_label = nothing
    max_result_cols_seen = 0

    for line in lines
        if !looks_like_table_row(line)
            continue
        end

        cells, _ = split_latex_row(line)
        n_result_cols = max(0, length(cells) - 1)
        max_result_cols_seen = max(max_result_cols_seen, n_result_cols)

        if length(cells) < source_col + 1
            continue
        end

        label_raw = normalize_label(cells[1])
        label_key = canonical_label(label_raw)
        value = strip(cells[source_col + 1])

        if isempty(label_raw) && current_label === nothing
            continue
        end

        if !isempty(label_raw)
            current_label = label_key

            if is_scalar_summary_row(label_raw)
                scalar_rows[label_key] = value
            else
                row_blocks[label_key] = [value]
            end
        else
            if current_label !== nothing && haskey(row_blocks, current_label)
                push!(row_blocks[current_label], value)
            end
        end
    end

    max_result_cols_seen == 0 && error("Could not detect any result columns in source panel $source_panel.")
    source_col > max_result_cols_seen && error("Requested source_col=$source_col, but source panel $source_panel appears to have only $max_result_cols_seen result column(s).")

    return SourceColumnData(row_blocks, scalar_rows)
end

function is_scalar_summary_row(label::AbstractString)
    key = canonical_label(label)
    return key == "Observations" ||
           key == "Adjusted R2 (Minority)" ||
           key == "Adjusted R2 (Category)" ||
           key == "Adjusted R2" ||
           key == "Number of Trials" ||
           key == "Number of Cities"
end

function canonical_label(label::AbstractString)
    s = strip(String(label))
    s = replace(s, r"\s+" => " ")

    # Normalize common LaTeX variants of Adjusted R^2
    s = replace(s, r"Adjusted\s+\$?R\$\^\{?2\}?\$?" => "Adjusted R2")
    s = replace(s, r"Adjusted\s+R\$\^\{?2\}?\$" => "Adjusted R2")
    s = replace(s, r"Adjusted\s+\$R\^\{?2\}?\$" => "Adjusted R2")

    if s == "Adjusted R2 (Category)" || s == "Adjusted R\$^2\$ (Category)"
        return "Adjusted R2 (Category)"
    elseif s == "Adjusted R2 (Minority)" || s == "Adjusted R\$^2\$ (Minority)"
        return "Adjusted R2 (Minority)"
    elseif s == "Adjusted R2"
        return "Adjusted R2"
    elseif s == "Racial Minority"
        return "Racial Minority"
    elseif s == "Observations"
        return "Observations"
    elseif s == "Number of Trials"
        return "Number of Trials"
    elseif s == "Number of Cities"
        return "Number of Cities"
    else
        return s
    end
end

function get_scalar_value_for_target(active_source::SourceColumnData, target_label::AbstractString)
    key = canonical_label(target_label)

    if haskey(active_source.scalar_rows, key)
        return active_source.scalar_rows[key]
    end

    # Critical mapping:
    # source "Adjusted R2" -> target "Adjusted R2 (Category)"
    if key == "Adjusted R2 (Category)" && haskey(active_source.scalar_rows, "Adjusted R2")
        return active_source.scalar_rows["Adjusted R2"]
    end

    return nothing
end

function is_panel_marker(line::AbstractString)
    s = strip(line)
    return occursin(r"^&\s*\\multicolumn\{6\}\{c\}\{Panel [A-Z]:", s) ||
           occursin(r"^&\s*\\multicolumn\{6\}\{c\}\{PANEL [A-Z]:", s)
end

function looks_like_table_row(line::AbstractString)
    s = strip(line)
    isempty(s) && return false
    startswith(s, raw"\cmidrule") && return false
    startswith(s, raw"\midrule") && return false
    startswith(s, raw"\toprule") && return false
    startswith(s, raw"\bottomrule") && return false
    startswith(s, raw"\begin{tabular}") && return false
    startswith(s, raw"\end{tabular}") && return false
    occursin("&", line) || return false
    occursin(raw"\\", line) || return false
    return true
end

function split_unescaped_ampersand(s::AbstractString)
    cells = String[]
    buf = IOBuffer()
    prev = '\0'

    for c in s
        if c == '&' && prev != '\\'
            push!(cells, String(take!(buf)))
        else
            print(buf, c)
        end
        prev = c
    end

    push!(cells, String(take!(buf)))
    return cells
end

function split_latex_row(line::AbstractString)
    m = match(r"^(.*?)(\\\\\s*)$", String(line))
    if m === nothing
        body = String(line)
        trail = ""
    else
        body = String(m.captures[1])
        trail = String(m.captures[2])
    end
    cells = split_unescaped_ampersand(body)
    return cells, trail
end

function join_latex_row(cells::AbstractVector{<:AbstractString}, trail::AbstractString)
    return join(cells, "&") * String(trail)
end

function normalize_label(x::AbstractString)
    y = strip(String(x))
    y = replace(y, r"\s+" => " ")
    return strip(y)
end

function padded(x::AbstractString)
    return " " * String(x) * " "
end

function set_first_five_to_dash!(cells::AbstractVector{<:AbstractString})
    for j in 2:6
        cells[j] = " - "
    end
    return cells
end

function make_trials_row(val::AbstractString)
    return "Number of Trials & - & - & - & - & - & " * String(val) * " \\\\"
end

end # module