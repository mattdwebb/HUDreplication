module FillCol6FromPanelSourceTools

export fill_col6_from_panel_source

function fill_col6_from_panel_source(targetfile::AbstractString,
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
                trials_val = get(active_source.scalar_rows, "Number of Trials", nothing)
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

            if length(cells) == 7
                label = normalize_label(cells[1])

                if !isempty(label)
                    current_label = label
                    continuation_index = 1

                    if label == "Number of Trials"
                        saw_trials_row = true
                        val = get(active_source.scalar_rows, "Number of Trials", nothing)
                        if val !== nothing
                            set_first_five_to_dash!(cells)
                            cells[7] = padded(val)
                            new_line = join_latex_row(cells, trail)
                        end

                    elseif label == "Number of Cities"
                        cells[7] = " - "
                        new_line = join_latex_row(cells, trail)

                    elseif haskey(active_source.scalar_rows, label)
                        cells[7] = padded(active_source.scalar_rows[label])
                        new_line = join_latex_row(cells, trail)

                    elseif haskey(active_source.row_blocks, label)
                        vals = active_source.row_blocks[label]
                        if !isempty(vals)
                            cells[7] = padded(vals[1])
                            new_line = join_latex_row(cells, trail)
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
            e = (j < length(starts)) ? starts[j+1] - 1 : length(lines)
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

        label = normalize_label(cells[1])
        value = strip(cells[source_col + 1])

        if isempty(label) && current_label === nothing
            continue
        end

        if !isempty(label)
            current_label = label

            if is_scalar_summary_row(label)
                scalar_rows[label] = value
            else
                row_blocks[label] = [value]
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
    return label == "Observations" ||
           label == "Adjusted R\$^2\$ (Minority)" ||
           label == "Adjusted R\$^2\$ (Category)" ||
           label == "Number of Trials" ||
           label == "Number of Cities"
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

end