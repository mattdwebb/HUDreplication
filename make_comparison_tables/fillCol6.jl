module FillCol6Tools

export fill_col6_from_source

function fill_col6_from_source(emptyfile::AbstractString,
                               sourcefile::AbstractString,
                               top_source_col::Int,
                               bottom_source_col::Union{Int,Nothing},
                               outfile::AbstractString)

    src_lines = readlines(sourcefile)
    tgt_lines = readlines(emptyfile)

    src_top = extract_source_column(src_lines, top_source_col)
    src_bottom = isnothing(bottom_source_col) ? nothing : extract_source_column(src_lines, bottom_source_col)

    out = String[]

    panel_idx = 0
    saw_any_panel_marker = false
    active_source = src_top   # default: treat whole table as top panel unless panel markers appear

    current_label = nothing
    continuation_index = 0
    saw_trials_row = false

    for line in tgt_lines
        stripped = strip(line)

        # Detect panel markers
        if is_panel_marker(line)
            saw_any_panel_marker = true
            panel_idx += 1
            current_label = nothing
            continuation_index = 0
            saw_trials_row = false

            if panel_idx == 1
                active_source = src_top
            elseif panel_idx == 2
                active_source = src_bottom
            else
                active_source = nothing
            end

            push!(out, line)
            continue
        end

        # Before closing a processed panel, insert Number of Trials if needed
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

            # target table: stub + 6 displayed columns = 7 cells
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
                        val = active_source.scalar_rows[label]
                        cells[7] = padded(val)
                        new_line = join_latex_row(cells, trail)

                    elseif haskey(active_source.row_blocks, label)
                        vals = active_source.row_blocks[label]
                        if !isempty(vals)
                            cells[7] = padded(vals[1])
                            new_line = join_latex_row(cells, trail)
                        end
                    end

                else
                    # continuation row: standard errors / confidence intervals / etc.
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

# --------------------------------------------------
# Internal container
# --------------------------------------------------

struct SourceColumnData
    row_blocks::Dict{String, Vector{String}}
    scalar_rows::Dict{String, String}
end

# --------------------------------------------------
# Parse one selected result column from source table
# --------------------------------------------------

function extract_source_column(lines::Vector{String}, source_col::Int)
    source_col >= 1 || error("source_col must be >= 1, got $source_col")

    row_blocks = Dict{String, Vector{String}}()
    scalar_rows = Dict{String, String}()

    in_body = false
    current_label = nothing
    max_result_cols_seen = 0

    for line in lines
        s = strip(line)

        if s == raw"\midrule"
            in_body = true
            current_label = nothing
            continue
        end

        if s == raw"\bottomrule"
            break
        end

        if !in_body || !looks_like_table_row(line)
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

    if max_result_cols_seen == 0
        error("Could not detect any result columns in source table.")
    end

    if source_col > max_result_cols_seen
        error("Requested source_col=$source_col, but source table appears to have only $max_result_cols_seen result column(s).")
    end

    return SourceColumnData(row_blocks, scalar_rows)
end

# --------------------------------------------------
# Helpers
# --------------------------------------------------

function is_scalar_summary_row(label::AbstractString)
    return label == "Observations" ||
           label == "Adjusted R\$^2\$ (Minority)" ||
           label == "Adjusted R\$^2\$ (Category)" ||
           label == "Number of Trials" ||
           label == "Number of Cities"
end

function is_panel_marker(line::AbstractString)
    s = strip(line)
    return occursin("Panel A", s) ||
           occursin("Panel B", s) ||
           occursin("Panel C", s) ||
           occursin("Panel D", s) ||
           occursin("PANEL A", s) ||
           occursin("PANEL B", s) ||
           occursin("PANEL C", s) ||
           occursin("PANEL D", s)
end

function looks_like_table_row(line::AbstractString)
    s = strip(line)
    isempty(s) && return false
    startswith(s, raw"\cmidrule") && return false
    startswith(s, raw"\midrule") && return false
    startswith(s, raw"\toprule") && return false
    startswith(s, raw"\bottomrule") && return false
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