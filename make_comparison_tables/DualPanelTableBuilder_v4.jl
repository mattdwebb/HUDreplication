module DualPanelTableBuilder

export PanelSources, build_table, fill_panel!, extract_source_blocks, parse_args, parse_copy_cols, main

struct PanelSources
    minority_path::String
    categories_path::String
end

const CATEGORY_DISPLAY_ORDER = [
    "African American",
    "Hispanic",
    "Asian",
    "Other Race",
]

const CATEGORY_SOURCE_ALIASES = Dict(
    "African American" => ["2", "African American"],
    "Hispanic"         => ["3", "Hispanic"],
    "Asian"            => ["4", "Asian"],
    "Other Race"       => ["5", "Other Race"],
)

function parse_args(args::Vector{String})
    opts = Dict{String,String}()
    i = 1
    while i <= length(args)
        arg = args[i]
        startswith(arg, "--") || error("Unexpected positional argument: $arg")
        key = arg[3:end]
        if i == length(args) || startswith(args[i + 1], "--")
            opts[key] = "true"
            i += 1
        else
            opts[key] = args[i + 1]
            i += 2
        end
    end
    return opts
end

required(opts::Dict{String,String}, key::String) = get(opts, key) do
    error("Missing required option --$key")
end

function parse_copy_cols(s::String)
    cols = sort(unique(parse.(Int, strip.(split(s, ',')))))
    all(1 .<= cols .<= 6) || error("--copy-cols must only contain integers from 1 to 6")
    return cols
end

function strip_row_ending(s::AbstractString)
    s2 = strip(String(s))
    s2 = replace(s2, r"\s*\\\\\s*$" => "")
    s2 = replace(s2, r"\s*\\\s*$" => "")
    return s2
end

function parse_latex_row(line::AbstractString)
    return String.(strip.(split(strip_row_ending(line), '&')))
end

function format_latex_row(cells::AbstractVector{<:AbstractString})
    clean = [replace(String(c), r"\s*\\\\\s*$" => "") for c in cells]
    clean = [replace(c, r"\s*\\\s*$" => "") for c in clean]
    return join(clean, " & ") * " \\\\"
end

function is_data_row(line::AbstractString)
    s = strip(String(line))
    isempty(s) && return false
    startswith(s, "%") && return false
    startswith(s, "\\") && return false
    return occursin('&', s) && occursin(r"\\\s*$", s)
end

function extract_source_blocks(path::String)
    lines = readlines(path)
    blocks = Dict{String,Vector{Vector{String}}}()
    summary = Dict{String,Vector{String}}()

    valid_category_labels = Set([
        "2", "3", "4", "5",
        "African American", "Hispanic", "Asian", "Other Race",
    ])

    i = 1
    while i <= length(lines)
        line = lines[i]

        if !is_data_row(line)
            i += 1
            continue
        end

        row = parse_latex_row(line)
        isempty(row) && (i += 1; continue)
        label = strip(row[1])

        if label == "Racial Minority" || (label in valid_category_labels)
            i + 2 <= length(lines) || error("Incomplete 3-line coefficient block for '$label' in $path")
            row2 = parse_latex_row(lines[i + 1])
            row3 = parse_latex_row(lines[i + 2])
            blocks[label] = [row, row2, row3]
            i += 3
            continue
        end

        if label == "Observations" || label == "Adjusted R\$^2\$" || label == "Number of Cities"
            summary[label] = row
            i += 1
            continue
        end

        i += 1
    end

    return blocks, summary
end

function find_panel_start(lines::Vector{String}, title::String)
    target = "& \\multicolumn{6}{c}{$title}\\\\"
    for i in eachindex(lines)
        if strip(lines[i]) == strip(target)
            return i
        end
    end
    error("Could not find panel title in template: $title")
end

function find_next_bottomrule(lines::Vector{String}, start_idx::Int)
    for i in start_idx + 1:length(lines)
        if strip(lines[i]) == "\\bottomrule"
            return i
        end
    end
    error("Could not find the next \\bottomrule after index $start_idx")
end

function find_label_row(lines::Vector{String}, panel_start::Int, panel_end::Int, label::String)
    for i in panel_start:panel_end
        if !is_data_row(lines[i])
            continue
        end
        row = parse_latex_row(lines[i])
        !isempty(row) || continue
        if strip(row[1]) == label
            return i
        end
    end
    error("Could not find row '$label' inside the target panel")
end

function replace_selected_columns!(
    target_row::AbstractVector{<:AbstractString},
    source_row::AbstractVector{<:AbstractString},
    copy_cols::Vector{Int},
)
    length(target_row) >= 7 || error("Target row has fewer than 7 cells")
    length(source_row) >= 7 || error("Source row has fewer than 7 cells")

    for c in copy_cols
        1 <= c <= 6 || error("copy_cols entries must be between 1 and 6")
        target_row[c + 1] = String(source_row[c + 1])
    end
    return nothing
end

function fill_three_line_block!(lines::Vector{String}, row_idx::Int, source_block::Vector{Vector{String}}, copy_cols::Vector{Int})
    for k in 0:2
        target_row = parse_latex_row(lines[row_idx + k])
        replace_selected_columns!(target_row, source_block[k + 1], copy_cols)
        lines[row_idx + k] = format_latex_row(target_row)
    end
    return nothing
end

function fill_one_line_row!(lines::Vector{String}, row_idx::Int, source_row::Vector{String}, copy_cols::Vector{Int})
    target_row = parse_latex_row(lines[row_idx])
    replace_selected_columns!(target_row, source_row, copy_cols)
    lines[row_idx] = format_latex_row(target_row)
    return nothing
end

function replace_exact_line!(lines::Vector{String}, old_line::String, new_line::String)
    for i in eachindex(lines)
        if strip(lines[i]) == strip(old_line)
            lines[i] = new_line
            return nothing
        end
    end
    error("Could not find line to replace:\n$old_line")
end

function get_category_block(blocks_cat::Dict{String,Vector{Vector{String}}}, display_label::String, categories_path::String)
    aliases = get(CATEGORY_SOURCE_ALIASES, display_label, String[])
    for key in aliases
        if haskey(blocks_cat, key)
            return blocks_cat[key]
        end
    end
    error("Missing category block for '$display_label' in $categories_path. Tried source labels: " * join(aliases, ", "))
end

function relabel_three_line_block!(lines::Vector{String}, row_idx::Int, new_label::String)
    for k in 0:2
        row_cells = parse_latex_row(lines[row_idx + k])
        row_cells[1] = (k == 0 ? new_label : "")
        lines[row_idx + k] = format_latex_row(row_cells)
    end
    return nothing
end

function fill_panel!(lines::Vector{String}, panel_title::String, minority_path::String, categories_path::String, copy_cols::Vector{Int})
    blocks_min, summary_min = extract_source_blocks(minority_path)
    blocks_cat, summary_cat = extract_source_blocks(categories_path)

    panel_start = find_panel_start(lines, panel_title)
    panel_end = find_next_bottomrule(lines, panel_start)

    haskey(blocks_min, "Racial Minority") || error("Missing minority block 'Racial Minority' in $minority_path")
    minority_row = find_label_row(lines, panel_start, panel_end, "Racial Minority")
    fill_three_line_block!(lines, minority_row, blocks_min["Racial Minority"], copy_cols)

    for display_label in CATEGORY_DISPLAY_ORDER
        row_idx = find_label_row(lines, panel_start, panel_end, display_label)
        source_block = get_category_block(blocks_cat, display_label, categories_path)
        fill_three_line_block!(lines, row_idx, source_block, copy_cols)
        relabel_three_line_block!(lines, row_idx, display_label)
    end

    haskey(summary_min, "Observations") || error("Minority source missing 'Observations' row")
    obs_idx = find_label_row(lines, panel_start, panel_end, "Observations")
    fill_one_line_row!(lines, obs_idx, summary_min["Observations"], copy_cols)

    haskey(summary_min, "Adjusted R\$^2\$") || error("Minority source missing 'Adjusted R\$^2\$' row")
    adj_min_idx = find_label_row(lines, panel_start, panel_end, "Adjusted R\$^2\$ (Minority)")
    fill_one_line_row!(lines, adj_min_idx, summary_min["Adjusted R\$^2\$"], copy_cols)

    haskey(summary_cat, "Adjusted R\$^2\$") || error("Category source missing 'Adjusted R\$^2\$' row")
    adj_cat_idx = find_label_row(lines, panel_start, panel_end, "Adjusted R\$^2\$ (Category)")
    fill_one_line_row!(lines, adj_cat_idx, summary_cat["Adjusted R\$^2\$"], copy_cols)

    haskey(summary_min, "Number of Cities") || error("Minority source missing 'Number of Cities' row")
    ncity_idx = find_label_row(lines, panel_start, panel_end, "Number of Cities")
    fill_one_line_row!(lines, ncity_idx, summary_min["Number of Cities"], copy_cols)

    return nothing
end

function build_table(template_path::String;
    top::PanelSources,
    bottom::PanelSources,
    output_path::String,
    copy_cols::Vector{Int},
    caption::Union{Nothing,String}=nothing,
    top_title::Union{Nothing,String}=nothing,
    bottom_title::Union{Nothing,String}=nothing,
)
    lines = readlines(template_path)

    if caption !== nothing
        replace_exact_line!(lines, "\\caption{TABLE CAPTION}", "\\caption{$caption}")
    end

    actual_top_title = something(top_title, "TOP PANEL TITLE")
    actual_bottom_title = something(bottom_title, "BOTTOM PANEL TITLE")

    if top_title !== nothing
        replace_exact_line!(lines, "& \\multicolumn{6}{c}{TOP PANEL TITLE}\\\\", "& \\multicolumn{6}{c}{$actual_top_title}\\\\")
    end
    if bottom_title !== nothing
        replace_exact_line!(lines, "& \\multicolumn{6}{c}{BOTTOM PANEL TITLE}\\\\", "& \\multicolumn{6}{c}{$actual_bottom_title}\\\\")
    end

    fill_panel!(lines, actual_top_title, top.minority_path, top.categories_path, copy_cols)
    fill_panel!(lines, actual_bottom_title, bottom.minority_path, bottom.categories_path, copy_cols)

    open(output_path, "w") do io
        write(io, join(lines, "\n") * "\n")
    end

    return output_path
end

function main(args::Vector{String}=ARGS)
    opts = parse_args(args)

    template = required(opts, "template")
    output = required(opts, "output")
    copy_cols = parse_copy_cols(get(opts, "copy-cols", "1,2,3,4,5,6"))

    top = PanelSources(
        required(opts, "top-minority"),
        required(opts, "top-categories"),
    )

    bottom = PanelSources(
        required(opts, "bottom-minority"),
        required(opts, "bottom-categories"),
    )

    build_table(template;
        top = top,
        bottom = bottom,
        output_path = output,
        copy_cols = copy_cols,
        caption = get(opts, "caption", nothing),
        top_title = get(opts, "top-title", nothing),
        bottom_title = get(opts, "bottom-title", nothing),
    )

    println("Wrote output to: $output")
    println("Copied columns: " * join(string.(copy_cols), ", "))

    return output
end

if abspath(PROGRAM_FILE) == @__FILE__
    main()
end

end # module
