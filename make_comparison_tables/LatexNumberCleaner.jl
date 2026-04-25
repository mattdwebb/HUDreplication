module LatexNumberCleaner

export clean_latex_numbers!

"""
    clean_latex_numbers!(input_path; output_path=nothing)

Remove LaTeX thousand separators `{,}` that appear between digits,
e.g. `6{,}027` → `6027`.

- If `output_path` is provided, writes to a new file.
- Otherwise overwrites the original file.
"""
function clean_latex_numbers!(input_path::String; output_path::Union{Nothing,String}=nothing)
    lines = readlines(input_path)

    # regex: match "{,}" only when it is between digits
    pattern = r"(?<=\d)\{,\}(?=\d)"

    cleaned_lines = [replace(line, pattern => "") for line in lines]

    out_path = isnothing(output_path) ? input_path : output_path

    open(out_path, "w") do io
        for line in cleaned_lines
            println(io, line)
        end
    end

    return out_path
end

end