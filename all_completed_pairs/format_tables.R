resolve_repo_root <- function() {
  cwd <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  if (basename(cwd) == "HUDreplication") return(cwd)
  if (basename(cwd) == "all_completed_pairs") return(dirname(cwd))
  candidate <- file.path(cwd, "HUDreplication")
  if (dir.exists(candidate)) return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
  stop("Could not infer repo_root. Run from HUDreplication or all_completed_pairs.")
}

repo_root <- resolve_repo_root()
root <- file.path(repo_root, "all_completed_pairs")
source_dir <- file.path(root, "Tables")
output_dir <- file.path(root, "Tables", "Formatted_Tables")

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

standard_note <- c(
  "Cluster-robust standard errors in parentheses; clustered at the trial level. 95\\% confidence intervals in square brackets.",
  "Adjusted R$^2$ corresponds to the regression with indicators for belonging to each racial/ethnic group.",
  "\\sym{*} \\(p<0.10\\), \\sym{**} \\(p<0.05\\), \\sym{***} \\(p<0.01\\)"
)
comparison_mean_note <- "Comparison mean (white) is the raw mean for white testers in the corresponding estimation sample."
sym_def <- "\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}"

single_tables <- c("table5", "table6", "table7", "table12")
panel_tables <- list(
  table8 = c("table8a", "table8b"),
  table9 = c("table9a", "table9b"),
  table10 = c("table10a", "table10b")
)

preview_lines <- c(
  "\\documentclass[11pt]{article}",
  "",
  "\\usepackage[margin=1in]{geometry}",
  "\\usepackage[T1]{fontenc}",
  "\\usepackage[utf8]{inputenc}",
  "\\usepackage{lmodern}",
  "\\usepackage{booktabs}",
  "\\usepackage{graphicx}",
  "\\usepackage{amsmath}",
  "\\usepackage{caption}",
  "",
  "\\title{Formatted Tables Preview}",
  "\\date{\\today}",
  "",
  "\\begin{document}",
  "",
  "\\maketitle",
  "",
  "\\section*{Main Tables}",
  "",
  "\\input{table5.tex}",
  "\\clearpage",
  "",
  "\\input{table6.tex}",
  "\\clearpage",
  "",
  "\\input{table7.tex}",
  "\\clearpage",
  "",
  "\\input{table8.tex}",
  "\\clearpage",
  "",
  "\\input{table9.tex}",
  "\\clearpage",
  "",
  "\\input{table10.tex}",
  "\\clearpage",
  "",
  "\\input{table12.tex}",
  "",
  "\\end{document}"
)

read_text <- function(path) {
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

extract_caption <- function(text, path) {
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  caption_idx <- grep("^\\\\caption\\{", lines)
  if (length(caption_idx) == 0) stop("Could not find caption in ", basename(path))
  raw <- trimws(lines[caption_idx[1]])
  sub("^\\\\caption\\{", "", sub("\\}$", "", raw))
}

extract_caption_parts <- function(raw_caption) {
  parts <- strsplit(raw_caption, "\\\\\\\\", perl = TRUE)[[1]]
  title <- trimws(parts[1])
  subtitle <- ""
  if (length(parts) > 1) {
    subtitle <- paste(parts[-1], collapse = "\\\\")
    subtitle <- sub("^\\[0\\.5em\\]", "", subtitle)
    subtitle <- trimws(subtitle)
  }
  list(title = title, subtitle = subtitle)
}

clean_combined_title <- function(title) {
  title <- gsub("\\s*\\(Panel [AB]:.*?\\)", "", title, perl = TRUE)
  title <- gsub("\\s*\\(Mothers, Panel [AB]\\)", " (Mothers)", title, perl = TRUE)
  trimws(title)
}

build_caption_line <- function(title, subtitle) {
  if (nzchar(subtitle)) {
    sprintf("\\caption{%s\\\\[0.5em]%s}", title, subtitle)
  } else {
    sprintf("\\caption{%s}", title)
  }
}

caption_title_overrides <- c(
  table5 = "Differences in Recommendations, Availability of Ad Properties and Appointments"
)

panel_title_overrides <- list(
  table8 = c(
    "School Quality and Neighborhood Safety",
    "American Community Survey Characteristics"
  ),
  table9 = c(
    "Differences for Entire Sample",
    "Differences for Sample of Mothers"
  ),
  table10 = c(
    "School Quality and Neighborhood Safety",
    "American Community Survey Characteristics"
  )
)

panel_width_overrides <- c(
  table9 = "0.86\\textwidth"
)

dependent_variable_headers <- list(
  table6 = "Dependent variable: Neighborhood household share by race",
  table7 = "Dependent variable: White household share by income",
  table12 = "Dependent variable: log(Median Income)"
)

table_extra_notes <- list(
  table6 = "Black refers to Black or African American."
)

extract_panel_title <- function(title) {
  panel_match <- regexec("Panel [AB]:\\s*(.*)\\)", title, perl = TRUE)
  panel_parts <- regmatches(title, panel_match)[[1]]
  if (length(panel_parts) > 1) return(trimws(panel_parts[2]))

  mothers_match <- regexec("\\(Mothers,\\s*Panel ([AB])\\)", title, perl = TRUE)
  mothers_parts <- regmatches(title, mothers_match)[[1]]
  if (length(mothers_parts) > 1) return(paste("Panel", mothers_parts[2]))

  title
}

extract_outer_tabular <- function(text, path) {
  begin_positions <- gregexpr("\\\\begin\\{tabular\\}", text, perl = TRUE)[[1]]
  end_positions <- gregexpr("\\\\end\\{tabular\\}", text, perl = TRUE)[[1]]

  if (begin_positions[1] == -1 || end_positions[1] == -1) {
    stop("Could not find tabular environment in ", basename(path))
  }

  events <- data.frame(
    pos = c(begin_positions, end_positions),
    type = c(rep("begin", length(begin_positions)), rep("end", length(end_positions))),
    stringsAsFactors = FALSE
  )
  events <- events[order(events$pos, ifelse(events$type == "begin", 0, 1)), ]

  depth <- 0L
  start <- NA_integer_
  end_token <- "\\end{tabular}"

  for (i in seq_len(nrow(events))) {
    if (events$type[i] == "begin") {
      if (depth == 0L) start <- events$pos[i]
      depth <- depth + 1L
    } else {
      depth <- depth - 1L
      if (depth == 0L && !is.na(start)) {
        end_pos <- events$pos[i] + nchar(end_token) - 1L
        return(substr(text, start, end_pos))
      }
    }
  }

  stop("Could not find matching outer tabular end in ", basename(path))
}

tabular_without_notes <- function(tabular) {
  lines <- strsplit(tabular, "\n", fixed = TRUE)[[1]]
  bottom_idx <- grep("\\\\bottomrule", lines)
  if (length(bottom_idx) == 0) stop("Missing \\bottomrule in tabular.")
  lines <- c(lines[seq_len(max(bottom_idx))], "\\end{tabular}")
  paste(lines, collapse = "\n")
}

format_tabular <- function(tabular, table_name) {
  lines <- strsplit(tabular, "\n", fixed = TRUE)[[1]]
  has_minority_block <- any(grepl("^Racial Minority[[:space:]]*&", lines))
  output <- character()

  for (line in lines) {
    trimmed <- trimws(line)

    if (grepl("^Adjusted R\\$\\^2\\$ \\(Minority\\)", trimmed)) next
    if (trimmed == "[1ex]") next

    line <- sub(
      "^Adjusted R\\$\\^2\\$ \\(Category\\)",
      "Adjusted R$^2$",
      line
    )

    if (!is.null(dependent_variable_headers[[table_name]])) {
      line <- sub(
        "Dependent [Vv]ariable",
        dependent_variable_headers[[table_name]],
        line
      )
    }

    line <- gsub("SEDA Elementary", "\\begin{tabular}{@{}c@{}}Elementary School\\\\Scores (Std.)\\end{tabular}", line, fixed = TRUE)
    line <- gsub("SEDA Middle", "\\begin{tabular}{@{}c@{}}Middle School\\\\Scores (Std.)\\end{tabular}", line, fixed = TRUE)
    line <- gsub("GreatSchools Elem", "GreatSchools Rating", line, fixed = TRUE)
    line <- gsub("Single Family", "\\begin{tabular}{@{}c@{}}Single-Parent\\\\Household\\end{tabular}", line, fixed = TRUE)
    line <- gsub("Toxics (RSEI)", "Toxics", line, fixed = TRUE)
    line <- gsub("PM2.5", "PM", line, fixed = TRUE)

    if (has_minority_block &&
        grepl("^African American[[:space:]]*&", trimmed) &&
        length(output) > 0 &&
        tail(output, 1) != "\\midrule") {
      output <- c(output, "\\midrule")
    }

    output <- c(output, line)
  }

  paste(output, collapse = "\n")
}

write_output <- function(lines, path) {
  if (file.exists(path)) unlink(path)
  writeLines(lines, path, useBytes = TRUE)
}

build_single_table <- function(name) {
  path <- file.path(source_dir, paste0(name, ".tex"))
  text <- read_text(path)
  caption_parts <- extract_caption_parts(extract_caption(text, path))
  if (name %in% names(caption_title_overrides)) {
    caption_parts$title <- caption_title_overrides[[name]]
  }
  tabular <- format_tabular(tabular_without_notes(extract_outer_tabular(text, path)), name)
  table_note <- c(comparison_mean_note, table_extra_notes[[name]], standard_note)

  c(
    "\\begin{table}[!htbp]",
    "\\centering",
    sym_def,
    build_caption_line(caption_parts$title, caption_parts$subtitle),
    sprintf("\\label{tab:%s}", name),
    "\\resizebox{\\textwidth}{!}{%",
    tabular,
    "}",
    "\\begin{minipage}{\\textwidth}",
    "\\footnotesize",
    table_note,
    "\\end{minipage}",
    "\\end{table}",
    ""
  )
}

build_panel_table <- function(output_name, panel_a_name, panel_b_name) {
  panel_a_path <- file.path(source_dir, paste0(panel_a_name, ".tex"))
  panel_b_path <- file.path(source_dir, paste0(panel_b_name, ".tex"))
  panel_a_text <- read_text(panel_a_path)
  panel_b_text <- read_text(panel_b_path)

  caption_parts <- extract_caption_parts(extract_caption(panel_a_text, panel_a_path))
  caption_title <- clean_combined_title(caption_parts$title)
  panel_a_title <- extract_panel_title(extract_caption_parts(extract_caption(panel_a_text, panel_a_path))$title)
  panel_b_title <- extract_panel_title(extract_caption_parts(extract_caption(panel_b_text, panel_b_path))$title)
  if (!is.null(panel_title_overrides[[output_name]])) {
    panel_a_title <- panel_title_overrides[[output_name]][[1]]
    panel_b_title <- panel_title_overrides[[output_name]][[2]]
  }
  panel_a_tabular <- format_tabular(tabular_without_notes(extract_outer_tabular(panel_a_text, panel_a_path)), output_name)
  panel_b_tabular <- format_tabular(tabular_without_notes(extract_outer_tabular(panel_b_text, panel_b_path)), output_name)
  table_note <- c(comparison_mean_note, standard_note)
  panel_width <- ifelse(
    output_name %in% names(panel_width_overrides),
    panel_width_overrides[[output_name]],
    "\\textwidth"
  )

  c(
    "\\begin{table}[!htbp]",
    "\\centering",
    sym_def,
    build_caption_line(caption_title, caption_parts$subtitle),
    sprintf("\\label{tab:%s}", output_name),
    sprintf("\\textbf{%s}\\\\[0.35em]", panel_a_title),
    sprintf("\\resizebox{%s}{!}{%%", panel_width),
    panel_a_tabular,
    "}",
    "\\par\\vspace*{0.35em}",
    sprintf("\\textbf{%s}\\\\[0.35em]", panel_b_title),
    "",
    sprintf("\\resizebox{%s}{!}{%%", panel_width),
    panel_b_tabular,
    "}",
    "\\begin{minipage}{\\textwidth}",
    "\\footnotesize",
    table_note,
    "\\end{minipage}",
    "\\end{table}",
    ""
  )
}

for (table_name in single_tables) {
  output <- build_single_table(table_name)
  write_output(output, file.path(output_dir, paste0(table_name, ".tex")))
}

for (output_name in names(panel_tables)) {
  parts <- panel_tables[[output_name]]
  output <- build_panel_table(output_name, parts[1], parts[2])
  write_output(output, file.path(output_dir, paste0(output_name, ".tex")))
}

write_output(preview_lines, file.path(output_dir, "formatted_tables_preview.tex"))

manifest <- c(
  "Generated formatted main-table outputs.",
  "",
  "Single-table outputs:",
  paste("- ", single_tables, ".tex", sep = ""),
  "",
  "Combined panel outputs:",
  paste0(
    "- ",
    names(panel_tables),
    ".tex combines ",
    vapply(panel_tables, `[`, "", 1),
    ".tex + ",
    vapply(panel_tables, `[`, "", 2),
    ".tex"
  ),
  "",
  "Source directory: Tables/",
  "Output directory: Tables/Formatted_Tables/",
  "Canonical generator: format_tables.R",
  "Preview document: formatted_tables_preview.tex"
)
write_output(manifest, file.path(output_dir, "README.txt"))
