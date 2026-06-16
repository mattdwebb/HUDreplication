# Build the comparison tables used in the comment.
#
# Usage:
#   Rscript make_comparison_tables.R
#   Rscript make_comparison_tables.R /absolute/output/dir
#
# The script reads C&T-sample comparison-estimate outputs from
# ct_sample/Output/comparison_table_estimates, corrected C&T-sample appendix CSVs
# from ct_sample/Output/corrected, and reconstructed-sample LaTeX outputs from
# reconstructed_sample/Tables/Formatted_Tables.

args <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(args) == 0) "comparison_tables" else args[[1]]

cmd_args <- commandArgs(FALSE)
file_arg <- grep("^--file=", cmd_args, value = TRUE)
script_dir <- if (length(file_arg) == 1) {
  script_arg <- sub("^--file=", "", file_arg)
  if (!file.exists(script_arg)) {
    script_arg <- gsub("~+~", " ", script_arg, fixed = TRUE)
  }
  dirname(normalizePath(script_arg))
} else {
  getwd()
}
setwd(script_dir)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

ct_comparison_estimates_dir <- file.path("ct_sample", "Output", "comparison_table_estimates")
ct_corrected_dir <- file.path("ct_sample", "Output", "corrected")
reconstructed_formatted_dir <- file.path("reconstructed_sample", "Tables", "Formatted_Tables")

mark_no <- "--"
spec_yes <- "Yes"
spec_no <- "No"
main_notes_label <- "tab:comparison_table_notes_main"
appendix_notes_label <- "tab:comparison_table_notes_appendix"

header_cell <- function(lines) {
  paste0("\\begin{tabular}[b]{@{}c@{}}", paste(lines, collapse = "\\\\"), "\\end{tabular}")
}

column_labels <- list(
  `1` = header_cell("Original"),
  `2` = header_cell(c("HDS Race", "Definition \\&", "Deduplicated")),
  `3` = header_cell(c("Standardized", "City FE")),
  `4` = header_cell(c("Advertised-Home", "City FE")),
  `5` = header_cell(c("Corrected C\\&T", "Sample")),
  `6` = header_cell(c("Reconstructed", "Sample"))
)

paired_header_label <- "Drop Trial-Invariant Controls"
model_col_width <- function(n_cols) {
  if (n_cols == 5L) "1.15in" else "1.25in"
}

fixed_header_col <- function(contents, logical_col, n_cols) {
  paste0(
    "\\multicolumn{1}{>{\\centering\\arraybackslash}p{", model_col_width(n_cols), "}}",
    "{\\makebox[\\linewidth][c]{", contents, "}}"
  )
}

paired_group_cell <- function(label) {
  paste0("\\multicolumn{2}{c}{\\makebox[0pt][c]{", label, "}}")
}

split_latex_cells <- function(x) {
  row_body <- sub("[[:space:]]*\\\\\\\\[[:space:]]*$", "", x)
  chars <- strsplit(row_body, "", fixed = TRUE)[[1]]
  cells <- character()
  buf <- character()
  prev <- ""

  for (ch in chars) {
    if (ch == "&" && prev != "\\") {
      cells <- c(cells, paste0(buf, collapse = ""))
      buf <- character()
    } else {
      buf <- c(buf, ch)
    }
    prev <- ch
  }

  trimws(c(cells, paste0(buf, collapse = "")))
}

is_table_row <- function(x) {
  y <- trimws(x)
  nzchar(y) &&
    !startsWith(y, "%") &&
    !startsWith(y, "\\") &&
    grepl("&", y, fixed = TRUE) &&
    grepl("\\\\\\\\[[:space:]]*$", y)
}

latex_row <- function(cells) {
  paste0(paste(cells, collapse = " & "), " \\\\")
}

read_source_table <- function(path) {
  lines <- if (length(path) == 1 && file.exists(path)) readLines(path, warn = FALSE) else path
  blocks <- list()
  summary <- list()
  valid_blocks <- c(
    "Racial Minority", "2", "3", "4", "5",
    "African American", "Hispanic", "Asian", "Other Race"
  )
  valid_summary <- c(
    "Observations", "Adjusted R$^2$", "Adjusted R$^2$ (Minority)",
    "Adjusted R$^2$ (Category)", "Number of Cities", "Number of Trials"
  )

  i <- 1
  while (i <= length(lines)) {
    if (!is_table_row(lines[[i]])) {
      i <- i + 1
      next
    }

    row <- split_latex_cells(lines[[i]])
    label <- gsub("[[:space:]]+", " ", trimws(row[[1]]))

    if (label %in% valid_blocks) {
      if (i + 2 > length(lines)) stop("Incomplete coefficient block in ", path)
      blocks[[label]] <- list(
        row,
        split_latex_cells(lines[[i + 1]]),
        split_latex_cells(lines[[i + 2]])
      )
      i <- i + 3
      next
    }

    if (label %in% valid_summary) {
      summary[[label]] <- row
    }

    i <- i + 1
  }

  list(blocks = blocks, summary = summary)
}

read_esttab_csv <- function(path) {
  raw <- read.csv(
    path,
    header = FALSE,
    check.names = FALSE,
    stringsAsFactors = FALSE,
    na.strings = character()
  )
  raw[] <- lapply(raw, function(x) trimws(sub("^=", "", x)))

  blocks <- list()
  summary <- list()
  valid_summary <- c(
    "Observations", "Adjusted R^2", "Number of Cities", "Number of Trials",
    "White SD", "White N"
  )

  i <- 1L
  while (i <= nrow(raw)) {
    label <- raw[i, 1]
    if (label == "ofcolor") label <- "Racial Minority"

    if (label %in% valid_summary) {
      summary[[if (label == "Adjusted R^2") "Adjusted R$^2$" else label]] <- as.character(unlist(raw[i, ]))
      i <- i + 1L
      next
    }

    if (nzchar(label) && i + 2L <= nrow(raw)) {
      blocks[[label]] <- list(
        as.character(unlist(raw[i, ])),
        as.character(unlist(raw[i + 1L, ])),
        as.character(unlist(raw[i + 2L, ]))
      )
      i <- i + 3L
      next
    }

    i <- i + 1L
  }

  list(blocks = blocks, summary = summary)
}

split_source_panels <- function(path) {
  lines <- readLines(path, warn = FALSE)
  panel_starts <- grep("Panel [A-D]|PANEL [A-D]", lines)

  if (length(panel_starts) > 0) {
    panel_ends <- c(panel_starts[-1] - 1, length(lines))
    return(Map(function(a, b) lines[a:b], panel_starts, panel_ends))
  }

  ranges <- list()
  depth <- 0L
  start <- NA_integer_

  for (i in seq_along(lines)) {
    begins <- gregexpr("\\\\begin\\{tabular\\}", lines[[i]], perl = TRUE)[[1]]
    ends <- gregexpr("\\\\end\\{tabular\\}", lines[[i]], perl = TRUE)[[1]]
    events <- data.frame(
      pos = c(if (begins[1] == -1) integer() else begins,
              if (ends[1] == -1) integer() else ends),
      type = c(rep("begin", if (begins[1] == -1) 0L else length(begins)),
               rep("end", if (ends[1] == -1) 0L else length(ends))),
      stringsAsFactors = FALSE
    )
    if (nrow(events) == 0) next
    events <- events[order(events$pos, ifelse(events$type == "begin", 0, 1)), ]

    for (j in seq_len(nrow(events))) {
      if (events$type[[j]] == "begin") {
        if (depth == 0L) start <- i
        depth <- depth + 1L
      } else {
        if (depth == 0L) stop("Unmatched \\end{tabular} in ", path)
        depth <- depth - 1L
        if (depth == 0L) {
          ranges[[length(ranges) + 1L]] <- c(start, i)
          start <- NA_integer_
        }
      }
    }
  }

  if (depth != 0L) stop("Unmatched \\begin{tabular} in ", path)
  if (length(ranges) == 0L) stop("Could not find top-level tabular panels in ", path)
  lapply(ranges, function(rng) lines[rng[[1]]:rng[[2]]])
}

extract_result_column <- function(lines, source_col) {
  rows <- read_source_table(lines)
  row_blocks <- list()
  scalar_rows <- list()

  for (label in names(rows$blocks)) {
    row_blocks[[label]] <- vapply(
      rows$blocks[[label]],
      function(x) x[[source_col + 1]],
      character(1)
    )
  }

  for (label in names(rows$summary)) {
    scalar_rows[[label]] <- rows$summary[[label]][[source_col + 1]]
  }

  list(row_blocks = row_blocks, scalar_rows = scalar_rows)
}

extract_csv_column <- function(stem, source_col) {
  rows <- read_esttab_csv(file.path(ct_corrected_dir, paste0(stem, ".csv")))
  row_blocks <- list()
  scalar_rows <- list()

  for (label in names(rows$blocks)) {
    row_blocks[[label]] <- vapply(
      rows$blocks[[label]],
      function(x) x[[source_col + 1]],
      character(1)
    )
  }

  for (label in names(rows$summary)) {
    scalar_rows[[label]] <- rows$summary[[label]][[source_col + 1]]
  }

  list(row_blocks = row_blocks, scalar_rows = scalar_rows, source_stem = stem, source_col = source_col)
}

category_block <- function(source, display_label, path) {
  aliases <- switch(
    display_label,
    "African American" = c("2", "African American"),
    "Hispanic" = c("3", "Hispanic"),
    "Asian" = c("4", "Asian"),
    "Other Race" = c("5", "Other Race")
  )

  for (alias in aliases) {
    if (!is.null(source$blocks[[alias]])) return(source$blocks[[alias]])
  }
  stop("Missing category block for ", display_label, " in ", path)
}

copy_values <- function(defaults, source_row, copy_cols) {
  out <- defaults
  for (col in copy_cols) out[[col]] <- source_row[[col + 1]]
  out
}

insert_column_values <- function(vals, source, target_col) {
  if (!is.null(source)) vals[[target_col]] <- source
  vals
}

select_display <- function(vals, display_cols) {
  vals[display_cols]
}

block_lines <- function(label, source_block, copy_cols, display_cols, within_block = NULL, reconstructed_sample_block = NULL) {
  defaults <- if (label == "Other Race") rep("", 6) else rep(mark_no, 6)

  out <- character()
  for (i in seq_along(source_block)) {
    vals <- copy_values(defaults, source_block[[i]], copy_cols)
    if (!is.null(within_block)) vals[[5]] <- within_block[[i]]
    if (!is.null(reconstructed_sample_block)) vals[[6]] <- reconstructed_sample_block[[i]]
    out <- c(out, latex_row(c(if (i == 1) label else "", select_display(vals, display_cols))))
  }
  out
}

summary_value <- function(source, label, copy_cols) {
  vals <- rep(mark_no, 6)
  if (!is.null(source$summary[[label]])) {
    vals <- copy_values(vals, source$summary[[label]], copy_cols)
  }
  vals
}

add_model_summary <- function(vals, source, label, target_col) {
  if (!is.null(source) && !is.null(source$scalar_rows[[label]])) {
    vals[[target_col]] <- source$scalar_rows[[label]]
  }
  vals
}

panel_header <- function(title, display_cols, single_panel, style, table_id, continuation_panel = FALSE) {
  title_prefix <- if (single_panel) "\t" else ""
  n_cols <- length(display_cols)
  cmidrules <- paste0(
    vapply(seq_len(n_cols), function(i) {
      paste0("\\cmidrule(lr){", i + 1, "-", i + 1, "}")
    }, character(1)),
    collapse = ""
  )
  number_row <- paste0("(", seq_along(display_cols), ")")

  is_table5 <- style == "table5"
  has_paired_sample_cols <- any(
    display_cols[-length(display_cols)] == 5 &
      display_cols[-1] == 6
  )
  has_group_header <- has_paired_sample_cols
  group_cells <- character()
  label_cells <- character()
  i <- 1L
  while (i <= length(display_cols)) {
    col <- display_cols[[i]]
    if (col == 5 && i < length(display_cols) && display_cols[[i + 1L]] == 6) {
      group_label <- paired_header_label
      group_cells <- c(group_cells, paired_group_cell(group_label))
      label_cells <- c(
        label_cells,
        fixed_header_col(column_labels[["5"]], 5L, n_cols),
        fixed_header_col(column_labels[["6"]], 6L, n_cols)
      )
      i <- i + 2L
      next
    }

    label <- column_labels[[as.character(col)]]
    group_cells <- c(group_cells, fixed_header_col("", col, n_cols))
    label_cells <- c(label_cells, fixed_header_col(label, col, n_cols))
    i <- i + 1L
  }

  header_lines <- c("\\toprule")
  if (continuation_panel) {
    header_lines <- c(header_lines, "\\addlinespace[0.3em]")
  }
  header_lines <- c(
    header_lines,
    paste0(title_prefix, "& \\multicolumn{", n_cols, "}{c}{", title, "}\\\\"),
    "\\toprule"
  )
  if (has_group_header) {
    header_lines <- c(
      header_lines,
      paste0(
        "                    &",
        paste(group_cells, collapse = "&"),
        "\\\\[-0.35ex]"
      )
    )
  }

  c(
    header_lines,
    paste0(
      "                    &",
      paste(label_cells, collapse = "&"),
      "\\\\"
    ),
    paste0("                    ", cmidrules),
    paste0(
      "                    &",
      paste(
        mapply(
          fixed_header_col,
          number_row,
          display_cols,
          MoreArgs = list(n_cols = n_cols),
          USE.NAMES = FALSE
        ),
        collapse = "   &"
      ),
      "\\\\"
    ),
    "\\midrule"
  )
}

spec_rows <- function(style, display_cols, table_id) {
  if (style == "table5") {
    rows <- list(
      c("HDS Race Definition", spec_no, spec_yes, spec_yes, mark_no, spec_yes, spec_yes),
      c("Duplicate Rows Removed", spec_no, spec_yes, spec_yes, mark_no, spec_yes, spec_yes),
      c("City Names Standardized", spec_no, spec_no, spec_yes, mark_no, mark_no, mark_no),
      c("City Fixed Effect", "Ad.", "Ad.", "Ad.", mark_no, mark_no, mark_no),
      c("Drop Trial-Invariant Controls", spec_no, spec_no, spec_no, mark_no, spec_yes, spec_yes),
      c("Reconstructed Sample", spec_no, spec_no, spec_no, mark_no, spec_no, spec_yes)
    )
  } else {
    completed_pair_col <- if (style == "reconstructed_sample") spec_yes else mark_no
    rows <- list(
      c("HDS Race Definition", spec_no, spec_yes, spec_yes, spec_yes, spec_yes, completed_pair_col),
      c("Duplicate Rows Removed", spec_no, spec_yes, spec_yes, spec_yes, spec_yes, completed_pair_col),
      c("City Names Standardized", spec_no, spec_no, spec_yes, spec_yes, mark_no, mark_no),
      c("City Fixed Effect", "Rec.", "Rec.", "Rec.", "Ad.", mark_no, mark_no),
      c("Drop Trial-Invariant Controls", spec_no, spec_no, spec_no, spec_no, spec_yes, completed_pair_col),
      c("Reconstructed Sample", spec_no, spec_no, spec_no, spec_no, spec_no, completed_pair_col)
    )
  }

  vapply(rows, function(row) latex_row(c(row[[1]], select_display(row[-1], display_cols))), character(1))
}

panel_lines <- function(panel, reconstructed_sample, within, panel_index, single_panel) {
  minority <- read_source_table(panel$minority)
  categories <- read_source_table(panel$categories)
  copy_cols <- panel$copy_cols
  display_cols <- panel$display_cols

  lines <- panel_header(
    panel$title,
    display_cols,
    single_panel,
    panel$style,
    panel$table_id,
    continuation_panel = panel_index > 1L
  )
  if (panel_index > 1L) {
    lines <- c("\\addlinespace[1.0em]", lines)
  }

  minority_reconstructed_sample_block <- if (isTRUE(panel$reconstructed_sample_no_minority)) {
    rep(mark_no, 3)
  } else if (is.null(reconstructed_sample)) {
    NULL
  } else {
    reconstructed_sample$row_blocks[["Racial Minority"]]
  }
  minority_within_block <- if (is.null(within)) NULL else within$minority$row_blocks[["Racial Minority"]]
  lines <- c(lines, block_lines(
    "Racial Minority",
    minority$blocks[["Racial Minority"]],
    copy_cols,
    display_cols,
    minority_within_block,
    minority_reconstructed_sample_block
  ))
  lines <- c(lines, "\\midrule")

  for (label in c("African American", "Hispanic", "Asian", "Other Race")) {
    reconstructed_sample_block <- if (!is.null(reconstructed_sample)) reconstructed_sample$row_blocks[[label]] else NULL
    within_block <- if (!is.null(within)) within$categories$row_blocks[[label]] else NULL
    lines <- c(lines, block_lines(
      label,
      category_block(categories, label, panel$categories),
      copy_cols,
      display_cols,
      within_block,
      reconstructed_sample_block
    ))
  }

  lines <- c(lines, "\\midrule", spec_rows(panel$style, display_cols, panel$table_id), "\\midrule")

  obs <- summary_value(minority, "Observations", copy_cols)
  obs <- add_model_summary(obs, if (is.null(within)) NULL else within$minority, "Observations", 5)
  obs <- add_model_summary(obs, reconstructed_sample, "Observations", 6)

  adj_cat <- summary_value(categories, "Adjusted R$^2$", copy_cols)
  adj_cat <- add_model_summary(adj_cat, if (is.null(within)) NULL else within$categories, "Adjusted R$^2$", 5)
  if (!is.null(reconstructed_sample)) {
    if (!is.null(reconstructed_sample$scalar_rows[["Adjusted R$^2$"]])) {
      adj_cat <- add_model_summary(adj_cat, reconstructed_sample, "Adjusted R$^2$", 6)
    } else if (!isTRUE(panel$reconstructed_sample_no_minority)) {
      adj_cat <- add_model_summary(adj_cat, reconstructed_sample, "Adjusted R$^2$ (Category)", 6)
    }
  }

  cities <- summary_value(minority, "Number of Cities", copy_cols)
  cities[[5]] <- mark_no
  cities[[6]] <- mark_no

  trials <- summary_value(minority, "Number of Trials", copy_cols)
  trials <- add_model_summary(trials, if (is.null(within)) NULL else within$minority, "Number of Trials", 5)
  trials <- add_model_summary(trials, reconstructed_sample, "Number of Trials", 6)

  lines <- c(
    lines,
    latex_row(c("Observations", select_display(obs, display_cols))),
    latex_row(c("Adjusted R$^2$", select_display(adj_cat, display_cols))),
    latex_row(c("Number of Cities", select_display(cities, display_cols))),
    latex_row(c("Number of Pairs", select_display(trials, display_cols))),
    "\\bottomrule"
  )

  lines
}

comparison_colspec <- function(n_cols) {
  paste0("l*{", n_cols, "}{>{\\centering\\arraybackslash}p{", model_col_width(n_cols), "}}")
}

table_header <- function(caption, label, n_cols, compact = FALSE, table_width = "\\textwidth") {
  lines <- c(
    "% Generated by make_comparison_tables.R",
    "",
    "\\begin{table}[htbp]\\centering",
    "\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}",
    paste0("\\caption{", caption, "}")
  )
  if (!is.na(label)) lines <- c(lines, paste0("\\label{", label, "}"))
  if (compact) {
    lines <- c(lines, "\\begingroup")
  }
  c(lines, paste0("\\resizebox{", table_width, "}{!}{"), paste0("\\begin{tabular}{", comparison_colspec(n_cols), "}"))
}

table_footer <- function(note, n_cols, compact = FALSE, table_width = "\\textwidth", notes_label = NA_character_) {
  table_note <- if (!is.na(notes_label)) {
    c(
      "\\vspace{0.5em}",
      paste0("\\caption*{\\footnotesize See notes for comparison tables on p.~\\pageref{", notes_label, "}.}")
    )
  }

  c(
    "\\end{tabular}",
    "}",
    if (compact) "\\endgroup",
    table_note,
    "\\end{table}"
  )
}

# Reconstructed-sample tables are parsed once here, then attached to the relevant panels below.
table5_reconstructed_sample <- readLines(file.path(reconstructed_formatted_dir, "table5.tex"), warn = FALSE)
table6_reconstructed_sample <- readLines(file.path(reconstructed_formatted_dir, "table6.tex"), warn = FALSE)
table7_reconstructed_sample <- readLines(file.path(reconstructed_formatted_dir, "table7.tex"), warn = FALSE)
table8_panels <- split_source_panels(file.path(reconstructed_formatted_dir, "table8.tex"))
table9_panels <- split_source_panels(file.path(reconstructed_formatted_dir, "table9.tex"))
table10_panels <- split_source_panels(file.path(reconstructed_formatted_dir, "table10.tex"))
table12_panels <- split_source_panels(file.path(reconstructed_formatted_dir, "table12.tex"))

reconstructed_sample_results <- list(
  table5_col1 = extract_result_column(table5_reconstructed_sample, 1),
  table5_col2 = extract_result_column(table5_reconstructed_sample, 2),
  table6_col1 = extract_result_column(table6_reconstructed_sample, 1),
  table7_col1 = extract_result_column(table7_reconstructed_sample, 1),
  table7_col3 = extract_result_column(table7_reconstructed_sample, 3),
  table8_panel1_col4 = extract_result_column(table8_panels[[1]], 4),
  table8_panel2_col1 = extract_result_column(table8_panels[[2]], 1),
  table9_panel1_col1 = extract_result_column(table9_panels[[1]], 1),
  table9_panel2_col1 = extract_result_column(table9_panels[[2]], 1),
  table10_panel1_col1 = extract_result_column(table10_panels[[1]], 1),
  table10_panel1_col2 = extract_result_column(table10_panels[[1]], 2),
  table10_panel2_col2 = extract_result_column(table10_panels[[2]], 2),
  table10_panel2_col4 = extract_result_column(table10_panels[[2]], 4),
  table12_panel1_col1 = extract_result_column(table12_panels[[1]], 1)
)

within_results <- list(
  table5_col2 = list(
    minority = extract_csv_column("table5_minority_corrected", 2),
    categories = extract_csv_column("table5_categories_corrected", 2)
  ),
  table5_col4 = list(
    minority = extract_csv_column("table5_minority_corrected", 4),
    categories = extract_csv_column("table5_categories_corrected", 4)
  ),
  table6_col5 = list(
    minority = extract_csv_column("table6_minority_corrected", 5),
    categories = extract_csv_column("table6_categories_corrected", 5)
  ),
  table7_col1 = list(
    minority = extract_csv_column("table7_minority_corrected", 1),
    categories = extract_csv_column("table7_categories_corrected", 1)
  ),
  table7_col3 = list(
    minority = extract_csv_column("table7_minority_corrected", 3),
    categories = extract_csv_column("table7_categories_corrected", 3)
  ),
  table8_panel1_col4 = list(
    minority = extract_csv_column("table8A2_minority_corrected", 2),
    categories = extract_csv_column("table8A2_categories_corrected", 2)
  ),
  table8_panel2_col1 = list(
    minority = extract_csv_column("table8B_minority_corrected", 1),
    categories = extract_csv_column("table8B_categories_corrected", 1)
  ),
  table9_panel1_col1 = list(
    minority = extract_csv_column("table9A_minority_corrected", 1),
    categories = extract_csv_column("table9A_categories_corrected", 1)
  ),
  table9_panel2_col1 = list(
    minority = extract_csv_column("table9B_minority_corrected", 1),
    categories = extract_csv_column("table9B_categories_corrected", 1)
  ),
  table10_panel1_col1 = list(
    minority = extract_csv_column("table10A1_minority_corrected", 1),
    categories = extract_csv_column("table10A1_categories_corrected", 1)
  ),
  table10_panel1_col2 = list(
    minority = extract_csv_column("table10A1_minority_corrected", 2),
    categories = extract_csv_column("table10A1_categories_corrected", 2)
  ),
  table10_panel2_col2 = list(
    minority = extract_csv_column("table10B_minority_corrected", 2),
    categories = extract_csv_column("table10B_categories_corrected", 2)
  ),
  table10_panel2_col4 = list(
    minority = extract_csv_column("table10B_minority_corrected", 4),
    categories = extract_csv_column("table10B_categories_corrected", 4)
  ),
  table11_col1 = list(
    minority = extract_csv_column("table11_minority_corrected", 1),
    categories = extract_csv_column("table11_categories_corrected", 1)
  ),
  table12_col1 = list(
    minority = extract_csv_column("table12_minority_corrected", 1),
    categories = extract_csv_column("table12_categories_corrected", 1)
  ),
  table14_col5 = list(
    minority = extract_csv_column("table14B_minority_corrected", 5),
    categories = extract_csv_column("table14B_categories_corrected", 5)
  )
)

table5_note <- "Note: Column 3 standardizes advertised-home city names; \\origpaper Table 5 does not use recommended-home city fixed effects."
table7_note <- paste0(
  "Note: ``Drop Trial-Invariant Controls'' removes trial-invariant controls relative to \\origpaper's original specification, ",
  "as in other tables, and also removes post-treatment controls, such as number of recommendations (STOTUNIT) and ",
  "whether the agent indicated the advertised home was available (SAVLBAD)."
)

table_specs <- data.frame(
  file = c("comparison_table5.tex", "comparison_table6.tex", "comparison_table7.tex",
           "comparison_table8.tex", "comparison_table9.tex", "comparison_table10A.tex",
           "comparison_table10B.tex", "comparison_table11.tex", "comparison_table12.tex",
           "comparison_table13.tex", "comparison_table14.tex"),
  caption = c("\\small Results Comparison for \\origpaper Table 5",
              "\\small Results Comparison for \\origpaper Table 6",
              "\\small Results Comparison for \\origpaper Table 7",
              "\\small Results Comparison for \\origpaper Table 8",
              "\\small Results Comparison for \\origpaper Table 9",
              "\\small Results Comparison for \\origpaper Table 10A",
              "\\small Results Comparison for \\origpaper Table 10B",
              "\\small Results Comparison for \\origpaper Table 11",
              "\\small Results Comparison for \\origpaper Table 12",
              "\\small Results Comparison for \\origpaper Table 13",
              "\\small Results Comparison for \\origpaper Table 14"),
  label = c("comtab:table5", "comtab:table6", "comtab:table7", "comtab:table8",
            "comtab:table9",
            "comtab:table10A", "comtab:table10B",
            "comtab:table11", "comptab:table12", NA, NA),
  note = I(list(table5_note, NA, table7_note, NA, NA, NA, NA, NA, NA, NA, NA)),
  compact = c(TRUE, rep(FALSE, 10)),
  table_width = c(
    "0.80\\textwidth", "0.94\\textwidth", "0.92\\textwidth",
    "0.92\\textwidth", "0.92\\textwidth", "0.92\\textwidth",
    "0.92\\textwidth", "0.94\\textwidth", "0.94\\textwidth",
    "0.78\\textwidth", "0.94\\textwidth"
  ),
  notes_label = c(
    main_notes_label, appendix_notes_label, main_notes_label,
    appendix_notes_label, main_notes_label, appendix_notes_label,
    appendix_notes_label, appendix_notes_label, appendix_notes_label,
    appendix_notes_label, appendix_notes_label
  ),
  stringsAsFactors = FALSE
)

panel_specs <- data.frame(
  file = c("comparison_table5.tex", "comparison_table5.tex", "comparison_table6.tex",
           "comparison_table7.tex", "comparison_table7.tex", "comparison_table8.tex",
           "comparison_table8.tex", "comparison_table9.tex", "comparison_table9.tex",
           "comparison_table10A.tex", "comparison_table10A.tex", "comparison_table10B.tex",
           "comparison_table10B.tex", "comparison_table11.tex", "comparison_table12.tex",
           "comparison_table13.tex", "comparison_table13.tex", "comparison_table14.tex"),
  panel_order = c(1, 2, 1, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 1, 1, 2, 1),
  panel_title = c("Panel A: \\origpaper Column 2 - Number of Recommendations",
                  "Panel B: \\origpaper Column 4 - Advertised Home Availability",
                  "Panel A: \\origpaper Column 5 - White Household Share",
                  "Panel A: \\origpaper Column 1 - White High-Income Household Share",
                  "Panel B: \\origpaper Column 3 - White Low-Income Household Share",
                  "Panel A: \\origpaper Panel A Column 4 - Elementary School Rating on Housing Search Platform",
                  "Panel B: \\origpaper Panel B Column 1 - Poverty Rate in Neighbourhood",
                  "Panel A: \\origpaper Panel A Column 1 - Superfund Sites",
                  "Panel B: \\origpaper Panel B Column 1 - Superfund Sites (Mothers Only)",
                  "Panel A: \\origpaper Panel A Column 1 - Elementary School Test Scores (Mothers Only)",
                  "Panel B: \\origpaper Panel A Column 2 - Middle School Test Scores (Mothers Only)",
                  "Panel A: \\origpaper Panel B Column 2 - High-Skill Workers in Neighbourhood",
                  "Panel B: \\origpaper Panel B Column 4 - Single-Parent Households in Neighbourhood",
                  "Panel A: \\origpaper Column 1 - Low-Poverty Neighbourhoods",
                  "Panel A: \\origpaper Column 1 - Median Income in Neighbourhood",
                  "Panel A: \\origpaper Panel A Column 4 - Elementary School Rating on Housing Search Platform",
                  "Panel B: \\origpaper Panel B Column 2 - High-Skill Workers in Neighbourhood",
                  "Panel A: \\origpaper Column 5 - Log Sale Price"),
  table_id = c("table5", "table5", "table6", "table7", "table7", "table8", "table8",
               "table9", "table9", "table10A", "table10A", "table10B", "table10B",
               "table11", "table12", "table13", "table13", "table14"),
  dep_var = c(1, 2, 1, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 1, 1, 2, 1),
  copy_cols = I(c(list(c(1, 2, 3), c(1, 2, 3)), rep(list(1:4), 16))),
  display_cols = I(c(list(c(1, 2, 3, 5, 6), c(1, 2, 3, 5, 6)), rep(list(1:6), 16))),
  style = c("table5", "table5", rep("reconstructed_sample", 11), "no_reconstructed_sample",
            "reconstructed_sample", "no_reconstructed_sample", "no_reconstructed_sample", "no_reconstructed_sample"),
  reconstructed_sample_key = c("table5_col1", "table5_col2", "table6_col1", "table7_col1",
                              "table7_col3", "table8_panel1_col4", "table8_panel2_col1",
                              "table9_panel1_col1", "table9_panel2_col1", "table10_panel1_col1",
                              "table10_panel1_col2", "table10_panel2_col2", "table10_panel2_col4",
                              NA, "table12_panel1_col1", NA, NA, NA),
  within_key = c("table5_col2", "table5_col4", "table6_col5", "table7_col1",
                 "table7_col3", "table8_panel1_col4", "table8_panel2_col1",
                 "table9_panel1_col1", "table9_panel2_col1", "table10_panel1_col1",
                 "table10_panel1_col2", "table10_panel2_col2", "table10_panel2_col4",
                 "table11_col1", "table12_col1", NA, NA, "table14_col5"),
  reconstructed_sample_no_minority = c(rep(FALSE, 14), TRUE, FALSE, FALSE, FALSE),
  stringsAsFactors = FALSE
)

table13_rows <- panel_specs$file == "comparison_table13.tex"
panel_specs$display_cols[table13_rows] <- rep(list(1:4), sum(table13_rows))

clean_scalar <- function(x) {
  if (is.null(x) || length(x) == 0L) return(NA_character_)
  y <- trimws(as.character(x[[1]]))
  if (!nzchar(y) || y %in% c(".", mark_no)) return(NA_character_)
  y
}

white_sd_rows <- list()
for (i in seq_len(nrow(panel_specs))) {
  panel_row <- panel_specs[i, ]
  if (is.na(panel_row$within_key)) next

  within_key <- as.character(panel_row$within_key)
  within <- within_results[[within_key]]
  if (is.null(within)) next

  for (analysis in c("minority", "categories")) {
    source <- within[[analysis]]
    if (is.null(source)) next
    source_stem <- if (is.null(source$source_stem)) NA_character_ else source$source_stem
    source_col <- if (is.null(source$source_col)) NA_integer_ else source$source_col
    white_sd_rows[[length(white_sd_rows) + 1L]] <- data.frame(
      comparison_table = sub("\\.tex$", "", panel_row$file),
      table_id = panel_row$table_id,
      panel_order = panel_row$panel_order,
      panel_title = panel_row$panel_title,
      sample_label = "Cleaned C&T Sample",
      comparison_column = 5L,
      analysis = analysis,
      within_key = within_key,
      source_csv = ifelse(is.na(source_stem), NA_character_, paste0(source_stem, ".csv")),
      source_column = source_col,
      white_sd = clean_scalar(source$scalar_rows[["White SD"]]),
      white_n = clean_scalar(source$scalar_rows[["White N"]]),
      stringsAsFactors = FALSE
    )
  }
}

white_sd_output <- do.call(rbind, white_sd_rows)
write.csv(
  white_sd_output,
  file.path(output_dir, "cleaned_ct_sample_white_sds.csv"),
  row.names = FALSE
)

for (i in seq_len(nrow(table_specs))) {
  table <- table_specs[i, ]
  table_panels <- panel_specs[panel_specs$file == table$file, ]
  table_panels <- table_panels[order(table_panels$panel_order), ]
  n_cols <- length(table_panels$display_cols[[1]])

  lines <- table_header(
    table$caption,
    table$label,
    n_cols,
    table$compact,
    table$table_width
  )

  for (j in seq_len(nrow(table_panels))) {
    panel_row <- table_panels[j, ]
    reconstructed_sample <- if (is.na(panel_row$reconstructed_sample_key)) {
      NULL
    } else {
      reconstructed_sample_results[[panel_row$reconstructed_sample_key]]
    }
    within <- if (is.na(panel_row$within_key)) NULL else within_results[[panel_row$within_key]]
    panel <- list(
      title = panel_row$panel_title,
      minority = file.path(ct_comparison_estimates_dir, paste0(panel_row$table_id, "_dep_var_", panel_row$dep_var, "_minority.tex")),
      categories = file.path(ct_comparison_estimates_dir, paste0(panel_row$table_id, "_dep_var_", panel_row$dep_var, "_categories.tex")),
      copy_cols = panel_row$copy_cols[[1]],
      display_cols = panel_row$display_cols[[1]],
      style = panel_row$style,
      table_id = panel_row$table_id,
      reconstructed_sample_no_minority = panel_row$reconstructed_sample_no_minority
    )
    lines <- c(lines, panel_lines(panel, reconstructed_sample, within, j, nrow(table_panels) == 1))
  }

  lines <- c(lines, table_footer(table$note[[1]], n_cols, table$compact, table$table_width, table$notes_label))
  lines <- gsub("{,}", "", lines, fixed = TRUE)
  writeLines(lines, file.path(output_dir, table$file), useBytes = TRUE)
}

message("Wrote ", nrow(table_specs), " comparison tables to ", normalizePath(output_dir))
message("Wrote cleaned C&T white-group SDs to ", normalizePath(file.path(output_dir, "cleaned_ct_sample_white_sds.csv")))
