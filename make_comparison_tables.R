# Build the comparison tables used in the comment.
#
# Usage:
#   Rscript make_comparison_tables.R
#   Rscript make_comparison_tables.R /absolute/output/dir
#
# The script reads selected-sample Stata LaTeX outputs from selected_sample/Output,
# within-trial-control Stata CSV outputs from selected_sample/Output/Within-Trial-Control Tables,
# and all-completed-pairs LaTeX outputs from all_completed_pairs/Tables/Formatted_Tables.

args <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(args) == 0) "Comparison_Tables" else args[[1]]

cmd_args <- commandArgs(FALSE)
file_arg <- grep("^--file=", cmd_args, value = TRUE)
script_dir <- if (length(file_arg) == 1) {
  dirname(normalizePath(sub("^--file=", "", file_arg)))
} else {
  getwd()
}
setwd(script_dir)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

selected_sample_dir <- file.path("selected_sample", "Output")
within_trial_dir <- file.path(
  "selected_sample", "Output", "Within-Trial-Control Tables", "corrected_full"
)
all_completed_pairs_dir <- file.path("all_completed_pairs", "Tables", "Formatted_Tables")

mark_no <- "--"
spec_yes <- "Yes"
spec_no <- "No"

column_labels <- list(
  `1` = "Original",
  `2` = "\\begin{tabular}{@{}c@{}}HDS Race\\\\Definition \\&\\\\Deduplicated\\end{tabular}",
  `3` = "\\begin{tabular}{@{}c@{}}Standardized\\\\City FE\\end{tabular}",
  `4` = "\\begin{tabular}{@{}c@{}}Advertised-Home\\\\City FE\\end{tabular}",
  `5` = "Selected Sample",
  `6` = "All Completed Pairs"
)

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
  valid_summary <- c("Observations", "Adjusted R^2", "Number of Cities", "Number of Trials")

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

  starts <- grep("\\\\begin\\{tabular\\}", lines)
  ends <- grep("\\\\end\\{tabular\\}", lines)
  Map(function(a, b) lines[a:b], starts, ends)
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
  rows <- read_esttab_csv(file.path(within_trial_dir, paste0(stem, ".csv")))
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

block_lines <- function(label, source_block, copy_cols, display_cols, within_block = NULL, all_completed_pairs_block = NULL) {
  defaults <- if (label == "Other Race") rep("", 6) else rep(mark_no, 6)

  out <- character()
  for (i in seq_along(source_block)) {
    vals <- copy_values(defaults, source_block[[i]], copy_cols)
    if (!is.null(within_block)) vals[[5]] <- within_block[[i]]
    if (!is.null(all_completed_pairs_block)) vals[[6]] <- all_completed_pairs_block[[i]]
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

panel_header <- function(title, display_cols, single_panel, style, table_id) {
  title_prefix <- if (single_panel) "\t" else ""
  n_cols <- length(display_cols)
  cmidrules <- paste0(
    vapply(seq_len(n_cols), function(i) {
      paste0("\\cmidrule(lr){", i + 1, "-", i + 1, "}")
    }, character(1)),
    collapse = ""
  )
  number_row <- paste0("(", seq_along(display_cols), ")")

  top_cells <- character()
  sub_cells <- character()
  i <- 1L
  while (i <= length(display_cols)) {
    col <- display_cols[[i]]
    if (col == 5 && i < length(display_cols) && display_cols[[i + 1L]] == 6) {
      control_label <- if (table_id == "table7") "Correct Controls Only" else "Within-Trial Controls Only"
      top_cells <- c(top_cells, paste0("\\multicolumn{2}{c}{", control_label, "}"))
      sub_cells <- c(
        sub_cells,
        paste0("\\multicolumn{1}{c}{", column_labels[["5"]], "}"),
        paste0("\\multicolumn{1}{c}{", column_labels[["6"]], "}")
      )
      i <- i + 2L
      next
    }

    label <- column_labels[[as.character(col)]]
    top_cells <- c(top_cells, paste0("\\multicolumn{1}{c}{", label, "}"))
    sub_cells <- c(sub_cells, "\\multicolumn{1}{c}{}")
    i <- i + 1L
  }

  c(
    "\\toprule",
    paste0(title_prefix, "& \\multicolumn{", n_cols, "}{c}{", title, "}\\\\"),
    "\\toprule",
    paste0(
      "                    &",
      paste(top_cells, collapse = "&"),
      "\\\\"
    ),
    paste0(
      "                    &",
      paste(sub_cells, collapse = "&"),
      "\\\\"
    ),
    paste0("                    ", cmidrules),
    paste0(
      "                    &",
      paste(paste0("\\multicolumn{1}{c}{", number_row, "}"), collapse = "   &"),
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
      c("Within-Trial Controls Only", spec_no, spec_no, spec_no, mark_no, spec_yes, spec_yes),
      c("All Completed Pairs", spec_no, spec_no, spec_no, mark_no, spec_no, spec_yes)
    )
  } else {
    controls_row <- if (table_id == "table7") "Correct Controls Only" else "Within-Trial Controls Only"
    completed_pair_col <- if (style == "all_completed_pairs") spec_yes else mark_no
    rows <- list(
      c("HDS Race Definition", spec_no, spec_yes, spec_yes, spec_yes, spec_yes, completed_pair_col),
      c("Duplicate Rows Removed", spec_no, spec_yes, spec_yes, spec_yes, spec_yes, completed_pair_col),
      c("City Names Standardized", spec_no, spec_no, spec_yes, spec_yes, mark_no, mark_no),
      c("City Fixed Effect", "Rec.", "Rec.", "Rec.", "Ad.", mark_no, mark_no),
      c(controls_row, spec_no, spec_no, spec_no, spec_no, spec_yes, completed_pair_col),
      c("All Completed Pairs", spec_no, spec_no, spec_no, spec_no, spec_no, completed_pair_col)
    )
  }

  vapply(rows, function(row) latex_row(c(row[[1]], select_display(row[-1], display_cols))), character(1))
}

panel_lines <- function(panel, all_completed_pairs, within, panel_index, single_panel) {
  minority <- read_source_table(panel$minority)
  categories <- read_source_table(panel$categories)
  copy_cols <- panel$copy_cols
  display_cols <- panel$display_cols

  lines <- panel_header(panel$title, display_cols, single_panel, panel$style, panel$table_id)

  minority_all_completed_pairs_block <- if (isTRUE(panel$all_completed_pairs_no_minority)) {
    rep(mark_no, 3)
  } else if (is.null(all_completed_pairs)) {
    NULL
  } else {
    all_completed_pairs$row_blocks[["Racial Minority"]]
  }
  minority_within_block <- if (is.null(within)) NULL else within$minority$row_blocks[["Racial Minority"]]
  lines <- c(lines, block_lines(
    "Racial Minority",
    minority$blocks[["Racial Minority"]],
    copy_cols,
    display_cols,
    minority_within_block,
    minority_all_completed_pairs_block
  ))
  lines <- c(lines, "\\midrule")

  for (label in c("African American", "Hispanic", "Asian", "Other Race")) {
    all_completed_pairs_block <- if (!is.null(all_completed_pairs)) all_completed_pairs$row_blocks[[label]] else NULL
    within_block <- if (!is.null(within)) within$categories$row_blocks[[label]] else NULL
    lines <- c(lines, block_lines(
      label,
      category_block(categories, label, panel$categories),
      copy_cols,
      display_cols,
      within_block,
      all_completed_pairs_block
    ))
  }

  lines <- c(lines, "\\midrule", spec_rows(panel$style, display_cols, panel$table_id), "\\midrule")

  obs <- summary_value(minority, "Observations", copy_cols)
  obs <- add_model_summary(obs, if (is.null(within)) NULL else within$minority, "Observations", 5)
  obs <- add_model_summary(obs, all_completed_pairs, "Observations", 6)

  adj_min <- summary_value(minority, "Adjusted R$^2$", copy_cols)
  adj_min <- add_model_summary(adj_min, if (is.null(within)) NULL else within$minority, "Adjusted R$^2$", 5)
  if (isTRUE(panel$all_completed_pairs_no_minority)) {
    adj_min[[6]] <- mark_no
  } else {
    adj_min <- add_model_summary(adj_min, all_completed_pairs, "Adjusted R$^2$ (Minority)", 6)
  }

  adj_cat <- summary_value(categories, "Adjusted R$^2$", copy_cols)
  adj_cat <- add_model_summary(adj_cat, if (is.null(within)) NULL else within$categories, "Adjusted R$^2$", 5)
  if (isTRUE(panel$all_completed_pairs_no_minority)) {
    adj_cat <- add_model_summary(adj_cat, all_completed_pairs, "Adjusted R$^2$", 6)
  } else {
    adj_cat <- add_model_summary(adj_cat, all_completed_pairs, "Adjusted R$^2$ (Category)", 6)
  }

  cities <- summary_value(minority, "Number of Cities", copy_cols)
  cities[[5]] <- mark_no
  cities[[6]] <- mark_no

  trials <- summary_value(minority, "Number of Trials", copy_cols)
  trials <- add_model_summary(trials, if (is.null(within)) NULL else within$minority, "Number of Trials", 5)
  trials <- add_model_summary(trials, all_completed_pairs, "Number of Trials", 6)

  lines <- c(
    lines,
    latex_row(c("Observations", select_display(obs, display_cols))),
    latex_row(c("Adjusted R$^2$ (Minority)", select_display(adj_min, display_cols))),
    latex_row(c("Adjusted R$^2$ (Category)", select_display(adj_cat, display_cols))),
    latex_row(c("Number of Cities", select_display(cities, display_cols))),
    latex_row(c("Number of Trials", select_display(trials, display_cols))),
    "\\bottomrule"
  )

  lines
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
  c(lines, paste0("\\resizebox{", table_width, "}{!}{"), paste0("\\begin{tabular}{l*{", n_cols, "}{c}}"))
}

table_footer <- function(note, n_cols, compact = FALSE) {
  note_span <- n_cols + 1L
  lines <- c(
    paste0("\\multicolumn{", note_span, "}{l}{\\footnotesize Cluster-robust standard errors in parentheses; clustered at the trial level. 95\\% confidence intervals in square brackets.}\\\\"),
    paste0("\\multicolumn{", note_span, "}{l}{\\footnotesize \\sym{*} \\(p<0.10\\), \\sym{**} \\(p<0.05\\), \\sym{***} \\(p<0.01\\)}\\\\"),
    paste0("\\multicolumn{", note_span, "}{l}{\\footnotesize Notes: Columns report estimates under successive corrections to the original specification.}\\\\"),
    paste0("\\multicolumn{", note_span, "}{l}{\\footnotesize ``HDS Race Definition'' replaces the \\origpaper race and ethnicity coding with that used in the HDS 2012 design.}\\\\"),
    paste0("\\multicolumn{", note_span, "}{l}{\\footnotesize ``Duplicate Rows Removed'' removes duplicated recommendation records. ``City Names Standardized'' harmonizes city-name spellings before constructing city fixed effects.}\\\\"),
    paste0("\\multicolumn{", note_span, "}{l}{\\footnotesize ``City Fixed Effect'' indicates whether the fixed effect is based on the recommended-home city (``Rec.'') or advertised-home city (``Ad.'').}\\\\"),
    paste0("\\multicolumn{", note_span, "}{l}{\\footnotesize ``Within-Trial Controls Only'' retains only controls expected to vary within trial by the design of the audit,}\\\\"),
    paste0("\\multicolumn{", note_span, "}{l}{\\footnotesize excluding controls that provide no additional identifying power while risking introducing bias from mismeasurement.}\\\\"),
    paste0("\\multicolumn{", note_span, "}{l}{\\footnotesize ``All Completed Pairs'' refers to a sample that is not selected on the event of being recommended the advertised home,}\\\\"),
    paste0("\\multicolumn{", note_span, "}{l}{\\footnotesize but instead includes observations from all completed paired trials that passed protocol approval by the HDS 2012 team and are not missing covariate or outcome data.}\\\\"),
    paste0("\\multicolumn{", note_span, "}{l}{\\footnotesize A dash indicates that the correction is not applicable in that specification.}\\\\")
  )
  if (!(length(note) == 1L && is.na(note))) lines <- c(lines, note)
  c(lines, "\\end{tabular}", "}", if (compact) "\\endgroup", "\\end{table}")
}

# All-completed-pairs tables are parsed once here, then attached to the relevant panels below.
table5_all_completed_pairs <- readLines(file.path(all_completed_pairs_dir, "table5.tex"), warn = FALSE)
table6_all_completed_pairs <- readLines(file.path(all_completed_pairs_dir, "table6.tex"), warn = FALSE)
table7_all_completed_pairs <- readLines(file.path(all_completed_pairs_dir, "table7.tex"), warn = FALSE)
table8_panels <- split_source_panels(file.path(all_completed_pairs_dir, "table8.tex"))
table9_panels <- split_source_panels(file.path(all_completed_pairs_dir, "table9.tex"))
table10_panels <- split_source_panels(file.path(all_completed_pairs_dir, "table10.tex"))
table12_panels <- split_source_panels(file.path(all_completed_pairs_dir, "table12.tex"))

all_completed_pairs_results <- list(
  table5_col1 = extract_result_column(table5_all_completed_pairs, 1),
  table5_col2 = extract_result_column(table5_all_completed_pairs, 2),
  table6_col1 = extract_result_column(table6_all_completed_pairs, 1),
  table7_col1 = extract_result_column(table7_all_completed_pairs, 1),
  table7_col3 = extract_result_column(table7_all_completed_pairs, 3),
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

table5_note <- paste0(
  "\\multicolumn{6}{l}{\\footnotesize Note: Column 3 standardizes advertised-home city names; \\origpaper Table 5 does not use recommended-home city fixed effects.}"
)
table7_note <- paste0(
  "\\multicolumn{7}{l}{\\footnotesize Note: ``Correct Controls Only'' removes trial-invariant controls relative to \\origpaper's original specification, as in other tables,}\\\\"
)
table7_note <- c(
  table7_note,
  "\\multicolumn{7}{l}{\\footnotesize and also removes post-treatment controls, such as number of recommendations (STOTUNIT) and whether the agent indicated the advertised home was available (SAVLBAD).}"
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
  label = c("comtab:table5", NA, "comtab:table7", "comtab:table8", NA, NA, NA,
            "comtab:table11", "comptab:table12", NA, NA),
  note = I(list(table5_note, NA, table7_note, NA, NA, NA, NA, NA, NA, NA, NA)),
  compact = c(TRUE, rep(FALSE, 10)),
  table_width = c("0.86\\textwidth", rep("\\textwidth", 8), "0.85\\textwidth", "\\textwidth"),
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
  style = c("table5", "table5", rep("all_completed_pairs", 11), "no_all_completed_pairs",
            "all_completed_pairs", "no_all_completed_pairs", "no_all_completed_pairs", "no_all_completed_pairs"),
  all_completed_pairs_key = c("table5_col1", "table5_col2", "table6_col1", "table7_col1",
                              "table7_col3", "table8_panel1_col4", "table8_panel2_col1",
                              "table9_panel1_col1", "table9_panel2_col1", "table10_panel1_col1",
                              "table10_panel1_col2", "table10_panel2_col2", "table10_panel2_col4",
                              NA, "table12_panel1_col1", NA, NA, NA),
  within_key = c("table5_col2", "table5_col4", "table6_col5", "table7_col1",
                 "table7_col3", "table8_panel1_col4", "table8_panel2_col1",
                 "table9_panel1_col1", "table9_panel2_col1", "table10_panel1_col1",
                 "table10_panel1_col2", "table10_panel2_col2", "table10_panel2_col4",
                 "table11_col1", "table12_col1", NA, NA, "table14_col5"),
  all_completed_pairs_no_minority = c(rep(FALSE, 14), TRUE, FALSE, FALSE, FALSE),
  stringsAsFactors = FALSE
)

table13_rows <- panel_specs$file == "comparison_table13.tex"
panel_specs$display_cols[table13_rows] <- rep(list(1:4), sum(table13_rows))

for (i in seq_len(nrow(table_specs))) {
  table <- table_specs[i, ]
  table_panels <- panel_specs[panel_specs$file == table$file, ]
  table_panels <- table_panels[order(table_panels$panel_order), ]
  n_cols <- length(table_panels$display_cols[[1]])

  lines <- table_header(table$caption, table$label, n_cols, table$compact, table$table_width)

  for (j in seq_len(nrow(table_panels))) {
    panel_row <- table_panels[j, ]
    all_completed_pairs <- if (is.na(panel_row$all_completed_pairs_key)) {
      NULL
    } else {
      all_completed_pairs_results[[panel_row$all_completed_pairs_key]]
    }
    within <- if (is.na(panel_row$within_key)) NULL else within_results[[panel_row$within_key]]
    panel <- list(
      title = panel_row$panel_title,
      minority = file.path(selected_sample_dir, paste0(panel_row$table_id, "_dep_var_", panel_row$dep_var, "_minority.tex")),
      categories = file.path(selected_sample_dir, paste0(panel_row$table_id, "_dep_var_", panel_row$dep_var, "_categories.tex")),
      copy_cols = panel_row$copy_cols[[1]],
      display_cols = panel_row$display_cols[[1]],
      style = panel_row$style,
      table_id = panel_row$table_id,
      all_completed_pairs_no_minority = panel_row$all_completed_pairs_no_minority
    )
    lines <- c(lines, panel_lines(panel, all_completed_pairs, within, j, nrow(table_panels) == 1))
  }

  lines <- c(lines, table_footer(table$note[[1]], n_cols, table$compact))
  lines <- gsub("{,}", "", lines, fixed = TRUE)
  writeLines(lines, file.path(output_dir, table$file), useBytes = TRUE)
}

message("Wrote ", nrow(table_specs), " comparison tables to ", normalizePath(output_dir))
