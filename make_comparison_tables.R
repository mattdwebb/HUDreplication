# Build the comparison tables used in the comment.
#
# Usage:
#   Rscript make_comparison_tables.R
#   Rscript make_comparison_tables.R /tmp/comparison_tables
#
# The script reads pooled-analysis Stata LaTeX outputs from Pooled_Analysis/Output
# and matched-pair LaTeX outputs from Paired_Tester_Analysis/Tables/Formatted_Tables.

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

pooled_dir <- file.path("Pooled_Analysis", "Output")
paired_dir <- file.path("Paired_Tester_Analysis", "Tables", "Formatted_Tables")

split_latex_cells <- function(x) {
  # Keep escaped ampersands intact while using unescaped ampersands as cell breaks.
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

block_lines <- function(label, source_block, copy_cols, matched_block = NULL) {
  defaults <- if (label == "Other Race") rep("", 6) else rep("-", 6)

  out <- character()
  for (i in seq_along(source_block)) {
    vals <- copy_values(defaults, source_block[[i]], copy_cols)
    if (!is.null(matched_block)) vals[[6]] <- matched_block[[i]]
    out <- c(out, latex_row(c(if (i == 1) label else "", vals)))
  }
  out
}

summary_value <- function(source, label, copy_cols) {
  vals <- rep("-", 6)
  copy_values(vals, source$summary[[label]], copy_cols)
}

add_matched_summary <- function(vals, matched, label) {
  if (!is.null(matched) && !is.null(matched$scalar_rows[[label]])) {
    vals[[6]] <- matched$scalar_rows[[label]]
  }
  vals
}

panel_header <- function(title, single_panel) {
  title_prefix <- if (single_panel) "\t" else ""
  c(
    "\\toprule",
    paste0(title_prefix, "& \\multicolumn{6}{c}{", title, "}\\\\"),
    "\\toprule",
    "                    &\\multicolumn{1}{c}{Original}&\\multicolumn{1}{c}{Correct}&\\multicolumn{1}{c}{Updated City Name}&\\multicolumn{1}{c}{Proper City Name}&\\multicolumn{1}{c}{Place Name}&\\multicolumn{1}{c}{Matched Pairs}\\\\",
    paste0(
      "                      &\\multicolumn{1}{c}{Data}&\\multicolumn{1}{c}{Race Only}&\\multicolumn{1}{c}{\\& Correct Race}&\\multicolumn{1}{c}{\\& Correct Race}&\\multicolumn{1}{c}{\\& Correct Race}&\\multicolumn{1}{c}{Design}\\\\",
      strrep(" ", 18)
    ),
    "                    \\cmidrule(lr){2-2}\\cmidrule(lr){3-3}\\cmidrule(lr){4-4}\\cmidrule(lr){5-5}\\cmidrule(lr){6-6}\\cmidrule(lr){7-7}",
    "                    &\\multicolumn{1}{c}{(1)}   &\\multicolumn{1}{c}{(2)}   &\\multicolumn{1}{c}{(3)}   &\\multicolumn{1}{c}{(4)}   &\\multicolumn{1}{c}{(5)}   &\\multicolumn{1}{c}{(6)}\\\\",
    "\\midrule"
  )
}

control_rows <- function(style, panel_index) {
  # Table 5 omits column 4 by design; preserve the manuscript spacing exactly.
  if (style == "table5") {
    city_row <- if (panel_index == 1) {
      "HDS city name, recommended homes &  &  & &- & - & -\\\\"
    } else {
      "HDS city name, recommended homes &  &  &  &- & - & -\\\\"
    }

    return(c(
      "HDS race identifier & & Yes & Yes & - & Yes & Yes \\\\",
      city_row,
      "HDS city name, advertised homes & Yes & Yes & Yes & - & - & -\\\\",
      "HDS city name spelling fixed & & & Yes & - & - & -\\\\",
      "Duplicate rows removed &  & Yes & Yes & - & Yes & - \\\\"
    ))
  }

  col6 <- if (style == "matched") "Yes" else "-"
  c(
    paste0("HDS race identifier & & Yes & Yes & Yes & Yes & ", col6, " \\\\"),
    "HDS city name, recommended homes & Yes & Yes & Yes & & - & -\\\\",
    "HDS city name, advertised homes &  &  &  & Yes & - & -\\\\",
    "HDS city name spelling fixed & & & Yes & Yes & - & -\\\\",
    "Duplicate rows removed &  & Yes & Yes & Yes & Yes & - \\\\"
  )
}

ad_control_rows <- function(style) {
  if (style == "table5") {
    return(c(
      "Share white, advertised home  & Yes & Yes & Yes & - & Yes & - \\\\",
      "ln(price), advertised home  & Yes & Yes & Yes & - & Yes & - \\\\",
      "Racial composition, advertised home & Yes & Yes & Yes & - & Yes & - \\\\",
      "Poverty share advertised home & Yes & Yes & Yes & - & Yes & - \\\\"
    ))
  }

  c(
    "Share white, advertised home  & Yes & Yes & Yes & Yes & Yes & - \\\\",
    "ln(price), advertised home  & Yes & Yes & Yes & Yes & Yes & - \\\\",
    "Racial composition, advertised home & Yes & Yes & Yes & Yes & Yes & - \\\\",
    "Poverty share advertised home & Yes & Yes & Yes & Yes & Yes & - \\\\"
  )
}

panel_lines <- function(panel, matched, panel_index, single_panel) {
  minority <- read_source_table(panel$minority)
  categories <- read_source_table(panel$categories)
  copy_cols <- panel$copy_cols

  lines <- panel_header(panel$title, single_panel)

  minority_matched_block <- if (isTRUE(panel$matched_no_minority)) {
    rep("-", 3)
  } else if (is.null(matched)) {
    NULL
  } else {
    matched$row_blocks[["Racial Minority"]]
  }
  lines <- c(lines, block_lines(
    "Racial Minority",
    minority$blocks[["Racial Minority"]],
    copy_cols,
    minority_matched_block
  ))
  lines <- c(lines, "\\midrule")

  for (label in c("African American", "Hispanic", "Asian", "Other Race")) {
    matched_block <- if (!is.null(matched)) matched$row_blocks[[label]] else NULL
    lines <- c(lines, block_lines(
      label,
      category_block(categories, label, panel$categories),
      copy_cols,
      matched_block
    ))
  }

  lines <- c(lines, "\\midrule", control_rows(panel$style, panel_index), "\\midrule")
  lines <- c(lines, ad_control_rows(panel$style))

  obs <- add_matched_summary(summary_value(minority, "Observations", copy_cols), matched, "Observations")
  adj_min <- summary_value(minority, "Adjusted R$^2$", copy_cols)
  if (isTRUE(panel$matched_no_minority)) {
    adj_min[[6]] <- "-"
  } else {
    adj_min <- add_matched_summary(adj_min, matched, "Adjusted R$^2$ (Minority)")
  }

  adj_cat <- summary_value(categories, "Adjusted R$^2$", copy_cols)
  if (isTRUE(panel$matched_no_minority)) {
    adj_cat <- add_matched_summary(adj_cat, matched, "Adjusted R$^2$")
  } else {
    adj_cat <- add_matched_summary(adj_cat, matched, "Adjusted R$^2$ (Category)")
  }

  cities <- summary_value(minority, "Number of Cities", copy_cols)
  cities[[6]] <- "-"

  trials <- rep("-", 6)
  trials <- add_matched_summary(trials, matched, "Number of Trials")

  lines <- c(
    lines,
    latex_row(c("Observations", obs)),
    latex_row(c("Adjusted R$^2$ (Minority)", adj_min)),
    latex_row(c("Adjusted R$^2$ (Category)", adj_cat)),
    latex_row(c("Number of Cities", cities)),
    latex_row(c("Number of Trials", trials)),
    "\\bottomrule"
  )

  lines
}

table_header <- function(caption, label) {
  lines <- c(
    "% April 13, 2026 -- New panel title",
    "",
    "\\begin{table}[htbp]\\centering",
    "\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}",
    paste0("\\caption{", caption, "}")
  )
  if (!is.na(label)) lines <- c(lines, paste0("\\label{", label, "}"))
  c(lines, "\\resizebox{\\textwidth}{!}{", "\\begin{tabular}{l*{6}{c}}")
}

table_footer <- function(note) {
  lines <- c(
    "\\multicolumn{7}{l}{\\footnotesize Cluster-robust standard errors in parentheses. Clustered at the trial level. 95\\% confidence intervals in square brackets.}\\\\",
    "\\multicolumn{7}{l}{\\footnotesize \\sym{*} \\(p<0.10\\), \\sym{**} \\(p<0.05\\), \\sym{***} \\(p<0.01\\)}\\\\"
  )
  if (!is.na(note)) lines <- c(lines, note)
  c(lines, "\\end{tabular}", "}", "\\end{table}")
}

# Matched-pair tables are parsed once here, then attached to the relevant panels
# below. The source column choices mirror the old Julia script.
table5_matched <- readLines(file.path(paired_dir, "table5.tex"), warn = FALSE)
table6_matched <- readLines(file.path(paired_dir, "table6.tex"), warn = FALSE)
table7_matched <- readLines(file.path(paired_dir, "table7.tex"), warn = FALSE)
table8_panels <- split_source_panels(file.path(paired_dir, "table8.tex"))
table9_panels <- split_source_panels(file.path(paired_dir, "table9.tex"))
table10_panels <- split_source_panels(file.path(paired_dir, "table10.tex"))
table12_panels <- split_source_panels(file.path(paired_dir, "table12.tex"))

matched_results <- list(
  table5_col1 = extract_result_column(table5_matched, 1),
  table5_col2 = extract_result_column(table5_matched, 2),
  table6_col1 = extract_result_column(table6_matched, 1),
  table7_col1 = extract_result_column(table7_matched, 1),
  table7_col3 = extract_result_column(table7_matched, 3),
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

table5_note <- "\\multicolumn{7}{l}{\\footnotesize Note: C\\&T Table 5 is based on city name of advertised homes. Corrections to ``Place Name'' is therefore not required as Column (4) would be identical to Column (3).}"

table_specs <- data.frame(
  file = c("comparison_table5.tex", "comparison_table6.tex", "comparison_table7.tex",
           "comparison_table8.tex", "comparison_table9.tex", "comparison_table10A.tex",
           "comparison_table10B.tex", "comparison_table11.tex", "comparison_table12.tex",
           "comparison_table13.tex", "comparison_table14.tex"),
  caption = c("\\small Results Comparison for Table 5 Column 2 and 4, C\\&T 2022",
              "\\small Results Comparison for Table 6 Column 5, C\\&T 2022",
              "\\small Results Comparison for Table 7 Column 1 and 3, C\\&T 2022",
              "\\small Results Comparison for Table 8 Panel A Column 4 and Panel B Column 1, C\\&T 2022",
              "\\small Results Comparison for Table 9 Column 1, C\\&T 2022",
              "\\small Results Comparison for Table 10 Panel A Column 1 and 2, C\\&T 2022",
              "\\small Results Comparison for Table 10 Panel B Column 2 and 4, C\\&T 2022",
              "\\small Results Comparison for Table 11 Column 1, C\\&T 2022",
              "\\small Results Comparison for Table 12 Column 1, C\\&T 2022",
              "\\small Results Comparison for Table 13 Panel A Column 4 and Panel B Column 2, C\\&T 2022",
              "\\small Results Comparison for Table 14 Column 5, C\\&T 2022"),
  label = c("comtab:table5", NA, "comtab:table7", "comtab:table8", NA, NA, NA,
            "comtab:table11", "comptab:table12", NA, NA),
  note = c(table5_note, rep(NA, 10)),
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
  panel_title = c("Panel A: Number of Recommendations, Clustered at Trial",
                  "Panel B: Home Availability, Clustered at Trial",
                  "White Household Share, Clustered at Trial",
                  "Panel A: White Household Share (High Income Neighbourhood), Clustered at Trial",
                  "Panel B: White Household Share (Low Income Neighbourhood), Clustered at Trial",
                  "Panel A: Elementary School Rating on Housing Search Platform, Clustered at Trial",
                  "Panel B: American Community Survey Poverty Rate, Clustered at Trial",
                  "Panel A: Local Pollution Exposures as Superfund Sites (Entire Sample), Clustered at Trial",
                  "Panel B: Local Pollution Exposures as Superfund Sites (Mothers Only), Clustered at Trial",
                  "Panel A: Elementary School Test Scores (Mothers Only), Clustered at Trial",
                  "Panel B: Middle School Test Scores (Mothers Only), Clustered at Trial",
                  "Panel A: American Community Survey High Skill Neighbourhood, Clustered at Trial",
                  "Panel B: American Community Survey Single Parent Household, Clustered at Trial",
                  "Low-Poverty Neighbourhoods, Clustered at Trial",
                  "Median Income in Neighbourhood, Clustered at Trial",
                  "Panel A: Elementary School Rating on Housing Search Platform, Clustered at Trial",
                  "Panel B: American Community Survey High Skill Neighbourhood, Clustered at Trial",
                  "Logarithm of Sale Price, Clustered at Trial"),
  table_id = c("table5", "table5", "table6", "table7", "table7", "table8", "table8",
               "table9", "table9", "table10A", "table10A", "table10B", "table10B",
               "table11", "table12", "table13", "table13", "table14"),
  dep_var = c(1, 2, 1, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 1, 1, 2, 1),
  copy_cols = I(list(c(1, 2, 3, 5, 6), c(1, 2, 3, 5, 6), 1:6, 1:6, 1:6, 1:6,
                     1:6, 1:6, 1:6, 1:6, 1:6, 1:6, 1:6, 1:5, 1:6, 1:5, 1:5, 1:5)),
  style = c("table5", "table5", rep("matched", 11), "no_matched",
            "matched", "no_matched", "no_matched", "no_matched"),
  matched_key = c("table5_col1", "table5_col2", "table6_col1", "table7_col1",
                  "table7_col3", "table8_panel1_col4", "table8_panel2_col1",
                  "table9_panel1_col1", "table9_panel2_col1", "table10_panel1_col1",
                  "table10_panel1_col2", "table10_panel2_col2", "table10_panel2_col4",
                  NA, "table12_panel1_col1", NA, NA, NA),
  matched_no_minority = c(rep(FALSE, 14), TRUE, FALSE, FALSE, FALSE),
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(table_specs))) {
  table <- table_specs[i, ]
  table_panels <- panel_specs[panel_specs$file == table$file, ]
  table_panels <- table_panels[order(table_panels$panel_order), ]

  lines <- table_header(table$caption, table$label)

  for (j in seq_len(nrow(table_panels))) {
    panel_row <- table_panels[j, ]
    matched <- if (is.na(panel_row$matched_key)) NULL else matched_results[[panel_row$matched_key]]
    panel <- list(
      title = panel_row$panel_title,
      minority = file.path(pooled_dir, paste0(panel_row$table_id, "_dep_var_", panel_row$dep_var, "_minority.tex")),
      categories = file.path(pooled_dir, paste0(panel_row$table_id, "_dep_var_", panel_row$dep_var, "_categories.tex")),
      copy_cols = panel_row$copy_cols[[1]],
      style = panel_row$style,
      matched_no_minority = panel_row$matched_no_minority
    )
    lines <- c(lines, panel_lines(panel, matched, j, nrow(table_panels) == 1))
  }

  lines <- c(lines, table_footer(table$note))
  lines <- gsub("{,}", "", lines, fixed = TRUE)
  writeLines(lines, file.path(output_dir, table$file), useBytes = TRUE)
}

message("Wrote ", nrow(table_specs), " comparison tables to ", normalizePath(output_dir))
