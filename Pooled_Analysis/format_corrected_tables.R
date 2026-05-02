resolve_repo_root <- function() {
  cwd <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  if (basename(cwd) == "HUDreplication") return(cwd)
  if (basename(cwd) == "Pooled_Analysis") return(dirname(cwd))

  candidate <- file.path(cwd, "HUDreplication")
  if (dir.exists(candidate)) {
    return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
  }

  stop("Could not infer repo_root. Run from HUDreplication or Pooled_Analysis.")
}

repo_root <- resolve_repo_root()
source_dir <- file.path(repo_root, "Pooled_Analysis", "Output")
output_dir <- file.path(source_dir, "Corrected Tables")

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

sym_def <- "\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}"
standard_note <- c(
  "Cluster-robust standard errors in parentheses; clustered at the trial level. 95\\% confidence intervals in square brackets.",
  "\\sym{*} \\(p<0.10\\), \\sym{**} \\(p<0.05\\), \\sym{***} \\(p<0.01\\)"
)

table_cache <- new.env(parent = emptyenv())

read_raw_csv <- function(stem) {
  path <- file.path(source_dir, paste0(stem, ".csv"))
  if (!file.exists(path)) stop("Missing raw corrected CSV: ", path)

  raw <- read.csv(
    path,
    header = FALSE,
    check.names = FALSE,
    stringsAsFactors = FALSE,
    na.strings = character()
  )
  raw[] <- lapply(raw, function(x) trimws(sub("^=", "", x)))

  out <- list(
    path = path,
    n_cols = ncol(raw) - 1L,
    blocks = list(),
    summary = list()
  )

  summary_labels <- c("Observations", "Adjusted R^2", "Number of Cities")
  i <- 1L
  while (i <= nrow(raw)) {
    label <- raw[i, 1]

    if (label %in% summary_labels) {
      out$summary[[label]] <- unname(unlist(raw[i, -1]))
      i <- i + 1L
      next
    }

    if (nzchar(label) && i + 2L <= nrow(raw)) {
      out$blocks[[label]] <- list(
        coef = unname(unlist(raw[i, -1])),
        se = unname(unlist(raw[i + 1L, -1])),
        ci = unname(unlist(raw[i + 2L, -1]))
      )
      i <- i + 3L
      next
    }

    i <- i + 1L
  }

  out
}

get_table <- function(stem) {
  if (!exists(stem, envir = table_cache, inherits = FALSE)) {
    assign(stem, read_raw_csv(stem), envir = table_cache)
  }
  get(stem, envir = table_cache, inherits = FALSE)
}

format_stars <- function(cells) {
  vapply(cells, function(x) {
    if (!nzchar(x)) return("")
    star_pos <- regexpr("\\*+$", x)
    if (star_pos[1] == -1L) return(x)

    stars <- regmatches(x, star_pos)
    base <- substr(x, 1L, star_pos[1] - 1L)
    paste0(base, "\\sym{", stars, "}")
  }, character(1))
}

format_counts <- function(cells) {
  vapply(cells, function(x) {
    if (!grepl("^[0-9]+$", x)) return(x)
    format(as.integer(x), big.mark = ",", scientific = FALSE)
  }, character(1))
}

latex_row <- function(label, cells, suffix = "") {
  paste0(label, " & ", paste(cells, collapse = " & "), " \\\\", suffix)
}

combine_blocks <- function(stems, row_label) {
  combined <- list(coef = character(), se = character(), ci = character())

  for (stem in stems) {
    tbl <- get_table(stem)
    block <- tbl$blocks[[row_label]]
    if (is.null(block)) stop("Missing row `", row_label, "` in ", basename(tbl$path))

    combined$coef <- c(combined$coef, block$coef)
    combined$se <- c(combined$se, block$se)
    combined$ci <- c(combined$ci, block$ci)
  }

  combined
}

combine_summary <- function(stems, summary_label) {
  values <- character()

  for (stem in stems) {
    tbl <- get_table(stem)
    row <- tbl$summary[[summary_label]]
    if (is.null(row)) stop("Missing summary row `", summary_label, "` in ", basename(tbl$path))
    values <- c(values, row)
  }

  values
}

effect_lines <- function(label, block) {
  c(
    latex_row(label, format_stars(block$coef)),
    latex_row("", block$se),
    latex_row("", block$ci, "[1ex]")
  )
}

summary_lines <- function(category_stems, minority_stems = NULL, include_minority = TRUE) {
  lines <- c(
    latex_row("Observations", format_counts(combine_summary(category_stems, "Observations")))
  )

  if (include_minority) {
    lines <- c(
      lines,
      latex_row("Adjusted R$^2$ (Minority)", combine_summary(minority_stems, "Adjusted R^2")),
      latex_row("Adjusted R$^2$ (Category)", combine_summary(category_stems, "Adjusted R^2"))
    )
  } else {
    lines <- c(
      lines,
      latex_row("Adjusted R$^2$", combine_summary(category_stems, "Adjusted R^2"))
    )
  }

  c(
    lines,
    latex_row("Number of Cities", format_counts(combine_summary(category_stems, "Number of Cities")))
  )
}

build_model_panel <- function(panel_title = NULL, header_span = "Dependent Variable",
                              col_names, category_stems, minority_stems = NULL,
                              include_minority = TRUE, additional_rows = list()) {
  n_cols <- length(col_names)

  lines <- character()
  if (!is.null(panel_title)) {
    lines <- c(lines, paste0("\\textbf{", panel_title, "}\\\\[0.5em]"))
  }

  lines <- c(
    lines,
    "\\resizebox{\\textwidth}{!}{%",
    paste0("\\begin{tabular}{l*{", n_cols, "}{c}}"),
    "\\toprule",
    paste0("& \\multicolumn{", n_cols, "}{c}{", header_span, "} \\\\"),
    paste0("\\cmidrule(lr){2-", n_cols + 1L, "}"),
    paste0("& ", paste(paste0("\\multicolumn{1}{c}{", col_names, "}"), collapse = " & "), " \\\\"),
    "\\midrule"
  )

  if (include_minority) {
    lines <- c(lines, effect_lines("Racial Minority", combine_blocks(minority_stems, "Racial Minority")))
  }

  # The raw appendix exports can include nuisance "Other Race" rows from the
  # legacy APRACE coding. The formatted corrected tables follow the original
  # and matched-pair table layout and display the substantive HDS race groups.
  category_rows <- c("African American", "Hispanic", "Asian")
  for (row_label in category_rows) {
    lines <- c(lines, effect_lines(row_label, combine_blocks(category_stems, row_label)))
  }

  lines <- c(lines, "\\midrule")
  for (row in additional_rows) {
    lines <- c(lines, latex_row(row[1], row[-1]))
  }

  lines <- c(
    lines,
    summary_lines(
      category_stems = category_stems,
      minority_stems = minority_stems,
      include_minority = include_minority
    ),
    "\\bottomrule",
    "\\end{tabular}",
    "}"
  )

  lines
}

build_table14a_panel <- function() {
  panel <- get_table("table14A_override_corrected")

  same_race <- list(
    coef = c(
      panel$blocks$whitetester$coef[1],
      panel$blocks$blacktester$coef[2],
      panel$blocks$hisptester$coef[3],
      panel$blocks$asiantester$coef[4]
    ),
    se = c(
      panel$blocks$whitetester$se[1],
      panel$blocks$blacktester$se[2],
      panel$blocks$hisptester$se[3],
      panel$blocks$asiantester$se[4]
    ),
    ci = c(
      panel$blocks$whitetester$ci[1],
      panel$blocks$blacktester$ci[2],
      panel$blocks$hisptester$ci[3],
      panel$blocks$asiantester$ci[4]
    )
  )

  c(
    "\\textbf{Panel A: Buyers upon Sale}\\\\[0.5em]",
    "\\resizebox{\\textwidth}{!}{%",
    "\\begin{tabular}{l*{4}{c}}",
    "\\toprule",
    "& \\multicolumn{1}{c}{White} & \\multicolumn{1}{c}{African American} & \\multicolumn{1}{c}{Hispanic} & \\multicolumn{1}{c}{Asian} \\\\",
    "\\midrule",
    effect_lines("Same-race tester", same_race),
    "\\midrule",
    latex_row("Observations", format_counts(panel$summary$Observations)),
    latex_row("Adjusted R$^2$", panel$summary[["Adjusted R^2"]]),
    latex_row("Number of Cities", format_counts(panel$summary[["Number of Cities"]])),
    "\\bottomrule",
    "\\end{tabular}",
    "}"
  )
}

write_table <- function(file_name, caption, label, panels) {
  lines <- c(
    "\\begin{table}[!htbp]",
    "\\centering",
    sym_def,
    paste0("\\caption{", caption, "}"),
    paste0("\\label{", label, "}")
  )

  for (i in seq_along(panels)) {
    if (i > 1L) lines <- c(lines, "\\medskip")
    lines <- c(lines, panels[[i]])
  }

  lines <- c(
    lines,
    "\\begin{minipage}{\\textwidth}",
    "\\footnotesize",
    standard_note,
    "\\end{minipage}",
    "\\end{table}",
    ""
  )

  writeLines(lines, file.path(output_dir, file_name), useBytes = TRUE)
}

write_table(
  "table5.tex",
  "Discriminatory Steering and Availability of Advertised Properties\\\\[0.5em]\\textit{Corrected Model-Based Replication of Table 5, C\\&T 2022}",
  "tab:corrected_table5",
  list(build_model_panel(
    col_names = c("(1)", "(2)", "(3)", "(4)"),
    category_stems = "table5_categories_corrected",
    minority_stems = "table5_minority_corrected",
    additional_rows = list(
      c("ln(price) advertised home", "No", "Yes", "No", "Yes"),
      c("Racial composition advertised home", "No", "Yes", "No", "Yes")
    )
  ))
)

write_table(
  "table6.tex",
  "Discriminatory Steering and Neighborhood Racial Composition\\\\[0.5em]\\textit{Corrected Model-Based Replication of Table 6, C\\&T 2022}",
  "tab:corrected_table6",
  list(build_model_panel(
    col_names = c("(1)", "(2)", "(3)", "(4)", "(5)"),
    category_stems = "table6_categories_corrected",
    minority_stems = "table6_minority_corrected",
    additional_rows = list(
      c("Share white advertised home", "No", "Yes", "Yes", "Yes", "Yes"),
      c("ln(price) advertised home", "No", "No", "Yes", "Yes", "Yes"),
      c("Racial composition advertised home", "No", "No", "No", "Yes", "Yes"),
      c("Poverty share advertised home", "No", "No", "No", "No", "Yes")
    )
  ))
)

write_table(
  "table7.tex",
  "Discriminatory Steering and Neighborhood Racial Composition by Income\\\\[0.5em]\\textit{Corrected Model-Based Replication of Table 7, C\\&T 2022}",
  "tab:corrected_table7",
  list(build_model_panel(
    col_names = c("High Income", "Middle Income", "Low Income"),
    category_stems = "table7_categories_corrected",
    minority_stems = "table7_minority_corrected",
    additional_rows = list(
      c("Share white advertised home", "Yes", "Yes", "Yes"),
      c("ln(price) advertised home", "Yes", "Yes", "Yes"),
      c("Racial composition advertised home", "Yes", "Yes", "Yes"),
      c("Poverty share advertised home", "Yes", "Yes", "Yes")
    )
  ))
)

school_headers <- c(
  "\\begin{tabular}{@{}c@{}}Elementary School\\\\Test Score\\end{tabular}",
  "\\begin{tabular}{@{}c@{}}Middle School\\\\Test Score\\end{tabular}",
  "Assaults",
  "\\begin{tabular}{@{}c@{}}Elementary School\\\\Rating\\end{tabular}"
)
acs_headers <- c("Poverty Rate", "High Skill", "College", "Single-Parent Household", "Ownership Rate")
school_controls <- list(
  c("ln(price) advertised home", "Yes", "Yes", "Yes", "Yes"),
  c("Racial composition advertised home", "Yes", "Yes", "Yes", "Yes"),
  c("Outcome advertised home", "Yes", "Yes", "Yes", "Yes")
)
acs_controls <- list(
  c("ln(price) advertised home", "Yes", "Yes", "Yes", "Yes", "Yes"),
  c("Racial composition advertised home", "Yes", "Yes", "Yes", "Yes", "Yes"),
  c("Outcome advertised home", "Yes", "Yes", "Yes", "Yes", "Yes")
)

write_table(
  "table8.tex",
  "Discriminatory Steering and Neighborhood Effects\\\\[0.5em]\\textit{Corrected Model-Based Replication of Table 8, C\\&T 2022}",
  "tab:corrected_table8",
  list(
    build_model_panel(
      panel_title = "Panel A: School Quality and Neighborhood Safety",
      header_span = "Dependent Variable",
      col_names = school_headers,
      category_stems = c("table8A1_categories_corrected", "table8A2_categories_corrected"),
      minority_stems = c("table8A1_minority_corrected", "table8A2_minority_corrected"),
      additional_rows = school_controls
    ),
    build_model_panel(
      panel_title = "Panel B: American Community Survey",
      header_span = "Dependent Variable",
      col_names = acs_headers,
      category_stems = "table8B_categories_corrected",
      minority_stems = "table8B_minority_corrected",
      additional_rows = acs_controls
    )
  )
)

pollution_headers <- c("Superfund", "Toxics", "PM")
pollution_controls <- list(
  c("ln(price) advertised home", "Yes", "Yes", "Yes"),
  c("Racial composition advertised home", "Yes", "Yes", "Yes"),
  c("Outcome advertised home", "Yes", "Yes", "Yes")
)

write_table(
  "table9.tex",
  "Discriminatory Steering and Local Pollution Exposures\\\\[0.5em]\\textit{Corrected Model-Based Replication of Table 9, C\\&T 2022}",
  "tab:corrected_table9",
  list(
    build_model_panel(
      panel_title = "Panel A: Differences for the Entire Sample",
      col_names = pollution_headers,
      category_stems = "table9A_categories_corrected",
      minority_stems = "table9A_minority_corrected",
      additional_rows = pollution_controls
    ),
    build_model_panel(
      panel_title = "Panel B: Differences for Sample of Mothers",
      col_names = pollution_headers,
      category_stems = "table9B_categories_corrected",
      minority_stems = "table9B_minority_corrected",
      additional_rows = pollution_controls
    )
  )
)

write_table(
  "table10.tex",
  "Discriminatory Steering and Neighborhood Effects (Mothers)\\\\[0.5em]\\textit{Corrected Model-Based Replication of Table 10, C\\&T 2022}",
  "tab:corrected_table10",
  list(
    build_model_panel(
      panel_title = "Panel A: School Quality and Neighborhood Safety",
      col_names = school_headers,
      category_stems = c("table10A1_categories_corrected", "table10A2_categories_corrected"),
      minority_stems = c("table10A1_minority_corrected", "table10A2_minority_corrected"),
      additional_rows = school_controls
    ),
    build_model_panel(
      panel_title = "Panel B: American Community Survey",
      col_names = acs_headers,
      category_stems = "table10B_categories_corrected",
      minority_stems = "table10B_minority_corrected",
      additional_rows = acs_controls
    )
  )
)

write_table(
  "table11.tex",
  "Discriminatory Steering: Low-Poverty Neighborhoods\\\\[0.5em]\\textit{Corrected Model-Based Replication of Table 11, C\\&T 2022}",
  "tab:corrected_table11",
  list(build_model_panel(
    col_names = c(
      "Low Poverty",
      "Low Poverty: Families",
      "Low Poverty: Moms",
      "Low Poverty/High Dad",
      "Low Poverty/High Dad: Families",
      "Low Poverty/High Dad: Moms"
    ),
    category_stems = "table11_categories_corrected",
    include_minority = FALSE
  ))
)

write_table(
  "table12.tex",
  "Discriminatory Steering: Median Income in Neighborhood\\\\[0.5em]\\textit{Corrected Model-Based Replication of Table 12, C\\&T 2022}",
  "tab:corrected_table12",
  list(build_model_panel(
    col_names = c("All Testers", "Families", "Moms"),
    category_stems = "table12_categories_corrected",
    include_minority = FALSE
  ))
)

write_table(
  "table13.tex",
  "Discriminatory Steering by Implied Preferences for Neighborhood Attributes\\\\[0.5em]\\textit{Corrected Model-Based Replication of Table 13, C\\&T 2022}",
  "tab:corrected_table13",
  list(
    build_model_panel(
      panel_title = "Panel A: School Quality and Neighborhood Safety",
      col_names = school_headers,
      category_stems = c("table13A1_categories_corrected", "table13A2_categories_corrected"),
      minority_stems = c("table13A1_minority_corrected", "table13A2_minority_corrected"),
      additional_rows = school_controls
    ),
    build_model_panel(
      panel_title = "Panel B: American Community Survey",
      col_names = acs_headers,
      category_stems = "table13B_categories_corrected",
      minority_stems = "table13B_minority_corrected",
      additional_rows = acs_controls
    )
  )
)

write_table(
  "table14.tex",
  "Discriminatory Steering and Later Transactions\\\\[0.5em]\\textit{Corrected Model-Based Replication of Table 14, C\\&T 2022}",
  "tab:corrected_table14",
  list(
    build_table14a_panel(),
    build_model_panel(
      panel_title = "Panel B: Dependent Variable: Logarithm of Price",
      col_names = c("(1)", "(2)", "(3)", "(4)", "(5)"),
      category_stems = "table14B_categories_corrected",
      minority_stems = "table14B_minority_corrected",
      additional_rows = list(
        c("Share white advertised home", "No", "Yes", "Yes", "Yes", "Yes"),
        c("ln(price) advertised home", "No", "No", "Yes", "Yes", "Yes"),
        c("Racial composition advertised home", "No", "No", "No", "Yes", "Yes"),
        c("Poverty share advertised home", "No", "No", "No", "No", "Yes"),
        c("Year", "Yes", "Yes", "Yes", "Yes", "Yes"),
        c("Month of year", "Yes", "Yes", "Yes", "Yes", "Yes")
      )
    )
  )
)

preview_lines <- c(
  "\\documentclass[11pt]{article}",
  "\\usepackage[margin=1in]{geometry}",
  "\\usepackage{booktabs}",
  "\\usepackage{graphicx}",
  "\\begin{document}",
  "\\input{table5.tex}",
  "\\clearpage",
  "\\input{table6.tex}",
  "\\clearpage",
  "\\input{table7.tex}",
  "\\clearpage",
  "\\input{table8.tex}",
  "\\clearpage",
  "\\input{table9.tex}",
  "\\clearpage",
  "\\input{table10.tex}",
  "\\clearpage",
  "\\input{table11.tex}",
  "\\clearpage",
  "\\input{table12.tex}",
  "\\clearpage",
  "\\input{table13.tex}",
  "\\clearpage",
  "\\input{table14.tex}",
  "\\end{document}"
)
writeLines(preview_lines, file.path(output_dir, "formatted_corrected_tables_preview.tex"), useBytes = TRUE)

manifest <- c(
  "Generated formatted corrected model-based tables.",
  paste("Generated at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  paste("Input directory:", source_dir),
  paste("Output directory:", output_dir),
  "",
  "Outputs:",
  paste0("- table", c(5:14), ".tex"),
  "- formatted_corrected_tables_preview.tex",
  "",
  "Notes:",
  "- The script consumes the current raw corrected CSV outputs; it does not rerun Stata.",
  "- Split raw outputs for Tables 8, 10, 13, and 14 are combined into one file per C&T table.",
  "- Table 14 Panel A is rebuilt from table14A_override_corrected.csv and therefore reports the corrected same-race tester coefficients and available summary rows only."
)
writeLines(manifest, file.path(output_dir, "README.txt"), useBytes = TRUE)

cat("Wrote formatted corrected tables to:\n")
cat(output_dir, "\n")
