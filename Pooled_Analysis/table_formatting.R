generate_corrected_table <- function(table_title, subtitle, table_number, panel_title,
                   input_file_categories, input_file_minority, 
                   col_names, set_dashes = TRUE, 
                   show_minority = TRUE, corrected = TRUE,
                   additional_rows = list(c())) {

  output_file <- if (corrected) {
  paste0(output_dir, "/Corrected Tables/corrected_table", table_number, ".tex")
  } else {
  paste0(output_dir, "/Replicated Tables/replicated_table", table_number, ".tex")
  }

  # Create directories for Corrected and Replicated Tables if they don't exist
  corrected_dir <- file.path(output_dir, "Corrected Tables")
  replicated_dir <- file.path(output_dir, "Replicated Tables")
  
  if (!dir.exists(corrected_dir)) {
  dir.create(corrected_dir, recursive = TRUE)
  cat("Created directory for corrected tables:", corrected_dir, "\n")
  } 
  
  if (!dir.exists(replicated_dir)) {
  dir.create(replicated_dir, recursive = TRUE)
  cat("Created directory for replicated tables:", replicated_dir, "\n")
  } 

  # Function to extract relevant data from LaTeX table
  extract_data <- function(file_path) {
  lines <- readLines(file_path)
  start <- grep("\\\\midrule", lines)[1] + 1
  end <- grep("\\\\midrule", lines)[2] - 1
  data_lines <- lines[start:end]
  
  # Add [1ex] at the end of every three rows
  for (i in seq_along(data_lines)) {
    if (i %% 3 == 0) {
    data_lines[i] <- paste0(data_lines[i], " [1ex]")
    }
  }
  
  return(as.character(data_lines))
  }

  # Function to replace estimates, standard errors, and confidence intervals with dashes
  replace_with_dashes <- function(data_lines) {
  for (i in seq_along(data_lines)) {
    if (grepl("&", data_lines[i])) {
    parts <- unlist(strsplit(data_lines[i], "&"))
    for (j in 3:length(parts)) {
      parts[j] <- "     -     "
    }
    data_lines[i] <- paste(parts, collapse = "&")
    }
  }
  return(data_lines)
  }

  # Read and extract data from the LaTeX tables
  data_categories <- extract_data(input_file_categories)
  if (show_minority) {
  data_minority <- extract_data(input_file_minority)
  }

  # Apply dashes to category rows if set_dashes is TRUE
  if (set_dashes) {
  data_categories <- replace_with_dashes(data_categories)
  }

  # Extract adjusted R^2 values, observations, and number of cities
  extract_info <- function(file_path, pattern) {
  lines <- readLines(file_path)
  info_line <- grep(pattern, lines, value = TRUE)
  return(str_extract_all(info_line, "(?<!\\$)-?\\d+\\.?\\d*(?!\\$)")[[1]])
  }

  adj_r2_categories <- extract_info(input_file_categories, "Adjusted R\\$\\^2\\$")
  if (show_minority) {
  adj_r2_minority <- extract_info(input_file_minority, "Adjusted R\\$\\^2\\$")
  }
  obs <- extract_info(input_file_categories, "Observations")
  num_cities <- extract_info(input_file_categories, "Number of Cities")

  # Determine number of columns
  num_columns <- length(col_names)

  # Combine the extracted data into a standalone LaTeX table (no document wrapper)
  combined_table <- c(
  "\\begin{table}[p]",
  "\\centering",
  "\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}",
  paste0("\\caption{", table_title, "\\\\[0.5em]\\textit{", subtitle, "}}"),
  paste0("\\label{tab:table", table_number, "}"),
  "\\resizebox{\\textwidth}{!}{",
  paste0("\\begin{tabular}{l*{", num_columns, "}{c}}"),
  "\\toprule",
  paste0("& \\multicolumn{", num_columns, "}{c}{", panel_title, "} \\\\"),
  paste0("\\cmidrule(lr){2-", num_columns + 1, "}"),
  paste0("&", paste(sapply(1:num_columns, function(i) paste0("\\multicolumn{1}{c}{", col_names[i], "}")), collapse = "&"), "\\\\"),
  "\\midrule",
  if (show_minority) data_minority,
  if (show_minority) "\\midrule",
  data_categories,
  "\\midrule",
  sapply(additional_rows, function(row) paste0(row[1], "      &", paste(row[-1], collapse = "&"), "\\\\")),
  paste0("Observations      &", paste(obs, collapse = "&"), "\\\\"),
  if (show_minority) paste0("Adjusted R$^2$ (Minority)      &", paste(adj_r2_minority, collapse = "&"), "\\\\"),
  paste0("Adjusted R$^2$ (Category)      &", paste(adj_r2_categories, collapse = "&"), "\\\\"),
  paste0("Number of Cities      &", paste(num_cities, collapse = "&"), "\\\\"),
  "\\bottomrule",
  paste0("\\multicolumn{", num_columns + 1, "}{l}{\\footnotesize Cluster-robust standard errors in parentheses. Clustered at the trial level. 95\\% confidence intervals in square brackets.}\\\\"),
  paste0("\\multicolumn{", num_columns + 1, "}{l}{\\footnotesize \\sym{*} \\(p<0.10\\), \\sym{**} \\(p<0.05\\), \\sym{***} \\(p<0.01\\)}\\\\"),
  "\\end{tabular}",
  "}",
  "\\end{table}"
  )

  # Write the combined table to a new LaTeX file
  writeLines(combined_table, output_file)
}

generate_combined_table <- function(table_title, subtitle, table_number,
                   panel_a_title = "", panel_b_title = "",
                   input_file_categories_top = NULL, input_file_minority_top = NULL,
                   input_file_categories_bottom = NULL, input_file_minority_bottom = NULL,
                   single_panel = FALSE,
                   col_group_labels = NULL, col_numbers = NULL,
                   keep_columns = NULL, set_dashes = TRUE, dash_columns = NULL,
                   show_minority_top = TRUE, show_minority_bottom = TRUE,
                   additional_rows_top = list(c()), additional_rows_bottom = list(c()),
                   add_ex_spacing = TRUE, resize_width = "\\textwidth",
                   output_filename = NULL) {

  comparison_dir <- file.path(output_dir, "Comparison Tables")
  if (!dir.exists(comparison_dir)) {
    dir.create(comparison_dir, recursive = TRUE)
    cat("Created directory for comparison tables:", comparison_dir, "\n")
  }

  if (is.null(output_filename)) {
    output_filename <- paste0("table", table_number, ".tex")
  }
  output_file <- file.path(comparison_dir, output_filename)

  # Default input file paths from comparison_tables.do outputs
  if (is.null(input_file_categories_top)) {
    input_file_categories_top <- paste0(output_dir, "/table", table_number, "_dep_var_1_categories.tex")
  }
  if (is.null(input_file_minority_top)) {
    input_file_minority_top <- paste0(output_dir, "/table", table_number, "_dep_var_1_minority.tex")
  }
  if (!single_panel) {
    if (is.null(input_file_categories_bottom)) {
      input_file_categories_bottom <- paste0(output_dir, "/table", table_number, "_dep_var_2_categories.tex")
    }
    if (is.null(input_file_minority_bottom)) {
      input_file_minority_bottom <- paste0(output_dir, "/table", table_number, "_dep_var_2_minority.tex")
    }
  }

  # Helpers
  extract_data <- function(file_path) {
    lines <- readLines(file_path)
    mid_idx <- grep("\\\\midrule", lines)
    if (length(mid_idx) < 2) return(character())
    data_lines <- lines[(mid_idx[1] + 1):(mid_idx[2] - 1)]
    data_lines
  }

  extract_info <- function(file_path, pattern) {
    lines <- readLines(file_path)
    info_line <- grep(pattern, lines, value = TRUE)
    if (length(info_line) == 0) return(character())
    matches <- regmatches(info_line, gregexpr("(?<!\\$)-?\\d+\\.?\\d*(?!\\$)", info_line, perl = TRUE))
    unlist(matches)
  }

  normalize_label <- function(label) {
    label_trim <- trimws(label)
    if (label_trim == "") return(label)
    if (label_trim == "ofcolor") return("Racial Minority")
    if (label_trim == "othrace") return("Other Race")
    if (grepl("^(2|2\\.)", label_trim) || grepl("aprace\\s*[=\\.]*\\s*2", label_trim) || grepl("apracex\\s*[=\\.]*\\s*2", label_trim)) return("African American")
    if (grepl("^(3|3\\.)", label_trim) || grepl("aprace\\s*[=\\.]*\\s*3", label_trim) || grepl("apracex\\s*[=\\.]*\\s*3", label_trim)) return("Hispanic")
    if (grepl("^(4|4\\.)", label_trim) || grepl("aprace\\s*[=\\.]*\\s*4", label_trim) || grepl("apracex\\s*[=\\.]*\\s*4", label_trim)) return("Asian")
    if (grepl("^(5|5\\.)", label_trim) || grepl("aprace\\s*[=\\.]*\\s*5", label_trim) || grepl("apracex\\s*[=\\.]*\\s*5", label_trim)) return("Other Race")
    label_trim
  }

  format_lines <- function(lines, keep_cols, dash_cols = NULL, add_spacing = TRUE) {
    out <- character()
    for (i in seq_along(lines)) {
      line <- lines[i]
      if (!grepl("&", line, fixed = TRUE)) {
        out <- c(out, line)
        next
      }
      parts <- strsplit(line, "&", fixed = TRUE)[[1]]
      label <- parts[1]
      cols <- parts[-1]
      cols <- gsub("\\\\\\\\", "", cols)
      cols <- gsub("\\[1ex\\]", "", cols)

      label_trim <- trimws(label)
      if (label_trim != "") {
        prefix <- sub("^(\\s*).*", "\\1", label)
        label <- paste0(prefix, normalize_label(label_trim))
      }

      if (!is.null(dash_cols) && length(dash_cols) > 0) {
        for (col in dash_cols) {
          if (col <= length(cols)) cols[col] <- "     -     "
        }
      }

      if (!is.null(keep_cols) && length(keep_cols) > 0) {
        cols <- cols[keep_cols]
      }

      cols[length(cols)] <- paste0(cols[length(cols)], " \\\\")
      line_out <- paste(c(label, cols), collapse = "&")
      if (add_spacing && (i %% 3 == 0)) {
        line_out <- paste0(line_out, " [1ex]")
      }
      out <- c(out, line_out)
    }
    out
  }

  # Determine which columns to keep
  if (is.null(keep_columns)) {
    keep_columns <- c(1, 2, 3)
  }
  num_columns <- length(keep_columns)

  # Default column headers
  if (is.null(col_group_labels)) {
    if (num_columns == 2) {
      col_group_labels <- c("Original Data", "Updated City Name")
    } else {
      col_group_labels <- c("Original Data", "Correct Race Only", "Updated City Name \\& Correct Race")
    }
  }
  if (is.null(col_numbers)) {
    col_numbers <- paste0("(", seq_len(num_columns), ")")
  }

  if (is.null(dash_columns) && set_dashes && num_columns == 3) {
    dash_columns <- 2
  }
  if (!set_dashes) {
    dash_columns <- NULL
  }

  build_panel <- function(panel_title, input_file_minority, input_file_categories,
                          show_minority, additional_rows) {
    data_categories <- extract_data(input_file_categories)
    data_categories <- format_lines(data_categories, keep_columns, dash_columns, add_ex_spacing)

    if (show_minority) {
      data_minority <- extract_data(input_file_minority)
      data_minority <- format_lines(data_minority, keep_columns, NULL, add_ex_spacing)
    }

    obs <- extract_info(input_file_categories, "Observations")
    adj_r2_categories <- extract_info(input_file_categories, "Adjusted R\\$\\^2\\$")
    num_cities <- extract_info(input_file_categories, "Number of Cities")

    if (show_minority) {
      adj_r2_minority <- extract_info(input_file_minority, "Adjusted R\\$\\^2\\$")
    }

    if (length(keep_columns) > 0) {
      obs <- obs[keep_columns]
      adj_r2_categories <- adj_r2_categories[keep_columns]
      num_cities <- num_cities[keep_columns]
      if (show_minority) adj_r2_minority <- adj_r2_minority[keep_columns]
    }

    header_lines <- c()
    if (panel_title != "") {
      header_lines <- c(header_lines,
                        paste0("&\\multicolumn{", num_columns, "}{c}{", panel_title, "} \\\\"),
                        paste0("\\cmidrule(lr){2-", num_columns + 1, "}"))
    }
    header_lines <- c(header_lines,
                      paste0("&", paste(sapply(col_group_labels, function(x) paste0("\\multicolumn{1}{c}{", x, "}")), collapse = "&"), "\\\\"),
                      paste0("\\cmidrule(lr){2-2}", paste(sapply(3:(num_columns + 1), function(i) paste0("\\cmidrule(lr){", i, "-", i, "}")), collapse = "")),
                      paste0("&", paste(sapply(col_numbers, function(x) paste0("\\multicolumn{1}{c}{", x, "}")), collapse = "&"), "\\\\"),
                      "\\midrule")

    panel_lines <- header_lines
    if (show_minority) {
      panel_lines <- c(panel_lines, data_minority, "\\midrule")
    }
    panel_lines <- c(panel_lines, data_categories, "\\midrule")

    # Add any extra rows (e.g., controls)
    if (length(additional_rows) > 0 && length(additional_rows[[1]]) > 0) {
      for (row in additional_rows) {
        row_label <- row[1]
        row_vals <- row[-1]
        if (length(keep_columns) > 0 && length(row_vals) >= max(keep_columns)) {
          row_vals <- row_vals[keep_columns]
        }
        panel_lines <- c(panel_lines, paste0(row_label, "      &", paste(row_vals, collapse = "&"), "\\\\"))
      }
    }

    panel_lines <- c(panel_lines,
                     paste0("Observations      &", paste(obs, collapse = "&"), "\\\\"))
    if (show_minority) {
      panel_lines <- c(panel_lines,
                       paste0("Adjusted R$^2$ (Minority)      &", paste(adj_r2_minority, collapse = "&"), "\\\\"))
    }
    panel_lines <- c(panel_lines,
                     paste0("Adjusted R$^2$ (Category)      &", paste(adj_r2_categories, collapse = "&"), "\\\\"),
                     paste0("Number of Cities      &", paste(num_cities, collapse = "&"), "\\\\"))

    panel_lines
  }

  panel_a <- build_panel(panel_a_title, input_file_minority_top, input_file_categories_top,
                         show_minority_top, additional_rows_top)
  if (!single_panel) {
    panel_b <- build_panel(panel_b_title, input_file_minority_bottom, input_file_categories_bottom,
                           show_minority_bottom, additional_rows_bottom)
  }

  table_lines <- c(
    "\\begin{table}[p]",
    "\\centering",
    "\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}",
    paste0("\\caption{", table_title, " \\\\ \\small{", subtitle, "}}"),
    paste0("\\label{tab:table", table_number, "}"),
    paste0("\\resizebox{", resize_width, "}{!}{"),
    paste0("\\begin{tabular}{l*{", num_columns, "}{c}}"),
    "\\toprule",
    panel_a
  )

  if (!single_panel) {
    table_lines <- c(table_lines, "\\midrule", panel_b)
  }

  table_lines <- c(table_lines,
                   "\\bottomrule",
                   paste0("\\multicolumn{", num_columns + 1, "}{l}{\\footnotesize Cluster-robust standard errors in parentheses. Clustered at the trial level. 95\\% confidence intervals in square brackets.}\\\\"),
                   paste0("\\multicolumn{", num_columns + 1, "}{l}{\\footnotesize \\sym{*} \\(p<0.10\\), \\sym{**} \\(p<0.05\\), \\sym{***} \\(p<0.01\\). \\cisentence }\\\\"),
                   "\\end{tabular}",
                   "}",
                   "\\end{table}")

  writeLines(table_lines, output_file)
}


# ### Generate Tables Showing Original and Corrections

# # Call function for Table 7
# generate_combined_table(
#   table_title = "Differences in Results for Racial Composition of Recommended Neighbourhood",
#   subtitle = "Table 7, CT2022",
#   panel_a_title = "White Household Income Share in High Income Neighbourhoods (Column 1)",
#   panel_b_title = "White Household Income Share in Low Income Neighbourhoods (Column 3)",
#   table_number = 7,
#   single_panel = FALSE,
#   set_dashes = TRUE,
#   show_minority_top = TRUE,
#   show_minority_bottom = TRUE
# )

# # Call function for Table 9
# generate_combined_table(
#   table_title = "Differences in Results for Local Pollution Exposure",
#   subtitle = "Table 9, CT2022",
#   panel_a_title = "Differences in Superfund Proximity, Whole Sample (Panel A, Column 1)",
#   panel_b_title = "Differences in Superfund Proximity, Mothers Only (Panel B, Column 1)",
#   table_number = 9,
#   single_panel = FALSE,
#   set_dashes = TRUE,
#   show_minority_top = TRUE,
#   show_minority_bottom = TRUE
# )

# # Call function for Table 11
# generate_combined_table(
#   table_title = "Differences in Results for Low-Poverty Neighbourhood Recommendations",
#   subtitle = "Table 11 Column 1, CT2022",
#   panel_a_title = "",
#   panel_b_title = "",
#   table_number = 11,
#   single_panel = TRUE,
#   set_dashes = FALSE,
#   show_minority_top = FALSE,
#   show_minority_bottom = FALSE,
#   two_columns = TRUE
# )

# # Call function for Table 12
# generate_combined_table(
#   table_title = "Differences in Results for Median Income",
#   subtitle = "Table 12 Column 1, CT 2022",
#   panel_a_title = "",
#   panel_b_title = "",
#   table_number = 12,
#   single_panel = TRUE,
#   set_dashes = FALSE,
#   show_minority_top = FALSE,
#   show_minority_bottom = FALSE,
#   two_columns = TRUE
# )

# # Call the function for Table 14
# generate_combined_table(
#   table_title = "Differences in Results for Recommended Home's Log Sale Price",
#   subtitle = " Table 14 Panel B Column 5, CT2022",
#   panel_a_title = "",
#   panel_b_title = "",
#   table_number = 14,
#   single_panel = TRUE,
#   show_minority_top = TRUE,
#   show_minority_bottom = TRUE,
#   two_columns = FALSE
# )

### Generate Tables from comparison_tables.do outputs (approximate examples)

generate_combined_table(
  table_title = "Differences in Recommendations and Availability of Advertised Properties",
  subtitle = "C\\&T Table 5, Columns 2 and 4",
  table_number = 5,
  panel_a_title = "Differences in Number of Recommendations (Panel A)",
  panel_b_title = "Differences in Home Availability (Panel B)",
  single_panel = FALSE,
  set_dashes = TRUE,
  show_minority_top = TRUE,
  show_minority_bottom = TRUE,
  additional_rows_top = list(
    c("ln(price), advertised home", "Yes", "Yes", "Yes"),
    c("Racial composition, advertised home", "Yes", "Yes", "Yes")
  ),
  additional_rows_bottom = list(
    c("ln(price), advertised home", "Yes", "Yes", "Yes"),
    c("Racial composition, advertised home", "Yes", "Yes", "Yes")
  ),
  resize_width = "0.75\\columnwidth",
  output_filename = "table5.tex"
)

generate_combined_table(
  table_title = "Discriminatory Steering and Neighborhood Racial Composition",
  subtitle = "C\\&T Table 6, Column 5",
  table_number = 6,
  panel_a_title = "",
  single_panel = TRUE,
  set_dashes = TRUE,
  show_minority_top = TRUE,
  additional_rows_top = list(
    c("Share white, advertised home", "Yes", "Yes", "Yes"),
    c("ln(price), advertised home", "Yes", "Yes", "Yes"),
    c("Racial composition, advertised home", "Yes", "Yes", "Yes"),
    c("Poverty share advertised home", "Yes", "Yes", "Yes")
  ),
  resize_width = "0.9\\columnwidth",
  output_filename = "table6.tex"
)

generate_combined_table(
  table_title = "Discriminatory Steering and Neighborhood Racial Composition by Income",
  subtitle = "C\\&T Table 7, Column 1, Panel 1",
  table_number = 7,
  panel_a_title = "",
  single_panel = TRUE,
  set_dashes = TRUE,
  show_minority_top = TRUE,
  additional_rows_top = list(
    c("Share White Advert Home", "Yes", "Yes", "Yes"),
    c("ln(price), advertised home", "Yes", "Yes", "Yes"),
    c("Racial composition, advertised home", "Yes", "Yes", "Yes"),
    c("Poverty Share Advert Home", "Yes", "Yes", "Yes")
  ),
  resize_width = "0.9\\columnwidth",
  output_filename = "table7_control(1).tex"
)

generate_combined_table(
  table_title = "Discrimination Steering and Neighborhood Effects",
  subtitle = "C\\&T Table 8",
  table_number = 8,
  panel_a_title = "School Quality and Neighbourhood Safety: Elementary School Rating (Panel A, Column 4)",
  panel_b_title = "American Community Survey: Poverty Rate (Panel B, Column 1)",
  single_panel = FALSE,
  set_dashes = TRUE,
  show_minority_top = TRUE,
  show_minority_bottom = TRUE,
  additional_rows_top = list(
    c("ln(price) advertised home", "Yes", "Yes", "Yes"),
    c("Racial composition, advertised home", "Yes", "Yes", "Yes"),
    c("Outcome, advertised home", "Yes", "Yes", "Yes")
  ),
  additional_rows_bottom = list(
    c("ln(price) advertised home", "Yes", "Yes", "Yes"),
    c("Racial composition, advertised home", "Yes", "Yes", "Yes"),
    c("Outcome, advertised home", "Yes", "Yes", "Yes")
  ),
  resize_width = "0.8\\columnwidth",
  output_filename = "table8_trial.tex"
)

generate_combined_table(
  table_title = "Differences in Results for Local Pollution Exposure",
  subtitle = "C\\&T Table 9",
  table_number = 9,
  panel_a_title = "Differences in Superfund Proximity, Whole Sample (Panel A, Column 1)",
  panel_b_title = "Differences in Superfund Proximity, Mothers Only (Panel B, Column 1)",
  single_panel = FALSE,
  set_dashes = TRUE,
  show_minority_top = TRUE,
  show_minority_bottom = TRUE,
  resize_width = "0.75\\columnwidth",
  output_filename = "table9.tex"
)

generate_combined_table(
  table_title = "Discriminatory Steering and Neighbourhood Effects (Mothers)",
  subtitle = "C\\&T Table 10, Columns 1 and 4, Panel A",
  table_number = "10A",
  panel_a_title = "School Quality and Neighbourhood Safety: Elementary School Scores (Panel A Column 1)",
  panel_b_title = "School Quality and Neighbourhood Safety: Middle School Scores (Panel A Column 2)",
  input_file_categories_top = paste0(output_dir, "/table10A_dep_var_1_categories.tex"),
  input_file_minority_top = paste0(output_dir, "/table10A_dep_var_1_minority.tex"),
  input_file_categories_bottom = paste0(output_dir, "/table10A_dep_var_2_categories.tex"),
  input_file_minority_bottom = paste0(output_dir, "/table10A_dep_var_2_minority.tex"),
  single_panel = FALSE,
  set_dashes = TRUE,
  show_minority_top = TRUE,
  show_minority_bottom = TRUE,
  additional_rows_top = list(
    c("ln(price) advertised home", "Yes", "Yes", "Yes"),
    c("Racial composition, advertised home", "Yes", "Yes", "Yes"),
    c("Outcome, advertised home", "Yes", "Yes", "Yes")
  ),
  additional_rows_bottom = list(
    c("ln(price) advertised home", "Yes", "Yes", "Yes"),
    c("Racial composition, advertised home", "Yes", "Yes", "Yes"),
    c("Outcome, advertised home", "Yes", "Yes", "Yes")
  ),
  resize_width = "0.9\\columnwidth",
  output_filename = "Table10A_trial.tex"
)

generate_combined_table(
  table_title = "Discriminatory Steering and Neighbourhood Effects (Mothers)",
  subtitle = "C\\&T Table 10, Columns 2 and 4, Panel B",
  table_number = "10B",
  panel_a_title = "American Community Survey: High Skill (Panel B Column 2)",
  panel_b_title = "American Community Survey: Single Parent Household (Panel B Column 4)",
  input_file_categories_top = paste0(output_dir, "/table10B_dep_var_1_categories.tex"),
  input_file_minority_top = paste0(output_dir, "/table10B_dep_var_1_minority.tex"),
  input_file_categories_bottom = paste0(output_dir, "/table10B_dep_var_2_categories.tex"),
  input_file_minority_bottom = paste0(output_dir, "/table10B_dep_var_2_minority.tex"),
  single_panel = FALSE,
  set_dashes = TRUE,
  show_minority_top = TRUE,
  show_minority_bottom = TRUE,
  additional_rows_top = list(
    c("ln(price) advertised home", "Yes", "Yes", "Yes"),
    c("Racial composition, advertised home", "Yes", "Yes", "Yes"),
    c("Outcome, advertised home", "Yes", "Yes", "Yes")
  ),
  additional_rows_bottom = list(
    c("ln(price) advertised home", "Yes", "Yes", "Yes"),
    c("Racial composition, advertised home", "Yes", "Yes", "Yes"),
    c("Outcome, advertised home", "Yes", "Yes", "Yes")
  ),
  resize_width = "0.90\\columnwidth",
  output_filename = "Table10B_trial.tex"
)

generate_combined_table(
  table_title = "Differences in Results for Low-Poverty Neighbourhood Recommendations",
  subtitle = "C\\&T Table 11 Column 1",
  table_number = 11,
  panel_a_title = "",
  single_panel = TRUE,
  keep_columns = c(1, 3),
  col_group_labels = c("Original Data", "Updated City Name"),
  col_numbers = c("(1)", "(2)"),
  set_dashes = FALSE,
  show_minority_top = FALSE,
  resize_width = "\\textwidth",
  output_filename = "table11.tex"
)

generate_combined_table(
  table_title = "Differences in Results for Median Income",
  subtitle = "C\\&T Table 12 Column 1",
  table_number = 12,
  panel_a_title = "",
  single_panel = TRUE,
  keep_columns = c(1, 3),
  col_group_labels = c("Original Data", "Updated City Name"),
  col_numbers = c("(1)", "(2)"),
  set_dashes = FALSE,
  show_minority_top = FALSE,
  resize_width = "\\textwidth",
  output_filename = "table12.tex"
)

generate_combined_table(
  table_title = "Discriminatory Steering by Implied Preferences for Neighbourhood Attributes",
  subtitle = "C\\&T Table 13",
  table_number = 13,
  panel_a_title = "Housing Search Platform: Elementary School Quality (Panel A, Column 4)",
  panel_b_title = "American Community Survey: High Skill Neighbourhood (Panel B, Column 2)",
  single_panel = FALSE,
  set_dashes = TRUE,
  show_minority_top = TRUE,
  show_minority_bottom = TRUE,
  resize_width = "0.75\\columnwidth",
  output_filename = "table13.tex"
)

generate_combined_table(
  table_title = "Differences in Results for Recommended Home's Log Sale Price",
  subtitle = "C\\&T Table 14 Panel B, Column 5",
  table_number = 14,
  panel_a_title = "",
  single_panel = TRUE,
  set_dashes = TRUE,
  show_minority_top = TRUE,
  resize_width = "\\textwidth",
  output_filename = "table14.tex"
)



### Generate Full Replication Tables for Appendix B



# Corrected Table 5
generate_corrected_table(
  table_title = "Discriminatory Steering and Availability of Advertised Properties",
  subtitle = "Table 5, C\\&T 2022",
  table_number = 5,
  panel_title = "Dependent Variable",
  input_file_categories = paste0(output_dir, "/table5_categories_corrected.tex"),
  input_file_minority = paste0(output_dir, "/table5_minority_corrected.tex"),
  col_names = c("(1)", "(2)", "(3)", "(4)"),
  additional_rows = list( c("ln(price) advertised home", "No", "Yes", "No", "Yes"),
                          c("Racial composition advertised home", "No", "Yes", "No", "Yes")),
  set_dashes = FALSE,
  show_minority = TRUE,
  corrected = TRUE
)

# Corrected Table 6
generate_corrected_table(
  table_title = "Discriminatory Steering and Neighborhood Racial Composition",
  subtitle = "Table 6, C\\&T 2022",
  table_number = 6,
  panel_title = "Dependent Variable: White Household Share",
  input_file_categories = paste0(output_dir, "/table6_categories_corrected.tex"),
  input_file_minority = paste0(output_dir, "/table6_minority_corrected.tex"),
  col_names = c("(1)", "(2)", "(3)", "(4)", "(5)"),
  additional_rows = list( c("Share white advertised home", "No", "Yes", "Yes", "Yes", "Yes"),
                          c("ln(price) advertised home", "No", "No", "Yes", "Yes", "Yes"),
                          c("Racial composition advertised home", "No", "No", "No", "Yes", "Yes"),
                          c("Poverty share advertised home", "No", "No", "No", "No", "Yes")),
  set_dashes = FALSE,
  show_minority = TRUE
)

# Corrected Table 7
generate_corrected_table(
  table_title = "Discriminatory Steering and Neighborhood Racial Composition by Income",
  subtitle = "Table 7, C\\&T 2022",
  table_number = 7,
  panel_title = "Dependent Variable: White Household Share by Income",
  input_file_categories = paste0(output_dir, "/table7_categories_corrected.tex"),
  input_file_minority = paste0(output_dir, "/table7_minority_corrected.tex"),
  col_names = c("High Income", "Middle Income", "Low Income"),
  additional_rows = list( c("Share white advertised home", "Yes", "Yes", "Yes"),
                          c("ln(price) advertised home", "Yes", "Yes", "Yes"),
                          c("Racial composition advertised home", "Yes", "Yes", "Yes"),
                          c("Poverty share advertised home", "Yes", "Yes", "Yes")),
  set_dashes = FALSE,
  show_minority = TRUE
)

# Corrected Table 8A1
generate_corrected_table(
  table_title = "Discriminatory Steering and Neighborhood Effects",
  subtitle = "Table 8A Columns 1 and 2, C\\&T 2022",
  table_number = 8.11,
  panel_title = "School Specific Test Scores",
  input_file_categories = paste0(output_dir, "/table8A1_categories_corrected.tex"),
  input_file_minority = paste0(output_dir, "/table8A1_minority_corrected.tex"),
  col_names = c("Elementary School (1)", "Middle School (2)"),
  additional_rows = list( c("ln(price) advertised home", "Yes", "Yes"),
                          c("Racial composition advertised home", "Yes", "Yes"),
                          c("Outcome advertised home", "Yes", "Yes")),
  set_dashes = FALSE,
  show_minority = TRUE
)

# Corrected Table 8A2
generate_corrected_table(
  table_title = "Discriminatory Steering and Neighborhood Effects",
  subtitle = "Table 8A Columns 3 and 4, C\\&T 2022",
  table_number = 8.12,
  panel_title = "School Specific Test Scores",
  input_file_categories = paste0(output_dir, "/table8A2_categories_corrected.tex"),
  input_file_minority = paste0(output_dir, "/table8A2_minority_corrected.tex"),
  col_names = c("Assaults (3)", "Elementary School (4)"),
  additional_rows = list( c("ln(price) advertised home", "Yes", "Yes"),
                          c("Racial composition advertised home", "Yes", "Yes"),
                          c("Outcome advertised home", "Yes", "Yes")),
  set_dashes = FALSE,
  show_minority = TRUE
)

# Corrected Table 8B
generate_corrected_table(
  table_title = "Discriminatory Steering and Neighborhood Effects",
  subtitle = "Table 8 Panel B, C\\&T 2022",
  table_number = 8.2,
  panel_title = "American Community Survey",
  input_file_categories = paste0(output_dir, "/table8B_categories_corrected.tex"),
  input_file_minority = paste0(output_dir, "/table8B_minority_corrected.tex"),
  col_names = c("Poverty Rate (1)", "High Skill (2)", "College (3)", "Single-Parent Household (4)", "Ownership Rate (5)"),
  additional_rows = list( c("ln(price) advertised home", "Yes", "Yes", "Yes", "Yes", "Yes"),
                          c("Racial composition advertised home", "Yes", "Yes", "Yes", "Yes", "Yes"),
                          c("Outcome advertised home", "Yes", "Yes", "Yes", "Yes", "Yes")),
  set_dashes = FALSE,
  show_minority = TRUE,
  corrected = TRUE
)

# Corrected Table 9A
generate_corrected_table(
  table_title = "Discriminatory Steering and Local Pollution Exposures",
  subtitle = "Table 9 Panel A, C\\&T 2022",
  table_number = 9.1,
  panel_title = "Pollution: Differences for Entire Sample",
  input_file_categories = paste0(output_dir, "/table9A_categories_corrected.tex"),
  input_file_minority = paste0(output_dir, "/table9A_minority_corrected.tex"),
  col_names = c("Superfund", "Toxics", "PM"),
  additional_rows = list( c("ln(price) advertised home", "Yes", "Yes", "Yes"),
                          c("Racial composition advertised home", "Yes", "Yes", "Yes"),
                          c("Outcome advertised home", "Yes", "Yes", "Yes")),
  set_dashes = FALSE,
  show_minority = TRUE,
  corrected = TRUE
)

# Corrected Table 9B
generate_corrected_table(
  table_title = "Discriminatory Steering and Local Pollution Exposures",
  subtitle = "Table 9 Panel B, C\\&T 2022",
  table_number = 9.2,
  panel_title = "Pollution: Differences for Mothers",
  input_file_categories = paste0(output_dir, "/table9B_categories_corrected.tex"),
  input_file_minority = paste0(output_dir, "/table9B_minority_corrected.tex"),
  col_names = c("Superfund", "Toxics", "PM"),
  additional_rows = list( c("ln(price) advertised home", "Yes", "Yes", "Yes"),
                          c("Racial composition advertised home", "Yes", "Yes", "Yes"),
                          c("Outcome advertised home", "Yes", "Yes", "Yes")),
  set_dashes = FALSE,
  show_minority = TRUE,
  corrected = TRUE
)

# Corrected Table 10A1
generate_corrected_table(
  table_title = "Discriminatory Steering and Neighborhood Effects (Mothers)",
  subtitle = "Table 10A Columns 1 and 2, C\\&T 2022",
  table_number = 10.11,
  panel_title = "School Specific Test Scores",
  input_file_categories = paste0(output_dir, "/table10A1_categories_corrected.tex"),
  input_file_minority = paste0(output_dir, "/table10A1_minority_corrected.tex"),
  col_names = c("Elementary School (1)", "Middle School (2)"),
  additional_rows = list( c("ln(price) advertised home", "Yes", "Yes"),
                          c("Racial composition advertised home", "Yes", "Yes"),
                          c("Outcome advertised home", "Yes", "Yes")),
  set_dashes = FALSE,
  show_minority = TRUE
)

# Corrected Table 10A2
generate_corrected_table(
  table_title = "Discriminatory Steering and Neighborhood Effects (Mothers)",
  subtitle = "Table 10A Columns 3 and 4, C\\&T 2022",
  table_number = 10.12,
  panel_title = "School Specific Test Scores",
  input_file_categories = paste0(output_dir, "/table10A2_categories_corrected.tex"),
  input_file_minority = paste0(output_dir, "/table10A2_minority_corrected.tex"),
  col_names = c("Assaults (3)", "Elementary School (4)"),
  additional_rows = list( c("ln(price) advertised home", "Yes", "Yes"),
                          c("Racial composition advertised home", "Yes", "Yes"),
                          c("Outcome advertised home", "Yes", "Yes")),
  set_dashes = FALSE,
  show_minority = TRUE
)

# Corrected Table 10B
generate_corrected_table(
  table_title = "Discriminatory Steering and Neighborhood Effects (Mothers)",
  subtitle = "Table 10 Panel B, C\\&T 2022",
  table_number = 10.2,
  panel_title = "American Community Survey",
  input_file_categories = paste0(output_dir, "/table10B_categories_corrected.tex"),
  input_file_minority = paste0(output_dir, "/table10B_minority_corrected.tex"),
  col_names = c("Poverty Rate (1)", "High Skill (2)", "College (3)", "Single-Parent Household (4)", "Ownership Rate (5)"),
  additional_rows = list( c("ln(price) advertised home", "Yes", "Yes", "Yes", "Yes", "Yes"),
                          c("Racial composition advertised home", "Yes", "Yes", "Yes", "Yes", "Yes"),
                          c("Outcome advertised home", "Yes", "Yes", "Yes", "Yes", "Yes")),
  set_dashes = FALSE,
  show_minority = TRUE,
  corrected = TRUE
)

# Corrected Table 11
generate_corrected_table(
  table_title = "Discriminatory Steering: Low Poverty Neighbourhoods",
  subtitle = "Table 11, C\\&T 2022",
  table_number = 11,
  panel_title = "Dependent Variable",
  input_file_categories = paste0(output_dir, "/table11_categories_corrected.tex"),
  input_file_minority = paste0(output_dir, "/table11_minority_corrected.tex"),
  col_names = c("Low Poverty (1)", "Low Poverty: Families (2)", "Low Poverty: Moms (3)", "Low Poverty High Dad (4)", "Low Poverty High Dad: Families (5)", "Low Poverty High Dad: Moms (6)"),
  additional_rows = list(c()),
  set_dashes = FALSE,
  show_minority = FALSE,
  corrected = TRUE
)

# Corrected Table 12
generate_corrected_table(
  table_title = "Discriminatory Steering: Median Income in Neighbourhoods",
  subtitle = "Table 12, C\\&T 2022",
  table_number = 12,
  panel_title = "Dependent Variable: log(Median Income)",
  input_file_categories = paste0(output_dir, "/table12_categories_corrected.tex"),
  input_file_minority = paste0(output_dir, "/table12_minority_corrected.tex"),
  col_names = c("All Testers", "Families", "Moms"),
  set_dashes = FALSE,
  show_minority = FALSE,
  corrected = TRUE
)

# Corrected Table 13A1
generate_corrected_table(
  table_title = "Discriminatory Steering by Implied Preferences for Neighbourhood Attributes",
  subtitle = "Table 13A Columns 1 and 2, C\\&T 2022",
  table_number = 13.11,
  panel_title = "School Specific Test Scores",
  input_file_categories = paste0(output_dir, "/table13A1_categories_corrected.tex"),
  input_file_minority = paste0(output_dir, "/table13A1_minority_corrected.tex"),
  col_names = c("Elementary School (1)", "Middle School (2)"),
  additional_rows = list( c("ln(price) advertised home", "Yes", "Yes"),
                          c("Racial composition advertised home", "Yes", "Yes"),
                          c("Outcome advertised home", "Yes", "Yes")),
  set_dashes = FALSE,
  show_minority = TRUE
)

# Corrected Table 13A2
generate_corrected_table(
  table_title = "Discriminatory Steering by Implied Preferences for Neighbourhood Attributes",
  subtitle = "Table 13A Columns 3 and 4, C\\&T 2022",
  table_number = 13.12,
  panel_title = "School Specific Test Scores",
  input_file_categories = paste0(output_dir, "/table13A2_categories_corrected.tex"),
  input_file_minority = paste0(output_dir, "/table13A2_minority_corrected.tex"),
  col_names = c("Assaults (3)", "Elementary School (4)"),
  additional_rows = list( c("ln(price) advertised home", "Yes", "Yes"),
                          c("Racial composition advertised home", "Yes", "Yes"),
                          c("Outcome advertised home", "Yes", "Yes")),
  set_dashes = FALSE,
  show_minority = TRUE
)



# Corrected Table 13B
generate_corrected_table(
  table_title = "Discriminatory Steering by Implied Preferences for Neighbourhood Attributes",
  subtitle = "Table 13 Panel B, C\\&T 2022",
  table_number = 13.2,
  panel_title = "American Community Survey",
  input_file_categories = paste0(output_dir, "/table13B_categories_corrected.tex"),
  input_file_minority = paste0(output_dir, "/table13B_minority_corrected.tex"),
  col_names = c("Poverty Rate (1)", "High Skill (2)", "College (3)", "Single-Parent Household (4)", "Ownership Rate (5)"),
  additional_rows = list( c("ln(price) advertised home", "Yes", "Yes", "Yes", "Yes", "Yes"),
                          c("Racial composition advertised home", "Yes", "Yes", "Yes", "Yes", "Yes"),
                          c("Outcome advertised home", "Yes", "Yes", "Yes", "Yes", "Yes")),
  set_dashes = FALSE,
  show_minority = TRUE,
  corrected = TRUE
)

# Corrected Table 14B
generate_corrected_table(
  table_title = "Discriminatory Steering and Later Transactions",
  subtitle = "Table 14 Panel B, C\\&T 2022",
  table_number = 14.2,
  panel_title = "B. Dependent Variable: Logarithm of Price",
  input_file_categories = paste0(output_dir, "/table14B_categories_corrected.tex"),
  input_file_minority = paste0(output_dir, "/table14B_minority_corrected.tex"),
  col_names = c("(1)", "(2)", "(3)", "(4)"),
  additional_rows = list(
    c("Share white advertised home", "No", "Yes", "Yes", "Yes"),
    c("ln(price) advertised home", "No", "No", "Yes", "Yes"),
    c("Racial composition advertised home", "No", "No", "No", "Yes"),
    c("Year", "Yes", "Yes", "Yes", "Yes"),
    c("Month of year", "Yes", "Yes", "Yes", "Yes")
  ),
  set_dashes = FALSE,
  show_minority = TRUE,
  corrected = TRUE
)
