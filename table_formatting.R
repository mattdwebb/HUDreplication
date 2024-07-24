# Load necessary libraries
library(stringr)

generate_combined_table <- function(table_title, panel_a_title, panel_b_title, table_number, single_panel, set_dashes = TRUE, top_rows = list(), bottom_rows = list(), show_minority_top = TRUE, show_minority_bottom = TRUE, two_columns = FALSE) {
  
  # Automatically generate input_files and output_file based on table_number
  input_files <- list(
    data1_minority = paste0("Output/table", table_number, "_dep_var_1_minority.tex"),
    data1_categories = paste0("Output/table", table_number, "_dep_var_1_categories.tex"),
    data2_minority = paste0("Output/table", table_number, "_dep_var_2_minority.tex"),
    data2_categories = paste0("Output/table", table_number, "_dep_var_2_categories.tex")
  )
  output_file <- paste0("Output/combined_table", table_number, ".tex")

  # Function to extract relevant data from LaTeX table
  extract_data <- function(file_path, two_columns) {
    lines <- readLines(file_path)
    start <- grep("\\\\midrule", lines)[1] + 1
    end <- grep("\\\\midrule", lines)[2] - 1
    data_lines <- lines[start:end]
    
    if (two_columns) {
      data_lines <- lapply(data_lines, function(line) {
        parts <- unlist(strsplit(line, "&"))
        if (length(parts) > 2) {
          new_line <- paste(parts[c(1, 2, 4)], collapse = "&")
          return(as.character(new_line))  # Ensure the result is a character string
        }
        return(as.character(line))  # Ensure the result is a character string
      })
    }
    
    # Add [1ex] at the end of every three rows
    for (i in seq_along(data_lines)) {
      if (i %% 3 == 0) {
        data_lines[i] <- paste0(data_lines[i], " [1ex]")
      }
    }
    
    return(as.character(data_lines))  # Ensure the result is a character vector
  }

  # Function to replace estimates, standard errors, and confidence intervals with dashes in column 2
  replace_with_dashes <- function(data_lines) {
    for (i in seq_along(data_lines)) {
      if (grepl("&", data_lines[i])) {
        parts <- unlist(strsplit(data_lines[i], "&"))
        if (length(parts) >= 3) {
          parts[3] <- "     -     "
          data_lines[i] <- paste(parts, collapse = "&")
          }
        }
      }
    return(data_lines)
  }

  # Read and extract data from the LaTeX tables
  if (show_minority_top) {
    data1_minority <- extract_data(input_files$data1_minority, two_columns)
  }
  data1_categories <- extract_data(input_files$data1_categories, two_columns)
  if (show_minority_bottom) {
    data2_minority <- extract_data(input_files$data2_minority, two_columns)
  }
  data2_categories <- extract_data(input_files$data2_categories, two_columns)

  # Apply dashes to category rows if set_dashes is TRUE
  if (set_dashes) {
    data1_categories <- replace_with_dashes(data1_categories)
    data2_categories <- replace_with_dashes(data2_categories)
  }

  # Extract adjusted R^2 values
  extract_adj_r2 <- function(file_path) {
    lines <- readLines(file_path)
    adj_r2_line <- grep("Adjusted R\\$\\^2\\$", lines, value = TRUE)
    return(adj_r2_line)
  }

  if (show_minority_top) {
    adj_r2_1 <- str_extract_all(extract_adj_r2(input_files$data1_minority), "-?\\d+\\.\\d+")[[1]]
  }
  adj_r2_2 <- str_extract_all(extract_adj_r2(input_files$data1_categories), "-?\\d+\\.\\d+")[[1]]
  if (show_minority_bottom) {
    adj_r2_3 <- str_extract_all(extract_adj_r2(input_files$data2_minority), "-?\\d+\\.\\d+")[[1]]
  }
  adj_r2_4 <- str_extract_all(extract_adj_r2(input_files$data2_categories), "-?\\d+\\.\\d+")[[1]]

  # Extract observations only from categories tables
  extract_obs <- function(file_path) {
    lines <- readLines(file_path)
    obs_line <- grep("Observations", lines, value = TRUE)
    return(obs_line)
  }

  obs_2 <- str_extract_all(extract_obs(input_files$data1_categories), "\\d+")[[1]]
  obs_4 <- str_extract_all(extract_obs(input_files$data2_categories), "\\d+")[[1]]

  # Extract number of cities only from categories tables
  extract_num_cities <- function(file_path) {
    lines <- readLines(file_path)
    num_cities_line <- grep("Number of Cities", lines, value = TRUE)
    return(num_cities_line)
  }

  num_cities_2 <- str_extract_all(extract_num_cities(input_files$data1_categories), "\\d+")[[1]]
  num_cities_4 <- str_extract_all(extract_num_cities(input_files$data2_categories), "\\d+")[[1]]

  # Combine the extracted data into a single LaTeX table
  combined_table <- c(
    "\\documentclass{article}",
    "\\usepackage{pdflscape}",
    "\\usepackage{booktabs}",
    "\\begin{document}",
    "\\begin{table}[p]",
    "\\centering",
    "\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}",
    paste0("\\caption{", table_title, "}"),
    paste0("\\label{tab:table", table_number, "}"),
    "\\resizebox{\\textwidth}{!}{",
    paste0("\\begin{tabular}{l*", if (two_columns) "2" else "3", "{c}}"),
    "\\toprule",
    if (!single_panel) paste0("&\\multicolumn{", if (two_columns) "2" else "3", "}{c}{", panel_a_title, "}\\\\"),
    if (!single_panel) paste0("\\cmidrule{2-", if (two_columns) "3" else "4", "}"),
    if (two_columns) paste0("&\\multicolumn{1}{c}{Original Data}&\\multicolumn{1}{c}{Updated City Name}\\\\") else paste0("&\\multicolumn{1}{c}{Original Data}&\\multicolumn{1}{c}{Correct Race Only}&\\multicolumn{1}{c}{Updated City Name \\& Correct Race}\\\\"),
    if (two_columns) paste0("\\cmidrule(lr){2-2}\\cmidrule(lr){3-3}") else paste0("\\cmidrule(lr){2-2}\\cmidrule(lr){3-3}\\cmidrule(lr){4-4}"),
    if (two_columns) paste0("&\\multicolumn{1}{c}{(1)}         &\\multicolumn{1}{c}{(2)}         \\\\") else paste0("&\\multicolumn{1}{c}{(1)}         &\\multicolumn{1}{c}{(2)}         &\\multicolumn{1}{c}{(3)}         \\\\"),
    "\\midrule",
    if (show_minority_top) data1_minority,
    if (show_minority_top) "\\midrule",  # Add horizontal rule between minority rows and category rows if shown
    data1_categories,
    "\\midrule",
    if (two_columns) paste0("Observations      &      ", obs_2[1], "         &      ", obs_2[3], "         \\\\"),
    if (two_columns && show_minority_top) paste0("Adjusted R$^2$ (Minority)      &      ", adj_r2_1[1], "         &      ", adj_r2_1[3], "         \\\\"),
    if (two_columns) paste0("Adjusted R$^2$ (Category)      &      ", adj_r2_2[1], "         &      ", adj_r2_2[3], "         \\\\"),
    if (two_columns) paste0("Number of Cities      &      ", num_cities_2[1], "         &      ", num_cities_2[3], "         \\\\"),
    if (!two_columns) paste0("Observations      &      ", obs_2[1], "         &      ", obs_2[2], "         &      ", obs_2[3], "         \\\\"),
    if (!two_columns && show_minority_top) paste0("Adjusted R$^2$ (Minority)      &      ", adj_r2_1[1], "         &      ", adj_r2_1[2], "         &      ", adj_r2_1[3], "         \\\\"),
    if (!two_columns) paste0("Adjusted R$^2$ (Category)      &      ", adj_r2_2[1], "         &      ", adj_r2_2[2], "         &      ", adj_r2_2[3], "         \\\\"),
    if (!two_columns) paste0("Number of Cities      &      ", num_cities_2[1], "         &      ", num_cities_2[2], "         &      ", num_cities_2[3], "         \\\\"),
    if (!single_panel) "\\addlinespace",
    if (!single_panel) "\\midrule",
    if (!single_panel) "\\addlinespace",
    if (!single_panel) paste0("&\\multicolumn{", if (two_columns) "2" else "3", "}{c}{", panel_b_title, "}\\\\"),
    if (!single_panel) paste0("\\cmidrule{2-", if (two_columns) "3" else "4", "}"),
    if (!single_panel && two_columns) paste0("&\\multicolumn{1}{c}{Original Data}&\\multicolumn{1}{c}{Updated City Name}\\\\"),
    if (!single_panel && two_columns) paste0("\\cmidrule(lr){2-2}\\cmidrule(lr){3-3}"),
    if (!single_panel && two_columns) paste0("&\\multicolumn{1}{c}{(1)}         &\\multicolumn{1}{c}{(2)}         \\\\"),
    if (!single_panel && !two_columns) paste0("&\\multicolumn{1}{c}{Original Data}&\\multicolumn{1}{c}{Correct Race Only}&\\multicolumn{1}{c}{Updated City Name \\& Correct Race}\\\\"),
    if (!single_panel && !two_columns) paste0("\\cmidrule(lr){2-2}\\cmidrule(lr){3-3}\\cmidrule(lr){4-4}"),
    if (!single_panel && !two_columns) paste0("&\\multicolumn{1}{c}{(1)}         &\\multicolumn{1}{c}{(2)}         &\\multicolumn{1}{c}{(3)}         \\\\"),
    if (!single_panel) "\\midrule",
    if (!single_panel && show_minority_bottom) data2_minority,
    if (!single_panel && show_minority_bottom) "\\midrule",  # Add horizontal rule between minority rows and category rows if shown
    if (!single_panel) data2_categories,
    if (!single_panel) "\\midrule",
    if (!single_panel && two_columns) paste0("Observations      &      ", obs_4[1], "         &      ", obs_4[3], "         \\\\"),
    if (!single_panel && two_columns && show_minority_bottom) paste0("Adjusted R$^2$ (Minority)      &      ", adj_r2_3[1], "         &      ", adj_r2_3[3], "         \\\\"),
    if (!single_panel && two_columns) paste0("Adjusted R$^2$ (Category)      &      ", adj_r2_4[1], "         &      ", adj_r2_4[3], "         \\\\"),
    if (!single_panel && two_columns) paste0("Number of Cities      &      ", num_cities_4[1], "         &      ", num_cities_4[3], "         \\\\"),
    if (!single_panel && !two_columns) paste0("Observations      &      ", obs_4[1], "         &      ", obs_4[2], "         &      ", obs_4[3], "         \\\\"),
    if (!single_panel && !two_columns && show_minority_bottom) paste0("Adjusted R$^2$ (Minority)      &      ", adj_r2_3[1], "         &      ", adj_r2_3[2], "         &      ", adj_r2_3[3], "         \\\\"),
    if (!single_panel && !two_columns) paste0("Adjusted R$^2$ (Category)      &      ", adj_r2_4[1], "         &      ", adj_r2_4[2], "         &      ", adj_r2_4[3], "         \\\\"),
    if (!single_panel && !two_columns) paste0("Number of Cities      &      ", num_cities_4[1], "         &      ", num_cities_4[2], "         &      ", num_cities_4[3], "         \\\\"),
    "\\bottomrule",
    paste0("\\multicolumn{", if (two_columns) "3" else "4", "}{l}{\\footnotesize Cluster-robust standard errors in parentheses. Clustered at the trial level. 95\\% confidence intervals in square brackets.}\\\\"),
    paste0("\\multicolumn{", if (two_columns) "3" else "4", "}{l}{\\footnotesize \\sym{*} \\(p<0.10\\), \\sym{**} \\(p<0.05\\), \\sym{***} \\(p<0.01\\)}\\\\"),
    "\\end{tabular}",
    "}",
    "\\end{table}",
    "\\end{document}"
  )

  # Write the combined table to a new LaTeX file
  writeLines(combined_table, output_file)
}


# Call function for Table 7
generate_combined_table(
  table_title = "Differences in Results for Racial Composition of Reccomended Neighbourhood (CT2022 Table 7)",
  panel_a_title = "White Household Income Share in High Income Neighbourhoods (Column 1)",
  panel_b_title = "White Household Income Share in Low Income Neighbourhoods (Column 3)",
  table_number = 7,
  single_panel = FALSE,
  set_dashes = TRUE,
  show_minority_top = TRUE,
  show_minority_bottom = TRUE
)

# Call function for Table 9
generate_combined_table(
  table_title = "Differences in Results for Local Pollution Exposures (CT2022 Table 9)",
  panel_a_title = "Differences in Superfund Proximity, Whole Sample (Panel A, Column 1)",
  panel_b_title = "Differences in Superfund Proximity, Mothers Only (Panel B, Column 1)",
  table_number = 9,
  single_panel = FALSE,
  set_dashes = TRUE,
  show_minority_top = TRUE,
  show_minority_bottom = TRUE
)

# Call function for Table 11
generate_combined_table(
  table_title = "Differences in Results for Low-Poverty Neighbourhood Recommendations (CT2022 Table 11, Column 1)",
  panel_a_title = "",
  panel_b_title = "",
  table_number = 11,
  single_panel = TRUE,
  set_dashes = FALSE,
  show_minority_top = TRUE,
  show_minority_bottom = FALSE,
  two_columns = TRUE
)

# Call function for Table 12
generate_combined_table(
  table_title = "Differences in Results for Median Income (CT2022 Table 12, Column 1)",
  panel_a_title = "",
  panel_b_title = "",
  table_number = 12,
  single_panel = TRUE,
  set_dashes = FALSE,
  show_minority_top = FALSE,
  show_minority_bottom = FALSE,
  two_columns = TRUE
)

# Call the function for Table 14
generate_combined_table(
  table_title = "Differences in Results for Recommended Home's Log Sale Price (CT2022 Table 14 Panel B, Column 5)",
  panel_a_title = "",
  panel_b_title = "",
  table_number = 14,
  single_panel = TRUE,
  show_minority_top = TRUE,
  show_minority_bottom = TRUE,
  two_columns = FALSE
)
