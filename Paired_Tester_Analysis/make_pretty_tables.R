resolve_repo_root <- function() {
  cwd <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  if (basename(cwd) == "HUDreplication_new") return(cwd)
  if (basename(cwd) == "Paired_Tester_Analysis") return(dirname(cwd))
  candidate <- file.path(cwd, "HUDreplication_new")
  if (dir.exists(candidate)) return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
  stop("Could not infer repo_root. Run from HUDreplication_new or Paired_Tester_Analysis.")
}

repo_root <- resolve_repo_root()
root <- file.path(repo_root, "Paired_Tester_Analysis")
source_dir <- file.path(root, "Tables")
output_dir <- file.path(root, "Tables", "Pretty_Tables")

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

standard_note <- c(
  "Cluster-robust standard errors in parentheses; clustered at the trial level. 95\\% confidence intervals in square brackets.",
  "\\sym{*} \\(p<0.10\\), \\sym{**} \\(p<0.05\\), \\sym{***} \\(p<0.01\\)"
)
sym_def <- "\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}"

single_tables <- c("table5", "table6", "table7", "table12")
panel_tables <- list(
  table8 = c("table8a", "table8b"),
  table9 = c("table9a", "table9b"),
  table10 = c("table10a", "table10b")
)

read_text <- function(path) {
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

extract_caption <- function(text, path) {
  match <- regexpr("\\\\caption\\{.*\\}", text)
  if (match[1] == -1) stop("Could not find caption in ", basename(path))
  raw <- regmatches(text, match)
  sub("^\\\\caption\\{", "", sub("\\}$", "", raw))
}

clean_caption <- function(raw_caption) {
  parts <- strsplit(raw_caption, "\\\\\\\\", perl = TRUE)[[1]]
  trimws(parts[1])
}

clean_combined_caption <- function(raw_caption) {
  caption <- clean_caption(raw_caption)
  caption <- gsub("\\s*\\(Panel [AB]:.*?\\)", "", caption, perl = TRUE)
  caption <- gsub("\\s*\\(Mothers, Panel [AB]\\)", " (Mothers)", caption, perl = TRUE)
  trimws(caption)
}

extract_panel_title <- function(raw_caption) {
  caption <- clean_caption(raw_caption)
  panel_match <- regexec("Panel [AB]:\\s*(.*)\\)", caption, perl = TRUE)
  panel_parts <- regmatches(caption, panel_match)[[1]]
  if (length(panel_parts) > 1) return(trimws(panel_parts[2]))

  mothers_match <- regexec("\\(Mothers,\\s*Panel ([AB])\\)", caption, perl = TRUE)
  mothers_parts <- regmatches(caption, mothers_match)[[1]]
  if (length(mothers_parts) > 1) return(paste("Panel", mothers_parts[2]))

  caption
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

build_single_table <- function(name) {
  path <- file.path(source_dir, paste0(name, ".tex"))
  text <- read_text(path)
  caption <- clean_caption(extract_caption(text, path))
  tabular <- tabular_without_notes(extract_outer_tabular(text, path))

  c(
    "\\begin{table}[!htbp]",
    "\\centering",
    sym_def,
    sprintf("\\caption{%s}", caption),
    sprintf("\\label{tab:%s}", name),
    "\\resizebox{\\textwidth}{!}{%",
    tabular,
    "}",
    "\\begin{minipage}{\\textwidth}",
    "\\footnotesize",
    standard_note,
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

  caption <- clean_combined_caption(extract_caption(panel_a_text, panel_a_path))
  panel_a_title <- extract_panel_title(extract_caption(panel_a_text, panel_a_path))
  panel_b_title <- extract_panel_title(extract_caption(panel_b_text, panel_b_path))
  panel_a_tabular <- tabular_without_notes(extract_outer_tabular(panel_a_text, panel_a_path))
  panel_b_tabular <- tabular_without_notes(extract_outer_tabular(panel_b_text, panel_b_path))

  c(
    "\\begin{table}[!htbp]",
    "\\centering",
    sym_def,
    sprintf("\\caption{%s}", caption),
    sprintf("\\label{tab:%s}", output_name),
    sprintf("\\textbf{%s}\\\\[0.5em]", panel_a_title),
    "\\resizebox{\\textwidth}{!}{%",
    panel_a_tabular,
    "}",
    "\\medskip",
    sprintf("\\textbf{%s}\\\\[0.5em]", panel_b_title),
    "\\resizebox{\\textwidth}{!}{%",
    panel_b_tabular,
    "}",
    "\\begin{minipage}{\\textwidth}",
    "\\footnotesize",
    standard_note,
    "\\end{minipage}",
    "\\end{table}",
    ""
  )
}

for (table_name in single_tables) {
  output <- build_single_table(table_name)
  writeLines(output, file.path(output_dir, paste0(table_name, ".tex")), useBytes = TRUE)
}

for (output_name in names(panel_tables)) {
  parts <- panel_tables[[output_name]]
  output <- build_panel_table(output_name, parts[1], parts[2])
  writeLines(output, file.path(output_dir, paste0(output_name, ".tex")), useBytes = TRUE)
}

manifest <- c(
  "Generated pretty main-table outputs.",
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
  "Output directory: Tables/Pretty_Tables/",
  "Canonical generator: make_pretty_tables.R"
)
writeLines(manifest, file.path(output_dir, "README.txt"), useBytes = TRUE)
