#= This file creates 6-column tables presented in the HUD comment =#
#= HUDreplication, March 27, 2026 =#

# ++++++ This file requires the following julia modules: ++++++++++++++++++
#
# [1] "DualPanelTableBuilder_v4.jl"
# |-> builts dual-panel tables using template and Stata LaTeX outputs
#
# [2] "SinglePanelTableBuilder_v3"
# |-> builts single-panel tables using template and Stata LaTeX outputs
#
# [3] "fillCol6.jl"
# |-> fills matched pairs results to Column 6 of corresponding tables
# |   !! specific to matched pair results in single-panel format
#
# [4] "fillCol6FromPanelSource.jl"
# |-> fills matched pairs results to Column 6 of corresponding tables
# |   !! specific to matched pairs results in dual-panel format
#
# [5] "fillCol6FromPanelSourceNoMinority.jl"
# |-> fills matched pairs results to Column 6 of corresponding tables
#     specifc to table 12 as matched pair results have no racial minority row
#
# [6] "LatexNumberCleaner.jl"
# |-> removes the thousand separator "{,}" in comparison tables
#
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# ++++++ This file uses the following input structure: ++++++++++++++++++++
#
# [a] "templates" folder: containing templates
#
# [b] "stata_matched_pairs" folder: containing matched pairs result tables
#
# [c] "stata_pooled" folder: containing comparison table results
#
# [d] "temp_tables" folder: storing intermediate tables
#
# [e] "output_tables" folder: storing final comparison tables
#
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



#= Set working directory  =#
cd("/Users/shichen/Desktop/HUD_comparison_tables/mar27_cleaned_modules")

#= Create needed folders =#
mkdir("temp_tables")
mkdir("output_tables")


#= Include modules =#
include("DualPanelTableBuilder_v4.jl")
using .DualPanelTableBuilder

include("SinglePanelTableBuilder_v3.jl")
using .SinglePanelTableBuilder

include("fillCol6.jl")
using .FillCol6Tools

include("fillCol6FromPanelSource.jl")
using .FillCol6FromPanelSourceTools

include("fillCol6FromPanelSourceNoMinority.jl")
using .FillCol6FromPanelSourceNoMinorityTools

include("LatexNumberCleaner.jl")
using .LatexNumberCleaner


#=++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
===== Makes original 6 column tables =====================================================
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++=#

# === table 5 ===

# specify input source files for top panel of comparison table
top = DualPanelTableBuilder.PanelSources(
    "stata_pooled/table5_dep_var_1_minority.tex",
    "stata_pooled/table5_dep_var_1_categories.tex",
)

# specify input source files for bottom panel of comparison table
bottom = DualPanelTableBuilder.PanelSources(
    "stata_pooled/table5_dep_var_2_minority.tex",
    "stata_pooled/table5_dep_var_2_categories.tex",
)

# build the comparison table using template
DualPanelTableBuilder.build_table(
    "templates/template_dualPanel_table5.tex";    # (a) specify the type of template used
    top = top,                                      # (b) specify input source for top panel
    bottom = bottom,                                # (c) specify input source for bottom panel
    output_path = "temp_tables/all6col_comparison_table5_built.tex",    # (d) specify output path and file name
    copy_cols = [1,2,3,5,6],                                            # (e) specify columns of input source to be copied to comparison table
    caption = "\\small Results Comparison for Table 5 Column 2 and 4, C\\&T 2022",  # (f) specify caption for comparison table
    top_title = "Panel A: Number of Recommendations, Clustered at Trial",   # (g) specify title for top panel
    bottom_title = "Panel B: Home Availability, Clustered at Trial",        # (h) specify title for bottom panel
)


# === table 6 ===
# this is a single panel table

# specify input source
single_panel = SinglePanelTableBuilder.SinglePanelSources(
    "stata_pooled/table6_dep_var_1_minority.tex",
    "stata_pooled/table6_dep_var_1_categories.tex",
)

# build the comparison table using template
SinglePanelTableBuilder.build_single_panel_table(
    "templates/template_onePanel_withCol6.tex";
    panel = single_panel,
    output_path = "temp_tables/all6col_comparison_table6_built.tex",
    copy_cols = [1,2,3,4,5,6],
    caption = "\\small Results Comparison for Table 6 Column 5, C\\&T 2022",
    panel_title = "White Household Share, Clustered at Trial",
)


# === table 7 ===

top = DualPanelTableBuilder.PanelSources(
    "stata_pooled/table7_dep_var_1_minority.tex",
    "stata_pooled/table7_dep_var_1_categories.tex",
)

bottom = DualPanelTableBuilder.PanelSources(
    "stata_pooled/table7_dep_var_2_minority.tex",
    "stata_pooled/table7_dep_var_2_categories.tex",
)

DualPanelTableBuilder.build_table(
    "templates/template_dualPanel_withCol6.tex";
    top = top,
    bottom = bottom,
    output_path = "temp_tables/all6col_comparison_table7_built.tex",
    copy_cols = [1,2,3,4,5,6],
    caption = "\\small Results Comparison for Table 7 Column 1 and 3, C\\&T 2022",
    top_title = "Panel A: White Household Share (High Income Neighbourhood), Clustered at Trial",
    bottom_title = "Panel B: White Household Share (Low Income Neighbourhood), Clustered at Trial",
)



# === Table 8 ===

top = DualPanelTableBuilder.PanelSources(
    "stata_pooled/table8_dep_var_1_minority.tex",
    "stata_pooled/table8_dep_var_1_categories.tex",
)

bottom = DualPanelTableBuilder.PanelSources(
    "stata_pooled/table8_dep_var_2_minority.tex",
    "stata_pooled/table8_dep_var_2_categories.tex",
)

DualPanelTableBuilder.build_table(
    "templates/template_dualPanel_withCol6.tex";
    top = top,
    bottom = bottom,
    output_path = "temp_tables/all6col_comparison_table8_built.tex",
    copy_cols = [1,2,3,4,5,6],
    caption = "\\small Results Comparison for Table 8 Panel A Column 4 and Panel B Column 1, C\\&T 2022",
    top_title = "Panel A: Elementary School Rating on Housing Search Platform, Clustered at Trial",
    bottom_title = "Panel B: American Community Survey Poverty Rate, Clustered at Trial",
)

# === Table 9 ===

top = DualPanelTableBuilder.PanelSources(
    "stata_pooled/table9_dep_var_1_minority.tex",
    "stata_pooled/table9_dep_var_1_categories.tex",
)

bottom = DualPanelTableBuilder.PanelSources(
    "stata_pooled/table9_dep_var_2_minority.tex",
    "stata_pooled/table9_dep_var_2_categories.tex",
)

DualPanelTableBuilder.build_table(
    "templates/template_dualPanel_withCol6.tex";
    top = top,
    bottom = bottom,
    output_path = "temp_tables/all6col_comparison_table9_built.tex",
    copy_cols = [1,2,3,4,5,6],
    caption = "\\small Results Comparison for Table 9 Column 1, C\\&T 2022",
    top_title = "Panel A: Local Pollution Exposures as Superfund Sites (Entire Sample), Clustered at Trial",
    bottom_title = "Panel B: Local Pollution Exposures as Superfund Sites (Mothers Only), Clustered at Trial",
)

# === Table 10A ===

top = DualPanelTableBuilder.PanelSources(
    "stata_pooled/table10A_dep_var_1_minority.tex",
    "stata_pooled/table10A_dep_var_1_categories.tex",
)

bottom = DualPanelTableBuilder.PanelSources(
    "stata_pooled/table10A_dep_var_2_minority.tex",
    "stata_pooled/table10A_dep_var_2_categories.tex",
)

DualPanelTableBuilder.build_table(
    "templates/template_dualPanel_withCol6.tex";
    top = top,
    bottom = bottom,
    output_path = "temp_tables/all6col_comparison_table10A_built.tex",
    copy_cols = [1,2,3,4,5,6],
    caption = "\\small Results Comparison for Table 10 Panel A Column 1 and 2, C\\&T 2022",
    top_title = "Panel A: Elementary School Test Scores (Mothers Only), Clustered at Trial",
    bottom_title = "Panel B: Middle School Test Scores (Mothers Only), Clustered at Trial",
)

# === Table 10B ===

top = DualPanelTableBuilder.PanelSources(
    "stata_pooled/table10B_dep_var_1_minority.tex",
    "stata_pooled/table10B_dep_var_1_categories.tex",
)

bottom = DualPanelTableBuilder.PanelSources(
    "stata_pooled/table10B_dep_var_2_minority.tex",
    "stata_pooled/table10B_dep_var_2_categories.tex",
)

DualPanelTableBuilder.build_table(
    "templates/template_dualPanel_withCol6.tex";
    top = top,
    bottom = bottom,
    output_path = "temp_tables/all6col_comparison_table10B_built.tex",
    copy_cols = [1,2,3,4,5,6],
    caption = "\\small Results Comparison for Table 10 Panel B Column 2 and 4, C\\&T 2022",
    top_title = "Panel A: American Community Survey High Skill Neighbourhood, Clustered at Trial",
    bottom_title = "Panel B: American Community Survey Single Parent Household, Clustered at Trial",
)

# === Table 11 ===
# single panel table

single_panel = SinglePanelTableBuilder.SinglePanelSources(
    "stata_pooled/table11_dep_var_1_minority.tex",
    "stata_pooled/table11_dep_var_1_categories.tex",
)

SinglePanelTableBuilder.build_single_panel_table(
    "templates/template_onePanel_noCol6.tex";   # <<< note the template used has changed
    panel = single_panel,
    output_path = "temp_tables/all6col_comparison_table11_built.tex",
    copy_cols = [1,2,3,4,5],
    caption = "\\small Results Comparison for Table 11 Column 1, C\\&T 2022",
    panel_title = "Low-Poverty Neighbourhoods, Clustered at Trial",
)


# === Table 12 ===
# single panel table

single_panel = SinglePanelTableBuilder.SinglePanelSources(
    "stata_pooled/table12_dep_var_1_minority.tex",
    "stata_pooled/table12_dep_var_1_categories.tex",
)

SinglePanelTableBuilder.build_single_panel_table(
    "templates/template_onePanel_withCol6.tex";
    panel = single_panel,
    output_path = "temp_tables/all6col_comparison_table12_built.tex",
    copy_cols = [1,2,3,4,5,6],
    caption = "\\small Results Comparison for Table 12 Column 1, C\\&T 2022",
    panel_title = "Median Income in Neighbourhood, Clustered at Trial",
)


# === Table 13 ===
top = DualPanelTableBuilder.PanelSources(
    "stata_pooled/table13_dep_var_1_minority.tex",
    "stata_pooled/table13_dep_var_1_categories.tex",
)

bottom = DualPanelTableBuilder.PanelSources(
    "stata_pooled/table13_dep_var_2_minority.tex",
    "stata_pooled/table13_dep_var_2_categories.tex",
)

DualPanelTableBuilder.build_table(
    "templates/template_dualPanel_noCol6.tex";  # <<< note the template used has changed
    top = top,
    bottom = bottom,
    output_path = "temp_tables/all6col_comparison_table13_built.tex",
    copy_cols = [1,2,3,4,5],
    caption = "\\small Results Comparison for Table 13 Panel A Column 4 and Panel B Column 2, C\\&T 2022",
    top_title = "Panel A: Elementary School Rating on Housing Search Platform, Clustered at Trial",
    bottom_title = "Panel B: American Community Survey High Skill Neighbourhood, Clustered at Trial",
)


# === Table 14 ===
# single panel table

single_panel = SinglePanelTableBuilder.SinglePanelSources(
    "stata_pooled/table14_dep_var_1_minority.tex",
    "stata_pooled/table14_dep_var_1_categories.tex",
)

SinglePanelTableBuilder.build_single_panel_table(
    "templates/template_onePanel_noCol6.tex";   # <<< note the template used has changed
    panel = single_panel,
    output_path = "temp_tables/all6col_comparison_table14_built.tex",
    copy_cols = [1,2,3,4,5],
    caption = "\\small Results Comparison for Table 14 Column 5, C\\&T 2022",
    panel_title = "Logarithm of Sale Price, Clustered at Trial",
)


#=++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
===== Calls the function to insert paired tester results =================================
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++=#

# Table 5
fill_col6_from_source(
    "temp_tables/all6col_comparison_table5_built.tex",      # (a) comparison table with empty col 6
    "stata_matched_pairs/table5.tex",                       # (b) source table with new results
    1,                                                      # (c) source column for top panel
    2,                                                      # (d) source column for bottom panel
    "output_tables/comparison_table5.tex"                   # (e) output file
)

# Table 6
fill_col6_from_source(
    "temp_tables/all6col_comparison_table6_built.tex",
    "stata_matched_pairs/table6.tex",
    1,
    nothing,
    "output_tables/comparison_table6.tex"
)

# Table 7
fill_col6_from_source(
    "temp_tables/all6col_comparison_table7_built.tex",
    "stata_matched_pairs/table7.tex",
    1,
    3,
    "output_tables/comparison_table7.tex"
)


# Table 8

fill_col6_from_panel_source(
    "temp_tables/all6col_comparison_table8_built.tex",  # (a) comparison table with empty col 6
    "stata_matched_pairs/table8.tex",                   # (b) source table with new results
    1, 4,                                               # (c) input source for top panel of comparison table, [1,4] indicates Column 4 in top panel of input source
    2, 1,                                               # (d) input source for bottom panel of comparison table, [2,1] indicates Column 1 in bottom panel of input source
    "output_tables/comparison_table8.tex"               # (e) output path and file name
)


# Table 9

fill_col6_from_panel_source(
    "temp_tables/all6col_comparison_table9_built.tex",
    "stata_matched_pairs/table9.tex",
    1, 1,
    2, 1,
    "output_tables/comparison_table9.tex"
)

# Table 10A
fill_col6_from_panel_source(
    "temp_tables/all6col_comparison_table10A_built.tex",
    "stata_matched_pairs/table10.tex",
    1, 1,
    1, 2,
    "output_tables/comparison_table10A.tex"
)

# Table 10B
fill_col6_from_panel_source(
    "temp_tables/all6col_comparison_table10B_built.tex",
    "stata_matched_pairs/table10.tex",
    2, 2,
    2, 4,
    "output_tables/comparison_table10B.tex"
)

# Table 12


fill_col6_from_panel_source_no_minority(
    "temp_tables/all6col_comparison_table12_built.tex",
    "stata_matched_pairs/table12.tex",
    1, 1,
    nothing, nothing,
    "output_tables/comparison_table12.tex"
)

#=++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
===== Copy certain tables w/o match pairs results to output folder =======================
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++=#

# the following tables has no matched pairs results
# their column 6 has been replaced with "-" in relevant fields
# copy them from temp output folder to final output folder + rename

cp("temp_tables/all6col_comparison_table11_built.tex","output_tables/comparison_table11.tex")
cp("temp_tables/all6col_comparison_table13_built.tex","output_tables/comparison_table13.tex")
cp("temp_tables/all6col_comparison_table14_built.tex","output_tables/comparison_table14.tex")


#=++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
===== Fix the separator issue in number of obs/trials ====================================
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++=#

# === Table 6 ===
clean_latex_numbers!(
    "output_tables/comparison_table6.tex";                  # (a) specify input file to be fixed
    output_path = "output_tables/comparison_table6.tex"     # (b) specify output path and file name, overwrite existing file here
)

# === Table 7 ===
clean_latex_numbers!(
    "output_tables/comparison_table7.tex";
    output_path = "output_tables/comparison_table7.tex"
)

# === Table 8 ===
clean_latex_numbers!(
    "output_tables/comparison_table8.tex";
    output_path = "output_tables/comparison_table8.tex"
)


# === Table 9 ===
clean_latex_numbers!(
    "output_tables/comparison_table9.tex";
    output_path = "output_tables/comparison_table9.tex"
)


# === Table 10A ===
clean_latex_numbers!(
    "output_tables/comparison_table10A.tex";
    output_path = "output_tables/comparison_table10A.tex"
)


# === Table 10B ===
clean_latex_numbers!(
    "output_tables/comparison_table10B.tex";
    output_path = "output_tables/comparison_table10B.tex"
)


# === Table 12 ===
clean_latex_numbers!(
    "output_tables/comparison_table12.tex";
    output_path = "output_tables/comparison_table12.tex"
)