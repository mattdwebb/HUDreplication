# HUDreplication

This repository provides the code required to replicate an upcoming comment which itself replicates Christensen and Timmins' 2022 JPE paper, "Sorting or Steering: The Effects of Housing Discrimination on Neighborhood Choice". In addition, the repository and comment correct several coding and data cleaning errors, and use a specification more appropriate to the paired tester design to reanalyze the same questions from the original paper. The original paper measures the degree of discrimination that people of color face in the housing market.

The repository has two major sections:

1. `Pooled_Analysis`: replication of the Christensen and Timmins (2022) analysis, starting from their replication files, with corrections to a number of data cleaning errors and several approaches to generating fixed effects for the geographical region of recommended houses.
2. `Paired_Tester_Analysis`: a new specification that is appropriate for the paired testing design of the 2012 Housing Discrimination Study (HDS 2012). This section works directly from the raw data of HDS 2012 with no inputs from the C&T2022 replication package.

The repository is organized so that raw or source inputs live in `Data`, pooled intermediates live in `Data/Generated/Pooled_Analysis`, paired-tester intermediates live in `Data/Generated/Paired_Tester_Analysis`, and final output tables are written inside the corresponding folder for each analysis approach.

## Data Layout

All data is dsitributed 

`Data/CT2022_Replication_Data`
- Christensen and Timmins replication inputs used by the pooled workflow. 
- Includes:
  - `adsprocessed_JPE_censor.rds`
  - `HUDprocessed_JPE_census_042021.rds`
  - `HUDprocessed_JPE_names_042021.rds`
  - `HUDprocessed_JPE_testscores_042021.rds`
  - `HUDprocessed_tract.rds`
  - `recsprocessed_JPE.rds`
  - `tester_assignment.csv`
  - `zipinfo.dta`
  - `zipinfo-county.dta`

`Data/HDS2012_Raw_Data`
- HDS raw SAS files used by the paired-tester workflow
- Required files:
  - `assignment.sas7bdat`
  - `rechomes.sas7bdat`
  - `rhgeo.sas7bdat`
  - `sales.sas7bdat`
  - `taf.sas7bdat`
  - `tester_censored.sas7bdat`

`Data/Non_HDS_Data`
- External non-HDS inputs used by the paired-tester workflow
- Includes school-boundary, SEDA, Superfund, PM2.5, and RSEI inputs

`Data/meta_comparison.csv`
- Input for the pooled meta-analysis figure

`Data/Generated`
- Shared generated-data root
- Includes:
  - `Pooled_Analysis`
  - `Paired_Tester_Analysis`
  - `Intersection Files`

## Software

- R
- Stata
- Internet access for:
  - `tigris` downloads in pooled preprocessing
  - `tidycensus` ACS pulls in paired cleaning
  - geocoding APIs in paired cleaning
  - Stata SSC package installs in pooled analysis

The scripts install many missing R and Stata packages automatically. The paired ACS step requires a valid Census API key.

## One-Time Setup

Set the repository root path in each top-level driver before running:

- `Pooled_Analysis/main.do`
- `Pooled_Analysis/preprocess_place_city_dedup.R`
- `Paired_Tester_Analysis/data_cleaning.R`
- `Paired_Tester_Analysis/analysis.R`

For the paired workflow, `Paired_Tester_Analysis/api_keys.R` must exist and contain a valid `CENSUS_API_KEY`. A template is provided at `Paired_Tester_Analysis/api_keys_template.R`.

## Pooled Workflow

### Step 1: Build corrected pooled inputs

Run:

- `Pooled_Analysis/preprocess_place_city_dedup.R`
This script reads the source C\&T inputs from `Data/CT2022_Replication_Data` and writes `.rds`, `.csv`, and `.dta` versions of the corrected-city pooled outputs (both `_processed` and `_with_duplicates`) for:

- `adsprocessed`
- `HUDprocessed_census`
- `HUDprocessed_names`
- `HUDprocessed_testscores`

It also writes:

- pooled intermediates to `Data/Generated/Pooled_Analysis`
- logs (`dedup_log_processed.csv` and `dedup_log_with_duplicates.csv`) inside `Data/Generated/Pooled_Analysis`
- cached spatial intersection files in `Data/Generated/Intersection Files`

The first run can take a long time because it builds and caches state-level spatial intersections.

The `_processed` files are the default pooled-analysis inputs. The `_with_duplicates` files preserve duplicates and are used only for the original-data comparison-table columns.

### Step 2: Run pooled Stata analysis

Open Stata and run:

- `Pooled_Analysis/main.do`

This script runs:

- `comparison_tables.do`
- `appendix_tables.do`
- `meta_analysis.do`

Pooled outputs are written to:

- `Pooled_Analysis/Output`

## Paired-Tester Workflow

### Step 1: Clean and merge paired-tester data

Run:

- `Paired_Tester_Analysis/data_cleaning.R`

This script reads the HDS raw files from `Data/HDS2012_Raw_Data`, performs the internal merges and cleaning, geocodes recommended properties, pulls ACS data, merges the external datasets, and writes paired intermediates back to `Data`.

Key outputs include:

- `Data/sales_cleaned.csv`
- `Data/sales_and_tester_merged.csv`
- `Data/sales_and_tester_appointments.csv`
- `Data/sales_tester_rechomes_merged.csv`
- `Data/sales_tester_rechomes_geocoded.csv`
- `Data/cleaned_hds.csv`

If `Data/sales_tester_rechomes_geocoded.csv` already exists and you want to skip the geocoding step, rerun with:

- `Rscript Paired_Tester_Analysis/data_cleaning.R --skip-geocoding`

### Step 2: Run paired-tester analysis

Run:

- `Paired_Tester_Analysis/analysis.R`

This script reads the paired cleaned data from `Data` and writes outputs to:

- `Paired_Tester_Analysis/Tables`
- `Paired_Tester_Analysis/Appendix_Tables`

### Step 3: Generate formatted LaTeX tables

Run:

- `Rscript Paired_Tester_Analysis/format_tables.R`

This script reads the main paired-tester LaTeX outputs from `Paired_Tester_Analysis/Tables` and writes cleaned presentation versions to:

- `Paired_Tester_Analysis/Tables/Formatted_Tables`

The formatted outputs include:

- single-table versions of Tables 5, 6, 7, and 12
- combined panel versions of Tables 8, 9, and 10
- the original Christensen and Timmins table number as a subtitle in each formatted caption
- a preview wrapper document at `Paired_Tester_Analysis/Tables/Formatted_Tables/formatted_tables_preview.tex`

To compile the preview PDF, run:

- `cd Paired_Tester_Analysis/Tables/Formatted_Tables`
- `pdflatex -interaction=nonstopmode -halt-on-error formatted_tables_preview.tex`

This writes:

- `Paired_Tester_Analysis/Tables/Formatted_Tables/formatted_tables_preview.pdf`

## Expected Execution Order

For a full clean replication from source inputs:

1. Set the repo root in the top-level scripts listed above
2. Ensure `Paired_Tester_Analysis/api_keys.R` contains a valid Census API key
3. Run `Pooled_Analysis/preprocess_place_city_dedup.R`
4. Run `Pooled_Analysis/main.do`
5. Run `Paired_Tester_Analysis/data_cleaning.R`
6. Run `Paired_Tester_Analysis/analysis.R`
7. Run `Paired_Tester_Analysis/format_tables.R`
