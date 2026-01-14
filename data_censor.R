# HUD Replication
# Originally contributed by Areez Gangji and Anthony McCanny
# Later cleaned and streamlined by Shi Chen

library(dplyr)

# Set the working directory to the directory location of the github repository 
# This will be appended to the front of all addresses in the file
WORKING_DIRECTORY = "/Users/shichen/Desktop/HUDrep_block_update"

# Define the path to the output folder
output_folder <- paste0(WORKING_DIRECTORY, "/Data/Generated")
input_folder <- paste0(WORKING_DIRECTORY, "/Data/Original")

# Load the adsprocessed_JPE data
adsprocessed_data <- readRDS(paste0(input_folder, "/adsprocessed_JPE.rds"))

# Get all column names
all_columns <- colnames(adsprocessed_data)

# Find the indices of TADDR and RELEASE
taddr_index <- which(all_columns == "TADDR")
release_index <- which(all_columns == "RELEASE")

# Get all columns before TADDR and after RELEASE
cols_before_taddr <- all_columns[1:taddr_index]
cols_after_release <- all_columns[release_index:length(all_columns)]

# Get all column names between TADDR and RELEASE
cols_between <- all_columns[(taddr_index + 1):(release_index - 1)]

# Define variables to keep between TADDR and RELEASE
vars_to_keep_full <- c(
    "TesterID", "APRACE", "TRACESPY", "TASIANG", "TNATORIG", "THISPUBG",
    "TASIANS", "TTRIBE", "TSEX.x", "TDOB", "TCORGIN", "TCORGI2", "TTIMEUS",
    "TTMUSMO", "TTMUSYR", "TMALIVE", "TLIVMON", "TLIVYR", "TENGFL",
    "TENGAGE", "TPROFO", "TPROFOS", "TLANGHOM", "TCUREMP", "TPREVE",
    "TSTUDNT", "THIGHEDU", "TDEGREE", "TPEGAI", "THHEGAI", "TTIMECUR",
    "TCURTENR", "TCURTENS", "TDWLTYP", "TDWLTYPS", "TMORTFIN", "THOMEHNT",
    "THMHNTS", "TIFWRE", "TIFWRED", "TIFWREP", "TVIEWRE", "TVIEWRES",
    "TEXPERNC", "TNOTESTS", "TTESTTP1", "TTESTTP2", "TTESTTP3", "TTESTTP4",
    "TTESTTP5", "TTESTTP6", "TTESTTP7", "TTSTTYPS", "TRESERV", "THIPROF",
    "TDRIVER", "TOWNCAR", "TCOMPUTE", "TCOMPUTS", "TCOMPACC", "TCOMPACS",
    "age"
)

# Keep only variables that actually exist in the dataset
vars_to_keep <- intersect(vars_to_keep_full, cols_between)

# Combine all columns to keep
columns_to_keep <- c(cols_before_taddr, vars_to_keep, cols_after_release)

# Filter the dataset to keep only the specified variables
adsprocessed_data_censored <- adsprocessed_data %>%
    select(all_of(unique(columns_to_keep)))

# Save the censored dataset to a new RDS file
saveRDS(adsprocessed_data_censored, paste0(input_folder, "/adsprocessed_JPE_censor.rds"))

# Print a message to confirm the operation
cat("Data censored and saved to", paste0(input_folder, "/adsprocessed_JPE_censor.rds"), "\n")

# Check dimensions of original and censored data
cat("Original data dimensions:", dim(adsprocessed_data)[1], "rows x", dim(adsprocessed_data)[2], "columns\n")
cat("Censored data dimensions:", dim(adsprocessed_data_censored)[1], "rows x", dim(adsprocessed_data_censored)[2], "columns\n")
