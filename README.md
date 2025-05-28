# HUDreplication
Files used in replication of Christensen and Timmins (2022)


Setup Data:

Copy adsprocessed_JPE.rds, HUDprocessed_JPE_census_042021.rds, HUDprocessed_JPE_testscores_042021.rds, and HUDprocessed_JPE_names_042021.rds from Christensen and Timmins (2022) replication package to Data/Original.

To Clean the Data, and Check Structure:

1. Clean Tester-input Address Names (to be used for verifying structure of the data, not in the final analysis) by running address_cleaning.py 
    - Can take up to an hour
    - Requires an Anthropic API key to send queries to Claude 3.7, saved in api_keys.py, which must be created. Template given in api_keys_TEMPLATE.py to fill in your own api key.


2. Generate Correct City Names by matching block group ids for the address in the initial ad to the list of incorporated and unincorporated cities (or in US census terminology, 'places') in the 2020 US Census. Run city_name_cleaning.r. 
    - Can take up to a couple hours due to the "spatial merge" requiring calculating intersections of thousands of shape files of census tracts and city boundaries

3. Verify Paired Block Structure of data and distribution of tests across sites. Run data_diagnosis.r. 

To Run the Analysis and Replicate All Tables:

1. Set the path in main.do to the PARENT DIRECTORY of this repository
2. Create a directory named "Output" in the PARENT DIRECTORY of this repository
3. Run main.do, all table and cleaning code will be run from this script
