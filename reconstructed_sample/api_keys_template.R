# API Keys Configuration Template
# Copy this file to 'api_keys.R' and add your actual API keys
# DO NOT commit api_keys.R to version control!

# =================================================================================================== #
# CENSUS AND GEOCODING API KEYS
# =================================================================================================== #

# U.S. Census Data API (required for ACS data via tidycensus)
# Get key from: https://api.census.gov/data/key_signup.html
CENSUS_API_KEY <- "your_census_api_key_here"



# =================================================================================================== #
# USAGE INSTRUCTIONS
# =================================================================================================== #

# 1. Copy this file to 'api_keys.R' in the same directory
# 2. Replace the placeholder values with your actual API keys  
# 3. Add 'api_keys.R' to your .gitignore file to avoid committing secrets
# 4. The geocoding functions will automatically use these keys if available

# For the HDS project, the free Census API should be sufficient for most needs
# But having backup APIs can help if you hit rate limits

