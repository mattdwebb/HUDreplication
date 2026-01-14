import time
import re
import pandas as pd
import anthropic
from tqdm import tqdm
import json

# Set the working directory to the directory location of the github repository 
# This will be appended to the front of all addresses in the file
WORKING_DIRECTORY = "cities-from-geoid"


def standardize_addresses(
    input_file,
    address_column='HSITEAD',
    batch_size=100,
    batch_overlap=10,
    model="claude-3-7-sonnet-20250219",
    testing_mode=False,
    api_key=None
):
    """
    Standardize addresses using Claude API.
    
    Args:
        input_file (str): Path to CSV file containing addresses
        address_column (str): Name of the column containing addresses
        batch_size (int): Number of addresses to process in each batch
        batch_overlap (int): Number of addresses to overlap between batches
        model (str): Claude model to use
        testing_mode (bool): If True, only process the first batch
        api_key (str): Anthropic API key. If None, will try to import from api_keys.py
        
    Returns:
        pandas.DataFrame: DataFrame with original data plus standardized addresses
    """
    # Start timing if in testing mode
    if testing_mode:
        start_time = time.time()
    
    # Get API key if not provided
    if api_key is None:
        try:
            # Try importing directly from the same directory
            from api_keys import ANTHROPIC_API_KEY
            api_key = ANTHROPIC_API_KEY
        except ImportError:
            try:
                # Try importing from HUDreplication module
                from HUDreplication.api_keys import ANTHROPIC_API_KEY
                api_key = ANTHROPIC_API_KEY
            except ImportError:
                # Fallback in case the secrets file doesn't exist yet
                print("Warning: api_keys.py file not found. Please provide an API key.")
                return None

    # Load the CSV with addresses
    df = pd.read_csv(input_file)

    # Extract the address column to a list
    addresses = df[address_column].dropna().unique().tolist()

    # Function to extract numeric parts for proper sorting
    def extract_number(address):
        numbers = re.findall(r'\d+', address)
        return int(numbers[0]) if numbers else 0

    # Sort addresses alphanumerically
    # First by numeric component, then alphabetically
    addresses.sort(key=lambda x: (extract_number(x), x))

    # Initialize the Anthropic client
    client = anthropic.Anthropic(api_key=api_key)
    
    # Create batches with overlap (to avoid context limits and improve clustering)
    def chunk_list_with_overlap(lst, chunk_size=batch_size, overlap=batch_overlap):
        if len(lst) <= chunk_size:
            return [lst]
        
        chunks = []
        i = 0
        while i < len(lst):
            # Include overlap from previous batch for context, except for first batch
            start = max(0, i - overlap)
            end = min(i + chunk_size, len(lst))
            chunks.append(lst[start:end])
            
            # Move forward by chunk_size
            i += chunk_size
        
        return chunks
    
    address_batches = chunk_list_with_overlap(addresses, chunk_size=batch_size, overlap=batch_overlap)
    
    # If in testing mode, only process the first batch
    if testing_mode:
        address_batches = [address_batches[0]]
        print(f"TESTING MODE: Processing only the first batch of {len(address_batches[0])} addresses")

    standardized_addresses = {}

    # Process each batch
    for batch_idx, batch in enumerate(tqdm(address_batches)):
        # Create a prompt with context of all addresses in this batch
        prompt = f"""
        I need to standardize a list of handwritten addresses from research participants and research assistants. These addresses have inconsistent formatting, typos, variations in street name spellings (e.g., one vs. two words), and inconsistent use of directional indicators (N, S, E, W).

        Your task:
        1. Convert all addresses to USPS standard format with UPPERCASE text
        2. Identify addresses that likely refer to the same physical location, even with different spellings or formatting, and give them the same standardized format
        3. Assign identical standardized formats to addresses that probably match, even if you're only 80% confident
        4. Apply these matching rules aggressively:
           - Same number + similar street name = SAME LOCATION
           - Addresses differing only by street type (ST/AVE/BLVD) should be considered the same if number + base name match
           - When one address lacks a street type but otherwise matches another with a type, use the one with the type
           - Treat abbreviations and full words as equivalent (Ave = Avenue, St = Street)
           - Assume one/two word variations of the same name are the same street (e.g., "Pinewood" vs "Pine Wood")
    
    For each address, return a JSON object with:
    1. "original": The original address
    2. "standardized": The standardized version
    
    The addresses are already sorted alphanumerically to help you identify patterns:
    {batch}
    
    Format your response as valid JSON like this:
    [
          {{"original": "1 Orange Heights", "standardized": "1 ORANGE HEIGHTS AVENUE"}},
          {{"original": "1 Orange Heights Ave.", "standardized": "1 ORANGE HEIGHTS AVENUE"}},
          {{"original": "789 Pine Wood Dr", "standardized": "789 PINEWOOD DRIVE"}},
          {{"original": "789 Pinewood Drive", "standardized": "789 PINEWOOD DRIVE"}},
          {{"original": "1010 Maple Ave NW", "standardized": "1010 MAPLE AVENUE"}},
          {{"original": "1010 Maple Avenue", "standardized": "1010 MAPLE AVENUE"}}
    ]
    
    Only include the JSON array in your response, nothing else.
    """
        
        # Call Claude
        try:
            response = client.messages.create(
                model=model,
                max_tokens=8000,
                messages=[
                    {"role": "user", "content": prompt}
                ]
            )
            
            # Parse the response
            result = json.loads(response.content[0].text)
            
            # Print the result if in testing mode
            if testing_mode:
                print("\nAPI Response:")
                for item in result:
                    print(item)
            
            # Update our mapping dictionary
            # For batches after the first, skip the first addresses (context only, half the overlap)
            start_idx = 0 if batch_idx == 0 else batch_overlap//2
            end_idx = len(result) if batch_idx == 0 else len(result) - batch_overlap//2
            for item in result[start_idx:end_idx]:
                standardized_addresses[item['original']] = {
                    'standardized': item['standardized']
                }
            
            # Avoid rate limiting
            time.sleep(1)
            
        except Exception as e:
            print(f"Error processing batch: {e}")
            print("Response content:", response.content[0].text if 'response' in locals() else "No response")
            continue

    # Apply the standardization back to the original dataframe
    df['standardized_address'] = df[address_column].map(
        lambda x: standardized_addresses.get(x, {}).get('standardized', x) if pd.notna(x) else x
    )
    
    if testing_mode:
        end_time = time.time()
        execution_time = end_time - start_time
        print(f"\nTesting complete. Processed {len(standardized_addresses)} addresses.")
        print(f"Execution time: {execution_time:.2f} seconds ({execution_time/60:.2f} minutes)")
    
    return df


# Process only the adsprocessed_JPE file and save with standardized addresses
input_file = f"{WORKING_DIRECTORY}/Data/Generated/adsprocessed_JPE.csv"
output_file = f"{WORKING_DIRECTORY}/Data/Generated/adsprocessed_JPE_clean_addresses.csv"
address_col = "HSITEAD"  # This is confirmed correct for adsprocessed_JPE.csv

print(f"Processing {input_file} (address column: {address_col})...")
df_with_standardized = standardize_addresses(input_file, 
                                           address_column=address_col)
if df_with_standardized is not None:
    df_with_standardized.to_csv(output_file, index=False)
    print(f"Saved to {output_file}")
else:
    print(f"Failed to process {input_file}")


# Load the standardized addresses file
print("\nLoading standardized addresses file...")
df_standardized = pd.read_csv(output_file)

# Count unique standardized addresses per control
print("Analyzing unique addresses per control...")
df_standardized_grouped = df_standardized.groupby('CONTROL')['standardized_address'].apply(lambda x: x.nunique())
df_standardized['num_unique_addresses'] = df_standardized['CONTROL'].map(df_standardized_grouped)

# Create a summary dataframe with controls and their unique addresses
print("Creating summary of controls and their addresses...")
control_addresses = []
for control, group in df_standardized.groupby('CONTROL'):
    unique_addresses = group['standardized_address'].unique().tolist()
    control_addresses.append({
        'control': control,
        'unique_addresses': unique_addresses,
        'num_unique_addresses': len(unique_addresses)
    })

# Convert to DataFrame
control_address_df = pd.DataFrame(control_addresses)

# Print some statistics
print(f"\nStatistics:")
print(f"Total controls: {len(control_address_df)}")
print(f"Controls with multiple addresses: {len(control_address_df[control_address_df['num_unique_addresses'] > 1])}")
print(f"Maximum addresses per control: {control_address_df['num_unique_addresses'].max()}")


# Export detailed list of controls with multiple addresses
if len(control_address_df[control_address_df['num_unique_addresses'] > 1]) > 0:
    multi_address_file = "HUDreplication/Data/controls_with_multiple_addresses.json"
    multi_address_controls = control_address_df[control_address_df['num_unique_addresses'] > 1].to_dict(orient='records')
    with open(multi_address_file, 'w') as f:
        json.dump(multi_address_controls, f, indent=2)
    print(f"Saved detailed list of controls with multiple addresses to {multi_address_file}")

    # Analyze controls with multiple addresses to identify potential duplicates
    print("\nAnalyzing controls with multiple addresses for potential duplicates...")
    
    # Load the controls with multiple addresses
    with open(multi_address_file, 'r') as f:
        multi_address_controls = json.load(f)
    
    # Reuse the API key from the standardize_addresses function
    try:
        # Try importing directly from the same directory
        from api_keys import ANTHROPIC_API_KEY
        api_key = ANTHROPIC_API_KEY
    except ImportError:
        try:
            # Try importing from HUDreplication module
            from HUDreplication.api_keys import ANTHROPIC_API_KEY
            api_key = ANTHROPIC_API_KEY
        except ImportError:
            print("Warning: api_keys.py file not found. Skipping address correction.")
            api_key = None
    
    if api_key:
        # Initialize the client with the API key
        client = anthropic.Anthropic(api_key=api_key)
        model = "claude-3-7-sonnet-20250219"
        
        # Process each control separately
        all_address_corrections = {}
        print(f"Processing {len(multi_address_controls)} controls with multiple addresses...")
        
        for i, control_data in enumerate(multi_address_controls):
            control = control_data['control']
            addresses = control_data['unique_addresses']
            
            # Skip if there's only one address (shouldn't happen, but just in case)
            if len(addresses) <= 1:
                continue
                
            print(f"Processing control {i+1}/{len(multi_address_controls)}: {control} ({len(addresses)} addresses)")
            
            # Create a prompt specific to this control
            prompt = f"""
            I have a set of addresses for control ID "{control}" that may or may not refer to the same physical location. The documentation says that in each control, one address was measured by two different testers, who may have recorded the address differently. But in some cases, certainly multiple addresses were visited.
            Please analyze these addresses to identify if any should be standardized to match others, considering:
            1. Small typos or formatting differences
            2. Transposed digits in street numbers (e.g., 2503 vs 2305)
            3. Variations in street name formatting (AVE vs AVENUE, etc.)
            4. Directional prefixes/suffixes that might be missing or different (N, S, E, W)
            5. Unit numbers that might be included in one but not the other

            IMPORTANT: When there are only two or three addresses in a control and two share the same (or similar) street name but have different numbers, they should be considered the same location with a recording error if:
            - The numbers are transposed (e.g., 507 vs 705)
            - The numbers differ by only 1-2 digits (e.g., 507 vs 508)
            - The numbers share some digits in the same positions (e.g., 507 vs 807, 2503 vs 2304)
            
            However, addresses with completely different numbers (e.g., 123 vs 789) should not be matched even if on the same street.

            The addresses are:
            {json.dumps(addresses, indent=2)}

            If you find addresses that should be standardized, return a JSON object with this structure:
            {{
              "address_corrections": {{
                "address_to_correct_1": "corrected_form",
                "address_to_correct_2": "corrected_form",
                ...
              }}
            }}

            For the corrected form, use one of the existing addresses that appears most likely to be correct.
            If you're unsure which address is correct, choose the one that appears first in the list.

            If all addresses are already standardized correctly or can't be confidently matched, return:
            {{
              "address_corrections": {{}}
            }}

            THIS IS CRUCIAL: Only include the JSON array in your response, nothing else. 
            """
            
            try:
                # Call Claude API for this specific control
                response = client.messages.create(
                    model=model,
                    max_tokens=1000,  # Lower token limit for smaller job
                    messages=[
                        {"role": "user", "content": prompt}
                    ]
                )
                
                # Print raw response for debugging if needed
                # print(f"\nRaw response: {response.content[0].text[:100]}...")
                
                # Parse the response
                try:
                    # Get the response text
                    response_text = response.content[0].text
                    
                    # Check if the response starts with "json" and remove it
                    if response_text.strip().startswith("json"):
                        response_text = response_text.strip()[4:].strip()  # Remove "json" prefix and trim whitespace
                    
                    # Parse the cleaned response
                    result = json.loads(response_text)
                    
                    # Rest of your code...
                except json.JSONDecodeError as e:
                    print(f"  JSON parsing error: {str(e)}")
                    print(f"  Raw response: {response.content[0].text[:500]}")
                except Exception as e:
                    print(f"  Error processing control {control}: {str(e)}")
                
                # Add corrections to the master list
                if 'address_corrections' in result and result['address_corrections']:
                    correction_count = len(result['address_corrections'])
                    print(f"  Found {correction_count} address corrections for control {control}")
                    all_address_corrections.update(result['address_corrections'])
                else:
                    print(f"  No corrections needed for control {control}")
                
                # Avoid rate limiting
                time.sleep(1)
                
            except Exception as e:
                print(f"  Error processing control {control}: {str(e)}")
                print(f"  Response content: {response.content[0].text[:500] if 'response' in locals() and hasattr(response, 'content') else 'No response'}")
                continue
        
        # Save all address corrections to a file
        corrections_file = f"{WORKING_DIRECTORY}/Data/Generated/address_corrections.json"
        with open(corrections_file, 'w') as f:
            json.dump({"address_corrections": all_address_corrections}, f, indent=2)
        print(f"Saved {len(all_address_corrections)} address corrections to {corrections_file}")
        
        # Apply corrections to the standardized addresses dataframe
        if all_address_corrections:
            print(f"Applying {len(all_address_corrections)} address corrections...")
            
            # Create a mapping function that applies corrections
            def apply_correction(address):
                if address in all_address_corrections:
                    return all_address_corrections[address]
                return address
            
            # Apply corrections to the dataframe
            df_standardized['standardized_address'] = df_standardized['standardized_address'].apply(apply_correction)
            
            # Save the corrected dataframe
            corrected_output = f"{WORKING_DIRECTORY}/Data/Generated/adsprocessed_JPE_clean_addresses.csv"
            df_standardized.to_csv(corrected_output, index=False)
            print(f"Saved corrected standardized addresses to {corrected_output}")
            
            # Recalculate unique addresses per control
            print("Recalculating unique addresses per control...")
            df_standardized_grouped = df_standardized.groupby('CONTROL')['standardized_address'].apply(lambda x: x.nunique())
            df_standardized['num_unique_addresses'] = df_standardized['CONTROL'].map(df_standardized_grouped)
            
            # Create updated summary
            updated_control_addresses = []
            for control, group in df_standardized.groupby('CONTROL'):
                unique_addresses = group['standardized_address'].unique().tolist()
                updated_control_addresses.append({
                    'control': control,
                    'unique_addresses': unique_addresses,
                    'num_unique_addresses': len(unique_addresses)
                })
            
            # Print updated statistics
            print(f"\nUpdated Statistics:")
            print(f"Total controls: {len(updated_control_addresses)}")
            print(f"Controls with multiple addresses: {len(updated_control_addresses[updated_control_addresses['num_unique_addresses'] > 1])}")
            max_addresses = max(item['num_unique_addresses'] for item in updated_control_addresses)
            print(f"Maximum addresses per control: {max_addresses}")
        else:
            print("No address corrections were identified.")
    else:
        print("Skipping address correction due to missing API key.")


# Print a list of the remaining controls with multiple addresses and the addresses assigned to them

if all_address_corrections:
    # Use the updated data if corrections were applied
    controls_with_multiple = [item for item in updated_control_addresses if item['num_unique_addresses'] > 1]
    data_source = "corrected"
else:
    # Use the original data if no corrections were applied
    controls_with_multiple = pd.DataFrame(control_addresses)[pd.DataFrame(control_addresses)['num_unique_addresses'] > 1]
    data_source = "original"
    
print(f"\nRemaining controls with multiple addresses ({data_source} data):")
print(f"Total: {len(controls_with_multiple)} controls")
print("\nDetailed list:")

if all_address_corrections:
    # For list of dictionaries
    for item in controls_with_multiple:
        print(f"\nControl: {item['control']}")
        print(f"Number of unique addresses: {item['num_unique_addresses']}")
        print("Addresses:")
        for address in item['unique_addresses']:
            print(f"  - {address}")
else:
    # For DataFrame
    for _, row in controls_with_multiple.iterrows():
        print(f"\nControl: {row['control']}")
        print(f"Number of unique addresses: {row['num_unique_addresses']}")
        print("Addresses:")
        for address in row['unique_addresses']:
            print(f"  - {address}")