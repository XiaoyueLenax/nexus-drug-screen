import pandas as pd
import sys

# Check if the file path is provided as a command-line argument
if len(sys.argv) != 2:
    print("Usage: python convert_tsv_to_excel.py <path_to_tsv_file>")
    sys.exit(1)

# Read the provided file path from command-line argument
tsv_file_path = sys.argv[1]

# Assuming the TSV filename is formatted like '20240105_L3900_SelleckChem_Clinical.tsv',
# we generate the Excel filename by replacing '.tsv' with '.xlsx'
excel_file_path = tsv_file_path.replace('.tsv', '.xlsx')

# Load the TSV file
df = pd.read_csv(tsv_file_path, sep='\t')

# Save as Excel file
df.to_excel(excel_file_path, index=False)

print(f"Converted '{tsv_file_path}' to '{excel_file_path}'")