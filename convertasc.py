import pandas as pd
import sys

# Check if the file path is provided as a command-line argument
if len(sys.argv) != 2:
    print("Usage: python convert_asc_to_excel.py <path_to_asc_file>")
    sys.exit(1)

# Read the provided file path from command-line argument
asc_file_path = sys.argv[1]

# Assuming the ASC filename is like '20240304-131356_240229MR_2.asc',
# generate the Excel filename by replacing '.asc' with '.xlsx'
excel_file_path = asc_file_path.replace('.asc', '.xlsx')

# Load the ASC file, assuming it's tab-delimited; change 'sep' as needed
df = pd.read_csv(asc_file_path, sep='\t', encoding='ISO-8859-1')


# Save as Excel file
df.to_excel(excel_file_path, index=False)

print(f"Converted '{asc_file_path}' to '{excel_file_path}'")


# python convertasc.py C:\Users\coruf\Downloads\Raw_Data_Spliceosome_Pilot_Screen_1\Tecan_m1000\3_Lum_Minor\20240304-161852_240229MR_7.asc
