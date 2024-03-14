import pandas as pd

# Example DataFrame load
df = pd.read_excel('20240304-131356_240229MR_2_example.xlsx')

# Normalize luminescence by mCherry
df['Minor_norm'] = df['Minor'] / df['mCherry']
df['Major_norm'] = df['Major'] / df['mCherry']

# Calculate average DMSO control x-fold for normalization
avg_dmso_minor = df[df['compound'] == 'DMSO ctr']['Minor_norm'].mean()
avg_dmso_major = df[df['compound'] == 'DMSO ctr']['Major_norm'].mean()

# Calculate x-fold change relative to DMSO control
df['Minor_x_fold'] = df['Minor_norm'] / avg_dmso_minor
df['Major_x_fold'] = df['Major_norm'] / avg_dmso_major

# Assume df_replicates contains average x-folds for replicates and has a 'compound' column, indicate a threshold...
significant_decrease_minor = df_replicates[df_replicates['Minor_x_fold'] < threshold_minor]
significant_decrease_major = df_replicates[df_replicates['Major_x_fold'] < threshold_major]

# Further processing to categorize and match with library sheet would follow...
