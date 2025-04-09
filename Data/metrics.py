import pandas as pd
import numpy as np

# Read the CSV file
df = pd.read_csv('Data/simu_perf_processed.csv')

# Calculate the new simu_global_perf with ldg weighted twice
metrics = ['ldg', 'app', 'alt', 'spd', 'time']
# Create weighted sum (ldg counts twice)
weighted_sum = df['ldg'] + df['app'] + df['alt'] + df['spd'] + df['time']
# Divide by total number of weights (6 instead of 5 since ldg counts twice)
df['simu_global_perf'] = weighted_sum / 5

# Save the modified dataframe back to CSV
# Using the same format as the original file (quoted text)
df.to_csv('Data/simu_perf_processed.csv', index=False, quoting=1)

print("Recalculation complete! The simu_global_perf column has been updated with weighted ldg.")