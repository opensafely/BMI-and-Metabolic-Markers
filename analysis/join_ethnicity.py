import pandas as pd
import os

ethnicity_df = pd.read_feather('output/data/input_ethnicity.feather')  #  files read in as feather format.  for csv.   code = ethnicity_df = pd.read_csv('output/data/input_ethnicity.csv')

for file in os.listdir('output/data'):
    if file.startswith('input'):
        #exclude ethnicity
        if file.split('_')[1] not in ['ethnicity.feather']:
            file_path = os.path.join('output/data', file)
            df = pd.read_feather(file_path)
            merged_df = df.merge(ethnicity_df, how='left', on='patient_id').set_index('patient_id')
            
            merged_df.to_feather(file_path)
            
