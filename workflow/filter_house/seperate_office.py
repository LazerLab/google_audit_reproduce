"""
Take input from raw data with domain and filter to only house data. 

Input: parquet with domain file
Output: reduced parquet file
"""


import sys
import pandas as pd

input_file = sys.argv[1]
politician_info_dir = sys.argv[2]
house_output_file = sys.argv[-1]


# import files
raw_df = pd.read_parquet(input_file)

# import office info
# note that this is already filtered to only house members with high enough relevance_score

politician_info = pd.read_csv(politician_info_dir)

house_members = politician_info['qry'].unique()

# keep only house member data
house_df = raw_df[raw_df['qry'].isin(house_members)]

house_df.to_parquet(house_output_file, index=False)
