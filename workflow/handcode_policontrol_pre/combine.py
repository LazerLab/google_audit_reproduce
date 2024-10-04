'''
This files loads in all the partial summaries in sanity_check and combine them into a csv.

input: folder path of daily summary parquets. 
output: combined csv.
'''

import os
import pandas as pd
import sys
import ast

from urllib.parse import urlparse

def get_account(path):
    return path.split("/")[1]

def get_path(url):
    parsed = urlparse(url)
    account = get_account(parsed.path)
    return account

def combine_summaries(input_dir, output_csv, groupby):
    """
    Combine all summary files from the input directory into a single CSV file.

    :param input_dir: Path to the directory containing summary files.
    :param output_csv: Path for the output combined CSV file.
    """
    
    all_dfs = []

    for filename in input_dir: #os.listdir(input_dir): #
       # if filename.endswith(".parquet") and filename != '.ipynb_checkpoints':
            #filepath = os.path.join(input_dir, filename)
            #df = pd.read_parquet(filepath)
        df = pd.read_parquet(filename)
        
        print(filename)

        all_dfs.append(df)

    combined_df = pd.concat(all_dfs, ignore_index=True)

    combined_df = combined_df.groupby(groupby, dropna=False, as_index=False)['counts'].sum()
    combined_df['path'] = combined_df['url'].apply(get_path)
    
    combined_df.to_csv(output_csv, index=False)

if __name__ == "__main__":
    
    input_dir = sys.argv[1:-1]
    #input_dir = sys.argv[1]
    output_dir = sys.argv[-1]

    groupby = ['url','qry', 'domain','title', 'text']
    combine_summaries(input_dir, output_dir, groupby)
