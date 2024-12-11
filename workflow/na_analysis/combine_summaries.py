'''
This files loads in all the partial summaries in error_summary.

input: folder path of daily summary parquets. 
output: combined csv.
'''

import os
import pandas as pd
import sys
import ast

def combine_summaries(input_dir, output_csvs, groupbys):
    """
    Combine all summary files from the input directory into a single CSV file.

    :param input_dir: Path to the directory containing summary files.
    :param output_csv: Path for the output combined CSV file.
    """
    
    all_dfs = []

    for filename in os.listdir(input_dir):
        if filename.endswith(".parquet") and filename != '.ipynb_checkpoints':
            filepath = os.path.join(input_dir, filename)
            df = pd.read_parquet(filepath)
            
            print(filepath)

            all_dfs.append(df)

    combined_df = pd.concat(all_dfs, ignore_index=True)

    combined_df0 = combined_df.groupby(groupbys, dropna=False, as_index=False)['counts'].sum()
    combined_df0.to_csv(output_csvs, index=False)

if __name__ == "__main__":
    
    input_dir = sys.argv[1]
    output_dir = sys.argv[-1]

    groupbys = ['crawl_id', "is_na_url", "type"]
    combine_summaries(input_dir, output_dir, groupbys)




    