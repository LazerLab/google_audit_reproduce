'''
This files loads in all the partial summaries in summary_by_day and combine them into a csv.

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
    #for filename in input_dir:
        if filename.endswith(".parquet") and filename != '.ipynb_checkpoints':
            filepath = os.path.join(input_dir, filename)
            df = pd.read_parquet(filepath)
            
            print(filepath)

            all_dfs.append(df)

    combined_df = pd.concat(all_dfs, ignore_index=True)

    for i in range(len(output_csvs)):
        # if 'domain' in groupbys[i]:
        #     continue
        combined_df0 = combined_df.groupby(groupbys[i], dropna=True, as_index=False)['counts'].sum()
        combined_df0.to_csv(output_csvs[i], index=False)

if __name__ == "__main__":
    
    # input_dir = sys.argv[1:-7]
    # output_dir = sys.argv[-7:]
    input_dir = sys.argv[1]
    output_dir = sys.argv[2:]

    groupbys = [ ["domain"], 
                ["qry", "domain"], 
                ["domain",'serp_rank', 'qry', 'loc_id'], 
                ["crawl_id", 'qry', 'loc_id'],
                ["crawl_id", 'qry', 'domain', 'loc_id'],
                ["qry", "domain", 'serp_rank'],  
                ["crawl_id", "type", 'serp_rank']
    ]
    combine_summaries(input_dir, output_dir, groupbys)




    