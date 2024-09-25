"""
Take input from preprocessed results. Filter out specific politicians and investigate loc difference for them.

Input: parquet version of results
Output: parquet files with only .gov and social media domain and certain fields
"""

import sys
import pandas as pd


if __name__ == "__main__":
    input_file = sys.argv[1]
    output_file = sys.argv[-1]
    
    results = pd.read_parquet(input_file)

    #drop duplicates
    results = results.drop_duplicates(subset=['domain', 'cmpt_rank', 'qry', 'loc_id'], keep='first')
    
    domains_keep = ['twitter.com', 'instagram.com', 'facebook.com', 'youtube.com']
    results_filtered = results[results['domain'].isin(domains_keep)]
    
    group_by = ['url','qry', 'domain','title', 'text']
    results_filtered = results_filtered.groupby(group_by, dropna=False).size().reset_index(name='counts')
    
    results_filtered.to_parquet(output_file, index=False)
