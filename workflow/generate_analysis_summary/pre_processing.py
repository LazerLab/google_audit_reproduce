"""
Take input from preprocessed parquet version web_search results,
conduct sanity check and save check summary.

Input: parquet version of results
Output: summary parquet for each variables
"""



import sys
import pandas as pd
import numpy as np


if __name__ == "__main__":
    input_file = sys.argv[1]
    output_file = sys.argv[-1]
    
    results = pd.read_parquet(input_file)
    results.replace('', np.nan, inplace=True)
    results = results[results['domain'].notna()]

    
    group_by = ['crawl_id', 'type', 'qry', 'domain', 'serp_rank', 'loc_id']
    results_by_qry_domain = results.groupby(group_by, dropna=False).size().reset_index(name='counts')

    results_by_qry_domain.to_parquet(output_file, index=False)

