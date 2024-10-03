"""
Take input from house_parquet, remove knowledge and related search results. Also remove search results with multiple instance at cmpt_rank. 

Input: parquet version of results
Output: cleaned parquet
"""

import sys
import pandas as pd


if __name__ == "__main__":
    input_file = sys.argv[1]
    qry_info_dir = sys.argv[2]
    output_file = sys.argv[-1]
    
    results = pd.read_parquet(input_file)
    
    #drop knowledge and search related
    results = results[~((results['type']=='searches_related') | (results['type']=='knowledge')) ]

    #drop duplicates
    results = results.drop_duplicates(subset=['domain', 'cmpt_rank', 'qry', 'loc_id'], keep='first')

    qry_info = pd.read_csv(qry_info_dir)
    qry_info1 = qry_info[['qry', 'party']].drop_duplicates()

    combined_df = pd.merge(results, qry_info1, how="left", on='qry')

    combined_df.to_parquet(output_file, index=False)

