"""
Take input from preprocessed parquet version web_search results,
conduct sanity check on NaN urls and type of results.

Input: parquet version of house member results
Output: summary parquet for each variables
"""



import sys
import pandas as pd


if __name__ == "__main__":
    input_file = sys.argv[1]
    output_file = sys.argv[-1]
    
    results = pd.read_parquet(input_file)

    results["is_na_url"] = results["url"].isna()

    
    group_by = ['crawl_id', 'is_na_url', 'type']
    results_by_url_type = results.groupby(group_by, dropna=False).size().reset_index(name='counts')

    results_by_url_type.to_parquet(output_file, index=False)

