"""
Take input from house_parquet, remove knowledge and related search results. Also remove search results with multiple instance at cmpt_rank. 

Input: parquet version of results
Output: cleaned parquet
"""

import sys
import pandas as pd
import os
import tldextract

# Initialize tldextract with no live suffix update
# This is for domain extraction
no_fetch_extract = tldextract.TLDExtract(suffix_list_urls=None)
    

# Extract domains from urls
def extract_domain(url):
    
    # Check if the URL is None or empty before proceeding
    if not url:
        return None
    try:
        extracted = no_fetch_extract(url)
        # Correctly access the domain and suffix properties of the ExtractResult
        domain_parts = [extracted.domain, extracted.suffix]
        # Join the non-empty parts with '.'
        return '.'.join(part for part in domain_parts if part)
    
    except Exception as e:
        print(f"Error extracting domain from {url}: {e}")
        return None



if __name__ == "__main__":
    input_file = sys.argv[1]
    qry_info_dir = sys.argv[2]
    output_file = sys.argv[-1]
    
    results = pd.read_parquet(input_file)
    
    #keep only type of results we need
    types = ['general', 'news_quotes', 'top_stories', 'twitter_cards', 'twitter_result', 'videos']
    results = results[results['type'].isin(types)]


    # extract domain
    results['domain'] = results['url'].apply(extract_domain)

    #drop results that have the same cmpt_rank on the same page
    results = results.drop_duplicates(subset=['domain', 'serp_rank', 'qry', 'loc_id'], keep='first')

    #read qry info
    qry_info = pd.read_csv(qry_info_dir)
    qry_info1 = qry_info[['qry', 'party']].drop_duplicates()

    #make sure qries are correct
    results = results[results['qry'].isin(qry_info1['qry'])]

    #add partisanship to the data
    combined_df = pd.merge(results, qry_info1, how="left", on='qry')

    #drop domains that are na
    combined_df.dropna(subset=['domain'], inplace=True)

    combined_df.to_parquet(output_file, index=False)
    