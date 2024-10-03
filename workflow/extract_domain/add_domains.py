'''
This file goes over all the parquet results and add a domain column.

input: parquet file.
output: updated parquet file. 
'''

import os
import sys
import tldextract
import pandas as pd

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
    input_dir = sys.argv[1]
    output_dir = sys.argv[2]
    

    results = pd.read_parquet(input_dir)
    
    # extract domain
    results['domain'] = results['url'].apply(extract_domain)
    
    results.to_parquet(output_dir, index=False)
    print(output_dir)