"""
Take input from house_parquet_no_error, calculate similarity for search results.

Input: parquet version of results
Output: csv of similarity
"""

import sys
import pandas as pd
import rbo
import numpy as np
import re
import random
import os


# This function returns all the days in the dataset
def get_all_dates(path):
    # Infer dates
    dir_list = os.listdir(RAW_PARQUET_PATH)
    DATES = []
    for r in dir_list:
        if r=='.ipynb_checkpoints':
            continue
        r = r.replace('results-', '')
        r = r.replace('.parquet', '')
        DATES.append(r)
    return DATES


# This function gives us the rbo similarity between two days of data
def calculate_rbo(sample1, sample2):
    S = sample1.dropna()["url"].to_list() # Remove None
    T = sample2.dropna()["url"].to_list() # Remove None

    # Sort sample by rank
    sample1.sort_values('cmpt_rank', inplace=True)
    sample2.sort_values('cmpt_rank', inplace=True)

    # cap the longer list ot the length of shorter list
    l = min(len(S), len(T))

    return rbo.RankingSimilarity(S[:l], T[:l]).rbo()


def get_dataframe(path, qry, day, district1, district2):
    RAW_PARQUET_FILE = os.path.join(path, day+".parquet")
    df = pd.read_parquet(RAW_PARQUET_FILE)

    df1 = df[(df["qry"]==qry) & (df['loc_id']==district1)]
    df2 = df[(df["qry"]==qry) & (df['loc_id']==district2)]
    
    return df1, df2


# Find random sample from the data to compare.
# For all qry, choose 10 random days, for each day, choose a random district and compare with home dist.
def get_results(qry_loc_checkhome, path):
    results = []
    dates = get_all_dates(path)

    # subset the data to politicians with home districts
    qry_loc_checkhome_sub = qry_loc_checkhome[qry_loc_checkhome['is_home_district']==True]
    qry_list = qry_loc_checkhome_sub["qry"].drop_duplicates().to_list() #get qry list
    
    for qry in qry_list:
        
        qry_set = qry_loc_checkhome[qry_loc_checkhome["qry"]==qry]
        
        # list unique loc ids and home 
        homedist = qry_set[qry_set["is_home_district"]==True]['loc_id'].drop_duplicates().to_list()[0] # home district
        loc_list = qry_set[qry_set["is_home_district"]==False]['loc_id'].drop_duplicates().to_list()

        #pick 10 random days and a random location for each day
        ten_days = random.sample(dates, 10) 
        random_locs = random.sample(loc_list, 10)
        
        # loop through days and random locations
        for i in range(10):
            home_df, random_df = get_dataframe(path, qry, ten_days[i], homedist, random_locs[i])
            similarity = calculate_rbo(home_df, random_df)
            results.append([ten_days[i], qry, homedist, random_locs[i], len(home_df), similarity])

    result_df = pd.DataFrame(results, columns=['crawl_id', 'qry', 'home_dist', 'random_dist', 'result_length', 'rbo_similarity'])
    
    return result_df


if __name__ == "__main__":

    DATA_ROOT = "/net/lazer/lab-lazer/shared_projects/google_audit_reproduce/intermedidate_files"
    RAW_PARQUET_PATH = os.path.join(DATA_ROOT, "house_parquet_no_error")

    qry_loc_checkhome = pd.read_csv('../../data/house_analysis/qry_loc_checkhome.csv')

    result_df = get_results(qry_loc_checkhome, RAW_PARQUET_PATH)
    result_df.to_csv('../../data/house_analysis/home_random_dist_rbo_similarity.csv', index=False)






    
