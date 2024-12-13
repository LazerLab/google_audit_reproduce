# Running dependency of the notebooks.

The notebooks are independent analysis of the data. However, certain files are generated throughout the process. The following is the suggested running order of notebooks, in order to make sure files are generated propoerly before need.

Gnereally, notebooks in `data_generate/` generates data, and notebooks in `analysis_stats` do the analysis. 

Note that graphs are not listed here. 

1. `na_analysis.ipynb` - input: `"/net/lazer/lab-lazer/shared_projects/google_audit_reproduce/intermedidate_files/merged_summary/na_summary.csv"`/ - output: `"../data/house_analysis/type_na_summary.csv"`.

2. `sanity_check.ipynb` - input: `/google_audit_reproduce/intermedidate_files/merged_summary/day_qry_loc.csv`.
   
3.  `top_domains.ipynb` - input: `/google_audit_reproduce/intermedidate_files/merged_summary/domain_counts.csv`. output: `/google_audit_reproduce/data/house_analysis/house_domain_counts_unique.csv`
   
4. `cumulative_percentage.ipynb` - input: `'../data/house_analysis/house_domain_counts_unique.csv'`.

5. `partisan_scatter.ipynb` - input: `/google_audit_reproduce/intermedidate_files/merged_summary/qry_domain.csv`. output: `../data/house_analysis/house_party_domain_no_duplicate.csv`, `../data/house_analysis/domain_dem_rep_prop.csv`.

6. `partisan_bias_score_analysis.ipynb` - input: `../data/house_analysis/domain_dem_rep_prop.csv`. output: `../data/house_analysis/domain_party_proportion_with_biasscore.csv`.

7. `national_local_prepare_data.ipynb` - input `/google_audit_reproduce/intermedidate_files/merged_summary/domain_rank_qry_loc.csv`. output: `"../data/house_analysis/qry_loc_checkhome.csv"`, `../data/house_analysis/domain_rank_ishome_category.csv`, `../data/house_analysis/domain_rank_ishome_category_counts.csv`.

8. `national_local_analysis.ipynb` input: `"../data/house_analysis/domain_rank_ishome_category_counts.csv"`.  

9. `data_for_r_plots.ipynb` - input: `"../data/house_analysis/domain_rank_ishome_category_counts.csv"`. output: `"../data/house_analysis/domain_isnews_islocal_ispolicontrol.csv"`.

10. `domain_qry_rank_data.ipynb` - input: `"/net/lazer/lab-lazer/shared_projects/google_audit_reproduce/intermedidate_files/merged_summary/day_qry_domain_rank.csv"`. output: `"../data/house_analysis/party_domain_rank_class_cat_isnews_islocal_ispolicontrol.csv"`.

11. `category_analysis.ipynb` - input: `"../data/house_analysis/party_domain_rank_class_cat_isnews_islocal_ispolicontrol.csv"`.

12. `coverage_rate_check.ipynb` - input: `'../data/house_analysis/house_domain_counts_unique.csv'`, `"../data/house_analysis/domain_rank_ishome_category_counts.csv"`.

13. `loc_similarity.ipynb` - input: `"../data/house_analysis/home_random_dist_rbo_similarity_clean_locid.csv"`, `'../data/house_analysis/qry_loc_checkhome.csv'`.
