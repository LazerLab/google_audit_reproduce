# Introduction

This folder stores the scripts for data processing and analysis.

# How to run each sub-workflow

You can using the following command to run the sub workflows

```bash
snakemake 1>snakemake_output.log 2>snakemake_error.log
```

The sub-workflows have the following dependency relationships.
Please run them in the given order.

* Run `na_analysis` to analyze for errors.   
* Run `drop_na_add_domain` to add domains, and keep only type of resul, which have lots of errors and not are related to our reserach questions. Also keep only the qries we are using for the analysis, which are US house members.
* Run `hancode_policontrol_pre` to generate samples for hand coding.  
* Run `generate_analysis_summary` to generate summaries for analysis.  
* Run `result_similarity` to analyze similarity of results. This can be run with `python3 file.py`.
* Run `r_plots` to generate plots using r language. This code does not work with snakemake. 
   