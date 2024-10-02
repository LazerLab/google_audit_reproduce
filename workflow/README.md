This folder stores the scripts for data processing and analysis.

# How to run each sub-workflows
snakemake 1>snakemake_output.log 2>snakemake_error.log

# Workflow dependency
1. Run `extract_domain` to get data with domains.
2. Run `filter_house` to get data for only house members.
3. Run `hancode_policontrol_pre` to generate samples for hand coding.