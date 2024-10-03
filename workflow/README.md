# Introduction

This folder stores the scripts for data processing and analysis.

# How to run each sub-workflow

You can using the following command to run the sub workflows

```bash
snakemake 1>snakemake_output.log 2>snakemake_error.log
```

The sub-workflows have the following dependency relationships.
Please run them in the given order.

1. Run `extract_domain` to get data with domains.
2. Run `filter_house` to get data for only house members.
3. Run `hancode_policontrol_pre` to generate samples for hand coding.