# Introduction

This folder contains the datasets used in the project. Only small data files are committed to the repository. Larger files are stored on a separate data repository.

# Files

## House representatives information

The file `qry_info.csv` contains information of house representatives used in the analysis.
The table schema is as follows:

| Column Name     | Description                                      |
|-----------------|--------------------------------------------------|
| qry             | Name of the congressional candidate              |
| qry_clean       | Cleaned name of the congressional candidate      |
| Twitter         | Twitter handle of the congressional candidate    |
| state           | State abbreviation                               |
| district        | Congressional district number                    |
| party           | Political party affiliation, can be "Democrat", "Republican", "Independent" |
| relevance_score | Relevance score of the candidate, range from 0 to 6 |

There are a few things to note.
The list only contains the house of representatives candidates.
If a politician has multiple Twitter handles, they will appear in the list multiple times. Be sure to remove duplicates before analysis.
There are 425 unique politicians in the list.

We also exclude the following politicians from the list: "Chris Smith", "Mario Díaz-Balart", "Carol Miller", "Roger Williams", "Joaquín Castro", "Jesús Chuy García", "Jason Smith", "Daniel Webster", "Paul Mitchell", "John Curtis", "John Carter", "John Lewis".
Searching these names often lead to irrelevant results, e.g., due to name sharing with another famous person.
See our paper for more details.

## Domain Classification

To analyze the search results of Google, we focus on the domains and classify them into different categories.
Here are the files containing the domain classifications:

| File Name                                    | Number of unique domains | Description                                                      |
|----------------------------------------------|--------------------------|----------------------------------------|
| `bias_score_ron_2018.csv`                    | 19,014 | Political bias score from [Robertson et al. (2018)](https://doi.org/10.1145/3274417). |
| `domain_category.csv`                        | 8,878 | Category of domains labeled by the authors. See the paper for more details. |
| `domain_classification_local_vs_national.csv`| 16,489 | Compiled list of domains and their local vs national classification. See the paper for more details. |
