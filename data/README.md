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
| relevance_score | The number of relevant results in a random sample of 6 URLs from the qry's search results |

There are a few things to note.
The list only contains the house of representatives candidates.
If a politician has multiple Twitter handles, they will appear in the list multiple times. Be sure to remove duplicates before analysis.
There are 425 unique politicians in the list.

While cleaning the data, we find that the search results of some politicians contain irrelevant results, often due to name sharing with other famous people.
To quantitatively measure the relevance of the search results, we count the number of relevant results in a random sample of 6 URLs from each politician's search results.
This score is the `relevance_score` column in the table.
We exclude the politicians with a relevance score less than 3 from our study.
Specifically, we exclude the following politicians: "Chris Smith", "Mario Díaz-Balart", "Carol Miller", "Roger Williams", "Joaquín Castro", "Jesús Chuy García", "Jason Smith", "Daniel Webster", "Paul Mitchell", "John Curtis", "John Carter", "John Lewis".
See our paper for more details.

## Domain Classification

To analyze the search results of Google, we focus on the domains and classify them into different categories.
Here are the files containing the domain classifications:

| File Name                                    | Number of unique domains | Description                                                      |
|----------------------------------------------|--------------------------|----------------------------------------|
| `bias_score_ron_2018.csv`                    | 19,014 | Political bias score from [Robertson et al. (2018)](https://doi.org/10.1145/3274417). |
| `domain_category.csv`                        | 8,878 | Category of domains labeled by the authors. See the paper for more details. |
| `domain_classification_local_vs_national.csv`| 16,489 | Compiled list of domains and their local vs national classification. See the paper for more details. |
