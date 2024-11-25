# Analyzing F1 Race Car Statistics

With the spirit of reproducible research, this repository contains all the codes required to produce the results in the manuscript: 

> Anonymous

Please cite the above paper if you intend to use whole/part of the code. This code is only for academic and research purposes.

![biplot representation](Rplot_corrplot-new.png)

*Correlation between the various F1 car race variables*

## Code Organization
All the code is written in R. 

### Code 
The script to reproduce all the figures, tables in the paper is `F1_code.R`. Please run this script in R studio.

### Results and Contributions 
The three plots to explore the dataset - correlation plot, biplot and scree plot are all added in this repo. Additionally, to facilitate the analysis for this paper, the data is obtained by scraping this [website](https://www.racefans.net/). Data from different pages on this website was combined into one csv for efficient use. This data can be found in `combined_data.csv`.
