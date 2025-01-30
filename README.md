# Popularity ~ Feature Analysis for INF6027
 ## Investigating quantitative features in music

<img 
src = "https://raw.githubusercontent.com/G-Imola/INF6027/main/plots/Github%20plot%20for%20readme.png"
 alt = "GitHub README Image">


## Introduction

Investigating quantitative features in music: 

A statistical analysis of popularity and its relationship to musical features


Music forecasting is a highly discussed topic within the music industry, as identifying 
the formula for "success" can give artists and producers a commercial advantage regarding investment opportunities.
Despite its importance, it remains as an obstacle in the music industry due to the inherently contextual nature of song popularity.


This report intended to provide additional perspectives to this field of knowledge, through examination of quantitative data present in all audio tracks. Additionally,
the report aimed to define what musical features are associated with popular songs and genres. The specific research questions were listed as follows:


1. Which musical feature most impacts a song’s popularity? 
2. What features distinguish high – and low –popularity genres? 
3. Can genre popularity be predicted?


## Sourcing

The dataset was sourced from the <a href="https://hf-proxy-cf.effarig.site/datasets/maharshipandya/spotify-tracks-dataset" target="_blank"> Maharshipandya Dataset</a>.

The dataset contains 114,000 tracks with distinct musical features spanning across 114 genres, with each track containing unique numerical features with ranging scales and values.

## Packages

The report was carried out using R programming language Version 4.4.1 (2024-06-14 ucrt) and software  RStudio Desktop for winOS version 2024.12.0+467. Besides the base R packages, the following packages were also used:

* `tidyverse`
* `car`
* `corrplot`
* `RColorBrewer`
* `treemapify`
* `ggalt`


## Composite visualisations
 The dataset and code generated composite visualisations depicting the predictive power of models employed throughout analysis.
These were visualised to provide a detailed view of musical features and their relationships to popularity across genres:
* **Univariate popularity ~ feature analysis:**
Presented relationships between popularity across genres, and their musical features.

* **Multivariate popularity ~ features analysis:**
Displayed genre - feature associations to evaluate differences between high/low popularity genres.


## Steps to run the code:

### 1. **Prerequisites**

Before running the code, ensure the following software is installed:

* R(Version 4.0 or later)
* RStudio (Integrated Development Environment, **IDE**)
* Git (to clone the repository)


### **2.Clone the repository and verify branch**

Download the project files by cloning the repository.
This can be performed by running the following command
in your **IDE**:

`git clone https://github.com/G-Imola/Popularity-Feature-Analysis.git`

After cloning, verify that the active Git branch is set to **main**.

To check the branch, run the following command:

`git branch`

You've done this correctly if the output shows ***main**.

If the branch is not set to main, you can switch to the **main**
branch by following these steps:

```
#enter the terminal on your selected IDE and input the command below:

cd Popularity-Feature-Analysis


#Following this, type the code below:

git checkout main


#Finally, test to verify "main" branch has been selected:

git branch
```
This ensures that you're working on the correct branch for the project.

After following the steps above, your IDE should display the repository, alongside all other data that comes with it!



 ### 3. Dataset Placement
Ensure that the <a href="https://github.com/G-Imola/Popularity-Feature-Analysis/blob/main/Original%20DatasetOriginal" target="_blank">Original Dataset</a> is downloaded and placed in the root
directory of the cloned repository.

 ### 4. Execute the script
Open `R file.R` (<a href ="https://github.com/G-Imola/Popularity-Feature-Analysis/blob/main/R%20file.R">link</a>) in Rstudio, or your preferred IDE, and run the script sequentially to generate:
1. Descriptive box plots
2. Correlation plots
3. Composite scatter-plots of models
4. Additional descriptive plots

 ### 5.Outputs
Generated visualisations are all stored in the `plots` directory.

The outputs include:

* `Faceted univariate plot.png`
* `Faceted MVR plot.png`
* `Popularity-genre Box plot.png`
* `Corrplot full data(grouped).png`
* `Corrplot full data(ungrouped).png`

Additionally, generated `.csv` files are stored in the `csv ouputs` folder, which contain various outputs from the dataset, including:

* `MVR test residuals.csv`
* `Full MVR Model output.csv`
* `vif scores for training data.csv`
* `Test-dataset predicted Avg.csv`
* and more!

## key findings

1. Primary predictors of popularity:
   * High popularity tracks presented high danceability, loudness, low instrumentalness and liveness.
     * Suggests high popularity tracks are highly loud danceable tracks with little instruments and studio-produced.

 
   * low popularity tracks presented high accousticness, and low significant values for valence,
     * Suggests low popularity songs are primarily acoustic, slower songs with less rhythm and tempo.

2. Linear modelling provides high predictive precision, however displays low explanatory power (R^2 = 7%).
   * This suggests that popularity is primarily determined through non non-musical drivers, i.e marketing and trends.

3. Genre, as a category, masks features present when displayed amongst aggregated data
   * This implies that smaller samples with less genres allow for greater predictability, but poses the risk of skewed/biased data.

## License

This project is licensed under <a href="https://creativecommons.org/licenses/by/4.0/?ref=chooser-v1" target="_blank" rel="license noopener noreferrer" style="display:inline-block;">CC BY 4.0<img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/cc.svg?ref=chooser-v1" alt=""><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/by.svg?ref=chooser-v1" alt=""></a>

See `license` file for more details.

## Contacts

For queries or further information, please contact:

Name: Gianmarco Imola
Email: gian.imola2003@gmail.com
GitHub: https://github.com/G-Imola
