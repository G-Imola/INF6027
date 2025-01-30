# INF6027
 ### Investigating quantitative features in music

#analysis
## Introduction

Investigating quantitative features in music: Statistical analysis of popularity and its relationship to musical features


## Composite visualisations
 The dataset and code generate composite visualisations depicting the predictive power of models employed throughout analysis.
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

`link`

After cloning, verify that the active Git branch is set to **main:**

To check the branch, run the following command

`git branch`

You've done this correctly if the output shows ***main.**

If the branch is not set to main, you can switch to the **main:**
branch using the following command:

`git checkout main`

This ensures that you're working on the correct branch for the project.

 ### 3. Dataset Placement
Ensure that the dataset `music_data.csv` is placed in the root
directory of the cloned repository.

 ### 4. Execute the script
Open #link# in Rstudio, or your preferred IDE, and run the script to generate:
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
