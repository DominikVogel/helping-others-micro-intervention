<!-- badges: start -->
[![Launch Rstudio Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/DominikVogel/helping-others-micro-intervention/binder-running?urlpath=rstudio) [![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT) [![](https://img.shields.io/badge/CodeOcean-Capsule-green.svg)](https://doi.org/10.24433/CO.2463564.v1) [![Zenodo](https://img.shields.io/badge/Zenodo-long--time%20archive-informational)](https://zenodo.org/badge/latestdoi/223441260)
<!-- badges: end -->

# The effects of making public service employees aware of their prosocial and societal impact

**Repository** for Vogel, D., & Willems, J. (2019). The effects of making public service employees aware of their prosocial and societal impact: A micro-intervention. *Journal of Public Administration Research and Theory*.

The **main repository** can be found here: https://doi.org/10.17605/OSF.IO/W97H4

The **codebook** for the data can be found here: https://dominikvogel.github.io/helping-others-micro-intervention/

A **long-term archived** version of the data and code is available at https://doi.org/10.5281/zenodo.3570986

There is a **Code Ocean Capsule** for long-term computational reproducibility: https://doi.org/10.24433/CO.2463564.v1. See below for more information.

This **GitHub repository** provides another way to computationally reproduce the results using [Binder][1]. See below for more information.


# Reproducibility

## Using Binder

You can reproduce our results and even test how changes in the code affect the results by using the [Binder][3] environment of the paper:

1. Click on the "Launch Binder" badge
2. Wait for the server to be started
3. Open `code/Paper.Rmd` in the right panel
4. Click on `Knit` -> `Knit to pdf_document2`
5. Wait for a complete reproduction of the paper with all results.

## Using Code Ocean

You can easily reproduce our results using the free service of [Code Ocean][2]:

1. Open the paper's Code Ocean Capsule: https://doi.org/10.24433/CO.2463564.v1
2. Log-in to Code Ocean
3. Click *Re-Run*
4. Code Ocean runs a virtual environment and produces a PDF with the full paper including all results.


You can inspect the *R* code by clicking on the files in the left panel (see below for an explanation of the file structure).

## On your own machine (using your own *R* installation)

If you want to reproduce our results on your own computer you need *R* (we used version 3.5.3) and RStudio. 

1. Download the `.here` file and the folders `code` and `data`
2. Make sure that you installed all required packages (see below for a list of required packages)
3. Open `code/Paper.Rmd` in RStudio.
4. Press `Knit` and than select `Knit to pdf_document2`
5. *R* runs all analyses and creates the full paper as a PDF document. 


## On your own machine (using Docker)

1. Install Docker Community Edition
2. Download the `Docker-Capsule.zip` file from the [OSF Project Repository][4] and extract it
3. Open a Terminal and navigate to the extracted folder
4. Execute the following command: 
    ```shell
    docker load --input micro-intervention.tar
    docker run --rm \
    --workdir /code \
    --volume "$PWD/data":/data \
    --volume "$PWD/code":/code \
    --volume "$PWD/results":/results \
    micro-intervention run
    ```

# File structure

The repository consists of some files in the root folder and two folders. 

* `.here` is an empty file that helps the `here` package to set the right working directory in *R*.
* `/data` contains the data of the paper in three CSV files:
    * `Study1_public.csv`: Data for Study 1
    * `Study2_public.csv`: Data for Study 2
    * `Study3_public.csv`: Data for Study 3
* `/code` contains all scripts and files necessary to reproduce the results:
    * `Paper.Rmd`: main file containing the text of the paper and loads all analyses. 
    * `Paper_functions.R`: Custom functions
    * `Paper_s1.R`: Analysis code for Study 1
    * `Paper_s2.R`: Analysis code for Study 2
    * `Paper_s3.R`: Analysis code for Study 3
    * `Paper_BF.R`: Code to calculate Bayes factors
    * `Paper_forestplot_prosocial.R`: Code to create left part of Figure 5
    * `Paper_forestplot_societal.R`: Code to create right part of Figure 5
    * `Bibliography.bib`: References used in the paper (in BibTeX format)
    * `chicago-author-date.csl`: Defines references style
    * `latex_template.tex`: LaTeX template for the PDF document
    * `CONSORT.pdf`: CONSORT flow chart of Study 1 for the Appendix
    * `CONSORT2.pdf`: CONSORT flow chart of Study 2 for the Appendix
    * `CONSORT3.pdf`: CONSORT flow chart of Study 3 for the Appendix
    * `forestplot_combined.pdf`: Figure 5 (merge of figures created by `Paper_forestplot_prosocial.R` and `Paper_forestplot_societal.R`



# Required R packages

* apaTables
* BayesFactor
* bookdown
* broman
* cowplot (<= 0.9.4)
* dplyr
* effsize
* ez
* forestplot
* ggsignif
* here
* knitr
*  MBESS
*  nlme
*  psych
*  rlang
*  rmarkdown
*  sjstats
*  stargazer
*  stats
*  tibble
*  tidyverse
*  weights
*  xtable

It is advised to use `R` version 3.5.3 with the `checkpoint` package and set the date to 2019-04-01 to ensure that the package versions are comparable:

```R
library(checkpoint)
checkpoint("2019-04-01")
```

  [1]: https://mybinder.org
  [2]: https://codeocean.com
  [3]: https://mybinder.org
  [4]: https://doi.org/10.17605/OSF.IO/W97H4
