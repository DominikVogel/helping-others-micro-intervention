<!-- badges: start -->
[![Launch Rstudio Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/DominikVogel/helping-others-micro-intervention/binder-running?urlpath=rstudio)
<!-- badges: end -->

# The effects of making public service employees aware of their prosocial and societal impact

**Repository** for Vogel, D., & Willems, J. (2019). The effects of making public service employees aware of their prosocial and societal impact: A micro-intervention. *Journal of Public Administration Research and Theory*.

The **main repository** can be found here: https://doi.org/10.17605/OSF.IO/W97H4

The **codebook** for the data can be found here: https://dominikvogel.github.io/helping-others-micro-intervention/

A **long-term archived** version of the data and code is available at XXX

There is a **Code Ocean Capsule** for long-term computational reproducibility: XXX


# Reproducibility

## Using Code Ocean

You can easily reproduce our results using the free service of [Code Ocean][2]:

1. Open the paper's Code Ocean Capsule: 
2. Click *Reproducible Run*
3. Code Ocean runs a virtual environment and produces a PDF with the full paper including all results.

You can inspect the *R* code by clicking on the files in the left panel (see below for an explanation of the file structure).

## On your own machine

If you want to reproduce our results on your own computer you need *R* (we used version 3.5.3) and RStudio. 

1. Download the full OSF Storage of the Project (including `.here` and the folders `code` and `data`
2. Make sure to install all required packages (see `DESCRIPTION` file)
2. Open `code/Paper.Rmd` in RStudio.
3. Press `Knit` and than select `Knit to pdf_document2`
4. *R* runs all analyses and creates the full paper as a PDF document. 


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


  [1]: https://mybinder.org
  [2]: https://codeocean.com
