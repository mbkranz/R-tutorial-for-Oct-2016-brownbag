# VCHP Brownbag R tutorial Oct. 5th 2016, Mike Kranz

### uses built in dataset provided by ggplot2 (mpg) 
### examples with "fake" datasets for applications to cog psych research
### imported cognitive training time series dataset focusing on longitudinal data (using all parts of data analysis workflow)

one of my goals was to try to be hipsteR with the tidyverse philosophy of data analysis (at least as much as I can)

## Outline of tutorial
1.) Brief intro to some of the relevant R basics
2.) R Studio orientation
3.) Data manipulation/wrangling (dplyr and tidyr packages)
4.) Data visualization (ggplot2 packages)
5.) Exploratory Data Modeling and Model Visualization (purrr and broom packages)
6.) Short example with a cognitive training dataset 

### A few things to look at if interested: 
http://r4ds.had.co.nz/: Hadley Wickam's intro to the tidyverse way of doing data science
https://www.r-bloggers.com/ articles
    ### nice intro to machine learning with glmnet package (not topic of this tutorial): 
        #https://www.r-bloggers.com/in-depth-introduction-to-machine-learning-in-15-hours-of-expert-videos/
vignettes for packages (link at beginning of package documentation)
a lot of google/stackoverflow have a number of solutions to specific data manipulation/visualization problems
### UCLA stats site provides code for comparison with other software tools (such as SPSS)
 for example, see http://www.ats.ucla.edu/stat/examples/alda.htm for Hierarchical Models (a little old but still informative)
