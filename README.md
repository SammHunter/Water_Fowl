# Water_Fowl

This repo is for Samantha Hunter's Water_Fowl Shiny App. This is mostly to explore and test the various features of Shiny, and also get some experience cleaning and working with real world data. That being said, no predictions from this project have any statistical merit. I retrieved H. W. Heusmann and John R. Sauer's survey data on the Atlantic Flyaway Atlantic Flyway Breeding Waterfowl from the [US Fish and Wildlife’s site](https://migbirdapps.fws.gov/mbdc/databases/afbws/afbws.asp). 


The packages used to complete this project are:  

*pacman
*shiny
*tidyverse
*lubridate
*caret
*randomForest
*gbm
*DT
*doParallel
*rJava
*RWeka

Install and access all packages:
`pckg <- c("shiny", "tidyverse", "lubridate", "caret", "randomForest", "gbm", "DT", "doParallel", "rJava", "RWeka")`

`lapply(pckg, library, character.only = TRUE)`

Render Code:
`shiny::runGitHub("sammhunter/Water_Fowl", ref="main")`