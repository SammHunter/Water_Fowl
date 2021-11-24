#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)
library(caret)

# R Studio Layout guide (about halfway down) explains navbarPage() >>> https://shiny.rstudio.com/articles/layout-guide.html

fluidPage(
    navbarPage("Water Fowl Counts along the American North-East Coast",
    
      tabPanel("About"), 
        # IN ABOUT:
        #     - Mention that you don't know if a blank count is a zero or missing so I treated it as zero to be on the 
        #       conservative side for the birds
        #     - Link to JSTOR
      
        # Mallards went from moderate density in 89-92, to great density in 93-97
        # This is more an exploration of some real data primarily through visualization
        ### This is not a replication of the study or representative of the study. I was not involved in the study
        # and I do not reflect the views of that study. This is personal growth and skill development. 
      
        # - Describe purpose of app, discuss data and source, purpose of each page, include a picture
      
      tabPanel("Data Exploration", 
               
               # Put these in their own categories...choosing them should produce different things
               checkboxGroupInput("SpeciesUI", "Choose the Species", 
                                  c("American Black Duck" = "AmBlackDuck", 
                                    "Canadian Goose" = "CanadianGoose", 
                                    "Mallard" = "Mallard", 
                                    "Wood Duck" = "WoodDuck")),
               
               # Bar plot of species counted during what times of day
               selectInput("TODUI", "Time Of Day", 
                           c("Mid-Day" = 1, 
                             "Twilight" = 2, 
                             "Not Sampled" = 3)),
               
               selectInput("StratumUI", "Stratum/Habitat", 
                           c(unique(birds$Stratum))), 
               
               radioButtons("WetHabUI", "Wetland Habitat", 
                            c("Yes", "No")),
               
               radioButtons("HandfeedUI", "Handfeeder Present?",
                            c("Yes", "No")), 
               
               submitButton("Quack")),
        # Numerical and graphical summaries, change type of plot and type of summary reported, change variables and filter the 
        # the rows to change the data in the plots/summaries
        
        navbarMenu("Modelling",
                   
          tabPanel("Information"
                   
                   ),
          # Explain the three modeling approaches, inc the pros and cons of each
          # NEED mathJax
          
          tabPanel("Model Fitting"), 
          # Split data into training and test, allowing the user to choose the proportion of data used in each
          # Need to be able to choose the variables used (Year, State, Stratum, TimeofDay, WetHab, Handfeed)
          # ALL MODELS NEED TO INCLUDE COUNT AND SPECIES??
          #### Don't change until button is pressed
          # Fit stats should be reported for each model
          # Models sb compared on test sets
          
        
        # Will want to pass an argument and result in count of birds 
          tabPanel("Prediction")
          # Select the values of the predictors and obtain a prediction for the response
        # Have Shiny throw an error if User tries to go under 1993, 2010, 2012. or over 2015
        ), 
      
      tabPanel("Data"
        # Scroll through data set, subset data set (rows and columns), save the subsetted data as a file
        )
))

