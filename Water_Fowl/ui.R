








library(shiny)
library(tidyverse)
library(lubridate)
library(caret)
library(DT)
library(doParallel)


# R Studio Layout guide (about halfway down) explains navbarPage() >>> https://shiny.rstudio.com/articles/layout-guide.html

fluidPage(
    navbarPage("Water Fowl Counts along the American North-East Coast",
    
      #tabPanel("About"), 
        # IN ABOUT:
        #     - Mention that you don't know if a blank count is a zero or missing so I treated it as zero to be on the 
        #       conservative side for the birds
        #     - Link to JSTOR
      
        # Mallards went from moderate density in 89-92, to great density in 93-97
        # This is more an exploration of some real data primarily through visualization
        ### This is not a replication of the study or representative of the study. I was not involved in the study
        # and I do not reflect the views of that study. This is personal growth and skill development. 
      
        # - Describe purpose of app, discuss data and source, purpose of each page, include a picture
      
      # tabPanel("Data Exploration", 
      #          
      #          column(6, # The radio button decides which variable to explore in the graph
      #                 radioButtons("DisplayGraph", "Choose the Variable to Explore",
      #                                  c("Sepcies" = 1, "Year" = 2, "State" = 3, "Stratum", "Time of Day" = 4, #"TimeOfDay", 
      #                                    "Wetland Habitat" = 5)),#"WetHab")),
      #                 #textOutput("tbCond"), 
      #                 
      #                 # This is only displayed for the Species selection
      #                 # Need the slider to remove values of count in the scatter plot based on Species
      #                 conditionalPanel(condition = "input.DisplayGraph == 1", 
      #                                  sliderInput("RangeUI", "Limit the Values for Number of Birds Seen", 
      #                                              min = 0, max = 400, value = c(1, 50)),
      #                                  
      #                                  # checkboxGroupInput("SpeciesUI", "Choose the Species",
      #                                  #                    c("American Black Duck" = "AmBlackDuck",
      #                                  #                      "Canadian Goose" = "CanadianGoose",
      #                                  #                      "Mallard" = "Mallard",
      #                                  #                      "Wood Duck" = "WoodDuck")),
      #                                  # textOutput("tbCond"),
      #                                  textOutput("range"),
      #                 ),
      #                 
      #                 submitButton("Graph"),
      #                 plotOutput("expGraph")),
      #          
      #         column(3, 
      #                radioButtons("TableGroupUI1", "First Variable Grouping",
      #                                   c("Species","Year", "State", "Stratum", "Time of Day" = "TimeOfDay",
      #                                     "Wetland Habitat" = "WetHab"))),
      #         column(3,
      #                radioButtons("TableGroupUI2", "Second Variable Grouping",
      #                             c("None", "Species","Year", "State", "Stratum", "Time of Day" = "TimeOfDay",
      #                               "Wetland Habitat" = "WetHab")),
      #                submitButton("Table"),
      #                textOutput("tbCond")),
      #                # textOutput("vars"),
      #         column(6,
      #                DTOutput("summary")), 
      #         ),
      
              # these should be used if you want to show only these vars...maybe use for histogram or bar graphs
                     # Put these in their own categories...choosing them should produce different things
                     
                     
                     # # Bar plot of species counted during what times of day
                     # selectInput("TODUI", "Time Of Day", 
                     #             c("Mid-Day" = 1, 
                     #               "Twilight" = 2, 
                     #                 "Not Sampled" = 3)),
                     #  
                     # selectInput("StratumUI", "Stratum/Habitat", 
                     #              c(unique(birds$Stratum))), 
                     #  
                     # radioButtons("WetHabUI", "Wetland Habitat", 
                     #               c("Yes", "No")),
                     #  
                     # radioButtons("HandfeedUI", "Handfeeder Present?",
                     #               c("Yes", "No")), )
              
               
        # Numerical and graphical summaries, change type of plot and type of summary reported, change variables and filter the 
        # the rows to change the data in the plots/summaries
        
        navbarMenu("Modelling",
                   
          tabPanel("Information on Supervised Learning Methods",
                   h4("Multiple Linear Regression, Regression Trees, and Random Forest Model"),
                   br(), 
                   h4("")
                   ),
          # Explain the three modeling approaches, inc the pros and cons of each
          # NEED mathJax
          
          tabPanel("Model Fitting",
                   sliderInput("split", "Choose the Percentage Of Total Observations in Training Set", 
                               min = 50, max = 90, value = 70), 
                   selectInput("predictors", "Predictors for Bird Count", 
                                names(birds), multiple = TRUE, selectize = FALSE), 
                   
                   textOutput("Preds"), 
                   submitButton("Model"),
                   
                   verbatimTextOutput("LMResults"),
                   plotOutput("RFResults")
                   
                   ), 

        # Will want to pass an argument and result in count of birds 
          tabPanel("Prediction",
                   selectInput("predSpecies", "Choose a Species",
                               unique(birds$Species)),
                   numericInput("predYear", "Choose a Year between 1993 through 2015",
                                min = 1993, max = 2015, value=2000),
                   selectInput("predState", "Pick an North-Eastern State",
                               unique(birds$State)),
                   selectInput("predStrat", "Pick a Stratum",
                                unique(birds$Stratum))
                   )
          # Select the values of the predictors and obtain a prediction for the response
        # Have Shiny throw an error if User tries to go under 1993, 2010, 2012. or over 2015
        ), 
      
      tabPanel("Data"
        # Scroll through data set, subset data set (rows and columns), save the subsetted data as a file
        )
))

