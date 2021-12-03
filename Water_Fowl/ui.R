library(shiny)
library(tidyverse)
library(lubridate)
library(caret)
library(DT)
library(doParallel)
# Packages for the Model Tree regression
library(rJava)
#install.packages("RWeka")
library(RWeka)



fowls$STRATUM = as.character(fowls$STRATUM)

birds <- fowls %>% 
  mutate( 
    # Combining the old goose counts with the new goose counts, 
    # if the new count exists, then that is what we will use, otherwise we'll use the old count...
    # not the best practice but good enough for now
    CanadianGoose = if_else(!is.na(CAGO_TIBN), CAGO_TIBN, CAGO_TIB)) %>%
  
  rename(Year = YEAR, State = STATE, TimeOfDay = CKTYPE, Plot = PLOT, Date = DATE,
         Stratum = STRATUM, WetHab = WETHAB, Handfeed = HANDFEED, 
         Mallard = MALL_TIB, 
         AmBlackDuck = ABDU_TIB, 
         WoodDuck = WODU_TIB) %>%
  
  # Removing extra columns - Have the TIBN and TIB data else where
  # The pairs of birds seems to have been calculated based on the number of birds seen
  # so I am just using the birds variables -> Maybe come back to automate for Pairs
  select(!CAGO_TIB, -CAGO_TIBN, -ends_with("_TIP")) %>%
  
  # Pivoting the table so each observation denotes the count of one species
  # (making it easier to filter)
  pivot_longer(cols = 9:12, names_to = "Species", values_to = "Count") %>%
  
  # Only keeping explicit zeroes. The data was square so I assume those were introduced
  # by the structure as there are plenty of zeroes as well
  filter(!is.na(Count))


# Fixing the dates to actual date instead of character strings
birds$Date <- mdy(birds$Date)
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
                      
                      column(6, # The radio button decides which variable to explore in the graph
                             radioButtons("DisplayGraph", "Choose the Variable to Explore",
                                          c("Sepcies" = 1, "Year" = 2, "State" = 3, "Stratum" = 4, "Time of Day" = 5,
                                            "Wetland Habitat" = 6, "Handfeed" = 7, "Count" = 8)), 
                             
                             # This is only displayed for the Species selection
                             # Need the slider to remove values of count in the scatter plot based on Species
                             conditionalPanel(condition = "input.DisplayGraph == 1",
                                              sliderInput("RangeUI", "Limit the Values for Number of Birds Seen",
                                                          min = 0, max = 400, value = c(1, 50)),
                                              
                                              radioButtons("SpeciesUI", "Choose the Species",
                                                           c("All" = 1, 
                                                             "American Black Duck" = "AmBlackDuck",
                                                             "Canadian Goose" = "CanadianGoose",
                                                             "Mallard" = "Mallard",
                                                             "Wood Duck" = "WoodDuck")),
                                              renderText("Spec"), 
                                              plotOutput("expGraph")),
                             
                             # Can add option to just see one species at a time
                             conditionalPanel(condition = "input.DisplayGraph == 2",
                                              plotOutput("yearHist")),
                             conditionalPanel(condition = "input.DisplayGraph == 3", 
                                              plotOutput("stateHist")), 
                             conditionalPanel(condition = "input.DisplayGraph == 4", 
                                              plotOutput("stratumHist")),
                             
                             conditionalPanel(condition = "input.DisplayGraph == 5", 
                                              plotOutput("circTOD")), 
                             conditionalPanel(condition = "input.DisplayGraph == 6", 
                                              plotOutput("circWetHab")), 
                             conditionalPanel(condition = "input.DisplayGraph == 7", 
                                              plotOutput("hfGraph")), 
                             conditionalPanel(condition = "input.DisplayGraph == 8", 
                                              plotOutput("countGraph")), 
                             
                             submitButton("Graph", "Graph")),
                      
                      column(3,
                             checkboxGroupInput("tg", "First Variable Grouping",
                                                c("Species","Year", "State", "Stratum", "Time of Day" = "TimeOfDay",
                                                  "Wetland Habitat" = "WetHab")), 
                             submitButton("Table", "Table")
                      ),
                      
                      column(6,
                             DTOutput("summary"))
             ),
             
             navbarMenu("Modelling",
                        
                        tabPanel("Information on Supervised Learning Methods",
                                 h4("Multiple Linear Regression, Regression Trees, and Random Forest Model"),
                                 br(), 
                                 h4("")
                        ),
                        # Explain the three modeling approaches, inc the pros and cons of each
                        # NEED mathJax
                        
                        tabPanel("Model Fitting",
                                 uiOutput("ntrees_num"),
                                 uiOutput("str_ntrees_num"),
                                 
                                 column(4, "Modelling Choices for all Models", 
                                        sliderInput("split", "Choose the Percentage Of Total Observations in Training Set",
                                                    min = 50, max = 90, value = 70), 
                                        selectInput("predictors", "Predictors for Bird Count", 
                                                   names(birds[,c(1:3, 7, 9)]), multiple = TRUE, selectize = FALSE), 
                                        numericInput("cv", "Set the Number of Folds Used in Cross-Validation", 
                                              min = 2, max = 10, value = 5)),

                                 column(4, "Parameter Tuning for Boosted Tree Models",
                                        selectInput("ntrees", "Number of Trees to Evaluate",
                                                    c(25, 50, 75, 100, 125, 150, 175, 200, 250, 300, 350, 400, 450, 500),
                                                    multiple = TRUE, selectize = FALSE),
                                        selectInput("interactiondepth", "Max Interaction Depth Available to the Trees",
                                                    c(1:10), multiple = TRUE, selectize = FALSE),
                                        selectInput("shrink", "Shrinkage", c(.001, .01, .1),
                                                    multiple = TRUE, selectize = FALSE),
                                        selectInput("nminobs", "Minimum Number of Observations in Node",
                                                    c(100, 500, 750, 1000), multiple = TRUE, selectize = FALSE)),

                                 column(4, "Parameter Tuning for Random Forest Models",
                                        selectInput("mtry_num", "mTry", c(1:9),
                                                    multiple = TRUE, selectize = FALSE)),
                                 
                                 textOutput("Preds"), 
                                 actionButton("Model", "ModelGO"),
                                 
                                 verbatimTextOutput("LMResults"),
                                 plotOutput("BTResults"),
                                 plotOutput("RFResults")),
                        
                        # Root Mean Square 
                                 
                                 # Testing the Models on the unseen data, we'll make a comparison on the RMSEs 
                        
                        # Will want to pass an argument and result in count of birds 
                        tabPanel("Prediction",
                                 selectInput("predSpecies", "Choose a Species",
                                             unique(birds$Species)),
                                 numericInput("predYear", "Choose a Year between 1993 through 2015",
                                              min = 1993, max = 2015, value=2000),
                                 selectInput("predState", "Pick an North-Eastern State",
                                             unique(birds$State)),
                                 selectInput("predStrat", "Pick a Stratum",
                                             unique(birds$Stratum)), 
                                 radioButtons("predHab", "Is there a Wetlands habitat nearby?", 
                                              c("Yes" = "Y", "No" = "N"))
                                 # Ignore Plot, Date, TimeofDay, Handfeed bc they are too homogenous
                        )
                        # Select the values of the predictors and obtain a prediction for the response
                        # Have Shiny throw an error if User tries to go under 1993, 2010, 2012. or over 2015
             ), 
             
             tabPanel("Data", 
                      # Scroll through data set, subset data set (rows and columns), save the subsetted data as a file
                      
                      downloadButton("downloadData", "Download"), 
                      DTOutput("dlTable")
             )
  ))

