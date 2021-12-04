library(shiny)
library(tidyverse)
library(lubridate)
library(caret)
library(gbm)
library(randomForest)
library(DT)
library(doParallel)
# Packages for the Model Tree regression
library(rJava)
#install.packages("RWeka")
library(RWeka)


#############################################
############# Data Prep #####################
#############################################

birds <- fowls %>% 
  mutate( 
    # Combining the old goose counts with the new goose counts, 
    # if the new count exists, then that is what we will use
    CanadianGoose = if_else(!is.na(CAGO_TIBN), CAGO_TIBN, CAGO_TIB), 
    WETHAB = (if_else(WETHAB == "Y", "Yes", "No")), 
    HANDFEED = if_else(HANDFEED == "Y", "Yes", "No"), 
    CKTYPE = if_else(CKTYPE == 1, "Midday", 
                  if_else(CKTYPE == 2, "Twilight", "Not Sampled"))) %>%
  
  rename(Year = YEAR, State = STATE, TimeOfDay = CKTYPE, 
         WetHab = WETHAB, Handfeed = HANDFEED,
         Plot = PLOT, Date = DATE, Stratum = STRATUM, 
         Mallard = MALL_TIB, AmBlackDuck = ABDU_TIB, 
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

# These are just records where birds were seen, that have a date recorded, and fixing the date
bird_count <- birds %>% filter(Count != 0) %>% 
  mutate(Species = as.factor(Species), State = as.factor(State),
         Stratum = as.factor(Stratum), WetHab = as.factor(WetHab),
         Handfeed = as.factor(Handfeed))
# Over 80% of the records are where no birds were seen so I removed those records
bird_count <- bird_count %>% filter(-is.na(Date)|Date != "1900-01-01")
bird_count[bird_count == "1930-04-20"] <- as.Date("2003-04-20")


#############################################
################ UI     #####################
#############################################

fluidPage(
  navbarPage("Water Fowl Counts along the American North-East Coast",
             
#########################
####### ABOUT ###########
#########################
             tabPanel("About",
                      h1("The Data"), 
                      h2("Variables"), 
                      p("What do they stand for? What are the limitations?"), 
                      h1("Sources"), 
                      p("JSTOR Link")),

#########################
### DATA EXPLORATION ####
#########################
             tabPanel("Data Exploration",

                      column(6, # The radio button decides which variable to explore in the graph
                             radioButtons("DisplayGraph", "Choose the Variable to Explore",
                                          c("Sepcies" = 1, "Year" = 2, "State" = 3, "Stratum" = 4, "Time of Day" = 5,
                                            "Wetland Habitat" = 6, "Handfeed" = 7, "Count" = 8)),

                             # This is only displayed for the Species selection
                             # Need the slider to remove values of count in the scatter plot based on Species
                             conditionalPanel(condition = "input.DisplayGraph == 1",
                                              sliderInput("RangeUI", "Limit the Values for Number of Birds Seen",
                                                          min = 1, max = 400, value = c(1, 50)),

                                              radioButtons("de_spec_spec", "Choose the Species",
                                                           c("All" = 1,
                                                             "American Black Duck" = "AmBlackDuck",
                                                             "Canadian Goose" = "CanadianGoose",
                                                             "Mallard" = "Mallard",
                                                             "Wood Duck" = "WoodDuck")),
                                              renderText("Spec"),
                                              plotOutput("expGraph")),

                             # Can add option to just see one species at a time
                             conditionalPanel(condition = "input.DisplayGraph == 2",

                                              radioButtons("de_year_spec", "Choose the Species",
                                                           c("All" = 1,
                                                             "American Black Duck" = "AmBlackDuck",
                                                             "Canadian Goose" = "CanadianGoose",
                                                             "Mallard" = "Mallard",
                                                             "Wood Duck" = "WoodDuck")),

                                              plotOutput("yearHist")),

                             conditionalPanel(condition = "input.DisplayGraph == 3",
                                              plotOutput("stateHist")),

                             conditionalPanel(condition = "input.DisplayGraph == 4",
                                              plotOutput("stratumHist")),
                                              # put map of the stratum here ),

                             conditionalPanel(condition = "input.DisplayGraph == 5",
                                              tableOutput("circTOD")),

                             conditionalPanel(condition = "input.DisplayGraph == 6",
                                              plotOutput("circWetHab")),

                             conditionalPanel(condition = "input.DisplayGraph == 7",
                                              plotOutput("hfGraph"),
                                              tableOutput("hf")),

                             conditionalPanel(condition = "input.DisplayGraph == 8",
                                              plotOutput("countGraph")),

                             actionButton("Graph", "Graph")),

                      column(3,
                             checkboxGroupInput("tg", "Variable Grouping",
                                                c("Species","Year", "State", "Stratum", "Time of Day" = "TimeOfDay",
                                                  "Wetland Habitat" = "WetHab")),
                             actionButton("Table", "Table")
                      ),

                      column(6,
                             DTOutput("summary"))
             ),

#########################
####### MODELLING #######
#########################

             navbarMenu("Modelling",
                        ###############
                        ### Information
                        ###############
                        tabPanel("Information on Supervised Learning Methods",
                                 h1("Information on Supervised Learning Methods"),
                                 h2("General Information"), 
                                 br(), 
                                 p("Within the model fitting tab you should first make all your choices and then 
                                   press the ModelGO button to generate the models. This will take a varying amount of time
                                   based on how many and how complicated the models run are. The more selections you make, the 
                                   longer the models take to complete.", style = "font-family: 'times'"),
                                 p("Each model is created using cross validation on a training set. The size of the training set
                                   is chosen using the slider. All models will be run with the same predictors and the same number
                                   of folds.", style = "font-family: 'times'"), 
                                 br(), 
                                 h3("Linear Modelling"), 
                                 uiOutput("lmFormula"), 
                                 h4("Pros and Cons"),
                                 
                                 h3("Boosted Trees"), 
                                 p(), 
                                 h4("Pros and Cons"), 
                                 
                                 h3("Random Forests"), 
                                 p(), 
                                 h4("Pros and Cons")
                                 
                                 #columns(8, 
                                 #)
                                 
                        ),
                        ###############
                        ####### Fitting
                        ###############
                        tabPanel("Model Fitting",
                                 
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
                                                    multiple = TRUE, selectize = FALSE)),
                                         selectInput("nminobs", "Minimum Number of Observations in Node",
                                                    c(100, 500, 750, 1000), multiple = TRUE, selectize = FALSE),

                                 column(4, "Parameter Tuning for Random Forest Models",
                                        selectInput("mtry_num", "mTry", c(1:9),
                                                    multiple = TRUE, selectize = FALSE)),

                                 actionButton("Model", "ModelGO"),
                                 
                                 verbatimTextOutput("LMResults"),
                                 verbatimTextOutput("BTPlot"),
                                 verbatimTextOutput("RFResults"),
                                 verbatimTextOutput("lmRMSE"), 
                                 verbatimTextOutput("btRMSE"), 
                                 verbatimTextOutput("rfRMSE")), 
                        
                        # Root Mean Square 
                                 
                                 # Testing the Models on the unseen data, we'll make a comparison on the RMSEs 
                        
                        tabPanel("Prediction",
                                 ###############
                                 #### Prediction
                                 ###############
                                 checkboxGroupInput("predModel", "Which Model Would You Like to Run", 
                                                    c("Linear Model"=1, "Boosted Trees"=2, "Random Forest"=3)), 
                                 selectInput("predSpecies", "Choose a Species",
                                             unique(bird_count$Species)),
                                 numericInput("predYear", "Choose a Year between 1993 through 2015",
                                              min = 1993, max = 2015, value=2000),
                                 selectInput("predState", "Pick an North-Eastern State",
                                             unique(bird_count$State)),
                                 selectInput("predStrat", "Pick a Stratum",
                                             unique(bird_count$Stratum)), 
                                 radioButtons("predHab", "Is there a Wetlands habitat nearby?", 
                                              c("Yes", "No")),
                                 actionButton("Predict", "Predict"), 
                                 verbatimTextOutput("lmPredCount"),
                                 verbatimTextOutput("btPredCount"),
                                 verbatimTextOutput("rfPredCount")
                                 # Ignore Plot, Date, TimeofDay, Handfeed bc they are too homogenous
                        )
                        # Select the values of the predictors and obtain a prediction for the response
                        # Have Shiny throw an error if User tries to go under 1993, 2010, 2012. or over 2015
             ), 
             
             tabPanel("Data", 
                      ###############
                      ####### DATA DL
                      ###############
                      selectInput("dataVars", "Variables to Display",
                                  names(bird_count), multiple = TRUE, selectize = TRUE),
                      # selectInput("dataSpecies", "Species",
                      #             unique(birds$Species), multiple = TRUE, selectize = TRUE),
                      # numericInput("dataYear", "Years",
                      #              min = 1993, max = 2015, value=2000),
                      # selectInput("dataState", "States",
                      #             unique(birds$State), multiple = TRUE, selectize = TRUE),
                      # selectInput("dataStrat", "Stratums",
                      #             unique(birds$Stratum), multiple = TRUE, selectize = TRUE), 
                      # selectInput("dataTOD", "Time of Day",
                      #             unique(birds$TimeOfDay), multiple = TRUE, selectize = TRUE), 
                      # selectInput("dataHab", "Wetlands habitat nearby?", 
                      #              c("Yes", "No"), multiple = TRUE, selectize = TRUE),
                      # selectInput("dataHF", "Handfeeder nearby?", 
                      #              c("Yes", "No"), multiple = TRUE, selectize = TRUE),
                      # 
                      downloadButton(outputId = "DuckData", 
                                     label = "Download CSV"), 
                      DTOutput("dlTable") 
                      )
  ))

