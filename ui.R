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
#fowls <- read.csv("duck_data.csv")

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




# These are just records where birds were seen, that have a date recorded, and fixing the date
bird_count <- birds %>% filter(Count != 0) %>% 
  mutate(Species = as.factor(Species), State = as.factor(State),
         Stratum = as.factor(Stratum), WetHab = as.factor(WetHab),
         Handfeed = as.factor(Handfeed))

# # Fixing the dates to actual date instead of character strings
# birds$Date <- mdy(birds$Date)
# # Over 80% of the records are where no birds were seen so I removed those records
# bird_count <- bird_count %>% filter(-is.na(Date)|Date != "1900-01-01")
# bird_count[bird_count == "1930-04-20"] <- as.Date("2003-04-20")


#############################################
################ UI     #####################
#############################################

fluidPage(
  navbarPage("Water Fowl Counts along the American North-East Coast",
             
#########################
####### ABOUT ###########
#########################
             tabPanel("About",
                      column(6,
                             h1("Acknowledgement"),
                             p("I would first like that like to thank H. W. Heusmann and John R. Sauer for publishing their
                               data on the US Fish and Wildlife’s site, and then I would like to apologize to them for abusing
                               their data. Because the point of this project was to really get some good practice with R Shiny 
                               and review the supervised learning models, I did several things that a statistician should never do
                               (which is described more in Data section below). In my defense, I didn’t want my app to 
                               boring and I don't know much about migratory birds of the Northeastern states.
                               If you are interested in the actual conclusions from this survey, please read",
                               a("The Northeastern States' Waterfowl Breeding Population Survey",
                                 href = "http://proxying.lib.ncsu.edu/index.php?url=https://www.jstor.org/stable/3783692"),
                               style = "font-family: 'times'; font-si18pt"),
                             br()),
                      column(6,
                             h1("Purpose of this App"),
                             p("As mentioned, the purpose of this app is to explore the features of R's Shiny application using
                               a real data set. I wanted something I could show to nearly adult and they would maybe spend at 
                               least one or two minutes poking around and maybe learn something about the basics of these models (or ducks).
                               We are using a fairly straight-foward data set from USFW about the counts of migratory birds in the
                               Northeastern coast. The",
                               a("Atlantic Flyway Breeding Waterfowl Survey Homepage", 
                                 href = "https://migbirdapps.fws.gov/mbdc/databases/afbws/afbws.asp"), 
                               "has tabs that both briefly describe the data as well as the ability to retrieve the data. The 
                               highlights are that this survey, which has data published from 1993 to 2015 was 'designed
                               to primarily estimate breeding population size of mallards, black ducks, wood ducks, and Canada geese', 
                               but other bird counts were recorded. I only retrieved data on the first four ducks listed.", 
                               style = "font-family: 'times'; font-si18pt"),
                             br()),
                      column(6,
                      h1("The Data"),
                      p("I downloaded the Counts for all available years (1993 - 2013) and all states. The birds 
                        that I chose to investigate are the American black duck, the Candaian goose, the Mallard, and the 
                        Wood Duck. Count are our numeric response variable. The other variables are Stratum, which
                        is a numeric variable that informs the reader of the type of habitat the count was taken in. 
                        Figure 1 is a map from the published paper that shows where the stratum borders are found. 
                        Time of day indicates when the bird was seen. Wethab and Handfeed indicate whethere a wetland
                        habitat or handfeeder is nearby, although I did not see nearby defined within the Heusmann's and 
                        Sauer's paper."
                        ,style = "font-family: 'times'; font-si18pt"),
                        img(src = "Stratum_Map.png", height = 400, width = 372), 
                      p("I excluded all counts of 0 from that data because those points made up over 80% of the data
                        and without them we still have over 20k observations. I didn't want to mostly predict that we wouldn't
                        see birds because I was concerned that someone would think the predict function is incorrect. Also, I'm
                        not sure how useful that information is. After excluding these 0 counts, I also decided to exclude the
                        Date and Plot information. Date was excluded becaues all but four observations take place in April or
                        May. Plot was excluded because I believe it is an indentifying variable, but not one that should have
                        a bearing on the bird counts.",
                        style = "font-family: 'times'; font-si18pt"), 
                      br()),
                      column(6, 
                      h1("The Pages"),
                      h3("Data Exploration"), 
                      p("The Data Exploration page allows the user to explore all of the predictors as well as Count. 
                        Depending on the variable chosen, a graph or table may appear, as well as possibly more 
                        ways to investigate the data or a note about the variable. You can change which variable is
                        shown by clicking on the `Graph` button.",
                        style = "font-family: 'times'; font-si18pt"), 
                      p("There is also a table provided that gives the the mean, median, range, and count of  the
                        data by default. If a variable is chosen, the data is grouped and new mean, median, 
                        ranges, and count are calcuated when the 'Table' button is pressed.",
                        style = "font-family: 'times'; font-si18pt"), 
                      br(), 
                      h3("Modelling"), 
                      p("Within the Modelling page there are three tabs - Information, Model Fitting, and Prediction. 
                        Information has general information about the basic of the supervised models we fit to our
                        data. The Model Fitting tab allows the user to decide on the setting and variables used to fit
                        a multiple linear regression models, boosted tree models, and random forest models. The Prediction
                        tab allows the user to get a Count based on the values provided. While the previous tab allowed the
                        user to select attributes about the model, the user must provide all the inputs requested of them
                        before a prediction can be output.",
                        style = "font-family: 'times'; font-si18pt"),
                      br(), 
                      h3("Data"), 
                      p("Finally, we have the Data page where the user can select which variables they want to see
                        and what values they want for those variables. If the user desires, they can press hte
                        'Download CSV' button and download the subsetted data set. It will appear as 'Filtered_Duck.CSV'.",
                        style = "font-family: 'times'; font-si18pt"))),

#########################
### DATA EXPLORATION ####
#########################
             tabPanel("Data Exploration",
                      column(6,
                      column(3,
                      # The radio button decides which variable to explore in the graph
                      radioButtons("DisplayGraph", "Choose the Variable to Explore",
                                   c("Sepcies" = 1, "Year" = 2, "State" = 3, "Stratum" = 4, "Time of Day" = 5,
                                     "Wetland Habitat" = 6, "Handfeed" = 7, "Count" = 8))),
                      column(3,
                      conditionalPanel(condition = "input.DisplayGraph == 1",
                                       sliderInput("RangeUI", "Limit the Values for Number of Birds Seen",
                                                   min = 1, max = 400, value = c(1, 50)),
                                       
                                       radioButtons("de_spec_spec", "Choose the Species",
                                                    c("All" = 1, "American Black Duck" = "AmBlackDuck",
                                                      "Canadian Goose" = "CanadianGoose", "Mallard" = "Mallard",
                                                      "Wood Duck" = "WoodDuck")))),
                      column(3,
                      conditionalPanel(condition = "input.DisplayGraph == 2",
                                       radioButtons("de_year_spec", "Choose the Species",
                                                    c("All" = 1, "American Black Duck" = "AmBlackDuck",
                                                      "Canadian Goose" = "CanadianGoose", "Mallard" = "Mallard",
                                                      "Wood Duck" = "WoodDuck"))))),
                      column(6,
                      checkboxGroupInput("tg", "Variable Grouping for Table",
                                         c("Species","Year", "State", "Stratum", "Time of Day" = "TimeOfDay",
                                           "Wetland Habitat" = "WetHab")),
                      actionButton("Graph", "Graph"),
                      actionButton("Table", "Table")),
                      
                      column(12, 
                      conditionalPanel(condition = "input.DisplayGraph == 1",
                                       plotOutput("expGraph")),
                      conditionalPanel(condition = "input.DisplayGraph == 2",
                                       plotOutput("yearHist")),
                      conditionalPanel(condition = "input.DisplayGraph == 3",
                                       plotOutput("stateHist")),
                      conditionalPanel(condition = "input.DisplayGraph == 4",
                                       plotOutput("stratumHist"), 
                                       img(src = "Stratum_Map.png", height = 400, width = 372)),
                      conditionalPanel(condition = "input.DisplayGraph == 5",
                                       tableOutput("circTOD")),
                      conditionalPanel(condition = "input.DisplayGraph == 6",
                                       plotOutput("circWetHab")),
                      conditionalPanel(condition = "input.DisplayGraph == 7",
                                       plotOutput("hfGraph"),
                                       tableOutput("hf")),
                      conditionalPanel(condition = "input.DisplayGraph == 8",
                                       plotOutput("countGraph"))),
                      br(),
                      br(), 
                      column(12,
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
                                 h3("Data Cleaning and Manipulation"),
                                 p("First, let me explain how the data was cleaned and transformed. I first combined those 
                                   counts for Canadian geese. There is an old counts column, from 1993 - 2002, and a new counts 
                                   column from 2003 onward.", style = "font-family: 'times'; font-si18pt"),
                                 p("Per USFW,", style = "font-family: 'times'; font-si18pt"),
                                 
                                 p("'...in 2003, the formula for calculating total indicated birds (TIB) for Canada geese was changed. 
                                   Previously, TIBs for Canada geese were calculated as (2 × pairs) + singles + grouped birds. 
                                   ...the formula was changed to: TIB = 2 × (pairs + singles) + grouped birds. As raw data are not currently
                                   available to recalculate TIBs for Canada geese for the previous years, population estimates based on both
                                   formulae will be presented for at least 5 years to allow comparison. The Division of Migratory Bird Management, 
                                   USFWS, is responsible for performing data analyses.'", style = "font-family: 'bookman'; font-si18pt"),
                                 
                                 p("I just used the new calculated data because USFW said it was better, but I mostly likely drew at least some
                                   error into my models by doing so. If not, I certainly did when I decided to to exclude all the zero counts
                                   of the birds. I'm not an ornithologist so anything besides the most basic anaylsis is beyond me, but I did
                                   want something that changed when I input new things. Finally, I changed the data from a wide format into a long
                                   format (so from Counts for several birds per observation into one count per observation). Finally, for
                                   the modelling tab, I don't allow the user to run models using those variables that do not have enough 
                                   observations in each category to model those predictors. Think about it, if you don't have any observations
                                   involving that category, there is no data to model. In this case, those variables are TimeOfDay and Handfeed.", 
                                   style = "font-family: 'times'; font-si18pt"),
                                 br(), 
                                 p("Within the model fitting tab you should first make all your choices and then 
                                   press the 'Model' button to generate the models. This will take a varying amount of time
                                   based on how many and how complicated the models run are. The more selections you make, the 
                                   longer the models take to complete.",
                                   style = "font-family: 'times'; font-si18pt"),
                                 h3("The Models"), 
                                 p("Each model is created using k-fold cross validation on a training set. The size of the training set
                                   is chosen by the user using the slider, but the default is a 70/30 split favoring the training set. 
                                   All models will be trained with the same predictors data, same number of cross validation folds and then
                                   tested on the same test set. In 
                                   cross validation, k represents the number of equal sized folds that the data is split into. Each time, 
                                   the model is trained on one the folds and then tested against the rest of the folds. Once the data has 
                                   been trained on each fold, the error is then averaged across the models. We want the model with the smallest
                                   error. This method often gives us an accurate test error rate. We often want to use 5 or 10 folds.",
                                   style = "font-family: 'times'; font-si18pt"), 
                                 p("We will also be calculating the root mean square error (RMSE) for each model. The RMSE is
                                   one method that we can use to compare how good each of these models are. While it may be 
                                   informative to see the RMSE from the cross-validation training results, we really
                                   only care about how the models perform on data that they haven't seen yet. We will allow
                                   the models to predict the count of birds based on the predictors from the training set. Then
                                   we compare (subtract) those expected Counts from the actual counts. We square these differences
                                   to calculate the distance of the points. Finally, We will average the that squared valued 
                                   across all the points and we will have our RMSE.", 
                                   style = "font-family: 'times'; font-si18pt"), 
                                 uiOutput("rmseFormula"), 
                                 br(), 
                                 
                                 column(4,
                                 h3("Linear Modelling"), 
                                 uiOutput("lmFormula"), 
                                 p("$Y_i$ is our response variable. In our case, this is counts. The $//beta$'s represent
                                   the coefficients of the predictors. An easy example I think of the price of a house based on the 
                                   number of square feet it is. I kept our the users of this data set to a model with neither interactions
                                   nor higher order terms. Interactions can be thought of those predictors who may have a strong correlation
                                   together on the response variable. Continuing with our house price example, it could be that houses
                                   whose bedrooms take up too much of total space of the house sell for less, so we may want to multiply
                                   those together when we predict the price of a house. Higher order terms are those that we may square, cube, 
                                   or even further. The common example I can think of is chance of flooding by how much it rains in volume.
                                   I didn't include either of those here because I expected we would need a flexible model to account for
                                   the amout of data we have and the other two, more flexible models already take a very long to calcuate.", 
                                   style = "font-family: 'times'; font-si18pt"), 
                                 
                                 p("The linear model is great because it is easy to understand, and very applicable. Unfortunately, 
                                   outliers can have a huge affect on the model and as we saw in our exploration, we seem to have a lot
                                   of outliers. The linear model we use assumes also assumes independence between predictors. That just means
                                   that the predictors don't affect each other, but because we mostly have character predictors I have no idea
                                   if that is true.", 
                                   style = "font-family: 'times'; font-si18pt")),
                        
                                 
                                 column(4, 
                                 h3("Boosted Trees"), 
                                 p("This model type and Random Forest are quite similar. They are both more flexible than 
                                   the linear regression model. They both are tree models. Normally tree models are great because
                                   they are also very easy to interpret, but, ironically, that is not true in either of these models' cases. 
                                   The procedure for theboosted tree approach is one where the trees are grown one sequentially. Each tree is a 
                                   modified version of the one before and the predictions are updated as the tree is grown. The user gets the 
                                   most choices for this one.", 
                                   style = "font-family: 'times'; font-si18pt"),
                                 
                                   p("'n.trees' = How many trees the program will look at before stopping. The higher the number, the more trees
                                     are looked at. We want this number to be large so we can catch when the tree stops changing (converges).
                                   'interaction.depth'= Interaction depth is how many models can interact withe each other. 
                                   The more you greater this number is, the more flexible the model is.
                                   'n.minobsinnode' = the minimum number of observation each tree must have at each end point. The higher this number,
                                   the more flexible the trees. 
                                   
                                   'shrinkage' = Called the shrinkage parameter, or the learning rate. A smaller learning rate typically requires 
                                   more trees.", 
                                   style = "font-family: 'times'; font-si18pt")),
                     
                                 
                                 column(4,
                                 h3("Random Forests"), 
                                 p("This is the most conceptual model we have. We are doing both cross-validation and bootstrapping. Bootstrapping
                                   is the process of resampling the training data set with replacement to create estimates. It is another method for
                                   reducing variance in the model. Random forests do not use all the predictors. In theory, a strong predictor will
                                   exist and most of the bootstrap samples will use this strong predictor for the first split in a tree branch. While
                                   this catches strong predictors, it also causes correlation between the trees, which is not good. Correlation leads to
                                   models that overfit the training data, and perform poorly on the testing data set. To avoid this, we don't
                                   use a random subset of predictors on each model. The strong ones appear for the first split still, but the next
                                   splits aren't all correlated. For this model, the user only selects mtry, which is the number of randomly 
                                   samples predictors (m) that we will use in the bootstraps. Common values are the total number of predictors (p)
                                   divided by 3 or the square root of them. In this case, that is somewhere around 2, but you can go use all 5 predictors.
                                   If you use all of the predictors, that is a special case of the random forest method called boosting.", 
                                   style = "font-family: 'times'; font-si18pt"))
                                 ),

                        ###############
                        ####### Fitting
                        ###############
                        tabPanel("Model Fitting",
                                 
                                 h1("All Choices of Models"),
                                 column(4, 
                                        h3("Modelling Choices for all Models"), 
                                        sliderInput("split", "Choose the Percentage Of Total Observations in Training Set",
                                                    min = 50, max = 90, value = 70), 
                                        selectInput("predictors", "Predictors for Bird Count", 
                                                   names(birds[,c(1:3, 6:9)]), multiple = TRUE, selectize = FALSE), 
                                        numericInput("cv", "Set the Number of Folds Used in Cross-Validation", 
                                              min = 2, max = 10, value = 5)),
                                 
                                 column(4, 
                                        h3("Boosted Trees"),
                                        selectInput("ntrees", "Number of Trees to Evaluate",
                                                    c(25, 50, 75, 100, 125, 150, 175, 200, 250, 300, 350, 400, 450, 500),
                                                    multiple = TRUE, selectize = FALSE),
                                        selectInput("interactiondepth", "Max Interaction Depth Available to the Trees",
                                                    c(1:10), multiple = TRUE, selectize = FALSE),
                                        selectInput("shrink", "Shrinkage", c(.001, .01, .1),
                                                    multiple = TRUE, selectize = FALSE),
                                         selectInput("nminobs", "Minimum Number of Observations in Node",
                                                    c(100, 500, 750, 1000), multiple = TRUE, selectize = FALSE)),
                                 column(4, 
                                        h3("Random Forest"),
                                        selectInput("mtry_num", "mTry", c(1:9),
                                                    multiple = TRUE, selectize = FALSE)),

                                 actionButton("Model", "Run Models"),
                                 
                                 verbatimTextOutput("LMResults"),
                                 verbatimTextOutput("BTPlot"),
                                 verbatimTextOutput("RFResults"),
                                 verbatimTextOutput("lmRMSE"), 
                                 verbatimTextOutput("btRMSE"), 
                                 verbatimTextOutput("rfRMSE")),
                        
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
                        )), 
             
             tabPanel("Data", 
                      ###############
                      ####### DATA DL
                      ###############
                      selectizeInput("dataVars", "Variables to Display",
                                  names(bird_count), selected = names(bird_count), multiple = TRUE),
                      downloadButton(outputId = "DuckData", 
                                     label = "Download CSV"), 
                      DTOutput("dlTable") 
                      )
  ))

