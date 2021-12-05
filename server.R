library(shiny)
library(tidyverse)
library(lubridate)
library(caret)
library(randomForest)
library(gbm)
library(DT)
library(doParallel)
# Package for the Model Tree regression
#install.packages("RWeka")
library(rJava)
library(RWeka)


#############################################
############# Data Prep #####################
#############################################
#fowls <- read.csv("data/duck_data.csv")

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
# # Over 80% of the records are where no birds were seen so I removed those records
# bird_count <- bird_count %>% filter(-is.na(Date)|Date != "1900-01-01")
# bird_count[bird_count == "1930-04-20"] <- as.Date("2003-04-20")



#############################################
################ SERVER #####################
#############################################


shinyServer(function(input, output, session) {
  
### EXPLORATION PAGE  
  scatter <- eventReactive(input$Graph,{
    
    # Scatter Plot for Count By Year
    if(input$DisplayGraph == 1 && input$de_spec_spec == 1){
    range <- input$RangeUI
    min_count <- range[1]
    max_count <- range[2]
      scatter <- ggplot(data = subset(bird_count, min_count <= Count & Count <= max_count), 
                        aes(x = Year, y = Count, shape = Species, color = State)) +
        geom_jitter() +
        stat_smooth(data = subset(bird_count, Species == "AmBlackDuck",
                                  method = "lm", se = T)) +
        stat_smooth(data = subset(bird_count, Species == "CanadianGoose", 
                                  method = "lm", se = T)) + 
        stat_smooth(data = subset(bird_count, Species == "Mallard", 
                                  method = "lm", se = T)) + 
        stat_smooth(data = subset(bird_count, Species == "WoodDuck", 
                                  method = "lm", se = T))
      scatter
    }
    # Scatter Plot for Count By Year by Species
    else if(input$DisplayGraph == 1 && input$de_spec_spec != 1){
      range <- input$RangeUI
      min_count <- range[1]
      max_count <- range[2]
      scatter <- ggplot(data = subset(bird_count, min_count <= Count & Count <= max_count &
                                        Species == input$de_spec_spec)) +
        geom_jitter(aes(x = Year, y = Count, color = State)) + 
        stat_smooth(data = subset(bird_count, Species == input$de_spec_spec, method = "lm", se = T), 
                    (aes(x = Year, y = Count)))
      scatter
    }
    # Bar Plots For Year By Species
    else if(input$DisplayGraph == 2 && input$de_year_spec == 1){
      yr <- ggplot(bird_count) + 
        geom_bar(aes(x=Year, color = WetHab), position = "dodge")
      yr
    }else if(input$DisplayGraph == 2 && input$de_year_spec != 1){
      yrsp <- ggplot(data = subset(bird_count, Species == input$de_year_spec)) +
      geom_histogram(aes(x=Year, color = WetHab))
      yrsp
    }
    # State History
    else if(input$DisplayGraph == 3)
      {
      state <- ggplot(bird_count) +
        geom_bar(aes(x=State, fill=Species), position = "dodge")
      state
    }
    # Stratum
    else if(input$DisplayGraph == 4){
      strat <- ggplot(bird_count) +
        geom_bar(aes(x=Stratum, fill=Species), position = "dodge")
      strat
    }
    # TimeOfDay
    else if(input$DisplayGraph == 5){
      plot(5, 15)
    }
    # Handfeed
    else if(input$DisplayGraph == 7){
      hf <- ggplot(bird_count) +
        geom_bar(aes(x=Handfeed, fill=Species), position = "dodge")
      hf
    }
    # Wet Hab
    else if(input$DisplayGraph == 6){
      hab <- ggplot(bird_count) +
        geom_bar(aes(x=WetHab, fill=Species), position = "dodge")
      hab
    }
    # Histogram of Counts, looks like Poisson
    else{
      count <- ggplot(bird_count) + geom_bar(aes(x=Count, color=Species), position = "dodge")
      count
    }
  })
  
  #strat_rem <- eventReactive()
  
  output$expGraph <- renderPlot({
    scatter()
  })
  
  summs <- eventReactive(input$Table,{
    summary_table <- bird_count %>% group_by(across(all_of(input$tg))) %>%
      summarize(Mean = round(mean(Count),3), Median = round(median(Count), 3), 
                Range = paste0(min(Count), " - ", max(Count), IQR = IQR(Count)), 
                Observations = n())
  })
  
  output$summary <- renderDT(summs(), filter = "top", options = list(paging = FALSE, 
                                                                  autowidth = TRUE))
  
  
  #########################
  ####### MODELLING #######
  #########################
  
  ###############
  ### Information
  ###############
  output$rmseFormula <- renderUI({
    withMathJax(helpText('RMSE:  
                         $$\\frac{1}{n}\\sum_{i=1}^{n}\\,X_n$$'))
  })
  
  output$lmFormula <- renderUI({
    withMathJax(helpText('Multiple Linear Regression Model:  
                         $$Y=\\beta_0+\\beta_1\\,X_1+...+\\beta_n\\,X_n+\\epsilon$$'))
  })

  
  ###############
  ####### Fitting
  ###############
  cv_num <- eventReactive(input$Model, {
    input$cv
  })
  
  split <- eventReactive(input$Model, {
    input$split/100
  })

  pred_train <- eventReactive(input$Model, {
    set.seed(12)
    trainIndex <- createDataPartition(bird_count$Species, p = split(), list=FALSE)
    # Training Data Set that is proportional to species dsitr
    birds_train <- bird_count[trainIndex,]
    
    req(input$predictors)
    # Selecting Predictors
    pred_train <- bird_count %>% select(Count, all_of(input$predictors))
    pred_train
  })
  
  pred_test <- eventReactive(input$Model, {
    set.seed(12)
    trainIndex <- createDataPartition(bird_count$Species, p = split(), list=FALSE)
    # Testing Data Set that is proportional to species dsitr
    birds_test <- bird_count[-trainIndex,]
    
    req(input$predictors)
    # Selecting Predictors
    pred_test <- bird_count %>% select(Count, all_of(input$predictors))
    pred_test
  })
  
  
  ###############
  ## Linear Model
  ###############
  lmFit <- eventReactive(input$Model, {
    withProgress(message = 'Linear Model', value=0, {
      for(i in 1:15){
        incProgress(1/15)
      }
      })

      lmFit <- train(Count ~ ., data =pred_train(),
                      method = "lm", trControl = trainControl(method = "cv", number = cv_num()),
                      na.action = na.exclude)
    })

  output$LMResults <- renderPrint({
    summary(lmFit())
  })
  
  
  ###############
  ## Boosted Trees
  ###############
  ntrees_num <- eventReactive(input$Model, {
    input$ntrees
  })

  interactiondepth <- eventReactive(input$Model, {
    input$interactiondepth
  })

  shrink <- eventReactive(input$Model, {
    input$shrink
  })

  nminobs <- eventReactive(input$Model, {
    input$nminobs
  })

  bt <- eventReactive(input$Model, {
    withProgress(message = 'Boosted Trees', value=0, {
      for(i in 1:15){
        incProgress(1/15)
      }
    })
      cl <- makePSOCKcluster(6)
      registerDoParallel(cl)
      btFit <- train(Count ~ ., data = pred_train(), method = "gbm",
                     trControl = trainControl(method = "repeatedcv", number = cv_num()),
                     tuneGrid = expand.grid(n.trees = c(as.numeric(paste(ntrees_num()))),
                                            interaction.depth = c(as.numeric(paste(interactiondepth()))),
                                            shrinkage = c(as.numeric(paste(shrink()))),
                                            n.minobsinnode = c(as.numeric(paste(nminobs())))))
      stopCluster(cl)
      btFit
    })

  output$BTPlot <- renderPrint({
    bt()
  })

  #################
  ## Random Forests
  #################
  mtry_num <- eventReactive(input$Model, {
    input$mtry_num
  })

  rf <- eventReactive(input$Model, {
    withProgress(message = 'Random Forest', value=0, {
      for(i in 1:200){
        incProgress(1/200)
      }
    })
      cl <- makePSOCKcluster(6)
      registerDoParallel(cl)
      rfFit <- train(Count~ ., data = pred_train(), method = "rf",
                          trControl = trainControl(method = "cv", number = cv_num()),
                          tuneGrid = data.frame(mtry = c(as.numeric(paste(mtry_num())))))
      stopCluster(cl)
      rfFit
  })

  output$RFResults <- renderPrint({
    rf()
  })

  
  #################
  ############ RMSE
  #################
  output$lmRMSE <- renderPrint({
    lmPred <- predict(lmFit(), newdata = pred_test())
    # Calculating RMSE
    lmRMSE <- sqrt(mean((lmPred-pred_test()$Count)^2))
    lmRMSE
  })
  
  output$btRMSE <- renderPrint({
    btPred <- predict(bt(), newdata = pred_test())
    # Calculating RMSE
    btRMSE <- sqrt(mean((btPred-pred_test()$Count)^2))
    btRMSE
  })
  
  output$rfRMSE <- renderPrint({
    rfPred <- predict(rf(), newdata = pred_test())
    # Calculating RMSE
    rfRMSE <- sqrt(mean((rfPred-pred_test()$Count)^2))
    rfRMSE
  })
  
  
  #################
  ###### Prediction
  ################# 
  pred_species <- observe({
    input$predSpecies
  })
  
  pred_year <- observe({
    input$predYear
  })
  
  pred_state <- observe({
    input$predState
  })
  
  pred_stratum <- observe({
    input$predStrat
  })
  
  pred_hab <- observe({
    input$predHab
  })
  
  # Prediction from a Random Forest
  output$rfPredCount <- renderPrint({   
    rfFit <- randomForest(Count ~ Year + State + Stratum + WetHab + Species,
                          data = birds_train, mtry = 4,
                          ntree = 200, importance = TRUE)
    predict(rfFit, newdata = data.frame(Year = input$predYear, 
                                        State = input$predState,
                                        Stratum = input$predStrat, 
                                        WetHab = input$predHab,
                                        Species = input$predSpecies))
  })
  
  
  ###############
  ####### DATA DL
  ###############
  dt <- eventReactive(input$dataVars, {
    # Selecting Predictors
    dt <- bird_count %>% select(Count, all_of(input$dataVars))
    dt
  })
  
  output$dlTable <- renderDT(
    dt(), filter = "top", options = list(pageLength = 50, 
                                               autowidth = TRUE))
  #   options = list(paging = FALSE))
  
  output$DuckData <- downloadHandler(
    filename = "Filtered_Duck.csv", 
    content = function(file){
      write.csv(bird_count[input[["dlTable_rows_all"]],],
                file)}
  )
})


