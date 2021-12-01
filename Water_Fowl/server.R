



library(shiny)
library(tidyverse)
library(lubridate)
library(caret)
library(DT)
library(doParallel)
# Package for the Model Tree regression
#install.packages("RWeka")
library(rJava)
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

birds <- fowls %>% 
  mutate( 
    CanadianGoose = if_else(!is.na(CAGO_TIBN), CAGO_TIBN, CAGO_TIB), 
    STRATUM = as.character(STRATUM)) %>%
  
  rename(Year = YEAR, State = STATE, TimeOfDay = CKTYPE, Plot = PLOT, Date = DATE,
         Stratum = STRATUM, WetHab = WETHAB, Handfeed = HANDFEED, 
         Mallard = MALL_TIB, 
         AmBlackDuck = ABDU_TIB, 
         WoodDuck = WODU_TIB) %>%

  select(!CAGO_TIB, -CAGO_TIBN, -ends_with("_TIP")) %>%
  pivot_longer(cols = 9:12, names_to = "Species", values_to = "Count") %>%
  filter(!is.na(Count))


# Fixing the dates to actual date instead of character strings
birds$Date <- mdy(birds$Date)

# These are just records where birds were seen, that have a date recorded, and fixing the date
bird_count <- birds %>% filter(Count != 0)
# Over 80% of the records are where no birds were seen so I removed those records
bird_count <- bird_count %>% filter(-is.na(Date)|Date != "1900-01-01")
bird_count[bird_count == "1930-04-20"] <- as.Date("2003-04-20")

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  output$Spec <- renderText({
    input$SpeciesUI
  })
  
  output$expGraph <- renderPlot({
    range <- input$RangeUI
    species <- input$SpeciesUI
    min_count <- range[1]
    max_count <- range[2]
    
    if(input$SpeciesUI == 1){
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
    }else{
      scatter <- ggplot(data = subset(bird_count, min_count <= Count & Count <= max_count &
                                        Species == input$SpeciesUI)) +
        geom_jitter(aes(x = Year, y = Count, color = State)) + 
        stat_smooth(data = subset(bird_count, Species == input$SpeciesUI,
                                  method = "lm", se = T)) 
    }
    scatter
  })
  
  output$yearHist <- renderPlot({
    # Can add option to just see one species at a time
    ggplot(bird_count) + 
      geom_bar(aes(x=Year, fill=Species), position = "dodge")
  })
  
  output$stateHist <- renderPlot({
    # Can add option to just see one species at a time
    ggplot(bird_count) + 
      geom_bar(aes(x=State, fill=Species), position = "dodge")
  })
  
  output$stratumHist <- renderPlot({
    # Can add option to just see one species at a time
    ggplot(bird_count) + 
      geom_bar(aes(x=Stratum, fill=Species), position = "dodge")
  })
  
  output$circTOD <- renderPlot({
    # Can add option to just see one species at a time
    ggplot(bird_count) + 
      geom_bar(aes(x=Stratum, fill=Species), position = "dodge")
  })
  
  output$circWetHab <- renderPlot({
    # Can add option to just see one species at a time
    ggplot(bird_count) + 
      geom_bar(aes(x=WetHab, fill=Species), position = "dodge")
  })
  
  output$hfGraph <- renderPlot({
    # Can add option to just see one species at a time
    ggplot(bird_count) + 
      geom_bar(aes(x=WetHab, fill=Species), position = "dodge")
  })
  
  output$countGraph <- renderPlot({
    # Can add option to just see one species at a time
    ggplot(bird_count) + 
      geom_bar(aes(x=WetHab, fill=Species), position = "dodge")
  })
  
  output$summary <- renderDT({
    summary_table <- bird_count %>% group_by(across(all_of(input$tg))) %>%
      summarize(Mean = round(mean(Count),3), Median = round(median(Count), 3), 
                Range = paste0(min(Count), " - ", max(Count), IQR = IQR(Count)))
    summary_table
    })


### MODELLING
  
  pred_data <- reactive({
    split <- input$split/100
    trainIndex <- createDataPartition(bird_count$Species, p = split, list=FALSE)
    birds_train <- bird_count[trainIndex,]
    birds_test <- bird_count[-trainIndex,]
    
    req(input$predictors)
    pred_data <- bird_count %>% select(Count, all_of(input$predictors))
    pred_data
  })
  
  ## Linear Model
  output$LMResults <- renderPrint({
    withProgress(message = 'Linear Model', value=0, ({
      lmFit1 <- train(Count ~ ., data =pred_data(),
                      method = "lm", trControl = trainControl(method = "cv", number = 5),
                      na.action = na.exclude)
    }))
    summary(lmFit1)
  })

  ## Boosted Trees
  output$BTResults <- renderPlot({
    withProgress(message = 'Boosted Trees', value=0, ({
      cl <- makePSOCKcluster(6)
      registerDoParallel(cl)
      Count_tree <- train(Count ~ ., data = pred_data(), method = "gbm",
                          trControl = trainControl(method = "repeatedcv", number = 5),
                          tuneGrid = expand.grid(n.trees = c(25, 50, 100, 150, 200),
                                                 interaction.depth = c(1:4), shrinkage = 0.1, 
                                                 n.minobsinnode = 10),)
      stopCluster(cl)
    }))

    plot(Count_tree, xlab = "Max Tree Depth (by Interaction)",
         ylab = "Root Mean Square Error (Found through CV)",
         main = "Subsetted Predictors - Minimizing RMSE")
  })

  ## Random Forests
  output$RFResults <- renderPlot({
    withProgress(message = 'Random Forests', value=0, ({
      cl <- makePSOCKcluster(6)
      registerDoParallel(cl)

      rf_SigPred <- train(Count~ ., data = pred_data(), method = "rf",
      trControl = trainControl(method = "cv", number = 5),
      tuneGrid = data.frame(mtry = c(1, 2, 3, 4)))

      stopCluster(cl)
      }))
    rf_SigPred
    plot(rf_SigPred)
  })
  
  
### Datatable
  # Make table first
  
  
  output$dlTable <- renderDT({
    bird_count
  })
  
})

