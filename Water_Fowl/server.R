



library(shiny)
library(tidyverse)
library(lubridate)
library(caret)
library(DT)
library(doParallel)



fowls$STRATUM = as.character(fowls$STRATUM)

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
  
  output$expGraph <- renderPlot({
    range <- input$RangeUI
    min_count <- range[1]
    max_count <- range[2]
    scatter <- ggplot(data = subset(bird_count, min_count <= Count & Count <= max_count)) +
      geom_jitter(aes(x = Year, y = Count, shape = Species, color = State))
    
    scatter
  })
  
  output$summary <- renderDT({
    if(input$TableGroupUI2=="None"){
      summary_table <- bird_count %>% group_by(.data[[input$TableGroupUI1]]) %>%
        summarize(Mean = round(mean(Count),3), Median = round(median(Count), 3), 
                  Range = paste0(min(Count), " - ", max(Count), IQR = IQR(Count)))
      summary_table
    }else{
    summary_table <- bird_count %>% group_by(.data[[input$TableGroupUI1]], .data[[input$TableGroupUI2]]) %>%
      summarize(Mean = round(mean(Count),3), Median = round(median(Count), 3), 
                Range = paste0(min(Count), " - ", max(Count), IQR = IQR(Count)))
    summary_table
    }
  })
  
  
  output$tbCond <- renderPrint({
    .data[[input$TableGroupUI]]
  })

  output$range <- renderPrint({
    range <- input$RangeUI
    range
  })


### MODELLING
  
  ## Linear Model
  output$LMResults <- renderPrint({
    split <- input$split/100
    trainIndex <- createDataPartition(bird_count$Species, p = split, list=FALSE)
    birds_train <- bird_count[trainIndex,]
    birds_test <- bird_count[-trainIndex,]

    req(input$predictors)
    pred_data <- bird_count %>% select(Count, all_of(input$predictors))

    withProgress(message = 'Linear Model', value=0, ({
      lmFit1 <- train(Count ~ ., data =pred_data,
                      method = "lm", trControl = trainControl(method = "cv", number = 5),
                      na.action = na.exclude)
    }))
    summary(lmFit1)
  })
  
  ## Random Forests
  # output$RFResults <- renderPlot({
  #   split <- input$split/100
  #   trainIndex <- createDataPartition(bird_count$Species, p = split, list=FALSE)
  #   birds_train <- bird_count[trainIndex,]
  #   birds_test <- bird_count[-trainIndex,]
  #   
  #   req(input$predictors)
  #   pred_data <- bird_count %>% select(Count, all_of(input$predictors))
  #   
  #   withProgress(message = 'Random Forests', value=0, ({
  #     rf_SigPred <- train(Count ~ Year + State + Handfeed + Species,
  #     data = birds_train, method = "rf",
  #     trControl = trainControl(method = "cv", number = 5),
  #     tuneGrid = data.frame(mtry = c(1, 2, 3, 4)))
  #     })) 
  #   rf_SigPred
  #   plot(rf_SigPred)
  # })
})

