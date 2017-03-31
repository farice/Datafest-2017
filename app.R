#
# Copyright © 2016 by Faris Sbahi 
# 7/20/16 
# All right reserved
#
# No portion of this code may be reproduced or copied in any fashion,
# for any purpose, commerical or otherwise, without explicit written 
# permission from the author.
#
# Any piece of code taken from outside sources is part of the open source domain
# and all credit is due to the respective owner.
# 
# Special thanks to the Shiny Open Source package
# and the Microsoft Open R software suite
#
# All inquiries:
# farissbahi@gmail.com
#

library(shiny)
library(dplyr)
library(survival)
library(shinythemes)
library(randomForestSRC)
library(ggRandomForests)
library(rms)
library(risksetROC)
library(caret)
library(glmnet)
library(doParallel)
library(zoo)
library(RcppRoll)
registerDoParallel(detectCores()-1)
detectCores()
options(rf.cores = detectCores() -1, mc.cores = detectCores() - 1)


outputDir <- "C:\\Users\\fsbahi\\Documents\\Attrition-Web-App\\Attrition-Analytics\\backups"

saveData <- function(data) {
  #data <- t(data)
  # Create a unique file name
  fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
  # Write the file to the local system
  write.csv(
    x = data,
    file = file.path(outputDir, fileName), 
    row.names = FALSE, quote = TRUE
  )
}

loadData <- function() {
  # Read all the files into a list
  backup_names <- list.files(outputDir, full.names = TRUE)
  backup_names
}

# Define UI for application

# To change header font move to line preceding h4 in style tag definitions 
#  h1 {
#    font-family: 'Inconsolata', monospace;
#    font-weight: 500;
#    line-height: 1.1;
#    
#  }

ui <- shinyUI(fluidPage(theme = shinytheme("cosmo"),
                        tags$head(
                          tags$style(HTML("
                                          @import 'https://fonts.googleapis.com/css?family=Dosis|Inconsolata|Poiret+One';
                                          
                                          
                                          h4 {
                                          font-family: 'Poiret One', cursive; 
                                          font-weight: 500;
                                          line-height: 1.1;
                                          
                                          }
                                          
                                          "))
                          ),
                        # Welcome title
                        headerPanel("Welcome to the Attrition Predictive Analytic Tool."),
                        withTags({
                          div(class="header", checked=NA,
                              tags$a(href="http://texnotes.me", "by Faris Sbahi")
                          )
                        }),
                        sidebarLayout(
                          sidebarPanel(
                            # adding the new div tag to the sidebar            
                            
                            #Selector for file upload
                            fileInput('csv_input', 'Choose CSV file',
                                      accept=c('text/csv', 'text/comma-separated-values,text/plain')),
                            selectInput("dataset", "Choose to load a backup. If you've just uploaded a file select the option below:", choices = c("Uploaded CSV" 
                                                                                                                                                   #                                                                                                           ,"Backup (Inactive)"
                            )),
                            
                            helpText("Begin date of upload should precede test set start by min. one year."),
                            
                            sliderInput("dabble",
                                        "Min Orders per Month:",
                                        min = 5,
                                        max = 20,
                                        value = 5),
                            
                            withTags({
                              div(class="header", checked=NA,
                                  h3("Prediction Options")
                              )
                            }),
                            dateInput("tp",
                                      "Select the Sunday following the week of interest.",
                                      # default value
                                      value = "2016-05-01",
                                      min = "2015-01-01",
                                      max = Sys.Date(),
                                      format = "mm/dd/yy",
                                      startview = "month",
                                      weekstart = 0,
                                      language = "en",
                                      width = NULL
                            ),
                            
                            helpText("Kit received week. Prediction is applied on all clinics with kit orders placed that week, scored relative to the 80th percentile."),
                            
                            #The action button prevents an action firing before we're ready
                            downloadButton('downloadData', 'Download CSV'),
                            #  actionButton("filterMe", "NULL")
                            
                            
                            
                            tags$div(class="header", checked=NA,
                                     tags$p("Need help? See the documentation."),
                                     tags$a(href="http://texnotes.s3.amazonaws.com/uploads/Cox_PH_Attrition.pdf", "Click Here!")
                            ),
                            
                            
                            helpText("Note: No data is stored on the server. All files are cleared upon exit for security reasons."),
                            
                            
                            
                            withTags({
                              div(class="header", checked=NA,
                                  h3("Training Data Options")
                              )
                            }),
                            dateRangeInput(
                              "trainingRange",
                              "Choose the model building range.",
                              start = "2015-01-04",
                              end = "2015-12-16",
                              max = Sys.Date(),
                              min = "2015-01-01",
                              format = "mm/dd/yy",
                              startview = "month",
                              separator = "to",
                              width =  NULL
                            ),
                            uiOutput("choose_regressors"),
                            
                            # checkboxInput("columns", "Hide Column Select"),
                            # conditionalPanel(
                            #   condition = "input.columns == false",
                            
                            withTags({
                              div(class="header", checked=NA,
                                  h3("Validation Options")
                              )
                            }),
                            dateRangeInput(
                              "valRange",
                              "Choose the validation range.",
                              start = "2016-01-04",
                              end = (Sys.Date() - 40),
                              max = (Sys.Date() - 30),
                              min = "2015-01-01",
                              format = "mm/dd/yy",
                              startview = "month",
                              separator = "to",
                              width =  NULL
                            ),
                            
                            # ),
                            
                            # checkboxInput("regressors", "Show Regressors (Inactive)"),
                            #  conditionalPanel(
                            #   condition = "input.regressors == true",
                            #  uiOutput("clinicID"),
                            #  uiOutput("kitID"),
                            #  uiOutput("ProductGroup"),
                            #  uiOutput("RegionGroup"),
                            #  uiOutput("ReceivedWeek")
                            #  ),
                            
                            #  checkboxInput("filter", "Show Filtration (Inactive)"),
                            #  conditionalPanel(
                            #  condition = "input.filter == true",
                            #  uiOutput("filter")
                            #  ),
                            
                            
                            
                            
                            withTags({
                              div(class="header", checked=NA,
                                  h3("Core Data Options")
                              )
                            }),
                            numericInput("obs", "Number of rows to view:", 3),
                            uiOutput("choose_columns"),
                            
                            withTags({
                              div(class="header", checked=NA,
                                  h3("Additional Covariates")
                              )
                            }),
                            
                            fileInput('add_input', 'Choose a CSV file formatted in accord with the documentation.', multiple = FALSE, 
                                      accept=c('text/csv', 'text/comma-separated-values,text/plain')),
                            uiOutput("choose_clinic"),
                            uiOutput("choose_week"),
                            selectInput('add_date_format', "Date format in add. CSV:", c("mm/dd/yyyy" = "%m/%d/%Y",
                                                                                         "mm/dd/yy" = "%m/%d/%y",
                                                                                         "mm-dd-yyyy" = "%m-%d-%Y", "mm-dd-yy" = "%m-%d-%y"))
                          ),
                          
                          
                          
                          mainPanel(
                            withTags({
                              div(class="header", checked=NA,
                                  h3("Predictions")
                              )
                            }),
                            tabsetPanel(
                              tabPanel("Prediction", verbatimTextOutput("guess_summary"), dataTableOutput("guess_view")),
                              tabPanel("Validation C-index", verbatimTextOutput("test_cind")),
                              
                              
                              tabPanel("Validation ROC median", plotOutput("ROC_median")),
                              tabPanel("Validation AUC", plotOutput("test_AUC")),
                              tabPanel("Training AUC", plotOutput("training_AUC"))
                              
                              
                            ),
                            withTags({
                              div(class="header", checked=NA,
                                  h3("Training Data")
                              )
                            }),
                            tabsetPanel(
                              tabPanel("Model Summary",plotOutput("gg_err", click = "plot_click"),verbatimTextOutput("coxph_summary")),
                              tabPanel("C-index and Error Rate", verbatimTextOutput("rf_cind"), verbatimTextOutput("rf_err")),
                              
                              tabPanel("Var Importance",plotOutput("gg_vimp", click = "plot_click"),verbatimTextOutput("coxph_zph")),
                              
                              tabPanel("Summary",verbatimTextOutput("cox_summary")),
                              tabPanel("View", dataTableOutput("cox_view")),
                              tabPanel("Structure", verbatimTextOutput("cox_str"))
                              
                            ),
                            withTags({
                              div(class="header", checked=NA,
                                  h3("Core Data")
                              )
                            }),
                            tabsetPanel(
                              tabPanel("Summary",verbatimTextOutput("summary")),
                              tabPanel("View", tableOutput("view")),
                              tabPanel("Structure", verbatimTextOutput("str"))
                            ),
                            
                            withTags({
                              div(class="header", checked=NA,
                                  h3("Additional Data")
                              )
                            }),
                            tabsetPanel(
                              tabPanel("Details",verbatimTextOutput("add_summary"), verbatimTextOutput("add_str")),
                              tabPanel("Processed",verbatimTextOutput("add_proc"))
                              
                            )
                          )
                          
                          
                        )
                        
                        
                        
                          ))


# Define server logic
server <- shinyServer(function(input, output) {
  #Adjust maximum file upload size from 5MB default
  options(shiny.maxRequestSize=500*1024^2)
  
  #This function is repsonsible for loading in the selected file
  salesdata <- reactive({
    infile <- input$csv_input
    
    shiny::validate(
      need(input$csv_input != "", "Please upload your sales CSV (Kits Received).")
    )
    
    
    
    switch(input$dataset, 
           #recover previously uploaded backup
           "backup" = NULL,
           
           # select uploaded CSV file for processing
           "Uploaded CSV" = 
             if (is.null(infile)) {
               return(NULL)
             }
           else{
             # show progress for length process of reading in uploaded CSV file
             withProgress( message = "CONVERTING CSV...", value = 0, {
               read.csv(infile$datapath, header = T, sep = ',')
               # dummy variable, new file was uploaded
             })
           })
    #endswitch
    
  })
  
  add_data <- reactive({
    in_file <- input$add_input
    
    shiny::validate(
      need(input$add_input != "", "Please upload an additional CSV, following the guidelines described in the documentation.")
    )
    
    
    if (is.null(in_file)) {
      return(NULL)
    }
    else{
      # show progress for length process of reading in uploaded CSV file
      withProgress( message = "CONVERTING CSV...", value = 0, {
        read.csv(in_file$datapath, header = T, sep = ',')
        # dummy variable, new file was uploaded
      })
    }
    
  })
  
  add_data_processed <- reactive({
    shiny::validate(
      need(input$add_date != "", "Please select the column that corresponds to date.")
    )
    
    shiny::validate(
      need(input$add_clinic != "", "Please select the column that corresponds to LIMS ID.")
    )
    
    df <- add_data()
    # convert clinic id to numeric
    withProgress( message = 'Processing Additional Data', value = 0, {
      
      incProgress(0.1, detail = "Rendering types")
      clinic_identifier <- input$add_clinic
      
      date_identifier <- input$add_date
      
      cat(file=stderr(), "Searching for clinic id", paste0("df$",clinic_identifier), "column\n")
      cat(file=stderr(), "Searching for date", paste0("df$",date_identifier), "column\n")
      
      #renames columns to standard clinic_id and Received_Week
      df$clinic_id <- as.numeric(as.character(df[ , clinic_identifier]))
      df$Received_Week <- as.Date(df[ , date_identifier], input$add_date_format)
      df$weekday <- as.POSIXlt(df$Received_Week)$wday
      
      incProgress(0.1, detail = "Rounding to next Sunday")
      df <- mutate(df, Received_Week = ifelse(weekday > 0 , Received_Week + (7-weekday), Received_Week))
      df$Received_Week <- as.Date(df$Received_Week)
      
      incProgress(0.1, detail = "Calculating density")
      df_grouped <- group_by(df, clinic_id, Received_Week)
      df <- mutate(df_grouped, density = n())
      df$density = as.integer(df$density)
      
      add_data <- distinct(df, clinic_id, Received_Week, .keep_all = TRUE)
      add_data <- data.frame(add_data$clinic_id, add_data$Received_Week, add_data$density)
      incProgress(0.1, detail = "Removing null rows")
      add_data <- na.omit(add_data)
      
      add_data
    })
  })
  
  output$choose_clinic <- renderUI({
    # If missing input, return to avoid error later in function
    df <- add_data()
    #df_temp <- add_data_processed()
    #ifelse(is.null(df_temp), df <- df , df <- df_temp)
    if(is.null(df))
    {  return(NULL) }
    
    # Get the data set with the appropriate name
    colnames <- names(df)
    
    # Select the variable corresponding to clinic id 
    selectInput("add_clinic", "Choose column corresponding to LIMS ID", 
                choices  = colnames)
    
  })
  
  output$choose_week <- renderUI({
    # If missing input, return to avoid error later in function
    df <- add_data()
    #df_temp <- add_data_processed()
    #ifelse(is.null(df_temp), df <- df , df <- df_temp)
    if(is.null(df))
    {  return(NULL) }
    
    # Get the data set with the appropriate name
    colnames <- names(df)
    
    # Select the variable corresponding to clinic id 
    selectInput("add_date", "Choose column corresponding to date", 
                choices  = colnames)
    
    
    
    
  })
  
  
  # Check boxes
  output$choose_columns <- renderUI({
    # If missing input, return to avoid error later in function
    df <- salesdata()
    df_temp <- salesdata_filtered()
    ifelse(is.null(df_temp), df <- df , df <- df_temp)
    if(is.null(df))
    {  return(NULL) }
    
    # Get the data set with the appropriate name
    colnames <- names(df)
    
    # Create the checkboxes and select defaults
    checkboxGroupInput("columns", "Choose columns", 
                       choices  = colnames,
                       selected = c("clinic_id", "Product_Group", "Region_Group", "Received_Week", "Kit_ID", "total_days", 
                                    "total_volume", "volmo", "volume", "age", "max_week", "status", "cumvol", "vol_2", "vol_3", 
                                    "vol_4", "vol_6", "vol_8", "prev_2", "prev_3", "prev_4", "diff_2", "diff_3", "diff_4"))
    
  })
  
  output$choose_regressors <- renderUI({
    
    # If missing input, return to avoid error later in function
    df <- cox_data()
    
    if(is.null(df)) return(NULL)
    
    # Get the data set with the appropriate name
    colnames <- names(df)
    
    # Create the checkboxes and select them all by default 
    
    checkboxGroupInput("regress", "Choose regressors", 
                       choices  = colnames,
                       selected = c("volume", "vol_2", "vol_3", 
                                    "vol_4", "vol_6", "vol_8", "age", "diff_2", "diff_3", "diff_4",
                                    "cumvol"))
    
    # "volume:stop", "stop", , "stop:age", "age:cumvol"
  })
  
  salesdata_columned <- reactive(
    {
      df <- salesdata()
      df_temp <- salesdata_filtered()
      ifelse(is.null(df_temp), df <- df , df <- df_temp)
      if (is.null(df)) return(NULL)
      
      if (is.null(input$columns) || !(input$columns %in% names(df)))
        return(NULL)
      
      # Keep the selected columns
      df <- df[, input$columns, drop = FALSE]
      
    })
  
  cox_data <- reactive(
    {
      data <- salesdata_columned()
      if(is.null(data$status)) return(NULL)
      withProgress( message = 'Forming Hazards Data', value = 0, {
        
        incProgress(0.1, detail = "Building Tr. Set")
        training = filter(data, Received_Week > format(input$trainingRange[1]), Received_Week < format(input$trainingRange[2]))
        incProgress(0.1, detail = "Creating baselines")
        by_clinid <- group_by(training, clinic_id)
        training <- mutate(by_clinid, start = age - min(age))
        training <- mutate(training, stop = start + 7)
        
        # time dependency 
        by_clingroup <- group_by(training, clinic_id, Received_Week)
        training <- mutate(by_clingroup, mark_for_removal = ifelse(Received_Week == max_week || Received_Week > (input$trainingRange[2] - 6), 0, 1))
        training <- filter(training, mark_for_removal == 0)
        
      })
    }
  )
  
  cox_ph <- reactive({
    
    shiny::validate(
      need(input$regress != "", "Waiting for data...")
    )
    
    training <- cox_data()
    if(is.null(training)) return(NULL)
    withProgress( message = 'Forming Survival Forest', value = 0, {
      # allow adjustment of variables 
      incProgress(0.3, detail = "Fitting model")
      trfit <- rfsrc(as.formula(paste("Surv(stop, status) ~", paste(input$regress, collapse = "+"))), data = training, nsplit = 1, importance = TRUE)
      # summarize and check proportional hazard assumption
      
    })
  })
  
  test_data <- reactive({
    data <- salesdata_columned()
    if(is.null(data$status)) return(NULL)
    withProgress( message = 'Forming Test Data', value = 0, {
      
      incProgress(0.1, detail = "Building Test Set")
      test = filter(data, Received_Week > format(input$valRange[1]), Received_Week < format(input$valRange[2]))
      incProgress(0.1, detail = "Creating baselines")
      by_clinid <- group_by(test, clinic_id)
      test <- mutate(by_clinid, start = age - min(age))
      test <- mutate(test, stop = start + 7)
    })
  })
  
  tp_data <- reactive({
    data <- salesdata_columned()
    if(is.null(data$status)) return(NULL)
    withProgress( message = 'Forming TP Data', value = 0, {
      
      incProgress(0.1, detail = "Building Test Set")
      data = filter(data, Received_Week > format(input$valRange[1]))
      incProgress(0.1, detail = "Creating baselines")
      by_clinid <- group_by(data, clinic_id)
      data <- mutate(by_clinid, start = age - min(age))
      data <- mutate(data, stop = start + 7)
      data = filter(data, Received_Week == input$tp)
    })
  })
  
  output$test_cind <- renderPrint({
    
    rf_fit <- cox_ph()
    dat <- test_data()
    if(is.null(rf_fit)) return(NULL)
    withProgress( message = 'Analyzing test set', value = 0, {
      incProgress( 0.4, detail = "Making predictions")
      pred <- predict(rf_fit, newdata = dat, importance = "none")
      incProgress( 0.3, detail = "Calculating C-index")
      c<-rcorr.cens(-pred$predicted, Surv(dat$stop, dat$status))["C Index"]
      c
    })
  })
  cox_survfit <- reactive({
    withProgress( message = 'Fitting Survival Function', value = 0, {
      incProgress( 0.2, detail = "Analyzing fit")
      bhaztr <- survfit(trfit)
      
    })
  })
  
  output$gg_vimp <- renderPlot({
    rf_fit <- cox_ph()
    if(is.null(rf_fit)) return(NULL)
    plot(gg_vimp(rf_fit))
  })
  
  output$gg_err <- renderPlot({
    rf_fit <- cox_ph()
    if(is.null(rf_fit)) return(NULL)
    gg_dta <- gg_error.rfsrc(rf_fit)
    
    plot(gg_dta)
  })
  
  output$ROC_median <- renderPlot({
    training <- cox_data()
    rf_fit <- cox_ph()
    if(is.null(training) | is.null(rf_fit)) return(NULL)
    withProgress( message = "Creating Median Survival ROC", value = 0, {
      ROC = risksetROC(Stime = training$stop, status = training$status, marker = rf_fit$predicted.oob, predict.time = median(training$stop),
                       method = "Cox", main = paste("Survival ROC Curve at Median time", median(training$stop)), lwd = 3, col ="red")
      
    })  
  })
  
  output$training_AUC <- renderPlot({
    training <- cox_data()
    rf_fit <- cox_ph()
    if(is.null(training) | is.null(rf_fit)) return(NULL)
    withProgress( message = "Creating AUC for training", value = 0, {
      ROC = risksetAUC(Stime = training$stop, status = training$status, marker = rf_fit$predicted.oob, tmax = 350)
    })
  })
  
  output$test_AUC <- renderPlot({
    test <- test_data()
    rf_fit <- cox_ph()
    if(is.null(test) | is.null(rf_fit)) return(NULL)
    withProgress( message = "Creating AUC for test", value = 0, {
      incProgress(0.3, detail = "Predicting")
      pred <- predict(rf_fit, newdata = test, importance = "none")
      incProgress(0.3, detail = "Plotting")
      ROC = risksetAUC(Stime = test$stop, status = test$status, marker = pred$predicted, tmax = 200, method = "Cox")
    })
  })
  
  output$filter <- renderUI({
    df <- salesdata_columned()
    if (is.null(df)) return(NULL)
    prodnames <- levels(df$Product_Group)
    
    selectInput("prod_filter", "Select Products",
                choices = prodnames,
                selected = prodnames,
                multiple = TRUE
    )
  })
  
  #Generate structure of the dataset
  output$cox_str <- renderPrint({
    data <- cox_data()
    if(is.null(data)) return(NULL)
    str(data)
  })
  
  output$str <- renderPrint({
    df <- salesdata_columned()
    if(is.null(df)) return(NULL)
    str(df)
  })
  
  output$rf_cind <- renderPrint({
    rf_fit <- cox_ph()
    dat <- cox_data()
    if(is.null(rf_fit)) return(NULL)
    withProgress( message = 'Analyzing test set', value = 0, {
      incProgress( 0.5, detail = "Calculating C-index")  
      c <- rcorr.cens(1-rf_fit$predicted.oob, Surv(dat$stop, dat$status))["C Index"]
      c
    })
  })
  
  output$rf_err <- renderPrint({
    rf_fit <- cox_ph()
    dat <- cox_data()
    if(is.null(rf_fit)) return(NULL)
    err = rf_fit$err.rate[rf_fit$ntree]
    err
  })
  
  output$cox_summary <- renderPrint({
    data <- cox_data()
    if(is.null(data)) return(NULL)
    summary(data)
  })
  
  output$coxph_summary <- renderPrint({
    rf_fit <- cox_ph()
    if(is.null(rf_fit)) return(NULL)
    rf_fit
  })
  
  output$coxph_zph <- renderPrint({
    rf_fit <- cox_ph()
    if(is.null(rf_fit$importance)) return(NULL)
    rf_imp <- sort(rf_fit$importance, decreasing = TRUE)
    rf_imp
  })
  
  output$add_summary <- renderPrint({
    df <- add_data()
    df_temp <- add_data_processed()
    ifelse(is.null(df_temp), df <- df , df <- df_temp)
    if(is.null(df)) return(NULL)
    summary(df)
    
  })
  
  output$add_str <- renderPrint({
    df <- add_data()
    df_temp <- add_data_processed()
    ifelse(is.null(df_temp), df <- df , df <- df_temp)
    if(is.null(df)) return(NULL)
    str(df)
  })
  
  
  output$summary <- renderPrint({
    df <- salesdata_columned()
    if(is.null(df)) return(NULL)
    summary(df)
  })
  # show the first "n" observations
  output$cox_view <- renderDataTable({
    data <- cox_data()
    if(is.null(data)) return(NULL)
    data
  })
  
  output$guess_view <- renderDataTable({
    rf_fit <- cox_ph()
    dat <- tp_data()
    if(is.null(rf_fit)) return(NULL)
    withProgress( message = 'Loading test set', value = 0, {
      incProgress( 0.4, detail = "Making predictions")
      pred <- predict(rf_fit, newdata = dat, importance = "none")
      incProgress(0.1, detail = "Normalizing")
      eighty_decile <- unname(quantile(pred$predicted, 0.8))
      pred$predicted <- pred$predicted - eighty_decile
      incProgress( 0.3, detail = "Rendering")
      pred <- as.data.frame(pred$predicted)
      df <- data.frame(dat, pred)
      df
    })
  })
  
  
  
  output$guess_summary <- renderPrint ({
    rf_fit <- cox_ph()
    dat <- tp_data()
    if(is.null(rf_fit)) return(NULL)
    withProgress( message = 'Loading test set', value = 0, {
      incProgress( 0.4, detail = "Making predictions")
      pred <- predict(rf_fit, newdata = dat, importance = "none")
      incProgress( 0.3, detail = "Rendering")
      eighty_decile <- unname(quantile(pred$predicted, 0.8))
      print(paste0("We've normalized risk scores in the outputted CSV by the 80th percentile: ", eighty_decile))
      summary(pred)
      
    })
  })
  
  output$view <- renderTable({
    df <- salesdata_columned()
    if(is.null(df)){return(NULL)}
    head(df, n = input$obs)
  })
  
  output$downloadData <- downloadHandler( filename = function() { 'predictions.csv'}, 
                                          content = function(file) {
                                            rf_fit <- cox_ph()
                                            dat <- tp_data()
                                            if(is.null(rf_fit)) return(NULL)
                                            withProgress( message = 'Saving TP CSV', value = 0, {
                                              incProgress( 0.4, detail = "Making predictions")
                                              pred <- predict(rf_fit, newdata = dat, importance = "none")
                                              incProgress(0.1, detail = "Normalizing")
                                              eighty_decile <- unname(quantile(pred$predicted, 0.8))
                                              pred$predicted <- pred$predicted - eighty_decile
                                              incProgress( 0.2, detail = "Rendering")
                                              pred <- as.data.frame(pred$predicted)
                                              incProgress( 0.2, detail = "Saving")
                                              df <- data.frame(dat$clinic_id, dat$Received_Week, pred)
                                              
                                              write.csv(df, file, row.names = F)
                                            })
                                          })
  
  salesdata_filtered <- reactive(
    {
      df <- salesdata()
      
      
      if (is.null(df)) return(NULL)
      
      withProgress( message = 'Processing Core Data', value = 0, {
        
        # Filter for HCS, NPT, Direct 
        
        incProgress(0.1, detail = "Filtering for HCS & NPT")
        df <- filter(df, Product_Group == "HCS" | Product_Group == "NPT")
        incProgress(0.1, detail = "Filtering for Direct sales")
        df <- filter(df, Region_Group == "Direct")
        
        #Convert received week to date type and clinic id to integer
        incProgress(0.1, detail = "Fixing types")
        df$Received_Week = as.Date(df$Received_Week, format = "%m/%d/%y")
        df$clinic_id = as.integer(as.character(df$clinic_id))
        
        #Remove dabblers dependent on position of slider
        incProgress(0.1, detail = "Removing dabblers")
        by_clinid <- group_by(df, clinic_id)
        df <- mutate(by_clinid, total_days = max(Received_Week) - min(Received_Week))
        by_clinid <- group_by(df, clinic_id)
        df <- mutate(by_clinid, total_volume = n())
        df$total_days = as.integer(df$total_days)
        by_clinid <- group_by(df, clinic_id)
        df <- mutate(by_clinid, volmo = ifelse(total_days > 30, total_volume/((total_days)/30+0.25), total_volume/(30+0.25)))
        df <- filter(df, volmo > input$dabble)
        
        # Calculate age, volume, cumvol, and status
        incProgress(0.1, detail = "Extracting Covariates")
        by_clingroup <- group_by(df, clinic_id, Received_Week)
        df <- mutate(by_clingroup, volume = n())
        df$volume = as.integer(df$volume)
        by_clinid <- group_by(df, clinic_id)
        df <- mutate(by_clinid, age = Received_Week - min(Received_Week))
        df$age = as.integer(df$age)
        by_clinid <- group_by(df, clinic_id)
        df <- mutate(by_clinid, max_week = max(Received_Week)) 
        max_global_offset <- (max(df$max_week)-30)
        by_clingroup <- group_by(df, clinic_id, Received_Week)
        df <- mutate(by_clingroup, status = ifelse(Received_Week == max_week && Received_Week < max_global_offset , 1, 0))
        data <- distinct(df, clinic_id, Received_Week, .keep_all = TRUE)
        
        incProgress(0.2, detail = "Adding extra metrics")
        by_clinid <- group_by(data, clinic_id)
        data <- mutate(by_clinid, cumvol = cumsum(volume))
        # vol last 2/3/4 weeks
        by_clinid <- group_by(data, clinic_id)
        data <- mutate(by_clinid, dummy = 1)
        by_clinid <- group_by(data, clinic_id)
        data <- mutate(by_clinid, entry = cumsum(dummy))
        by_clinid <- group_by(data, clinic_id)
        data <- mutate(by_clinid, vol_4 = ifelse(entry>3, as.integer(roll_sumr(volume, 4)), as.integer(cumsum(volume))))
        by_clinid <- group_by(data, clinic_id)
        data <- mutate(by_clinid, vol_3 = ifelse(entry>2, as.integer(roll_sumr(volume, 3)), as.integer(cumsum(volume))))
        by_clinid <- group_by(data, clinic_id)
        data <- mutate(by_clinid, vol_2 = ifelse(entry>1, as.integer(roll_sumr(volume, 2)), as.integer(cumsum(volume))))
        # dummy metrics for previous 2/3/4
        by_clinid <- group_by(data, clinic_id)
        data <- mutate(by_clinid, vol_6 = ifelse(entry>5, as.integer(roll_sumr(volume, 6)), as.integer(cumsum(volume))))
        by_clinid <- group_by(data, clinic_id)
        data <- mutate(by_clinid, vol_8 = ifelse(entry>7, as.integer(roll_sumr(volume, 8)), as.integer(cumsum(volume))))
        # prev 2, 3, 4
        data <- mutate(data, prev_2 = vol_4 - vol_2)
        data <- mutate(data, prev_3 = vol_6 - vol_3)
        data <- mutate(data, prev_4 = vol_8 - vol_4)
        # current / prev
        data <- mutate(data, diff_2 = ifelse(prev_2 == 0, 1, vol_2/prev_2))
        data <- mutate(data, diff_3 = ifelse(prev_3 == 0, 1, vol_3/prev_3))
        data <- mutate(data, diff_4 = ifelse(prev_4 == 0, 1, vol_4/prev_4))
        
        incProgress(0.1, detail = "Checking for add. covariates")
        
        cat(file=stderr(), "State of add_input (is null?):", is.null(input$add_input), "\n")
        if(!is.null(input$add_input)) {
          
          add <- add_data_processed()
          
          
          if(!is.null(add$data.Received_Week)&&!is.null(data$Received_Week)) {
            incProgress(0.1, detail = "Processing add. covariates")
            add$Received_Week <- add$data.Received_Week
            add$clinic_id <- add$data.clinic_id
            data <- left_join(data, add, by = c("Received_Week", "clinic_id"))
            cat(file=stderr(), "Left join completed\n")
            data$data.density = ifelse(is.na(data$data.density), 
                                       0, data$data.density)
          }
        }
        
        data
        
        
      }
      )
      
    })
  
})

# Run the application 
shinyApp(ui = ui, server = server)

