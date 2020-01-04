# By default, the file size limit is 5MB. It can be changed by setting
# this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 9 * 1024 ^ 2)
# Original_data=NULL

library(shiny)
library(sas7bdat)
library(haven)
library(DT)
library(Hmisc)
# library(xlsx)
library(Information)
library(htmlTable)
library(Matrix)
library(ggplot2)
# library(rminer)


function(input, output, session)
{
  js$disableTab("tab2")
  js$disableTab("tab3")
  js$disableTab("tab4")
  js$disableTab("tab5")
  js$disableTab("tab6")
  Original_data <- reactive({
    inFile <- input$chooseData
    
    if (is.null(inFile))
      return(NULL)
    if (input$fileType == "sas7bdat")
    {
      read_sas(inFile$datapath)
    } else if (input$fileType == "csv")
    {
      read.csv(inFile$datapath,
               header = input$header,
               sep = input$sep)
    }
  })
  
  
  # op <-
  #   reactiveValues(
  #     data = NULL,
  #     Original_data.WoSALES = data.frame(),
  #     influential_data = data.frame(),
  #     Original_data_Subset = data.frame(),
  #     Original_Subset_WithSales = data.frame(),
  #     Results_Data = data.frame(),
  #     Results_Data_Splitted = data.frame(),
  #     ResultTable = data.frame()
  #   )
  VarList <- reactiveValues(
    Original_data.Selected = data.frame(),
    Training_Data = data.frame(),
    Validation_Data = data.frame(),
    Treatment_Group = data.frame(),
    Control_Group = data.frame(),
    NIVVariables = NULL,
    NIVVariablesUpdated = NULL,
    PredictTreatmentGroup = NULL,
    PredictTreatmentGroupResponse = NULL,
    misClassErrorTreatment = NULL,
    PredictControlGroup = NULL,
    PredictControlGroupResponse = NULL,
    misClassErrorControl = NULL,
    Difference_Score = NULL,
    PredictedTreatmentVect = c(),
    PredictedControlVect = c(),
    PredictedIncrVect = c(),
    tabVariable = NULL,
    tempdata1 = data.frame(),
    tempdata2 = data.frame(),
    data = NULL,
    Original_data.WoSALES = data.frame(),
    influential_data = data.frame(),
    Original_data_Subset = data.frame(),
    Original_Subset_WithSales = data.frame(),
    Results_Data = data.frame(),
    Results_Data_Splitted = data.frame(),
    ResultTable = data.frame(),
    Original_data_Subset.OneModelMethod = data.frame(),
    PredictionProb.CombinedModel = NULL,
    Prediction.CombinedModel = NULL,
    Accuracy.CombinedModel = NULL,
    Results_Data.CombinedModel = data.frame(),
    Results_Data_Splitted.CombinedModel = data.frame(),
    Increment.CombinedModelVect = c(),
    ResultTable.CombinedModel = data.frame()
  )
  
  
  observeEvent(input$SummaryOfData, {
    withBusyIndicatorServer("SummaryOfData", {
      VarList$data <- describe(Original_data())
    })
    
  })
  
  observeEvent(input$CheckforNA, {
    VarList$data <- sapply(Original_data(), function(x)
      sum(is.na(x)))
    output$summary <- renderPrint({
      VarList$data
    })
    
  })
  observeEvent(input$CheckforNAN, {
    VarList$data <- sapply(Original_data(), function(x)
      sum(is.nan(x)))
    output$summary <- renderPrint({
      VarList$data
    })
    
  })
  
  output$summary <- renderPrint({
    if (is.null(VarList$data))
      return()
    VarList$data
  })
  
  
  ShowData <- eventReactive(input$ShowData, {
    Original_data()
    
  })
  observeEvent(input$ShowData, {
    withBusyIndicatorServer("ShowData", {
      # enable tab2 when clicking the button
      js$enableTab("tab2")
      # switch to tab2
      updateTabsetPanel(session, "navbar", "tab2")
      hide(id = "StartupDiv",
           anim = TRUE,
           animType = "fade")
    })
    
  })
  dataModal <- function(failed = FALSE) {
    modalDialog(
      selectInput(
        'NARemOption',
        'Options',
        c(
          Choose = '',
          "Hot-Deck Method" = 1,
          "Remove all missing records" = 0
        ),
        selectize = FALSE
      ),
      span('How to treat the missing records???'),
      if (failed)
        div(tags$b("Invalid selection", style = "color: red;")),
      
      footer = tagList(modalButton("Cancel"),
                       actionButton("okRemoveNA", "OK"))
    )
  }
  observeEvent(input$NARemove, {
    showModal(dataModal())
  })
  observeEvent(input$okRemoveNA, {
    if (input$NARemOption == 1) {
      tempdata <- as.data.frame(VarList$Original_data.WoSALES)
      # VarList$Original_data.WoSALES<- imputation("hotdeck",  D=tempdata, 'RECENCY', Value = 1)
      output$summary <- renderPrint({
        'Missing records imputed using hot deck imputation method!!!'
      })
      removeModal()
    }
    if (input$NARemOption == 0) {
      VarList$Original_data.WoSALES <-
        na.omit(VarList$Original_data.WoSALES)
      output$summary <- renderPrint({
        'Missing records omitted from dataset!!!'
      })
      removeModal()
    }
  })
  output$DataTbl <- DT::renderDataTable({
    DT::datatable(ShowData(), style = 'bootstrap', class = 'table-bordered table-condensed')
  })
  output$VariableList <- renderUI({
    selectInput(
      "VariableList",
      "Select varaibles for processing. Use Ctrl to select multiple!",
      names(Original_data()),
      multiple = TRUE,
      selectize = FALSE,
      size = 25,
      selected = c(
        "CUST_TENURE",
        "RECENCY",
        "ORDER_TOTAL",
        "ORDER_ONLINE",
        "SPEND_CAT1",
        "SPEND_CAT2",
        "SPEND_CAT3",
        "SPEND_CAT4",
        "SPEND_CAT5",
        "AVE_INTV_PURCHASE",
        "LAST_YEAR_SPEND",
        "ITEM_TOTAL",
        "RESPONSE",
        "PROMOTION"
      )
    )
  })
  
  observeEvent(input$OutlierRemove, {
    withBusyIndicatorServer("OutlierRemove", {
      Sys.sleep(3)
      VarList$Original_data.WoSALES <-
        Original_data()[, !(names(Original_data()) %in%
                              "SALES")]
      modelOutlier <-
        glm(
          RESPONSE ~ .,
          family = binomial(link = "logit"),
          data = VarList$Original_data.WoSALES
        )
      
      
      cooksd <- cooks.distance(model = modelOutlier)
      
      output$plotOp <- renderPlot({
        plot(cooksd,
             pch = "*",
             cex = 2,
             main = "Influential Obs by Cooks distance")
        abline(h = 4 * mean(cooksd, na.rm = T), col = "red")  # add cutoff line
      })
      
      influential <-
        as.numeric(names(cooksd)[(cooksd > 4 * mean(cooksd,
                                                    na.rm = T))])
      VarList$influential_data <- (Original_data()[influential, ])
      output$CustomMessageOutlier <- renderUI({
        textOutput("NumOutlier")
      })
      output$NumOutlier <- renderText({
        paste("Number of Influential records detected are:",
              length(influential))
      })
      output$CustomeBtnOutlier <- renderUI({
        fluidRow(
          column(
            4,
            actionButton("RemoveOutliersBtn", "Remove detected Outliers")
          ),
          column(
            4,
            downloadButton("DownloadOutliersBtn", "Download Removed records")
          ),
          column(4,
                 actionButton(
                   "KeepOutliersBtn", "Keep all records"
                 ))
        )
        
        
      })
    })
  })
  observeEvent(input$RemoveOutliersBtn , {
    VarList$Original_data_Subset <-
      VarList$Original_data.WoSALES[!(VarList$Original_data.WoSALES$ID %in% VarList$influential_data$ID), ]
    VarList$Original_Subset_WithSales <-
      Original_data()[!(Original_data()$ID %in% VarList$influential_data$ID), ]
    showModal(
      modalDialog(
        title = "Success",
        "Detected outliers removed successfully!!! You can proceed to next step with data without outliers",
        easyClose = TRUE
      )
    )
    # enable tab2 when clicking the button
    js$enableTab("tab3")
    # switch to tab2
    updateTabsetPanel(session, "navbar", "tab3")
  })
  observeEvent(input$KeepOutliersBtn, {
    VarList$Original_data_Subset <-
      VarList$Original_data.WoSALES
    VarList$Original_Subset_WithSales <-
      Original_data()
    showModal(
      modalDialog(
        title = "Success",
        "Original Data preserved!!!
        You can proceed to next step with Original Data.",
        easyClose = TRUE
        
      )
    )
    # enable tab2 when clicking the button
    js$enableTab("tab3")
    # switch to tab2
    updateTabsetPanel(session, "navbar", "tab3")
  })
  output$DownloadOutliersBtn <- downloadHandler(
    filename = function() {
      paste("Outliers", '.csv', sep = '')
    },
    content = function(file) {
      write.csv(VarList$influential_data, file)
    }
  )
  
  observeEvent(input$VariableSelectBtn, {
    selected.varaibles <- input$VariableList
    
    VarList$Original_data.Selected <-
      VarList$Original_data_Subset[selected.varaibles]
    output$SelectedColumn <- DT::renderDataTable({
      DT::datatable(VarList$Original_data.Selected,
                    style = 'bootstrap',
                    class = 'table-bordered table-condensed')
      
    })
    # enable tab2 when clicking the button
    js$enableTab("tab4")
    # switch to tab2
    updateTabsetPanel(session, "navbar", "tab4")
    
  })
  
  observeEvent(input$CreateTrainTestBtn, {
    set.seed(42)
    trainValue <- as.numeric(input$RadioTrainingPercentage)
    validValue <- 1 - trainValue
    NumberOfRows <- nrow(VarList$Original_data.Selected)
    #get row numbers of training data
    NumRowsTrain <- sample(NumberOfRows, trainValue * NumberOfRows)
    #get row numbers of test/validation data
    NumRowsValid <-
      sample(setdiff(seq_len(NumberOfRows), NumRowsTrain), validValue * NumberOfRows)
    VarList$Training_Data <-
      VarList$Original_data.Selected[NumRowsTrain,]
    
    VarList$Validation_Data <-
      VarList$Original_data.Selected[NumRowsValid,]
    showModal(
      modalDialog(
        title = "Success",
        "Data divided into Training and Validation sets.",
        easyClose = TRUE
      )
    )
    
  })
  observeEvent(input$ShowTrainTestSummryBtn, {
    output$Label1 <- renderPrint("Structure of Training Data Set:")
    output$Summary1 <- renderPrint(str(VarList$Training_Data))
    output$Label2 <-
      renderPrint("Structure of Validation Data Set:")
    output$Summary2 <- renderPrint(str(VarList$Validation_Data))
    
  })
  
  observeEvent(input$CreateTreatmentControlBtn, {
    Treatment_Group_Train = VarList$Training_Data[VarList$Training_Data$PROMOTION == 1, ]
    Control_Group_Train = VarList$Training_Data[VarList$Training_Data$PROMOTION == 0, ]
    
    Treatment_Group_Valid = VarList$Validation_Data[VarList$Validation_Data$PROMOTION == 1, ]
    Control_Group_Valid = VarList$Validation_Data[VarList$Validation_Data$PROMOTION == 0, ]
    
    
    VarList$Treatment_Group = rbind(Treatment_Group_Train, Treatment_Group_Valid)
    VarList$Control_Group = rbind(Control_Group_Valid, Control_Group_Train)
    showModal(
      modalDialog(
        title = "Success",
        "Data divided into Treatment and Control sets.
        Click on respective Summary Button",
        easyClose = TRUE
      )
    )
    # enable tab2 when clicking the button
    js$enableTab("tab5")
    # switch to tab2
    updateTabsetPanel(session, "navbar", "tab5")
  })
  observeEvent(input$ShowControlTreatSummryBtn, {
    output$Label1 <- renderPrint("Structure of Treatment Data Set:")
    output$Summary1 <- renderPrint(str(VarList$Treatment_Group))
    output$Label2 <- renderPrint("Structure of Control Data Set:")
    output$Summary2 <- renderPrint(str(VarList$Control_Group))
  })
  
  observeEvent(input$NIVAnalysisBtn, {
    NIV <- create_infotables(
      data = VarList$Training_Data,
      valid = VarList$Validation_Data,
      y = "RESPONSE",
      trt = "PROMOTION",
      parallel = FALSE,
      bins = 10
    )
    # enable tab2 when clicking the button
    js$enableTab("tab6")
    # switch to tab2
    updateTabsetPanel(session, "navbar", "tab6")
    VarList$NIVVariables <- head(NIV$Summary$Variable)
    output$SelectedVarListNIV <- renderUI(
      checkboxGroupInput(
        "NIVList",
        "Selected Variables after NIV Analysis",
        choices = VarList$NIVVariables,
        selected = VarList$NIVVariables[1:4],
        inline = TRUE
        
        
      )
    )
    
    output$NIVResultLabel <-
      renderUI(helpText("Results of NIV Analysis are dispayed below"),
               tags$hr())
    
    output$SummaryNIV1 <- renderPrint(knitr::kable((NIV$Summary)))
    output$SummaryNIV2 <- renderPrint(knitr::kable(NIV$Tables))
  })
  
  
  observeEvent(input$TwoModelMethod, {
    js$disableTab("Model Diagnosis 2")
    VarList$NIVVariablesUpdated <- input$NIVList
    formulaVar = VarList$NIVVariablesUpdated[1]
    for (a in 2:length(VarList$NIVVariablesUpdated))
    {
      formulaVar <-
        paste(formulaVar , VarList$NIVVariablesUpdated[a] , sep = " + ")
    }
    gsub("varstart + ", "", formulaVar)
    formulaVar <- paste(" RESPONSE ~ ", formulaVar, sep = "")
    LogModel.TreatmentGroup <-
      glm(formulaVar,
          #  get(VarList$NIVVariablesUpdated[1])  + get(VarList$NIVVariablesUpdated[2]) +
          #  get(VarList$NIVVariablesUpdated[3]) + get(VarList$NIVVariablesUpdated[4]),
          family = binomial(link = "logit"),
          data = VarList$Treatment_Group)
    
    
    LogModel.ControlGroup <-
      glm(formulaVar,
          #RESPONSE ~ get(VarList$NIVVariables[1]) + get(VarList$NIVVariables[2]) +
          #  get(VarList$NIVVariables[3]) + get(VarList$NIVVariables[4]),
          family = binomial(link = "logit"),
          data = VarList$Control_Group)
    output$LogisticSummaryLabel1 <-
      renderPrint("Details of Treatment Model:")
    output$LogisticSummary1 <-
      renderPrint(summary(LogModel.TreatmentGroup))
    output$LogisticSummaryLabel2 <-
      renderPrint("Details of Control Model:")
    output$LogisticSummary2 <-
      renderPrint(summary(LogModel.ControlGroup))
    
    VarList$PredictTreatmentGroup <-
      predict(LogModel.TreatmentGroup,
              VarList$Original_data_Subset,
              type = "response")
    VarList$PredictTreatmentGroupResponse <-
      ifelse(VarList$PredictTreatmentGroup > 0.5, 1, 0)
    VarList$misClassErrorTreatment <-
      mean(
        VarList$PredictTreatmentGroupResponse != VarList$Original_data_Subset$RESPONSE
      )
    
    
    VarList$PredictControlGroup <-
      predict(LogModel.ControlGroup,
              VarList$Original_data_Subset,
              type = "response")
    VarList$PredictControlGroupResponse <-
      ifelse(VarList$PredictControlGroup > 0.5, 1, 0)
    VarList$misClassErrorControl <-
      mean(VarList$PredictControlGroupResponse != VarList$Original_data_Subset$RESPONSE)
    VarList$Difference_Score <-
      VarList$PredictTreatmentGroup - VarList$PredictControlGroup
    
    
    
  })
  
  observeEvent(input$ModelDiagTwoModel, {
    VarList$Results_Data <- VarList$Original_data_Subset
    VarList$Results_Data <-
      cbind(
        VarList$Results_Data,
        ResponseTreatmentGroup = VarList[['PredictTreatmentGroupResponse']],
        PredictTreatmentGroup = VarList[['PredictTreatmentGroup']],
        ResponseControlGroup = VarList[['PredictControlGroupResponse']],
        PredictControlGroup = VarList[['PredictControlGroup']],
        Difference_Score = VarList[['Difference_Score']]
      )
    output$ShowResults <-
      renderUI(actionButton("BtnShowResult", "Show Result"))
    output$DownloadResultsRadioUI <-
      renderUI(
        radioButtons(
          "DownloadQuantity",
          label = "Download:",
          choices = list(
            "Download All records" = 1,
            "Top 10%" = 10,
            "Top 20%" = 9,
            "Top 30%" = 8
          ),
          selected = 10
        )
      )
    output$DownloadResultsBtnUI <-
      renderUI(downloadButton("BtnDownloadResults", "Download Results"))
    output$ResultsSummaryHelpText <-
      renderPrint(
        "To constitute a good incremental response model, the top percentiles of actual data should have higher incremental
        rates, and the bottom percentiles should have lower incremental rates. In addition, the rates should decrease
        monotonically from the top to the bottom percentiles."
      )
  })
  
  observeEvent(input$BtnShowResult, {
    VarList$Results_Data <-
      VarList$Results_Data[with(VarList$Results_Data,
                                order(-VarList$Results_Data$Difference_Score)),]
    VarList$Results_Data_Splitted <-
      split(VarList$Results_Data,
            cut2(VarList$Results_Data$Difference_Score, g = 10))
    for (i in length(VarList$Results_Data_Splitted):1)
    {
      VarList$PredictedTreatmentVect <-
        c(VarList$PredictedTreatmentVect ,
          round(
            mean(VarList$Results_Data_Splitted[[i]]$PredictTreatmentGroup),
            digits = 4
          ))
      
      VarList$PredictedControlVect <-
        c(VarList$PredictedControlVect,
          round(
            mean(VarList$Results_Data_Splitted[[i]]$PredictControlGroup),
            digits = 4
          ))
      
      VarList$PredictedIncrVect <-
        c(VarList$PredictedIncrVect,
          round(
            mean(VarList$Results_Data_Splitted[[i]]$Difference_Score),
            digits = 4
          ))
      
      
    }
    # VarList$tempdata1<-merge(VarList$Results_Data_Splitted[[10]],VarList$Results_Data_Splitted[[9]])
    # VarList$tempdata2<-merge(VarList$Results_Data_Splitted[[10]],VarList$Results_Data_Splitted[[9]],VarList$Results_Data_Splitted[[8]])
    
    ResultMatrix <- matrix(
      c(
        c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
        VarList$PredictedTreatmentVect,
        VarList$PredictedControlVect,
        VarList$PredictedIncrVect
      ),
      nrow = 10,
      ncol = 4
    )
    VarList$ResultTable <- data.frame(ResultMatrix)
    colnames(VarList$ResultTable) <-
      c("BinNr",
        "Predicted_Treatment",
        "Predicted_Control",
        "Predicted_Increment")
    output$ResultSummaryTable <- renderTable({
      VarList$ResultTable
    })
    
  })
  
  output$BtnDownloadResults <- downloadHandler(
    filename = function() {
      paste("Selected Customers", '.csv', sep = '')
    },
    content = function(file) {
      tempNr <- as.numeric(input$DownloadQuantity)
      if (tempNr == 1) {
        write.csv(VarList$Results_Data, file)
      }
      if (tempNr == 10) {
        write.csv(VarList$Results_Data_Splitted[[10]], file)
      }
      if (tempNr == 9) {
        write.csv(
          rbind(
            VarList$Results_Data_Splitted[[10]],
            VarList$Results_Data_Splitted[[9]]
          ),
          file
        )
      }
      if (tempNr == 8) {
        write.csv(
          rbind(
            VarList$Results_Data_Splitted[[10]],
            VarList$Results_Data_Splitted[[9]],
            VarList$Results_Data_Splitted[[8]]
          ),
          file
        )
      }
    }
  )
  output$ResultPlot <- renderPlot({
    ggplot(data = VarList$ResultTable, aes(BinNr, Predicted_Increment)) + geom_point(color = 'red', size = 3) + geom_smooth(method = 'lm')
    
  })
  
  observeEvent(input$OneModelMethod, {
    js$disableTab("Model Diagnosis 1")
    VarList$Original_data_Subset.OneModelMethod <-
      cbind(
        VarList$Original_data_Subset,
        IndicatorVar = rep(VarList$Original_data_Subset$PROMOTION)
      )
    VarList$Original_data_Subset.OneModelMethod$AVE_INTV_PURCHASE.New =  VarList$Original_data_Subset.OneModelMethod$AVE_INTV_PURCHASE * VarList$Original_data_Subset.OneModelMethod$IndicatorVar
    
    
    VarList$Original_data_Subset.OneModelMethod$CUST_TENURE.New =  VarList$Original_data_Subset.OneModelMethod$CUST_TENURE * VarList$Original_data_Subset.OneModelMethod$IndicatorVar
    
    
    VarList$Original_data_Subset.OneModelMethod$RECENCY.New =  VarList$Original_data_Subset.OneModelMethod$RECENCY * VarList$Original_data_Subset.OneModelMethod$IndicatorVar
    
    
    VarList$Original_data_Subset.OneModelMethod$ORDER_TOTAL.New =  VarList$Original_data_Subset.OneModelMethod$ORDER_TOTAL * VarList$Original_data_Subset.OneModelMethod$IndicatorVar
    
    Combined.Model <- glm(
      RESPONSE ~ AVE_INTV_PURCHASE +  CUST_TENURE +  RECENCY + ORDER_TOTAL + IndicatorVar +
        AVE_INTV_PURCHASE.New + CUST_TENURE.New + RECENCY.New + ORDER_TOTAL.New,
      family = binomial(link = "logit"),
      data = VarList$Original_data_Subset.OneModelMethod
    )
    
    output$LogisticSummaryLabel1 <-
      renderPrint("Details of Model built:")
    output$LogisticSummary1 <-
      renderPrint(summary(Combined.Model))
    output$LogisticSummaryLabel2 <-
      renderPrint(" ")
    output$LogisticSummary2 <-
      renderPrint(" ")
    
    VarList$PredictionProb.CombinedModel <-
      predict(Combined.Model,
              VarList$Original_data_Subset.OneModelMethod,
              type = "response")
    VarList$Prediction.CombinedModel <-
      ifelse(VarList$PredictionProb.CombinedModel > 0.5, 1, 0)
    VarList$Accuracy.CombinedModel <-
      1 -  mean(
        VarList$Prediction.CombinedModel != VarList$Original_data_Subset.OneModelMethod$RESPONSE
      )
    
    VarList$Original_data_Subset.OneModelMethod <-
      cbind(
        VarList$Original_data_Subset.OneModelMethod,
        VarList$PredictionProb.CombinedModel,
        VarList$Prediction.CombinedModel
      )
    
    VarList$Original_data_Subset.OneModelMethod$DiffScore.CombinedModel <-
      Combined.Model$coefficients[["IndicatorVar"]] + (
        VarList$Original_data_Subset.OneModelMethod$AVE_INTV_PURCHASE.New * Combined.Model$coefficients[["AVE_INTV_PURCHASE.New"]]
      ) + (
        VarList$Original_data_Subset.OneModelMethod$CUST_TENURE.New * Combined.Model$coefficients[["CUST_TENURE.New"]]
      ) + (
        VarList$Original_data_Subset.OneModelMethod$RECENCY.New * Combined.Model$coefficients[["RECENCY.New"]]
      ) + (
        VarList$Original_data_Subset.OneModelMethod$ORDER_TOTAL.New * Combined.Model$coefficients[["ORDER_TOTAL.New"]]
      )
    
    
    
  })
  observeEvent(input$ModelDiagOneModel, {
    output$ShowResultsCombinedModel <-
      renderUI(actionButton("BtnShowResultCombinedModel", "Show Result"))
    output$DownloadResultsRadioUICombinedModel <-
      renderUI(
        radioButtons(
          "DownloadQuantityCombinedModel",
          label = "Download:",
          choices = list(
            "Download All records" = 1,
            "Top 10%" = 10,
            "Top 20%" = 9,
            "Top 30%" = 8
          ),
          selected = 10
        )
      )
    output$DownloadResultsBtnUICombinedModel <-
      renderUI(downloadButton("BtnDownloadResultsCombinedModel", "Download Results"))
    
  })
  
  observeEvent(input$BtnShowResultCombinedModel, {
    VarList$Results_Data.CombinedModel <-
      VarList$Original_data_Subset.OneModelMethod[order(-VarList$Original_data_Subset.OneModelMethod$DiffScore.CombinedModel), ]
    
    VarList$Results_Data_Splitted.CombinedModel <-
      split(
        VarList$Results_Data.CombinedModel,
        cut2(
          VarList$Results_Data.CombinedModel$DiffScore.CombinedModel,
          g = 10
        )
      )
    for (i in length(VarList$Results_Data_Splitted.CombinedModel):1)
    {
      VarList$Increment.CombinedModelVect <-
        c(VarList$Increment.CombinedModelVect,
          round(
            mean(
              VarList$Results_Data_Splitted.CombinedModel[[i]]$DiffScore.CombinedModel
            ),
            digits = 4
          ))
      
      
    }
    # VarList$tempdata1<-merge(VarList$Results_Data_Splitted[[10]],VarList$Results_Data_Splitted[[9]])
    # VarList$tempdata2<-merge(VarList$Results_Data_Splitted[[10]],VarList$Results_Data_Splitted[[9]],VarList$Results_Data_Splitted[[8]])
    
    ResultMatrix.CombinedModel <- matrix(
      c(c(
        1:length(VarList$Results_Data_Splitted.CombinedModel)
      ),
      VarList$Increment.CombinedModelVect),
      nrow = length(VarList$Results_Data_Splitted.CombinedModel),
      ncol = 2
    )
    VarList$ResultTable.CombinedModel <-
      data.frame(ResultMatrix.CombinedModel)
    colnames(VarList$ResultTable.CombinedModel) <-
      c("BinNr",
        "Predicted_Increment")
    output$ResultSummaryTableCombinedModel <- renderTable({
      VarList$ResultTable.CombinedModel
    })
    
  })
  
  output$BtnDownloadResultsCombinedModel <- downloadHandler(
    filename = function() {
      paste("Selected Customers", '.csv', sep = '')
    },
    content = function(file) {
      tempNr <- as.numeric(input$DownloadQuantityCombinedModel)
      id <- length(VarList$Results_Data_Splitted.CombinedModel)
      if (tempNr == 1) {
        write.csv(VarList$Results_Data.CombinedModel, file)
      }
      if (tempNr == 10) {
        write.csv(VarList$Results_Data_Splitted.CombinedModel[[id]], file)
      }
      if (tempNr == 9) {
        write.csv(
          rbind(
            VarList$Results_Data_Splitted.CombinedModel[[id]],
            VarList$Results_Data_Splitted.CombinedModel[[id - 1]]
          ),
          file
        )
      }
      if (tempNr == 8) {
        write.csv(
          rbind(
            VarList$Results_Data_Splitted.CombinedModel[[id]],
            VarList$Results_Data_Splitted.CombinedModel[[id - 1]],
            VarList$Results_Data_Splitted.CombinedModel[[id - 2]]
          ),
          file
        )
      }
    }
  )
  output$ResultPlotCombinedModel <- renderPlot({
    ggplot(data = VarList$ResultTable.CombinedModel,
           aes(BinNr, Predicted_Increment)) + geom_point(color = 'red', size = 3) + geom_smooth(method = 'lm')
    
  })
  }
