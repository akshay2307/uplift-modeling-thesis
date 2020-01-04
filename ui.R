library(shiny)
library(haven)
library(DT)
library(Hmisc)
# library(xlsx)
library(Information)
library(htmlTable)
library(Matrix)
library(shinythemes)
library(ggplot2)
library(shinyjs)
# library(rminer)

source("helpers.R")
jscode <- "
shinyjs.disableTab = function(name) {
var tab = $('.nav li a[data-value=' + name + ']');
tab.bind('click.tab', function(e) {
e.preventDefault();
return false;
});
tab.addClass('disabled');
}

shinyjs.enableTab = function(name) {
var tab = $('.nav li a[data-value=' + name + ']');
tab.unbind('click.tab');
tab.removeClass('disabled');
}
"

navbarcss <- "
.nav li a.disabled {
background-color: #333 !important;
color: #eee !important;
cursor: not-allowed !important;
border-color: #333 !important;
}"

Errorcss <- "
.shiny-output-error { visibility: hidden; }
.shiny-output-error:before {
visibility: hidden;
content: 'An error occurred'; }
}
"
containerCSS <- "
#MP1{
min-height:620px;
}
"
fluidPage(
  theme = shinytheme("darkly"),
  #tags$head(tags$style(HTML(mycss))),
  useShinyjs(),
  extendShinyjs(text = jscode),
  inlineCSS(navbarcss),
  inlineCSS(Errorcss),
  inlineCSS(containerCSS),
  navbarPage(
    position = "static-top",
    title = "Uplift Model",
    windowTitle = "Uplift Modeling",
    tabPanel(
      "Select and View Data",
      value = "tab1",
      sidebarLayout(
        sidebarPanel(
          fileInput('chooseData', 'Choose file to upload'),
          checkboxInput('header', 'Header', TRUE),
          fluidRow(column(
            6,
            radioButtons('sep', 'Separator',
                         c(
                           Comma = ',',
                           Semicolon = ';',
                           Tab = '\t'
                         ),
                         ',')
          ),
          column(
            6,
            radioButtons(
              'fileType',
              'File Type',
              c('SAS7BDAT' = 'sas7bdat',
                'CSV' = "csv"),
              'sas7bdat'
            )
          )),
          
          withBusyIndicatorUI(actionButton("ShowData", "View Data", class = "btn btn-primary")),
          
          tags$hr()
          
        ),
        mainPanel(
          id = "MP1",
          DT::dataTableOutput("DataTbl")
          ,
          div(
            id = "StartupDiv",
            tags$h3('How to use this WebApp???'),
            tags$br(),
            img(src = 'Startup Diagram5.jpg')
          )
          
        )
      ),
      
      HTML(
        '<footer class="text-center">
        <p class="txt-railway"> Master Thesis on Incremental Response Modelling by Akshay Laddha</p>
        </footer>'
      )
      ),
    tabPanel(
      "Data Pre-Processing",
      value = "tab2",
      fluidRow(
        column(
          2,
          actionButton("CheckforNA", "Check for NAs", class = "btn btn-primary")
        ),
        column(
          2,
          actionButton("NARemove", "Process NAs", class = "btn btn-primary")
        ),
        column(2,
               helpText("Check for Other Inconsistancy")),
        column(
          2,
          actionButton("CheckforNAN", "Click here", class = "btn btn-primary")
        ),
        column(2,
               withBusyIndicatorUI(
                 actionButton("SummaryOfData", "Show Summary", class = "btn btn-primary")
               )),
        column(2,
               withBusyIndicatorUI(
                 actionButton("OutlierRemove", "Detect Outliers", class = "btn btn-primary")
               ))
        
      ),
      
      tags$hr()
      ,
      fluidRow(
        column(6, verbatimTextOutput('summary')),
        column(
          6,
          plotOutput('plotOp'),
          tags$br(),
          uiOutput("CustomMessageOutlier"),
          tags$br(),
          uiOutput("CustomeBtnOutlier")
        )
      )
    ),
    tabPanel(
      "Variable Pre Screening",
      value = "tab3",
      sidebarLayout(
        sidebarPanel(fluidRow(uiOutput("VariableList"))),
        mainPanel(
          id = "MP2",
          actionButton("VariableSelectBtn", "Select Varaibles", class = "btn btn-primary"),
          DT::dataTableOutput("SelectedColumn"),
          uiOutput("VarSelection")
        )
      )
      
      
    ),
    tabPanel(
      "Data Preparation",
      value = "tab4",
      sidebarLayout(
        sidebarPanel(
          fluidRow(
            radioButtons(
              "RadioTrainingPercentage",
              label =  h3("Select Training and Validation Data percents:"),
              choices = list(
                "80% Training and 20% Validation" = 0.8,
                "70% Training and 30% Validation" = 0.7,
                "60% Training and 40% Validation" = 0.6,
                "50% Training and 50% Validation" = 0.5
              ),
              selected = 0.7
            )
            
          ),
          tags$hr(),
          helpText("Prepare Training and Validation Sets of Data"),
          fluidRow(column(
            6,
            actionButton("CreateTrainTestBtn", "Click Here", class = "btn btn-primary")
          ),
          column(
            6,
            actionButton("ShowTrainTestSummryBtn", "Summary", class = "btn btn-primary")
          )),
          tags$hr(),
          helpText("Create Control and Treatment sets of Data"),
          fluidRow(column(
            6,
            actionButton("CreateTreatmentControlBtn", "Click Here", class = "btn btn-primary")
          ),
          column(
            6,
            actionButton("ShowControlTreatSummryBtn", "Summary", class = "btn btn-primary")
          ))
          
        ),
        mainPanel(id = "MP3", fluidRow(
          column(12, textOutput("Label1")),
          column(12, verbatimTextOutput("Summary1")),
          column(12, textOutput("Label2")),
          column(12, verbatimTextOutput("Summary2"))
        ))
      )
    ),
    tabPanel(
      "Variable Selection for Modelling",
      value = "tab5",
      fluidRow(column(
        12,
        helpText(
          "Click here to perform NIV method to find out most effective variables for Modelling"
        )
      )),
      fluidRow(column(
        4,
        actionButton("NIVAnalysisBtn", "Perform Analysis", class = "btn btn-primary")
      ),
      column(8, uiOutput(
        "SelectedVarListNIV"
      ))),
      fluidRow(
        column(12, uiOutput("NIVResultLabel")),
        column(12, verbatimTextOutput("SummaryNIV1")),
        column(12, verbatimTextOutput("SummaryNIV2"))
      )
    ),
    tabPanel(
      "Incremental Response Modelling",
      value = "tab6",
      sidebarLayout(
        sidebarPanel(
          helpText("Perform IR Modelling using two model approach"),
          actionButton("TwoModelMethod", "Click here", class = "btn btn-primary"),
          helpText("Perform IR Modelling using One model approach"),
          actionButton("OneModelMethod", "Click here", class = "btn btn-primary")
        ),
        mainPanel(id = "MP4", tabsetPanel(
          tabPanel(
            "Summary",
            textOutput("LogisticSummaryLabel1"),
            verbatimTextOutput("LogisticSummary1"),
            textOutput("LogisticSummaryLabel2"),
            verbatimTextOutput("LogisticSummary2")
          ),
          tabPanel(
            "Model Diagnosis for Two Model Approach",
            sidebarLayout(
              sidebarPanel(
                tags$br(),
                actionButton("ModelDiagTwoModel", "Show Model Diagnosis", class =
                               "btn btn-primary"),
                tags$hr(),
                uiOutput("ShowResults"),
                tags$br(),
                uiOutput("DownloadResultsRadioUI"),
                tags$br(),
                uiOutput("DownloadResultsBtnUI")
              ),
              mainPanel(
                tags$br(),
                tableOutput("ResultSummaryTable"),
                helpText(
                  "*** To constitute a good incremental response model, the top percentiles of actual data should have higher incremental
                  rates, and the bottom percentiles should have lower incremental rates. In addition, the rates should decrease
                  monotonically from the top to the bottom percentiles."
                ),
                plotOutput("ResultPlot")
                )
              )
            ),
          tabPanel(
            "Model Diagnosis for One Model Approach",
            sidebarLayout(
              sidebarPanel(
                tags$br(),
                actionButton("ModelDiagOneModel", "Show Model Diagnosis", class =
                               "btn btn-primary"),
                tags$hr(),
                uiOutput("ShowResultsCombinedModel"),
                tags$br(),
                uiOutput("DownloadResultsRadioUICombinedModel"),
                tags$br(),
                uiOutput("DownloadResultsBtnUICombinedModel")
              ),
              mainPanel(
                tags$br(),
                tableOutput("ResultSummaryTableCombinedModel"),
                helpText(
                  "*** To constitute a good incremental response model, the top percentiles of actual data should have higher incremental
                  rates, and the bottom percentiles should have lower incremental rates. In addition, the rates should decrease
                  monotonically from the top to the bottom percentiles."
                ),
                plotOutput("ResultPlotCombinedModel")
                
                )
              )
            
          )
          
          ))
            )
        )
    ,
    tabPanel(
      "How to use this WebApp?"
      ,
      value = "tab7",
      div(
        id = "HowtoUseAppDiv",
        tags$h3('How to use this WebApp???'),
        tags$hr(),
        img(src = 'Startup Diagram5.jpg')
      )
    )
    
    )
    )
