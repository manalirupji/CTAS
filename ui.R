library(shiny)
library(DT)
library(openxlsx)
library(dplyr)
library(tidyr)
library(plotly)
library(reshape2)
library(HH)
library(colourpicker)
library(tidyverse)
library(knitr)
library(colourpicker)
library(ggplot2)
library(data.table)
library(survminer)
library(gridExtra)
library(swimplot)
library(RColorBrewer)
library(grDevices)


ui <- fluidPage(
  navbarPage("Clinical Trial Analysis Suite (CTAS)",
             tabPanel("Adverse Event Reporting",
                      fluidRow(
                        column(2,
                               conditionalPanel("input.cPanels1 == 1", 
                               wellPanel(
                                 h4(strong("Input your file")),
                                 selectInput("file1",label= "Select an example ds or upload your own with 'Load my own'", 
                                             choices = c("Example ds File"="Example", "Load my own data" = "load_my_own1")),
                                 conditionalPanel("input.file1 == 'load_my_own1'",
                                                  fileInput('file11', 'Choose file to upload (maximum size 500 MB).', 
                                                            accept=c('.xlsx','text/csv', 'text/comma-separated-values,text/plain', '.csv'))),
                                 conditionalPanel("input.file1 == 'Example'",
                                                  downloadButton('downloadEx1', 'Download Example data')
                                 ))),
                               
                               conditionalPanel("input.cPanels1 == 1", 
                                                wellPanel(
                                                  h4(strong("Input Demo file")),
                                                  selectInput("file_demo",label= "Select an example demo ds or upload your own with 'Load my own'", 
                                                              choices = c("Example ds File"="Example_demo", "Load my own data" = "load_my_own_demo")),
                                                  conditionalPanel("input.file_demo == 'load_my_own_demo'",
                                                                   fileInput('file_demo1', 'Choose file to upload (maximum size 500 MB).', 
                                                                             accept=c('.xlsx','text/csv', 'text/comma-separated-values,text/plain', '.csv'))),
                                                  selectInput("select_id", "Select a ID Variable", choices=c("SEQUENCE_NO_")),
                                                  selectInput("select_arms", "Select a Treatment/Arms (Group) Variable", choices=c("ARMS")),
                                                  conditionalPanel("input.file_demo == 'Example_demo'",
                                                                   downloadButton('downloadEx_demo', 'Download Example Demo data')
                                                  ))),
                              
                                conditionalPanel("input.cPanels1 == 2",
                                                 h4(strong("AE table specifications")),
                                                wellPanel(
                                                  h4(strong("Select Variables")),
                                                  selectInput("select11", "Select a ID Variable", 
                                                              choices=c("SEQUENCE_NO")),
                                                  selectInput("select12", "Select a Treatment (Group) Variable", 
                                                              choices=c("ARMS")),
                                                  selectInput("select13", "Select a Preferred Term Variable (Toxicity)", 
                                                              choices=c("AETERMC")),
                                                  selectInput("select14", "Select a System Organ Class (SoC) Variable", 
                                                              choices=c("SOC")),
                                                  selectInput("select15", "Select a Grade Variable", 
                                                              choices=c("AETOXGR_N")),
                                                  checkboxInput("bySOC", "By Organ Class ?", TRUE),
                                                  conditionalPanel("input.bySOC == 0",
                                                          checkboxInput("topdisplay", "Display top AEs ?", FALSE),
                                                          conditionalPanel("input.topdisplay == 1",
                                                          numericInput("top", label = "Display top 'n' AEs", value = "15")))
                                                  )),
                               
                               conditionalPanel("input.cPanels1 == 3", 
                                                h4(strong("Volcano plot specifications")),
                                                wellPanel(
                                                  sliderInput("pval_cutoff", "Annotate AEs that are significant at alpha (default <= 0.1)", min = 0, max = 0.99, value = 0.1)
                                                ),
                                                h4(strong("Dot plot specifications")),
                                                wellPanel(
                                                  checkboxInput("bySOC1", "By Organ Class ?", TRUE),
                                                  conditionalPanel("input.bySOC1 == 1",
                                                     # selectInput("show_vars1", "Select Single or mutliple SOCs to generate dot plot", 
                                                      #        c("Blood and lymphatic system disorders", "Investigations"), choices=c("Blood and lymphatic system disorders", "Investigations"), multiple = TRUE))
                                                  uiOutput('ref1')),
                                                  conditionalPanel("input.bySOC1 == 0",
                                                                   checkboxInput("topdisplay1", "Display top AEs ?", FALSE),
                                                                   conditionalPanel("input.topdisplay1 == 1",
                                                                                    numericInput("top1", label = "Display top 'n' AEs", value = "10"),
                                                                                    checkboxInput("orderbyB", "Top AEs based on higher difference in events between Arms B and A, ordered by Arm B", TRUE)))
                                                  
                                                  
                                                )),
                              
                              conditionalPanel("input.cPanels1 == 2",
                                                h4(strong("Downloads")),
                                                wellPanel(
                                                  textInput("fname21", "Type the file name you would like to save as", value = "AETable"),
                                                  downloadButton('downloadAE', 'Download AE Table')
                                                )),
                               conditionalPanel("input.cPanels1 == 3",
                                                h4(strong("Downloads")),
                                                wellPanel(
                                                  textInput("fname22", "Type the project name you would like to save as (name will be ammended to each plot)", value = "AEPlots"),
                                                  downloadButton('x3', 'Dot Plot')
                                                ))
                             ),
                        
                        
                        column(10,
                               tabsetPanel(
                                 tabPanel("Data", DT::dataTableOutput("out1"), htmlOutput("distribution"), value = 1),
                                 tabPanel("AE Table", htmlOutput("wrongdata"),  DT::dataTableOutput("out2"), value =2),
                                 tabPanel("AE Plots", htmlOutput("heading1"), plotlyOutput("VolcanoPlot", height= 600, width = 800),
                                          htmlOutput("heading2"), plotOutput("DotPlot", height= 600, width = 800), value = 3),
                                 id = "cPanels1"
                               )),                              
                       column(12,
                                tags$head(tags$style(type="text/css", "
                                                    #loadmessage {
                                                    position: fixed;
                                                    bottom: 0px;
                                                    right: 0px;
                                                    width: 100%;
                                                    padding: 5px 0px 5px 0px;
                                                    text-align: center;
                                                    font-weight: bold;
                                                    font-size: 100%;
                                                    color: #000000;
                                                    background-color: #b8b8b8;
                                                    z-index: 105;
                                                    }
                                                    ")),
                                conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                       tags$div("Loading...",id="loadmessage"))
                                      
                               )
                        )),
                    tabPanel("Response Related Plots",
                             fluidRow(
                               column(2,
                                      conditionalPanel("input.cPanels2 == 1", 
                                                       wellPanel(
                                                         h4(strong("Input your file")),
                                                         selectInput("file21",label= "Select an example ds or upload your own with 'Load my own'", 
                                                                     choices = c("Example ds File"="Example2", "Load my own data" = "load_my_own2")),
                                                         conditionalPanel("input.file21 == 'load_my_own2'",
                                                                          fileInput('file211', 'Choose file to upload (maximum size 500 MB).', 
                                                                                    accept=c('.xlsx','text/csv', 'text/comma-separated-values,text/plain', '.csv'))),
                                                         conditionalPanel("input.file21 == 'Example2'",
                                                                          downloadButton('downloadEx21', 'Download Example data')
                                                         ))),
                                      conditionalPanel("input.cPanels2 == 1",
                                                       wellPanel(
                                                         h4(strong("Select Variables")),
                                                         selectInput("select21", "Select a Treatment (Group) Variable", 
                                                                     choices=c("treatment")),
                                                         selectInput("select22", "Select a Response Variable", 
                                                                     choices=c("bor_N")),
                                                         selectInput("select23", "Select Measured Change Variable", 
                                                                     choices=c("br_change"))
                                                         
                                                         
                                                       ),
                                                       wellPanel(
                                                         h4(strong("Plot specifications")),
                                                         numericInput("bar_ylim_low", label = "Lower Y-axis limit", value = "-70"),
                                                         numericInput("bar_ylim_high", label = "Upper Y-axis limit", value = "80"),
                                                         checkboxInput("displayPR", "Display horizontal PR line", TRUE),
                                                         conditionalPanel("input.displayPR == 1",
                                                             numericInput("abline_low", label = "PR line (negative horizontal line)", value = "-30")),
                                                         checkboxInput("displayPD", "Display horizontal PD line", TRUE),
                                                         conditionalPanel("input.displayPD == 1",
                                                             numericInput("abline_high", label = "PD line (positive horizontal line)", value = "20")),
                                                         checkboxInput("displayTrt", "Display Treatment labels", TRUE),
                                                         h4("Best Response status colors"),
                                                         colourpicker::colourInput("cc_CR", "CR", "green", returnName = TRUE, palette = "limited", showColour = "background"),
                                                         colourpicker::colourInput("cc_PR", "PR", "blue", returnName = TRUE, palette = "limited", showColour = "background"),
                                                         colourpicker::colourInput("cc_SD", "SD", "yellow", returnName = TRUE, palette = "limited", showColour = "background"),
                                                         colourpicker::colourInput("cc_PD", "PD", "red", returnName = TRUE, palette = "limited", showColour = "background")
                                                         
                                                         
                                                       )),
                                      conditionalPanel("input.cPanels2 == 1",
                                                       h4(strong("Downloads")),
                                                       wellPanel(
                                                         textInput("fname24", "Type the file name you would like to save as (name will be ammended to each plot)", value = "WaterfallPlot"),
                                                         downloadButton('x24', 'Download WaterFall Plot')
                                                         
                                                       )),
                                      conditionalPanel("input.cPanels2 == 2", 
                                                       wellPanel(
                                                         h4(strong("Input your file")),
                                                         selectInput("file22",label= "Select an example ds or upload your own with 'Load my own'", 
                                                                     choices = c("Example ds File"="Example22", "Load my own data" = "load_my_own22")),
                                                         conditionalPanel("input.file22 == 'load_my_own22'",
                                                                          fileInput('file221', 'Choose file to upload (maximum size 500 MB). This data should ordered by date.', 
                                                                                    accept=c('.xlsx','text/csv', 'text/comma-separated-values,text/plain', '.csv'))),
                                                         conditionalPanel("input.file22 == 'Example22'",
                                                                          downloadButton('downloadEx22', 'Download Example data')
                                                         ))),
                                      conditionalPanel("input.cPanels2 == 2",
                                                       wellPanel(
                                                         h4(strong("Select Variables")),
                                                         selectInput("select31", "Select a ID Variable", 
                                                                     choices=c("sequence_no_")),
                                                         selectInput("select32", "Select Measured Change Variable", 
                                                                     choices=c("Change.From.Baseline")),
                                                         checkboxInput("time_avail", "Is time variable (x-axis) in data?", FALSE),
                                                         conditionalPanel("input.time_avail == 1",
                                                                    selectInput("select33", "Select Time Variable", choices=c("weeks"))),
                                                         conditionalPanel("input.time_avail == 0",
                                                                          selectInput("select35", "Select variable from which time variable should be calculated", choices=c("Study.Date"))),
                                                         checkboxInput("group_avail", "By group?", TRUE),
                                                         conditionalPanel("input.group_avail == 1",
                                                                    selectInput("select34", "Select a Group Variable", 
                                                                     choices=c("ras_status")))
                                                       ),
                                                       wellPanel(
                                                         h4(strong("Spider Plot specifications")),
                                                         textInput("xlab1", "X axis label", value = "Time (months)"),
                                                         textInput("ylab1", "X axis label", value = "% Change from Baseline"),
                                                         conditionalPanel("input.group_avail == 1",
                                                             textInput("legend.lab", "Legend label", value = "Ras mutation")),
                                                         checkboxInput("displayPR1", "Display horizontal PR line", TRUE),
                                                         conditionalPanel("input.displayPR1 == 1",
                                                                          numericInput("abline_low1", label = "PR line (negative horizontal line)", value = "-30")),
                                                         checkboxInput("displayPD1", "Display horizontal PD line", TRUE),
                                                         conditionalPanel("input.displayPD1 == 1",
                                                                          numericInput("abline_high1", label = "PD line (positive horizontal line)", value = "20")),
                                                         
                                                         )),
                                      
                                      conditionalPanel("input.cPanels2 == 3", 
                                                       wellPanel(
                                                         h4(strong("Input your file")),
                                                         selectInput("file32",label= "Select an example ds or upload your own with 'Load my own'", 
                                                                     choices = c("Example ds File"="Example32", "Load my own data" = "load_my_own32")),
                                                         conditionalPanel("input.file32 == 'load_my_own32'",
                                                                          fileInput('file321', 'Choose file to upload (maximum size 500 MB). This data should ordered by date.', 
                                                                                    accept=c('.xlsx','text/csv', 'text/comma-separated-values,text/plain', '.csv'))),
                                                         conditionalPanel("input.file32 == 'Example32'",
                                                                          downloadButton('downloadEx32', 'Download Example data')
                                                         ))),
                                      conditionalPanel("input.cPanels2 == 3",
                                                       wellPanel(
                                                         h4(strong("Select Variables")),
                                                         selectInput("select51", "Select a ID Variable", choices=c("subij")),
                                                         selectInput("select52", "First Partial Response Variable", choices=c("firstPR")),
                                                         selectInput("select53", "First Partial Response Time Variable", choices=c("firstPR_time")),
                                                         selectInput("select54", "First Complete Response Variable", choices=c("firstCR")),
                                                         selectInput("select55", "First Complete Response Time Variable", choices=c("firstCR_time")),
                                                         selectInput("select56", "First Stable Disease Variable", choices=c("firstSD")),
                                                         selectInput("select57", "First Stable Disease Time Variable", choices=c("firstSD_time")),
                                                         selectInput("select58", "First Progressive Disease Variable", choices=c("firstPD")),
                                                         selectInput("select59", "First Progressive Disease Time Variable", choices=c("firstPD_time")),
                                                         selectInput("select60", "Death Variable", choices=c("OS_censor")),
                                                         selectInput("select61", "Time to Death Variable", choices=c("OS")),
                                                         selectInput("select62", "Time Start Variable", choices=c("trtstart")),
                                                         selectInput("select63", "Time to treatment End", choices=c("trtend")),
                                                         selectInput("select63_2", "Treatment Stop Indicator", choices=c("trtcap")),
                                                         checkboxInput("trt_avail", "Is treatment variable (or variable to differenciate samples) in data?", TRUE),
                                                         conditionalPanel("input.trt_avail == 1",
                                                                          selectInput("select64", "Select Treatment Variable", choices=c("hpvcat"))),
                                                         selectInput("select65", "Follow-up Start Variable", choices=c("fustart")),
                                                         selectInput("select66", "Follow-up End Variable", choices=c("fuend")),
                                                         selectInput("select67", "Durable Responder Variable", choices=c("DR")),
                                                         selectInput("select68", "Continued Response Variable", choices = c("Continued_Resp"))
                                                         
                                                         
                                                      ) 
                                                       ),
                                      conditionalPanel("input.cPanels2 == 2",
                                                       h4(strong("Downloads")),
                                                       wellPanel(
                                                         textInput("fname32", "Type the file name you would like to save as (name will be ammended to each plot)", value = "SpiderPlot"),
                                                         downloadButton('x32', 'Download Spider Plot')
                                                         
                                                       )),
                                      conditionalPanel("input.cPanels2 == 3",
                                                       h4(strong("Downloads")),
                                                       wellPanel(
                                                         textInput("fname33", "Type the file name you would like to save as (name will be ammended to each plot)", value = "SwimmerPlot"),
                                                         downloadButton('x33', 'Download Swimmer Plot')
                                                         
                                                       ))
                                    
                                      
                                      
                                      
                                      ),
                               column(8,
                                      tabsetPanel(
                                        tabPanel("Waterfall Plot", plotOutput("WaterFallPlot", height= 600, width = 600),  value =1),
                                        tabPanel("Spider Plots", plotOutput("SpiderPlot", height= 600, width = 900), value = 2),
                                        tabPanel("Swimmer Plots", plotOutput("SwimmerPlot", height= 700, width = 900), value = 3),
                                        id = "cPanels2"
                                      )),   
                               column(2, 
                                      conditionalPanel("input.cPanels2 == 3",
                                                       wellPanel(
                                                         h4(strong("Swimmer Plot specifications")),
                                                         checkboxInput("display_subj", "Display subject ID in swimmer plot?", TRUE),
                                                         textInput("xlab2", "X axis label", value = "Time from Treatment Initiation (Months)"),
                                                         textInput("ylab2", "Y axis label", value = "Subjects Recieved Study Drug"),
                                                         conditionalPanel("input.trt_avail == 1",
                                                                          textInput("legend.lab2", "Legend label", value = "HPV status")),
                                                         checkboxInput("displayTStop", "Display Treatment Stop status", TRUE),
                                                         checkboxInput("displayPResp", "Display Partial response status", TRUE),
                                                         checkboxInput("displaySDis", "Display Stable disease status", TRUE),
                                                         checkboxInput("displayPDis", "Display Progressive disease status", TRUE),
                                                         checkboxInput("displayCResp", "Display Complete response status", TRUE),
                                                         checkboxInput("displayDeath", "Display Death status", TRUE),
                                                         checkboxInput("displayDR", "Display Durable Responder status", TRUE),
                                                         checkboxInput("displayContResp", "Display Continued Response status", TRUE)
                                                         
                                                         
                                                         
                                                       ))
                               ),
                               column(12,
                                             tags$head(tags$style(type="text/css", "
                                                    #loadmessage {
                                                    position: fixed;
                                                    bottom: 0px;
                                                    right: 0px;
                                                    width: 100%;
                                                    padding: 5px 0px 5px 0px;
                                                    text-align: center;
                                                    font-weight: bold;
                                                    font-size: 100%;
                                                    color: #000000;
                                                    background-color: #b8b8b8;
                                                    z-index: 105;
                                                    }
                                                    ")),
                                             conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                              tags$div("Loading...",id="loadmessage"))
                                             
                                      ))),
             tabPanel("Survival",
                      fluidRow(
                        column(2,
                               conditionalPanel("input.cPanels3 == 1",
                               wellPanel(
                                 h4(strong("Input your file")),
                                 selectInput("file42",label= "Select an example ds or upload your own with 'Load my own'", 
                                             choices = c("Example ds File"="Example42", "Load my own data" = "load_my_own42")),
                                 conditionalPanel("input.file42 == 'load_my_own42'",
                                                  fileInput('file422', 'Choose file to upload (maximum size 500 MB).', accept=c('.xlsx','text/csv', 'text/comma-separated-values,text/plain', '.csv'))),
                                 conditionalPanel("input.file42 == 'Example42'",
                                                  downloadButton('downloadEx42', 'Download Example data')
                                 ))),
                               conditionalPanel("input.cPanels3 == 1",
                                                wellPanel(
                                                  h4(strong("KM Analysis")),
                                                  selectInput("select41", "Select a Variable of Interest as Cohort Group", 
                                                              choices=c("ARMS")),
                                                  selectInput("binary",label= "The variable of interest is categorical or continuous", 
                                                              choices = c("Categorical Variable" = "categorical", "Continuous Variable"="continuous"), selected= "categorical"),
                                                  conditionalPanel("input.binary == 'continuous'",
                                                                   radioButtons("cutoff2", "Choose Cutoff Point for Continuous Variable:", list("Optimal Cutoff" = 1,"25th Percentile" = 25,"50th Percentile" = 50, "75th Percentile" = 75), selected = 25)),
                                                  selectInput("select42", "Select a Time Variable to Visualize KM Plot", 
                                                              choices=c("PFS_SURTIME_MONTHS")),
                                                  selectInput("select43", "Select a Censoring Variable to Visualize KM Plot", 
                                                              choices=c("PFS_censor")),
                                                  radioButtons("time", "Time Unit:", list("Years" = 1, "Months" = 2, "Days" = 3), selected = 2),
                                                  radioButtons("riskt", "Risk Table:", list("Yes" = TRUE, "No" = FALSE), selected = TRUE),
                                                  hr()                                
                                                )),
                               conditionalPanel("input.cPanels3 == 2",
                                                wellPanel(
                                                  h4(strong("Univariate Association Analysis")),
                                                  selectInput("select44", "Select a Time Variable for Survival Analysis", 
                                                              choices=c("os")),
                                                  selectInput("select45", "Select a Censoring Variable for Survival Analysis", 
                                                              choices=c("os_censor")),
                                                  selectInput("show_vars46", "Select multiple Variables to Generate Univariate Survival Association Table", 
                                                              c("AGE", "RACE"), choices=c("AGE", "RACE"), multiple = TRUE),
                                                  #radioButtons("exp_ref1", "Choose Reference Level:", list("High" = "High","Low" = "Low"), selected = "High"),
                                                  uiOutput('ref2'),
                                                  radioButtons("isconti1", "Treat continuous variable as continuous or categorical?", list("categorical" = "categorical", "continuous" = "continuous"), selected = "categorical"),
                                                  radioButtons("assum", "Test for Proportional Hazards Assumption:", list("Yes" = 1,"No" = 0), selected = 0),
                                                  hr()                                
                                                )),
                               
                               conditionalPanel("input.cPanels3 == 2",
                                                h4(strong("Downloads")),
                                                wellPanel(
                                                  textInput("fname42", "Type the file name you would like to save as", value = "survivaltable"),
                                                  downloadButton('x44', 'Download Survival Report')
                                                )),
                               conditionalPanel("input.cPanels3 == 1",
                                                h4(strong("Downloads")),
                                                wellPanel(
                                                  textInput("fname41", "Type the file name you would like to save as", value = "kmplot"),
                                                  downloadButton('downloadKM', 'Download KM Plot')
                                                ))
                               ),
                        column(10,
                               tabsetPanel(
                                 tabPanel("KM Analysis and Plot", htmlOutput("pv21"), plotOutput("kmplot", height= 600, width = 800), value =1),
                                 tabPanel("Univariate Survival Association (Cox Model)", htmlOutput("pv22"),
                                            DT::dataTableOutput("out3"), value = 2),
                                   id = "cPanels3"
                               )),                              
                        column(12,
                                      tags$head(tags$style(type="text/css", "
                                                    #loadmessage {
                                                    position: fixed;
                                                    bottom: 0px;
                                                    right: 0px;
                                                    width: 100%;
                                                    padding: 5px 0px 5px 0px;
                                                    text-align: center;
                                                    font-weight: bold;
                                                    font-size: 100%;
                                                    color: #000000;
                                                    background-color: #b8b8b8;
                                                    z-index: 105;
                                                    }
                                                    ")),
                                      conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                       tags$div("Loading...",id="loadmessage"))
                                      
                               ))),         
                    tabPanel("Tutorial",
                               tags$iframe(src= "CTAS_Tutorial.pdf", width = 1800, height = 1000)),
                      navbarMenu("About Us",
                                 tabPanel("How to Cite",
                                          fluidRow(
                                            column(8, offset = 2,
                                                   "Rupji M, Switchenko J. One stop show for Clinical Trial Analyses",
                                                   br(),
                                                   br(),
                                                   "The National Cancer Institute (NCI) requires that publications acknowledge the Winship Cancer Institute CCSG support, and they are tracking compliance. When using this tool to report results in your publication, please include the following statement in the acknowledgment section of your publication(s):",
                                                   br(),
                                                   br(),
                                                   em("Research reported in this publication was supported in part by the Biostatistics Shared Resource of Winship Cancer Institute of Emory University and NIH/NCI under award number P30CA138292. The content is solely the responsibility of the authors and does not necessarily represent the official views of the National Institutes of Health.")
                                            ))),
                                 tabPanel("Contact Us",
                                          fluidRow(
                                            column(8, offset = 2,
                                                   "This tool was prepared by members of the Winship Biostatistics Shared Resource (BSR) of Emory University.",
                                                   br(),
                                                   a(href="https://bbisr.winship.emory.edu/", "https://bbisr.winship.emory.edu/"),
                                                   br(),
                                                   br(),
                                                   "Authors- Manali Rupji, dual M.S., Switcheno Jeffrey Ph.D.",
                                                   br(),
                                                   "Maintainer- Manali Rupji 'manali(dot)rupji(at)emory(dot)edu'")
                                          )),
                                 tabPanel("Feedback",
                                          fluidRow(
                                            column(8, offset = 2,
                                                   #br(),
                                                   "As a Biostatistics core, we are actively improving and expanding our tool box and analytical products and services. For any questions, comments, or suggestions, please email the developer at manali(dot)rupji(at)emory(dot)edu."
                                            )))
                      )
             ))
                        
                        
                        

