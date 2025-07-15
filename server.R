options(shiny.maxRequestSize= 500*1024^2)
options(shiny.sanitize.errors = FALSE)

source("aetable.R")
source("volcano_plot.R")
source("dot_plot.R")
source("spider_plot.R")
source("swim_plot.R")
source("generate_legend_color.R")
source("generate_shape.R")
source("assign_group_colors.R")

server <- function(input, output, session) {
  
  data_input <- reactive({
    if(input$file1 == 'Example'){
      d <- read.csv("data/Adverse Events Form2 V1 Two Arm_CaseStudy1.csv", header =  TRUE)
    }
    else if(input$file1 == 'load_my_own1'){
      inFile <- input$file11
      if (is.null(inFile))
        return(NULL)
      else if(grepl(".xlsx", inFile[1])) { d = read.xlsx(as.character(inFile$datapath), colNames = TRUE, rowNames = F) }
      else if(grepl(".csv", inFile[1])) { d = read.csv(as.character(inFile$datapath), header = TRUE, sep = ",", stringsAsFactors = F, as.is = T, fill = T) }
      else if(grepl(".txt", inFile[1])) { d = read.table(as.character(inFile$datapath), header = TRUE, sep = "\t", stringsAsFactors = F, as.is = T, fill = T) }
    }
    else 
      return(NULL)
    # dim(data)
    Dataset <- data.frame(d)
    return(as.data.frame(Dataset))
  })
  
  data_demo <- reactive({
    if(input$file_demo == 'Example_demo'){
      d <- read.csv("data/Demographics 2 trt_CaseStudy1.csv", header =  TRUE)
    }
    else if(input$file_demo == 'load_my_own_demo'){
      inFile <- input$file_demo1
      if (is.null(inFile))
        return(NULL)
      else if(grepl(".xlsx", inFile[1])) { d = read.xlsx(as.character(inFile$datapath), colNames = TRUE, rowNames = F) }
      else if(grepl(".csv", inFile[1])) { d = read.csv(as.character(inFile$datapath), header = TRUE, sep = ",", stringsAsFactors = F, as.is = T, fill = T) }
      else if(grepl(".txt", inFile[1])) { d = read.table(as.character(inFile$datapath), header = TRUE, sep = "\t", stringsAsFactors = F, as.is = T, fill = T) }
    }
    else 
      return(NULL)
    # dim(data)
    Dataset <- data.frame(d)
    return(as.data.frame(Dataset))
  })
  
  data_input2 <- reactive({
    if(input$file21 == 'Example2'){
      d <- read.csv("data/waterfall plot data_CaseStudy2.csv", header =  TRUE)
    }
    else if(input$file21 == 'load_my_own2'){
      inFile <- input$file211
      if (is.null(inFile))
        return(NULL)
      else if(grepl(".xlsx", inFile[1])) { d = read.xlsx(as.character(inFile$datapath), colNames = TRUE, rowNames = F, as.is = T) }
      else if(grepl(".csv", inFile[1])) { d = read.csv(as.character(inFile$datapath), header = TRUE, sep = ",", stringsAsFactors = F, as.is = T, fill = T) }
      else if(grepl(".txt", inFile[1])) { d = read.table(as.character(inFile$datapath), header = TRUE, sep = "\t", stringsAsFactors = F, as.is = T, fill = T) }
    }
    else 
      return(NULL)
    # dim(data)
    Dataset <- data.frame(d)
    return(as.data.frame(Dataset))
  })
  
  data_input3 <- reactive({
    if(input$file22 == 'Example22'){
      d <- read.csv("data/Spider_plot_data_Dr_Saba_tumorresponse_withPre_CaseStudy3.csv", header =  TRUE)
    }
    else if(input$file22 == 'load_my_own22'){
      inFile <- input$file221
      if (is.null(inFile))
        return(NULL)
      else if(grepl(".xlsx", inFile[1])) { d = read.xlsx(as.character(inFile$datapath), colNames = TRUE, rowNames = F, as.is = T) }
      else if(grepl(".csv", inFile[1])) { d = read.csv(as.character(inFile$datapath), header = TRUE, sep = ",", stringsAsFactors = F, as.is = T, fill = T) }
      else if(grepl(".txt", inFile[1])) { d = read.table(as.character(inFile$datapath), header = TRUE, sep = "\t", stringsAsFactors = F, as.is = T, fill = T) }
    }
    else 
      return(NULL)
    # dim(data)
    Dataset <- data.frame(d)
    return(as.data.frame(Dataset))
  })
  
  data_input5 <- reactive({
    if(input$file32 == 'Example32'){
      d <- read.csv("data/Swimmer Plot data for plotting_CaseStudy3.csv", header =  TRUE)
    }
    else if(input$file32 == 'load_my_own32'){
      inFile <- input$file321
      if (is.null(inFile))
        return(NULL)
      else if(grepl(".xlsx", inFile[1])) { d = read.xlsx(as.character(inFile$datapath), colNames = TRUE, rowNames = F, as.is = T) }
      else if(grepl(".csv", inFile[1])) { d = read.csv(as.character(inFile$datapath), header = TRUE, sep = ",", stringsAsFactors = F, as.is = T, fill = T) }
      else if(grepl(".txt", inFile[1])) { d = read.table(as.character(inFile$datapath), header = TRUE, sep = "\t", stringsAsFactors = F, as.is = T, fill = T) }
    }
    else 
      return(NULL)
    # dim(data)
    Dataset <- data.frame(d)
    return(as.data.frame(Dataset))
  })
  
  data_input4 <- reactive({
    if(input$file42 == 'Example42'){
      d2 <- read.csv("data/Survival analysis data_CaseStudy1.csv", header =T, sep =",", stringsAsFactors = F)
    }
    else if(input$file42 == 'load_my_own42'){
      inFile <- input$file422
      if (is.null(inFile))
        return(NULL)
      else if(grepl(".xlsx", inFile[1])) { d2 = read.xlsx(as.character(inFile$datapath), colNames = TRUE, rowNames = F, as.is = T) }
      else if(grepl(".csv", inFile[1])) { d2 = read.csv(as.character(inFile$datapath), header = TRUE, sep = ",", stringsAsFactors = F, as.is = T, fill = T) }
      else if(grepl(".txt", inFile[1])) { d2 = read.table(as.character(inFile$datapath), header = TRUE, sep = "\t", stringsAsFactors = F, as.is = T, fill = T) }
    }
    else 
      return(NULL)
    # dim(data)
    Dataset2 <- data.frame(d2)
    return(as.data.frame(Dataset2))
  })
  
  output$downloadEx42 <- downloadHandler(
    
    filename <- function() {`                                                                             `
      paste('Example survival ds data', Sys.time(),'.csv', sep='')
    },
    content <- function(file) {
      ds2 <- data_input4()
      write.csv(ds2, file, row.names = FALSE)
    }
  )
  
  output$downloadEx43 <- downloadHandler(
    
    filename <- function() {
      paste('Example swimmer plot ds data', Sys.time(),'.csv', sep='')
    },
    content <- function(file) {
      ds2 <- data_input5()
      write.csv(ds2, file, row.names = FALSE)
    }
  )
  
  observe({
    dsnames1 <- colnames(data_input())
    cb_options <- dsnames1
    
    updateSelectInput(session, "select11", label = "Select a ID Variable",
                      choices = cb_options,
                      selected=cb_options[1])
    updateSelectInput(session, "select12", label = "Select Treatment group variable",
                      choices = cb_options,
                      selected =cb_options[11] )
    updateSelectInput(session, "select13", label = "Select a Preferred Term Variable (Toxicity)",
                      choices = cb_options,
                      selected =cb_options[35] )
    updateSelectInput(session, "select14", label = "Select a System Organ Class Variable",
                      choices = cb_options,
                      selected =cb_options[34] )
    updateSelectInput(session, "select15", label = "Select a Grade Variable",
                      choices = cb_options,
                      selected =cb_options[36] )
    
    dsnames_demo <- colnames(data_demo())
    cb_options_demo <- dsnames_demo
    updateSelectInput(session, "select_id", label = "Select a ID Variable",
                      choices = cb_options_demo,
                      selected =cb_options_demo[1] )
    updateSelectInput(session, "select_arms", label = "Select a Treatment/Arms (Group) Variable",
                      choices = cb_options_demo,
                      selected =cb_options_demo[19] )
    
    dsnames2 <- colnames(data_input2())
    cb_options2 <- dsnames2
    updateSelectInput(session, "select21", label = "Select a Treatment (Group) Variable",
                      choices = cb_options2,
                      selected=cb_options2[2])
    updateSelectInput(session, "select22", label = "Select a Response Variable",
                      choices = cb_options2,
                      selected=cb_options2[5])
    updateSelectInput(session, "select23", label = "Select Measured Change Variable",
                      choices = cb_options2,
                      selected=cb_options2[7])
    
    dsnames3 <- colnames(data_input3())
    cb_options3 <- dsnames3
    updateSelectInput(session, "select31", label = "Select a ID Variable",
                      choices = cb_options3,
                      selected=cb_options3[1])
    updateSelectInput(session, "select32", label = "Select Measured Change Variable",
                      choices = cb_options3,
                      selected=cb_options3[4])
    updateSelectInput(session, "select33", label = "Select Time Variable",
                      choices = cb_options3,
                      selected=cb_options3[3])
    updateSelectInput(session, "select34", label = "Select a Group Variable",
                      choices = cb_options3,
                      selected=cb_options3[7])
    updateSelectInput(session, "select35", label = "Select variable from which time variable (x-axis) should be calculated",
                      choices = cb_options3,
                      selected=cb_options3[3])
    
    dsnames5 <<- colnames(data_input5())
    cb_options5 <- dsnames5
    updateSelectInput(session, "select51", label = "Select a ID Variable",
                      choices = cb_options5,
                      selected=cb_options5[1])
    
    updateSelectInput(session, "select52", label = "First Partial Response Variable",
                      choices = cb_options5,
                      selected=cb_options5[2])
    updateSelectInput(session, "select53", label = "First Partial Response Time Variable",
                      choices = cb_options5,
                      selected=cb_options5[3])
    updateSelectInput(session, "select54", label = "First Complete Response Variable",
                      choices = cb_options5,
                      selected=cb_options5[4])
    updateSelectInput(session, "select55", label = "First Complete Response Time Variable",
                      choices = cb_options5,
                      selected=cb_options5[5])
    updateSelectInput(session, "select56", label = "First Stable Disease Variable",
                      choices = cb_options5,
                      selected=cb_options5[6])
    updateSelectInput(session, "select57", label = "First Stable Disease Time Variable",
                      choices = cb_options5,
                      selected=cb_options5[7])
    updateSelectInput(session, "select58", label = "First Progressive Disease Variable",
                      choices = cb_options5,
                      selected=cb_options5[8])
    updateSelectInput(session, "select59", label = "First Progressive Disease Time Variable",
                      choices = cb_options5,
                      selected=cb_options5[9])
    updateSelectInput(session, "select60", label = "Death Variable",
                      choices = cb_options5,
                      selected=cb_options5[14])
    updateSelectInput(session, "select61", label = "Time to Death Variable",
                      choices = cb_options5,
                      selected=cb_options5[13])
    updateSelectInput(session, "select62", label = "Treatment Start Variable",
                      choices = cb_options5,
                      selected=cb_options5[11])
    updateSelectInput(session, "select63", label = "Time to Treatment End",
                      choices = cb_options5,
                      selected=cb_options5[12])
    updateSelectInput(session, "select63_2", label = "Treatment Stop Indicator",
                      choices = cb_options5,
                      selected=cb_options5[10])
    updateSelectInput(session, "select64", label = "Select Treatment Variable",
                      choices = cb_options5,
                      selected=cb_options5[15])  
    updateSelectInput(session, "select65", label = "Follow-up Start Variable",
                      choices = cb_options5,
                      selected=cb_options5[19]) 
    updateSelectInput(session, "select66", label = "Follow-up End Variable",
                      choices = cb_options5,
                      selected=cb_options5[20]) 
    updateSelectInput(session, "select67", label = "Durable Responder Variable",
                      choices = cb_options5,
                      selected=cb_options5[17])
    updateSelectInput(session, "select68", label = "Continued Response Variable",
                      choices = cb_options5,
                      selected=cb_options5[18])
    
    dsnames4 <- colnames(data_input4())
    cb_options4 <- dsnames4
    updateSelectInput(session, "select41", label = "Select a Variable of Interest as Cohort Group",
                      choices = c(cb_options4, "All Patients"),
                      selected = cb_options4[52])
    updateSelectInput(session, "select42", label = "Select a Time Variable to Visualize KM Plot",
                      choices = cb_options4,
                      selected=cb_options4[49])
    updateSelectInput(session, "select43", label = "Select a Censoring Variable to Visualize KM Plot",
                      choices = cb_options4,
                      selected =cb_options4[50] )
    updateSelectInput(session, "select44", label = "Select a Time Variable for Survival Analysis",
                      choices = cb_options4,
                      selected=cb_options4[49])
    updateSelectInput(session, "select45", label = "Select a Censoring Variable for Survival Analysis",
                      choices = cb_options4,
                      selected =cb_options4[50] )
    updateSelectInput(session, "show_vars46", label = "Select multiple Variables to Generate Univariate Survival Association Table",
                      choices = cb_options4,
                      selected = cb_options4[c(52,2)])
  })
  
  res <- eventReactive(input$run, {
    data <- data_input()
  })
  
  output$downloadEx1 <- downloadHandler(
    
    filename <- function() {
      paste('Example ds', Sys.time(),'.csv', sep='')
    },
    content <- function(file) {
      ds <- data_input()
      write.csv(ds, file, row.names = FALSE)
    }
  )
  
  output$downloadEx_demo <- downloadHandler(
    
    filename <- function() {
      paste('Example demo ds', Sys.time(),'.csv', sep='')
    },
    content <- function(file) {
      ds <- data_demo()
      write.csv(ds, file, row.names = FALSE)
    }
  )
  
  output$downloadEx21 <- downloadHandler(
    
    filename <- function() {
      paste('Example water fall ds', Sys.time(),'.csv', sep='')
    },
    content <- function(file) {
      ds <- data_input2()
      write.csv(ds, file, row.names = FALSE)
    }
  )
  
  output$downloadEx22 <- downloadHandler(
    
    filename <- function() {
      paste('Example spider plot ds', Sys.time(),'.csv', sep='')
    },
    content <- function(file) {
      ds <- data_input3()
      write.csv(ds, file, row.names = FALSE)
    }
  )
  
  output$downloadEx32 <- downloadHandler(
    
    filename <- function() {
      paste('Example swimmer plot ds', Sys.time(),'.csv', sep='')
    },
    content <- function(file) {
      ds <- data_input5()
      write.csv(ds, file, row.names = FALSE)
    }
  )
  
  output$heading1 <- renderUI({
    
    if (is.null(data_input())) {
      return(NULL)
    } else {
      hs1 <- paste("&emsp;")
      hs2 <- paste("Volcano plot for AEs with 2 Arms")
      HTML(paste(h2(strong(hs2)), hs1, sep = '<br/>'))
    }
  })
  
  output$distribution <- renderUI({
    
    if (is.null(data_demo())) {
      return(NULL)
    } else {
      
      d <- data_demo()
      ARMSN <- d[,input$select_arms]
      
      big_n <- c(table(ARMSN)[1], table(ARMSN)[2])
      
      hs1 <- paste("&emsp;")
      if(length(big_n) >1 & !is.na(big_n[2])) {
      hs2 <- paste("Based on Demographics data, distribution of patients in Arms A = ", big_n[1], 
                   " and B = ", big_n[2], sep = "")
      } else {
        hs2 <- paste("Based on Demographics data, distribution of patients in Single Arm = ", big_n[1] ) 
      }
      HTML(paste(h4(strong(hs2)), hs1, sep = '<br/>'))
    }
  })
  
  output$ref1 <- renderUI({
    data2 <- data_input()
    vals <- list()
    
    SOC <- data2[,input$select14, drop = FALSE]
  
    vals <- unique(SOC)
    values2 <- vals$SOC
    
    selectizeInput('exp_ref1', 'Select Single or mutliple SOCs to generate dot plot (limiting to 2-3 will result in less dense dot plot)', 
          choices = values2, multiple = TRUE, selected= c(values2[4], values2[6]))
    
  })
  
  
  output$heading2 <- renderUI({
    if (is.null(data_input())) {
      return(NULL)
    } else {
      
      hs1 <- paste("&emsp;")
      hs2 <- paste("Dot plot for AEs with 2 Arms")
      HTML(paste(h2(strong(hs2)), hs1, sep = '<br/>'))
    }
  })
  
  output$out1 <- DT::renderDataTable({
    out <- data_input()
    
    DT::datatable(out)
  })
  
  output$VolcanoPlot <- renderPlotly({
    
    d <- data_demo()
    ARMSN <- d[,input$select_arms]
    
    big_n <- c(table(ARMSN)[1], table(ARMSN)[2])
    
    if(length(big_n) >1 & !is.na(big_n[2])) {
    data <- data_input()
    
    SEQUENCE_NO <- data[,input$select11]
    ARMS <- data[,input$select12]
    AETERMC <- data[,input$select13]
    SOC <- data[,input$select14]
    AETOXGR_N <- data[, input$select15]
    
    adae <- cbind.data.frame(SEQUENCE_NO, ARMS, AETERMC, SOC, AETOXGR_N)
    
    p <-  volcano_plot(adae, pval_cutoff = input$pval_cutoff)
    } else {
      return(NULL)
    }
    
  })
  
  
  output$wrongdata <- renderUI({
    
    data <- data_input()
    
    SEQUENCE_NO <- data[,input$select11]
    ARMS <- data[,input$select12]
    AETERMC <- data[,input$select13]
    SOC <- data[,input$select14]
    AETOXGR_N <- data[, input$select15]
    
    d <- data_demo()
    ARMSN <- d[, input$select_arms]
    SEQUENCE_NO_ <- d[, input$select_id]
    
    dat_AE <<- cbind.data.frame(SEQUENCE_NO, ARMS, AETERMC, SOC, AETOXGR_N)
    dat_demo <<- cbind.data.frame(SEQUENCE_NO_, ARMSN)
    colnames(dat_demo)[1] <- "SEQUENCE_NO"
    
    # check if data is in concordance
    check_tmp <- merge(dat_AE, dat_demo, by = "SEQUENCE_NO")
    
    if(dim(check_tmp)[1] == 0){
      hs1 <- paste("&emsp;")
      hs2 <- paste("The AE data patients are different from Demographics dataset. Please ensure same patients are included.")
      HTML(paste(h4(hs2), hs1, sep = '<br/>'))
    } else 
      return(NULL)
  })
  
  output$out2 <- DT::renderDataTable({
    
    data <- data_input()
    
    SEQUENCE_NO <- data[,input$select11]
    ARMS <- data[,input$select12]
    AETERMC <- data[,input$select13]
    SOC <- data[,input$select14]
    AETOXGR_N <- data[, input$select15]
    
    d <- data_demo()
    ARMSN <- d[, input$select_arms]
    SEQUENCE_NO_ <- d[, input$select_id]
    
    dat_AE <- cbind.data.frame(SEQUENCE_NO, ARMS, AETERMC, SOC, AETOXGR_N)
    dat_demo <- cbind.data.frame(SEQUENCE_NO_, ARMSN)
    colnames(dat_demo)[1] <- "SEQUENCE_NO"
    
    # check if data is in concordance
    check_tmp2 <- merge(dat_AE, dat_demo, by = "SEQUENCE_NO")
    
    if(dim(check_tmp2)[1] != 0){
    
    big_n <<- c(table(ARMSN)[1], table(ARMSN)[2])
    
    adae <<- cbind.data.frame(SEQUENCE_NO, ARMS, AETERMC, SOC, AETOXGR_N)
    
    out <- aetable(adae, bySOC = input$bySOC,topdisplay= input$topdisplay, top = as.numeric(input$top), big_n = big_n)
    
    if(length(big_n)>1 & !is.na(big_n[2])){
    sketch = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, 'AE term'),
          th(colspan = 2, paste('Arm A (n=', big_n[1], ')', sep = "")),
          th(colspan = 2, paste('Arm B (n=', big_n[2], ')', sep = ""))
        ),
        tr(
          lapply(rep(c('All Grade', 'Grade 3-4'), 2), th)
        )
      )
    ))
    } else {
      sketch = htmltools::withTags(table(
        class = 'display',
        thead(
          tr(
            th(rowspan = 2, 'AE term'),
            th(colspan = 2, paste('Arm A (n=', big_n[1], ')', sep = ""))
            
          ),
          tr(
            lapply(rep(c('All Grade', 'Grade 3-4'), 1), th)
          )
        )
      ))
    }
    
    DT::datatable(out, container = sketch, rownames=FALSE, escape = FALSE)
    } 
    
  })
  
  output$downloadAE = downloadHandler(
    filename <- function() {
      csv_file <<- as.character(input$fname22)
      paste('ST_', csv_file, Sys.time(),'.csv', sep='')
    },
    content = function(file) {
      data <- data_input()
      
      SEQUENCE_NO <- data[,input$select11]
      ARMS <- data[,input$select12]
      AETERMC <- data[,input$select13]
      SOC <- data[,input$select14]
      AETOXGR_N <- data[, input$select15]
      
      adae <- cbind.data.frame(SEQUENCE_NO, ARMS, AETERMC, SOC, AETOXGR_N)
      
      
      d <- data_demo()
      ARMSN <- d[,input$select_arms]
      
      big_n <- c(table(ARMSN)[1], table(ARMSN)[2])
      
      out <- aetable(adae, bySOC = input$bySOC, topdisplay= input$topdisplay, top = as.numeric(input$top), big_n = big_n)
      
      out[,1] <- gsub("\\&nbsp;", " ", out[,1])
     
      write.csv(out, file, row.names = F)
      
    })
  
  output$DotPlot <- renderPlot ({
    data <- data_input()
    
    SEQUENCE_NO <- data[,input$select11]
    ARMS <- data[,input$select12]
    AETERMC <- data[,input$select13]
    SOC <- data[,input$select14]
    AETOXGR_N <- data[, input$select15]
    
    adae <- cbind.data.frame(SEQUENCE_NO, ARMS, AETERMC, SOC, AETOXGR_N)
    
    d <- data_demo()
    ARMSN <- d[,input$select_arms]
    
    big_n <- c(table(ARMSN)[1], table(ARMSN)[2])
    
    if(length(big_n) >1 & !is.na(big_n[2])) {
    dp <- dot_plot(adae, bySOC = input$bySOC1, topdisplay = input$topdisplay1, 
             top= as.numeric(input$top1), orderbyB = input$orderbyB,
             SOCs = input$exp_ref1, big_n = big_n)
    
    dp$text.plot$panel.args.common$cex <- 0.97
    
    return(dp)
    }
    else {
      return(NULL)
    }
    
  })
  
 output$WaterFallPlot <- renderPlot({
    data <- data_input2()
    
    treatment <- data[,input$select21]
    bor <- data[,input$select22]
    br_change <- data[,input$select23]
    
    d <- cbind.data.frame(treatment, bor, br_change)
    
    d %>% arrange(factor(bor, levels = c("PD", "SD", "PR", "CR")))
    d <- d[order(-d$br_change),]
    
    d$bor <- factor(d$bor, levels = c("PD", "SD", "PR", "CR"))
   
    col <- ifelse(d$bor == "CR", input$cc_CR,
                  ifelse(d$bor == "PR", input$cc_PR,
                         ifelse(d$bor == "PD", input$cc_PD, 
                                ifelse(d$bor == "SD", input$cc_SD, ""))))
    
    num <- table(d$bor)
    
    MCC <- barplot(d$br_change, 
                  col=col, 
                  border=col, 
                  space=0.5, 
                  ylim=c(as.numeric(input$bar_ylim_low),as.numeric(input$bar_ylim_high)),
                  main = " ",
                  ylab="Change from baseline (%)",
                  cex.axis=1.5, 
                  cex.lab=1.5,
                  legend.text= c( paste("CR: Complete Response (n = ", ifelse( any(grepl('CR', names(num))) == TRUE,  paste(num[names(num) %in% "CR"], round(num[names(num) %in% "CR"]*100/sum(num), 2), sep = ", "), 0), " % )", sep = ""), 
                                  paste("PR: Partial Response (n = ", ifelse( any(grepl('PR', names(num))) == TRUE, paste(num[names(num) %in% "PR"], round(num[names(num) %in% "PR"]*100/sum(num), 2), sep = ", "), 0), " % )", sep = ""),
                                  paste("SD: Stable Disease (n = ", ifelse( any(grepl('SD', names(num))) == TRUE, paste(num[names(num) %in% "SD"], round(num[names(num) %in% "SD"]*100/sum(num), 2), sep = ", "), 0), " % )", sep = ""),
                                  paste("PD: Progressive Disease (n = ", ifelse( any(grepl('PD', names(num))) == TRUE, paste(num[names(num) %in% "PD"], round(num[names(num) %in% "PD"]*100/sum(num), 2), sep = ", "), 0), " % )", sep = ""),
                                  paste("Total evaluable patients = ", sum(num), sep = "" )),
                  args.legend=list(title="Best Overall Response", 
                  fill=c(input$cc_CR,input$cc_PR, input$cc_SD, input$cc_PD, "white"), border=NA, cex=1.0))
    
    if (input$displayPD == TRUE) {
        abline(h=input$abline_high, col = "grey", lwd=0.9, lty=3) # The "PD" line
    }
    if (input$displayPR == TRUE) {
        abline(h=input$abline_low, col = "grey", lwd=0.9, lty =3) # This "PR" line
    }
    if(input$displayTrt == TRUE) {
    text(MCC, ifelse(d$br_change > 0, d$br_change + 2.5, d$br_change - 2), labels = d$treatment)
    }
  })
  
  output$SpiderPlot <- renderPlot({
    data <- data_input3()
    
    id <- data[,input$select31]
    br_change <- data[,input$select32]
    week <- data[, input$select33]
    date <- data[, input$select35]
    grp <- data[,input$select34]
    
    d <<- cbind.data.frame(id, br_change, week, date, grp)
    var_name <- as.character(input$select34)
    
    spider_plot(data= d, time_avail= input$time_avail,var_name= as.character(input$select34),  date = date, 
                xlab = input$xlab1, ylab = input$ylab1, grp_avail = input$group_avail,
                legend.lab = input$legend.lab, PRline = input$displayPR1, PRvalue = input$abline_low1,
                PDline = input$displayPD1, PDvalue = input$abline_high1)
    
    
  })
  
  output$x32 <- downloadHandler(
    filename <- function() {
      pdf_file44 <<- as.character(input$fname32)
      paste('Spider_plot', pdf_file44, Sys.time(),'.pdf', sep='')
    },
    content <- function(file) {
      pdf(file=paste(pdf_file44,".pdf",sep="") , height= 11, width=12)
      
      data <- data_input3()
      
      id <- data[,input$select31]
      br_change <- data[,input$select32]
      week <- data[, input$select33]
      date <- data[, input$select35]
      grp <- data[,input$select34]
      
    
      d <- cbind.data.frame(id, br_change, week, date, grp)
      
      p <- spider_plot(data= d, time_avail= input$time_avail,var_name= as.character(input$select34),  date = date, 
                  xlab = input$xlab1, ylab = input$ylab1, grp_avail = input$group_avail,
                  legend.lab = input$legend.lab, PRline = input$displayPR1, PRvalue = input$abline_low1,
                  PDline = input$displayPD1, PDvalue = input$abline_high1)
      
      plot(p)
      dev.off()
      file.copy(paste(pdf_file44,'.pdf', sep='') ,file, overwrite=TRUE)
    })
  
  output$SwimmerPlot <- renderPlot({
    data <- data_input5()
    
    id <- data[,input$select51]
    firstPR <- data[,input$select52]
    firstPR_time <- data[, input$select53]
    firstCR <- data[, input$select54]
    firstCR_time <- data[, input$select55]
    firstSD <- data[, input$select56]
    firstSD_time <- data[, input$select57]
    firstPD <- data[, input$select58]
    firstPD_time <- data[, input$select59]
    
    death <- data[, input$select60]
    death_time <- data[, input$select61]
    
    trtstart <- data[, input$select62]
    trtend <- data[, input$select63]
    trtcap <- data[, input$select63_2]
    group <- data[, input$select64]
    
    fustart <- data[, input$select65]
    fuend <- data[, input$select66]
    
    DR <- data[, input$select67]
    Continued_Resp <- data[, input$select68]
    
    swimdataN <- cbind.data.frame(id, firstPR, firstPR_time, firstCR, firstCR_time,
                                  firstSD, firstSD_time, firstPD, firstPD_time,
                                  death, death_time, trtstart, trtend, trtcap, group,
                                  fustart, fuend, DR, Continued_Resp)
    
    swim_plot(swimdata= swimdataN, by_sub = input$display_subj, 
        xlab = input$xlab2, ylab = input$ylab2, trt_avail = input$trt_avail,
        legend.lab= input$legend.lab2,
        displayTStop = input$displayTStop,
        displayPResp = input$displayPResp,
        displaySDis = input$displaySDis,
        displayPDis = input$displayPDis,
        displayCResp = input$displayCResp,
        displayDeath = input$displayDeath,
        displayDR = input$displayDR,
        displayContResp = input$displayContResp)
    
    
    
  })
  
  output$x33 <- downloadHandler(
    filename <- function() {
      pdf_file33 <<- as.character(input$fname33)
      paste('Swimmer_plot', pdf_file33, Sys.time(),'.pdf', sep='')
    },
    content <- function(file) {
      grDevices::cairo_pdf(file=paste(pdf_file33,".pdf",sep="") , height= 11, width=12)
      
      data <- data_input5()
      
      id <- data[,input$select51]
      firstPR <- data[,input$select52]
      firstPR_time <- data[, input$select53]
      firstCR <- data[, input$select54]
      firstCR_time <- data[, input$select55]
      firstSD <- data[, input$select56]
      firstSD_time <- data[, input$select57]
      firstPD <- data[, input$select58]
      firstPD_time <- data[, input$select59]
      
      death <- data[, input$select60]
      death_time <- data[, input$select61]
      
      trtstart <- data[, input$select62]
      trtend <- data[, input$select63]
      trtcap <- data[, input$select63_2]
      group <- data[, input$select64]
      
      fustart <- data[, input$select65]
      fuend <- data[, input$select66]
      
      DR <- data[, input$select67]
      Continued_Resp <- data[, input$select68]
      
      swimdataN <- cbind.data.frame(id, firstPR, firstPR_time, firstCR, firstCR_time,
                                     firstSD, firstSD_time, firstPD, firstPD_time,
                                     death, death_time, trtstart, trtend, trtcap, group,
                                     fustart, fuend, DR, Continued_Resp)
      
      p <- swim_plot(swimdata= swimdataN, by_sub = input$display_subj, 
                xlab = input$xlab2, ylab = input$ylab2, trt_avail = input$trt_avail,
                legend.lab= input$legend.lab2,
                displayTStop = input$displayTStop,
                displayPResp = input$displayPResp,
                displaySDis = input$displaySDis,
                displayPDis = input$displayPDis,
                displayCResp = input$displayCResp,
                displayDeath = input$displayDeath,
                displayDR = input$displayDR,
                displayContResp = input$displayContResp)
      
      plot(p)
      dev.off()
      file.copy(paste(pdf_file33,'.pdf', sep='') ,file, overwrite=TRUE)
    })
      
  
  output$x24 <- downloadHandler(
    filename <- function() {
      pdf_file2 <<- as.character(input$fname24)
      paste('WaterFall_plot', pdf_file2, Sys.time(),'.pdf', sep='')
    },
    content <- function(file) {
      pdf(file=paste(pdf_file2,".pdf",sep="") , height= 11, width=12)
      
      data <- data_input2()
      
      treatment <- data[,input$select21]
      bor <- data[,input$select22]
      br_change <- data[,input$select23]
      
      d <- cbind.data.frame(treatment, bor, br_change)
      
      d %>% arrange(factor(bor, levels = c("PD", "SD", "PR", "CR")))
      d <- d[order(-d$br_change),]
      
      d$bor <- factor(d$bor, levels = c("PD", "SD", "PR", "CR"))
      
      col <- ifelse(d$bor == "CR", input$cc_CR,
                    ifelse(d$bor == "PR", input$cc_PR,
                           ifelse(d$bor == "PD", input$cc_PD, 
                                  ifelse(d$bor == "SD", input$cc_SD, ""))))
      
      num <- table(d$bor)
      
      MCC <- barplot(d$br_change, 
                     col=col, 
                     border=col, 
                     space=0.5, 
                     ylim=c(as.numeric(input$bar_ylim_low),as.numeric(input$bar_ylim_high)),
                     main = " ",
                     ylab="Change from baseline (%)",
                     cex.axis=1.5, 
                     cex.lab=1.5,
                     legend.text= c( paste("CR: Complete Response (n = ", ifelse( any(grepl('CR', names(num))) == TRUE,  paste(num[names(num) %in% "CR"], round(num[names(num) %in% "CR"]*100/sum(num), 2), sep = ", "), 0), " % )", sep = ""), 
                                     paste("PR: Partial Response (n = ", ifelse( any(grepl('PR', names(num))) == TRUE, paste(num[names(num) %in% "PR"], round(num[names(num) %in% "PR"]*100/sum(num), 2), sep = ", "), 0), " % )", sep = ""),
                                     paste("SD: Stable Disease (n = ", ifelse( any(grepl('SD', names(num))) == TRUE, paste(num[names(num) %in% "SD"], round(num[names(num) %in% "SD"]*100/sum(num), 2), sep = ", "), 0), " % )", sep = ""),
                                     paste("PD: Progressive Disease (n = ", ifelse( any(grepl('PD', names(num))) == TRUE, paste(num[names(num) %in% "PD"], round(num[names(num) %in% "PD"]*100/sum(num), 2), sep = ", "), 0), " % )", sep = ""),
                                     paste("Total evaluable patients = ", sum(num), sep = "" )),
                     args.legend=list(title="Best Overall Response", 
                                      fill=c(input$cc_CR,input$cc_PR, input$cc_SD, input$cc_PD, "white"), border=NA, cex=1.0))
      
      if (input$displayPD == TRUE) {
        abline(h=input$abline_high, col = "grey", lwd=0.5, lty=3) # The "PD" line
      }
      if (input$displayPR == TRUE) {
        abline(h=input$abline_low, col = "grey", lwd=0.5, lty =3) # This "PR" line
      }
      if(input$displayTrt == TRUE) {
        text(MCC, ifelse(d$br_change > 0, d$br_change + 2.5, d$br_change - 2), labels = d$treatment)
      }
      dev.off()
      file.copy(paste(pdf_file2,'.pdf', sep='') ,file, overwrite=TRUE)
    })
  
  output$x3 <- downloadHandler(
    filename <- function() {
      pdf_file3 <<- as.character(input$fname22)
      paste('Dot_plot', pdf_file3, Sys.time(),'.pdf', sep='')
    },
    content <- function(file) {
      pdf(file=paste(pdf_file3,".pdf",sep="") , height= 11, width=12)
      
      data <- data_input()
      
      SEQUENCE_NO <- data[,input$select11]
      ARMS <- data[,input$select12]
      AETERMC <- data[,input$select13]
      SOC <- data[,input$select14]
      AETOXGR_N <- data[, input$select15]
      
      adae <- cbind.data.frame(SEQUENCE_NO, ARMS, AETERMC, SOC, AETOXGR_N)
      
      d <- data_demo()
      ARMSN <- d[,input$select_arms]
      
      big_n <- c(table(ARMSN)[1], table(ARMSN)[2])
      
     tmp <<-  dot_plot(adae, bySOC = input$bySOC1, topdisplay = input$topdisplay1, 
               top= as.numeric(input$top1), orderbyB = input$orderbyB,
               SOCs = input$exp_ref1, big_n = big_n)
      
     tmp$text.plot$panel.args.common$cex <- 1.0
     print(tmp)
      
      dev.off()
      file.copy(paste(pdf_file3,'.pdf', sep='') ,file, overwrite=TRUE)
    })
  
  
  
  output$kmplot <- renderPlot({
    data2 <- data_input4()
    if(!is.null(data2)){
      for (j in 1:ncol(data2)) {
        if (class(data2[, j]) %in% c("character")) 
          data2[,j] <- as.factor(data2[,j])
        else data2[,j] = data2[, j]
      }
      if (input$select41 == "All Patients") {
        
        time <- data2[,input$select42]
        censor <- data2[,input$select43]
        dat <- cbind.data.frame(time, censor)
        
        fit <- survfit(Surv(as.numeric(time), as.numeric(factor(censor))) ~ 1, data = dat)
        # Drawing curves
        xlabel <- c("Years", "Months", "Days")
        b <- c(1, 12, 365)
        #TF <- c(TRUE, FALSE)
        if (input$riskt == TRUE) {
          res <- ggsurvplot(fit, data = dat, break.time.by = b[as.numeric(input$time)],  font.main = 18,
                            font.x =  14,
                            font.y = 14, xlab = xlabel[as.numeric(input$time)], pval = T, pval.method = T, 
                            pval.coord = c(max(time, na.rm = T)*0.8, 0.9), pval.method.coord = c(max(time, na.rm= T)*0.8, 0.95),
                            font.tickslab = 12,legend = c(0.2, 0.2), legend.title = input$select41,
                            risk.table = TRUE, risk.table.y.text.col = TRUE)#, fun = function(y) y*100)
          return(res) 
          
        } else {
          res <- ggsurvplot(fit, data = dat, break.time.by = b[as.numeric(input$time)],  font.main = 18,
                            font.x =  14,
                            font.y = 14, xlab = xlabel[as.numeric(input$time)], pval = T, pval.method = T, 
                            pval.coord = c(max(time, na.rm= T)*0.8, 0.9), pval.method.coord = c(max(time, na.rm= T)*0.8, 0.95),
                            font.tickslab = 12,legend = c(0.2, 0.2), legend.title = input$select41,
                            risk.table = FALSE, risk.table.y.text.col = FALSE)#, fun = function(y) y*100)
          return(res) 
          
        }
      } else {
        xvar <- data2[, input$select41]
        time <- data2[,input$select42]
        censor <- data2[,input$select43]
        dat <- cbind.data.frame(time, censor, xvar)
        
        if (is.null(xvar) | is.null(time) | is.null(censor)) {
          return(NULL)
        } else if (nlevels(xvar) > 30) {
          return(NULL)
        } else if (class(xvar) == "factor") {
          fit <- survfit(Surv(as.numeric(time), as.numeric(factor(censor))) ~ xvar, data = dat)
          # Drawing curves
          xlabel <- c("Years", "Months", "Days")
          b <- c(1, 12, 365)
          #TF <- c(TRUE, FALSE)
          if (input$riskt == TRUE) {
            #check_xvar <<- levels(xvar)
            res <- ggsurvplot(fit, data = dat, break.time.by = b[as.numeric(input$time)],  font.main = 18,
                              font.x =  14,
                              font.y = 14, xlab = xlabel[as.numeric(input$time)], pval = T, pval.method = T, 
                              pval.coord = c(max(time)*0.8, 0.9), pval.method.coord = c(max(time)*0.8, 0.95),
                              font.tickslab = 12,legend = c(0.2, 0.2), legend.title = input$select41, legend.labs = levels(xvar),
                              risk.table = TRUE, risk.table.y.text.col = TRUE)#, fun = function(y) y*100)
            return(res) 
            
          } else {
            
            res <- ggsurvplot(fit, data = dat, break.time.by = b[as.numeric(input$time)],  font.main = 18,
                              font.x =  14,
                              font.y = 14, xlab = xlabel[as.numeric(input$time)], pval = T, pval.method = T, 
                              pval.coord = c(max(time, na.rm= T)*0.8, 0.9), pval.method.coord = c(max(time, na.rm= T)*0.8, 0.95),
                              font.tickslab = 12,legend = c(0.2, 0.2), legend.title = input$select41, legend.labs = levels(xvar),
                              risk.table = FALSE, risk.table.y.text.col = FALSE)#, fun = function(y) y*100)
            return(res) 
            
          }
        }else if (class(xvar) %in% c("integer", "numeric") & input$binary == "continuous") {
          if (as.numeric(as.integer(input$cutoff2)) == 1) {
            perc <- optimalcut(dat)*100
          } else {perc <- as.numeric(as.integer(input$cutoff2))}
          
          dat$Group <- ifelse(dat[, 'xvar'] <= quantile(dat[, 'xvar'], perc/100, na.rm= TRUE), "Low", "High")
          Group <- as.factor(dat$Group)
          
          fit <- survfit(Surv(as.numeric(time), as.numeric(factor(censor))) ~ Group, data = dat)
          # Drawing curves
          xlabel <- c("Years", "Months", "Days")
          b <- c(1, 12, 365)
          #TF <- c(TRUE, FALSE)
          if (input$riskt == TRUE) {
            res <- ggsurvplot(fit, data = dat, break.time.by = b[as.numeric(input$time)],  font.main = 18,
                              font.x =  14,
                              font.y = 14, xlab = xlabel[as.numeric(input$time)], pval = T, pval.method = T, 
                              pval.coord = c(max(time, na.rm= T)*0.8, 0.9), pval.method.coord = c(max(time, na.rm= T)*0.8, 0.95),
                              font.tickslab = 12,legend = c(0.2, 0.2), legend.title = input$select41, legend.labs = levels(Group),
                              risk.table = TRUE, risk.table.y.text.col = TRUE)#, fun = function(y) y*100, type="cairo")
            return(res) 
            
          } else {
            res <- ggsurvplot(fit, data = dat, break.time.by = b[as.numeric(input$time)],  font.main = 18,
                              font.x =  14,
                              font.y = 14, xlab = xlabel[as.numeric(input$time)], pval = T, pval.method = T, 
                              pval.coord = c(max(time, na.rm= T)*0.8, 0.9), pval.method.coord = c(max(time, na.rm= T)*0.8, 0.95),
                              font.tickslab = 12,legend = c(0.2, 0.2), legend.title = input$select41, legend.labs = levels(Group),
                              risk.table = FALSE, risk.table.y.text.col = FALSE)#, fun = function(y) y*100, type="cairo")
            return(res) 
            
          }
        } else {
          return(NULL)
        }
      }
    }
  })
  
  output$ref2 <- renderUI({
    data2 <- data_input4()
    vals <- list()
    
    xvar <- data2[,input$show_vars46, drop = FALSE]
    time <- data2[,input$select44]
    censor <- data2[,input$select45]
    
    for (i in 1: ncol(xvar)) {
      x <- xvar[, i]
      
      if(class(x) %in% c('numeric', 'integer')) {
        
        dat <- cbind.data.frame(time, censor, xvar=x)
        if (as.numeric(as.integer(input$cutoff2)) == 1) {
          perc <- optimalcut(dat)*100
        } else { perc <- as.numeric(as.integer(input$cutoff2)) }
        
        values <- ifelse(x <= quantile(x, perc/100, na.rm= TRUE), "Low", "High")
        
        vals[[i]] <- paste(colnames(xvar)[i], unique(values), sep= ":")
        
      } else if(class(x) %in% c('character', 'factor')) {
        vals[[i]] <- paste(colnames(xvar)[i],unique(x),  sep= ":")
        
      }
      
    }
    
    values2 <- unlist(vals)
    selectizeInput('exp_ref2', 'Choose Reference level for each variable in the order they were chosen', choices = values2, multiple = TRUE, selected= c(values2[1], values2[4]))
    
  })
  
  output$out3 <- renderDataTable({
    
    data2 <- data_input4()
    for (j in 1:ncol(data2)) {
      if (class(data2[, j]) %in% c("character")) 
        data2[,j] <- as.factor(data2[,j])
      else data2[,j] = data2[, j]
    } 
    xvar <- data2[,input$show_vars46, drop = FALSE]
    time <- data2[,input$select44]
    censor <- data2[,input$select45]
    
    options(warn=-1)
    res <- list()
    for (i in 1: ncol(xvar)){
      
      x <- xvar[, i]
      
      if (is.null(x) | is.null(time) | is.null(censor)) {
        return(NULL)
      } else if (nlevels(x) > 30) {
        return(NULL)
      } else if (class(x) == "factor" | (class(x) %in% c("integer", "numeric") & input$isconti1 == "categorical")){
        
        if (class(x) %in% c("integer", "numeric")){
          dat <- cbind.data.frame(time, censor, xvar=x)
          if (as.numeric(as.integer(input$cutoff2)) == 1) {
            perc <- optimalcut(dat)*100
          } else {perc <- as.numeric(as.integer(input$cutoff2))}
          
          dat$Group <- ifelse(dat[, 'xvar'] <= quantile(dat[, 'xvar'], perc/100, na.rm= TRUE), "Low", "High")
          Group <- as.factor(dat$Group)
          x <- Group
        }
        
        ref <- gsub(".*:", "", input$exp_ref2[i])
        x <- relevel(x, ref)
        
        Variable <- c(colnames(xvar)[i], rep("", (length(levels(x))-1)))
        #x <- C(x, contr.treatment, base=3)
        fit <- coxph(Surv(as.numeric(time), as.numeric(factor(censor))) ~ x)
        temp <- cox.zph(fit)
        assum.p.value <- ifelse(temp$table[3] < 0.001, "<0.001", paste0(round(temp$table[3], 4)))
        assump <- c(assum.p.value, rep("", (length(levels(x))-1)))
        
        sum <- summary(fit)
        hazard.ratio = round(sum$conf.int[, 1], 2)
        lower95 = round(sum$conf.int[, 3], 2)
        upper95 = round(sum$conf.int[, 4], 2)
        logrank.p.value = ifelse(sum$sctest[3] < 0.001, "<0.001", paste0(round(sum$sctest[3], 4)))
        logrankp <- c(logrank.p.value, rep("", (length(levels(x))-1)))
        type3.p.value = ifelse(sum$coefficients[, 5] < 0.001, "<0.001", paste0(round(sum$coefficients[, 5], 4)))
        counts <- data.frame(table(x))
        counts <- rbind.data.frame(counts[2:nrow(counts),], counts[1, ])
        coxres <- cbind.data.frame(Variable, counts, c(paste0(hazard.ratio, " (", lower95,
                                                              "-", upper95, ")"), ""), c(type3.p.value, ""), logrankp)
        colnames(coxres) <- c("Variable", "Level", "N", "Hazard Ratio (95% CI)", "Type 3 P-value", "Log-rank P-value")
        if (input$assum == 0) {
          res[[i]] <- coxres
        } else if (input$assum == 1) {
          coxres2 <- cbind.data.frame(coxres, assump)
          colnames(coxres2)[7] <- "P-value for Proportional Hazards Assumption"
          res[[i]] <- coxres2
        }
      }  else if (class(x) %in% c("integer", "numeric") & input$isconti1 == "continuous") {
        fit <- coxph(Surv(as.numeric(time), as.numeric(factor(censor))) ~ x)
        temp <- cox.zph(fit)
        assump <- ifelse(temp$table[3] < 0.001, "<0.001", paste0(round(temp$table[3], 4)))   
        
        sum <- summary(fit)
        Variable <- colnames(xvar)[i]
        hazard.ratio <- round(sum$conf.int[, 1], 2)
        lower95 <- round(sum$conf.int[, 3], 2)
        upper95 <- round(sum$conf.int[, 4], 2)
        
        type3.p.value <- ifelse(sum$coefficients[, 5] < 0.001, "<0.001", paste0(round(sum$coefficients[, 5], 4)))
        counts <- length(x)
        coxres <- cbind.data.frame(Variable, "", counts, paste0(hazard.ratio, " (", lower95, "-", upper95, ")"), type3.p.value, "")
        colnames(coxres) <- c("Variable", "Level", "N", "Hazard Ratio (95% CI)", "Type 3 P-value", "Log-rank P-value")
        if (input$assum == 0) {
          res[[i]] <- coxres
        } else if (input$assum == 1) {
          coxres2 <- cbind.data.frame(coxres, assump)
          colnames(coxres2)[7] <- "P-value for Proportional Hazards Assumption"
          res[[i]] <- coxres2
        }
        
      } else 
        return(NULL)
      
    }
    
    res_table <- do.call("rbind", res)
  }, rownames = FALSE) #, options = list(dom = 'tip'))
  
  
  output$pv21 <- renderUI({
    hs3 <- paste("&emsp;")
    hs4 <- paste("To Visualize the Kaplan Meier Plot:")
    HTML(paste(h2(strong(hs4)), hs3, sep = '<br/>'))
  })
  
  
  output$pv22 <- renderUI({
    hs3 <- paste("&emsp;")
    hs4 <- paste("Univariate Survival Association Analysis for Multiple Selected Variables")
    HTML(paste(hs3, h2(strong(hs4)), hs3, sep = '<br/>'))
  })
  
  output$downloadKM <- downloadHandler(
    filename <- function() {
      pdf_file4 <<- as.character(input$fname41)
      paste('KM_', pdf_file4, Sys.time(),'.pdf', sep='')
    },
    content <- function(file) {
      pdf(file=paste(pdf_file4,".pdf",sep="") , height= 8, width=12)
      
      data2 <- data_input4()
      for (j in 1:ncol(data2)) {
        if (class(data2[, j]) %in% c("character"))  
          data2[,j] <- as.factor(data2[,j])
        else data2[,j] = data2[, j]
      }
      if (input$select41 == "All Patients") {
        time <- data2[,input$select42]
        censor <- data2[,input$select43]
        dat <- cbind.data.frame(time, censor)
        fit <- survfit(Surv(as.numeric(time), as.numeric(factor(censor))) ~ 1, data = dat)
        # Drawing curves
        xlabel <- c("Years", "Months", "Days")
        b <- c(1, 12, 365)
        #TF <- c(TRUE, FALSE)
        if (input$riskt == TRUE) {
          res <- ggsurvplot(fit, data = dat, break.time.by = b[as.numeric(input$time)],  font.main = 18,
                            font.x =  14,
                            font.y = 14, xlab = xlabel[as.numeric(input$time)], pval = T, pval.method = T, 
                            pval.coord = c(max(time, na.rm= T)*0.8, 0.9), pval.method.coord = c(max(time, na.rm= T)*0.8, 0.95),
                            font.tickslab = 12,legend = c(0.2, 0.2), legend.title = input$select41,
                            risk.table = TRUE, risk.table.y.text.col = TRUE)#, fun = function(y) y*100)
          p <- grid.arrange(res$plot, res$table, nrow = 2, heights=c(0.7,0.3))
          
          
        } else {
          res <- ggsurvplot(fit, data = dat, break.time.by = b[as.numeric(input$time)],  font.main = 18,
                            font.x =  14,
                            font.y = 14, xlab = xlabel[as.numeric(input$time)], pval = T, pval.method = T, 
                            pval.coord = c(max(time, na.rm= T)*0.8, 0.9), pval.method.coord = c(max(time, na.rm= T)*0.8, 0.95),
                            font.tickslab = 12,legend = c(0.2, 0.2), legend.title = input$select41,
                            risk.table = FALSE, risk.table.y.text.col = FALSE)#, fun = function(y) y*100)
          p <- grid.arrange(res$plot, res$table, nrow = 2, heights=c(0.7,0.3))
          
          
        }
      } else {
        xvar <- data2[, input$select41]
        time <- data2[,input$select42]
        censor <- data2[,input$select43]
        dat <- cbind.data.frame(time, censor, xvar)
        if (is.null(xvar) | is.null(time) | is.null(censor)) {
          return(NULL)
        } else if (nlevels(xvar) > 30) {
          return(NULL)
        } else if (class(xvar) == "factor") {
          fit <- survfit(Surv(as.numeric(time), as.numeric(factor(censor))) ~ xvar, data = dat)
          # Drawing curves
          xlabel <- c("Years", "Months", "Days")
          b <- c(1, 12, 365)
          #TF <- c(TRUE, FALSE)
          if (input$riskt == TRUE) {
            res <- ggsurvplot(fit, data = dat, break.time.by = b[as.numeric(input$time)],  font.main = 18,
                              font.x =  14,
                              font.y = 14, xlab = xlabel[as.numeric(input$time)], pval = T, pval.method = T, 
                              pval.coord = c(max(time, na.rm= T)*0.8, 0.9), pval.method.coord = c(max(time, na.rm= T)*0.8, 0.95),
                              font.tickslab = 12,legend = c(0.2, 0.2), legend.title = input$select41, legend.labs = levels(xvar),
                              risk.table = TRUE, risk.table.y.text.col = TRUE)#, fun = function(y) y*100)
            p <- grid.arrange(res$plot, res$table, nrow = 2, heights=c(0.7,0.3))
            
            
          } else {
            res <- ggsurvplot(fit, data = dat, break.time.by = b[as.numeric(input$time)],  font.main = 18,
                              font.x =  14,
                              font.y = 14, xlab = xlabel[as.numeric(input$time)], pval = T, pval.method = T, 
                              pval.coord = c(max(time, na.rm= T)*0.8, 0.9), pval.method.coord = c(max(time, na.rm= T)*0.8, 0.95),
                              font.tickslab = 12,legend = c(0.2, 0.2), legend.title = input$select41, legend.labs = levels(xvar),
                              risk.table = FALSE, risk.table.y.text.col = FALSE)#, fun = function(y) y*100)
            p <- grid.arrange(res$plot, res$table, nrow = 2, heights=c(0.7,0.3))
            
            
          }
        }else if (class(xvar) %in% c("integer", "numeric") & input$binary == "continuous") {
          if (as.numeric(as.integer(input$cutoff2)) == 1) {
            perc <- optimalcut(dat)*100
          } else {perc <- as.numeric(as.integer(input$cutoff2))}
          
          dat$Group <- ifelse(dat[, 'xvar'] <= quantile(dat[, 'xvar'], perc/100), "Low", "High")
          Group <- as.factor(dat$Group)
          
          fit <- survfit(Surv(as.numeric(time), as.numeric(factor(censor))) ~ Group, data = dat)
          # Drawing curves
          xlabel <- c("Years", "Months", "Days")
          b <- c(1, 12, 365)
          #TF <- c(TRUE, FALSE)
          if (input$riskt == TRUE) {
            res <- ggsurvplot(fit, data = dat, break.time.by = b[as.numeric(input$time)],  font.main = 18,
                              font.x =  14,
                              font.y = 14, xlab = xlabel[as.numeric(input$time)], pval = T, pval.method = T, 
                              pval.coord = c(max(time, na.rm= T)*0.8, 0.9), pval.method.coord = c(max(time, na.rm= T)*0.8, 0.95),
                              font.tickslab = 12,legend = c(0.2, 0.2), legend.title = input$select41, legend.labs = levels(Group),
                              risk.table = TRUE, risk.table.y.text.col = TRUE)#, fun = function(y) y*100)
            p <- grid.arrange(res$plot, res$table, nrow = 2, heights=c(0.7,0.3))
            
            
          } else {
            res <- ggsurvplot(fit, data = dat, break.time.by = b[as.numeric(input$time)],  font.main = 18,
                              font.x =  14,
                              font.y = 14, xlab = xlabel[as.numeric(input$time)], pval = T, pval.method = T, 
                              pval.coord = c(max(time, na.rm= T)*0.8, 0.9), pval.method.coord = c(max(time, na.rm= T)*0.8, 0.95),
                              font.tickslab = 12,legend = c(0.2, 0.2), legend.title = input$select41, legend.labs = levels(Group),
                              risk.table = FALSE, risk.table.y.text.col = FALSE)#, fun = function(y) y*100)
            p <- grid.arrange(res$plot, res$table, nrow = 2, heights=c(0.7,0.3))
            
            
          }
        } else {
          return(NULL)
        }
      }
      dev.off()
      file.copy(paste(pdf_file4,'.pdf', sep='') ,file, overwrite=TRUE)
    })
  
  output$x44 = downloadHandler(
    filename <- function() {
      csv_file <<- as.character(input$fname42)
      paste('ST_', csv_file, Sys.time(),'.csv', sep='')
    },
    content = function(file) {
      data2 <- data_input4()
      for (j in 1:ncol(data2)) {
        if (class(data2[, j]) %in% c("character")) 
          data2[,j] <- as.factor(data2[,j])
        else data2[,j] = data2[, j]
      } 
      xvar <- data2[,input$show_vars46, drop = FALSE]
      time <- data2[,input$select44]
      censor <- data2[,input$select45]
      
      options(warn=-1)
      res <- list()
      for (i in 1: ncol(xvar)){
        x <- xvar[, i]
        if (is.null(x) | is.null(time) | is.null(censor)) {
          return(NULL)
        } else if (nlevels(x) > 30) {
          return(NULL)
        } else if (class(x) == "factor" | (class(x) %in% c("integer", "numeric") & input$isconti1 == "categorical")){
          
          if (class(x) %in% c("integer", "numeric")){
            dat <- cbind.data.frame(time, censor, xvar=x)
            if (as.numeric(as.integer(input$cutoff2)) == 1) {
              perc <- optimalcut(dat)*100
            } else {perc <- as.numeric(as.integer(input$cutoff2))}
            
            dat$Group <- ifelse(dat[, 'xvar'] <= quantile(dat[, 'xvar'], perc/100, na.rm= TRUE), "Low", "High")
            Group <- as.factor(dat$Group)
            x <- Group
          }
          
          ref <- gsub(".*:", "", input$exp_ref2[i])
          x <- relevel(x, ref)
          
          Variable <- c(colnames(xvar)[i], rep("", (length(levels(x))-1)))
          #x <- C(x, contr.treatment, base=3)
          fit <- coxph(Surv(as.numeric(time), as.numeric(factor(censor))) ~ x)
          temp <- cox.zph(fit)
          assum.p.value <- ifelse(temp$table[3] < 0.001, "<0.001", paste0(round(temp$table[3], 4)))
          assump <- c(assum.p.value, rep("", (length(levels(x))-1)))
          sum <- summary(fit)
          hazard.ratio = round(sum$conf.int[, 1], 2)
          lower95 = round(sum$conf.int[, 3], 2)
          upper95 = round(sum$conf.int[, 4], 2)
          logrank.p.value = ifelse(sum$sctest[3] < 0.001, "<0.001", paste0(round(sum$sctest[3], 4)))
          logrankp <- c(logrank.p.value, rep("", (length(levels(x))-1)))
          type3.p.value = ifelse(sum$coefficients[, 5] < 0.001, "<0.001", paste0(round(sum$coefficients[, 5], 4)))
          counts <- data.frame(table(x))
          counts <- rbind.data.frame(counts[2:nrow(counts),], counts[1, ])
          coxres <- cbind.data.frame(Variable, counts, c(paste0(hazard.ratio, " (", lower95,
                                                                "-", upper95, ")"), ""), c(type3.p.value, ""), logrankp)
          colnames(coxres) <- c("Variable", "Level", "N", "Hazard Ratio (95% CI)", "Type 3 P-value", "Log-rank P-value")
          if (input$assum == 0) {
            res[[i]] <- coxres
          } else if (input$assum == 1) {
            coxres2 <- cbind.data.frame(coxres, assump)
            colnames(coxres2)[7] <- "P-value for Proportional Hazards Assumption"
            res[[i]] <- coxres2
          }
        } else if (class(x) %in% c("integer", "numeric") & input$isconti1 == "continuous") {
          fit <- coxph(Surv(as.numeric(time), as.numeric(factor(censor))) ~ x)
          temp <- cox.zph(fit)
          assump <- ifelse(temp$table[3] < 0.001, "<0.001", paste0(round(temp$table[3], 4)))   
          
          sum <- summary(fit)
          Variable <- colnames(xvar)[i]
          hazard.ratio <- round(sum$conf.int[, 1], 2)
          lower95 <- round(sum$conf.int[, 3], 2)
          upper95 <- round(sum$conf.int[, 4], 2)
          
          type3.p.value <- ifelse(sum$coefficients[, 5] < 0.001, "<0.001", paste0(round(sum$coefficients[, 5], 4)))
          counts <- length(x)
          coxres <- cbind.data.frame(Variable, "", counts, paste0(hazard.ratio, " (", lower95, "-", upper95, ")"), type3.p.value, "")
          colnames(coxres) <- c("Variable", "Level", "N", "Hazard Ratio (95% CI)", "Type 3 P-value", "Log-rank P-value")
          if (input$assum == 0) {
            res[[i]] <- coxres
          } else if (input$assum == 1) {
            coxres2 <- cbind.data.frame(coxres, assump)
            colnames(coxres2)[7] <- "P-value for Proportional Hazards Assumption"
            res[[i]] <- coxres2
          }
          
        } else 
          return(NULL)
      }
      
      res_table <- do.call("rbind", res)
      
      write.csv(res_table, file, row.names = F)
      
    })
  
  
  
}