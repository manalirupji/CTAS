dot_plot <- function(adae, bySOC, topdisplay, top, orderbyB, SOCs, big_n= big_n) {
  
  # load data
  # Step: 2 - Selection of
  adae2 <- subset(adae, select = c(SEQUENCE_NO, SOC,
                                   AETERMC, AETOXGR_N, ARMS))
  
  #Sorting
  adae3 <- adae2[order(adae2$SOC, adae2$AETERMC, -adae2$AETOXGR_N),]
  
  #Step 3:Select Highest Toxicity Grade AEs
  any_ae <- adae3 %>%
    group_by(SEQUENCE_NO, ARMS) %>%
    arrange(SEQUENCE_NO, ARMS, -AETOXGR_N)%>%
    slice_head(n=1)
  
  any_pref <- adae3 %>%
    group_by(SEQUENCE_NO, ARMS, AETERMC) %>%
    arrange(SEQUENCE_NO,ARMS, AETERMC,-AETOXGR_N)%>%
    slice_head(n=1)
  
  #Step 4:Get Freq and Calculate %- ANY AE
  
  #Get Big N from AE data
  #df <- table(any_ae$SEQUENCE_NO, any_ae$ARMS)
  #bign <- colSums(df)
  
  # Get Big N from demo data
  bign <- big_n
  
  #::C:: Get Freq and Calculate % - ANY AEBODSYS-AEDECOD
  any_decod1 <- any_pref %>%
    group_by(ARMS, AETERMC )%>%
    summarise(n=n(),.groups = "keep") %>%
    mutate( pct = if_else(ARMS == 'A', n*100/bign[1], n*100/bign[2] ),
            ORD1=1,
            ORD2=9999,
            ORD3=n, 
            txt=paste("", AETERMC, sep = "")
            )
  
   # Step: 5 - Data Arrangement
  
  SOC_names <- adae3[, c("SOC", "AETERMC")]
  SOC_names2 <- SOC_names[!duplicated(SOC_names$AETERMC),]
  
  all_ae <- merge(any_decod1, SOC_names2, by = "AETERMC")
  
  all_ae2 <- all_ae %>% 
    mutate(nAE = n,
           ARMS = ifelse(ARMS=="A", "DrugA","DrugB"))
  
  t_ae <- all_ae2 %>%
    ungroup()%>%
    dplyr::select(-n,-pct, -AETERMC, -ORD3) %>%  #-TRTAN
    spread(ARMS, nAE) %>%
    mutate(DrugA= ifelse(is.na(DrugA),0, DrugA), DrugB = ifelse(is.na(DrugB),0, DrugB) )%>%
    mutate(diff = DrugB-DrugA,
           tot = DrugA+DrugB) %>%
    arrange(ORD1, SOC, ORD2)
  # Drop unnecessary variables
 
  t_ae1 <- subset(t_ae, select=c(-ORD1, -ORD2, -SOC))
  
  
  ae_ds <<- reshape2::melt(t_ae1, id.vars=c("txt", "diff", "tot"))
  colnames(ae_ds)[1] <- "AETERMC"
  colnames(ae_ds)[4] <- "TRT"
  colnames(ae_ds)[5] <- "nAE"
   
  AEdata <- merge(ae_ds, SOC_names2, by = "AETERMC")
 
  
  AEdata$nTRT <- ifelse(AEdata$TRT == "DrugA", bign[1], bign[2])

  AEdata$TRT <- as.factor(ifelse(AEdata$TRT == "DrugA", "Arm A", "Arm B"))
  AEdata$AETERMC <- as.factor(AEdata$AETERMC)
  AEdata$SOC <- as.factor(AEdata$SOC)
  
  AEdata <<- AEdata[order(-AEdata$nAE), ]
  
  #AEdata$tot <- 
  
   if(length(table(AEdata$TRT)) >1) {
    if(bySOC == TRUE) {
       AEdata <- AEdata[AEdata$SOC %in% SOCs,]
       AEdotplot(AETERMC ~ nAE/nTRT | SOC, groups = TRT, data = AEdata, 
                 sortbyVar="PCT", main = "", sub = "", 
                 cex.AB.y.scale=.9, col.AB=c("red","blue"),
                 cex.x.scale=0.7, par.strip.text=list(cex=.9))
       #print(tmp, AEtable=FALSE)
    } else if(bySOC == FALSE & topdisplay == FALSE) {
      AEdotplot(AETERMC ~ nAE/nTRT, groups = TRT, data = AEdata, 
                sortbyVar="PCT", main = "", sub= "", cex.AB.y.scale=.9, 
                col.AB=c("red","blue"), cex.x.scale=.7)
    } else if(bySOC == FALSE & topdisplay == TRUE) {
      AEdata <- AEdata[order(AEdata$TRT),]
      AEdata <- AEdata[order(AEdata$AETERMC),]
      
      if(orderbyB == TRUE) {
        AEdata <- AEdata[order(-AEdata$diff),]
        AEdata <- AEdata[1:(top*2),]
        AEdotplot(AETERMC ~ nAE/nTRT, groups = TRT, data = AEdata, 
                  sortbyVar="PCT", sortbyVarBegin=2, main = "", 
                  sub= "", cex.AB.y.scale=.9, col.AB=c("red","blue"),  cex.x.scale=.7)
        }
      else if(orderbyB == FALSE) {
        AEdata <- AEdata[order(AEdata$diff),]
        AEdata <- AEdata[1:(top*2),]
        AEdotplot(AETERMC ~ nAE/nTRT, groups = TRT, data = AEdata, 
                  sortbyVar="PCT", sortbyVarBegin=1, main = "", sub= "", 
                  cex.AB.y.scale=.9, col.AB=c("red","blue"), cex.x.scale=.7)
        }
    }
   
  } else {
    return("Atleast Two treatment groups are needed")
  }
   
}