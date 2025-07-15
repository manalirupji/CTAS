aetable <- function(adae, bySOC, topdisplay, top, big_n ) {
  # Step: 2 - Selection of
  adae2 <<- subset(adae, select = c(SEQUENCE_NO, SOC,
                                   AETERMC, AETOXGR_N, ARMS))
  
  #Sorting
  adae3 <- adae2[order(adae2$SOC, adae2$AETERMC, rev(adae2$AETOXGR_N)),]
  
  # by SOC
  if(bySOC == TRUE) {
  
  #Step 3:Select Highest Toxicity Grade AEs
  any_ae <- adae3 %>%
    group_by(SEQUENCE_NO, ARMS) %>%
    arrange(SEQUENCE_NO, ARMS, -AETOXGR_N)%>%
    slice_head(n=1)
  
  any_soc <- adae3 %>%
    group_by(SEQUENCE_NO, ARMS, SOC) %>%
    arrange(SEQUENCE_NO, ARMS, SOC,-AETOXGR_N)%>%
    slice_head(n=1)
  
  any_pref <- adae3 %>%
    group_by(SEQUENCE_NO, ARMS, AETERMC) %>%
    arrange(SEQUENCE_NO,ARMS, AETERMC,-AETOXGR_N)%>%
    slice_head(n=1)
  
  #Step 4:Get Freq and Calculate %- ANY AE
  
  #Get Big N from AE ds
  #df <- table(any_ae$SEQUENCE_NO, any_ae$ARMS)
  #bign<- colSums(df)
  
  #Get Big N from demo
  bign <- big_n
  
  any_ae1 <- any_ae %>%
    group_by(ARMS)%>%
    summarise(n=n(),.groups = "keep") %>%
    mutate( pct = if_else(ARMS == "A", n*100/bign[1], n*100/bign[2]),
            ORD1=0,
            ORD2=0,
            txt="Total Subjects with any Event",
            SOC="Total Subjects with any Event",
            AETERMC = " " )
  
  #::B:: Get Freq and Calculate % - ANY AEBODSYS
  any_bodsys1 <- any_soc %>%
    group_by(ARMS, SOC)%>%
    summarise(n=n(),.groups = "keep") %>%
    mutate( pct = if_else(ARMS == 'A', n*100/bign[1], n*100/bign[2] ),
            ORD1=1,
            ORD2=0, 
            ORD3=9999,
            txt=SOC, 
            AETERMC = " " )
  # To Use for Ordering ;
  any_bodsys_ord <- any_bodsys1 %>%
    group_by(SOC ) %>%
    summarise(ORD2=sum(ORD2), .groups ="keep")
  
  #::C:: Get Freq and Calculate % - ANY AEBODSYS-AEDECOD
  any_decod1 <- any_pref %>%
    group_by(ARMS,SOC, AETERMC )%>%
    summarise(n=n(),.groups = "keep") %>%
    mutate( pct = if_else(ARMS == 'A', n*100/bign[1], n*100/bign[2] ),
            ORD1=1,
            ORD2=9999,
            ORD3=n, 
            txt=paste("&nbsp;", AETERMC, sep = "&nbsp;") )
  
  
  # Step: 5 - Data Arrangement
  
  all_ae <- rbind(any_ae1, any_bodsys1, any_decod1)
  all_ae1 <- all_ae[order(all_ae$ARMS, all_ae$ORD1,all_ae$SOC, -all_ae$ORD2, -all_ae$ORD3),]
  
  #Create Freq (PCT) variable
  
  all_ae2 <<- all_ae1 %>% 
    mutate(var1 = paste (n,"(",format(round(pct,digits = 2),nsmall = 2),")"),
           ARMS = ifelse(ARMS=="A", "DrugA","DrugB"))
  
  
  #--------------------------------
  # Transpose Data 
  #--------------------------------
  if(length(table(all_ae2$ARMS)) >1) {
  
  t_ae <- all_ae2 %>%
    ungroup()%>%
    dplyr::select(-n,-pct, -AETERMC, -ORD3) %>%  #-TRTAN
    spread(ARMS, var1) %>%
    mutate(DrugA= ifelse(is.na(DrugA),0, DrugA), DrugB = ifelse(is.na(DrugB),0, DrugB) )%>%
    arrange(ORD1, SOC, ORD2)
  # Drop unnecessary variables
  t_ae1 <- subset(t_ae, select=c(-ORD1, -ORD2, -SOC))
  
  } else {
    t_ae <<- all_ae2 %>%
      ungroup()%>%
      dplyr::select(-n,-pct, -AETERMC, -ORD3) %>%  #-TRTAN
      spread(ARMS, var1) %>%
    #  mutate(DrugA= ifelse(is.na(DrugA),0, DrugA), DrugB = ifelse(is.na(DrugB),0, DrugB) )%>%
      arrange(ORD1, SOC, ORD2)
    # Drop unnecessary variables
    t_ae1 <- subset(t_ae, select=c(-ORD1, -ORD2, -SOC))
  }
  
  
  ############### By Grade 3-4 ##################
  
  any_ae_Gr34 <- any_ae[any_ae$AETOXGR_N %in% c(3, 4),]
  
  any_ae1_Gr34 <- any_ae_Gr34 %>%
    group_by(ARMS)%>%
    summarise(n=n(),.groups = "keep") %>%
    mutate( pct = if_else(ARMS == "A", n*100/bign[1], n*100/bign[2]),
            ORD1=0,
            ORD2=0,
            txt="Total Subjects with any Event",
            SOC="Total Subjects with any Event",
            AETERMC = " " )
  
  
  any_soc_Gr34 <- any_soc[any_soc$AETOXGR_N %in% c(3, 4),] 
  
  #::B:: Get Freq and Calculate % - ANY AEBODSYS
  any_bodsys1_Gr34 <- any_soc_Gr34 %>%
    group_by(ARMS, SOC)%>%
    summarise(n=n(),.groups = "keep") %>%
    mutate( pct = if_else(ARMS == 'A', n*100/bign[1], n*100/bign[2] ),
            ORD1=1,
            ORD2=0, 
            ORD3=9999,
            txt=SOC, 
            AETERMC = " " )
  # To Use for Ordering ;
  any_bodsys_ord_Gr34 <- any_bodsys1_Gr34 %>%
    group_by(SOC ) %>%
    summarise(ORD2=sum(ORD2), .groups ="keep")
  
  #::C:: Get Freq and Calculate % - ANY AEBODSYS-AEDECOD
  any_pref_Gr34 <- any_pref[any_pref$AETOXGR_N %in% c(3, 4),] 
  
  any_decod1_Gr34 <- any_pref_Gr34 %>%
    group_by(ARMS,SOC, AETERMC )%>%
    summarise(n=n(),.groups = "keep") %>%
    mutate( pct = if_else(ARMS == 'A', n*100/bign[1], n*100/bign[2] ),
            ORD1=1,
            ORD2=9999,
            ORD3=n, 
            txt=paste("&nbsp;", AETERMC, sep = "&nbsp;") )
  
  # Step: 5 - Data Arrangement
  all_ae_Gr34 <- rbind(any_ae1_Gr34, any_bodsys1_Gr34, any_decod1_Gr34)
  all_ae1_Gr34 <- all_ae_Gr34[order(all_ae_Gr34$ARMS, all_ae_Gr34$ORD1,all_ae_Gr34$SOC, -all_ae_Gr34$ORD2, -all_ae_Gr34$ORD3),]
  
  #Create Freq (PCT) variable
  
  all_ae2_Gr34 <<- all_ae1_Gr34 %>% 
    mutate(var1 = paste (n,"(",format(round(pct,digits = 2),nsmall = 2),")"),
           ARMS = ifelse(ARMS=="A", "DrugA","DrugB"))
  
  #--------------------------------
  # Transpose Data 
  #--------------------------------
  if(length(table(all_ae2_Gr34$ARMS))>1) {
  t_ae_Gr34 <- all_ae2_Gr34 %>%
    ungroup()%>%
    dplyr::select(-n,-pct, -AETERMC, -ORD3) %>%  #-TRTAN
    spread(ARMS, var1) %>%
    mutate(DrugA= ifelse(is.na(DrugA),0, DrugA), DrugB = ifelse(is.na(DrugB),0, DrugB) )%>%
    arrange(ORD1, SOC, ORD2)
  # Drop unnecessary variables
  t_ae1_Gr34 <- subset(t_ae_Gr34, select=c(-ORD1, -ORD2, -SOC))
  
  colnames(t_ae1_Gr34)[2] <- "Grade 3-4 Drug A"
  colnames(t_ae1_Gr34)[3] <- "Grade 3-4 Drug B"
  
  
  t_ae1$id  <- 1:nrow(t_ae1)
  
  m <- merge(t_ae1, t_ae1_Gr34, by = "txt", all.x= T)
  
  m2 <- m[order(m$id),]
  m2 <- m2 [, -which(colnames(m2) %in% c("id"))]
  
  names(m2)[1] <- "Adverse Events"
  
  m2 <- m2 [, c(1, 2, 4, 3, 5)]
  
  if (!("Grade 3-4 Drug A" %in% colnames(m2))) {
    m2$`Grade 3-4 Drug A` <- NA
  }
  
  if (!("Grade 3-4 Drug B" %in% colnames(m2))) {
    m2$`Grade 3-4 Drug B` <- NA
  }
  
  #m2$`Grade 3-4 Drug A` <- ifelse(is.na(m2$`Grade 3-4 Drug A`), 0, m2$`Grade 3-4 Drug A`)
  #m2$`Grade 3-4 Drug B` <- ifelse(is.na(m2$`Grade 3-4 Drug B`), 0, m2$`Grade 3-4 Drug B`)
  
  
  #colnames(m2)[2] <- paste("All Grade Arm A (n = ", bign[1],  ")", sep = "")
  #colnames(m2)[3] <- paste("Grade 3-4 Arm A (n = ", bign[1],  ")", sep = "")
  #colnames(m2)[4] <- paste("All Grade Arm B (n = ", bign[2],  ")", sep = "")
  #colnames(m2)[5] <- paste("Grade 3-4 Arm B (n = ", bign[2],  ")", sep = "")
  
  m2$`Grade 3-4 Drug A` <- ifelse(is.na(m2$`Grade 3-4 Drug A`), 0, m2$`Grade 3-4 Drug A`)
  
  if(!is.na(bign[2])) {
    m2$`Grade 3-4 Drug B` <- ifelse(is.na(m2$`Grade 3-4 Drug B`), 0, m2$`Grade 3-4 Drug B`)
    
    m2 <- m2[, c("Adverse Events", "DrugA", "Grade 3-4 Drug A", "DrugB", "Grade 3-4 Drug B")]
    
    colnames(m2)[which(names(m2) == "DrugA")] <- paste("All Grade Arm A (n = ", bign[1],  ")", sep = "")
    colnames(m2)[which(names(m2) == "DrugB")] <- paste("All Grade Arm B (n = ", bign[2],  ")", sep = "")
    colnames(m2)[which(names(m2) == "Grade 3-4 Drug A")] <- paste("Grade 3-4 Arm A (n = ", bign[1],  ")", sep = "")
    colnames(m2)[which(names(m2) == "Grade 3-4 Drug B")] <- paste("Grade 3-4 Arm B (n = ", bign[2],  ")", sep = "")
    
  } else {
    
    m2 <- m2[, c("Adverse Events", "DrugA", "Grade 3-4 Drug A")]
    
    colnames(m2)[which(names(m2) == "DrugA")] <- paste("All Grade Arm A (n = ", bign[1],  ")", sep = "")
    colnames(m2)[which(names(m2) == "Grade 3-4 Drug A")] <- paste("Grade 3-4 Arm A (n = ", bign[1],  ")", sep = "")
  }
  check_m2 <<- m2
  
  return(m2)
  
  } else {
    t_ae_Gr34 <- all_ae2_Gr34 %>%
      ungroup()%>%
     dplyr::select(-n,-pct, -AETERMC, -ORD3) %>%  #-TRTAN
      spread(ARMS, var1) %>%
      #mutate(DrugA= ifelse(is.na(DrugA),0, DrugA), DrugB = ifelse(is.na(DrugB),0, DrugB) )%>%
      arrange(ORD1, SOC, ORD2)
    
    # if no Grade 3-4 
    #if(nrow(t_ae1_Gr34)==0) {
    #  rows <- nrow(t_ae_Gr34)
    #  t_ae_Gr34[rows+1,] <- NA
    #}
    
    # Drop unnecessary variables
    t_ae1_Gr34 <- subset(t_ae_Gr34, select=c(-ORD1, -ORD2, -SOC))
    
    colnames(t_ae1_Gr34)[2] <- "Grade 3-4 Drug A"
    #colnames(t_ae1_Gr34)[3] <- "Grade 3-4 Drug B"
    
    t_ae1$id  <- 1:nrow(t_ae1)
    
    m <- merge(t_ae1, t_ae1_Gr34, by = "txt", all.x= T)
    
    m2 <- m[order(m$id),]
    m2 <- m2 [, -which(colnames(m2) %in% c("id"))]
    
    names(m2)[1] <- "Adverse Events"
    
    #m2 <- m2 [, c(1, 2, 4, 3, 5)]
    
    if (!("Grade 3-4 Drug A" %in% colnames(m2))) {
      m2$`Grade 3-4 Drug A` <- NA
    }
    
    if (!("Grade 3-4 Drug B" %in% colnames(m2))) {
      m2$`Grade 3-4 Drug B` <- NA
    }
    
    m2$`Grade 3-4 Drug A` <- ifelse(is.na(m2$`Grade 3-4 Drug A`), 0, m2$`Grade 3-4 Drug A`)
    
    if(!is.na(bign[2])) {
    m2$`Grade 3-4 Drug B` <- ifelse(is.na(m2$`Grade 3-4 Drug B`), 0, m2$`Grade 3-4 Drug B`)
    
    m2 <- m2[, c("Adverse Events", "DrugA", "Grade 3-4 Drug A", "DrugB", "Grade 3-4 Drug B")]
    
    colnames(m2)[which(names(m2) == "DrugA")] <- paste("All Grade Arm A (n = ", bign[1],  ")", sep = "")
    colnames(m2)[which(names(m2) == "DrugB")] <- paste("All Grade Arm B (n = ", bign[2],  ")", sep = "")
    colnames(m2)[which(names(m2) == "Grade 3-4 Drug A")] <- paste("Grade 3-4 Arm A (n = ", bign[1],  ")", sep = "")
    colnames(m2)[which(names(m2) == "Grade 3-4 Drug B")] <- paste("Grade 3-4 Arm B (n = ", bign[2],  ")", sep = "")
    
    } else {
    
    m2 <- m2[, c("Adverse Events", "DrugA", "Grade 3-4 Drug A")]
    
    colnames(m2)[which(names(m2) == "DrugA")] <- paste("All Grade Arm A (n = ", bign[1],  ")", sep = "")
   colnames(m2)[which(names(m2) == "Grade 3-4 Drug A")] <- paste("Grade 3-4 Arm A (n = ", bign[1],  ")", sep = "")
    }
    
    check_m2 <<- m2
    
    return(m2)
    
  }
  
  # not by SOC
  } else {
    
    #Step 3:Select Highest Toxicity Grade AEs
    any_ae <- adae3 %>%
      group_by(SEQUENCE_NO, ARMS) %>%
      arrange(SEQUENCE_NO, ARMS, -AETOXGR_N)%>%
      slice_head(n=1)
    
    any_pref <- adae3 %>%
      group_by(SEQUENCE_NO, ARMS, AETERMC) %>%
      arrange(SEQUENCE_NO,ARMS, AETERMC, -AETOXGR_N)%>%
      slice_head(n=1)
    
    #Step 4:Get Freq and Calculate %- ANY AE
    
    #Get Big N from AE
    #df <- table(any_ae$SEQUENCE_NO, any_ae$ARMS)
    #bign<- colSums(df)
    
    #Get Big N from demo
    bign <- big_n
    
    any_ae1 <- any_ae %>%
      group_by(ARMS)%>%
      summarise(n=n(),.groups = "keep") %>%
      mutate( pct = if_else(ARMS == "A", n*100/bign[1], n*100/bign[2]),
              ORD1=0,
              ORD2=0,
              txt="Total Subjects with any Event",
              SOC="Total Subjects with any Event",
              AETERMC = " " )
    
    #::C:: Get Freq and Calculate % - ANY AEBODSYS-AEDECOD
    any_decod1 <- any_pref %>%
      group_by(ARMS,AETERMC )%>%
      summarise(n=n(),.groups = "keep") %>%
      mutate( pct = if_else(ARMS == 'A', n*100/bign[1], n*100/bign[2] ),
              ORD1=1,
              ORD2=9999,
              ORD3=n, 
              txt=paste("", AETERMC, sep = "") )
    
    # Step: 5 - Data Arrangement
    all_ae <- rbind(any_ae1, any_decod1)
    all_ae1 <- all_ae[order(all_ae$ARMS, all_ae$ORD1, rev(all_ae$ORD3)),]
    
    #Create Freq (PCT) variable
    
    all_ae2 <- all_ae1 %>% 
      mutate(var1 = paste (n,"(",format(round(pct,digits = 2),nsmall = 2),")"),
             ARMS = ifelse(ARMS=="A", "DrugA","DrugB"))
    
     
    #--------------------------------
    # Transpose Data 
    #--------------------------------
    
    if(length(table(all_ae2$ARMS))>1){
    t_ae <- all_ae2 %>%
      ungroup()%>%
      dplyr::select(-n,-pct, -AETERMC, -ORD3) %>%  #-TRTAN # ORD3 has the order
      spread(ARMS, var1) %>%
      mutate(DrugA= ifelse(is.na(DrugA),0, DrugA), DrugB = ifelse(is.na(DrugB),0, DrugB) )%>%
      arrange(ORD1, ORD2)
    # Drop unnecessary variables
    t_ae1 <- subset(t_ae, select=c(-ORD1, -ORD2, -SOC))
    } else {
      t_ae <- all_ae2 %>%
        ungroup()%>%
        dplyr::select(-n,-pct, -AETERMC, -ORD3) %>%  #-TRTAN # ORD3 has the order
        spread(ARMS, var1) %>%
        #mutate(DrugA= ifelse(is.na(DrugA),0, DrugA), DrugB = ifelse(is.na(DrugB),0, DrugB) )%>%
        arrange(ORD1, ORD2)
      # Drop unnecessary variables
      t_ae1 <- subset(t_ae, select=c(-ORD1, -ORD2, -SOC))
    }
    
    
    
    order1 <- all_ae2[all_ae2$ARMS== "DrugA", c("txt")]
    order2 <- all_ae2[all_ae2$ARMS== "DrugB", c("txt")]
   
    order <- order2 %>%
      filter(!order2$txt %in% order1$txt)
    
    order3 <- rbind(order1, order)
    order3$id <- 1:nrow(order3)
    
    t_ae1_ordered <- merge(t_ae1, order3, by = "txt", all.x =T)
    #t_ae1_ordered$id <- t_ae1_ordered[order(t_ae1_ordered$id),] 
    check_t_ae1_ordered <<- t_ae1_ordered[order(t_ae1_ordered$id),]
   
    ############### By Grade 3-4 ##################
    
    check_any_ae <<- any_ae # seems wrong
    
    any_ae_Gr34 <- any_ae[any_ae$AETOXGR_N %in% c(3, 4),]
    
    any_ae1_Gr34 <- any_ae_Gr34 %>%
      group_by(ARMS)%>%
      summarise(n=n(),.groups = "keep") %>%
      mutate( pct = if_else(ARMS == "A", n*100/bign[1], n*100/bign[2]),
              ORD1=0,
              ORD2=0,
              txt="Total Subjects with any Event",
              SOC="Total Subjects with any Event",
              AETERMC = " " )
    
    #::C:: Get Freq and Calculate % - ANY AEBODSYS-AEDECOD
    
    any_pref_Gr34 <- any_pref[any_pref$AETOXGR_N %in% c(3, 4),]
    any_decod1_Gr34 <- any_pref_Gr34 %>%
      group_by(ARMS,AETERMC )%>%
      summarise(n=n(),.groups = "keep") %>%
      mutate( pct = if_else(ARMS == 'A', n*100/bign[1], n*100/bign[2] ),
              ORD1=1,
              ORD2=9999,
              ORD3=n, 
              txt=paste("", AETERMC, sep = "") )
    
    # Step: 5 - Data Arrangement
    all_ae_Gr34 <- rbind(any_ae1_Gr34, any_decod1_Gr34)
    all_ae1_Gr34 <- all_ae_Gr34[order(all_ae_Gr34$ARMS, all_ae_Gr34$ORD1, -all_ae_Gr34$ORD3),]
    
    #Create Freq (PCT) variable
    
    all_ae1_Gr34 <- all_ae1_Gr34 %>% 
      mutate(var1 = paste (n,"(",format(round(pct,digits = 2),nsmall = 2),")"),
             ARMS = ifelse(ARMS=="A", "DrugA","DrugB"))
    
    check_all_ae1_Gr34 <<- all_ae1_Gr34 ## incorrect calculation
    #--------------------------------
    # Transpose Data 
    #--------------------------------
    if(length(table(all_ae1_Gr34$ARMS)) >1 ){
    t_ae_Gr34 <- all_ae1_Gr34 %>%
      ungroup()%>%
      dplyr::select(-n,-pct, -AETERMC, -ORD3) %>%  #-TRTAN
      spread(ARMS, var1) %>%
      mutate(DrugA= ifelse(is.na(DrugA),0, DrugA), DrugB = ifelse(is.na(DrugB),0, DrugB) )%>%
      arrange(ORD1, ORD2)
    # Drop unnecessary variables
    t_ae1_Gr34 <- subset(t_ae_Gr34, select=c(-ORD1, -ORD2, -SOC))
    
    colnames(t_ae1_Gr34)[2] <- "Grade 3-4 Drug A"
    colnames(t_ae1_Gr34)[3] <- "Grade 3-4 Drug B"
    
    #t_ae1_ordered$id  <- 1:nrow(t_ae1_ordered)
    
    m <- merge(t_ae1_ordered, t_ae1_Gr34, by = "txt", all.x= T)
    
    m2 <- m[order(m$id),]
    m2 <- m2 [, -which(names(m2) %in% c("id"))]
    
    names(m2)[1] <- "Adverse Events"
    
    m2 <- m2 [, c(1, 2, 4, 3, 5)]
    
    
    
    if (!("Grade 3-4 Drug A" %in% colnames(m2))) {
      m2$`Grade 3-4 Drug A` <- NA
    }
    
    if (!("Grade 3-4 Drug B" %in% colnames(m2))) {
      m2$`Grade 3-4 Drug B` <- NA
    }
    
    #m2$`Grade 3-4 Drug A` <- ifelse(is.na(m2$`Grade 3-4 Drug A`), 0, m2$`Grade 3-4 Drug A`)
    #m2$`Grade 3-4 Drug B` <- ifelse(is.na(m2$`Grade 3-4 Drug B`), 0, m2$`Grade 3-4 Drug B`)
    
    
    #colnames(m2)[2] <- paste("All Grade Arm A (n = ", bign[1],  ")", sep = "")
    #colnames(m2)[3] <- paste("Grade 3-4 Arm A (n = ", bign[1],  ")", sep = "")
    #colnames(m2)[4] <- paste("All Grade Arm B (n = ", bign[2],  ")", sep = "")
    #colnames(m2)[5] <- paste("Grade 3-4 Arm B (n = ", bign[2],  ")", sep = "")
    
    if (!("Grade 3-4 Drug A" %in% colnames(m2))) {
      m2$`Grade 3-4 Drug A` <- NA
    }
    
    if (!("Grade 3-4 Drug B" %in% colnames(m2))) {
      m2$`Grade 3-4 Drug B` <- NA
    }
    
    m2$`Grade 3-4 Drug A` <- ifelse(is.na(m2$`Grade 3-4 Drug A`), 0, m2$`Grade 3-4 Drug A`)
    
    if(!is.na(bign[2])) {
      m2$`Grade 3-4 Drug B` <- ifelse(is.na(m2$`Grade 3-4 Drug B`), 0, m2$`Grade 3-4 Drug B`)
      
      m2 <- m2[, c("Adverse Events", "DrugA", "Grade 3-4 Drug A", "DrugB", "Grade 3-4 Drug B")]
      
      colnames(m2)[which(names(m2) == "DrugA")] <- paste("All Grade Arm A (n = ", bign[1],  ")", sep = "")
      colnames(m2)[which(names(m2) == "DrugB")] <- paste("All Grade Arm B (n = ", bign[2],  ")", sep = "")
      colnames(m2)[which(names(m2) == "Grade 3-4 Drug A")] <- paste("Grade 3-4 Arm A (n = ", bign[1],  ")", sep = "")
      colnames(m2)[which(names(m2) == "Grade 3-4 Drug B")] <- paste("Grade 3-4 Arm B (n = ", bign[2],  ")", sep = "")
      
    } else {
      
      m2 <- m2[, c("Adverse Events", "DrugA", "Grade 3-4 Drug A")]
      
      colnames(m2)[which(names(m2) == "DrugA")] <- paste("All Grade Arm A (n = ", bign[1],  ")", sep = "")
      colnames(m2)[which(names(m2) == "Grade 3-4 Drug A")] <- paste("Grade 3-4 Arm A (n = ", bign[1],  ")", sep = "")
    }
   
    } # Single ARm
    else {
      t_ae_Gr34 <- all_ae1_Gr34 %>%
        ungroup()%>%
        dplyr::select(-n,-pct, -AETERMC, -ORD3) %>%  #-TRTAN
        spread(ARMS, var1) %>%
        #mutate(DrugA= ifelse(is.na(DrugA),0, DrugA), DrugB = ifelse(is.na(DrugB),0, DrugB) )%>%
        arrange(ORD1, ORD2)
      # Drop unnecessary variables
      t_ae1_Gr34 <- subset(t_ae_Gr34, select=c(-ORD1, -ORD2, -SOC))
      
      colnames(t_ae1_Gr34)[2] <- "Grade 3-4 Drug A"
      #colnames(t_ae1_Gr34)[3] <- "Grade 3-4 Drug B"
      
      #t_ae1_ordered$id  <- 1:nrow(t_ae1_ordered)
   
      
      m <- merge(t_ae1_ordered, t_ae1_Gr34, by = "txt", all.x= T)
      
      m2 <- m[order(m$id),]
      m2 <- m2 [, -which(names(m2) %in% c("id"))]
      
      names(m2)[1] <- "Adverse Events"
      
      #m2 <- m2 [, c(1, 2, 4, 3, 5)]
      
      if (!("Grade 3-4 Drug A" %in% colnames(m2))) {
        m2$`Grade 3-4 Drug A` <- NA
      }
      
      if (!("Grade 3-4 Drug B" %in% colnames(m2))) {
        m2$`Grade 3-4 Drug B` <- NA
      }
      
      m2$`Grade 3-4 Drug A` <- ifelse(is.na(m2$`Grade 3-4 Drug A`), 0, m2$`Grade 3-4 Drug A`)
      
      if(!is.na(bign[2])) {
        m2$`Grade 3-4 Drug B` <- ifelse(is.na(m2$`Grade 3-4 Drug B`), 0, m2$`Grade 3-4 Drug B`)
        
        m2 <- m2[, c("Adverse Events", "DrugA", "Grade 3-4 Drug A", "DrugB", "Grade 3-4 Drug B")]
        
        colnames(m2)[which(names(m2) == "DrugA")] <- paste("All Grade Arm A (n = ", bign[1],  ")", sep = "")
        colnames(m2)[which(names(m2) == "DrugB")] <- paste("All Grade Arm B (n = ", bign[2],  ")", sep = "")
        colnames(m2)[which(names(m2) == "Grade 3-4 Drug A")] <- paste("Grade 3-4 Arm A (n = ", bign[1],  ")", sep = "")
        colnames(m2)[which(names(m2) == "Grade 3-4 Drug B")] <- paste("Grade 3-4 Arm B (n = ", bign[2],  ")", sep = "")
        
      } else {
        
        m2 <- m2[, c("Adverse Events", "DrugA", "Grade 3-4 Drug A")]
        
        colnames(m2)[which(names(m2) == "DrugA")] <- paste("All Grade Arm A (n = ", bign[1],  ")", sep = "")
        colnames(m2)[which(names(m2) == "Grade 3-4 Drug A")] <- paste("Grade 3-4 Arm A (n = ", bign[1],  ")", sep = "")
      }
      check_m <<- m2
     
    }
    
    
    if (topdisplay == FALSE){
      return(m2)
    } else {
      final_m2 <<- m2
      m2$extracted <- as.numeric(sub(" \\(.*", "", m2[,2]))
      m2 <- m2[order(-(m2$extracted)),]
      final_c <<- m2
      
    # m3 <- m2[(1:top)+1, -6]
      m2 <- m2[, -6]
     #return(m3)  
     return(head(m2, n= top +1))
    }
  }
  
 
}