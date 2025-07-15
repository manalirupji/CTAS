volcano_plot <- function(adae, pval_cutoff) {
  
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
  
  #Get Big N
  df <- table(any_ae$SEQUENCE_NO, any_ae$ARMS)
  bign<- colSums(df)
  
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
    group_by(ARMS, AETERMC )%>%
    summarise(n=n(),.groups = "keep") %>%
    mutate( pct = if_else(ARMS == 'A', n*100/bign[1], n*100/bign[2] ),
            ORD1=1,
            ORD2=9999,
            ORD3=n, 
            txt=paste("", AETERMC, sep = "") )
  
  # Step: 5 - Data Arrangement
  all_ae <- rbind(any_ae1, any_decod1)
  all_ae1 <- all_ae[order(all_ae$ARMS, all_ae$ORD1, -all_ae$ORD3),]
  
  #Create Freq (PCT) variable
  
  all_ae2 <- all_ae1 %>% 
    mutate(var1 = n,
           ARMS = ifelse(ARMS=="A", "DrugA","DrugB"))
  
  #--------------------------------
  # Transpose Data 
  #--------------------------------
  
  t_ae <- all_ae2 %>%
    ungroup()%>%
    dplyr::select(-n, -pct, -AETERMC, -ORD3) %>%  #-TRTAN # ORD3 has the order
    spread(ARMS, var1) %>%
    mutate(DrugA= ifelse(is.na(DrugA),0, DrugA), DrugB = ifelse(is.na(DrugB),0, DrugB) )%>%
    arrange(ORD1, ORD2)
  # Drop unnecessary variables
  t_ae1 <- subset(t_ae, select=c(-ORD1, -ORD2, -SOC))
  
  t_ae2<- as.data.frame(t_ae1[-1,])
  
  t_ae2$RR <- (t_ae2[,3]/bign[2])/(t_ae2[,2]/bign[1]) # Drug B vs Drug A
  
  for(i in 1:nrow(t_ae2)) {
  m <- matrix(c(t_ae2[i,2], bign[1]- t_ae2[i, 2], t_ae2[i,3], bign[2]-t_ae2[i, 3]), nrow = 2)
  
  t_ae2$p[i] <- fisher.test(m)$p.value
  
  }
  
  t_ae2$adj.p <- p.adjust(t_ae2$p, method = "fdr")
  t_ae2$log2rr <- log2(t_ae2$RR)
  
 
  t_ae2$total <- t_ae2$DrugA + t_ae2$DrugB
  
  t_ae2$AE <- ifelse(t_ae2$p <= pval_cutoff, t_ae2$txt, "")  #(t_ae2$RR >= 2| t_ae2$RR <= -2)
  #t_ae2$AE <- ifelse(t_ae2$total >=  20, t_ae2$txt, "") 
  t_ae2$log10p <- -log10(t_ae2$p)
 
  t_ae3 <- t_ae2[t_ae2$log2rr != Inf & t_ae2$log2rr != -Inf,]
  
  plot_ly(t_ae3, x = ~ log2rr, y = ~ -log10(p), marker = list(size = ~total, opacity = 0.9 ), 
          type = 'scatter', mode = 'markers', color = ~log2rr, colors = c("green", "blue"),
          hoverinfo = "text",
          text = paste(t_ae3$txt), height = 500) %>% 
    layout(title = '',
           xaxis = list(showgrid = FALSE, title = 'Log2 (Relative Risk)'), 
           yaxis = list(showgrid = FALSE, title = '-Log10 (p-value)'), 
           legend = list(titele = list(txt = "<b></b>"))) %>%
    add_annotations(x = t_ae3$log2rr, y = t_ae3$log10p, text = t_ae3$AE, 
                  xref = "x", yref = "y",
                  showarrow = FALSE, font = list(size = 10)) %>%
    add_annotations(xref = "paper", yref = "paper",
                    x = 1, y = 0.1, 
                    text = "Increased Risk in Arm B", showarrow = FALSE) %>%
    add_annotations(xref = "paper", yref = "paper",
                    x = 0, y = 0.1, 
                    text = "Increased Risk in Arm A", showarrow = FALSE) %>%
    layout(margin = list(l = 50, r = 50, b = 100, t = 50),
           annotations = list(x = 1, y = -0.2, text = "The size of the bubble is proportional to the total number of events for both treatment arms combined",
                              xref='paper', yref='paper', showarrow = F, 
                              xanchor='auto', yanchor='auto', xshift=0, yshift=0,
                              font = list(size = 10, color = "grey")))
  
  
    
    
    
   

}
