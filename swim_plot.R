swim_plot <- function(swimdata, by_sub = TRUE, xlab, ylab, trt_avail, legend.lab,
    displayTStop, displayPResp, displaySDis, displayPDis, displayCResp, displayDeath, displayDR,
    displayContResp) {

  swimdata$EndTime <- -0.5
  swimdata$firstPR <- as.factor(swimdata$firstPR)
  swimdata$firstCR <- as.factor(swimdata$firstCR)
  swimdata$firstPD <- as.factor(swimdata$firstPD)
  swimdata$firstSD <- as.factor(swimdata$firstSD)
  swimdata$death <- as.factor(swimdata$death)
  swimdata$DR <- as.factor(swimdata$DR)
  swimdata$trtcap <- as.factor(swimdata$trtcap)
  
  # check if CR PR PD and SD is not all na in dataset - plot legends etc accordingly
  # remove PR CR PD SD based on user input
  
  if(trt_avail == TRUE) {
    arm_plot <- swimmer_plot(df=swimdata,id='id',end='fuend',name_fill='group', width=.85)
     
    g <- names(table(swimdata$group))
    n_g <- length(g)
    #col_group <<- brewer.pal(n = n_g, name = "Set2")[1:n_g]
    
    if (displayContResp == TRUE) {
    Response_plot <- arm_plot +   
       swimmer_arrows(df_arrows = swimdata, id = 'id', arrow_start = 'fuend', name_col= "group" , 
                     cont = 'Continued_Resp', show.legend = FALSE, type = "open", cex=1.5) +
       scale_color_discrete(drop=FALSE) +
      annotate("text", x = 3.5, y= max(swimdata$fuend)+ 8, label="Continued response",size=4.25)+
      annotate("text", x = 2.75,  y= max(swimdata$fuend)+ 6, label=sprintf('\u2192'), size=8.25)
    } else {
      Response_plot <- arm_plot 
    }
    
      
  } else {
    arm_plot <- swimmer_plot(df=swimdata,id='id',end='fuend', width=.85, fill='#ace6e4')
    
    swimdata$newcol <- "one"
    
    if (displayContResp == TRUE) {
      Response_plot <- arm_plot +   
        swimmer_arrows(df_arrows = swimdata, id = 'id', arrow_start = 'fuend',  name_col = "newcol",
                       cont = 'Continued_Resp', show.legend = FALSE, type = "open", cex=1.5) +
        annotate("text", x = 3.5, y= max(swimdata$fuend)+ 8, label="Continued response",size=4.25)+
        annotate("text", x = 2.75,  y= max(swimdata$fuend)+ 6, label=sprintf('\u2192'), size=8.25)
        
    } else {
      Response_plot <- arm_plot
    }
    
  }
  
  if(displayDR == TRUE & all(is.na(swimdata$DR)) == FALSE) {
    Response_plot <- Response_plot + 
      swimmer_points(df_points=swimdata,id='id',time='EndTime',name_shape =
                       'DR',size=3,fill='darkblue', col= 'darkblue')
  }
  
  if(displayDeath == TRUE & all(is.na(swimdata$death)) == FALSE) {
    Response_plot <- Response_plot + 
      swimmer_points(df_points= swimdata,
                     id='id',time='death_time',name_shape =
                       'death',size=3,fill='#141414',col='#141414')
  }
  
  if(displayPResp == TRUE & all(is.na(swimdata$firstPR)) == FALSE) {
    Response_plot <- Response_plot + 
      swimmer_points(df_points= swimdata,
                     id='id',time='firstPR_time',name_shape =
                       'firstPR',size=3,fill="#b134eb",col="#b134eb")
  }
  
  if(displayCResp == TRUE & all(is.na(swimdata$firstCR)) == FALSE) {
    Response_plot <- Response_plot +
      swimmer_points(df_points= swimdata,
                     id='id',time='firstCR_time',name_shape =
                       'firstCR',size=3,fill="#eb34de",col="#eb34de") }
  
  if(displaySDis == TRUE & all(is.na(swimdata$firstSD)) == FALSE) {
    Response_plot <- Response_plot + 
      swimmer_points(df_points= swimdata,
                     id='id',time='firstSD_time',name_shape =
                       'firstSD',size=3,fill='#3456eb',col='#3456eb')
  }
  
  if(displayPDis == TRUE & all(is.na(swimdata$firstPD)) == FALSE) {
    Response_plot <- Response_plot + 
      swimmer_points(df_points= swimdata,
                     id='id',time='firstPD_time',name_shape =
                       'firstPD',size=3,fill='#14b305',col='#14b305') 
  }
  
  if(displayTStop == TRUE & all(is.na(swimdata$trtcap)) == FALSE) {
    Response_plot <- Response_plot + 
      swimmer_points(df_points= swimdata,
                     id='id',time='trtend',name_shape =
                       'trtcap',size=3,fill="#eb3434",col='#eb3434') 
  }
  
  
  
  Response_plot <- Response_plot + 
    ggplot2::ylab(xlab) +
    ggplot2::xlab(ylab) +
    coord_flip(clip = 'off', ylim = c(0, max(swimdata$fuend))) +
    scale_y_continuous(breaks = seq(0,max(swimdata$fuend),by=2)) +
    theme( axis.title.x = element_text(size=14, face="bold", colour = "black"),    
           axis.title.y = element_text(size=14, face="bold", colour = "black"),
           axis.text.x = element_text(size=12),
          axis.text.y = element_text(size = 12),
          legend.text=element_text(size=12))
  
  # all point are NAs but display point is T, convert it to F
  if(displayCResp == TRUE & all(is.na(swimdata$firstCR)) == TRUE ) {displayCResp = FALSE}
  if(displayPResp == TRUE & all(is.na(swimdata$firstPR)) == TRUE ) {displayPResp = FALSE}
  if(displayTStop == TRUE & all(is.na(swimdata$trtcap)) == TRUE ) {displayTStop = FALSE}
  if(displayPDis == TRUE & all(is.na(swimdata$firstPD)) == TRUE ) {displayPDis = FALSE}
  if(displaySDis == TRUE & all(is.na(swimdata$firstSD)) == TRUE ) {displaySDis = FALSE}
  if(displayDeath == TRUE & all(is.na(swimdata$death)) == TRUE ) {displayDeath = FALSE}
  if(displayDR == TRUE & all(is.na(swimdata$DR)) == TRUE ) {displayDR = FALSE}
  
  legend_point_colors <- generate_legend_color(displayTStop,  displayPResp, 
                                         displaySDis , displayPDis, 
                                         displayCResp , displayDeath,
                                         displayDR)
  
  legend_point_shape <- generate_shape(displayTStop,  displayPResp, 
                                      displaySDis , displayPDis, 
                                      displayCResp , displayDeath, displayDR)
    
    
  if(trt_avail == TRUE) {
    
  group_colors <- assign_group_colors(num_groups = n_g, group_names= g)
  legend_colors <- c(legend_point_colors, group_colors)
    
  Response_plot <-  Response_plot +
    ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(shape = 21)))+
    scale_shape_manual(name = "", values=legend_point_shape, breaks= names(legend_point_shape),
                       label = names(legend_point_shape)) +
    scale_fill_manual(name = legend.lab, values  = legend_colors) +
    scale_color_manual( values = legend_colors) +
    
    guides(colour = "none", # removes extra group legend
           shape = guide_legend(override.aes = list(size = 3, 
                                                    shape = legend_point_shape,
                                                    colour = legend_point_colors,
                                                    fill= legend_point_colors))) +
    labs(caption = "A Durable Responder (DR) is a subject who has confirmed response for at least 6 months") +
    theme(plot.caption = element_text(hjust = 0),
          legend.title=element_text(size=14, face="bold"))

  } else {
    Response_plot <-  Response_plot +
      scale_shape_manual(name="",values= legend_point_shape,
                         breaks= names(legend_point_shape)) +
      scale_fill_manual(name = , values = legend_point_colors) +
        ggplot2::scale_color_manual(values=legend_point_colors) +
      
      guides(colour = "none", # removes extra group legend
             shape = guide_legend(override.aes = list(size = 3, 
                                                      shape = legend_point_shape,
                                                      colour = legend_point_colors,
                                                      fill=legend_point_colors))) +
      labs(caption = "A Durable Responder (DR) is a subject who has confirmed response for at least 6 months") +
      theme(plot.caption = element_text(hjust = 0))
    
   
  }
  
 
  if(by_sub == FALSE){
    Response_plot <- Response_plot+
      theme(axis.text.y=element_blank())
  } 
 
  
  Response_plot
  
}