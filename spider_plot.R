spider_plot <- function(data, var_name, time_avail, date, xlab, ylab, grp_avail,
                        legend.lab, PRline, PRvalue, PDline, PDvalue) {
  
  if(time_avail == FALSE) {
    data <- data[, -which(colnames(data) %in% ("week"))]
    data$date <- strptime(data$date, format = "%m/%d/%Y")
    data2 <- by(data, data$id,
           function(subset) within(subset,
                                   { 
                                     o <- order(date)
                                     time <- date[o]
                                     baseline <- date[1]
                                   }))
  
    data3 <- do.call("rbind", data2)
    data3$week <- as.numeric(sub(" months", "", difftime(data3$time, data3$baseline, units = "days")/30.4166667)) # instead of week using month
  } else {
    
    data3 <- data
  }
  
  data3 <<- data3
  data3$grp <- factor(data3$grp)
  data3$grp <- relevel(data3$grp, ref = names(table(data3$grp))[2])
  
  data3 <- data3 %>% 
    group_by(id) %>%
    filter(n()>1) 

  num <- aggregate(data = data3,                # Applying aggregate
            id ~ grp,
            function(x) length(unique(x)))
  names(num)[2] <- "n"
  
  data4 <- merge(data3, num, by = "grp")
  data4$grp_N <- paste(data4$grp, " (n = ", data4$n, ", ", round(data4$n*100/sum(num$n), 2) ," % )", sep = "" )
  
  check_data4 <<- data4
  if (grp_avail == TRUE ) {
  p <- ggplot(data4, aes(x= week, y =br_change , group=id)) +
    theme_bw(base_size=14) +
    theme(axis.title.x = element_text(face="bold"), axis.text.x = element_text(face="bold")) +
    theme(axis.title.y = element_text(face="bold"), axis.text.y = element_text(face="bold")) +
    theme(plot.title = element_text(size=18, hjust=0.5)) +
    theme( plot.background = element_blank(),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank()) +
    labs(title = "")+
    xlab(xlab)+
    ylab(ylab) +
    labs(color = data4$grp_N) + #, tag = paste('Total number of evaluable patients post baseline = ', sum(num$n))) +
   # theme(plot.tag.position = c(0.95, 0.4))
    annotation_custom(grob = textGrob(paste('Total number of evaluable patients post baseline = ', sum(num$n))),  
                      xmin = 2, xmax = max(data4$week), ymin = min(data4$br_change) - 5, ymax = min(data4$br_change) - 5)
  
  ## Now plot
  options(repr.plot.width=240, repr.plot.height=160)
  p <- p + 
    scale_color_manual(values=c("blue", "red", "orange", "purple"))+
    geom_line(aes(color=grp_N)) + #colored lines
    geom_point(aes(shape=grp_N, color=grp_N), show.legend=FALSE) +
    coord_cartesian(xlim=c(0, max(data4$week, na.rm= T))) +
    coord_cartesian(ylim=c(min(data4$br_change, na.rm =T), max(data4$br_change, na.rm= T))) +
    scale_y_continuous(limits = c(min(data4$br_change, na.rm =T), max(data4$br_change, na.rm= T)), breaks = seq(min(data4$br_change, na.rm =T), max(data4$br_change, na.rm= T), 20))+
    scale_x_continuous(limits = c(min(data4$week, na.rm =T), max(data4$week, na.rm= T)), breaks = seq(min(data4$week, na.rm =T), max(data4$week, na.rm= T), 3))+
    geom_hline(yintercept =0, colour = "grey", linetype=2 )  #dashed
    

  
  if(PRline == TRUE) {
   p <- p + geom_hline(yintercept =PRvalue, colour = "grey", linetype=2 ) } #dashed
  
  if(PDline == TRUE) {
    p <- p + geom_hline(yintercept =PDvalue, colour = "grey", linetype=2) } #dashed
    
  p$labels$colour <- legend.lab
  
      p
  
  } else {
    p <- ggplot(data3, aes(x= week, y =br_change , group=id)) +
      theme_bw(base_size=14) +
      theme(axis.title.x = element_text(face="bold"), axis.text.x = element_text(face="bold")) +
      theme(axis.title.y = element_text(face="bold"), axis.text.y = element_text(face="bold")) +
      theme(plot.title = element_text(size=18, hjust=0.5)) +
      theme( plot.background = element_blank(),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank()) +
      labs(title = "")+
      xlab(xlab)+
      ylab(ylab) +
      labs(color = var_name) +
      annotation_custom(grob = textGrob(paste('Total number of evaluable patients post baseline = ', sum(num$n))),  
                        xmin = 2, xmax = max(data4$week), ymin = min(data4$br_change) - 1, ymax = min(data4$br_change) - 3)
    
    
    ## Now plot
    p <- p + 
      geom_line()+ # single color
      geom_point(show.legend=FALSE) +
    coord_cartesian(xlim=c(0, max(data3$week, na.rm= T)))+
      geom_hline(yintercept =0, colour = "grey", linetype=2 )  #dashed
    
    if(PRline == TRUE) {
      p <- p + geom_hline(yintercept =PRvalue, colour = "grey", linetype= 2) }
    
    if(PDline == TRUE) {
      p <- p + geom_hline(yintercept =PDvalue, colour = "grey", linetype=2) }
    p
  }
  
  
  
}
