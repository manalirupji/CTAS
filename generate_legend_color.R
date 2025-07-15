generate_legend_color <- function(displayTStop = FALSE, displayPResp = FALSE, 
                                  displaySDis = FALSE, displayPDis = FALSE, 
                                  displayCResp = FALSE, displayDeath = FALSE,
                                  displayDR = FALSE) {
  legend_color <- character(0)
  
  if (displayTStop) {
    legend_color <- c(legend_color, "Treatment Stop" = "#eb3434")
  }
  if (displayPResp) {
    legend_color <- c(legend_color,  "First Partial Response" = "#b134eb")
  }
  if (displaySDis) {
    legend_color <- c(legend_color, "First Stable Disease" = "#3456eb")
  }
  if (displayPDis) {
    legend_color <- c(legend_color, "First Progressive Disease"= "#14b305")
  }
  if (displayCResp) {
    legend_color <- c(legend_color, "First Complete Response"= "#eb34de")
  }
  if (displayDeath) {
    legend_color<- c(legend_color, "Death"= "black")
  }
  if (displayDR) {
    legend_color <- c(legend_color, "DR" = "darkblue")
  }

  
  return(legend_color)
}

# Example usage:

# Conditions 1 and 2 are TRUE
#legend_color2 <- generate_legend_color(displayTStop = TRUE, displayPResp = TRUE)
#print(legend_color2)
