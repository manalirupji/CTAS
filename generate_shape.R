generate_shape <- function(displayTStop = FALSE, displayPResp = FALSE, 
                                  displaySDis = FALSE, displayPDis = FALSE, 
                                  displayCResp = FALSE, displayDeath = FALSE,
                                  displayDR = FALSE) {
  legend_shape <- numeric(0)
  
  if (displayTStop) {
    legend_shape <- c(legend_shape, "Treatment Stop" = 23)
  }
  if (displayPResp) {
    legend_shape <- c(legend_shape, "First Partial Response"= 17)
  }
  if (displaySDis) {
    legend_shape <- c(legend_shape, "First Stable Disease" =17)
  }
  if (displayPDis) {
    legend_shape <- c(legend_shape, "First Progressive Disease" = 17)
  }
  if (displayCResp) {
    legend_shape <- c(legend_shape, "First Complete Response" = 17)
  }
  if (displayDeath) {
    legend_shape<- c(legend_shape, "Death"= 16)
  }
  if (displayDR) {
    legend_shape <- c(legend_shape, "DR"= 15)
  }
  
  
  return(legend_shape)
}

# Example usage:

# Conditions 1 and 2 are TRUE
#legend_color2 <- generate_shape(displayTStop = TRUE, displayPResp = TRUE)
#print(legend_color2)
