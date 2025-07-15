assign_group_colors <- function(num_groups, group_names) {
  # Define a vector of colors to assign to groups
  group_colors <- c("#ace6e4", "#f1f7a8", "pink", "grey", "#aaff00", "#9933ff")
  
  # Ensure the number of groups is not greater than the number of available colors
  if (num_groups > length(group_colors)) {
    stop("Number of groups exceeds the available colors.")
  }
  
  # Create a named vector of group colors
  group_color_vector <- setNames(group_colors[1:num_groups], group_names[1:num_groups])
  
  return(group_color_vector)
}

# Example usage:
#num_groups <- 2
#group_names <- c("Positive", "Negative")
#group_colors <- assign_group_colors(num_groups, group_names)
#print(group_colors)
