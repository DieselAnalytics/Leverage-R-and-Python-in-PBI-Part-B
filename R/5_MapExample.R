####################################################################################
# Set dataset variable
setwd("C:/Users/ryanwade44/Documents/UKPBI/Script")
dataset = read.csv("./Data/dataset_mapexample.csv")
####################################################################################

library(tidyverse)
library(ggthemes)

currentColumns <- sort(colnames(dataset))
requiredColumns <- c("County", "Index", "lat", "long", "State", "Total.Population")
columnTest <- isTRUE(all.equal(currentColumns, requiredColumns))
state <- str_to_title(unique(dataset$State))

if (length(state) == 1 & columnTest) {
  
  chartTitle <- paste0(state, "'s County Population Analysis")
  subTitle <- "(the darker shades the higher the population)"
  
  chartdata <-
    dataset %>%
    mutate(quintile = factor(ntile(Total.Population, 5)))
  
  # http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
  quintileColors <-
    c(
      "1" = "dodgerblue",
      "2" = "dodgerblue1",
      "3" = "dodgerblue2",
      "4" = "dodgerblue3",
      "5" = "dodgerblue4"
    )
  
  ggplot(chartdata, aes(long, lat, group = County, fill = quintile)) +
    geom_polygon(show.legend = FALSE, color = "black") +
    scale_x_continuous(name = NULL, labels = NULL, breaks = NULL) +
    scale_y_continuous(name = NULL, labels = NULL, breaks = NULL) +  
    scale_fill_manual(values = quintileColors) +
    coord_quickmap() + 
    ggtitle(chartTitle, subtitle = subTitle) +
    theme_map() +
    theme(
      plot.title = element_text(size =30),
      plot.subtitle = element_text(size = 20)
    )
  
} else {
  
  plot.new()
  title("The data supplied did not meet the requirements of the chart.")
  
}