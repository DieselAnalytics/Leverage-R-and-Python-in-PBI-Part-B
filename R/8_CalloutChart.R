####################################################################################
# Set dataset variable
setwd("C:/Users/ryanwade44/Documents/UKPBI/Script/")
dataset = read.csv("./Data/dataset_calloutchart.csv")
####################################################################################

library(tidyverse)
library(scales)
library(ggthemes)

currentColumns <- sort(colnames(dataset))
requiredColumns <- c("Country", "Total.Expend.Per", "Year")
columnTest <- isTRUE(all.equal(currentColumns, requiredColumns))
reportYear <- unique(dataset$Year)

if (length(reportYear) == 1 & columnTest) {
  
  plotdata <- 
    dataset %>%
    mutate(
      rank = dense_rank(desc(`Total.Expend.Per`)),
      `Country` = fct_reorder(`Country`, rank, .desc = TRUE), 
      callout = ifelse(rank == 1, TRUE, FALSE)
    ) %>%
    filter(rank <= 7)
  
  countryToAnnotate <- plotdata$`Country`[plotdata$rank == 1]
  minExpenditure <- min(plotdata$Total.Expend.Per)
  maxExpenditure <- max(plotdata$`Total.Expend.Per`)
  minCountry <- plotdata$`Country`[which(plotdata$`Total.Expend.Per` == minExpenditure)]
  minCountry <- paste(minCountry, collapse = " & ") 
  maxCountry <- plotdata$`Country`[which(plotdata$`Total.Expend.Per` == maxExpenditure)]
  maxCountry <- paste(maxCountry, collapse = " & ")
  mainTitle = "% of Government Expenditure on Education"
  subTitle = paste("The top 7 ranked countries in", reportYear, sep = " ")
  caption = "Source:  https://databank.worldbank.org/source/world-development-indicators"
  
  label_val <- 
    str_wrap(
      paste(maxCountry, "spent", percent((maxExpenditure/minExpenditure-1)), "more of their expenditures on education than", minCountry, sep = " "),
      width = 35
    )
  
  p <- 
    ggplot(
      data = plotdata, 
      aes(x = `Country`, y = `Total.Expend.Per`, fill = callout, label = percent(`Total.Expend.Per`))
    ) +
    geom_bar(stat="identity", aes(fill = callout)) +
    geom_text(nudge_y = -0.05) +
    scale_y_continuous(limits = c(0,0.75), labels = NULL, breaks = NULL) +
    coord_flip() +
    annotate("text", label = label_val, x = countryToAnnotate[1], y = maxExpenditure + 0.15, size =6) +
    labs(title = mainTitle, subtitle = subTitle, caption = caption) +
    xlab(label = NULL) +
    ylab(label = NULL) +
    #guides(fill=FALSE) +
    theme_few() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 25), 
      plot.subtitle = element_text(hjust = 0.5, size = 20), 
      plot.caption = element_text(size = 15), 
      axis.text.y = element_text(size = 15))
  p
  
} else {
  
  plot.new()
  title("The data supplied did not meet the requirements of the chart.")
}