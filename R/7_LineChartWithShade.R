####################################################################################
# Set dataset variable
setwd("<path to root folder>")
dataset = read.csv("./Data/dataset_shade.csv")
####################################################################################

library(tidyverse)
library(lubridate)
library(ggthemes)
library(scales)

currentColumns <- sort(colnames(dataset))
requiredColumns <- c("GetGDPStat", "GetGDPStatName", "GetPoliticalLevel", "GetPoliticalLevelName", "Year")
columnTest <- isTRUE(all.equal(currentColumns, requiredColumns))

politicalLevelName <- unique(dataset$GetPoliticalLevelName)
gdpStatName <- unique(dataset$GetGDPStatName)

if(length(politicalLevelName) == 1 & length(gdpStatName) == 1 & columnTest) {
  dfPolicticalInfo <- dataset
  dfPolicticalInfo$GetPoliticalLevel <- as.character(dfPolicticalInfo$GetPoliticalLevel)
  runs <- rle(dfPolicticalInfo$GetPoliticalLevel)
  
  politicalLevelName <- unique(dfPolicticalInfo$GetPoliticalLevelName)
  gdpStatName <- unique(dfPolicticalInfo$GetGDPStatName)
  yAxisName <- paste(gdpStatName, ifelse(gdpStatName == "Actual GDP","(in Billions)",""), sep = " ")
  chartTitle <- paste(gdpStatName, "Analysis", sep = " ") 
  chartSubtitle <- paste(politicalLevelName, "view", sep = " ")
  
  partyColors = c("Republican"=I("red"), "Democrat"=I("blue"), "Tie" = "green")
  
  dfShadeInfo <- 
    dfPolicticalInfo %>%
    transmute(
      group_id = rep(seq_along(runs$lengths), runs$lengths), 
      Year = Year,
      Party = GetPoliticalLevel
    ) %>%
    group_by(group_id, Party) %>%
    summarize(start = min(Year), end = max(Year)+0.99) %>%
    ungroup()
  
  dfLineChartInfo <-
    dfPolicticalInfo %>%
    transmute(Year = Year, GetGDPStat)
  
  p <- ggplot(data = dfLineChartInfo) +
    geom_rect(
      data = dfShadeInfo,
      aes(xmin = start, xmax = end, fill = Party),
      ymin = -Inf, ymax = Inf, alpha = 0.4, color = NA
    ) +
    geom_line(aes(x = Year, y = GetGDPStat)) +
    scale_fill_manual(values=partyColors) +
    scale_y_continuous(labels=ifelse(gdpStatName == "Actual GDP",comma_format(),percent_format())) +
    ylab(yAxisName) +
    xlab("Year") +
    ggtitle(chartTitle, subtitle = chartSubtitle) +
    theme_few()
  
  p 
} else {
  
  plot.new()
  title("The data supplied did not meet the requirements of the chart.")
  
}
