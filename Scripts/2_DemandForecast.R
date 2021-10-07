library(tidyverse)
library(ggthemes)
library(lubridate)
library(scales)
library(cowplot)


# The three lines below compares the columns in the data set from Power BI to the required columns
# needed by the visual. If they are equal than the columnTest variable will be set to TRUE otherwise
# it will be set to false.
currentColumns <- sort(colnames(dataset))
requiredColumns <- 
  c("DateKey", "Forecast End Date", "Predicted_Sales", "Predicted_Sales_Lower", 
    "Predicted_Sales_Upper", "ProductName", "Sales", "StoreName", "Trend", 
    "Trend_Lower", "Trend_Upper", "Weekly_Seasonality", "Yearly_Seasonality")
columnTest <- isTRUE(all.equal(currentColumns, requiredColumns))

# Get the store and product the data set is based on. It is used in the validation
# Test. Only one store and one product should be in the data set
Store <- unique(dataset$StoreName)
Product <- unique(dataset$ProductName)

# Get the forecast end date that was passed to R from Power BI
ForecastEndDate <-unique(dataset$`Forecast End Date`)

# Performs the data validation test. If it is paassed then code is excecuted that generates the 
# forecast plots. Otherwise, a blank chart is displayed with information about why R was not able 
# generate a chart.
if(length(Store)==1 & length(Product)==1 & columnTest == TRUE){  
    
    #chart_title <- unique(dataset$`Forecast Chart Title (All Charts)`)
    # Gets the max date for actual sales. The date is in an integer format 
    # in the form of yyyyMMdd so the ymd() function from lubridate is used
    # to parse the date to a valid date format.
    max_data_date <-ymd(max(dataset$Date[!is.na(dataset$Sales)]))
    
    #Subsets the data frame to get the required columns needed by the visuals and creates the 
    # Date column by parsing the DateKey column which is in yyyyMMdd as an int into a valid date.
    initial_data <-
      dataset %>%
      select(DateKey, Sales, Predicted_Sales, Predicted_Sales_Lower, Predicted_Sales_Upper, 
             Trend, Trend_Lower, Trend_Upper, Weekly_Seasonality, Yearly_Seasonality) %>%
      mutate(Date = ymd(DateKey))
    
    # Creates the data set needed for the p_forecast chart
    sales_forecast_date <- 
      initial_data %>%
      filter(DateKey >= 20170101 & DateKey <= ForecastEndDate)

    #Creates the data set needed for both the p_year_seasonality and p_year_trend charts
    yearchart_data <-
      initial_data %>%
      select(Date, Sales, Predicted_Sales, Predicted_Sales_Lower, Predicted_Sales_Upper, 
             Trend, Trend_Lower, Trend_Upper, Yearly_Seasonality)
    
    # Gets the information needed for the week seansonality chart. The first_monday variable 
    # represents the first monday in the data set and it is used with the first_sunday variable 
    # to subset the data to get a full week worth of data. The weekly seasonality data repeats
    # for every week so the code below enables us to get just a week which is needed to 
    # create the p_week_seasonality visual.
    first_monday <- 
      min(initial_data$Date) + 
      days(8-wday(min(initial_data$Date), week_start = getOption("lubridate.week.start", 1)))
    
    first_sunday <- first_monday + days(6)
    
    weekchart_data <-
      initial_data %>%
      filter(Date >= first_monday & Date <= first_sunday) %>%
      mutate(Date, weekday = as.character(weekdays(Date, abbreviate = FALSE)), Weekly_Seasonality, group = "a") %>%
      select(Date, weekday, Weekly_Seasonality, group)
    
    #Creates the forecast chart
    p_forecast <- ggplot(data = sales_forecast_date, mapping = aes(x = Date, y=Predicted_Sales)) +
      geom_ribbon(mapping = aes(ymin = Predicted_Sales_Lower, ymax = Predicted_Sales_Upper), fill = "grey70") +
      geom_line(color = '#118DFF', size = 1) +  
      geom_point(mapping = aes(y=Sales), na.rm=TRUE) +
      geom_vline(xintercept = max_data_date, linetype = "dashed", color = "black", size = 1) +
      theme_economist_white() +
      labs(title = "Sales Forecast", x=NULL, y=NULL)
    
    #Creates the week seasonality chart
    p_week_seasonality <- ggplot(data = weekchart_data, aes(x = fct_reorder(weekday, Date, .desc = FALSE), y = Weekly_Seasonality, group = group)) +
      geom_line(size = 2 ,color = "#118DFF") +
      labs(title = "Weekly Seasonality", x=NULL, y=NULL) +
      theme_economist_white()
    
    #Creates the year seasonality chart. Note that the filter for the data set used in the visual is hardcoded. This need to be
    #made dynamic if it is productionize.
    p_year_seasonality <- ggplot(data = filter(yearchart_data, year(Date) == 2016), aes(x = Date, y = Yearly_Seasonality)) +
      geom_line(color = "#118DFF", size=2) +
      scale_x_date(labels = date_format("%b")) +
      labs(title = "Yearly Seasonality", x=NULL, y=NULL) +
      theme_economist_white()
    
    #Creates the trend chart
    p_year_trend <- ggplot(data = yearchart_data, aes(x = Date, y = Trend, ymin = Trend_Lower, ymax = Trend_Upper)) +
      geom_ribbon(fill = "light grey") +
      geom_line(color = "#118DFF", size=2) +  
      labs(title = "Trend Analysis", x=NULL, y=NULL) +
      theme_economist_white()
    
    #Combine the 4 charts created above into one chart.
    plot_grid(
      p_forecast, p_week_seasonality, p_year_seasonality, p_year_trend, 
      align = "v", 
      nrow = 4, 
      rel_heights = c(2/5, 1/5, 1/5, 1/5) #The p_forecast visual is double the size of others. Note all parts sums to 1.
    )
} else {
  
    # The code below produces a chart that contains a message relaying why the desired chart could not be produced
    message_str <- 
      paste("The data supplied did not meet the requirements of the chart.",
            "You need to drill through from a cell in the matrix visual located",
            "in the 'Inventory Report' so that both a product name and store name",
            "will be passed to this chart.", sep = " ")
    plot.new()
    title(str_wrap(message_str, width = 100))
  
}
