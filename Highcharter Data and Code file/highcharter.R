library(highcharter) 
library(dplyr)
library(viridis)

options(highcharter.theme = hc_theme_538(tooltip = list(valueDecimals = 2)))
getwd()
# copy the directory inside setwd()
setwd("")

data <- read.csv("data2021.csv")

# Pie Chart

pie_data <- aggregate(Brand ~ Rating, data= data, FUN = length)
colors <- c("")

pie_data %>%
  hchart(
    "pie", hcaes(x = Rating, y = Brand, color = colors), 
    dataLabels = list(enabled = TRUE,
                      format = '{point.name} :  {point.y:.1f} ')) %>%
  hc_title(text = "Number of Brands by Ratings")


# Bar Chart 
data2 <- aggregate(Value ~ Rating, data= data, FUN = mean)
data2$brand_num <- pie_data$Brand

data2 %>% 
  hchart(
    'bar', hcaes(y = Value, x = Rating ,color = Rating),
    showInLegend = F,
    maxSize = "15%",
    dataLabels = list(enabled = TRUE,
                      format = '{point.y: .3f}')) %>%
  hc_title(
    text = "Bar-Chart Showing Average Brand Value by Ratings (2021)",
    style = list(fontWeight = "bold", fontSize = "15px"),
    align = "center"
  ) %>% 
  hc_subtitle(
    text = "Source: Brand Directory", 
    style = list(fontWeight = "bold", fontSize = "13px"),
    align = "center"
  ) %>%
  hc_yAxis(title = list(text = "Average Brand Values")) %>%
  hc_xAxis(title = list(text = "Ratings"))

# Bubble chart
data %>% 
  hchart(
    'bubble', hcaes(y = Rank, x = RankLastYear),
    color = "#319e81",
    showInLegend = F,
    maxSize = "2%") %>%
  hc_title(
    text = "Scatter-Plot (Ranking 2020 vs Ranking 2021)",
    style = list(fontWeight = "bold", fontSize = "15px"),
    align = "center"
  ) %>% 
  hc_subtitle(
    text = "Source: Brand Directory", 
    style = list(fontWeight = "bold", fontSize = "13px"),
    align = "center"
  ) %>%
  hc_xAxis(title = list(text = "Ranking 2020")) %>%
  hc_yAxis(title = list(text = "Ranking 2021")) %>%
  hc_legend(align = "right", verticalAlign = "middle", 
            layout = "vertical")




# Scatter chart
data %>% 
  hchart(
    'point', hcaes(y = Rank, x = RankLastYear, group = Rating),
    showInLegend = T) %>%
  hc_title(
    text = "Scatter-Plot (Ranking 2020 vs Ranking 2021)",
    style = list(fontWeight = "bold", fontSize = "15px"),
    align = "center"
  ) %>% 
  hc_subtitle(
    text = "Source: Brand Directory", 
    style = list(fontWeight = "bold", fontSize = "13px"),
    align = "center"
  ) %>%
  hc_xAxis(title = list(text = "Ranking 2020")) %>%
  hc_yAxis(title = list(text = "Ranking 2021")) %>%
  hc_legend(align = "right", verticalAlign = "middle", 
            layout = "vertical")



# Line chart
data(economics_long, package = "ggplot2")

economics_long2 <- filter(economics_long, variable %in% c("pop", "uempmed", "unemploy"))

hchart(economics_long2, "line", hcaes(x = date, y = value01, 
                                      group = variable))
