### get data from 
library(httr)
library(jsonlite)

### plotting 
library(ggplot2)
library(cowplot)

res <- GET("https://opendata.arcgis.com/datasets/52fb11a8a07f49c1b28335a9de9ba99f_0.geojson")
data <- fromJSON(rawToChar(res$content))
data.properties <- data$features$properties

Mesa.data <- dplyr::filter(data.properties, COUNTY == "MESA")
# unique(Mesa.data$Metric)
# unique(Mesa.data$Desc_)

Mesa.data <- Mesa.data %>%
  select(Desc_, Value, Date)
Mesa.data$Date <- as.Date(Mesa.data$Date, "%m/%d/%y")

Mesa.cases <- Mesa.data %>%
  dplyr::filter(Desc_ == "Cases of COVID-19 in Colorado by County")

Mesa.deaths <- Mesa.data %>%
  dplyr::filter(Desc_ == "Deaths Among COVID-19 Cases in Colorado by County")

nodeaths <- anti_join(Mesa.cases, Mesa.deaths, by = "Date") %>%
  select("Date") %>%
  mutate(Value = 0, Desc_ = "Deaths Among COVID-19 Cases in Colorado by County") 
Mesa.deaths <- rbind(nodeaths, Mesa.deaths)

cases <- ggplot(Mesa.cases, aes(x = Date, y = Value)) +
  geom_point() +
  geom_path() + 
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d", name = "") +
  scale_y_continuous(name = "", breaks=seq(from = 0, to = max(Mesa.cases$Value)+500, by = 500)) +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  ggtitle("Total COVID-19 cases in Mesa County in Mesa County")
  

deaths <- ggplot(Mesa.deaths, aes(x = Date, y = Value)) +
  geom_point() +
  geom_path() + 
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d", name = "Date in 2-week increments") +
  scale_y_continuous(name = "", 
                     breaks=seq(from = 0,to = max(Mesa.deaths$Value)+10, by = 5)) +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  ggtitle("Total Deaths Among COVID-19 Cases in Mesa County")

plot_grid(cases, deaths, ncol = 1) 
