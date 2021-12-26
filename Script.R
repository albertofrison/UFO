#PROJECT TO PLOT UFO SIGTHINGS IN MAPS
install.packages("rworldmap")

#LIBRARIES LOAD
library(ggplot2)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)

#DATA LOAD
world_data <- ne_countries(scale = "medium", returnclass = "sf")
#move the maven analytics data into the source file directory and make sure to change working directory
ufo_data <- data.frame(read.csv("ufo_sightings_scrubbed.csv"))

#DATA WRANGLING AND CLEANIND
#table(ufo_data$duration..seconds.)
#summary (ufo_data$duration..seconds.)

#clean duration column
ufo_data$duration..seconds. <- as.integer(as.character(ufo_data$duration..seconds.))

#clean spatial data
ufo_data$longitude <- as.double(as.character(ufo_data$longitude))
ufo_data$latitude <- as.double(as.character(ufo_data$latitude))

#summary (ufo_data$city)
#summary(ufo_data$datetime)

#clean dates
ufo_data$datetime <- format(as.Date(ufo_data$datetime),"%Y-%m-%d")
ufo_data$year <- format(as.Date(ufo_data$datetime), "%Y")
ufo_data$month <- format(as.Date(ufo_data$datetime), "%m")
ufo_data$day <- format(as.Date(ufo_data$datetime), "%d")

#clean some countries / states
US_states <- c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")

ufo_data$country <- ifelse (ufo_data$state %in% tolower(US_states), "us", ufo_data$country)
ufo_data$country <- ifelse (ufo_data$country == "", "other", ufo_data$country)

ufo_data_lat_long <- ufo_data %>%
  filter (!is.na(latitude) & !is.na(longitude))

#PLOTS
#plot(worldmap, xlim = c(-80, 160), ylim = c(-50, 100), asp = 1, bg = "lightblue", col = "black")
#plot(worldmap, asp = 1, bg = "lightblue", col = "black")
# add points
# points(ufo_data_lat_long$longitude, ufo_data_lat_long$latitude, col = "red", cex = .01)

# COUNTRY
ufo_data %>%
  filter(!is.na(country) &!is.na(state) & state !=""  )%>%
  ggplot(aes(x=fct_infreq(country), fill = as.factor(state))) +
  geom_bar()


# SHAPE AND DURATION
summary(ufo_data$duration..seconds.)

ufo_data %>%
  filter(!is.na(duration..seconds.) & shape != "" & duration..seconds. < 10^4)%>%
  ggplot(aes(x = as.factor(month), y=duration..seconds.)) +
  geom_boxplot()

ufo_data %>%
  filter(!is.na(duration..seconds.) & shape != ""  & duration..seconds. < 10^4)%>%
  ggplot(aes(x=duration..seconds.)) +
  geom_histogram(binwidth = 60*15)

ufo_data %>%
  filter (shape != "") %>%
  ggplot(aes(x=fct_infreq(shape))) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# DATE / TIME ANALYSIS
ggplot(data = ufo_data) +
  geom_bar (aes(x = month, fill = year))

ggplot(data = ufo_data) +
  geom_bar (aes(x = year, fill = month))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(x = "Year", y= "count of sightings", title = "UFO sightings by year", subtitle = "Data from Maven Analytics - https://www.mavenanalytics.io/data-playground", caption ="https://github.com/albertofrison/UFO")


# gene world map
ufo_data_lat_long %>%
  filter (latitude<48.00 & latitude> 36.00 & longitude >0.4 & longitude < 24.35)  %>%
  ggplot() +
  geom_sf(data = world_data) +
  geom_point (aes(x=longitude, y=latitude, color = as.factor(year)), size = 2) +
  coord_sf (xlim = c(0.40, 24.35), ylim = c(36.00, 48.00), expand = T) + #ITALIA
  labs(x ="", y="", title = "UFO sightings in Italy and surrouding areas", subtitle = "Data from Maven Analytics - https://www.mavenanalytics.io/data-playground", caption ="https://github.com/albertofrison/UFO")+
  theme(legend.position = "bottom", legend.title = element_text (size =2), legend.text = element_text (size =7)) +
  guides(color = guide_legend(nrow = 4, byrow = TRUE, title = ""))
  
ggsave("UFO - ITALY.png", device ="png")
