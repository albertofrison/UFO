# PROJECT TO PLOT MAPS
install.packages("rworldmap")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")

# load library
library(ggplot2)
library(rworldmap)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyverse)


world_data <- ne_countries(scale = "medium", returnclass = "sf")
ufo_data <- data.frame(read.csv("ufo_sightings_scrubbed.csv"))

table(ufo_data$duration..seconds.)
summary (ufo_data$duration..seconds.)

ufo_data$duration..seconds. <- as.integer(as.character(ufo_data$duration..seconds.))

ufo_data$longitude <- as.double(as.character(ufo_data$longitude))
ufo_data$latitude <- as.double(as.character(ufo_data$latitude))

summary (ufo_data$city)
summary(ufo_data$datetime)

ufo_data$datetime <- format(as.Date(ufo_data$datetime),"%Y-%m-%d")

ufo_data$year <- format(as.Date(ufo_data$datetime), "%Y")
ufo_data$month <- format(as.Date(ufo_data$datetime), "%m")
ufo_data$day <- format(as.Date(ufo_data$datetime), "%d")

US_states <- c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")

ufo_data$country <- ifelse (ufo_data$state %in% tolower(US_states), "us", ufo_data$country)
ufo_data$country <- ifelse (ufo_data$country == "missing", "other", ufo_data$country)


############## PLOTS
plot(worldmap, xlim = c(-80, 160), ylim = c(-50, 100), asp = 1, bg = "lightblue", col = "black")
plot(worldmap, asp = 1, bg = "lightblue", col = "black")
# add points

ufo_data_lat_long <- ufo_data %>%
  filter (!is.na(latitude) & !is.na(longitude))

points(ufo_data_lat_long$longitude, ufo_data_lat_long$latitude, col = "red", cex = .01)

ufo_data %>%
  filter(!is.na(country) )%>%
  ggplot(aes(x=fct_infreq(country), fill = state)) +
  geom_bar()



# SHAPE AND DURATION
ufo_data %>%
  filter(!is.na(duration..seconds.) & duration..seconds. <= 25000 & shape != "")%>%
  ggplot(aes(x=duration..seconds., fill = as.factor(shape))) +
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
  labs(x = "Year", y= "count of sightings", title = "UFO sightings by year", subtitle = "Data from Maven Analytics - https://www.mavenanalytics.io/data-playground", caption ="")




# gene world map
ufo_data_lat_long %>%
  #filter (latitude<47.03 & latitude> 36.40 & longitude >6.41 & longitude < 18.32)  %>%
  ggplot() +
  geom_sf(data = world_data) +
  geom_point (aes(x=longitude, y=latitude), color = "blue", size = 2) +
  coord_sf (xlim = c(6.40, 18.35), ylim = c(36.00, 48.00), expand = T) #ITALIA
#coord_sf (xlim = c(-74.00, -28.40), ylim = c(5.30, -34.10), expand = T)

#shape = as.factor(format(as.Date(datetime, "%Y-%m-%d"),"%Y")
