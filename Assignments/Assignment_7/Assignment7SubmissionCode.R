
#HAYLEY KARNS
#BIOL3100
#ASSIGNMENT 7
#DATE: 11/13/24


#TASKS:
#Import the Assignment_7/Utah_Religions_by_County.csv data set

#Clean it up into “tidy” shape

#Explore the cleaned data set with a series of figures (I want to see you exploring the data set)

#Address the questions:

#  “Does population of a county correlate with the proportion of any specific religious group in that county?”
# “Does proportion of any specific religion in a given county correlate with the proportion of non-religious people?”
# Just stick to figures and maybe correlation indices…no need for statistical tests yet

#add comment lines that show your thought processes _____________


#Assignment prep work, loading data, setting directory, loading libraries"

install.packages("stringr")
install.packages(c("sf", "tigris", "viridis"))
install.packages("terra")

library(terra)
library(tidyr)
library(ggplot2)
library(readr)
library(dplyr)
library(stringr)
library(sf)
library(tigris)
library(viridis)



getwd()
setwd("C:/Users/User/Desktop/Data_Course_KARNS/Assignments/Assignment_7")
getwd()

data <- read.csv('Utah_Religions_by_County.csv')
View(data)

#Getting data into long form
tidy_data <- data %>%
  pivot_longer(cols = -c("County", "Pop_2010", "Religious", "Non.Religious"),
               names_to = "Religion",
               values_to = "Proportion")

View(tidy_data)  

#Renaming columns for clarity and adjusting proportions to percentage
#Also filtering out religions with 0 for percentage value
tidy_data <- tidy_data %>%
  rename(`Non Religious` = `Non.Religious`)

tidy_data <- tidy_data %>%
  rename("Percentage" = "Proportion")

tidy_data <- tidy_data %>%
  mutate(`Non Religious` = `Non Religious` * 100)


tidy_data <- tidy_data %>%
  mutate("Percentage" = Percentage * 100)

tidy_data <- tidy_data %>%
  mutate("Religious" = Religious * 100)

tidy_data <- tidy_data %>%
  filter(Percentage != 0)

tidy_data <- tidy_data %>%
  mutate(County = str_remove(County, "County", ignore_case = TRUE))

# Removing County from the county names to make shorter for plot
tidy_data <- tidy_data %>%
  mutate(County = ifelse(endsWith(County, "County"),
                         substr(County, 1, nchar(County) - 6),
                         County))

View(tidy_data)  



#PLOTS AND FIGURES

#Bar chart

ggplot(tidy_data, aes(x = County, y = Percentage, fill = Religion)) +
  geom_bar(stat = "identity") +
  labs(title = "Religious Affiliations by County in Utah", 
       x = "County", 
       y = "Percentage") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")

#Heat map Grid

ggplot(tidy_data, aes(x = County, y = Religion, fill = Percentage)) +
  geom_tile() +
  labs(title = "Heatmap of Religious Proportions by County",
       x = "County",
       y = "Religion") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



# Filter LDS data
lds_tidy <- tidy_data %>% filter(Religion == "LDS")

# Load Utah county shapefile
if (!file.exists("./Counties.shp")) stop("Shapefile not found!")
utah_counties <- terra::vect("./Counties.shp")


# Convert county names to lowercase and trim spaces
lds_tidy$County <- tolower(trimws(lds_tidy$County))
utah_counties$NAME <- tolower(trimws(utah_counties$NAME))

# Join religion data with spatial data
lds_tidy_df <- as.data.frame(lds_tidy)
utah_counties <- terra::merge(utah_counties, lds_tidy_df, by.x = "NAME", by.y = "County", all.x = TRUE)

# Verify the spatial object
print(terra::names(utah_counties))  # Ensure 'Percentage' column exists

# Plot with terra
terra::plot(utah_counties, "Percentage", breaks = 5, col = c("blue", "lightblue", "white", "pink", "red"))

# Convert to sf object for ggplot2
utah_counties_sf <- sf::st_as_sf(utah_counties)

# Plot with ggplot2
ggplot(utah_counties_sf) +
  geom_sf(aes(fill = Percentage)) +
  scale_fill_gradient2(name = "LDS Religion Percentage", 
                       low = "red", mid = "white", high = "blue", 
                       midpoint = 50) +
  theme_minimal() +
  labs(title = "Percentage of Religion in Utah by County") +
  theme(legend.position = "right")

# QUESTIONS
# #“Does population of a county correlate with the proportion of any specific religious group in that county?”
# Yes. Utah County specifically has a high percentage of the LDS religion, so it has a higher proportion of people 
# that are LDS. San Juan has the lowest percentage of LDS people, which could correlate with native american 
# religions, which aren't taken into account in this data.

#“Does proportion of any specific religion in a given county correlate with the proportion of non-religious people?”
# I noticed Morgan county was around 90% LDS and 10% non religious, which would mean most if not all of religious
# people in that county were LDS. Some counties had this direct relationship, but some counties like SLC did not
# have this relationship. I also would have liked to see an athiest option or an option to represent native american
# religions, because San Juan county has the lowest percentage of LDS and religious, but there are many reservations 
# there.
#