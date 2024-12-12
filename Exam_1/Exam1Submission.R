# Hayley Karns
# BIOL3100 Exam 1
# Date: 10/13/24


# Prep: Opening libraries and setting directory
library(ggplot2)
library(tidyverse)
library(dplyr)

getwd()
setwd("C:/Users/User/Desktop/Data_Course_KARNS/BIOL3100_Exams/Exam_1")
getwd()

#Step 1: Get cleaned_covid_data.csv file into an R data frame
read.csv('cleaned_covid_data.csv')

file_path <- "C:/Users/User/Desktop/Data_Course_KARNS/BIOL3100_Exams/Exam_1/cleaned_covid_data.csv"

data <- read.csv(file_path)

head(data)


#Step 2:Subset the data set to just show states that begin with "A" and save this as an object called A_states.

A_states <- subset(data, grepl("^A", Province_State))

print(A_states)




#Step 3: Create a plot _of that subset_ showing Deaths over time, with a separate facet for each state.

A_states$Last_Update <- as.POSIXct(A_states$Last_Update)

ggplot(A_states, aes(x = Last_Update, y = Deaths))+
  geom_point()+  
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  facet_wrap(~ Province_State, scales = "free") +
  labs(title = "Deaths Over Time by State Starting with 'A'",
       x = "Date",
       y = "Number of Deaths") +
  theme_minimal()  

#Step 4: Find the "peak" of Case_Fatality_Ratio for each state and save this as a new data frame object 
#called state_max_fatality_rate.

state_max_fatality_rate <- data %>%
  group_by(Province_State) %>%                  
  summarize(Maximum_Fatality_Ratio = max(Case_Fatality_Ratio, na.rm = TRUE)) %>%  
  arrange(desc(Maximum_Fatality_Ratio))           

print(state_max_fatality_rate)
view(state_max_fatality_rate)


#Step 5: Use that new data frame from task IV to create another plot.

#Making factor
state_max_fatality_rate$Province_State <- factor(
  state_max_fatality_rate$Province_State,
  levels = state_max_fatality_rate$Province_State[order(-state_max_fatality_rate$Maximum_Fatality_Ratio)]
)

#Errors popped up, checking data
str(state_max_fatality_rate)
summary(state_max_fatality_rate$Maximum_Fatality_Ratio)



state_max_fatality_rate <- state_max_fatality_rate %>%
  filter(!is.na(Maximum_Fatality_Ratio))

state_max_fatality_rate$Maximum_Fatality_Ratio <- as.numeric(state_max_fatality_rate$Maximum_Fatality_Ratio)

#Plottine
ggplot(state_max_fatality_rate, aes(x = Province_State, y = Maximum_Fatality_Ratio)) +
  geom_bar(stat = "identity", fill = "steelblue") + 
  labs(title = "Maximum Case Fatality Ratio by State",
       x = "Province State",
       y = "Maximum Fatality Ratio") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#90 degree adjustment


#Bonus Point Question

view(data)

data$Last_Update <- as.Date(data$Last_Update)


us_cumulative_deaths <- data %>%
  group_by(Last_Update) %>% 
  summarize(daily_deaths = sum(Deaths, na.rm = TRUE)) %>%
  arrange(Last_Update)


ggplot(us_cumulative_deaths, aes(x = Last_Update, y = daily_deaths)) +
  geom_line(color = "blue", linewidth = 1) +  
  labs(title = "Cumulative Deaths in the US Over Time",
       x = "Date",
       y = "Cumulative Deaths") +
  theme_minimal()

