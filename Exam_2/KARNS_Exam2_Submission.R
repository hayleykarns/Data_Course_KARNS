# Hayley Karns
#
# BIOL3100
#
# EXAM 2
#
# Attempt 3.0
#
# Date: 11/5/2024
#



# PREP WORK

library(ggplot2)
library(tidyr)
library(dplyr)

setwd('C:/Users/User/Desktop/Data_Course_KARNS/BIOL3100_Exams/Exam_2')
getwd()



#1. Read in the unicef data (10 pts) 

data <- read.csv("unicef-u5mr.csv")
view(data)



#2. Get it into tidy format (10 pts) 

tidy_data <- pivot_longer(
  data,
  cols = starts_with("U5MR"),
  names_to = "Year",
  names_prefix = "U5MR.",
  values_to = "U5MR"
)

tidy_data$Year <- as.numeric(tidy_data$Year)
view(tidy_data)



#3. Plot each country’s U5MR over time (20 points)


ggplot(tidy_data, aes(x = Year, y = U5MR, group = CountryName, color = CountryName)) +
  geom_line() +
  labs(title = "Under-5 Mortality Rate (U5MR) Over Time by Country",
       x = "Year",
       y = "U5MR (per 1000 live births)") +
  facet_wrap(~ Continent, scales = "free_y") +
  theme_minimal(base_size = 12) +  
  theme(
    strip.text = element_text(size = 14, face = "bold"),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    panel.border = element_blank(),      
    axis.text = element_text(size = 10), 
    legend.position = "none"            
  )


# 4. Save this plot as LASTNAME_Plot_1.png (5 pts)


#Plot saved 


# 5. Create another plot that shows the mean U5MR for all the countries within a given continent at each year (20 pts)

continent_mean <- tidy_data %>%
  group_by(Continent, Year) %>%
  summarize(Mean_U5MR = mean(U5MR, na.rm = TRUE))

ggplot(continent_mean, aes(x = Year, y = Mean_U5MR, color = Continent)) +
  geom_line(size = 1.2) +  # Thicker line for visibility
  labs(title = "Mean Under-5 Mortality Rate (U5MR) Over Time by Continent",
       x = "Year",
       y = "Mean U5MR (per 1000 live births)",
       color = "Continent") +
  theme_minimal(base_size = 14) +  # Clean, minimal theme with larger font
  scale_color_manual(values = c("Africa" = "red", "Americas" = "green",
                                "Asia" = "blue", "Europe" = "purple",
                 
                                               "Oceania" = "cyan")) 

# 6. Save that plot as LASTNAME_Plot_2.png (5 pts) 

#Saved 


# 7. Create three models of U5MR (20 pts)


mod1 <- lm(U5MR ~ Year, data = tidy_data)                 # Model 1: Only Year
mod2 <- lm(U5MR ~ Year + Continent, data = tidy_data)     # Model 2: Year and Continent
mod3 <- lm(U5MR ~ Year * Continent, data = tidy_data)     # Model 3: Year, Continent, and their interaction

summary(mod1)
summary(mod2)
summary(mod3)

# 8. Compare the three models with respect to their performance

mod_compared <- data.frame(
  Model=c("mod1", "mod2", "mod3"), 
  R_squared= c(summary(mod1)$r.squared, summary(mod2)$r.squared, summary(mod3)$r.squared),
  AIC=c(AIC(mod1), AIC(mod2), AIC(mod3)),
  BIC=c(BIC(mod1), BIC(mod2), BIC(mod3))
)

view(mod_compared)
# Answer: Model3 is the best model because it has the highest R^2 value and the lowest
#         BIC and AIC values. That being said, none of the models are excellent becuase the
#         highest R^2 value is 0.64 which is pretty low. A value of 0.90 would be more robust.



# 9. Plot the 3 models’ predictions like so: (10 pts)


tidy_data <- tidy_data %>%
  mutate(
    Fitted_mod1 = predict(mod1, newdata = tidy_data),
    Fitted_mod2 = predict(mod2, newdata = tidy_data),
    Fitted_mod3 = predict(mod3, newdata = tidy_data)
  )


predictions <- tidy_data %>%
pivot_longer(cols= starts_with("Pred"), names_to = "model", values_to = "pred_U5MR")



# Average by Continent and Year
continent_fitted <- tidy_data %>%
  group_by(Continent, Year) %>%
  summarize(
    U5MR = mean(U5MR, na.rm = TRUE),
    Fitted_mod1 = mean(Fitted_mod1, na.rm = TRUE),
    Fitted_mod2 = mean(Fitted_mod2, na.rm = TRUE),
    Fitted_mod3 = mean(Fitted_mod3, na.rm = TRUE)
  ) %>%
  ungroup()

# Reshaping data
plot_data <- continent_fitted %>%
  select(Continent, Year, U5MR, Fitted_mod1, Fitted_mod2, Fitted_mod3) %>%
  pivot_longer(cols = starts_with("Fitted"), names_to = "Model", values_to = "Fitted_U5MR") %>%
  mutate(Model = recode(Model, 
                        "Fitted_mod1" = "Model 1: Year Only",
                        "Fitted_mod2" = "Model 2: Year + Continent",
                        "Fitted_mod3" = "Model 3: Year * Continent"))

# Plotting Model
ggplot(plot_data, aes(x = Year, y = Fitted_U5MR, color = Continent, group = Continent)) +
  geom_line(size=1.5) + 
  facet_wrap(~ Model) + 
  labs(
    title = "Predicted Under-5 Mortality Rate (U5MR) Over Time by Model",
    x = "Year",
    y = "Fitted U5MR (per 1000 live births)",
    color = "Continent"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        panel.border = element_rect(color = "black", fill = NA, size = 1)
  )


# 10. BONUS - 
#       Using your preferred model, predict what the U5MR would be for Ecuador in the year 2020. 
#       The real value for Ecuador for 2020 was 13 under-5 deaths per 1000 live births. 
#       How far off was your model prediction???


ecuador_data <- data.frame(Year = 2020, Continent = "Americas", CountryName = "Ecuador")

predicted_U5MR <- predict(mod3, newdata = ecuador_data)

cat("Predicted U5MR for Ecuador in 2020:", predicted_U5MR)

difference <-abs(predicted_U5MR-13)

print(difference)

#Answer: The value I found was -10.58018, and the difference between my 
#         predicted value and the real value is 23.58018.








