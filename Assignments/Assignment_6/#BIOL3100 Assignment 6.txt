#BIOL3100 Assignment 6
#Hayley Karns
#11/4/24


#Assignment instructions:
# Your task is to Write an R script that:
#     Cleans this data into tidy (long) form
#     Creates a new column specifying whether a sample is from soil or water
#     Generates a plot that matches this one (note just plotting dilution == 0.1):
#     Generates an animated plot that matches this one (absorbance values are mean of all 3 replicates for each group):


setwd('C:/Users/User/Desktop/Data_Course_KARNS/Assignments/Assignment_6')
getwd()
#set working directory

library(ggplot2)
library(tidyr)
library(dplyr)
library(gganimate)
#loaded libraries

#Step 1:  ...............................Clean Data....................................

dat <- read_csv('C:/Users/User/Desktop/Data_Course_KARNS/Data/BioLog_Plate_Data.csv')
view(dat)
#Loaded data

dat_long <- pivot_longer(dat, cols=Hr_24:Hr_144, names_to="Hours", values_to="Absorbance")
view(dat_long)
#Data changed from wide to long form

dat_long$Hours <- as.numeric(gsub("Hr_", "", dat_long$Hours))
view(dat_long)
#got rid of Hr in front of the times

#Step 2: ...............................Create new column....................................
df <- dat_long %>%
  mutate(sample_type = ifelse(grepl("Water", `Sample ID`, ignore.case = TRUE), "Water", "Soil"))
view(df)
#Created new column showing soil vs water




#Step 3: ...............................Create Plot 1....................................
filtered <- df %>%
  filter(Dilution == 0.1)
view(filtered)
#getting rid of dolutions over 0.1


ggplot(filtered, aes(x = Hours, y = Absorbance, color = sample_type, group=sample_type)) +
  geom_smooth(method= "loess", se = FALSE) +
  facet_wrap(~ Substrate) + 
  labs(
    title = "Absorbance Over Time with Dilution 0.1",
    x = "Time (hours)",
    y = "Absorbance",
  ) +
  scale_color_manual(values = c("red", "cyan3")) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size =5),
    strip.text = element_text(size = 8), 
    legend.position = "right"
  )
#Made plot, key things" loess did smoothing, facet wrapped by substrated, and sample type shown by different colors


#Step 4:  .................Create animate plot for just Itaconic Acid ...........................

data <- df %>% filter(Substrate == "Itaconic Acid")
view(data)
#filtering to just itaconic acid


mean_absorbance <- data %>%
  group_by(`Sample ID`, Hours, Dilution) %>%
  summarize(Mean_absorbance = mean(Absorbance, na.rm = TRUE), .groups = 'drop')
view(mean_absorbance)
# Calculated mean absorbance by Sample ID, Hours, and Dilution

animated_plot <- ggplot(mean_absorbance, aes(x = Hours, y = Mean_absorbance, color = `Sample ID`, group = `Sample ID`)) +
  geom_line(size = 1) +
  labs(
    title = "Itaconic Acid Mean Absorbance by Dilution and Location",
    x = "Time (Hours)",
    y = "Mean Absorbance"
  ) +
  scale_color_manual(values = c("red", "green3", "cyan3", "purple")) +
  scale_y_continuous(limits = c(0, 2.5)) +  
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10),  
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)    
  ) +
  facet_wrap(~ Dilution) +  # Facet by Dilution to create distinct panels
  transition_reveal(Hours)
#Plot made


animate(animated_plot, nframes = 100, fps = 10)
# Animated plot
