# Assignment 4 --> Final project plot
# 9/30/24
# Data used for this plot is the values pulled from quartz in mylonite sample NU-02, extracted using ImageJ

library(ggplot2)
library(magrittr)

getwd()
#checking working directory

df <- read.csv('DiffusionPractice.csv')
#reading in file

head(df)
#checking to make sure the file is correct

head(df)
#made file edits, double checking file

df %>%
  ggplot()+
  geom_point(aes(x=Seconds, y=Diffusivity1), color='blue')+
  geom_point(aes(x=Seconds, y=Diffusivity2), color='green')+
  geom_point(aes(x=Seconds, y=Diffusivity3), color='yellow')+
  geom_point(aes(x=Seconds, y=Diffusivity4), color='orange')+
  labs(title = "Diffusion Profile of Ttn in Qtz",
       x = "Time",
       y = "Diffusivity") +
  theme_minimal()
#plot has been made, different colors represent different diffusivities

