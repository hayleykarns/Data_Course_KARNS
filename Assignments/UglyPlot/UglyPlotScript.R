#Ugly Plot Script
#BIOL3100 10/9/24


install.packages("jpeg")
install.packages("grid")

library(ggplot2)
library(dplyr)
library(jpeg)
library(grid)

getwd()

df <-read.csv('./Assignments/UglyPlot/tarantino.csv')

img <- readJPEG("./Assignments/UglyPlot/tarantinopic.jpeg")

img_raster <- rasterGrob(img, width = unit(1, "npc"), height = unit(1, "npc"), interpolate = TRUE)

ggplot(df,(aes(x=word, y=minutes_in, col=movie)))+
  annotation_custom(img_raster, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)+
  geom_point()+
  labs(title = 'naughty words ;) minut0s in "Tarantino" Movies', 
       caption = 'boi does quentin tarantino really likes swear words', 
       x= 'cUrs3 w0rd',
       y= 'minut0s in le m0vie')+
  theme(panel.background = element_rect(fill = "cyan", color = "red"),
  plot.title=element_text(color='chartreuse3', size=16, face='italic'),      
  plot.background = element_rect(fill = "chartreuse1", color = "red"),
  axis.title.x=element_text(color='red', size=14, face='bold'),
  axis.title.y=element_text(color='magenta1', size=60, face='italic'),
  scale_fill_manual(values = c("A" = "red", "B" = "blue", "C" = "green")),
  legend.key = element_rect(fill="cyan", color="yellow"),
  legend.text = element_text(size=10, color="darkblue", angle=45),
  legend.background = element_rect(fill="darkgoldenrod", color="maroon1", size=0.5))






