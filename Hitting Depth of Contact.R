library(ggplot2)
library(plyr)
library(tidyverse)

df <- read.csv('sample.csv') %>%
  filter(PitchCall == 'InPlay',
         HitType != 'Bunt')

# from Yakkertech Glossary - yt_HitLocation (X,Y,Z) - The point where contact was made, with (X,Z) being the plate location and Y being the "depth" (measured from the back tip of home plate)
# Contact depth overhead using Yakkertech data. Trackman data will have differently named columns, but the same can be achieved.
# Yakkertech measures in feet, so we multiply by 12 to get inches instead to get more granular

ggplot(df, aes(yt_HitLocationX * 12, yt_HitLocationY * 12, PitchNo, ExitSpeed, Angle, Bearing))+
  scale_x_continuous(limit= c(-10,10), breaks=seq(-10,10 ,1))+
  scale_y_continuous(limit= c(-3,51), breaks=seq(0,51 ,3)) +
  geom_point(aes(color = ExitSpeed) ,size = 6, alpha = .9)+
  scale_color_gradient2(midpoint=86, low="cornflowerblue", mid="white",high="darkred")+
  # scale_color_gradient(low="lightblue",high="darkred")+
  geom_text(aes(label = PitchNo),size =3, check_overlap = TRUE, colour = 'black')+
  # home plate outline
  geom_segment(aes(x = -8.5, y = 8.5, xend = -8.5, yend = 17), size = 1, alpha = 1)+ # left side
  geom_segment(aes(x = 8.5, y = 8.5, xend = 8.5, yend = 17), size = 1, alpha = 1)+ # right side
  geom_segment(aes(x = -8.5, y = 17, xend = 8.5, yend = 17), size = 1, alpha = 1)+ # front plate
  geom_segment(aes(x = -8.5, y = 8.5, xend = 0, yend = 0), size = 1, alpha = 1)+
  geom_segment(aes(x = 8.5, y = 8.5, xend = 0, yend = 0), size = 1, alpha = 1)+
  geom_hline(yintercept=-3:51 *6, linetype="dashed", color = "red", size = 1, alpha = .3)+
  theme_bw()+
  theme(legend.position="right",
        legend.text = element_text(size = 8),
        aspect.ratio=2/1,
        # panel.background = element_rect(fill = "lightgrey",
        #                                 colour = "lightblue",
        #                                 size = 0.5, linetype = "solid"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  
  labs(title = paste(#df$Batter[1],
    "\nDepth of Contact"),
       subtitle = "Overhead View",
       x = "Horizontal Contact Point",
       y = "Contact Depth",
       color = paste("ExitVelo Scale\nAvg = ", round((mean(df$ExitSpeed,na.rm=TRUE)),1)))


# contact depth side 

ggplot(df, aes(yt_HitLocationY * 12, PlateLocHeight * 12 ))+
  scale_x_continuous(limit= c(-3,42), breaks=seq(0,42 ,6)) +
  scale_y_continuous(limit= c(0,60), breaks=seq(0,60 ,6)) +
  geom_point(aes(color = ExitSpeed) ,size = 6, alpha = .7)+
  scale_color_gradient2(midpoint=86, low="cornflowerblue", mid="white",high="darkred")+
  # scale_color_gradient(low="lightblue",high="darkred")+ # this is here in case you want to try 2 different color scales
  # geom_text(aes(label = PitchNo),size =3, check_overlap = TRUE, colour = 'black')+ # if you want to label batted balls
  # home plate outline
  geom_segment(aes(x = 8.5, y = 3, xend = 17, yend = 3), size = 1, alpha = .5)+ # left side
  geom_segment(aes(x = 8.5, y = 0, xend = 17, yend = 0), size = 1, alpha = .5)+ # right side
  geom_segment(aes(x = 17, y = 0, xend = 17, yend = 3), size =1, alpha = .5)+ # front plate
  geom_segment(aes(x = 8.5, y = 3, xend = 0, yend = 1.5), size = 1, alpha = .5)+ # left slant
  geom_segment(aes(x = 0, y = 1.5, xend = 8.5, yend = 0), size = 1, alpha = .5)+ # right slant
  # strike zone bottom
  geom_segment(aes(x = 8.5, y = 21, xend = 17, yend = 21), size = .1, alpha = .5)+ # left side
  geom_segment(aes(x = 8.5, y = 18, xend = 17, yend = 18), size = .1, alpha = .5)+ # right side
  geom_segment(aes(x = 17, y = 18, xend = 17, yend = 21), size = .1, alpha = .5)+ # front plate
  geom_segment(aes(x = 8.5, y = 21, xend = 0, yend = 19.5), size = .1, alpha = .5)+ # left slant
  geom_segment(aes(x = 0, y = 19.5, xend = 8.5, yend = 18), size = .1, alpha = .5)+ # right slant
  # strike zone top
  geom_segment(aes(x = 8.5, y = 54, xend = 17, yend = 54), size = .1, alpha = .5)+ # left side
  geom_segment(aes(x = 8.5, y = 51, xend = 17, yend = 51), size = .1, alpha = .5)+ # right side
  geom_segment(aes(x = 17, y = 51, xend = 17, yend = 54), size = .1, alpha = .5)+ # front plate
  geom_segment(aes(x = 8.5, y = 54, xend = 0, yend = 52.5), size = .1, alpha = .5)+ # left slant
  geom_segment(aes(x = 0, y = 52.5, xend = 8.5, yend = 51), size = .1, alpha = .5)+ # right slant
  # strikezone back & front
  geom_segment(aes(x = 0, y = 19.5, xend = 0, yend = 52.5), size = .1, alpha = .1)+ # back corner
  geom_segment(aes(x = 17, y = 18, xend = 17, yend = 54), size = .1, alpha = .1)+ # back corner
 # geom_hline(yintercept=-3:51 *6, linetype="dashed", color = "red", size = 1, alpha = .3)+
  theme_bw()+
  theme(legend.position="right",
        legend.text = element_text(size = 8),
        aspect.ratio=1.5/1,
        panel.background = element_rect(fill = "grey",
                                        colour = "cornflowerblue",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  labs(title = paste(df$Batter[1],"\nDepth of Contact"),
       subtitle = "Side View",
       x = "Contact Depth",
       y = "Pitch Height",
       color = paste("ExitVelo Scale\nAvg = ", round((mean(df$ExitSpeed,na.rm=TRUE)),1)))



# ----- plotly 3D tests
# this is a test to make a 3D plot. I would just need to add a 3d strike zone

plot_ly(df, x = ~yt_HitLocationX * 12, y = ~yt_HitLocationY * 12 , z = ~yt_HitLocationZ * 12, color = ~ExitSpeed) %>% 
  add_markers() %>%
  layout(scene = list(xaxis = list(range = c(-20,20) , title = 'Horz Contact Point'),
                      yaxis = list(range = c(-3,42), title = 'Depth of Contact'),
                      zaxis = list(range = c(0,60), title = 'Height of Pitch')) )


