library(tidyverse)
library(lubridate)
library(ggmap)
library(ggrepel)
library(gridExtra)
library(pander)

data(Minard.troops)
data(Minard.cities)
data(Minard.temp)

troops = Minard.troops
cities = Minard.cities
temps = Minard.temp
Minard.temp2 <- data.frame(long=rev(temps$long),
                           lat=c(54.4,54.3,54.4,54.1,54.2,54.6,54.8,55.2,55.7))
lo = c(cities$long)
la = c(cities$lat)
s <- subset(troops, troops$long %in% lo & troops$lat %in% la)
Minard.temp <-Minard.temp %>% 
  mutate(date = as.character(date)) %>% 
  mutate(date = format(as.Date(date,format = "%b%d"), "%B %d")) %>% 
  mutate(date = ifelse(is.na(date),"",date)) 
breaks <- c(1, 2, 3) * 10^5
p1 <- ggplot() +
  geom_segment(data=Minard.temp2,aes(x=long,y=lat,xend=long,yend=53.8),size=0.2) +
  geom_path(data = troops, aes(x = long, y = lat, group = group, 
                               color = direction, size = survivors),
            lineend = "round") +
  geom_point(data = cities, aes(x = long, y = lat),
             color = "#DC5B44") +
  geom_text_repel(data = cities, aes(x = long, y = lat, label = city),
                  color = "#DC5B44", family = "Open Sans Condensed Bold",
                  size=ifelse(Minard.cities$city=="Moscou",8,4)) +
  geom_text_repel(
    data = s,
    nudge_y      = 0.5,
    nudge_x      = -0.7,
    angle        = 0,
    vjust        = 0,
    segment.size = 0.2,
    aes(x = s$long, y = s$lat, label = survivors),
  ) +
  scale_size(range = c(0.5, 18)) + 
  scale_colour_manual(values = c("#DFC17E", "#252523")) +
  labs(x = NULL, y = NULL) + 
  #guides(color = FALSE, size = FALSE) + 
  coord_cartesian(xlim = c(24, 38)) +
  xlab(NULL) + 
  ylab("Latitude") + 
  ggtitle("Napoleon's March on Moscow. Recreation of Minard's graphic by Rushikesh Joshi, Student no. 19300976") +
  theme_bw() +
  theme(legend.position="top", legend.box="horizontal")
temps.nice <- temps %>%
  mutate(nice.label = paste0(temp, "°C, ", date))
p2 <- ggplot(data = temps.nice, aes(x = long, y = temp)) +
  geom_line() +
  geom_label(aes(label = nice.label),
             family = "Open Sans Condensed Bold", size = 2.5) + 
  labs(x = NULL, y = "° Celsius") +
  scale_x_continuous(limits=c(24,38),name="")+
  scale_y_continuous(limits=c(-31.5,0),position = "right",name="")+
  theme(panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "EB Garamond 08"),
        panel.grid.major.y = element_line(color="black",size=0.1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(-1.3,0,0,0),"cm"),
        plot.title = element_text(hjust=0.5,size=12,family="EB Garamond 08")
  )+
  geom_segment(aes(xend=Minard.temp$long,yend=0),size=0.2)
p <- rbind(ggplotGrob(p1), ggplotGrob(p2))

# Adjust panels
panels <- p$layout$t[grep("panel", p$layout$name)]

# Because this plot doesn't use coord_equal, since it's not a map, we can use whatever relative numbers we want, like a 3:1 ratio
p$heights[panels] <- unit(c(3, 1), "null")

grid::grid.newpage()
grid::grid.draw(p)