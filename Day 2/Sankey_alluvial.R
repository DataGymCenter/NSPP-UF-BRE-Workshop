#############################
library(readtext)
library(LexisNexisTools)
library(tidyverse)
library(tools)
library(tidytext)
library(ggplot2)
library(RColorBrewer)
library(darklyplot)
library(ggfx)
library(gganimate)
library(gifski)
library(directlabels)
library(vroom)
###################################
## VROOMING it
olympics <- vroom("C:/NSPP_Workshop/Data/olympics.csv")

#####
olympics_mod <- olympics %>%
  filter(!Year < 1960 & 
           !Year > 2016 & 
           !Medal == "NA" & 
           !Year %in% c(1993,1994,1997,1998,2001,2002,2005,2006,2009,2010,2013,2014)) %>%
  mutate(Period = case_when(Year %in% c(1960:1969) ~ '1960-1969',
                            Year %in% c(1970:1979) ~ '1970-1979',
                            Year %in% c(1980:1989) ~ '1980-1989',
                            Year %in% c(1990:1999) ~ '1990-1999',
                            Year %in% c(2000:2009) ~ '2000-2009',
                            Year %in% c(2010:2016) ~ '2010-2016')) %>%
  filter(str_detect(Sport, fixed("ball"))) |>
  #dplyr::select(Year, Sex, NOC, Medal, Period) %>%
  group_by(Sex, Sport, Medal, Period) %>%
  count() %>%
  #filter(n > 40) |>
  summarise(Count = sum(n)) %>%
  ungroup()

########## Convert to factors
#olympics_mod$City <- factor(olympics_mod$City)
olympics_mod$Sex <- factor(olympics_mod$Sex)
olympics_mod$Sport <- factor(olympics_mod$Sport)
olympics_mod$Medal <- factor(olympics_mod$Medal)
olympics_mod$Period <- factor(olympics_mod$Period)

#################################
#######        1        #########
library(flipPlots)
#####
SankeyDiagram(olympics_mod[, -(4:5)],
              link.color = c("None", "Source", "Target", "First variable", 
                             "Last variable")[5], 
              weights = olympics_mod$Count,
              font.size = 20, font.unit = "pt",
              # variables.share.values = F,
              label.show.counts = T,
              label.show.percentages = F,
              #hovertext.show.percentages = F,
              max.categories = 66,
              label.show.varname = F, node.padding = 10)



################
olympics_mod <- olympics %>%
  filter(!Year < 1960 & !Year > 2016 & !Medal == "NA" & !Year %in% c(1993,1994,1997,1998,2001,2002,2005,2006,2009,2010,2013,2014)) %>%
  mutate(Period = case_when(Year %in% c(1960:1969) ~ '1960-1969',
                            Year %in% c(1970:1979) ~ '1970-1979',
                            Year %in% c(1980:1989) ~ '1980-1989',
                            Year %in% c(1990:1999) ~ '1990-1999',
                            Year %in% c(2000:2009) ~ '2000-2009',
                            Year %in% c(2010:2016) ~ '2010-2016')) %>%
  #dplyr::select(Year, Sex, NOC, Medal, Period) %>%
  group_by(Sex, City, Medal, Period) %>%
  count() %>%
  summarise(Count = sum(n))

#################################
#######        2        #########
source("C:/NSPP_Workshop/sankey_lly.R")
library(sfo)
library(htmlwidgets)
sfoed <- sankey_ly(x = olympics_mod,
          cat_cols = c("Sex", "City", "Medal", "Period"),
          num_col = "Count",
          title = " "); sfoed

## New Function
sfoed1 <- sankey_lyy(x = olympics_mod,
                   cat_cols = c("Sex", "City", "Medal", "Period"),
                   num_col = "Count",
                   title = " "); sfoed1
#Export
saveWidget(sfoed, file = "myplot.html")

library(svglite)
ggsave(file="sankeyed.svg", plot=sfoed, width=10, height=10, dpi = 300, limitsize = F)
#################################
#######        3        #########
df <- olympics %>%
  filter(!Year < 1960 & !Year > 2016 & !Medal == "NA" & !Year %in% c(1993,1994,1997,1998,2001,2002,2005,2006,2009,2010,2013,2014)) %>%
  mutate(Period = case_when(Year %in% c(1960:1969) ~ '1960-1969',
                            Year %in% c(1970:1979) ~ '1970-1979',
                            Year %in% c(1980:1989) ~ '1980-1989',
                            Year %in% c(1990:1999) ~ '1990-1999',
                            Year %in% c(2000:2009) ~ '2000-2009',
                            Year %in% c(2010:2016) ~ '2010-2016')) %>%
  #dplyr::select(Year, Sex, NOC, Medal, Period) %>%
  group_by(Year, Sex, City, Medal) %>%
  count() %>%
  summarise(Count = sum(n)) %>%
  ungroup()

#olympics_mod$City <- factor(olympics_mod$City)
df$Sex <- factor(df$Sex)
df$City <- factor(df$City)
df$Medal <- factor(df$Medal)
df$Year <- factor(df$Year)

library(ggforce)  
### Transforms the data for use by geom_parallel_sets
alluv <- gather_set_data(df, 1:4); alluv

### Create custom color palette
mycolors <- c("brown", "gold", "darkgrey")

#### Graphing
alluv_chart <- ggplot(alluv, aes(x, id = id, split = y, value=Count)) +
  geom_parallel_sets(
    aes(fill = Medal), alpha = 4, axis.width = 0.3, n=100, strength = 0.9
    ) + # 'n' here is similar to pixel
  geom_parallel_sets_axes(axis.width = 0.3, fill="grey41") +
  geom_parallel_sets_labels(colour = "white", angle = 360, size = 4.8) +
  scale_fill_manual(values = mycolors) +
  theme_economist_white() +
  #facet_wrap(vars(Year), ncol = 5) +
  #coord_flip() +
  #theme_base() +
  theme(
    plot.margin = ggplot2::margin(-0.6, # Top
                         -6.2, # Right
                         0.4, # Bottom
                         -6.2, # Left
                         "cm"),
    legend.position = "none",
    panel.background = element_rect(fill = "#232732"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 20, face = "bold", vjust = 0.5, hjust = 0.5),
    axis.title.x  = element_blank(),
    plot.title = element_text(size = 28, vjust = -25, hjust = 0.6, color = "brown",face = "bold"),
    plot.subtitle =  element_text(size = 21, color = "navyblue", vjust =-30, hjust = 0.6,face = "bold", lineheight = 0.9),
    plot.caption =  element_text(size = 14, vjust = 0.9, hjust = 0.84, face = "bold.italic", color = "brown", lineheight = 0.5)
); alluv_chart



###### Plot the TILE and ANIMATE

olym_sport <- olympics_mod %>% select(Year, Sport) %>% filter(!is.na(Year))

#library(ggthemes)
#########
gg1 <- ggplot(olym_sport) +
  aes(x = factor(Year), y = Sport, fill=factor(Sport)) +
  geom_tile(size = 1.0, na.rm = FALSE) +
  scale_y_discrete(limits = rev) +
  scale_fill_manual(values=mycolors) + 
  theme_base() + 
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(size=14, angle = 90, vjust = 0.5, hjust=0.5),
    axis.text.y = element_text(size=14, face = "italic", vjust = 0.38),
    axis.title.y = element_blank(),
    panel.spacing = unit(2, "lines"),
    panel.grid.major = element_line(colour = "black", size=0.05),
    panel.background = element_rect(fill = "#232732"),
    legend.position = "none",
    axis.line.x = element_line(colour = "darkorange", size=1.5, lineend = "butt"),
    axis.line.y = element_line(colour = "darkorange", size=1.5, lineend = "butt") 
  ); gg1 

###
anims_tile <- gg1 + transition_reveal(-Year) + ease_aes('linear'); anims_tile

#####
animate(anims_tile, duration = 15, end_pause = 30, fps = 40, width = 2000, height = 1200, renderer = gifski_renderer())

#### SAVE GIF - Reversed
anim_save(file.path("Workshop/", "Olympics_sports.gif"))


