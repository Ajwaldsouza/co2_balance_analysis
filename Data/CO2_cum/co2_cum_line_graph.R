#INITIAL SETUP
##Load necessary packages
install.packages("librarian")
librarian::shelf(tidyverse, ggplot2, dplyr, tibble, readr, lubridate, ggpubr, 
                 scales, tidyr, rstudioapi,
                 update_all = FALSE)


##Setup working Directory
setwd(dirname(getActiveDocumentContext()$path))                                 # Set working directory to source file location




#*********************************************************************************
#-------------------------------PEAT BASED----------------------------------------
#********************************************************************************


#Reading the co2 .csv file
actual_co2_peat <- read_csv("actual_co2_grams_peat.csv")%>%
  as_tibble()%>%
  mutate_at(c(2:17), ~ifelse(is.na(.), 0, .))%>%
  select(c(1,2, 14,15,17))
  
head(actual_co2_peat)



# Making cumulative CO2 readings
cum_co2_peat <-actual_co2_peat%>%mutate_at(c(2:5), ~cumsum(.x), na.rm = TRUE)#%>%add_column(actual_co2, .before = 1)




#Manipulating Time
##Setting first column as date-time variable
cum_co2_peat$time <- as_datetime(cum_co2_peat$time)                                  


groups <- cut(as.POSIXct(cum_co2_peat$time), breaks="10 min")
library(plyr)
cum_co2_peat <- ddply(cum_co2_peat, "groups", tail, 1)[, -1]
###Through the code above, the dataset size is reduced by retaining the sampling
###of every 10 mins ("breaks= 10min"). Change the time interval as necessary.



groups <- cut(as.POSIXct(actual_co2_peat$time), breaks="10 min")
library(plyr)
actual_co2_peat <- ddply(actual_co2_peat, "groups", tail, 1)[, -1]




#Making individual treatment-wise datasets

t1 <- cum_co2_peat %>%
  select(time, co2_grams1, co2_grams13,  co2_grams16) %>%
  mutate(time = as_datetime(paste0(time, "-01"))) %>%
  gather(vessel, co2_ppm, -time) %>%
  filter(complete.cases(.))%>%
  add_column(trmt = "peat")%>%
  add_column(type = "cum")%>%
  mutate(co2_ppm = co2_ppm/57.89)   # dividing by total dry matter in the vessel


t2 <- actual_co2_peat %>%
  select(time, co2_grams1, co2_grams13,  co2_grams16) %>%
  mutate(time = as_datetime(paste0(time, "-01"))) %>%
  gather(vessel, co2_ppm, -time) %>%
  filter(complete.cases(.))%>%
  add_column(trmt = "peat")%>%
  add_column(type = "actual")%>%
  mutate(co2_ppm = co2_ppm/57.89)


t3 <- rbind(t1, t2)


# Making a combined plot with means and SEM
fig_peat <- ggplot(data=t3, aes(x=time, y=co2_ppm, fill = trmt, linetype = trmt )) +
  scale_y_continuous(limits = c(0,0.3), breaks=seq(0, 0.3,0.1))+
  scale_x_datetime(
    date_labels = c("1", "5", "10", "15"),
    breaks = c(
      as_datetime('2021-12-02 19:30:00'),
      as_datetime('2021-12-06 19:30:00'),
      as_datetime('2021-12-11 19:30:00'),
      as_datetime('2021-12-16 19:30:00')
    ),
    limits = c(
      as_datetime('2021-12-01 19:30:00'),
      as_datetime('2021-12-16 19:30:00')
    )
  ) +
  labs(title = "Mat-based biowaste",
       x="Time (days)",
       y=expression(Cumulative~CO[2]~(g)/unit~DM~(g)),
       color="Treatment")+
  theme_pubr()+
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        axis.line = element_line(size = 0.4))+
  theme(#legend.position = c(0.8, 0.75),
        legend.direction = "horizontal",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.position="top")+
  scale_fill_manual(name="SCG (% of total mixture)",
    labels = c("0%", "15%","30%", "50%"),
    values = c("#e41a1c", "#377eb8", "#984ea3", "#ff7f00"),
    guide = guide_legend())+
  scale_color_manual(name="SCG (% of total mixture)",
    labels = c("0%", "15%","30%", "50%"),
    values = c("#e41a1c", "#377eb8", "#984ea3", "#ff7f00"),
    guide = guide_legend())+
  stat_summary(aes(color=trmt), fun  = "mean", geom = "line", size=0.85)+
  stat_summary(geom = "ribbon", fun.data = mean_se, alpha = 0.25)



fig_peat

















#*********************************************************************************
#-------------------------------PLUG BASED----------------------------------------
#********************************************************************************


#Reading the co2 .csv file
actual_co2_plug <- read_csv("actual_co2_grams_plug.csv")%>%
  as_tibble()%>%
  mutate_at(c(2:17), ~ifelse(is.na(.), 0, .))%>%
  select(c(1,8,11,15,17))
head(actual_co2_plug)


# Making cumulative CO2 readings
cum_co2_plug <-actual_co2_plug%>%mutate_at(c(2:5), ~cumsum(.x), na.rm = T)



cums <- cumsum(actual_co2_plug[2:ncol(actual_co2_plug)])


#Manipulating Time
##Setting first column as date-time variable
cum_co2_plug$time <- as_datetime(cum_co2_plug$time)                                  
# #Setting the time boundaries of the dataset.
# cum_co2 <- subset(cum_co2,
#                          time >= as_datetime('2021-04-21 20:00:00')) #&
# #time <= as_datetime('2021-04-23 15:30:00'))

#Reducing the dataset by reducing sampling interval
groups <- cut(as.POSIXct(cum_co2_plug$time), breaks="10 min")
library(plyr)
cum_co2_plug <- ddply(cum_co2_plug, "groups", tail, 1)[, -1]
###Through the code above, the dataset size is reduced by retaining the sampling
###of every 10 mins ("breaks= 10min"). Change the time interval as necessary.




groups <- cut(as.POSIXct(actual_co2_plug$time), breaks="10 min")
library(plyr)
actual_co2_plug <- ddply(actual_co2_plug, "groups", tail, 1)[, -1]







#Making individual treatment-wise datasets

p1 <- cum_co2_plug %>%
  select(time, co2_grams7, co2_grams10,  co2_grams16) %>%
  mutate(time = as_datetime(paste0(time, "-01"))) %>%
  gather(vessel, co2_ppm, -time) %>%
  filter(complete.cases(.))%>%
  add_column(trmt = "plug")%>%
  add_column(type = "cum")%>%
  mutate(co2_ppm = co2_ppm/47.7)%>%   # dividing by total dry matter in the vessel
  slice(-c(1:177))


p2 <- actual_co2_plug %>%
  select(time, co2_grams7, co2_grams10,  co2_grams16) %>%
  mutate(time = as_datetime(paste0(time, "-01"))) %>%
  gather(vessel, co2_ppm, -time) %>%
  filter(complete.cases(.))%>%
  add_column(trmt = "plug")%>%
  add_column(type = "actual")%>%
  mutate(co2_ppm = co2_ppm/47.7)%>%   # dividing by total dry matter in the vessel
  slice(-c(1:177))




# Replacing plugs time stamps with peats time stamps
p1 <- p1%>%
  select(-c(1))
peat_time <- t1%>% select(c(1))
p1 <- cbind(peat_time, p1)



p2 <- p2%>%
  select(-c(1))

peat_time <- t1%>% select(c(1))
p2 <- cbind(peat_time, p2)


#_____________________________________________________________________________
# CUMULATIVE CO2 LINE GRAPH

p_co2 <- rbind(t1, p1)


# Making a combined plot with means and SEM
fig_plug <- ggplot(data=p_co2, aes(x=time, y=co2_ppm, fill=trmt)) +
  scale_y_continuous(limits = c(0,0.3), breaks=seq(0, 0.3,0.1))+
  # scale_x_datetime(
  #   date_labels = c("1", "5", "10", "15"),
  #   breaks = c(
  #     as_datetime('2021-12-22 15:30:00'),
  #     as_datetime('2021-12-26 15:30:00'),
  #     as_datetime('2021-12-31 15:30:00'),
  #     as_datetime('2022-01-05 15:30:00')
  #   ),
  #   limits = c(
  #     as_datetime('2021-12-21 15:30:00'),
  #     as_datetime('2022-01-05 15:30:00')
  #   )
  # ) +
  labs(title = "Plug-based biowaste",
       x="Time (days)", 
       y=expression(Cumulative~CO[2]~(g)/unit~DM~(g)), 
       color="Treatment")+
  theme_pubr()+
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        axis.line = element_line(size = 0.4))+
  theme(#legend.position = c(0.8, 0.75),
    legend.direction = "horizontal",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.position="top")+
  # scale_fill_manual(name="Proportion of SCG in the mixture:",
  #                   labels = c("0%", "15%","30%", "50%"),
  #                   values = c("#e41a1c", "#377eb8", "#984ea3", "#ff7f00"),
  #                   guide = guide_legend())+
  # scale_color_manual(name="Proportion of SCG in the mixture:",
  #                    labels = c("0%", "15%","30%", "50%"),
  #                    values = c("#e41a1c", "#377eb8", "#984ea3", "#ff7f00"),
  #                    guide = guide_legend())+
  stat_summary(aes(color=trmt), fun  = "mean", geom = "line", size=0.85)+
  stat_summary(geom = "ribbon", fun.data = mean_se, alpha = 0.25)



fig_plug





#_____________________________________________






# ALL TOGETHER

combined <- rbind(t1, t2, p1, p2)
















# Making a combined plot with means and SEM
fig_plug <- ggplot(data=combined, aes(x=time, y=co2_ppm, fill=trmt, linetype = type)) +
  scale_y_continuous(limits = c(0,0.3), breaks=seq(0, 0.3,0.1))+
  # scale_x_datetime(
  #   date_labels = c("1", "5", "10", "15"),
  #   breaks = c(
  #     as_datetime('2021-12-22 15:30:00'),
  #     as_datetime('2021-12-26 15:30:00'),
  #     as_datetime('2021-12-31 15:30:00'),
  #     as_datetime('2022-01-05 15:30:00')
  #   ),
  #   limits = c(
  #     as_datetime('2021-12-21 15:30:00'),
  #     as_datetime('2022-01-05 15:30:00')
#   )
# ) +
labs(title = "Plug-based biowaste",
     x="Time (days)", 
     y=expression(Cumulative~CO[2]~(g)/unit~DM~(g)), 
     color="Treatment")+
  theme_pubr()+
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        axis.line = element_line(size = 0.4))+
  theme(#legend.position = c(0.8, 0.75),
    legend.direction = "horizontal",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.position="top")
  # scale_fill_manual(name="Proportion of SCG in the mixture:",
  #                   labels = c("0%", "15%","30%", "50%"),
  #                   values = c("#e41a1c", "#377eb8", "#984ea3", "#ff7f00"),
  #                   guide = guide_legend())+
  # scale_color_manual(name="Proportion of SCG in the mixture:",
  #                    labels = c("0%", "15%","30%", "50%"),
  #                    values = c("#e41a1c", "#377eb8", "#984ea3", "#ff7f00"),
  #                    guide = guide_legend())+
  # stat_summary(aes(color=trmt, linetype = type), fun  = "mean", geom = "line", size=0.85)+
  # stat_summary(geom = "ribbon", fun.data = mean_se, alpha = 0.25)



fig_plug




