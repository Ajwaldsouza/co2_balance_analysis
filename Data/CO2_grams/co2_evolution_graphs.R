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
co2_grams_peat <- read_csv("actual_co2_grams_peat.csv")
head(co2_grams_peat)


#Manipulating Time
##Setting first column as date-time variable
co2_grams_peat$time <- as_datetime(co2_grams_peat$time)                                  
#Setting the time boundaries of the dataset.
# co2_grams <- subset(co2_grams,
#                          time >= as_datetime('2021-04-21 20:00:00')) #&
#time <= as_datetime('2021-04-23 15:30:00'))

#Reducing the dataset by reducing sampling interval
groups <- cut(as.POSIXct(co2_grams_peat$time), breaks="30 min")
library(plyr)
co2_grams_peat <- ddply(co2_grams_peat, "groups", tail, 1)[, -1]
###Through the code above, the dataset size is reduced by retaining the sampling
###of every 10 mins ("breaks= 10min"). Change the time interval as necessary.

#Setting up a function the converts 1000s to k
# ks <- function (x) { number_format(accuracy = 1,
#                                    scale = 1/1000,
#                                    suffix = "k",
#                                    big.mark = ",")(x) }
# 



#Making individual treatment-wise datasets


t1 <- co2_grams_peat %>%
  select(time, co2_grams1, co2_grams13, co2_grams16) %>%
  mutate(time = as_datetime(paste0(time, "-01"))) %>%
  gather(vessel, co2_ppm, -time) %>%
  filter(complete.cases(.))%>%
  add_column(trmt = "peat")%>%
  mutate(co2_ppm = co2_ppm/57.89)






# Converting "grams CO2 per gram" DM TO  "grams CO2 per kg DM"
t1$co2_ppm <- t1$co2_ppm*1000





#*********************************************************************************
#-------------------------------PLUG BASED----------------------------------------
#********************************************************************************

#Reading the co2 .csv file
actual_co2_plug <- read_csv("actual_co2_grams_plug.csv")%>%as_tibble()
head(actual_co2_plug)





#Manipulating Time
##Setting first column as date-time variable
actual_co2_plug$time <- as_datetime(actual_co2_plug$time)                                  
# #Setting the time boundaries of the dataset.
# cum_co2 <- subset(cum_co2,
#                          time >= as_datetime('2021-04-21 20:00:00')) #&
# #time <= as_datetime('2021-04-23 15:30:00'))

#Reducing the dataset by reducing sampling interval
groups <- cut(as.POSIXct(actual_co2_plug$time), breaks="30 min")
library(plyr)
actual_co2_plug <- ddply(actual_co2_plug, "groups", tail, 1)[, -1]
###Through the code above, the dataset size is reduced by retaining the sampling
###of every 10 mins ("breaks= 10min"). Change the time interval as necessary.





#Making individual treatment-wise datasets

t2 <- actual_co2_plug %>%
  select(time, co2_grams7, co2_grams10,  co2_grams16) %>%
  mutate(time = as_datetime(paste0(time, "-01"))) %>%
  gather(vessel, co2_ppm, -time) %>%
  filter(complete.cases(.))%>%
  add_column(trmt = "plug")%>%
  mutate(co2_ppm = co2_ppm/47.7)   # dividing by total dry matter in the vessel


t2$co2_ppm <- t2$co2_ppm*1000










t2_time <- t2%>%select(c(1))


t1x <- t1%>%select(-c(1))%>%slice(c(1:2068))
t1x <- cbind(t2_time, t1x)




co2 <- rbind(t1x, t2)




# Making a combined plot with means and SEM
fig <- ggplot(data=co2, aes(x=time, y=co2_ppm, color = trmt)) +
  scale_y_continuous(limits = c(0,0.005), breaks=seq(0, 0.005,0.001))+
  scale_x_datetime(
    date_labels = c("1", "5", "10", "15"),
    breaks = c(
      as_datetime('2021-12-22 15:30:00'),
      as_datetime('2021-12-26 15:30:00'),
      as_datetime('2021-12-31 15:30:00'),
      as_datetime('2022-01-05 15:30:00')
    ),
    limits = c(
      as_datetime('2021-12-21 15:30:00'),
      as_datetime('2022-01-05 15:30:00')
    )
  ) +
  labs(x="Time (days)", y=expression(CO[2]~(g)/dry~matter~(kg)), color="Treatment")+
  theme_pubr()+
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        axis.line = element_line(size = 0.4))+
  theme(#legend.position = c(0.8, 0.75),
    legend.direction = "horizontal",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.position="top")+
  scale_fill_manual(name="",
                    labels = c("Mat-based", "Plug-based"),
                    values = c("#e41a1c", "#377eb8"),
                    guide = guide_legend())+
  scale_color_manual(name="",
                     labels = c("Mat-based", "Plug-based"),
                     values = c("#e41a1c", "#377eb8"),
                     guide = guide_legend())+
  stat_summary(aes(color=trmt), fun  = "mean", geom = "line", size=0.85)+
  stat_summary(aes(fill=factor(trmt)),colour = NA, geom = "ribbon", fun.data = mean_se, alpha = 0.35)



fig






ggsave(file="co2_linegraph.pdf", plot=fig, width = 5.83, 
       height = 4.13)

























