#INITIAL SETUP
##Load necessary packages
install.packages("librarian")
librarian::shelf(tidyverse, ggplot2, dplyr, tibble, readr, lubridate, ggpubr,
                 rstudioapi,
                 update_all = FALSE)


##Setup working Directory
setwd(dirname(getActiveDocumentContext()$path))                                 # Set working directory to source file location




#*********************************************************************************
#-------------------------------PEAT BASED----------------------------------------
#********************************************************************************

temp_substrate_peat <- read_csv("temp_substrate_peat.csv")

#
#Manipulating Time
##Setting first column as date-time variable
temp_substrate_peat$time <- as_datetime(temp_substrate_peat$time)                                  
#Setting the time boundaries of the dataset.
# temp_substrate <- subset(temp_substrate,
#                       time >= as_datetime('2021-04-21 20:00:00')) #&
#  #time <= as_datetime('2021-04-23 15:30:00'))




#Reducing the dataset by reducing sampling interval
groups <- cut(as.POSIXct(temp_substrate_peat$time), breaks="30 min")
library(plyr)
temp_substrate_peat <- ddply(temp_substrate_peat, "groups", tail, 1)[, -1]
###Through the code above, the dataset size is reduced by retaining the sampling
###of every 5 mins ("breaks= 5min"). Change the time interval as necessary.




#Making individual treatment-wise datasets


p1 <- temp_substrate_peat %>%
  select(time,  sub_temp13, sub_temp14, sub_temp16) %>%                #Selecting the Vessels for the respective treatment
  mutate(time = as_datetime(paste0(time, "-01"))) %>%                           
  gather(vessel, sub_temp, -time) %>%
  filter(complete.cases(.)) %>%
  add_column(trmt = "1")

p2 <- temp_substrate_peat %>%
  select(time, sub_temp6, sub_temp8, sub_temp9, sub_temp11) %>%
  mutate(time = as_datetime(paste0(time, "-01"))) %>%
  gather(vessel, sub_temp, -time) %>%
  filter(complete.cases(.)) %>%
  add_column(trmt = "2")


p3 <- temp_substrate_peat %>%
  select(time, sub_temp2,  sub_temp12, sub_temp15) %>%
  mutate(time = as_datetime(paste0(time, "-01"))) %>%
  gather(vessel, sub_temp, -time) %>%
  filter(complete.cases(.)) %>%
  add_column(trmt = "3")


p4 <- temp_substrate_peat %>%
  select(time, sub_temp3, sub_temp4, sub_temp7, sub_temp10) %>%
  mutate(time = as_datetime(paste0(time, "-01"))) %>%
  gather(vessel, sub_temp, -time) %>%
  filter(complete.cases(.)) %>%
  add_column(trmt = "4")


p5 <- temp_substrate_peat %>%
  select(time, sub_temp17)  %>%
   mutate(time = as_datetime(paste0(time, "-01"))) %>%
   gather(vessel, sub_temp, -time) %>%
   filter(complete.cases(.)) %>%
   add_column(trmt = "ambient")


p <- rbind(p1, p2, p3, p4)




# Making a single plot with means and CI
fig_peat <- ggplot(data=p, aes(x=time, y=sub_temp)) +
  scale_y_continuous(limits = c(18,24), breaks=seq(18, 24,2))+
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
  labs(title = "Mat-based biowaste",x="Time (days)", y="Temperature (°C)")+
  theme_classic()+
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        axis.line = element_line(size = 0.4))+
  # theme(legend.position = c(0.8, 0.75),
  #       legend.direction = "vertical",
  #       legend.title = element_text(size = 12),
  #       legend.text = element_text(size = 10))+
  scale_fill_manual(name="SCG (% of total mixture)",
                    labels = c("0", "15","30", "50"),
                    values = c("#e41a1c", "#377eb8", "#984ea3", "#ff7f00"))+
  scale_color_manual(name="SCG (% of total mixture)",
                     labels = c("0", "15","30", "50"),
                     values = c("#e41a1c", "#377eb8", "#984ea3", "#ff7f00"))+
  stat_summary(aes(color=trmt), fun  = "mean", geom = "line" , size=0.6)+
  stat_summary(aes(fill=trmt), geom = "ribbon", fun.data = mean_se , alpha = 0.2)+
  geom_line(data = p5, aes(x=time, y=sub_temp, linetype=trmt), 
            size=0.55 )+
  scale_linetype_manual(name ="", labels= c("Ambient"),values=c("ambient"=2))+
  guides(linetype = guide_legend(order=2), col = guide_legend(order = 1), fill = guide_legend(order = 1))
  
# fig_peat <- annotate_figure(fig_peat, top = text_grob("Mat-based", hjust = 4.5, vjust = 4)  )

fig_peat





#*********************************************************************************
#-------------------------------PLUG BASED----------------------------------------
#********************************************************************************
temp_substrate_plug <- read_csv("temp_substrate_plug.csv")

#
#Manipulating Time
##Setting first column as date-time variable
temp_substrate_plug$time <- as_datetime(temp_substrate_plug$time)                                  
#Setting the time boundaries of the dataset.
# temp_substrate <- subset(temp_substrate,
#                       time >= as_datetime('2021-04-21 20:00:00')) #&
#  #time <= as_datetime('2021-04-23 15:30:00'))




#Reducing the dataset by reducing sampling interval
groups <- cut(as.POSIXct(temp_substrate_plug$time), breaks="30 min")
library(plyr)
temp_substrate_plug <- ddply(temp_substrate_plug, "groups", tail, 1)[, -1]
###Through the code above, the dataset size is reduced by retaining the sampling
###of every 5 mins ("breaks= 5min"). Change the time interval as necessary.




#Making individual treatment-wise datasets


t1 <- temp_substrate_plug %>%
  select(time, sub_temp10, sub_temp14, sub_temp16) %>%                #Selecting the Vessels for the respective treatment
  mutate(time = as_datetime(paste0(time, "-01"))) %>%                           
  gather(vessel, sub_temp, -time) %>%
  filter(complete.cases(.)) %>%
  add_column(trmt = "1")

t2 <- temp_substrate_plug %>%
  select(time,  sub_temp3, sub_temp11) %>%
  mutate(time = as_datetime(paste0(time, "-01"))) %>%
  gather(vessel, sub_temp, -time) %>%
  filter(complete.cases(.)) %>%
  add_column(trmt = "2")


t3 <- temp_substrate_plug %>%
  select(time, sub_temp4,  sub_temp8, sub_temp9) %>%
  mutate(time = as_datetime(paste0(time, "-01"))) %>%
  gather(vessel, sub_temp, -time) %>%
  filter(complete.cases(.)) %>%
  add_column(trmt = "3")


t4 <- temp_substrate_plug %>%
  select(time, sub_temp6, sub_temp12, sub_temp15) %>%
  mutate(time = as_datetime(paste0(time, "-01"))) %>%
  gather(vessel, sub_temp, -time) %>%
  filter(complete.cases(.)) %>%
  add_column(trmt = "4")


t5 <- temp_substrate_plug %>%
  select(time, sub_temp17)  %>%
  mutate(time = as_datetime(paste0(time, "-01"))) %>%
  gather(vessel, sub_temp, -time) %>%
  filter(complete.cases(.)) %>%
  add_column(trmt = "ambient")


t <- rbind(t1, t2, t3, t4)




# Making a single plot with means and CI
fig_plug <- ggplot(data=t, aes(x=time, y=sub_temp)) +
  scale_y_continuous(limits = c(18,24), breaks=seq(18, 24,2))+
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
      as_datetime('2022-01-05 19:30:00')
    )
  ) +
  labs(title = "Plug-based biowaste",x="Time (days)", y="Temperature (°C)")+
  theme_classic()+
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        axis.line = element_line(size = 0.4))+
  # theme(legend.position = c(0.8, 0.75),
  #       legend.direction = "vertical",
  #       legend.title = element_text(size = 12),
  #       legend.text = element_text(size = 10))+
  scale_fill_manual(name="SCG (% of total mixture)",
                    labels = c("0", "15","30", "50"),
                    values = c("#e41a1c", "#377eb8", "#984ea3", "#ff7f00"))+
  scale_color_manual(name="SCG (% of total mixture)",
                     labels = c("0", "15","30", "50"),
                     values = c("#e41a1c", "#377eb8", "#984ea3", "#ff7f00"))+
  stat_summary(aes(color=trmt), fun  = "mean", geom = "line" , size=0.6)+
  stat_summary(aes(fill=trmt), geom = "ribbon", fun.data = mean_se , alpha = 0.2)+
  geom_line(data = t5, aes(x=time, y=sub_temp, linetype=trmt), 
            size=0.55 )+
  scale_linetype_manual(name ="", labels= c("Ambient"),values=c("ambient"=2))+
  guides(linetype = guide_legend(order=2), col = guide_legend(order = 1), fill = guide_legend(order = 1))


fig_plug








#________________________________________________________________________________
#""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

# Making collage of peat and plug based temperature
figure_temp_substrate <- ggarrange(fig_peat, fig_plug, 
                                   labels = c("a", "b"),
                                   ncol = 2, nrow = 1, 
                                   common.legend = T, legend = "bottom")
# %>%
#   annotate_figure(top = text_grob("Substrate Temperature - Trial 3", 
#                                   color = "black", 
#                                   size = 12))

figure_temp_substrate




#Saving the plot
# Step 1: Call the pdf command to start the plot
pdf(file = "temp_sub.pdf",   # The directory you want to save the file in
    width = 10, # The width of the plot in inches
    height = 4.5)

figure_temp_substrate


dev.off()      


ggsave(file="temp_sub_trial3.svg", plot=figure_temp_substrate, width=10.69, height=4.4)






















#*********************************************************************************
#-------------------------------PEAT BASED----------------------------------------
#********************************************************************************


#Plotting individual temperature-time vessel-wise

p <- temp_substrate_peat %>%
  select(time, sub_temp1, sub_temp13, sub_temp14, sub_temp16) %>%                #Selecting the Vessels for the respective treatment
  mutate(time = as_datetime(paste0(time, "-01"))) %>%                           
  gather(trmt1, sub_temp, -time) %>%
  filter(complete.cases(.)) %>%
  ggplot(aes(x=time, y=sub_temp, color = trmt1)) +
  geom_line(alpha=0.8)+#ylim(17, 45)+
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
  ) +  labs(x="Time (days)", y="Temperature (°C)", color="15:1")+
  theme_classic()+
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        axis.line = element_line(size = 0.4))+
  theme(legend.position = c(0.85, 0.83),
        legend.direction = "vertical",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))+
  guides(color=guide_legend(override.aes=list(fill=NA)))+
  # scale_color_manual(labels = c("Vessel 13", "Vessel 6",
  #                               "Vessel 8", "Vessel 9"),
  #                    values = c("#e41a1c", "#377eb8", "#984ea3", "#ff7f00"))+
  stat_smooth( method = "gam",color="#000000", size=0.9, formula = y ~ s(x, bs = "ad"),
               se = T, na.rm=T, linetype = "dashed")
p


q <- temp_substrate_peat %>%
  select(time, sub_temp6, sub_temp8, sub_temp9, sub_temp11) %>%
  mutate(time = as_datetime(paste0(time, "-01"))) %>%
  gather(trmt2, sub_temp, -time) %>%
  filter(complete.cases(.)) %>%
  ggplot(aes(x=time, y=sub_temp, color = trmt2)) +
  geom_line( alpha=0.8)+#ylim(17, 45)+
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
  ) +  labs(x="Time (days)", y="Temperature (°C)", color="25:1")+
  theme_classic()+
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        axis.line = element_line(size = 0.4))+
  theme(legend.position = c(0.85, 0.83),
        legend.direction = "vertical",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))+
  guides(color=guide_legend(override.aes=list(fill=NA)))+
  # scale_color_manual(labels = c("Vessel 1", "Vessel 14",
  #                               "Vessel 4", "Vessel 5"),
  #                    values = c("#e41a1c", "#377eb8", "#984ea3", "#ff7f00"))+
  stat_smooth( method = "gam",color="#000000", size=0.9, formula = y ~ s(x, bs = "ad"),
               se = T, na.rm=T, linetype = "dashed")

q



  
r <- temp_substrate_peat %>%
  select(time, sub_temp2, sub_temp5, sub_temp12, sub_temp15) %>%
  mutate(time = as_datetime(paste0(time, "-01"))) %>%
  gather(trmt3, sub_temp, -time) %>%
  filter(complete.cases(.)) %>%
  ggplot(aes(x=time, y=sub_temp, color = trmt3)) +
  geom_line(alpha=0.8)+ylim(17, 45)+
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
  ) +  labs(x="Time (days)", y="Temperature (°C)", color="35:1")+
  theme_classic()+
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        axis.line = element_line(size = 0.4))+
  theme(legend.position = c(0.85, 0.83),
        legend.direction = "vertical",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))+
  guides(color=guide_legend(override.aes=list(fill=NA)))+
  # scale_color_manual(labels = c("Vessel 10", "Vessel 2",
  #                               "Vessel 3", "Vessel 7"),
  #                    values = c("#e41a1c", "#377eb8", "#984ea3", "#ff7f00"))+
  theme(legend.position = c(0.84, 0.79),
        legend.direction = "vertical")+
  stat_smooth( method = "gam",color="#000000", size=0.9, formula = y ~ s(x, bs = "ad"),
               se = T, na.rm=T, linetype = "dashed")
r




s <- temp_substrate_peat %>%
  select(time, sub_temp3, sub_temp4, sub_temp7, sub_temp10) %>%
  mutate(time = as_datetime(paste0(time, "-01"))) %>%
  gather(trmt4, sub_temp, -time) %>%
  filter(complete.cases(.)) %>%
  ggplot(aes(x=time, y=sub_temp, color = trmt4)) +
  geom_line(alpha=0.8)+ylim(17, 45)+
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
  ) +  labs(x="Time (days)", y="Temperature (°C)", color="48:1")+
  theme_classic()+
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        axis.line = element_line(size = 0.4))+
  theme(legend.position = c(0.85, 0.83),
        legend.direction = "vertical",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))+
  guides(color=guide_legend(override.aes=list(fill=NA)))+
  # scale_color_manual(labels = c("Vessel 11", "Vessel 12",
  #                               "Vessel 15", "Vessel 16"),
  #                    values = c("#e41a1c", "#377eb8", "#984ea3", "#ff7f00"))+
  stat_smooth( method = "gam",color="#000000", size=0.9, formula = y ~ s(x, bs = "ad"),
               se = T, na.rm=T, linetype = "dashed")
s


#Compiling the  four figures into one
figure_temp_substrate <- ggarrange(p, q, r, s,
                    labels = c("a", "b", "c", "d"),
                    ncol = 2, nrow = 2)
# %>%
#   annotate_figure(top = text_grob("Substrate Temperature - Trial 3", 
#                                   color = "black", 
#                                   size = 12))

figure_temp_substrate






#Saving the plot
# Step 1: Call the pdf command to start the plot
pdf(file = "temp_sub.pdf",   # The directory you want to save the file in
    width = 11.69, # The width of the plot in inches
    height = 8.27)

figure_temp_substrate


dev.off()      


ggsave(file="temp_sub_trial3.svg", plot=figure_temp_substrate, width=11.69, height=8.27)







#*********************************************************************************
#-------------------------------PLUG BASED----------------------------------------
#********************************************************************************

#Plotting individual temperature-time vessel-wise

p1 <- temp_substrate_plug %>%
  select(time, sub_temp7, sub_temp16, sub_temp10, sub_temp14) %>%                #Selecting the Vessels for the respective treatment
  mutate(time = as_datetime(paste0(time, "-01"))) %>%                           
  gather(trmt1, sub_temp, -time) %>%
  filter(complete.cases(.)) %>%
  ggplot(aes(x=time, y=sub_temp, color = trmt1)) +
  geom_line(alpha=0.8)+#ylim(17, 45)+
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
      as_datetime('2022-01-05 19:30:00')
    )
  ) + labs(x="Time (days)", y="Temperature (°C)", color="15:1")+
  theme_classic()+
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        axis.line = element_line(size = 0.4))+
  theme(legend.position = c(0.85, 0.83),
        legend.direction = "vertical",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))+
  guides(color=guide_legend(override.aes=list(fill=NA)))+
  # scale_color_manual(labels = c("Vessel 13", "Vessel 6",
  #                               "Vessel 8", "Vessel 9"),
  #                    values = c("#e41a1c", "#377eb8", "#984ea3", "#ff7f00"))+
  stat_smooth( method = "gam",color="#000000", size=0.9, formula = y ~ s(x, bs = "ad"),
               se = T, na.rm=T, linetype = "dashed")
p1


q1 <- temp_substrate_plug %>%
  select(time, sub_temp3, sub_temp13, sub_temp11, sub_temp1) %>%
  mutate(time = as_datetime(paste0(time, "-01"))) %>%
  gather(trmt2, sub_temp, -time) %>%
  filter(complete.cases(.)) %>%
  ggplot(aes(x=time, y=sub_temp, color = trmt2)) +
  geom_line( alpha=0.8)+#ylim(17, 45)+
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
      as_datetime('2022-01-05 19:30:00')
    )
  ) +  labs(x="Time (days)", y="Temperature (°C)", color="25:1")+
  theme_classic()+
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        axis.line = element_line(size = 0.4))+
  theme(legend.position = c(0.85, 0.83),
        legend.direction = "vertical",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))+
  guides(color=guide_legend(override.aes=list(fill=NA)))+
  # scale_color_manual(labels = c("Vessel 1", "Vessel 14",
  #                               "Vessel 4", "Vessel 5"),
  #                    values = c("#e41a1c", "#377eb8", "#984ea3", "#ff7f00"))+
  stat_smooth( method = "gam",color="#000000", size=0.9, formula = y ~ s(x, bs = "ad"),
               se = T, na.rm=T, linetype = "dashed")

q1




r1 <- temp_substrate_plug %>%
  select(time, sub_temp8, sub_temp9, sub_temp2, sub_temp4) %>%
  mutate(time = as_datetime(paste0(time, "-01"))) %>%
  gather(trmt3, sub_temp, -time) %>%
  filter(complete.cases(.)) %>%
  ggplot(aes(x=time, y=sub_temp, color = trmt3)) +
  geom_line(alpha=0.8)+#ylim(17, 45)+
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
      as_datetime('2022-01-05 19:30:00')
    )
  ) + labs(x="Time (days)", y="Temperature (°C)", color="35:1")+
  theme_classic()+
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        axis.line = element_line(size = 0.4))+
  theme(legend.position = c(0.85, 0.83),
        legend.direction = "vertical",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))+
  guides(color=guide_legend(override.aes=list(fill=NA)))+
  # scale_color_manual(labels = c("Vessel 10", "Vessel 2",
  #                               "Vessel 3", "Vessel 7"),
  #                    values = c("#e41a1c", "#377eb8", "#984ea3", "#ff7f00"))+
  theme(legend.position = c(0.84, 0.79),
        legend.direction = "vertical")+
  stat_smooth( method = "gam",color="#000000", size=0.9, formula = y ~ s(x, bs = "ad"),
               se = T, na.rm=T, linetype = "dashed")
r1




s1 <- temp_substrate_plug %>%
  select(time, sub_temp15, sub_temp5, sub_temp6, sub_temp12) %>%
  mutate(time = as_datetime(paste0(time, "-01"))) %>%
  gather(trmt4, sub_temp, -time) %>%
  filter(complete.cases(.)) %>%
  ggplot(aes(x=time, y=sub_temp, color = trmt4)) +
  geom_line(alpha=0.8)+#ylim(17, 45)+
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
      as_datetime('2022-01-05 19:30:00')
    )
  ) + labs(x="Time (days)", y="Temperature (°C)", color="48:1")+
  theme_classic()+
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        axis.line = element_line(size = 0.4))+
  theme(legend.position = c(0.85, 0.83),
        legend.direction = "vertical",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))+
  guides(color=guide_legend(override.aes=list(fill=NA)))+
  # scale_color_manual(labels = c("Vessel 11", "Vessel 12",
  #                               "Vessel 15", "Vessel 16"),
  #                    values = c("#e41a1c", "#377eb8", "#984ea3", "#ff7f00"))+
  stat_smooth( method = "gam",color="#000000", size=0.9, formula = y ~ s(x, bs = "ad"),
               se = T, na.rm=T, linetype = "dashed")
s1


#Compiling the  four figures into one
figure_temp_plug_comb <- ggarrange(p1, q1, r1, s1,
                                   labels = c("a", "b", "c", "d"),
                                   ncol = 2, nrow = 2)


figure_temp_plug_comb

