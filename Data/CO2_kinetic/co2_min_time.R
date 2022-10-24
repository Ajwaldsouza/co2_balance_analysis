#INITIAL SETUP
##Load necessary packages
install.packages("librarian")
librarian::shelf(tidyverse, ggplot2, dplyr, tibble, readr, lubridate, ggpubr, 
                 scales, tidyr, rstudioapi, car, latex2exp, Rmisc,
                 update_all = FALSE)


##Setup working Directory
setwd(dirname(getActiveDocumentContext()$path))                                 # Set working directory to source file location


#_______________________________________________________________________________
#                            PEAT
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#Reading the co2 .csv file
co2_g_peat <- read_csv("actual_co2_grams_peat.csv")%>%as_tibble()%>%
  select(c(1,2, 14, 17))
  #%>%mutate_at(c(2:17), ~ifelse(is.na(.), 0, .))
head(co2_g_peat)


# # Making cumulative CO2 readings
co2_g_peat_cum <-co2_g_peat%>%mutate_at(c(2:4), ~cumsum(.x), na.rm = TRUE)#%>%add_column(actual_co2, .before = 1)



#Manipulating Time
##Setting first column as date-time variable
# co2_g$time <- as_datetime(co2_g$time)                                  
# #Setting the time boundaries of the dataset.
# co2_g <- subset(co2_g,
#                          time >= as_datetime('2021-04-21 23:59:00')) #&
#time <= as_datetime('2021-04-23 15:30:00'))

#Reducing the dataset by reducing sampling interval
groups <- cut(as.POSIXct(co2_g_peat_cum$time), breaks="1 day")
library(plyr)
co2_g_peat_cum <- ddply(co2_g_peat_cum, "groups", tail, 1)[, -1]
###Through the code above, the dataset size is reduced by retaining the sampling
###of every 10 mins ("breaks= 10min"). Change the time interval as necessary.



# co2_g <- co2_g%>%slice(-c(1))



co2_g_peat_cum$day <- c(0,1,2,3,4,5,6,7, 8,9,10,11,12,13,14,15)



# Calculating CO2 Mineralization Percentage

# The equation for calculating the C mineralization percentage is :
#   
#         C min (%) =    gCO2
#                   ~~~~~~~~~~~~~~~~~~~~~~~~~ x 100
#                     gsub(%Csub/100) 44/12
# 
# Here, gsub(%Csub/100), is the mass of total organic carbon in the initial dry substrate (Cg).
# Simplyfying the equation and solving for numbers, the final equation that is used for calculation here is 

#         C min (%) =    gCO2
#                      ~~~~~~~~ x 27.27
#                         Cg


# 






#Making individual treatment-wise datasets

t1 <- co2_g_peat_cum %>%
  select(time, day, co2_grams1, co2_grams13,  co2_grams16) %>%
  mutate(time = as_datetime(paste0(time, "-01"))) %>%
  gather(vessel, co2_per_kgdm, -time, -day) %>%
  filter(complete.cases(.))%>%
  add_column(trmt = "peat")%>%
  # mutate(c_min = (c_min/25.03)*27.27)   # calculating C mineralization; 25.03 is the total carbon (g) in the initial dry matter 
  mutate(co2_per_kgdm = ((co2_per_kgdm*1000)/57.9)/1000) #calculating the mass of CO2 released (kg) per kg dm










#_______________________________________________________________________________
#                            PLUG
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#Reading the co2 .csv file
co2_g_plug <- read_csv("actual_co2_grams_plug.csv")%>%as_tibble()%>%
  select(c(1,8, 11, 17))%>%
  mutate_at(c(2:4), ~ifelse(is.na(.), 0, .))
head(co2_g_plug)


# # Making cumulative CO2 readings
co2_g_plug_cum <-co2_g_plug%>%mutate_at(c(2:4), ~cumsum(.x), na.rm = TRUE)#%>%add_column(actual_co2, .before = 1)



#Manipulating Time
##Setting first column as date-time variable
# co2_g$time <- as_datetime(co2_g$time)                                  
# #Setting the time boundaries of the dataset.
# co2_g <- subset(co2_g,
#                          time >= as_datetime('2021-04-21 23:59:00')) #&
#time <= as_datetime('2021-04-23 15:30:00'))

#Reducing the dataset by reducing sampling interval
groups <- cut(as.POSIXct(co2_g_plug_cum$time), breaks="1 day")
library(plyr)
co2_g_plug_cum <- ddply(co2_g_plug_cum, "groups", tail, 1)[, -1]
###Through the code above, the dataset size is reduced by retaining the sampling
###of every 10 mins ("breaks= 10min"). Change the time interval as necessary.



# co2_g <- co2_g%>%slice(-c(1))



co2_g_plug_cum$day <- c(0,1,2,3,4,5,6,7, 8,9,10,11,12,13,14,15)



# Calculating CO2 Mineralization Percentage

# The equation for calculating the C mineralization percentage is :
#   
#         C min (%) =    gCO2
#                   ~~~~~~~~~~~~~~~~~~~~~~~~~ x 100
#                     gsub(%Csub/100) 44/12
# 
# Here, gsub(%Csub/100), is the mass of total organic carbon in the initial dry substrate (Cg).
# Simplyfying the equation and solving for numbers, the final equation that is used for calculation here is 

#         C min (%) =    gCO2
#                      ~~~~~~~~ x 27.27
#                         Cg


# 






#Making individual treatment-wise datasets

t2 <- co2_g_plug_cum %>%
  select(time, day, co2_grams7, co2_grams10,  co2_grams16) %>%
  mutate(time = as_datetime(paste0(time, "-01"))) %>%
  gather(vessel, co2_per_kgdm, -time, -day) %>%
  filter(complete.cases(.))%>%
  add_column(trmt = "plug")%>%
  # mutate(c_min = (c_min/21.1311)*27.27)   # calculating C mineralization; 21.1311 is the total carbon (g) in the initial dry matter 
  mutate(co2_per_kgdm = ((co2_per_kgdm*1000)/47.7)/1000) #calculating the mass of CO2 released (kg) per kg dm


#_______________________________________________________________________________
#                            KINETIC MODELLING
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

t_co2 <- rbind(t1, t2)




fit1 <- nls(
  co2_per_kgdm ~ a*(1-exp((-k)*day)),
  data = t1,
  start = list(a = 10, k = 0.1),
  trace =  T
  ,
  control = list(maxiter = 1000, warnOnly= T , minFactor = 1/1024)
)
summary(fit1)





fit2 <- nls(
  co2_per_kgdm ~ a*(1-exp((-k)*day)),
  data = t2,
  start = list(a = 10, k = 0.1),
  trace =  T
  ,
  control = list(maxiter = 1000, warnOnly= T , minFactor = 1/1024)
)
summary(fit2)







# Plotting data with the generated data
fig_e3 <- ggplot(data=t_co2, aes(x=day, y=co2_per_kgdm, fill=trmt, color = trmt)) +
  geom_point(alpha = 0,
             position = position_jitter(width = 0.1, height = NULL))+
  stat_summary(geom = "errorbar",
               fun.data = "mean_se",
               size =0.35,
               width = 0.2)+
  stat_summary(geom = "point",
               fun.data = "mean_se",
               size =1.5)+
  scale_x_continuous(breaks = seq(0, 15, 2)) +
  scale_y_continuous(
    limits = c(0, 0.2),
    breaks = seq(0, 0.25, 0.05)) +
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
                    values = c("#e41a1c", "#377eb8", "#984ea3", "#ff7f00"),
                    guide = guide_legend())+
  scale_color_manual(name="",
                     labels = c("Mat-based", "Plug-based"),
                     values = c("#e41a1c", "#377eb8", "#984ea3", "#ff7f00"),
                     guide = guide_legend())+
  labs(x="Time (days)", y= expression(Cumulative~CO[2]~(kg)/Initial~dry~matter~(kg)))+
  stat_smooth(aes(color = trmt), method = 'nls', formula = 'y~a*(1-exp(-k*x))',
              method.args = list(start=c(a=10, k =0.1)), se=F, size = 0.75)+
  annotate(geom="text", x=1.5, y=0.2, label= TeX(r'($\textit{C_t= C_p(1-e^{-kt})}$)'))






fig_e3









plot(predict(fit2x),                                # Draw plot using Base R
     t2$co2_per_kgdm,
     xlab = "Predicted Values",
     ylab = "Observed Values")


cor(predict(fit1), t1$co2_per_kgdm)
cor(predict(fit2), t2$co2_per_kgdm)







ggsave(file="c_min_model.pdf", plot=fig_e3, width=6, height=4)














# BLACK AND WHITE



fig_e3_bw <-
  ggplot(data = t_co2, aes(
    x = day,
    y = co2_per_kgdm,
    shape = trmt
  )) +
  # geom_point(alpha = 0,
  #            position = position_jitter(width = 0.1, height = NULL)) +
  stat_summary(geom = "errorbar",
               fun.data = "mean_se",
               size =0.5,
               width = 0.075)+
  stat_summary(geom = "point",
               fun.data = "mean_se",
               size =1.5)+
  scale_x_continuous(breaks = seq(0, 15, 1)) +
  scale_y_continuous(
    limits = c(0, 12),
    breaks = seq(0, 12, 2)) +
  theme_pubr()+
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        axis.line = element_line(size = 0.4))+
  theme(#legend.position = c(0.8, 0.75),
    legend.direction = "horizontal",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.position="top")+
  scale_shape_manual(name="",
                     labels = c("15:1", "25:1","35:1", "48:1"),
                     values = c(1,2,3,4),
                     guide = guide_legend())+
  scale_linetype_manual(name="",
                        labels = c("15:1", "25:1","35:1", "48:1"),
                        values = c(1,2,3,4),
                        guide = guide_legend())+
  labs(x="Time (days)", y= "Total carbon mineralisation (%)")+
  stat_smooth(aes(linetype = trmt), method = 'nls', formula = 'y~a*(1-exp(-k*x))',
              method.args = list(start=c(a=10, k =0.1)), se=F, size = 0.75, color = "black")+
  annotate(geom="text", x=0.5, y=11, label= TeX(r'($\textit{C_t= kt^m}$)'))

fig_e3_bw












fig <- ggarrange(fig_e2_bw, fig_e3_bw, labels = c("a", "b"),ncol = 2, nrow = 1,
                 common.legend = T, legend = "bottom", align = "v")



ggsave(file="bw_c_min_model.pdf", plot=fig, width=10, height=4)
ggsave(file="bw_c_min_model.svg", plot=fig, width=10, height=4)
