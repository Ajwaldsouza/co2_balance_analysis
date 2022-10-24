#INITIAL SETUP
##Load necessary packages
install.packages("librarian")
librarian::shelf(tidyverse, ggplot2, plyr, dplyr, tibble, readr, lubridate, ggpubr, 
                 scales, tidyr, rstudioapi,readxl,Rmisc, latex2exp, ggeasy,
                 update_all = FALSE)


##Setup working Directory
setwd(dirname(getActiveDocumentContext()$path))                                 # Set working directory to source file location


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CO2 EVOLUTION PATTERN
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Mat-based_______

#Reading the co2 .csv file
co2_grams_mat <- read_csv("CO2 data/co2_grams_mat.csv")


#Reducing the dataset by reducing sampling interval
groups1 <- cut(as.POSIXct(co2_grams_mat$time), breaks="30 min")
co2_mat_line <- plyr::ddply(co2_grams_mat, "groups1", tail, 1)[, -1]



line_mat <- co2_mat_line %>%
  select(time, co2_grams1, co2_grams13, co2_grams16) %>%
  mutate(time = as_datetime(paste0(time, "-01"))) %>%
  gather(vessel, co2_ppm, -time) %>%
  filter(complete.cases(.))%>%
  add_column(trmt = "mat")%>%
  mutate(co2_ppm = co2_ppm/57.89)


# Converting "grams CO2 per gram" DM TO  "grams CO2 per kg DM"
line_mat$co2_ppm <- line_mat$co2_ppm*1000





# PLUG_______

#Reading the co2 .csv file
co2_grams_plug <- read_csv("CO2 data/co2_grams_plug.csv")


#Reducing the dataset by reducing sampling interval
groups1 <- cut(as.POSIXct(co2_grams_plug$time), breaks="30 min")
co2_plug_line <- plyr::ddply(co2_grams_plug, "groups1", tail, 1)[, -1]



line_plug <- co2_plug_line %>%
  select(time, co2_grams7, co2_grams10,  co2_grams16) %>%
  mutate(time = as_datetime(paste0(time, "-01"))) %>%
  gather(vessel, co2_ppm, -time) %>%
  filter(complete.cases(.))%>%
  add_column(trmt = "plug")%>%
  mutate(co2_ppm = co2_ppm/47.7)


# Converting "grams CO2 per gram" DM TO  "grams CO2 per kg DM"
line_plug$co2_ppm <- line_plug$co2_ppm*1000




# Merging both treatments onto a single timeline 
line_plug_time <- line_plug%>%select(c(1))

line_mat_x <- line_mat%>%select(-c(1))%>%slice(c(1:2068))
line_mat_x <- cbind(line_plug_time, line_mat_x)




co2 <- rbind(line_mat_x, line_plug)


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





ggsave(file="Plots/co2_linegraph.pdf", plot=fig, width = 5.83, 
       height = 4.13)









#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# TOTAL CUMULATIVE CO2 EVOLVED
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# mat_______

#Reading the co2 .csv file
co2_grams_mat <- read_csv("CO2 data/co2_grams_mat.csv")
head(co2_grams_mat)


# Summing the column to get cumulative CO2 (grams)
co2_cum_mat <-
  co2_grams_mat %>% select(-c(1)) %>% 
  colSums(na.rm = T) %>% 
  as_tibble() %>%
  rename(cum_co2_g = value)%>%
  mutate(cum_co2_g = cum_co2_g/57.89) # Dividing cumulative CO2 by the initial dry matter



#Adding the treatments 
co2_cum_mat$trmt <- c("mat", "mat", "mat")%>%as.factor()




# PLUG________

#Reading the co2 .csv file
co2_grams_plug <- read_csv("CO2 data/co2_grams_plug.csv")
head(co2_grams_plug)


# Summing the column to get cumulative CO2 (grams)
co2_cum_plug <-
  co2_grams_plug %>% select(-c(1)) %>% 
  colSums(na.rm = T) %>% 
  as_tibble() %>%
  rename(cum_co2_g = value)%>%
  mutate(cum_co2_g = cum_co2_g/47.7) # Dividing cumulative CO2 by the initial dry matter



#Adding the treatments 
co2_cum_plug$trmt <- c("plug", "plug", "plug")%>%as.factor()







# Binding mat and plug datasets
cum_co2 <- rbind(co2_cum_mat, co2_cum_plug)

str(cum_co2)






#ANOVA
model1 <- lm(cum_co2_g ~ trmt, data = cum_co2)
model1
summary(model1)
anova(model1)





#Tukeys test
library(agricolae)
HSD.test(model1, "trmt", group=T, alpha = 0.05, console = T)



#generating summary data
summary <- summarySE(data=cum_co2, measurevar="cum_co2_g",
                             groupvars=c("trmt"), na.rm = T)%>%as_tibble()
summary
#add tukey comparison groups
summary$tukey <- c("a", "b")




p <- ggplot(data=summary, aes(x=trmt, y=cum_co2_g))+
  geom_bar(stat= "identity", width = 0.75, fill="#bdbdbd")+
  geom_point(data=cum_co2, stat = "identity", size=3, 
             #alpha=0.95, 
             color = "#e34a33",
            position = position_jitter(width = 0.2)
  )+
  geom_errorbar(aes(ymin=cum_co2_g-se, ymax=cum_co2_g+se), width=.2, 
                position = position_dodge2(width=0.85))+
  geom_text(aes(label=round(cum_co2_g, digits = 3)), 
            position=position_dodge(width=0.9), vjust=4, size=4)+
  geom_text(aes(label=tukey), nudge_y=0.02,  size=4)+
  scale_y_continuous(expand=c(0,0), limits = c(0, 0.2))+
  scale_x_discrete(labels=c("mat" = "Mat-based", "plug" = "Plug-based"))+
  labs(x = "CEA biowaste type", y= expression(Total~CO[2]~(kg)/Initial~dry~matter~(kg)))+ 
  theme_pubr()



p





#Saving the plot
# Call the pdf command to start the plot
pdf(file = "Plots/cum_co2.pdf",   # The directory you want to save the file in
    width = 5.83, # The width of the plot in inches
    height = 4.13)

p

dev.off()





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CO2 KINETIC MODELLING
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Mat-based________


#Reading the co2 .csv file
co2_grams_mat <- read_csv("CO2 data/co2_grams_mat.csv")%>%as_tibble()
#%>%mutate_at(c(2:17), ~ifelse(is.na(.), 0, .))
head(co2_grams_mat)



# # Making cumulative CO2 readings
co2_g_mat_cum <-co2_grams_mat%>%mutate_at(c(2:4), ~cumsum(.x), na.rm = TRUE)#%>%add_column(actual_co2, .before = 1)



#Reducing the dataset by reducing sampling interval
groups <- cut(as.POSIXct(co2_g_mat_cum$time), breaks="1 day")
library(plyr)
co2_g_mat_cum <- ddply(co2_g_mat_cum, "groups", tail, 1)[, -1]
###Through the code above, the dataset size is reduced by retaining the sampling
###of every 10 mins ("breaks= 10min"). Change the time interval as necessary.



# Add day numbers as a separate column
co2_g_mat_cum <- co2_g_mat_cum %>% dplyr::mutate(day = 0:15)


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





#Making individual treatment-wise datasets

t1 <- co2_g_mat_cum %>%
  select(time, day, co2_grams1, co2_grams13,  co2_grams16) %>%
  mutate(time = as_datetime(paste0(time, "-01"))) %>%
  gather(vessel, co2_per_kgdm, -time, -day) %>%
  filter(complete.cases(.))%>%
  add_column(trmt = "mat")%>%
  # mutate(c_min = (c_min/25.03)*27.27)   # calculating C mineralization; 25.03 is the total carbon (g) in the initial dry matter 
  mutate(co2_per_kgdm = ((co2_per_kgdm*1000)/57.9)/1000) #calculating the mass of CO2 released (kg) per kg dm









# Plug-based________


#Reading the co2 .csv file
co2_grams_plug <- read_csv("CO2 data/co2_grams_plug.csv")%>%as_tibble()%>%
  mutate_at(c(2:4), ~ifelse(is.na(.), 0, .))
head(co2_grams_plug)



# # Making cumulative CO2 readings
co2_g_plug_cum <-co2_grams_plug%>%mutate_at(c(2:4), ~cumsum(.x), na.rm = TRUE)#%>%add_column(actual_co2, .before = 1)



#Reducing the dataset by reducing sampling interval
groups <- cut(as.POSIXct(co2_g_plug_cum$time), breaks="1 day")
library(plyr)
co2_g_plug_cum <- ddply(co2_g_plug_cum, "groups", tail, 1)[, -1]
###Through the code above, the dataset size is reduced by retaining the sampling
###of every 10 mins ("breaks= 10min"). Change the time interval as necessary.



# Add day numbers as a separate column
co2_g_plug_cum <- co2_g_plug_cum %>% dplyr::mutate(day = 0:15)


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





#Making individual treatment-wise datasets

t2 <- co2_g_plug_cum %>%
  select(time, day, co2_grams7, co2_grams10,  co2_grams16) %>%
  mutate(time = as_datetime(paste0(time, "-01"))) %>%
  gather(vessel, co2_per_kgdm, -time, -day) %>%
  filter(complete.cases(.))%>%
  add_column(trmt = "plug")%>%
  # mutate(c_min = (c_min/21.1311)*27.27)   # calculating C mineralization; 25.03 is the total carbon (g) in the initial dry plugter 
  mutate(co2_per_kgdm = ((co2_per_kgdm*1000)/47.7)/1000) #calculating the mass of CO2 released (kg) per kg dm







# Modelling

t_co2 <- rbind(t1, t2)




fit1 <- nls(
  co2_per_kgdm ~ a*(1-exp((-k)*day)),
  data = t1,
  start = list(a = 0.05, k = 0.1),
  trace =  T
  ,
  control = list(maxiter = 1000, warnOnly= T , minFactor = 1/1024)
)
summary(fit1)




fit2 <- nls(
  co2_per_kgdm ~ a*(1-exp((-k)*day)),
  data = t2,
  start = list(a = 0.1, k = 0.1),
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



ggsave(file="Plots/c_min_model.pdf", plot=fig_e3, width=6, height=4 )













#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CO2 BALANCE ANALYSIS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# CALCULATING CO2 REQUIRED_____



# CO2s = CO2p + CO2v

# Where:
# CO2s = CO2 supply rate (kg hr–1) to maintain a fixed targeted CO2 concentration
# CO2p = rate of net CO2 fixed during photosynthesis by the total canopy (kg hr–1), 
# CO2v = rate of CO2 loss due to ventilation/air exchanges (kg hr–1). 


#________________



CO2p = 0.05*4.54*2*0.3211*3.66
# = Dry matter percent * Total fresh weight*2 m2 * 


#________________

# CO2v (kg CO2/ hour) = k * Nexch * Vsys * (PPMdes - PPMamb)/ 10^6


CO2v = (1.96 * 0.02 * 1 * (1000 - 415.68)/ 10^6)*16*16


CO2total =  CO2p+CO2v

CO2total





# Calculating the CO2 released from biomass based on respirometric data

# mat based CO2 per DM: 1 kg DM produces 0.0362 kg CO2
# Crop residue produced in the system: 0.205 kg DM  
# CO2 produced from the total residue:
mat =0.204*0.0362



# Plug based CO2 per DM: 1 kg DM produces 0.158 kg CO2
# Crop residue produced in the system: 0.205 kg DM  
# CO2 produced from the total residue:
plug =0.204*0.158   



# Theoretical max based on 0.3 kg CO2/ kg DM  
TMax =  0.204*0.3

# Making a dataset

source <- c("co2total" , "mat", "plug", "theoretical")
value <- c(CO2total, mat, plug, TMax)
type <- c("demand", "supply", "supply", "supply")
sd <- c(NA, 0.005, 0.017, NA)

co2_dataset <- data.frame(source, value, type, sd)%>%as_tibble()

head(co2_dataset)



# Plotting dataset
q <-    ggbarplot(data = co2_dataset, x= "source", y="value",
                  color = "white",
                  fill = "type",
                  width = 0.6,
                  # palette = c("#ED2201", "#01468B"),
                  sort.val = "asc",
                  sort.by.groups = F,     # Don't sort within each group
                  rotate = T,
                  label = T,
                  lab.nb.digits = 3,
                  lab.hjust	= -0.5,
                  lab.vjust = 0.4,
                  xlab = "",
                  ylab = expression(CO[2]~(kg)),
                  ggtheme = theme_pubr()
                  
)+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), size=0.5, width = 0.15 
  )+
  scale_y_continuous(limits = c(0, 0.65), breaks = c(0, 0.2, 0.4, 0.6 ),
                     expand =c(0,0),
                     labels = c("0.00"= "0",  "0.20" = "0.2", "0.40" = "0.4", "0.60" = "0.6"))+
  scale_x_discrete(labels = c("co2total" = expression(CO[2~Total]), "theoretical" = "Max. theoretical",
                              "plug"= "Plug-based residue", "mat" = "Mat-based residue"))+
  scale_fill_manual(labels = c("demand" = "Required", "supply" = "Recoverable"),
                    # values = c("#ED2201", "#01468B"),
                    values = c("#67a9cf", "#999999")
  )+
  easy_remove_y_axis(what = c("ticks", "line"), teach = FALSE)+
  labs(fill = "")
q



# Export figure

#Call the pdf command to start the plot
pdf(file = "Plots/co2_balance.pdf",   # The directory you want to save the file in
    width = 7, # The width of the plot in inches
    height = 4)

p

dev.off()                 






















