#INITIAL SETUP
##Load necessary packages
install.packages("librarian")
librarian::shelf(tidyverse, ggplot2, dplyr, tibble, readr, lubridate, ggpubr, 
                 scales, tidyr, rstudioapi,readxl,Rmisc,
                 update_all = FALSE)


##Setup working Directory
setwd(dirname(getActiveDocumentContext()$path))                                 # Set working directory to source file location






#*********************************************************************************
#-------------------------------PEAT BASED----------------------------------------
#********************************************************************************


# Reading files

dm_peat <- read_xlsx("DM_loss_peat.xlsx", sheet = "means")%>%as_tibble()%>%select(c(11))
cum_co2_peat <- read_csv("co2_cum_g_peat.csv")%>%as_tibble()%>%select(-c(1,4,5))%>%slice(-c(17))
dm_peat <- read_csv("C_mineralization_peat.csv")%>%as_tibble()%>%select(c(5))




# Calculating CO2 evolved per unit dry matter for each treatment

## Making a column to add the dry weights in each vessel for the respective treatment
# cum_co2$dm <- c(57.89,57.89,57.89,57.89, 70.47,70.47, 70.47, 70.47,
#                   82.27,82.27, 82.27, 82.27, 94.42, 94.42, 94.42, 94.42)
# 

# 
# cum_co2$dm <- c(54.19,55.32,56.83,56.25, 63.50,62.99, 63.31, 63.94,
#                 69.95,70.96, 76.48, 73.65, 84.57, 86.49, 83.74, 88.16)




# ## Dividing cumulative CO2 per vessel by dry matter per vessel
# cum_co2$co2_per_dm <- cum_co2$cum_co2_g/cum_co2$dm







dataset_peat <- cbind(cum_co2_peat, dm_peat)
dataset_peat$trmt <- as.factor(dataset_peat$trmt)

 dataset_peat_out <- dataset_peat%>%slice(-c(3, 8, 9, 13))


#generating summary data
summary_cum_co2_peat <- summarySE(data=dataset_peat_out, measurevar="cum_co2_g",
                             groupvars=c("trmt"), na.rm = T)%>%as_tibble()%>%dplyr::rename(
                               sd_co2 = sd,
                               se_co2 = se,
                               ci_co2 = ci
                             )

summary_dm_peat <- summarySE(data=dataset_peat_out, measurevar="c_loss",
                        groupvars=c("trmt"), na.rm = T)%>%as_tibble()%>%dplyr::rename(
                          sd_dm = sd,
                          se_dm = se,
                          ci_dm = ci
                        )


# Merge summary datafiles

summary_peat <- cbind(summary_cum_co2_peat, summary_dm_peat)%>%select(-c(2,7,8))







p_peat <- ggplot(data=dataset_peat_out, aes(x=c_loss, y=cum_co2_g))+
  stat_smooth(formula = y~x, method = "lm", alpha= 0.3, color= "#0571b0")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  # stat_cor(method = "pearson")+
  geom_point(aes(shape = trmt), size=2)+
  geom_point(data = summary_peat, aes(x=c_loss, y=cum_co2_g, shape = trmt), color="#ca0020", size=3.5)+
  geom_errorbar(data = summary_peat, aes(xmin=c_loss-se_dm, xmax=c_loss+se_dm), color="#ca0020", width=0.5)+
  geom_errorbar(data = summary_peat, aes(ymin=cum_co2_g-se_co2, ymax=cum_co2_g+se_co2), color="#ca0020", width=0.25)+
  scale_shape_manual(name = "SCG (% of total mixture)", 
    values=c(15, 16, 17, 8), 
    labels = c("0%", "15%","30%", "50%"))+
  # scale_y_continuous(limits = c(0,21), breaks=seq(0, 25,5))+
  # scale_x_continuous(limits = c(0,21), breaks=seq(0, 20,5))+
  labs(title = "Mat-based biowaste",x = "TOC loss (grams)", y=expression(Cumulative~CO[2]~(grams)))+
  theme_pubr() 

p_peat





#Saving the plot
# Call the pdf command to start the plot
pdf(file = "dm_loss_vs_co2_peat.pdf",   # The directory you want to save the file in
    width = 5.83, # The width of the plot in inches
    height = 4.13)

p_peat

dev.off()


ggsave(file="dm_loss_vs_co2_peat.svg", plot=p, width=5.83, height=4.13)



#*********************************************************************************
#-------------------------------PLUG BASED----------------------------------------
#********************************************************************************

# Reading files

# dm_plug <- read_xlsx("DM_loss_plug.xlsx", sheet = "means")%>%as_tibble()%>%select(c(11))
cum_co2_plug <- read_csv("co2_cum_g_plug.csv")%>%as_tibble()%>%slice(-c(17))
dm_plug <- read_csv("C_mineralization_plug.csv")%>%as_tibble()%>%select(c(5))



# Calculating CO2 evolved per unit dry matter for each treatment

## Making a column to add the dry weights in each vessel for the respective treatment
# cum_co2$dm <- c(57.89,57.89,57.89,57.89, 70.47,70.47, 70.47, 70.47,
#                   82.27,82.27, 82.27, 82.27, 94.42, 94.42, 94.42, 94.42)
# 

# 
# cum_co2$dm <- c(54.19,55.32,56.83,56.25, 63.50,62.99, 63.31, 63.94,
#                 69.95,70.96, 76.48, 73.65, 84.57, 86.49, 83.74, 88.16)




# ## Dividing cumulative CO2 per vessel by dry matter per vessel
# cum_co2$co2_per_dm <- cum_co2$cum_co2_g/cum_co2$dm







dataset_plug <- cbind(cum_co2_plug, dm_plug)
dataset_plug$trmt <- as.factor(dataset_plug$trmt)

dataset_plug_out <- dataset_plug%>%slice(-c(3, 7, 10, 16))


#generating summary data
summary_cum_co2_plug <- summarySE(data=dataset_plug_out, measurevar="cum_co2_g",
                                  groupvars=c("trmt"), na.rm = T)%>%as_tibble()%>%dplyr::rename(
                                    sd_co2 = sd,
                                    se_co2 = se,
                                    ci_co2 = ci
                                  )

summary_dm_plug <- summarySE(data=dataset_plug_out, measurevar="c_loss",
                             groupvars=c("trmt"), na.rm = T)%>%as_tibble()%>%dplyr::rename(
                               sd_dm = sd,
                               se_dm = se,
                               ci_dm = ci
                             )


# Merge summary datafiles

summary_plug <- cbind(summary_cum_co2_plug, summary_dm_plug)%>%select(-c(2,7,8))







p_plug <- ggplot(data=dataset_plug_out, aes(x=c_loss, y=cum_co2_g))+
  stat_smooth(formula = y~x, method = "lm", alpha= 0.3, color= "#0571b0")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  # stat_cor(method = "pearson")+
  geom_point(aes(shape = trmt), size=2)+
  geom_point(data = summary_plug, aes(x=c_loss, y=cum_co2_g, shape = trmt), color="#ca0020", size=3.5)+
  geom_errorbar(data = summary_plug, aes(xmin=c_loss-se_dm, xmax=c_loss+se_dm), color="#ca0020", width=0.5)+
  geom_errorbar(data = summary_plug, aes(ymin=cum_co2_g-se_co2, ymax=cum_co2_g+se_co2), color="#ca0020", width=0.25)+
  scale_shape_manual(name = "SCG (% of total mixture)", 
                     values=c(15, 16, 17, 8), 
                     labels = c("0%", "15%","30%", "50%"))+
  # scale_y_continuous(limits = c(0,21), breaks=seq(0, 25,5))+
  # scale_x_continuous(limits = c(0,21), breaks=seq(0, 20,5))+
  labs(title = "Plug-based biowaste",
       x = "TOC loss (grams)", y=expression(Cumulative~CO[2]~(grams)))+
  theme_pubr() 

p_plug






















#_______________________________________________________________________________
## Compiling figures
fig <- ggarrange(p_peat, p_plug, labels = c("a", "b"),ncol = 2, nrow = 1,
                 common.legend = T, legend = "bottom")

fig




ggsave(file="toc_loss_vs_co2.pdf", plot=fig, width=10, height=4.5)







