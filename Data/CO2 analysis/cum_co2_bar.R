#INITIAL SETUP
##Load necessary packages
install.packages("librarian")
librarian::shelf(tidyverse, ggplot2, dplyr, tibble, readr, lubridate, ggpubr, 
                 scales, tidyr, rstudioapi,readxl,Rmisc,
                 update_all = FALSE)


##Setup working Directory
setwd(dirname(getActiveDocumentContext()$path))                                 # Set working directory to source file location







# TOTAL CUMULATIVE CO2 EVOLVED


# PEAT_______

#Reading the co2 .csv file
co2_grams_peat <- read_csv("co2_grams_peat.csv")
head(co2_grams_peat)


# Summing the column to get cumulative CO2 (grams)
co2_cum_peat <-
  co2_grams_peat %>% select(-c(1)) %>% 
  colSums(na.rm = T) %>% 
  as_tibble() %>%
  rename(cum_co2_g = value)%>%
  mutate(cum_co2_g = cum_co2_g/57.89) # Dividing cumulative CO2 by the initial dry matter



#Adding the treatments 
co2_cum_peat$trmt <- c("peat", "peat", "peat")%>%as.factor()




# PLUG________

#Reading the co2 .csv file
co2_grams_plug <- read_csv("co2_grams_plug.csv")
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
cum_co2 <- rbind(co2_cum_peat, co2_cum_plug)

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
  scale_x_discrete(labels=c("peat" = "Mat-based", "plug" = "Plug-based"))+
  labs(x = "CEA biowaste type", y= expression(Total~CO[2]~(kg)/Initial~dry~matter~(kg)))+ 
  theme_pubr()



p





#Saving the plot
# Call the pdf command to start the plot
pdf(file = "cum_co2.pdf",   # The directory you want to save the file in
    width = 5.83, # The width of the plot in inches
    height = 4.13)

p

dev.off()






