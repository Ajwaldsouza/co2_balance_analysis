#INITIAL SETUP
##Load necessary packages
install.packages("librarian")
librarian::shelf(tidyverse, ggplot2, dplyr, tibble, readr, lubridate, ggpubr, 
                 scales, tidyr, rstudioapi,readxl,Rmisc,
                 update_all = FALSE)


##Setup working Directory
setwd(dirname(getActiveDocumentContext()$path))                                 # Set working directory to source file location


# Reading files

co2_cum_peat <- read_csv("co2_cum_g_peat.csv")%>%as_tibble()%>%
  select(c(2,3))%>%
  slice(c(1,2,4))%>%
  mutate(cum_co2_g = cum_co2_g/57.89)

co2_cum_plug <- read_csv("co2_cum_g_plug.csv")%>%as_tibble()%>%
  select(c(2,3))%>%
  slice(c(1,2,4))%>%
  mutate(cum_co2_g = cum_co2_g/47.7)



cum_co2 <- rbind(co2_cum_peat, co2_cum_plug)

cum_co2$cum_co2_g <- as.numeric(cum_co2$cum_co2_g)
cum_co2$trmt <- as.factor(cum_co2$trmt)





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






