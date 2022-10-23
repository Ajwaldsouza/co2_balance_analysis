#INITIAL SETUP
##Load necessary packages
install.packages("librarian")
librarian::shelf(tidyverse, ggplot2, dplyr, tibble, readr, lubridate, ggpubr, 
                 scales, tidyr, rstudioapi,readxl,Rmisc,
                 update_all = FALSE)


##Setup working Directory
setwd(dirname(getActiveDocumentContext()$path))                                 # Set working directory to source file location



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#*********************************************************************************
#-------------------------------PEAT BASED----------------------------------------
#********************************************************************************




# Reading files

dm_peat <- read_xlsx("DM_loss_peat.xlsx", sheet="means")%>%as_tibble()%>%select(c(2,10))
# dm_peat$trmt <- as.factor(dm_peat$trmt)
dm_peat$dm_loss_perc <- as.numeric(dm_peat$dm_loss_perc)



dm_peat$scg_perc <- c(0,0,0,0, 15, 15, 15, 15, 30, 30 ,30, 30, 50, 50, 50, 50)
 dm_peat$scg_perc <- as.factor(dm_peat$scg_perc)



#ANOVA
model1 <- lm(dm_loss_perc ~ scg_perc, data = dm_peat)
model1
summary(model1)
anova(model1)
coefficients(model1)





#Tukeys test
library(agricolae)
HSD.test(model1, "scg_perc", group=T, alpha = 0.05, console = T)



#generating summary data


summary_dm_peat <- summarySE(data=dm_peat, measurevar="dm_loss_perc",
                             groupvars=c("scg_perc"), na.rm = T)%>%as_tibble()
#add tukey comparison groups
summary_dm_peat$tukey <- c("a", "b", "b", "b")






p <- ggplot(data=summary_dm_peat, aes(x=scg_perc, y=dm_loss_perc))+
  geom_bar(stat= "identity", width = 0.75, fill="#bdbdbd")+
  geom_point(data=dm_peat, stat = "identity", size=3, 
             #alpha=0.95, 
             color = "#e34a33",
            position = position_jitter(width = 0.2)
  )+
  geom_errorbar(aes(ymin=dm_loss_perc-se, ymax=dm_loss_perc+se), width=.2, 
                position = position_dodge2(width=0.85))+
  geom_text(aes(label=round(dm_loss_perc, digits = 2)), 
            position=position_dodge(width=0.9), vjust=4, size=4)+
  geom_text(aes(label=tukey), nudge_y=2.5,  size=4)+
  scale_y_continuous(expand=c(0,0), limits = c(0, 30))+
  labs(title = "Mat-based biowaste",
       x = "SCG (% of total mixture)", y= "DM loss (%)")+ 
  theme_pubr()



p





#Saving the plot
# Call the pdf command to start the plot
pdf(file = "dm_loss.pdf",   # The directory you want to save the file in
    width = 5.83, # The width of the plot in inches
    height = 4.13)

p

dev.off()


ggsave(file="dm_loss_vs_co2.svg", plot=p, width=5.83, height=4.13)




#*********************************************************************************
#-------------------------------PLUG BASED----------------------------------------
#********************************************************************************



dm_plug <- read_xlsx("DM_loss_plug.xlsx", sheet="means (2)")%>%as_tibble()%>%select(c(2,10))
# dm_plug$trmt <- as.factor(dm_plug$trmt)
dm_plug$dm_loss_perc <- as.numeric(dm_plug$dm_loss_perc)



dm_plug$scg_perc <- c(0,0,0,0, 15, 15, 15, 15, 30, 30 ,30, 30, 50, 50, 50, 50)
dm_plug$scg_perc <- as.factor(dm_plug$scg_perc)






#ANOVA
model2 <- lm(dm_loss_perc ~ scg_perc, data = dm_plug)
model2
summary(model2)
anova(model2)


plot(model2)




#Tukeys test
library(agricolae)
HSD.test(model2, "scg_perc", group=T, alpha = 0.05, console = T)


# dm_plug_out <- dm_plug%>% slice(-c(16))
# model3 <- lm(dm_loss_perc ~ scg_perc, data = dm_plug_out)
# anova(model3)
# 
# HSD.test(model3, "scg_perc", group=T, alpha = 0.05, console = T)
# 

#generating summary data


summary_dm_plug <- summarySE(data=dm_plug, measurevar="dm_loss_perc",
                             groupvars=c("scg_perc"), na.rm = T)%>%as_tibble()
#add tukey comparison groups
summary_dm_plug$tukey <- c("a", "ab", "b", "ab")






q <- ggplot(data=summary_dm_plug, aes(x=scg_perc, y=dm_loss_perc))+
  geom_bar(stat= "identity", width = 0.75, fill="#bdbdbd")+
  geom_point(data=dm_plug, stat = "identity", size=3, 
             #alpha=0.95, 
             color = "#e34a33",
             position = position_jitter(width = 0.2)
  )+
  geom_errorbar(aes(ymin=dm_loss_perc-se, ymax=dm_loss_perc+se), width=.2, 
                position = position_dodge2(width=0.85))+
  geom_text(aes(label=round(dm_loss_perc, digits = 2)), 
            position=position_dodge(width=0.9), vjust=5.5, size=4)+
  geom_text(aes(label=tukey), nudge_y=3.86,  size=4)+
  scale_y_continuous(expand=c(0,0), limits = c(0, 30))+
  labs(title = "Plug-based biowaste",
       x = "SCG (% of total mixture)", y= "DM loss (%)")+ 
  theme_pubr()



q











# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Compiling figures
fig <- ggarrange(p, q, labels = c("a", "b"),ncol = 2, nrow = 1)




fig



ggsave(file="dm_loss_perc.pdf", plot=fig, width=10, height=4.5)
