#INITIAL SETUP
##Load necessary packages
install.packages("librarian")
librarian::shelf(tidyverse, ggplot2, dplyr, tibble, readr, lubridate, ggpubr,
                 rstudioapi, Rmisc, reshape2, plyr, ggeasy, RColorBrewer,
                 update_all = FALSE)

##Setup working Directory
setwd(dirname(getActiveDocumentContext()$path))                                 # Set working directory to source file location




#********************************************************************************
#-------------------------------PEAT BASED---------------------------------------
#********************************************************************************


cfinal_peat <- read_csv("C_mineralization_peat.csv")%>%select(c(1:4))


cfinal_peat$trmt <- factor(cfinal_peat$trmt, levels = c("1", "2", "3", "4"))
head(cfinal_peat)



#____

cfinal_peat$co2_c_perc <- (cfinal_peat$cum_co2_c/cfinal_peat$initial_c)*100
# cfinal_peat$final_c_perc <- (cfinal_peat$final_c/cfinal_peat$initial_c)*100
# cfinal_peat$c_diff <- 100-(cfinal_peat$final_c_perc+cfinal_peat$co2_c_perc)


cfinal_peat <- cfinal_peat%>%slice(-c(3, 13, 8,9))


#Summary statistics


#ANOVA
model1 <- aov(co2_c_perc ~ trmt, data = cfinal_peat)
summary(model1)
anova(model1)
coefficients(model1)

#Tukeys test
library(agricolae)
HSD.test(model1, "trmt", group=T, alpha = 0.05, console = T)









# #Calculate means for both groups
# melted_peat <- melt(cfinal_peat, id.vars=c( "trmt"))
# 
# means_peat <- ddply(melted_peat, c("variable", "trmt"), summarise,
#                mean=mean(value))
# 
# means2_peat <- means_peat%>%slice(c(1:4,9:12))%>%
#   rename(c('mean'='value'))
# min_perc_peat <- means_peat%>%slice(c(13:16))%>%select(c(2:3))%>%
#   rename(c('mean'='perc'))%>%as_tibble()
# min_perc_peat$trmt <- as.factor(min_perc_peat$trmt)
# 
# 
# 
# 
# summary_peat <- summarySE(data=melted_peat, measurevar="value",
#                                   groupvars=c("variable", "trmt"), na.rm = T)%>%as_tibble()
# summary_peat




#Add tukeys comparison grouping


summary_minperc_peat <- summarySE(data=cfinal_peat, measurevar="co2_c_perc",
                             groupvars=c("trmt"), na.rm = T)%>%as_tibble()

summary_minperc_peat$tukey <- c("a", "b", "b", "b")




q <- ggplot(data=summary_minperc_peat, aes(x=trmt, y=co2_c_perc))+
  geom_bar(stat= "identity", width = 0.75, fill="#bdbdbd")+
  geom_point(data=cfinal_peat, stat = "identity", size=3, 
             #alpha=0.95, 
             color = "#e34a33",
             position = position_jitter(width = 0.2)
  )+
  geom_errorbar(aes(ymin=co2_c_perc-se, ymax=co2_c_perc+se), width=.2, 
                position = position_dodge2(width=0.85))+
  geom_text(aes(label=round(co2_c_perc, digits = 2)), 
            position=position_dodge(width=0.9), vjust=3.2, size=4)+
  geom_text(aes(label=tukey), nudge_y=3.86,  size=4)+
  scale_y_continuous(expand=c(0,0), limits = c(0, 20))+
  scale_x_discrete(labels = c("1" = "0 ", "2" = "15", "3" = "30", "4"="50"))+
  labs(title = "Mat-based biowaste",
       x = "SCG (% of total mixture)", y= "Carbon mineralization (% of initial carbon)")+ 
  theme_pubr()



q










# #Draw bar plot with ggplot2
# plot_peat <- ggplot(data=means2_peat, aes(x=trmt, y=value, fill=variable)) + 
#   geom_bar(stat="identity",
#            width = 0.9,
#            position = "fill",
#            color="white",
#            alpha = 0.75
#            ) + 
#   # geom_text(data = min_perc_peat, aes(x=trmt,y=100, label=round(perc, digits = 2)), 
#   #            position = "fill", vjust = 1.5,
#   #           inherit.aes = F)+ 
#   # geom_text(data=min_perc_peat, aes(label=tukey, x=trmt, y= 95), 
#   #           position = "fill", vjust = 3.5,
#   #           inherit.aes = F)+
#   ylab("Percentage of initial carbon") +
#   labs(title = "Mat-based biowaste")+
#   scale_fill_manual(name=NULL, 
#                     #values = c(brewer.pal(3, "Set2")),
#                     values = c("#ef8a62", "#67a9cf"),
#                     labels = c(expression(CO[2]-C), "Final C"))+
#   # scale_y_continuous(labels=c("0.00"="0", "0.25"="25","0.50"= "50", 
#   #                             "0.75"="75","1.00"= "100"), expand = c(0,0))+
#   scale_x_discrete(name="SCG (% of total mixture)",
#                    labels = c("0%", "15%","30%", "50%"))+
#   theme_pubr()+ 
#   theme(legend.position="right")+
#   easy_remove_x_axis(what = c("ticks", "line"), teach = FALSE)
#   
#   
# 
# plot_peat
# 
# 
# 
# 
# 







#********************************************************************************
#-------------------------------PLUG BASED---------------------------------------
#********************************************************************************

cfinal_plug <- read_csv("C_mineralization_plug.csv")%>%select(c(1:4))


cfinal_plug$trmt <- factor(cfinal_plug$trmt, levels = c("1", "2", "3", "4"))
head(cfinal_plug)



#____

cfinal_plug$co2_c_perc <- (cfinal_plug$cum_co2_c/cfinal_plug$initial_c)*100
# cfinal_plug$final_c_perc <- (cfinal_plug$final_c/cfinal_plug$initial_c)*100
# cfinal_plug$c_diff <- 100-(cfinal_plug$final_c_perc+cfinal_plug$co2_c_perc)


cfinal_plug <- cfinal_plug%>%slice(-c(3, 7, 10, 16))



#ANOVA
model2 <- aov(co2_c_perc ~ trmt, data = cfinal_plug)
summary(model2)
anova(model2)
coefficients(model2)

#Tukeys test
library(agricolae)
HSD.test(model2, "trmt", group=T, alpha = 0.05, console = T)









# #Calculate means for both groups
# melted_plug <- melt(cfinal_plug, id.vars=c( "trmt"))
# 
# means_plug <- ddply(melted_plug, c("variable", "trmt"), summarise,
#                     mean=mean(value))
# 
# means2_plug <- means_plug%>%slice(c(1:4,9:12))%>%
#   rename(c('mean'='value'))
# min_perc_plug <- means_plug%>%slice(c(13:16))%>%select(c(2:3))%>%
#   rename(c('mean'='perc'))%>%as_tibble()
# min_perc_plug$trmt <- as.factor(min_perc_plug$trmt)
# 



#Add tukeys comparison grouping



summary_minperc_plug <- summarySE(data=cfinal_plug, measurevar="co2_c_perc",
                                  groupvars=c("trmt"), na.rm = T)%>%as_tibble()


summary_minperc_plug$tukey <- c("a", "bc", "b", "ac")




p <- ggplot(data=summary_minperc_plug, aes(x=trmt, y=co2_c_perc))+
  geom_bar(stat= "identity", width = 0.75, fill="#bdbdbd")+
  geom_point(data=cfinal_plug, stat = "identity", size=3, 
             #alpha=0.95, 
             color = "#e34a33",
             position = position_jitter(width = 0.2)
  )+
  geom_errorbar(aes(ymin=co2_c_perc-se, ymax=co2_c_perc+se), width=.2, 
                position = position_dodge2(width=0.85))+
  geom_text(aes(label=round(co2_c_perc, digits = 2)), 
            position=position_dodge(width=0.9), vjust=5.5, size=4)+
  geom_text(aes(label=tukey), nudge_y=3.86,  size=4)+
  scale_y_continuous(expand=c(0,0), limits = c(0, 20))+
  scale_x_discrete(labels = c("1" = "0 ", "2" = "15", "3" = "30", "4"="50"))+
  labs(title = "Plug-based biowaste",
       x = "SCG (% of total mixture)", y= "Carbon mineralization (% of initial carbon)")+ 
  theme_pubr()



p












# #Draw bar plot with ggplot2
# plot_plug <- ggplot(data=means2_plug, aes(x=trmt, y=value, fill=variable)) + 
#   geom_bar(stat="identity",
#            width = 0.9,
#            position = "fill",
#            color="white",
#            alpha = 0.75
#   ) + 
#   geom_text(data = min_perc_plug, aes(x=trmt,y=100, label=round(perc, digits = 2)), 
#             position = "fill", vjust = 1.5,
#             inherit.aes = F)+ 
#   geom_text(data=min_perc_plug, aes(label=tukey, x=trmt, y= 95), 
#             position = "fill", vjust = 3.5,
#             inherit.aes = F)+
#   ylab("Percentage of initial carbon") +
#   labs(title = "Plug-based biowaste")+
#   scale_fill_manual(name=NULL, 
#                     #values = c(brewer.pal(3, "Set2")),
#                     values = c("#ef8a62", "#67a9cf"),
#                     labels = c(expression(CO[2]-C), "Final C"))+
#   scale_y_continuous(labels=c("0.00"="0", "0.25"="25","0.50"= "50", 
#                               "0.75"="75","1.00"= "100"), expand = c(0,0))+
#   scale_x_discrete(name="SCG (% of total mixture)",
#                    labels = c("0%", "15%","30%", "50%"))+
#   theme_pubr()+ 
#   theme(legend.position="right")+
#   easy_remove_x_axis(what = c("ticks", "line"), teach = FALSE)
# 
# 
# 
# plot_plug









#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Compiling figures
fig <- ggarrange(q, p, labels = c("a", "b"),ncol = 2, nrow = 1,
                 common.legend = T, legend = "bottom")




fig






ggsave(file="c_min_2.pdf", plot=fig, width=10, height=4.5)







