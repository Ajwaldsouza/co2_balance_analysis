#INITIAL SETUP
##Load necessary packages
install.packages("librarian")
librarian::shelf(tidyverse, ggplot2, dplyr, tibble, readr, lubridate, ggpubr, 
                 scales, tidyr, rstudioapi,readxl,Rmisc,ggeasy,scales, ggforce,
                 update_all = FALSE)


##Setup working Directory
setwd(dirname(getActiveDocumentContext()$path)) 






# CALCULATING CO2 REQUIRED
#*************************


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

# Peat based CO2 per DM: 1 kg DM produces 0.0362 kg CO2
# Crop residue produced in the system: 0.205 kg DM  
# CO2 produced from the total residue:
   peat =0.204*0.0362


   
# Plug based CO2 per DM: 1 kg DM produces 0.158 kg CO2
# Crop residue produced in the system: 0.205 kg DM  
# CO2 produced from the total residue:
   plug =0.204*0.158   
   
   
   
   
   
   

   
# Theoretical max based on 0.3 kg CO2/ kg DM  
   TMax =  0.204*0.3

# Making a dataset
    
    source <- c("co2total" , "peat", "plug", "theoretical")
    value <- c(CO2total, peat, plug, TMax)
    type <- c("demand", "supply", "supply", "supply")
    sd <- c(NA, 0.005, 0.017, NA)

    co2_dataset <- data.frame(source, value, type, sd)%>%as_tibble()

    head(co2_dataset)

    
    
    
    
    
    
    
    
    
    
    
# Plotting dataset
p <-    ggbarplot(data = co2_dataset, x= "source", y="value",
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
                                  "plug"= "Plug-based residue", "peat" = "Mat-based residue"))+
      scale_fill_manual(labels = c("demand" = "Required", "supply" = "Recoverable"),
                          # values = c("#ED2201", "#01468B"),
                          values = c("#67a9cf", "#999999")
                        )+
       easy_remove_y_axis(what = c("ticks", "line"), teach = FALSE)+
      labs(fill = "")
p
 
     
      
# Export figure
    
#Call the pdf command to start the plot
pdf(file = "co2_balance.pdf",   # The directory you want to save the file in
    width = 7, # The width of the plot in inches
    height = 4)

p

dev.off()                 



ggsave(file="co2_balance_2.pdf", plot=p, width=9, height=4)

  
      

      
      
 
    
    
    