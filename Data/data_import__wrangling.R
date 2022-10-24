#----IMPORTING DATASET AND INITIAL WRANGLING:-----------------------------------


#Install and load the necessary packages
install.packages("librarian")
librarian::shelf(plyr, dplyr, lubridate, tidyr, tibble, rstudioapi, 
                 update_all = FALSE)


# Setting working directory
setwd(dirname(getActiveDocumentContext()$path))                                 # Set working directory to source file location
mydir="Raw data"                                                                #Mention the directory (as "mydir") in the W.Dir. containing the datafiles
myfiles= list.files(path=mydir, pattern="*.csv", full.names=TRUE)               #List all the .csv files in the specified "mydir" and assign it to "myfiles". 
myfiles                                                                         #Shows all the files present 





#Importing datafiles into single dataframe
data_master <-
  ldply(myfiles,
        read.table,
        sep = " ",
        fill = T,
        header = F) %>%     #collectively apply "impost function for all the files in the specified directory
  as_tibble() %>%                                                                #Convert the imported data as a tibble
  select(select = -c(V2:V4, V10, V11, V17, V18, V24, V25,                       #Selecting only the relevant columns
                     V31, V32, V38, V39, V45, V46, V52, V53,
                     V59, V60, V66, V67, V73, V74, V80, V81,
                     V87, V88, V94, V95, V101, V102, V108, V109,
                     V115, V116))%>%
#Rename all the columns while importing. 
dplyr::rename(time=V1,                              
       sub_temp1=V5,co2_sample1=V6,scd30_temp1=V7,rh1=V8,wall_temp1=V9,
       sub_temp2=V12,co2_sample2=V13,scd30_temp2=V14,rh2=V15,wall_temp2=V16,
       sub_temp3=V19,co2_sample3=V20,scd30_temp3=V21,rh3=V22,wall_temp3=V23,
       sub_temp4=V26,co2_sample4=V27,scd30_temp4=V28,rh4=V29,wall_temp4=V30,
       sub_temp5=V33,co2_sample5=V34,scd30_temp5=V35,rh5=V36,wall_temp5=V37,
       sub_temp6=V40,co2_sample6=V41,scd30_temp6=V42,rh6=V43,wall_temp6=V44,
       sub_temp7=V47,co2_sample7=V48,scd30_temp7=V49,rh7=V50,wall_temp7=V51,
       sub_temp8=V54,co2_sample8=V55,scd30_temp8=V56,rh8=V57,wall_temp8=V58,
       sub_temp9=V61,co2_sample9=V62,scd30_temp9=V63,rh9=V64,wall_temp9=V65,
       sub_temp10=V68,co2_sample10=V69,scd30_temp10=V70,rh10=V71,wall_temp10=V72,
       sub_temp11=V75,co2_sample11=V76,scd30_temp11=V77,rh11=V78,wall_temp11=V79,
       sub_temp12=V82,co2_sample12=V83,scd30_temp12=V84,rh12=V85,wall_temp12=V86,
       sub_temp13=V89,co2_sample13=V90,scd30_temp13=V91,rh13=V92,wall_temp13=V93,
       sub_temp14=V96,co2_sample14=V97,scd30_temp14=V98,rh14=V99,wall_temp14=V100,
       sub_temp15=V103,co2_sample15=V104,scd30_temp15=V105,rh15=V106,wall_temp15=V107,
       sub_temp16=V110,co2_sample16=V111,scd30_temp16=V112,rh16=V113,wall_temp16=V114,
       sub_temp17=V117,co2_sample17=V118,scd30_temp17=V119,rh17=V120,wall_temp17=V121)

##Setting all the columns except 1st to as numeric
data_master[, 2:ncol(data_master)] <- lapply(data_master                       
                                             [,2:ncol(data_master)], 
                                             as.numeric)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~








#*********************************************************************************
#-------------------------------PEAT BASED----------------------------------------
#********************************************************************************


#Manipulating Time
##Setting first column as date-time variable
data_master$time <- as_datetime(data_master$time)                                  
#Setting the time boundaries of the dataset.
data_peat <- data_master %>%
  subset(
    time >= as_datetime('2021-12-01 19:30:00') &
      time <= as_datetime('2021-12-16 19:30:00')
  )


str(data_peat)

# #Exporting the master  as a .csv if necessary
# write.csv(data_peat,"data_master_peat.csv", row.names = FALSE)
# # The dataset exported above (data_raw) contains raw, unmangled sampled readings





#----CALCULATING "ACTUAL" CO2 (grams) (SAMPLED *minus* AMBIENT):---------------

#Making a dataset with only sampled CO2 from the raw data 
##The numbers are the order number of the columns pertaining to CO2 sampling.
dataco2_peat <- select(data_peat, c(1, 3, 8, 13, 18, 23, 28,
                                 33, 38, 43, 48, 53,
                                 58, 63, 68, 73, 78, 83))



##First, we need SCD30 temperature in Kelvin
convert_to_kelvin <- function(x, na.rm=FALSE) (x+273.15)
###The above line creates a function (i.e. x+273.15), which is the formula to
### convert C to K. This function is assigned as "convert_to_kelvin".

dataco2_peat <-  select(data_peat, c( 4, 9, 14, 19, 24, 29,
                                         34, 39, 44, 49, 54,
                                         59, 64, 69, 74, 79, 84))%>% 
  mutate_at(c(1:17),convert_to_kelvin, na.rm = TRUE)%>%                         
  add_column(dataco2_peat, .before = 1) 
###Above, the columns in the master file pertaining to temperature are selected,
###and then piped into mutate_at, assigning the "convert_to_kelvin" function.
### The the columns in dataco2_actual (ppm readings) are appended to the datafile
### where ".before=1" means the appended columns are added in the front (before 1st column of the active dataframe)
### written as a new datafile, i.e., dataco2_cum




#Calculating CO2-grams for each datapoint
{
  dataco2_peat$co2_grams1 <- ((dataco2_peat$co2_sample1/dataco2_peat$scd30_temp1)*0.00001116657826)                                    
  dataco2_peat$co2_grams2 <- ((dataco2_peat$co2_sample2/dataco2_peat$scd30_temp2)*0.00001116657826)
  dataco2_peat$co2_grams3 <- ((dataco2_peat$co2_sample3/dataco2_peat$scd30_temp3)*0.00001116657826)
  dataco2_peat$co2_grams4 <- ((dataco2_peat$co2_sample4/dataco2_peat$scd30_temp4)*0.00001116657826)
  dataco2_peat$co2_grams5 <- ((dataco2_peat$co2_sample5/dataco2_peat$scd30_temp5)*0.00001116657826)
  dataco2_peat$co2_grams6 <- ((dataco2_peat$co2_sample6/dataco2_peat$scd30_temp6)*0.00001116657826)
  dataco2_peat$co2_grams7 <- ((dataco2_peat$co2_sample7/dataco2_peat$scd30_temp7)*0.00001116657826)
  dataco2_peat$co2_grams8 <- ((dataco2_peat$co2_sample8/dataco2_peat$scd30_temp8)*0.00001116657826)
  dataco2_peat$co2_grams9 <- ((dataco2_peat$co2_sample9/dataco2_peat$scd30_temp9)*0.00001116657826)
  dataco2_peat$co2_grams10 <- ((dataco2_peat$co2_sample10/dataco2_peat$scd30_temp10)*0.00001116657826)
  dataco2_peat$co2_grams11 <- ((dataco2_peat$co2_sample11/dataco2_peat$scd30_temp11)*0.00001116657826)
  dataco2_peat$co2_grams12 <- ((dataco2_peat$co2_sample12/dataco2_peat$scd30_temp12)*0.00001116657826)
  dataco2_peat$co2_grams13 <- ((dataco2_peat$co2_sample13/dataco2_peat$scd30_temp13)*0.00001116657826)
  dataco2_peat$co2_grams14 <- ((dataco2_peat$co2_sample14/dataco2_peat$scd30_temp14)*0.00001116657826)
  dataco2_peat$co2_grams15 <- ((dataco2_peat$co2_sample15/dataco2_peat$scd30_temp15)*0.00001116657826)
  dataco2_peat$co2_grams16 <- ((dataco2_peat$co2_sample16/dataco2_peat$scd30_temp16)*0.00001116657826)
  dataco2_peat$co2_grams17 <- ((dataco2_peat$co2_sample17/dataco2_peat$scd30_temp17)*0.00001116657826)
  
  }


# NOTE_________
###The calculation above [co2 actual (ppm)/scd30 temp (K) * 0.00001116657826] is simplified
### version of the original equation used to calculate grams of CO2. The equation is given below

###        CO2 (grams) = (Cppm*F*t*44)/(R*T*10^6)

###             Where, 
###                   Cppm = actual CO2 concentration in ppm
###                   F = flow rate (L/min)
###                   t = sampling interval (min)
###                   44 = molar mass of CO2 
###                   R = ideal gas constant (0.082057366080960 L⋅atm⋅K^−1⋅mol^−1)
###                   T = temperature of gas at sampling point (Kelvin)
###                   10^6 = concentration conversion factor 

###   For the calculation in trial 1 above,
###   F = 0.25, t= 0.0833 min
#_______________



dataco2_grams_peat <- dataco2_peat%>% select(-c(2:35))


#Subtract Ambient from sampled values

#Making a dataset with "actual" CO2 readings (with ambient readings subtracted)
dataco2_grams_actual_peat <-  dataco2_grams_peat  %>%  mutate(dataco2_grams_peat[2:ncol(dataco2_grams_peat)] - co2_grams17)
###Here, mutate is assigned to "each" column to be subtracted from column 17 (ambient)
## and is written as new columns in new datafile (i.e., dataco2_actual)


#Converting all the negative values in the columns (in dataco2_actual) to zero.
##Assign the columns to be converted as "columns_to_convert".
columns_to_convert <- c(2:18)
#Mention the function: If negative>convert to 0; If not> let it be.
replace_with_zero <- function(x, na.rm=FALSE) (ifelse(x < 0, yes = 0, no = x))
##Choose the parent datafile (dataco2_actual) to be mutated at the columns assigned,
## as the predefined function. 
dataco2_grams_actual_peat <- dataco2_grams_actual_peat %>% mutate_at(columns_to_convert, 
                                               replace_with_zero, na.rm = TRUE)%>% 
  rename_with( ~ gsub("sample", "actual", .x, fixed = TRUE), .cols = c(2:18))    #Renaming to substitute "sample" in the column names to "actual".

colnames(dataco2_grams_actual_peat)
head(dataco2_grams_actual_peat)




# Selecting columns with only the peat treatment used for CO2 balance analysis
dataco2_grams_actual_peat <- dataco2_grams_actual_peat%>%
  select(c(1,2,14,17))





#Exporting the actual CO2 (g) datafile  as a .csv
write.csv(dataco2_grams_actual_peat,"CO2 data/co2_grams_peat.csv", row.names = FALSE)










#----TEMPERATURE DATA-----------------------------------------------------------

#SUBSTRATE TEMPERATURE
#Making a dataset with only substrate temperature from the master file 
##These numbers below are the order number of the columns pertaining to substrate temperature.
temp_substrate_peat <- select(data_peat, c(1, 2,  62, 77))
temp_substrate_peat



#Reducing the dataset by reducing sampling interval
# groups <- cut(as.POSIXct(temp_substrate$time), breaks="5 min")
# library(plyr)
# temp_substrate <- ddply(temp_substrate, "groups", tail, 1)[, -1]
###Through the code above, the dataset size is reduced by retaining the sampling
###of every 5 mins ("breaks= 5min"). Change the time interval as necessary.




#Exporting substrate temperature as .csv for further analyses
write.csv(temp_substrate_peat,"Temperature/temp_substrate_peat.csv", row.names = FALSE)





















#*********************************************************************************
#-------------------------------PLUG BASED----------------------------------------
#********************************************************************************


#Manipulating Time
##Setting first column as date-time variable
data_master$time <- as_datetime(data_master$time)                                  
#Setting the time boundaries of the dataset.
data_plug <- subset(data_master,
                    time >= as_datetime('2021-12-21 10:30:00') &
time <= as_datetime('2022-01-05 15:30:00'))


str(data_plug)

# #Exporting the master  as a .csv
# write.csv(data_plug,"data_master_plug.csv", row.names = FALSE)
# # The dataset exported above (data_raw) contains raw, unmangled sampled readings





#----CALCULATING "ACTUAL" CO2 (grams) (SAMPLED *minus* AMBIENT):---------------

#Making a dataset with only sampled CO2 from the raw data 
##The numbers are the order number of the columns pertaining to CO2 sampling.
dataco2_plug <- select(data_plug, c(1, 3, 8, 13, 18, 23, 28,
                                    33, 38, 43, 48, 53,
                                    58, 63, 68, 73, 78, 83))



#Reference CO2 stropped working for brief time; replacing NA with specific value "460"
dataco2_plug <- dataco2_plug%>%mutate_at(c(18), ~ifelse(is.na(.), 460, .))
dataco2_plug <- dataco2_plug%>%mutate_at(c(2:17), ~ifelse(is.na(.), 0, .))






##First, we need SCD30 temperature in Kelvin
convert_to_kelvin <- function(x, na.rm=FALSE) (x+273.15)
###The above line creates a function (i.e. x+273.15), which is the formula to
### convert C to K. This function is assigned as "convert_to_kelvin".

dataco2_plug <-  select(data_plug, c( 4, 9, 14, 19, 24, 29,
                                      34, 39, 44, 49, 54,
                                      59, 64, 69, 74, 79, 84))%>% 
  mutate_at(c(1:17),convert_to_kelvin, na.rm = TRUE)%>%                         
  add_column(dataco2_plug, .before = 1)%>%
  mutate_at(c(35), ~ifelse(is.na(.), 295, .))
###Above, the columns in the master file pertaining to temperature are selected,
###and then piped into mutate_at, assigning the "convert_to_kelvin" function.
### The the columns in dataco2_actual (ppm readings) are appended to the datafile
### where ".before=1" means the appended columns are added in the front (before 1st column of the active dataframe)
### written as a new datafile, i.e., dataco2_cum




#Calculating CO2-grams for each datapoint
{
  dataco2_plug$co2_grams1 <- ((dataco2_plug$co2_sample1/dataco2_plug$scd30_temp1)*0.00001116657826)                                    
  dataco2_plug$co2_grams2 <- ((dataco2_plug$co2_sample2/dataco2_plug$scd30_temp2)*0.00001116657826)
  dataco2_plug$co2_grams3 <- ((dataco2_plug$co2_sample3/dataco2_plug$scd30_temp3)*0.00001116657826)
  dataco2_plug$co2_grams4 <- ((dataco2_plug$co2_sample4/dataco2_plug$scd30_temp4)*0.00001116657826)
  dataco2_plug$co2_grams5 <- ((dataco2_plug$co2_sample5/dataco2_plug$scd30_temp5)*0.00001116657826)
  dataco2_plug$co2_grams6 <- ((dataco2_plug$co2_sample6/dataco2_plug$scd30_temp6)*0.00001116657826)
  dataco2_plug$co2_grams7 <- ((dataco2_plug$co2_sample7/dataco2_plug$scd30_temp7)*0.00001116657826)
  dataco2_plug$co2_grams8 <- ((dataco2_plug$co2_sample8/dataco2_plug$scd30_temp8)*0.00001116657826)
  dataco2_plug$co2_grams9 <- ((dataco2_plug$co2_sample9/dataco2_plug$scd30_temp9)*0.00001116657826)
  dataco2_plug$co2_grams10 <- ((dataco2_plug$co2_sample10/dataco2_plug$scd30_temp10)*0.00001116657826)
  dataco2_plug$co2_grams11 <- ((dataco2_plug$co2_sample11/dataco2_plug$scd30_temp11)*0.00001116657826)
  dataco2_plug$co2_grams12 <- ((dataco2_plug$co2_sample12/dataco2_plug$scd30_temp12)*0.00001116657826)
  dataco2_plug$co2_grams13 <- ((dataco2_plug$co2_sample13/dataco2_plug$scd30_temp13)*0.00001116657826)
  dataco2_plug$co2_grams14 <- ((dataco2_plug$co2_sample14/dataco2_plug$scd30_temp14)*0.00001116657826)
  dataco2_plug$co2_grams15 <- ((dataco2_plug$co2_sample15/dataco2_plug$scd30_temp15)*0.00001116657826)
  dataco2_plug$co2_grams16 <- ((dataco2_plug$co2_sample16/dataco2_plug$scd30_temp16)*0.00001116657826)
  dataco2_plug$co2_grams17 <- ((dataco2_plug$co2_sample17/dataco2_plug$scd30_temp17)*0.00001116657826)
  
  }


# NOTE_________
###The calculation above [co2 actual (ppm)/scd30 temp (K) * 0.00001116657826] is simplified
### version of the original equation used to calculate grams of CO2. The equation is given below

###        CO2 (grams) = (Cppm*F*t*44)/(R*T*10^6)

###             Where, 
###                   Cppm = actual CO2 concentration in ppm
###                   F = flow rate (L/min)
###                   t = sampling interval (min)
###                   44 = molar mass of CO2 
###                   R = ideal gas constant (0.082057366080960 L⋅atm⋅K^−1⋅mol^−1)
###                   T = temperature of gas at sampling point (Kelvin)
###                   10^6 = concentration conversion factor 

###   For the calculation in trial 1 above,
###   F = 0.25, t= 0.0833 min
#_______________



dataco2_grams_plug <- dataco2_plug%>% select(-c(2:35))






#Subtract Ambient from sampled values

#Making a dataset with "actual" CO2 readings (with ambient readings subtracted)
dataco2_grams_actual_plug <-  dataco2_grams_plug  %>%  mutate(dataco2_grams_plug[2:ncol(dataco2_grams_plug)] - co2_grams17)
###Here, mutate is assigned to "each" column to be subtracted from column 17 (ambient)
## and is written as new columns in new datafile (i.e., dataco2_actual)


#Converting all the negative values in the columns (in dataco2_actual) to zero.
##Assign the columns to be converted as "columns_to_convert".
columns_to_convert <- c(2:18)
#Mention the function: If negative>convert to 0; If not> let it be.
replace_with_zero <- function(x, na.rm=FALSE) (ifelse(x < 0, yes = 0, no = x))
##Choose the parent datafile (dataco2_actual) to be mutated at the columns assigned,
## as the predefined function. 
dataco2_grams_actual_plug <- dataco2_grams_actual_plug %>% mutate_at(columns_to_convert, 
                                                                     replace_with_zero, na.rm = TRUE)%>% 
  rename_with( ~ gsub("sample", "actual", .x, fixed = TRUE), .cols = c(2:18))    #Renaming to substitute "sample" in the column names to "actual".

colnames(dataco2_grams_actual_plug)
head(dataco2_grams_actual_plug)



# Selecting columns with only the peat treatment used for CO2 balance analysis
dataco2_grams_actual_plug <- dataco2_grams_actual_plug%>%
  select(c(1, 8, 11, 17))





#Exporting the actual CO2 (ppm) datafile  as a .csv
write.csv(dataco2_grams_actual_plug,"CO2 data/co2_grams_plug.csv", row.names = FALSE)













#----TEMPERATURE DATA-----------------------------------------------------------

#SUBSTRATE TEMPERATURE
#Making a dataset with only substrate temperature from the master file 
##These numbers below are the order number of the columns pertaining to substrate temperature.
temp_substrate_plug <- select(data_plug, c(1, 32, 47, 52, 77))
temp_substrate_plug





#Exporting substrate temperature as .csv for further analyses
write.csv(temp_substrate_plug,"Temperature/temp_substrate_plug.csv", row.names = FALSE)






