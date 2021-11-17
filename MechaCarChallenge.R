# Import libraries
library(dplyr)

# Import and read in the MechaCar_mpg.csv 
MechaCar_table <- read.csv(file='MechaCar_mpg.csv', check.names=F, stringsAsFactors=F)

# Perform linear regression
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=MechaCar_table)

# Determine the p-value and the r-squared value for the linear regression model
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=MechaCar_table))

# Summary Statistics on Suspension Coils

# Import and read in the Suspension_Coil.csv as a table

Suspension_Coil_table <- read.csv(file='Suspension_Coil.csv', check.names=F, stringsAsFactors=F)

# Summary

total_summary = Suspension_Coil_table %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI))

# Lot Summary Suspension coil

lot_summary = Suspension_Coil_table %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI))

## T-Tests on Suspension Coils

# t-test across all lots
t.test(Suspension_Coil_table$PSI, mu=1500)

# t-test for Lot1
t.test(subset(Suspension_Coil_table$PSI, Suspension_Coil_table$Manufacturing_Lot == 'Lot1'), mu=1500)

# t-test for Lot2
t.test(subset(Suspension_Coil_table$PSI, Suspension_Coil_table$Manufacturing_Lot == 'Lot2'), mu=1500)

# t-test for Lot3
t.test(subset(Suspension_Coil_table$PSI, Suspension_Coil_table$Manufacturing_Lot == 'Lot3'), mu=1500)
