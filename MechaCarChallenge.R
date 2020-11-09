library(tidyverse)

#Deliverable #1 
car_data<- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)
head(car_data)
lin_mod= lm(mpg~vehicle_length+vehicle_weight+spoiler_angle+ground_clearance+AWD,data = car_data)
summary(lin_mod)

#Deliverable #2

suspension_table <- read.csv(file='Suspension_Coil.csv')
total_summary = summarize(suspension_table,mean=mean(PSI),median=median(PSI),Variance=var(PSI),sd=sd(PSI))

lot_summary <- suspension_table %>% group_by(Manufacturing_Lot) %>% summarize(mean=mean(PSI),median=median(PSI),Variance=var(PSI),sd=sd(PSI), .groups = 'keep') 


#  Deliverable #3
t.test((suspension_table$PSI),mu=1500)
t.test(subset(suspension_table,Manufacturing_Lot=="Lot1")$PSI,mu=1500)
t.test(subset(suspension_table,Manufacturing_Lot=="Lot2")$PSI,mu=1500)
t.test(subset(suspension_table,Manufacturing_Lot=="Lot3")$PSI,mu=1500)