library(tidyverse)
#Deliverable #1 
car_data<- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)
head(car_data)
lin_mod= lm(mpg~vehicle_length+vehicle_weight+spoiler_angle+ground_clearance+AWD,data = car_data)
summary(lin_mod)
#suspension_table <- read.table(file='Suspension_Coil.csv',sep=",",check.names=F,stringsAsFactors = F)
#head(suspension_table)
#a= summarise(suspension_table,mean=mean(PSI),.groups = 'keep')
#head(suspension_table,)

#suspension_table %>% group_by(V3) %>% summarize(mean=mean(V3))

#Deliverable #2

suspension_table <- read.csv(file='Suspension_Coil.csv')
PSI_summary = summarize(suspension_table,mean=mean(PSI),median=median(PSI),Variance=var(PSI),sd=sd(PSI))

lot_summary <- suspension_table %>% group_by(Manufacturing_Lot) %>% summarize(mean=mean(PSI),median=median(PSI),Variance=var(PSI),sd=sd(PSI), .groups = 'keep') #create summary table with multiple columns

>t.test(PSI_summary(sample_table$Miles_Driven),mu=mean(PSI_summary(population_table$Miles_Driven)))
t.test(log10(PSI_summary$PSI),mu=mean(log10(PSI_summary$PSI))) #compare sample versus population means

# Deliverable #3 Answer to question 1.
x = subset(suspension_table,Manufacturing_Lot=="Lot1" )
y = suspension_table["PSI"]
t.test(x["PSI"],mean(y))
t.test(x["PSI"],mu=1500)
t.test((suspension_table$PSI),mu=mean(PSI_summary$mean))

mean(PSI_summary$mean)


# Deliverable #3, Answer to question 2
t.test((suspension_table$PSI),mu=1500)
t.test(subset(suspension_table,Manufacturing_Lot=="Lot1")$PSI,mu=1500)
t.test(subset(suspension_table,Manufacturing_Lot=="Lot2")$PSI,mu=1500)
t.test(subset(suspension_table,Manufacturing_Lot=="Lot3")$PSI,mu=1500)