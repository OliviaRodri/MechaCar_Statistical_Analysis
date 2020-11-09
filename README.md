## Linear Regression to Predict MPG


![Images_here](Resources/deliv_1.PNG)

- The variables/coefficients that provided a non-random amount of variance to the mpg values in the dataset were the vehicle length and ground clearance.

- The slopes of the linear model  of vehicle weight and spoiler angle are nearly zero, because they do not impact the miles per gallon.


- This linear model does predict effectively, because it has a good R-squared value of 0.71.

## Summary Statistics on Suspension Coils

  The design specifications for the MechaCar suspension coils dictate that the variance of the suspension coils must not exceed 100 pounds per square inch (PSI). Does the current manufacturing data meet this design specification for all manufacturing lots in total and each lot individually? Why or why not?
   

- All manufacturing lots have a mean of 1499 with a variance of 62.3 which is less than 100, therefore meet the design specification.
 ![Images_here](Resources/suspension_coil.PNG)   
 
- Lot 1 and 2 have a mean of 1500 and a variance of 1.0 and 7.5 therefore they meet the design specification. Lot 3 has a mean of 1496 with a variance of 170.3 which is greater than 100, therefore does not meet the design specification.
![Images_here](Resources/lots.PNG)
## T-Tests on Suspension Coils
For all lots, the null hypothesis is rejected due to the low p-value of 6.0%. 
![Images_here](Resources/t_test_all_lots.PNG)
For Lot 1, the p-value is 1 which is a very high probability for the null hypothesis.
![Images_here](Resources/t_test_lot1.PNG)
For Lot 2, the p-value is 61% for the null hypothesis.
![Images_here](Resources/t_test_lot2.PNG)
For Lot 3, the null hypothesis is rejected because the p-value is very low at 4.2%.
![Images_here](Resources/t_test_lot3.PNG)

## Study Design: MechaCar vs Competition
Examples of metrics of interest to the consumer may be safety rating, cost, or fuel efficiency.

What metric or metrics are you going to test?
- The metric to test for this example is fuel efficiency.  

What is the null hypothesis or alternative hypothesis? 

- The null hypothesis would be that high fuel efficiency vehicles have a greater cost than the mean cost of all manufacturers.
  
What statistical test would you use to test the hypothesis? And why?
- First find the mean cost of all manufacturers. Select the vehicles that have a cost greater than the mean cost. Then perform a t-test of those vehicle's fuel efficiency against against all vehicle's fuel efficiency. If the hypothesis is true then a high p-value would indicate that high efficiency vehicles have a higher cost, which would support the null hypothesis. Then you can apply the same test by manufacturer to determine which vehicles have the lower cost.

What data is needed to run the statistical test?
- Fuel efficiency 
- Vehicle cost
- Manufacturer
  