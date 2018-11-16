#assignment1

rm(list = ls())

library(dplyr)
library(ggplot2)
library(lintr)
library(rworldmap)
library(tibble)
library(tidyverse)

# q1 ----------------------------------------------------------------------

#Read the forbes.csv data file and make sure you assign appropriate column types 
#(in particular, you need rank, net_worth, and age to be numeric and not character)

# q2 ----------------------------------------------------------------------

#The bottom end of the list contains people with less than 1 Billion. 
#However, these numbers can influence the analysis results, 
#so remove everyone with a net_worth of less than 1 Billion from the data set.

# q3 ----------------------------------------------------------------------

#Plot the age against net_worth. Is there are pattern? 
#Does it help to plot age against log(net_worth) instead? 
#Add your answer as a comment to the code.

# q4 ----------------------------------------------------------------------

#Compute for each country the difference between the net worth of the highest and lowest person on the list. 
#Filter out countries with less than 6 persons on the list and sort by ascending difference.

# q5 ----------------------------------------------------------------------

#Plot the differences per country with a bar chart. 
#Make sure the labels of the countries are readable.

# q6 ----------------------------------------------------------------------

#You sorted the countries on difference in the data, but the graph is not sorted like that.
#Update the plot so countries are shown sorted by difference and give proper axis labels.

# q7 ----------------------------------------------------------------------

#Are there persons with the same rank in this list? If so, which ranks are shared?

# q8 ----------------------------------------------------------------------

#The ranks are computed as min_rank. 
#Compute for each original rank also the average rank. 
#So, if 4 people have rank 7, they are actually at rank 7,8,9, and 10, so their average rank is 8.5. 
#Your output should contain one column with the original ranks and another column with the newly computed average_rank values.

# q9 ----------------------------------------------------------------------

#Plot the sum of net_worth per country on the world map. 
#Use a logarithmic scale for clarity.