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

forbes_q1a <- read.csv("forbes.csv", header = TRUE, colClasses = c(age = "numeric"), na.strings = "-", fileEncoding="UTF-8-BOM")

str(forbes_q1a) #Age is numeric (as specified), but rank and net_worth still need to be modified.

as_tibble(forbes_q1a) #Second check also shows that rank and net_worth are indexed as factors.

forbes_q1b <- as_tibble(forbes_q1a) #Let's put these data in a tibble anyway for tidy purposes.

#Let's then create a function for net_worth modification.

nwf <- function(nw) {
  v <- unlist(strsplit(str_sub(nw, 2), split = " ", fixed = TRUE))
  n <- as.double(v[1])
  x <- ifelse(v[2]=="B", n * 1000000000, n * 1000000)
  return(x)
}

#Then use that function to do modifications on net_worth (and at the same time modify rank).

forbes_q1c <- forbes_q1b %>%
  mutate(rank = as.integer(str_sub(rank, 2))) %>%
  rowwise() %>%
  mutate(net_worth = nwf(net_worth))

str(forbes_q1c) #Now rank has no hashtag; net_worth has no dollar sign or letter. Both are numeric.

# q2 ----------------------------------------------------------------------

#The bottom end of the list contains people with less than 1 Billion. 
#However, these numbers can influence the analysis results, 
#so remove everyone with a net_worth of less than 1 Billion from the data set.

message('Lowest net worth before filter: ', min(forbes_q1c$net_worth)) #This is below 1 B.

forbes_q2 <- filter(forbes_q1c, net_worth >= 1000000000)

message('Lowest net worth after filter: ', min(forbes_q2$net_worth)) #This is 1 B.

tail(forbes_q2, 10) #Additional check confirms that the filter was succesful.

# q3 ----------------------------------------------------------------------

#Plot the age against net_worth. Is there are pattern? 
#Does it help to plot age against log(net_worth) instead? 
#Add your answer as a comment to the code.

ggplot(forbes_q2, aes(x = age, y = net_worth)) + 
  geom_point(size = 0.8) + 
  theme_bw() +
  xlab("Age") +
  ylab("Net Worth") + 
  ggtitle("Scatter plot age against net worth") +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme(text = element_text(size = 10), axis.text.x = element_text(angle = 0, hjust = 1, size = 10)) +
  theme(text = element_text(size = 10), axis.text.y = element_text(angle = 0, hjust = 1, size = 10))

#Most sample pps have < 2 billion, so it is difficult to distinguish any pattern.

ggplot(forbes_q2, aes(x = age, y = log(net_worth))) + 
  geom_point(size = 0.8) + 
  theme_bw() +
  xlab("Age") +
  ylab("Net Worth") +
  ggtitle("Scatter plot age against log of net worth") +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme(text = element_text(size = 10), axis.text.x = element_text(angle = 0, hjust = 1, size = 10)) +
  theme(text = element_text(size = 10), axis.text.y = element_text(angle = 0, hjust = 1, size = 10))

#Already clearer when using a log, so yes, that helps. There seems to be a weak positive relationship.

#We can further examine this relationship by adding a line to the plot.

ggplot(forbes_q2, aes(x = age, y = log(net_worth))) +
  geom_point(size = 0.8) +
  geom_smooth(size = 0.7, se = FALSE, colour = 2) + 
  theme_bw() +
  xlab("Age") +
  ylab("Net Worth") +
  ggtitle("Scatter plot age against log of net worth") +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme(text = element_text(size = 10), axis.text.x = element_text(angle = 0, hjust = 1, size = 10)) +
  theme(text = element_text(size = 10), axis.text.y = element_text(angle = 0, hjust = 1, size = 10))

#This confirms that the relationship is not that strong, probably around r = .10.

#Let's check the exact correlation:

cor.test(forbes_q2$age, forbes_q2$net_worth) 

#Indeed the correlation is small: r = .09.

# q4 ----------------------------------------------------------------------

#Compute for each country the difference between the net worth of the highest and lowest person on the list. 
#Filter out countries with less than 6 persons on the list and sort by ascending difference.

forbes_q4 <- forbes_q2 %>%
  group_by(country) %>%
  filter(n() >= 6) %>%
  mutate(dif_nw = max(net_worth) - min(net_worth)) %>%
  arrange(dif_nw)

# q5 ----------------------------------------------------------------------

#Plot the differences per country with a bar chart. 
#Make sure the labels of the countries are readable.

forbes_q5 <- summarise(forbes_q4, dif_nw = dif_nw[1])

ggplot(forbes_q5) +
  geom_bar(mapping = aes(x = reorder(country, desc(country)), y = dif_nw/1e+09), stat = "identity") +
  xlab("Country") +
  ylab("Difference in net worth (Billion $)") +
  ggtitle("Difference highest vs. lowest Forbes members") +
  theme(plot.title = element_text(size = 8)) +
  theme(text = element_text(size = 8), axis.text.x = element_text(angle = 45, hjust = 1, size = 7)) +
  theme(text = element_text(size = 8), axis.text.y = element_text(angle = 0, hjust = 1, size = 5)) + 
  coord_flip()

# q6 ----------------------------------------------------------------------

#You sorted the countries on difference in the data, but the graph is not sorted like that.
#Update the plot so countries are shown sorted by difference and give proper axis labels.

ggplot(forbes_q5) +
  geom_bar(mapping = aes(x = reorder(country, dif_nw), y = dif_nw/1e+09), stat = "identity") +
  xlab("Country") +
  ylab("Difference in net worth (Billion $)") +
  ggtitle("Difference highest vs. lowest Forbes members") +
  theme(plot.title = element_text(size = 8)) +
  theme(text = element_text(size = 8), axis.text.x = element_text(angle = 45, hjust = 1, size = 7)) +
  theme(text = element_text(size = 8), axis.text.y = element_text(angle = 0, hjust = 1, size = 5)) + 
  coord_flip()

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