
#This solution presents a very powerful approach for customer analytics to address customer segmentation as well as customer's lifecycle with the use of RFM analysis for behavior customer mining. The approach focuses on frequency and recent transactions, because frequency affects customer's lifetime value and recency affects retention. As a result, the solution will help gain clarity and understand different phases of the customer's lifecycle. 
#This information helps to know each customer's phase and also split customer base into groups (segments) in transaction to:
# .	understand the state of affairs,
#.	effectively use of marketing budget through accurate targeting,
#.	use different offers for every group,
#.	effectively using other marketing channels,
#.	increase customers' life-time and value.
#Life-Time Matrix which is a matrix is used to address the customer segmentation and lifecycle.
#Processing initial data (transaction) for the matrix and code for visualization using the R programming language.

#Let's create a data sample with the following code:

# loading libraries

library(dplyr)
library(reshape2)
library(ggplot2)

# creating data sample
set.seed(10)
data <- data.frame(transactionId=sample(c(1:10000), 8000000, replace=TRUE), product=sample(c('NULL','a','b','c'), 8000000, replace=TRUE, prob=c(0.15, 0.65, 0.3, 0.15)))
transaction <- data.frame(transactionId=c(1:10000), customerId=sample(c(1:3000), 10000, replace=TRUE))
gender <- data.frame(customerId=c(1:3000),
                     gender=sample(c('male', 'female'), 3000, replace=TRUE, prob=c(0.40, 0.60)))
date <- data.frame(transactionId=c(1:10000), transactiondate=sample((1:1000), 10000, replace=TRUE))
transactions <- merge(data, transaction, by='transactionId')
transactions <- merge(transactions, gender, by='customerId')
transactions <- merge(transactions, date, by='transactionId')
transactions <- transactions[transactions$product!='NULL', ]
transactions$transactiondate <- as.Date(transactions$transactiondate, origin="2015-01-01")
rm(data, date, transaction, gender)


#You can see that there is a gender of customer in the table. We will use it as an example of some in-depth analysis later. 
#It should be noted that additional features of customer information such as transaction location, transaction channel, Promotion subscription, and so on and be included to address any insight of interest. 
#A few words about Life-Time Matrix. It is a matrix with 2 dimensions:
# .	frequency, which is the volume or number of transactions,
#.	recency, which is the number of days before the most recent transaction.
#For customer analytics, a fundamental strategy is to segment customers into different categories. In this case, the data above will be defined by some boundaries of frequency and recency, which should help to split customers into homogeneous groups (segments). It should be noted that the distribution of frequency and recency of transactions procide some respite for customer segmentation but knowledge of business of interest can contribute to setting the suitable boundaries.
#The plot for the frequency and recency of the generated data is done with the following code:
 
 # reporting date
  today <- as.Date('2017-03-24', format='%Y-%m-%d')

# processing data
transactions <- dcast(transactions, transactionId + customerId + gender + transactiondate ~ product, value.var='product', fun.aggregate=length)

transactions <- transactions %>%
  group_by(customerId) %>%
  mutate(frequency=n(),
         recency=as.numeric(today-transactiondate)) %>%
  filter(transactiondate==max(transactiondate)) %>%
  filter(transactionId==max(transactionId)) %>%
  ungroup()

# exploratory analysis
ggplot(transactions, aes(x=frequency)) +
  theme_bw() +
  scale_x_continuous(breaks=c(1:10)) +
  geom_bar(alpha=0.6, binwidth=1) +
  ggtitle("Dustribution by frequency")

ggplot(transactions, aes(x=recency)) +
  theme_bw() +
  geom_bar(alpha=0.6, binwidth=1) +
  ggtitle("Dustribution by recency")

#Now it is time set the boundaries for the frequency and recency variables because there is a significant difference between customers who bought say 10 times and those who bought just 2 times. 
#It will also be important to see if there any difference between customers who bought the 10 times and another who bought 15 times? 
#We would need to categorize our customers by segments without losing the essence of the set boundaries. Hence, a new variable 'cart' will be created to capture products for some in-depth analysis.

transactions.segm <- transactions %>%
  mutate(segm.freq=ifelse(between(frequency, 1, 1), '1',
                          ifelse(between(frequency, 2, 2), '2',
                                 ifelse(between(frequency, 3, 3), '3',
                                        ifelse(between(frequency, 4, 4), '4',
                                               ifelse(between(frequency, 5, 5), '5', '>5')))))) %>%
  mutate(segm.rec=ifelse(between(recency, 0, 6), '0-6 days',
                         ifelse(between(recency, 7, 13), '7-13 days',
                                ifelse(between(recency, 14, 19), '14-19 days',
                                       ifelse(between(recency, 20, 45), '20-45 days',
                                              ifelse(between(recency, 46, 80), '46-80 days', '>80 days')))))) %>%
  # creating last cart feature
  mutate(cart=paste(ifelse(a!=0, 'a', ''),
                    ifelse(b!=0, 'b', ''),
                    ifelse(c!=0, 'c', ''), sep='')) %>%
  arrange(customerId)

# defining transaction of boundaries
transactions.segm$segm.freq <- factor(transactions.segm$segm.freq, levels=c('>5', '5', '4', '3', '2', '1'))
transactions.segm$segm.rec <- factor(transactions.segm$segm.rec, levels=c('>80 days', '46-80 days', '20-45 days', '14-19 days', '7-13 days', '0-6 days'))

#We have everything need to create Life-Time Matrix. We need to combine customers into segments with the following code:
  lcg <- transactions.segm %>%
  group_by(segm.rec, segm.freq) %>%
  summarise(quantity=n()) %>%
  mutate(customer='customer') %>%
  ungroup()

  #The classic matrix can be created with the following code:
  
  
  lcg.matrix <- dcast(lcg, segm.freq ~ segm.rec, value.var='quantity', fun.aggregate=sum)
lcg_matrix

#A good visualization is obtained through the following code:
  
  lcg_1_1

  #Changing colors to transactions for a better understanding of how to work with the matrix. 
  
  #There are four quadrants:
  
  #yellow - here are our best customers, who have placed quite a few transactions and made their last purchase recently. They have higher value and higher potential to buy again. We have to take care of them.
  #green - here are our new customers, who placed several transactions (1-3) recently. Although they have lower value, they have potential to move into the yellow zone. Therefore, we have to help them move into the right quadrant (yellow).
  #red - here are our former best customers. We need to understand why they are former and, maybe, try to reactivate them.
  #blue - here are our onetime-buyers.

  #Does it make sense to make the same offer to all of these customers? 
  
  #It is clear that it will be profitable to create different approaches not only for each quadrant, but for btransaction cells as well.

  #What I really like about this model of segmentation is that it is stable and alive simultaneously. It is alive in terms of customers flow. Every day, with or without purchases, it will provide customers flow from one cell to another. And it is stable in terms of working with segments. It allows to work with customers who are on the same lifecycle phase. That means you can create suitable campaigns / offers / emails for each or several close cells and use them constantly.

  #Ok, it's time to study how we can do some in-depth analysis. 
  #R allows us to create subsegments and visualize them effectively. 
  #It can be helpful to distribute each cell via some features. 
  #For instance, there can distribute customers by gender. 
  #For the other example, where our products have different lifecycles, it can be helpful to analyze which product/s was/were in the last cart or we can combine these features. Let's do this with the following code:
  
  
  lcg_2
  #or even:
  lcg_3

  #Therefore, there is a lot of space for creativity. If you want to know much more about Life-Time Matrix and strategies for working with quadrants, I highly recommend that you read Jim Novo's works, e.g. this blogpost.


