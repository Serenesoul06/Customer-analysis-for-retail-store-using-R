library(dplyr)
library(plotly)
library(lubridate)
library(eeptools)
getwd()
setwd("D:\\GITHUB\\R\\Customer analysis for retail store")
Cust=read.csv("Customer.csv")
Trans=read.csv("Transaction.csv")
Prod=read.csv("Product.csv")

# Q.1.Merge the datasets Customer, Product and Transaction as 
# Final. Ensure to keep all customers who have done transactions 
# with us and select the join type accordingly.
# a.	Use the base merge()
# b.	Dplyr merge functions

# a. Use the base merge()
Final=merge(x=Trans, y=Cust, by.x="cust_id", by.y ="customer_Id", all.x=T ) %>%
  merge(y=Prod, by.x=c("prod_subcat_code","prod_cat_code"), by.y =c("prod_sub_cat_code","prod_cat_code"), all.x=T )

View(Final)

# b.	Dplyr merge function
Final=left_join(x=Trans, y=Cust, by=c("cust_id"="customer_Id")) %>%
                  left_join(y=Prod, by=c("prod_subcat_code"="prod_sub_cat_code",
                                         "prod_cat_code"="prod_cat_code"))

str(Final)

# since both the date variables are character. We will convert it into date variables.
Final$tran_date=as.Date(Final$tran_date, "%d-%m-%Y")
Final$DOB=as.Date(Final$DOB, "%d-%m-%Y")

# 2.	Prepare a summary report for the merged data set.
# a.	Get the column names and their corresponding data types

str(Final)

# b.	Top/Bottom 10 observations

Top10=head(Final, 10)
Bottom10=tail(Final, 10)

# c.	"Five-number summary" for continuous variables (min, Q1, median, 
#      Q3 and max)

summary(Final$Tax)
summary(Final$Rate)
summary(Final$total_amt)
summary(Final$Qty)

# d.	Frequency tables for all the categorical variables
names(Final)
freq_table=table(Final$prod_cat,Final$prod_subcat)
View(freq_table)
freq_table

freq_table1=table(Final$Store_type)
View(freq_table1)
freq_table1

freq_table2=table(Final$Gender)
View(freq_table2)
freq_table2=freq_table2[-1]
class(freq_table2)
freq_table2

freq_table3=table(Final$prod_cat)
View(freq_table3)
freq_table3

freq_table4=table(Final$prod_subcat)
View(freq_table4)
freq_table4

freq_table5=table(Final$city_code)
View(freq_table5)
freq_table5

# 3.Generate histograms for all continuous variables and frequency bars 
# for categorical variables.
names(Final)
hist(Final$Qty, breaks=6, xlab="Quantity Purchased",main="Histogram of Quantity", col="Blue", border="Red")
hist(Final$Rate, xlab="Rate",main="Histogram of Rate", col="Yellow", border="Red")
hist(Final$Tax, xlab="Tax",main="Histogram of Tax", col="Orange", border="Red" )
hist(Final$total_amt,xlab="Total Amount",main="Total Amount", col="black", border="Red")

#length(Final$city_code[is.na(Final$city_code)])
barplot(freq_table5, xlab="City Code", ylab="Frequency",main="Frequency of City Code" , col="green", border="Red")
barplot(freq_table2, xlab="Gender", ylab="Frequency", main="Frequency of Gender", col="BLUE", border="pink")
barplot(freq_table1, xlab="Store type", ylab="Frequency", main="Frequency of Store type", col="BLUE", border="pink")
barplot(freq_table3, xlab="Product category", ylab="Frequency", main="Frequency of Product category", col="BLUE", border="pink")
barplot(freq_table4, xlab="Product Sub_category", ylab="Frequency", main="Frequency of Product Sub_category", col="BLUE", border="pink")

# 4.	Calculate the following information using the merged dataset :
# a.	Time period of the available transaction data
# b.	Count of transactions where the total amount of transaction was 
#     negative

# a
#is.na(Final$tran_date)
MAX_DATE=max(Final$tran_date, na.rm=T)
MIN_DATE=min(Final$tran_date, na.rm=T)
y1=year(MAX_DATE)-year(MIN_DATE)
y2=age_calc(MIN_DATE, MAX_DATE, units="months") #from eeptools pkg
y2=interval(MIN_DATE, MAX_DATE) %/% months(1) #from lubridate
y3=difftime(MAX_DATE,MIN_DATE,units="days")
y4=difftime(MAX_DATE,MIN_DATE,units="weeks")


message("TIME PERIOD OF AVAILABLE TRANSACTION DATA IS ", y1, " YEARS. 
WHICH IS EQUIVALENT TO ", y2," MONTHS. 
WHICH IS EQUIVALENT TO ", y3," DAYS. 
WHICH IS EQUIVALENT TO ", y4," WEEKS.")


# b.
Final4=filter(Final,total_amt<=0) %>% nrow()
Final4

# 5.Analyze which product categories are more popular among females 
#   vs male customers.

Final5=filter(Final,Qty>0) %>%
  group_by(Gender, prod_cat) %>%
  summarise(Quantity_ordered= sum(Qty)) %>%
              filter(Gender %in% c("M","F"))
G=plot_ly(Final5, x=~prod_cat, y=~Quantity_ordered, type="bar", color = ~Gender) %>%
  layout(barmode="group", title="Popularity of products by gender", 
         xaxis=list(title="Quantity ordered"), 
         yaxis=list(title="Product category")) 
G

# 6.	Which City code has the maximum customers and what was the percentage of customers 
#     from that city?

Final6=filter(Final, Qty>0) %>%
  group_by(city_code, cust_id) %>%
  summarise(No_of_cust=n()) %>% 
  summarise(No_of_cust=n()) %>% 
  arrange(desc(No_of_cust)) %>%
  mutate(cust_percent=No_of_cust*100/sum(No_of_cust)) %>%
  head(1)
Final6

# 7.	Which store type sells the maximum products by value and by quantity?

Final7=filter(Final,Qty>0) %>%
  group_by(Store_type) %>%
  summarise(Quantity_ordered= sum(Qty),Total_amt=sum(total_amt)) %>%
  arrange(desc(Quantity_ordered), desc(Total_amt)) %>%
  head(1)
paste("Store type", Final7[1], "sells maximum quantity of",Final7[2], "and maximum amount of", Final7[3], sep=" ")
# OR
message("THE Final7[1] STORE TYPE THAT SELLS MAXIMUM QUANTITY OF ", Final7[2], " AND MAXIMUM AMOUNT OF ", Final7[3])


# 8.	What was the total amount earned from the "Electronics" and 
#   "Clothing" categories from Flagship Stores? 

Final8=filter(Final, Store_type=="Flagship store", prod_cat %in% c("Electronics","Clothing"))
message("Total amount earned from the Electronics and Clothing categories 
        from Flagship Stores is : " ,sum(Final8$total_amt)  )

# 9.	What was the total amount earned from "Male" customers under  
#     the "Electronics" category?  

Final9=filter(Final, Gender=="M", prod_cat=="Electronics")
message("Total amount earned from Male customers under the Electronics is : ",
        sum(Final9$total_amt)  )

# 10.	How many customers have more than 10 unique transactions, 
# after removing all transactions which have any negative amounts?  

Final10=filter(Final, Qty>0) %>%
  group_by(cust_id) %>%
  summarise(transactions=n()) %>%
  filter(transactions>10)
message("TOTAL NUMBER OF CUSTOMERS HAVING MORE THAN 10 UNIQUE TRANSACTIONS IS ", 
        length(Final10$cust_id))


# 11.	For all customers aged between 25 - 35, find out:
# a.	What was the total amount spent for "Electronics" and 
# "Books" product categories?
# b.	What was the total amount spent by these customers between 
# 1st Jan, 2014 to 1st Mar, 2014?

# a
Final11.a=mutate(Final, Age=year(Sys.Date())-year(DOB)) %>%
  filter(Age %in% c(25:35), prod_cat %in% c("Electronics", "Books"))
message("Total amount earned spent for Electronics and 
        Books product categories: ", sum(Final11.a$total_amt))


# b
Final11.b=mutate(Final, Age=year(Sys.Date())-year(DOB)) %>%
  filter(Age %in% c(25:35), prod_cat %in% c("Electronics", "Books"), 
         tran_date> "2014-01-01", tran_date< "2014-03-01")
message("Total amount earned spent for Electronics and 
        Books product categories from Jan to Mar'2014: ", sum(Final11.b$total_amt))

