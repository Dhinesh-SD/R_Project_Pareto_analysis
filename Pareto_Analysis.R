library(dplyr)
Item_Summary<-read.csv('item_summary.csv')

#QUESTION 1:
#-------------------------------------------------------------------------------------------------
#Adding new Column Calculating Sales = units_sold*unit_selling_price
Item_Summary$Sales_Dollars<- Item_Summary$Units_Sold*Item_Summary$Unit_Selling_Price
#Summarizing by grouping by categories and then Ordering by descending Order of total sales by category
Sales_by_Category<-Item_Summary %>% group_by(Category) %>% summarise(Total_Sales_Amount = sum(Sales_Dollars)) %>% arrange(desc(Total_Sales_Amount))
#the top 2 Highest sold categories are
head(Sales_by_Category,n=2)
#Answer
# Category            Total_Sales_Amount
#1 Single-Use         260479528.
#2 Syringes            51813321.


#-------------------------------------------------------------------------------------------------

#Question 2:
#-------------------------------------------------------------------------------------------------

#sorting the table Item_Summary descending order based on Units Sold and display top 2 rows
new_table<-arrange(Item_Summary,desc(Item_Summary$Units_Sold))
head(new_table[,1:3],n=2)
#Answer
#  Item_Number    Category       Units_Sold
#1       21530    Single-Use     839365
#2       73158    Single-Use     779879


#-------------------------------------------------------------------------------------------------

#Question 3:
#-------------------------------------------------------------------------------------------------
#Create new table summarising cummulative dollar amount
Cummulative_Table<-Item_Summary%>% select(Item_Number,Sales_Dollars)
Cummulative_Table<- arrange(Cummulative_Table, desc(Sales_Dollars))
#Creating a pareto chart
#NOTE: Need to install and run 'qcc' package before executing pareto.chart
library(qcc)
pareto.chart(Cummulative_Table$Sales_Dollars,xlab='Item_Number', #x-Axis-Label
                                ylab='Frequency',#y-axis Label
             col=heat.colors(length(Cummulative_Table$Sales_Dollars)),
             cumperc = seq(0, 100, by = 10),
             ylab2 = "Cumulative Percentage",
             main = "Sales Analysis")
#Answer
#%of highest selling Item = 5.930%
#Pareto chart analysis for Cummulative_Table$Sales_Dollars
#       Frequency    Cum.Freq.   Percentage Cum.Percent.
#A   2.488384e+07 2.488384e+07 5.930302e+00 5.930302e+00
#B   2.446480e+07 4.934864e+07 5.830439e+00 1.176074e+01
#C   2.443418e+07 7.378282e+07 5.823140e+00 1.758388e+01
#D   2.204660e+07 9.582942e+07 5.254133e+00 2.283801e+01
#E   1.746719e+07 1.132966e+08 4.162770e+00 2.700078e+01
#F   1.629439e+07 1.295910e+08 3.883269e+00 3.088405e+01
#G   1.402358e+07 1.436146e+08 3.342093e+00 3.422615e+01        

#Manually Creating Table For Pareto Analysis
Cummulative_Table$cum_sum <- cumsum(Cummulative_Table$Sales_Dollars)
#Calculating Cummulative Percentage
Cummulative_Table$cummulative_Percentage<- (Cummulative_Table$cum_sum/sum(Cummulative_Table$Sales_Dollars))*100


#-------------------------------------------------------------------------------------------------

#Question 4:
#-------------------------------------------------------------------------------------------------
#Answer
# Number of items representing the first 50% of total dollars sold
sum(Cummulative_Table$cummulative_Percentage<50,na.rm = TRUE)
# First 14 items represent the top50%

# Number of items representing the first 80% of total dollars sold
sum(Cummulative_Table$cummulative_Percentage<80,na.rm = TRUE)
# First 61 items represent the top80%

#-------------------------------------------------------------------------------------------------
#FINANCIAL METRICS
#-------------------------------------------------------------------------------------------------

#Question 5:
#-------------------------------------------------------------------------------------------------

Gross_Margin_Table<- Item_Summary
Gross_Margin_Table$GM<- Gross_Margin_Table$Sales_Dollars-(Gross_Margin_Table$Units_Sold*Gross_Margin_Table$Unit_Inventory_Value)
Summary_Table<- Gross_Margin_Table%>%
                  group_by(Category)%>%
                    summarise(Sum_Inv=sum(Average_Inventory_Level)
                                ,Sum_Sales=sum(Sales_Dollars)
                                  ,Sum_GM=sum(GM),.groups ='drop')%>%
                  as.data.frame()
#the total gross margin and the gross margin percent for the Stethoscopes category:

Summary_Table$GM_Percent<-(Summary_Table$Sum_GM/Summary_Table$Sum_Sales)*100
Summary_Table
#Answer
#   Category    Sum_Avg_Inventory   Sum_Sales      Sum_GM     GM_Percent
#8 Stethoscopes 300283              4504428        1166589.3  25.89872


#-------------------------------------------------------------------------------------------------

#Question 6:
#-------------------------------------------------------------------------------------------------
#Answer
#GM of All Items Added Together
sum(Summary_Table$Sum_GM) #=$163,567,204
#GM Percent for all Items Together
(sum(Summary_Table$Sum_GM)/sum(Summary_Table$Sum_Sales))*100 #= 38.98125%

#-------------------------------------------------------------------------------------------------

#Question 7:
#-------------------------------------------------------------------------------------------------

Summary_Table$GMROI<-Summary_Table$Sum_GM/Summary_Table$Sum_Inv
Summary_Table
#GMROI For Gloves Category = 5.295076


#-------------------------------------------------------------------------------------------------

#Question 8:
#-------------------------------------------------------------------------------------------------

arrange(Summary_Table,desc(GMROI))
#The Devices Category Has Highest GMROI of 9.089084

#-------------------------------------------------------------------------------------------------

#Question 9:
#-------------------------------------------------------------------------------------------------

#Net Profit = Gross Margin - Total Expense
sum(Summary_Table$Sum_GM)-42000000 #= $121,567,204

#-------------------------------------------------------------------------------------------------
#END
#-------------------------------------------------------------------------------------------------
