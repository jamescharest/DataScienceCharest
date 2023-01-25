## Load the libraries I need: 
library(tidyverse)
library(openxlsx)
library(lubridate)

## Upload the files as data frames
AddsToCart <- read_csv("DataAnalyst_Ecom_data_addsToCart.csv")
SessionCounts <- read_csv("DataAnalyst_Ecom_data_sessionCounts.csv")

## Get some info about the data
# start with AddstoCart
View(AddsToCart)
str(AddsToCart)

# now look at SessionCounts
View(SessionCounts)
str(SessionCounts)

## Manipulating the data - AddsToCart
#Add a column with the abbrev name of the month
AddsToCart$month <- month.abb[AddsToCart$dim_month]

## Manipulating the data - SessionCounts
#The dim_date is not in date format
SessionCounts$dim_date <- as.Date(SessionCounts$dim_date,"%m/%d/%y")
#Extracts the month
SessionCounts$month <- month(ymd(SessionCounts$dim_date))
#Turn month column to abbrev
SessionCounts$month <- month.abb[SessionCounts$month]

#Creating Sheet 1
Sheet1 <- SessionCounts %>%
  group_by(year = year(dim_date), month = month(dim_date), device = dim_deviceCategory) %>% 
  summarise(TotalSessions = sum(sessions),
            Totaltransactions = sum(transactions),
            TotalQTY = sum(QTY),
            ECR = sum(transactions)/sum(sessions))

#Work on creating Sheet 2
#Get just the metrics and the year-month for SessionCounts
SessionCountsMY <- SessionCounts %>% 
  group_by(year=year(dim_date), month=month(dim_date)) %>% 
  summarise(TotalSessions= sum(sessions),
             Totaltransactions = sum(transactions),
             TotalQTY = sum(QTY),
             ECR = sum(transactions)/sum(sessions))

#Join this with AddstoCart
Sheet2 <- full_join(AddsToCart, SessionCountsMY, 
                    by = c("dim_year"="year", "dim_month" = "month"))

#Find most recent date
max(SessionCounts$dim_date)

#Look at only months of 2013-06 and 2013-05
#Also reorganize order of columns
Sheet2 <- select(Sheet2, monthname = month, monthnum = dim_month, year = dim_year,
                 TotalAddsToCart = addsToCart, TotalSessions, Totaltransactions, 
                 TotalQTY, ECR) %>% 
  filter(monthnum == 6 | monthnum == 5 & year == 2013)

#Sheet 2 doesn't have the differences: 
Sheet2b <- Sheet2 %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), diff),
                      across(where(is.character), ~"Diff")))
 
#Creating XLS file
final_dataset <- list("Sheet1" = Sheet1, "Sheet2" = Sheet2b)
write.xlsx(final_dataset, file = "IXIS.xlsx")


##This was trial and error to see if we could get calculations a different way
#Transpose the sheet, and see if we can adjust from there
Sheet2c <- t(Sheet2)
#Rename columns
colnames(Sheet2c) <- c("Last_Month", "This_Month")
#Eliminate row with month label
Sheet2c <-Sheet2c[-(1:3),]
# Still unable to take differences between the columns
# Converting to a dataframe eliminates the key labels of measurements


##Analysis and Visualizations
#Does increase in session counts lead to increase in transactions?
ggplot(SessionCounts, aes(x=sessions, y=transactions))+
  geom_point()+
  geom_smooth()

#Does the device type impact this?
ggplot(SessionCounts, aes(x=sessions, y=transactions, col=dim_deviceCategory))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~dim_deviceCategory)

ggplot(SessionCounts, aes(x=sessions, y=transactions, col=dim_deviceCategory))+
  geom_point()+
  geom_smooth()

#Looking at the number of transactions per month
PlotSessionsCount <- SessionCounts 
PlotSessionsCount$month_year <- format(as.Date(PlotSessionsCount$dim_date), "%Y-%m")


PlotSessionsCount <- PlotSessionsCount %>%
  group_by(date = dim_date, device = dim_deviceCategory) %>% 
  summarise(TotalSessions = sum(sessions),
            Totaltransactions = sum(transactions),
            TotalQTY = sum(QTY),
            ECR = sum(transactions)/sum(sessions))

ggplot(PlotSessionsCount, aes(x=date, y=Totaltransactions, color=device))+
  geom_line()+
  xlab("")

ggplot(PlotSessionsCount, aes(x=date, y=Totaltransactions))+
  geom_line()+
  xlab("")+
  facet_wrap(~device)

#Too chaotic when taking into account each day, try grouping by month
PlotSessionsCount2 <- SessionCounts 
PlotSessionsCount2$month_year <- format(as.Date(PlotSessionsCount2$dim_date), "%Y-%m")
PlotSessionsCount2 <- PlotSessionsCount2 %>%
  group_by(month = month_year, device = dim_deviceCategory) %>% 
  summarise(TotalSessions = sum(sessions),
            Totaltransactions = sum(transactions),
            TotalQTY = sum(QTY),
            ECR = sum(transactions)/sum(sessions))

ggplot(PlotSessionsCount2, aes(x=month, y=Totaltransactions, group=device, color=device))+
  geom_line()+
  xlab("")+
  theme(axis.text.x = element_text(angle = 45))

#Look at adds to cart per month
PlotFull <- full_join(AddsToCart, SessionCountsMY, 
                      by = c("dim_year"="year", "dim_month" = "month"))
PlotFull$year_month <- paste(PlotFull$dim_year, PlotFull$dim_month, sep="-")
PlotFull <- select(PlotFull, year_month, addsToCart, TotalSessions, Totaltransactions,
                   TotalQTY, ECR)

ggplot(PlotFull, aes(x=year_month, y=addsToCart, group="1"))+
  geom_line(color="royalblue")+
  xlab("")+
  theme(axis.text.x = element_text(angle = 45))

#Does adds to cart influence transactions?
ggplot(PlotFull, aes(x=addsToCart, y=Totaltransactions))+
  geom_point()+
  geom_smooth()

#Does adds to cart influence QTY?
ggplot(PlotFull, aes(x=addsToCart, y=TotalQTY))+
  geom_point()+
  geom_smooth()
