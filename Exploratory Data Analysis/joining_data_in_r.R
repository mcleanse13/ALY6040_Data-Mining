#load dplyr
library(dplyr)

#create some toy data to join
df_primary <- tribble(
  ~ID, ~y,
  "A", 5,
  "B", 5,
  "C", 8,
  "D", 0,
  "F", 9)
df_secondary <- tribble(
  ~ID, ~z,
  "A", 30,
  "B", 21,
  "C", 22,
  "D", 25,
  "E", 29)

#don't forget to look at the help pages for new functions!
#All joins are lumped together under this page:
?join

#left join: keys from original table, don't keep anything from new table that 
#doesn't have a matching key
left_join(df_primary, df_secondary, by ='ID')

#right join: like left join, but keys and rows are from the new table
right_join(df_primary, df_secondary, by = 'ID')

#inner join: only rows found in both datasets are kept
inner_join(df_primary, df_secondary, by ='ID')

#full join: all rows are kept
full_join(df_primary, df_secondary, by ='ID')

#different, more modern syntax:
left_join(df_primary, df_secondary, join_by(ID))

#on more complex tables, can join by multiple fields
df_primary <- tribble(
  ~ID, ~year, ~items,
  "A", 2015,3,
  "A", 2016,7,
  "A", 2017,6,
  "B", 2015,4,
  "B", 2016,8,
  "B", 2017,7,
  "C", 2015,4,
  "C", 2016,6,
  "C", 2017,6)
df_secondary <- tribble(
  ~ID, ~year, ~prices,
  "A", 2015,9,
  "A", 2016,8,
  "A", 2017,12,
  "B", 2015,13,
  "B", 2016,14,
  "B", 2017,6,
  "C", 2015,15,
  "C", 2016,15,
  "C", 2017,13)

left_join(df_primary, df_secondary, by = c('ID', 'year'))

#############################################################################
#Practice problems!

#install.packages("nycflights13")
library(nycflights13)

# This package contains information about all flights that departed from NYC 
#(e.g. EWR, JFK and LGA) to destinations in the United States, Puerto Rico, and 
#the American Virgin Islands) in 2013: 336,776 flights in total.

#1.) Select only year, time_hour, origin, dest, tailnum, carrier from flights
?flights
head(flights)

#flights2<- 

#2.) add the temperature and wind speed weather metadata to flights2
  
head(weather)


#3.) add the names of the destination airports to flights2
#(hint: how can you specify the destination as the key?)

head(airports)
