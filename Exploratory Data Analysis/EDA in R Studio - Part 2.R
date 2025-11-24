###
# Slicing a vector in R
###

#create a vector
x <- c(10:19)

#slicing data in different ways
x[7]
x[-3]
x[8:10]
x[c(2, 6, 10)]
x[x %% 2 == 0] # %% is a modelo

###
# slicing dataframes
###

#load USGS water data for the USA
intro_df <- read.csv(file = "course_NWISdata.csv", stringsAsFactors = FALSE,
                     colClasses = c("character", rep(NA, 6)))

#look at first 6 rows
head(intro_df)

#single row or column, and ranges
intro_df[15, ]
intro_df[3:7, ]
intro_df[, 4]
head(intro_df["dateTime"])
head(intro_df$dateTime)
head(intro_df[c("dateTime", "Flow_Inst")])

#subset data by conditions
high_temp_df <- intro_df[intro_df$Wtemp_Inst > 15,]
head(high_temp_df)

###
# data munging with dplyr
###

#install.packages("dplyr")
library("dplyr")

#select some columns
head(select(intro_df, site_no, dateTime, DO_Inst))

#select observations with estimated flows
head(filter(intro_df, Flow_Inst_cd == "E"))

#add a column with dissolved oxygen in mg/mL instead of mg/L
head(mutate(intro_df, DO_mgml = DO_Inst/1000))

#arrange by DO
head(arrange(intro_df, DO_Inst))
head(arrange(intro_df, desc(DO_Inst)))

###
# Using multiple functions at once
###

#Intermediate data frames
tmp <- select(intro_df, site_no, dateTime, Flow_Inst, Flow_Inst_cd)
error_df <- filter(tmp, Flow_Inst_cd == "X")
head(error_df)

#Nested functions
error_df <- filter(
  select(intro_df, site_no, dateTime, Flow_Inst, Flow_Inst_cd),
  Flow_Inst_cd == "X")
head(error_df)

#Pipes
error_df <- intro_df %>%
  select(site_no, dateTime, Flow_Inst, Flow_Inst_cd) %>%
  filter(Flow_Inst_cd == "X")
head(error_df)

#new syntax for pipes
error_df <- intro_df |>
  select(site_no, dateTime, Flow_Inst, Flow_Inst_cd) |>
  filter(Flow_Inst_cd == "X")
head(error_df)

#rename multiple columns (new_name = old_name)
intro_df <- rename(intro_df,
                   Flow = Flow_Inst,
                   Flow_cd = Flow_Inst_cd,
                   Wtemp = Wtemp_Inst,
                   pH = pH_Inst,
                   DO = DO_Inst)

#keep only rows with real flow value
nrow(intro_df) #nrow: number of rows
intro_df <- filter(intro_df, !is.na(Flow)) # !=not missing
nrow(intro_df)

#remove erroneous and estimated
intro_df <- filter(intro_df, Flow_cd != "X")
intro_df <- filter(intro_df, Flow_cd != "E")
# how could we do this shorter?

#convert temp to F
intro_df <- mutate(intro_df, Wtemp_F = (Wtemp * 9/5) + 32)

#############################################################################
#Practice problems!

#1. Remove the Flow_cd column

intro_df <- filter(intro_df, Flow_cd)
intro_df

#2. Keep only observations where flow was greater than 10 cubic feet per second


#3. Add a new column where flow is in cubic meters per second (hint: 3.28 feet  = 1 m)
head(mutate(intro_df, Flow = DO_Inst/1000))