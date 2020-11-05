

## Purpose of this document
# create a cleaned version of my qualtrics data
# which is called "original.csv"




## Steps I want to take

#1 load the data into r
#2 clean it
#3 output a new, cleaned file















# 1 load data into r

df_dirty <- read.csv("../data/working.csv")

## load from crazy folder

# df <- read.csv("../../crazy/working.csv")
# I didn't realize it was "read.csv" rather than "load.csv"
# so I used ?write.csv

?read.csv














# 2 clean it

head(df_dirty)


## select columns stupid, bad, and people

library(tidyverse)
df_dirty <- df_dirty %>% 
  select(stupid, 
         bad,
         people)


## filter participants to those less than 450
## parhaps the fire alarm went off for participants 451 through 500
## so I don't want to use them

df_dirty <- df_dirty %>% 
  filter(people < 451)



## add my conditions
## people who were less than 300 were in "red pill"
## people who were 301 and above were in "blue pill"

df_dirty <- df_dirty %>% 
  mutate(condition = ifelse(people < 301, "red pill", "blue pill"))


## use case_when()
## when you have many discrete categories

df_dirty <- df_dirty %>% 
  mutate(birth = case_when(
    people < 100 ~ "Mexico",
    people < 200 & people > 100 ~ "US",
    people < 451 & people > 201 ~ "Brazil"
  ))


## get rid of row 2

df_dirty <- df_dirty %>% 
  slice(-2)



















# 3 output the cleaned version

df <- df_dirty
write.csv(df, file = "../data/cleaned.csv")
