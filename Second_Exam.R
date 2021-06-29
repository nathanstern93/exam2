#1 - Clear the environment
rm(list = ls(all=TRUE))
#2 - Load dataset and rename it "college_scorecared"
library(readr)
college_scorecard <- read_csv("college_scorecard.csv")
View(college_scorecard)
#3 - provide summary statistics for dataset
summary(college_scorecard)
#4 - 4. Create a smaller dataset with just data measured in 2014 and 2015 on former students who graduated from four-year+ colleges and universities located in Texas (state_abbr: “TX”) and Louisiana (state_abbr: “LA”). Call the resulting data frame “small_scorecard”
small_scorecard <- filter(college_scorecard, year == 2014 | year ==2015) %>%
  filter(state_abbr == "TX" | state_abbr == "LA")
# 5 get a) avg people working who graduated from universities in TX and LA b) total number of people working who graduated
summary(small_scorecard)
even_smaller_scorecard <- small_scorecard %>%
                        group_by(state_abbr) %>%
                      summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
                      select(c(state_abbr, count_working, count_not_working))

205786 + 757121  # sum = 962907
962907/955 #mean = 1008.28

# 6 use bar graph detailing percent of people working on even smaller scorecard
even_smaller_scorecard$percent_working <- even_smaller_scorecard$count_working / 
                                            (even_smaller_scorecard$count_working +
                                               even_smaller_scorecard$count_working)
library(ggplot2)
library(gapminder)
library(gganimate)
p<- ggplot(data = even_smaller_scorecard,
       aes(x = state_abbr, y = percent_working)) +
  geom_bar()

#8 Load the avocados dataset and name it avocados
library(readr)
avocados <- read_csv("avocados.csv")
View(avocados)

#9 create new variable called year that only captures the year in which they were sold
library(tidyverse)
library(lubri)
avocados$Year <- format(as.Date(avocados$date, format="%d/%m/%Y"),"%Y")

#10 deflate the price variable
library(WDI)
deflator_data = WDI(country = "USA", indicator = c("NY.GDP.DEFL.ZS"),
                    start = 1960,
                    end = 2017, 
                    # end of of foreign aid data 
                    extra = FALSE, cache = NULL)
subset(deflator_data, deflator==100)


