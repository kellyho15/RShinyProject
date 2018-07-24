library(tidyverse)
library(DT)
library(shiny)
library(googleVis)
library(ggplot2)

# clean adult data frame ####

# import adult data 
data = read.csv("./Nutrition__Physical_Activity__and_Obesity_-_Behavioral_Risk_Factor_Surveillance_System.csv", header = T, stringsAsFactors = F)

# shorten question description (for ploting purpose)
data$Question = gsub("Percent of adults who report consuming fruit less than one time daily", "Fruit consumption", data$Question, fixed=TRUE)
data$Question = gsub("Percent of adults who report consuming vegetables less than one time daily", "Vegetables consumption", data$Question, fixed=TRUE)
data$Question = gsub("Percent of adults aged 18 years and older who have obesity", "Obesity", data$Question, fixed=TRUE)
data$Question = gsub("Percent of adults aged 18 years and older who have an overweight classification", "Overweight", data$Question, fixed=TRUE)
data$Question = gsub("Percent of adults who achieve at least 150 minutes a week of moderate-intensity aerobic physical activity or 75 minutes a week of vigorous-intensity aerobic activity (or an equivalent combination)", "Short duration aerobic", data$Question, fixed=TRUE)
data$Question = gsub("Percent of adults who achieve at least 150 minutes a week of moderate-intensity aerobic physical activity or 75 minutes a week of vigorous-intensity aerobic physical activity and engage in muscle-strengthening activities on 2 or more days a week", "Short duration aerobic and strengthening", data$Question, fixed=TRUE)
data$Question = gsub("Percent of adults who achieve at least 300 minutes a week of moderate-intensity aerobic physical activity or 150 minutes a week of vigorous-intensity aerobic activity (or an equivalent combination)", "Long duration aerobic", data$Question, fixed=TRUE)
data$Question = gsub("Percent of adults who engage in muscle-strengthening activities on 2 or more days a week", "Strengthening", data$Question, fixed=TRUE)
data$Question = gsub("Percent of adults who engage in no leisure-time physical activity", "No physical activity", data$Question, fixed=TRUE)
data$Stratification1 = gsub("Non-Hispanic Black", "African American", data$Stratification1, fixed=TRUE)
data$Stratification1 = gsub("Non-Hispanic White", "Caucasian", data$Stratification1, fixed=TRUE)

data$Stratification1 = gsub("Less than $15,000", "$0k - $15k", data$Stratification1, fixed=TRUE)
data$Stratification1 = gsub("$15,000 - $24,999", "$15k - $25k", data$Stratification1, fixed=TRUE)
data$Stratification1 = gsub("$25,000 - $34,999", "$25k - $35k", data$Stratification1, fixed=TRUE)
data$Stratification1 = gsub("$35,000 - $49,999", "$35k - $50k", data$Stratification1, fixed=TRUE)
data$Stratification1 = gsub("$50,000 - $74,999", "$50k - $75k", data$Stratification1, fixed=TRUE)
data$Stratification1 = gsub("$75,000 or greater", "Above $75k ", data$Stratification1, fixed=TRUE)

data$Stratification1 = gsub("Some college or technical school", "Technical school/college", data$Stratification1, fixed=TRUE)

data$StratificationCategory1 = gsub("Race/Ethnicity", "Ethnicity", data$StratificationCategory1, fixed=TRUE)
data$StratificationCategory1 = gsub("Age (years)", "Age", data$StratificationCategory1, fixed=TRUE)


# convert fruits/veggies consumption to show percentage of adult consuming fruits/veggies more than once a day for easy understanding 
data1 = 
  data %>%
  filter(., Question == "Fruit consumption") %>%
  mutate(., Data_Value = 100-Data_Value)
data2 = 
  data %>%
  filter(., Question == "Vegetables consumption") %>%
  mutate(., Data_Value = 100-Data_Value)


# reconstructing data frame
data3 = 
  data %>%
  filter(., !Question %in% c("Fruit consumption", "Vegetables consumption")) # filter out old fruits/veggies consumption

data4 =
  rbind(data3, data1, data2) # combine in new conversion of fruit/veggies


# select needed cols
data5 = select(data4, Year = YearStart, LocationDesc, Question, Data_Value,
               Stratification = Stratification1, StratificationCategory = StratificationCategory1)


# filter out rows
data6 = 
  data5 %>%
  mutate(., Data = "adult") %>% # add new column for differentiating youth vs adult data in dataframe: ay_combine 
  filter(., !Data_Value == is.na(Data_Value), 
         !Question == "Overweight",  # limit scope to obesity only
         !Stratification %in% c("Data not reported", "2 or more races", "Other", 
                                "American Indian/Alaska Native", "Hawaiian/Pacific Islander"),  # note: Stratification include Total still
         !LocationDesc %in% c("District of Columbia", "Guam", "Puerto Rico", "Virgin Islands"))  # note: LocationDesc include National still


#############################################################################################################

# clean youth data frame ####

# import youth data
youth = data = read.csv("./Nutrition__Physical_Activity__and_Obesity_-_Youth_Risk_Behavior_Surveillance_System.csv", header = T, stringsAsFactors = F)

# shorten question description (for ploting purpose)
youth$Question = gsub("Percent of students in grades 9-12 watching 3 or more hours of television each school day", "≥ 3 hr television", youth$Question, fixed=TRUE)
youth$Question = gsub("Percent of students in grades 9-12 who achieve 1 hour or more of moderate-and/or vigorous-intensity physical activity daily", ">1 hr physical activity", youth$Question, fixed=TRUE)
youth$Question = gsub("Percent of students in grades 9-12 who consume fruit less than 1 time daily", "Fruit consumption", youth$Question, fixed=TRUE)
youth$Question = gsub("Percent of students in grades 9-12 who consume vegetables less than 1 time daily", "Vegetables consumption", youth$Question, fixed=TRUE)
youth$Question = gsub("Percent of students in grades 9-12 who drank regular soda/pop at least one time per day", "Soda consumption ≥ once/day", youth$Question, fixed=TRUE)
youth$Question = gsub("Percent of students in grades 9-12 who have an overweight classification", "Overweight", youth$Question, fixed=TRUE)
youth$Question = gsub("Percent of students in grades 9-12 who have obesity", "Obesity", youth$Question, fixed=TRUE)
youth$Question = gsub("Percent of students in grades 9-12 who participate in daily physical education", "Daily PE class", youth$Question, fixed=TRUE)
youth$Stratification1 = gsub("Non-Hispanic Black", "African American", youth$Stratification1, fixed=TRUE)
youth$Stratification1 = gsub("Non-Hispanic White", "Caucasian", youth$Stratification1, fixed=TRUE)
youth$StratificationCategoryId1 = gsub("GRADE", "AGEYR", youth$StratificationCategoryId1, fixed=TRUE)
youth$StratificationCategory1 = gsub("Race/Ethnicity", "Ethnicity", youth$StratificationCategory1, fixed=TRUE)
youth$StratificationCategory1 = gsub("Grade", "Age", youth$StratificationCategory1, fixed=TRUE)

# convert grade to age (for ploting purpose)
youth$Stratification1 = gsub("9th", "14", youth$Stratification1, fixed=TRUE)
youth$Stratification1 = gsub("10th", "15", youth$Stratification1, fixed=TRUE)
youth$Stratification1 = gsub("11th", "16", youth$Stratification1, fixed=TRUE)
youth$Stratification1 = gsub("12th", "17", youth$Stratification1, fixed=TRUE)

# convert fruits/veggies consumption to show percentage of youth consuming fruits/veggies more than once a day for easy understanding
youth1 =
  youth %>%
  filter(., Question == "Fruit consumption") %>%
  mutate(., Data_Value = 100-Data_Value)
youth2 =
  youth %>%
  filter(., Question == "Vegetables consumption") %>%
  mutate(., Data_Value = 100-Data_Value)

# reconstructing data frame
youth3 = 
  youth %>%
  filter(., !Question %in% c("Fruit consumption", 
                             "Vegetables consumption")) # filter out old fruits/veggies consumption
youth4 =
  rbind(youth3, youth1, youth2) # combine in new conversion of fruit/veggies

# select needed cols
youth5 = select(youth4, Year = YearStart, LocationDesc, Question, Data_Value,
                Stratification = Stratification1, StratificationCategory = StratificationCategory1)

# filter out rows
youth6 = 
  youth5 %>%
  mutate(., Data = "youth") %>%  # add new column for differentiating youth vs adult data in dataframe: ay_combine 
  filter(.,!Data_Value == is.na(Data_Value),
         !Question == "Overweight",  # limit scope to obesity only
         !Stratification %in% c("2 or more races", "Hawaiian/Pacific Islander", "American Indian/Alaska Native"), # note: Stratification include Total still
         !LocationDesc %in% c("District of Columbia", "Guam", "Puerto Rico", "Virgin Islands")) # note: LocationDesc include National still

#############################################################################################################

# final data frames to work with ####
ay_combine = 
  rbind(data6, youth6)    # long data frame 

# new data frame for sorting education level in order for graphing
edu = filter(ay_combine, StratificationCategory == "Education")
edu$Stratification = as.factor(edu$Stratification)
edu$Stratification = factor(edu$Stratification, levels(edu$Stratification)[c(3,2,4,1)])

# wide data frame for lin reg model
y_spread =
  youth6 %>%
    spread(key = Question, value = Data_Value) %>%
    filter(Stratification == "Total", !LocationDesc == "National")    # wide data frame for lin reg

a_spread =
  data6 %>%
    spread(key = Question, value = Data_Value) %>%
    filter(Stratification == "Total", !LocationDesc == "National")    # wide data frame for lin reg


#############################################################################################################
# graphical output ####

# heat map for US (adult) ####
a_heatmap_2013 =
  ay_combine %>%
  filter(., Stratification == "Total", Year == 2013, !LocationDesc == "National", Data == "adult") %>%   # heat map for 2015
  spread(., key = Question, value = Data_Value) %>%
  select(., 2, 6:13)

# selection for heat map
a_choice2013 = c("Obesity", "Fruit consumption", "Vegetables consumption", "Short duration aerobic",
                 "Short duration aerobic and strengthening", "Long duration aerobic", "Strengthening", 
                 "No physical activity")  # column names for heat map selection


# heat map for US (youth) ####
y_heatmap_2013 =   # use 2013 data instead becasue 2013 has more complete data set (has 43 staetes instead of 37 states for 2015)
  ay_combine %>%       # obesity trend also did not change that much between 2013 and 2015
  filter(., Stratification == "Total", Year == 2013, !LocationDesc == "National", Data == "youth") %>%   # heat map for 2015
  spread(., key = Question, value = Data_Value) %>%
  select(., 2, 6:12)

# selection for heat map
y_choice2013 = c("Obesity", "Fruit consumption", "Vegetables consumption", "Soda consumption", "Daily PE class", 
                 ">1 hr physical activity", ">3 hr television")  # column names for heat map selection


# selection for demographic ####
y_choice_cat = c("Gender", "Age", "Ethnicity")
y_choice_states = c("National", sort(unique(filter(youth6, !LocationDesc == "National")$LocationDesc))) # chance the order of choice selection

a_choice_cat = c("Gender", "Age", "Ethnicity", "Education", "Income")
a_choice_states = c("National", sort(unique(filter(data6, !LocationDesc == "National")$LocationDesc)))


# selection for comparison between adult and youth ####
ay_choice_food  = c("Fruit consumption", "Vegetables consumption")

y_choice_pa  = c(">1 hr physical activity", "Daily PE class")
a_choice_pa  = c("Short duration aerobic", "Short duration aerobic and strengthening", "Long duration aerobic", "Strengthening")


