library(dplyr)
library(eurostat)
library(sf)

# Set the working directory to the folder where the CSV file is located
setwd("D:/STUDY/STUDY/R")

# Read the CSV file and store it in a data frame
BCGdata <- read.csv("BCG.csv")

# Create object with all countries in Europe
eu_countries <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden")

# filter the data frame to include only EU countries and remove any missing values
eu_bcg_data <- BCGdata %>%
  filter(Country %in% eu_countries & !is.na(Coverage))

# print the head of the resulting data frame
question1 <- eu_bcg_data[,-2]
head(question1)



# select only the data from 2010 onwards
bcg_data_filtered <- question1 %>%
  filter(Year >= 2010)

# group the data by country
bcg_data_grouped <- bcg_data_filtered %>%
  group_by(Country)

# select only the countries where the minimum coverage rate of each year is above 0.9
bcg_data_above_0.9 <- bcg_data_grouped %>%
  filter(all(Coverage > 0.9)) %>%
  ungroup()

# print the resulting data frame
question2_df <- data.frame(Country = unique(bcg_data_above_0.9$Country))
print(question2_df)



# group the data by country
bcg_data_grouped <- question1 %>% 
  group_by(Country)

# count the number of years with coverage above 0.90 for each country
bcg_data_count <- bcg_data_grouped %>% summarize(count = sum(Coverage > 0.90))

# sort the data in descending order by the number of years with coverage above 0.90
bcg_data_sorted <- bcg_data_count %>% arrange(desc(count))

# select the top country
question3 <- bcg_data_sorted %>% slice(1:10)
print(question3)



# filter the data to include only years after 2000
bcg_data_filtered2 <- question1 %>% 
  filter(Year > 2000)

# group the data by country
bcg_data_grouped2 <- bcg_data_filtered2 %>% 
  group_by(Country)

# count the number of years with coverage above 0.90 for each country
bcg_data_count2 <- bcg_data_grouped2 %>% summarize(count = sum(Coverage > 0.90))

# sort the data in descending order by the number of years with coverage above 0.90
bcg_data_sorted2 <- bcg_data_count2 %>% arrange(desc(count))

# select the top country
question4 <- bcg_data_sorted2 %>% slice(1:11)
print(question4)




# function to calculate the maximum sequence count for a vector
max_seq_count <- function(x) {
  rle_x <- rle(x > 0.9)
  max_seq <- max(rle_x$lengths[rle_x$values])
  return(max_seq)
}

# calculate the maximum sequence count for each country
max_seq <- aggregate(question1$Coverage, by = list(question1$Country), FUN = max_seq_count)
colnames(max_seq) <- c("Country", "MaxSequenceCount")

# sort the results by MaxSequenceCount in descending order
max_seq_sorted <- max_seq[order(-max_seq$MaxSequenceCount),]

# select the top 10 countries with the highest MaxSequenceCount
max_seq_sorted1 <- head(max_seq_sorted, 10)

# create new data frame with top 10 countries
question5 <- data.frame(Country = max_seq_sorted1$Country,
                        MaxSequenceCount = max_seq_sorted1$MaxSequenceCount)

# print the result
print(question5)