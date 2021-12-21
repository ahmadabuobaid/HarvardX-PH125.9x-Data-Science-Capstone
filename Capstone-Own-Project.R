## Ahmad Abu Obaid
## Fragrance Recommendation System Project
## HarvardX: PH125.9x - Data Science: Capstone


#### Introduction ####

## Dataset ##

# Note: this process could take a couple of minutes
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")

# Loading library 
library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(stringr)
library(ggplot2)
library(knitr)
library(kableExtra)
library(scales)

# Fragrances - Notes and User Rating dataset:
# https://www.kaggle.com/sagikeren88/fragrances-and-perfumes
# https://github.com/ahmadabuobaid/HarvardX-PH125.9x-Data-Science-Capstone/raw/main/archive.zip

## Import the Data
# Downloading the Dataset
# Note: this process could take a couple of minutes
db <- tempfile()
download.file("https://github.com/ahmadabuobaid/HarvardX-PH125.9x-Data-Science-Capstone/raw/main/archive.zip", db)
unzip(db)

# Loading the Dataset
perfume_tb <- read.csv("perfume.csv")

## Exploring and reading the Dataset

# observing first few rows of Dataset
head(perfume_tb)

# here is the structure of Dataset
str(perfume_tb)

# Summarizing Dataset
summary(perfume_tb)

# Knowing all column names
colnames(perfume_tb)

# Cleaning the data for analysis
# Dataset have 86 Columns, we want make a subset from it with only 11 Columns to works on it in this project
# selecting the specific useful columns for analysis

trimmed_perfume_tb <- perfume_tb %>% select(brand,title,date,accords,rating_score,votes,Ihaveit,Ihadit,Iwantit,gender)

# replace empty cells and spaces cell with NA
trimmed_perfume_tb[trimmed_perfume_tb == "" | trimmed_perfume_tb == " "] <- NA

# check null values in dataset
map_df(trimmed_perfume_tb, ~sum(is.na(.x)))

count(trimmed_perfume_tb)

# Dataset have NA values and it is not clean and some of data is not consistent.
trimmed_perfume_tb$accords[is.na(trimmed_perfume_tb$accords)] <- "NotDefined"
trimmed_perfume_tb$gender[is.na(trimmed_perfume_tb$gender)] <- "NotDefined"

# as the First fragrance and personal care product company was in America and founded in 1752,
# date column is not have a valid values.
# we will remove any rows that have date < 1752 or date > 2021 .
# which is mean that we will drop 12457 rows.
trimmed_perfume_tb$date[trimmed_perfume_tb$date < 1752 | trimmed_perfume_tb$date > 2021] <- NA

sum(is.na(trimmed_perfume_tb$date))

trimmed_perfume_tb <- trimmed_perfume_tb %>% drop_na()
count(trimmed_perfume_tb)
# after we drop NA rows the dataset now have 38755 rows
# now we want to show how much unique values we have in each column
unique_count <- trimmed_perfume_tb %>% summarize(n_brand= n_distinct(brand), n_title=n_distinct(title), n_date=n_distinct(date), n_accords=n_distinct(accords), n_rating_score=n_distinct(rating_score), n_votes=n_distinct(votes), n_Ihaveit=n_distinct(Ihaveit), n_Ihadit=n_distinct(Ihadit), n_Iwantit=n_distinct(Iwantit), n_gender=n_distinct(gender))
unique_count

# after investigate duplicates columns, we found the same fragrance title with different date and accords and voting, so we can deal with it as a new fragrance.
# to make the fragrance unique we need to number the rows
trimmed_perfume_tb <- trimmed_perfume_tb %>% mutate(id = row_number())

# to re allocate id column to be first column
trimmed_perfume_tb <- trimmed_perfume_tb %>% relocate(id, .before = brand)

#### Exploratory Methods and Analysis ####

### Data Analysis ###

# Explore perfume_set subset data
str(trimmed_perfume_tb)
head(trimmed_perfume_tb)
summary(trimmed_perfume_tb)

# number of unique Brands and Perfumes in perfume_set subset data
trimmed_perfume_tb %>% 
  summarize(brand_count = n_distinct(brand),
            pefume_count = n_distinct(id))

# check if there is any NA value in perfume_set subset data
anyNA(trimmed_perfume_tb)

# trimmed_perfume_tb subset data Ratings distribution
trimmed_perfume_tb %>%
  ggplot(aes(rating_score)) +
  geom_histogram(binwidth = 0.25, color = ("black")) +
  scale_x_discrete(limits = c(seq(0,5,0.5))) +
  labs(x = "Rating", y = "Count", caption = "Source: perfume subset data") +
  ggtitle("Purfumes Rating distribution")

# Voting distributions
rat <- group_by(trimmed_perfume_tb, round(rating_score,digits = 1))
purf_rat <- summarise(rat,  count=sum(votes))
rat_score<- arrange(purf_rat, desc(0))
names(rat_score)[names(rat_score)=="round(rating_score, digits = 1)"] <- "Rating"
ggplot(rat_score, aes(x = Rating, y = count))+
  geom_bar(stat = 'identity', width = 0.1, col = "black", fill = "blue") +
  labs(x = "Rating", y = "Votes", title = "Voting count Vs Rating")+
  scale_y_continuous(breaks = c(0,100000,200000,300000,400000,500000,600000),labels = c("0","100K", "200K","300K","400K","500K","600K"))+
  scale_x_discrete(limits = c(seq(0.5,5,0.5)))

# Average of Ratings Over the Years
rate_year_avg <- aggregate(trimmed_perfume_tb$rating_score, by=list(Year=trimmed_perfume_tb$date),FUN=mean,na.rm=TRUE)
#Rename column x in rate_year_avg
names(rate_year_avg)[names(rate_year_avg)=="x"] <- "Average_Rating"
plot(rate_year_avg,type="o",col = "blue", xlab = "Year", ylab = "Average Rating",main = "Average Ratings Over the Years")

# number of perfumes per brand in perfume_set subset data
# Checking which brand have more perfumes and taking the top 10
select_brand <- trimmed_perfume_tb %>% group_by(brand) %>% summarize(count=n()) %>% arrange(desc(count)) %>% top_n(10)
ggplot(select_brand, aes(x = reorder(brand, count), y = count))+
  geom_bar(stat = 'identity', width = 0.7, col = "black", fill = "blue") +
  labs(x = "Brand", y = "Count", title = "Top 10 Brands with more purfumes")+
  coord_flip()

# With a boxplot we're going to see the relation between brand and their ratings for the brands which have more perfumes.
select_top_brand = as.character(select_brand$brand)
score_select_brand <-  trimmed_perfume_tb %>% filter(brand %in% select_top_brand) %>% select(brand, rating_score) %>% arrange(brand)
ggplot(score_select_brand,aes(x = reorder(brand,rating_score), y = rating_score)) +
  geom_boxplot(aes(fill = brand)) +
  labs(x='Brand',y='Rating')

# Brand produces the highest rate perfume on average which is perfume count > perfume mean count for the same brand
# we cant include rate for brands have 1 or little count of perfumes.
# we will take only the top 10
bra <- group_by(trimmed_perfume_tb, brand)
purf <- summarise(bra,  count=n(),
                  rate1= mean(rating_score)) %>% filter(count > mean(count) )
bra_score<- arrange(purf, desc(rate1)) %>% top_n(10)

ggplot(bra_score,aes(x=reorder(brand,rate1), y=rate1)) +geom_point(aes(size=count, colour=factor(rate1)), alpha=1/2) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1) , legend.position="none") +
  labs(x="Brand", y="Purfume Rating",title="Top 10 brands which produced the highest rate perfume on average")

# Perfume Gender distribution
ggplot(trimmed_perfume_tb, aes(x=gender,fill=rating_score))+geom_bar(fill="indianred3")+ labs(x="Sex",title = "Perfume Gender distribution")+
  scale_y_continuous(breaks = c(0,5000,10000,15000,20000,25000,30000,35000)) +
  theme_minimal(base_size=10)

# as we see, accords column have multiple values, one fragrance have multiple accords, we want to investigate this column.
Acc <- trimmed_perfume_tb %>% 
  separate_rows(accords, sep = "\\,") %>%
  group_by(accords) %>%
  summarize(PerfumeCount=n_distinct(id))
Acc

Acc2 <- trimmed_perfume_tb %>% 
  separate_rows(accords, sep = "\\,") %>%
  group_by(accords) %>%
  summarize(MeanRating=mean(rating_score))
Acc2

# As we're interested how accords can affect ratings,
# let us make a visualization of ratings per accords:
# we will see that the ratings per accords is not evenly distributed
plot_Acc2 <- Acc2 %>% ggplot(aes(x = accords, y=MeanRating)) +
  geom_bar(stat="identity", colour = "darkblue", fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 90, vjust=0.25, hjust = 1)) +
  labs(title = "Accords Vs Mean Ratings",
       x = "Accords", y = "Mean Rating")
plot_Acc2

# let us make a visualization of perfumes per accords:
# we will see that the perfumes per accords is not evenly distributed
plot_Acc <- Acc %>% ggplot(aes(x = accords, y=PerfumeCount)) +
  geom_bar(stat="identity", colour = "darkblue", fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 90, vjust=0.25, hjust = 1)) +
  labs(title = "Accords Distribution",
       x = "Accords", y = "Perfume Count")
plot_Acc

### Modeling ###

# RMSE Function to calculate (root mean square error)
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2, na.rm = TRUE))
}

# modifying accords column
# we separate accords information using separate_rows function for all out dataset that we use:
trimmed_perfume_tb_final <- trimmed_perfume_tb %>% separate_rows(accords, sep = "\\,")

#This caused an amplification of the total number of rows:
bf_train_set <- data.frame(Dataset = "trimmed_perfume_tb", Rows = length(trimmed_perfume_tb$rating_score))
af_train_set <- data.frame(Dataset = "trimmed_perfume_tb_final", Rows = length(trimmed_perfume_tb_final$rating_score))
union_set <- rbind(bf_train_set,af_train_set)
union_set
rm(union_set,bf_train_set,af_train_set)

# Validation set will be 10% of trimmed_perfume_tb_final data
set.seed(3, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index1 <- createDataPartition(y = trimmed_perfume_tb_final$rating_score, times = 1, p = 0.1, list = FALSE)
perfume_set1 <- trimmed_perfume_tb_final[-test_index1,]
temp1 <- trimmed_perfume_tb_final[test_index1,]
# Make sure Id in validation subset are also in perfume_set subset
validation_set1 <- temp1 %>% semi_join(perfume_set1, by = "id")

# Add rows removed from validation set back into perfume_set set
removed <- anti_join(temp1, validation_set1)
perfume_set1 <- rbind(perfume_set1, removed)

# Remove temporary files to tidy environment
rm(db,trimmed_perfume_tb_final,test_index,temp1,removed)

## Model1: Average perfumes rating (Calculate the overall average rating across all perfumes in perfume_set_final subset data) ##
mu <- mean(perfume_set1$rating_score)

# Calculate RMSE between each rating included in validation subset and the overall Average perfume rating
Model1_rmse <- RMSE(validation_set1$rating_score, mu)
Model1_rmse

rmse_results <- data_frame(method = "Model1: Average Perfume rating (Calculate the overall average rating across all perfume)", RMSE = Model1_rmse)

## Model2: taking into account the brand effect ## 
# Calculate b_i (Estimate brand effect b_i)
brand_avgs <- perfume_set1 %>%
  group_by(brand) %>%
  summarise(b_i = mean(rating_score - mu))

# Predict ratings adjusting for brand effects
predicted_b_i <- mu + validation_set1 %>%
  left_join(brand_avgs, by = "brand") %>%
  pull(b_i)
# Calculate RMSE based on brand effects model
Model2_rmse <- RMSE(predicted_b_i, validation_set1$rating_score)
Model2_rmse

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Model2: taking into account the brand effect",  
                                     RMSE = Model2_rmse ))


## Model3: taking into account the brand and votes effect ## 
# Calculate b_u (Estimate votes effect b_u)
votes_avgs <- perfume_set1 %>%
  left_join(brand_avgs, by = "brand") %>%
  group_by(votes) %>%
  summarise(b_u = mean(rating_score - mu - b_i))
# Predict ratings adjusting for brand and votes effects
predicted_b_u <- validation_set1 %>%
  left_join(brand_avgs, by="brand") %>%
  left_join(votes_avgs, by="votes") %>%
  mutate(pred =  mu + b_i + b_u) %>%
  pull(pred)
# Calculate RMSE based on votes effects model
Model3_rmse <- RMSE(predicted_b_u, validation_set1$rating_score)
Model3_rmse

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Model3: taking into account the brand and votes effect",  
                                     RMSE = Model3_rmse ))


## Model4: Using Model3 taking into account the accords effect ## 
# Calculate b_g (Estimate accords effect b_g)
accords_avgs <- perfume_set1 %>%
  left_join(brand_avgs, by="brand") %>%
  left_join(votes_avgs, by="votes") %>%
  group_by(accords) %>%
  summarise(b_g = mean(rating_score - mu - b_i - b_u))
# Predict ratings adjusting for brand, votes and accords effects
predicted_b_g <- validation_set1 %>%
  left_join(brand_avgs, by="brand") %>%
  left_join(votes_avgs, by="votes") %>%
  left_join(accords_avgs, by = "accords") %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  pull(pred)
# Calculate RMSE based on accords effects model
Model4_rmse <- RMSE(predicted_b_g, validation_set1$rating_score)
Model4_rmse

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Model4: taking into account the the brand and votes and accords effect",  
                                     RMSE = Model4_rmse ))



##Regularized model: Using Model4 (taking into account the the brand and votes and accords effect) ##

lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(perfume_set1$rating_score)
  
  b_i <- perfume_set1 %>% 
    group_by(brand) %>%
    summarize(b_i = sum(rating_score - mu)/(n()+l))
  
  b_u <- perfume_set1 %>% 
    left_join(b_i, by="brand") %>%
    group_by(votes) %>%
    summarize(b_u = sum(rating_score - b_i - mu)/(n()+l))
  
  b_g <- perfume_set1 %>%
    left_join(brand_avgs, by = "brand") %>%
    left_join(votes_avgs, by = "votes") %>%
    group_by(accords) %>%
    summarise(b_g = sum(rating_score - mu - b_i - b_u)/(n()+l))
  
  predicted_ratings <- 
    validation_set1 %>% 
    left_join(b_i, by = "brand") %>%
    left_join(b_u, by = "votes") %>%
    left_join(b_g, by = "accords") %>%
    mutate(pred = mu + b_i + b_u + b_g) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, validation_set1$rating_score))
})

#The optimal lambda
lambda <- lambdas[which.min(rmses)]
lambda

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularizing Model: take into account brand and votes and accords effect",  
                                     RMSE = min(rmses)))


### Results ###
#The RMSE values of all the represented models are the following:
rmse_results
