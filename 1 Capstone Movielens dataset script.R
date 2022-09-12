
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(visreg)) install.packages("visreg", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(tidytext)) install.packages("tidytext", repos = "http://cran.us.r-project.org")
if(!require(textrecipes)) install.packages("textrecipes", repos = "http://cran.us.r-project.org")
if(!require(textfeatures)) install.packages("textfeatures", repos = "http://cran.us.r-project.org")
if(!require(LiblineaR)) install.packages("LiblineaR", repos = "http://cran.us.r-project.org")
if(!require(doParallel)) install.packages("doParallel", repos = "http://cran.us.r-project.org")
if(!require(vip)) install.packages("vip", repos = "http://cran.us.r-project.org")
if(!require(skimr)) install.packages("skimr", repos = "http://cran.us.r-project.org")
theme_set(theme_classic())

#Load the packages required (in Linux #use readxl instead of xlsx).
library(visreg)
library(readr)
library(tidyverse)
library(caret)
library(data.table)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(lubridate)
library(tidytext)
library(textrecipes)
library(textfeatures)
library(LiblineaR)
library(doParallel)
library(vip)
library(skimr)
                            

#Preparation of the datasets
ratings <- read.csv("ratings.csv")
head(ratings)
str(ratings)

movies <- read_csv("movies.csv")
head(movies)
class(movies)

#check if the object is a data frame and coerce as a dataframe if necessary, then use mutate 
#to change existing variables
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

#join the datasets then create edx.csv file
edx <- left_join(ratings, movies, by = "movieId")
head(edx)
write.csv(edx, file = "edx.csv")


#Load the file named edx.csv, give instructions to omit Na's, and read the first lines.

edx <- read.csv("edx.csv")
edx <- subset(edx, select = -c(X.1, X,...1 ))
edx <- na.omit(edx)
head(edx)


#The scope of this project is to predict rating, thus I immediately plot rating to see how it looks like. There are more high ratings than low ones, and "half ratings" are used less.

edx %>% 
  ggplot(aes(rating))+
  geom_histogram(bins = 15)


#The following code comes from the capstone exercise and tells how many unique users the dataset contains (69878); this is relevant to understand if some users have voted many movies. In this case, will this aspect be relevant for the model?
  #unique users 
length(edx$userId)
n_distinct(edx$userId)

#some users made a lot of reviews, the "reviewer_weigth" variable illustrates them
reviewer_weigth <- edx %>% 
  select(userId, rating, title) %>% 
  group_by(userId) %>% 
  summarise(total_rating=n())%>% 
  arrange(desc(total_rating))
head(reviewer_weigth)

plot(reviewer_weigth)


#Boxplot and histogram represent the users who rated more movies.These results show that just a few users rated a really high number of movies (such as those who rated 6616 movies), and most users rated less than 250 movies.

#Boxplotof total ratings for each userID
boxplot(reviewer_weigth$total_rating,
        col="red",
        main ="Boxplot of IDs rating more movies - totals ")

#Histogram
hist(reviewer_weigth$total_rating,
     col= "lightblue",
     main= "IDs who rated more movies - totals",
     xlab= "user IDs",
     ylab= "ratings_total",
     labels = TRUE)


#After exploring users, I check ratings, and the top three most used ratings were 4 stars (2588430), then 3 stars (2121240), 5 stars (1390114).

edx_totratings <- edx %>% group_by(rating) %>%
  summarize(count = n()) %>% 
  arrange(desc(count))
head(edx_totratings)

#The bar chart represents in details such results. My takeaway is that some users were benevolent so I wonder if these ratings were given to the same movies, or maybe the same genres? I create an histogram representing movies that received more rating assessments, and I identify the titles of such movies. Hover on chart to see details.

gedxratings <- edx_totratings %>% mutate(rating = factor(rating)) %>%
  ggplot(aes(rating, count)) +
  geom_col(fill = "lightblue", color = "black") +
  theme_dark() + 
  labs(x = "Ratings (range)", y = "Count",
       title = "Ratings Range Grouped",
       caption = "Figure 1 - edx dataset ratings")
ggplotly(gedxratings)

movie_ratings <- edx %>% 
  count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins=20, color = "black", fill = "lightblue") +
  scale_x_log10() + 
  ggtitle("# of Ratings for each Movie Id") +
  xlab("Movie IDs") +
  ylab("# of Ratings")+
  theme_dark()
movie_ratings

edx %>% group_by(movieId, title) %>%
  summarize(count = n()) %>% 
  top_n(10) %>%
  arrange(desc(count))

#Some movies received many more ratings than others, now I investigate genres. Drama (3910127) and comedy (3540930) are the most rated, but we need to further clean the genres variable since some of them are grouped.


genres = c("Drama", "Comedy", "Thriller", "Romance")
sapply(genres, function(g) {
  sum(str_detect(edx$genres, g))
})



# the year released is attached to the title variable, I extract the year and I check if the most rated movies were released in some specific years, or maybe some genres were most popular in certain years.The new dataset edx2 contains the new variable named "year_released".

edx2 <- edx %>% mutate(year_released = as.numeric(str_extract(str_extract(title, "[/(]\\d{4}[/)]$"), regex("\\d{4}"))),title = str_remove(title, "[/(]\\d{4}[/)]$"))
head(edx2)

#After extracting the year released, I split the number of movies for each year. Looks like most movies have been released during the 90s, so I plot this information to illustrate yearly movie trends.The graphic confirms there was a peak of movies released around the 90s. This is an element I will consider when approaching the choice of the models I will use.

movies_per_year <- edx2 %>%
  select(movieId, year_released) %>% # I need movieId and year_released variables
  group_by(year_released) %>% # group_by to collect them by year
  summarise(count = n())  %>% # summarise/count to sum movies per year
  arrange(desc(count))# to see them in order from top released year
movies_per_year

ggmovies_per_year <- movies_per_year %>%
  ggplot(aes(x = year_released, y = count)) +
  geom_line(color="white")+
  theme_dark()
ggmovies_per_year


#Here I understand that dates are important but I must correct some of them.

edx2 <- mutate(edx2, year_rated = year(as_datetime(timestamp)))

release <- stringi::stri_extract(edx$title, regex = "(\\d{4})", comments = TRUE) %>% as.numeric()
edx_age <- edx2 %>% mutate(release_date = year_released) %>% select(-timestamp) #change the name of the variable to distinguish from the previous one and rename the new dataset
head(edx_age)


edx_age %>% 
  filter(release_date < 1900) %>% #filter release dates
  group_by(movieId, title, release_date) %>% #group the variables
  summarize(n = n())


edx_age[edx_age$movieId == "4311", "release_date"] <- 1998 #remove wrong dates after 2000
edx_age[edx_age$movieId == "5472", "release_date"] <- 1972
edx_age[edx_age$movieId == "6290", "release_date"] <- 2003
edx_age[edx_age$movieId == "6645", "release_date"] <- 1971
edx_age[edx_age$movieId == "8198", "release_date"] <- 1960
edx_age[edx_age$movieId == "8905", "release_date"] <- 1992
edx_age[edx_age$movieId == "53953", "release_date"] <- 2007

# fix out of range dates
edx_age %>% filter(release_date > 2020) %>% group_by(movieId, title, release_date) %>% summarize(n = n())

edx_age[edx_age$movieId == "27266", "release_date"] <- 2004 #remove remaining wrong dates 
edx_age[edx_age$movieId == "671", "release_date"] <- 1996
edx_age[edx_age$movieId == "2308", "release_date"] <- 1973
edx_age[edx_age$movieId == "4159", "release_date"] <- 2001
edx_age[edx_age$movieId == "5310", "release_date"] <- 1985
edx_age[edx_age$movieId == "8864", "release_date"] <- 2004
edx_age[edx_age$movieId == "1422", "release_date"] <- 1997


edx_age <- edx_age %>% 
  mutate(age_movie = 2022 - release_date, rating_age = year_rated - release_date) #update new age


#Now that release dates are clean, I wonder: "do movies with the highest number/count of rating have higher ratings? I check the previous table I have created with the top 10 movies and I use movieId to calculate their average ratings.I get 4.154789 for"Pulp Fiction" and 4.012822 for "Forrest Gump" . T


pulp_fiction <- edx2 %>% 
  select (movieId, rating, title, genres) %>% 
  filter (movieId == 296) %>% #here I filer Pulp Fiction
  summarise(avg = mean(rating)) %>%
  arrange(avg)
pulp_fiction

forrest_gump <- edx2 %>% 
  select (movieId, rating, title, genres) %>% 
  filter (movieId == 356) %>% #here I filer Forrest Gump
  summarise(avg = mean(rating)) %>%
  arrange(avg)
forrest_gump


#Thus, I use movieId to pull out average ratings of top 10 movies.


eamean_movie <- edx2 %>% 
  select (movieId, rating, title, genres) %>% 
  filter (movieId %in% c(296, 356, 593, 480, 318, 110, 457, 589, 260, 150)) %>% #these are the IDs of the top 10 movies receiving more ratings
  group_by(movieId) %>% 
  summarise(avg = mean(rating)) %>%
  arrange(avg)
eamean_movie
plot(eamean_movie)


#I want to compare the mean of all the movies in the dataset. Since the top 10 movies together have an average rating of 4.063742; the average ratings of all the movies in the dataset is 3.512465. This is the most interesting aspect to me, and I will use these results to choose my algorithm model.

mean(edx$rating) #calculate the mean of all the movies in the dataset


#In the next chart I use the mean rating of all movies and the average age. This way I understand whether the movie's age increases or decreases ratings.The scatterplot shows that the oldest movies have higher ratings compared to most recent ones.

edx_avg_age <- edx_age %>% group_by(age_movie) %>% summarize(age_rating_avg = mean(rating)) #create average age of all movies and assign the new variable to edx_avg_age

edx_avg_age %>%
  ggplot(aes(age_rating_avg, age_movie)) +
  geom_point(aes(color=age_movie)) +
  ggtitle("Average Ratings compared to Average Age")+
  theme_gray()


#At this point I recall that a lot of movies have been released in the 90's, and for the purpose of this project, I want to check if some genres have been released in particular years, for example whether customers' preferences had seasonal trends. So, first I clean the genres variable since some of these are grouped together, then I pull out the correct release year.

genres_df <- edx_age  %>%
  separate_rows(genres, sep = "\\|") %>% #tell R to use sep to separate genres
  group_by(genres) %>% #group the genres I have split
  summarise(number = n()) %>% #sum the number of genres
  arrange(desc(number))
head(genres_df, 10)



genres_per_year <- edx_age %>%
  select(genres, year_released) %>% # genres and year_released are selected from edx_age dataset
  group_by(genres, year_released) %>% # group by year released
  summarise(count = n())  %>% # count how many movies were released every year
  arrange(desc(count))
head(genres_per_year, 10)


#Modeling approach 1 (we must provide at least 2 models).*

edx_reduced <- edx %>% select(movieId, rating, userId) %>% #select the numeric variables I want to investigate
  group_by(movieId, userId) %>% 
  summarise(rating = mean(rating)) %>% #assign rating variable to the mean rating of the movies selected
  top_n(10) #pick the top 10 movies

head(edx_reduced)
print(edx_reduced, n=10)

#The formula for linear regression is mod \<- lm(y \~ x, my_data)

#The formula to make predictions is pred \<- predict(mod, my_data)

#The formula to calculate RMSE is sqrt(mean(error \^ 2))

#I will fit my linear model, make my prediction, and calculate errors using the formula errors = predicted - actual(Mayer & Kuhn, n.d.)


model <- lm(rating ~ ., edx_reduced) #this is the formula
model

#ow I use the same dataset to compute "Out-of-sample" RMSE for linear regression. This is important because it tells me how my model performs on new data. I randomly order my data and then split the dataset using train/test functions; this process is often compared to "shuffling decks of playing cards" before playing. The train and split functions are also very important to avoid over optimistic predictions (overfitting) after using the same dataset (Mayer & Kuhn, n.d.). "The model is accurate when the error rate for the training and test dataset is low" (Theobald, 2017 p. 48).

set.seed(42) #set a random seed

rows <- sample(nrow(edx_reduced)) #the sample function shuffles row indices in the edx_reduced dataset I have previously created


edx_age_reduced <- edx_reduced[rows, ] # randomly reorder data


#There are many ways to train/test and split data (also called "split validation"). Theobald (2017, p. 46) recommends 70/30 or 80/2, but we should also consider the size of the dataset, so there is not a fixed rule. The instructions of the Capstone exercise required that "Validation set to be 10% of the whole data", so I will use 90/10.

split <- round(nrow(edx_reduced) * 0.90) # use the split function to tell the percentage to split

train <- edx_reduced[1:split, ] # Create train

test <- edx_reduced[(split + 1):nrow(edx_reduced), ] # Create test

#To predict on test set I have split edx_reduced using the split function to train and test, then I use the lm() function for model fitting only on the training dataset (instead of the whole dataset).

#In R, the predict() function predicts the model on new data - the test dataset - because this has not been used for training the model. This way I obtain the error for the out-of-sample model; then, I use the error for RMSE's formula (sqrt(mean(error\^2))).

model <- lm(rating ~ ., train) # regression formula to train model

p <- predict(model, test) # assign prediction to "p" and predict using test

error <- p - test[["rating"]] #apply formula errors = predicted - actual, thus errors between the predicted rating and the actual rating


sqrt(mean(error^2)) #RMSE formula, the next result is our RMSE


#According to the above RMSE the model is accurate.

#**Model Number 2 Kmeans** 

#k-Means is an unsupervised clustering model that groups similar data points. The method splits data in k groups and it is helpful to discover new patterns or similarities, or disclose information about the number of clusters identified (Theobald, 2017, p. 72). I am using it following the lofica of the previous analysis related to the top 10 movies, those receiving most ratings. The next model runs using the tidymodels package (Silge & Kuhn, 2022). This code takes some time to run; in order to see how it works I recommend running just a part of the dataset.
#Do not run if you have the other dataset loaded from EDA, but if you want to run this faster, then select fewer rows, for example 10000 such as below
edx <- read.csv("edx.csv", nrows = 10000)


#reload edx_reduced with fewer observations
edx_reduced <- edx %>% select(movieId, rating, userId) %>% #select the variables we have previously identified
  group_by(movieId, userId) %>%
  summarise(rating = mean(rating)) %>%
  top_n(10) 


#In Tidymodels, recipes are used to prepare data we will use (feature engineering). The extraction method named Principal Component Analysis (PCA) is an unsupervised method and it combines new features with the predictors we originally used. PCA's new features are not correlated each other and they berform better when variables are normalized. The next code implies that we have already performed EDA as we did at the beginning of this analysis (Silge & Kuhn, 2022, chapter 8).


edx_rec <- recipe(~ ., data = edx_reduced) %>% 
  step_normalize(all_predictors()) %>% #normalize variables 
  step_pca(all_predictors(), num_comp = 2, id = "pca")

# Print out recipe
edx_rec


#I call prep() to estimate the statistics required by PCA and I apply them to a new variable named "features_2d_edx" I call bake(new_data = NULL) to get fitted PC transformation of "features_2d_edx"

edx_estimates <- prep(edx_rec) #the function prep() estimates the necessary statistics and applies them to the new variable "edx_estimates"


features_2d_edx <- edx_estimates %>% # the function bake(new_data = NULL) returns preprocessed data
  bake(new_data = NULL)

features_2d_edx %>% # Print baked data 
  slice_head(n = 5)

#Components containing more information (i.e. variance); "pca_estimates" return each component's variance (RPubs, 2021).

edx_estimates %>% 
  tidy(id = "pca", type = "variance") %>% #variance for each component in original variables
  filter(str_detect(terms, "percent"))

theme_set(theme_dark())
# Plot PC variance 
edx_estimates %>% 
  tidy(id = "pca", type = "variance") %>% 
  filter(terms == "percent variance") %>% 
  ggplot(mapping = aes(x = component, y = value)) +
  geom_col(fill = "white", alpha = 0.7) +
  ylab("% of total variance")

#Plot of PC scores
features_2d_edx %>% 
  ggplot(mapping = aes(x = PC1, y = PC2)) +
  geom_point(size = 0.5, color = "white")


#kmeans() built-in function runs after using numeric values having the same scale
edx_features<- recipe(~ ., data = edx_reduced) %>% 
  step_normalize(all_predictors()) %>% #normalize data 
  prep() %>% 
  bake(new_data = NULL)

# Print to see data
edx_features %>% 
  slice_head(n = 5)

#Create model, at this point I still do not know the ideal number of clusters
#set.seed(2056)
# Create 10 models with 1 to 10 clusters
kclusts <- tibble(k = 1:10) %>% 
  mutate(
    model = map(k, ~ kmeans(x = edx_features, centers = .x, nstart = 20)), #use map to replace for loops
    glanced = map(model, glance)) %>% 
  unnest(cols = c(glanced))

# See kclusts
kclusts


#Plot to obtain an elbow curve showing the ideal number of clusters using the Total within-cluster sum of squares (WCSS) method (tot.withinss)\`. The result shows a change at the 4th point, meaning that the optimal clusters are 4 (I tried and this result is similar if you run the whole edx_reduced dataset or 100000 observations).

kclusts %>% 
  ggplot(mapping = aes(x = k, y = tot.withinss)) +
  geom_line(size = 1.2, alpha = 0.5, color = "white") +
  geom_point(size = 2, color = "white")

#Now I use K-Means with k = 4 clusters as per previous elbow plot

set.seed(2056)
final_kmeans <- kmeans(edx_features, centers = 4, nstart = 100, iter.max = 1000)

results <- augment(final_kmeans, edx_features) %>% #prediction is added 
  bind_cols(features_2d_edx)  # bind columns pca_data - features_2d_edx

results %>% 
  slice_head(n = 5) #see results

#Visualize clusters using plotly package to so hover and see data together with clusters
cluster_plot <- results %>% 
  ggplot(mapping = aes(x = PC1, y = PC2)) +
  geom_point(aes(shape = .cluster), size = 2) +
  scale_color_manual(values = c("darkorange","purple","cyan4"))

ggplotly(cluster_plot)

#If you zoom you can see the 4 clusters represented by the different shapes. 
#These are the group of movies that are similar. One group at the top and three more 
#similar ones at the bottom



