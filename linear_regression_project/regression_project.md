---
title: "Modeling and prediction for movies"
output: 
  html_document: 
    fig_height: 4
    highlight: pygments
    theme: spacelab
---

## Setup

### Load packages

```{r load-packages, message = FALSE}
library(ggplot2)
library(dplyr)
library(statsr)
library(GGally)
library(tidyverse)
```

### Load data

```{r load-data}
load("movies.Rdata")
```



* * *

## Part 1: Data
The data set is a random sample of movies from IMDB and Rotten Tomatoes which includes the data regarding ratings, release dates, whether the movies were directed by Oscar winnig directors and more. We would like to dive into the data to reach some interesting conclusions and make predictions for other movies. The data set contains information about 651 randomly sampled movies released before 2016. There are 32 available variables.<br>
The data generalizable to the population of interest. The potential biases are associated with non-voting or no-rating because voting and rating are voluntary on IMDB and Rotten Tomatoes. <br>
The variables used in this analysis are listed below : <br>
title_type<br>
title<br>
genre<br>
runtime<br>
mpaa_rating<br>
thtr_rel_year<br>
thtr_rel_day<br>
thtr_rel_month<br>
imdb_rating<br>
best_actor_win<br>
best_actress_win<br>
best_director_win<br>

* * *

## Part 2: Research question
Determine which variables can be used to predict the IMDB ratings while lookinng at the p-values associated with the variables and to predict IMDB movie ratings for select movies after building a liner regression model while looking at the Adjusted R-Squared values.

* * *

## Part 3: Exploratory data analysis

```{r}
str(movies)
```
Looking at the above table shows that there are many irrelevant variables which can be ignored. "best_actor_win" and "best _actress_win" can be grouped together as a single variable "best_actor". Changing the variable naming can improve the data set a lot. Making these changes : <br>
```{r}
movies <- movies %>%
  rename(type = title_type, length = runtime, maturity = mpaa_rating, year = thtr_rel_year, month = thtr_rel_month, day = thtr_rel_day, rating = imdb_rating, best_director = best_dir_win) %>% 
  mutate(best_actor= (best_actor_win =='yes' | best_actress_win =='yes')) %>%
  select(1:9, 13, 23, 33)

```

Let's have a look at some categorical variables in our data set:<br>
```{r}
summary(movies$type)
```
There are only 5 "TV movie" class which are likely lower in ratings compared to "Documentary" and "Feature Film". Hence we can remove this class.
```{r}
movies <- movies %>% 
  filter(type != "TV Movie")
movies$type = factor(movies$type)  #This gets rid of the now-empty level
```

Now, let's look at maturity:<br>
```{r}
summary(movies$maturity)
```
Most of the data is concentrated in "PG, PG-13 and R" category. The remaining data contributes to 69 points between the remaining classes which can't be ignored. Hence, it's best to combine this data into a single class called "Other":
```{r}
movies$maturity <- movies$maturity %>% 
  fct_collapse('Other'=c('G', 'NC-17', 'Unrated'))
```
Now, let's look at "studio" and see how mnay classes are in this:<br>
```{r}
movies$studio %>% 
  levels() %>% 
  length()
```
This number is too much for our current data set. So, we'll keep the 4 most abundant classes excluding (Other) collapsing it into (Other). So, the top 4 studios are:<br>
```{r}
movies$studio %>% 
  summary() %>%
  sort() %>%
  tail(5)
```
```{r}
#Collapsing the levels
levels(movies$studio) <- union(levels(movies$studio), 'Other')

movies$studio[!(movies$studio %in% c('Paramount Pictures', 'Warner Bros. Pictures', 'Sony Pictures Home Entertainment', 'Universal Pictures'))] = 'Other'
movies$studio <- factor(movies$studio)  #This gets rid of the now-empty levels
```

Now, our data set is in good form and ready for use.<br>
<br>
<b>Univariate Analysis</b>
```{r}
#The length of the movies around 80-125 minutes are most common
ggplot(aes(x = length), data = movies)+geom_bar()+scale_x_continuous(breaks = c(80, 125))+geom_vline(xintercept = c(80, 125), color = "blue")
```
```{r}
#Drama movies are the most watched in the genre class
ggplot(data = movies, aes(x = genre))+geom_bar()
```
```{r}
ggplot(data = movies, aes(x = best_director))+geom_bar()
```

```{r}
#R rated movies are the most watched content 
ggplot(data = movies, aes(x = maturity))+geom_bar()
```
```{r}
sort(summary(movies$genre))
```
As can also be seen from this table, "Drama" movies are the most common class of movies in our data set. However it's not clear how the genre is determined for each movie. We know that each movie on IMDB is classified under multiple genres. So, it can be assumed that this data set uses the first genre mentioned in genre's list on IMDB.<br>
Now, let's inspect the length variable and its potential outliers. We see some movies with length less than 50 minutes and more than 250 minutes. Let's see which movies are these:<br>
```{r}
movies %>%
  filter(length<50 | length>250)
```
As we see, these are all documentaries. We won't expect feature films to be this short or long. We won't eliminate these outliers as we will use these in the later stages.
<br>
<br>
<b>Bivariate analysis</b><br>

```{r}
ggplot(data = movies, aes(x = best_actor, y = rating))+geom_boxplot(fill='lightblue')+labs(x = "Best actor", y = "IMDB Rating", title = "Do best actors improve movie ratings?")
```
<br>As we can see from the boxplot, the points beneath the left boxplot and singular point above the right boxplot, it would be valid to say that movies with best actors are more likely to get more ratings compared to movies with no best actors. This is coincidental with out human intuition  which is probably true.<br> 
```{r}
ggplot(data = movies, aes(x = best_director, y = rating))+geom_boxplot(fill='lightblue')+labs(x = "Best director", y = "IMDB Rating", title = "Do best directors improve movie ratings?")
```
<br>As we can see from this boxplot, no movies with ratings below 5 have been directed by an Oscar winning director. And movies directed by Oscar winning directors have significantly higher ratings than movies directed by a non Oscar winning director.<br>
As we have seen, there is very less number of movies directed by Oscar winning directors, so we have to be careful of sample sizes while using this variable in linear regression section.

```{r}
#Comedy, drama and documentary are the focused genres in our analysis
ggplot(data = movies, aes(x = genre, y = rating))+geom_boxplot(fill='lightblue')+scale_x_discrete(breaks = c("Drama", "Comedy", "Documentary"))+labs(x = "Genre", y = "IMDB rating", title = "Genres preferred by movie-goers")
```
<br>As can be seen from this boxplot, Documentary genre movies are the highest rated movies on IMDB which was also seen in the TYPE graph. Also, Comedy is seen to be the lowest rated genre. 

```{r}
ggplot(data = movies, aes(x = maturity, y = rating))+geom_boxplot(fill='lightblue')+labs(x = "Maturity rating", y = "IMDB rating", title = "Movie ratings preferred by movie-goers")
```
<br>The boxplots for PG-13, PG and R movies are comparable to one another. There is however a noticeable jump when comparing them to the Other boxplot. Referring back to our univariate section we saw that 48 out of 69 (70%) of the Other movies were originally Unrated, so we can't really say too much about the specifics of this class. What we can say is that, due to this difference, maturity is a variable worth considering when building our linear regression model.

```{r}
ggplot(data = movies, aes(x = year, y = rating))+geom_point()+labs(x = "Theater release year", y = "IMDB rating", title = "Movie ratings overs the years")
```
<br>There is no obvious change in movie ratings over the years, and if we were to make charts comparing IMDB ratings to theater release dates by month or day of the month then we would arrive at a similar conclusion.<br>

```{r}
ggplot(data = movies, aes(x = length, y = rating))+geom_point()+geom_vline(xintercept = c(76, 130))+geom_hline(yintercept = 5.5)+geom_hline(yintercept = 5.5, color = "blue")+geom_vline(xintercept = c(76, 130), color = "blue")+scale_y_continuous(breaks = 5.6)+scale_x_continuous(breaks = c(76, 130))+labs(y = "IMDB rating", x = "Length of movie in minutes(runtime)", title = "Preference of movie-goers w.r.t runtime")
```
<br>As for the movie length plot, there is an empty box beyond 30 minutes with no rating less than 5.6 which means that people usually love watching longer movies.The movies that get poor ratings tend to be shorter, around 77-129 minutes in length. Movie length is positively correlated with movie rating.

```{r}
#Our main variables for prediction are used for making regression model
significant_predictors <- lm(rating ~ type + genre + day + month + best_director + length + maturity + studio + year + best_actor, data = movies)
summary(significant_predictors)
```

```{r}
signif(summary(significant_predictors)$coefficients[c(2, 7, 13:17), ], 3)
```
The significant predictors of rating(p-value < 0.05) : type, genre, length, maturity, best_director<br>
The very insignificant predictors of rating(p-value > 0.15) : studio, year, month, day<br>
The insignificant predictors of rating : best_actor<br>

* * *

## Part 4: Modeling


```{r}
significant_line = lm(rating ~ type + genre + length + maturity + best_director, data=movies)
summary(significant_line)
```
We see that the adjusted R-squared is 0.2984, which isn't spectacular but is still sufficient to offer a predictive edge over blind guessing movie quality. There are numerous encouraging details when we inspect the p-values and coefficient signs in this model.<br>
When searching for the most optimal subset of predictors for a model, it's tedious to construct all the models and extract the one with the best evaluation metric value due to exponential growth of number of models. As we have chosen 10 predictors, number of models increases to 2^10 = 1024 models.<br>
To build all 1024 models, we use all.possible.regressions() function.
```{r}

#The code for this function was found here:
#http://r.789695.n4.nabble.com/Generating-all-possible-models-from-full-model-td2222377.html.

all.possible.regressions <- function(dat, k){ 
    n <- nrow(dat) 
    regressors <- paste("x", 1:k, sep="") 
    lst <- rep(list(c(T, F)), k) 
    regMat <- expand.grid(lst); 
    names(regMat) <- regressors 
    formular <- apply(regMat, 1, function(x) 
            as.character(paste(c("y ~ 1", regressors[x]), collapse="+"))) 
    allModelsList <- apply(regMat, 1, function(x) 
            as.formula(paste(c("y ~ 1", regressors[x]),collapse=" + ")) ) 
    allModelsResults <- lapply(allModelsList, 
            function(x, data) lm(x, data=data), data=dat) 
    n.models <- length(allModelsResults) 
    extract <- function(fit) { 
        df.sse <- fit$df.residual 
        p <- n - df.sse -1 
        sigma <- summary(fit)$sigma 
        MSE <- sigma^2 
        R2 <- summary(fit)$r.squared 
        R2.adj <- summary(fit)$adj.r.squared 
        sse <- MSE*df.sse 
        aic <- n*log(sse) + 2*(p+2) 
        bic <- n*log(sse) + log(n)*(p+2) 
        out <- data.frame(df.sse=df.sse, p=p, SSE=sse, MSE=MSE, 
            R2=R2, R2.adj=R2.adj, AIC=aic, BIC=bic) 
        return(out) 
    } 
    result <- lapply(allModelsResults, extract) 
    result <- as.data.frame(matrix(unlist(result), nrow=n.models, byrow=T)) 
    result <- cbind(formular, result) 
rownames(result) <- NULL 
colnames(result) <- c("model", "df.sse", "p", "SSE", "MSE", "R2", 
"R2.adj", "AIC", "BIC") 
    return(result) 
}

#This function requires that our target variable be renamed y and our explanatory variables as x1, x2, x3, ... 
moviesALL = movies %>% 
  select(y=rating, x1=type, x2=genre, x3=length, x4=maturity, x5=studio, x6=year, x7=month, x8=day, x9=best_director, x10=best_actor)

#This is a dataframe that includes all 1024 models and their adjusted R-squared values, among other metrics.
apr = all.possible.regressions(moviesALL,10)
apr[345, ]
```
We are interested in the model with the highest R^2 adjusted value. Hence, below model shows the model with the highest adjusted R-squared value.
 
```{r}
m = which.max(apr$R2.adj)
tibble(model=apr$model[m], R2.adj=round(apr$R2.adj[m],4))
```
This model uses "type, genre, length, maturity, studio, year and best_director" as the predictors for modeling. Earlier, we had noted "year and studio" as to be an insignificant predictor for our model. But as seen from this model, it doesn't seem to be the case. This model uses all the significant predictors which we had mentioned earlier, which is encouraging. Let's now build this model and inspect its summary.
```{r}
best_line <- lm(rating~type + genre + length + maturity + studio + year + best_director, data=movies)
summary(best_line)
```

* * *

## Part 5: Prediction

For predictions, we take 4 movies :<br>
1. Mission Impossible - Fallout<br>
2. Birdman<br>
3. Despicable Me 3<br>
4. Dunkirk<br>
We then look up the feature values from IMDB website for each of these movies.

```{r}
#Gathering the data
test_movies <- tibble( 
  title = c("Mission Impossible - Fallout", "Birdman", "Despicable Me 3", "Dunkirk "),
  rating = c(8.1, 7.7, 6.3, 7.9), 
  type = as.factor(c("Feature Film", "Feature Film", "Feature Film", "Feature Film")),
  genre = as.factor(c("Action & Adventure", "Drama", "Action & Adventure", "Drama")),
  length = c(147, 119, 89, 106),
  maturity = as.factor(c("PG-13", "R", "PG", "PG-13")),
  studio = as.factor(c("Paramount Pictures", "Other",  "Universal Pictures", "Universal Pictures")),
  year = c(2018, 2014, 2017, 2017),
  best_director = as.factor(c("yes", "yes", "no", "yes"))
)

test_predictions <- predict(best_line, newdata = test_movies, interval = "predict") %>%
  round(1) %>%
  as.data.frame()

tibble(
  title = c("Mission Impossible - Fallout", "Birdman", "Despicable Me 3", "Dunkirk"),
  ratingIMDB = c(8.1, 7.7, 6.3, 7.9),
  ratingPredicted = test_predictions$fit,
  ratingDifference = ratingIMDB-ratingPredicted)
```
It's impossible to tell from just 4 test points how well our model has done. On the one hand, an error of 0.2 points out of 10 (2%) sounds reasonable, but on the other hand, an error of 1.5 out of 10 (15%) does not.<br>
To obtain a more trustworthy estimate for our out-of-sample adjusted R-squared we would need to acquire a bigger data set.

* * *

## Part 6: Conclusion

In this project we successfully identified 5 significant individual predictors of IMDB movie ratings: genre, length, maturity rating, whether or not the director has won an Oscar for any movie, and whether the movie is a feature film or a documentary. We built a linear regression model that uses these 5 explanatory variables to predict movie ratings with some success (adjusted R-squared: 0.2993). We then computed the adjusted R-squared for all possible models from our chosen set of 10 variables and selected the best one, which offered a slight improvement (adjusted R-squared: 0.3127). It was encouraging to see that all of the 5 significant individual predictors are included in this model. While the adjusted R-squared values are not very impressive, this has been the expected case since the beginning of this project given the small size of our dataset.<br>
If we could get our hands on a bigger dataset in the future then it might be worth further exploring the noticeable difference in movie ratings between feature films and documentaries.
