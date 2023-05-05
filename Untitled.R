weather_df <- readRDS('/Users/namgunuk/R for DS/hw_5/weather-1.rds')


# Q1
'''
각 변수가 하나의 열에 위치해 있지 않다.
각 관측치가 하나의 행에 위치해 있지 않다.
'''

# Q2

str(weather_df)

weather_df <- weather_df[, -1]

# Q3



install.packages("tidyr")

library(tidyr)
library(dplyr)

weather_tidy <- weather_df %>% 
  gather(key = "dayOfMonth", value = "value", -year, -month, -measure) %>% 
  spread(key = measure, value = value)


# Q4

weather_tidy$dayOfMonth <- gsub("X", "", weather_tidy$dayOfMonth)

weather_tidy$dayOfMonth <- as.numeric(weather_tidy$dayOfMonth)

str(weather_tidy)

# Q5

weather_tidy <- unite(weather_tidy, date, c("year", "month", "dayOfMonth"), sep = "-")

weather_tidy$date <- as.Date(weather_tidy$date)


#----


# Q6

weather_tidy$PrecipitationIn <- gsub('T', 0, weather_tidy$PrecipitationIn)


weather_tidy$PrecipitationIn



# Q7

weather_tidy <- weather_tidy %>% mutate_at(vars(Max.Dew.PointF:WindDirDegrees), as.double)



weather_tidy$Events <- as.factor(weather_tidy$Events)

weather_tidy$CloudCover <- as.double(weather_tidy$CloudCover)

glimpse(weather_tidy)


# Q8

any(is.na(weather_tidy))

sum(is.na(weather_tidy))

colSums(is.na(weather_tidy))


# Q9

z_score_a <- abs(scale(weather_tidy$Max.Humidity))

outlier_a <- weather_tidy$Max.Humidity[z_score_a>3]

outlier_a

which(weather_tidy$Max.Humidity==1000)

weather_tidy$Max.Humidity[138] <- 100

weather_tidy$Max.Humidity[138]

#----

boxplot(weather_tidy$Max.Humidity, horizontal = T)
summary(weather_tidy$Max.Humidity)
weather_tidy[which.max(weather_tidy$Max.Humidity),"Max.Humidity"] <- 
  max(weather_tidy$Max.Humidity, na.rm = T) / 10


# Q10



boxplot(weather_tidy$Mean.VisibilityMiles, horizontal = T)
summary(weather_tidy$Mean.VisibilityMiles)


#-----

z_score_b <- abs(scale(weather_tidy$Mean.VisibilityMiles))

outlier_b <- weather_tidy$Mean.VisibilityMiles[z_score_b>3]

outlier_b

weather_tidy$Mean.VisibilityMiles <- ifelse(weather_tidy$Mean.VisibilityMiles < 0, NA, weather_tidy$Mean.VisibilityMiles)


weather_tidy$Mean.VisibilityMiles


# Q11
levels(weather_tidy$Events)
levels(weather_tidy$Events)[levels(weather_tidy$Events) == ""] <- "None"

head(weather_tidy$Events, 20)

weather_tidy$Events


# Q12

names(weather_tidy) <- tolower(names(weather_tidy))

glimpse(weather_tidy)


# Q13

save(weather_tidy, file = '/Users/namgunuk/R for DS/hw_5/weather_tidy.RData')










