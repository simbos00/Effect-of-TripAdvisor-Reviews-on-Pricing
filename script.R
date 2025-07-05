rm(list=ls())
setwd('')
library(tidyverse)
library(readr)
library(rdd)
library(rddtools)
library(rddensity)
library(magrittr)
library(rdrobust)
data <- read.csv('data_rdd.csv')

# Histogram
data %>%
  ggplot(aes(x = score_adjusted)) +
  geom_histogram(binwidth = 0.1, fill = "SteelBlue")

#Showing it's a sharp design
ggplot(data, aes(x = score_adjusted, y = bubble_rating)) +
  geom_point(alpha = 0.5) +
  geom_vline(xintercept = 4.25, linetype = "dashed") +
  coord_cartesian(xlim = c(4, 4.5), ylim = c(3.5, 5)) +
  labs(title = "RDD Sharp Design", x = "Score Adjusted", y = "Bubble Rating") +
  theme_minimal()



### 1 - test if the hypothesis that the density of the running variable is continuous at the cutoff
# McCrary density test using rdd package
runvar <- data$score_adjusted
DCdensity(runvar, c = 4.25)

# McCrary density test using rddensity package
result <- rddensity(X = data$score_adjusted, c = 4.25)
summary(result)
rdplotdensity(result, runvar, type = 'both', pwd = , pcol = 1)



### 2 - Checking for a discontinuity in the outcome at the cutoff
#plot 1
rdplot(y = data$price_curr_min,
       x = data$score_adjusted,
       c = 4.25,
       kernel = "triangular",
       x.label = "Score Adjusted",
       y.label = "Current Price"
) 

#plot 2
rdplot(y = data$price_curr_min,
       x = data$score_adjusted,
       c = 4.25,
       kernel = "triangular",
       p = 1,
       x.label = "Score Adjusted",
       y.label = "Current Price"
       
) 
 #plot 3
rdplot(y = data$price_curr_min,
       x = data$score_adjusted,
       c = 4.25,
       kernel = "triangular",
       p= 2,
       x.label = "Score Adjusted",
       y.label = "Current Price"
       
) 

#plot 4
rdplot(y = data$price_curr_min,
       x = data$score_adjusted,
       c = 4.25,
       kernel = "triangular",
       p= 3,
       x.label = "Score Adjusted",
       y.label = "Current Price"
       
) 





#rdwb <- rdbwselect(y = data$price_curr_min,                   rdrobust gi? lo fa
         #x = data$score_adjusted,
         #c = 4.25,
         #kernel = "triangular")
#summary(rdwb)


rd_result <- rdrobust(y = data$price_curr_min,             #questo ? del plot 1 (di default rdplot p = 4)
                      x = data$score_adjusted,
                      c = 4.25,
                      kernel = "triangular",
                      p = 4
) 
summary(rd_result)


### 3 - Continuity of covariates


##class (stars of the hotel)
#ordering
data$class_clean <- as.numeric(ifelse(data$class == "no stars", 0, data$class))
data_sorted <- data[order(data$score_adjusted), ]

#binning parameters
bin_size <- 10
n <- nrow(data_sorted)
num_bins <- floor(n / bin_size)
class_bin_means <- numeric(num_bins)

#calculating the avarages
for (i in 1:num_bins) {
  start_index <- (i - 1) * bin_size + 1
  end_index <- i * bin_size
  bin_data <- data_sorted[start_index:end_index, ]
  class_bin_means[i] <- mean(bin_data$class_clean, na.rm = TRUE)
}

print(class_bin_means)


score_bin_means <- numeric(num_bins)

for (i in 1:num_bins) {
  start_index <- (i - 1) * bin_size + 1
  end_index <- i * bin_size
  bin_data <- data_sorted[start_index:end_index, ]
  score_bin_means[i] <- mean(bin_data$score_adjusted, na.rm = TRUE)
}

print(score_bin_means)


#building the dataframe
bin_df <- data.frame(
  score_adjusted_bin = score_bin_means,
  class_clean_bin = class_bin_means
)

#plot
cutoff <- 4.25
bin_df$side <- ifelse(bin_df$score_adjusted_bin < cutoff, "left", "right")

ggplot(bin_df, aes(x = score_adjusted_bin, y = class_clean_bin)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_smooth(data = subset(bin_df, side == "left"), method = "loess", se = FALSE, color = "blue") +
  geom_smooth(data = subset(bin_df, side == "right"), method = "loess", se = FALSE, color = "orange") +
  geom_vline(xintercept = cutoff, linetype = "dashed", color = "red") +
  labs(
    title = "Continuity check for class (stars) using LOESS (left/right)",
    x = "Score Adjusted (bin mean)",
    y = "Stars (bin mean)"
  ) +
  theme_minimal()



##views
#binning parameters
views_bin_means <- numeric(num_bins)

#calculating the avarages
for (i in 1:num_bins) {
  start_index <- (i - 1) * bin_size + 1
  end_index <- i * bin_size
  bin_data <- data_sorted[start_index:end_index, ]
  views_bin_means[i] <- mean(bin_data$views, na.rm = TRUE)
}

print(views_bin_means)

#building the dataframe
bin_df2 <- data.frame(
  score_adjusted_bin = score_bin_means,
  views_clean_bin = views_bin_means
)
bin_df2$side <- ifelse(bin_df$score_adjusted_bin < cutoff, "left", "right")

#plot
ggplot(bin_df2, aes(x = score_adjusted_bin, y = views_clean_bin)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_smooth(data = subset(bin_df2, side == "left"), method = "loess", se = FALSE, color = "blue") +
  geom_smooth(data = subset(bin_df2, side == "right"), method = "loess", se = FALSE, color = "orange") +
  geom_vline(xintercept = cutoff, linetype = "dashed", color = "red") +
  labs(
    title = "Continuity check for views using LOESS (left/right)",
    x = "Score Adjusted (bin mean)",
    y = "Views per Day (bin mean)"
  ) + 
  coord_cartesian(ylim = c(0, 5))+
  theme_minimal()


##Amenity Air Conditioning
ac_bin_means <- numeric(num_bins)

#calculating the avarages
for (i in 1:num_bins) {
  start_index <- (i - 1) * bin_size + 1
  end_index <- i * bin_size
  bin_data <- data_sorted[start_index:end_index, ]
  ac_bin_means[i] <- mean(bin_data$amenities_Air.conditioning, na.rm = TRUE)
}

print(ac_bin_means)

#building the dataframe
bin_df3 <- data.frame(
  score_adjusted_bin = score_bin_means,
  ac_clean_bin = ac_bin_means
)
bin_df3$side <- ifelse(bin_df$score_adjusted_bin < cutoff, "left", "right")

#plot
ggplot(bin_df3, aes(x = score_adjusted_bin, y = ac_clean_bin)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_smooth(data = subset(bin_df3, side == "left"), method = "loess", se = FALSE, color = "blue") +
  geom_smooth(data = subset(bin_df3, side == "right"), method = "loess", se = FALSE, color = "orange") +
  geom_vline(xintercept = cutoff, linetype = "dashed", color = "red") +
  labs(
    title = "Continuity check for Air Conditioning using LOESS (left/right)",
    x = "Score Adjusted (bin mean)",
    y = "Air Conditioning (bin mean)"
  ) + 
  coord_cartesian(ylim = c(0, 1))+
  theme_minimal()


##Amenity Bar Lounge
bl_bin_means <- numeric(num_bins)

#calculating the avarages
for (i in 1:num_bins) {
  start_index <- (i - 1) * bin_size + 1
  end_index <- i * bin_size
  bin_data <- data_sorted[start_index:end_index, ]
  ac_bin_means[i] <- mean(bin_data$amenities_Bar...lounge, na.rm = TRUE)
}

print(bl_bin_means)

#building the dataframe
bin_df4 <- data.frame(
  score_adjusted_bin = score_bin_means,
  ac_clean_bin = ac_bin_means
)
bin_df4$side <- ifelse(bin_df$score_adjusted_bin < cutoff, "left", "right")

#plot
ggplot(bin_df4, aes(x = score_adjusted_bin, y = ac_clean_bin)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_smooth(data = subset(bin_df4, side == "left"), method = "loess", se = FALSE, color = "blue") +
  geom_smooth(data = subset(bin_df4, side == "right"), method = "loess", se = FALSE, color = "orange") +
  geom_vline(xintercept = cutoff, linetype = "dashed", color = "red") +
  labs(
    title = "Continuity check for Bar Lounge using LOESS (left/right)",
    x = "Score Adjusted (bin mean)",
    y = "Bar Lounge (bin mean)"
  ) + 
  coord_cartesian(ylim = c(0, 1))+
  theme_minimal()






### 4 - Test the hypothesis of *no jumps* on the outcome variable at alternative cutoffs (placebo effects)
#1
rdplot(y = data$price_curr_min,
       x = data$score_adjusted,
       c = 4.1,
       h = 0.1,
       x.label = "Score Adjusted",
       y.label = "Current Price")  


rd_result <- rdrobust(y = data$price_curr_min,
                      x = data$score_adjusted,
                      c = 4.1,
                      h = 0.1)  
summary(rd_result)

#2
rdplot(y = data$price_curr_min,
       x = data$score_adjusted,
       c = 3.6,
       h = 0.1,,
       x.label = "Score Adjusted",
       y.label = "Current Price")  


rd_result <- rdrobust(y = data$price_curr_min,
                      x = data$score_adjusted,
                      c = 3.6,
                      h = 0.1)  
summary(rd_result)

#3
rdplot(y = data$price_curr_min,
       x = data$score_adjusted,
       c = 3.4,
       h = 0.1,
       x.label = "Score Adjusted",
       y.label = "Current Price")  


rd_result <- rdrobust(y = data$price_curr_min,
                      x = data$score_adjusted,
                      c = 3.4,
                      h = 0.1)  
summary(rd_result)


#4
rdplot(y = data$price_curr_min,
       x = data$score_adjusted,
       c = 2.9,
       h = 0.1,,
       x.label = "Score Adjusted",
       y.label = "Current Price")  


rd_result <- rdrobust(y = data$price_curr_min,
                      x = data$score_adjusted,
                      c = 2.9,
                      h = 0.1)  
summary(rd_result)





########### Estimating the RD effect

rd_result <- rdrobust(y = data$price_curr_min,
                      x = data$score_adjusted,
                      c = 4.25,
                      kernel = "triangular",
                      p = 4,
                      covs = data[, c("views", "class_clean","amenities_Air.conditioning","amenities_Bar...lounge")]
) 
summary(rd_result)

###sensitivity analysis table 1
#no covariates
rd_result <- rdrobust(y = data$price_curr_min,
                      x = data$score_adjusted,
                      c = 4.25,
                      kernel = "triangular",
                      p = 4,
                      h = 0.25
                      ) 
summary(rd_result)

#views
rd_result <- rdrobust(y = data$price_curr_min,
                      x = data$score_adjusted,
                      c = 4.25,
                      kernel = "triangular",
                      p = 4,
                      h = 0.25,
                      covs = data[, c("views")]
) 
summary(rd_result)
#views + class
rd_result <- rdrobust(y = data$price_curr_min,
                      x = data$score_adjusted,
                      c = 4.25,
                      kernel = "triangular",
                      p = 4,
                      h = 0.25,
                      covs = data[, c("views", "class_clean")]
) 
summary(rd_result)


#views + amenities
rd_result <- rdrobust(y = data$price_curr_min,
                      x = data$score_adjusted,
                      c = 4.25,
                      kernel = "triangular",
                      p = 4,
                      h = 0.25,
                      covs = data[, c("views","amenities_Air.conditioning","amenities_Bar...lounge")]
) 
summary(rd_result)

#class + amenities
rd_result <- rdrobust(y = data$price_curr_min,
                      x = data$score_adjusted,
                      c = 4.25,
                      kernel = "triangular",
                      p = 4,
                      h = 0.25,
                      covs = data[, c("class_clean","amenities_Air.conditioning","amenities_Bar...lounge")]
) 
summary(rd_result)

#all
rd_result <- rdrobust(y = data$price_curr_min,
                      x = data$score_adjusted,
                      c = 4.25,
                      kernel = "triangular",
                      p = 4,
                      h = 0.25,
                      covs = data[, c("views", "class_clean","amenities_Air.conditioning","amenities_Bar...lounge")]
) 
summary(rd_result)


###sensitivity analysis table 2
#p = 0

rd_result  <- rdrobust(y = data$price_curr_min, 
                       x = data$score_adjusted,
                       covs = data[, c("views", "class_clean","amenities_Air.conditioning","amenities_Bar...lounge")],
                       c = 4.25, 
                       h = 0.25,
                       p = 0)

summary(rd_result)



rd_result  <- rdrobust(y = data$price_curr_min, 
                       x = data$score_adjusted,
                       covs = data[, c("views", "class_clean","amenities_Air.conditioning","amenities_Bar...lounge")],
                       c = 4.25, 
                       h = 0.20,
                       p = 0)

summary(rd_result)



rd_result  <- rdrobust(y = data$price_curr_min, 
                       x = data$score_adjusted,
                       covs = data[, c("views", "class_clean","amenities_Air.conditioning","amenities_Bar...lounge")],
                       c = 4.25, 
                       h = 0.15,
                       p = 0)

summary(rd_result)



rd_result  <- rdrobust(y = data$price_curr_min, 
                       x = data$score_adjusted,
                       covs = data[, c("views", "class_clean","amenities_Air.conditioning","amenities_Bar...lounge")],
                       c = 4.25, 
                       h = 0.10,
                       p = 0)

summary(rd_result)

rd_result  <- rdrobust(y = data$price_curr_min, 
                       x = data$score_adjusted,
                       covs = data[, c("views", "class_clean","amenities_Air.conditioning","amenities_Bar...lounge")],
                       c = 4.25, 
                       h = 0.05,
                       p = 0)

summary(rd_result)

#p = 1
rd_result  <- rdrobust(y = data$price_curr_min, 
                       x = data$score_adjusted,
                       covs = data[, c("views", "class_clean","amenities_Air.conditioning","amenities_Bar...lounge")],
                       c = 4.25, 
                       h = 0.25,
                       p = 1)

summary(rd_result)



rd_result  <- rdrobust(y = data$price_curr_min, 
                       x = data$score_adjusted,
                       covs = data[, c("views", "class_clean","amenities_Air.conditioning","amenities_Bar...lounge")],
                       c = 4.25, 
                       h = 0.20,
                       p = 1)

summary(rd_result)



rd_result  <- rdrobust(y = data$price_curr_min, 
                       x = data$score_adjusted,
                       covs = data[, c("views", "class_clean","amenities_Air.conditioning","amenities_Bar...lounge")],
                       c = 4.25, 
                       h = 0.15,
                       p = 1)

summary(rd_result)



rd_result  <- rdrobust(y = data$price_curr_min, 
                       x = data$score_adjusted,
                       covs = data[, c("views", "class_clean","amenities_Air.conditioning","amenities_Bar...lounge")],
                       c = 4.25, 
                       h = 0.10,
                       p = 1)

summary(rd_result)

rd_result  <- rdrobust(y = data$price_curr_min, 
                       x = data$score_adjusted,
                       covs = data[, c("views", "class_clean","amenities_Air.conditioning","amenities_Bar...lounge")],
                       c = 4.25, 
                       h = 0.05,
                       p = 1)

summary(rd_result)

#p = 2
rd_result  <- rdrobust(y = data$price_curr_min, 
                       x = data$score_adjusted,
                       covs = data[, c("views", "class_clean","amenities_Air.conditioning","amenities_Bar...lounge")],
                       c = 4.25, 
                       h = 0.25,
                       p = 2)

summary(rd_result)



rd_result  <- rdrobust(y = data$price_curr_min, 
                       x = data$score_adjusted,
                       covs = data[, c("views", "class_clean","amenities_Air.conditioning","amenities_Bar...lounge")],
                       c = 4.25, 
                       h = 0.20,
                       p = 2)

summary(rd_result)



rd_result  <- rdrobust(y = data$price_curr_min, 
                       x = data$score_adjusted,
                       covs = data[, c("views", "class_clean","amenities_Air.conditioning","amenities_Bar...lounge")],
                       c = 4.25, 
                       h = 0.15,
                       p = 2)

summary(rd_result)



rd_result  <- rdrobust(y = data$price_curr_min, 
                       x = data$score_adjusted,
                       covs = data[, c("views", "class_clean","amenities_Air.conditioning","amenities_Bar...lounge")],
                       c = 4.25, 
                       h = 0.10,
                       p = 2)

summary(rd_result)

rd_result  <- rdrobust(y = data$price_curr_min, 
                       x = data$score_adjusted,
                       covs = data[, c("views", "class_clean","amenities_Air.conditioning","amenities_Bar...lounge")],
                       c = 4.25, 
                       h = 0.05,
                       p = 2)

summary(rd_result)

#p = 3
rd_result  <- rdrobust(y = data$price_curr_min, 
                       x = data$score_adjusted,
                       covs = data[, c("views", "class_clean","amenities_Air.conditioning","amenities_Bar...lounge")],
                       c = 4.25, 
                       h = 0.25,
                       p = 3)

summary(rd_result)



rd_result  <- rdrobust(y = data$price_curr_min, 
                       x = data$score_adjusted,
                       covs = data[, c("views", "class_clean","amenities_Air.conditioning","amenities_Bar...lounge")],
                       c = 4.25, 
                       h = 0.20,
                       p = 3)

summary(rd_result)



rd_result  <- rdrobust(y = data$price_curr_min, 
                       x = data$score_adjusted,
                       covs = data[, c("views", "class_clean","amenities_Air.conditioning","amenities_Bar...lounge")],
                       c = 4.25, 
                       h = 0.15,
                       p = 3)

summary(rd_result)



rd_result  <- rdrobust(y = data$price_curr_min, 
                       x = data$score_adjusted,
                       covs = data[, c("views", "class_clean","amenities_Air.conditioning","amenities_Bar...lounge")],
                       c = 4.25, 
                       h = 0.10,
                       p = 3)

summary(rd_result)

rd_result  <- rdrobust(y = data$price_curr_min, 
                       x = data$score_adjusted,
                       covs = data[, c("views", "class_clean","amenities_Air.conditioning","amenities_Bar...lounge")],
                       c = 4.25, 
                       h = 0.05,
                       p = 3)

summary(rd_result)

#p = 4
rd_result  <- rdrobust(y = data$price_curr_min, 
                       x = data$score_adjusted,
                       covs = data[, c("views", "class_clean","amenities_Air.conditioning","amenities_Bar...lounge")],
                       c = 4.25, 
                       h = 0.25,
                       p = 4)

summary(rd_result)



rd_result  <- rdrobust(y = data$price_curr_min, 
                       x = data$score_adjusted,
                       covs = data[, c("views", "class_clean","amenities_Air.conditioning","amenities_Bar...lounge")],
                       c = 4.25, 
                       h = 0.20,
                       p = 4)

summary(rd_result)



rd_result  <- rdrobust(y = data$price_curr_min, 
                       x = data$score_adjusted,
                       covs = data[, c("views", "class_clean","amenities_Air.conditioning","amenities_Bar...lounge")],
                       c = 4.25, 
                       h = 0.15,
                       p = 4)

summary(rd_result)



rd_result  <- rdrobust(y = data$price_curr_min, 
                       x = data$score_adjusted,
                       covs = data[, c("views", "class_clean","amenities_Air.conditioning","amenities_Bar...lounge")],
                       c = 4.25, 
                       h = 0.10,
                       p = 4)

summary(rd_result)

rd_result  <- rdrobust(y = data$price_curr_min, 
                       x = data$score_adjusted,
                       covs = data[, c("views", "class_clean","amenities_Air.conditioning","amenities_Bar...lounge")],
                       c = 4.25, 
                       h = 0.05,
                       p = 4)

summary(rd_result)
