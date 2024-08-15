library(dplyr); library(psych); library(tidyverse)
source("MultipleRegression.R"); source("Columns_to_analyse.R")
df <- read.csv("RWA CSV 2024.csv")

# Are there any differences in scores between the four sites?
df[, columns_to_analyse$DASS] <- lapply(df[, columns_to_analyse$DASS], function(x) as.numeric(as.character(factor(x,c(1,2,3,4), c(0, 1, 2, 3)))))
df[, columns_to_analyse$K6] <- lapply(df[, columns_to_analyse$K6], function(x) as.numeric(as.character(factor(x,c(1,2,3,4,5), c(0, 1, 2, 3,4)))))

for (name in names(columns_to_analyse)) {
  df[,paste(name,"_sum", sep = "")] <- as.data.frame(rowSums(df[, columns_to_analyse[[name]]]))
}

df$DASS42_sum <- df$DASS_sum * 2
sum_cols <- names(df[72:86])

# Checking for missing data
as.matrix(colSums(is.na(df)))
nrow(df[complete.cases(df[,sum_cols]) == TRUE,])
typeof(df[911,]$Gender)
df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)], function(x) ifelse(x == "", NA, x))
as.matrix(colSums(is.na(df)))
nrow(df[complete.cases(df[,sum_cols]) == TRUE,])

# Checking if DASS data is missing at random for Organisation 4. Comparing the means of K6 scores for the missing V not missing.
proxy <- df[df$OrgID == 4,]
proxy$missing_DASS_sum <- is.na(proxy$DASS_sum)
t_test_result <- t.test(K6_sum ~ missing_DASS_sum, data = proxy)


descriptives <- describe(df[,sum_cols])

means_sum <- dplyr::data_frame(sum_cols[!sum_cols %in% c("K6_sum" , "DASS_sum")])
colnames(means_sum) <- "Variables"
means_sum$means <- as.numeric(lapply(df[,sum_cols[!sum_cols %in% c("K6_sum" , "DASS_sum")]], function(x) mean(x, na.rm = TRUE)))
write.csv(means_sum, file = "means_sum.csv", row.names = FALSE)

means_sum <- means_sum %>%
  mutate(colour = case_when(
    means > 17 ~ "darkgreen",
    means >= 14 & means <= 17 ~ "gold",
    means < 14 ~ "orange"
  ))

ggplot(means_sum, aes(means, Variables, fill = colour)) +
  geom_col() +
  geom_text(aes(label = round(means, 2)), 
            hjust = 1.1,
            color = "white",
            ) +
  labs(title = "Bar Graph of Variables by OrgID",
       x = "Value",
       y = "Varaible",
       fill = "Test") +
  scale_fill_identity() + 
  theme_minimal()

loc_means_sum <- df |>
  group_by(OrgID) |>
  summarise(across(sum_cols, mean, na.rm = TRUE))
write.csv(loc_means_sum, file = "loc_means_sum.csv", row.names = FALSE)

data_long_sum <- loc_means_sum[!names(loc_means_sum) %in% c("DASS_sum", "K6_sum")] %>%
  pivot_longer(cols = -OrgID, names_to = "Variable", values_to = "Value")
write.csv(data_long_sum, file = "data_long_sum.csv", row.names = FALSE)

ggplot(data_long_sum, aes(Value, Variable, fill = factor(OrgID))) +
  geom_col(position = "dodge") +
  labs(title = "Bar Graph of Variables by OrgID",
       x = "Value",
       y = "Varaible",
       fill = "OrgID") +
  theme_minimal()

for (i in sum_cols) {
  formula <- as.formula(paste(i, "~ factor(OrgID)"))
  model <- aov(formula, data = df)
  print(i)
  print(summary(model))
  if(summary(model)[[1]][1,5] < 0.05){
    print(TukeyHSD(model, conf.level=.95))
  }
}

desc <- list()
desc$cor <- cor(df[,sum_cols], method = "pearson", use = "pair")
desc$mean <- sapply(na.omit(df[,sum_cols]), mean)
desc$sd <- sapply(na.omit(df[,sum_cols]), sd)
desc$cron <- as.numeric(sapply(columns_to_analyse, function(sum_cols) psych::alpha(df[, sum_cols])$total$raw_alpha))
desc$tab <- round(data.frame(mean = desc$mean, sd = desc$sd, cron = desc$cron, desc$cor),2)
desc$tab

# What are the key drivers of the outcome variable?
# Create the varaibales and assign values
outcomes <- c("K6_sum", "DASS_sum")
predictors <- sum_cols[!sum_cols %in% outcomes]
# num.bootstraps <- 1500

# Run rwa on the data
rwa_models <- list()
for (outcome in outcomes){
  rwa_models[[outcome]] <- rwa(df, outcome = outcome, predictors = predictors)
}

# Print Results----
print(paste("R-Squared:", rwa_models$K6$rsquare, sep = " "))
rwa_models$K6$result
print(paste("R-Squared:", rwa_models$DASS$rsquare, sep = " "))
rwa_models$DASS$result

K6_means <- dplyr::tibble(predictors)
K6_means$means <- descriptives[predictors, "mean"]
K6_means$weights <- rwa_models$K6_sum$result[,3]
write.csv(K6_means, file = "K6_means.csv", row.names = FALSE)

DASS_means <- dplyr::tibble(predictors)
DASS_means$means <- descriptives[predictors, "mean"]
DASS_means$weights <- rwa_models$DASS_sum$result[,3]
write.csv(DASS_means, file = "DassMeans.csv", row.names = FALSE)

ggplot(K6_means, aes(means, weights, label = predictors)) +  
  geom_point(colour = "blue", size = 3) +
  labs(title = "Plot of Weights v Mean for K6",
       x = "Mean",
       y = "Weight") +
  geom_text(nudge_y = 1)+
  theme_minimal()

ggplot(DASS_means, aes(means, weights, label = predictors)) +  
  geom_point(colour = "blue", size = 3) +
  labs(title = "Plot of Weights v Mean for DASS",
       x = "Mean",
       y = "Weight") +
  geom_text(nudge_y = 0.5)+
  theme_minimal()

# Call the function above {my.boot} within the bootstrap function built in R
myBootstrap <- boot(df, my.boot, R=num.bootstraps, outcome = outcome, predictors = predictors, focal = focal) #run the bootstrap

# Get the bca CIs 
ci.results <- tidy(myBootstrap, conf.int=TRUE, conf.method="bca")

print(kable(cbind(myrwa$result[1], ci.results[1:num.predictors,c(4,5)]), caption = "CIs around the raw Relative Weights"))
print(kable(cbind(myrwa$result[1], ci.results[I(num.predictors+1):I(num.predictors*2),c(4,5)]), caption = "CIs around the difference between a Relative Weight for a substantive variable and a random variable. If zero is included in the interval that predictor is not significant"))
if (compare == "Yes") { 
  print(kable(cbind(comparisons, ci.results[I((num.predictors*2)+1):nrow(ci.results),c(4,5)]), caption = "CIs around the difference between two substantive variables. If zero is included in the interval, those two predictors are not significantly different from one another"))
}
