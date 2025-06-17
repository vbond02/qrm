install.packages("ggplot2")
install.packages("quantmod")
install.packages("xts")
library(ggplot2) #, warn.conflicts = FALSE, quietly = TRUE)
library(quantmod) #, warn.conflicts = FALSE, quietly = TRUE)
#library(readxl)
#data <- read_excel("data.xlsx")
#data <- read.csv("data.csv")
#options(xts.warn_dplyr_breaks_lag = FALSE)

getSymbols(Symbols="AAPL", src = "yahoo")

df <- data.frame(Date=index(AAPL), coredata(AAPL))
head(df)

ggplot(data = df, aes(x = Date, y = AAPL.Close)) +
  geom_line(color = "steelblue") +
  geom_smooth(aes(y = AAPL.Close), method = "loess", color = "red") +
  labs(title = "Stock Prices Over Time", x = "Date", y = "Price") +
  theme_minimal()
?geom_smooth
df[5:7, "AAPL.Open"] <- NA
ii <- is.na(df)
for (col in names(df)) {
  if (is.numeric(df[, col]) && any(ii[, col])) 
    {df[[col]][ii[, col]] <- mean(df[[col]], na.rm = TRUE)}
}
df 
  
df <- data.frame(Date=index(AAPL), coredata(AAPL))
df[seq(5, 4000, by=125), "AAPL.Open"] <- NA
df[, "AAPL.Open"]
df <- na.omit(df)
df
any(is.na(df))



#####################
#You have a portfolio consisting of two assets: Apple (AAPL) and Microsoft (MSFT).
#you are intrested in estimating the 1-day 95% Value-at-Risk (VaR) for the portfolio
#based on the last year of historical daily returns.

library("quantmod")    # for getSymbols, etc
library(PerformanceAnalytics)   # for CalculateReturns, Return.portfolio, etc.

#create df
ap <- getSymbols(c("AAPL", "MSFT"), from = Sys.Date() - 365, auto.assign = TRUE)
prices <- merge(Cl(AAPL), Cl(MSFT))
colnames(prices) <- c("Apple", "Microsoft")
head(prices)

#calculate returns
returns <- na.omit(CalculateReturns(prices, method = "log"))

#calculate portfolio return
weights <- c(0.6, 0.4)
portfolio2_returns <- Return.portfolio(returns, weights)

#calculate VaR VC (or HS)
value_at_risk <- VaR(portfolio2_returns, p=0.95, method = "gaussian") #VC-method
value_at_risk


########## MONTE CARLO
set.seed(123)
mu <- mean(portfolio2_returns)
sigma <- sd(portfolio2_returns)
simulated_returns <- rnorm(10000, mean = mu, sd = sigma)

VaR_MC <- quantile(simulated_returns, probs = 0.95)
VaR_MC

#####Predict whether a customer will default on a loan
#using DECISION TREES.

# Load required packages
install.packages(c("rpart", "caret", "dplyr"))
library(caret)
library(rpart)
library(dplyr)

# Simulate or load a sample dataset
data(GermanCredit)  # From caret package
df <- GermanCredit

# Check structure
str(df)

# Create a binary target variable
df$Default <- ifelse(df$Class == "Bad", 1, 0)

# Drop the original Class column
df$Class <- NULL

# Split into training and test sets
set.seed(123)
train_idx <- createDataPartition(df$Default, p = 0.8, list = FALSE)
train <- df[train_idx, ]
test <- df[-train_idx, ]


# Fit a decision tree model
model_tree <- rpart(Default ~ ., data = train, method = "class")

# Predict
pred_tree <- predict(model_tree, test, type = "class")

df[["ResidenceDuration"]]
model <- lm(D~., data = df)
summary(model)

# Decision Tree Evaluation
confusionMatrix(pred_tree, as.factor(test$Default))