# This R script analyses some of the epidemic data that has been reported for
# the novel coronavirus (2019-nCoV) outbreak in China by the Nathinal Health 
# Commission of the People's Republic of China http://www.nhc.gov.cn/.
#
# Author: Chenfeng Chen
# Created on 2020-01-29

library(shiny)
library(readr)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(egg)
library(segmented)


# Functions -------------------------------------------------------------------

log2_lm_predict <- function(lm.model, x.predict, ci.level = 0.95) {
  # Calculate the predict value and its confidence interval for the log2 
  #   linear model.
  # lm.model: a linear model return by function lm().
  # x.predict: a vector contains the values of x for prediction of y using the 
  #   lm.model.
  # ci.levle: the confidence interval levels for the predicted y.
  lm.summary <- summary(lm.model)
  a <- lm.summary$coefficients[1, 1]
  b <- lm.summary$coefficients[2, 1]
  a.se <- lm.summary$coefficients[1, 2]
  b.se <- lm.summary$coefficients[2, 2]
  # Choose confidence interval level.
  ci <- qnorm((1 - ci.level)/2, mean = 0, sd = 1, lower.tail = F)
  y <- 2^(a + b*x.predict)
  y.left <- 2^(a - ci*a.se + (b - ci*b.se)*x.predict)
  y.right <- 2^(a + ci*a.se + (b + ci*b.se)*x.predict)
  return(data.frame(y, y.left, y.right))
}


R2_p <- function(r2, p){
  # Display the R2 and p values in Figure 2.
  # r2: the value of r2.
  # p: the value of p
  # Return: an character that can be displayed in ggplot2 with geom_text().
  eq <- substitute(R^2~"="~r2~ ","~~P~"="~p,
                   list(r2 = format(r2, digits = 3),
                        p = format(p, digits = 3)))
  
  y <- as.character(as.expression(eq))
  return(y)
}


x_range <- function(x, n) {
  # To calculate the x.range for the tabel under Figure 2.
  # x: a vector of breakpoint.
  # n: the number of rows of the raw data, i.e. n = NROW(d)
  # Return: a character vector contains the range of x for piecewise linear
  #   regression.
  pp <- c(1, round(x, 1), n)
  y <- vector()
  for (i in 2:length(pp)) {
    y[i - 1] <- paste(pp[i - 1], "<=", "x", "<=", pp[i], sep = " ")
  }
  return(y)
}


# Load data -------------------------------------------------------------------

d <- readr::read_csv("2019_nCoV_data.csv") # data.
# d <- readr::read_csv("2019_nCoV/2019_nCoV_data.csv") # data for local test.

group.names <- names(d[-1]) # Get the group names.
dates <- d$date
d.log2 <- data.frame(date = dates, log2(d[, -1]))
n <- NROW(d) # Number of days.

# Melt data for plots.
d.melt <- reshape2::melt(d, id = "date")
d.log2.melt <- reshape2::melt(d.log2, id = "date")

days <- 1:n
d$days <- days # Add days.

# This solves the incorrect display of date problem in shiny renderTable().
d0 <- d
d0$date <- as.character(d0$date)


# First plot ------------------------------------------------------------------

p1 <- ggplot(data = d.melt, aes(x = date, value, colour = variable)) + 
  geom_point() + 
  geom_line() + 
  labs(title = "2019-nCoV epidemic data in China",
       y = "Number of cases",
       colour = "Group") +
  scale_x_date(date_breaks = "1 day", date_labels = "%m-%d",
               minor_breaks = NULL) +
  theme(legend.position = "right",
        axis.title.x = element_blank(),
        axis.text.x = element_blank())
p2 <- ggplot(data = d.log2.melt, aes(x = date, value, colour = variable)) + 
  geom_point() + 
  geom_line() + 
  labs(x = "Date",
       y = "log2(Number of cases)",
       colour = "Group") +
  scale_x_date(date_breaks = "1 day", date_labels = "%m-%d",
               minor_breaks = NULL) +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 90, hjust = 0.5))
plot1 <- egg::ggarrange(plots = list(p1, p2), 
                        nrow = 2, heights = c(1, 1))

