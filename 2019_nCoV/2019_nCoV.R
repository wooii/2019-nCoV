# This R script analyses some of the epidemic data that has been reported for
# the novel coronavirus (2019-nCoV) outbreak in China by the Nathinal Health 
# Commission of the People's Republic of China http://www.nhc.gov.cn/.
#
# Author: Chenfeng Chen
# Created on 2020-01-29

library(shiny)
library(readr)
library(ggplot2)
library(ggpubr)
library(egg)


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


# Load data -------------------------------------------------------------------

d0 <- readr::read_csv("2019_nCoV_data.csv")
# d0 <- readr::read_csv("2019_nCoV/2019_nCoV_data.csv") # for local test.
dates <- d0$date
n <- NROW(d0)

# Rename interested variables.
# v <- c("确诊 (confirmed)", "重症 (hospital)", "死亡 (dead)", "治愈 (healed)",
#        "疑似 (suspected)", "密切接触者 (contacted)")

d1 <- d0[c(1, 3:8)]
# colnames(d1)[2:7] <- v
d.melt <- reshape2::melt(d1, id = "date")

d2 <- data.frame(date = dates, log2(d0[3:8]))
# colnames(d2)[2:7] <- v
d.log2.melt <- reshape2::melt(d2, id = "date")


# Plots -----------------------------------------------------------------------

# First plot.
p1 <- ggplot(data = d.melt, aes(x = date, value, colour = variable)) + 
  geom_point() + 
  labs(title = "2019-nCoV epidemic data in China",
       y = "Number of cases",
       color = "Class") +
  scale_x_date(date_breaks = "1 day", date_labels = "%m-%d",
               minor_breaks = NULL) +
  theme(legend.position = "right",
        axis.title.x = element_blank(),
        axis.text.x = element_blank())
p2 <- ggplot(data = d.log2.melt, aes(x = date, value, colour = variable)) + 
  geom_line() + 
  labs(x = "Date",
       y = "log2(Number of cases)",
       color = "Class") +
  scale_x_date(date_breaks = "1 day", date_labels = "%m-%d",
               minor_breaks = NULL) +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 90, hjust = 0.5))

plot1 <- egg::ggarrange(plots = list(p1, p2), 
                     nrow = 2, heights = c(1, 1))


# Second plot.
l <- log2(d0$confirmed)
d0$confirmed.log <- l
l.max <- max(l, na.rm = T)
l.min <- min(l, na.rm = T)

plot2 <- ggplot(d0, aes(days, confirmed.log)) +
  geom_point() +
  stat_smooth(formula = y ~ x, method = "lm", level = 0.99) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
    formula = y ~ x,
    label.x = 1, label.y = l.max) +
  stat_cor(label.x = 1, label.y = l.min + 0.8*(l.max - l.min)) +
  labs(title = "2019-nCoV confirmed infection cases in China",
       subtitle = paste(dates[1], " - ", tail(dates, 1), sep = ""),
       x = "Days",
       y = "y = log2(Number of cases)") +
  scale_x_continuous(breaks = seq(from = 1, to = n, by = 1)) +
  theme(legend.position = "right")

