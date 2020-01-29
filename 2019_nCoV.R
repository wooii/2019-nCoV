# This R script analyses some of the epidemic data that has been reported for
# the novel coronavirus (2019-nCoV) outbreak in China by the Nathinal Health 
# Commission of the People's Republic of China http://www.nhc.gov.cn/.
#
# Author: Chenfeng Chen
# Created on 2020-01-29


# Functions -------------------------------------------------------------------

load_required_packages <- function(x) {
  # Load all the required packages, and automatically install the ones that have
  # not been installed.
  # x: a character vector of required packages.
  # Return: NULL.
  x_d <- tools::package_dependencies(x)
  x_all <- base::unique(c(x, unlist(x_d)))
  # Index of packages to be installed.
  x_i <- !(x_all %in% rownames(installed.packages()))
  if (any(x_i)) {
    y <- x_all[x_i]
    cat("Installing the required package(s):", y, "\n", sep = " ")
    install.packages(y, dependencies = T)
  }
  # Load the required packages.
  invisible(lapply(x, library, character.only = T))
}

load_required_packages(c("readr", "ggplot2", "ggpubr", "egg"))


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
dates <- d0$date
n <- NROW(d0)

# Rename interested variables.
v <- c("确诊 (confirmed)", "重症 (hospital)", "死亡 (dead)", "治愈 (healed)",
       "疑似 (suspected)", "密切接触者 (contacted)")

d1 <- d0[2:8]
colnames(d1)[2:7] <- v
d.melt <- reshape2::melt(d1, id = "days")

d2 <- data.frame(days = d0$days, log2(d0[3:8]))
colnames(d2)[2:7] <- v
d.log2.melt <- reshape2::melt(d2, id = "days")


# Plots -----------------------------------------------------------------------

# First plot.
plot1 <- ggplot(data = d.melt, aes(x = days, value, colour = variable)) + 
  geom_point() + 
  labs(title = paste("2019-nCoV data (", dates[1], " - ", tail(dates, 1), ")",
                     sep = ""),
       y = "Number of people \n 人数",
       color = "类别 (Class)") +
  scale_x_continuous(breaks = seq(from = 1, to = n, by = 1)) +
  theme(legend.position = "right",
        axis.title.x = element_blank(),
        axis.text.x = element_blank())
plot2 <- ggplot(data = d.log2.melt, aes(x = days, value, colour = variable)) + 
  geom_line() + 
  labs(x = "天数 (days)",
       y = "log2(Number of people) \n log2(人数)",
       color = "类别 (Class)") +
  scale_x_continuous(breaks = seq(from = 1, to = n, by = 1)) +
  theme(legend.position = "right")
egg::ggarrange(plots = list(plot1, plot2), 
               nrow = 2, heights = c(1, 1))


# Second plot.
d0$confirmed.log <- log2(d0$confirmed)
plot3 <- ggplot(d0, aes(days, confirmed)) +
  geom_point() +
  geom_smooth() +
  labs(title = paste("2019-nCoV 确诊人数的指数增长 (", 
                     dates[1], " - ", tail(dates, 1), ")",
                     sep = ""),
       y = "Number of people \n 人数",
       color = "类别 (Class)") +
  scale_x_continuous(breaks = seq(from = 1, to = n, by = 1)) +
  theme(legend.position = "right",
        axis.title.x = element_blank(),
        axis.text.x = element_blank())
plot4 <- ggplot(d0, aes(days, confirmed.log)) +
  geom_point() +
  stat_smooth() +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
    formula = y ~ x) +
  stat_cor(label.x = 1, label.y = 11) +
  labs(x = "天数 (days)",
       y = "y = log2(Number of people) \n log2(人数)") +
  scale_x_continuous(breaks = seq(from = 1, to = n, by = 1)) +
  theme(legend.position = "right")
egg::ggarrange(plots = list(plot3, plot4), 
               nrow = 2, heights = c(1, 1))


# Make prediction -------------------------------------------------------------

model <- lm(confirmed.log ~ days, data = d0)

# number of days to predict from the most recent date.
m <- 3
predicted <- log2_lm_predict(lm.model = model,
                x.predict = 1:(n + m), 
                ci.level = 0.95)

predicted <- data.frame(predicted, 
                        days = 1:(n + m),
                        confirmed = c(d0$confirmed, rep(NA, m)))

# Third plot.
ggplot(predicted, aes(days, confirmed)) +
  geom_point() +
  stat_smooth() +
  geom_line(aes(y = y, colour = "y")) +
  geom_line(aes(y = y.left, colour = "y.left")) +
  geom_line(aes(y = y.right, colour = "y.right")) +
  labs(x = "天数 (days)",
       y = "y = log2(Number of people) \n log2(人数)") +
  scale_x_continuous(breaks = seq(from = 1, to = n + m, by = 1)) +
  theme(legend.position = "right")


