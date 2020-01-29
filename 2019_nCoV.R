# This R script analyses 2019-nCoV infection data.

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


# Load data -------------------------------------------------------------------


d0 <- readr::read_csv("2019_nCoV_data.csv")
dates <- d0$date

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
  scale_x_continuous(breaks = seq(from = 1, to = NROW(d0), by = 1)) +
  theme(legend.position = "right",
        axis.title.x = element_blank(),
        axis.text.x = element_blank())
plot2 <- ggplot(data = d.log2.melt, aes(x = days, value, colour = variable)) + 
  geom_line() + 
  labs(x = "天数 (days)",
       y = "log2(Number of people) \n log2(人数)",
       color = "类别 (Class)") +
  scale_x_continuous(breaks = seq(from = 1, to = NROW(d0), by = 1)) +
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
  scale_x_continuous(breaks = seq(from = 1, to = NROW(d0), by = 1)) +
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
  scale_x_continuous(breaks = seq(from = 1, to = NROW(d0), by = 1)) +
  theme(legend.position = "right")
egg::ggarrange(plots = list(plot3, plot4), 
               nrow = 2, heights = c(1, 1))


# Predict ---------------------------------------------------------------------

model <- lm(confirmed.log ~ days, data = d0)
para <- summary(model)

a <- para$coefficients[1, 1]
b <- para$coefficients[2, 1]
a.se <- para$coefficients[1, 2]
b.se <- para$coefficients[2, 2]
x <- 1:(NROW(d0) + 3)

# Choose confidence interval level.
ci <- qnorm(0.025, mean = 0, sd = 1, lower.tail = F)

y <- 2^(a + b*x)
y.left <- 2^(a - ci*a.se + (b - ci*b.se)*x)
y.right <- 2^(a + ci*a.se + (b + ci*b.se)*x)

predicted <- data.frame(y, y.left, y.right, days = x, 
                        confirmed = c(d0$confirmed, 
                                      rep(NA, length(x) - NROW(d0))))

# Third plot.
ggplot(predicted, aes(days, confirmed)) +
  geom_point() +
  stat_smooth() +
  geom_line(aes(y = y, colour = "y")) +
  geom_line(aes(y = y.left, colour = "y.left")) +
  geom_line(aes(y = y.right, colour = "y.right")) +
  labs(x = "天数 (days)",
       y = "y = log2(Number of people) \n log2(人数)") +
  scale_x_continuous(breaks = seq(from = 1, to = NROW(x), by = 1)) +
  theme(legend.position = "right")


