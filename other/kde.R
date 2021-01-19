# some testing of kernel density estimation
rm(list = ls());
library(ggplot2);
library(dplyr);

# get some univariate data
data_df <- datasets::cars;
x <- data_df$speed;

# look at the histogram for a first feel
hist(x);
as_tibble(x = data_df) %>%
  ggplot() +
  geom_density(aes(x = speed), kernel = "cosine", bw = 1 - (8/pi**2)) +
  scale_y_continuous(labels = scales::percent) +
  theme_light() +
  labs(title = "Kernel Density Estimation",
       subtitle = "Speed of the cars dataset")


K_cosine <- function(t) {
  pi2 <- pi / 2;
  pi4 <- pi / 4;

  return(pi4 * cos(pi2 * t));
}

kde <- function(X, kernel, band_width) {
  n <- length(X);
  ninv <- 1/n;
  bwinv <- 1/band_width;

  KDE <- function(x) {
    e <- 0;
    for (j in 1:n) {
      k_i <- kernel((x - X[j]) * bwinv);
      e <- e + bwinv * k_i;
    }

    return(e * ninv);
  }

  return(KDE);
}

kde_x <- kde(X = x, kernel = K_cosine, band_width = 1 - (8/pi**2));

x_test <- c(-0.77, -0.6, -.25, .14, .45, .64, .64, 1.19, 1.71, 1.74);

dor <- density(x = x_test, bw=1 - (8/pi**2), kernel = "cosine", n = 32);
plot(dor, col="blue")

# x_test <- dor$x;
# y_test <- numeric(length = length(x_test));
# for (i in 1:length(x_test)) {
#   y_test[i] <- kde_x(x_test[i]);
# }

as_tibble("x" = x_test) %>%
  ggplot() +
  geom_density(aes(x = x), kernel = "cosine", bw = 1 - (8/pi**2)) +
  scale_y_continuous(labels = scales::percent) +
  theme_light() +
  labs(title = "Kernel Density Estimation",
       subtitle = "cosine kernel of test data")



tibble("x" = x_test, "y" = y_test) %>%
  ggplot() +
  geom_line(aes(x = x, y = y)) +
  theme_light()
