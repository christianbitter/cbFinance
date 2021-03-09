rm(list = ls());

library(dplyr);
library(ggplot2);
library(tidymodels);
library(broom);
# generate some random data
x <- seq(from=-5, to=50, length.out=100);
y <- 50 + 2 * x + rnorm(n = length(x), mean = 0, sd = 20);
df <- tibble("X" = x, "Y" = y);

df %>%
  ggplot(aes(x = x, y = Y)) +
  geom_point() +
  theme_light();

# fit a linear model
model <- lm(formula = Y ~ x, data = df);
broom::tidy(model);

# plot the data

# compute the rmse of the fit
rmse <- function(y, yhat) {
  return(sqrt(mean((y - yhat)^2)));
}

df <- df %>% dplyr::bind_cols(YHat = predict(model)) %>%
  dplyr::mutate(ymin = ifelse(Y >= YHat, YHat, Y),
                ymax = ifelse(Y >= YHat, Y, YHat));

df %>%
  ggplot() +
  geom_line(aes(x = x, y = YHat), colour = "lightblue") +
  geom_point(aes(x = x, y = Y), colour = "blue") +
  geom_linerange(aes(x = x, ymin = ymin, ymax = ymax)) +
  theme_light() +
  labs(title = "Regression Analysis", x = "x", y = "Y")

y <- df$Y;
yhat <- predict(model);
y.rmse <- rmse(y, yhat);

# we do not show the SGD here