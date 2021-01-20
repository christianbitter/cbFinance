# auth: christian bitter (c) 2020
# name: kde.r
# desc: some testing of kernel density estimation, on a toy dataset

rm(list = ls());
library(ggplot2);
library(dplyr);

kde <- function(X,
                kernel_name = c("cosine", "gaussian", "rectangular", "triangular"),
                band_width,
                no_sample = 512) {
  K_cosine <- function(t) {
    pi2 <- pi / 2;
    pi4 <- pi / 4;

    return(pi4 * cos(pi2 * t));
  }

  K_gaussian <-function(t) {
    p <- 1/sqrt(2*pi);

    return(p * exp(-0.5*t^2))
  }

  K_box <- function(t) {
    return(ifelse(abs(t) < 1, .5, 0.));
  }

  K_triangle <- function(t) {
    return(ifelse(abs(t) <= 1., 1. - abs(t), 0));
  }

  x_density <- function(x, X, fn_kernel, band_width) {
    p <- 1 / (length(X) * band_width);
    return(sum(p * fn_kernel((x - X)/band_width)));
  }

  fn_kernel <- list(
    "gaussian" = K_gaussian,
    "triangular" = K_triangle,
    "rectangular" = K_box,
    "cosine" = K_cosine
  )[[kernel_name]];

  if (is.null(fn_kernel)) stop(sprintf("Unknown kernel: %s", kernel_name));

  n <- length(X);
  bwinv <- 1 / band_width;
  p <- bwinv / n;
  .x. <- seq(min(X), max(X), length.out = no_sample);

  KDE <- function(x) {
    return(sum(p * fn_kernel((x - X) * bwinv)));
  }

  return(list(
    "X"  = X,
    "bw" = band_width,
    "xs" = .x.,
    "kernel" = kernel_name,
    "ys" = sapply(X = .x., FUN=KDE, simplify = T)
    )
  );
}

X <- c(-2.1, -1.3, -0.4, 1.9, 5.1, 6.2);
bw <- sqrt(2.25);
yg <- kde(X, "gaussian", bw);
yc <- kde(X, "cosine", bw);
yt <- kde(X, "triangular", bw);
yb <- kde(X, "rectangular", bw);

ggplot() +
  geom_line(aes(x = yg$xs, y = yg$ys, colour = "gaussian")) +
  geom_line(aes(x = yc$xs, y = yc$ys, colour = "cosine")) +
  geom_line(aes(x = yb$xs, y = yb$ys, colour = "rectangular")) +
  geom_line(aes(x = yt$xs, y = yt$ys, colour = "triangular")) +
  labs(title = "Kernel Density Estimation",
       subtitle = "Comparison of various kernels on small synthetic data set",
       colour = "Kernel", x = "t", y = "density") +
  theme_light()