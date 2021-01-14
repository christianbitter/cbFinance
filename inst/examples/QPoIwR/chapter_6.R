rm(list = ls());

library(dplyr);
library(ggplot2);

# 3 (Basic Time Value of Money)
# suppose we are going to invest in a zero coupon bond. bond pays 1000$ risk free.
# (zero coupon bond does not pay intermediate rates)
# a) bond matures in 1y, yields .3% - what is it worth now
A <- 1000;
r <- .003;
n <- 1;
pv <- discount(A = A, r = r, n = n);

# b) suppose the bond matures in one year and yields 3%
A <- 1000;
r <- .03;
n <- 1;
pv <- discount(A = A, r = r, n = n);

# c) suppose the bond matures in two years and yields 3% annualized
A <- 1000;
r <- .03;
n <- 2;
pv <- discount(A = A, r = r, n = n, simple = T);

# 4 (Compounding and Time Value of Money)
# a) suppose we invest in half year zero coupon bonds, which yield 3% annualized risk free
# what is the present value of 1000$ in one year
A <- 1000;
r <- .03;
n <- 1;
interval <- 2;
fv <- compound(A = A, r = r, n = n, interval = interval, simple = F);

# the value is 1030.225
# let's see if this works out ... we compound 1000 for 6/12 month * .03
p1 <- A * (1 + .03*.5) # this yields 1015, we compound this again for the rest of the 6 month
p2 <- p1 * (1 + .03 * .5) # this yields the 1030.225
# so we make a bit more than putting our money in a single compounded zero coupon bond
# the difference is about 23 cents.
p3 <- compound(A = A, r = r, n = n, interval = 1, simple = F);

# b) so now we invest in a monthly coupon
A <- 1000;
r <- 0.03;
n <- 1;
interval <- 12;
fv <- compound(A = A, r = r, n = n, interval = interval, simple = F);
# we get 1030.416 # so an additional 20cents

# c) let's see about daily compounding
interval <- 365;
fv <- compound(A = A, r = r, n = n, interval = interval, simple = F);
# a difference of 5 cents

# d) so what would the terminal be - continuous compounding
fv <- compound(A = A, r = r, n = n, interval = 1, simple = F, continuous = T);
# we can see that the continuous compounding and the daily compouding are rather similar.