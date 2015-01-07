put.crises <- function(dat = obs, c0 = 1867.581, c1 = 1868.580){
    ## calculate time spent 1867.7-1868.7 in different ages

    ## First, pregnancy, 'preg':
    preg <- numeric(NROW(dat))
    s1 <- dat$birthdate
    s0 <- dat$birthdate - 0.75
    I1 <- s1 > c0 & s1 < c1
    I2 <- s1 >= c1 & s0 < c1

    preg[I1] <- s1[I1] - pmax(s0[I1], c0)
    preg[I2] <- c1 - pmax(s0[I2], c0)

    ## Second, first year, 't01':
    t01 <- numeric(NROW(dat))
    s1 <- dat$birthdate + 1
    s0 <- dat$birthdate
    I1 <- s1 > c0 & s1 < c1
    I2 <- s1 >= c1 & s0 < c1

    t01[I1] <- s1[I1] - pmax(s0[I1], c0)
    t01[I2] <- c1 - pmax(s0[I2], c0)

    ## Third, second and third year, 't13':
    t13 <- numeric(NROW(dat))
    s1 <- dat$birthdate + 3
    s0 <- dat$birthdate + 1
    I1 <- s1 > c0 & s1 < c1
    I2 <- s1 >= c1 & s0 < c1

    t13[I1] <- s1[I1] - pmax(s0[I1], c0)
    t13[I2] <- c1 - pmax(s0[I2], c0)


    ## Fourth, fourth, fifth and sixth year, 't36':
    t36 <- numeric(NROW(dat))
    s1 <- dat$birthdate + 6
    s0 <- dat$birthdate + 3
    I1 <- s1 > c0 & s1 < c1
    I2 <- s1 >= c1 & s0 < c1

    t36[I1] <- s1[I1] - pmax(s0[I1], c0)
    t36[I2] <- c1 - pmax(s0[I2], c0)

    dat$preg <- preg
    dat$t01 <- t01
    dat$t13 <- t13
    dat$t36 <- t36
    dat
}
