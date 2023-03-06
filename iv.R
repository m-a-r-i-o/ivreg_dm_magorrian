library('AER')
d <- read.csv('BH-Sph-DM.csv')
names(data)
naive_OLS <- lm(log.M_BH.M_Sun. ~ log.M_sph.M_Sun., data=d)
IV <- ivreg(log.M_BH.M_Sun. ~ log.M_sph.M_Sun. | log.M_DM.M_Sun., data=d, diagnostics=TRUE)

summary(naive_OLS)
summary(IV)

#Let's bootstrap within the error bars, shall we?

set.seed(888)

bootstrap_ols <- function(i)
{
    y <- d$log.M_BH.M_Sun. + rnorm(length(d$log.M_BH.M_Sun.))*d$log.M_BH.M_Sun._std
    x <- d$log.M_sph.M_Sun. + rnorm(length(d$log.M_sph.M_Sun.))*d$log.M_sph.M_Sun._std
    z <- d$log.M_DM.M_Sun. + rnorm(length(d$log.M_DM.M_Sun.))*d$log.M_DM.M_Sun._std
    lm(y ~ x)$coefficients[2]
}

bootstrap_iv <- function(i)
{
    y <- d$log.M_BH.M_Sun. + rnorm(length(d$log.M_BH.M_Sun.))*d$log.M_BH.M_Sun._std
    x <- d$log.M_sph.M_Sun. + rnorm(length(d$log.M_sph.M_Sun.))*d$log.M_sph.M_Sun._std
    z <- d$log.M_DM.M_Sun. + rnorm(length(d$log.M_DM.M_Sun.))*d$log.M_DM.M_Sun._std
    ivreg(y ~ x | z)$coefficients[2]
}

plot(density(sapply(1:1000, bootstrap_ols)), xlim = c(0,5), xlab = "regression coefficient", ylab = "frequency", main = "")
lines(density(sapply(1:1000, bootstrap_iv)), col = "#C44218")
