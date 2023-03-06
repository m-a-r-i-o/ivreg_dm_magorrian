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

pdf("bootstrap.pdf")
plot(density(sapply(1:1000, bootstrap_ols)), xlim = c(0,5), xlab = "regression coefficient", ylab = "frequency", main = "")
lines(density(sapply(1:1000, bootstrap_iv)), col = "#C44218")

predicted_values <- lm(d$log.M_sph.M_Sun. ~ d$log.M_DM.M_Sun.)$fitted.values
secondstage <- lm(d$log.M_BH.M_Sun. ~ predicted_values)
summary(secondstage)

pdf("naive_OLS.pdf")
plot(d$log.M_sph.M_Sun., d$log.M_BH.M_Sun., ylab = expression(M[BH]), xlab = expression(M[bulge]), main = "Naive OLS regression", pch = 16)
#abline(mean(d$log.M_BH.M_Sun.)-mean(d$log.M_sph.M_Sun.),1, col = "#A0A0A0")
#abline(mean(d$log.M_BH.M_Sun.)-2*mean(d$log.M_sph.M_Sun.),2, col = "#C44218")
abline(naive_OLS$coefficients)
abline(secondstage$coefficients, col = "#C44218")

pdf("second_stage.pdf")
plot(predicted_values, d$log.M_BH.M_Sun., ylab = expression(M[BH]), xlab = expression("Predicted "*M[bulge]), main = "Second stage IV regression", pch = 16)
#abline(mean(d$log.M_BH.M_Sun.)-2*mean(predicted_values),2, col = "#C44218")
#abline(mean(d$log.M_BH.M_Sun.)-mean(predicted_values),1, col = "#A0A0A0")
abline(naive_OLS$coefficients)
abline(secondstage$coefficients, col = "#C44218")

