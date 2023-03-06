library('AER')
data <- read.csv('BH-Sph-DM.csv')
naive_OLS <- lm(log.M_BH.M_Sun. ~ log.M_sph.M_Sun., data=data)
IV <- ivreg(log.M_BH.M_Sun. ~ log.M_sph.M_Sun. | log.M_DM.M_Sun., data=data)

summary(naive_OLS)
summary(IV)