# Q3 #######
require(MASS)
n = 500
mu1 <- mu2 <- mu3 <- 0
sigma1 <- sigma2 <- sigma3 <- 1
set.seed(1)
Z1 = rnorm(n = n, mean = mu1, sd = sigma1)
Z2 = rnorm(n = n, mean = mu2, sd = sigma2)
Z3 = rnorm(n = n, mean = mu3, sd = sigma3)

Y1 = 1 + Z1
Y2 = 5 + 2 * Z1 + Z2
Y_complete = data.frame(Y1, Y2)

# (a)
a = 2
b = 0
c = a*(Y1-1)+b*(Y2-5)+Z3
ind_obs = which(c>=0)
Y2_obs = Y2[ind_obs]

plot(density(Y_complete$Y2), lwd = 2, col = "blue", xlab = "Y2", main = "MAR", ylim = c(0, 0.3))
lines(density(Y2_obs), lwd = 2, col = "red")
legend(8, 0.3, legend = c("Complete data", "Observed data"), col = c("blue", "red"), lty = c(1, 1), lwd = c(2, 2), bty = "n")

# (b)
Y2[which(c<0)] = NA
Y2
data = data.frame(Y1,Y2)
fit = lm(Y2 ~ Y1, data = data)
summary(fit)
set.seed(1)
predicted_sri = predict(fit, newdata = data) + rnorm(nrow(data), 0, sigma(fit))

Y2_sri = ifelse(is.na(Y2) == TRUE, predicted_sri, Y2)

plot(density(Y_complete$Y2), lwd = 2, col = "blue", xlab = "Y2", ylim = c(0, 0.3), main = "stochastic regression imputation")
lines(density(Y2_sri), lwd = 2, col = "red")
legend(8, 0.3, legend = c("Complete data", "Completed data"), col = c("blue", "red"), lty = c(1, 1), lwd = c(2, 2), bty = "n")


# (c)
a = 0
b = 2
c = a*(Y1-1)+b*(Y2-5)+Z3
ind_obs = which(c>=0)
Y2_obs = Y2[ind_obs]

plot(density(Y_complete$Y2), lwd = 2, col = "blue", xlab = "Y2", main = "MNAR", ylim = c(0, 0.3))
lines(density(Y2_obs), lwd = 2, col = "red")
legend(8, 0.3, legend = c("Complete data", "Observed data"), col = c("blue", "red"), lty = c(1, 1), lwd = c(2, 2), bty = "n")


# (d)
Y2[which(c<0)] = NA
Y2
data = data.frame(Y1,Y2)
fit = lm(Y2 ~ Y1, data = data)
summary(fit)
set.seed(1)
predicted_sri = predict(fit, newdata = data) + rnorm(nrow(data), 0, sigma(fit))

Y2_sri = ifelse(is.na(Y2) == TRUE, predicted_sri, Y2)

plot(density(Y_complete$Y2), lwd = 2, col = "blue", xlab = "Y2", ylim = c(0, 0.3), main = "stochastic regression imputation")
lines(density(Y2_sri), lwd = 2, col = "red")
legend(8, 0.3, legend = c("Complete data", "Completed data"), col = c("blue", "red"), lty = c(1, 1), lwd = c(2, 2), bty = "n")



# Q4 #########
# load("databp.Rdata")
load("~/Desktop/Semester 1/Incomplete Data Analysis (IDA)/Assignment1/databp.Rdata")
# (a)
ind = which(is.na(databp$recovtime) == FALSE)
mcc = mean(databp$recovtime, na.rm = TRUE)
secc = sd(databp$recovtime, na.rm = TRUE)/sqrt(length(ind))
mcc; secc
corcc1 = cor(databp$logdose, databp$recovtime, use = "complete", method = "pearson")
corcc2 = cor(databp$bloodp, databp$recovtime, use = "complete", method = "pearson")
corcc1; corcc2

# (b)
bpmi = ifelse(is.na(databp$recovtime) == TRUE, mean(databp$recovtime, na.rm = TRUE), databp$recovtime)
n = length(bpmi)
mmi = mean(bpmi)
semi = sd(bpmi)/sqrt(n)
mmi; semi
cormi1 = cor(databp$logdose, bpmi, method = "pearson")
cormi2 = cor(databp$bloodp, bpmi, method = "pearson")
cormi1; cormi2

# (c)
fitbp = lm(recovtime ~ logdose + bloodp, data = databp)
summary(fitbp)
coef(fitbp)
predri = predict(fitbp, newdata = databp)
predri[4]; predri[10]; predri[22]

bpri = ifelse(is.na(databp$recovtime) == TRUE, predri, databp$recovtime)  
mri = mean(bpri)
seri = sd(bpri)/sqrt(n)
mri; seri
corri1 = cor(databp$logdose, bpri, method = "pearson")
corri2 = cor(databp$bloodp, bpri, method = "pearson")
corri1; corri2

#check
plot(fitbp$fitted.values, residuals(fitbp), xlab = "Fitted values", ylab = "Residuals")
qqnorm(rstandard(fitbp), xlim = c(-3, 3), ylim = c(-3, 3))
qqline(rstandard(fitbp), col = 2)

# (d)
set.seed(1)
predsri <- predict(fitbp, newdata = databp) + rnorm(n, 0, sigma(fitbp))
predsri[4]; predsri[10]; predsri[22]

bpsri <- ifelse(is.na(databp$recovtime) == TRUE, predsri, databp$recovtime)  
msri <- mean(bpsri)
sesri <- sd(bpsri)/sqrt(n)
msri; sesri
corsri1 = cor(databp$logdose, bpsri, method = "pearson")
corsri2 = cor(databp$bloodp, bpsri, method = "pearson")
corsri1; corsri2

# (e)
pred = predict(fitbp, newdata = databp)
pred_other=pred[-c(4,10,22)]

pred_4_diff=(pred_other-pred[4])^2
pred_4_ind=which.min(pred_4_diff) # subject 6
pred[4]=databp$recovtime[6]

pred_10_diff=(pred_other-pred[10])^2
pred_10_ind=which.min(pred_10_diff) # subject 2
pred[10]=databp$recovtime[2]

pred_22_diff=(pred_other-pred[22])^2
pred_22_ind=which.min(pred_22_diff) # subject 17
pred[22]=databp$recovtime[17]

bppmm <- ifelse(is.na(databp$recovtime) == TRUE, pred, databp$recovtime)
mpmm <- mean(bppmm)
sepmm <- sd(bppmm)/sqrt(n)
mpmm; sepmm
corpmm1 = cor(databp$logdose, bppmm, method = "pearson")
corpmm2 = cor(databp$bloodp, bppmm, method = "pearson")
corpmm1; corpmm2
