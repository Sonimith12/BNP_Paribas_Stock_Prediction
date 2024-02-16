
library(forecast)
library(lmtest)

sample_data <- c(32.37,36.6,35.58,30.35,25.73,30.33,30.17,34.6,36.98,38.81,42.94,42.6,
                 46.21,43.1,40.04,42.31,45.33,41.97,48.63,47.4,50,54.54,55.25,56.7,
                 57.45,59.5,55.99,54.11,51.37,49.54,49.5,51.4,52.52,50.14,51.56,49.3,
                 46.78,52.1,56.57,56.44,54.93,54.15,59.31,56.3,52.46,55.26,56.11,52.2,
                 43.72,43,44.23,46.24,49.76,39.76,44.35,45.6,45.77,52.84,54.78,60.6,
                 59.18,55.1,62.43,64.78,62.82,63.06,65.57,63.9,68.25,67.04,63.58,62.3,
                 66.6,65.4,60.17,63.98,53.06,53.19,55.67,50.6,52.71,46.13,44.38,39.5,
                 40.97,45.1,42.61,47.45,40.88,41.76,42.18,41,44.67,46.81,50.97,52.8,
                 48.04,43.8,27.51,28.67,32.26,35.37,34.12,36.6,30.98,29.85,42.97,43.1,
                 39.78,49.3,51.88,53.39,55.85,52.87,51.45,53.7,55.44,57.9,55.08,60.8)

sample_data_latest <- c(32.37,36.6,35.58,30.35,25.73,30.33,30.17,34.6,36.98,38.81,42.94,42.6,
                        46.21,43.1,40.04,42.31,45.33,41.97,48.63,47.4,50,54.54,55.25,56.7,
                        57.45,59.5,55.99,54.11,51.37,49.54,49.5,51.4,52.52,50.14,51.56,49.3,
                        46.78,52.1,56.57,56.44,54.93,54.15,59.31,56.3,52.46,55.26,56.11,52.2,
                        43.72,43,44.23,46.24,49.76,39.76,44.35,45.6,45.77,52.84,54.78,60.6,
                        59.18,55.1,62.43,64.78,62.82,63.06,65.57,63.9,68.25,67.04,63.58,62.3,
                        66.6,65.4,60.17,63.98,53.06,53.19,55.67,50.6,52.71,46.13,44.38,39.5,
                        40.97,45.1,42.61,47.45,40.88,41.76,42.18,41,44.67,46.81,50.97,52.8,
                        48.04,43.8,27.51,28.67,32.26,35.37,34.12,36.6,30.98,29.85,42.97,43.1,
                        39.78,49.3,51.88,53.39,55.85,52.87,51.45,53.7,55.44,57.9,55.08,60.8,
                        62.99151281,	52.50980951,	51.93536683,	49.69975483,	53.09060594,
                        45.37675638,	46.01649872,	46.47897222,	43.59750832,	47.51284046,
                        53.41010712,	53.25011691)


ts_data <- ts(sample_data, frequency = 12)
print(ts_data)

fit_hw_additive <- HoltWinters(ts_data)

forecasts_hw_additive <- forecast(fit_hw_additive, h = 12)  

print(forecasts_hw_additive)

plot(forecasts_hw_additive, main ="Les Actions de la bourse BNP Paribas", xlab="Mois (2012 Jan - 2022 Dec)", ylab = "Valeurs en EUR")
legend("topleft", legend = c("Prévision", "95% Intervalle de confiance", "80% Intervalle de confiance"), col = c("deepskyblue2", "gray", "cornflowerblue"), lty = c(1, 1), cex = 0.8)

forecast_values_HW <- forecasts_hw_additive$mean

print(forecast_values_HW)

alpha <- fit_hw_additive$alpha
beta <- fit_hw_additive$beta
gamma <- fit_hw_additive$gamma

print(alpha)
print(beta)
print(gamma)




#components <- decompose(ts_data)
#plot(components)

#RMSE 
actual_value_2022 = c(62.99151281,	52.50980951,	51.93536683,	49.69975483,	53.09060594,	45.37675638,	46.01649872,	46.47897222,	43.59750832,	47.51284046,	53.41010712,	53.25011691)
print(actual_value_2022)

#par lissage exponential double
forecast_values_LED = c(53.47569978,	52.76738297,	49.72335769,	50.88124135,	51.23587516,	48.91004864,	49.12059438,	49.67023826,	49.14446023,	50.51304193,	54.24335273,	55.57459543)

#par holt-winter = forecast_values_HW


rmse_holt_winter <- sqrt(mean((forecast_values_HW - actual_value_2022)^2))
rmse_LED <- sqrt(mean((forecast_values_LED - actual_value_2022)^2))
print(rmse_holt_winter)
print(rmse_LED)


actual_value_2022_ts <- ts(c(sample_data, actual_value_2022), frequency = 12)
plot(actual_value_2022_ts, main ="Les Actions de la bourse BNP Paribas", xlab="Mois (2012 Jan - 2022 Dec)", ylab = "Valeurs en EUR")
acf_resid2 <- acf(sample_data_latest, main = "La fonction d'autocorrélation", lag.max = 40, plot = TRUE)


#comparer et plotter les deux méthode
predicted_value_HW <- ts(c(sample_data, forecast_values_HW), frequency = 12)
predicted_value_LED <- ts(c(sample_data, forecast_values_LED), frequency = 12)
plot(predicted_value_HW, main ="Comparaison Holt-Winters et LED", col="red",  lty = 2, xlab="Mois (2012 Jan - 2022 Dec)", ylab = "Valeurs  EUR")

lines(predicted_value_LED, col = "blue", lty = 2)
lines(actual_value_2022_ts, col = "black" )

legend("topright", legend = c("Actual Values 2022", "Holt-Winters Prediction", "LED Prediction"),
       col = c("black", "red", "blue"), lty = 1, cex = 0.4)



# test normalilé de residu du holt winter
residuals_hw <- forecast_values_HW - actual_value_2022
shapiro.test(residuals_hw)

# Assuming residuals_hw contains your residuals

# Q-Q plot to check normality
qqnorm(residuals_hw)
qqline(residuals_hw, col = 2)  # Add a reference line

# Histogram of residuals
hist(residuals_hw, breaks = 20, main = "Histogram of Residuals", xlab = "Residuals", ylab = "Frequency")
plot(density(residuals_hw), xlab = "Residuals")

# autocorrelation de residues HW
print(residuals_hw)
acf_resid1 <- acf(residuals_hw, main = "Autocorrelation de Residus")






#test normalité de LED
residus <- forecast_values_LED - actual_value_2022
shapiro.test(residus)
print(residus)
plot(density(residus), xlab = "Residuals")

# autocorrelation de residues LED
acf_resid <- acf(residus, main = "Autocorrelation de Residus")



# Hétéroscédasticité
# LED
plot(forecast_values_LED, residus, xlab = "Forecasted Values", ylab = "Residuals", main = "Residuals vs. Forecasted Values")
abline(h = 0, col = "red")

#HW
print(forecast_values_HW)
forecast_values_HW_he = c(60.79666, 61.30282, 56.91732, 57.68235, 56.97374, 55.39662, 55.86018, 56.95194, 59.21851, 61.47745, 64.71239, 65.10397)

plot(forecast_values_HW_he, residuals_hw, xlab = "Forecasted Values", ylab = "Residuals", main = "Residuals vs. Forecasted Values")
abline(h = 0, col = "red")



forcast_LED_2023 = c(58.52087861,	50.16744496,	50.44417754,	47.79906142,	53.14128825,	44.84907555,	44.47983331,	45.9847927,	42.91486651,	47.54297831,	52.57428604,	54.02473474)
print(forcast_LED_2023)
real_and_forcast_LED <- ts(c(sample_data_latest, forcast_LED_2023), frequency = 12)

plot(real_and_forcast_LED, main ="Les Actions de la bourse BNP Paribas", xlab="Mois (2012 Jan - 2022 Dec)", ylab = "Valeurs en EUR", col="orange")

lines(actual_value_2022_ts, col = "black")




