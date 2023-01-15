#--- Finanzwissenschaften ---#
# "Klimaschutzpolitik: Die Energiewende gestalten"
# Ersteller: Henning Dinkela | matr.nr.: 6155187
# Quellen: https://de.statista.com/statistik/daten/studie/42226/umfrage/welt-insgesamt-verbrauch-an-primaerenergie-in-millionen-tonnen-oelaequivalent/
# https://de.statista.com/statistik/daten/studie/159806/umfrage/bip-bruttoinlandsprodukt-pro-kopf-weltweit/


# Pakete ----------------------------------------------------------------

#install.packages("dplyr")
library(dplyr)
#install.packages("modelsummary")
library(modelsummary)
#install.packages("estimatr")
library(estimatr)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("lmtest")
library(lmtest)
library(zoo)
#install.packages("ggpubr")
library(ggpubr)
#install.packages("tidyverse")
library(tidyverse)

# lade Daten ---------------------------------------------------------------

setwd("/Users/Henning/Desktop/FiWi")
df = read.csv2("gdp_energy.csv", header=TRUE, sep=";", dec=",")
View(df)
head(df)
tail(df)
summary(df)

plot(df$energy, df$GDP)

# regressiere absolutes Pro-Kopf-Einkommen und Primärenergieverbrauch --------

reg = lm_robust(GDP ~ energy, data=df)
summary(reg)
modelsummary(reg, stars=c('***' = 0.01, '**' = 0.05, '*' = 0.1), gof_omit = "^(?!R2|Num)", title="Pro-Kopf-Einkommen als Funktion des Primärenergieverbrauchs")

yhat = reg$fitted.values
plot(df$energy, df$GDP, xlab="Primärenergieverbrauch", ylab="Pro-Kopf-Einkommen", col="blue")
lines(df$energy, yhat, col ="red", lwd =1.5)

# log-log-Modell

reg_loglog = lm_robust(log(GDP) ~ log(energy), data=df)
summary(reg_loglog)
modelsummary(reg_loglog, stars=c('***' = 0.01, '**' = 0.05, '*' = 0.1), gof_omit = "^(?!R2|Num)")

# Änderungen im log-log-Modell --> ln(y+∆y) = b0+b1*ln(X+∆X) | -ln(y)
#                             ln(y+∆y) -ln(y) = b0+b1*ln(X+∆X) -ln(y)
#                             ln(1+[∆y/y]) = b0+b1*ln(X+∆X) - b0+b1*ln(X)
#                             ∆y/y = b1*[ln(X+∆X) - ln(X)]
#                             ∆y/y = b1*[∆X/X]
#                             b1 = [∆y/y] / [∆X/X] --> Elastizität
# ====> b1 = 2,076 und ∆y/y = 2,076 * 1% = 2,076%
# Wenn X sich um 1% ändert, dann wird y um steigen 2,076%

yhat_loglog = reg_loglog$fitted.values
# df = df[order(df$energy),]
plot(log(df$energy), log(df$GDP), xlab="Primärenergieverbrauch", ylab="Pro-Kopf-Einkommen", col="blue", main="Pro-Kopf-Einkommen als Funktion des Primärenergieverbrauchs")
lines(log(df$energy), yhat_loglog, col="red", lwd=1.5)

df %>% 
  ggplot(aes(energy, GDP)) + geom_point() + stat_smooth()


# output for both regression in one modelsummary:
models = list()
models[["linear"]] = reg
models[["log-log"]] = reg_loglog
modelsummary(models, stars=c('***' = 0.01, '**' = 0.05, '*' = 0.1), gof_omit = "^(?!R2|Num)")


