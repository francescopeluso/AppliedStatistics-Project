# ------------------------------------------------------------------------------
# Progetto finale del corso di Statistica Applicata - A.A. 2024/25
# Gruppo #5 - Corrado Giachetta, Francesco Peluso, Gerardo Selce, Anuar Zouhri
#
# Università degli Studi di Salerno - DIEM
# Dipartimento di Ingegneria dell'Informazione, Elettrica e Matematica Applicata
# ------------------------------------------------------------------------------

# Impostazione path cartella progetto (N.B. cambia a seconda di chi usa il file)
#setwd('~/GitHub Repos/AppliedStatistics-Project')     # path Francesco
setwd('C:/Users/anuar/AppliedStatistics-Project')     # path Corrado
#setwd('')     # path Anuar
#setwd('')     # path Gerardo

# ------------------------------------------------------------------------------
#package da scaricare 
install.packages("GGally")

# Lettura del dataset da file CSV
data = read.csv('./dataset.csv');

#------------------------------------------------
#STATISTICA DESCRITTIVA
#
# Scatterplot delle correlazioni tra le coppie di variabili
plot(data);

library(ggplot2)
library("GGally")

ggpairs(data)

library("corrplot")
corrplot.mixed(cor(data),number.cex=0.8,tl.cex=0.8)

lin_mod_all=lm(y_VideoQuality ~ ., data=data)
summary(lin_mod_all)

step_lin=step(lin_mod_all, trace = 1, data=data)
summary(step_lin)

median(data$y_VideoQuality) #analisi mediana
mean(data$y_VideoQuality) #analisi media
quantile(data$y_VideoQuality,probs=c(0.25,0.50,0.75)) #analisi quantili
boxplot(data[, !(names(data) %in% "y_VideoQuality")])
#-------------------------------------------------------------

#Valutazione della relazione tra y_VideoQuality e le varibili xi

#x1_ISO
dev.new()
plot(data$x1_ISO,data$y_VideoQuality,xlab="x1_ISO",ylab = 'y_VideoQuality')
fit1 = lm(data$y_VideoQuality~data$x1_ISO,data=data)
abline(fit1, col="red",lw=2)
summary(fit1)

#x2_FRatio
dev.new()
plot(data$x2_FRatio,data$y_VideoQuality,xlab="x2_FRatio",ylab = 'y_VideoQuality')
fit2 = lm(data$y_VideoQuality~data$x2_FRatio,data=data)
abline(fit2, col="red",lw=2)
summary(fit2)

#x3_TIME
dev.new()
plot(data$x3_TIME,data$y_VideoQuality,xlab="x3_TIME",ylab = 'y_VideoQuality')
fit3 = lm(data$y_VideoQuality~data$x3_TIME,data=data)
abline(fit3, col="red",lw=2)
summary(fit3)

#x4_MP
dev.new()
plot(data$x4_MP,data$y_VideoQuality,xlab="x4_MP",ylab = 'y_VideoQuality')
fit4 = lm(data$y_VideoQuality~data$x4_MP,data=data)
abline(fit4, col="red",lw=2)
summary(fit4)

#x5_CROP
dev.new()
plot(data$x5_CROP,data$y_VideoQuality,xlab="x5_CROP",ylab = 'y_VideoQuality')
fit5 = lm(data$y_VideoQuality~data$x5_CROP,data=data)
abline(fit5, col="red",lw=2)
summary(fit5)

#x6_FOCAL
dev.new()
plot(data$x6_FOCAL,data$y_VideoQuality,xlab="x1_ISO",ylab = 'y_VideoQuality')
fit6 = lm(data$y_VideoQuality~data$x6_FOCAL,data=data)
abline(fit6, col="red",lw=2)
summary(fit6)

#x7_PixDensity
dev.new()
plot(data$x7_PixDensity,data$y_VideoQuality,xlab="x1_ISO",ylab = 'y_VideoQuality')
fit6 = lm(data$y_VideoQuality~data$x7_PixDensity,data=data)
abline(fit6, col="red",lw=2)
summary(fit6)


