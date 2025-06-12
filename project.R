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

dev.new()
plot(data$x1_ISO,data$y_VideoQuality,xlab="x1_ISO",ylab = 'y_VideoQuality')
fit1 = lm(data$y_VideoQuality~data$x1_ISO,data=data)
abline(fit1, col="red",lw=2)
summary(fit1)


