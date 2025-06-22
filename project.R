# ------------------------------------------------------------------------------
# Progetto finale del corso di Statistica Applicata - A.A. 2024/25
# Gruppo #5 - Corrado Giachetta, Francesco Peluso, Gerardo Selce, Anuar Zouhri
#
# Università degli Studi di Salerno - DIEM
# Dipartimento di Ingegneria dell'Informazione, Elettrica e Matematica Applicata
# ------------------------------------------------------------------------------

# Impostazione path cartella progetto (N.B. cambia a seconda di chi usa il file)
#setwd('~/GitHub Repos/AppliedStatistics-Project')     # path Francesco
#setwd('C:/Users/anuar/AppliedStatistics-Project')     # path Anuar
#setwd('/Users/corry/AppliedStatistics-Project')     # path Corrado
#setwd('C:/Users/ACER/Desktop/GitHub/AppliedStatistics-Project')     # path Gerardo

# ------------------------------------------------------------------------------

#package da scaricare 
install.packages("GGally")

# Lettura del dataset da file CSV
data = read.csv('./dataset.csv');

#per la funzione alpha
install.packages("scales")  
library(scales)


# ------------------------------------------------------------------------------
# PUNTO 1:
# Analisi preliminare dei dati.
# Scatterplot delle correlazioni tra le coppie di variabili
# ------------------------------------------------------------------------------

plot(data);


lin_mod_all=lm(y_VideoQuality ~ ., data=data)
summary(lin_mod_all)

step_lin=step(lin_mod_all, trace = 1, data=data)
summary(step_lin)

median(data$y_VideoQuality) #analisi mediana
mean(data$y_VideoQuality) #analisi media
quantile(data$y_VideoQuality,probs=c(0.25,0.50,0.75)) #analisi quantili
boxplot(data[, !(names(data) %in% "y_VideoQuality")])
summary(data$y_VideoQuality)
#Istogrammi
hist(data$y_VideoQuality,main="Distribuzione di y_VideoQuality", xlab="y_VideoQuality")
hist(data$x1_ISO)
hist(data$x2_FRatio)
hist(data$x3_TIME)
hist(data$x4_MP)
hist(data$x5_CROP)
hist(data$x6_FOCAL)
hist(data$x7_PixDensity)

#Normalità
par(mfrow = c(2, 2))  # 2 righe, 2 colonne, per rappresentare i qqplot


qqnorm(data$x1_ISO, main = "Q-Q Plot di x1_ISO")
qqline(data$x1_ISO,  col = "red", lwd = 2)
qqnorm(data$x2_FRatio, main = "Q-Q Plot di x2_FRatio")
qqline(data$x2_FRatio,  col = "red", lwd = 2)
qqnorm(data$x3_TIME, main = "Q-Q Plot di x3_TIME")
qqline(data$x3_TIME,  col = "red", lwd = 2)
qqnorm(data$x4_MP, main = "Q-Q Plot di x4_MP")
qqline(data$x4_MP, col = "red", lwd = 2)

par(mfrow = c(2, 2))   # 2 righe, 2 colonne, per rappresentare i qqplot


qqnorm(data$x5_CROP, main = "Q-Q Plot di x5_CROP")
qqline(data$x5_CROP,  col = "red", lwd = 2)
qqnorm(data$x6_FOCAL, main = "Q-Q Plot di x6_FOCAL")
qqline(data$x6_FOCAL,  col = "red", lwd = 2)
qqnorm(data$x7_PixDensity, main = "Q-Q Plot di x7_PixDensity")
qqline(data$x7_PixDensity,  col = "red", lwd = 2)

qqnorm(data$y_VideoQuality, main = "Q-Q Plot di y_VideoQuality")
qqline(data$y_VideoQuality)

shapiro.test(data$x6_FOCAL)


# ------------------------------------------------------------------------------
# PUNTO 2:
# Valutazione della relazione tra variabile dipendente e le singole
# variabili indipendenti
# ------------------------------------------------------------------------------
# Basic Scatterplot Matrix
pairs(data , upper.panel = NULL)

#Advanced Scatterplot matrix
library(ggplot2)
library("GGally")
ggpairs(data, upper = list(continuous = "blank"))   # nasconde i grafici sotto la diagonale

#CorrPlot
library("corrplot")
corrplot.mixed(cor(data),number.cex=0.8,tl.cex=0.8)

#matrice dei coefficienti di correlazione
cor(data)

#x1_ISO - sensibilità sensore
dev.new()
plot(data$x1_ISO,data$y_VideoQuality,xlab="x1_ISO",ylab = 'y_VideoQuality')
fit1 = lm(data$y_VideoQuality~data$x1_ISO,data=data)
abline(fit1, col="red",lw=2)
summary(fit1)


#x2_FRatio - rapporto focale
dev.new()
plot(data$x2_FRatio,data$y_VideoQuality,xlab="x2_FRatio",ylab = 'y_VideoQuality')
fit2 = lm(data$y_VideoQuality~data$x2_FRatio,data=data)
abline(fit2, col="red",lw=2)
summary(fit2)

#x3_TIME - tempo di esposizione
dev.new()
plot(data$x3_TIME,data$y_VideoQuality,xlab="x3_TIME",ylab = 'y_VideoQuality')
fit3 = lm(data$y_VideoQuality~data$x3_TIME,data=data)
abline(fit3, col="red",lw=2)
summary(fit3)

#x4_MP - megapixel sensore
dev.new()
plot(data$x4_MP,data$y_VideoQuality,xlab="x4_MP",ylab = 'y_VideoQuality')
fit4 = lm(data$y_VideoQuality~data$x4_MP,data=data)
abline(fit4, col="red",lw=2)
summary(fit4)

#x5_CROP - crop factor
dev.new()
plot(data$x5_CROP,data$y_VideoQuality,xlab="x5_CROP",ylab = 'y_VideoQuality')
fit5 = lm(data$y_VideoQuality~data$x5_CROP,data=data)
abline(fit5, col="red",lw=2)
summary(fit5)

#x6_FOCAL - focale
dev.new()
plot(data$x6_FOCAL,data$y_VideoQuality,xlab="x6_FOCAL",ylab = 'y_VideoQuality')
fit6 = lm(data$y_VideoQuality~data$x6_FOCAL,data=data)
abline(fit6, col="red",lw=2)
summary(fit6)

#x7_PixDensity - densità pixel
dev.new()
plot(data$x7_PixDensity,data$y_VideoQuality,xlab="x7_PixDensity",ylab = 'y_VideoQuality')
fit6 = lm(data$y_VideoQuality~data$x7_PixDensity,data=data)
abline(fit6, col="red",lw=2)
summary(fit6)



# ------------------------------------------------------------------------------
# PUNTO 2.5
# Valutazione della relazione tra variabile dipendente e le singole
# variabili indipendenti al quadrato
# ------------------------------------------------------------------------------

#x1_ISO - sensibilità sensore
dev.new()
plot(data$x1_ISO,data$y_VideoQuality,xlab="x1_ISO",ylab="y_VideoQuality")
fit7 <- lm(y_VideoQuality ~ poly(x1_ISO, 2, raw = TRUE), data = data)
x_vals <- seq(min(data$x1_ISO), max(data$x1_ISO), length.out = 200)
y_pred <- predict(fit7, newdata = data.frame(x1_ISO = x_vals))
lines(x_vals, y_pred, col = "red", lwd = 2)
summary(fit7)

#x2_FRatio - rapporto focale
dev.new()
plot(data$x2_FRatio,data$y_VideoQuality,xlab="x2_FRatio",ylab="y_VideoQuality")
fit8 <- lm(y_VideoQuality ~ poly(x2_FRatio, 2, raw = TRUE), data = data)
x_vals <- seq(min(data$x2_FRatio), max(data$x2_FRatio), length.out = 200)
y_pred <- predict(fit8, newdata = data.frame(x2_FRatio = x_vals))
lines(x_vals, y_pred, col = "red", lwd = 2)
summary(fit8)

#x3_TIME - tempo di esposizione
dev.new()
plot(data$x3_TIME,data$y_VideoQuality,xlab="x3_TIME",ylab="y_VideoQuality")
fit11 <- lm(y_VideoQuality ~ poly(x3_TIME, 2, raw = TRUE), data = data)
x_vals <- seq(min(data$x3_TIME), max(data$x3_TIME), length.out = 200)
y_pred <- predict(fit11, newdata = data.frame(x3_TIME = x_vals))
lines(x_vals, y_pred, col = "red", lwd = 2)
summary(fit11)

#x4_MP - megapixel sensore
dev.new()
plot(data$x4_MP,data$y_VideoQuality,xlab="x4_MP",ylab="y_VideoQuality")
fit12 <- lm(y_VideoQuality ~ poly(x4_MP, 2, raw = TRUE), data = data)
x_vals <- seq(min(data$x4_MP), max(data$x4_MP), length.out = 200)
y_pred <- predict(fit12, newdata = data.frame(x4_MP = x_vals))
lines(x_vals, y_pred, col = "red", lwd = 2)
summary(fit12)

#x5_CROP - crop factor
dev.new()
plot(data$x5_CROP,data$y_VideoQuality,xlab="x5_CROP",ylab="y_VideoQuality")
fit9 <- lm(y_VideoQuality ~ poly(x5_CROP, 2, raw = TRUE), data = data)
x_vals <- seq(min(data$x5_CROP), max(data$x5_CROP), length.out = 200)
y_pred <- predict(fit9, newdata = data.frame(x5_CROP = x_vals))
lines(x_vals, y_pred, col = "red", lwd = 2)
summary(fit9)

#x6_FOCAL - focale
dev.new()
plot(data$x6_FOCAL,data$y_VideoQuality,xlab="x6_FOCAL",ylab="y_VideoQuality")
fit13 <- lm(y_VideoQuality ~ poly(x6_FOCAL, 2, raw = TRUE), data = data)
x_vals <- seq(min(data$x6_FOCAL), max(data$x6_FOCAL), length.out = 200)
y_pred <- predict(fit13, newdata = data.frame(x6_FOCAL = x_vals))
lines(x_vals, y_pred, col = "red", lwd = 2)
summary(fit13)

#x7_PixDensity - densità pixel
dev.new()
plot(data$x7_PixDensity,data$y_VideoQuality,xlab="x7_PixDensity",ylab="y_VideoQuality")
fit14 <- lm(y_VideoQuality ~ poly(x7_PixDensity, 2, raw = TRUE), data = data)
x_vals <- seq(min(data$x7_PixDensity), max(data$x7_PixDensity), length.out = 200)
y_pred <- predict(fit14, newdata = data.frame(x7_PixDensity = x_vals))
lines(x_vals, y_pred, col = "red", lwd = 2)
summary(fit14)

#x5_CROP - crop factor e x7_PixDensity
dev.new()
plot(data$x5_CROP,data$x7_PixDensity,xlab="x5_CROP",ylab="x7_PixDensity")
fit10 <- lm(x7_PixDensity ~ poly(x5_CROP, 2, raw = TRUE), data = data)
x_vals <- seq(min(data$x5_CROP), max(data$x5_CROP), length.out = 200)
y_pred <- predict(fit10, newdata = data.frame(x5_CROP = x_vals))
lines(x_vals, y_pred, col = "red", lwd = 2)
summary(fit10)


# ------------------------------------------------------------------------------
# PUNTO 3:
# Definizione del modello statistico dei dati tramite regressione lineare
# multipla
# ------------------------------------------------------------------------------

# Modello con tutte le variabili indipendenti
model_full <- lm(y_VideoQuality ~ ., data=data)
summary(model_full)

# Il modello completo presenta R^2 = 0.7871 e Adj. R^2 = 0.7709, 
# indicando una buona capacità esplicativa. Il p-value globale è significativo (< 2.2e-16),
# e le variabili x1_ISO, x2_FRatio, x3_TIME e x5_CROP risultano significative.

# Creiamo ora un modello ridotto, includendo solo le variabili significative
model_reduced <- lm(y_VideoQuality ~ x1_ISO + x2_FRatio + x3_TIME + x5_CROP, data=data)
summary(model_reduced) #Modello 1

# Tutti i regressori restano altamente significativi.
# R^2 e Adj. R^2 sono molto simili al modello completo, quindi il modello è più
# "leggero" mantenendo comunque delle buone prestazioni.

# Dalle analisi precedenti è emersa una possibile non linearità in alcune variabili.
# Aggiungiamo quindi termini quadratici per migliorare l'adattamento.


model_quad <- lm(y_VideoQuality ~ 
                   x1_ISO + I(x1_ISO^2) +
                   x2_FRatio + I(x2_FRatio^2) +
                   x3_TIME + x5_CROP,
                 data = data)
summary(model_quad) #Modello 2

# I termini quadratici migliorano il modello: R^2 e Adj. R^2 aumentano,
# il residuo standard diminuisce e i coefficienti sono tutti altamente significativi.

# Confronto tramite AIC e MSE per valutare le prestazioni dei modelli
extractAIC(model_full)
extractAIC(model_reduced)
extractAIC(model_quad)

mse_full = mean(residuals(model_full)^2); print(mse_full);
mse_reduced = mean(residuals(model_reduced)^2); print(mse_reduced);
mse_quad = mean(residuals(model_quad)^2); print(mse_quad);

# Il modello con termini quadratici risulta il migliore finora,
# con un indice AIC di 460.7592 e un MSE pari a 87.1466

# Proviamo ora ad automatizzare il processo di costruzione dei modelli, tramite
# approcci stepwise (basata su AIC classico, ovvero parametro k = 2)

# Partiamo dal modello completo
model_stepwise <- step(model_full, direction = "both", trace = 1, k = 2)
summary(model_stepwise)
extractAIC(model_stepwise) # AIC = 514.1132
mse_stepwise = mean(residuals(model_stepwise)^2); print(mse_stepwise); # MSE = 151

# Non sembrano esserci miglioramenti, anzi, sono stati lasciati dei regressori
# abbastanza significativi come x7_PixDensity, e il valore dell'indice AIC
# e dell'MSE sono aumentati.

# Proviamo a costruire un ulteriore modello con termini quadratici e interazioni
# tra le varie variabili indipendenti del dataset
model_step_interactions <- lm(y_VideoQuality ~ (.)^2 + 
                                I(x1_ISO^2)+ I(x2_FRatio^2)+I(x3_TIME^2)+
                                I(x4_MP^2)+I(x5_CROP^2)+I(x6_FOCAL^2)+I(x7_PixDensity^2), 
                              data = data)
model_step_interactions <- step(model_step_interactions, k = 2)
summary(model_step_interactions) #Modello 3
extractAIC(model_step_interactions) # AIC = 448.2659
mse_step_interactions = mean(residuals(model_step_interactions)^2); print(mse_step_interactions); # MSE = 61.72313

# Notiamo come nonostante la funzione step() abbia lasciato dei regressori apparentemente
# non significativi (es. x4MP con p-value = 0.86183 > 0.5), ma che comunque sia
# riuscito a costruire un modello con un indice AIC più basso e con MSE più basso

# Costruiamo un modello simile, escludendo x4_MP e x6_FOCAL (che apparentemente
# sembrano essere non significative)
model_step_interactions_reduced <- lm(
  y_VideoQuality ~ (.)^2 +
    I(x1_ISO^2) + I(x2_FRatio^2) + I(x3_TIME^2) + 
    I(x5_CROP^2) + I(x7_PixDensity^2), 
  data = data[, !names(data) %in% c("x4_MP", "x6_FOCAL")]
)
model_step_interactions_reduced <- step(model_step_interactions_reduced, k = 2)
summary(model_step_interactions_reduced) #Modello 4
extractAIC(model_step_interactions_reduced)
mse_step_interactions_rdcd = mean(residuals(model_step_interactions_reduced)^2); print(mse_step_interactions_rdcd);

# Andando a rimuovere i due regressori, notiamo come gli indici AIC e MSE non
# siano variati di molto (rispettivamente 451.666 e 76.45178), quindi **potrebbe**
# essere il modello ideale da usare, dove non si manifesta un overfitting rispetto
# al dataset utilizzato

# Confronto AIC di tutti i modelli finora creati
extractAIC(model_full)
extractAIC(model_reduced)
extractAIC(model_quad)
extractAIC(model_stepwise)
extractAIC(model_step_interactions)
extractAIC(model_step_interactions_reduced)

# Introduzione di termini cubici per migliorare ulteriormente il fit
model_cubic <- lm(
  y_VideoQuality ~ 
    (x1_ISO + x2_FRatio + x3_TIME + x5_CROP + x6_FOCAL + x7_PixDensity)^2 +
    I(x1_ISO^2) + I(x2_FRatio^2) + I(x3_TIME^2) + I(x5_CROP^2)+ I(x5_CROP^2) + I(x6_FOCAL^2) + I(x7_PixDensity^2) +
    I(x1_ISO^3) + I(x2_FRatio^3) + I(x3_TIME^3) + I(x5_CROP^3) + I(x6_FOCAL^3) + I(x7_PixDensity^3),
  data = data
)

model_cubic_step <- step(model_cubic, k = 2)
summary(model_cubic_step) #Modello 5
extractAIC(model_cubic_step)
mse_step_cubic = mean(residuals(model_cubic_step)^2); print(mse_step_cubic);

# Quest'ultimo modello vediamo come anch'esso abbassa i valori dell'indice AIC
# (pari a 431.9128) e dell'MSE (pari a 55.65254)


# ------------------------------------------------------------------------------
# PUNTO 4-5:
# Stima dei parametri dei modelli e intervalli di confidenza
# Calcolo del coefficiente di determinazione e grafici diagnostici
# ------------------------------------------------------------------------------

# Modello completo
summary(model_full)
confint(model_full, level = 0.95)
confint(model_full, level = 0.90)
dev.new(width = 550, height = 330, unit = "px")
par(mfrow=c(2,2))
plot(model_full, main = "Diagnostica - Modello completo")
dev.print(device=pdf,"diag_model_full.pdf")

# Modello ridotto (Modello 1)
summary(model_reduced)
confint(model_reduced, level = 0.95)
confint(model_reduced, level = 0.90)
dev.new(width = 550, height = 330, unit = "px")
par(mfrow=c(2,2))
plot(model_reduced, main = "Diagnostica - Modello 1")
dev.print(device=pdf,"diag_model_reduced.pdf")

# Modello quadratico (Modello 2)
summary(model_quad)
confint(model_quad, level = 0.95)
confint(model_quad, level = 0.90)
dev.new(width = 550, height = 330, unit = "px")
par(mfrow=c(2,2))
plot(model_quad, main = "Diagnostica - Modello 2")
dev.print(device=pdf,"diag_model_quad.pdf")


# Modello con interazioni (Modello 3)
summary(model_step_interactions)
confint(model_step_interactions, level = 0.95)
confint(model_step_interactions, level = 0.90)
dev.new(width = 550, height = 330, unit = "px")
par(mfrow=c(2,2))
plot(model_step_interactions, main = "Diagnostica - Modello interazioni")
dev.print(device=pdf,"diag_model_step_interactions.pdf")

# Modello cubico (Modello 5- finale)
summary(model_cubic_step)
confint(model_cubic_step, level = 0.95)
confint(model_cubic_step, level = 0.90)
dev.new(width = 550, height = 330, unit = "px")
par(mfrow=c(2,2))
plot(model_cubic_step, main = "Diagnostica - Modello cubico")
dev.print(device=pdf,"diag_model_cubic_step.pdf")


# ------------------------------------------------------------------------------
# PUNTO 6:
# Analisi di normalità dei residui
# ------------------------------------------------------------------------------

# 1. Modello ridotto
residui_model_reduced <- residuals(model_reduced)
shapiro.test(residui_model_reduced)

# 2. Modello con termini quadratici
residui_model_quad <- residuals(model_quad)
shapiro.test(residui_model_quad)

# 3. Modello stepwise
residui_model_stepwise <- residuals(model_stepwise)
shapiro.test(residui_model_stepwise)

# 4. Modello con interazioni e quadrati
residui_model_step_interactions <- residuals(model_step_interactions)
shapiro.test(residui_model_step_interactions)

# 5. Modello finale scelto: cubico + interazioni lineari
residui_model_cubic <- residuals(model_cubic_step)
shapiro.test(residui_model_cubic)

