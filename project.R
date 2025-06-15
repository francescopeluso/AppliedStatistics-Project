# ------------------------------------------------------------------------------
# Progetto finale del corso di Statistica Applicata - A.A. 2024/25
# Gruppo #5 - Corrado Giachetta, Francesco Peluso, Gerardo Selce, Anuar Zouhri
#
# Università degli Studi di Salerno - DIEM
# Dipartimento di Ingegneria dell'Informazione, Elettrica e Matematica Applicata
# ------------------------------------------------------------------------------

# Impostazione path cartella progetto (N.B. cambia a seconda di chi usa il file)
setwd('~/GitHub Repos/AppliedStatistics-Project')     # path Francesco
#setwd('C:/Users/anuar/AppliedStatistics-Project')     # path Anuar
#setwd('/Users/corry/AppliedStatistics-Project')     # path Corrado
#setwd('')     # path Gerardo

# ------------------------------------------------------------------------------

#package da scaricare 
install.packages("GGally")

# Lettura del dataset da file CSV
data = read.csv('./dataset.csv');


# ------------------------------------------------------------------------------
# PUNTO 1:
# Analisi preliminare dei dati.
# Scatterplot delle correlazioni tra le coppie di variabili
# ------------------------------------------------------------------------------

plot(data);

library(ggplot2)
library("GGally")

ggpairs(data, upper = list(continuous = "blank"))   # nasconde i grafici sotto la diagonale

# Basic Scatterplot Matrix
pairs(~,
      data=mtcars,
      main="Simple Scatterplot Matrix"
)

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

#Istogrammi
hist(data$y_VideoQuality,main="Distribuzione di y_VideoQuality", xlab="y_VideoQuality")


# ------------------------------------------------------------------------------
# PUNTO 2:
# Valutazione della relazione tra variabile dipendente e le singole
# variabili indipendenti
# ------------------------------------------------------------------------------

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
# PUNTO 3:
# Definizione del modello statistico dei dati tramite regressione lineare
# multipla
# ------------------------------------------------------------------------------

# Modello costruto con tutte le variabili indipendenti
model_full <- lm(y_VideoQuality ~ ., data=data)
summary(model_full)

# Squared R e Adj. Squared R sono rispettivamente 0.7871 e 0.7709,
# il che indica che queto modello ha un buon potere esplicativo.

# Inoltre, con un p-value globale minore di 2.2*10^16, ciò significa
# che il modello è complessivamente significativo.

# Tra le variabili che risultano significative nel modello complessivo,
# abbiamo: ISO, FRatio, TIME, CROP

# Con questi dati, ora posso creare un modello ridotto, composto dalle
# sole variabili significative

model_reduced <- lm(y_VideoQuality ~ x1_ISO + x2_FRatio + x3_TIME + x5_CROP, data=data)
summary(model_reduced)

# Dal summary deduciamo che tutti i regressori restano significativi, avendo
# tutti loro un p-value minore di 0.01 (**), e che gli indici Squared R e
# Adj. Squared R sono rimasti pressochè simili

# Dalle analisi effettuate nel primo punto, però, notiamo che con alcuni
# regressori è presente una correlazione non lineare, ma quadratica,
# dunque possiamo tentare di aggiungere dei termini quadratici al nostro
# modello di regressione lineare (stiamo parlando di )

model_quad <- lm(y_VideoQuality ~ 
                   x1_ISO + I(x1_ISO^2) +
                   x2_FRatio + I(x2_FRatio^2) +
                   x3_TIME + x5_CROP,
                 data = data)
summary(model_quad)

# Con l'introduzione di questi termini quadratici, notiamo come tutti i
# coefficienti sono altamente significativi, poichè hanno p-value < 0.001 (***),
# e che anche gli indici di Squared R e Adj. Squared R sono aumentati, mentre
# il residual standard error sia diminuito da 12.8 a 9.68, con il p-value
# complessivo del modello minore di 2.2*10^16.

# Proviamo adesso ad introdurre come nuovi regressori, come per esempio
# le interazioni tra tutte le possibili coppie di variabil indipendenti,
# per verificare se è possibile migliorare ulteriormente il modello

model_final <- lm(y_VideoQuality ~ (x1_ISO + x2_FRatio + x3_TIME + x4_MP + 
                                      x5_CROP + x6_FOCAL + x7_PixDensity)^2, data = data)
summary(model_final)

# Procediamo a confrontare i tre modelli appena creati tramite AIC e BIB:
# il modello con valore più basso di AIC e BIC sarà quello preferibile

AIC(model_full, model_reduced, model_quad, model_final)
BIC(model_full, model_reduced, model_quad, model_final)

# In effetti, entrambi i confronti ci confermano che l'ultimo modello costruito,
# ovvero quello con i termini quadratici, sia il migliore.

# Adesso però, vogliamo verificare se è possibile ottenere un modello ancora
# più efficiente, utilizzando un approccio "automatizzato".

# Per fare ciò, applichiamo una procedura di selezione stepwise delle variabili
# indipendenti da aggiungere o rimuovere dal nostro modello, basandoci
# sul valore AIC più basso.

model_stepwise <- step(model_full, direction = "both", trace = 1)
summary(model_stepwise)  # risultato AIC = 514*

model_stepwise_quad <- (model_quad, direction = "both", trace = 1)
summary(model_stepwise_quad)  # risultato AIC = 514*

# Confronto AIC/BIC tra tutti i modelli
AIC(model_full, model_reduced, model_quad, model_stepwise) # *però qui risulta essere 799, wtf
BIC(model_full, model_reduced, model_quad, model_stepwise)

mod_step_full <- lm(y_VideoQuality ~ (.)^2 + I(x5_CROP^2) + I(x7_PixDensity^2), data = data)
mod_stepwise <- step(mod_step_full)
summary(mod_stepwise)

AIC(model_full, model_reduced, model_quad, model_stepwise, mod_stepwise)
BIC(model_full, model_reduced, model_quad, model_stepwise, mod_stepwise)

