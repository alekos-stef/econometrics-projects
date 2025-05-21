# CASE PROJECT COURSERA - ECONOMETRICS: METHODS AND APPLICATIONS

# model with constant (m = 2 restrictions) vs full model - critical value=5.991
LR_full <- -2*(-152.763 - (-134.178)) ; LR_full


# model with li1 (m =1 restrictions) vs full model - critical value = 3.841
LR_1 <- -2*(-139.747 - (-134.178)) ; LR_1


# model with li2 (m=1 restrictions) vs full model
LR_2 <- -2*(-149.521 - (-134.178)) ; LR_2


# Answer for (b) :

# McFaden Rsq 

Rsq_li11 <- 1 - (-134.178)/(-152.763) ; Rsq_li11
Rsq_li12 <- 1 - (-134.126)/(-152.763) ; Rsq_li12
Rsq_li21 <- 1 - (-130.346)/(-152.763) ; Rsq_li21
Rsq_li22 <- 1 - (-130.461)/(-152.763) ; Rsq_li22


# Answer for (d) : 
rm(list = ls())  # Clears the variables from memory



 library(readxl)
Case_GDP_round2 <- read_excel("Case_GDP-round2.xls", col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))  
View(Case_GDP_round2)

install.packages('Hmisc')   # Bibliothiki gia ta LAGS (Auth einai h swsth)
library(Hmisc)

install.packages('dplyr')
library(dplyr)



# ADF test for Yt
 

df = Case_GDP_round2
attach(df)


df$Yt <- df$LOGGDP



Yt1 <- Lag(df$Yt, shift = 1);length(Yt1) <- length(df$Yt)
df$Yt1 <- Yt1

DYt <- diff(df$Yt, differences = 1) ; length(DYt) <- length(df$Yt)
df$DYt <- DYt

DYt_Lag1 <- Lag(DYt, shift = 1);length(DYt_Lag1) <- length(df$Yt)
df$DYt_Lag1 <- DYt_Lag1


df_new<- df[5:244, ]
str(df_new)


model_d <- lm (DYt ~ T + Yt1 + DYt_Lag1, data = df_new);summary(model_d)


# solution for (e) :
# H metavlhth GrowthRate kai mia allh, einai character kai prepei na tin allaksw se numeric

 
GrowthRate_Lag1 <- Lag(df$GrowthRate, shift = 1);length(GrowthRate_Lag1) <- length(df$GrowthRate)
df$GrowthRate_Lag1 <- GrowthRate_Lag1

str(GrowthRate_Lag1)


# Tha ftiaksw mia synarthsh me metavlites k1 kai k2.

 
model <- function(k1,k2) {


li1_Lag_k1 <- Lag(df$li1, shift = k1);length(li1_Lag_k1) <- length(df$li1)
df$li1_Lag_k1 <- li1_Lag_k1

li2_Lag_k2 <- Lag(df$li2, shift = k2);length(li2_Lag_k2) <- length(df$li2)
df$li2_Lag_k2 <- li2_Lag_k2

df_new<- df[5:244, ]


model_e11 <- lm (GrowthRate ~ GrowthRate_Lag1 + li1_Lag_k1 + li2_Lag_k2, data=df_new)
summary (model_e11)

}



model(k1=2,k2=1)


# Answer for (f) 
k1 <- 1; k2<-1

li1_Lag_k1 <- Lag(df$li1, shift = k1);length(li1_Lag_k1) <- length(df$li1)
df$li1_Lag_k1 <- li1_Lag_k1

li2_Lag_k2 <- Lag(df$li2, shift = k2);length(li2_Lag_k2) <- length(df$li2)
df$li2_Lag_k2 <- li2_Lag_k2

df_new<- df[5:244, ]



modelf <- lm (GrowthRate ~ GrowthRate_Lag1 + li1_Lag_k1 + li2_Lag_k2, data=df_new)

e <- modelf$residuals
df_new$e <- e

e_Lag1 <- Lag(df_new$e, shift = 1);length(e_Lag1) <- length(df_new$e)
df_new$e_Lag1 <- e_Lag1

model_d1 <- lm (e ~ GrowthRate_Lag1 + li1_Lag_k1 + li2_Lag_k2 + e_Lag1, data=df_new)
summary(model_d1)

length(e)
