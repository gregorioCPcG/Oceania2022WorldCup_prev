pais <- c("NZ","Solomon Islands", "New Caledonia","Tahiti","Fiji","Vanuatu","Papua New Guinea", "Tonga", "Cook Islands" )
FIFA_NOV_2021 <- c(110, 141,153,159,161,163,164,199,205)
dep <- c(9,8,7,6,5,4,3,2,1) # posicao com base no ranking da FIFA de Novembro de 2021
pop <- c(5132160,652857,271407,189517,926276,307815, 8935000,100651,17459)
rugbyranking <- c(88.75,23.81,0,0,76.62,15.45,33.68,67.76,45.11)
# 1= 9, 2 = 8, 3 =7, 4 = 6, 5=5, 6 =4, 7 =3, 8=2, 9=1
#10 - 1/4/2/7/3/4/na/9/na
hist10 <- c(9,6,8,3,7,6,0,1,0)
#14 - 1/5/2/3/6/4/7/9/na
hist14 <- c(9,5,8,7,4,6,3,1,0)
#18 -  1/2/4/3/6//7/5/na/na
hist18 <- c(9,8,6,7,4,3,5,0,0)
# 1= 9, 2 = 8, 3 =7, 4 = 6, 5=5, 6 =4, 7 =3, 8=2, 9=1
pib_billion <- c(204,1.3,8.85,14.33,5.06,0.863,20.54,0.428,0.384)


hist <- ((hist10*1)+(hist10*5)+(hist18*10))
#9 pontos para primeiro, até 1 para nono colocado, menor que isso zero (maior peso para edição mais recente)

league <- c("semi", "semi", "amaut","amaut", "semi", "amaut", "semi", "amaut", "amaut" )
euro <- c(15,0,3,3,1,0,0,0,0)

data <- data.frame(euro, FIFA_NOV_2021, hist, league,pais,rugbyranking, pop, pib_billion, dep)

library(huxtable)



mod1 <- lm(dep ~ hist, data = data)
summary(mod1)
mod2 <- lm(dep ~hist + rugbyranking, data = data)
summary(mod2)
mod3 <- lm(dep ~ hist + euro + league)
summary(mod3)
mod4 <- lm(dep ~hist + pib_billion+pop, data = data)
summary(mod4)

cor(dep,hist18)
cor(dep,hist)
cor(hist18,hist)
cor(dep,rugbyranking)
t.test(dep ~ league)
cor(dep, pib_billion)
cor(dep,pop)
cor(dep,euro)

## estatística bayesiana

library(dplyr)
library(statsr)
library(BAS)
library(tidyr)
library(tidyverse)


log_pop <- log(pop)
log_pib <- log(pib_billion)

base <- data.frame(euro, log_pop, log_pib, hist, dep)

modelbas<-bas.lm(dep ~.,data=base, prior="ZS-null", modelprior = uniform(), method = "MCMC")
plot(modelbas, which=1, add.smooth=F)
#2. cumulative probability
plot(modelbas, which=2)


#3. model dimension plot
plot(modelbas, which=2)
#4.PIP
plot(modelbas, which = 4, ask=FALSE, caption="", sub.caption="")

#model rank
image(modelbas, rotate = F)

summary(modelbas)

data <- data.frame(euro, FIFA_NOV_2021, hist, league,pais,rugbyranking, pop, pib_billion, dep, log_pib, log_pop)

mod <- lm(dep ~ log_pop+ hist, data = data )
summary(mod)

#diagnósticos
plot(mod) # a segunda

library(olsrr)
ols_vif_tol(mod)


# predict com base na pop e no ranking ####

table(data$pais, data$log_pop)
table(data$pais, data$hist)

base <- data.frame(log_pop, hist, dep)

modelbas<-bas.lm(dep ~.,data=base, prior="ZS-null", modelprior = uniform(), method = "MCMC")
plot(modelbas, which=1, add.smooth=F)
#2. cumulative probability
plot(modelbas, which=2)


#3. model dimension plot
plot(modelbas, which=2)
#4.PIP
plot(modelbas, which = 4, ask=FALSE, caption="", sub.caption="")

#model rank
image(modelbas, rotate = F)

summary(modelbas)

data2 <- data.frame(pais, log_pop, hist)
head(data2)
tail(data2)

NZ <- data.frame(log_pop = 15.45104, hist = 144)
NZ = predict(modelbas, newdata = NZ, estimator = "BPM",  se.fit=T)
NZ$fit  # fitted values
NZ <- 8.7
# bozo em rio do sul 82%

SOL <- data.frame(log_pop = 13.38911, hist = 116)
SOL = predict(modelbas, newdata = SOL, estimator = "BPM",  se.fit=T)
SOL$fit  # fitted values
SOL <- 7.330943
head(data2)

head(data2)
CAL <- data.frame(log_pop = 12.5111, hist = 108)
CAL = predict(modelbas, newdata = CAL, estimator = "BPM",  se.fit=T)
CAL$fit  # fitted values
CAL <- 6.89

head(data2)
TAI <- data.frame(log_pop = 12.15223, hist = 88)
TAI = predict(modelbas, newdata = TAI, estimator = "BPM",  se.fit=T)
TAI$fit  # fitted values
TAI <- 5.80

head(data2)
FIJ <- data.frame(log_pop = 13.73893, hist = 82)
FIJ = predict(modelbas, newdata = FIJ, estimator = "BPM",  se.fit=T)
FIJ$fit  # fitted values
FIJ <- 5.47

head(data2)
VAN <- data.frame(log_pop = 12.63725, hist = 66)
VAN = predict(modelbas, newdata = VAN, estimator = "BPM",  se.fit=T)
VAN$fit  # fitted values
VAN <- 4.59

# PNG - não jogou 2010 (tudo quinto lugar)
tail(data2)
(5*1)+(5*5)+(5*10) #usar essa estimativa como PNG hist
PNG <- data.frame(log_pop = 16.01, hist = 80)
PNG = predict(modelbas, newdata = PNG, estimator = "BPM",  se.fit=T)
PNG$fit  # fitted values
PNG <- 5.36

# para tonga e cook islands
# sempre a frente de cook islands , jogou em 2019, tem uma maior org deve ficar em oitavo e nono

TON <- 2
COK <- 1

head(data2)

com <- c(NZ,SOL,CAL,TAI,FIJ,VAN,PNG,TON,COK)


com2 <- data.frame(pais, com, log_pop, hist)
prev <- c("vencedor", "finalista", "semi", "semi", "grupos", "grupos", "grupos", "grupos","preliminar")
prev <- data.frame(pais, prev)
rm(data2,mod,mod1,mod2,mod3,mod4,modelbas,CAL,COK,dep,euro,FIFA_NOV_2021,FIJ,hist,hist10,
   hist14,hist18,log_pib,log_pop,NZ,pais,pib_billion,PNG,pop,rugbyranking, SOL, TON, VAN,
   TAI, league, com2, data, base)


library(knitr)
library(kableExtra)
# rode script ra e depois parta daqui
prev <- prev %>% 
  dplyr::select(pais, prev) 
prev %>%
  kbl(caption = "Previsão em 01/12/2021 Eliminatórias Oceania Copa do Mundo 2022") %>%
  kable_classic(full_width = F, html_font = "Garamond")

# previsão por pais #########

pais <- c('NZ','SOL','CAL','TAI','FIJ','VAN','PNG','TON','COK')
prob <- (com)*10
rm(prev)
prob
a1 <- prob + 10
prev <- c(5000, 50,40,40,20,20,20,0,0)#enfaase maior por NZ sempre vencer, tonga zero por nunca passar fase de grupos
a1 <- a1 + prev
a1
a2 <- 6000- a1
a2
prev <- data.frame(pais, a1, a2)
#NZ
head(prev)
a1 <- 5097
a2 <- 903
n <- 1
s <- 1
w <- a1+s
z <- a2+(n-s)
posteriori1 <- rbeta(10000, w, z)
NZ <-mean(posteriori1)
NZ
#SOL
head(prev)
a1 <- 133.3094
a2 <- 5866.6906 
n <- 1
s <- 1
w <- a1+s
z <- a2+(n-s)
posteriori1 <- rbeta(10000, w, z)
SOL <-mean(posteriori1)
SOL
#TON
tail(prev)
a1 <- 30
a2 <- 5970 
n <- 1
s <- 1
w <- a1+s
z <- a2+(n-s)
posteriori1 <- rbeta(10000, w, z)
TON <-mean(posteriori1)
TON
#COOK
tail(prev)
a1 <- 20
a2 <- 5980 
n <- 1
s <- 1
w <- a1+s
z <- a2+(n-s)
posteriori1 <- rbeta(10000, w, z)
COK <-mean(posteriori1)
COK
#PNG
tail(prev)
a1 <- 83.6
a2 <- 5916.4 
n <- 1
s <- 1
w <- a1+s
z <- a2+(n-s)
posteriori1 <- rbeta(10000, w, z)
PNG <-mean(posteriori1)
PNG

#VAN
tail(prev)
a1 <- 75.9
a2 <- 5924.1 
n <- 1
s <- 1
w <- a1+s
z <- a2+(n-s)
posteriori1 <- rbeta(10000, w, z)
VAN <-mean(posteriori1)
VAN

#FIJ
tail(prev)
a1 <- 84.7
a2 <- 5915.3 
n <- 1
s <- 1
w <- a1+s
z <- a2+(n-s)
posteriori1 <- rbeta(10000, w, z)
FIJ <-mean(posteriori1)
FIJ

#TAI
tail(prev)
a1 <- 108
a2 <- 5892 
n <- 1
s <- 1
w <- a1+s
z <- a2+(n-s)
posteriori1 <- rbeta(10000, w, z)
TAI <-mean(posteriori1)


#CAL
head(prev)
a1 <- 118.9
a2 <- 5881.1
n <- 1
s <- 1
w <- a1+s
z <- a2+(n-s)
posteriori1 <- rbeta(10000, w, z)
CAL <-mean(posteriori1)


#pais <- c('NZ','SOL','CAL','TAI','FIJ','VAN','PNG','TON','COK')
chance <-c(NZ,SOL,CAL,TAI,FIJ,VAN,PNG,TON,COK)

options("scipen"=100, "digits"=2)
chance <- chance*100
prev <- data.frame(pais, chance)
prev <- prev %>% 
  dplyr::select(pais, chance) 
prev %>%
  kbl(caption = "Previsão em 01/12/2021 classificação aos Playoffs 2022") %>%
  kable_classic(full_width = F, html_font = "Garamond")

