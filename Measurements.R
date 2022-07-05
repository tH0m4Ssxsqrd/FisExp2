# File:   Measurements.R

##########################################
#This notebook is supposed to help calculate and plot the linear movement of both accelerated particles and ones with constant speed, and also the associated error that comes from the experiment and pure randomness.
#Este programa pretende ajudar a calcular e fazer gráficos que descrevam o movimento retilíneo uniforme e o acelerado de um carro em um trilho de ar, para além do erro associado ao processo experimental que vem de fatores aleatórios.

#This notebook was written to be used as such. Do read and understand what each part does, and load the required packages,
#datasets and functions in order to calculate whatever you want to.

#Este programa foi escrito para ser rodado como um notebook. Leia o programa e tente entender o que cada parte faz, lembrando
#de carregar os pacotes, datasets e funções necessárias para calcular o que quiser.
##########################################

# INSTALL AND LOAD PACKAGES ##########################################

# INSTALLS PACMAN IF IT ISN'T INSTALLED ALREADY
if (!require("pacman")) install.packages("pacman")

# USES PACMAN TO LOAD ADD-ONS
pacman::p_load(pacman, rio, pracma)

#DECLARES FUNCTIONS
std <- function(a) sd(a) / sqrt(length(a)) #STANDARD ERROR
sde <- function(a) sd(a) #STANDARD DEVIATION
vmed <- function(a, b) a/b
# ACTUALLY IMPORTS DATA
mru <- read.csv("./mru.csv", header=T, stringsAsFactors = F)
mruv <- read.csv("./mruv.csv", header=T, stringsAsFactors = F)

#MRU
##PLOTS MOVEMENT AND TIME
mrutime <- c(mean(mru$X10cm),mean(mru$X20cm),mean(mru$X30cm),mean(mru$X40cm),mean(mru$X50cm))
intervallen1 <- c(10,20,30,40,50)
plot(intervallen1/100, mrutime/1000, main = "Tempo para percorrer dada distância (MRU)", xlab = "Distância em cm", ylab = "Tempo em s")
velmru <- vmed(intervallen1/100, mrutime/1000)
#MRUV
##PLOTS TIME IN FUNCTION OF DISTANCE
mruvtime <-c(mean(mruv$X20cm),mean(mruv$X30cm),mean(mruv$X40cm),mean(mruv$X50cm),mean(mruv$X60cm))
intervallen2 <- c(20,30,40,50,60)
plot(intervallen2/100,mruvtime/1000,main = "Tempo para percorrer dada distância (MRUV)", xlab = "Distância em cm", ylab = "Tempo em s", type = "l")
velmruv <- (intervallen2/100)/(mruvtime/1000)
##THROUGH QUADATIC REGRESSION APROXIMATES THE FUNCTION THAT MORE CLOSELY REPRESENTS THE DATA DISTRIBUTION
pol <- polyfit(intervallen2/100,mruvtime/1000,2)
plot(mruvtime/1000, intervallen2/100, main = "Distância em função do tempo (MRUV)", xlab = "Tempo em s", ylab = "Distância em m", col = "blue")
func <- function(a) pol[1]*((a)^2) + pol[2]*(a) + pol[3] 
x <- seq(0,0.7,0.01)
y <- func(x)
lines(y, x, type = "l", col= "red")
grid() ##OPTIONAL

# ARRANGES THE DATA IN PLOTS

##MRU
boxplot(mru$X10cm,
        main = "Variação dos intervalos para 10cm de separação",
        xlab = "Em ms",
        horizontal = TRUE
        )

boxplot(mru$X20cm,
        main = "Variação dos intervalos para 20cm de separação",
        xlab = "Em ms",
        horizontal = TRUE
)

boxplot(mru$X30cm,
        main = "Variação dos intervalos para 30cm de separação",
        xlab = "Em ms",
        horizontal = TRUE
)
boxplot(mru$X40cm,
        main = "Variação dos intervalos para 40cm de separação",
        xlab = "Em ms",
        horizontal = TRUE
)

boxplot(mru$X50cm,
        main = "Variação dos intervalos para 50cm de separação",
        xlab = "Em ms",
        horizontal = TRUE
)

##MRUV ##########################################

boxplot(mruv$X20cm,
        main = "Variação dos intervalos para 20cm de separação",
        xlab = "Em ms",
        horizontal = TRUE
)

boxplot(mruv$X30cm,
        main = "Variação dos intervalos para 30cm de separação",
        xlab = "Em ms",
        horizontal = TRUE
)

boxplot(mruv$X40cm,
        main = "Variação dos intervalos para 40cm de separação",
        xlab = "Em ms",
        horizontal = TRUE
)

boxplot(mruv$X50cm,
        main = "Variação dos intervalos para 50cm de separação",
        xlab = "Em ms",
        horizontal = TRUE
)

boxplot(mruv$X60cm,
        main = "Variação dos intervalos para 60cm de separação",
        xlab = "Em ms",
        horizontal = TRUE
)

##########################################
# CLEAR ENVIRONMNENT
rm(list = ls())

# CLEAR PACKAGES
p_unload(all)  # Remove all add-ons

# CLEAR CONSOLE
cat("\014")  # ctrl+L

# CLEAR PLOT (IF THERE IS A PLOT)
dev.off()

