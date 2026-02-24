# Codes TP 1 : regression nonparamétrique

##################
# Question 1
##################

# Ensemble de 4 fonctions tests
T = 1000
t = (1:T)/T
t
truef1 = 0.5 + (0.2*cos(4*pi*t)) + (0.1*cos(24*pi*t))
truef2 = 0.2 + 0.6*(t > 1/3 & t <= 0.75)
truef3 = 4*sin(4*pi*t) - sign(t - .3) - sign(.72 - t) + 5
truef4 = sqrt(t*(1-t))*sin((2*pi*1.05) /(t+.05)) + 0.5

# Visualisation des fonctions de regression
x11()
plot(t,truef1,type = "l", col="red", lwd=2)

x11()
plot(t,truef2,type = "l", col="red", lwd=2)

x11()
plot(t,truef3,type = "l", col="red", lwd=2)

x11()
plot(t,truef4,type = "l", col="red", lwd=2)

# Choix des points du design : n valeurs regulierement espacees sur [0,1]
n <- 100
x <- (1:n)/n  #######x_i
x
# Choix du rapport signal sur bruit
rsnr = 5

# Data 1
f1 = 0.5 + (0.2*cos(4*pi*x)) + (0.1*cos(24*pi*x))  ###### f(x_i)
sigma1 <- sd(f1)/rsnr # Niveau de bruit epsilon
Y1 <- f1 + rnorm(n,mean=0,sd=sigma1)  ####### y_i=f(x_i)+epsilon_i

# Visualisation des donnees
x11()
plot(x,Y1,type="p",pch=19)

# Visualisation des donnees avec la fonction de regression
x11()
plot(x,Y1,type="p",pch=19)
lines(t,truef1, type = "l", col="red", lty="dotdash", lwd=2)

###########utiliser ces observations pour estimer la fonction truef1

# Data 2
f2 = 0.2 + 0.6*(x > 1/3 & x <= 0.75)
sigma2 <- sd(f2)/rsnr # Niveau de bruit
Y2 <- f2 + rnorm(n,mean=0,sd=sigma2)

# Visualisation des donnees avec la fonction de regression
x11()
plot(x,Y2,type="p",pch=19)
lines(t,truef2, type = "l", col="red", lty="dotdash", lwd=2)

# Data 3
f3 = 4*sin(4*pi*x) - sign(x - .3) - sign(.72 - x) + 5
sigma3 <- sd(f3)/rsnr # Niveau de bruit
Y3 <- f3 + rnorm(n,mean=0,sd=sigma3)

# Visualisation des donnees avec la fonction de regression
x11()
plot(x,Y3,type="p",pch=19)
lines(t,truef3, type = "l", col="red", lty="dotdash", lwd=2)

# Data 4
f4 = sqrt(x*(1-x))*sin((2*pi*1.05) /(x+.05)) + 0.5
sigma4 <- sd(f4)/rsnr # Niveau de bruit
Y4 <- f4 + rnorm(n,mean=0,sd=sigma4)

# Visualisation des donnees avec la fonction de regression
x11()
plot(x,Y4,type="p",pch=19)
lines(t,truef4, type = "l", col="red", lty="dotdash", lwd=2)

####################
# Questions 2 et 3
####################

#library(KernSmooth)
#telecharger aussi la library (np)
# Faire varier h, d et la fonction f1 dans le code ci-dessus

#####################
# Estimation a noyau
#####################

h = 0.01
###### estime f par noyau en T points dans l'intervalle [0.001, 1]
?locpoly

hatf1kern = locpoly(x,Y1,degree=0,bandwidth=h,gridsize=T,range.x=c(0.001,1))


########resultats
hatf1kern$x  ######les points x où on evalue la fonction
hatf1kern$y  ######les estimations de f1 en ces points x

# Visualisation
# Estimation
x11()
plot(t,truef1, type = "l", col="red", lty="dotdash", lwd=2) ###vraie fonction
lines(hatf1kern$x, hatf1kern$y,col="blue", pch=19)
###estimation

# Erreur quadratique ponctuelle

x11()
plot(t,(truef1-hatf1kern$y)^2, type = "l", col="red",lwd=2)

########mean square error 


mean((truef1-hatf1kern$y)^2)





#######################################
# Estimation par polynomes locaux
#######################################

h = 0.01
#p = 0## c'est le noyau
#p = 1## c'est l'estimateur local linéaire
p = 2## à priori un meilleur

hatf1poly = locpoly(x,Y1,degree=p,bandwidth=h,gridsize=T,
range.x=c(0.001,1))

# Visualisation
# Estimation
x11()
plot(t,truef1, type = "l", col="red", lty="dotdash", lwd=2)
lines(hatf1kern$x, hatf1poly$y,type="l",col="blue", pch=19)

# Erreur quadratique ponctuelle
x11()
plot(hatf1poly$x,(truef1-hatf1poly$y)^2, type = "l", col="red",lwd=2)
lines(hatf1kern$x,(truef1-hatf1kern$y)^2, type = "l", col="green",lwd=2)
mean((truef1-hatf1poly$y)^2)

########on peut faire de la validation croisée avec np ou sm ou locpoly############
#library(np)

####### validation croisée avec SM par noyau########
#library(sm)
h.cv <- hcv(x,y=Y1,hstart=0.001,display="lines")
h.cv
########Choix de h par Kernsmooth avec la fonction dpill########

h.dpill=dpill(x,Y1)
h.dpill
###########
# Question 4
################
# Validation croisee pour la methode du noyau
# Choix d'un ensemble de donnees
Y = Y1  #### echantillon des Yi
truef = truef1

# Choix d'une grille de valeurs pour h
M = 20
h = seq(h.cv,0.1,length.out=M)  ### grille où on cherche h
d = 0 ##estimateur à noyau

erreur = rep(0,M)

for (k in 1:M) {
	print(k)

	for (i in 1:n) {
		xi = x[(1:n) != i] ### l'echantillon des xi prive de x_i
		Yi = Y[(1:n) != i] ### l'echantillon des yi prive de y_i
		fit = locpoly(xi,Yi,degree=d,bandwidth=h[k],gridsize=n,range.x=c(0.01,1))$y
		hatfi = fit[i] ### f1kernhat(-i)
		erreur[k] = erreur[k] + (Y[i]-hatfi)^2/n
		}
	}

x11()
plot(h,erreur)

# Minimisation de l'erreur
hopt = h[which.min(erreur)]#### h optimal
hopt


hatf = locpoly(x,Y,degree=d,bandwidth=hopt,gridsize=T,range.x=c(0.001,1))

###ou

hatf = locpoly(x,Y,degree=d,bandwidth=h.cv,gridsize=T,range.x=c(0.001,1))

# Visualisation
x11()
plot(t,truef, type = "l", col="red", lty="dotdash", lwd=2)
lines(hatf$x, hatf$y,type="l",col="blue", pch=19)


x11()
plot(hatf$x,(truef1-hatf$y)^2, 
type = "l", col="red",lwd=2)


mean((truef1-hatf$y)^2)

## 2. Exemple de données réelles

acceleration <- read.table("/home/layedev/Documents/miashs2026/MQME2026_SEMESTRE2/data mining/supervisé/Régression à noyau-20260209/Motorcycledata.txt",header=TRUE)# pour lire les données
Y<-acceleration$acc # pour sélectionner l’accélération
x<-acceleration$temp # pour sélectionner le temps après le choc
# Visualisation des données
x11()
plot(x,Y,type="p",pch=19)
### QUESTION 5
h = 0.01
###### estime f par noyau en T points dans l'intervalle [0.001, 1]

hatf1kern = locpoly(x,Y1,degree=0,bandwidth=h,gridsize=T,range.x=c(0.001,1))

########resultats
#hatf1kern$x  ######les points x où on evalue la fonction
#hatf1kern$y  ######les estimations de f1 en ces points x

# Visualisation
# Estimation
x11()
plot(x,Y, type = "l", col="red", lty="dotdash", lwd=2) ###vraie fonction
lines(hatf1kern$x, hatf1kern$y,col="blue", pch=19)
