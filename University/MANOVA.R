#One-Way MANOVA
install.packages("car")
library(car)
data(Baumann)
attach(Baumann)
??Bauman

#Testing the Independance Assumption
install.packages("psych")
library("psych")
ICC(Baumann[,4:6])

group=factor(group) #encode group as a categorial variable
Y = cbind(post.test.1, post.test.2, post.test.3)
Baumann.manova = manova(Y~group)

summary(Baumann.manova, test="Wilks")
summary(Baumann.manova, test="Pillai")
summary(Baumann.manova, test="Hotelling-Lawley")
summary(Baumann.manova, test="Roy")



#Factorial MANOVA
data(Soils)
attach(Soils)
??Soils
options(scipen=999)

install.packages("MASS")
library(MASS)

Contour = factor(Contour)
Contour
Depth = factor(Depth)
Depth
soils.mod = lm(cbind(pH,Dens,Conduc)~Contour+Depth+Contour*Depth -1, data=Soils)
summary(soils.mod)
wilks = manova(soils.mod, multivariate=TRUE, type = c("III"), test=("Wilks"))

manova(soils.mod, multivariate=TRUE, type=c("III"), test=("Pillai"))
manova(soils.mod, multivariate=TRUE, type=c("III"), test=("Hotelling-Lawley"))
manova(soils.mod, multivariate=TRUE, type=c("III"), test=("Roy"))

manova(soils.mod, multivariate=TRUE, type=c("II"), test=("Wilks"))
manova(soils.mod, multivariate=TRUE, type=c("II"), test=("Pillai"))
manova(soils.mod, multivariate=TRUE, type=c("II"), test=("Hotelling-Lawley"))
manova(soils.mod, multivariate=TRUE, type=c("II"), test=("Roy"))