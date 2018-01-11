Datos=read.csv(file.choose(), header=TRUE)
Datos

attach(Datos)

Datos = Datos[order(IQS),]

attach(Datos)

###### TEMPORALIDAD ########

plot(IQS,RCT, type="overplotted", col="blue", main="RCT vs Tiempo")
plot(IQS,SCT, type="overplotted", col="red", main="SCT vs Tiempo")


###### MODELO ORIGINAL #######
A=TSI.CD*Peso/1000

plot(RCT,SCT, type="p", col="red",pch=18)
matplot(TSI.CD*Peso/1000, SCT, type="p", col="red",pch=18, sub="Ajuste de SCT")
legend(x=.405, y=2.05, legend=c("Recta sin intercepto", "Recta con intercepto"), lty=c(1,5), bty="n", lwd=3)

abline(lm(SCT ~ A-1), lty=1, lwd=3)
abline(lm(SCT ~ A),lty=5, lwd=3)

matplot(TSI.CD*PESO/1000, RCT, type="p", col="red",pch=18, main="Ajuste de RCT")
legend(x=.40, y=37, legend=c("Recta sin intercepto", "Recta con intercepto"), col=c(1,3), pch=16, bty="n")

abline(lm(RCT ~ A-1))
abline(lm(RCT ~ A),col=3)

plot(lm(SCT ~ A))
plot(lm(SCT ~ A-1))
summary(lm(RCT ~ A))
modelo <- summary(lm(SCT ~ A))
modelo1 <- summary(lm(SCT ~ A-1))

###### USANDO LA VARIABLE TSI CD/MD*PESO ###### 

B=TSI.CD.MD*PESO/1000
summary(lm(SCT ~ B))
summary(lm(SCT ~ B-1))

matplot(TSI.CD.MD*Peso/1000, SCT, type="p", col="red",pch=18, main="Ajuste de SCT")
legend(x=.21, y=2, legend=c("Recta sin intercepto", "Recta con intercepto"), col=c(1,3), pch=16, bty="n")
abline(lm(SCT ~ B-1))
abline(lm(SCT ~ B),col=3)

###### USANDO LA VARIABLE TSI CD/MD ###### 

summary(lm(SCT ~ TSI.CD.MD))
summary(lm(SCT ~ TSI.CD.MD-1))

matplot(TSI.CD.MD, SCT, type="p", col="red",pch=18, main="Ajuste de SCT")
legend(x=.21, y=2, legend=c("Recta sin intercepto", "Recta con intercepto"), col=c(1,3), pch=16, bty="n")
abline(lm(SCT ~ TSI.CD.MD-1))
abline(lm(SCT ~ TSI.CD.MD),col=3)


###### USANDO LA VARIABLE TSI.CD ######

summary(lm(SCT ~ TSI.CD))
summary(lm(SCT ~ TSI.CD-1))

matplot(TSI.CD, SCT, type="p", col="red",pch=18, main="Ajuste de SCT")
legend(x=.405, y=2.05, legend=c("Recta sin intercepto", "Recta con intercepto"), col=c(1,3), pch=16, bty="n")

abline(lm(SCT ~ TSI.CD-1))
abline(lm(SCT ~ TSI.CD),col=3)

###### MODELO MULTIVARIADO TSI.CD, PESO #####

summary(lm(SCT ~ TSI.CD+PESO))
summary(lm(SCT ~ TSI.CD+PESO-1))

library(rgl)

plot3d(SCT, TSI.CD,Peso, col=2, size=8)
panel.abline(lm(SCT ~ TSI.CD+PESO-1))
abline(lm(SCT ~ TSI.CD+PESO),col=3)
