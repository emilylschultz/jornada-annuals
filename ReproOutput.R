### Script to calculate size-reproductive output relationship for 5 species (BOBA, BOIN, PAHI, ERAB, TILA)
### Created by: Emily Schultz
### Created on: 17 May 2022

library(tidyverse)

# Data files
data.2012 <- read.csv("Dryad_Demo_Data_2012.csv")
data.2013 <- read.csv("Spp_data_2013.csv")

data.repr <- rbind(data.2012,data.2013[,-which(names(data.2013)=="plot")])

data.repr <- data.repr[,c(1:8,which(str_detect(names(data.repr),"._6")))]
data.repr$FLWR_6[which(is.na(data.repr$FLWR_6) & !is.na(data.repr$FRT_6))] = 0
data.repr$FRT_6[which(is.na(data.repr$FRT_6) & !is.na(data.repr$FLWR_6))] = 0
data.repr$REPR_6 <- data.repr$FLWR_6 + data.repr$FRT_6

data.boba <- subset(data.repr,spp=="BOBA")
data.boin <- subset(data.repr,spp=="BOIN")
data.pahi <- subset(data.repr,spp=="PAHI") # Does this species have a different code?
data.erab <- subset(data.repr,spp=="ERAB")
data.tila <- subset(data.repr,spp=="TILA")

data.tila$AREA_6 <- (data.tila$A1_6*data.tila$A2_6*pi)*(1-(data.tila$PM_6/100))

# Models of size vs reproductive output
boba.repr.model <- glm(REPR_6 ~ HT_6, family="poisson", data=data.boba)

boin.repr.model <- glm(REPR_6 ~ HT_6, family="poisson", data=data.boin)

pahi.repr.model <- glm(REPR_6 ~ HT_6, family="poisson", data=data.pahi)

erab.repr.model <- glm(REPR_6 ~ HT_6, family="poisson", data=data.erab)

tila.repr.model <- glm(REPR_6 ~ AREA_6, family="poisson", data=data.tila)

predict.boba <- data.frame(Height=seq(min(data.boba$HT_6,na.rm=T),max(data.boba$HT_6,na.rm=T),length=50),
                           Repr=exp(predict(boba.repr.model,newdata=data.frame(HT_6=seq(min(data.boba$HT_6,na.rm=T),max(data.boba$HT_6,na.rm=T),length=50)))))
predict.boin <- data.frame(Height=seq(min(data.boin$HT_6,na.rm=T),max(data.boin$HT_6,na.rm=T),length=50),
                           Repr=exp(predict(boin.repr.model,newdata=data.frame(HT_6=seq(min(data.boin$HT_6,na.rm=T),max(data.boin$HT_6,na.rm=T),length=50)))))
predict.pahi <- data.frame(Height=seq(min(data.pahi$HT_6,na.rm=T),max(data.pahi$HT_6,na.rm=T),length=50),
                           Repr=exp(predict(pahi.repr.model,newdata=data.frame(HT_6=seq(min(data.pahi$HT_6,na.rm=T),max(data.pahi$HT_6,na.rm=T),length=50)))))
predict.erab <- data.frame(Height=seq(min(data.erab$HT_6,na.rm=T),max(data.erab$HT_6,na.rm=T),length=50),
                           Repr=exp(predict(erab.repr.model,newdata=data.frame(HT_6=seq(min(data.erab$HT_6,na.rm=T),max(data.erab$HT_6,na.rm=T),length=50)))))
predict.tila <- data.frame(Height=seq(min(data.tila$AREA_6,na.rm=T),max(data.tila$AREA_6,na.rm=T),length=50),
                           Repr=exp(predict(tila.repr.model,newdata=data.frame(AREA_6=seq(min(data.tila$AREA_6,na.rm=T),max(data.tila$AREA_6,na.rm=T),length=50)))))


# Plots of size vs reproductive output
ggplot(data=data.boba,aes(x=HT_6,y=REPR_6)) +
  geom_point() +
  geom_line(data=predict.boba,aes(x=Height,y=Repr)) +
  labs(x="Height",y="Reproductive output") +
  theme_classic()

ggplot(data=data.boin,aes(x=HT_6,y=REPR_6)) +
  geom_point() +
  geom_line(data=predict.boin,aes(x=Height,y=Repr)) +
  labs(x="Height",y="Reproductive output") +
  theme_classic()

ggplot(data=data.pahi,aes(x=HT_6,y=REPR_6)) +
  geom_point() +
  geom_line(data=predict.pahi,aes(x=Height,y=Repr)) +
  labs(x="Height",y="Reproductive output") +
  theme_classic()

ggplot(data=data.erab,aes(x=HT_6,y=REPR_6)) +
  geom_point() +
  geom_line(data=predict.erab,aes(x=Height,y=Repr)) +
  labs(x="Height",y="Reproductive output") +
  theme_classic()

ggplot(data=data.tila,aes(x=AREA_6,y=REPR_6)) +
  geom_point() +
  geom_line(data=predict.tila,aes(x=Height,y=Repr)) +
  labs(x="Height",y="Reproductive output") +
  theme_classic()


