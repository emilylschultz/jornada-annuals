## Models to test for storage effect

source("./DataPrep20220411.R")

find.min <- cpath.cover.dat
find.min$C3[which(find.min$C3==0)]<-NA
find.min$C4[which(find.min$C4==0)]<-NA

min.cover <- min(find.min[,3:4],na.rm=T)

cpath.cover.dat$C3 <- cpath.cover.dat$C3 + min.cover
cpath.cover.dat$C4 <- cpath.cover.dat$C4 + min.cover
cpath.cover.dat$Total <- cpath.cover.dat$C3 + cpath.cover.dat$C4

cpath.cover.t <- cpath.cover.dat[which(cpath.cover.dat$year<max(cpath.cover.dat$year)),]
names(cpath.cover.t)<-c("year_t","site","C3_t","C4_t","w.precip_t","s.precip_t","Total_t")
cpath.cover.t1 <- cpath.cover.dat[which(cpath.cover.dat$year>min(cpath.cover.dat$year)),]
names(cpath.cover.t1)<-c("year_t1","site","C3_t1","C4_t1","w.precip_t1","s.precip_t1","Total_t1")
cpath.cover.t1$year_t <- cpath.cover.t1$year_t1-1

cpath.cover<-inner_join(cpath.cover.t,cpath.cover.t1)
cpath.cover$lambda_C3 <- cpath.cover$C3_t1/cpath.cover$C3_t
cpath.cover$lambda_C4 <- cpath.cover$C4_t1/cpath.cover$C4_t
cpath.cover$r_C3 <- log(cpath.cover$lambda_C3)
cpath.cover$r_C4 <- log(cpath.cover$lambda_C4)

# Using competition and climate in year t1 as predictors

C3_model <- lm(r_C3 ~ log(Total_t1)*s.precip_t1*w.precip_t1,data=cpath.cover)
C4_model <- lm(r_C4 ~ log(Total_t1)*s.precip_t1*w.precip_t1,data=cpath.cover)

C3.predict.Wet <- predict(C3_model,
                           data.frame(Total_t1=seq(min(cpath.cover$Total_t1),max(cpath.cover$Total_t1),length=50),
                                      s.precip_t1=median(cpath.cover$s.precip_t1,na.rm=T),
                                      w.precip_t1=quantile(cpath.cover$w.precip_t1,0.75,na.rm=T)))
C3.predict.Dry <- predict(C3_model,
                           data.frame(Total_t1=seq(min(cpath.cover$Total_t1),max(cpath.cover$Total_t1),length=50),
                                      s.precip_t1=median(cpath.cover$s.precip_t1,na.rm=T),
                                      w.precip_t1=quantile(cpath.cover$w.precip_t1,0.25,na.rm=T)))

C3.predict.dat <- data.frame(Total_t1=rep(seq(min(cpath.cover$Total_t1),max(cpath.cover$Total_t1),length=50),2),
                             r_C3=c(C3.predict.Wet,C3.predict.Dry),Environment=c(rep("Wet",50),rep("Dry",50)))


C4.predict.Wet <- predict(C4_model,
                           data.frame(Total_t1=seq(min(cpath.cover$Total_t1),max(cpath.cover$Total_t1),length=50),
                                      w.precip_t1=median(cpath.cover$w.precip_t1,na.rm=T),
                                      s.precip_t1=quantile(cpath.cover$s.precip_t1,0.75,na.rm=T)))
C4.predict.Dry <- predict(C4_model,
                          data.frame(Total_t1=seq(min(cpath.cover$Total_t1),max(cpath.cover$Total_t1),length=50),
                                     w.precip_t1=median(cpath.cover$w.precip_t1,na.rm=T),
                                     s.precip_t1=quantile(cpath.cover$s.precip_t1,0.25,na.rm=T)))

C4.predict.dat <- data.frame(Total_t1=rep(seq(min(cpath.cover$Total_t1),max(cpath.cover$Total_t1),length=50),2),
                             r_C4=c(C4.predict.Wet,C4.predict.Dry),Environment=c(rep("Wet",50),rep("Dry",50)))


cpath.cover$Environment.C3 <- ifelse(cpath.cover$w.precip_t1<median(cpath.cover$w.precip_t1,na.rm=T),"Dry","Wet")
cpath.cover$Environment.C4 <- ifelse(cpath.cover$s.precip_t1<median(cpath.cover$s.precip_t1,na.rm=T),"Dry","Wet")

ggplot(data=cpath.cover,aes(x=Total_t1,y=r_C3,col=Environment.C3)) +
  geom_point()+
  geom_line(data=C3.predict.dat,aes(x=Total_t1,y=r_C3,col=Environment),size=1)  +
  theme_classic()+
  labs(x="Total percent cover, year t+1",y="r for C3 plants",col="Winter Precipitation, \n year t+1")


ggplot(data=cpath.cover,aes(x=Total_t1,y=r_C4,col=Environment.C4)) +
  geom_point()+
  geom_line(data=C4.predict.dat,aes(x=Total_t1,y=r_C4,col=Environment),size=1) +
  theme_classic()+
  labs(x="Total percent cover, year t+1",y="r for C4 plants",col="Summer Precipitation, \n year t+1")

# Using competition in year t and climate in year t1 as predictors

C3_model <- lm(r_C3 ~ log(Total_t)*s.precip_t1*w.precip_t1,data=cpath.cover)
C4_model <- lm(r_C4 ~ log(Total_t)*s.precip_t1*w.precip_t1,data=cpath.cover)

C3.predict.Wet <- predict(C3_model,
                           data.frame(Total_t=seq(min(cpath.cover$Total_t),max(cpath.cover$Total_t),length=50),
                                      s.precip_t1=median(cpath.cover$s.precip_t1,na.rm=T),
                                      w.precip_t1=quantile(cpath.cover$w.precip_t1,0.75,na.rm=T)))
C3.predict.Dry <- predict(C3_model,
                          data.frame(Total_t=seq(min(cpath.cover$Total_t),max(cpath.cover$Total_t),length=50),
                                     s.precip_t1=median(cpath.cover$s.precip_t1,na.rm=T),
                                     w.precip_t1=quantile(cpath.cover$w.precip_t1,0.25,na.rm=T)))

C3.predict.dat <- data.frame(Total_t=rep(seq(min(cpath.cover$Total_t),max(cpath.cover$Total_t),length=50),2),
                             r_C3=c(C3.predict.Wet,C3.predict.Dry),Environment=c(rep("Wet",50),rep("Dry",50)))


C4.predict.Wet <- predict(C4_model,
                           data.frame(Total_t=seq(min(cpath.cover$Total_t),max(cpath.cover$Total_t),length=50),
                                      w.precip_t1=median(cpath.cover$w.precip_t1,na.rm=T),
                                      s.precip_t1=quantile(cpath.cover$s.precip_t1,0.75,na.rm=T)))
C4.predict.Dry <- predict(C4_model,
                          data.frame(Total_t=seq(min(cpath.cover$Total_t),max(cpath.cover$Total_t),length=50),
                                     w.precip_t1=median(cpath.cover$w.precip_t1,na.rm=T),
                                     s.precip_t1=quantile(cpath.cover$s.precip_t1,0.25,na.rm=T)))

C4.predict.dat <- data.frame(Total_t=rep(seq(min(cpath.cover$Total_t),max(cpath.cover$Total_t),length=50),2),
                             r_C4=c(C4.predict.Wet,C4.predict.Dry),Environment=c(rep("Wet",50),rep("Dry",50)))

cpath.cover$Environment.C3 <- ifelse(cpath.cover$w.precip_t1<median(cpath.cover$w.precip_t1,na.rm=T),"Dry","Wet")
cpath.cover$Environment.C4 <- ifelse(cpath.cover$s.precip_t1<median(cpath.cover$s.precip_t1,na.rm=T),"Dry","Wet")

ggplot(data=cpath.cover,aes(x=Total_t,y=r_C3,col=Environment.C3)) +
  geom_point()+
  geom_line(data=C3.predict.dat,aes(x=Total_t,y=r_C3,col=Environment),size=1) +
  theme_classic()+
  labs(x="Total percent cover, year t",y="r for C3 plants",col="Winter Precipitation, \n year t+1")


ggplot(data=cpath.cover,aes(x=Total_t,y=r_C4,col=Environment.C4)) +
  geom_point()+
  geom_line(data=C4.predict.dat,aes(x=Total_t,y=r_C4,col=Environment),size=1) +
  theme_classic()+
  labs(x="Total percent cover, year t",y="r for C4 plants",col="Summer Precipitation, \n year t+1")


# Using competition and climate in year t as predictors

C3_model <- lm(r_C3 ~ log(Total_t)*s.precip_t*w.precip_t,data=cpath.cover)
C4_model <- lm(r_C4 ~ log(Total_t)*s.precip_t*w.precip_t,data=cpath.cover)

C3.predict.Wet <- predict(C3_model,
                           data.frame(Total_t=seq(min(cpath.cover$Total_t),max(cpath.cover$Total_t),length=50),
                                      s.precip_t=median(cpath.cover$s.precip_t,na.rm=T),
                                      w.precip_t=quantile(cpath.cover$w.precip_t,0.75,na.rm=T)))
C3.predict.Dry <- predict(C3_model,
                          data.frame(Total_t=seq(min(cpath.cover$Total_t),max(cpath.cover$Total_t),length=50),
                                     s.precip_t=median(cpath.cover$s.precip_t,na.rm=T),
                                     w.precip_t=quantile(cpath.cover$w.precip_t,0.25,na.rm=T)))

C3.predict.dat <- data.frame(Total_t=rep(seq(min(cpath.cover$Total_t),max(cpath.cover$Total_t),length=50),2),
                             r_C3=c(C3.predict.Wet,C3.predict.Dry),Environment=c(rep("Wet",50),rep("Dry",50)))


C4.predict.Wet <- predict(C4_model,
                           data.frame(Total_t=seq(min(cpath.cover$Total_t),max(cpath.cover$Total_t),length=50),
                                      w.precip_t=median(cpath.cover$w.precip_t,na.rm=T),
                                      s.precip_t=quantile(cpath.cover$s.precip_t,0.75,na.rm=T)))
C4.predict.Dry <- predict(C4_model,
                          data.frame(Total_t=seq(min(cpath.cover$Total_t),max(cpath.cover$Total_t),length=50),
                                     w.precip_t=median(cpath.cover$w.precip_t,na.rm=T),
                                     s.precip_t=quantile(cpath.cover$s.precip_t,0.25,na.rm=T)))

C4.predict.dat <- data.frame(Total_t=rep(seq(min(cpath.cover$Total_t),max(cpath.cover$Total_t),length=50),2),
                             r_C4=c(C4.predict.Wet,C4.predict.Dry),Environment=c(rep("Wet",50),rep("Dry",50)))

cpath.cover$Environment.C3 <- ifelse(cpath.cover$w.precip_t<median(cpath.cover$w.precip_t,na.rm=T),"Dry","Wet")
cpath.cover$Environment.C4 <- ifelse(cpath.cover$s.precip_t<median(cpath.cover$s.precip_t,na.rm=T),"Dry","Wet")

ggplot(data=cpath.cover,aes(x=Total_t,y=r_C3,col=Environment.C3)) +
  geom_point()+
  geom_line(data=C3.predict.dat,aes(x=Total_t,y=r_C3,col=Environment),size=1) +
  theme_classic()+
  labs(x="Total percent cover, year t",y="r for C3 plants",col="Winter Precipitation, \n year t")


ggplot(data=cpath.cover,aes(x=Total_t,y=r_C4,col=Environment.C4)) +
  geom_point()+
  geom_line(data=C4.predict.dat,aes(x=Total_t,y=r_C4,col=Environment),size=1) +
  theme_classic()+
  labs(x="Total percent cover, year t",y="r for C4 plants",col="Summer Precipitation, \n year t+1")
