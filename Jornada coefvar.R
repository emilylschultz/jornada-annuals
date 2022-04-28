#########
# Coef of Var for each pathway
#Calculate the coefficient of variation for:
#C3 pathway at each site
#C4 pathway at each site
#Both C3 & C4 at each site


library(tidyverse)
library(dplyr)
library(ggplot2)
library(wesanderson)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))##set working directory to current file 


source("DataPrep20220411.R")

data <- read_csv("JRN_011002_npp_quadrat_meas.csv")

rain <- read_csv("JRN_NPPsite_graduated_rain_gauge_data.csv")


###We need to clean up the dates, and other parts of the data. 
data$month<-format(as.Date(data$date,format="%Y-%m-%d"), "%m")
data$year<-format(as.Date(data$date,format="%Y-%m-%d"), "%Y")
data$day<-format(as.Date(data$date,format="%Y-%m-%d"), "%d")
data$cnt<-as.numeric(as.character(data$cnt))
data$cover<-as.numeric(as.character(data$cover))
data$tcover<-data$cover*data$cnt ###Total cover is individual plant cover times the number of plants (needed for counts of multiple plants of same size)

###Vector for photo pathway by species.
c3<-unique(data$spp[which(data$cpath=="C3")])
c4<-unique(data$spp[which(data$cpath=="C4")])


#winter.data<-filter(data,month=="04"|month=="05"|month=="06",  habit=="A" )
###We are only interested in summer annuals so those that were found in summer/early fall and are annuals
###Note that this implicitly includes include c3 summer annuals 
summer.data<-filter(data,month=="08"|month=="09"|month=="10"|month=="11")
summer.data<-filter(summer.data,habit=="A")



####Spread out data by year and species for each quad-year
#*****sum cover at species-level
CP_total.cover.quad.sp<-spread(aggregate(tcover~site*quad*spp*year, data=rbind(summer.data),sum, drop=F),spp,tcover)

####Fill in zeros for NA's, NA's indicate quad years where species were not observed, so NA's. 
CP_total.cover.quad.sp[which(is.na(CP_total.cover.quad.sp)==T,arr.ind=T)]<-0 

CP_total.cover.quad.sp$year<-as.numeric(CP_total.cover.quad.sp$year)

CP_total.cover.sp<-(aggregate(.~year, data=CP_total.cover.quad.sp[,-c(1,2)],mean)) ###Mean cover overall across jornada

# ----------------- Plot total cover by pathway across all plots for each year-------------
#Sum of total cover per pathway group
CP_total.cover.quad.cpath<-spread(aggregate(tcover~site*quad*cpath*year, data=rbind(summer.data),sum, drop=F),cpath,tcover) ###total cover per quad

# a little more data formatting
CP_total.cover.quad.cpath[which(is.na(CP_total.cover.quad.cpath)==T,arr.ind=T)]<-0
CP_total.cover.quad.cpath$year<-as.numeric(CP_total.cover.quad.cpath$year)
head(CP_total.cover.quad.cpath)

#Mean Cover per pathway per site
CP_total.cover.cpath.site<-(aggregate(.~year*site, data=CP_total.cover.quad.cpath[,-c(2)],mean))
head(CP_total.cover.cpath.site)

# summarize total cover by pathway 
CP_total.cover.cpath.site %>% 
  group_by(year) %>% 
  summarize(across(c("C3", "C4"), ~ mean(.x, na.rm = TRUE)))

# Across all plots for all years
CP_total.cover.cpath.site
head(CP_total.cover.cpath.site)



##### CV for all ####

#Coefficient of Variation = (sd / mu) *100
co.var <- function(x) ( 100*sd(x)/mean(x) )
######################## Delete cpath to make all the cover per site 
#Mean Cover per site per year
CP_total.cover.cpath.site$total_cover <- CP_total.cover.cpath.site$C3 + CP_total.cover.cpath.site$C4
CP_total.cover <- tibble(CP_total.cover.cpath.site$year, CP_total.cover.cpath.site$site,CP_total.cover.cpath.site$total_cover)
colnames(CP_total.cover) <- c("year", "site", "total cover")
head(CP_total.cover)
rbind(CP_total.cover)

cv_site_all_path <- aggregate(CP_total.cover$`total cover` ~ CP_total.cover$site, data = CP_total.cover, co.var) # coefficient of variation for each site across all paths and years
colnames(cv_site_all_path) <- c("site","CV")


# CV for c3 for each site
head(CP_total.cover.cpath.site)
CP_total.cover.c3.site <- aggregate(CP_total.cover.cpath.site$C3 ~ CP_total.cover.cpath.site$site, data = CP_total.cover.cpath.site, co.var)


# CV for c4 for each site
CP_total.cover.c4.site <- aggregate(CP_total.cover.cpath.site$C4 ~ CP_total.cover.cpath.site$site, data = CP_total.cover.cpath.site, co.var)

###### # CV for pathway #######
# compare the CV between C3 and C4
CV_path_comp <- tibble(CP_total.cover.c4.site$`CP_total.cover.cpath.site$site`, CP_total.cover.c3.site$`CP_total.cover.cpath.site$C3`,CP_total.cover.c4.site$`CP_total.cover.cpath.site$C4`)
colnames(CV_path_comp) <- c("site","C3", "C4")




#########################
# calculate the CV for the individuals 
#########################
#species <- unique(data$spp)
spp.cov.site <- data.frame(data$site, data$spp, data$cover, data$cpath)
colnames(spp.cov.site) <- c("site", "spp", "cover", "cpath")
spp.cov.site.cv <- aggregate(cover ~ site + spp, data = spp.cov.site, co.var)
colnames(spp.cov.site.cv) <- c("site","spp","CV" )

#look at data
View(spp.cov.site.cv)
spp.cov.site.cv <- spp.cov.site.cv[complete.cases(spp.cov.site.cv), ]

# plot
ggplot(spp.cov.site.cv, aes(x = CV, color = spp)) +
  geom_histogram(color = "black", fill = "white", binwidth = 2)

# can calculate the median and add 95% CI across all species, then for pathway, 
# make a multi-panel figure for variation across each site
ggplot(spp.cov.site.cv, aes( x=median(CV))) +
  geom_histogram(aes(color = site, fill = site), 
                 position = "dodge", binwidth = 1) 
  #geom_errorbar(aes( y=y, x=x))

test = spp.cov.site.cv %>% 
  group_by(site, spp) %>% 
  arrange(CV) %>% 
  mutate(count = 1:n())
  
p <- ggplot(test, aes(x=site, y=CV, 
                  group=count)) +
  geom_bar( stat="identity", position=position_dodge(0.7), width=0.6) +
  geom_errorbar()

  p+scale_color_brewer(palette="FantasticFox")+
  scale_fill_brewer(palette="FantasticFox")
#geom_errorbar()
  p+scale_color_gradient(low = "blue", high = "red")
  
  
ggplot(spp.cov.site.cv, aes( x=median(CV))) +
    geom_histogram(aes(color = site, fill = site), 
                   position = "dodge", binwidth = 1) 

