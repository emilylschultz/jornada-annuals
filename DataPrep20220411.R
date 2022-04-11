# Script to format Jornada annuals data for analyses

# load packages
library(tidyverse)
library(reshape2)
library(corrplot)
library(heatmaply)
library(lubridate)
library(lme4)

# *** unless we want to upload data probably change this to your local data repository		 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set working directory to current file 
		 
#-------------Import Data, Species specific cover data, rainfall, species list---------------------
data<-read.csv('JRN_011002_npp_quadrat_meas.csv')  
rain<-read.csv('JRN_NPPsite_graduated_rain_gauge_data.csv')

# Clean up the dates, and other parts of the data. 
data$month<-format(as.Date(data$date,format="%Y-%m-%d"), "%m")
data$year<-format(as.Date(data$date,format="%Y-%m-%d"), "%Y")
data$day<-format(as.Date(data$date,format="%Y-%m-%d"), "%d")
data$cnt<-as.numeric(as.character(data$cnt))
data$cover<-as.numeric(as.character(data$cover))
data$tcover<-data$cover*data$cnt ###Total cover is individual plant cover times the number of plants (needed for counts of multiple plants of same size)

###Vector for photo pathway by species.
c3<-unique(data$spp[which(data$cpath=="C3")])
c4<-unique(data$spp[which(data$cpath=="C4")])


# We are only interested in summer annuals so those that were found in summer/early fall and are annauls
###Note that this implicitly includes include c3 summer annuals 
summer.data<-filter(data,month=="08"|month=="09"|month=="10"|month=="11")
summer.data<-filter(summer.data,habit=="A")

# ----------------------- Aggregate data at species level ----------------------------

# Spread out data by year and species for each quad-year
# summarize cover at species-level
CP_total.cover.quad.sp<-spread(aggregate(tcover~site*quad*spp*year, data=rbind(summer.data),sum, drop=F),spp,tcover)

# Fill in zeros for NA's, NA's indicate quad years where species were not observed, so NA's. 
CP_total.cover.quad.sp[which(is.na(CP_total.cover.quad.sp)==T,arr.ind=T)]<-0 

CP_total.cover.quad.sp$year<-as.numeric(CP_total.cover.quad.sp$year) # make year numeric

# Mean cover overall across jornada
CP_total.cover.sp<-(aggregate(.~year, data=CP_total.cover.quad.sp[,-c(1,2)],mean)) 

# -------------- Aggregate Total cover data by pathway ------------------
#Sum of total cover per pathway group
CP_total.cover.quad.cpath<-spread(aggregate(tcover~site*quad*cpath*year, data=rbind(summer.data),sum, drop=F),cpath,tcover) ###total cover per quad

# Replace  NA's with zeroes, make year numeric
CP_total.cover.quad.cpath[which(is.na(CP_total.cover.quad.cpath)==T,arr.ind=T)]<-0
CP_total.cover.quad.cpath$year<-as.numeric(CP_total.cover.quad.cpath$year)

#Mean Cover per pathway per site
CP_total.cover.cpath.site<-(aggregate(.~year*site, data=CP_total.cover.quad.cpath[,-c(2)],mean))

# Visualize total cover by pathway 
colors <- c("C3" = "#fc9272", "C4" = "#67a9cf")

CP_total.cover.cpath.site %>% group_by(year) %>% summarize(across(c("C3", "C4"), ~ mean(.x, na.rm = TRUE))) %>% ggplot(aes(x = year, y = log(C3))) + geom_line(aes(col = "C3"), lwd = 1.2) + geom_line(aes(y = log(C4), col = "C4"), lwd = 1.2) + theme_classic() + labs(y = "log(total cover)", color = "Pathway") + scale_color_manual(values = colors)

# ---------------- Adding in rainfall data -------------------------
# Adding both winter and summer rainfall
glimpse(rain)

rain$month<-as.numeric(format(as.Date(rain$date,format="%Y-%m-%d"), "%m"))
rain$year<-as.numeric(format(as.Date(rain$date,format="%Y-%m-%d"), "%Y"))
rain$day<-as.numeric(format(as.Date(rain$date,format="%Y-%m-%d"), "%d"))

# reformat year so that winter rainfall can be calculated
rain$year[ which(rain$month>10)] = rain$year[which(rain$month>10)] +1

# specifiy seasonal rainfall
summer.rain <-filter(rain , month=="06"|month=="07"|month=="08"|month=="09"| month == "10")
winter.rain <- filter(rain , month=="11"|month=="12"|month=="01"|month=="02"|month =="03"|month=="04"|month =="05")

# aggregate rain by site
summer.rain.site <-spread(aggregate(p_mm~site*year, data=rbind(summer.rain),sum, drop=F),site,p_mm) 
winter.rain.site <- spread(aggregate(p_mm~site*year, data=rbind(winter.rain),sum, drop=F),site,p_mm)

# Mean rainfall across all sites
summer.rain <- summer.rain.site %>% rowwise() %>% 
  mutate(meanrain = mean(c_across(-1), na.rm = T)) %>% select(c(year,meanrain))

# ------------------ Total cover as a function of summer precip. --------------------

# rainfall and cover dataframes
s.rain.site.long <- summer.rain.site %>% pivot_longer(-c(1), names_to = "site", values_to = "s.precip")
w.rain.site.long <- winter.rain.site %>% pivot_longer(-c(1), names_to = "site", values_to = "w.precip")

rain.site.long <- left_join(w.rain.site.long, s.rain.site.long)
cpath.cover.dat <- left_join(CP_total.cover.cpath.site, rain.site.long)

# ------------------- Aggregate Abundance (count data) ---------------------

# Sum of abundance per pathway per group
CP_count.quad.cpath<-spread(aggregate(cnt~site*quad*cpath*year, data=rbind(summer.data),sum, drop=F),cpath,cnt) #counts per quad

# Fill in zeros for NA's aka where species were not observed 
CP_count.quad.cpath[which(is.na(CP_count.quad.cpath)==T,arr.ind=T)]<-0 

CP_count.quad.cpath$year<-as.numeric(CP_count.quad.cpath$year)

# Mean abundance per pathway per site
CP_count.cpath.site<-(aggregate(.~year*site, data=CP_count.quad.cpath[,-c(2)],mean))

# Visualize total cover by pathway 
colors <- c("C3" = "#fc9272", "C4" = "#67a9cf")

CP_count.cpath.site %>% group_by(year) %>% summarize(across(c("C3", "C4"), ~ mean(.x, na.rm = TRUE))) %>% ggplot(aes(x = year, y = log(C3))) + geom_line(aes(col = "C3"), lwd = 1.2) + geom_line(aes(y = log(C4), col = "C4"), lwd = 1.2) + theme_classic() + labs(y = "log(abundance)", color = "Pathway") + scale_color_manual(values = colors)

# ---------------------------------------------------------------------------------------
# code to create heat map/corr-plot

CP_total.cover.sp<-(aggregate(.~year, data=CP_total.cover.quad.sp[,-c(1,2)],mean)) 

ppcorrplot<-function(data, yfilter=10){
pathway<-rep(NA,length(colnames(data)))
pathway[which(colnames(data)%in%c3)]<-"c3"
pathway[which(colnames(data)%in%c4)]<-"c4"

keep<-which(apply(data>0,2,sum)>yfilter)
spkeep<-colnames(data)

cols<-rep ("black",length(colnames(data) ))
cols[which(pathway=='c3')]<-'steelblue'
cols[which(pathway=='c4')]<-'coral'
cols<-cols[keep]


corder<-corrMatOrder(cor((data[,keep])),order ="hclust")
corrplot(cor((data[,keep])),method='color', diag = TRUE, order = "hclust", 
         tl.cex = .75, 
         tl.col=cols[corder], addrect = 2)
}
ppcorrplot(CP_total.cover.sp[,-1])
