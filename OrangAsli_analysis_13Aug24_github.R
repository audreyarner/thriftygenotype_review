### Starting git commit: 53f1207b3c197189ae060a8832af8dece84d7578

library(ggplot2);library(ggmap); library(ggrepel); library(dplyr)
library(data.table)
library(ggrepel)
library(plyr)
library(reshape2)

setwd("~/Library/CloudStorage/Box-Box/Lea Lab/Audrey_Arner/Lab/Malaysia")

###############
# Figure 2 Map
###############

register_google('AIzaSyBqRWaHZTtsvaSOIwDdHjmTVPD-cXvaKrE')

register = read.delim("./TGHData/oa_village_register.csv", sep = ",")

#only villages clinics were conducted in
register = subset(register, village_id %in% c(2,13,19,5,0,3,20,21,22,23))
colnames(register)[2] <- "name"

register = select(register, name, lat, long, village_id, population)

#latitude and longitude for Kuala Lumpur to be included as a reference
kl = list(name = "Kuala Lumpur", lat=3.1319, long = 101.6841, village_id = 42482, population = "Capital")
register = rbind(register, kl)

s <- "element:geometry%7Ccolor:0xf5f5f5&style=element:labels%7Cvisibility:off&style=element:labels.icon%7Cvisibility:off&style=element:labels.text.fill%7Ccolor:0x616161&style=element:labels.text.stroke%7Ccolor:0xf5f5f5&style=feature:administrative%7Celement:geometry%7Cvisibility:off&style=feature:administrative.country%7Celement:geometry.stroke%7Ccolor:0x000000%7Cvisibility:on&style=feature:administrative.land_parcel%7Cvisibility:off&style=feature:administrative.land_parcel%7Celement:labels.text.fill%7Ccolor:0xbdbdbd&style=feature:administrative.neighborhood%7Cvisibility:off&style=feature:poi%7Cvisibility:off&style=feature:poi%7Celement:geometry%7Ccolor:0xeeeeee&style=feature:poi%7Celement:labels.text.fill%7Ccolor:0x757575&style=feature:poi.park%7Celement:geometry%7Ccolor:0xe5e5e5&style=feature:poi.park%7Celement:labels.text.fill%7Ccolor:0x9e9e9e&style=feature:road%7Cvisibility:off&style=feature:road%7Celement:geometry%7Ccolor:0xffffff&style=feature:road%7Celement:labels.icon%7Cvisibility:off&style=feature:road.arterial%7Celement:labels.text.fill%7Ccolor:0x757575&style=feature:road.highway%7Celement:geometry%7Ccolor:0xdadada&style=feature:road.highway%7Celement:labels.text.fill%7Ccolor:0x616161&style=feature:road.local%7Celement:labels.text.fill%7Ccolor:0x9e9e9e&style=feature:transit%7Cvisibility:off&style=feature:transit.line%7Celement:geometry%7Ccolor:0xe5e5e5&style=feature:transit.station%7Celement:geometry%7Ccolor:0xeeeeee&style=feature:water%7Celement:geometry%7Ccolor:0xc9c9c9&style=feature:water%7Celement:labels.text.fill%7Ccolor:0x9e9e9e&size=480x360"

#center map in median latitude and longitude of villages
medianlat = median(register$lat, na.rm=T)
medianlong = median(register$long, na.rm = T)
map <- get_googlemap(center=c(medianlong,medianlat), zoom = 7, scale = 1, style = s)
m<- ggmap(map)

plot = m + geom_point(data=register, aes(x = long, y = lat, color=population), size = 3, alpha = 0.7)+xlab('Longitude')+ylab('Latitude')+ 
            xlim(100,105) + ylim(1.5,7) + theme_bw(13) +
  scale_color_manual(values=c("#8d548d", "#ffd700", "#b989b9",
                              "#228b22", "#31ca31", "#f36500")) +
  ggtitle('Village locations in relation to Kuala Lumpur') +
  geom_label_repel(data = register, aes(x=long, y=lat, label = name, fill = population),min.segment.length=unit(0,'lines'),
                   box.padding = 0.35) +
  scale_fill_manual(values=c("#8d548d","#ffd700", "#b989b9",
                              "#228b22", "#31ca31", "#f36500")) 
plot
ggsave(plot, file="Fig2_gps.pdf", height=6, width =8, useDingbats=FALSE)

###############
# Data wrangling
###############
data1=read.csv('./TGHData/early_life.csv')
data2=read.csv('./TGHData/personal_information.csv')
data3=read.csv('./TGHData/traditional_lifestyle.csv')
data4=read.csv('./TGHData/medical.csv')

#calculate age when seen
data2$age<-as.numeric(as.Date(data2$date)-as.Date(data2$date_of_birth_ic))/365

#new column for ethnolinguistic group
data2$group<-'Other'
data2$group[which(data2$ethnicity___0==1)]<-'Batek Nong'
data2$group[which(data2$ethnicity___7==1)]<-'Semai'
data2$group[which(data2$ethnicity___1==1)]<-'Jehai'
data2$group[which(data2$ethnicity___10==1)]<-'Temuan'
data2$group[which(data2$ethnicity___9==1)]<-'Temiar'


both<-merge(data1,data3,by='rid')
both<-merge(both,data2[,c('rid','sex','age','group')],by='rid')

#put people into age binds
both$age_bin<-round_any(both$age,10, f = ceiling)

table(both$age_bin)
# only 3 people, remove
both<-subset(both,age_bin!=90)

#select only needed data from medical
both<-merge(both,data4[,c('rid','interview_location_med','waist_circum','body_fat')],by='rid')

register2=dplyr::select(register, c("village_id", "name", "lat", "long"))
both = merge(both, register2, by.x = "interview_location_med", by.y = "village_id")

###############
# Linear models distance to KL
###############

#new column for distance to KL
both$distance_kl = both$lat - 3.1319

mod_wild_meat = glm(meat_wild ~ distance_kl + sex + age, data = both, family = gaussian)
summary(mod_wild_meat)

mod_sugar = glm(sugar ~ distance_kl + sex + age, data = both, family = gaussian)
summary(mod_sugar)

mod_highest_education_stage = glm(highest_education_stage ~ distance_kl + sex + age, data = both, family = gaussian)
summary(mod_highest_education_stage)

mod_electricity_resid = glm(electricity_resid ~ distance_kl + sex + age, data = both, family = gaussian)
summary(mod_electricity_resid)

mod_kl_times = glm(kl_times ~ distance_kl + sex + age, data = both, family = gaussian)
summary(mod_kl_times)

mod_waist_circum = glm(waist_circum ~ distance_kl + sex + age, data = both, family = gaussian)
summary(mod_waist_circum)

mod_body_fat = glm(body_fat ~ distance_kl + sex + age, data = both, family = gaussian)
summary(mod_body_fat)

###############
# Figure 2 diet
###############
tmp1 <- as.data.frame(table(both$name,both$meat_wild))
tmp2 <- as.data.frame(table(both$name,both$sugar))

tmp1$Food<-'Wild meat'
tmp2$Food<-'Sugar'

tmp = rbind(tmp1,tmp2)
tmp<-merge(tmp,dplyr::distinct(both[,c('name','lat')],name,.keep_all=T),by.x = "Var1", by.y='name', all = T)

tmp<-subset(tmp,Var1!='Other')

plot = ggplot(tmp, aes(fill=Var2, y=Freq, x=reorder(Var1,-lat))) + geom_bar(position="fill", stat="identity") + 
  xlab("Location") +ylab('Proportion')+theme_classic(13)+ facet_wrap(~factor(Food)) + theme(text = element_text(size=20), plot.title = element_text(hjust = 0.5)) +
  scale_fill_discrete(name="Response",labels=c("Never", "Several times per month", "Several times per week", "Everyday"))+ggtitle('Food eaten during the last month')+ theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_brewer(palette = "Paired", name = "Response", labels = c("Never", "Several times \nper month", "Several times \nper week", "Everyday")) 

plot

ggsave(plot, file="Fig2_Food_13Aug24.pdf", height=6, width =12, useDingbats=FALSE)

###############
# Figure 2 electricity in residence
###############

tmp = as.data.frame(table(both$name,both$electricity_resid))
tmp<-merge(tmp,distinct(both[,c('name','lat')],name,.keep_all=T),by.x = "Var1", by.y='name', all = T)

tmp$Var2 <- factor(tmp$Var2, levels=c("1", "0"))

plot = ggplot(tmp, aes(fill=Var2, y=Freq, x=reorder(Var1,-lat))) + geom_bar(position="fill", stat="identity") + 
  xlab("name") +ylab('Proportion')+theme_classic(13)+ theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  theme(text = element_text(size=20), plot.title = element_text(hjust = 0.5)) +
  ggtitle('Electricity in residence')+ scale_fill_brewer(palette = "Paired", name = "Response", labels = c("Yes", "No"))
plot
ggsave(plot, file="Fig2_electricity.pdf", height=6, width =6, useDingbats=FALSE)

###############
# Figure 2 highest education achieved
###############
tmp <- as.data.frame(table(both$name,both$highest_education_stage))
tmp<-merge(tmp,distinct(both[,c('name','lat')],name,.keep_all=T),by.x = "Var1", by.y='name', all = T)

tmp$Var2 <- factor(tmp$Var2, levels=c("2", "1", "0"))

plot = ggplot(tmp, aes(fill=Var2, y=Freq, x=reorder(Var1,-lat))) + geom_bar(position="fill", stat="identity") + 
  xlab("name") +ylab('Proportion')+theme_classic(13)+ theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  theme(text = element_text(size=20), plot.title = element_text(hjust = 0.5)) +
  ggtitle('Highest education achieved')+ scale_fill_brewer(palette = "Paired", name = "Response", labels = c("Secondary", "Primary", "None"))
plot
ggsave(plot, file="Fig2_edu.pdf", height=6, width =6, useDingbats=FALSE)

###############
# Figure 3 age variation in diet
###############

tmp<-both[,c('group','age_bin','early_sugar_relative', "early_wild_meat_rel")]            

tmp2<-melt(tmp, id.vars=c("group", "age_bin"))
tmp3<-as.data.frame(table(tmp2$age_bin,tmp2$variable,tmp2$value))

new_names <- c(
  `early_sugar_relative` = "Sugar",
  `early_wild_meat_rel` = "Wild meat"
)

tmp3<-subset(tmp3,Var1!=90)
tmp3$Var3 <- factor(tmp3$Var3, levels=c("more", "same", "less"))

plot = ggplot(tmp3, aes(fill=Var3, y=Freq, x=Var1)) + geom_bar(position="fill", stat="identity") + 
  xlab("Age category") +ylab('Proportion')+theme_classic(13)+ theme (text = element_text(size=20), plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette = "Paired", name="Response",labels=c("More", "Same","Less"))+ggtitle('Change in diet from childhood')+facet_wrap(~Var2, labeller = as_labeller(new_names))

plot
ggsave(plot, file="Fig3_diet.pdf", height=6, width =10, useDingbats=FALSE)

###############
# Figure 3 age variation schooling
###############

tmp<-both[,c('group','age_bin','highest_education_stage')]            

tmp2<-melt(tmp, id.vars=c("group", "age_bin"))
tmp3<-as.data.frame(table(tmp2$age_bin,tmp2$variable,tmp2$value))
tmp3$Var3 <- factor(tmp3$Var3, levels=c("2", "1", "0"))

tmp3<-subset(tmp3,Var1!=90)
plot = ggplot(tmp3, aes(fill=Var3, y=Freq, x=Var1)) + geom_bar(position="fill", stat="identity") + 
  xlab("Age category") +ylab('Proportion')+theme_classic(13)+ theme(text = element_text(size=20), plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette = "Paired", name = "Response", labels = c("Secondary", "Primary", "None"))+ggtitle('Change in schooling across cohorts')
plot
ggsave(plot, file="Fig3_edu.pdf", height=5, width =5, useDingbats=FALSE)

###############
# Figure 3 age variation electricity
###############

tmp<-both[,c('group','age_bin','electricity_resid')]            

library(reshape2)
tmp2<-melt(tmp, id.vars=c("group", "age_bin"))
tmp3<-as.data.frame(table(tmp2$age_bin,tmp2$variable,tmp2$value))
tmp3$Var3 <- factor(tmp3$Var3, levels=c("1", "0"))

tmp3<-subset(tmp3,Var1!=90)
plot = ggplot(tmp3, aes(fill=Var3, y=Freq, x=Var1)) + geom_bar(position="fill", stat="identity") + 
  xlab("Age category") +ylab('Proportion')+theme_classic(13)+ theme(text = element_text(size=20), plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette = "Paired", name = "Response", labels = c("Yes", "No"))+ggtitle('Electricity in residence across cohorts')
plot
ggsave(plot, file="Fig3_electricity_residency.pdf", height=5, width =5, useDingbats=FALSE)

###############
# Supplementary figures
###############

detach(package:plyr)
library(dplyr)
WaistCirc = as.data.frame(both %>% group_by(name, sex) %>%  summarise(lat = mean(lat), disp=mean(body_fat, na.rm=T), sd = sd(body_fat, na.rm = T)))
WaistCirc$lowsd = WaistCirc$disp-WaistCirc$sd
WaistCirc$highsd = WaistCirc$disp+WaistCirc$sd

#took out kampung names
plot = ggplot(WaistCirc, aes(y=disp, x=reorder(name,-lat), fill = name)) + geom_pointrange(aes(ymin = lowsd, ymax = highsd, color = name)) + 
  xlab("Location") +ylab('Body fat percentage') +
  theme_classic() + 
  facet_wrap(~sex,labeller = labeller(sex = c("0" = "Female","1" = "Male"))) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ theme(text = element_text(size=20), legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values=c("#31ca31","#228b22","#f36500", "#31ca31","#b989b9","#31ca31","#b989b9","#8d548d","#228b22","#228b22"))+ggtitle('Variation in body fat percentage')

plot
ggsave(plot, file="Fig4_BodyFat_13Aug24.pdf", height=6, width =8, useDingbats=FALSE)

WaistCirc = as.data.frame(both %>% group_by(name, sex) %>%  summarise(lat = mean(lat), disp=mean(waist_circum, na.rm=T), sd = sd(waist_circum, na.rm = T)))
WaistCirc$lowsd = WaistCirc$disp-WaistCirc$sd
WaistCirc$highsd = WaistCirc$disp+WaistCirc$sd

#took out kampung names
plot = ggplot(WaistCirc, aes(y=disp, x=reorder(name,-lat), fill = name)) + geom_pointrange(aes(ymin = lowsd, ymax = highsd, color = name)) + 
  xlab("Location") +ylab('Waist circumference') + scale_color_manual(values=c("#31ca31","#228b22","#f36500", "#31ca31","#b989b9","#31ca31","#b989b9","#8d548d","#228b22","#228b22")) +
  theme_classic() + 
  facet_wrap(~sex,labeller = labeller(sex = c("0" = "Female","1" = "Male"))) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ theme(text = element_text(size=20), legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle('Variation in waist circumference') 

plot
ggsave(plot, file="Fig4_WaistCirc_13Aug24.pdf", height=6, width =8, useDingbats=FALSE)


