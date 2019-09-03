# Schisto snail analyses incl. NMDS of exhibited behaviors, GLMMs of
# movement, depth, and distance traveled w/ fixed efffect of infection state 
# RMF 5/30/19
library(vegan)
library(ggplot2)
library(cowplot)
library(lme4)

# Load in raw behavioral data
bulinus.raw <- read.csv("Bulinusbehavior_master.csv")
biomph.raw <- read.csv("Biomphalariabehavior_master.csv")

#### NMDS OF EXHIBITED BEHAVIORS ###############################
# Splitting the datasets to run adonis and simper NMDS analyses 
cols.raw<- c(7:10) 
bul.behavs.raw<- bulinus.raw[, cols.raw]
biomph.behavs.raw <- biomph.raw[, cols.raw]
View(bul.behavs.raw)
View(biomph.behavs.raw)

cols.factors<- c(1:6)
bul.factors <- bulinus.raw[, cols.factors]
biomph.factors <- biomph.raw[, cols.factors]
View(bul.factors)
View(biomph.factors)

#adonis analyses 
anosim(bul.behavs.raw, bul.factors$infection.state, permutations = 1000, distance = "bray")
anosim(biomph.behavs.raw, biomph.factors$Infection.state, permutations = 1000, distance = "bray")

simper(bul.behavs.raw,bul.factors$infection.state)
simper(biomph.behavs.raw,biomph.factors$Infection.state)

# Run NMDS for all behaviors
bul.behavsMDSresults <- metaMDS(bul.behavs.raw,distance="bray",k=2,autotransform=FALSE)
biomph.behavsMDSresults <- metaMDS(biomph.behavs.raw,distance="bray",k=2,autotransform=FALSE)

bul.behavsMDSresults
biomph.behavsMDSresults

# Create behavior ordination objects
bul.ord <- bul.behavsMDSresults[["species"]]
biomph.ord <- biomph.behavsMDSresults[["species"]]
bul.infection.state <- bul.factors[,5]
biomph.infection.state <- biomph.factors[,5]

# Plot NMDS for Bulinus
bul.scores <- as.data.frame(scores(bul.behavsMDSresults))
bul.scores$infection.state <- bul.infection.state

bul <- ggplot() + 
  geom_point(data=bul.scores,aes(x=NMDS1,y=NMDS2,colour=infection.state,shape=infection.state),size=3,stroke=2) +
  scale_colour_manual(values=c("gray30","gray70")) +
  scale_fill_manual(values=c("transparent","gray60")) +
  scale_shape_manual(values=c(1,17)) +
  coord_equal(xlim=c(-2,0.5),ylim=c(-2,1)) +
  annotate("segment",x=0,xend=bul.ord[,1],y=0,yend=bul.ord[,2], 
             colour="black",size=1.5,arrow=arrow()) +
  annotate("text",x=0.3,y=0.3,label="M",size=6) +
  annotate("text",x=-1.1,y=0.5, label = "S",size=6) +
  annotate("text",x=-0.1,y=-1.3,label="FL",size=6) +
  annotate("text",x=-0.6,y=-0.6, label = "FE",size=6) +
  theme_bw() +
  theme(axis.text.x = element_text(colour="black",size=12),  
        axis.text.y = element_text(colour="black",size=12), 
        axis.ticks = element_line(colour="black"),  
        axis.title.x = element_text(colour="black",size=18), 
        axis.title.y = element_text(colour="black",size=18),
        legend.position = "none",
        panel.background = element_rect(colour="white"), 
        panel.grid.major = element_line(colour="darkgray"),  
        panel.grid.minor = element_line(colour="darkgray"),  
        plot.background = element_rect(fill="white"))
bul
bul + labs(shape="Infection Status",colour="Infection Status")

biomph.scores <- as.data.frame(scores(biomph.behavsMDSresults))
biomph.scores$infection.state <- biomph.infection.state
  
biomph <- ggplot() + 
  geom_point(data=biomph.scores,aes(x=NMDS1,y=NMDS2,colour=infection.state,shape=infection.state),size=3,stroke=2) +
  scale_colour_manual(values=c("gray30","gray70")) +
  scale_fill_manual(values=c("transparent","gray70")) +
  scale_shape_manual(values=c(1,17)) +
  coord_equal(xlim=c(-1,1),ylim=c(-1.75,0.75)) +
  annotate("segment",x=0,xend=biomph.ord[,1],y=0,yend=biomph.ord[,2], 
           colour="black",size=1.5,arrow=arrow()) +
  annotate("text",x=-0.35,y=0.1,label="M",size=6) +
  annotate("text",x=0.8,y=-1.25, label = "S",size=6) +
  annotate("text",x=-0.3,y=0.45,label="FL",size=6) +
  annotate("text",x=0.8,y=0.52, label = "FE",size=6) +
  theme_bw() +
  theme(axis.text.x = element_text(colour="black",size=12),  
        axis.text.y = element_text(colour="black",size=12), 
        axis.ticks = element_line(colour="black"),  
        axis.title.x = element_text(colour="black",size=18), 
        axis.title.y = element_text(colour="black",size=18),
        legend.position = "none",
        panel.background = element_rect(colour="white"), 
        panel.grid.major = element_line(colour="darkgray"),  
        panel.grid.minor = element_line(colour="darkgray"),  
        plot.background = element_rect(fill="white"))
biomph
biomph + labs(shape="Infection Status",colour="Infection Status")

ggdraw(plot=NULL,xlim=c(0,20),ylim=c(0,10))+
  draw_plot(bul,x=0,y=0,width=9.5,height=10)+
  draw_plot(biomph,x=10,y=0,width=9.5,height=10)+
  draw_label("(a)",x=0.5,y=8,size=20)+
  draw_label("(b)",x=10.5,y=8.2,size=20)

# t-test assuming unequal variance for each behavior
# Bulinus
t.test(bul.behavs.infected$M, bul.behavs.uninfected$M, alternative = "two.sided", var.equal = FALSE)
t.test(bul.behavs.infected$S, bul.behavs.uninfected$S, alternative = "two.sided", var.equal = FALSE)
t.test(bul.behavs.infected$FL, bul.behavs.uninfected$FL, alternative = "two.sided", var.equal = FALSE)
t.test(bul.behavs.infected$FE, bul.behavs.uninfected$FE, alternative = "two.sided", var.equal = FALSE)

# Biomphalaria
t.test(biomph.behavs.infected$M, biomph.behavs.uninfected$M, alternative = "two.sided", var.equal = FALSE)
t.test(biomph.behavs.infected$S, biomph.behavs.uninfected$S, alternative = "two.sided", var.equal = FALSE)
t.test(biomph.behavs.infected$FL, biomph.behavs.uninfected$FL, alternative = "two.sided", var.equal = FALSE)
t.test(biomph.behavs.infected$FE, biomph.behavs.uninfected$FE, alternative = "two.sided", var.equal = FALSE)

#### GLMM FOR MOVEMENT ####################
# General linear mixed model (mixed because we need random effects)
# the model has one fixed effect of infection status
# and one random effect of date

# Bulinus
bulinus_movement_model<-glmer(M~infection.state+(1|Date)
                     ,data=bulinus.raw)
summary(bulinus_movement_model)

# Biomphalaria
biomph_movement_model<-glmer(M~Infection.state+(1|Date)
                    ,data=biomph.raw)
summary(biomph_movement_model)

# Extract p-values from these two models
# Bulinus
coefs <- data.frame(coef(summary(bulinus_movement_model)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
confint(bulinus_movement_model)

# Biomph
coefs <- data.frame(coef(summary(biomph_movement_model)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
confint(biomph_movement_model)

# Plot underlying data distribution as box plots
#Now let's make a box-and-whisker plot that shows underlying data distribution.

#first, we need to take the mean depth within each trial for infected versus uninfected. 
#First, we need to make an ID column that gives us the date-infection status.

bulinus.raw$unique_ID<-paste(bulinus.raw$Date,bulinus.raw$infection.state,sep="-")
biomph.raw$unique_ID<-paste(biomph.raw$Date,biomph.raw$Infection.state,sep="-")


#BULINUS

bulinus_aggregated<-aggregate(bulinus.raw$M,by=list(bulinus.raw$unique_ID),
                              FUN=mean,na.rm=T)

#make a column that gives infection status
aggregate(cbind((bulinus.raw$infection.state)),by=list(bulinus.raw$unique_ID),function(x){
  ux<-unique(x)
  ux[which.max(tabulate(match(x,ux)))]})
agg_infection_status<-aggregate(cbind((bulinus.raw$infection.state)),by=list(bulinus.raw$unique_ID)
                                ,function(x){levels(bulinus.raw$infection.state)[max(as.numeric(x))]})
agg_infection_status<-agg_infection_status[,-1]

#put it all together

bulinus_aggregated$infection_status<-agg_infection_status

names(bulinus_aggregated)<-c("unique_ID","movement","infection_status")

bulinus_boxplot<-ggplot(bulinus_aggregated,aes(x=infection_status,y=movement,group=infection_status))+
  geom_boxplot()+
  xlab("Infection Status")+
  ylab("Frequency of movement")+
  theme_bw() +
  theme(axis.text.x = element_text(colour="black",size=18),  
        axis.text.y = element_text(colour="black",size=18), 
        axis.ticks = element_line(colour="black"),  
        axis.title.x = element_text(colour="black",size=18), 
        axis.title.y = element_text(colour="black",size=18),
        panel.background = element_rect(colour="white"), 
        panel.grid.major = element_line(colour="gray"),  
        panel.grid.minor = element_line(colour="gray"),  
        plot.background = element_rect(fill="white"),
        legend.position = "none")
bulinus_boxplot

#BIOMPH

biomph_aggregated<-aggregate(biomph.raw$M,by=list(biomph.raw$unique_ID),
                             FUN=mean,na.rm=T)

#make a column that gives infection status
aggregate(cbind((biomph.raw$Infection.state)),by=list(biomph.raw$unique_ID),function(x){
  ux<-unique(x)
  ux[which.max(tabulate(match(x,ux)))]})
agg_infection_status<-aggregate(cbind((biomph.raw$Infection.state)),by=list(biomph.raw$unique_ID)
                                ,function(x){levels(biomph.raw$Infection.state)[max(as.numeric(x))]})
agg_infection_status<-agg_infection_status[,-1]

#put it all together

biomph_aggregated$infection_status<-agg_infection_status

names(biomph_aggregated)<-c("unique_ID","movement","infection_status")

biomph_boxplot<-ggplot(biomph_aggregated,aes(x=infection_status,y=movement,group=infection_status))+
  geom_boxplot()+
  xlab("Infection Status")+
  ylab("Frequency of movement")+
  theme_bw() +
  theme(axis.text.x = element_text(colour="black",size=18),  
        axis.text.y = element_text(colour="black",size=18), 
        axis.ticks = element_line(colour="black"),  
        axis.title.x = element_text(colour="black",size=18), 
        axis.title.y = element_text(colour="black",size=18),
        panel.background = element_rect(colour="white"), 
        panel.grid.major = element_line(colour="gray"),  
        panel.grid.minor = element_line(colour="gray"),  
        plot.background = element_rect(fill="white"),
        legend.position = "none")
biomph_boxplot

ggdraw(plot=NULL,xlim=c(0,20),ylim=c(0,10))+
  draw_plot(bulinus_boxplot,x=0,y=0,width=9.5,height=10)+
  draw_plot(biomph_boxplot,x=10,y=0,width=9.5,height=10)+
  draw_label("(a)",x=1,y=9.5,size=20)+
  draw_label("(b)",x=11,y=9.5,size=20)

#### GLMM FOR DEPTH #########################################
#start by loading up your data

#tell R to pull in the file you want to work with
rawdata<-read.delim("depth_master.csv",header=T,sep=",",fileEncoding = "Latin1")

#this command tells R to recognize each column as a variable
attach(rawdata)

#now we can manipulate the dataset. Let's look at it first.
rawdata

#we want to get rid of any runs where there was a plastic plant or where we didn't eliminate
#repeater snails.

data<-rawdata[which(rawdata$exclude_plant!="y" & rawdata$exclude_repeat_snails!="y"),]

#cool, that worked.
#now let's set up datasets just for bulinus and just for biomph

bulinus_data<-data[which(data$species=="bulinus"),]
biomph_data<-data[which(data$species=="biomph"),]

#now we're ready to do our analysis
#we will do a general linear MIXED model (mixed because we need random effects)
#the model has a fixed effect of infection status + a fixed effect of mean shell height
#and two random effects: one for the effect
#of minute (since depth will vary over the course of time) and one for the effect of date
#(since depth could differ between different dates)

library(lme4)

bulinus_model<-glmer(depth~infection_status+as.numeric(mean_shell_height_mm)+(1|minutes)+(1|date)
                     ,data=bulinus_data)
summary(bulinus_model)

biomph_model<-glmer(depth~infection_status+as.numeric(mean_shell_height_mm)+(1|minutes)+(1|date)
                    ,data=biomph_data)
summary(biomph_model)


#to extract p-values from these two models, use this code:

#first for bulinus
coefs <- data.frame(coef(summary(bulinus_model)))
#use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
confint(bulinus_model)

#then for biomph
coefs <- data.frame(coef(summary(biomph_model)))
#use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
confint(biomph_model)

#so this tells us that uninfected biomphs are lower down in space, but not where they are relative
#to the water's surface
#now let's test the hypothesis that infected snails should be closer to water surface

abs_depth_bulinus<-abs(bulinus_data$depth)

bulinus_model<-glmer(abs_depth_bulinus~infection_status+as.numeric(mean_shell_height_mm)+(1|minutes)+(1|date)
                     ,data=bulinus_data)
summary(bulinus_model)

abs_depth_biomph<-abs(biomph_data$depth)

biomph_model<-glmer(abs_depth_biomph~infection_status+as.numeric(mean_shell_height_mm)+(1|minutes)+(1|date)
                    ,data=biomph_data)
summary(biomph_model)

#to extract p-values from these two models, use this code:

#first for bulinus
coefs <- data.frame(coef(summary(bulinus_model)))
#use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

#then for biomph
coefs <- data.frame(coef(summary(biomph_model)))
#use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs


#cool, so uninfected snails tend to be deeper down in the water, further away from the water's 
#surface. We'd expect this bc infected snails should hang out near the water's surface, bc
#that is most convenient for the cercariae. But are infecteds actually water-quitting? Because
#that wouldn't make sense.

#first test whether uninfecteds are more likely to water quit

#begin by making columns that say whether the snail water quit or not

water_quit_bulinus<-vector("numeric",length(bulinus_data$depth))

for(i in 1:length(bulinus_data$depth))
  if(bulinus_data$depth[i]>0) {
    water_quit_bulinus[i]<-"y" } else {
      water_quit_bulinus[i]<-"n"
    }

water_quit_biomph<-vector("numeric",length(biomph_data$depth))

for(i in 1:length(biomph_data$depth))
  if(biomph_data$depth[i]>0) {
    water_quit_biomph[i]<-"y" } else {
      water_quit_biomph[i]<-"n"
    }

#now stick those new vectors into your dataset

bulinus_data$water_quit <- water_quit_bulinus
biomph_data$water_quit <- water_quit_biomph

#now do a logistic regression (logistic because the response is either yes or no, not continuous)

str(bulinus_data)

bulinus_model<-glmer(as.factor(water_quit)~infection_status+as.numeric(mean_shell_height_mm)+(1|minutes)
                     +(1|date),family="binomial",data=bulinus_data)
summary(bulinus_model)

biomph_model<-glmer(as.factor(water_quit)~infection_status+as.numeric(mean_shell_height_mm)+(1|minutes)
                    +(1|date),family="binomial",data=biomph_data)
summary(biomph_model)

#no one is more likely to water quit - everyone stayed in the water at equal rates


#then remove the water quitters and ask are the remainder of infected snails shallower than the remainder of 
#uninfected snails

in_water<-data[which(data$depth<0),]
in_water_bulinus<-in_water[which(in_water$species=="bulinus"),]
in_water_biomph<-in_water[which(in_water$species=="biomph"),]

bulinus_model<-glmer(depth~infection_status+as.numeric(mean_shell_height_mm)+(1|minutes)+(1|date)
                     ,data=in_water_bulinus)
summary(bulinus_model)


biomph_model<-glmer(depth~infection_status+as.numeric(mean_shell_height_mm)+(1|minutes)+(1|date)
                    ,data=in_water_biomph)
summary(biomph_model)

#to extract p-values from these two models, use this code:

#first for bulinus
coefs <- data.frame(coef(summary(bulinus_model)))
#use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
confint(bulinus_model)

#then for biomph
coefs <- data.frame(coef(summary(biomph_model)))
#use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
confint(biomph_model)

library(ggplot2)

#Now let's make a box-and-whisker plot that shows underlying data distribution.

#first, we need to take the mean depth within each trial for infected versus uninfected. 
#First, we need to make an ID column that gives us the date-infection status.

bulinus_data$unique_ID<-paste(bulinus_data$date,bulinus_data$infection_status,sep="-")
biomph_data$unique_ID<-paste(biomph_data$date,biomph_data$infection_status,sep="-")


#BULINUS

bulinus_aggregated<-aggregate(bulinus_data$depth,by=list(bulinus_data$unique_ID),
                              FUN=mean,na.rm=T)

#make a column that gives infection status
aggregate(cbind((bulinus_data$infection_status)),by=list(bulinus_data$unique_ID),function(x){
  ux<-unique(x)
  ux[which.max(tabulate(match(x,ux)))]})
agg_infection_status<-aggregate(cbind((bulinus_data$infection_status)),by=list(bulinus_data$unique_ID)
                                ,function(x){levels(bulinus_data$infection_status)[max(as.numeric(x))]})
agg_infection_status<-agg_infection_status[,-1]

#put it all together

bulinus_aggregated$infection_status<-agg_infection_status

names(bulinus_aggregated)<-c("unique_ID","depth","infection_status")

bulinus_boxplot<-ggplot(bulinus_aggregated,aes(x=infection_status,y=depth,group=infection_status,fill=infection_status))+
  geom_boxplot()+
  scale_fill_manual(values=c("tomato3","turquoise3")) +
  xlab("Infection Status")+
  ylab("Depth (cm)")+
  theme_bw() +
  theme(axis.text.x = element_text(colour="white",size=24),  
        axis.text.y = element_text(colour="white",size=24), 
        axis.ticks = element_line(colour="white"),  
        axis.title.x = element_text(colour="white",size=24), 
        axis.title.y = element_text(colour="white",size=24),
        panel.background = element_rect(colour="white"), 
        panel.grid.major = element_line(colour="darkgray"),  
        panel.grid.minor = element_line(colour="darkgray"),  
        plot.background = element_rect(fill="black"),
        legend.position = "none")
bulinus_boxplot

#BIOMPH

biomph_aggregated<-aggregate(biomph_data$depth,by=list(biomph_data$unique_ID),
                             FUN=mean,na.rm=T)

#make a column that gives infection status
aggregate(cbind((biomph_data$infection_status)),by=list(biomph_data$unique_ID),function(x){
  ux<-unique(x)
  ux[which.max(tabulate(match(x,ux)))]})
agg_infection_status<-aggregate(cbind((biomph_data$infection_status)),by=list(biomph_data$unique_ID)
                                ,function(x){levels(biomph_data$infection_status)[max(as.numeric(x))]})
agg_infection_status<-agg_infection_status[,-1]

#put it all together

biomph_aggregated$infection_status<-agg_infection_status

names(biomph_aggregated)<-c("unique_ID","depth","infection_status")

biomph_boxplot<-ggplot(biomph_aggregated,aes(x=infection_status,y=depth,group=infection_status,fill=infection_status))+
  geom_boxplot()+
  scale_fill_manual(values=c("tomato3","turquoise3")) +
  xlab("Infection Status")+
  ylab("Depth (cm)")+
  theme_bw() +
  theme(axis.text.x = element_text(colour="white",size=24),  
        axis.text.y = element_text(colour="white",size=24), 
        axis.ticks = element_line(colour="white"),  
        axis.title.x = element_text(colour="white",size=24), 
        axis.title.y = element_text(colour="black",size=24),
        panel.background = element_rect(colour="white"), 
        panel.grid.major = element_line(colour="darkgray"),  
        panel.grid.minor = element_line(colour="darkgray"),  
        plot.background = element_rect(fill="black"),
        legend.position = "none")
biomph_boxplot

#### GLMM FOR DISTANCE ######################################
# General linear mixed model (mixed because we need random effects)
# the model has one fixed effect of infection status
# and one random effect of date  
bulinus_dist_model<-glmer(Distance~infection.state+(1|Date)
                       ,data=bulinus.raw)
summary(bulinus_dist_model)
  
biomph_dist_model<-glmer(Distance~Infection.state+(1|Date)
                      ,data=biomph.raw)
summary(biomph_dist_model)
  
# Extract p-values from these two models
# Bulinus
coefs <- data.frame(coef(summary(bulinus_dist_model)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
confint(bulinus_dist_model)
  
# Biomphalaria
coefs <- data.frame(coef(summary(biomph_dist_model)))
#use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs 
confint(biomph_dist_model)

# Plot underlying data distribution as box plots
#Now let's make a box-and-whisker plot that shows underlying data distribution.

#first, we need to take the mean depth within each trial for infected versus uninfected. 
#First, we need to make an ID column that gives us the date-infection status.

bulinus.raw$unique_ID<-paste(bulinus.raw$Date,bulinus.raw$infection.state,sep="-")
biomph.raw$unique_ID<-paste(biomph.raw$Date,biomph.raw$Infection.state,sep="-")


#BULINUS

bulinus_aggregated<-aggregate(bulinus.raw$Distance,by=list(bulinus.raw$unique_ID),
                              FUN=mean,na.rm=T)

#make a column that gives infection status
aggregate(cbind((bulinus.raw$infection.state)),by=list(bulinus.raw$unique_ID),function(x){
  ux<-unique(x)
  ux[which.max(tabulate(match(x,ux)))]})
agg_infection_status<-aggregate(cbind((bulinus.raw$infection.state)),by=list(bulinus.raw$unique_ID)
                                ,function(x){levels(bulinus.raw$infection.state)[max(as.numeric(x))]})
agg_infection_status<-agg_infection_status[,-1]

#put it all together

bulinus_aggregated$infection_status<-agg_infection_status

names(bulinus_aggregated)<-c("unique_ID","distance","infection_status")

bulinus_boxplot<-ggplot(bulinus_aggregated,aes(x=infection_status,y=distance,group=infection_status))+
  geom_boxplot()+
  xlab("Infection Status")+
  ylab("Distance (cm)")+
  theme_bw() +
  theme(axis.text.x = element_text(colour="black",size=18),  
        axis.text.y = element_text(colour="black",size=18), 
        axis.ticks = element_line(colour="black"),  
        axis.title.x = element_text(colour="black",size=18), 
        axis.title.y = element_text(colour="black",size=18),
        panel.background = element_rect(colour="white"), 
        panel.grid.major = element_line(colour="gray"),  
        panel.grid.minor = element_line(colour="gray"),  
        plot.background = element_rect(fill="white"),
        legend.position = "none")
bulinus_boxplot

#BIOMPH

biomph_aggregated<-aggregate(biomph.raw$Distance,by=list(biomph.raw$unique_ID),
                             FUN=mean,na.rm=T)

#make a column that gives infection status
aggregate(cbind((biomph.raw$Infection.state)),by=list(biomph.raw$unique_ID),function(x){
  ux<-unique(x)
  ux[which.max(tabulate(match(x,ux)))]})
agg_infection_status<-aggregate(cbind((biomph.raw$Infection.state)),by=list(biomph.raw$unique_ID)
                                ,function(x){levels(biomph.raw$Infection.state)[max(as.numeric(x))]})
agg_infection_status<-agg_infection_status[,-1]

#put it all together

biomph_aggregated$infection_status<-agg_infection_status

names(biomph_aggregated)<-c("unique_ID","distance","infection_status")

biomph_boxplot<-ggplot(biomph_aggregated,aes(x=infection_status,y=distance,group=infection_status))+
  geom_boxplot()+
  xlab("Infection Status")+
  ylab("Distance (cm)")+
  theme_bw() +
  theme(axis.text.x = element_text(colour="black",size=18),  
        axis.text.y = element_text(colour="black",size=18), 
        axis.ticks = element_line(colour="black"),  
        axis.title.x = element_text(colour="black",size=18), 
        axis.title.y = element_text(colour="black",size=18),
        panel.background = element_rect(colour="white"), 
        panel.grid.major = element_line(colour="gray"),  
        panel.grid.minor = element_line(colour="gray"),  
        plot.background = element_rect(fill="white"),
        legend.position = "none")
biomph_boxplot

ggdraw(plot=NULL,xlim=c(0,20),ylim=c(0,10))+
  draw_plot(bulinus_boxplot,x=0,y=0,width=9.5,height=10)+
  draw_plot(biomph_boxplot,x=10,y=0,width=9.5,height=10)+
  draw_label("(a)",x=0.5,y=9.5,size=20)+
  draw_label("(b)",x=10.2,y=9.5,size=20)
