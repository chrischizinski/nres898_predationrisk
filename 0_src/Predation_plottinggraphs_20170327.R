basic<-theme(axis.line = element_line(colour = "black"),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             panel.border = element_blank(),
             panel.background = element_blank(),
             text=element_text(size=25, family="Helvetica")
)
library(effects)
library(ggplot2)

#predation_all
#treatment:harvest
eff<-effect("treatment:harvest",m1)
summary(eff)
eff_df<- as.data.frame(eff)
eff_df

tiff('Figure4.tiff', units="in", width=9, height=8, res=600)

P=ggplot(data=eff_df, aes(treatment, fit, fill=harvest)) + geom_point(aes(colour=harvest),alpha=1, size=8,position=pd) + geom_errorbar(aes(ymax=upper,ymin=lower, colour=harvest), position=pd, width=0.2, size=1) 
P + theme(legend.position="top", text= element_text(size=22),axis.title.x=element_text(size=16,margin=margin(10,0,0,0)),axis.title.y=element_text(size=16,margin=margin(0,10,0,0)))
P + ylab("Predator attacks (All predators)") + theme(panel.grid=element_line(size=2))+basic+
  annotate("text", x = 0.93, y = 0.48, label = "ab",size=6)+
  annotate("text", x = 1.08, y = 0.29, label = "a",size=6)+
  annotate("text", x = 1.93, y = 0.66, label = "ab",size=6)+
  annotate("text", x = 2.08, y = 0.46, label = "ab",size=6)+
  annotate("text", x = 2.93, y = 0.44, label = "ab",size=6)+
  annotate("text", x = 3.08, y = 0.45, label = "ab",size=6)+
  annotate("text", x = 3.93, y = 0.47, label = "ab",size=6)+
  annotate("text", x = 4.08, y = 0.66, label = "b",size=6)+
  
  theme(axis.title.y=element_text(),axis.title.x = element_blank())+theme(legend.text=element_text(size=16))+theme(legend.position=c(.11, .95))+theme(legend.title=element_blank())+scale_y_continuous(expand = c(0, 0), limits = c(0, 0.75))

dev.off()

pd<-position_dodge(width = 0.3) #code to offset bars

levels(eff_df$harvest)
eff_df$harvest<- factor(eff_df$harvest, levels = c("Pre-harvest", "Post-harvest"))
levels(eff_df$harvest)

#treatment:habitat:harvest
eff <- Effect(c("treatment", "habitat", "harvest"), m4)
eff_df <- as.data.frame(eff)
eff_df

treatmentpmcol<-c("cyan4","coral1")

P=ggplot(data=eff_df, aes(treatment, fit, fill=harvest, group=habitat, shape=harvest)) + geom_point(aes(colour=harvest),alpha=5/8, size=8,position=pd) + 
  geom_errorbar(aes(ymax=upper,ymin=lower, colour=harvest), position=pd,alpha=5/8,width=0.2, size=1.5) 
P + theme(legend.position="top", text= element_text(size=22),axis.title.x=element_text(size=16,margin=margin(10,0,0,0)),axis.title.y=element_text(size=16,margin=margin(0,10,0,0)))
P + scale_x_discrete(labels=c("R E M","R E M","R E M","R E M"))+xlab("Cropped             Pasture                          Planting                 Woody debris")+ ylab("Predation rate (All predators)") + theme(axis.title.y=element_text(),axis.title.x = element_blank())+theme(legend.text=element_text(size=16))+basic+
  theme(legend.position=c(.1, .9))+theme(legend.title=element_blank())+scale_y_continuous(expand = c(0, 0), limits = c(0, 0.95))


pd<-position_dodge(width = 0.4)#code to offset bars

pd2<-position_dodge(width = 0.2)

levels(eff_df$harvesting)

levels(eff_df$harvest)
eff_df$harvest<- factor(eff_df$harvest, levels = c("Pre-harvest", "Post-harvest"))
levels(eff_df$harvest)

levels(eff_df$habitat)
eff_df$habitat <- factor(eff_df$habitat, levels = c("Remnant", "Edge", "Matrix"))
levels(eff_df$habitat)

#habitat
eff<-effect("habitat",m1)
summary(eff)
eff_df<- as.data.frame(eff)
eff_df

tiff('Figure3a.tiff', units="in", width=6.5, height=6.5, res=600)

P=ggplot(data=eff_df, aes(habitat, fit)) + geom_point(alpha=1, size=8,position=pd) + 
  geom_errorbar(aes(ymax=upper,ymin=lower), position=pd, width=0.2, size=1.5) 
P + theme(legend.position="top", text= element_text(size=22),axis.title.x=element_text(size=16,margin=margin(10,0,0,0)),axis.title.y=element_text(size=16,margin=margin(0,10,0,0)))
P + ylab("Predator attacks (All predators)") + theme(panel.grid=element_line(size=2))+basic+
  annotate("text", x = 1, y = 0.41, label = "a",size=6)+
  annotate("text", x = 2, y = 0.66, label = "b",size=6)+
  annotate("text", x = 3, y = 0.37, label = "a",size=6)+
  theme(axis.title.y=element_text(),axis.title.x = element_blank())+
  theme(legend.text=element_text(size=16))+theme(legend.position="none")+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.7))

dev.off()

levels(eff_df$habitat)
eff_df$habitat <- factor(eff_df$habitat, levels = c("Remnant", "Edge", "Matrix"))
levels(eff_df$habitat)

pd<-position_dodge(width = 0.3) #code to offset bars

lsm = lsmeans(m1, pairwise ~ treatment:habitat:harvest,adjust="tukey")
letterz<-cld(lsm, alpha=0.15, Letters=letters)
posthoc<-letterz$.group
letterz

#BIRDS
#habitat:harvest
eff<-effect("habitat:harvest",m3)
summary(eff)
eff_df<- as.data.frame(eff)
eff_df

tiff('Figure5a.tiff', units="in", width=9, height=8, res=600)

P=ggplot(data=eff_df, aes(habitat, fit, fill=harvest)) + geom_point(aes(colour=harvest),alpha=1, size=8,position=pd) +geom_errorbar(aes(ymax=upper,ymin=lower, colour=harvest), position=pd, width=0.2, size=1.5) 
P + theme(legend.position="top", text= element_text(size=22),axis.title.x=element_text(size=16,margin=margin(10,0,0,0)),axis.title.y=element_text(size=16,margin=margin(0,10,0,0)))
P + ylab("Predator attacks (birds)") + theme(panel.grid=element_line(size=2))+basic+
  annotate("text", x = 0.93, y = 0.26, label = "ab",size=6)+
  annotate("text", x = 1.08, y = 0.31, label = "ab",size=6)+
  annotate("text", x = 1.93, y = 0.72, label = "c",size=6)+
  annotate("text", x = 2.08, y = 0.54, label = "bc",size=6)+
  annotate("text", x = 2.93, y = 0.34, label = "ab",size=6)+
  annotate("text", x = 3.08, y = 0.18, label = "a",size=6)+
  theme(axis.title.y=element_text(),axis.title.x = element_blank())+
  theme(legend.text=element_text(size=16))+theme(legend.position=c(.11, .95))+
  theme(legend.title=element_blank())+scale_y_continuous(expand = c(0, 0), limits = c(0, 0.75))

dev.off()

pd<-position_dodge(width = 0.3) #code to offset bars

levels(eff_df$harvest)
eff_df$harvest<- factor(eff_df$harvest, levels = c("Pre-harvest", "Post-harvest"))
levels(eff_df$harvest)

levels(eff_df$habitat)
eff_df$habitat <- factor(eff_df$habitat, levels = c("Remnant", "Edge", "Matrix"))
levels(eff_df$habitat)

#habitat
eff<-effect("habitat",m3)
summary(eff)
eff_df<- as.data.frame(eff)
eff_df

tiff('Figure3b.tiff', units="in", width=6.5, height=6.5, res=600)

P=ggplot(data=eff_df, aes(habitat, fit)) + geom_point(alpha=1, size=8,position=pd) + 
  geom_errorbar(aes(ymax=upper,ymin=lower), position=pd, width=0.2, size=1.5) 
P + theme(legend.position="top", text= element_text(size=22),axis.title.x=element_text(size=16,margin=margin(10,0,0,0)),axis.title.y=element_text(size=16,margin=margin(0,10,0,0)))
P + ylab("Predator attacks (birds)") + theme(panel.grid=element_line(size=2))+basic+
  annotate("text", x = 1, y = 0.26, label = "a",size=6)+
  annotate("text", x = 2, y = 0.6, label = "b",size=6)+
  annotate("text", x = 3, y = 0.23, label = "a",size=6)+
  theme(axis.title.y=element_text(),axis.title.x = element_blank())+
  theme(legend.text=element_text(size=16))+theme(legend.position="none")+
  theme(legend.title=element_blank())+scale_y_continuous(expand = c(0, 0), limits = c(0, 0.7))

dev.off()

levels(eff_df$habitat)
eff_df$habitat <- factor(eff_df$habitat, levels = c("Remnant", "Edge", "Matrix"))
levels(eff_df$habitat)

pd<-position_dodge(width = 0.3) #code to offset bars

#harvest
eff<-effect("harvest",m3)
summary(eff)
eff_df<- as.data.frame(eff)
eff_df

P=ggplot(data=eff_df, aes(harvest, fit)) + geom_point(aes(colour=harvest),alpha=1, size=8,position=pd) + geom_errorbar(aes(ymax=upper,ymin=lower, colour=harvest), position=pd, width=0.2, size=1.5) 
P + theme(legend.position="top", text= element_text(size=22),axis.title.x=element_text(size=16,margin=margin(10,0,0,0)),axis.title.y=element_text(size=16,margin=margin(0,10,0,0)))
P + ylab("Predation rate (birds)") + theme(panel.grid=element_line(size=2))+basic+
  annotate("text", x = 1, y = 0.33, label = "a",size=6)+
  annotate("text", x = 2, y = 0.24, label = "b",size=6)+
  theme(axis.title.y=element_text(),axis.title.x = element_blank())+theme(legend.text=element_text(size=16))+theme(legend.position="none")+theme(legend.title=element_blank())+scale_y_continuous(expand = c(0, 0), limits = c(0, 0.35))

levels(eff_df$harvest)
eff_df$harvest<- factor(eff_df$harvest, levels = c("Pre-harvest", "Post-harvest"))
levels(eff_df$harvest)

pd<-position_dodge(width = 0.3) #code to offset bars

#treatment:harvest
eff<-effect("treatment:harvest",m5)
summary(eff)
eff_df<- as.data.frame(eff)
eff_df

P=ggplot(data=eff_df, aes(treatment, fit, fill=harvest)) + geom_point(aes(colour=harvest),alpha=1, size=8,position=pd) +geom_errorbar(aes(ymax=upper,ymin=lower, colour=harvest), position=pd, width=0.2, size=1.5) 
P + theme(legend.position="top", text= element_text(size=22),axis.title.x=element_text(size=16,margin=margin(10,0,0,0)),axis.title.y=element_text(size=16,margin=margin(0,10,0,0)))
P + ylab("Predation rate (birds)") + theme(panel.grid=element_line(size=2))+basic+
  annotate("text", x = 0.93, y = 0.32, label = "ab",size=6)+
  annotate("text", x = 1.08, y = 0.19, label = "a",size=6)+
  annotate("text", x = 1.93, y = 0.56, label = "b",size=6)+
  annotate("text", x = 2.08, y = 0.26, label = "a",size=6)+
  annotate("text", x = 2.93, y = 0.35, label = "ab",size=6)+
  annotate("text", x = 3.08, y = 0.28, label = "ab",size=6)+
  annotate("text", x = 3.93, y = 0.40, label = "ab",size=6)+
  annotate("text", x = 4.08, y = 0.45, label = "ab",size=6)+
  theme(axis.title.y=element_text(face="italic"),axis.title.x = element_blank())+theme(legend.text=element_text(size=16))+theme(legend.position=c(.1, .85))+theme(legend.title=element_blank())+scale_y_continuous(expand = c(0, 0), limits = c(0, 0.6))

pd<-position_dodge(width = 0.3) #code to offset bars

levels(eff_df$harvest)
eff_df$harvest<- factor(eff_df$harvest, levels = c("Pre-harvest", "Post-harvest"))
levels(eff_df$harvest)

#MAMMALS
#habitat:harvest
eff<-effect("habitat:harvest",m2)
summary(eff)
eff_df<- as.data.frame(eff)
eff_df

tiff('Figure5b.tiff', units="in", width=9.5, height=8, res=600)

P=ggplot(data=eff_df, aes(habitat, fit, fill=harvest)) + geom_point(aes(colour=harvest),alpha=1, size=8,position=pd) + geom_errorbar(aes(ymax=upper,ymin=lower, colour=harvest), position=pd, width=0.2, size=1.5) 
P + theme(legend.position="top", text= element_text(size=22),axis.title.x=element_text(size=16,margin=margin(10,0,0,0)),axis.title.y=element_text(size=16,margin=margin(0,10,0,0)))
P + ylab("Predator attacks (mammals)") + theme(panel.grid=element_line(size=2))+basic+
  annotate("text", x = 0.93, y = 0.269, label = "ab",size=6)+
  annotate("text", x = 1.08, y = 0.169, label = "ab",size=6)+
  annotate("text", x = 1.93, y = 0.41, label = "b",size=6)+
  annotate("text", x = 2.08, y = 0.189, label = "ab",size=6)+
  annotate("text", x = 2.92, y = 0.124, label = "a",size=6)+
  annotate("text", x = 3.08, y = 0.199, label = "ab",size=6)+
  
  theme(axis.title.y=element_text(),axis.title.x = element_blank())+
  theme(legend.text=element_text(size=16))+theme(legend.position=c(.11, .95))+
  theme(legend.title=element_blank())+scale_y_continuous(expand = c(0, 0), limits = c(0, 0.45))

dev.off()

pd<-position_dodge(width = 0.3) #code to offset bars

levels(eff_df$harvest)
eff_df$harvest<- factor(eff_df$harvest, levels = c("Pre-harvest", "Post-harvest"))
levels(eff_df$harvest)

levels(eff_df$habitat)
eff_df$habitat <- factor(eff_df$habitat, levels = c("Remnant", "Edge", "Matrix"))
levels(eff_df$habitat)

#model.type
eff<-effect("model.type",m2)
summary(eff)
eff_df<- as.data.frame(eff)
eff_df

P=ggplot(data=eff_df, aes(model.type, fit)) + geom_point(aes(colour=model.type),alpha=1, size=8,position=pd) + geom_errorbar(aes(ymax=upper,ymin=lower, colour=model.type), position=pd, width=0.2, size=1.5) 
P + theme(legend.position="top", text= element_text(size=22),axis.title.x=element_text(size=16,margin=margin(10,0,0,0)),axis.title.y=element_text(size=16,margin=margin(0,10,0,0)))
P + ylab("Predation rate (mammals)") + theme(panel.grid=element_line(size=2))+basic+
  annotate("text", x = 1, y = 0.185, label = "b",size=6)+
  annotate("text", x = 2, y = 0.12, label = "a",size=6)+
  theme(axis.title.y=element_text(face="italic"),axis.title.x = element_blank())+theme(legend.text=element_text(size=16))+theme(legend.position=c(.11, .95))+theme(legend.title=element_blank())+scale_y_continuous(expand = c(0, 0), limits = c(0, 0.25))

levels(eff_df$harvest)
eff_df$harvest<- factor(eff_df$harvest, levels = c("Pre-harvest", "Post-harvest"))
levels(eff_df$harvest)

pd<-position_dodge(width = 0.3) #code to offset bars

#mammals model
#model.type:habitat:harvest
eff <- Effect(c("model.type", "habitat", "harvest"), m1)
eff_df <- as.data.frame(eff)
eff_df

treatmentpmcol<-c("cyan4","coral1")

P=ggplot(data=eff_df, aes(model.type, fit, fill=harvest, group=habitat, shape=harvest)) + geom_point(aes(colour=harvest),alpha=5/8, size=8,position=pd) + 
  geom_errorbar(aes(ymax=upper,ymin=lower, colour=harvest), position=pd,alpha=5/8,width=0.2, size=1.5) 
P + theme(legend.position="top", text= element_text(size=22),axis.title.x=element_text(size=16,margin=margin(10,0,0,0)),axis.title.y=element_text(size=16,margin=margin(0,10,0,0)))
P + scale_x_discrete(labels=c("R E M","R E M","R E M","R E M"))+xlab("Scented  Unscented")+ ylab("Predation attacks (mammals)") + theme(axis.title.y=element_text(),axis.title.x = element_blank())+theme(legend.text=element_text(size=16))+basic+
  theme(legend.position=c(.1, .9))+theme(legend.title=element_blank())+scale_y_continuous(expand = c(0, 0), limits = c(0, 0.95))


pd<-position_dodge(width = 0.4)#code to offset bars

pd2<-position_dodge(width = 0.2)

levels(eff_df$harvesting)

levels(eff_df$harvest)
eff_df$harvest<- factor(eff_df$harvest, levels = c("Pre-harvest", "Post-harvest"))
levels(eff_df$harvest)

levels(eff_df$habitat)
eff_df$habitat <- factor(eff_df$habitat, levels = c("Remnant", "Edge", "Matrix"))
levels(eff_df$habitat)

#model.type
eff<-effect("model.type",m2)
summary(eff)
eff_df<- as.data.frame(eff)
eff_df

P=ggplot(data=eff_df, aes(model.type, fit)) + geom_point(aes(colour=model.type),alpha=1, size=8,position=pd) + geom_errorbar(aes(ymax=upper,ymin=lower, colour=model.type), position=pd, width=0.2, size=1.5) 
P + theme(legend.position="top", text= element_text(size=22),axis.title.x=element_text(size=16,margin=margin(10,0,0,0)),axis.title.y=element_text(size=16,margin=margin(0,10,0,0)))
P + ylab("Predation rate (mammals)") + theme(panel.grid=element_line(size=2))+basic+
  annotate("text", x = 1, y = 0.199, label = "b",size=6)+
  annotate("text", x = 2, y = 0.139, label = "a",size=6)+
  theme(axis.title.y=element_text(),axis.title.x = element_blank())+theme(legend.text=element_text(size=16))+theme(legend.position="none")+theme(legend.title=element_blank())+scale_y_continuous(expand = c(0, 0), limits = c(0, 0.24))

