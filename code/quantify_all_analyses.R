###### code to plot all of the sensitivity curves on the same figure
#### read in all the data frames
### process out
### organize into the same plot
library(ggplot2)
library(gridExtra)
library(grid)
library(reshape)

#### read in all the data frames
OO=read.csv("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_Sensitivity_Hindcast/EcoCast-Sensitivity-Hindcast-Analyses/analysis_DFs/OO.csv")
LOO=read.csv("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_Sensitivity_Hindcast/EcoCast-Sensitivity-Hindcast-Analyses/analysis_DFs/LOO.csv")
lagged=read.csv("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_Sensitivity_Hindcast/EcoCast-Sensitivity-Hindcast-Analyses/analysis_DFs/lagged.csv")

### process out
################ LAGGED OFFICIAL OUTPUT SENSITIVITY ################
OO=OO[complete.cases(OO),]
OO=OO[,c(5:10)]
a=melt(OO,id="t.minus")
OO_means=cast(a,t.minus~variable,mean)

##make %
OO_means$p.GT.1=OO_means$p.GT.1*100
OO_means$p.GT.25=OO_means$p.GT.25*100
OO_means$p.GT.5=OO_means$p.GT.5*100

################ LEAVE ONE OUT ANALYSIS SENSITIVITY ################
LOO=LOO[,c(5:10)]
a=melt(LOO,id="missing_var")
LOO_means=cast(a,missing_var~variable,mean)

##make %
LOO_means$p.GT.1=LOO_means$p.GT.1*100
LOO_means$p.GT.25=LOO_means$p.GT.25*100
LOO_means$p.GT.5=LOO_means$p.GT.5*100

LOO_means$psuedo_lag=seq(3,27,by=3)

################ LAGGED VARIABLE SENSITIVITY ################
lagged=lagged[,c(5:11)]
lagged$p.GT.1=lagged$p.GT.1*100
lagged$p.GT.25=lagged$p.GT.25*100
lagged$p.GT.5=lagged$p.GT.5*100

a=melt(lagged,id=c("missing_var","lag"))

means=cast(a,lag~missing_var,mean,subset=variable=="s.mean")
lagged_means_s.mean=melt(means,id=c("missing_var","lag"))
means=cast(a,missing_var~lag,mean,subset=variable=="s.SD")
lagged_means_s.SD=melt(means,id=c("missing_var","lag"))
means=cast(a,missing_var~lag,mean,subset=variable=="p.GT.5")
lagged_means_p.GT.5=melt(means,id=c("missing_var","lag"))
means=cast(a,missing_var~lag,mean,subset=variable=="p.GT.1")
lagged_means_p.GT.1=melt(means,id=c("missing_var","lag"))

###plotting
########### --------------------> Mean per pixel difference from zero lag
LOO_means=LOO_means[order(LOO_means[,2]),]
LOO_means$psuedo_lag=seq(3,27,by=3)
master=ggplot() + geom_col(data=LOO_means, aes(psuedo_lag, s.mean)) + expand_limits(y=0)+geom_text(data=LOO_means,aes(x=psuedo_lag, y=s.mean,label=missing_var),vjust=-1,size=2.2)+
geom_line(data=OO_means,aes(x=t.minus, y=s.mean),color="blue")+ geom_point(data=OO_means,aes(x=t.minus, y=s.mean),color="black")+ expand_limits(y=0) + #+geom_text(data=OO_means,aes(x=t.minus, y=s.mean,label=t.minus),hjust=2)
geom_line(data=lagged_means_s.mean,aes(lag, value,color=missing_var))+ geom_point(data=lagged_means_s.mean,aes(lag, value,color=missing_var)) #+geom_text(data=lagged_means_s.mean,aes(lag, value,label=lag),hjust=2)+ expand_limits(y=0)
final=master+labs(x="Number of days lagged (lines only)")+labs(y="Mean per pixel difference from official output")+theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=15))+ theme(legend.position="none",legend.key = element_blank())

master=ggplot() + geom_line(data=OO_means,aes(x=t.minus, y=s.mean),color="blue")+ geom_point(data=OO_means,aes(x=t.minus, y=s.mean),color="black")+ expand_limits(y=0) + geom_text(data=OO_means,aes(x=t.minus, y=s.mean,label=t.minus),hjust=2)+
geom_line(data=lagged_means_s.mean,aes(lag, value,color=missing_var))+ geom_point(data=lagged_means_s.mean,aes(lag, value,color=missing_var)) +geom_text(data=lagged_means_s.mean,aes(lag, value,label=lag),hjust=2)+ expand_limits(y=0)
final_lines=master+labs(x="Number of days lagged")+labs(y="Mean per pixel difference from official output")+theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=15))+ theme(legend.position="none",legend.key = element_blank())

png("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_Sensitivity_Hindcast/EcoCast-Sensitivity-Hindcast-Analyses/analysis_DFs/s.means.png",width=1100,height=600,units='px',pointsize=100)
grid.arrange(final,final_lines,ncol=2,top=textGrob("Mean per pixel difference from official output, all analyses",gp=gpar(fontsize=20)))
dev.off()

########### --------------------> SD per pixel difference from zero lag
LOO_means=LOO_means[order(LOO_means[,3]),]
LOO_means$psuedo_lag=seq(3,27,by=3)
master=ggplot() + geom_col(data=LOO_means, aes(psuedo_lag, s.SD)) + expand_limits(y=0)+geom_text(data=LOO_means,aes(x=psuedo_lag, y=s.SD,label=missing_var),vjust=-1,size=2.2)+
  geom_line(data=OO_means,aes(x=t.minus, y=s.SD),color="blue")+ geom_point(data=OO_means,aes(x=t.minus, y=s.SD),color="black")+ expand_limits(y=0) + #+geom_text(data=OO_means,aes(x=t.minus, y=s.mean,label=t.minus),hjust=2)
  geom_line(data=lagged_means_s.SD,aes(lag, value,color=missing_var))+ geom_point(data=lagged_means_s.SD,aes(lag, value,color=missing_var)) #+geom_text(data=lagged_means_s.mean,aes(lag, value,label=lag),hjust=2)+ expand_limits(y=0)
final=master+labs(x="Number of days lagged (lines only)")+labs(y="Standard deviation of per pixel difference from official output")+theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=15))+ theme(legend.position="none",legend.key = element_blank())

master=ggplot() + geom_line(data=OO_means,aes(x=t.minus, y=s.SD),color="blue")+ geom_point(data=OO_means,aes(x=t.minus, y=s.SD),color="black")+ expand_limits(y=0) + geom_text(data=OO_means,aes(x=t.minus, y=s.SD,label=t.minus),hjust=2)+
  geom_line(data=lagged_means_s.SD,aes(lag, value,color=missing_var))+ geom_point(data=lagged_means_s.SD,aes(lag, value,color=missing_var)) +geom_text(data=lagged_means_s.SD,aes(lag, value,label=lag),hjust=2)+ expand_limits(y=0)
final_lines=master+labs(x="Number of days lagged")+labs(y="Standard deviation of per pixel difference from official output")+theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=15))+ theme(legend.position="none",legend.key = element_blank())

png("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_Sensitivity_Hindcast/EcoCast-Sensitivity-Hindcast-Analyses/analysis_DFs/s.SD.png",width=1100,height=600,units='px',pointsize=100)
grid.arrange(final,final_lines,ncol=2,top=textGrob("Standard deviation of per pixel difference from official output, all analyses",gp=gpar(fontsize=20)))
dev.off()

########### --------------------> % of pixels with > .1 difference from no missing variables
LOO_means=LOO_means[order(LOO_means[,6]),]
LOO_means$psuedo_lag=seq(3,27,by=3)
master=ggplot() + geom_col(data=LOO_means, aes(psuedo_lag,p.GT.1)) + expand_limits(y=0)+geom_text(data=LOO_means,aes(x=psuedo_lag, y=p.GT.1,label=missing_var),vjust=-1,size=2.2)+
  geom_line(data=OO_means,aes(x=t.minus, y=p.GT.1),color="blue")+ geom_point(data=OO_means,aes(x=t.minus, y=p.GT.1),color="black")+ expand_limits(y=0) + #+geom_text(data=OO_means,aes(x=t.minus, y=s.mean,label=t.minus),hjust=2)
  geom_line(data=lagged_means_p.GT.1,aes(lag, value,color=missing_var))+ geom_point(data=lagged_means_p.GT.1,aes(lag, value,color=missing_var)) #+geom_text(data=lagged_means_s.mean,aes(lag, value,label=lag),hjust=2)+ expand_limits(y=0)
final=master+labs(x="Number of days lagged (lines only)")+labs(y="% of pixels with > .1 difference from official output")+theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=15))+ theme(legend.position="none",legend.key = element_blank())

master=ggplot() + geom_line(data=OO_means,aes(x=t.minus, y=p.GT.1),color="blue")+ geom_point(data=OO_means,aes(x=t.minus, y=p.GT.1),color="black")+ expand_limits(y=0) + geom_text(data=OO_means,aes(x=t.minus, y=p.GT.1,label=t.minus),hjust=2)+
  geom_line(data=lagged_means_p.GT.1,aes(lag, value,color=missing_var))+ geom_point(data=lagged_means_p.GT.1,aes(lag, value,color=missing_var)) +geom_text(data=lagged_means_p.GT.1,aes(lag, value,label=lag),hjust=2)+ expand_limits(y=0)
final_lines=master+labs(x="Number of days lagged")+labs(y="% of pixels with > .1 difference from official output")+theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=15))+ theme(legend.position="none",legend.key = element_blank())

png("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_Sensitivity_Hindcast/EcoCast-Sensitivity-Hindcast-Analyses/analysis_DFs/p.GT.1.png",width=1100,height=600,units='px',pointsize=100)
grid.arrange(final,final_lines,ncol=2,top=textGrob("% of pixels with > .1 difference from official output, all analyses",gp=gpar(fontsize=20)))
dev.off()

########### --------------------> % of pixels with > .5 difference from no missing variables
LOO_means=LOO_means[order(LOO_means[,4]),]
LOO_means$psuedo_lag=seq(3,27,by=3)
master=ggplot() + geom_col(data=LOO_means, aes(psuedo_lag,p.GT.5)) + expand_limits(y=0)+geom_text(data=LOO_means,aes(x=psuedo_lag, y=p.GT.5,label=missing_var),vjust=-1,size=2.2)+
  geom_line(data=OO_means,aes(x=t.minus, y=p.GT.5),color="blue")+ geom_point(data=OO_means,aes(x=t.minus, y=p.GT.5),color="black")+ expand_limits(y=0) + #+geom_text(data=OO_means,aes(x=t.minus, y=s.mean,label=t.minus),hjust=2)
  geom_line(data=lagged_means_p.GT.5,aes(lag, value,color=missing_var))+ geom_point(data=lagged_means_p.GT.5,aes(lag, value,color=missing_var)) #+geom_text(data=lagged_means_s.mean,aes(lag, value,label=lag),hjust=2)+ expand_limits(y=0)
final=master+labs(x="Number of days lagged (lines only)")+labs(y="% of pixels with > .5 difference from official output")+theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=15))+ theme(legend.position="none",legend.key = element_blank())

master=ggplot() + geom_col(data=LOO_means, aes(psuedo_lag,p.GT.5)) + expand_limits(y=0)+geom_text(data=LOO_means,aes(x=psuedo_lag, y=p.GT.5,label=missing_var),vjust=-1,size=2.2)+ expand_limits(y=c(0,4))+
  geom_line(data=OO_means,aes(x=t.minus, y=p.GT.5),color="blue")+ geom_point(data=OO_means,aes(x=t.minus, y=p.GT.5),color="black")+ expand_limits(y=c(0,4)) + geom_text(data=OO_means,aes(x=t.minus, y=p.GT.5,label=t.minus),hjust=2)+
  geom_line(data=lagged_means_p.GT.5,aes(lag, value,color=missing_var))+ geom_point(data=lagged_means_p.GT.5,aes(lag, value,color=missing_var)) +geom_text(data=lagged_means_p.GT.5,aes(lag, value,label=lag),hjust=2)+ expand_limits(y=c(0,4))
final_lines=master+labs(x="Number of days lagged")+labs(y="% of pixels with > .5 difference from official output")+theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=15))+ theme(legend.position="none",legend.key = element_blank())+coord_cartesian(ylim = c(0,4)) 

png("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_Sensitivity_Hindcast/EcoCast-Sensitivity-Hindcast-Analyses/analysis_DFs/p.GT.5.png",width=1100,height=600,units='px',pointsize=100)
grid.arrange(final,final_lines,ncol=2,top=textGrob("% of pixels with > .5 difference from official output, all analyses",gp=gpar(fontsize=20)))
dev.off()


#### combining to assess rank order
OO_means$analysis="OO"
OO_means$sensitivity=OO_means$t.minus
OO_means=OO_means[,2:ncol(OO_means)]
OO_means=OO_means[,c(1,2,3,5,6,7)]
LOO_means$analysis="LOO"
LOO_means$sensitivity=LOO_means$missing_var
LOO_means=LOO_means[,c(2:6,8,9)]
LOO_means=LOO_means[,c(1,2,3,5,6,7)]
lagged_means_s.mean$metric="s.mean"
lagged_means_s.SD$metric="s.SD"
lagged_means_p.GT.5$metric="p.GT.5"
lagged_means_p.GT.1$metric="p.GT.1"
lagged_all=rbind(lagged_means_s.mean,lagged_means_s.SD,lagged_means_p.GT.5,lagged_means_p.GT.1)
lagged_all$var_lag=paste0(lagged_all$missing_var,"_",lagged_all$lag)
lagged_all$analysis="lagged"
lagged_pivot=cast(lagged_all,var_lag~metric)
lagged_pivot$analysis="lagged"
lagged_pivot$sensitivity=lagged_pivot$var_lag
lagged_pivot=lagged_pivot[,c(4,5,3,2,6,7)]
all=rbind(OO_means,LOO_means,lagged_pivot)
all$lable=paste0(all$analysis,"_",all$sensitivity)

write.csv(all,"/Volumes/SeaGate/EcoCast_HW/EcoCastGit_Sensitivity_Hindcast/EcoCast-Sensitivity-Hindcast-Analyses/analysis_DFs/summary.csv")



