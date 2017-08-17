###### code to plot all of the sensitivity curves on the same figure
#### read in all the data frames
### process out
### organize into the same plot
library(ggplot2)
library(gridExtra)
library(grid)
library(reshape)
library(wesanderson)

#### read in all the data frames
OO=read.csv("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_Sensitivity_Hindcast/EcoCast-Sensitivity-Hindcast-Analyses/analysis_DFs/OO.csv")
LOO=read.csv("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_Sensitivity_Hindcast/EcoCast-Sensitivity-Hindcast-Analyses/analysis_DFs/LOO.csv")
lagged=read.csv("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_Sensitivity_Hindcast/EcoCast-Sensitivity-Hindcast-Analyses/analysis_DFs/lagged.csv")
# lagged_raw=read.csv("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_Sensitivity_Hindcast/EcoCast-Sensitivity-Hindcast-Analyses/analysis_DFs/lagged_envt_vars.csv")
# lagged_lbst=read.csv("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_Sensitivity_Hindcast/EcoCast-Sensitivity-Hindcast-Analyses/analysis_DFs/lagged_lbst.csv")
# lagged_raw_UnS=read.csv("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_Sensitivity_Hindcast/EcoCast-Sensitivity-Hindcast-Analyses/analysis_DFs/lagged_envt_vars_unstandardized.csv")

### process out
################ LAGGED OFFICIAL OUTPUT SENSITIVITY ################
OO=OO[complete.cases(OO),]
OO=OO[,c(5:10)]
a=melt(OO,id="t.minus")
OO_means=cast(a,t.minus~variable,c(mean,sd))

##make %
OO_means$p.GT.1=OO_means$p.GT.1_mean*100
OO_means$p.GT.25=OO_means$p.GT.25_mean*100
OO_means$p.GT.5=OO_means$p.GT.5_mean*100
OO_means$upper=OO_means$s.mean_mean+OO_means$s.mean_sd
OO_means$lower=OO_means$s.mean_mean-OO_means$s.mean_sd
OO_means$s.mean=OO_means$s.mean_mean
OO_means$s.SD=OO_means$s.SD_mean
OO_means$p.GT.1_upper=OO_means$p.GT.1+(OO_means$p.GT.1_sd*100)
OO_means$p.GT.1_lower=OO_means$p.GT.1-(OO_means$p.GT.1_sd*100)
OO_means$p.GT.25_upper=OO_means$p.GT.25+(OO_means$p.GT.25_sd*100)
OO_means$p.GT.25_lower=OO_means$p.GT.25-(OO_means$p.GT.25_sd*100)

################ LEAVE ONE OUT ANALYSIS SENSITIVITY ################
LOO=LOO[,c(5:10)]
a=melt(LOO,id="missing_var")
LOO_means=cast(a,missing_var~variable,c(mean,sd))

##make %
LOO_means$p.GT.1=LOO_means$p.GT.1_mean*100
LOO_means$p.GT.25=LOO_means$p.GT.25_mean*100
LOO_means$p.GT.5=LOO_means$p.GT.5_mean*100

LOO_means$upper=LOO_means$s.mean_mean+LOO_means$s.mean_sd
LOO_means$lower=LOO_means$s.mean_mean-LOO_means$s.mean_sd
LOO_means$s.mean=LOO_means$s.mean_mean
LOO_means$s.SD=LOO_means$s.SD_mean
LOO_means$p.GT.1_upper=LOO_means$p.GT.1+(LOO_means$p.GT.1_sd*100)
LOO_means$p.GT.1_lower=LOO_means$p.GT.1-(LOO_means$p.GT.1_sd*100)
LOO_means$p.GT.25_upper=LOO_means$p.GT.25+(LOO_means$p.GT.25_sd*100)
LOO_means$p.GT.25_lower=LOO_means$p.GT.25-(LOO_means$p.GT.25_sd*100)

LOO_means$psuedo_lag=seq(3,27,by=3)

################ LAGGED VARIABLE SENSITIVITY ################
lagged=lagged[,c(5:11)]
lagged$p.GT.1=lagged$p.GT.1*100
lagged$p.GT.25=lagged$p.GT.25*100
lagged$p.GT.5=lagged$p.GT.5*100

a=melt(lagged,id=c("missing_var","lag"))

means=cast(a,lag~missing_var,c(mean,sd),subset=variable=="s.mean")
lagged_means_s.mean1=melt(means,id=c("missing_var","lag"))
lagged_means_s.mean=lagged_means_s.mean1[lagged_means_s.mean1$result_variable=="mean",]
lagged_sd_s.mean=lagged_means_s.mean1[lagged_means_s.mean1$result_variable=="sd",]
lagged_means_s.mean$upper=lagged_means_s.mean$value+lagged_sd_s.mean$value
lagged_means_s.mean$lower=lagged_means_s.mean$value-lagged_sd_s.mean$value
lagged_means_s.mean$lower=ifelse(lagged_means_s.mean$lower<0,0,lagged_means_s.mean$lower)


means=cast(a,missing_var~lag,c(mean,sd),subset=variable=="s.SD")
lagged_means_s.SD1=melt(means,id=c("missing_var","lag"))
lagged_means_s.SD=lagged_means_s.SD1[lagged_means_s.SD1$result_variable=="mean",]
lagged_sd_s.SD=lagged_means_s.SD1[lagged_means_s.SD1$result_variable=="sd",]
lagged_means_s.SD$upper=lagged_means_s.SD$value+lagged_sd_s.SD$value
lagged_means_s.SD$lower=lagged_means_s.SD$value-lagged_sd_s.SD$value
lagged_means_s.SD$lower=ifelse(lagged_means_s.SD$lower<0,0,lagged_means_s.SD$lower)

# means=cast(a,missing_var~lag,mean,subset=variable=="p.GT.5")
# lagged_means_p.GT.5a=melt(means,id=c("missing_var","lag"))

means=cast(a,missing_var~lag,c(mean,sd),subset=variable=="p.GT.1") ############# ERRORS GALORE. THESE RESULTS ARE WAY TOO HIGH
lagged_means_p.GT.1a=melt(means,id=c("missing_var","lag"))
lagged_means_p.GT.1=lagged_means_p.GT.1a[lagged_means_p.GT.1a$result_variable=="mean",]
lagged_sd_p.GT.1=lagged_means_p.GT.1a[lagged_means_p.GT.1a$result_variable=="sd",]
lagged_means_p.GT.1$upper=lagged_means_p.GT.1$value+lagged_sd_p.GT.1$value
lagged_means_p.GT.1$lower=lagged_means_p.GT.1$value-lagged_sd_p.GT.1$value
lagged_means_p.GT.1$lower=ifelse(lagged_means_p.GT.1$lower<0,0,lagged_means_p.GT.1$lower)

means=cast(a,missing_var~lag,c(mean,sd),subset=variable=="p.GT.25")
lagged_means_p.GT.25a=melt(means,id=c("missing_var","lag"))
lagged_means_p.GT.25=lagged_means_p.GT.25a[lagged_means_p.GT.25a$result_variable=="mean",]
lagged_sd_p.GT.25=lagged_means_p.GT.25a[lagged_means_p.GT.25a$result_variable=="sd",]
lagged_means_p.GT.25$upper=lagged_means_p.GT.25$value+lagged_sd_p.GT.25$value
lagged_means_p.GT.25$lower=lagged_means_p.GT.25$value-lagged_sd_p.GT.25$value
lagged_means_p.GT.25$lower=ifelse(lagged_means_p.GT.25$lower<0,0,lagged_means_p.GT.25$lower)

###plotting
########### --------------------> Mean per pixel difference from zero lag
pd <- position_dodge(0.1) # move them .05 to the left and right

LOO_means=LOO_means[order(LOO_means[,"s.mean"]),]
LOO_means$psuedo_lag=seq(3,27,by=3)
master=ggplot() + geom_col(data=LOO_means, aes(psuedo_lag, s.mean)) + expand_limits(y=0)+geom_text(data=LOO_means,aes(x=psuedo_lag, y=s.mean,label=missing_var),vjust=-1,size=2.2)+geom_errorbar(data=LOO_means,aes(x=psuedo_lag,ymin=lower, ymax=upper), width = 0.6,position=pd)+
  geom_line(data=OO_means,aes(x=t.minus, y=s.mean,color="OO"))+ geom_point(data=OO_means,aes(x=t.minus, y=s.mean),color="black")+ expand_limits(y=0)+geom_ribbon(data=OO_means,aes(x=t.minus,ymin=lower, ymax=upper,fill="OO"),alpha=0.2)+ #+ geom_text(data=OO_means,aes(x=t.minus, y=s.mean,label=t.minus),hjust=2)+
  geom_line(data=lagged_means_s.mean,aes(lag, value,group=missing_var,color=missing_var))+ geom_point(data=lagged_means_s.mean,aes(lag, value,color=missing_var))+geom_ribbon(data=lagged_means_s.mean,aes(x=lag,ymin=lower, ymax=upper,fill=missing_var),alpha=0.2)#+geom_text(data=lagged_means_s.mean,aes(lag, value,label=lag),hjust=2)+ expand_limits(y=c(0,.08))
final=master+labs(x="Number of days lagged (lines only)")+labs(y="Mean per pixel difference from official output +/- 1sd")+theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=15))+
  scale_fill_manual("",values=c("OO"="blue","CHLA"="green3","EKE"="red3","SLA"="maroon3","SST"="turquoise3","YWIND"="orange3"))+scale_color_manual("",values=c("OO"="blue","CHLA"="green3","EKE"="red3","SLA"="maroon3","SST"="turquoise3","YWIND"="orange3"))+theme(legend.position="none",legend.key = element_blank())

master=ggplot()+ geom_col(data=LOO_means, aes(psuedo_lag,s.mean)) + expand_limits(y=0)+geom_text(data=LOO_means,aes(x=psuedo_lag, y=s.mean,label=missing_var),vjust=-1,size=2.2)+ expand_limits(y=c(0,.08)) + 
  geom_line(data=OO_means,aes(x=t.minus, y=s.mean,color="OO"))+ geom_point(data=OO_means,aes(x=t.minus, y=s.mean),color="black")+ expand_limits(y=0) + geom_text(data=OO_means,aes(x=t.minus, y=s.mean,label=t.minus),hjust=2)+
  geom_line(data=lagged_means_s.mean,aes(lag, value,group=missing_var,color=missing_var))+ geom_point(data=lagged_means_s.mean,aes(lag, value,color=missing_var)) +geom_text(data=lagged_means_s.mean,aes(lag, value,label=lag),hjust=2)+ expand_limits(y=c(0,.08))
final_lines=master+labs(x="Number of days lagged (lines only)")+labs(y="Mean per pixel difference from official output")+theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=15))+ 
  scale_color_manual("",values=c("OO"="blue","CHLA"="green3","EKE"="red3","SLA"="maroon3","SST"="turquoise3","YWIND"="orange3"))+theme(legend.position="none",legend.key = element_blank())+coord_cartesian(ylim = c(0,.08))

png("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_Sensitivity_Hindcast/EcoCast-Sensitivity-Hindcast-Analyses/analysis_DFs/s.means_error.png",width=1100,height=600,units='px',pointsize=100)
grid.arrange(final,final_lines,ncol=2,top=textGrob("Mean per pixel difference from official output, all analyses",gp=gpar(fontsize=20)))
dev.off()

#####
########### --------------------> % of pixels with > .1 difference from no missing variables
LOO_means=LOO_means[order(LOO_means[,"p.GT.1"]),]
LOO_means$psuedo_lag=seq(3,27,by=3)
master=ggplot() + geom_col(data=LOO_means, aes(psuedo_lag,p.GT.1)) + expand_limits(y=0)+geom_text(data=LOO_means,aes(x=psuedo_lag, y=p.GT.1,label=missing_var),vjust=-1,size=2.2)+geom_errorbar(data=LOO_means,aes(x=psuedo_lag,ymin=p.GT.1_lower, ymax=p.GT.1_upper), width = 0.6,position=pd)+
  geom_line(data=OO_means,aes(x=t.minus, y=p.GT.1,color="OO"))+ geom_point(data=OO_means,aes(x=t.minus, y=p.GT.1),color="black")+ expand_limits(y=0)+geom_ribbon(data=OO_means,aes(x=t.minus,ymin=p.GT.1_lower, ymax=p.GT.1_upper,fill="OO"),alpha=0.2) + #+geom_text(data=OO_means,aes(x=t.minus, y=s.mean,label=t.minus),hjust=2)
  geom_line(data=lagged_means_p.GT.1,aes(lag, value,group=missing_var,color=missing_var))+ geom_point(data=lagged_means_p.GT.1,aes(lag, value,color=missing_var))+geom_ribbon(data=lagged_means_p.GT.1,aes(x=lag,ymin=lower, ymax=upper,fill=missing_var),alpha=0.2) #+geom_text(data=lagged_means_s.mean,aes(lag, value,label=lag),hjust=2)+ expand_limits(y=0)
final=master+labs(x="Number of days lagged (lines only)")+labs(y="% of pixels with > .1 difference from official output +/- 1sd")+theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=15))+ 
  scale_fill_manual("",values=c("OO"="blue","CHLA"="green3","EKE"="red3","SLA"="maroon3","SST"="turquoise3","YWIND"="orange3"))+scale_color_manual("",values=c("OO"="blue","CHLA"="green3","EKE"="red3","SLA"="maroon3","SST"="turquoise3","YWIND"="orange3"))+theme(legend.position="none",legend.key = element_blank())

master=ggplot()+ geom_col(data=LOO_means, aes(psuedo_lag,p.GT.1)) + expand_limits(y=0)+geom_text(data=LOO_means,aes(x=psuedo_lag, y=p.GT.1,label=missing_var),vjust=-1,size=2.2)+ expand_limits(y=c(0,30)) + 
  geom_line(data=OO_means,aes(x=t.minus, y=p.GT.1),color="blue")+ geom_point(data=OO_means,aes(x=t.minus, y=p.GT.1),color="black")+ expand_limits(y=0) + geom_text(data=OO_means,aes(x=t.minus, y=p.GT.1,label=t.minus),hjust=2)+
  geom_line(data=lagged_means_p.GT.1,aes(lag, value,color=missing_var))+ geom_point(data=lagged_means_p.GT.1,aes(lag, value,color=missing_var)) +geom_text(data=lagged_means_p.GT.1,aes(lag, value,label=lag),hjust=2)+ expand_limits(y=c(0,30))
final_lines=master+labs(x="Number of days lagged (lines only)")+labs(y="% of pixels with > .1 difference from official output")+theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=15))+ theme(legend.position="none",legend.key = element_blank())+coord_cartesian(ylim = c(0,30))

png("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_Sensitivity_Hindcast/EcoCast-Sensitivity-Hindcast-Analyses/analysis_DFs/p.GT.1_error.png",width=1100,height=600,units='px',pointsize=100)
grid.arrange(final,final_lines,ncol=2,top=textGrob("% of pixels with > .1 difference from official output, all analyses",gp=gpar(fontsize=20)))
dev.off()

#####
########### --------------------> % of pixels with > .25 difference from no missing variables
LOO_means=LOO_means[order(LOO_means[,"p.GT.25"]),]
LOO_means$psuedo_lag=seq(3,27,by=3)
master=ggplot() + geom_col(data=LOO_means, aes(psuedo_lag,p.GT.25)) + expand_limits(y=0)+geom_text(data=LOO_means,aes(x=psuedo_lag, y=p.GT.25,label=missing_var),vjust=-1,size=2.2)+geom_errorbar(data=LOO_means,aes(x=psuedo_lag,ymin=p.GT.25_lower, ymax=p.GT.25_upper), width = 0.6,position=pd)+
  geom_line(data=OO_means,aes(x=t.minus, y=p.GT.25,color="OO"))+ geom_point(data=OO_means,aes(x=t.minus, y=p.GT.25),color="black")+ expand_limits(y=0)+geom_ribbon(data=OO_means,aes(x=t.minus,ymin=p.GT.25_lower, ymax=p.GT.25_upper,fill="OO"),alpha=0.2) + #+geom_text(data=OO_means,aes(x=t.minus, y=s.mean,label=t.minus),hjust=2)
  geom_line(data=lagged_means_p.GT.25,aes(lag, value,group=missing_var,color=missing_var))+ geom_point(data=lagged_means_p.GT.25,aes(lag, value,color=missing_var))+geom_ribbon(data=lagged_means_p.GT.25,aes(x=lag,ymin=lower, ymax=upper,fill=missing_var),alpha=0.2) #+geom_text(data=lagged_means_s.mean,aes(lag, value,label=lag),hjust=2)+ expand_limits(y=0)
final=master+labs(x="Number of days lagged (lines only)")+labs(y="% of pixels with > .25 difference from official output +/- 1sd")+theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=15))+ 
  scale_fill_manual("",values=c("OO"="blue","CHLA"="green3","EKE"="red3","SLA"="maroon3","SST"="turquoise3","YWIND"="orange3"))+scale_color_manual("",values=c("OO"="blue","CHLA"="green3","EKE"="red3","SLA"="maroon3","SST"="turquoise3","YWIND"="orange3"))+theme(legend.position="none",legend.key = element_blank())

master=ggplot() + geom_col(data=LOO_means, aes(psuedo_lag,p.GT.25)) + expand_limits(y=0)+geom_text(data=LOO_means,aes(x=psuedo_lag, y=p.GT.25,label=missing_var),vjust=-1,size=2.2)+ expand_limits(y=c(0,10))+
  geom_line(data=OO_means,aes(x=t.minus, y=p.GT.25,color="OO"))+ geom_point(data=OO_means,aes(x=t.minus, y=p.GT.25),color="black")+ expand_limits(y=c(0,4)) + geom_text(data=OO_means,aes(x=t.minus, y=p.GT.25,label=t.minus),hjust=2)+
  geom_line(data=lagged_means_p.GT.25,aes(lag, value,group=missing_var,color=missing_var))+ geom_point(data=lagged_means_p.GT.25,aes(lag, value,color=missing_var)) +geom_text(data=lagged_means_p.GT.25,aes(lag, value,label=lag),hjust=2)+ expand_limits(y=c(0,10))
final_lines=master+labs(x="Number of days lagged (lines only)")+labs(y="% of pixels with > .25 difference from official output")+theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=15))+ 
  scale_color_manual("",values=c("OO"="blue","CHLA"="green3","EKE"="red3","SLA"="maroon3","SST"="turquoise3","YWIND"="orange3"))+theme(legend.position="none",legend.key = element_blank())+coord_cartesian(ylim = c(0,10)) 

png("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_Sensitivity_Hindcast/EcoCast-Sensitivity-Hindcast-Analyses/analysis_DFs/p.GT.25_error.png",width=1100,height=600,units='px',pointsize=100)
grid.arrange(final,final_lines,ncol=2,top=textGrob("% of pixels with > .25 difference from official output, all analyses",gp=gpar(fontsize=20)))
dev.off()


