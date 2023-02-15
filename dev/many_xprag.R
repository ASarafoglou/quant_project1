############################# ORIGINAL EXPERIMENT #############################

############################# EXCLUDE PERTICIPANTS ############################
exp1.replication.trials$response=ifelse(exp1.replication.trials$response=="true",1,0)
exp1.replication.trials=subset.data.frame(exp1.replication.trials, exp1.replication.trials$workerid!="0")
exp1.replication.trials=subset.data.frame(exp1.replication.trials, exp1.replication.trials$workerid!="1")
exp1.replication.trials=subset.data.frame(exp1.replication.trials, exp1.replication.trials$workerid!="5")
exp1.replication.trials=subset.data.frame(exp1.replication.trials, exp1.replication.trials$workerid!="18")
exp1.replication.trials=subset.data.frame(exp1.replication.trials, exp1.replication.trials$workerid!="32")
exp1.replication.trials=subset.data.frame(exp1.replication.trials, exp1.replication.trials$workerid!="49")
exp1.replication.trials=subset.data.frame(exp1.replication.trials, exp1.replication.trials$workerid!="52")
exp1.replication.trials=subset.data.frame(exp1.replication.trials, exp1.replication.trials$workerid!="53")
exp1.replication.trials=subset.data.frame(exp1.replication.trials, exp1.replication.trials$workerid!="58")
exp1.replication.trials=subset.data.frame(exp1.replication.trials, exp1.replication.trials$workerid!="62")
exp1.replication.trials=subset.data.frame(exp1.replication.trials, exp1.replication.trials$workerid!="81")
exp1.replication.trials=subset.data.frame(exp1.replication.trials, exp1.replication.trials$workerid!="82")
exp1.replication.trials=subset.data.frame(exp1.replication.trials, exp1.replication.trials$workerid!="83")
exp1.replication.trials=subset.data.frame(exp1.replication.trials, exp1.replication.trials$workerid!="85")
exp1.replication.trials=subset.data.frame(exp1.replication.trials, exp1.replication.trials$workerid!="86")
exp1.replication.trials=subset.data.frame(exp1.replication.trials, exp1.replication.trials$workerid!="87")

############################ QUANT ANALYSIS ###########################
exp1.replication.trials=subset.data.frame(exp1.replication.trials, exp1.replication.trials$read_and_decide_time > 300)
rmany=subset.data.frame(exp1.replication.trials, exp1.replication.trials$quant=="\"Many\"")
rfew=subset.data.frame(exp1.replication.trials, exp1.replication.trials$quant=="\"Few\"")
rmost=subset.data.frame(exp1.replication.trials, exp1.replication.trials$quant=="\"Most\"")
rmth=subset.data.frame(exp1.replication.trials, exp1.replication.trials$quant=="\"More than half\"")
rfth=subset.data.frame(exp1.replication.trials, exp1.replication.trials$quant=="\"Fewer than half\"")

################# EXCULUDE TO SHORT AND SLOW OUTLIERS RESPONSES #################
######## MANY 
rmany_yes=subset.data.frame(rmany, rmany$response==1)
rcut_many_yes_1=mean(rmany_yes$read_and_decide_time)+2*sd(rmany_yes$read_and_decide_time)
rmany_yes_1=subset.data.frame(rmany_yes, rmany_yes$read_and_decide_time<rcut_many_yes_1)
rmany_no=subset.data.frame(rmany, rmany$response==0)
rcut_many_no_1=mean(rmany_no$read_and_decide_time)+2*sd(rmany_no$read_and_decide_time)
rmany_no_1=subset.data.frame(rmany_no, rmany_no$read_and_decide_time<rcut_many_no_1)
rmany2=rbind(rmany_no_1, rmany_yes_1)

######## MOST
rmost_yes=subset.data.frame(rmost, rmost$response==1)
rcut_most_yes_1=mean(rmost_yes$read_and_decide_time)+2*sd(rmost_yes$read_and_decide_time)
rmost_yes_1=subset.data.frame(rmost_yes, rmost_yes$read_and_decide_time<rcut_most_yes_1)
rmost_no=subset.data.frame(rmost, rmost$response==0)
rcut_most_no_1=mean(rmost_no$read_and_decide_time)+2*sd(rmost_no$read_and_decide_time)
rmost_no_1=subset.data.frame(rmost_no, rmost_no$read_and_decide_time<rcut_most_no_1)
rmost2=rbind(rmost_no_1, rmost_yes_1)

######## MTH
rmth_yes=subset.data.frame(rmth, rmth$response==1)
rcut_mth_yes_1=mean(rmth_yes$read_and_decide_time)+2*sd(rmth_yes$read_and_decide_time)
rmth_yes_1=subset.data.frame(rmth_yes, rmth_yes$read_and_decide_time<rcut_mth_yes_1)
rmth_no=subset.data.frame(rmth, rmth$response==0)
rcut_mth_no_1=mean(rmth_no$read_and_decide_time)+2*sd(rmth_no$read_and_decide_time)
rmth_no_1=subset.data.frame(rmth_no, rmth_no$read_and_decide_time<rcut_mth_no_1)
rmth2=rbind(rmth_no_1, rmth_yes_1)

######## FTH
rfth_yes=subset.data.frame(rfth, rfth$response==1)
rcut_fth_yes_1=mean(rfth_yes$read_and_decide_time)+2*sd(rfth_yes$read_and_decide_time)
rfth_yes_1=subset.data.frame(rfth_yes, rfth_yes$read_and_decide_time<rcut_fth_yes_1)
rfth_no=subset.data.frame(rfth, rfth$response==0)
rcut_fth_no_1=mean(rfth_no$read_and_decide_time)+2*sd(rfth_no$read_and_decide_time)
rfth_no_1=subset.data.frame(rfth_no, rfth_no$read_and_decide_time<rcut_fth_no_1)
rfth2=rbind(rfth_no_1, rfth_yes_1)

######## FEW
rfew_yes=subset.data.frame(rfew, rfew$response==1)
rcut_few_yes_1=mean(rfew_yes$read_and_decide_time)+2*sd(rfew_yes$read_and_decide_time)
rfew_yes_1=subset.data.frame(rfew_yes, rfew_yes$read_and_decide_time<rcut_few_yes_1)
rfew_no=subset.data.frame(rfew, rfew$response==0)
rcut_few_no_1=mean(rfew_no$read_and_decide_time)+2*sd(rfew_no$read_and_decide_time)
rfew_no_1=subset.data.frame(rfew_no, rfew_no$read_and_decide_time<rcut_few_no_1)
rfew2=rbind(rfew_no_1, rfew_yes_1)


rmany2$zpercent=scale(rmany2$percent)
rmost2$zpercent=scale(rmost2$percent)
rmth2$zpercent=scale(rmth2$percent)
rfth2$zpercent=scale(rfth2$percent)
rfew2$zpercent=scale(rfew2$percent)

############################# THRESHOLDS ESTIMATION ###############################
library(plyr)
library(dplyr)
model <- function(x){
  try(coef(nls(response ~ 1/(1+exp((xmid-percent)/scal)), data=x, 
               start=list(xmid=50,scal=4))))
}


fitted.linear.model <- dlply(rmany2, .(workerid), model)
x=fitted.linear.model[-(which(sapply(fitted.linear.model,is.character),arr.ind=TRUE))]
y=fitted.linear.model[(which(sapply(fitted.linear.model,is.character),arr.ind=TRUE))]
#print(names(x))
#[1] "1"  "3"  "6"  "7"  "9"  "11" "13" "14" "15" "16" "17" "20" "21" "22" "23" "24" "26" "27" "28"
#[20] "29" "30" "31" "32" "33" "35" "37" "38" "40" "41" "43" "46" "50" "55" "56" "57" "59" "63" "65"
#[39] "66" "67" "68" "69" "72" "73" "74" "75" "77" "80" "84"
#print(names(y))
#[1] "2"  "4"  "8"  "10" "12" "19" "25" "34" "36" "42" "44" "45" "47" "48" "51" "54" "60" "61" "64"
#[20] "70" "71" "78" "79" "88" "89"
df <- data.frame(Reduce(rbind, x))
df$workerid=names(x)
#colnames(df)

vector=as.vector(names(x))
certain <- rmany2[!rmany2$workerid %in% vector, ]

threshold <- function(n){
  (max(n$percent[which(n$response=="0")])+min(n$percent[which(n$response=="1")]))/2
}

#threshold(rmany2[rmany2$workerid=="29",])

certained.fitted =dlply(certain, .(workerid), threshold)
df_cert <- data.frame(Reduce(rbind, certained.fitted))
df_cert$workerid=names(y)
colnames(df_cert)[1] <- "xmid"
df_cert$scal=rep(c(0))
#colnames(df_cert)
#df_cert[,c("xmid", "scal", "workerid")]
df_all=rbind(df_cert, df[,c(1:3)])
#colnames(df_all)

#for (var in levels(rmany22$workerid)) {
  #dev.new()
  #myplot <- ggplot(rmany22[rmany22$workerid==var,], aes(percent, response)) + geom_point()
 # ggsave(myplot,filename=paste("myplot",var,".png",sep=""))
#}


#plot(rmost22$percent[which(rmost22$workerid=="8")],
   #  rmost22$response[which(rmost22$workerid=="8")])

hist(df_all$xmid)

#colnames(df_all)
#str(df_all)
df_all$workerid=as.numeric(df_all$workerid)
df_all_new=df_all[order(df_all[,2]),]
df_all$worerid[which(df_all$xmid>100)]

##### MOST ##########
fitted.linear.model.most <- dlply(rmost2, .(workerid), model)
x.most=fitted.linear.model.most[-(which(sapply(fitted.linear.model.most,is.character),arr.ind=TRUE))]
y.most=fitted.linear.model.most[(which(sapply(fitted.linear.model.most,is.character),arr.ind=TRUE))]
print(x.most)
df.most <- data.frame(Reduce(rbind, x.most))
df.most$workerid=names(x.most)

vector.most=as.vector(names(x.most))
certain.most <- rmost2[!rmost2$workerid %in% vector.most, ]

certained.fitted.most =dlply(certain.most, .(workerid), threshold)
df_cert.most <- data.frame(Reduce(rbind, certained.fitted.most))
df_cert.most$workerid=names(y.most)
colnames(df_cert.most)[1] <- "xmid"
df_cert.most$scal=rep(c(0))
df_cert.most[,c("xmid", "scal", "workerid")]
df_all.most=rbind(df_cert.most, df.most[,c(1:3)])


df_all.most$workerid=as.numeric(df_all.most$workerid)
df_all_new.most=df_all.most[order(df_all.most[,2]),]
df_all_new.most$workerid[which(df_all_new.most$xmid<0)]
df_all_new.most$workerid[which(df_all_new.most$xmid>100)]
#[1] 76

##### MTH ########
fitted.linear.model.mth <- dlply(rmth2, .(workerid), model)
x.mth=fitted.linear.model.mth[-(which(sapply(fitted.linear.model.mth,is.character),arr.ind=TRUE))]
y.mth=fitted.linear.model.mth[(which(sapply(fitted.linear.model.mth,is.character),arr.ind=TRUE))]
df.mth <- data.frame(Reduce(rbind, x.mth))
df.mth$workerid=names(x.mth)

vector.mth=as.vector(names(x.mth))
certain.mth <- rmth2[!rmth2$workerid %in% vector.mth, ]

certained.fitted.mth =dlply(certain.mth, .(workerid), threshold)
df_cert.mth <- data.frame(Reduce(rbind, certained.fitted.mth))
df_cert.mth$workerid=names(y.mth)
colnames(df_cert.mth)[1] <- "xmid"
df_cert.mth$scal=rep(c(0))
df_cert.mth[,c("xmid", "scal", "workerid")]
df_all.mth=rbind(df_cert.mth, df.mth[,c(1:3)])

df_all.mth$workerid=as.numeric(df_all.mth$workerid)
df_all_new.mth=df_all.mth[order(df_all.mth[,2]),]
df_all_new.mth$workerid[which(df_all_new.mth$xmid<0)]
#[1] 76 
df_all_new.mth$workerid[which(df_all_new.mth$xmid>100)]
hist(df_all_new.mth$xmid)

##### FEW ##########
library(plyr)
library(dplyr)
getInitial(response ~ SSlogis(percent, Asym, xmid, scal), data=rfew2)
model.few <- function(x){
  try(coef(nls(response ~ 1/(1+exp((xmid-percent)/scal)), data=x, 
               start=list(xmid=41,scal=-5))))
}
fitted.linear.model.few <- dlply(rfew2, .(workerid), model.few)
xfew=fitted.linear.model.few[-(which(sapply(fitted.linear.model.few,is.character),arr.ind=TRUE))]
yfew=fitted.linear.model.few[(which(sapply(fitted.linear.model.few,is.character),arr.ind=TRUE))]
df.few <- data.frame(Reduce(rbind, xfew))
df.few$workerid=names(xfew)

vector.few=as.vector(names(xfew))
certain.few <- rfew2[!rfew2$workerid %in% vector.few, ]

threshold.few <- function(n){
  (min(n$percent[which(n$response=="0")])+max(n$percent[which(n$response=="1")]))/2
}


certained.fitted.few =dlply(certain.few, .(workerid), threshold.few)
df_cert.few <- data.frame(Reduce(rbind, certained.fitted.few))
df_cert.few$workerid=names(yfew)
colnames(df_cert.few)[1] <- "xmid"
df_cert.few$scal=rep(c(0))
df_all.few=rbind(df_cert.few, df.few[,c(1:3)])

df_all.few$workerid=as.numeric(df_all.few$workerid)
df_all_new.few=df_all.few[order(df_all.few[,2]),]
df_all_new.few$workerid[which(df_all_new.few$xmid<0)]
#[1] 27 76
df_all_new.few$workerid[which(df_all_new.few$xmid>100)]

###### FTH ########
fitted.linear.model.fth <- dlply(rfth2, .(workerid), model.few)
xfth=fitted.linear.model.fth[-(which(sapply(fitted.linear.model.fth,is.character),arr.ind=TRUE))]
yfth=fitted.linear.model.fth[(which(sapply(fitted.linear.model.fth,is.character),arr.ind=TRUE))]
df.fth <- data.frame(Reduce(rbind, xfth))
df.fth$workerid=names(xfth)

vector.fth=as.vector(names(xfth))
certain.fth <- rfth2[!rfth2$workerid %in% vector.fth, ]

certained.fitted.fth =dlply(certain.fth, .(workerid), threshold.few)
df_cert.fth <- data.frame(Reduce(rbind, certained.fitted.fth))
df_cert.fth$workerid=names(yfth)
colnames(df_cert.fth)[1] <- "xmid"
df_cert.fth$scal=rep(c(0))
df_all.fth=rbind(df_cert.fth, df.fth[,c(1:3)])

df_all.fth$workerid=as.numeric(df_all.fth$workerid)
df_all_new.fth=df_all.fth[order(df_all.fth[,2]),]
df_all_new.fth$workerid[which(df_all_new.fth$xmid<0)]
df_all_new.fth$workerid[which(df_all_new.fth$xmid>100)]
#[1] 27 

####### PLOTS
df_all_new=cbind(df_all_new, quantifier="many")
df_all_new.few=cbind(df_all_new.few, quantifier="few")
df_all_new.fth=cbind(df_all_new.fth, quantifier="FTH")
df_all_new.most=cbind(df_all_new.most, quantifier="most")
df_all_new.mth=cbind(df_all_new.mth, quantifier="MTH")
df_all_new_plot=rbind(df_all_new, df_all_new.few, df_all_new.fth, df_all_new.most, df_all_new.mth)
df_all_new_plot=subset.data.frame(df_all_new_plot, df_all_new_plot$workerid!="27")
df_all_new_plot=subset.data.frame(df_all_new_plot, df_all_new_plot$workerid!="76")


ks.test(df_all_new_plot$xmid[which(df_all_new_plot$quantifier=="most")], df_all_new_plot$xmid[which(df_all_new_plot$quantifier=="MTH")])
ks.test(df_all_new_plot$xmid[which(df_all_new_plot$quantifier=="FTH")], df_all_new_plot$xmid[which(df_all_new_plot$quantifier=="MTH")])
ks.test(df_all_new_plot$xmid[which(df_all_new_plot$quantifier=="few")], df_all_new_plot$xmid[which(df_all_new_plot$quantifier=="many")])
ks.test(df_all_new_plot$xmid[which(df_all_new_plot$quantifier=="most")], df_all_new_plot$xmid[which(df_all_new_plot$quantifier=="many")])

bartlett.test(df_all_new_plot$xmid[which(rrdf_all_new_plot$quantifier=="most")], df_all_new_plot$xmid[which(rrdf_all_new_plot$quantifier=="MTH")])
levene.test(df_all_new_plot$xmid[which(df_all_new_plot$quantifier=="many")], df_all_new_plot$xmid[which(df_all_new_plot$quantifier=="MTH")])


library(moments)
anscombe.test(df_all_new_plot$xmid[which(df_all_new_plot$quantifier=="most")])
anscombe.test(df_all_new_plot$xmid[which(df_all_new_plot$quantifier=="MTH")])


t.test(df_all_new_plot$xmid[which(df_all_new_plot$quantifier=="most")], df_all_new_plot$xmid[which(df_all_new_plot$quantifier=="MTH")])
t.test(df_all_new_plot$xmid[which(df_all_new_plot$quantifier=="FTH")], df_all_new_plot$xmid[which(df_all_new_plot$quantifier=="MTH")])
t.test(df_all_new_plot$xmid[which(df_all_new_plot$quantifier=="few")], df_all_new_plot$xmid[which(df_all_new_plot$quantifier=="many")])
t.test(df_all_new_plot$xmid[which(df_all_new_plot$quantifier=="most")], df_all_new_plot$xmid[which(df_all_new_plot$quantifier=="many")])
t.test(df_all_new_plot$xmid[which(df_all_new_plot$quantifier=="most")],mu=50)

library(ggplot2)
library(scales)
library(plyr)
library(dplyr)
rmu <- ddply(df_all_new_plot, "quantifier", summarise, grp.mean=mean(xmid))
head(rmu)


ggplot(df_all_new_plot, aes(x=xmid, fill=quant)) +
  geom_histogram(binwidth=3, alpha=.5, position="identity")+
  #scale_y_continuous(labels = percent)+
  geom_vline(data=rmu, aes(xintercept=grp.mean, color=quant),
             linetype="dashed")+
  scale_x_continuous(name = "Individual threshold",
                     breaks = seq(0, 80, 10),
                     limits=c(10, 70)) +
  theme_light()+
  facet_wrap(.~quant)

######### CORRELATION BETWEEN TRHESHOLDS #######
cor.test(df_all_new_plot$xmid[which(df_all_new_plot$quantifier=="many")], df_all_new_plot$xmid[which(df_all_new_plot$quantifier=="few")])

######## CALCULATE THRESHOLDS DESCRIP STAT #########
max(df_all_new_plot$xmid[which(df_all_new_plot$quant=="many")])
max(df_all_new_plot$xmid[which(df_all_new_plot$quant=="most")])
max(df_all_new_plot$xmid[which(df_all_new_plot$quant=="MTH")])
max(df_all_new_plot$xmid[which(df_all_new_plot$quant=="few")])
max(df_all_new_plot$xmid[which(df_all_new_plot$quant=="FTH")])

######### Apply thresholds ##############
df_all_new$workerid=as.factor(df_all_new$workerid)
rmany2$workerid=as.factor(rmany2$workerid)
rmany2=left_join(rmany2, df_all_new)

df_all_new.most$workerid=as.factor(df_all_new.most$workerid)
rmost2$workerid=as.factor(rmost2$workerid)
rmost2=left_join(rmost2, df_all_new.most)

df_all_new.mth$workerid=as.factor(df_all_new.mth$workerid)
rmth2$workerid=as.factor(rmth2$workerid)
rmth2=left_join(rmth2, df_all_new.mth)

df_all_new.few$workerid=as.factor(df_all_new.few$workerid)
rfew2$workerid=as.factor(rfew2$workerid)
rfew2=left_join(rfew2, df_all_new.few)

df_all_new.fth$workerid=as.factor(df_all_new.fth$workerid)
rfth2$workerid=as.factor(rfth2$workerid)
rfth2=left_join(rfth2, df_all_new.fth)

########## REGRESSION ALL RESP ########
library(lme4)
library(lmerTest)
library(jtools)

rmany22<-rmany2
rmany22$response_rec=ifelse(rmany22$percent>rmany22$xmid & rmany22$response==1, "correct", 
                           ifelse(rmany22$percent>rmany22$xmid & rmany22$response==0, "incorrect",
                           ifelse(rmany22$percent<rmany22$xmid & rmany22$response==1, "incorrect", "correct")))
rmany22$response_rec=as.factor(rmany22$response_rec)
levels(rmany22$response_rec)

rmany22=subset.data.frame(rmany22, rmany22$workerid!="27")
rmany22=subset.data.frame(rmany22, rmany22$workerid!="76")

rmany222<-subset.data.frame(rmany22, rmany22$response_rec=="correct")
rmany222$response_t_f=ifelse(rmany222$percent>rmany222$xmid & rmany222$response==1, "true", "false")
rmany222$response_t_f=as.factor(rmany222$response_t_f)
levels(rmany222$response_t_f)

rmany222$zpercent=scale(rmany222$percent)
rmany222$zxmid=scale(rmany222$xmid)
rmany222$zrt=scale(rmany222$read_and_decide_time)


rmodel_many_t_f=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                     +(1|workerid), data=rmany222, REML = FALSE)

rmodel_many_t_f_1=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                     +(1+response_t_f|workerid), data=rmany222, REML = FALSE)

rmodel_many_t_f_2=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                       +(1+zpercent|workerid), data=rmany222, REML = FALSE)

rmodel_many_t_f_3=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                       +(1+zpercent+response_t_f|workerid), data=rmany222, REML = FALSE)

anova(rmodel_many_t_f, rmodel_many_t_f_1)
#finall model with random intercept and slope for precent; resulat the same regardless random effect structure

#TEST SIGNIFICANCE OF THE RESUALTS
rmodel_many_t_f_4=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                       -zpercent:zxmid:relevel(response_t_f, "true")
                       +(1+zpercent|workerid), data=rmany222, REML = FALSE)

anova(rmodel_many_t_f_2, rmodel_many_t_f_4)
#model with three way intarction
summary(rmodel_many_t_f_2)

#PLOT FINAL MODEL
rmodel_many_t_f_5=lmer(read_and_decide_time~zpercent*zxmid*response_t_f
                     +(1+zpercent|workerid), data=rmany222, REML = FALSE)
interact_plot(rmodel_many_t_f_5, pred = "zpercent", modx = "zxmid", mod2 = "response_t_f")


##### MTH
rmth22<-rmth2
rmth22$response_rec=ifelse(rmth22$percent>rmth22$xmid & rmth22$response==1, "correct", 
                             ifelse(rmth22$percent>rmth22$xmid & rmth22$response==0, "incorrect",
                                    ifelse(rmth22$percent<rmth22$xmid & rmth22$response==1, "incorrect", "correct")))
rmth22$response_rec=as.factor(rmth22$response_rec)
levels(rmth22$response_rec)

rmth22=subset.data.frame(rmth22, rmth22$workerid!="27")
rmth22=subset.data.frame(rmth22, rmth22$workerid!="76")

rmth222<-subset.data.frame(rmth22, rmth22$response_rec=="correct")
rmth222$response_t_f=ifelse(rmth222$percent>rmth222$xmid & rmth222$response==1, "true", "false")
rmth222$response_t_f=as.factor(rmth222$response_t_f)
levels(rmth222$response_t_f)

rmth222$zpercent=scale(rmth222$percent)
rmth222$zxmid=scale(rmth222$xmid)
rmth222$zrt=scale(rmth222$read_and_decide_time)

#MAXIMAL MODEL TEST RANDOM STRUCTURE
rmodel_mth_t_f=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                     +(1|workerid), data=rmth222, REML = FALSE)

rmodel_mth_t_f_1=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                    +(1+zpercent|workerid), data=rmth222, REML = FALSE)

anova(rmodel_mth_t_f, rmodel_mth_t_f_1)
#random slopes do not improve model

#MODEL WITH ONLY INTERCEPT TEST SIGNIFICANCE OF EFFECTS
rmodel_mth_t_f_2=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                    -zpercent:zxmid:relevel(response_t_f, "true")
                    +(1|workerid), data=rmth222, REML = FALSE)
anova(rmodel_mth_t_f, rmodel_mth_t_f_2)
#drop three way interaction

rmodel_mth_t_f_3=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                      -zpercent:zxmid:relevel(response_t_f, "true")
                      -zpercent:zxmid
                      +(1|workerid), data=rmth222, REML = FALSE)
anova(rmodel_mth_t_f_3, rmodel_mth_t_f_2)
#drop zpercent:zxmid interaction
#finall model
summary(rmodel_mth_t_f_3)


#FINAL MODEL PLOT
rmodel_mth_t_f_2=lmer(read_and_decide_time~zpercent*zxmid*response_t_f
                      -zpercent:zxmid:response_t_f
                      -zpercent:zxmid
                    +(1|workerid), data=rmth222, REML = FALSE)
interact_plot(rmodel_mth_t_f_2, pred = "zpercent", modx = "zxmid", mod2 = "response_t_f")

###### MOST
rmost22<-rmost2
rmost22$response_rec=ifelse(rmost22$percent>rmost22$xmid & rmost22$response==1, "correct", 
                             ifelse(rmost22$percent>rmost22$xmid & rmost22$response==0, "incorrect",
                                    ifelse(rmost22$percent<rmost22$xmid & rmost22$response==1, "incorrect", "correct")))
rmost22$response_rec=as.factor(rmost22$response_rec)
levels(rmost22$response_rec)

rmost22=subset.data.frame(rmost22, rmost22$workerid!="27")
rmost22=subset.data.frame(rmost22, rmost22$workerid!="76")


rmost222<-subset.data.frame(rmost22, rmost22$response_rec=="correct")
rmost222$response_t_f=ifelse(rmost222$percent>rmost222$xmid & rmost222$response==1, "true", "false")
rmost222$response_t_f=as.factor(rmost222$response_t_f)
levels(rmost222$response_t_f)

rmost222$zpercent=scale(rmost222$percent)
rmost222$zxmid=scale(rmost222$xmid)
rmost222$zrt=scale(rmost222$read_and_decide_time)

#TEST RANDOM STRUCTURE FOR MAXIMAL MODEL
rmodel_most_t_f=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                     +(1|workerid), data=rmost222, REML = FALSE)

rmodel_most_t_f_1=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                       +(1+response_t_f|workerid), data=rmost222, REML = FALSE)

rmodel_most_t_f_2=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                       +(1+zpercent|workerid), data=rmost222, REML = FALSE)

rmodel_most_t_f_3=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                       +(1+zpercent+response_t_f|workerid), data=rmost222, REML = FALSE)
#singular fit
anova(rmodel_most_t_f, rmodel_most_t_f_2)
#include random slope for response type; random effect do not affect the main effects resulats

#TEST SIGNIFICANCE OF EFFECTS
rmodel_most_t_f_4=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                       -zpercent:zxmid:relevel(response_t_f, "true")
                       +(1+response_t_f|workerid), data=rmost222, REML = FALSE)
anova(rmodel_most_t_f_4, rmodel_most_t_f_1)
#three way interaction has to stay
summary(rmodel_most_t_f_1)
#PLOT FINAL MODEL
rmodel_most_t_f_5=lmer(read_and_decide_time~zpercent*zxmid*response_t_f
                       +(1+response_t_f|workerid), data=rmost222, REML = FALSE)
interact_plot(rmodel_most_t_f_5, pred = "zpercent", modx = "zxmid", mod2 = "response_t_f")

#### FEW
rfew22<-rfew2
rfew22$response_rec=ifelse(rfew22$percent<rfew22$xmid & rfew22$response==1, "correct", 
                             ifelse(rfew22$percent<rfew22$xmid & rfew22$response==0, "incorrect",
                                    ifelse(rfew22$percent>rfew22$xmid & rfew22$response==1, "incorrect", "correct")))
rfew22$response_rec=as.factor(rfew22$response_rec)
levels(rfew22$response_rec)

rfew22=subset.data.frame(rfew22, rfew22$workerid!="27")
rfew22=subset.data.frame(rfew22, rfew22$workerid!="76")

rfew222<-subset.data.frame(rfew22, rfew22$response_rec=="correct")
rfew222$response_t_f=ifelse(rfew222$percent<rfew222$xmid & rfew222$response==1, "true", "false")
rfew222$response_t_f=as.factor(rfew222$response_t_f)
levels(rfew222$response_t_f)

rfew222$zxmid=scale(rfew222$xmid)
rfew222$zpercent=scale(rfew222$percent)
rfew222$zrt=scale(rfew222$read_and_decide_time)

rmodel_few_t_f=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                     +(1|workerid), data=rfew222, REML = FALSE)

rmodel_few_t_fs=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                    +(1+zpercent|workerid), data=rfew222, REML = FALSE)

rmodel_few_t_f_1=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                       +(1+relevel(response_t_f, "true")|workerid), data=rfew222, REML = FALSE)

rmodel_few_t_f_2=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                      +(1+zpercent+relevel(response_t_f, "true")|workerid), data=rfew222, REML = FALSE)

anova(rmodel_few_t_f_2, rmodel_few_t_f_1)
#include slope random slope for response; random effect structure does not affect resualts

#TEST SIGNIFICANCE OF EFFECTS
rmodel_few_t_f_3=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                      -zpercent:zxmid:relevel(response_t_f, "true")
                      +(1+relevel(response_t_f, "true")|workerid), data=rfew222, REML = FALSE)
anova(rmodel_few_t_f_3, rmodel_few_t_f_1)
#three way interaction has to stay
summary(rmodel_few_t_f_1)

# MODEL TO PLOT
rmodel_few_t_f_4=lmer(read_and_decide_time~zpercent*zxmid*response_t_f
                      +(1+response_t_f|workerid), data=rfew222, REML = FALSE)
interact_plot(rmodel_few_t_f_4, pred = "zpercent", modx = "zxmid", mod2 = "response_t_f")

#### FTH
rfth22<-rfth2
rfth22$response_rec=ifelse(rfth22$percent<rfth22$xmid & rfth22$response==1, "correct", 
                            ifelse(rfth22$percent<rfth22$xmid & rfth22$response==0, "incorrect",
                                   ifelse(rfth22$percent>rfth22$xmid & rfth22$response==1, "incorrect", "correct")))
rfth22$response_rec=as.factor(rfth22$response_rec)
levels(rfth22$response_rec)

rfth22=subset.data.frame(rfth22, rfth22$workerid!="27")
rfth22=subset.data.frame(rfth22, rfth22$workerid!="76")

rfth222<-subset.data.frame(rfth22, rfth22$response_rec=="correct")
rfth222$response_t_f=ifelse(rfth222$percent<rfth222$xmid & rfth222$response==1, "true", "false")
rfth222$response_t_f=as.factor(rfth222$response_t_f)
levels(rfth222$response_t_f)

rfth222$zpercent=scale(rfth222$percent)
rfth222$zxmid=scale(rfth222$xmid)
rfth222$zrt=scale(rfth222$read_and_decide_time)

rmodel_fth_t_f=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                    +(1|workerid), data=rfth222, REML = FALSE)

rmodel_fth_t_f_1=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                      +(1+relevel(response_t_f, "true")|workerid), data=rfth222, REML = FALSE)

rmodel_fth_t_f_2=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                      +(1+zpercent|workerid), data=rfth222, REML = FALSE)

rmodel_fth_t_f_3=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                      +(1+zpercent+relevel(response_t_f, "true")|workerid), data=rfth222, REML = FALSE)

rmodel_fth_t_f_4=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                      +(1+zpercent+relevel(response_t_f, "true")+zpercent:relevel(response_t_f, "true")|workerid), data=rfth222, REML = FALSE)
#singular fit

anova(rmodel_fth_t_f_3, rmodel_fth_t_f_2)
#include random slope for percent and response; does not change the resualts

#TEST EFFECTS
rmodel_fth_t_f_5=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                      -zpercent:zxmid:relevel(response_t_f, "true")
                      +(1+zpercent+relevel(response_t_f, "true")|workerid), data=rfth222, REML = FALSE)
anova(rmodel_fth_t_f_5, rmodel_fth_t_f_3)
summary(rmodel_fth_t_f_5)
#drop three way interaction
rmodel_fth_t_f_6=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                      -zpercent:zxmid:relevel(response_t_f, "true")
                      -zxmid:relevel(response_t_f, "true")
                      +(1+zpercent+relevel(response_t_f, "true")|workerid), data=rfth222, REML = FALSE)
anova(rmodel_fth_t_f_5, rmodel_fth_t_f_6)
summary(rmodel_fth_t_f_6)
#drop zxmid:relevel(response_t_f, "true")
rmodel_fth_t_f_7=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                      -zpercent:zxmid:relevel(response_t_f, "true")
                      -zxmid:relevel(response_t_f, "true")
                      -zpercent:zxmid
                      +(1+zpercent+relevel(response_t_f, "true")|workerid), data=rfth222, REML = FALSE)
anova(rmodel_fth_t_f_7, rmodel_fth_t_f_6)
summary(rmodel_fth_t_f_7)
#drop zpercent:zxmid
rmodel_fth_t_f_8=lmer(read_and_decide_time~zpercent+zxmid+relevel(response_t_f, "true")
                      +(1+zpercent+relevel(response_t_f, "true")|workerid), data=rfth222, REML = FALSE)
anova(rmodel_fth_t_f_7, rmodel_fth_t_f_8)
summary(rmodel_fth_t_f_8)
#drop all interactions

#FINAL MODEL TO PLOT
rmodel_fth_t_f_9=lmer(read_and_decide_time~zpercent+zxmid+response_t_f
                      +(1+response_t_f+zpercent|workerid), data=rfth222, REML = FALSE)
interact_plot(rmodel_fth_t_f_9, pred = "zpercent", modx = "zxmid", mod2 = "response_t_f")



############################### REPLICATION EXPERIMENT ############
exp1.replication.v2.trials$response=ifelse(exp1.replication.v2.trials$response=="false", 0,1)
##### EXCLUE PARTICIPANT 
exp1.replication.v2.trials=subset.data.frame(exp1.replication.v2.trials, exp1.replication.v2.trials$workerid!="3")
exp1.replication.v2.trials=subset.data.frame(exp1.replication.v2.trials, exp1.replication.v2.trials$workerid!="4")
exp1.replication.v2.trials=subset.data.frame(exp1.replication.v2.trials, exp1.replication.v2.trials$workerid!="5")
exp1.replication.v2.trials=subset.data.frame(exp1.replication.v2.trials, exp1.replication.v2.trials$workerid!="9")
exp1.replication.v2.trials=subset.data.frame(exp1.replication.v2.trials, exp1.replication.v2.trials$workerid!="10")
exp1.replication.v2.trials=subset.data.frame(exp1.replication.v2.trials, exp1.replication.v2.trials$workerid!="16")
exp1.replication.v2.trials=subset.data.frame(exp1.replication.v2.trials, exp1.replication.v2.trials$workerid!="21")
exp1.replication.v2.trials=subset.data.frame(exp1.replication.v2.trials, exp1.replication.v2.trials$workerid!="30")
exp1.replication.v2.trials=subset.data.frame(exp1.replication.v2.trials, exp1.replication.v2.trials$workerid!="33")
exp1.replication.v2.trials=subset.data.frame(exp1.replication.v2.trials, exp1.replication.v2.trials$workerid!="34")
exp1.replication.v2.trials=subset.data.frame(exp1.replication.v2.trials, exp1.replication.v2.trials$workerid!="48")
exp1.replication.v2.trials=subset.data.frame(exp1.replication.v2.trials, exp1.replication.v2.trials$workerid!="56")
exp1.replication.v2.trials=subset.data.frame(exp1.replication.v2.trials, exp1.replication.v2.trials$workerid!="69")
exp1.replication.v2.trials=subset.data.frame(exp1.replication.v2.trials, exp1.replication.v2.trials$workerid!="81")

exp1.replication.v2.trials=subset.data.frame(exp1.replication.v2.trials, exp1.replication.v2.trials$workerid!="1")
exp1.replication.v2.trials=subset.data.frame(exp1.replication.v2.trials, exp1.replication.v2.trials$workerid!="25")
exp1.replication.v2.trials=subset.data.frame(exp1.replication.v2.trials, exp1.replication.v2.trials$workerid!="29")
exp1.replication.v2.trials=subset.data.frame(exp1.replication.v2.trials, exp1.replication.v2.trials$workerid!="43")
exp1.replication.v2.trials=subset.data.frame(exp1.replication.v2.trials, exp1.replication.v2.trials$workerid!="46")
exp1.replication.v2.trials=subset.data.frame(exp1.replication.v2.trials, exp1.replication.v2.trials$workerid!="58")
exp1.replication.v2.trials=subset.data.frame(exp1.replication.v2.trials, exp1.replication.v2.trials$workerid!="70")
exp1.replication.v2.trials=subset.data.frame(exp1.replication.v2.trials, exp1.replication.v2.trials$workerid!="72")
exp1.replication.v2.trials=subset.data.frame(exp1.replication.v2.trials, exp1.replication.v2.trials$workerid!="74")
exp1.replication.v2.trials=subset.data.frame(exp1.replication.v2.trials, exp1.replication.v2.trials$workerid!="82")

exp1.replication.v2.trials=subset.data.frame(exp1.replication.v2.trials, exp1.replication.v2.trials$read_and_decide_time > 300)

rrmany=subset.data.frame(exp1.replication.v2.trials, exp1.replication.v2.trials$quant=="\"Many\"")
rrfew=subset.data.frame(exp1.replication.v2.trials, exp1.replication.v2.trials$quant=="\"Few\"")
rrmost=subset.data.frame(exp1.replication.v2.trials, exp1.replication.v2.trials$quant=="\"Most\"")
rrmth=subset.data.frame(exp1.replication.v2.trials, exp1.replication.v2.trials$quant=="\"More than half\"")
rrfth=subset.data.frame(exp1.replication.v2.trials, exp1.replication.v2.trials$quant=="\"Fewer than half\"")


#MANY
rrmany_yes=subset.data.frame(rrmany, rrmany$response==1)
rrcut_many_yes_1=mean(rrmany_yes$read_and_decide_time)+2*sd(rrmany_yes$read_and_decide_time)
rrmany_yes_1=subset.data.frame(rrmany_yes, rrmany_yes$read_and_decide_time<rrcut_many_yes_1)
rrmany_no=subset.data.frame(rrmany, rrmany$response==0)
rrcut_many_no_1=mean(rrmany_no$read_and_decide_time)+2*sd(rrmany_no$read_and_decide_time)
rrmany_no_1=subset.data.frame(rrmany_no, rrmany_no$read_and_decide_time<rrcut_many_no_1)
rrmany2=rbind(rrmany_no_1, rrmany_yes_1)

#FEW
rrfew_yes=subset.data.frame(rrfew, rrfew$response==1)
rrcut_few_yes_1=mean(rrfew_yes$read_and_decide_time)+2*sd(rrfew_yes$read_and_decide_time)
rrfew_yes_1=subset.data.frame(rrfew_yes, rrfew_yes$read_and_decide_time<rrcut_few_yes_1)
rrfew_no=subset.data.frame(rrfew, rrfew$response==0)
rrcut_few_no_1=mean(rrfew_no$read_and_decide_time)+2*sd(rrfew_no$read_and_decide_time)
rrfew_no_1=subset.data.frame(rrfew_no, rrfew_no$read_and_decide_time<rrcut_few_no_1)
rrfew2=rbind(rrfew_no_1, rrfew_yes_1)

#MOST
rrmost_yes=subset.data.frame(rrmost, rrmost$response==1)
rrcut_most_yes_1=mean(rrmost_yes$read_and_decide_time)+2*sd(rrmost_yes$read_and_decide_time)
rrmost_yes_1=subset.data.frame(rrmost_yes, rrmost_yes$read_and_decide_time<rrcut_most_yes_1)
rrmost_no=subset.data.frame(rrmost, rrmost$response==0)
rrcut_most_no_1=mean(rrmost_no$read_and_decide_time)+2*sd(rrmost_no$read_and_decide_time)
rrmost_no_1=subset.data.frame(rrmost_no, rrmost_no$read_and_decide_time<rrcut_most_no_1)
rrmost2=rbind(rrmost_no_1, rrmost_yes_1)

#MTH
rrmth_yes=subset.data.frame(rrmth, rrmth$response==1)
rrcut_mth_yes_1=mean(rrmth_yes$read_and_decide_time)+2*sd(rrmth_yes$read_and_decide_time)
rrmth_yes_1=subset.data.frame(rrmth_yes, rrmth_yes$read_and_decide_time<rrcut_mth_yes_1)
rrmth_no=subset.data.frame(rrmth, rrmth$response==0)
rrcut_mth_no_1=mean(rrmth_no$read_and_decide_time)+2*sd(rrmth_no$read_and_decide_time)
rrmth_no_1=subset.data.frame(rrmth_no, rrmth_no$read_and_decide_time<rrcut_mth_no_1)
rrmth2=rbind(rrmth_no_1, rrmth_yes_1)

#FTH
rrfth_yes=subset.data.frame(rrfth, rrfth$response==1)
rrcut_fth_yes_1=mean(rrfth_yes$read_and_decide_time)+2*sd(rrfth_yes$read_and_decide_time)
rrfth_yes_1=subset.data.frame(rrfth_yes, rrfth_yes$read_and_decide_time<rrcut_fth_yes_1)
rrfth_no=subset.data.frame(rrfth, rrfth$response==0)
rrcut_fth_no_1=mean(rrfth_no$read_and_decide_time)+2*sd(rrfth_no$read_and_decide_time)
rrfth_no_1=subset.data.frame(rrfth_no, rrfth_no$read_and_decide_time<rrcut_fth_no_1)
rrfth2=rbind(rrfth_no_1, rrfth_yes_1)


####### ESTIMATE THRESHOLDS #######
#MANY
library(plyr)
library(dplyr)
model <- function(x){
  try(coef(nls(response ~ 1/(1+exp((xmid-percent)/scal)), data=x, 
               start=list(xmid=50,scal=4))))
}

rrfitted.linear.model <- dlply(rrmany2, .(workerid), model)
rrx=rrfitted.linear.model[-(which(sapply(rrfitted.linear.model,is.character),arr.ind=TRUE))]
rry=rrfitted.linear.model[(which(sapply(rrfitted.linear.model,is.character),arr.ind=TRUE))]
rrdf <- data.frame(Reduce(rbind, rrx))
rrdf$workerid=names(rrx)

rrvector=as.vector(names(rrx))
rrcertain <- rrmany2[!rrmany2$workerid %in% rrvector, ]

threshold <- function(n){
  (max(n$percent[which(n$response=="0")])+min(n$percent[which(n$response=="1")]))/2
}

rrcertained.fitted =dlply(rrcertain, .(workerid), threshold)
rrdf_cert <- data.frame(Reduce(rbind, rrcertained.fitted))
rrdf_cert$workerid=names(rry)
colnames(rrdf_cert)[1] <- "xmid"
rrdf_cert$scal=rep(c(0))
rrdf_cert[, c("xmid", "scal", "workerid")]
rrdf_all=rbind(rrdf_cert, rrdf[,c(1:3)])
rrdf_all$workerid=as.numeric(rrdf_all$workerid)
rrdf_all_new=rrdf_all[order(rrdf_all[,2]),]
hist(rrdf_all_new$xmid)
rrdf_all_new$workerid[which(rrdf_all_new$xmid>100)]
#no workerid to exclude

#MOST
rrfitted.linear.model.most <- dlply(rrmost2, .(workerid), model)
rrx.most=rrfitted.linear.model.most[-(which(sapply(rrfitted.linear.model.most,is.character),arr.ind=TRUE))]
rry.most=rrfitted.linear.model.most[(which(sapply(rrfitted.linear.model.most,is.character),arr.ind=TRUE))]
rrdf.most <- data.frame(Reduce(rbind, rrx.most))
rrdf.most$workerid=names(rrx.most)

rrvector.most=as.vector(names(rrx.most))
rrcertain.most <- rrmost2[!rrmost2$workerid %in% rrvector.most, ]

rrcertained.fitted.most =dlply(rrcertain.most, .(workerid), threshold)
rrdf_cert.most <- data.frame(Reduce(rbind, rrcertained.fitted.most))
rrdf_cert.most$workerid=names(rry.most)
colnames(rrdf_cert.most)[1] <- "xmid"
rrdf_cert.most$scal=rep(c(0))
rrdf_cert.most[,c("xmid", "scal", "workerid")]
rrdf_all.most=rbind(rrdf_cert.most, rrdf.most[,c(1:3)])
rrdf_all.most$xmid
hist(rrdf_all.most$xmid)
rrdf_all.most$workerid=as.numeric(rrdf_all.most$workerid)
rrdf_all_new.most=rrdf_all.most[order(rrdf_all.most[,2]),]
rrdf_all.most$workerid[which(rrdf_all.most$xmid>100)]
#no workerids to exclude

#MTH
rrfitted.linear.model.mth <- dlply(rrmth2, .(workerid), model)
rrx.mth=rrfitted.linear.model.mth[-(which(sapply(rrfitted.linear.model.mth,is.character),arr.ind=TRUE))]
rry.mth=rrfitted.linear.model.mth[(which(sapply(rrfitted.linear.model.mth,is.character),arr.ind=TRUE))]
rrdf.mth <- data.frame(Reduce(rbind, rrx.mth))
rrdf.mth$workerid=names(rrx.mth)

rrvector.mth=as.vector(names(rrx.mth))
rrcertain.mth <- rrmth2[!rrmth2$workerid %in% rrvector.mth, ]

rrcertained.fitted.mth =dlply(rrcertain.mth, .(workerid), threshold)
rrdf_cert.mth <- data.frame(Reduce(rbind, rrcertained.fitted.mth))
rrdf_cert.mth$workerid=names(rry.mth)
colnames(rrdf_cert.mth)[1] <- "xmid"
rrdf_cert.mth$scal=rep(c(0))
rrdf_cert.mth[,c("xmid", "scal", "workerid")]
rrdf_all.mth=rbind(rrdf_cert.mth, rrdf.mth[,c(1:3)])

rrdf_all.mth$xmid
hist(rrdf_all.mth$xmid)
rrdf_all.mth$workerid=as.numeric(rrdf_all.mth$workerid)
rrdf_all_new.mth=rrdf_all.mth[order(rrdf_all.mth[,2]),]
rrdf_all_new.mth$workerid[which(rrdf_all_new.mth$xmid<0)]
rrdf_all_new.mth$workerid[which(rrdf_all_new.mth$xmid>100)]
#no workerids to exclude

#FEW
model.few <- function(x){
  try(coef(nls(response ~ 1/(1+exp((xmid-percent)/scal)), data=x, 
               start=list(xmid=41,scal=-5))))
}
rrfitted.linear.model.few <- dlply(rrfew2, .(workerid), model.few)
rrxfew=rrfitted.linear.model.few[-(which(sapply(rrfitted.linear.model.few,is.character),arr.ind=TRUE))]
rryfew=rrfitted.linear.model.few[(which(sapply(rrfitted.linear.model.few,is.character),arr.ind=TRUE))]
rrdf.few <- data.frame(Reduce(rbind, rrxfew))
rrdf.few$workerid=names(rrxfew)


rrvector.few=as.vector(names(rrxfew))
rrcertain.few <- rrfew2[!rrfew2$workerid %in% rrvector.few, ]

threshold.few <- function(n){
  (min(n$percent[which(n$response=="0")])+max(n$percent[which(n$response=="1")]))/2
}

rrcertained.fitted.few =dlply(rrcertain.few, .(workerid), threshold.few)
rrdf_cert.few <- data.frame(Reduce(rbind, rrcertained.fitted.few))
rrdf_cert.few$workerid=names(rryfew)
colnames(rrdf_cert.few)[1] <- "xmid"
rrdf_cert.few$scal=rep(c(0))
rrdf_all.few=rbind(rrdf_cert.few, rrdf.few[,c(1:3)])

rrdf_all.few$workerid=as.numeric(rrdf_all.few$workerid)
rrdf_all_new.few=rrdf_all.few[order(rrdf_all.few[,2]),]

rrdf_all_new.few$workerid[which(rrdf_all_new.few$xmid<0)]
#[1] 28 
hist(rrdf_all_new.few$xmid)

#FTH 
rrfitted.linear.model.fth <- dlply(rrfth2, .(workerid), model.few)
rrxfth=rrfitted.linear.model.fth[-(which(sapply(rrfitted.linear.model.fth,is.character),arr.ind=TRUE))]
rryfth=rrfitted.linear.model.fth[(which(sapply(rrfitted.linear.model.fth,is.character),arr.ind=TRUE))]
rrdf.fth <- data.frame(Reduce(rbind, rrxfth))
rrdf.fth$workerid=names(rrxfth)


rrvector.fth=as.vector(names(rrxfth))
rrcertain.fth <- rrfth2[!rrfth2$workerid %in% rrvector.fth, ]
rrcertained.fitted.fth =dlply(rrcertain.fth, .(workerid), threshold.few)
rrdf_cert.fth <- data.frame(Reduce(rbind, rrcertained.fitted.fth))
rrdf_cert.fth$workerid=names(rryfth)
colnames(rrdf_cert.fth)[1] <- "xmid"
rrdf_cert.fth$scal=rep(c(0))
rrdf_all.fth=rbind(rrdf_cert.fth, rrdf.fth[,c(1:3)])
rrdf_all.fth$workerid=as.numeric(rrdf_all.fth$workerid)
rrdf_all_new.fth=rrdf_all.fth[order(rrdf_all.fth[,2]),]
rrdf_all_new.fth$workerid[which(rrdf_all_new.fth$xmid<0)]
rrdf_all_new.fth$workerid[which(rrdf_all_new.fth$xmid>100)]
#no workerids to exclude

####### PLOTS
rrdf_all_new.q=cbind(rrdf_all_new, quantifier="many")
rrdf_all_new.few.q=cbind(rrdf_all_new.few, quantifier="few")
rrdf_all_new.fth.q=cbind(rrdf_all_new.fth, quantifier="FTH")
rrdf_all_new.most.q=cbind(rrdf_all_new.most, quantifier="most")
rrdf_all_new.mth.q=cbind(rrdf_all_new.mth, quantifier="MTH")

rrdf_all_new_plot=rbind(rrdf_all_new.q, rrdf_all_new.few.q, rrdf_all_new.fth.q, rrdf_all_new.most.q, rrdf_all_new.mth.q)
rrdf_all_new_plot=subset.data.frame(rrdf_all_new_plot, rrdf_all_new_plot$workerid!="28")


ks.test(rrdf_all_new_plot$xmid[which(rrdf_all_new_plot$quantifier=="most")], rrdf_all_new_plot$xmid[which(rrdf_all_new_plot$quantifier=="MTH")], exact=FALSE)
ks.test(rrdf_all_new_plot$xmid[which(rrdf_all_new_plot$quantifier=="FTH")], rrdf_all_new_plot$xmid[which(rrdf_all_new_plot$quantifier=="MTH")])
ks.test(rrdf_all_new_plot$xmid[which(rrdf_all_new_plot$quantifier=="few")], rrdf_all_new_plot$xmid[which(rrdf_all_new_plot$quantifier=="many")])
ks.test(rrdf_all_new_plot$xmid[which(rrdf_all_new_plot$quantifier=="most")], rrdf_all_new_plot$xmid[which(rrdf_all_new_plot$quantifier=="many")])
library(lawstat)
levene.test(rrdf_all_new_plot$xmid[which(rrdf_all_new_plot$quantifier=="most")], rrdf_all_new_plot$xmid[which(rrdf_all_new_plot$quantifier=="many")])
levene.test(rrdf_all_new_plot$xmid[which(rrdf_all_new_plot$quantifier=="few")], rrdf_all_new_plot$xmid[which(rrdf_all_new_plot$quantifier=="many")])

library(ggplot2)
library(scales)
library(plyr)
library(dplyr)
rrmu <- ddply(rrdf_all_new_plot, "quantifier", summarise, grp.mean=mean(xmid))
head(rrmu)
library(ggplot2)
ggplot(rrdf_all_new_plot, aes(x=xmid, fill=quantifier)) +
  geom_histogram(binwidth=3, alpha=.5, position="identity")+
  #scale_y_continuous(labels = percent)+
  geom_vline(data=rrmu, aes(xintercept=grp.mean, color=quantifier),
             linetype="dashed")+
  scale_x_continuous(name = "Individual threshold",
                     breaks = seq(0, 80, 10),
                     limits=c(10, 70)) +
  theme_light()+
  facet_wrap(.~quantifier)

######### CORELATE MANY AND FEW ########
cor.test(rrdf_all_new_plot$xmid[which(rrdf_all_new_plot$quantifier=="few")], 
         rrdf_all_new_plot$xmid[which(rrdf_all_new_plot$quantifier=="many")])
    
######## THRESHOLDS DESCRIPTIVES STAT ########
max(rrdf_all_new_plot$xmid[which(rrdf_all_new_plot$quantifier=="many")])
max(rrdf_all_new_plot$xmid[which(rrdf_all_new_plot$quantifier=="most")])
max(rrdf_all_new_plot$xmid[which(rrdf_all_new_plot$quantifier=="MTH")])
max(rrdf_all_new_plot$xmid[which(rrdf_all_new_plot$quantifier=="few")])
max(rrdf_all_new_plot$xmid[which(rrdf_all_new_plot$quantifier=="FTH")])

t.test(rrdf_all_new_plot$xmid[which(rrdf_all_new_plot$quantifier=="most")], 
       rrdf_all_new_plot$xmid[which(rrdf_all_new_plot$quantifier=="MTH")], paired = TRUE)
#different or approach
t.test(rrdf_all_new_plot$xmid[which(rrdf_all_new_plot$quantifier=="most")], mu=50)
#not different from 50%
t.test(rrdf_all_new_plot$xmid[which(rrdf_all_new_plot$quantifier=="FTH")], 
       rrdf_all_new_plot$xmid[which(rrdf_all_new_plot$quantifier=="MTH")], paired = TRUE)
#not different
t.test(rrdf_all_new_plot$xmid[which(rrdf_all_new_plot$quantifier=="most")], 
       rrdf_all_new_plot$xmid[which(rrdf_all_new_plot$quantifier=="many")], paired = TRUE)
#different
t.test(rrdf_all_new_plot$xmid[which(rrdf_all_new_plot$quantifier=="few")], 
       rrdf_all_new_plot$xmid[which(rrdf_all_new_plot$quantifier=="many")], paired = TRUE)
#not different

str(rrdf_all_new_plot)
summary(lm(xmid ~ quantifier, data=rrdf_all_new_plot))


######## THRESHOLD ##########
rrdf_all_new$workerid=as.factor(rrdf_all_new$workerid)
rrmany2$workerid=as.factor(rrmany2$workerid)
rrmany2=left_join(rrmany2, rrdf_all_new)

rrmany22<-rrmany2
rrmany22=subset.data.frame(rrmany22, rrmany22$workerid!="28")

rrmany22$response_rec=ifelse(rrmany22$percent>rrmany22$xmid & rrmany22$response==1, "correct", 
                             ifelse(rrmany22$percent>rrmany22$xmid & rrmany22$response==0, "incorrect",
                                    ifelse(rrmany22$percent<rrmany22$xmid & rrmany22$response==1, "incorrect", "correct")))
rrmany22$response_rec=as.factor(rrmany22$response_rec)
levels(rrmany22$response_rec)

rrmany222<-subset.data.frame(rrmany22, rrmany22$response_rec=="correct")
rrmany222$response
rrmany222$response_t_f=ifelse(rrmany222$percent>rrmany222$xmid & rrmany222$response==1, "true", "false")
rrmany222$response_t_f=as.factor(rrmany222$response_t_f)
levels(rrmany222$response_t_f)

rrmany222$zpercent=scale(rrmany222$percent)
rrmany222$zxmid=scale(rrmany222$xmid)

#RANDOM EFFECT STRUCTURE 
rrmodel_many_t_f=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                     +(1|workerid), data=rrmany222, REML = FALSE)

rrmodel_many_t_f_1=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                      +(1+relevel(response_t_f, "true")|workerid), data=rrmany222, REML = FALSE)
#singular fit
rrmodel_many_t_f_2=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                        +(1+zpercent|workerid), data=rrmany222, REML = FALSE)

rrmodel_many_t_f_3=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                        +(1+zpercent+response_t_f|workerid), data=rrmany222, REML = FALSE)
#fail to convarge
anova(rrmodel_many_t_f, rrmodel_many_t_f_2)
#include random intercept and slope for precent

#TEST EFFECTS SIGNIFICANCE
rrmodel_many_t_f_4=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                        -zpercent:zxmid:relevel(response_t_f, "true")
                        +(1+zpercent|workerid), data=rrmany222, REML = FALSE)
anova(rrmodel_many_t_f_2, rrmodel_many_t_f_4)
rrmodel_many_t_f_5=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                        -zpercent:zxmid:relevel(response_t_f, "true")
                        -zxmid:relevel(response_t_f, "true")
                        +(1+zpercent|workerid), data=rrmany222, REML = FALSE)
anova(rrmodel_many_t_f_5, rrmodel_many_t_f_4)
summary(rrmodel_many_t_f_5)

library(jtools)
rrmodel_many_t_f_5=lmer(read_and_decide_time~zpercent*zxmid*response_t_f
                        +(1+zpercent|workerid), data=rrmany222, REML = FALSE)
interact_plot(rrmodel_many_t_f_5, pred = "zpercent", modx = "zxmid", mod2 = "response_t_f")


############# MTH
rrdf_all_new.mth$workerid=as.factor(rrdf_all_new.mth$workerid)
rrmth2$workerid=as.factor(rrmth2$workerid)
rrmth2=left_join(rrmth2, rrdf_all_new.mth)

rrmth22<-rrmth2
rrmth22=subset.data.frame(rrmth22, rrmth22$workerid!="28")

rrmth22$response_rec=ifelse(rrmth22$percent>rrmth22$xmid & rrmth22$response==1, "correct", 
                              ifelse(rrmth22$percent>rrmth22$xmid & rrmth22$response==0, "incorrect",
                                     ifelse(rrmth22$percent<rrmth22$xmid & rrmth22$response==1, "incorrect", "correct")))
rrmth22$response_rec=as.factor(rrmth22$response_rec)
levels(rrmth22$response_rec)

rrmth222<-subset.data.frame(rrmth22, rrmth22$response_rec=="correct")
rrmth222$response_t_f=ifelse(rrmth222$percent>rrmth222$xmid & rrmth222$response==1, "true", "false")
rrmth222$response_t_f=as.factor(rrmth222$response_t_f)
levels(rrmth222$response_t_f)

rrmth222$zpercent=scale(rrmth222$percent)
rrmth222$zxmid=scale(rrmth222$xmid)

#RANDOM EFFECTS STRUCTURE
rrmodel_mth_t_f=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                     +(1|workerid), data=rrmth222, REML = FALSE)

rrmodel_mth_t_f_1=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                     +(1+response_t_f|workerid), data=rrmth222, REML = FALSE)

rrmodel_mth_t_f_2=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                       +(1+zpercent|workerid), data=rrmth222, REML = FALSE)

anova(rrmodel_mth_t_f, rrmodel_mth_t_f_2)
#only ranodm intercept, all models gives the same resualts

#FIXED EFFECT STRUCTURE
rrmodel_mth_t_f_3=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                     -zpercent:zxmid:relevel(response_t_f, "true")
                     +(1|workerid), data=rrmth222, REML = FALSE)
anova(rrmodel_mth_t_f, rrmodel_mth_t_f_3)
#drop three way interaction

rrmodel_mth_t_f_4=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                     -zpercent:zxmid:relevel(response_t_f, "true")
                     -zpercent:zxmid
                     +(1|workerid), data=rrmth222, REML = FALSE)
anova(rrmodel_mth_t_f_4, rrmodel_mth_t_f_3)
rrmodel_mth_t_f_5=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                     -zpercent:zxmid:relevel(response_t_f, "true")
                     -zpercent:zxmid
                     -zxmid:relevel(response_t_f, "true")
                     +(1|workerid), data=rrmth222, REML = FALSE)
anova(rrmodel_mth_t_f_4, rrmodel_mth_t_f_5)

rrmodel_mth_t_f_6=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                     -zpercent:zxmid:relevel(response_t_f, "true")
                     -zpercent:zxmid
                     -zxmid:relevel(response_t_f, "true")
                     -zpercent:relevel(response_t_f, "true")
                     +(1|workerid), data=rrmth222, REML = FALSE)
anova(rrmodel_mth_t_f_6, rrmodel_mth_t_f_5)
summary(rrmodel_mth_t_f_6)


rrmodel_mth_t_f_7=lmer(read_and_decide_time~zpercent*zxmid*response_t_f
                       -zpercent:zxmid:response_t_f
                       -zpercent:zxmid
                       -zxmid:response_t_f
                       -zpercent:response_t_f
                     +(1|workerid), data=rrmth222, REML = FALSE)
interact_plot(rrmodel_mth_t_f_7, pred = "zpercent", modx = "zxmid", mod2 = "response_t_f")

#MOST
rrdf_all_new.most$workerid=as.factor(rrdf_all_new.most$workerid)
rrmost2$workerid=as.factor(rrmost2$workerid)
rrmost2=left_join(rrmost2, rrdf_all_new.most)

rrmost22<-rrmost2
rrmost22=subset.data.frame(rrmost22, rrmost22$workerid!="28")

rrmost22$response_rec=ifelse(rrmost22$percent>rrmost22$xmid & rrmost22$response==1, "correct", 
                              ifelse(rrmost22$percent>rrmost22$xmid & rrmost22$response==0, "incorrect",
                                     ifelse(rrmost22$percent<rrmost22$xmid & rrmost22$response==1, "incorrect", "correct")))
rrmost22$response_rec=as.factor(rrmost22$response_rec)
levels(rrmost22$response_rec)

rrmost222<-subset.data.frame(rrmost22, rrmost22$response_rec=="correct")
rrmost222$response_t_f=ifelse(rrmost222$percent>rrmost222$xmid & rrmost222$response==1, "true", "false")
rrmost222$response_t_f=as.factor(rrmost222$response_t_f)
levels(rrmost222$response_t_f)

rrmost222$zpercent=scale(rrmost222$percent)
rrmost222$zxmid=scale(rrmost222$xmid)

rrmodel_most_t_f=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                      +(1|workerid), data=rrmost222, REML = FALSE)

rrmodel_most_t_f_1=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                        +(1+zpercent|workerid), data=rrmost222, REML = FALSE)


rrmodel_most_t_f_2=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                        +(1+relevel(response_t_f, "true")|workerid), data=rrmost222, REML = FALSE)

rrmodel_most_t_f_3=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                        +(1+relevel(response_t_f, "true")+zpercent|workerid), data=rrmost222, REML = FALSE)

rrmodel_most_t_f_4=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                        +(1+relevel(response_t_f, "true")+zpercent+relevel(response_t_f, "true"):zpercent|workerid), data=rrmost222, REML = FALSE)

anova(rrmodel_most_t_f_4, rrmodel_most_t_f_3)
#add random slope for response and precent, and interaction
rrmodel_most_t_f_5=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                        -zpercent:zxmid:relevel(response_t_f, "true")
                        +(1+relevel(response_t_f, "true")+zpercent+relevel(response_t_f, "true"):zpercent|workerid), data=rrmost222, REML = FALSE)
anova(rrmodel_most_t_f_4, rrmodel_most_t_f_5)
#drop three way interaction
rrmodel_most_t_f_6=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                        -zpercent:zxmid:relevel(response_t_f, "true")
                        -zxmid:relevel(response_t_f, "true")
                        +(1+relevel(response_t_f, "true")+zpercent+relevel(response_t_f, "true"):zpercent|workerid), data=rrmost222, REML = FALSE)
anova(rrmodel_most_t_f_6, rrmodel_most_t_f_5)

rrmodel_most_t_f_7=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                        -zpercent:zxmid:relevel(response_t_f, "true")
                        -zxmid:relevel(response_t_f, "true")
                        -zpercent:zxmid
                        +(1+relevel(response_t_f, "true")+zpercent+relevel(response_t_f, "true"):zpercent|workerid), data=rrmost222, REML = FALSE)
anova(rrmodel_most_t_f_6, rrmodel_most_t_f_7)

summary(rrmodel_most_t_f_7)

rrmodel_most_t_f_8=lmer(read_and_decide_time~zpercent*zxmid*response_t_f
                        -zpercent:zxmid:response_t_f
                        -zxmid:response_t_f
                        +(1+zpercent+response_t_f+response_t_f:zpercent|workerid), data=rrmost222, REML = FALSE)
interact_plot(rrmodel_most_t_f_8, pred = "zpercent", modx = "zxmid", mod2 = "response_t_f")

########## FEW
rrdf_all_new.few$workerid=as.factor(rrdf_all_new.few$workerid)
rrfew2$workerid=as.factor(rrfew2$workerid)
rrfew2=left_join(rrfew2, rrdf_all_new.few)

rrfew22<-rrfew2
rrfew22=subset.data.frame(rrfew22, rrfew22$workerid!="28")

rrfew22$response_rec=ifelse(rrfew22$percent<rrfew22$xmid & rrfew22$response==1, "correct", 
                              ifelse(rrfew22$percent<rrfew22$xmid & rrfew22$response==0, "incorrect",
                                     ifelse(rrfew22$percent>rrfew22$xmid & rrfew22$response==1, "incorrect", "correct")))
rrfew22$response_rec=as.factor(rrfew22$response_rec)
levels(rrfew22$response_rec)

rrfew222<-subset.data.frame(rrfew22, rrfew22$response_rec=="correct")
rrfew222$response_t_f=ifelse(rrfew222$percent<rrfew222$xmid & rrfew222$response==1, "true", "false")
rrfew222$response_t_f=as.factor(rrfew222$response_t_f)
levels(rrfew222$response_t_f)

rrfew222$zpercent=scale(rrfew222$percent)
rrfew222$zxmid=scale(rrfew222$xmid)

#RANDOM EFFECTS STRUCTURE
rrmodel_few_t_f=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                     #-zpercent:zxmid:relevel(response_t_f, "true")
                     #-zpercent:zxmid
                        +(1|workerid), data=rrfew222, REML = FALSE)

rrmodel_few_t_f_1=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                       +(1+zpercent|workerid), data=rrfew222, REML = FALSE)
#singular fit
rrmodel_few_t_f_2=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                       +(1+relevel(response_t_f, "true")|workerid), data=rrfew222, REML = FALSE)

anova(rrmodel_few_t_f, rrmodel_few_t_f_2)
#only random intercept included; the same resualts regardless radnom structure


#FIXED EFFECTS STRUCTURE
rrmodel_few_t_f_3=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                     -zpercent:zxmid:relevel(response_t_f, "true")
                     #-zpercent:zxmid
                     +(1|workerid), data=rrfew222, REML = FALSE)

anova(rrmodel_few_t_f, rrmodel_few_t_f_3)
#drop three way interaction

rrmodel_few_t_f_4=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                       -zpercent:zxmid:relevel(response_t_f, "true")
                       -zpercent:zxmid
                       +(1|workerid), data=rrfew222, REML = FALSE)
anova(rrmodel_few_t_f_4, rrmodel_few_t_f_3)
summary(rrmodel_few_t_f_4)
rrmodel_few_t_f_5=lmer(read_and_decide_time~zpercent*zxmid*response_t_f
                       -zpercent:zxmid:response_t_f
                       -zpercent:zxmid
                       +(1|workerid), data=rrfew222, REML = FALSE)

interact_plot(rrmodel_few_t_f_5, pred = "zpercent", modx = "zxmid", mod2 = "response_t_f")

########## FTH
rrdf_all_new.fth$workerid=as.factor(rrdf_all_new.fth$workerid)
rrfth2$workerid=as.factor(rrfth2$workerid)
rrfth2=left_join(rrfth2, rrdf_all_new.fth)

rrfth22 <- rrfth2
rrfth22=subset.data.frame(rrfth22, rrfth22$workerid!="28")

rrfth22$response_rec=ifelse(rrfth22$percent<rrfth22$xmid & rrfth22$response==1, "correct", 
                             ifelse(rrfth22$percent<rrfth22$xmid & rrfth22$response==0, "incorrect",
                                    ifelse(rrfth22$percent>rrfth22$xmid & rrfth22$response==1, "incorrect", "correct")))
rrfth22$response_rec=as.factor(rrfth22$response_rec)
levels(rrfth22$response_rec)

rrfth222<-subset.data.frame(rrfth22, rrfth22$response_rec=="correct")
rrfth222$response_t_f=ifelse(rrfth222$percent<rrfth222$xmid & rrfth222$response==1, "true", "false")
rrfth222$response_t_f=as.factor(rrfth222$response_t_f)
levels(rrfth222$response_t_f)

rrfth222$zpercent=scale(rrfth222$percent)
rrfth222$zxmid=scale(rrfth222$xmid)


#RANDOM EFFECTS STRUCTURE
rrmodel_fth_t_f=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                     +(1|workerid), data=rrfth222, REML = FALSE)

rrmodel_fth_t_f_2=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                       +(1+zpercent|workerid), data=rrfth222, REML = FALSE)

rrmodel_fth_t_f_3=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                     +(1+relevel(response_t_f, "true")|workerid), data=rrfth222, REML = FALSE)

anova(rrmodel_fth_t_f, rrmodel_fth_t_f_2)
#include only random intercept; the same output regardless random structure

#FIXED EFFECTS
rrmodel_fth_t_f_4=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                     -zpercent:zxmid:relevel(response_t_f, "true")
                     +(1|workerid), data=rrfth222, REML = FALSE)
anova(rrmodel_fth_t_f, rrmodel_fth_t_f_4)
rrmodel_fth_t_f_5=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                       -zpercent:zxmid:relevel(response_t_f, "true")
                       -zpercent:zxmid
                       +(1|workerid), data=rrfth222, REML = FALSE)
anova(rrmodel_fth_t_f_4, rrmodel_fth_t_f_5)
rrmodel_fth_t_f_6=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                       -zpercent:zxmid:relevel(response_t_f, "true")
                       -zpercent:zxmid
                       -zpercent:relevel(response_t_f, "true")
                       +(1|workerid), data=rrfth222, REML = FALSE)
anova(rrmodel_fth_t_f_6, rrmodel_fth_t_f_5)
rrmodel_fth_t_f_7=lmer(read_and_decide_time~zpercent*zxmid*relevel(response_t_f, "true")
                       -zpercent:zxmid:relevel(response_t_f, "true")
                       -zpercent:zxmid
                       -zpercent:relevel(response_t_f, "true")
                       -zxmid:relevel(response_t_f, "true")
                       +(1|workerid), data=rrfth222, REML = FALSE)
anova(rrmodel_fth_t_f_6, rrmodel_fth_t_f_7)
summary(rrmodel_fth_t_f_7)

########################### APPENDIX PARTICIPANTS INFO ###########################
exp1.replication.subject_information=subset.data.frame(exp1.replication.subject_information, exp1.replication.subject_information$workerid!="0")
exp1.replication.subject_information=subset.data.frame(exp1.replication.subject_information, exp1.replication.subject_information$workerid!="1")
exp1.replication.subject_information=subset.data.frame(exp1.replication.subject_information, exp1.replication.subject_information$workerid!="5")
exp1.replication.subject_information=subset.data.frame(exp1.replication.subject_information, exp1.replication.subject_information$workerid!="18")
exp1.replication.subject_information=subset.data.frame(exp1.replication.subject_information, exp1.replication.subject_information$workerid!="32")
exp1.replication.subject_information=subset.data.frame(exp1.replication.subject_information, exp1.replication.subject_information$workerid!="49")
exp1.replication.subject_information=subset.data.frame(exp1.replication.subject_information, exp1.replication.subject_information$workerid!="52")
exp1.replication.subject_information=subset.data.frame(exp1.replication.subject_information, exp1.replication.subject_information$workerid!="53")
exp1.replication.subject_information=subset.data.frame(exp1.replication.subject_information, exp1.replication.subject_information$workerid!="58")
exp1.replication.subject_information=subset.data.frame(exp1.replication.subject_information, exp1.replication.subject_information$workerid!="62")
exp1.replication.subject_information=subset.data.frame(exp1.replication.subject_information, exp1.replication.subject_information$workerid!="81")
exp1.replication.subject_information=subset.data.frame(exp1.replication.subject_information, exp1.replication.subject_information$workerid!="82")
exp1.replication.subject_information=subset.data.frame(exp1.replication.subject_information, exp1.replication.subject_information$workerid!="83")
exp1.replication.subject_information=subset.data.frame(exp1.replication.subject_information, exp1.replication.subject_information$workerid!="85")
exp1.replication.subject_information=subset.data.frame(exp1.replication.subject_information, exp1.replication.subject_information$workerid!="86")
exp1.replication.subject_information=subset.data.frame(exp1.replication.subject_information, exp1.replication.subject_information$workerid!="87")

require(dplyr)
require(stringr)
table(exp1.replication.subject_information$gender, exp1.replication.subject_information$education)
exp1.replication.subject_information=exp1.replication.subject_information %>% mutate_each(funs(str_replace_all(., "\"", "")))
exp1.replication.subject_information$age=as.numeric(exp1.replication.subject_information$age)
mean(exp1.replication.subject_information$age[which(exp1.replication.subject_information$gender=="Female")])
table(exp1.replication.subject_information$language)

################ APPENDIX PARTICIPANTS EXCLUDED #############
#Exp 1. original
exp1.replication.trials$response=ifelse(exp1.replication.trials$response=="true",1,0)
rmany=subset.data.frame(exp1.replication.trials, exp1.replication.trials$quant=="\"Many\"")
rfew=subset.data.frame(exp1.replication.trials, exp1.replication.trials$quant=="\"Few\"")
rmost=subset.data.frame(exp1.replication.trials, exp1.replication.trials$quant=="\"Most\"")
rmth=subset.data.frame(exp1.replication.trials, exp1.replication.trials$quant=="\"More than half\"")
rfth=subset.data.frame(exp1.replication.trials, exp1.replication.trials$quant=="\"Fewer than half\"")

#Many
rmany$workerid=as.factor(rmany$workerid)
res <- vector("list", 0)
for (n in levels(rmany$workerid)) {
  res[n]<-sum(ifelse(rmany$read_and_decide_time[which(rmany$workerid==n)]<300, 1, 0))
}
#0, 32, 49, 52, 53, 58, 62, 81, 82, 85, 87
rmany$read_and_decide_time[which(rmany$read_and_decide_time <300)]
rmany=subset.data.frame(rmany, rmany$read_and_decide_time>300)
table(rmany$workerid)
rmany_yes=subset.data.frame(rmany, rmany$response==1)
rcut_many_yes_1=mean(rmany_yes$read_and_decide_time)+2*sd(rmany_yes$read_and_decide_time)
rmany_yes_1=subset.data.frame(rmany_yes, rmany_yes$read_and_decide_time<rcut_many_yes_1)
rmany_no=subset.data.frame(rmany, rmany$response==0)
rcut_many_no_1=mean(rmany_no$read_and_decide_time)+2*sd(rmany_no$read_and_decide_time)
rmany_no_1=subset.data.frame(rmany_no, rmany_no$read_and_decide_time<rcut_many_no_1)
rmany2=rbind(rmany_no_1, rmany_yes_1)


#Few
rfew$workerid=as.factor(rfew$workerid)
res <- vector("list", 0)
for (n in levels(rfew$workerid)) {
  res[n]<-sum(ifelse(rfew$read_and_decide_time[which(rfew$workerid==n)]<300, 1, 0))
}
#fast gussers:
#0, 32, 49, 52, 53, 58, 62, 81, 82, 85, 87.
rfew=subset.data.frame(rfew, rfew$read_and_decide_time>300)
rfew_yes=subset.data.frame(rfew, rfew$response==1)
rcut_few_yes_1=mean(rfew_yes$read_and_decide_time)+2*sd(rfew_yes$read_and_decide_time)
rfew_yes_1=subset.data.frame(rfew_yes, rfew_yes$read_and_decide_time<rcut_few_yes_1)
rfew_no=subset.data.frame(rfew, rfew$response==0)
rcut_few_no_1=mean(rfew_no$read_and_decide_time)+2*sd(rfew_no$read_and_decide_time)
rfew_no_1=subset.data.frame(rfew_no, rfew_no$read_and_decide_time<rcut_few_no_1)
rfew2=rbind(rfew_no_1, rfew_yes_1)

# MOST
rmost$workerid=as.factor(rmost$workerid)
res <- vector("list", 0)
for (n in levels(rmost$workerid)) {
  res[n]<-sum(ifelse(rmost$read_and_decide_time[which(rmost$workerid==n)]<300, 1, 0))
}
#0, 49, 52, 53, 58, 62, 81, 82, 85, 87 
rmost=subset.data.frame(rmost, rmost$read_and_decide_time>300)
rmost_yes=subset.data.frame(rmost, rmost$response==1)
rcut_most_yes_1=mean(rmost_yes$read_and_decide_time)+2*sd(rmost_yes$read_and_decide_time)
rmost_yes_1=subset.data.frame(rmost_yes, rmost_yes$read_and_decide_time<rcut_most_yes_1)
rmost_no=subset.data.frame(rmost, rmost$response==0)
rcut_most_no_1=mean(rmost_no$read_and_decide_time)+2*sd(rmost_no$read_and_decide_time)
rmost_no_1=subset.data.frame(rmost_no, rmost_no$read_and_decide_time<rcut_most_no_1)
rmost2=rbind(rmost_no_1, rmost_yes_1)


### More than half
rmth$workerid=as.factor(rmth$workerid)
res <- vector("list", 0)
for (n in levels(rmth$workerid)) {
  res[n]<-sum(ifelse(rmth$read_and_decide_time[which(rmth$workerid==n)]<300, 1, 0))
}
#0, 32, 49, 52, 53, 58, 62, 81, 82, 85, 87
rmth=subset.data.frame(rmth, rmth$read_and_decide_time>300)
rmth_yes=subset.data.frame(rmth, rmth$response==1)
rcut_mth_yes_1=mean(rmth_yes$read_and_decide_time)+2*sd(rmth_yes$read_and_decide_time)
rmth_yes_1=subset.data.frame(rmth_yes, rmth_yes$read_and_decide_time<rcut_mth_yes_1)
rmth_no=subset.data.frame(rmth, rmth$response==0)
rcut_mth_no_1=mean(rmth_no$read_and_decide_time)+2*sd(rmth_no$read_and_decide_time)
rmth_no_1=subset.data.frame(rmth_no, rmth_no$read_and_decide_time<rcut_mth_no_1)
rmth2=rbind(rmth_no_1, rmth_yes_1)


### Fewer than half
rfth$workerid=as.factor(rfth$workerid)
res <- vector("list", 0)
for (n in levels(rfth$workerid)) {
  res[n]<-sum(ifelse(rfth$read_and_decide_time[which(rfth$workerid==n)]<300, 1, 0))
}
#0, 49, 52, 53, 58, 62, 81, 82, 85, 87
rfth=subset.data.frame(rfth, rfth$read_and_decide_time>300)
rfth_yes=subset.data.frame(rfth, rfth$response==1)
rcut_fth_yes_1=mean(rfth_yes$read_and_decide_time)+2*sd(rfth_yes$read_and_decide_time)
rfth_yes_1=subset.data.frame(rfth_yes, rfth_yes$read_and_decide_time<rcut_fth_yes_1)
rfth_no=subset.data.frame(rfth, rfth$response==0)
rcut_fth_no_1=mean(rfth_no$read_and_decide_time)+2*sd(rfth_no$read_and_decide_time)
rfth_no_1=subset.data.frame(rfth_no, rfth_no$read_and_decide_time<rcut_fth_no_1)
rfth2=rbind(rfth_no_1, rfth_yes_1)


#FG: 0, 32, 49, 52, 53, 58, 62, 81, 82, 85, 87
####### EXCLUDED
rm <- glmer(response ~ percent+(1+percent|workerid), data = rmany2, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(rm)
coef(rm)
#5, 39, 58, 83, 85, 86, 87

rm_most <- glmer(response ~ percent+(1+percent|workerid), data = rmost2, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(rm_most)
coef(rm_most)
#39, 52, 53, 81, 83

rm_2_mth=glmer(response~percent+(1+percent|workerid), data=rmth2, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(rm_2_mth)
coef(rm_2_mth)
#5, 18, 39, 52, 83

rm_2_fth=glmer(response~percent+(1+percent|workerid), data=rfth2, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(rm_2_fth)
coef(rm_2_fth)
#0, 1, 32, 39, 52, 85, 86, 87

rm_2_few=glmer(response~percent+(1+percent|workerid), data=rfew2, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(rm_2_few)
coef(rm_2_few)
#86; 62; 53; 39; 32; 5; 1

#GLMER: 0, 1, 5, 18, 32, 39, 52, 53, 62, 81, 83, 85, 86, 87

##################### REPLICATION ###################
exp1.replication.v2.trials$response=ifelse(exp1.replication.v2.trials$response=="false", 0,1)
##### EXCLUE PARTICIPANT THAT PARTICIPATED MORE THAN ONE TIME
exp1.replication.v2.trials=subset.data.frame(exp1.replication.v2.trials, exp1.replication.v2.trials$workerid!="3")
exp1.replication.v2.trials=subset.data.frame(exp1.replication.v2.trials, exp1.replication.v2.trials$workerid!="4")
exp1.replication.v2.trials=subset.data.frame(exp1.replication.v2.trials, exp1.replication.v2.trials$workerid!="5")
exp1.replication.v2.trials=subset.data.frame(exp1.replication.v2.trials, exp1.replication.v2.trials$workerid!="9")
exp1.replication.v2.trials=subset.data.frame(exp1.replication.v2.trials, exp1.replication.v2.trials$workerid!="10")
exp1.replication.v2.trials=subset.data.frame(exp1.replication.v2.trials, exp1.replication.v2.trials$workerid!="16")
exp1.replication.v2.trials=subset.data.frame(exp1.replication.v2.trials, exp1.replication.v2.trials$workerid!="21")
exp1.replication.v2.trials=subset.data.frame(exp1.replication.v2.trials, exp1.replication.v2.trials$workerid!="30")
exp1.replication.v2.trials=subset.data.frame(exp1.replication.v2.trials, exp1.replication.v2.trials$workerid!="33")
exp1.replication.v2.trials=subset.data.frame(exp1.replication.v2.trials, exp1.replication.v2.trials$workerid!="34")
exp1.replication.v2.trials=subset.data.frame(exp1.replication.v2.trials, exp1.replication.v2.trials$workerid!="48")
exp1.replication.v2.trials=subset.data.frame(exp1.replication.v2.trials, exp1.replication.v2.trials$workerid!="56")
exp1.replication.v2.trials=subset.data.frame(exp1.replication.v2.trials, exp1.replication.v2.trials$workerid!="69")
exp1.replication.v2.trials=subset.data.frame(exp1.replication.v2.trials, exp1.replication.v2.trials$workerid!="81")


rrmany=subset.data.frame(exp1.replication.v2.trials, exp1.replication.v2.trials$quant=="\"Many\"")
rrfew=subset.data.frame(exp1.replication.v2.trials, exp1.replication.v2.trials$quant=="\"Few\"")
rrmost=subset.data.frame(exp1.replication.v2.trials, exp1.replication.v2.trials$quant=="\"Most\"")
rrmth=subset.data.frame(exp1.replication.v2.trials, exp1.replication.v2.trials$quant=="\"More than half\"")
rrfth=subset.data.frame(exp1.replication.v2.trials, exp1.replication.v2.trials$quant=="\"Fewer than half\"")


#MANY
rrmany$workerid=as.factor(rrmany$workerid)
res <- vector("list", 0)
for (n in levels(rrmany$workerid)) {
  res[n]<-sum(ifelse(rrmany$read_and_decide_time[which(rrmany$workerid==n)]<300, 1, 0))
}
#29, 58, 70, 74, 82
rrmany$read_and_decide_time[which(rrmany$read_and_decide_time <300)]
rrmany=subset.data.frame(rrmany, rrmany$read_and_decide_time>300)
rrmany_yes=subset.data.frame(rrmany, rrmany$response==1)
rrcut_many_yes_1=mean(rrmany_yes$read_and_decide_time)+2*sd(rrmany_yes$read_and_decide_time)
rrmany_yes_1=subset.data.frame(rrmany_yes, rrmany_yes$read_and_decide_time<rrcut_many_yes_1)
rrmany_no=subset.data.frame(rrmany, rrmany$response==0)
rrcut_many_no_1=mean(rrmany_no$read_and_decide_time)+2*sd(rrmany_no$read_and_decide_time)
rrmany_no_1=subset.data.frame(rrmany_no, rrmany_no$read_and_decide_time<rrcut_many_no_1)
rrmany2=rbind(rrmany_no_1, rrmany_yes_1)


#FEW
rrfew$workerid=as.factor(rrfew$workerid)
res <- vector("list", 0)
for (n in levels(rrfew$workerid)) {
  res[n]<-sum(ifelse(rrfew$read_and_decide_time[which(rrfew$workerid==n)]<300, 1, 0))
}
#25, 29, 50, 70, 74, 82
rrfew=subset.data.frame(rrfew, rrfew$read_and_decide_time>300)
rrfew_yes=subset.data.frame(rrfew, rrfew$response==1)
rrcut_few_yes_1=mean(rrfew_yes$read_and_decide_time)+2*sd(rrfew_yes$read_and_decide_time)
rrfew_yes_1=subset.data.frame(rrfew_yes, rrfew_yes$read_and_decide_time<rrcut_few_yes_1)
rrfew_no=subset.data.frame(rrfew, rrfew$response==0)
rrcut_few_no_1=mean(rrfew_no$read_and_decide_time)+2*sd(rrfew_no$read_and_decide_time)
rrfew_no_1=subset.data.frame(rrfew_no, rrfew_no$read_and_decide_time<rrcut_few_no_1)
rrfew2=rbind(rrfew_no_1, rrfew_yes_1)
#MOST
rrmost$workerid=as.factor(rrmost$workerid)
res <- vector("list", 0)
for (n in levels(rrmost$workerid)) {
  res[n]<-sum(ifelse(rrmost$read_and_decide_time[which(rrmost$workerid==n)]<300, 1, 0))
}
#25, 29, 58, 70, 74, 82 
rrmost=subset.data.frame(rrmost, rrmost$read_and_decide_time>300)
rrmost_yes=subset.data.frame(rrmost, rrmost$response==1)
rrcut_most_yes_1=mean(rrmost_yes$read_and_decide_time)+2*sd(rrmost_yes$read_and_decide_time)
rrmost_yes_1=subset.data.frame(rrmost_yes, rrmost_yes$read_and_decide_time<rrcut_most_yes_1)
rrmost_no=subset.data.frame(rrmost, rrmost$response==0)
rrcut_most_no_1=mean(rrmost_no$read_and_decide_time)+2*sd(rrmost_no$read_and_decide_time)
rrmost_no_1=subset.data.frame(rrmost_no, rrmost_no$read_and_decide_time<rrcut_most_no_1)
rrmost2=rbind(rrmost_no_1, rrmost_yes_1)


#MTH
rrmth$workerid=as.factor(rrmth$workerid)
res <- vector("list", 0)
for (n in levels(rrmth$workerid)) {
  res[n]<-sum(ifelse(rrmth$read_and_decide_time[which(rrmth$workerid==n)]<300, 1, 0))
}
#29, 58, 70, 74, 82
rrmth=subset.data.frame(rrmth, rrmth$read_and_decide_time>300)
rrmth_yes=subset.data.frame(rrmth, rrmth$response==1)
rrcut_mth_yes_1=mean(rrmth_yes$read_and_decide_time)+2*sd(rrmth_yes$read_and_decide_time)
rrmth_yes_1=subset.data.frame(rrmth_yes, rrmth_yes$read_and_decide_time<rrcut_mth_yes_1)
rrmth_no=subset.data.frame(rrmth, rrmth$response==0)
rrcut_mth_no_1=mean(rrmth_no$read_and_decide_time)+2*sd(rrmth_no$read_and_decide_time)
rrmth_no_1=subset.data.frame(rrmth_no, rrmth_no$read_and_decide_time<rrcut_mth_no_1)
rrmth2=rbind(rrmth_no_1, rrmth_yes_1)


#FTH
rrfth$workerid=as.factor(rrfth$workerid)
res <- vector("list", 0)
for (n in levels(rrfth$workerid)) {
  res[n]<-sum(ifelse(rrfth$read_and_decide_time[which(rrfth$workerid==n)]<300, 1, 0))
}
#25, 29, 58, 70, 74, 82, 
rrfth=subset.data.frame(rrfth, rrfth$read_and_decide_time>300)
rrfth_yes=subset.data.frame(rrfth, rrfth$response==1)
rrcut_fth_yes_1=mean(rrfth_yes$read_and_decide_time)+2*sd(rrfth_yes$read_and_decide_time)
rrfth_yes_1=subset.data.frame(rrfth_yes, rrfth_yes$read_and_decide_time<rrcut_fth_yes_1)
rrfth_no=subset.data.frame(rrfth, rrfth$response==0)
rrcut_fth_no_1=mean(rrfth_no$read_and_decide_time)+2*sd(rrfth_no$read_and_decide_time)
rrfth_no_1=subset.data.frame(rrfth_no, rrfth_no$read_and_decide_time<rrcut_fth_no_1)
rrfth2=rbind(rrfth_no_1, rrfth_yes_1)

#### FG: 25, 29, 58, 70, 74, 82

####### EXCLUDED PARTICIPANTS #########
m <- glmer(response ~ percent+(1+percent|workerid), data = rrmany2, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(m)
coef(m)
#1, 46, 72

rrm_2_most=glmer(response~percent+(1+percent|workerid), data=rrmost2, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(rrm_2_most)
coef(rrm_2_most)
#1, 25, 29, 70, 72 

rrm_2_mth=glmer(response~percent+(1+percent|workerid), data=rrmth2, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(rrm_2_mth)
coef(rrm_2_mth)
#1, 25, 43, 72, 
plot(rrmany2$percent[which(rrmany2$workerid=="43")], rrmany2$response[which(rrmany2$workerid=="43")])

rrm_2_fth=glmer(response~percent+(1+percent|workerid), data=rrfth2, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(rrm_2_fth)
coef(rrm_2_fth)
#1, 25, 29, 46, 72

rrm_2_few=glmer(response~percent+(1+percent|workerid), data=rrfew2, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(rrm_2_few)
coef(rrm_2_few)
#1, 72
#GLMER: 1, 25, 29, 43, 46, 70, 72

############ PARTICIPANTS DESCRIPTION ########
exp1.replication.v2.subject_information=exp1.replication.v2.subject_information %>% mutate_each(funs(str_replace_all(., "\"", "")))
exp1.replication.v2.subject_information=subset.data.frame(exp1.replication.v2.subject_information, exp1.replication.v2.subject_information$workerid!="1")
exp1.replication.v2.subject_information=subset.data.frame(exp1.replication.v2.subject_information, exp1.replication.v2.subject_information$workerid!="25")
exp1.replication.v2.subject_information=subset.data.frame(exp1.replication.v2.subject_information, exp1.replication.v2.subject_information$workerid!="29")
exp1.replication.v2.subject_information=subset.data.frame(exp1.replication.v2.subject_information, exp1.replication.v2.subject_information$workerid!="43")
exp1.replication.v2.subject_information=subset.data.frame(exp1.replication.v2.subject_information, exp1.replication.v2.subject_information$workerid!="46")
exp1.replication.v2.subject_information=subset.data.frame(exp1.replication.v2.subject_information, exp1.replication.v2.subject_information$workerid!="58")
exp1.replication.v2.subject_information=subset.data.frame(exp1.replication.v2.subject_information, exp1.replication.v2.subject_information$workerid!="70")
exp1.replication.v2.subject_information=subset.data.frame(exp1.replication.v2.subject_information, exp1.replication.v2.subject_information$workerid!="73")
exp1.replication.v2.subject_information=subset.data.frame(exp1.replication.v2.subject_information, exp1.replication.v2.subject_information$workerid!="74")
exp1.replication.v2.subject_information=subset.data.frame(exp1.replication.v2.subject_information, exp1.replication.v2.subject_information$workerid!="82")

exp1.replication.v2.subject_information=subset.data.frame(exp1.replication.v2.subject_information, exp1.replication.v2.subject_information$workerid!="3")
exp1.replication.v2.subject_information=subset.data.frame(exp1.replication.v2.subject_information, exp1.replication.v2.subject_information$workerid!="4")
exp1.replication.v2.subject_information=subset.data.frame(exp1.replication.v2.subject_information, exp1.replication.v2.subject_information$workerid!="5")
exp1.replication.v2.subject_information=subset.data.frame(exp1.replication.v2.subject_information, exp1.replication.v2.subject_information$workerid!="9")
exp1.replication.v2.subject_information=subset.data.frame(exp1.replication.v2.subject_information, exp1.replication.v2.subject_information$workerid!="10")
exp1.replication.v2.subject_information=subset.data.frame(exp1.replication.v2.subject_information, exp1.replication.v2.subject_information$workerid!="16")
exp1.replication.v2.subject_information=subset.data.frame(exp1.replication.v2.subject_information, exp1.replication.v2.subject_information$workerid!="21")
exp1.replication.v2.subject_information=subset.data.frame(exp1.replication.v2.subject_information, exp1.replication.v2.subject_information$workerid!="30")
exp1.replication.v2.subject_information=subset.data.frame(exp1.replication.v2.subject_information, exp1.replication.v2.subject_information$workerid!="33")
exp1.replication.v2.subject_information=subset.data.frame(exp1.replication.v2.subject_information, exp1.replication.v2.subject_information$workerid!="34")
exp1.replication.v2.subject_information=subset.data.frame(exp1.replication.v2.subject_information, exp1.replication.v2.subject_information$workerid!="48")
exp1.replication.v2.subject_information=subset.data.frame(exp1.replication.v2.subject_information, exp1.replication.v2.subject_information$workerid!="56")
exp1.replication.v2.subject_information=subset.data.frame(exp1.replication.v2.subject_information, exp1.replication.v2.subject_information$workerid!="69")
exp1.replication.v2.subject_information=subset.data.frame(exp1.replication.v2.subject_information, exp1.replication.v2.subject_information$workerid!="81")

table(exp1.replication.v2.subject_information$gender)
exp1.replication.v2.subject_information$age=as.numeric(exp1.replication.v2.subject_information$age)
sd(exp1.replication.v2.subject_information$age[which(exp1.replication.v2.subject_information$gender=="Female")])
table(exp1.replication.v2.subject_information$gender, exp1.replication.v2.subject_information$education)
table(exp1.replication.v2.subject_information$language)



############################## EXPORT PLOTS ##############################
library(ggplot2)
library(scales)
library(plyr)
library(dplyr)
most_x=subset.data.frame(rrdf_all_new_plot, rrdf_all_new_plot$quantifier=="most")
mth_x=subset.data.frame(rrdf_all_new_plot, rrdf_all_new_plot$quantifier=="MTH")
mth_most_x=rbind(most_x, mth_x)
mth_most_mu <- ddply(mth_most_x, "quantifier", summarise, grp.mean=mean(xmid))

rmost_x=subset.data.frame(df_all_new_plot, df_all_new_plot$quantifier=="most")
rmth_x=subset.data.frame(df_all_new_plot, df_all_new_plot$quantifier=="MTH")
rmth_most_x=rbind(rmost_x, rmth_x)
rmth_most_mu <- ddply(rmth_most_x, "quantifier", summarise, grp.mean=mean(xmid))

rplot_most_mth=ggplot(rmth_most_x, aes(x=xmid, fill=quantifier)) +
  geom_histogram(aes(y=..count../sum(..count..)*2),binwidth=3, alpha=.4, position="identity")+
  geom_vline(data=rmth_most_mu, aes(xintercept=grp.mean, color=quantifier),
             linetype="dashed")+
  scale_x_continuous(name = "Individual thresholds",
                     limits = c(0,80)
                     #breaks = seq(0,100,10)
                     )+
  scale_y_continuous(name= "Proportion",
                     limits = c(0,0.6))+
  theme_light()+
  theme(
    axis.title.x = element_text(size=10, face="bold"),
    axis.title.y = element_text(size=10, face="bold"),
    legend.title = element_text(size=10, face="bold"),
    legend.text = element_text(size=10),
    axis.text.y = element_text(size=10),
    axis.text.x = element_text(size=10)
  )+annotate("text", x = 35, y = 0.5, label = "most = 52%")+
  annotate("text", x = 35, y = 0.45, label = "MTH = 49%")+
  scale_color_manual(values=c("#999999", "#E69F00"))+
  scale_fill_manual(values=c("#999999", "#E69F00"))


plot_most_mth=ggplot(mth_most_x, aes(x=xmid, fill=quantifier)) +
  geom_histogram(aes(y=..count../sum(..count..)*2),binwidth=3, alpha=.4, position="identity")+
  geom_vline(data=mth_most_mu, aes(xintercept=grp.mean, color=quantifier),
             linetype="dashed")+
  scale_x_continuous(name = "Individual thresholds",
                     limits = c(0,80)
                    # breaks = seq(0,100,10)
  )+
  scale_y_continuous(name= "Proportion",
                     limits = c(0,0.6))+
  #xlim(0,70)+
  theme_light()+
  theme(
    axis.title.x = element_text(size=10, face="bold"),
    axis.title.y = element_text(size=10, face="bold"),
    legend.title = element_text(size=10, face="bold"),
    legend.text = element_text(size=10),
    axis.text.y = element_text(size=10),
    axis.text.x = element_text(size=10)
  )+annotate("text", x = 40, y = 0.5, label = "most = 51%")+
  annotate("text", x = 40, y = 0.45, label = "MTH = 49%")+
  scale_color_manual(values=c("#999999", "#E69F00"))+
  scale_fill_manual(values=c("#999999", "#E69F00"))



library(magrittr)
library(ggpubr)
ggarrange(rplot_most_mth, plot_most_mth,labels = c("A", "B"),
          font.label = list(size = 11),
          vjust=0.9,
          ncol = 2, nrow = 1,
          common.legend = TRUE, legend = "top")
getwd()
setwd("/Users/sramoto/Desktop")
ggsave("mth_most.png")

fth_x=subset.data.frame(rrdf_all_new_plot, rrdf_all_new_plot$quantifier=="FTH")
mth_fth_x=rbind(fth_x, mth_x)
mth_fth_mu <- ddply(mth_fth_x, "quantifier", summarise, grp.mean=mean(xmid))


rfth_x=subset.data.frame(df_all_new_plot, df_all_new_plot$quantifier=="FTH")
rmth_fth_x=rbind(rfth_x, rmth_x)
rmth_fth_mu <- ddply(rmth_fth_x, "quantifier", summarise, grp.mean=mean(xmid))

rplot_mth_fth=ggplot(rmth_fth_x, aes(x=xmid, fill=quantifier)) +
  geom_histogram(aes(y=..count../sum(..count..)*2),binwidth=3, alpha=.4, position="identity")+
  geom_vline(data=rmth_fth_mu, aes(xintercept=grp.mean, color=quantifier),
             linetype="dashed")+
  scale_x_continuous(name = "Individual thresholds",
                     limits = c(0,80))+
  scale_y_continuous(name= "Proportion",
                     limits = c(0,0.6))+
  theme_light()+
  theme(
    axis.title.x = element_text(size=10, face="bold"),
    axis.title.y = element_text(size=10, face="bold"),
    legend.title = element_text(size=10, face="bold"),
    legend.text = element_text(size=10),
    axis.text.y = element_text(size=10),
    axis.text.x = element_text(size=10)
  )+annotate("text", x = 35, y = 0.5, label = "FTH = 48%")+
  annotate("text", x = 35, y = 0.45, label = "MTH = 49%")+
  scale_color_manual(values=c("#66CC99", "#E69F00"))+
  scale_fill_manual(values=c("#66CC99", "#E69F00"))


plot_mth_fth=ggplot(mth_fth_x, aes(x=xmid, fill=quantifier)) +
  geom_histogram(aes(y=..count../sum(..count..)*2),binwidth=3, alpha=.4, position="identity")+
  geom_vline(data=mth_fth_mu, aes(xintercept=grp.mean, color=quantifier),
             linetype="dashed")+
  scale_x_continuous(name = "Individual thresholds",
                     limits = c(0,80))+
  scale_y_continuous(name= "Proportion",
                     limits = c(0,0.6))+
  theme_light()+
  theme(
    axis.title.x = element_text(size=10, face="bold"),
    axis.title.y = element_text(size=10, face="bold"),
    legend.title = element_text(size=10, face="bold"),
    legend.text = element_text(size=10),
    axis.text.y = element_text(size=10),
    axis.text.x = element_text(size=10)
  )+annotate("text", x = 35, y = 0.5, label = "FTH = 48%")+
  annotate("text", x = 35, y = 0.45, label = "MTH = 49%")+
  scale_color_manual(values=c("#66CC99", "#E69F00"))+
  scale_fill_manual(values=c("#66CC99", "#E69F00"))

ggarrange(rplot_mth_fth, plot_mth_fth,labels = c("A", "B"),
          font.label = list(size = 11),
          vjust=0.9,
          ncol = 2, nrow = 1,
          common.legend = TRUE, legend = "top")

ggsave("mth_fth.png")

many_x=subset.data.frame(rrdf_all_new_plot, rrdf_all_new_plot$quantifier=="many")
many_most_x=rbind(many_x, most_x)
many_most_mu <- ddply(many_most_x, "quantifier", summarise, grp.mean=mean(xmid))

rmany_x=subset.data.frame(df_all_new_plot, df_all_new_plot$quantifier=="many")
rmany_most_x=rbind(rmany_x, rmost_x)
rmany_most_mu <- ddply(rmany_most_x, "quantifier", summarise, grp.mean=mean(xmid))

plot_many_most=ggplot(many_most_x, aes(x=xmid, fill=quantifier)) +
  geom_histogram(aes(y=..count../sum(..count..)*2),binwidth=3, alpha=.4, position="identity")+
  geom_vline(data=many_most_mu, aes(xintercept=grp.mean, color=quantifier),
             linetype="dashed")+
  scale_x_continuous(name = "Individual thresholds",
                     limits = c(0,80))+
  scale_y_continuous(name= "Proportion",
                     limits = c(0,0.6))+
  theme_light()+
  theme(
    axis.title.x = element_text(size=10, face="bold"),
    axis.title.y = element_text(size=10, face="bold"),
    legend.title = element_text(size=10, face="bold"),
    legend.text = element_text(size=10),
    axis.text.y = element_text(size=10),
    axis.text.x = element_text(size=10)
  )+annotate("text", x = 27, y = 0.5, label = "many = 41%")+
  annotate("text", x = 27, y = 0.45, label = "most = 51%")+
  scale_color_manual(values=c("#CC6666", "#999999"))+
  scale_fill_manual(values=c("#CC6666", "#999999"))

rplot_many_most=ggplot(rmany_most_x, aes(x=xmid, fill=quantifier)) +
  geom_histogram(aes(y=..count../sum(..count..)*2),binwidth=3, alpha=.4, position="identity")+
  geom_vline(data=rmany_most_mu, aes(xintercept=grp.mean, color=quantifier),
             linetype="dashed")+
  scale_x_continuous(name = "Individual thresholds",
                     limits = c(0,80))+
  scale_y_continuous(name= "Proportion",
                     limits = c(0,0.6))+
  theme_light()+
  theme(
    axis.title.x = element_text(size=10, face="bold"),
    axis.title.y = element_text(size=10, face="bold"),
    legend.title = element_text(size=10, face="bold"),
    legend.text = element_text(size=10),
    axis.text.y = element_text(size=10),
    axis.text.x = element_text(size=10)
  )+annotate("text", x = 30, y = 0.5, label = "many = 44%")+
  annotate("text", x = 30, y = 0.45, label = "most = 53%")+
  scale_color_manual(values=c("#CC6666", "#999999"))+
  scale_fill_manual(values=c("#CC6666", "#999999"))

ggarrange(rplot_many_most, plot_many_most,labels = c("A", "B"),
          font.label = list(size = 11),
          vjust=0.9,
          ncol = 2, nrow = 1,
          common.legend = TRUE, legend = "top")

ggsave("many_most.png")


few_x=subset.data.frame(rrdf_all_new_plot, rrdf_all_new_plot$quantifier=="few")
many_few_x=rbind(many_x, few_x)
many_few_mu <- ddply(many_few_x, "quantifier", summarise, grp.mean=mean(xmid))

rfew_x=subset.data.frame(df_all_new_plot, df_all_new_plot$quantifier=="few")
rmany_few_x=rbind(rmany_x, rfew_x)
rmany_few_mu <- ddply(rmany_few_x, "quantifier", summarise, grp.mean=mean(xmid))

plot_many_few=ggplot(many_few_x, aes(x=xmid, fill=quantifier)) +
  geom_histogram(aes(y=..count../sum(..count..)*2),binwidth=3, alpha=.4, position="identity")+
  geom_vline(data=many_few_mu, aes(xintercept=grp.mean, color=quantifier),
             linetype="dashed")+
  scale_x_continuous(name = "Individual thresholds",
                     limits = c(0,80))+
  scale_y_continuous(name= "Proportion",
                     limits = c(0,0.6))+
  theme_light()+
  theme(
    axis.title.x = element_text(size=10, face="bold"),
    axis.title.y = element_text(size=10, face="bold"),
    legend.title = element_text(size=10, face="bold"),
    legend.text = element_text(size=10),
    axis.text.y = element_text(size=10),
    axis.text.x = element_text(size=10)
  )+annotate("text", x = 25, y = 0.5, label = "many = 41%")+
  annotate("text", x = 25, y = 0.45, label = "few = 39%")+
  scale_color_manual(values=c("#CC6666", "#56B4E9"))+
  scale_fill_manual(values=c("#CC6666", "#56B4E9"))


rplot_many_few=ggplot(rmany_few_x, aes(x=xmid, fill=quantifier)) +
  geom_histogram(aes(y=..count../sum(..count..)*2),binwidth=3, alpha=.4, position="identity")+
  geom_vline(data=rmany_few_mu, aes(xintercept=grp.mean, color=quantifier),
             linetype="dashed")+
  scale_x_continuous(name = "Individual thresholds",
                     limits = c(0,80))+
  scale_y_continuous(name= "Proportion",
                     limits = c(0,0.6))+
  theme_light()+
  theme(
    axis.title.x = element_text(size=10, face="bold"),
    axis.title.y = element_text(size=10, face="bold"),
    legend.title = element_text(size=10, face="bold"),
    legend.text = element_text(size=10),
    axis.text.y = element_text(size=10),
    axis.text.x = element_text(size=10)
  )+annotate("text", x = 25, y = 0.5, label = "many = 44%")+
  annotate("text", x = 25, y = 0.45, label = "few = 39%")+
  scale_color_manual(values=c("#CC6666", "#56B4E9"))+
  scale_fill_manual(values=c("#CC6666", "#56B4E9"))

ggarrange(rplot_many_few, plot_many_few,labels = c("A", "B"),
          font.label = list(size = 11),
          vjust=0.9,
          ncol = 2, nrow = 1,
          common.legend = TRUE, legend = "top")

ggsave("mant_few.png")

#ALL HISTOGRAMS TOGETHER
rmu <- ddply(df_all_new_plot, "quantifier", summarise, grp.sd=sd(xmid))
head(rmu)
rrmu <- ddply(rrdf_all_new_plot, "quantifier", summarise, grp.mean=mean(xmid))
head(rrmu)

exp=ggplot(df_all_new_plot, aes(x=xmid, fill=quantifier)) +
  geom_histogram(aes(y=..count../sum(..count..)*5),binwidth=3, alpha=.5, position="identity")+
  geom_vline(data=rmu, aes(xintercept=grp.mean, color=quantifier),
             linetype="dashed")+
  scale_x_continuous(name = "Individual thresholds",
                     breaks = seq(0,100, 10))+
  scale_y_continuous(name= "Proportion",
                     limits = c(0,0.6))+
  theme_light()+
  theme(
    axis.title.x = element_text(size=8, face="bold"),
    axis.title.y = element_text(size=9, face="bold"),
    legend.title = element_text(size=8, face="bold"),
    legend.text = element_text(size=8),
    axis.text.y = element_text(size=8),
    axis.text.x = element_text(size=8)
  )+
  facet_wrap(.~quantifier)
ggsave("thresholds_1.png")

rep=ggplot(rrdf_all_new_plot, aes(x=xmid, fill=quantifier)) +
  geom_histogram(aes(y=..count../sum(..count..)*5),binwidth=3, alpha=.5, position="identity")+
  geom_vline(data=rrmu, aes(xintercept=grp.mean, color=quantifier),
             linetype="dashed")+
  scale_x_continuous(name = "Individual thresholds",
                     breaks = seq(0,100, 10))+
  scale_y_continuous(name= "Proportion",
                     limits = c(0, 0.6))+
  theme_bw()+
  theme(
    axis.title.x = element_text(size=8, face="bold"),
    axis.title.y = element_text(size=9, face="bold"),
    legend.title = element_text(size=8, face="bold"),
    legend.text = element_text(size=8),
    axis.text.y = element_text(size=8),
    axis.text.x = element_text(size=8)
  )+
  facet_wrap(.~quantifier)
  #facet.grid(.~quantifier)
ggsave("thresholds_2.png")

library(magrittr)
library(ggpubr)
ggarrange(exp, rep,labels = c("A", "B"),
          font.label = list(size = 11),
          vjust=0.9,
          ncol = 1, nrow = 2,
          common.legend = TRUE, legend = "right")




###################### PLOT MODEL
#Exp 1.
library(lme4)
library(lmerTest)
library(ggplot2)
rmany2222.plot=subset.data.frame(rmany2222, rmany2222$response_t_f=="true")
rmost2222.plot=subset.data.frame(rmost2222, rmost2222$response_t_f=="true")
rfew2222.plot=subset.data.frame(rfew2222, rfew2222$response_t_f=="true")
rfth2222.plot=subset.data.frame(rfth2222, rfth2222$response_t_f=="true")
rmth2222.plot=subset.data.frame(rmth2222, rmth2222$response_t_f=="true")

rmany_above=apply(with(rmany2222.plot, tapply(read_and_decide_time, list(workerid, percent), mean)), 2, mean, na.rm=TRUE)
rmost_above=apply(with(rmost2222.plot, tapply(read_and_decide_time, list(workerid, percent), mean)), 2, mean, na.rm=TRUE)
rfew_above=apply(with(rfew2222.plot, tapply(read_and_decide_time, list(workerid, percent), mean)), 2, mean, na.rm=TRUE)
rfth_above=apply(with(rfth2222.plot, tapply(read_and_decide_time, list(workerid, percent), mean)), 2, mean, na.rm=TRUE)
rmth_above=apply(with(rmth2222.plot, tapply(read_and_decide_time, list(workerid, percent), mean)), 2, mean, na.rm=TRUE)


rmany_df=data.frame(percent=c(9,11,22:49,51:99), mean=rmany_above, Q=rep(c("many"), each=79), truth=rep(c("true"), each=79))
rmost_df=data.frame(percent=c(25,42,45,47:49,51:98), mean=rmost_above, Q=rep(c("most"), each=54), truth=rep(c("true"), each=54))
rfew_df=data.frame(percent=c(1:49, 52), mean=rfew_above, Q=rep(c("few"), each=50), truth=rep(c("true"), each=50))
rfth_df=data.frame(percent=c(1:49, 51:52), mean=rfth_above, Q=rep(c("FTH"), each=51), truth=rep(c("true"), each=51))
rmth_df=data.frame(percent=c(39, 51:98), mean=rmth_above, Q=rep(c("MTH"), each=49), truth=rep(c("true"), each=49))

rmost2222$resp_AB=ifelse(rmost2222$response_t_f=="true", "A", "B")
rmany2222$resp_AB=ifelse(rmany2222$response_t_f=="true", "A", "B")
rfew2222$resp_AB=ifelse(rfew2222$response_t_f=="true", "A", "B")
rfth2222$resp_AB=ifelse(rfth2222$response_t_f=="true", "A", "B")
rmth2222$resp_AB=ifelse(rmth2222$response_t_f=="true", "A", "B")

rmany3=subset.data.frame(rmany2222, rmany2222$resp_AB=="A")
rmost3=subset.data.frame(rmost2222, rmost2222$resp_AB=="A")
rfew3=subset.data.frame(rfew2222, rfew2222$resp_AB=="A")
rfth3=subset.data.frame(rfth2222, rfth2222$resp_AB=="A")
rmth3=subset.data.frame(rmth2222, rmth2222$resp_AB=="A")

rmodel_most_t_f_1=lmer(read_and_decide_time~percent*xmid
                        +(1|workerid), data=rmost3, REML = FALSE)

rmodel_many_t_f_1=lmer(read_and_decide_time~percent*xmid
                        +(1|workerid), data=rmany3, REML = FALSE)

rmodel_few_t_f_1=lmer(read_and_decide_time~percent*xmid
                      +(1|workerid), data=rfew3, REML = FALSE)

rmodel_fth_t_f_1=lmer(read_and_decide_time~percent*xmid
                      +(1|workerid), data=rfth3, REML = FALSE)

rmodel_mth_t_f_1=lmer(read_and_decide_time~percent*xmid
                      +(1|workerid), data=rmth3, REML = FALSE)

library(effects)
# quantifier grp.mean
#1       many 43.76468
#2        few 39.42434
#3        FTH 47.99780
#4       most 52.62153
#5        MTH 49.48634

#quantifier   grp.sd
#1       many 9.748589
#2        few 8.744904
#3        FTH 7.430447
#4       most 7.330100
#5        MTH 6.501838


rInter.SD.few <- effect(c("percent*xmid"), rmodel_few_t_f_1,
                       xlevels=list(percent=c(1, 20, 49, 60, 99),
                                    xmid=c(30, 39, 48))) 
rInter.SD.few <- as.data.frame(rInter.SD.few)
rInter.SD.few$xmid<-factor(rInter.SD.few$xmid,
                          levels=c(30, 39, 48),
                          labels=c("1 SD Below Mean", "Mean", "1 SD Above Mean"))

rInter.SD <- effect(c("percent*xmid"), rmodel_many_t_f_1,
                   xlevels=list(percent=c(1,20, 49, 60, 99),
                                xmid=c(34, 44, 54))) 
rInter.SD <- as.data.frame(rInter.SD)
rInter.SD$xmid<-factor(rInter.SD$xmid,
                      levels=c(34, 44, 54),
                      labels=c("1 SD Below Mean", "Mean", "1 SD Above Mean"))

rInter.SD.most <- effect(c("percent*xmid"), rmodel_most_t_f_1,
                        xlevels=list(percent=c(1,20, 49, 60, 99),
                                     xmid=c(46, 53, 60))) 
rInter.SD.most <- as.data.frame(rInter.SD.most)
rInter.SD.most$xmid<-factor(rInter.SD.most$xmid,
                           levels=c(46, 53, 60),
                           labels=c("1 SD Below Mean", "Mean", "1 SD Above Mean"))

rInter.SD.mth <- effect(c("percent*xmid"), rmodel_mth_t_f_1,
                         xlevels=list(percent=c(1,20, 49, 60, 99),
                                      xmid=c(43, 49, 55))) 
rInter.SD.mth <- as.data.frame(rInter.SD.mth)
rInter.SD.mth$xmid<-factor(rInter.SD.mth$xmid,
                            levels=c(43, 49, 55),
                            labels=c("1 SD Below Mean", "Mean", "1 SD Above Mean"))


rInter.SD.fth <- effect(c("percent*xmid"), rmodel_fth_t_f_1,
                        xlevels=list(percent=c(1,20, 49, 60, 99),
                                     xmid=c(40, 47, 54))) 
rInter.SD.fth <- as.data.frame(rInter.SD.fth)
rInter.SD.fth$xmid<-factor(rInter.SD.fth$xmid,
                           levels=c(40, 47, 54),
                           labels=c("1 SD Below Mean", "Mean", "1 SD Above Mean"))


###### PLOT 
rpfth=ggplot(rfth_df, aes(x=percent, y=mean))+
  geom_point()+
  #geom_point(data=Inter.SD, aes(x=percent, y=fit, color=xmid), size=3)+
  theme_bw()+
  scale_y_continuous(name= "mean RTs [ms]",
                     limits = c(400, 3000))+
  scale_x_continuous(name="proportion [%]")+
  ggtitle("FTH")+
  geom_line(data=rInter.SD.fth, aes(x=percent, y=fit, color=xmid), size=1)+
  labs(color = "Individual threshold")+
  theme(
    axis.title.x = element_text(size=10, face="bold"),
    axis.title.y = element_text(size=10, face="bold"),
    legend.title = element_text(size=10, face="bold"),
    legend.text = element_text(size=10),
    axis.text.y = element_text(size=10),
    axis.text.x = element_text(size=10)
  )+annotate("text", x = 25, y = 3000, label = "-SD = 41%")+
  annotate("text", x = 27, y = 2700, label = "M = 48%")+
  annotate("text", x = 25, y = 2400, label = "+SD = 55%")

rpmth=ggplot(rmth_df, aes(x=percent, y=mean))+
  geom_point()+
  #geom_point(data=Inter.SD, aes(x=percent, y=fit, color=xmid), size=3)+
  theme_bw()+
  scale_y_continuous(name= "mean RTs [ms]",
                     limits = c(400, 3000))+
  scale_x_continuous(name="proportion [%]")+
  ggtitle("MTH")+
  geom_line(data=rInter.SD.mth, aes(x=percent, y=fit, color=xmid), size=1)+
  labs(color = "Individual threshold")+
  theme(
    axis.title.x = element_text(size=10, face="bold"),
    axis.title.y = element_text(size=10, face="bold"),
    legend.title = element_text(size=10, face="bold"),
    legend.text = element_text(size=10),
    axis.text.y = element_text(size=10),
    axis.text.x = element_text(size=10)
  )+annotate("text", x = 75, y = 3000, label = "-SD = 43%")+
  annotate("text", x = 77, y = 2700, label = "M = 49%")+
  annotate("text", x = 75, y = 2400, label = "+SD = 55%")

rpmany=ggplot(rmany_df, aes(x=percent, y=mean))+
  geom_point()+
  #geom_point(data=Inter.SD, aes(x=percent, y=fit, color=xmid), size=3)+
  theme_bw()+
  scale_y_continuous(name= "mean RTs [ms]",
                     limits = c(400, 3000))+
  scale_x_continuous(name="proportion [%]")+
  ggtitle("Many")+
  geom_line(data=rInter.SD, aes(x=percent, y=fit, color=xmid), size=1)+
  labs(color = "Individual threshold")+
  theme(
    axis.title.x = element_text(size=10, face="bold"),
    axis.title.y = element_text(size=10, face="bold"),
    legend.title = element_text(size=10, face="bold"),
    legend.text = element_text(size=10),
    axis.text.y = element_text(size=10),
    axis.text.x = element_text(size=10)
  )+annotate("text", x = 75, y = 3000, label = "-SD = 34%")+
  annotate("text", x = 77, y = 2800, label = "M = 44%")+
  annotate("text", x = 75, y = 2600, label = "+SD = 54%")

  
rpmost=ggplot(rmost_df, aes(x=percent, y=mean))+
  geom_point()+
  #geom_point(data=Inter.SD, aes(x=percent, y=fit, color=xmid), size=3)+
  theme_bw()+
  scale_y_continuous(name= "mean RTs [ms]",
                     limits = c(300, 3000))+
  scale_x_continuous(name="proportion [%]")+
  ggtitle("Most")+
  geom_line(data=rInter.SD.most, aes(x=percent, y=fit, color=xmid), size=1)+
  labs(color = "Individual threshold")+
  theme(
    axis.title.x = element_text(size=10, face="bold"),
    axis.title.y = element_text(size=10, face="bold"),
    legend.title = element_text(size=10, face="bold"),
    legend.text = element_text(size=10),
    axis.text.y = element_text(size=10),
    axis.text.x = element_text(size=10)
  )+annotate("text", x = 75, y = 3000, label = "-SD = 46%")+
  annotate("text", x = 77, y = 2800, label = "M = 53%")+
  annotate("text", x = 75, y = 2600, label = "+SD = 60%")

rpfew=ggplot(rfew_df, aes(x=percent, y=mean))+
  geom_point()+
  #geom_point(data=Inter.SD, aes(x=percent, y=fit, color=xmid), size=3)+
  theme_bw()+
  scale_y_continuous(name= "mean RTs [ms]", 
                     limits = c(500, 3000))+
  scale_x_continuous(name="proportion [%]")+
  ggtitle("Few")+
  geom_line(data=rInter.SD.few, aes(x=percent, y=fit, color=xmid), size=1)+
  labs(color = "Individual threshold")+
  theme(
    axis.title.x = element_text(size=10, face="bold"),
    axis.title.y = element_text(size=10, face="bold"),
    legend.title = element_text(size=10, face="bold"),
    legend.text = element_text(size=10),
    axis.text.y = element_text(size=10),
    axis.text.x = element_text(size=10)
  )+annotate("text", x = 25, y = 3000, label = "-SD = 30%")+
  annotate("text", x = 27, y = 2800, label = "M = 39%")+
  annotate("text", x = 25, y = 2600, label = "+SD = 48%")

library(magrittr)
library(ggpubr)
ggarrange(rpmost, rpmany, rpfew,rpfth, rpmth,  #labels = c("A", "B", "C"),
                  font.label = list(size = 11),
                  vjust=0.9,
                  ncol = 3, nrow = 2,
                                common.legend = TRUE, legend = "top")

ggsave("regression_1.png")

#REPLICATION
library(lme4)
library(lmerTest)
library(ggplot2)
rrmany2222.plot=subset.data.frame(rrmany2222, rrmany2222$response_t_f=="true")
rrmost2222.plot=subset.data.frame(rrmost2222, rrmost2222$response_t_f=="true")
rrfew2222.plot=subset.data.frame(rrfew2222, rrfew2222$response_t_f=="true")
rrfth2222.plot=subset.data.frame(rrfth2222, rrfth2222$response_t_f=="true")
rrmth2222.plot=subset.data.frame(rrmth2222, rrmth2222$response_t_f=="true")

rrmany_above=apply(with(rrmany2222.plot, tapply(read_and_decide_time, list(workerid, percent), mean)), 2, mean, na.rm=TRUE)
rrmost_above=apply(with(rrmost2222.plot, tapply(read_and_decide_time, list(workerid, percent), mean)), 2, mean, na.rm=TRUE)
rrfew_above=apply(with(rrfew2222.plot, tapply(read_and_decide_time, list(workerid, percent), mean)), 2, mean, na.rm=TRUE)
rrfth_above=apply(with(rrfth2222.plot, tapply(read_and_decide_time, list(workerid, percent), mean)), 2, mean, na.rm=TRUE)
rrmth_above=apply(with(rrmth2222.plot, tapply(read_and_decide_time, list(workerid, percent), mean)), 2, mean, na.rm=TRUE)

rrmany_df=data.frame(percent=c(10:18,20:49,51:99), mean=rrmany_above, Q=rep(c("many"), each=88), truth=rep(c("true"), each=88))
rrmost_df=data.frame(percent=c(39:41,43,45:49,51:98), mean=rrmost_above, Q=rep(c("most"), each=57), truth=rep(c("true"), each=57))
rrfew_df=data.frame(percent=c(1:49), mean=rrfew_above, Q=rep(c("few"), each=49), truth=rep(c("true"), each=49))
rrfth_df=data.frame(percent=c(1:49), mean=rrfth_above, Q=rep(c("FTH"), each=49), truth=rep(c("true"), each=49))
rrmth_df=data.frame(percent=c(45,48:49, 51:98), mean=rrmth_above, Q=rep(c("MTH"), each=51), truth=rep(c("true"), each=51))


rrmost2222$resp_AB=ifelse(rrmost2222$response_t_f=="true", "A", "B")
rrmany2222$resp_AB=ifelse(rrmany2222$response_t_f=="true", "A", "B")
rrfew2222$resp_AB=ifelse(rrfew2222$response_t_f=="true", "A", "B")
rrfth2222$resp_AB=ifelse(rrfth2222$response_t_f=="true", "A", "B")
rrmth2222$resp_AB=ifelse(rrmth2222$response_t_f=="true", "A", "B")


rrmany3=subset.data.frame(rrmany2222, rrmany2222$resp_AB=="A")
rrmost3=subset.data.frame(rrmost2222, rrmost2222$resp_AB=="A")
rrfew3=subset.data.frame(rrfew2222, rrfew2222$resp_AB=="A")
rrfth3=subset.data.frame(rrfth2222, rrfth2222$resp_AB=="A")
rrmth3=subset.data.frame(rrmth2222, rrmth2222$resp_AB=="A")



rrmodel_most_t_f_1=lmer(read_and_decide_time~percent*xmid
                        +(1|workerid), data=rrmost3, REML = FALSE)

rrmodel_many_t_f_1=lmer(read_and_decide_time~percent*xmid
                        +(1|workerid), data=rrmany3, REML = FALSE)

rrmodel_few_t_f_1=lmer(read_and_decide_time~percent*xmid*response_t_f
                       +(1|workerid), data=rrfew2222, REML = FALSE)
summary(rrmodel_few_t_f_1)
rrmodel_fth_t_f_1=lmer(read_and_decide_time~percent*xmid
                       +(1|workerid), data=rrfth3, REML = FALSE)

rrmodel_mth_t_f_1=lmer(read_and_decide_time~percent*xmid
                       +(1|workerid), data=rrmth3, REML = FALSE)


library(effects)
rrmu <- ddply(rrdf_all_new_plot, "quantifier", summarise, grp.mean=mean(xmid))
rrsd <- ddply(rrdf_all_new_plot, "quantifier", summarise, grp.sd=sd(xmid))
#quantifier grp.mean
#1       many 40.71186
#2        few 39.25635
#3        FTH 48.43745
#4       most 51.21062
#5        MTH 49.41572
#quantifier    grp.sd
#1       many 13.879936
#2        few  7.054526
#3        FTH  4.791116
#4       most  7.049681
#5        MTH  2.964740


Inter.SD.few <- effect(c("percent*xmid*response_t_f"), rrmodel_few_t_f_1,
                   xlevels=list(percent=c(1, 20, 49, 60, 99),
                                response_t_f="true",
                                xmid=c(32, 39, 46))) 
Inter.SD.few <- as.data.frame(Inter.SD.few)
Inter.SD.few$xmid<-factor(Inter.SD.few$xmid,
                      levels=c(32, 39, 46),
                      labels=c("1 SD Below Mean", "Mean", "1 SD Above Mean"))

Inter.SD.few=subset.data.frame(Inter.SD.few, Inter.SD.few$response_t_f=="true")

Inter.SD <- effect(c("percent*xmid"), rrmodel_many_t_f_1,
                   xlevels=list(percent=c(1,20, 49, 60, 99),
                                xmid=c(27, 41, 55))) 
Inter.SD <- as.data.frame(Inter.SD)
Inter.SD$xmid<-factor(Inter.SD$xmid,
                            levels=c(27, 41, 55),
                            labels=c("1 SD Below Mean", "Mean", "1 SD Above Mean"))

Inter.SD.most <- effect(c("percent*xmid"), rrmodel_most_t_f_1,
                   xlevels=list(percent=c(1,20, 49, 60, 99),
                                xmid=c(44, 51, 58))) 
Inter.SD.most <- as.data.frame(Inter.SD.most)
Inter.SD.most$xmid<-factor(Inter.SD.most$xmid,
                      levels=c(44, 51, 58),
                      labels=c("1 SD Below Mean", "Mean", "1 SD Above Mean"))


Inter.SD.mth <- effect(c("percent*xmid"), rrmodel_mth_t_f_1,
                        xlevels=list(percent=c(1,20, 49, 60, 99),
                                     xmid=c(46, 49, 52))) 
Inter.SD.mth <- as.data.frame(Inter.SD.mth)
Inter.SD.mth$xmid<-factor(Inter.SD.mth$xmid,
                           levels=c(46, 49, 52),
                           labels=c("1 SD Below Mean", "Mean", "1 SD Above Mean"))


Inter.SD.fth <- effect(c("percent*xmid"), rrmodel_fth_t_f_1,
                        xlevels=list(percent=c(1,20, 49, 60, 99),
                                     xmid=c(43, 48, 53))) 
Inter.SD.fth <- as.data.frame(Inter.SD.fth)
Inter.SD.fth$xmid<-factor(Inter.SD.fth$xmid,
                           levels=c(43, 48, 53),
                           labels=c("1 SD Below Mean", "Mean", "1 SD Above Mean"))



pmany=ggplot(rrmany_df, aes(x=percent, y=mean))+
  geom_point()+
  #geom_point(data=Inter.SD, aes(x=percent, y=fit, color=xmid), size=3)+
  theme_bw()+
  scale_y_continuous(name= "mean RTs [ms]",
                     limits = c(500, 3000))+
  scale_x_continuous(name="proportion [%]")+
  ggtitle("Many")+
  geom_line(data=Inter.SD, aes(x=percent, y=fit, color=xmid), size=1)+
  labs(color = "Individual threshold")+
  theme(
    axis.title.x = element_text(size=10, face="bold"),
    axis.title.y = element_text(size=10, face="bold"),
    legend.title = element_text(size=10, face="bold"),
    legend.text = element_text(size=10),
    axis.text.y = element_text(size=10),
    axis.text.x = element_text(size=10)
  )+annotate("text", x = 75, y = 3000, label = "-SD = 27%")+
  annotate("text", x = 77, y = 2800, label = "M = 41%")+
  annotate("text", x = 75, y = 2600, label = "+SD = 55%")

pmost=ggplot(rrmost_df, aes(x=percent, y=mean))+
  geom_point()+
  #geom_point(data=Inter.SD, aes(x=percent, y=fit, color=xmid), size=3)+
  theme_bw()+
  scale_y_continuous(name= "mean RTs [ms]",
                     limits = c(500, 3000))+
  scale_x_continuous(name="proportion [%]")+
  ggtitle("Most")+
  geom_line(data=Inter.SD.most, aes(x=percent, y=fit, color=xmid), size=1)+
  labs(color = "Individual threshold")+
  theme(
    axis.title.x = element_text(size=10, face="bold"),
    axis.title.y = element_text(size=10, face="bold"),
    legend.title = element_text(size=10, face="bold"),
    legend.text = element_text(size=10),
    axis.text.y = element_text(size=10),
    axis.text.x = element_text(size=10)
  )+annotate("text", x = 75, y = 3000, label = "-SD = 44%")+
  annotate("text", x = 77, y = 2800, label = "M = 51%")+
  annotate("text", x = 75, y = 2600, label = "+SD = 58%")

pfew=ggplot(rrfew_df, aes(x=percent, y=mean))+
  geom_point()+
  #geom_point(data=Inter.SD, aes(x=percent, y=fit, color=xmid), size=3)+
  theme_bw()+
  scale_y_continuous(name= "mean RTs [ms]", 
                     limits = c(500, 3000))+
  scale_x_continuous(name="proportion [%]")+
  ggtitle("Few")+
  geom_line(data=Inter.SD.few, aes(x=percent, y=fit, color=xmid), size=1)+
  labs(color = "Individual threshold")+
  theme(
    axis.title.x = element_text(size=10, face="bold"),
    axis.title.y = element_text(size=10, face="bold"),
    legend.title = element_text(size=10, face="bold"),
    legend.text = element_text(size=10),
    axis.text.y = element_text(size=10),
    axis.text.x = element_text(size=10)
  )+annotate("text", x = 25, y = 3000, label = "-SD = 32%")+
  annotate("text", x = 27, y = 2800, label = "M = 39%")+
  annotate("text", x = 25, y = 2600, label = "+SD = 46%")

pfth=ggplot(rrfth_df, aes(x=percent, y=mean))+
  geom_point()+
  #geom_point(data=Inter.SD, aes(x=percent, y=fit, color=xmid), size=3)+
  theme_bw()+
  scale_y_continuous(name= "mean RTs [ms]", 
                     limits = c(500, 3000))+
  scale_x_continuous(name="proportion [%]")+
  ggtitle("FTH")+
  geom_line(data=Inter.SD.fth, aes(x=percent, y=fit, color=xmid), size=1)+
  labs(color = "Individual threshold")+
  theme(
    axis.title.x = element_text(size=10, face="bold"),
    axis.title.y = element_text(size=10, face="bold"),
    legend.title = element_text(size=10, face="bold"),
    legend.text = element_text(size=10),
    axis.text.y = element_text(size=10),
    axis.text.x = element_text(size=10)
  )+annotate("text", x = 25, y = 3000, label = "-SD = 43%")+
  annotate("text", x = 27, y = 2700, label = "M = 48%")+
  annotate("text", x = 25, y = 2400, label = "+SD = 53%")

pmth=ggplot(rrmth_df, aes(x=percent, y=mean))+
  geom_point()+
  #geom_point(data=Inter.SD, aes(x=percent, y=fit, color=xmid), size=3)+
  theme_bw()+
  scale_y_continuous(name= "mean RTs [ms]", 
                     limits = c(500, 3000))+
  scale_x_continuous(name="proportion [%]")+
  ggtitle("MTH")+
  geom_line(data=Inter.SD.mth, aes(x=percent, y=fit, color=xmid), size=1)+
  labs(color = "Individual threshold")+
  theme(
    axis.title.x = element_text(size=10, face="bold"),
    axis.title.y = element_text(size=10, face="bold"),
    legend.title = element_text(size=10, face="bold"),
    legend.text = element_text(size=10),
    axis.text.y = element_text(size=10),
    axis.text.x = element_text(size=10)
  )+annotate("text", x = 75, y = 3000, label = "-SD = 46%")+
  annotate("text", x = 77, y = 2700, label = "M = 49%")+
  annotate("text", x = 75, y = 2400, label = "+SD = 52%")


library(magrittr)
library(ggpubr)
ggarrange(pmost, pmany, pfew,pmth, pfth,  #labels = c("A", "B", "C"),
                  font.label = list(size = 11),
                  vjust=0.9,
                  ncol = 3, nrow = 2,
                  common.legend = TRUE, legend = "top")

ggsave("regression_2.png")

ggarrange(rpmth, rpfth,pmth, pfth,  labels = c("A", "", "B"),
          font.label = list(size = 11),
          vjust=0.9,
          ncol = 2, nrow = 2,
          common.legend = TRUE, legend = "top")
ggsave("mth_fth_rts.png")

ggarrange(rpmany, rpfew,rpmost,  #labels = c("A", "", "B"),
          font.label = list(size = 11),
          vjust=0.9,
          ncol = 3, nrow = 1,
          common.legend = TRUE, legend = "top")

ggsave("most_many_few_original.png")

ggarrange(pmany, pfew,pmost,  #labels = c("A", "", "B"),
          font.label = list(size = 11),
          vjust=0.9,
          ncol = 3, nrow = 1,
          common.legend = TRUE, legend = "top")
ggsave("most_many_few_replication.png")


############ EXAMPLE PLOTS FOR PRESENTATION
library(ggplot2)
many_w24=subset.data.frame(rrmany2, rrmany2$workerid=="24")
many_w53=subset.data.frame(rrmany2, rrmany2$workerid=="53")
wid24=ggplot(many_w24, aes(percent, response)) + geom_point(size=3)+
  theme(
    axis.title.x = element_text(size=12, face="bold"),
    axis.title.y = element_text(size=12, face="bold"),
    # legend.title = element_text(size=8, face="bold"),
    # legend.text = element_text(size=8),
    axis.text.y = element_text(size=12),
    axis.text.x = element_text(size=12)
  )+
  xlim(0,100)+
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE)
ggsave("wid24.png")


wid53=ggplot(many_w53, aes(percent, response)) + geom_point(size=3)+
  theme(
    axis.title.x = element_text(size=12, face="bold"),
    axis.title.y = element_text(size=12, face="bold"),
    # legend.title = element_text(size=8, face="bold"),
    # legend.text = element_text(size=8),
    axis.text.y = element_text(size=12),
    axis.text.x = element_text(size=12)
  )+
  ylim(c(0,1))+
  xlim(0,100)+
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE)
ggsave("wid53.png")

library(magrittr)
library(ggpubr)
ggarrange(wid24, wid53,  #labels = c("A", "B", "C"),
          font.label = list(size = 11),
          vjust=0.9,
          ncol = 1, nrow = 2,
          common.legend = TRUE, legend = "top")

ggsave("example.png")

