#set####
setwd("D:/Rproject/Lanyu")
ly<-read.csv("蘭嶼採集紀錄_final.csv",stringsAsFactors=FALSE)#讀取資料
ly<-ly[ly$month!=7,]#剔除前測月分
ly<-ly[is.na(ly$chigger)==FALSE,]#去除NA值
ly[ly$reproduction=="",]$reproduction<-"未成熟"#轉換生殖狀態
ly[ly$reproduction!="未成熟",]$reproduction<-"性成熟"
ly$reproduction<-as.character(ly$reproduction)
ly$month<-factor(ly$month,levels = c("9","12","3","6"))#排序月份
ly$habitat_type<-factor(ly$habitat_type,levels = c("部落","草地","森林"))
ly$location<-factor(ly$location,levels = c("椰油部落","野銀部落","紅頭部落","小天池草地","野銀草地","青青草原",
                                           "小天池森林","氣象站","永興農場","忠愛橋"))#排序樣點
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(lme4)#GLMM
library(car)#ANOVA
library(emmeans)#post hoc
library(glmmTMB)#GLMMZINB
library(geepack)
windowsFonts(BL = windowsFont("標楷體"))#設定字形
theme_set(theme_bw()+ theme(text = element_text(size=20,family = "BL"),legend.position="none"))#作圖設定

#表一###
rat.mean<-ly%>%group_by(habitat_type,location,month)%>%summarise(rat=n())%>%#先計算各樣點老鼠隻數
  complete(month, fill = list(rat = 0))%>%group_by(habitat_type,month)%>%
  summarise(mean=mean(rat),se=sd(rat)/sqrt(n()))#再計算各棲地mean與se
mass.mean<-ly%>%group_by(habitat_type,location,month)%>%summarise(mass=mean(weight.g.))%>%
  group_by(habitat_type,month)%>%summarise(mean=mean(mass),se=sd(mass)/sqrt(n()))
chigger.mean<-ly%>%group_by(habitat_type,location,month)%>%summarise(chigger=mean(chigger))%>%
  complete(month, fill = list(chigger = 0))%>%group_by(habitat_type,month)%>%
  summarise(mean=mean(chigger),se=sd(chigger)/sqrt(n()))
tick.mean<-ly%>%group_by(habitat_type,location,month)%>%summarise(tick.total=mean(tick.total))%>%
  complete(month, fill = list(tick.total = 0))%>%group_by(habitat_type,month)%>%
  summarise(mean=mean(tick.total),se=sd(tick.total)/sqrt(n()))
#附錄一####
mas.location<-ly%>%group_by(location,month)%>%summarise(mean=mean(weight.g.),se=sd(weight.g.)/sqrt(n()))%>%
  complete(month,fill=list(mean=0,se=0))
chigger.location<-ly%>%group_by(location,month)%>%summarise(mean=mean(chigger),se=sd(chigger)/sqrt(n()))%>%
  complete(month,fill=list(mean=0,se=0))
tick.location<-ly%>%group_by(location,month)%>%summarise(mean=mean(tick.total),se=sd(tick.total)/sqrt(n()))%>%
  complete(month,fill=list(mean=0,se=0))
#籠具差異####
tra<-data.frame(a=c(9,1191),b=c(245,955))
chisq.test(tra)
#老鼠數量分析####
rat<-ly%>%group_by(habitat_type,location,month)%>%summarise(rat=n())%>%
  complete(month, fill = list(rat = 0))#計算各樣點老鼠隻數
#GLMM
ratLMM<-lmer(rat~habitat_type*month+(1|location),data = rat)
Anova(ratLMM,type = 3)#無交互
ratLMM<-lmer(rat~habitat_type+month+(1|location),data = rat)
Anova(ratLMM,type = 3)#月份間、棲地間都有差異
lsmeans(ratLMM,pairwise ~ habitat_type,adjust = "tukey")#部落低於森林
lsmeans(ratLMM,pairwise ~ month,adjust = "tukey")#12月高於3月6月
#GEE
ratgee<-geeglm(rat~habitat_type*month,id=location,data=na.omit(rat),family = gaussian,corstr  = "ar1")
anova(ratgee)#有交互
lsmeans(ratgee,pairwise ~ habitat_type,adjust = "tukey")#機地間都有差異
lsmeans(ratgee,pairwise ~ month,adjust = "tukey")#12月高於其他
lsmeans(ratgee,pairwise ~ habitat_type:month,adjust = "tukey")

ggsave(filename = "圖三.png",#圖三####
       ggplot(rat)+aes(month,rat,fill=month)+geom_boxplot()+
         scale_fill_grey(start = 1, end = 0.4)+facet_grid(.~habitat_type)+
         scale_y_continuous(expand = c(0, 0),limits = c(0,22))+xlab("捕捉月份")+ylab("家鼠數量"),
       width = 20, height = 14, dpi = 300, units = "cm", device='png')


#老鼠體型分析####
shapiro.test(ly$weight.g.^2)
ggplot(ly)+aes(weight.g.^2)+geom_density()
#GLMM
rat.massLMM<-lmer(weight.g.^2~habitat_type*month*reproduction+(1|location),data =ly)#LMM
Anova(rat.massLMM,type=3)#無棲地月份交互
rat.massLMM<-lmer(weight.g.^2~habitat_type+month*reproduction+(1|location),data =ly)#LMM
Anova(rat.massLMM,type=3)#生殖狀態月份交互
lsmeans(rat.massLMM,pairwise ~ reproduction:month,adjust = "tukey")#6月未成熟體型小於其他月份
#GEE
rat.massgee<-geeglm(weight.g.~habitat_type+month*reproduction,id=location,data=ly,
                    family = gaussian,corstr  = "ar1")
anova(rat.massgee)#生殖與月份交互
lsmeans(rat.massgee,pairwise ~ reproduction:month,adjust = "tukey")#6月未成熟體型小於其他月份

rat.mass<-ly%>%group_by(habitat_type,location,reproduction,month)%>%summarise(weight.g.=mean(weight.g.))

ggplot(ly)+geom_boxplot(aes(month,weight.g.))+
  geom_boxplot(aes(month,weight.g.,fill=reproduction),width=0.7,alpha=0.9)+
  scale_fill_grey(start = 0.4, end = 0.8,name="生殖狀態")+
  scale_y_continuous(expand = c(0, 0))+xlab("捕捉月份")+ylab("家鼠體重")
  
#老鼠性成熟比例分析####
ratrLMM<-glmer(as.numeric(reproduction)~habitat_type*month+(1|location),data =ly,family=binomial)
Anova(ratrLMM,type = 3)#有交互
lsmeans(ratrLMM,pairwise ~ habitat_type:month,adjust = "tukey")#森林中6月性成熟比例高於12月
#6月為主要繁殖季
ratrgee<-geeglm(as.numeric(reproduction)~habitat_type*month,id=location,data =ly,family=binomial,corstr ="ar1")
anova(ratrgee)
lsmeans(ratrgee,pairwise ~ habitat_type:month,adjust = "tukey")

ggsave(filename = "圖四.png",#圖四####
       ggplot(ly)+aes(month,fill=reproduction)+geom_bar(position = "fill")+
         facet_grid(.~habitat_type)+
         scale_fill_grey(start = 0.9, end = 0.2,name="生殖狀態")+
         scale_y_continuous(expand = c(0, 0))+xlab("捕捉月份")+ylab("性成熟比例"),
       width = 20, height = 14, dpi = 300, units = "cm", device='png')

#鑑定結果整理匯入####
cc<-read.csv("恙螨鑑定紀錄.csv",stringsAsFactors=FALSE)#讀取資料
cc1<-cc%>%group_by(NO.)%>%#將玻片計數資料處理成個體資料
  summarise(total=sum(total),L.deliense=sum(L.deliense),W.xishaensis=sum(W.xishaensis),
            E.wichmanni=sum(E.wichmanni),NA.=sum(NA.))
ly<-left_join(ly,cc1)
ly[is.na(ly)]<-0
#恙蟎物種分析####
ggsave(filename = "圖五.png",#圖五####
       ggplot(ly)+aes(location,L.deliense,fill=month)+geom_bar(stat="identity")+
         theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "bottom")+
         scale_fill_grey(start = 0.8, end = 0.2,name="捕捉月份")+
         scale_y_continuous(expand = c(0, 0),limits = c(0,1550))+xlab("捕捉樣點")+ylab("地里纖恙蟎"),
       width = 20, height = 14, dpi = 300, units = "cm", device='png')
#L.d是最主要恙蟎種類

allwx<-ly%>%group_by(location,month)%>%summarise(wx=sum(W.xishaensis))
ggsave(filename = "圖六.png",#圖六####
       ggplot(ly)+aes(location,W.xishaensis,fill=month)+geom_bar(stat="identity")+
         theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "bottom")+
         scale_fill_grey(start = 0.8, end = 0.2,name="捕捉月份")+
         scale_y_continuous(expand = c(0, 0),limits = c(0,210))+xlab("捕捉樣點")+ylab("西沙無前恙蟎"),
       width = 20, height = 14, dpi = 300, units = "cm", device='png')
#W.x幾乎都出現在森林

allew<-ly%>%group_by(location,month)%>%summarise(wx=sum(E.wichmanni))
ggsave(filename = "圖七.png",#圖七####
       ggplot(ly)+aes(location,E.wichmanni,fill=month)+geom_bar(stat="identity")+
         theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "bottom")+
         scale_fill_grey(start = 0.8, end = 0.2,name="捕捉月份")+
         scale_y_continuous(expand = c(0, 0),limits = c(0,105))+xlab("捕捉樣點")+ylab("威氏真恙蟎"),
       width = 20, height = 14, dpi = 300, units = "cm", device='png')
#E.w幾乎都出現在椰油部落

#恙蟲豐度分析####
#GLMM
chigger.glmm<-glmer.nb(chigger~habitat_type*month+sex+(1|block/location),data=ly,verbose=FALSE)#GLMM
Anova(chigger.glmm,type=3)#有交互
lsmeans(chigger.glmm,pairwise ~ habitat_type:month,adjust = "tukey")#森林6>9>3=12
lsmeans(chigger.glmm,pairwise ~ reproduction,adjust = "tukey")#性成熟>未成熟
#residual
ly$chi.resi<-residuals(chigger.glmm)
chi.lm<-lm(ly$chi.resi~ly$weight.g.)
summary(chi.lm)
ggplot(ly)+aes(weight.g.,chiresi)+geom_point()+geom_smooth(method = "lm")
#GEE
cgee<-geeglm(chigger~habitat_type*month+weight.g.,id=location,data=ly,family=poisson,corstr = "ar1")
anova(cgee)#有交互
lsmeans(cgee,pairwise ~ habitat_type:month,adjust = "tukey")

ggsave(filename = "圖八.png",#圖八####
       ggplot(ly)+geom_boxplot(aes(month,chigger,fill=month))+
         scale_fill_grey(start = 1, end = 0.4,name="捕捉月份")+facet_grid(.~habitat_type)+
           scale_y_continuous(expand = c(0, 0),limits = c(0,1970))+xlab("棲地類型")+ylab("恙蟎寄生量"),
         width = 20, height = 14, dpi = 300, units = "cm", device='png')

#恙蟲總量分析####
rat<-ly%>%group_by(habitat_type,location,block,month)%>%summarise(rat=n())%>%
  complete(month, fill = list(rat = 0))#計算各樣點老鼠隻數
chigger<-ly%>%group_by(habitat_type,location,block,month)%>%#計算各樣點恙蟲平均豐度
    summarise(chigger=mean(chigger,na.rm=TRUE))%>%complete(month, fill = list(chigger = 0))
total<-left_join(chigger,rat)#合併
total$total<-total$chigger*total$rat#計算恙蟲總量
totalGLMM<-glmer.nb(total~habitat_type*month+(1|block/location),data = total)#GLMM
Anova(totalGLMM,type = 3)#有交互
lsmeans(totalGLMM,pairwise ~ habitat_type:month,adjust = "tukey")

ggsave(filename = "圖九.png",#圖九####
       ggplot(total)+aes(month,total,fill=month)+geom_boxplot()+
         scale_fill_grey(start = 1, end = 0.4,name="捕捉月份")+facet_grid(.~habitat_type)+
         scale_y_continuous(expand = c(0, 0),limits = c(0,9000))+xlab("棲地類型")+ylab("恙蟎總數量"),
       width = 20, height = 14, dpi = 300, units = "cm", device='png')

#恙蟲病鑑定結果####
st<-read.csv("蘭嶼恙蟲病檢測結果.csv",stringsAsFactors=FALSE)
st[st$OT檢測結果=="+",]$OT檢測結果<-1
st[st$OT檢測結果!=1,]$OT檢測結果<-0
st.glmm<-glmer(OT檢測結果~habitat_type*month+(1|location),data=st,verbose=FALSE,family=binomial)#GLMM
Anova(st.glmm,type=3)#不顯著

#硬蜱物種分析####
alltick<-ly%>%group_by(location)%>%summarise(alltick=sum(tick.total))
ggsave(filename = "圖十.png",#圖十####
       ggplot(ly)+aes(location,tick.total,fill=month)+geom_bar(stat="identity")+
         scale_fill_grey(start = 0.8, end = 0.2,name="捕捉月份")+
         theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "bottom")+
         scale_y_continuous(expand = c(0, 0),limits = c(0,550))+xlab("捕捉樣點")+ylab("硬蜱數量"),
       width = 20, height = 14, dpi = 300, units = "cm", device='png')
allig<-ly%>%group_by(month,location)%>%summarise(alltick=sum(I.g.total))
ggsave(filename = "圖十一.png",#圖十一####
       ggplot(ly)+aes(location,I.g.total,fill=month)+geom_bar(stat="identity")+
         scale_fill_grey(start = 0.8, end = 0.2,name="捕捉月份")+
         theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "bottom")+
         scale_y_continuous(expand = c(0, 0),limits = c(0,450))+xlab("捕捉樣點")+ylab("粒形硬蜱"),
       width = 20, height = 14, dpi = 300, units = "cm", device='png')
allat<-ly%>%group_by(month,location)%>%summarise(alltick=sum(A.t.total))
ggsave(filename = "圖十二.png",#圖十二####
       ggplot(ly)+aes(location,A.t.total,fill=month)+geom_bar(stat="identity")+
         scale_fill_grey(start = 0.8, end = 0.2,name="捕捉月份")+
         theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "bottom")+
         scale_y_continuous(expand = c(0, 0),limits = c(0,180))+xlab("捕捉樣點")+ylab("龜形花蜱"),
       width = 20, height = 14, dpi = 300, units = "cm", device='png')
allhh<-ly%>%group_by(month,location)%>%summarise(alltick=sum(H.h.total))
ggsave(filename = "圖十三.png",#圖十三####
       ggplot(ly)+aes(location,H.h.total,fill=month)+geom_bar(stat="identity")+
         scale_fill_grey(start = 0.8, end = 0.2,name="捕捉月份")+
         theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "bottom")+
         scale_y_continuous(expand = c(0, 0),limits = c(0,21))+xlab("捕捉樣點")+ylab("豪豬血蜱"),
       width = 20, height = 14, dpi = 300, units = "cm", device='png')
ggplot(ly)+aes(month,I.g.larva)+geom_bar(stat="identity")

#硬蜱寄生率####
#寄生率為伯努力分布
ly1<-ly
ly1[ly1$tick.total!=0,]$tick.total<-1
para.glmm<-glmer(tick.total~habitat_type*month+(1|location),data=ly1,verbose=FALSE,family=binomial)#GLMM
Anova(para.glmm,type=3)#沒交互
para.glmm<-glmer(tick.total~habitat_type+month+(1|location),data=ly1,verbose=FALSE,family=binomial)
Anova(para.glmm,type=3)#棲地與月份都有差異
lsmeans(para.glmm,pairwise ~ habitat_type,adjust = "tukey")
lsmeans(para.glmm,pairwise ~ month,adjust = "tukey")

tick.rate<-ly
tick.rate[tick.rate$tick.total==0,]$tick.total<-"無"
tick.rate[tick.rate$tick.total!="無",]$tick.total<-"有"
tick.rate$tick.total<-factor(tick.rate$tick.total,levels = c("無","有"))
ggsave(filename = "圖十四.png",#圖十四####
       ggplot(tick.rate)+aes(month,fill=tick.total)+geom_bar(position = "fill")+
         facet_grid(.~habitat_type)+
         scale_fill_grey(start = 0.9, end = 0.2,name="有無寄生")+
         scale_y_continuous(expand = c(0, 0))+xlab("捕捉月份")+ylab("硬蜱寄生率"),
       width = 20, height = 14, dpi = 300, units = "cm", device='png')

#硬蜱豐度分析####
tick.glmmzinb<-glmmTMB(tick.total~habitat_type+month+sex+(1|location),zi=~habitat_type+month+weight.g.,
                   family=nbinom2,data=ly)#GLMMZINB
summary(tick.glmm)
tick.glmm<-glmer.nb(tick.total~habitat_type+month+sex+(1|location),data=ly,verbose=FALSE)
Anova(tick.glmm,type = 3)#無交互
lsmeans(tick.glmm,pairwise ~ habitat_type,adjust = "tukey")#草地>部落
lsmeans(tick.glmm,pairwise ~ month,adjust = "tukey")#12月<all
lsmeans(tick.glmm,pairwise ~ reproduction,adjust = "tukey")
#residual
ly$tick.resi<-residuals(tick.glmm)
tick.lm<-lm(ly$tick.resi~ly$weight.g.)
summary(tick.lm)
ggplot(ly)+aes(weight.g.,tick.resi)+geom_point()+geom_smooth(method = "lm")

ggsave(filename = "圖十五.png",#圖十五####
       ggplot(ly)+aes(month,tick.total,fill=month)+geom_boxplot()+
         scale_fill_grey(start = 1, end = 0.4,name="捕捉月份")+facet_grid(.~habitat_type)+
         scale_y_continuous(expand = c(0, 0),limits = c(0,105))+xlab("棲地類型")+ylab("硬蜱寄生量"),
       width = 20, height = 14, dpi = 300, units = "cm", device='png')

#硬蜱總量####
rat<-ly%>%group_by(habitat_type,location,month)%>%summarise(rat=n())%>%
  complete(month, fill = list(rat = 0))#計算各樣點老鼠隻數
tick<-ly%>%group_by(habitat_type,location,month)%>%#計算各樣點硬蜱平均豐度
  summarise(tick=mean(tick.total,na.rm=TRUE))%>%complete(month, fill = list(tick = 0))
total.tick<-left_join(tick,rat)#合併
total.tick$total<-total.tick$tick*total.tick$rat#計算硬蜱總量
total.tick.glmm<-glmer.nb(total~habitat_type*month+(1|location),data=total.tick,verbose=FALSE)
Anova(total.tick.glmm,type = 3)#無交互作用
lsmeans(total.tick.glmm,pairwise ~ habitat_type,adjust = "tukey")#草地>部落

ggsave(filename = "圖十六.png",#圖十六####
       ggplot(total.tick)+aes(month,total,fill=month)+geom_boxplot()+
         scale_fill_grey(start = 1, end = 0.4,name="捕捉月份")+facet_grid(.~habitat_type)+
         scale_y_continuous(expand = c(0, 0),limits = c(0,320))+xlab("棲地類型")+ylab("硬蜱總數量"),
       width = 20, height = 14, dpi = 300, units = "cm", device='png')

#硬蜱生活史階段####
ly$I.g.adult<-ly$I.g.female+ly$I.g.male
tick.stage<-melt(ly,id.vars = c("NO.","habitat_type","month","location"),#stage轉置 library(reshape2)
                 measure.vars = c("I.g.larva","I.g.nymph","I.g.adult"),variable.name = "stage")

i.g.glmm<-glmer.nb(value~habitat_type+month+stage+stage:month+stage:habitat_type+
                     (1|NO.),data=tick.stage)
Anova(i.g.glmm,type = 3)#stage:habitat_type沒有交互

i.g.glmm1<-glmer.nb(value~habitat_type+month*stage+(1|NO.),data=tick.stage)
Anova(i.g.glmm1,type = 3)
lsmeans(i.g.glmm,pairwise ~ habitat_type,adjust = "tukey")
lsmeans(i.g.glmm,pairwise ~ month:stage,adjust = "tukey")

#分開做無法收斂
i.g.l.glmm<-glmer.nb(I.g.larva~habitat_type*month+(1|location),data=ly,verbose=FALSE)
Anova(i.g.l.glmm,type=3)
i.g.n.glmm<-glmer.nb(I.g.nymph~habitat_type*month+(1|location),data=ly,verbose=FALSE)
Anova(i.g.n.glmm,type=3)
i.g.a.glmm<-glmer.nb(I.g.adult~habitat_type*month+(1|location),data=ly,verbose=FALSE)
Anova(i.g.a.glmm,type=3)
#

levels(tick.stage$stage)<-list("幼蜱"="I.g.larva","若蜱"="I.g.nymph","成蜱"="I.g.adult")#更改stage名稱
tick.stage$stage<-factor(tick.stage$stage,levels = c("幼蜱","若蜱","成蜱"))#排序
i.g.habitat<-tick.stage%>%group_by(habitat_type,stage)%>%summarise(value=sum(value))#position = "dodge" Y只能sum
ggsave(filename = "圖十七.png",#圖十七####
       ggplot(i.g.habitat)+aes(habitat_type,value,fill=stage)+geom_bar(stat = "identity",position = "dodge")+
         scale_fill_grey(start = 0.7, end = 0.2,name="生活史階段")+
         theme(legend.position = "bottom")+
         scale_y_continuous(expand = c(0, 0),limits = c(0,460))+xlab("棲地類型")+ylab("粒形硬蜱數量"),
       width = 20, height = 14, dpi = 300, units = "cm", device='png')
i.g.month<-tick.stage%>%group_by(month,stage)%>%summarise(value=sum(value))
ggsave(filename = "圖十八.png",#圖十八####
       ggplot(i.g.month)+aes(month,value,fill=stage)+geom_bar(stat = "identity",position = "dodge")+
         scale_fill_grey(start = 0.7, end = 0.2,name="生活史階段")+
         theme(legend.position = "bottom")+
         scale_y_continuous(expand = c(0, 0),limits = c(0,410))+xlab("捕捉月份")+ylab("粒形硬蜱數量"),
       width = 20, height = 14, dpi = 300, units = "cm", device='png')

#pie
i.g.stage<-tick.stage%>%group_by(stage,month)%>%summarise(an=sum(value))%>%
  mutate(per=an/sum(an))%>%
  mutate(label_pos = cumsum(per) - 0.8*per,perc_text = paste0(round(per * 100), "%"))
ggplot(i.g.stage)+geom_bar(aes(x="",y=an,fill=month),width=1,stat = "identity",position = "fill")+
  coord_polar("y",start = 0)+facet_grid(.~stage)+theme_void()+ scale_y_reverse()+
  geom_text(aes(x=1.1,y=label_pos,label=perc_text))