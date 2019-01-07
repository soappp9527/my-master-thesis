library(ggplot2)
windowsFonts(BL = windowsFont("微軟正黑體"))#設定字形
theme_set(theme_bw()+ theme(text = element_text(size=24,family = "BL"),legend.position="none"))#作圖設定
col.mo<-c("#FFB310", "#00A0FF", "#FE2370","#2BDE73")#色碼
col.ha<-c("#F2C545", "#ED8A3F", "#5A8F29")

rat<-ly%>%group_by(habitat_type,location,month)%>%summarise(rat=n())%>%
  complete(month, fill = list(rat = 0))#計算各樣點老鼠隻數
total.chigger<-ly%>%group_by(habitat_type,location,month)%>%summarise(total=sum(chigger))%>%
  complete(month, fill = list(chigger = NULL))
total.chigger[is.na(total.chigger)==T]<-0
total.tick<-ly%>%group_by(habitat_type,location,month)%>%summarise(total=sum(tick.total))%>%
  complete(month, fill = list(tick.total = NULL))
total.tick[is.na(total.tick)==T]<-0
tick.rate<-ly
tick.rate[tick.rate$tick.total==0,]$tick.total<-"無"
tick.rate[tick.rate$tick.total!="無",]$tick.total<-"有"
tick.rate$tick.total<-factor(tick.rate$tick.total,levels = c("無","有"))
ly$I.g.adult<-ly$I.g.female+ly$I.g.male
tick.stage<-melt(ly,id.vars = c("NO.","habitat_type","month","location"),#stage轉置 library(reshape2)
                 measure.vars = c("I.g.larva","I.g.nymph","I.g.adult"),variable.name = "stage")
levels(tick.stage$stage)<-list("幼蜱"="I.g.larva","若蜱"="I.g.nymph","成蜱"="I.g.adult")#更改stage名稱
tick.stage$stage<-factor(tick.stage$stage,levels = c("幼蜱","若蜱","成蜱"))#排序
i.g.habitat<-tick.stage%>%group_by(habitat_type,stage)%>%summarise(value=sum(value))#position = "dodge" Y只能sum
i.g.month<-tick.stage%>%group_by(month,stage)%>%summarise(value=sum(value))

rat.mo<-ly%>%group_by(habitat_type,location,month)%>%summarise(rat=n())%>%
  complete(month, fill = list(rat = 0))%>%group_by(month)%>%
  summarise(mean=mean(rat),se=sd(rat)/sqrt(n()))
rat.ha<-ly%>%group_by(habitat_type,location,month)%>%summarise(rat=n())%>%
  complete(month, fill = list(rat = 0))%>%group_by(habitat_type)%>%
ctotal.mean<-total.chigger%>%group_by(habitat_type,month)%>%summarise(mean=mean(total),se=sd(total)/sqrt(n()))
ttotal.mo<-total.tick%>%group_by(month)%>%summarise(mean=mean(total),se=sd(total)/sqrt(n()))
ttotal.ha<-total.tick%>%group_by(habitat_type)%>%summarise(mean=mean(total),se=sd(total)/sqrt(n()))

ggsave(filename = "18-1.png",#18-1####
       ggplot(rat.mo)+aes(month,mean,fill=month)+geom_bar(stat = "identity",width=.9,col=1)+
         geom_errorbar(aes(ymin=mean-se, ymax=mean+se),width=.5)+scale_fill_manual(values=col.mo)+
         scale_y_continuous(expand = c(0, 0),limits = c(0,13))+xlab("捕捉月份")+ylab("家鼠數量"),
       width = 15, height = 12, dpi = 300, units = "cm", device='png')
ggsave(filename = "18-2.png",#18-2####
       ggplot(rat.ha)+aes(habitat_type,mean,fill=habitat_type)+geom_bar(stat = "identity",width=.9,col=1)+
         geom_errorbar(aes(ymin=mean-se, ymax=mean+se),width=.5)+
         scale_fill_manual(values=col.ha)+
         scale_y_continuous(expand = c(0, 0),limits = c(0,13))+xlab("棲地類型")+ylab("家鼠數量"),
       width = 15, height = 12, dpi = 300, units = "cm", device='png')
ggsave(filename = "19.png",#19####
       ggplot(ly)+aes(month,fill=reproduction)+geom_bar(position = "fill")+facet_grid(.~habitat_type)+
         scale_fill_manual(values=c(0.1,"#FF3B1D"))+scale_y_continuous(expand = c(0, 0))+
         xlab("捕捉月份")+ylab("性成熟比例"),
       width = 30, height = 12, dpi = 300, units = "cm", device='png')
ggsave(filename = "20.png",#20####
       ggplot(ly)+aes(location,L.deliense,fill=month)+geom_bar(stat="identity")+
         theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "right")+
         scale_fill_manual(values=col.mo,name="捕捉月份")+
         scale_y_continuous(expand = c(0, 0),limits = c(0,1550))+xlab("捕捉樣點")+ylab("地里纖恙蟎"),
       width = 30, height = 12, dpi = 300, units = "cm", device='png')
ggsave(filename = "21.png",#21####
       ggplot(ly)+aes(location,W.xishaensis,fill=month)+geom_bar(stat="identity")+
         theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "right")+
         scale_fill_manual(values=col.mo,name="捕捉月份")+
         scale_y_continuous(expand = c(0, 0),limits = c(0,210))+xlab("捕捉樣點")+ylab("西沙無前恙蟎"),
       width = 30, height = 12, dpi = 300, units = "cm", device='png')
ggsave(filename = "22.png",#22####
       ggplot(ly)+aes(location,E.wichmanni,fill=month)+geom_bar(stat="identity")+
         theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "right")+
         scale_fill_manual(values=col.mo,name="捕捉月份")+
         scale_y_continuous(expand = c(0, 0),limits = c(0,105))+xlab("捕捉樣點")+ylab("威氏真恙蟎"),
       width = 30, height = 12, dpi = 300, units = "cm", device='png')
ggsave(filename = "23.png",#23####
       ggplot(ly)+geom_boxplot(aes(month,chigger,fill=month))+
         scale_fill_manual(values=col.mo)+facet_grid(.~habitat_type)+
         scale_y_continuous(expand = c(0, 0),limits = c(0,1970))+xlab("捕捉月份")+ylab("恙蟎寄生量"),
       width = 30, height = 12, dpi = 300, units = "cm", device='png')
ggsave(filename = "24.png",#24####
       ggplot(ctotal.mean)+aes(month,mean,fill= month)+geom_bar(stat = "identity",width = .9,col=1)+
         geom_errorbar(aes(ymin=mean-se, ymax=mean+se),width=.5)+
         scale_fill_manual(values=col.mo)+facet_grid(.~habitat_type)+
         scale_y_continuous(expand = c(0, 0),limits = c(0,7500))+xlab("棲地類型")+ylab("恙蟎總數量"),
       width = 30, height = 12, dpi = 300, units = "cm", device='png')
ggsave(filename = "25.png",#25####
       ggplot(ly)+aes(location,tick.total,fill=month)+geom_bar(stat="identity")+
         scale_fill_manual(values=col.mo,name="捕捉月份")+
         theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "right")+
         scale_y_continuous(expand = c(0, 0),limits = c(0,550))+xlab("捕捉樣點")+ylab("硬蜱數量"),
       width = 30, height = 12, dpi = 300, units = "cm", device='png')
ggsave(filename = "26.png",#26####
       ggplot(ly)+aes(location,I.g.total,fill=month)+geom_bar(stat="identity")+
         scale_fill_manual(values=col.mo,name="捕捉月份")+
         theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "right")+
         scale_y_continuous(expand = c(0, 0),limits = c(0,450))+xlab("捕捉樣點")+ylab("粒形硬蜱"),
       width = 30, height = 12, dpi = 300, units = "cm", device='png')
ggsave(filename = "27.png",#27####
       ggplot(ly)+aes(location,A.t.total,fill=month)+geom_bar(stat="identity")+
         scale_fill_manual(values=col.mo,name="捕捉月份")+
         theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "right")+
         scale_y_continuous(expand = c(0, 0),limits = c(0,180))+xlab("捕捉樣點")+ylab("龜形花蜱"),
       width = 30, height = 12, dpi = 300, units = "cm", device='png')
ggsave(filename = "28.png",#28####
       ggplot(ly)+aes(location,H.h.total,fill=month)+geom_bar(stat="identity")+
         scale_fill_manual(values=col.mo,name="捕捉月份")+
         theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "right")+
         scale_y_continuous(expand = c(0, 0),limits = c(0,21))+xlab("捕捉樣點")+ylab("豪豬血蜱"),
       width = 30, height = 12, dpi = 300, units = "cm", device='png')
ggsave(filename = "29-1.png",#29-1####
       ggplot(tick.rate)+aes(month,fill=tick.total)+geom_bar(position = "fill")+
         scale_fill_manual(values=c(0.1,"#FF3B1D"))+
         scale_y_continuous(expand = c(0, 0),limits = c(0,1.05))+xlab("捕捉月份")+ylab("硬蜱寄生率"),
       width = 15, height = 12, dpi = 300, units = "cm", device='png')
ggsave(filename = "29-2.png",#29-2####
       ggplot(tick.rate)+aes(habitat_type,fill=tick.total)+geom_bar(position = "fill")+
         scale_fill_manual(values=c(0.1,"#FF3B1D"))+
         scale_y_continuous(expand = c(0, 0),limits = c(0,1.05))+xlab("棲地類型")+ylab("硬蜱寄生率"),
       width = 15, height = 12, dpi = 300, units = "cm", device='png')
ggsave(filename = "30-1.png",#30-1####
       ggplot(ly)+aes(month,tick.total,fill=month)+geom_boxplot()+
         scale_fill_manual(values=col.mo)+
         scale_y_continuous(expand = c(0, 0),limits = c(0,105))+xlab("捕捉月份")+ylab("硬蜱寄生量"),
       width = 15, height = 12, dpi = 300, units = "cm", device='png')
ggsave(filename = "30-2.png",#30-2####
       ggplot(ly)+aes(habitat_type,tick.total,fill=habitat_type)+geom_boxplot()+
         scale_fill_manual(values=col.ha)+
         scale_y_continuous(expand = c(0, 0),limits = c(0,105))+xlab("棲地類型")+ylab("硬蜱寄生量"),
       width = 15, height = 12, dpi = 300, units = "cm", device='png')
ggsave(filename = "31-1.png",#31-1####
       ggplot(ttotal.mo)+aes(month,mean,fill=month)+geom_bar(stat = "identity",width = .9,col=1)+
         geom_errorbar(aes(ymin=mean-se, ymax=mean+se),width=.5)+
         scale_fill_manual(values=col.mo)+
         scale_y_continuous(expand = c(0, 0),limits = c(0,115))+xlab("捕捉月份")+ylab("硬蜱總數量"),
       width = 15, height = 12, dpi = 300, units = "cm", device='png')
ggsave(filename = "31-2.png",#31-2####
       ggplot(ttotal.ha)+aes(habitat_type,mean,fill=habitat_type)+geom_bar(stat = "identity",width = .9,col=1)+
         geom_errorbar(aes(ymin=mean-se, ymax=mean+se),width=.5)+
         scale_fill_manual(values=col.ha)+
         scale_y_continuous(expand = c(0, 0),limits = c(0,115))+xlab("棲地類型")+ylab("硬蜱總數量"),
       width = 15, height = 12, dpi = 300, units = "cm", device='png')
ggsave(filename = "32-1.png",#32-1####
       ggplot(i.g.habitat)+aes(habitat_type,value,fill=stage)+geom_bar(stat = "identity",position = "dodge")+
         scale_fill_manual(values=col.ha,name="生活史階段")+
         theme(legend.position = "bottom")+
         scale_y_continuous(expand = c(0, 0),limits = c(0,460))+xlab("棲地類型")+ylab("粒形硬蜱數量"),
       width = 15, height = 12, dpi = 300, units = "cm", device='png')
ggsave(filename = "32-2.png",#32-2####
       ggplot(i.g.month)+aes(month,value,fill=stage)+geom_bar(stat = "identity",position = "dodge")+
         scale_fill_manual(values=col.ha,name="生活史階段")+
         theme(legend.position = "bottom")+
         scale_y_continuous(expand = c(0, 0),limits = c(0,460))+xlab("捕捉月份")+ylab("粒形硬蜱數量"),
       width = 15, height = 12, dpi = 300, units = "cm", device='png')