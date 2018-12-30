#set####
setwd("D:/Rproject/Lanyu")
ly<-read.csv("蘭嶼採集紀錄_final.csv",stringsAsFactors=FALSE)#讀取資料
ly<-ly[ly$month!=7,]#剔除前測月分
ly<-ly[is.na(ly$chigger)==FALSE,]#去除NA值
ly[ly$reproduction=="",]$reproduction<-"未成熟"#轉換生殖狀態
ly[ly$reproduction!="未成熟",]$reproduction<-"性成熟"
ly$reproduction<-as.character(ly$reproduction)
ly$month<-factor(ly$month,levels = c("3","6","9","12"))#排序月份
ly[ly$habitat_type=="部落",]$habitat_type<-"dwelling"
ly[ly$habitat_type=="草地",]$habitat_type<-"grassland"
ly[ly$habitat_type=="森林",]$habitat_type<-"forest"
ly$habitat_type<-factor(ly$habitat_type,levels = c("dwelling","grassland","forest"))
library(dplyr)
library(tidyr)
library(ggplot2)
theme_set(theme_bw()+ theme(text = element_text(size=34),legend.position="none",
                            axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
                            axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))))#作圖設定
rat<-ly%>%group_by(habitat_type,location,block,month)%>%summarise(rat=n())%>%
  complete(month, fill = list(rat = 0))#計算各樣點老鼠隻數
chigger<-ly%>%group_by(habitat_type,location,block,month)%>%#計算各樣點恙蟲平均豐度
  summarise(chigger=mean(chigger,na.rm=TRUE))%>%complete(month, fill = list(chigger = 0))
total<-left_join(chigger,rat)#合併
total$total<-total$chigger*total$rat#計算恙蟲總量
tick<-ly%>%group_by(habitat_type,location,month)%>%#計算各樣點硬蜱平均豐度
  summarise(tick=mean(tick.total,na.rm=TRUE))%>%complete(month, fill = list(tick = 0))
total.tick<-left_join(tick,rat)#合併
total.tick$total<-total.tick$tick*total.tick$rat#計算硬蜱總量

library(Rmisc)
tgc <- summarySE(rat, measurevar="rat", groupvars=c("habitat_type"))#計算信賴區間CI
ggsave(filename = "afig3.png",
       ggplot(tgc)+aes(habitat_type,rat,fill=habitat_type)+geom_bar(stat = "identity",width=.5,col=1)+
         geom_errorbar(aes(ymin=rat-se, ymax=rat+se),width=.2)+
         theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
               axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))+
         scale_y_continuous(expand = c(0, 0),limits = c(0,11),breaks = seq(0,9,by = 3))+
         xlab("habitat type")+ylab("rats"),
       width = 25, height = 14, dpi = 300, units = "cm", device='png')
ggsave(filename = "afig5.png",
       ggplot(ly)+geom_boxplot(aes(habitat_type,chigger),width=.8)+
         geom_dotplot(aes(habitat_type,chigger,fill=habitat_type),
                      binaxis='y',stackdir='center',binwidth = 52,method="histodot")+
         theme(axis.title.y = element_text(size=28))+
         scale_y_continuous(expand = c(0, 0),limits = c(0,1999),breaks = seq(0,2000,by = 500))+
         xlab("habitat type")+ylab("mean chigger abundance"),
       width = 25, height = 14, dpi = 300, units = "cm", device='png')
totalse<- summarySE(total, measurevar="total", groupvars=c("habitat_type"))
ggsave(filename = "afig6.png",
       ggplot(totalse)+aes(habitat_type,total,fill=habitat_type)+geom_bar(stat = "identity",width = .5,col=1)+
         geom_errorbar(aes(ymin=total-se, ymax=total+se),width=.2)+
         scale_y_continuous(expand = c(0, 0),limits = c(0,4999))+xlab("habitat type")+ylab("total chiggers"),
       width = 25, height = 14, dpi = 300, units = "cm", device='png')
ggsave(filename = "afig7.png",
       ggplot(ly)+geom_boxplot(aes(habitat_type,tick.total))+
         geom_dotplot(aes(habitat_type,tick.total,fill=habitat_type),
                      binaxis='y',stackdir='center',binwidth = 1.7,method="histodot")+
         scale_y_continuous(expand = c(0, 0),limits = c(0,110),breaks = seq(0,100,by = 25))+
         xlab("habitat type")+ylab("mean tick abundance"),
       width = 25, height = 14, dpi = 300, units = "cm", device='png')
tickse<- summarySE(total.tick, measurevar="total", groupvars=c("habitat_type"))
ggsave(filename = "afig8.png",
       ggplot(tickse)+aes(habitat_type,total,fill=habitat_type)+geom_bar(stat = "identity",width = .5,col=1)+
         geom_errorbar(aes(ymin=total-se, ymax=total+se),width=.2)+
         scale_y_continuous(expand = c(0, 0),limits = c(0,84))+xlab("habitat type")+ylab("total ticks"),
       width = 25, height = 14, dpi = 300, units = "cm", device='png')