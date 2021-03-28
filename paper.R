library(ggplot2)
library(data.table)
library(lme4)#GLMM
library(car)#ANOVA
library(emmeans)#post hoc
library(broom)
library(patchwork)
theme_set(theme_linedraw()+ theme(text = element_text(size = 36),
                                  legend.position="none",
                                  panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(),
                                  axis.title = element_text(size = rel(1.2)),
                                  axis.text = element_text(size = rel(0.8)),
                                  axis.ticks = element_line(size = rel(1.5)),
                                  axis.ticks.length = unit(.25, "cm"),
                                  panel.border = element_rect(colour = "black")))#作圖設定

lanyu <- read.csv("蘭嶼採集紀錄_final.csv", stringsAsFactors = FALSE)
lanyu <- lanyu[lanyu$month != 7, ]
lanyu$habitat_type <- factor(lanyu$habitat_type, labels = c("forest", "grassland", "residential sites"), levels = c("森林", "草地", "部落"))
lanyu$month <- factor(lanyu$month, labels = c("Sep", "Dec", "Mar", "Jun"), levels = c( 9, 12, 3, 6))
lanyu <- data.table(lanyu)

chigger_sum <- lanyu[, .(mean = mean(chigger), se = sd(chigger)/sqrt(.N)), by = c("habitat_type", "month")]
chigger_sum$upper <- chigger_sum$mean + chigger_sum$se*1.96
chigger_sum$lower <- chigger_sum$mean - chigger_sum$se*1.96
fwrite(chigger_sum, "chigger_sum.csv", row.names = FALSE)

#GLMM
chigger.glmm<-glmer.nb(chigger~habitat_type*month+weight.g.+(1|location),data=lanyu,verbose=FALSE)#GLMM
wald <- tidy(Anova(chigger.glmm,type=3))#有交互
#write.csv(wald, "Type III Wald chisquare tests.csv", row.names = FALSE)
lsmeans(chigger.glmm,pairwise ~ habitat_type:month,adjust = "tukey")#森林6>9>3=12
lsmeans(chigger.glmm,pairwise ~ reproduction,adjust = "tukey")#性成熟>未成熟
#residual
lanyu$chi.resi<-residuals(chigger.glmm)

chi.lm<-lm(ly$chi.resi~ly$weight.g.)
summary(chi.lm)
ggplot(ly)+aes(weight.g.,chiresi)+geom_point()+geom_smooth(method = "lm")

ggplot(lanyu)+aes(chigger, chi.resi)+geom_point()


#rat <- lanyu[, .N, by = c("location", "habitat_type", "month")]
rat <- fread("p1.csv")
rat <- rat[, .(mean = mean(N), se = sd(N)/sqrt(.N)), by = c("habitat_type", "month")]
rat$upper <- rat$mean + rat$se*1.96
rat$lower <- rat$mean - rat$se*1.96
rat$month <- factor(rat$month, labels = c("Sep", "Dec", "Mar", "Jun"), levels = c( 9, 12, 3, 6))

#p3 <- lanyu[, .(sum = sum(chigger)), by = c("location", "habitat_type", "month")]
total <- fread("p3.csv")
total <- total[, .(mean = mean(sum), se = sd(sum)/sqrt(.N)), by = c("habitat_type", "month")]
total$month <- factor(total$month, levels = c("Sep", "Dec", "Mar", "Jun"))
total$upper <- total$mean + total$se*1.96
total$lower <- total$mean - total$se*1.96


#patchwork####
plot_a <- ggplot(rat)+aes(x = month, y = mean, ymin = mean-se, ymax = mean+se,fill = habitat_type)+
  geom_bar(stat = "identity", position = position_dodge(width = 0.9))+
  geom_errorbar(position = position_dodge(width = 0.9), size = 1, width = 0.4)+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 19))+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank())+
  labs(y = "Rodent abundance", fill = "Habitat type")

plot_b <- ggplot(p2)+aes(x = month, y = chigger, fill = habitat_type)+geom_boxplot(position = position_dodge(width = 0.8))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2050))+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank())+
  labs(y = "Chigger burdens", fill = "Habitat type")

plot_c <- ggplot(total)+aes(x = month, y = mean, ymin = mean-se, ymax = mean+se,fill = habitat_type)+
  geom_bar(stat = "identity", position = position_dodge(width = 0.9))+
  geom_errorbar(position = position_dodge(width = 0.9), size = 1, width = 0.4)+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 7500))+
  theme(legend.position="bottom")+
  labs(x = "Month", y = "Chigger abundance", fill = "Habitat type")

plot_com <- plot_a+plot_b+plot_c+plot_layout(ncol = 1)

ggsave(filename = "plot_com.svg", plot_com, width = 15, height = 25)
ggsave(filename = "plot_com.tiff", plot_com, width = 15, height = 25)


#residual plot####
resi_loc <- fread("residuals.csv")

wei_resi <- ggplot(lanyu)+aes(weight.g., chi.resi)+geom_point(size = 4)+
  scale_y_continuous(limits = c(-3.2, 3.2))+
  labs(x = "Rodent weights", y = "Residuals")

loc_resi <- ggplot(resi_loc)+aes(`rodent abundance`, residuals)+geom_point(size = 4)+
  scale_y_continuous(limits = c(-3900, 3900))+
  labs(x = "Rodent abundance")+theme(axis.title.y = element_blank())

plot_resi <- wei_resi+loc_resi
ggsave(filename = "plot_resi.svg", plot_resi, width = 25, height = 15)
ggsave(filename = "plot_resi.tiff", plot_resi, width = 25, height = 15)

#violin plot####
lanyu_sum <- fread("total.csv")
vio_bur <- ggplot(lanyu)+aes(habitat_type, chigger, fill = habitat_type)+geom_violin()+
             scale_y_continuous(limits = c(0, 2050), expand = c(0, 0))+labs(y = "Chigger burdens")+
             theme(axis.text.x = element_blank(),
                   axis.ticks.x = element_blank(),
                   axis.title.x = element_blank())

vio_abd <- ggplot(lanyu_sum)+aes(habitat_type, sum, fill = habitat_type)+geom_violin()+
             scale_y_continuous(limits = c(0, 9000), expand = c(0, 0))+labs(x = "Habitat type", y = "Chigger abundance")

plot_vio <- vio_bur+vio_abd+plot_layout(ncol = 1)
ggsave(filename = "plot_vio.svg", plot_vio, width = 15, height = 20)
ggsave(filename = "plot_vio.tiff", plot_vio, width = 15, height = 20)