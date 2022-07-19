
# 分类水平：Family
# 分组：normal，RRTP，
# normal，RRTJ
# RRTP，RRTJ
# 指数类型：richness，chao1，Shannon，Simpson 
# 双组检验，StudentT检验
# 颜色：红，蓝      云平台颜色集里第二个

rm(list = ls())
library('ggplot2')
library('ggsci')
library('readxl')
library(stringr)
library(tidyr)
library(ggpubr)
library(ggplot2)


mydata <- read.csv('Ana_out/3.alpha多样性分析/3-4.指数组间差异检验/default/Group2/alpha_All.csv',header = T)
mydata$Group <- str_split(mydata$Group2 ,'[.]',simplify = T)[,1]
mydata$Group <- ifelse(mydata$Group == 'cf','RRTP',ifelse(mydata$Group == 'clz','RRTJ','normal'))
table(mydata$Group)
names(mydata)

comparison = combn(c('normal',"RRTP","RRTJ"),2,simplify = F)
var1 <- c('richness','chao1','shannon_2','shannon_e','simpson')
for (i in 1:length(comparison)) {
  var0 <- unlist(comparison[i])
  dat0 <- mydata[mydata$Group %in% var0,]
  assign(paste("data",i,sep=""),dat0)
}

P1 <- ggplot(data3,aes(x=Group,y=simpson,fill=Group)) + 
  stat_boxplot(geom = "errorbar",width=0.4,aes(color="black"))+
  geom_boxplot(size=0.7,fill=c("red",'blue'))+ #size设置箱线图的边框线和胡须的线宽度，fill设置填充颜色，outlier.fill和outlier.color设置异常点的属性
  geom_jitter(aes(fill=Group),width =0.2,shape = 19,size=2.5)+ 
  scale_fill_manual(values = c("black", "black"))+
  scale_color_manual(values=c("black","black","black"))+ 
  theme_classic()+ 
  theme(legend.position="none", 
        axis.text.x=element_text(colour="black",family="Times",size=14), #设置x轴刻度标签的字体属性
        axis.text.y=element_text(colour="black",family="Times",size=14), #设置y轴刻度标签的字体属性
        axis.title.x=element_text(family="Times",size = 14,face="plain"), #设置x轴的标题的字体属性
        axis.title.y=element_text(family="Times",size = 14,face="plain"), #设置y轴的标题的字体属性
        plot.title = element_text(family="Times",size=15,face="bold",hjust = 0.5), #设置总标题的字体属性
        axis.ticks.x=element_line(color="black",size=0.7,lineend = 1),
        axis.ticks.y=element_line(color="black",size=0.7,lineend = 1),
        panel.grid.major = element_blank(), #不显示网格线
        panel.grid.minor = element_blank()) +  xlab(NULL)
P1
ggplot2::ggsave(filename = paste0('./RRTP-RRTJ-','simpson','.pdf'),P1,height = 6,width = 4)


data1$Group <- factor(data1$Group,levels=c("normal","RRTP"))
b <- combn(c('normal','RRTP'),2)
my_comparisons <- split(b, col(b))
