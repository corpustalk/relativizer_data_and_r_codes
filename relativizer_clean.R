library(languageR)
library(dplyr)
library(ggplot2)
library(ggstatsplot)
library(lme4)
library(lmerTest)
library(dplyr)
library(psych)
library(trend)
library(reshape)
library(reshape2)
library(readxl)

data <- read.csv2("data_SOTU.csv",sep = ',')

data = data[data$animacy!='animate',]
data = data[data$relativizer == 'which' | data$relativizer == 'that' |data$relativizer == 'zero',]

sub<-data[data$role.in.sub == 'nsubj'|data$role.in.sub == 'nsubjpass',]
obj<-data[data$role.in.sub == 'obj',]


# subject relative clauses
subfreq <- xtabs(~year + relativizer,data=sub)
subfreqprop <- prop.table(subfreq,1)
subfreqdf<-data.frame(subfreqprop)

subfreqdf$year = as.numeric(as.factor(subfreqdf$year))
subfreqdf$year = subfreqdf$year +1789

# simple linear regression
which = subfreqdf[subfreqdf$relativizer=='which',]
which_fit = lm(which$Freq~which$year)
summary(which_fit)

that = subfreqdf[subfreqdf$relativizer=='that',]
that_fit = lm(that$Freq~that$year)
summary(that_fit)


# plot
new_subfreqdf = subfreqdf
new_subfreqdf$year = as.numeric(new_subfreqdf$year)
new_subfreqdf$year = new_subfreqdf$year+1789

theme_set(theme_bw(base_size = 10, base_line_size = 1))
ggplot(new_subfreqdf,aes(x=year,y=Freq,color=relativizer))+
  geom_point()+
  geom_smooth(aes(x=year,y=Freq),size=1.2,level = 0.95)+
  scale_x_continuous(name = "Year",n.breaks=18)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(name = "Proportion of relativizers",limits = c(0,1),n.breaks=10)+
  scale_color_manual(values=c("#CC3333","#3399FF"))
# 720 360

ggsave('09_subject_RC.jpg',width = 7.2, height = 3.6, dpi = 1500)

# object relative clauses
objfreq <- xtabs(~year + relativizer,data=obj)
objfreqprop <- prop.table(objfreq,1)
objfreqdf<-data.frame(objfreqprop)

objfreqdf$year = as.numeric(as.factor(objfreqdf$year))
objfreqdf$year = objfreqdf$year +1789


# simple linear regression
which = objfreqdf[objfreqdf$relativizer == 'which',]
which_fit = lm(which$Freq~which$year)
summary(which_fit)

that = objfreqdf[objfreqdf$relativizer == 'that',]
that_fit = lm(that$Freq~that$year)
summary(that_fit)

zero = objfreqdf[objfreqdf$relativizer == 'zero',]
zero_fit = lm(zero$Freq~zero$year)
summary(zero_fit)


zero$year = as.numeric(zero$year)
#zero$year = zero$year + 1789
zero$year = zero$year + 1789
zero_1960_1990 = zero[zero$year > 1959 ,]
zero_1960_1990 = zero_1960_1990[zero_1960_1990$year < 1991 ,]
theme_set(theme_bw(base_size = 10, base_line_size = 0.5))
ggplot(zero_1960_1990,aes(x=year,y=Freq)) +
  geom_point()+
  geom_line()+
  theme(axis.text.x = element_text(angle = 0, hjust = 1,size=12),
        axis.text.y = element_text(hjust = 1,size=12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)) +
  scale_x_continuous(name = "Year")+
  scale_y_continuous(name = "Proportion of zero")

ggsave('09_1960-1990.jpg',width = 6, height = 3.2, dpi = 1500)

# plot
new_objfreqdf = objfreqdf
new_objfreqdf$year = as.numeric(new_objfreqdf$year)
new_objfreqdf$year = new_objfreqdf$year+1789

theme_set(theme_bw(base_size = 10, base_line_size = 1))
ggplot(new_objfreqdf,aes(x=year,y=Freq,color=relativizer))+
  geom_point()+
  geom_smooth(aes(x=year,y=Freq),size=1.2)+
  scale_x_continuous(name = "Year",n.breaks=18)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(name = "Proportion of relativizers",limits = c(0,1),n.breaks=10)+
  scale_color_manual(values=c("#CC3333","#3399FF","#00cc00"))
ggsave('09_object_RC.jpg',width = 7.2, height = 3.6, dpi = 1500)

# acceleration 
library(trend)
relativizer_trend = xtabs(~year + relativizer,data=data)
relativizer_df = prop.table(relativizer_trend,1)
relativizer_df = data.frame(relativizer_df)
that = relativizer_df[relativizer_df$relativizer == 'that',]
which = relativizer_df[relativizer_df$relativizer == 'which',]
zero = relativizer_df[relativizer_df$relativizer == 'zero',]

pettitt.test(that$Freq)
pettitt.test(which$Freq)
pettitt.test(zero$Freq)

zero_new = zero
zero_new$year = as.numeric(zero_new$year)
zero_new$year = zero_new$year+1789
zero_plot <- ggplot(zero_new,aes(x=year,y=Freq))+
  geom_point()+
  geom_smooth()+
  geom_vline(xintercept = 1942,color="#CC3333",size=1)+
  scale_y_continuous(name = "Proportion of zero-RC",limits = c(-0.1,1.0))+
  scale_x_continuous(name = "Year")+
  theme(axis.text.x = element_text(angle = 0, hjust = 1,size=12),
        axis.text.y = element_text(hjust = 1,size=12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

zero_plot+annotate("text",x=1955,y=-0.05,label="1942",color="#CC3333",size=5)
zero_plot
ggsave('09_zero.jpg',width = 6, height = 3.2, dpi = 1500)

that_new = that
that_new$year = as.numeric(that_new$year)
that_new$year = that_new$year+1789
that_plot <- ggplot(that_new,aes(x=year,y=Freq))+
  geom_point()+
  geom_smooth()+
  geom_vline(xintercept = 1939,color="#CC3333",size=1)+
  scale_y_continuous(name = "Proportion of that-RC",limits = c(-0.1,1.0))+
  scale_x_continuous(name = "Year")+
  theme(axis.text.x = element_text(angle = 0, hjust = 1,size=12),
        axis.text.y = element_text(hjust = 1,size=12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))


that_plot+annotate("text",x=1950,y=-0.05,label="1939",color="#CC3333",size=5)
that_plot
ggsave('09_that.jpg',width = 6, height = 3.2, dpi = 1500)

# png 480 230
which_new = which
which_new$year = as.numeric(which_new$year)
which_new$year = which_new$year+1789
which_plot <- ggplot(which_new,aes(x=year,y=Freq))+
  geom_point()+
  geom_smooth()+
  geom_vline(xintercept = 1939,color="#CC3333",size=1)+
  scale_y_continuous(name = "Proportion of which-RC",limits = c(-0.1,1.0))+
  scale_x_continuous(name = "Year")+
  theme(axis.text.x = element_text(angle = 0, hjust = 1,size=12),
        axis.text.y = element_text(hjust = 1,size=12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))


which_plot+annotate("text",x=1950,y=-0.05,label="1939",color="#CC3333",size=5)
which_plot
ggsave('09_which.jpg',width = 6, height = 3.2, dpi = 1500)


