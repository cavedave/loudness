---
title: "R Notebook"
output: html_notebook
---

```{r}
library(dplyr)
library(ggplot2)
loud <-read.csv(file="loudness.csv",sep="\t",header=TRUE)
loud <- subset(loud, select = -c(X) )
#loud<-filter(loud, year >= 16)
loud<-filter(loud, year >= 1950)
loud<-filter(loud,loudness >-40)

#loud<-filter(time, duration > 20)
head(loud)

```

```{r}
count(loud)
```

1950	n=214	-14.14	5.21
1960	n=725	-12.81	4.71
1970	n=1626	-11.85	4.22
1980	n=2752	-11.49	4.68
1990	n=8212	-10.87	5.04
2000	n=19510	-8.66	4.78
2010	n=601	-7.99	3.78

```{r}
#summary(loud)
#apply(loud$loudness, 2, sd)
apply(loud, 2, sd, na.rm = TRUE)
```


```{r}
#this<filter(loud, year >= 2010)
#head(this)
#summary(this)
apply(this, 2, sd, na.rm = TRUE)
#apply(this, 1, sd)
#mean(this)
#apply(this, 2, sd, na.rm = TRUE)
```

```{r}
head(this)
```

library("plyr")
```{r}
write.csv(loud, file = "loudstrip.csv")
```



```{r}
loud["decade"] <- loud["year"] - (loud["year"]%% 10)

count(loud, "decade")
```







```{r}
#graph over time
library(dplyr)
library(ggplot2)

decade<-c(1950,1960,1970,1980,1990,2000,2010)
loudness<-c(-14.14,-12.81,-11.85,-11.49,-10.87,-8.66,-7.99)

df<-data.frame(region=loudness,value=decade)

# Basic line plot with points
ggplot(data=df, aes(x=decade, y=loudness, group=1)) +
  geom_line(color="blue")+
  geom_point(color="blue")+
  ylim(-17, 0)+
 labs(#title = 'Average Loudness of Songs',subtitle='Difference of quietest to loudest part of songs',
         x="Decade",y="Loudness (dBFS)")+
  theme_minimal()+
    scale_x_discrete(breaks=c(2010,2000,1990,1980,1970, 1960,1950), expand = c(0.01, 0))
      


```




```{r}
library(ggridges)
min = min(loud$loudness)
max = max(loud$loudness)
#min_tempo = min(df$year)
#max_tempo = max(df$year)
print(max)
print(min)
loud["decade"] <- loud["year"] - (loud["year"]%% 10)
head(loud)
#summary(df)
#ggplot(df,aes(x = loudness,y = year,height=..density..))+
#     geom_joy(scale=3) +
# scale_y_reverse(breaks=c(2000, 1980, 1960, 1940, 1920, 1900), expand = c(0.01, 0))+
#      scale_x_continuous(limits = c(min,max))+
#  theme_joy() 
p1= ggplot(loud, aes(x = loudness, y = decade, group = decade))+ 
  geom_density_ridges2(fill = "lightblue")+
   #   geom_joy2()+
   xlab("Loudness (dBFS)") +
  ylab("Decade")+
    #labs(#title = 'Loudness of Songs',subtitle='difference of quietest to loudest part of songs',
    #     x="Loudness (dBFS)",y="Decade")+
    scale_x_continuous(limits=c(-40, 4.2), expand = c(0.01, 0)) +
    scale_y_reverse(breaks=c(2010,2000,1990, 1980,1970, 1960,1950), expand = c(0.01, 0))+
   theme_bw() + theme(plot.title = element_text(hjust=0.5))
  ##theme_ridges()  
#  theme_joy()   

#ggsave("loudNNoLabel.png")
```

```{r}
p1
```



```{r}
#head(loud)
count(loud)
```


```{r}
library(dplyr)
#detach(package:plyr)
#grouped <- group_by(loud, decade)
#summarise(grouped, mean=mean(loudness), sd=sd(loudness))

#loud %>%
#  group_by(decade) %>%
#    summarise(avg_loud = mean(loudness))

cat<-loud %>% 
    group_by(decade) %>%
    summarise(avg_loud = mean(loudness),
              stf_loud= sd(loudness))

#msleep %>% 
#    group_by(order) %>%
#    summarise(avg_sleep = mean(sleep_total), 
#              min_sleep = min(sleep_total), 
#              max_sleep = max(sleep_total))
#cars <-loud %>%
#select(loudness, decade) %>%
#group_by(decade)%>%
#summarise(lo = mean(loudness))

#mean.default(loud, "decade")
cat
```


grid.arrange(p1,p2,ncol=2)




```{r}
#scatterplot
#p2=
  ggplot(loud, aes(x = year, y = loudness))+ 
   geom_point(alpha = 0.1,size=.03,color = "lightblue") +
  labs(x="Year",y="Loudness (dBFS)")+
      scale_x_discrete(limits=c(1950,1960,1970,1980,1990,2000,2010))+#, expand = c(0.01, 0) breaks
  geom_smooth(method=lm,size=.5)+theme_minimal()# limits=c("trt1","trt2","ctrl") method=lm,size=3



  #+geom_jitter(width = 0.01)
#ggsave("loudscatterNJ1.png")
```


```{r}
cor.test(loud$decade,loud$loudness) 

```

```{r}
#cor.test(loud$decade,loud$loudness) 

#linearMod <- lm(decade ~ loudness, data=loud)  # build linear regression model on full data
linearMod <- lm(loudness ~ decade, data=loud)
print(linearMod)
summary(linearMod)
```

```{r}
summary(linearMod)
```



```{r}
#loud, aes(x = year, y = loudness
#cor(loud, use="complete.obs", method= "pearson")
cor.test(loud$year,loud$loudness) 
```


```{r}
#cor.test(loud$decade,loud$loudness) 
linearMod2 <- lm(loudness ~ year, data=loud)
#linearMod2 <- lm(year ~ loudness, data=loud)  # build linear regression model on full data
print(linearMod2)
summary(linearMod2)
```


```{r}
library(gridExtra)

g=arrangeGrob(p1,p2,ncol=2)
ggsave(file="loudBoth.png", g)
```








```{r}
#summary(loud)
#summary(loud)
library("plyr")
count(loud, "decade")
#head(loud)
```


