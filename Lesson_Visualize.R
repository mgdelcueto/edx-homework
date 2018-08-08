library(dslabs)
data(heights)
head(heights)
h<-heights$height
plot(h)
hist(h)
prop.table(table(heights$sex))
average<-sum(h)/length(h)
SD<-sqrt(sum((h-average)^2)/length(h))
index<-heights$sex=="Male"
x<-heights$height[index]
indef<-heights$sex=="Female"
f<-heights$height[indef]
z<-scale(x)               #Normalizado (tipificado)
mean(abs(z)<2)            #porcentaje entre dos sigmay - dos sigma (sigma=1)
#quantiles  ----------------------------------------------------------------
p<-seq(0.05,0.95,0.05)
observed_quantiles<-quantile(x,p)
theoretical_quantiles <- qnorm(p,mean=mean(x),sd=sd(x))
plot(theoretical_quantiles,observed_quantiles)
abline(0,1)
# in standard units ------------------------------------------------------
theoretical_quantiles <- qnorm(p)
observed_quantiles<-quantile(z,p)
plot(theoretical_quantiles,observed_quantiles)
abline(0,1)
#boxplot -----------------------------------------------------------------
boxplot(x)
#ggplot GGPLOT        GGPLOT       GGPLOT      GGPLOT   GGPLOT  ----------
ggplot(data=murders)
murders %>% ggplot()
p<-murders %>% ggplot()
p<-p+geom_point(aes(x=population/10^6,y=total),size=3)  #adding geom layer with aes as parameter
p<-p+geom_text(aes(x=population/10^6,y=total,label=abb),nudge_x=1) #layer geom_text or geom_label with aes
p
#now we use global asignation of aes to avoid mapping twice the geom for points and labels
p<-murders %>% ggplot(aes(x=population/10^6,y=total,label =abb))
p<-p+geom_point(size=3)
p<-p+geom_text(nudge_x=0.05)
#adding scales
p<-p+scale_x_continuous(trans="log10")
p<-p+scale_y_continuous(trans="log10")
p
library(ggthemes)       #addons themes for ggplot
library(ggrepal)        #to avoid labels one over another geom_text_repel()
#define rate of murders for all country
r<-murders %>%
  summarize(rate=sum(total)/sum(population)*10^6) %>% .$rate
#there exists an specific scale for log10
p<-murders %>% ggplot(aes(x=population/10^6,y=total,label =abb))
#p<-p+theme_economist()    ##use this theme
p<-p+theme_fivethirtyeight()
#p<-p+geom_point(size=3,color ="blue")             #we can add color here for all points
p<- p+geom_abline(intercept = log10(r),lty=2,color="darkgrey")   #we add a line of slope r 
p<-p+geom_point(aes(color=region),size=3)   #with this aes we assign a color to each region 
#p<-p+geom_text(nudge_x=0.075)  change for geom_text_repel() to avoid label overwritten
p<-p+geom_text_repel()
#adding scales
p<-p+scale_x_log10()
p<-p+scale_y_log10()
#adding axis labels and tittle
p<- p+xlab("Population in millions (log scale)")
p<- p+ylab("Total number of murders (log scale)")
p<-p+ggtitle("US Gun Murders in US 2010")
#changing the default label of regions
p<-p+scale_color_discrete(name="Region")
p
# SUMMARIZED PLOTS WITH GGPLOT ----------------------------------------------------
library(dslabs)
data(heights)
p<-heights %>% filter(sex=="Male") %>%
  ggplot(aes(x=height))+
  #geom_histogram(binwidth=1,fill="blue",color ="black")+
  geom_density(fill="blue")
  xlab("Males heights in inches")+
  ggtitle("Histogram")
p
# QQ PLOT ---------------------------------------------------------
params <- heights %>% filter(sex=="Male") %>% 
    summarize(mean=mean(height),sd = sd(height))   #define params to use with qqplot
p<-heights %>% filter(sex=="Male") %>%
  ggplot(aes(sample=height))+
  #geom_qq()                        # by defaul N(0,1)
  geom_qq(dparams=params) +          # we can qqplot againt N(mu,sigma)
  geom_abline()
p
#we can also typific the variable and plot against the N(0,1) this avoid
# to compute mean and sd of the data
p<-heights %>% filter(sex=="Male") %>%
  ggplot(aes(sample=scale(height)))+
  geom_qq()                        # by defaul N(0,1)
  geom_abline()
p
#GRID ARRANGE ------------------------------------------------------------
library(gridExtra)
p<-heights %>% filter(sex=="Male") %>%  ggplot(aes(x=height))
p1<- p+ geom_histogram(binwidth=1,fill="blue",col="black")
p2<- p+ geom_histogram(binwidth=2,fill="blue",col="black")
p3<- p+ geom_histogram(binwidth=3,fill="blue",col="black")
grid.arrange(p1,p2,p3,nrow=3)

#Library dplyr SUMMARIZE DATA --------------------------------------------
library(tidyverse)
library(dslabs)
data(heights)
s<-heights %>%
  filter(sex=="Male") %>%
  summarize(average=mean(height),standard_deviation=sd(height))
t<-heights %>%
  filter(sex=="Male") %>%
  summarize(median=median(height),
            max=max(height),
            min=min(height))
data(murders)
murders<-murders %>% mutate(murder_rate=total/population*10^6)
summarize(murders,mean(murder_rate))
mean(murder_rate)
us_murders_rate <- murders %>%
  summarize(rate=sum(total)/sum(population)*10^6)
us_murders_rate <- murders %>%
  summarize(rate=sum(total)/sum(population)*10^6) %>% .$rate
#GROUP ------------------------------------------------------------------
heights %>% group_by(sex)  # dplyr behaves differently with this object
heights %>% group_by(sex) %>%
  summarize(average=mean(height),sd=sd(height))
murders %>% group_by(region) %>%
  summarize(meadian=median(murder_rate))
      
# SORT ---> ARRANGE -----------------------------------------------------
murders %>% arrange(population) %>% head()
murders %>% arrange(desc(murder_rate)) %>% head()
murders %>% arrange(region,desc(murder_rate)) %>% head()
murders %>% top_n(10,murder_rate)
murders %>% arrange(desc(murder_rate)) %>% top_n(10)

#GAPMINDER database -----------------------------------------------------
library(dslabs)
data(gapminder)
gapminder %>%
  filter(year==2015 & country %in% c("Sri Lanka","Turkey")) %>%
  select (country,infant_mortality)

ds_theme_set()
filter(gapminder,year==1962) %>%
  ggplot(aes(fertility,life_expectancy,color =continent))+
  geom_point()

#faceting: Stratified the plot for some data and make plot for each strat
#facet the range in the axis is determined for all plots in order to 
#facilitate the comparaisons
filter(gapminder,year %in% c(1962,2012)) %>%
  ggplot(aes(fertility,life_expectancy,color =continent))+
  geom_point()+
    facet_grid(continent~year)
# if we want faceting with only one variable
filter(gapminder,year %in% c(1962,2012)) %>%
  ggplot(aes(fertility,life_expectancy,color =continent))+
  geom_point()+
  facet_grid(.~year)
#use of the facet_wrap
years<-c(1962,1980,1990,2000,2012)
continents <- c("Europe","Asia")
gapminder %>% filter(year %in% years & continent %in% continents) %>%
  ggplot(aes(fertility,life_expectancy,color =continent))+
  geom_point()+
  facet_wrap(~year)
  
#Time series plots (time in x axis)
gapminder %>% filter(country %in% c("South Korea","Germany")) %>%
  ggplot(aes(year,fertility,group=country))+
  geom_line()
#color adds groups and legends automatically
gapminder %>% filter(country %in% c("South Korea","Germany")) %>%
  ggplot(aes(year,fertility,color=country))+
  geom_line()
#Using labels
countries<-c("South Korea","Germany")
labels <- data.frame (country=countries,x=c(1975,1965),y=c(60,72))
gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(year,life_expectancy,color=country))+
  geom_line()+
  geom_text(data=labels,aes(x,y,label=country),size =5)+
  theme(legend.position="none")

#Transformations
gapminder<- gapminder %>%
  mutate(dollars_per_day=gdp/population/365)
past_year<-1970
gapminder %>% 
  filter (year==past_year & !is.na(gdp)) %>%
  ggplot(aes(log2(dollars_per_day)))+
  geom_histogram(binwidth=1,color="black")
#we can see two modes (moda=value of hight frequency) local mmodes multiple modes
#transform the scale strenghts and weakness
gapminder %>% 
  filter (year==past_year & !is.na(gdp)) %>%
  ggplot(aes(dollars_per_day))+
  geom_histogram(binwidth=1,color="black")+
  scale_x_continuous(trans="log2")

#box plots
library(dslabs)
data(gapminder)
gapminder<- gapminder %>%
  mutate(dollars_per_day=gdp/population/365)
past_year<-1970
gapminder %>% 
  filter (year==past_year & !is.na(gdp)) %>%
  ggplot(aes(region,dollars_per_day))+
  geom_boxplot()+
  theme(axis.text.x=element_text(angle=90,hjust=1))#rotate the labels
#reorder function reorder factor based in summary of numeric value
gapminder %>% 
  filter (year==past_year & !is.na(gdp)) %>%
  mutate(region=reorder(region,dollars_per_day,FUN=median))%>%
  ggplot(aes(region,dollars_per_day,fill=continent))+
  geom_boxplot()+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  xlab("")+
  scale_y_continuous(trans="log2")+geom_point(show.legend =FALSE)
#regions in the west
west <- c("Western Europe","Northern Europe","Southern Europe","Northern America",
          "Australia and New Zealand")
gapminder %>% 
  filter (year==past_year & !is.na(gdp)) %>%
  mutate(group=ifelse(region %in% west, "West","Developing"))%>%
  ggplot(aes(dollars_per_day))+
  geom_histogram(binwidth=1,color="black")+
  scale_x_continuous(trans="log2")+
  facet_grid(.~group)
#now faceting by year also to know the evolution after years
past_year<-1970
present_year <-2010
west <- c("Western Europe","Northern Europe","Southern Europe","Northern America",
          "Australia and New Zealand")
gapminder %>% 
  filter (year%in%c(past_year,present_year) & !is.na(gdp)) %>%
  mutate(group=ifelse(region %in% west, "West","Developing"))%>%
  ggplot(aes(dollars_per_day))+
  geom_histogram(binwidth=1,color="black")+
  scale_x_continuous(trans="log2")+
  facet_grid(year~group)
#NOw only countrues in both years
country_list_1<-gapminder %>% 
  filter (year%in%past_year & !is.na(gdp))  %>%.$country
country_list_2<-gapminder %>% 
  filter (year%in%present_year & !is.na(gdp)) %>%.$country
country_list<- intersect(country_list_1,country_list_2)
gapminder %>% 
  filter (year%in%c(past_year,present_year) & country %in% country_list & !is.na(gdp)) %>%
  mutate(group=ifelse(region %in% west, "West","Developing"))%>%
  ggplot(aes(dollars_per_day))+
  geom_histogram(binwidth=1,color="black")+
  scale_x_continuous(trans="log2")+
  facet_grid(year~group)
#box plot with year 2010
gapminder %>% 
  filter (year%in%c(past_year,present_year) & country %in% country_list & !is.na(gdp)) %>%
  mutate(region=reorder(region,dollars_per_day,FUN=median))%>%
  ggplot()+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  xlab("")+  scale_y_continuous(trans="log2")+
  geom_boxplot(aes(region,dollars_per_day,fill=continent))+
  facet_grid(year~.)
#easier to compare without faceting
gapminder %>% 
  filter (year%in%c(past_year,present_year) & country %in% country_list & !is.na(gdp)) %>%
  mutate(region=reorder(region,dollars_per_day,FUN=median))%>%
  ggplot()+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  xlab("")+  scale_y_continuous(trans="log2")+
  geom_boxplot(aes(region,dollars_per_day,fill=factor(year)))
#Number of countries in each group (west developing)
gapminder %>% 
  filter (year%in%c(past_year,present_year) & country %in% country_list & !is.na(gdp)) %>%
  mutate(group=ifelse(region %in% west, "West","Developing"))%>%group_by(group)%>%
  summarize(n=n()) %>% knitr ::kable()

#density plots
gapminder %>% 
  filter (year%in%c(past_year,present_year) & country %in% country_list & !is.na(gdp)) %>%
  mutate(group=ifelse(region %in% west, "West","Developing"))%>%
  ggplot(aes(dollars_per_day,y=..count..,fill=group))+
  scale_x_continuous(trans="log2")+
  geom_density(alpha=0.2,bw=0.75)+facet_grid(year~.)
#Show regions in the density plot
data(gapminder)
gapminder<-gapminder %>% 
  mutate(group=case_when(
    .$region %in%west ~"West",
    .$region %in% c("Eastern Asia","South-Eastern Asia")~"East Asia",
    .$region %in% c("Caribbean","Central America","South America") ~"Latin America",
    .$continent=="Africa" & .$region !="Northenr Africa" ~"Sub-Saharian Africa",
    TRUE ~"Others"))
gapminder<-gapminder %>% 
  mutate(group=factor(group,levels=c("Others","Latin America","East Asia",
                                     "Sub-Saharian Africa","West")))
gapminder<- gapminder %>%
  mutate(dollars_per_day=gdp/population/365)
gapminder %>%  
  filter (year%in%c(past_year,present_year) & country %in% country_list & !is.na(gdp)) %>%
  ggplot(aes(dollars_per_day,y=..count..,fill=group,color =group))+
  scale_x_continuous(trans="log2")+
geom_density(alpha=0.2,bw=0.75)+facet_grid(year~.)

#Now weight by population and stacking
gapminder %>%  
  filter (year%in%c(past_year,present_year) & country %in% country_list & !is.na(gdp)) %>%
  group_by(year)%>%
  mutate(weight=population/sum(population)*2) %>%
  ungroup()%>%
  ggplot(aes(dollars_per_day,fill=group,weight =weight))+
  scale_x_continuous(trans="log2")+
  geom_density(alpha=0.2,bw=0.75,position="stack")+facet_grid(year~.)

#
#
#Ecological falacie
#Show regions in the density plot
data(gapminder)
gapminder<-gapminder %>% 
  mutate(group=case_when(
    .$region %in%west ~"The West",
    .$region %in%"Northern Africa" ~"Northern Africa",
    .$region %in% c("Eastern Asia","South-Eastern Asia")~"East Asia",
    .$region %in%"Southern Asia" ~"Southern Asia",
    .$region %in% c("Caribbean","Central America","South America") ~"Latin America",
    .$continent=="Africa" & .$region !="Northenr Africa" ~"Sub-Saharian Africa",
    .$region %in% c("Melansia","Micronesia","Polinesia") ~"Pacific Islands"))
surv_income<-gapminder %>%  
  filter (year%in%present_year & !is.na(gdp)  & !is.na(infant_mortality)  & !is.na(group)) %>%
  group_by(group) %>%
  summarize (income = sum(gdp)/sum(population)/365,
             infant_survival_rate=1-sum(infant_mortality/1000*population)/sum(population))
#surv_income %>% arrange(income)
surv_income %>% ggplot(aes(income,infant_survival_rate,label=group,color=group))+
  scale_x_continuous(trans ="log2",limit=c(.5,150))+
  scale_y_continuous(trans="logit",limit=c(.875,.9981),
                     breaks =c(.85,.90,.95,.99,.995,.998))+
  geom_label(size=3,show.legend=FALSE)

#by country
data(gapminder)
gapminder<-gapminder %>% 
  mutate(group=case_when(
    .$region %in%west ~"The West",
    .$region %in%"Northern Africa" ~"Northern Africa",
    .$region %in% c("Eastern Asia","South-Eastern Asia")~"East Asia",
    .$region %in%"Southern Asia" ~"Southern Asia",
    .$region %in% c("Caribbean","Central America","South America") ~"Latin America",
    .$continent=="Africa" & .$region !="Northenr Africa" ~"Sub-Saharian Africa",
    .$region %in% c("Melansia","Micronesia","Polinesia") ~"Pacific Islands"))
gapminder<-gapminder %>% 
  filter (year%in%present_year & !is.na(gdp)  & !is.na(infant_mortality)  & 
            !is.na(group)) %>%
group_by(country,group) %>%
summarize (income = sum(gdp)/sum(population)/365,
        infant_survival_rate=1-sum(infant_mortality/1000*population)/sum(population))
ggplot(gapminder,aes(income,infant_survival_rate,color=group,label=country))+
  scale_y_continuous(trans="logit",limit=c(.875,.9981))+
  scale_x_continuous(trans ="log2",limit=c(.5,150))+
  geom_point(size=3)

library(dslabs)

