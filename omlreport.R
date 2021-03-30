##############################################################################

#Loading Packages
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)

##############################################################################

#Importing data
setwd("~/academic/c_class/c_calculus/HL IA")
oml_2018_19 <- read.csv(file = "oml2018-19.csv")
oml_2017_18 <- read.csv(file = "oml2017-18.csv")
oml_2016_17 <- read.csv(file = "oml2016-17.csv")
oml_2015_16 <- read.csv(file = "oml2015-16.csv")
oml_2014_15 <- read.csv(file = "oml2014-15.csv")
oml_2013_14 <- read.csv(file = "oml2013-14.csv")
oml_2012_13 <- read.csv(file = "oml2012-13.csv")

oml_2018_19 <- oml_2018_19 %>%
    mutate(year = 2019)
oml_2017_18 <- oml_2017_18 %>%
    mutate(year = 2018)
oml_2016_17 <- oml_2016_17 %>%
    mutate(year = 2017)
oml_2015_16 <- oml_2015_16 %>%
    mutate(year = 2016)
oml_2014_15 <- oml_2014_15 %>%
    mutate(year = 2015)
oml_2013_14 <- oml_2013_14 %>%
    mutate(year = 2014)
oml_2012_13 <- oml_2012_13 %>%
     mutate(year = 2013)
##############################################################################

#Merging tables
fulloml <- rbind(oml_2018_19,oml_2017_18, oml_2016_17, oml_2015_16, oml_2014_15, oml_2013_14, oml_2012_13)

##############################################################################

#Graphing histograms to view distribution 
# oml_2018_19 %>%
# ggplot( aes(x = Pts)) +
#   geom_histogram(aes(x = Pts), binwidth = 30) +
#   labs(title = "oml_18_19") +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# oml_2017_18 %>%
#   ggplot( aes(x = Pts)) +
#   geom_histogram(aes(x = Pts), binwidth = 30) +
#   labs(title = "oml_17_18") +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# oml_2016_17 %>%
#   ggplot( aes(x = Pts)) +
#   geom_histogram(aes(x = Pts), binwidth = 30) +
#   labs(title = "oml_16_17") +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# oml_2015_16 %>%
#   ggplot( aes(x = Pts)) +
#   geom_histogram(aes(x = Pts), binwidth = 30)+
#   labs(title = "oml_15_16") +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# oml_2014_15 %>%
#   ggplot( aes(x = Pts)) +
#   geom_histogram(aes(x = Pts), binwidth = 30) +
#   labs(title = "oml_14_15") +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# oml_2013_14 %>%
#   ggplot( aes(x = Pts)) +
#   geom_histogram(aes(x = Pts), binwidth = 30) +
#   labs(title = "oml_13_14") +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# oml_2012_13 %>%
#   ggplot( aes(x = Pts)) +
#   geom_histogram(aes(x = Pts), binwidth = 30) +
#   labs(title = "oml_12_13") +
#   theme(plot.title = element_text(hjust = 0.5))

##############################################################################

fulloml %>%
  ggplot( aes(x = year, y = Pts)) +
  geom_point(aes(x = fulloml$year, y = fulloml$Pts), position = "jitter", alpha = 0.5) +
  labs(title = "OML Points by Season") +
  ylab("Total Points") +
  xlab("Season") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) 

ptsyear_linearregression <- summary(lm(fulloml$Pts~fulloml$year))
ptsyear_lmcorrelation <- cor(fulloml$Pts,fulloml$year)

fulloml %>%
  ggplot( aes(x = Pts)) +
  geom_histogram(aes(x = Pts), binwidth = 10, alpha = 0, col="black") +
  labs(title = "Distribution of OML Points") +
  stat_ecdf() +
  ylab("Frequency") +
  xlab("Total Points") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) 

fulloml %>%
  ggplot( aes(x = Pts)) +
  geom_boxplot(aes(x = Pts)) +
  labs(title = "Distribution of OML Point Totals") +
  theme_classic() +
theme(plot.title = element_text(hjust = 0.5))

fulloml %>%
  ggplot( aes(x = Pts)) +
  geom_density(aes(x = Pts)) +
  labs(title = "OML") +
  theme(plot.title = element_text(hjust = 0.5))

dense = density(fulloml$Pts)
pdf <- approxfun(dense)

fulloml %>%
  ggplot( aes(x = Pts)) +
  geom_histogram(aes(x = Pts,y = stat(count) / sum(count)), alpha = 0, col="black") +
  labs(title = "OML Cumulative Relative Frequency Histogram") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::percent) +
  stat_ecdf() +
  scale_x_continuous() +
  ylab("Relative Frequency") +
  xlab("Total Points") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) 


  
x <- c(6,6+1*(146/15),6+2*(146/15),6+3*(146/15),6+4*(146/15),6+5*(146/15),6+6*(146/15),6+7*(146/15),6+8*(146/15),6+9*(146/15),6+10*(146/15),6+11*(146/15),6+12*(146/15),6+13*(146/15),6+14*(146/15),6+15*(146/15),6+16*(146/15),6+17*(146/15),6+18*(146/15),6+19*(146/15),6+20*(146/15),6+21*(146/15),6+22*(146/15),6+23*(146/15),6+24*(146/15),6+25*(146/15),6+1*(146/15),6+26*(146/15),6+27*(146/15),6+28*(146/15))
w <- c(6+1*(146/15),6+2*(146/15),6+3*(146/15),6+4*(146/15),6+5*(146/15),6+6*(146/15),6+7*(146/15),6+8*(146/15),6+9*(146/15),6+10*(146/15),6+11*(146/15),6+12*(146/15),6+13*(146/15),6+14*(146/15),6+15*(146/15),6+16*(146/15),6+17*(146/15),6+18*(146/15),6+19*(146/15),6+20*(146/15),6+21*(146/15),6+22*(146/15),6+23*(146/15),6+24*(146/15),6+25*(146/15),6+1*(146/15),6+26*(146/15),6+27*(146/15),6+28*(146/15),6+29*(146/15))
y = c(0.006968, 0.013937, 0.027874, 0.041811, 0.050522, 0.06620202, 0.050522, 0.0662021, 0.0452961, 0.0557491, 0.057491, 0.0470383, 0.0540069, 0.0522648, 0.026132, 0.0278745, 0.040069, 0.029616, 0.0365853, 0.0348432, 0.0209059, 0.02264808, 0.0139372, 0.019163, 0.026132, 0.02264808, 0.0174216, 0.0156794, 0.008710801, 0.001742)
histogram <- data.frame(x, y)


histogram %>%
  ggplot( aes(x = x, y = y)) +
  geom_line(data = histogram, aes(x = x, y = y)) 


calctrap <- function(b1, b2, h) {
  A <- 1/2*(146/15)*h
  return(A)
}

sum(calctrap(x,w,y))

##############################################################################

mpioml <- fulloml %>%
  filter(Leibniz == "Mid-Pacific")

mpioml %>%
ggplot( aes(x = Pts,y = stat(count) / sum(count))) +
  geom_histogram(aes(x = Pts,y = stat(count) / sum(count)), bins = 11) +
  stat_count(aes(y=tat(count) / sum(count),label=round(stat(count) / sum(count)),geom="text"))


omlpoints <- fulloml %>%
  transmute(Pts = as.numeric(Pts))

bins <- seq(6, 298, by= 10)
Points <- cut(omlpoints$Pts, bins)
frequencytable <- transform(table(Points),Rel_Ref = prop.table(Freq), Cum_Freq=cumsum(Freq))
