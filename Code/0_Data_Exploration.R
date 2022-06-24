library(dplyr)
library(haven)

turkey2002 <- read_dta("Turkey-2002--full-data-5.dta")
turkey2005 <- read_dta("Turkey-2005--full data-5.dta")
turkey2008 <- read_dta("Turkey-2008--full-data-.dta")
turkey2013 <- read_dta("Turkey-2013-full-data.dta")
turkey2015 <- read_dta("Turkey-2015-full-data.dta")
turkey2019 <- read_dta("Turkey-2019-full-data.dta")

ecpi <- read_dta("agg_ecpi.dta")
annual_inflation <- read_dta("annual.dta")

subset_turkey2019 <- turkey2019[c(1,2,3,5,11,12,13,16,26,27,28,29,32,33,34,35,42,43,48,51,60,61,62,63,64,70,71,72,74,75,76,77,78,79,80,81,82,105,120,121,122,128,167,189,301,310,311,312,313,314,315,344,345,372,373,375,376,377,378,381,382,383,384,385,386,387,388,389,392,393,394,395,396,397,398,399,400,401,403,404,405,406,407,408,409,410,411,412,413,423,424,425,426,427,428,429)]

sum(is.na.data.frame(subset_turkey2019))  # 41321 entries NA

sum(is.na.data.frame(turkey2008))  # 95002/324864 entries NA

library(tidyverse)

SC_turkey2008 <- turkey2008 %>% select(a1, a4b, a14m, a14y, b1, b3, b2a, b2b, b2c, ECAq5, b5, c6, c7, c8, c9b, c11, c15, c16, c17, c23, c24b, c24c, c24d, c25, c26, c27, c30a, c30b, d1a2, d1a3, d2, d30a, e3, e4, e16, e17, ECAq53, ECAo1, ECAo2, ECAo11, ECAo12, ECAo14, ECAo15, f1,f2,k4, n5a, n5b, l1, l2, l3a, l3b, l4a, l4b, l10, l11a, l11b)
SC_turkey2008$country <- 'Turkey'

SC_turkey2013 <- turkey2013 %>% select(a1, a4b, a14m, a14y, b1, b3, b2a, b2b, b2c, ECAq5, b5, c6, c7, c8, c9b, c11, c15, c16, c17, c23, c30a, c30b, d1a2, d1a3, d2, d30a, ECAq53, f1, f2, k4, n5a, n5b, l1, l2, l3a, l3b, l4a, l4b, l10, l11a, l11b)
SC_turkey2013$country <- 'Turkey'

SC_turkey2015 <- turkey2015 %>% select(a1, a4b, a14m, a14y, tub1, b3, b2a, b2b, b2c, b5, c6, c7, c8a, c8b, c9a, c11, c15, c16, c17, tuc10, tuc11, tuc12, tuc13, d1a2, d1a3, d2, f1,f2,k4, n5a, n5b, l1, l2, l5, l6, l6a, l8)
SC_turkey2015$country <- 'Turkey'
names(SC_turkey2015)[names(SC_turkey2015) == 'tub1'] <- 'b1' 

SC_turkey2019 <- turkey2019 %>% select(a1, a4b, a14m, a14y, b1, b3, b2a, b2b, b2c, b5, c6, c7, c8a, c8b, c9a, c9b, c11, c15, c16, c17, c30a, d1a2, d1a3, d2, d30a, f1,f2,k4, n5a, n5b, l1, l2, l3a, l3b)
SC_turkey2019$country <- 'Turkey'

total_1 <- merge(SC_turkey2015, SC_turkey2019, all.x = TRUE, all.y = TRUE)

inflation_monthly <- read_dta("monthly.dta")

inflation_monthly$months %% 100

inflation_monthly$month <- inflation_monthly$months %% 100
inflation_monthly$year <- inflation_monthly$months %/% 100

inflation_monthly <- subset(inflation_monthly, months > 199900)
inflation_monthly <- subset(inflation_monthly, country == "Turkey")
inflation_monthly <- inflation_monthly[c(1,4,3)]

library(dplyr)
total_1 <- full_join(x = SC_turkey2008, y = SC_turkey2013)
total_2 <- full_join(x= total_1, y = SC_turkey2015)
total_3 <- full_join(x= total_2, y= SC_turkey2019)

total_3$yearmonthtry <- paste(total_3$a14y, total_3$a14m)
total_3$yearmonth <- gsub(" ", "", total_3$yearmonthtry)
total_3 <- total_3[c(-71)]

inflation_monthly$yearmonth <- paste(inflation_monthly$year, inflation_monthly$month)
inflation_monthly$yearmonth <- gsub(" ", "")

annual_inflation <- subset(annual_inflation, year > 1999)
annual_inflation <- subset(annual_inflation, country == "Turkey")
annual_inflation <- annual_inflation[c(2,13)]

names(total_3)[names(total_3) == 'a14y'] <- 'year' 

total_4 <- full_join(x=total_3, y=annual_inflation, by = NULL)

library(ggplot2)
library("car")

poweroutage_SC <- ggplot(data = total_4, mapping = aes(x = c7, y = f1)) +
  geom_point(size=1) 
#  geom_line(size=1)
poweroutage_SC

try <- lm(total_4$f1 ~ poly(total_4$c7, raw=TRUE))
summary(try)

water_SC <- ggplot(data= total_4, mapping=aes(x=c16, y=f1)) +
  geom_point(size=1) +
  geom_smooth(size=1)
water_SC

inflation_SC <- ggplot(data=total_4, mapping=aes(x=ecpi_a, y=f1)) +
  geom_point(size=1) 
inflation_SC

inflation <- ggplot(data=total_4, mapping=aes(x=year, y=ecpi_a)) +
  geom_point(size=1) +
  geom_smooth(size=1)
inflation

turkeySC2008 <- subset(total_4, year==2008)
mean(turkeySC2008$f1, na.rm=TRUE)

turkeySC2013 <- subset(total_4, year==2013)
mean(turkeySC2013$f1, na.rm=TRUE)

turkeySC2015 <- subset(total_4, year==2015)
mean(turkeySC2015$f1, na.rm=TRUE)

turkeySC2019 <- subset(total_4, year==2019)
mean(turkeySC2019$f1, na.rm=TRUE)

year <- c("2008", "2013", "2015", "2019")
inflation <- c("10.7", "14.195865", "1.32432", "7.9164")
operating_capacity <- c("61.32558", "58.13448", "69.47742", "65.71535")

df <- data.frame(year, inflation, operating_capacity)

inflation_oc <- ggplot(data=df, mapping=aes(x=inflation, y=operating_capacity)) +
  geom_point(size=1) +
  geom_smooth(size=1)
inflation_oc



