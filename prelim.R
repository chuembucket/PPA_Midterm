#PPA midterm

library(tidyverse)
library(sf)
library(tidycensus)
library(tigris)
library(corrr)
library(Metrics)




options(tigris_use_cache = TRUE)
options(scipen = 999)


source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")
census_api_key("746ea8916547306ae2abf2aafe059e1a1b70b98a", overwrite = TRUE)


char.homesales <- read_sf('https://raw.githubusercontent.com/mafichman/MUSA_508_Lab/main/Midterm/data/2022/studentData.geojson')

char.census <- get_acs(geography = "tract", 
                       variables = c("B25026_001E"), 
                       year=2020, state='North Carolina', county='Mecklenburg', 
                       geometry=TRUE, output="wide") %>%
  st_transform(st_crs(char.homesales)) %>%
  rename(TotalPop = B25026_001E, 
         ) %>%
  dplyr::select(-NAME, -starts_with("B"))

elemschools <- read_sf('C:/Users/cchue/Documents/Penn MUSA/Public Policy Analytics/Midterm/SchoolBoundary_Elementary.shp')
highschools <- read_sf('C:/Users/cchue/Documents/Penn MUSA/Public Policy Analytics/Midterm/SchoolBoundary_High.shp')


ggplot(highschools)+
  geom_sf(aes(fill = HIGH_NAME), color = NA)







ggplot()+
  geom_sf(data = char.census)+
  geom_sf(data = char.homesales, aes(color = q5(totalac)))



#modelling 
homesales<- char.homesales %>% filter(toPredict != 'CHALLENGE')
competition <- homesales


inputdf <- homesales %>% st_drop_geometry() %>% 
  select(price, bedrooms, extwall, actype, numfirepla, bldggrade, fullbaths, zipcode) %>% na.omit()

model1 <- lm(price ~ ., data = inputdf)

summary(model1)
mae(inputdf$price, predict(model1))
mape(inputdf$price, predict(model1))

competition$predict <- predict(model1 , homesales %>% 
                         select(price, bedrooms, extwall, actype, numfirepla, bldggrade, fullbaths, zipcode) %>% na.omit()
) 





inputdf %>% 
  correlate() %>% 
  autoplot() +
  geom_text(aes(label = round(r,digits=2)),size = 2)



scatter.smooth(char.homesales$totalac, char.homesales$price)
plot(model1)


