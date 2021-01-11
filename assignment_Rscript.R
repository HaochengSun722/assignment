library(spatstat)
library(here)
library(sp)
library(rgeos)
library(maptools)
library(GISTools)
library(tmap)
library(sf)
library(geojson)
library(geojsonio)
library(tmaptools)
library(stringr)
library(tidyverse)
library(raster)#DBSCAN
library(fpc)#DBSCAN
library(dbscan)
library(ggplot2)
library(OpenStreetMap)
library(spdep)
library(caret)
library(car)
library(broom)
library(corrr)
library(spgwr)

##########################3
Heatland <- st_read(here::here("data", 
                                     "GLA 2016 Summer Urban Heat Island", 
                                     "GLA_UHI_2016.shp"))%>%
  st_transform(., 27700)
qtm(Heatland)
plot(Heatland)

fast_food_point <- st_read(here::here("data", "osm",
                               "fast_food", 
                               "amenity_fast_food_London_points.geojson"))%>%
  st_transform(., 27700)
plot(fast_food_point)

MSOA <- st_read(here::here("data",
                             "18_lsoa.json"))%>%
  st_transform(., 27700)
plot(MSOA)

LondonBoroughs <- st_read(here::here("data", 
                                     "ESRI", 
                                     "London_Borough_Excluding_MHW.shp"))
BoroughMap <- LondonBoroughs %>%
  dplyr::filter(stringr::str_detect(GSS_CODE, "^E09"))%>%
  st_transform(., 27700)
qtm(BoroughMap)

LondonWards <- st_read(here::here("data","London-wards-2018_ESRI", "London_Ward.shp"))
plot(LondonWards)

LondonWardsMerged <- st_read(here::here("data", 
                                        "statistical-gis-boundaries-london", 
                                        "ESRI",
                                        "London_Ward_CityMerged.shp"))%>%
  st_transform(.,27700)
plot(LondonWardsMerged)

census <- read_csv("data/ward-atlas-data.csv") %>% 
  na = c("NA", "n/a") %>% 
  janitor::clean_names()

WardData <- read_csv("https://data.london.gov.uk/download/ward-profiles-and-atlas/772d2d64-e8c6-46cb-86f9-e52b4c7851bc/ward-profiles-excel-version.csv", 
                       na = c("NA", "n/a")) %>% 
  janitor::clean_names()

LondonWardsObesity <- LondonWardsMerged %>% 
  left_join(census, 
            by = c("GSS_CODE" = "x2"))%>%
  distinct(GSS_CODE, NAME, childhood_obesity_prevalence_year_6_age_10_11_2010_11_to_2012_13_percent_obese)

st_crs(LondonWardsObesity)
names(LondonWardsObesity) <- c('NAME','GSS_CODE','Childhood_Obesity',"GEO")
st_geometry(LondonWardsObesity)<- "GEO"
plot(LondonWardsObesity) 
max(LondonWardsObesity$Childhood_Obesity)

breaks=c(0,10,15,20,25,30,35,40)
tmap_mode("plot")
plot1 <- tm_shape(LondonWardsObesity) + 
  tm_polygons("Childhood_Obesity",
              breaks=breaks, 
              palette="PuBu")+tm_layout(title = "Distribution of Childhood Obesity (%) Map", title.position = c("left","top"),title.size=.8)+
  tm_scale_bar(position = c("left", "bottom"), text.size = .5)+
  tm_layout(legend.position = c("right","bottom"), 
            legend.text.size=.75, 
            legend.title.size = 1,
            frame=FALSE)+
  tm_credits("Data source from 2011 Census.Created by author.", position=c(0.0,0.0))+
  #tm_text(text = "NAME", size = .5, along.lines =T, remove.overlap=T,  auto.placement=F)+
  tm_compass(type = "8star", position = c(0.02, 0.09)) +
  
  #bottom left top right
  tm_layout(inner.margin=c(0.02,0.02,0.02,0.2))

plot1

str(LondonWardsObesity)
LondonWardsObesity$Childhood_Obesity=as.numeric(unlist(LondonWardsObesity$Childhood_Obesity))
summary(LondonWardsObesity$Childhood_Obesity)
tmap_save(plot1, 'Childhood_Obesity_distribution.png',dpi = 300)

hist(LondonWardsObesity$Childhood_Obesity, 
     breaks=breaks, 
     col="lightblue", 
     main="Histogram of Childhood Obesity in London", 
     xlab="Prevalence of Childhood Obesity", 
     ylab="Frequency")

ggplot(LondonWardsObesity, aes(x=Childhood_Obesity, color="black", fill="lightblue")) + 
  geom_histogram(aes(y=..density../sum(..density..)), alpha=0.5, 
                 position="identity")+
  geom_density(alpha=.5)

ggplot(LondonWardsObesity, aes(x=Childhood_Obesity)) +
  geom_histogram(aes(y=..density../sum(..density..)),position="identity", alpha=0.5,color="black", 
                 fill="lightblue")+
  geom_vline(aes(xintercept=mean(Childhood_Obesity, 
                                 na.rm=TRUE)),
             linetype="dashed")+geom_density(alpha=.3,color="red",size=.8)+
  labs(title="Histogram of Childhood Obesity in London",
       x="Prevalence of Childhood Obesity",
       y="Frequency")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

ggsave("Histogram of Childhood Obesity in London.png",dpi=500)

######Moran's I
coordsW <- LondonWardsObesity%>%
  st_centroid()%>%
  st_geometry()

plot(coordsW,axes=TRUE)
#二进制空间weight
LWard_nb <- LondonWardsObesity %>%
  poly2nb(., queen=T)

#确定knn的k
set.seed(3033)
intrain <- createDataPartition(y = LondonWardsObesity$Childhood_Obesity, p= 0.7, list = FALSE)
training <- LondonWardsObesity[intrain,]
testing <- LondonWardsObesity[-intrain,]
str(training)
testing<- testing %>%
  st_drop_geometry()
training<- training %>%
  st_drop_geometry()
grid = expand.grid(.k = seq(4, 20, by = 1))

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3333)
knn_fit <- train(Childhood_Obesity~., data = training, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneGrid = grid)

knn_fit
plot2 <- plot(knn_fit)
plot2
ggsave('RMSE&K.png',dpi = 300)
knn.pred_new = predict(knn_fit,testing)
#KNN空间weight
knn_wards <-coordsW %>%
  knearneigh(., k=4)

knn_wards5 <-coordsW %>%
  knearneigh(., k=5)

LWard_knn <- knn_wards %>%
  knn2nb()

LWard_knn5 <- knn_wards5 %>%
  knn2nb()
#plot them
plot(LWard_nb, st_geometry(coordsW), col="red")

plot(LWard_knn, st_geometry(coordsW), col="blue")
#add a map underneath
plot(LondonWardsObesity$GEO, add=T)

Lward.queens_weight <- LWard_nb %>%
  nb2listw(., style="C")

Lward.knn_4_weight <- LWard_knn %>%
  nb2listw(., style="C")

Lward.knn_5_weight <- LWard_knn5 %>%
  nb2listw(., style="C")

I_LWard_Global_Obesity <- LondonWardsObesity %>%
  pull(Childhood_Obesity) %>%
  as.vector()%>%
  moran.test(., Lward.queens_weight)

Knn_LWard_Global_Obesity <- LondonWardsObesity %>%
  pull(Childhood_Obesity) %>%
  as.vector()%>%
  moran.test(., Lward.knn_4_weight)

Knn_LWard_Global_Obesity5 <- LondonWardsObesity %>%
  pull(Childhood_Obesity) %>%
  as.vector()%>%
  moran.test(., Lward.knn_5_weight)

I_LWard_Global_Obesity
Knn_LWard_Global_Obesity
Knn_LWard_Global_Obesity5

##计算空间自相关，②Geary's C
C_LWard_Global_Obesity <- 
  LondonWardsObesity %>%
  pull(Childhood_Obesity) %>%
  as.vector()%>%
  geary.test(., Lward.knn_5_weight)

C_LWard_Global_Obesity

#Local Moran's I
I_LWard_Local_Obesity <- LondonWardsObesity %>%
  pull(Childhood_Obesity) %>%
  as.vector()%>%
  localmoran(., Lward.knn_5_weight)%>%
  as_tibble()

#Getis OrdG∗i
Gi_LWard_Local_Obesity <- LondonWardsObesity %>%
  pull(Childhood_Obesity) %>%
  as.vector()%>%
  localG(., Lward.knn_5_weight)

LondonWardsObesity <- LondonWardsObesity %>%
  mutate(Childhood_Obesity_G = as.numeric(Gi_LWard_Local_Obesity))

LondonWardsObesity <- LondonWardsObesity %>%
  mutate(Childhood_Obesity_I = as.numeric(I_LWard_Local_Obesity$Ii))%>%
  mutate(Childhood_Obesity_Iz =as.numeric(I_LWard_Local_Obesity$Z.Ii))

summary(LondonWardsObesity$Childhood_Obesity_Iz)

MoranColours<- rev(brewer.pal(8, "RdBu"))#创建调色板
breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)#手动设计break
plot3 <- tm_shape(LondonWardsObesity) +
  tm_polygons("Childhood_Obesity_Iz",
              legend.hist=TRUE,
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA)+
tm_layout(title = "Local Moran's I of Childhood Obesity in London", title.position = c("left","top"),title.size=3)+
  tm_scale_bar(position = c("left", "bottom"), text.size = .4)+
  tm_layout(legend.outside = TRUE,legend.position = c("right","bottom"), 
            legend.text.size=.75, 
            legend.title.size = .8,
            frame=FALSE)+
  tm_credits("Data source from 2011 Census.Created by author.", position=c(0.0,0.0))+
  #tm_text(text = "NAME", size = .5, along.lines =T, remove.overlap=T,  auto.placement=F)+
  tm_compass(type = "8star", position = c(0.02, 0.09)) +
tm_shape(BoroughMap)+tm_borders("black")

plot3

tmap_save(plot3, 'Local_Moran_Childhood_Obesity_in_London.png',dpi = 300)

plot4 <- tm_shape(LondonWardsObesity) +
  tm_polygons("Childhood_Obesity_G",
              legend.hist=TRUE,
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Getis Ord, Childhood Obesity in London")+
  tm_layout(title = "Getis Ord of Childhood Obesity in London", title.position = c("left","top"),title.size=3)+
  tm_scale_bar(position = c("left", "bottom"), text.size = .4)+
  tm_layout(legend.outside = TRUE,legend.position = c("right","bottom"), 
            legend.text.size=.75, 
            legend.title.size = .8,
            frame=FALSE)+
  tm_credits("Data source from 2011 Census.Created by author.", position=c(0.0,0.0))+
  #tm_text(text = "NAME", size = .5, along.lines =T, remove.overlap=T,  auto.placement=F)+
  tm_compass(type = "8star", position = c(0.02, 0.09)) +
  tm_shape(BoroughMap)+tm_borders("black")

plot4
tmap_save(plot4, 'Getis_Ord_Childhood_Obesity_in_London.png',dpi = 300)

#####👆空间聚集性，SpatialAutocorrelation，👇Regression

census <- read_csv("data/ward-atlas-data.csv") %>% 
  na = c("NA", "n/a") %>% 
  janitor::clean_names()

fast_food_point <- st_read(here::here("data", "osm",
                                      "fast_food", 
                                      "amenity_fast_food_London_points.geojson"))%>%
  st_transform(., 27700)

LondonWardsObesity_Regression <- LondonWardsObesity %>% 
  left_join(census, 
            by = c("GSS_CODE" = "x2"))%>%
  distinct(GSS_CODE, NAME,GEO,Childhood_Obesity,Childhood_Obesity_I,Childhood_Obesity_Iz,Childhood_Obesity_G,
           household_income_mean_modelled_household_income_2012_13,qualifications_qualifications_and_students_2011_census_percent_no_qualifications,
           out_of_work_families_rates_children_0_18_living_in_out_of_work_benefit_claimant_households_2011,
           children_in_poverty_percent_of_children_aged_under_16_in_poverty_2011,
           car_access_cars_per_household_census_2011,
           access_to_green_space_and_nature_percent_homes_with_access_to_local_small_or_pocket_park,
           travel_to_work_by_bicycle_2011_census_percent_travel_by_bicycle)

leisure_point <- st_read(here::here("data", "leisure",
                                      "leisure.shp"))%>%
  st_transform(., 27700)

alcohol_point <- st_read(here::here("data", "shop",
                                      "alcohol.shp"))%>%
  st_transform(., 27700)

beverages_point <- st_read(here::here("data", "shop",
                                    "beverages.shp"))%>%
  st_transform(., 27700)

confectionery_point <- st_read(here::here("data", "shop",
                                      "confectionery.shp"))%>%
  st_transform(., 27700)

convenience_point <- st_read(here::here("data", "shop",
                                          "convenience.shp"))%>%
  st_transform(., 27700)

departmentstore_point <- st_read(here::here("data", "shop2",
                                        "departmentstore.shp"))%>%
  st_transform(., 27700)

farm_point <- st_read(here::here("data", "shop2",
                                            "farm.shp"))%>%
  st_transform(., 27700)

greengrocer_point <- st_read(here::here("data", "shop2",
                                            "greengrocer.shp"))%>%
  st_transform(., 27700)

grocery_point <- st_read(here::here("data", "shop2",
                                        "grocery.shp"))%>%
  st_transform(., 27700)

mall_point <- st_read(here::here("data", "shop2",
                                    "mall.shp"))%>%
  st_transform(., 27700)

supermarket_point <- st_read(here::here("data", "shop2",
                                    "supermarket.shp"))%>%
  st_transform(., 27700)

leisureSub <- leisure_point[LondonWardsMerged,]
fastfoodSub <- fast_food_point[LondonWardsMerged,]
alcoholSub <- alcohol_point[LondonWardsMerged,]
beveragesSub <- beverages_point[LondonWardsMerged,]
confectionerySub <- confectionery_point[LondonWardsMerged,]
convenienceSub <- convenience_point[LondonWardsMerged,]
departmentstoreSub <- departmentstore_point[LondonWardsMerged,]
farmSub <- farm_point[LondonWardsMerged,]
greengrocerSub <- greengrocer_point[LondonWardsMerged,]
grocerySub <- grocery_point[LondonWardsMerged,]
mallSub <- mall_point[LondonWardsMerged,]
supermarketSub <- supermarket_point[LondonWardsMerged,]

plot(leisureSub)

LondonWardsObesity_Regression1 <- LondonWardsObesity_Regression%>%
  st_join(leisureSub)%>%#将两个dataframe合并
  add_count(NAME)%>%#统计wardname中各observation的数量
  janitor::clean_names()%>%
  #calculate area
  mutate(area=st_area(.))%>%#计算出各area的面积
  #then density of the points per ward
  mutate(leisuredensity=n/area) %>% 
  dplyr::select(leisuredensity, name, gss_code, n)

LondonWardsObesity_Regression1<- LondonWardsObesity_Regression1 %>%                    
  group_by(gss_code) %>%  ##根据邮编进行分类，得到所有Ward的dataframe       
  summarise(leisure_density = first(leisuredensity),##永远记住 group_by()+summarise/summary()无论python&R
            wardname= first(name),
            leisurecount= first(n))
st_write(LondonWardsObesity_Regression1,"1.csv")

LondonWardsObesity_Regression2 <- LondonWardsObesity_Regression%>%
  st_join(fastfoodSub)%>%#将两个dataframe合并
  add_count(NAME)%>%#统计wardname中各observation的数量
  janitor::clean_names()%>%
  #calculate area
  mutate(area=st_area(.))%>%#计算出各area的面积
  #then density of the points per ward
  mutate(fastfooddensity=n/area) %>% 
  dplyr::select(fastfooddensity, name, gss_code, n)

LondonWardsObesity_Regression2<- LondonWardsObesity_Regression2 %>%                    
  group_by(gss_code) %>%  ##根据邮编进行分类，得到所有Ward的dataframe       
  summarise(fastfood_density = first(fastfooddensity),##永远记住 group_by()+summarise/summary()无论python&R
            wardname= first(name),
            fastfoodcount= first(n))
st_write(LondonWardsObesity_Regression2,"2.csv")

LondonWardsObesity_Regression3 <- LondonWardsObesity_Regression%>%
  st_join(alcoholSub)%>%#将两个dataframe合并
  add_count(NAME)%>%#统计wardname中各observation的数量
  janitor::clean_names()%>%
  #calculate area
  mutate(area=st_area(.))%>%#计算出各area的面积
  #then density of the points per ward
  mutate(alcoholdensity=n/area) %>% 
  dplyr::select(alcoholdensity, name, gss_code, n)

LondonWardsObesity_Regression3<- LondonWardsObesity_Regression3 %>%                    
  group_by(gss_code) %>%  ##根据邮编进行分类，得到所有Ward的dataframe       
  summarise(alcohol_density = first(alcoholdensity),##永远记住 group_by()+summarise/summary()无论python&R
            wardname= first(name),
            alcoholcount= first(n))
st_write(LondonWardsObesity_Regression3,"3.csv")

LondonWardsObesity_Regression4 <- LondonWardsObesity_Regression%>%
  st_join(beveragesSub)%>%#将两个dataframe合并
  add_count(NAME)%>%#统计wardname中各observation的数量
  janitor::clean_names()%>%
  #calculate area
  mutate(area=st_area(.))%>%#计算出各area的面积
  #then density of the points per ward
  mutate(density=n/area) %>% 
  dplyr::select(density, name, gss_code, n)

LondonWardsObesity_Regression4<- LondonWardsObesity_Regression4 %>%                    
  group_by(gss_code) %>%  ##根据邮编进行分类，得到所有Ward的dataframe       
  summarise(beverages_density = first(density),##永远记住 group_by()+summarise/summary()无论python&R
            wardname= first(name),
            beveragescount= first(n))
st_write(LondonWardsObesity_Regression4,"4.csv")

LondonWardsObesity_Regression5 <- LondonWardsObesity_Regression%>%
  st_join(confectionerySub)%>%#将两个dataframe合并
  add_count(NAME)%>%#统计wardname中各observation的数量
  janitor::clean_names()%>%
  #calculate area
  mutate(area=st_area(.))%>%#计算出各area的面积
  #then density of the points per ward
  mutate(density=n/area) %>% 
  dplyr::select(density, name, gss_code, n)

LondonWardsObesity_Regression5<- LondonWardsObesity_Regression5 %>%                    
  group_by(gss_code) %>%  ##根据邮编进行分类，得到所有Ward的dataframe       
  summarise(confectionery_density = first(density),##永远记住 group_by()+summarise/summary()无论python&R
            wardname= first(name),
            confectionerycount= first(n))
st_write(LondonWardsObesity_Regression5,"5.csv")

LondonWardsObesity_Regression6 <- LondonWardsObesity_Regression%>%
  st_join(convenienceSub)%>%#将两个dataframe合并
  add_count(NAME)%>%#统计wardname中各observation的数量
  janitor::clean_names()%>%
  #calculate area
  mutate(area=st_area(.))%>%#计算出各area的面积
  #then density of the points per ward
  mutate(density=n/area) %>% 
  dplyr::select(density, name, gss_code, n)

LondonWardsObesity_Regression6<- LondonWardsObesity_Regression6 %>%                    
  group_by(gss_code) %>%  ##根据邮编进行分类，得到所有Ward的dataframe       
  summarise(convenience_density = first(density),##永远记住 group_by()+summarise/summary()无论python&R
            wardname= first(name),
            conveniencecount= first(n))
st_write(LondonWardsObesity_Regression6,"6.csv")

LondonWardsObesity_Regression7 <- LondonWardsObesity_Regression%>%
  st_join(departmentstoreSub)%>%#将两个dataframe合并
  add_count(NAME)%>%#统计wardname中各observation的数量
  janitor::clean_names()%>%
  #calculate area
  mutate(area=st_area(.))%>%#计算出各area的面积
  #then density of the points per ward
  mutate(density=n/area) %>% 
  dplyr::select(density, name, gss_code, n)

LondonWardsObesity_Regression7<- LondonWardsObesity_Regression7 %>%                    
  group_by(gss_code) %>%  ##根据邮编进行分类，得到所有Ward的dataframe       
  summarise(departmentstore_density = first(density),##永远记住 group_by()+summarise/summary()无论python&R
            wardname= first(name),
            departmentstorecount= first(n))
st_write(LondonWardsObesity_Regression7,"7.csv")

LondonWardsObesity_Regression8 <- LondonWardsObesity_Regression%>%
  st_join(farmSub)%>%#将两个dataframe合并
  add_count(NAME)%>%#统计wardname中各observation的数量
  janitor::clean_names()%>%
  #calculate area
  mutate(area=st_area(.))%>%#计算出各area的面积
  #then density of the points per ward
  mutate(density=n/area) %>% 
  dplyr::select(density, name, gss_code, n)

LondonWardsObesity_Regression8<- LondonWardsObesity_Regression8 %>%                    
  group_by(gss_code) %>%       
  summarise(farm_density = first(density),
            wardname= first(name),
            farmcount= first(n))
st_write(LondonWardsObesity_Regression8,"8.csv")

LondonWardsObesity_Regression9 <- LondonWardsObesity_Regression%>%
  st_join(greengrocerSub)%>%
  add_count(NAME)%>%
  janitor::clean_names()%>%
  #calculate area
  mutate(area=st_area(.))%>%
  mutate(density=n/area) %>% 
  dplyr::select(density, name, gss_code, n)

LondonWardsObesity_Regression9<- LondonWardsObesity_Regression9 %>%                    
  group_by(gss_code) %>%     
  summarise(greengrocer_density = first(density),
            wardname= first(name),
            greengrocercount= first(n))
st_write(LondonWardsObesity_Regression9,"9.csv")

LondonWardsObesity_Regression10 <- LondonWardsObesity_Regression%>%
  st_join(grocerySub)%>%
  add_count(NAME)%>%
  janitor::clean_names()%>%
  #calculate area
  mutate(area=st_area(.))%>%
  mutate(density=n/area) %>% 
  dplyr::select(density, name, gss_code, n)

LondonWardsObesity_Regression10<- LondonWardsObesity_Regression10 %>%                    
  group_by(gss_code) %>%  
  summarise(grocery_density = first(density),
            wardname= first(name),
            grocerycount= first(n))
st_write(LondonWardsObesity_Regression10,"10.csv")

LondonWardsObesity_Regression11 <- LondonWardsObesity_Regression%>%
  st_join(mallSub)%>%
  add_count(NAME)%>%
  janitor::clean_names()%>%
  #calculate area
  mutate(area=st_area(.))%>%
  mutate(density=n/area) %>% 
  dplyr::select(density, name, gss_code, n)

LondonWardsObesity_Regression11<- LondonWardsObesity_Regression11 %>%                    
  group_by(gss_code) %>%      
  summarise(mall_density = first(density),
            wardname= first(name),
            mallcount= first(n))
st_write(LondonWardsObesity_Regression11,"11.csv")

LondonWardsObesity_Regression12 <- LondonWardsObesity_Regression%>%
  st_join(supermarketSub)%>%
  add_count(NAME)%>%
  janitor::clean_names()%>%
  #calculate area
  mutate(area=st_area(.))%>%
  mutate(density=n/area) %>% 
  dplyr::select(density, name, gss_code, n)

LondonWardsObesity_Regression12<- LondonWardsObesity_Regression12 %>%                    
  group_by(gss_code) %>%       
  summarise(supermarket_density = first(density),
            wardname= first(name),
            supermarketcount= first(n))
st_write(LondonWardsObesity_Regression12,"12.csv")

health_density_and_rate <- read_csv("1.csv") 

LondonWardsObesity_Regression <- LondonWardsObesity_Regression %>% 
  left_join(health_density_and_rate, 
            by = c("GSS_CODE" = "gss_code"))%>%
  distinct(GSS_CODE, NAME,GEO,Childhood_Obesity,Childhood_Obesity_I,Childhood_Obesity_Iz,Childhood_Obesity_G,
           household_income_mean_modelled_household_income_2012_13,qualifications_qualifications_and_students_2011_census_percent_no_qualifications,
           out_of_work_families_rates_children_0_18_living_in_out_of_work_benefit_claimant_households_2011,
           children_in_poverty_percent_of_children_aged_under_16_in_poverty_2011,
           car_access_cars_per_household_census_2011,
           access_to_green_space_and_nature_percent_homes_with_access_to_local_small_or_pocket_park,
           travel_to_work_by_bicycle_2011_census_percent_travel_by_bicycle,leisure_density,health_density,health_rate_parcent)

names(LondonWardsObesity_Regression) <- c('NAME','GSS_CODE','Childhood_Obesity','Childhood_Obesity_I','Childhood_Obesity_Iz',
                                          'Childhood_Obesity_G','household_income','children_qualification','children_living_in_outofwork_rate','children_in_poverty_percent',
                                          'household_access_cars','access_to_greenspace_rate',"travel_by_bicycle",'leisure_density','health_density','health_rate_parcent',
                                          "GEO")

str(LondonWardsObesity_Regression)
LondonWardsObesity_Regression$household_income=as.numeric(unlist(LondonWardsObesity_Regression$household_income))
LondonWardsObesity_Regression$children_qualification=as.numeric(unlist(LondonWardsObesity_Regression$children_qualification))
LondonWardsObesity_Regression$household_access_cars=as.numeric(unlist(LondonWardsObesity_Regression$household_access_cars))
LondonWardsObesity_Regression$travel_by_bicycle=as.numeric(unlist(LondonWardsObesity_Regression$travel_by_bicycle))

ggplot(LondonWardsObesity_Regression, aes(x=household_income)) +
  geom_histogram()##

ggplot(LondonWardsObesity_Regression, aes(x=children_qualification)) +
  geom_histogram()

ggplot(LondonWardsObesity_Regression, aes(x=children_living_in_outofwork_rate)) +
  geom_histogram()

ggplot(LondonWardsObesity_Regression, aes(x=children_in_poverty_percent)) +
  geom_histogram()

ggplot(LondonWardsObesity_Regression, aes(x=household_access_cars)) +
  geom_histogram()

ggplot(LondonWardsObesity_Regression, aes(x=access_to_greenspace_rate)) +
  geom_histogram()

ggplot(LondonWardsObesity_Regression, aes(x=travel_by_bicycle)) +
  geom_histogram()##

ggplot(LondonWardsObesity_Regression, aes(x=leisure_density)) +
  geom_histogram()##

ggplot(LondonWardsObesity_Regression, aes(x=health_density)) +
  geom_histogram()##

ggplot(LondonWardsObesity_Regression, aes(x=health_rate_parcent)) +
  geom_histogram()##

symbox(~household_income, 
       LondonWardsObesity_Regression, 
       na.rm=T,
       powers=seq(-3,3,by=.5))##household_income^-1.5,household_income to the power negative 1.5

symbox(~travel_by_bicycle, 
       LondonWardsObesity_Regression, 
       na.rm=T,
       powers=seq(-3,3,by=.5))##log of traval by bicycle

symbox(~leisure_density, 
       LondonWardsObesity_Regression, 
       na.rm=T,
       powers=seq(-3,3,by=.5))##log of leisure density

symbox(~health_density, 
       LondonWardsObesity_Regression, 
       na.rm=T,
       powers=seq(-3,3,by=.5))##log of health density

symbox(~health_rate_parcent, 
       LondonWardsObesity_Regression, 
       na.rm=T,
       powers=seq(-3,3,by=.5))##health_rate_percent^-3,health_rate_percent to the power negative three

LondonWardsObesity_Regression <- LondonWardsObesity_Regression %>% 
  mutate(household_income_negative_power=household_income^-1.5)

LondonWardsObesity_Regression <- LondonWardsObesity_Regression %>% 
  mutate(health_rate_parcent_negative_power=health_rate_parcent^-3)

###regression model1-OLS Regression
model1 <-lm(Childhood_Obesity ~
      household_income_negative_power+children_qualification+children_living_in_outofwork_rate+
       children_in_poverty_percent+household_access_cars+access_to_greenspace_rate+log(travel_by_bicycle)+
       log(leisure_density)+log(health_density)+health_rate_parcent_negative_power,
     data=LondonWardsObesity_Regression)
##👇系数
broom::tidy(model1)

##👇R square
glance(model1)

##👇Residual plot
model_data <- model1 %>%
  augment(., LondonWardsObesity_Regression)
#plot residuals
model_data%>%
  dplyr::select(.resid)%>%
  pull()%>%
  qplot()+ 
  geom_histogram() 

##👇多重共线性检验
vif(model1)

LondonWardsObesity_Regression <- LondonWardsObesity_Regression %>%
  mutate(model1resids = residuals(model1))

Correlation <- LondonWardsObesity_Regression %>%
  st_drop_geometry()%>%
  dplyr::select(Childhood_Obesity,household_income_negative_power,children_qualification,children_living_in_outofwork_rate,
                  children_in_poverty_percent,household_access_cars,access_to_greenspace_rate,
                  health_rate_parcent_negative_power,travel_by_bicycle,leisure_density,health_density) %>%
  mutate(travel_by_bicycle=log(travel_by_bicycle))%>%
  mutate(leisure_density=log(leisure_density))%>%
  mutate(health_density=log(health_density))%>%
  correlate() %>%
  # just focus on GCSE and house prices
  focus(-Childhood_Obesity, mirror = TRUE) 


#visualise the correlation matrix
rplot(Correlation)

###regression model2-OLS Regression Modified
model2 <-lm(Childhood_Obesity ~
              household_income_negative_power+children_qualification+
              access_to_greenspace_rate+log(travel_by_bicycle)+
              log(leisure_density)+log(health_density)+health_rate_parcent_negative_power,
            data=LondonWardsObesity_Regression)
##👇系数
broom::tidy(model2)

##👇R square
glance(model2)

##👇Residual plot
model_data2 <- model2 %>%
  augment(., LondonWardsObesity_Regression)
#plot residuals
model_data2%>%
  dplyr::select(.resid)%>%
  pull()%>%
  qplot()+ 
  geom_histogram()

par(mfrow=c(2,2))    #plot to 2 by 2 array
plot(model2)

##👇residual spatial autocorrelation
LondonWardsObesity_Regression <- LondonWardsObesity_Regression %>%
  mutate(model2resids = residuals(model2))

tm_shape(LondonWardsObesity_Regression) +
  tm_polygons("model2resids",
              palette = "RdYlBu") 

plot5 <- tm_shape(LondonWardsObesity_Regression) +
  tm_polygons("model2resids",
              legend.hist=TRUE,
              palette="RdYlBu",
              midpoint=NA,
              title="Residual of OLS Model")+tm_layout(title = "Plot the residual of OLS Model on Wards level in London",title.position = c("left","top"),title.size=3)+
  tm_scale_bar(position = c("left", "bottom"), text.size = .4)+
  tm_layout(legend.outside = TRUE,legend.position = c("right","bottom"), 
            legend.text.size=.75, 
            legend.title.size = .8,
            frame=FALSE)+
  tm_credits("Data source from 2011 Census.Created by author.", position=c(0.0,0.0))+
  #tm_text(text = "NAME", size = .5, along.lines =T, remove.overlap=T,  auto.placement=F)+
  tm_compass(type = "8star", position = c(0.02, 0.09)) +
  tm_shape(BoroughMap)+tm_borders("black")

plot5
tmap_save(plot5, 'Plot the residual of OLS Model on Wards level in London.png',dpi = 300)

Nearest_neighbour <- LondonWardsObesity_Regression %>%
  st_drop_geometry()%>%
  dplyr::select(model2resids)%>%
  pull()%>%
  moran.test(., Lward.knn_5_weight)%>%
  tidy()
Nearest_neighbour

###GWR
st_crs(LondonWardsObesity_Regression) = 27700
LondonWardsObesity_RegressionSP <- LondonWardsObesity_Regression %>%
  as(., "Spatial")

st_crs(coordsW) = 27700
coordsWSP <- coordsW %>%
  as(., "Spatial")

GWRbandwidth <- gwr.sel(Childhood_Obesity ~
                          household_income_negative_power+children_qualification+
                          access_to_greenspace_rate+log(travel_by_bicycle)+
                          log(leisure_density)+log(health_density)+health_rate_parcent_negative_power, 
                        data = LondonWardsObesity_RegressionSP, 
                        coords=coordsWSP,
                        adapt=T)

gwr.model = gwr(Childhood_Obesity ~
                  household_income_negative_power+children_qualification+
                  access_to_greenspace_rate+log(travel_by_bicycle)+
                  log(leisure_density)+log(health_density)+health_rate_parcent_negative_power, 
                data = LondonWardsObesity_RegressionSP, 
                coords=coordsWSP, 
                adapt=GWRbandwidth, 
                hatmatrix=TRUE, 
                se.fit=TRUE)

gwr.model

results <- as.data.frame(gwr.model$SDF)
names(results)

LondonWardsObesity_Regression_gwr <- LondonWardsObesity_Regression %>%
  mutate(coefHINP15 = results$household_income_negative_power,
         coefCQ = results$children_qualification,
         coefATGSR = results$access_to_greenspace_rate,
         coefTBBlog = results$log.travel_by_bicycle.,
         coefLDlog = results$log.leisure_density.,
         coefHDlog = results$log.health_density.,
         coefHRPNP3 = results$health_rate_parcent_negative_power,
         local_R2=results$localR2)

tm_shape(LondonWardsObesity_Regression_gwr) +
  tm_polygons(col = "coefCQ", 
              palette = "RdBu", 
              alpha = 0.5)