# Variables ---------------------------------------------------------------

`Title`,	`Publisher_`,	`Coverage_(years_(y-y)_`,
`Categories_(Oncology,Hematology,_medicne,_etc)_`,	`Country`,
`Region/Continent_`,	`H-Index_`,	`Original_articles_(Y/N)`,	`Cost_(currency)`,
`Cost_($)`,	`Original_article_cost_($)`,	`Review_articles_(Y/N)`,	`Cost_(currency)`,
`Cost_($)`,	`Cost_Review_of_article_($)`,	`Perspective_articles_(Y/N)`,	`Cost_(currency)`,
`Cost_($)`,	`Editorial`,	`Cost_(currency)`,	`Cost_($)`,	`Cost_editoria_($)`,
`Guidelines_articles_(Y/N)_`,	`Cost_(currency)`,	`Cost_($)`,	`Cost_of_Guidelines__($)`,
`Letteras_to_Editors_(Y/N)_`,	`Cost_(currency)`,	`Cost_($)`,	`Cost_Letter_to_editors_($)`,
`Case_reports_(Y/N)`,	`Cost_(currency)`,	`Cost_($)`,	`Cost_case_reports_($)`,	`Case_series_(Y/N)_`,
`Cost_(currency)`,	`Cost_($)`,	`Cost_case_series__($)`,	`Comments_(Y/N)_`,	`Cost_(currency)`,
`Cost_($)`,	`Cost_of_comments_($)`,	`Open_Access_(Y/N)`,	`Publishing_Model`,	`Has_APC_>0?_`,	
`Requires_APC_to_publish_your_paper?_`,	`Notes`,	`Impact_Factor-Clarivate`,
`Journal_Citation_Indicator-Clarivate`,	`Total_Citations_(2023`,

# Load packages -----------------------------------------------------------
library(writexl)
library(readxl)
library(dplyr)
library(gtsummary)
library(AER)
library(survival)
library(lubridate)
library(ggplot2)
library(survminer)
library(tidyr)
library(tidyverse)
library(broom)
library(patchwork)
library(prodlim)
library(maps)
library(leaflet)
library(lattice)
library(leafpop)
library(mapview)
library(vapoRwave)
library(viridis)
library(sf)
library(tmap)
library(RColorBrewer)


# Load the data -----------------------------------------------------------

Jour <- read_excel("Copy of 2_27_25_results.xlsx")

Jour$`Original_article_cost_($)` <- as.numeric(Jour$`Original_article_cost_($)`)
Jour$`Cost_Review_of_article_($)`<- as.numeric(Jour$`Cost_Review_of_article_($)`)
Jour$`Cost_editoria_($)`<- as.numeric(Jour$`Cost_editoria_($)`)
Jour$`Cost_of_Guidelines__($)`<- as.numeric(Jour$`Cost_of_Guidelines__($)`)
Jour$`Cost_Letter_to_editors_($)`<- as.numeric(Jour$`Cost_Letter_to_editors_($)`)
Jour$`Cost_case_series__($)`<- as.numeric(Jour$`Cost_case_series__($)`)
Jour$`Cost_of_comments_($)`<- as.numeric(Jour$`Cost_of_comments_($)`)
Jour$`Impact_Factor-Clarivate` <- as.numeric(Jour$`Impact_Factor-Clarivate`)
Jour$`Journal_Citation_Indicator-Clarivate` <- as.numeric(Jour$`Journal_Citation_Indicator-Clarivate`)
Jour$`Total_Citations_(2023` <- as.numeric(Jour$`Total_Citations_(2023`)
Jour$`Open_Access_(Y/N)` <- toupper(Jour$`Open_Access_(Y/N)`)

Jour$Index5 <- cut(Jour$`Impact_Factor-Clarivate`, 
                   breaks=c(-Inf, 5, Inf), 
                   labels=c("Less or equal to 5","Higher than 5")) 
#By default, labels are constructed using "(a,b]": a is not include and b is included.

Jour$Index55 <- cut(Jour$`Impact_Factor-Clarivate`, 
                    breaks=c(-Inf, 5, 10, 15, 20, 25, 30, 50, 75, 100, 200, 400, 500, Inf), 
                    labels=c("5","10", "15", "20", "25", "30", "50", "75", "100",
                             "200", "400", "500", ">501")) 
JourInd <- Jour %>%
  filter(!is.na(Jour$Index5))

#Jour %>%
#  select( Index5 ,`Impact_Factor-Clarivate` )%>%
#  print(n=30)

# Summary -----------------------------------------------------------------

#remove Joe1 in case wanted excel manually
Joe1<- Jour %>%
  select(`Country`,
         `Region/Continent_`,	`H-Index_`, `Original_article_cost_($)`, 
         `Cost_Review_of_article_($)`,  `Cost_editoria_($)`,
         `Cost_of_Guidelines__($)`, `Cost_Letter_to_editors_($)`, 
         `Cost_case_series__($)`, `Cost_of_comments_($)`,
         `Open_Access_(Y/N)`,	`Publishing_Model`, `Impact_Factor-Clarivate`,
         `Journal_Citation_Indicator-Clarivate`,	`Total_Citations_(2023`,
         Index5
  ) %>%
  tbl_summary(
    label =  list ( Index5 ~ "Impact_Factor_Index"
    ),
    percent ="column",
    digits =list (),
    statistic = list (
      all_continuous() ~ "{mean} (range: {min} to {max})"
    ), 
    type = list( )
  ) %>%
  modify_caption("**Table X. Summary of all data**")

Joe1 %>% 
  as_hux_xlsx("Joe1.xlsx")

# By IF_X_Publication --------------------------------------------------------

Joe2 <- JourInd %>%
  select(`H-Index_`, `Original_article_cost_($)`, 
         `Cost_Review_of_article_($)`,  `Cost_editoria_($)`,
         `Cost_Letter_to_editors_($)`, 
         `Cost_of_comments_($)`,
         `Open_Access_(Y/N)`,	`Publishing_Model`, `Impact_Factor-Clarivate`,
         `Journal_Citation_Indicator-Clarivate`,	`Total_Citations_(2023`,
         Index5
  ) %>%
  tbl_strata(
    strata = Index5,
    ~.x %>%
      tbl_summary(
        by = `Publishing_Model` ,
        label =  list (
        ),
        percent ="column",
        digits =list (),
        statistic = list (), 
        type = list(
          `H-Index_` ~ "continuous"  , `Original_article_cost_($)` ~ "continuous", 
          `Cost_Review_of_article_($)` ~ "continuous",  `Cost_editoria_($)` ~ "continuous",
          `Cost_Letter_to_editors_($)` ~ "continuous", 
          `Cost_of_comments_($)`~ "continuous",
          `Impact_Factor-Clarivate` ~ "continuous",
          `Journal_Citation_Indicator-Clarivate` ~ "continuous",	`Total_Citations_(2023` ~ "continuous"
        )
      )%>%
      add_p(
        test = list(all_continuous()  ~ "kruskal.test",
                    all_categorical() ~ "fisher.test"),
        pvalue_fun = function(x) style_pvalue(x, digits = 3)
      ) %>%
      add_q
  ) %>%
  modify_caption("**Table X. Summary of cost by publication model and index factor**")

Joe2 %>% 
  as_hux_xlsx("Joe2.xlsx")

# By Impact factor only ---------------------------------------------------

Joe3 <- Jour %>%
  select(`H-Index_`, `Original_article_cost_($)`, 
         `Cost_Review_of_article_($)`,  `Cost_editoria_($)`,
         `Cost_of_Guidelines__($)`, `Cost_Letter_to_editors_($)`, 
         `Cost_case_series__($)`, `Cost_of_comments_($)`,
         `Open_Access_(Y/N)`,	`Publishing_Model`, `Impact_Factor-Clarivate`,
         `Journal_Citation_Indicator-Clarivate`,	`Total_Citations_(2023`,
         Index5
  ) %>%
  tbl_summary(
    by= Index5,
    label =  list ( 
    ),
    percent ="column",
    digits =list (),
    statistic = list (
      all_continuous() ~ "{mean} (range: {min} to {max})"
    ), 
    type = list( )
  ) %>%
  add_p()%>%
  add_q() %>%
  modify_caption("**Table X. Summary of all data by Impact_Factor-Clarivate**")

Joe3 %>% 
  as_hux_xlsx("Joe3.xlsx")

# By region ---------------------------------------------------------------

Joe4 <- Jour %>%
  select(`H-Index_`, `Original_article_cost_($)`, `Region/Continent_`,
         `Cost_Review_of_article_($)`,  `Cost_editoria_($)`,
         `Cost_of_Guidelines__($)`, `Cost_Letter_to_editors_($)`, 
         `Cost_case_series__($)`, `Cost_of_comments_($)`,
         `Open_Access_(Y/N)`,	 `Impact_Factor-Clarivate`,
         `Journal_Citation_Indicator-Clarivate`,	`Total_Citations_(2023`,
         Index5
  ) %>%
  tbl_summary(
    by= `Region/Continent_`,
    label =  list ( 
    ),
    percent ="column",
    digits =list (),
    statistic = list (
      all_continuous() ~ "{mean} (range: {min} to {max})"
    ), 
    type = list( )
  ) %>%
  add_p()%>%
  add_q() %>%
  modify_caption("**Table X. Summary of all data by Impact_Factor-Clarivate**")

Joe4 %>% 
  as_hux_xlsx("Joe4.xlsx")

# Maps --------------------------------------------------------------------


Jour %>%
  select(	`Country`
  ) %>%
  tbl_summary(
    label =  list ( 
    ),
    percent = "column" ,
    digits =list (),
    statistic = list (), 
    type = list()
  )  -> coolmap 

coolmap <- as.data.frame(coolmap)
coolmap <- separate(coolmap,"**N = 458**", sep = " ", into = c("count","percent"))
coolmap <- coolmap[-c(1),]
coolmap <- coolmap[,-c(3)]
coolmap

#nc <- st_read(system.file("shape/nc.shp", package="sf")) 
#plot(nc["AREA"], main = "AREA", breaks = "quantile", 
#     nbreaks = 9, pal = brewer.pal(9, "YlOrRd"))

world_map <- map_data("world") 
coolmap$region <- coolmap$`**Characteristic**`
coolmap$region <- replace(coolmap$region, 38, "USA")
coolmap$region <- gsub("RussianFederation", "Russian Federation", coolmap$region)
coolmap$region <- gsub("UnitedArabEmirates", "United Arab Emirates", coolmap$region)
coolmap$region <- gsub("United Kingdom", "UK", coolmap$region)
mapdata <- left_join(world_map,coolmap, by="region" )
mapdata$count <- as.numeric(mapdata$count)
mapdata1 <- mapdata%>%
  filter(!is.na(mapdata$count))

map1 <- ggplot(mapdata, aes(long, lat, group = group)) +  
  geom_polygon(aes(fill = count ) )
map2 <- map1 + 
  scale_fill_gradient(name= "Journals_
  Count" )+
  theme ( axis.text.x= element_blank(),
          axis.text.y= element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          rect = element_blank(),
          text = element_text(size=rel(3.5))
  ) +
  theme(legend.key.size = unit(2, "cm")
  )+
  theme(legend.text = element_text(size=15))+ 
  theme(plot.title = element_text(size = 40, face = "bold"))+
  ggtitle("Countries by Journal counts")

map2


# Impact_Map --------------------------------------------------------------

Jour %>%
  select(		`Country`,`Impact_Factor-Clarivate`, 
  ) %>%
  tbl_summary(
    by= 	`Country`,
    label =  list ( 
    ),
    percent = "column" ,
    digits =list (),
    statistic = list (), 
    type = list()
  )  -> Immap 

Immap <- as.data.frame(Immap)
Immap <- t(Immap)
Immap <- as.data.frame(Immap)
row.names(Immap)
d <- Immap
names <- rownames(d)
names <- gsub("\\*\\*", "", names)
names <- gsub(" ", "", names)
rownames(d) <- NULL
Immap2 <- cbind(names,d)
Immap2 <- separate(Immap2,"names", sep = "\n", into = c("region","na","na2"))
Immap2 <- Immap2[-c(1),]
Immap2 <- separate(Immap2,"V1", sep = " ", into = c("count","na2"))
Immap2 <- Immap2[,-c(2,4,5)]
names(Immap2) <- c("region", "count")
ImmapF <- Immap2
ImmapF
#nc <- st_read(system.file("shape/nc.shp", package="sf")) 
#plot(nc["AREA"], main = "AREA", breaks = "quantile", 
#     nbreaks = 9, pal = brewer.pal(9, "YlOrRd"))
ImmapF$region <- gsub("RussianFederation", "Russian Federation", ImmapF$region)
ImmapF$region <- gsub("UnitedArabEmirates", "United Arab Emirates", ImmapF$region)
ImmapF$region <- gsub("UnitedKingdom", "UK", ImmapF$region)
ImmapF$region <- gsub("SaudiArabia", "Saudi Arabia", ImmapF$region)
ImmapF$region <- gsub("NewZealand", "New Zealand", ImmapF$region)
ImmapF$region <- gsub("NewZealand", "New Zealand", ImmapF$region)
ImmapF$region <- gsub("CzechRepublic", "Czech Republic", ImmapF$region)
ImmapF$region <- replace(ImmapF$region, 38, "USA")

ImmapF
world_map <- map_data("world") 
mapdataIM <- left_join(world_map,ImmapF, by="region" )
mapdataIM$count <- as.numeric(mapdataIM$count)

#mapdata1 <- mapdataIM%>%
#  filter(!is.na(mapdataIM$count))

map1Im <- ggplot(mapdataIM, aes(long, lat, group = group)) +  
  geom_polygon(aes(fill = count ) )
map2Im <- map1Im + 
  scale_fill_gradient(name= "Imapct of
  Journals_
  Clarivate", low="yellow", high= "darkgreen",
                      na.value= "gray50") +
  theme ( axis.text.x= element_blank(),
          axis.text.y= element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          rect = element_blank(),
          text = element_text(size=rel(3.5))
  ) +
  theme(legend.key.size = unit(2, "cm")
  )+
  theme(legend.text = element_text(size=15))+ 
  theme(plot.title = element_text(size = 40, face = "bold"))+
  ggtitle("Countries by median impact_Clarivate")

map2Im

#write.csv(world_map,"~/Downloads/filename.csv", row.names = FALSE)


# Cost Map ----------------------------------------------------------------

Jour %>%
  select(		`Country`, `Original_article_cost_($)`, 
  ) %>%
  tbl_summary(
    by= 	`Country`,
    label =  list ( 
    ),
    percent = "column" ,
    digits =list (),
    statistic = list (), 
    type = list()
  )  -> costmap 

costmap <- as.data.frame(costmap)
costmap <- t(costmap)
row.names(costmap)
d2 <- costmap
names2 <- rownames(d2)
names2 <- gsub("\\*\\*", "", names2)
rownames(d2) <- NULL
costmap2 <- as.data.frame(cbind(names2,d2))
costmap2 <- separate(costmap2,"names2", sep = "\n", into = c("na","region","na2"))
costmap2 <- costmap2[-c(1),]
costmap2 <- costmap2[,-c(3)]
costmap2 <- separate(costmap2,"V2", sep = " ", into = c("count","na2"))
costmap2 <- costmap2[,-c(2,4,5)]
names(costmap2) <- c("region", "count")
#Country.names <- unique(Jour$Country)
#Country.names <- replace(Country.names, 1 , "USA" )
#costmapF <- as.data.frame(cbind(Country.names, costmap2$count ))
#names(costmapF) <- c("region", "Mean index")
#costmapF$region <- replace(costmapF$region, 38, "USA")
costmapF <- costmap2
costmapF$region <- replace(costmapF$region, 38, "USA")
costmapF$region <- gsub(" ", "", costmapF$region)
costmapF$region <- gsub("RussianFederation", "Russian Federation", costmapF$region)
costmapF$region <- gsub("UnitedArabEmirates", "United Arab Emirates", costmapF$region)
costmapF$region <- gsub("UnitedKingdom", "UK", costmapF$region)
costmapF$region <- gsub("SaudiArabia", "Saudi Arabia", costmapF$region)
costmapF$region <- gsub("NewZealand", "New Zealand", costmapF$region)
costmapF$region <- gsub("NewZealand", "New Zealand", costmapF$region)
costmapF$region <- gsub("CzechRepublic", "Czech Republic", costmapF$region)
costmapF$count <- gsub(",", "", costmapF$count )
costmapF$count <- as.numeric(costmapF$count)
costmapF


world_map <- map_data("world") 
mapdataCOST <- left_join(world_map,costmapF, by="region" )
mapdataCOST$count <- as.numeric(mapdataCOST$count)
max(mapdataCOST$count, na.rm = T)
max(costmapF$count, na.rm = T)


#mapdata1 <- mapdataIM%>%
#  filter(!is.na(mapdataIM$count))

map1COST <- ggplot(mapdataCOST, aes(long, lat, group = group)) +  
  geom_polygon(aes(fill = count ) )
map2COST <- map1COST + 
  scale_fill_gradient(name= "Average cost of
original articles", low="yellow", high= "darkgreen",
                      na.value= "gray50") +
  theme ( axis.text.x= element_blank(),
          axis.text.y= element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          rect = element_blank(),
          text = element_text(size=rel(3.5))
  ) +
  theme(legend.key.size = unit(2, "cm")
  )+
  theme(legend.text = element_text(size=15))+ 
  theme(plot.title = element_text(size = 30, face = "bold"))+
  ggtitle("Countries by costs of publication in a peer reviewed journal")

map2COST

# Other codes -------------------------------------------------------------

summary(Jour$`Impact Factor`)
prop.table(table(Jour$`Region/Continent`))
as.matrix(prop.table(table(Jour$`Region/Continent`)))
fisher.test(as.matrix(table(Jour$`Region/Continent`)))

Jour %>% 
  count(`Region/Continent`) %>% 
  mutate(precent = n / sum(n) *100)

Jour %>% 
  count(Index5) %>% 
  mutate(precent = n / sum(n) *100)


# Or use `as_hux_xlsx()`
Immap %>% 
  as_hux_xlsx("example_gtsummary2.xlsx")

# End ---------------------------------------------------------------------


