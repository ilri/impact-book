#code to generate figures for the FUTURE CHAPTER
#
#NOTA BENE: some of the labels and notes to the Figures may differ
#slightly from what is in the book because of formatting changes made during printing
#
#
setwd("C:/Users/Jmm19/Documents/______ILRI/_______________DATA_PORTAL")


#PACKAGES AND LIBRARIES ARE IN THE WORKSPACE

library(bibliometrix)
library(dplyr)
library(doBy)
library(data.table)

library(tidyverse)
library(stringi)
library(stringr)

library(ggplot2)
library(ggpubr)
library(magrittr)
library(extrafont)
library(ggthemes)
library(gridExtra)
library(grid)
library(graphics)
library(openxlsx)


Future_df <- read.xlsx("FUTURE_PORTAL.xlsx",
 sheet = "FUTURE_R",startRow = 1,colNames = TRUE,cols = NULL,rows = NULL,na.strings = "NA")
Future_df$Growth <- as.numeric(as.character(Future_df$Growth))
Future_df$Growth <- Future_df$Growth / 100
Future_df$Region <- gsub("Sub-Saharan Africa","SSA",Future_df$Region)
Future_df$Species <- factor(Future_df$Species)
Future_df$Region <- factor(Future_df$Region)
Future_df$Period <- factor(Future_df$Period)

MeatConsGrowth_df <- filter(Future_df,Component ==  "MeatConsumptionGrowth")
MilkConsGrowth_df <- filter(Future_df,Component ==  "MilkConsumptionGrowth")
MeatProdGrowth_df <- filter(Future_df,Component ==  "MeatProductionGrowth")
AnimalNumbersGrowth_df <- filter(Future_df,Component ==  "AnimalNumbersGrowth")
Weight_df <- filter(Future_df, Component ==  "WeightKG")
WeightGrowth_df <- filter(Future_df, Component ==  "WeightGrowth")
AnimalNumbers_df <- filter(Future_df,Component ==  "AnimalNumbers")
Future_df$Component <- factor(Future_df$Component)
GrowthShares_df <- filter(Future_df, Component ==  "NumbersShare" | Component ==  "WeightShare")
GrowthShares_df$Growth <- GrowthShares_df$Growth * 100


#----------------------------------FIGURE 1-------------------------------------

AllMeatConsGrowth_df <- filter(MeatConsGrowth_df,Species ==  "All meat")
AllMeatConsGrowth_df <- AllMeatConsGrowth_df %>%
        mutate(Region = fct_relevel(Region,"World","Developing",
                                  "East Asia","South Asia","LAC","MENA",
                                  "SSA","Developed"))

(Future_Figure_1 <- ggplot(data = AllMeatConsGrowth_df) +
      aes(x = Period,y = Growth) +
     geom_col(aes(fill = Region), position = "dodge") +
     facet_wrap(~ Period, scales = "free_x",nrow = 1,ncol = 3) +
     labs(x = "Period",y = "Growth rates in annual percentages",
          title = "Fig. F.1. Aggregate meat consumption growth rates by period and region") +
     scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
     theme_tufte() +
    theme(text = element_text(size = 11), legend.position = "bottom") +
     scale_x_discrete(labels = NULL) +
     labs(caption = "1971-2007 rates estimated from historical data; 2007-2030 and 2007-2050 rates projected from a world agricultural model.
     LAC, Latin America and the Caribbean; MENA, Middle East and North Africa; SSA, sub-Saharan Africa.
          (Data from Alexandratos and Bruinsma, 2012).") +
     theme(plot.caption = element_text(hjust = 0)))

#------------------------------------FIGURE 2-----------------------------------

MilkConsGrowth_df <- MilkConsGrowth_df %>%
        mutate(Region = fct_relevel(Region,"World","Developing",
                                  "East Asia","South Asia","LAC","MENA",
                                  "SSA","Developed"))

(Future_Figure_2 <- ggplot(data = MilkConsGrowth_df) +
  aes(x = Period,fct_reorder(Region,Growth,desc = TRUE),y = Growth) +
  geom_col(aes(fill = Region), position = "dodge") +
  facet_wrap(~ Period, scales = "free_x",nrow = 1,ncol = 3) +
  labs(x = "",y = "Growth rates in annual percentages",
  title = "Fig. F.2. Aggregate milk consumption growth rates by period and region") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_tufte() +
    theme(text = element_text(size = 11), legend.position = "bottom") +
    scale_x_discrete(labels = NULL) +
    labs(caption = "1971-2007 rates estimated from historical data; 2007-2030 and 2007-2050 rates projected from a world agricultural model.
    LAC, Latin America and the Caribbean; MENA, Middle East and North Africa; SSA, sub-Saharan Africa.
    (Data from Alexandratos and Bruinsma, 2012).") +
    theme(plot.caption = element_text(hjust = 0)))

#------------------------------------FIGURE 3-----------------------------------

MeatProdGrowth_df <- rename(MeatProdGrowth_df,Product = Species)
MeatProdGrowth_df <- MeatProdGrowth_df %>%
mutate(Product = fct_relevel(Product,"All meat","Poultry meat","Pig meat",
                          "Bovine meat","Ovine meat"))

#MeatProdGrowth_df <- rename(MeatProdGrowth_df, "Product" = "Species")
(Future_Figure_3 <- ggplot(MeatProdGrowth_df, aes(x = Period, y = Growth)) +
  geom_col(aes(fill = Product), position = "dodge") +
  facet_wrap(~ Period, scales = "free_x",nrow = 1,ncol = 3) +
  labs(x = "Period", y = "Growth rates in annual percentages",
  title = "Fig. F.3. Meat production growth rates by period and product") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_tufte() +
    theme(text = element_text(size = 11), legend.position = "bottom")+
                scale_x_discrete(labels = NULL) +
          labs(caption = "1971-2007 rates estimated from historical data; 2007-2030 and 2007-2050 rates projected from a world agricultural model.  LAC, Latin America and the Caribbean;
          MENA, Middle East and North Africa; SSA, sub-Saharan Africa.
          (Data from Alexandratos and Bruinsma, 2012).") +
          theme(plot.caption = element_text(hjust = 0)))


#------------------------------------FIGURE 4-----------------------------------

AnimalNumbers_df <- AnimalNumbers_df %>%
        mutate(Species = fct_relevel(Species,"Poultry","Pigs",
                                   "Sheep and goats","Cattle and buffalo"))

(Future_Figure_4 <- ggplot(AnimalNumbers_df, aes(x = Species, y = Numbers/1000)) +
 geom_bar(aes(fill = Region),stat = "identity") +
 facet_wrap(~ Period, scales = "free_y",nrow = 1,ncol = 3) +
 labs(y = "Animal numbers in millions",title = "Fig. F.4. Animal numbers by period, region, and species")+
 scale_y_continuous(labels = scales::comma)  +
 theme_tufte() +
    theme(text = element_text(size = 11), legend.position = "bottom") +
 scale_x_discrete(labels = c("Poultry","Pigs","Sheep&goats","Cattle and buffalo")) +
 theme(axis.text.x = element_text(angle = 40,size = rel(1))) +
 labs(caption = "1961-1963 and 2005-2007 numbers estimated from historical data; 2050 numbers projected from world agricultural model.
 LAC, Latin America and the Caribbean;MENA, Middle East and North Africa; SSA, sub-Saharan Africa. +
  (Data from Alexandratos and Bruinsma, 2012).") +
    theme(plot.caption = element_text(hjust = 0)))

AnimalNumbersGrowth_df <- AnimalNumbersGrowth_df %>%
        mutate(Species = fct_relevel(Species,"Poultry","Pigs",
"Sheep and goats","Cattle and buffalo"))

#------------------------------------FIGURE 5-----------------------------------

AnimalNumbersGrowth_df <- AnimalNumbersGrowth_df %>%
mutate(Region = fct_relevel(Region,"World","Developing",
                          "East Asia","South Asia","LAC","MENA",
                          "SSA","Developed"))

(Future_Figure_5 <- ggplot(AnimalNumbersGrowth_df, aes(x = Region, y = Growth)) +
geom_bar(aes(fill = Species),stat = "identity") +
facet_wrap(~ Period, scales = "free_x",nrow = 1,ncol = 3) +
labs(y = "Growth rates in annual percentages",title = "Fig. F.5. Growth rates of animal numbers by period, region, and species")+
scale_y_continuous(labels = scales::percent_format(accuracy = 1))  +
theme_tufte() +
    theme(text = element_text(size = 11), legend.position = "bottom") +
scale_x_discrete(labels = c("World","Developing",
"East Asia","South Asia","LAC","MENA",
"SSA","Developed")) + theme(axis.text.x = element_text(angle = 20)) +
labs(caption = "1961-2007 rates estimated from historical data;
     2007-2050 rates projected from world agricultural model.
    LAC, Latin America and the Caribbean; MENA, Middle East and North Africa; SSA, sub-Saharan Africa.
 (Data from Alexandratos and Bruinsma, 2012).") +
theme(plot.caption = element_text(hjust = 0)))

#-----------------------------FIGURE 6 -----------------------------------------

WeightGrowth_df <- WeightGrowth_df %>%
        mutate(Species = fct_relevel(Species,"Poultry","Pigs",
                                   "Sheep and goats",
                                   "Cattle and buffalo"))

WeightGrowth_df <- WeightGrowth_df %>%
        mutate(Region = fct_relevel(Region,"World","Developing",
                                  "East Asia","South Asia","LAC","MENA",
                                  "SSA","Developed"))

(Future_Figure_6 <- ggplot(WeightGrowth_df, aes(x = Region, y = Growth)) +
geom_bar(aes(fill = Species),stat = "identity") +
facet_wrap(~ Period, scales = "free_x",nrow = 1,ncol = 2) +
labs(y = "Growth rates in annual percentages",title = "Fig. F.6. Growth rates of animal carcass weights by period, region, and species")+
scale_y_continuous(labels = scales::percent_format(accuracy = 1))  +
theme_tufte() +
    theme(text = element_text(size = 11), legend.position = "bottom") +
scale_x_discrete(labels = c("World","Developing",
 "East Asia","South Asia","LAC","MENA",
"SSA","Developed")) + theme(axis.text.x = element_text(angle = 20)) +
labs(caption = "1962-2006 rates estimated from historical data;
          2006-2050 rates projected from world agricultural model.
     LAC, Latin America and the Caribbean; MENA, Middle East and North Africa; SSA, sub-Saharan Africa.
 (Data from Alexandratos and Bruinsma, 2012).") +
     theme(plot.caption = element_text(hjust = 0)))

#------------------------------------FIGURE 7-----------------------------------

GrowthShares_df <- GrowthShares_df %>%
        mutate(Species = fct_relevel(Species,"Poultry","Pigs",
                                   "Sheep and goats","Cattle and buffalo"))

GrowthShares_df <- GrowthShares_df %>%
        mutate(Region = fct_relevel(Region,"World","Developing",
                                  "East Asia","South Asia","LAC","MENA",
                                  "SSA","Developed"))

(Future_Figure_7 <- ggplot(GrowthShares_df, aes(x = Region, y = Growth)) +
geom_col(aes(fill = Species),position = "dodge") +
facet_wrap(~ Period, scales = "free_x",nrow = 1,ncol = 2) +
labs(y = "Shares of growth rates in percentages",title = "Fig. F.7. Shares of growth rates by period, region, and species")+
scale_y_continuous(labels = scales::percent_format(accuracy = 1))  +
theme_tufte() +
    theme(text = element_text(size = 11), legend.position = "bottom") +
scale_x_discrete(labels = c("World","Developing",
"East Asia","South Asia","LAC","MENA",
"SSA","Developed")) + theme(axis.text.x = element_text(angle = 20)) +
labs(caption = "1962-2006 rates estimated from historical data;
          2006-2050 rates projected from world agricultural model.
     LAC, Latin America and the Caribbean; MENA, Middle East and North Africa; SSA, sub-Saharan Africa.
 (Data from Alexandratos and Bruinsma, 2012).") +
theme(plot.caption = element_text(hjust = 0)))


#CREATE THE TIFF FILES
#

# following code is commented out
# if you want to export the figures to tiff files then uncomment

# ggarrange(Future_Figure_1,ncol = 1,nrow = 1,align = "v")  %>%
# ggexport(filename = "Fig. F.1. Aggregate meat consumption growth rates by period and region.tiff")
#
# ggarrange(Future_Figure_2,ncol = 1,nrow = 1,align = "v")  %>%
#         ggexport(filename = "Fig. F.2. Aggregate milk consumption growth rates by period and region.tiff")
#
# ggarrange(Future_Figure_3,ncol = 1,nrow = 1,align = "hv")  %>%
#         ggexport(filename = "Fig. F.3. Meat production growth rates by period and product.tiff")
#
# ggarrange(Future_Figure_4,ncol = 1,nrow = 1,align = "hv")  %>%
#         ggexport(filename = "Fig. F.4. Animal numbers by period, region and species.tiff")
#
# ggarrange(Future_Figure_5,ncol = 1,nrow = 1,align = "hv")  %>%
#         ggexport(filename = "Fig. F.5. Growth rates of animal numbers by period, region and species.tiff")
#
# ggarrange(Future_Figure_6,ncol = 1,nrow = 1,align = "hv")  %>%
#         ggexport(filename = "Fig. F.6. Growth rates of animal carcass weights by period, region and species.tiff")
#
# ggarrange(Future_Figure_7,ncol = 1,nrow = 1,align = "hv")  %>%
#         ggexport(filename = "Fig. F.7. Shares of growth rates by period, region and species.tiff")


GroupedGrowthShares <- GrowthShares_df %>% group_by(Species,Region,Period,Component) %>%
        summarise(MeanGrowthShare = mean(Growth))
