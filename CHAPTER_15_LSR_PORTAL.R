#code to generate figures for the CHAPTER 15 on LSR
#
#NOTA BENE: some of the labels and notes to the Figures may differ
#slightly from what is in the book because of formatting changes made during printing
#
#

setwd("C:/Users/jmm19/Documents/______ILRI/_______________DATA_PORTAL")

library(ggplot2)
library(ggpubr)
library(magrittr)
library(dplyr)
library(tidyr)
library(tidyselect)
library(data.table)

library(ggthemes)
library(grid)
library(gridExtra)
library(gghighlight)
library(graphics)
library(extrafont)
library(extrafontdb)
library(openxlsx)
library(forcats)

library(janitor)
library(forecast)
library(stargazer)
library(tufte)
library(scales)


loadfonts(device = "win")
windowsFonts(Times = windowsFont("Times New Roman"))
col_global <- c("#0000FFFF")


#make the figure titles
#
tles.Fig.15.1 <- c("Fig.15.1 Some characteristics of global livestock systems by region, 1991-1993")
tles.Fig.15.2 <- c("Fig.15.2 Some characteristics of sub-Saharan Africa livestock systems by agroecology, 1991-93")
tles.Fig.15.3a <- c("Fig.15.3 (a). Livestock production index, East and southern Africa, 1970-2016")
tles.Fig.15.3b <- c("Fig.15.3 (b). Livestock production index, West and Central Africa, 1970-2016")
tles.Fig.15.4a <- c("Fig.15.4 (a). TLU density, East and southern Africa, 1970-2016")
tles.Fig.15.4b <- c("Fig.15.4 (b). TLU density, West and Central Africa, 1970-2016")
tles.Fig.15.5a <- c("Fig.15.5 (a). Sheep and goat numbers relative to cattle numbers, East and southern Africa, 1970-2016")
tles.Fig.15.5b <- c("Fig.15.5 (b). Sheep and goat numbers relative to cattle numbers, West and Central Africa, 1970-2016")
tles.Fig.15.6a <- c("Fig.15.6 (a). Arable land per capita, East and southern Africa, 1970-2016")
tles.Fig.15.6b <- c("Fig.15.6 (b). Arable land per capita, West and Central Africa, 1970-2016")
tles.Fig.15.7a <- c("Fig.15.7 (a). Fertilizer use per hectare, East and southern Africa, 2002-2016")
tles.Fig.15.7b <- c("Fig.15.7 (b). Fertilizer use per hectare, West and Central Africa, 2002-2016")
tles.Fig.15.8a <- c("Fig.15.8 (a). Cereal yields per hectare, East and southern Africa, 1970-2016")
tles.Fig.15.8b <- c("Fig.15.8 (b). Cereal yields per hectare, West and Central Africa, 1970-2016")
tles.Fig.15.9a <- c("Fig.15.9 (a). Food production index, East and southern Africa, 1970-2016")
tles.Fig.15.9b <- c("Fig.15.9 (b). Food production index, West and Central Africa, 1970-2016")

Robinson_data <- read.xlsx("CHAPTER_15_LSR_PORTAL.xlsx",
 sheet = "Robinson_2011",startRow  =  1,colNames = T,na.strings =  "NA")

Robinson_data$Farming.system <- factor(Robinson_data$Farming.system)
Robinson_data$Region <- factor(Robinson_data$Region)
Robinson_data <- filter(Robinson_data,Farming.system != "Other")
Robinson_data <- select(Robinson_data,Farming.system,Region,
                        `Area (ha)` = Area,
                        Population = Population,
                        `Cattle (TLU)` = TLU.Cattle)
Robinson_data_gather <- Robinson_data %>% pivot_longer(`Area (ha)`:`Cattle (TLU)`,
                                                names_to = "Variable",
                                                values_to = "Value")

Robinson_data_gather$Variable <- factor(Robinson_data_gather$Variable)

Robinson_data_gather <- Robinson_data_gather %>% mutate(Variable = fct_relevel(Variable,
 "Population","Area (ha)","Cattle (TLU)",))


#--------------------------------FIGURE 15.1----------------------
#
region_colors <- data.frame(Region = c("CSA","EA","SA","SEA","SSA","WANA"),
Color = c("#1DB000FF","#8DB000FF","#E6E600FF","#E9BD3AFF","#ECB176FF","#EFC2B3FF"))

Robinson_data_gather <- arrange(Robinson_data_gather,Region)
Robinson_data_gather <- merge(Robinson_data_gather,region_colors,by = "Region")
Robinson_data_gather$Color <- as.character(Robinson_data_gather$Color)

(LSR_Fig.15.1 <- ggplot(data = Robinson_data_gather) +
      geom_col(mapping = aes(x = Region,y = Value,fill = Color)) +
      facet_wrap(~Variable+Farming.system,ncol = 3,nrow = 3,scales = "free_y",
          labeller = labeller(.multi_line  =  FALSE)) +
      labs(title = tles.Fig.15.1, y = "Millions",x = "Farming system") +
      theme_tufte() +
      theme(text = element_text(family = "Times",size  =  11), legend.position = "none") +
      labs(caption = "Data from Robinson et al (2011: p. 48)") +
      theme(plot.caption = element_text(hjust = 0)))

#--------------------FIGURE 15.2 ---------------------
S_and_S_data <- data.table(read.xlsx("CHAPTER_15_LSR_PORTAL.xlsx",
  sheet = "S_and_S_data",startRow = 1,colNames = T,na.strings =  "NA"))
S_and_S_data <- filter(S_and_S_data,Agroecology != "Total")
S_and_S_data <- select(S_and_S_data,Agroecology,
                      `Land type` = Land.type,
                      `Population, m` = Population.m,
                      `Population density, n/km2` = Population.density.n.km2,
                      `Grazing land, m ha` = Grazing.land.m.ha,
                      `Arable land, m ha` = Arable.land.m.ha,
                      `Ruminants, n/ha` = Ruminants.grazing.land.n.ha,
                      `Meat, kg/ha` = Meat.kg.ha)

S_and_S_data_gather <- pivot_longer(S_and_S_data,`Population, m`:`Meat, kg/ha`,
                            names_to = "Variable",values_to = "Value")
S_and_S_data_gather$Variable <- factor(S_and_S_data_gather$Variable)
S_and_S_data_gather$Agroecology <- factor(S_and_S_data_gather$Agroecology)
S_and_S_data_gather <- S_and_S_data_gather %>% mutate(Variable = fct_relevel(Variable,
                       "Population, m","Population density, n/km2",
                         "Arable land, m ha","Grazing land, m ha",
                         "Ruminants, n/ha","Meat, kg/ha"))

S_and_S_data_gather$Land.type <- factor(S_and_S_data_gather$`Land type`)

(LSR_Fig.15.2 <- ggplot(data=S_and_S_data_gather) +
      geom_col(mapping = aes(x = Agroecology,y = Value,fill = Agroecology),na.rm  =  TRUE) +
      facet_wrap(~Variable,nrow = 3,ncol = 2,scales = "free_y",
          labeller = labeller(.multi_line  =  FALSE)) +
      labs(title  =  tles.Fig.15.2,x = "Land type",y = "") +
      theme_tufte() +
      theme(text = element_text(family = "Times",size = 11), legend.position = "none") +
      labs(caption = "Data from Sere and Steinfeld (1996: p. {})") +
      theme(plot.caption = element_text(hjust = 0)))


#--------------------------FIGURE 15.3 --------------------
AFR_test <- data.table(read.xlsx("CHAPTER_15_LSR_PORTAL.xlsx", sheet = "AFR_DATA_R",
  colNames = T))

AFR_test <- AFR_test[,-"TypeCol"]
AFR_test <- AFR_test[,-"Formula"]
AFR_test <- AFR_test[,-"Country_Code"]
AFR_test <- AFR_test[,-"Indicator_Code"]
AFR_test <- AFR_test[,-"X2017"]

#gather it to transpose the rows and columns
AFR_gather <- AFR_test %>%  pivot_longer(X1961:X2016,names_to = "Year",values_to = "Value")
AFR_gather$Value <- as.numeric(AFR_gather$Value)
AFR_gather <- rename(AFR_gather, Country = Country_Name)

#delete the X in the Year columns as assigned while reading in the CSV file
AFR_gather$Year <- gsub("X","",AFR_gather$Year)
AFR_gather$Year <- as.integer(AFR_gather$Year)
AFR_gather <- data.table(AFR_gather)

AFR_gather <- AFR_gather %>% mutate(Period = case_when(
     Year < 1976 ~ "1970-75",
     Year >= 1976 & Year <= 1980 ~ "1976-80",
     Year >= 1981 & Year <= 1985 ~ "1981-85",
     Year >= 1986 & Year <= 1990 ~ "1986-90",
     Year >= 1991 & Year <= 1995 ~ "1991-95",
     Year >= 1996 & Year <= 2000 ~ "1996-00",
     Year >= 2001 & Year <= 2005 ~ "2001-05",
     Year >= 2006 & Year <= 2010 ~ "2006-10",
     Year >= 2011 & Year <= 2016 ~ "2011-16"))
AFR_gather$Period <- factor(AFR_gather$Period)

#-----------------------------------------------------------
#Figure 15.3a and 15.3b Livestock production index
#Plot, ESA


AFR_LIVE_PROD_ESA <- filter(AFR_gather, Region == "East and southern Africa" & Year > 1969 & Year < 2017)
AFR_LIVE_PROD_ESA <- filter(AFR_LIVE_PROD_ESA,
Indicator_Name == "Livestock production index (2004-2006 = 100)" | Indicator_Name == "Weighted livestock production index" |
     Indicator_Name == "Regional livestock production index per capita")

AFR_LIVE_PROD_ESA <- select(AFR_LIVE_PROD_ESA,Year,Period,Region,Country,Indicator_Name,Value)
AFR_LIVE_PROD_ESA$Value <- AFR_LIVE_PROD_ESA$Value
LIVE_PROD_ESA_unwtd <- filter(AFR_LIVE_PROD_ESA,!Country=="Numbers weighted index")

LIVE_PROD_ESA_wtd <- filter(AFR_LIVE_PROD_ESA,Country=="Numbers weighted index") %>%
     select(Year,Regional=Country,Value)

LIVE_PROD_ESA_rpc <- filter(AFR_LIVE_PROD_ESA,Country=="Weighted regional livestock production index rpc") %>%
     select(Year,Regional=Country,Value)

(LSR_Fig.15.3a <- ggplot(data=LIVE_PROD_ESA_unwtd) +
      geom_line(mapping=aes(x=Year,y=Value,group=Country),stat="identity",na.rm = TRUE) +
      geom_line(aes(x=Year,y=Value,col=Regional),data=LIVE_PROD_ESA_wtd,
                colour=("red"),size = 1.1,linetype = 2) +
     geom_line(aes(x=Year,y=Value,col=Regional),data=LIVE_PROD_ESA_rpc,
                colour=("green"),size = 1.1,linetype = 2) +
      xlab("Year") +
      labs(y="Index (2004-2006 = 100)",title = tles.Fig.15.3a) +
      scale_x_continuous(breaks = c(seq(1970,2016,5)),limits = c(1970,2016.46),expand = c(0,0)) +
      theme(text=element_text(family="Times",size = 11), legend.position = "bottom") +
          theme_tufte() +
 labs(caption = "Black lines are the 13 selected countries from East and southern Africa; dashed red line is regional trend, estimated by ln(Y) = -34.2 + 0.0193*Year;
      dashed green line is regional trend in per capita terms, estimated by ln(Y) = 0.3599+0.0021*Year. Data from www.faostat.org") +
          theme(plot.caption = element_text(hjust = 0)))



#Plot, WCA
AFR_LIVE_PROD_WCA <- filter(AFR_gather, Region == "West and Central Africa" & Year > 1969 & Year < 2017)
AFR_LIVE_PROD_WCA <- filter(AFR_LIVE_PROD_WCA, Indicator_Name == "Livestock production index (2004-2006 = 100)" |
 Indicator_Name == "Weighted livestock production index" |
      Indicator_Name == "Regional livestock production index per capita")
AFR_LIVE_PROD_WCA <- select(AFR_LIVE_PROD_WCA,Year,Period,Region,Country,Indicator_Name,Value)
AFR_LIVE_PROD_WCA$Value <- AFR_LIVE_PROD_WCA$Value
LIVE_PROD_WCA_unwtd <- filter(AFR_LIVE_PROD_WCA,!Country=="Numbers weighted index")

LIVE_PROD_WCA_wtd <- filter(AFR_LIVE_PROD_WCA,Country=="Numbers weighted index") %>%
     select(Year,Regional=Country,Value)

LIVE_PROD_WCA_rpc <- filter(AFR_LIVE_PROD_WCA,Country=="Weighted regional livestock production index rpc") %>%
     select(Year,Regional=Country,Value)

(LSR_Fig.15.3b <- ggplot() +
      geom_line(aes(x=Year,y=Value,group=Country),na.rm = TRUE,data=LIVE_PROD_WCA_unwtd) +
      geom_line(aes(x=Year,y=Value,col=Regional),data=LIVE_PROD_WCA_wtd,
                colour=("red"),size = 1.1,linetype = 2) +
     geom_line(aes(x=Year,y=Value,col=Regional),data=LIVE_PROD_WCA_rpc,
                colour=("green"),size = 1.1,linetype = 2) +
      xlab("Year") +
      labs(y="Index (2004-2006 = 100)", title = tles.Fig.15.3b) +
      scale_x_continuous(breaks = c(seq(1970,2016,5)),limits = c(1970,2016.46),expand = c(0,0)) +
      theme(text=element_text(family="Times",size = 11), legend.position = "bottom") +
          theme_tufte() +
      labs(caption = "Black lines are 10 selected countries from West and Central Africa; dashed red line is regional trend, estimated by ln(Y) = -51.4 + 0.0279*Year;
      dashed green line is regional trend in per capita terms,estimated by ln(Y) = -9.1269 + 0.0068. Data from www.faostat.org") +
          theme(plot.caption = element_text(hjust = 0)))


#================================================

#run some regressions
#
mdl_ESA_LPI_regr <- lm(log(Value) ~ Year,data = LIVE_PROD_ESA_wtd)
mdl_ESA_LPC_regr <- lm(log(Value) ~ Year,data = LIVE_PROD_ESA_rpc)
mdl_WCA_LPI_regr <- lm(log(Value) ~ Year,data = LIVE_PROD_WCA_wtd)
mdl_WCA_LPC_regr <- lm(log(Value) ~ Year,data = LIVE_PROD_WCA_rpc)


stargazer(mdl_ESA_LPI_regr,mdl_ESA_LPC_regr,
          mdl_WCA_LPI_regr,mdl_WCA_LPC_regr,
          title = "Figure 15.3--Livestock production trends varied by sub-region in sub-Saharan Africa",
          single.row=FALSE,type = "text",
          column.labels = c("ESA","ESA, pc","WCA","WCA,pc"),
          dep.var.labels.include = TRUE,
          dep.var.labels=c("Livestock production index"),
          model.names=TRUE,
          df=T,digits=4)

#run some regressions by sub-period
#------------ECA
live_prod_esa_wtd_1994 <- filter(LIVE_PROD_ESA_wtd,Year <= 1994)
(mdl_esa_prod_regr_1994 <- lm(log(Value) ~ Year,data=live_prod_esa_wtd_1994))

live_prod_esa_unwtd_1994 <- filter(LIVE_PROD_ESA_unwtd,Year<=1994)
(live_prod_esa_unwtd_1994_country <- live_prod_esa_unwtd_1994 %>% group_by(Country) %>%
     summarise(cgr=log(Value[25]/Value[1])/25))
summary(live_prod_esa_unwtd_1994_country,na.rm = TRUE)

live_prod_esa_wtd_2016 <- filter(LIVE_PROD_ESA_wtd,Year > 1994)
(mdl_esa_prod_regr_2016 <- lm(log(Value) ~ Year,data=live_prod_esa_wtd_2016))

live_prod_esa_unwtd_2016 <- filter(LIVE_PROD_ESA_unwtd,Year > 1994)
(live_prod_esa_unwtd_2016_country <- live_prod_esa_unwtd_2016 %>% group_by(Country) %>%
     summarise(cgr=log(Value[22]/Value[1])/22))
summary(live_prod_esa_unwtd_2016_country,na.rm = TRUE)


#-----------WCA
live_prod_wca_wtd_1994 <- filter(LIVE_PROD_WCA_wtd,Year <= 1994)
(mdl_wca_prod_regr_1994 <- lm(log(Value) ~ Year,data=live_prod_wca_wtd_1994))

live_prod_wca_unwtd_1994 <- filter(LIVE_PROD_WCA_unwtd,Year<=1994)
(live_prod_wca_unwtd_1994_country <- live_prod_wca_unwtd_1994 %>% group_by(Country) %>%
     summarise(cgr=log(Value[25]/Value[1])/25))
summary(live_prod_wca_unwtd_1994_country,na.rm = TRUE)

live_prod_wca_wtd_2016 <- filter(LIVE_PROD_WCA_wtd,Year > 1994)
(mdl_wca_prod_regr_2016 <- lm(log(Value) ~ Year,data=live_prod_wca_wtd_2016))

live_prod_wca_unwtd_2016 <- filter(LIVE_PROD_WCA_unwtd,Year > 1994)
(live_prod_wca_unwtd_2016_country <- live_prod_wca_unwtd_2016 %>% group_by(Country) %>%
     summarise(cgr=log(Value[22]/Value[1])/22))
summary(live_prod_wca_unwtd_2016_country,na.rm = TRUE)


stargazer(mdl_esa_prod_regr_1994,mdl_wca_prod_regr_1994,
          type = "text",
          title = "Figure 15.3 -- Livestock production index by sub-region before 1995",
          column.labels = c("ESA,1994","WCA,1994"),
          single.row=FALSE,
          dep.var.labels.include = TRUE,
          dep.var.labels=c("TLU density"),
          model.names=TRUE,
          df=T,digits=4)

stargazer(mdl_esa_prod_regr_2016,mdl_wca_prod_regr_2016,
          type = "text",
          title = "Figure 15.3 -- Livestock production index by sub-region after 1994",
          column.labels = c("ESA,2016","WCA,2016"),
          single.row=FALSE,
          dep.var.labels.include = TRUE,
          dep.var.labels=c("TLU density"),
          model.names=TRUE,
          df=T,digits=4)


#================================================

#-----------------------------------------------------------
#Figure 15.4a and 15.4b TLU and TLU density index for ESA and WCA
#Plot, ESA
AFR_TLU_density_ESA <- filter(AFR_gather, Region == "East and southern Africa" & Year > 1969 & Year < 2017)
AFR_TLU_density_ESA <- filter(AFR_TLU_density_ESA, Indicator_Name == "TLU density" | Indicator_Name == "Area wtd TLU index")
AFR_TLU_density_ESA <- select(AFR_TLU_density_ESA,Year,Period,Region,Country,Indicator_Name,Value)
AFR_TLU_density_ESA$Value <- 100*AFR_TLU_density_ESA$Value
TLU_density_ESA_unwtd <- filter(AFR_TLU_density_ESA,!Country=="Area weighted mean, ESA")

TLU_density_ESA_wtd <- filter(AFR_TLU_density_ESA,Country=="Area weighted mean, ESA") %>%
     select(Year,Regional=Country,Value)

(LSR_Fig.15.4a <- ggplot()+
      geom_line(aes(x=Year,y=Value,group=Country),data=TLU_density_ESA_unwtd) +
      geom_line(aes(x=Year,y=Value,col=Regional),data=TLU_density_ESA_wtd,
                colour=("red"),size = 1.1,linetype = 2) +
      xlab("Year") +
      labs(y="Tropical livestock units per km2", title = tles.Fig.15.4a) +
      scale_x_continuous(breaks = c(seq(1970,2016,5)),limits = c(1970,2016.46),expand = c(0,0)) +
      theme(text=element_text(family="Times",size = 11), legend.position = "bottom") +
                    theme_tufte() +
 labs(caption = "Dashed red line is an area-weighted regional index of TLU, estimated by ln(Y) = -14.7 + 0.0097*Year. Data from www.faostat.org") +
          theme(plot.caption = element_text(hjust = 0)))


#Plot, WCA
AFR_TLU_density_WCA <- filter(AFR_gather, Region == "West and Central Africa" & Year > 1969 & Year < 2017)
AFR_TLU_density_WCA <- filter(AFR_TLU_density_WCA, Indicator_Name == "TLU density" | Indicator_Name == "Area wtd TLU index")
AFR_TLU_density_WCA <- select(AFR_TLU_density_WCA,Year,Period,Region,Country,Indicator_Name,Value)
AFR_TLU_density_WCA$Value <- 100*AFR_TLU_density_WCA$Value
TLU_density_WCA_unwtd <- filter(AFR_TLU_density_WCA,!Country=="Area weighted mean, WCA")

TLU_density_WCA_wtd <- filter(AFR_TLU_density_WCA,Country=="Area weighted mean, WCA") %>%
     select(Year,Regional=Country,Value)

(LSR_Fig.15.4b <- ggplot()+
      geom_line(aes(x=Year,y=Value,group=Country),data=TLU_density_WCA_unwtd) +
      geom_line(aes(x=Year,y=Value,col=Regional),data=TLU_density_WCA_wtd,colour=("red"),size = 1.1,linetype = 2) +
      xlab("Year") +
      labs(y="Tropical livestock units per km2", title = tles.Fig.15.4b) +
      scale_x_continuous(breaks = c(seq(1970,2016,5)),limits = c(1970,2016.46),expand = c(0,0)) +
      theme(text=element_text(family="Times",size = 11), legend.position = "bottom") +
                    theme_tufte() +
      labs(caption = "Dashed red line is regional trend, estimated by ln(Y) = -42.1 + 0.0233*Year") +
          theme(plot.caption = element_text(hjust = 0)))


#================================================

#run some regressions
#
mdl_ESA_TLU_regr <- lm(log(Value) ~ Year,data = TLU_density_ESA_wtd)
mdl_WCA_TLU_regr <- lm(log(Value) ~ Year,data = TLU_density_WCA_wtd)


#run some regressions by sub-period
#------------ECA
tlu_density_esa_wtd_1994 <- filter(TLU_density_ESA_wtd,Year <= 1994)
(mdl_esa_tlu_regr_1994 <- lm(log(Value) ~ Year,data=tlu_density_esa_wtd_1994))

tlu_density_esa_unwtd_1994 <- filter(TLU_density_ESA_unwtd,Year<=1994)
(tlu_density_esa_unwtd_1994_country <- tlu_density_esa_unwtd_1994 %>% group_by(Country) %>%
     summarise(cgr=log(Value[25]/Value[1])/25))
summary(tlu_density_esa_unwtd_1994_country,na.rm = TRUE)

tlu_density_esa_wtd_2016 <- filter(TLU_density_ESA_wtd,Year > 1994)
(mdl_esa_tlu_regr_2016 <- lm(log(Value) ~ Year,data=tlu_density_esa_wtd_2016))

tlu_density_esa_unwtd_2016 <- filter(TLU_density_ESA_unwtd,Year > 1994)
(tlu_density_esa_unwtd_2016_country <- tlu_density_esa_unwtd_2016 %>% group_by(Country) %>%
     summarise(cgr=log(Value[22]/Value[1])/22))
summary(tlu_density_esa_unwtd_2016_country,na.rm = TRUE)



#-----------WCA
tlu_density_wca_wtd_1994 <- filter(TLU_density_WCA_wtd,Year <= 1994)
(mdl_wca_tlu_regr_1994 <- lm(log(Value) ~ Year,data=tlu_density_wca_wtd_1994))

tlu_density_wca_unwtd_1994 <- filter(TLU_density_WCA_unwtd,Year<=1994)
(tlu_density_wca_unwtd_1994_country <- tlu_density_wca_unwtd_1994 %>% group_by(Country) %>%
     summarise(cgr=log(Value[25]/Value[1])/25))
summary(tlu_density_wca_unwtd_1994_country,na.rm = TRUE)

tlu_density_wca_wtd_2016 <- filter(TLU_density_WCA_wtd,Year > 1994)
(mdl_wca_tlu_regr_2016 <- lm(log(Value) ~ Year,data=tlu_density_wca_wtd_2016))

tlu_density_wca_unwtd_2016 <- filter(TLU_density_WCA_unwtd,Year > 1994)
(tlu_density_wca_unwtd_2016_country <- tlu_density_wca_unwtd_2016 %>% group_by(Country) %>%
     summarise(cgr=log(Value[22]/Value[1])/22))
summary(tlu_density_wca_unwtd_2016_country,na.rm = TRUE)

stargazer(mdl_ESA_TLU_regr,mdl_WCA_TLU_regr,
          title = "Figure 15.4 -- Mean TLU density varied by sub-region, 1970-2016",
          single.row=FALSE,type = "text",
          column.labels = c("ESA","WCA"),
          dep.var.labels.include = TRUE,
          dep.var.labels=c("TLU density"),
          model.names=TRUE,
          df=T,digits=4)

stargazer(mdl_esa_tlu_regr_1994,mdl_wca_tlu_regr_1994,
          type = "text",
          title = "Figure 15.4 -- TLU density varied by sub-region before 1995",
          column.labels = c("ESA,1994","WCA,1994"),
          single.row=FALSE,
          dep.var.labels.include = TRUE,
          dep.var.labels=c("TLU density"),
          model.names=TRUE,
          df=T,digits=4)

stargazer(mdl_esa_tlu_regr_2016,mdl_wca_tlu_regr_2016,
          type = "text",
          title = "Figure 15.4 -- TLU density varied by sub-region after 1994",
          column.labels = c("ESA,2016","WCA,2016"),
          single.row=FALSE,
          dep.var.labels.include = TRUE,
          dep.var.labels=c("TLU density"),
          model.names=TRUE,
          df=T,digits=4)


#===========================FIGURE 15.5 (a) and FIGURE 15.5 (b)===================

#Plot, ESA
AFR_SGC_ratio_ESA <- filter(AFR_gather, Region == "East and southern Africa" & Year > 1969 & Year < 2017)
AFR_SGC_ratio_ESA <- filter(AFR_SGC_ratio_ESA, Indicator_Name == "Unweighted SGC ratio" | Indicator_Name == "SGC ratio")
AFR_SGC_ratio_ESA <- select(AFR_SGC_ratio_ESA,Year,Period,Region,Country,Indicator_Name,Value)
AFR_SGC_ratio_ESA$Value <- AFR_SGC_ratio_ESA$Value

SGC_ratio_ESA_unwtd <- filter(AFR_SGC_ratio_ESA, Country != "Unweighted ratio, ESA")

SGC_ratio_ESA_wtd <- filter(AFR_SGC_ratio_ESA,Country == "Unweighted ratio, ESA") %>%
     select(Year,Regional=Country,Value)

(LSR_Fig.15.5a <- ggplot()+
      geom_line(aes(x=Year,y=Value,group=Country),data=SGC_ratio_ESA_unwtd) +
      geom_line(aes(x=Year,y=Value,col=Regional),data=SGC_ratio_ESA_wtd,
                colour=("red"),size = 1.1,linetype = 2) +
      xlab("Year") +
      labs(y="Ratio of sheep and goat numbers to cattle numbers", title = tles.Fig.15.5a) +
      scale_x_continuous(breaks = c(seq(1970,2016,5)),limits = c(1970,2016.46),expand = c(0,0)) +
      theme(text=element_text(family="Times",size = 11), legend.position = "bottom") +
                    theme_tufte() +
 labs(caption = "Dashed red line is regional trend, estimated by ln(Y) = 0.1 + 0.0001*Year. Data from www.faostat.org") +
          theme(plot.caption = element_text(hjust = 0)))


#Plot, WCA
AFR_SGC_ratio_WCA <- filter(AFR_gather, Region == "West and Central Africa" & Year > 1969 & Year < 2017)
AFR_SGC_ratio_WCA <- filter(AFR_SGC_ratio_WCA, Indicator_Name == "Unweighted SGC ratio" | Indicator_Name == "SGC ratio")
AFR_SGC_ratio_WCA <- select(AFR_SGC_ratio_WCA,Year,Period,Region,Country,Indicator_Name,Value)
AFR_SGC_ratio_WCA$Value <- AFR_SGC_ratio_WCA$Value

SGC_ratio_WCA_unwtd <- filter(AFR_SGC_ratio_WCA, Country != "Unweighted ratio, WCA")

SGC_ratio_WCA_wtd <- filter(AFR_SGC_ratio_WCA,Country == "Unweighted ratio, WCA") %>%
     select(Year,Regional=Country,Value)

(LSR_Fig.15.5b <- ggplot()+
      geom_line(aes(x=Year,y=Value,group=Country),data=SGC_ratio_WCA_unwtd) +
      geom_line(aes(x=Year,y=Value,col=Regional),data=SGC_ratio_WCA_wtd,
                colour=("red"),size = 1.1,linetype = 2) +
      xlab("Year") +
      labs(y="Ratio of sheep and goat numbers to cattle numbers", title = tles.Fig.15.5b) +
      scale_x_continuous(breaks = c(seq(1970,2016,5)),limits = c(1970,2016.46),expand = c(0,0)) +
      theme(text=element_text(family="Times",size = 11), legend.position = "bottom") +
                    theme_tufte() +
      labs(caption = "Dashed red line is regional trend, estimated by ln(Y) = -33.2 + 0.0171*Year. Data from www.faostat.org") +
          theme(plot.caption = element_text(hjust = 0)))


#================================================

#run some regressions
#
mdl_ESA_SGC_regr <- lm(log(Value) ~ Year,data = SGC_ratio_ESA_wtd)
mdl_WCA_SGC_regr <- lm(log(Value) ~ Year,data = SGC_ratio_WCA_wtd)

stargazer(mdl_ESA_SGC_regr,mdl_WCA_SGC_regr,
          title = "Figure 15.5 -- Mean SGC varied by sub-region in sub-Saharan Africa",
             single.row=FALSE,type = "text",
          column.labels = c("ESA","WCA"),
          dep.var.labels.include = TRUE,
          dep.var.labels=c("Ratio of sheep and goats to cattle"),
          model.names=TRUE,
          df=T,digits=4)

#============================FIGURE 15.6 (a) and FIGURE 15.6 (b)================
#-------------------------arable land------------------------
#Plot, ESA
#
AFR_ARA_ratio_ESA <- filter(AFR_gather, Region == "East and southern Africa" & Year > 1969 & Year < 2017)
AFR_ARA_ratio_ESA <- filter(AFR_ARA_ratio_ESA, Indicator_Name == "Arable land (hectares per person)" | Indicator_Name == "Weighted arable land per capita")
AFR_ARA_ratio_ESA <- select(AFR_ARA_ratio_ESA,Year,Period,Region,Country,Indicator_Name,Value)
AFR_ARA_ratio_ESA$Value <- AFR_ARA_ratio_ESA$Value

ARA_ratio_ESA_unwtd <- filter(AFR_ARA_ratio_ESA, Country != "Population weighted arable land")

ARA_ratio_ESA_wtd <- filter(AFR_ARA_ratio_ESA,Country == "Population weighted arable land") %>%
     select(Year,Regional=Country,Value)

(LSR_Fig.15.6a <- ggplot()+
      geom_line(aes(x=Year,y=Value,group=Country),data=ARA_ratio_ESA_unwtd) +
      geom_line(aes(x=Year,y=Value,col=Regional),data=ARA_ratio_ESA_wtd,
                colour=("green"),size = 1.1,linetype = 2) +
      xlab("Year") + labs(y="Hectares per capita", title = tles.Fig.15.6a) +
      scale_x_continuous(breaks = c(seq(1970,2016,5)),limits = c(1970,2016.46),expand = c(0,0)) +
      theme(text=element_text(family="Times",size = 11), legend.position = "bottom") +
                    theme_tufte() +
 labs(caption = "Dashed green line is regional trend, estimated by ln(Y) = 21.4 -0.0115*Year. Data from www.faostat.org") +
          theme(plot.caption = element_text(hjust = 0)))


#Plot, WCA
AFR_ARA_ratio_WCA <- filter(AFR_gather, Region == "West and Central Africa" & Year > 1969 & Year < 2017)
AFR_ARA_ratio_WCA <- filter(AFR_ARA_ratio_WCA, Indicator_Name == "Arable land (hectares per person)" | Indicator_Name == "Weighted arable land per capita")
AFR_ARA_ratio_WCA <- select(AFR_ARA_ratio_WCA,Year,Period,Region,Country,Indicator_Name,Value)
AFR_ARA_ratio_WCA$Value <- AFR_ARA_ratio_WCA$Value

ARA_ratio_WCA_unwtd <- filter(AFR_ARA_ratio_WCA, Country != "Population weighted arable land")

ARA_ratio_WCA_wtd <- filter(AFR_ARA_ratio_WCA,Country == "Population weighted arable land") %>%
     select(Year,Regional=Country,Value)

(LSR_Fig.15.6b <- ggplot()+
      geom_line(aes(x=Year,y=Value,group=Country),data=ARA_ratio_WCA_unwtd) +
      geom_line(aes(x=Year,y=Value,col=Regional),data=ARA_ratio_WCA_wtd,
                colour=("green"),size = 1.1,linetype = 2) +
      xlab("Year") + labs(y="Hectares per capita", title = tles.Fig.15.6b) +
      scale_x_continuous(breaks = c(seq(1970,2016,5)),limits = c(1970,2016.46),expand = c(0,0)) +
      theme(text=element_text(family="Times",size = 11), legend.position = "bottom") +
                    theme_tufte() +
 labs(caption = "Dashed green line is regional trend, estimated by ln(Y) = 27.5 - 0.0143*Year.Data from www.faostat.org") +
          theme(plot.caption = element_text(hjust = 0)))


#================================================

#run some regressions
#
mdl_ESA_ARA_regr <- lm(log(Value) ~ Year,data = ARA_ratio_ESA_wtd)
mdl_WCA_ARA_regr <- lm(log(Value) ~ Year,data = ARA_ratio_WCA_wtd)


#================================================
#run some regressions by sub-period
#------------ECA
ara_ratio_esa_wtd_1994 <- filter(ARA_ratio_ESA_wtd,Year <= 1994)
(mdl_esa_ara_regr_1994 <- lm(log(Value) ~ Year,data=ara_ratio_esa_wtd_1994))

ara_ratio_esa_unwtd_1994 <- filter(ARA_ratio_ESA_unwtd,Year<=1994)
(ara_ratio_esa_unwtd_1994_country <- ara_ratio_esa_unwtd_1994 %>% group_by(Country) %>%
     summarise(cgr=log(Value[25]/Value[1])/25,meanlevel=mean(Value[25])))
summary(ara_ratio_esa_unwtd_1994_country,na.rm = TRUE)

ara_ratio_esa_wtd_2016 <- filter(ARA_ratio_ESA_wtd,Year > 1994)
(mdl_esa_ara_regr_2016 <- lm(log(Value) ~ Year,data=ara_ratio_esa_wtd_2016))

ara_ratio_esa_unwtd_2016 <- filter(ARA_ratio_ESA_unwtd,Year > 1994)
(ara_ratio_esa_unwtd_2016_country <- ara_ratio_esa_unwtd_2016 %>% group_by(Country) %>%
     summarise(cgr=log(Value[22]/Value[1])/22,meanlevel=mean(Value[22])))
summary(ara_ratio_esa_unwtd_2016_country,na.rm = TRUE)


#-----------WCA
ara_ratio_wca_wtd_1994 <- filter(ARA_ratio_WCA_wtd,Year <= 1994)
(mdl_wca_ara_regr_1994 <- lm(log(Value) ~ Year,data=ara_ratio_wca_wtd_1994))

ara_ratio_wca_unwtd_1994 <- filter(ARA_ratio_WCA_unwtd,Year<=1994)
(ara_ratio_wca_unwtd_1994_country <- ara_ratio_wca_unwtd_1994 %>% group_by(Country) %>%
     summarise(cgr=log(Value[25]/Value[1])/25,meanlevel=mean(Value[25])))
summary(ara_ratio_wca_unwtd_1994_country,na.rm = TRUE)

ara_ratio_wca_wtd_2016 <- filter(ARA_ratio_WCA_wtd,Year > 1994)
(mdl_wca_ara_regr_2016 <- lm(log(Value) ~ Year,data=ara_ratio_wca_wtd_2016))

ara_ratio_wca_unwtd_2016 <- filter(ARA_ratio_WCA_unwtd,Year > 1994)
(ara_ratio_wca_unwtd_2016_country <- ara_ratio_wca_unwtd_2016 %>% group_by(Country) %>%
     summarise(cgr=log(Value[22]/Value[1])/22,meanlevel=mean(Value[22])))
summary(ara_ratio_wca_unwtd_2016_country,na.rm = TRUE)

stargazer(mdl_ESA_ARA_regr,mdl_WCA_ARA_regr,
          title = "Figure 15.6 -- Trends in arable land per capita varied by sub-region in sub-Saharan Africa",
          type = "text",
          column.labels=c("ESA","WCA"),
          dep.var.labels.include = TRUE,
          dep.var.labels=c("Arable land per capita"),
          model.names=TRUE,
          df=T,digits=4)

stargazer(mdl_esa_ara_regr_1994,mdl_wca_ara_regr_1994,
          title = "Figure 15.6 -- Trends in arable land per capita varied by sub-region before 1995",
          type = "text",
          column.labels=c("ESA","WCA"),
          dep.var.labels.include = TRUE,
          dep.var.labels=c("Arable land per capita"),
          model.names=TRUE,
          df=T,digits=4)

stargazer(mdl_esa_ara_regr_2016,mdl_wca_ara_regr_2016,
          title = "Figure 15.6 -- Trends in arable land per capita varied by sub-region after 1994",
          type = "text",
          column.labels=c("ESA","WCA"),
          dep.var.labels.include = TRUE,
          dep.var.labels=c("Arable land per capita"),
          model.names=TRUE,
          df=T,digits=4)

#===========================FIGURE 15.7 (a) and FIGURE 15.7 (b) =================
#Plot, ESA

AFR_FERT_ratio_ESA <- filter(AFR_gather, Region == "East and southern Africa" & Year > 1969 & Year < 2017)
AFR_FERT_ratio_ESA <- filter(AFR_FERT_ratio_ESA, Indicator_Name == "Fertilizer consumption (kilograms per hectare of arable land)" | Indicator_Name == "Weighted fert use per ha")
AFR_FERT_ratio_ESA <- select(AFR_FERT_ratio_ESA,Year,Period,Region,Country,Indicator_Name,Value)
AFR_FERT_ratio_ESA$Value <- AFR_FERT_ratio_ESA$Value

FERT_ratio_ESA_unwtd <- filter(AFR_FERT_ratio_ESA, Country != "Arable land weighted")

FERT_ratio_ESA_wtd <- filter(AFR_FERT_ratio_ESA,Country == "Arable land weighted") %>%
     select(Year,Regional=Country,Value)

(LSR_Fig.15.7a <- ggplot()+
      geom_line(aes(x=Year,y=Value,group=Country),data=FERT_ratio_ESA_unwtd) +
      geom_line(aes(x=Year,y=Value,col=Regional),data=FERT_ratio_ESA_wtd,
                colour=("red"),size = 1.1,linetype = 2) +
      xlab("Year") +
      labs(y="Kilograms per hectare", title = tles.Fig.15.7a) +
           scale_x_continuous(breaks = c(seq(2002,2016,3)),limits = c(2002,2016.15),expand = c(0,0)) +
      theme(text=element_text(family="Times",size = 11), legend.position = "bottom") +
                    theme_tufte() +
 labs(caption = "Dashed red line is an area-weighted regional index of fertilizer use per hectare, estimated by ln(Y) = -19.4 + 0.0112*Year") +
          theme(plot.caption = element_text(hjust = 0)))


#Plot, WCA
AFR_FERT_ratio_WCA <- filter(AFR_gather, Region == "West and Central Africa" & Year > 1969 & Year < 2017)
AFR_FERT_ratio_WCA <- filter(AFR_FERT_ratio_WCA, Indicator_Name == "Fertilizer consumption (kilograms per hectare of arable land)" | Indicator_Name == "Weighted fert use per ha")
AFR_FERT_ratio_WCA <- select(AFR_FERT_ratio_WCA,Year,Period,Region,Country,Indicator_Name,Value)
AFR_FERT_ratio_WCA$Value <- AFR_FERT_ratio_WCA$Value

FERT_ratio_WCA_unwtd <- filter(AFR_FERT_ratio_WCA, Country != "Arable land weighted")

FERT_ratio_WCA_wtd <- filter(AFR_FERT_ratio_WCA,Country == "Arable land weighted") %>%
     select(Year,Regional=Country,Value)

(LSR_Fig.15.7b <- ggplot()+
      geom_line(aes(x=Year,y=Value,group=Country),data=FERT_ratio_WCA_unwtd) +
      geom_line(aes(x=Year,y=Value,col=Regional),data=FERT_ratio_WCA_wtd,
                colour=("red"),size = 1.1,linetype = 2) +
      xlab("Year") +
      labs(y="Kilograms per hectare", title = tles.Fig.15.7b) +
      scale_x_continuous(breaks = c(seq(2002,2016,3)),limits = c(2002,2016.15),expand = c(0,0)) +
      theme(text=element_text(family="Times",size = 11), legend.position = "bottom") +
                    theme_tufte() +
 labs(caption = "Dashed red line is an area-weighted regional index of fertilizer use per hectare, estimated by ln(Y) = -93.8 + 0.0478*Year. Data from www.faostat.org") +
          theme(plot.caption = element_text(hjust = 0)))


#================================================

#run some regressions
#
mdl_ESA_FERT_regr <- lm(log(Value) ~ Year,data = FERT_ratio_ESA_wtd)
mdl_WCA_FERT_regr <- lm(log(Value) ~ Year,data = FERT_ratio_WCA_wtd)

stargazer(mdl_ESA_FERT_regr,mdl_WCA_FERT_regr,
          title = "Figure 15.7 -- Trends in fertilizer use per hectare varied by sub-region in sub-Saharan Africa",
          type = "text",
          column.labels = c("ESA","WCA"),
          dep.var.labels.include = TRUE,
          dep.var.labels=c("Fertilizer use per hectare"),
          model.names=TRUE,
          df=T,digits=4)


#===============================FIGURE 15.8 (a) and FIGURE 15.8 (b) =====

#Plot, ESA
AFR_CEREAL_YIELD_ESA <- filter(AFR_gather, Region == "East and southern Africa" & Year > 1969 & Year < 2017)
AFR_CEREAL_YIELD_ESA <- filter(AFR_CEREAL_YIELD_ESA, Indicator_Name == "Cereal yield (kg per hectare)" | Indicator_Name == "Weighted cereal yield")
AFR_CEREAL_YIELD_ESA <- select(AFR_CEREAL_YIELD_ESA,Year,Period,Region,Country,Indicator_Name,Value)
AFR_CEREAL_YIELD_ESA$Value <- AFR_CEREAL_YIELD_ESA$Value

AFR_CEREAL_YIELD_ESA_unwtd <- filter(AFR_CEREAL_YIELD_ESA,
     Country != "Land weighted cereal yield")

AFR_CEREAL_YIELD_ESA_wtd <- filter(AFR_CEREAL_YIELD_ESA,
     Country == "Land weighted cereal yield") %>% select(Year,Regional=Country,Value)

(LSR_Fig.15.8a <- ggplot() +
      geom_line(aes(x=Year,y=Value,group=Country),data=AFR_CEREAL_YIELD_ESA_unwtd) +
      geom_line(aes(x=Year,y=Value,col=Regional),data=AFR_CEREAL_YIELD_ESA_wtd,
                colour=("red"),size = 1.1,linetype = 2) +
      xlab("Year") +
      labs(y="Kg per hectare", title = tles.Fig.15.8a) +
      scale_x_continuous(breaks = c(seq(1970,2016,5)),limits = c(1970,2016.46),expand = c(0,0)) +
         scale_y_continuous(labels = scales::comma) +
            theme(text=element_text(family="Times",size = 11), legend.position = "bottom") +
                    theme_tufte() +
 labs(caption = "Dashed red line is an area-weighted regional index of cereal yield per hectare, estimated by ln(Y) = -10.9 0.0091*Year. Data from www.faostat.org") +
          theme(plot.caption = element_text(hjust = 0)))


#Plot, WCA
AFR_CEREAL_YIELD_WCA <- filter(AFR_gather, Region == "West and Central Africa" & Year > 1969 & Year < 2017)
AFR_CEREAL_YIELD_WCA <- filter(AFR_CEREAL_YIELD_WCA, Indicator_Name == "Cereal yield (kg per hectare)" | Indicator_Name == "Weighted cereal yield")
AFR_CEREAL_YIELD_WCA <- select(AFR_CEREAL_YIELD_WCA,Year,Period,Region,Country,Indicator_Name,Value)
AFR_CEREAL_YIELD_WCA$Value <- AFR_CEREAL_YIELD_WCA$Value

AFR_CEREAL_YIELD_WCA_unwtd <- filter(AFR_CEREAL_YIELD_WCA, Country != "Land weighted cereal yield")

AFR_CEREAL_YIELD_WCA_wtd <- filter(AFR_CEREAL_YIELD_WCA,Country == "Land weighted cereal yield") %>%
     select(Year,Regional=Country,Value)

(LSR_Fig.15.8b <- ggplot()+
      geom_line(aes(x=Year,y=Value,group=Country),data=AFR_CEREAL_YIELD_WCA_unwtd) +
      geom_line(aes(x=Year,y=Value,col=Regional),data=AFR_CEREAL_YIELD_WCA_wtd,
                colour=("red"),size = 1.1,linetype = 2) +
      xlab("Year") +
      labs(y="Kg per hectare", title = tles.Fig.15.8b) +
      scale_x_continuous(breaks = c(seq(1970,2016,5)),limits = c(1970,2016.46),expand = c(0,0)) +
        scale_y_continuous(labels = scales::comma) +
          theme(text=element_text(family="Times",size = 11), legend.position = "bottom") +
                    theme_tufte() +
 labs(caption = "Dashed red line is an area-weighted regional index of cereal yield per hectare, estimated by ln(Y) = -18.6 + 0.0127*Year. Data from www.faostat.org") +
          theme(plot.caption = element_text(hjust = 0)))


#================================================

#run some regressions
#
mdl_ESA_CEREAL_regr <- lm(log(Value) ~ Year,data = AFR_CEREAL_YIELD_ESA_wtd)
mdl_WCA_CEREAL_regr <- lm(log(Value) ~ Year,data = AFR_CEREAL_YIELD_WCA_wtd)

stargazer(mdl_ESA_CEREAL_regr,mdl_WCA_CEREAL_regr,
          title = "Figure 15.8 - Trends in cereal yields varied by sub-region in sub-Saharan Africa",
          type = "text",
          column.labels=c("ESA","WCA"),
          dep.var.labels.include = TRUE,
          dep.var.labels=c("Cereal yields per hectare"),
          model.names=TRUE,
          df=T,digits=4)

#run some regressions by sub-period
#------------ECA
cereal_esa_wtd_1994 <- filter(AFR_CEREAL_YIELD_ESA_wtd,Year <= 1994)
(mdl_esa_cereal_regr_1994 <- lm(log(Value) ~ Year,data=cereal_esa_wtd_1994))

cereal_esa_unwtd_1994 <- filter(AFR_CEREAL_YIELD_ESA_unwtd,Year <= 1994)
(cereal_esa_unwtd_1994_country <- cereal_esa_unwtd_1994 %>% group_by(Country) %>%
     summarise(cgr=log(Value[25]/Value[1])/25,
               meanlevel70=mean(Value[1]),
               meanlevel94=mean(Value[25])))
summary(cereal_esa_unwtd_1994_country,na.rm = TRUE)

cereal_esa_wtd_2016 <- filter(AFR_CEREAL_YIELD_ESA_wtd,Year > 1994)
(mdl_esa_cereal_regr_2016 <- lm(log(Value) ~ Year,data=cereal_esa_wtd_2016))

cereal_esa_unwtd_2016 <- filter(AFR_CEREAL_YIELD_ESA_unwtd,Year > 1994)
(cereal_esa_unwtd_2016_country <- cereal_esa_unwtd_2016 %>% group_by(Country) %>%
     summarise(cgr=log(Value[22]/Value[1])/22,
     meanlevel95=mean(Value[1]),
     mean2016=mean(Value[22])))
summary(cereal_esa_unwtd_2016_country,na.rm = TRUE)


#-----------WCA
cereal_wca_wtd_1994 <- filter(AFR_CEREAL_YIELD_WCA_wtd,Year <= 1994)
(mdl_wca_cereal_regr_1994 <- lm(log(Value) ~ Year,data=cereal_wca_wtd_1994))

cereal_wca_unwtd_1994 <- filter(AFR_CEREAL_YIELD_WCA_unwtd,Year<=1994)
(cereal_wca_unwtd_1994_country <- cereal_wca_unwtd_1994 %>% group_by(Country) %>%
     summarise(cgr=log(Value[25]/Value[1])/25,
               meanlevel70=mean(Value[1]),
               meanlevel94=mean(Value[25])))
summary(cereal_wca_unwtd_1994_country,na.rm = TRUE)

cereal_wca_wtd_2016 <- filter(AFR_CEREAL_YIELD_WCA_wtd,Year > 1994)
(mdl_wca_cereal_regr_2016 <- lm(log(Value) ~ Year,data=cereal_wca_wtd_2016))

cereal_wca_unwtd_2016 <- filter(AFR_CEREAL_YIELD_WCA_unwtd,Year > 1994)
(cereal_wca_unwtd_2016_country <- cereal_wca_unwtd_2016 %>% group_by(Country) %>%
     summarise(cgr=log(Value[22]/Value[1])/22,
               meanlevel95=mean(Value[1]),
               meanlevel2016=mean(Value[22])))
summary(cereal_wca_unwtd_2016_country,na.rm = TRUE)

#=======================================FIGURE 15.9 (a) and FIGURE 15.9 (b) =============
#------------------------FOOD PRODUCTION INDEX
#
AFR_FOOD_PROD_ESA <- filter(AFR_gather, Region == "East and southern Africa" & Year > 1969 & Year < 2017)
AFR_FOOD_PROD_ESA <- filter(AFR_FOOD_PROD_ESA, Indicator_Name == "Food production index (2004-2006 = 100)" |
 Indicator_Name == "Weighted food production index" |
 Indicator_Name == "Regional food production index per capita")
AFR_FOOD_PROD_ESA <- select(AFR_FOOD_PROD_ESA,Year,Period,Region,Country,Indicator_Name,Value)
AFR_FOOD_PROD_ESA$Value <- AFR_FOOD_PROD_ESA$Value
FOOD_PROD_ESA_unwtd <- filter(AFR_FOOD_PROD_ESA,!Country=="Population production weighted")

FOOD_PROD_ESA_wtd <- filter(AFR_FOOD_PROD_ESA,Country=="Population production weighted") %>%
     select(Year,Regional=Country,Value)

FOOD_PROD_ESA_rpc <- filter(AFR_FOOD_PROD_ESA,Country=="Weighted regional food production index rpc") %>%
     select(Year,Regional=Country,Value)

(LSR_Fig.15.9a <- ggplot()+
      geom_line(aes(x=Year,y=Value,group=Country),data=FOOD_PROD_ESA_unwtd) +
      geom_line(aes(x=Year,y=Value,col=Regional),data=FOOD_PROD_ESA_wtd,
                colour=("red"),size = 1.1,linetype = 2) +
      geom_line(aes(x=Year,y=Value,col=Regional),data=FOOD_PROD_ESA_rpc,
                colour=("green"),size = 1.1,linetype = 2) +
      xlab("Year") +
      labs(y="Index (2004-2006 = 100)", title = tles.Fig.15.9a) +
      scale_x_continuous(breaks = c(seq(1970,2016,5)),limits = c(1970,2016.46),expand = c(0,0)) +
      theme(text=element_text(family="Times",size = 11), legend.position = "bottom") +
                    theme_tufte() +
 labs(caption = "Black lines are 13 selected countries from East and southern Africa;
      Dashed red line is an area-weighted regional index of food availability, estimated by ln(Y) = -59.3 + 0.0319*Year;
      dashed green line is regional trend in per capita terms, estimated by ln(Y) = -5.6 + 0.0051*Year. Data from www.faostat.org") +
          theme(plot.caption = element_text(hjust = 0)))


#Plot, WCA
AFR_FOOD_PROD_WCA <- filter(AFR_gather, Region == "West and Central Africa" & Year > 1969 & Year < 2017)
AFR_FOOD_PROD_WCA <- filter(AFR_FOOD_PROD_WCA, Indicator_Name == "Food production index (2004-2006 = 100)" |
 Indicator_Name == "Weighted food production index" |
 Indicator_Name == "Regional food production index per capita")
AFR_FOOD_PROD_WCA <- select(AFR_FOOD_PROD_WCA,Year,Period,Region,Country,Indicator_Name,Value)
AFR_FOOD_PROD_WCA$Value <- AFR_FOOD_PROD_WCA$Value
FOOD_PROD_WCA_unwtd <- filter(AFR_FOOD_PROD_WCA,!Country=="Population production weighted")

FOOD_PROD_WCA_wtd <- filter(AFR_FOOD_PROD_WCA,Country=="Population production weighted") %>%
     select(Year,Regional=Country,Value)

FOOD_PROD_WCA_rpc <- filter(AFR_FOOD_PROD_WCA,Country=="Weighted regional food production index rpc") %>%
     select(Year,Regional=Country,Value)

(LSR_Fig.15.9b <- ggplot()+
      geom_line(aes(x=Year,y=Value,group=Country),data=FOOD_PROD_WCA_unwtd) +
      geom_line(aes(x=Year,y=Value,col=Regional),data=FOOD_PROD_WCA_wtd,
                colour=("red"),size = 1.1,linetype = 2) +
           geom_line(aes(x=Year,y=Value,col=Regional),data=FOOD_PROD_WCA_rpc,
                colour=("green"),size = 1.1,linetype = 2) +
      labs(x="Year",y="Index (2004-2006 = 100)", title = tles.Fig.15.9b) +
      scale_x_continuous(breaks = c(seq(1970,2016,5)),limits = c(1970,2016.46),expand = c(0,0)) +
      theme(text=element_text(family="Times",size = 11), legend.position = "bottom") +
                    theme_tufte() +
 labs(caption = "Black lines are 10 countries from West and Central Africa;
      Dashed red line is an area-weighted regional index of food availability, estimated by ln(Y) = -68.6 + 0.0365*Year;
      dashed green line is regional trend in per capita terms, estimated by ln(Y) = -19.2 + 0.0119*Year. Data from www.faostat.org") +
          theme(plot.caption = element_text(hjust = 0)))

#================================================
#run some regressions
#

mdl_ESA_FOOD_regr <- lm(log(Value) ~ Year,data = FOOD_PROD_ESA_wtd)
mdl_WCA_FOOD_regr <- lm(log(Value) ~ Year,data = FOOD_PROD_WCA_wtd)
mdl_ESA_FOOD_rpc_regr <- lm(log(Value) ~ Year,data = FOOD_PROD_ESA_rpc)
mdl_WCA_FOOD_rpc_regr <- lm(log(Value) ~ Year,data = FOOD_PROD_WCA_rpc)


#run some regressions by sub-period
#------------ECA
food_esa_rpc_1994 <- filter(FOOD_PROD_ESA_rpc,Year <= 1994)
(mdl_esa_ara_regr_1994 <- lm(log(Value) ~ Year,data=food_esa_rpc_1994))

food_esa_unwtd_1994 <- filter(FOOD_PROD_ESA_unwtd,Year <= 1994)
(food_esa_unwtd_1994_country <- food_esa_unwtd_1994 %>% group_by(Country) %>%
     summarise(cgr=log(Value[25]/Value[1])/25,
               meanlevel70=mean(Value[1]),
               meanlevel94=mean(Value[25])))
summary(food_esa_unwtd_1994_country,na.rm = TRUE)

food_esa_rpc_2016 <- filter(FOOD_PROD_ESA_rpc,Year > 1994)
(mdl_esa_ara_regr_2016 <- lm(log(Value) ~ Year,data=food_esa_rpc_2016))

food_esa_unwtd_2016 <- filter(FOOD_PROD_ESA_unwtd,Year > 1994)
(food_esa_unwtd_2016_country <- food_esa_unwtd_2016 %>% group_by(Country) %>%
     summarise(cgr=log(Value[22]/Value[1])/22,
     meanlevel95=mean(Value[1]),
     mean2016=mean(Value[22])))
summary(food_esa_unwtd_2016_country,na.rm = TRUE)


#-----------WCA
food_wca_rpc_1994 <- filter(FOOD_PROD_WCA_rpc,Year <= 1994)
(mdl_wca_ara_regr_1994 <- lm(log(Value) ~ Year,data=food_wca_rpc_1994))

food_wca_unwtd_1994 <- filter(FOOD_PROD_WCA_unwtd,Year<=1994)
(food_wca_unwtd_1994_country <- food_wca_unwtd_1994 %>% group_by(Country) %>%
     summarise(cgr=log(Value[25]/Value[1])/25,
               meanlevel70=mean(Value[1]),
               meanlevel94=mean(Value[25])))
summary(food_wca_unwtd_1994_country,na.rm = TRUE)

food_wca_rpc_2016 <- filter(FOOD_PROD_WCA_rpc,Year > 1994)
(mdl_wca_ara_regr_2016 <- lm(log(Value) ~ Year,data=food_wca_rpc_2016))

food_wca_unwtd_2016 <- filter(FOOD_PROD_WCA_unwtd,Year > 1994)
(food_wca_unwtd_2016_country <- food_wca_unwtd_2016 %>% group_by(Country) %>%
     summarise(cgr=log(Value[22]/Value[1])/22,
               meanlevel95=mean(Value[1]),
               meanlevel2016=mean(Value[22])))
summary(food_wca_unwtd_2016_country,na.rm = TRUE)

stargazer(mdl_ESA_FOOD_regr,mdl_ESA_FOOD_rpc_regr,
          mdl_WCA_FOOD_regr,mdl_WCA_FOOD_rpc_regr,
          title = "Figure 15.9 -- Trends in food production varied by sub-region in sub-Saharan Africa",
          single.ROW=FALSE,type = "text",
          column.labels = c("ESA","ESA, pc","WCA","WCA, pc"),
          dep.var.labels.include = TRUE,
          dep.var.labels=c("Food production index"),
          model.names=TRUE,
          df=T,digits=4)


#make some tables
AFR_gather_regions <- filter(AFR_gather,Year > 1969 & Year < 2017)
AFR_gather_regions <- filter(AFR_gather_regions, Region == "East and southern Africa" | Region == "West and Central Africa")
AFR_gather_regions.cereal.yield <- filter(AFR_gather_regions, Indicator_Name == "Cereal yield (kg per hectare)")
AFR_gather_regions.fert <- filter(AFR_gather_regions, Year >= 2001 & Indicator_Name == "Fertilizer consumption (kilograms per hectare of arable land)")

(tibble_AFR_gather_regions.cereal.yield <- AFR_gather_regions.cereal.yield %>%
          group_by(Region,Period) %>% summarise(meanYield=mean(Value,na.rm = TRUE)))

(tibble_AFR_gather_regions.fert <- AFR_gather_regions.fert %>%
          group_by(Region,Period) %>% summarise(meanFert=mean(Value,na.rm = TRUE)))


#
##
#CREATE THE TIFF FILES
#Figures 15.1 through 15.9
#

# following code is commented out
# if you want to export the figures to tiff files then uncomment
#

# ggarrange(LSR_Fig.15.1, ncol=1,nrow=1,align = "h")  %>%
#      ggexport(filename = paste(c(tles.Fig.15.1,".tiff"),collapse = ""))
#
# ggarrange(LSR_Fig.15.2, ncol=1,nrow=1,align = "h")  %>%
#      ggexport(filename = paste(c(tles.Fig.15.2,".tiff"),collapse = ""))
#
# ggarrange(LSR_Fig.15.3a, ncol=1,nrow=1,align = "h")  %>%
#      ggexport(filename = paste(c(tles.Fig.15.3a,".tiff"),collapse = ""))
#
# ggarrange(LSR_Fig.15.3b,align = "h") %>%
#      ggexport(filename = paste(c(tles.Fig.15.3b,".tiff"),collapse = ""))
#
# ggarrange(LSR_Fig.15.4a, ncol=1,nrow=1,align = "h")  %>%
#      ggexport(filename = paste(c(tles.Fig.15.4a,".tiff"),collapse = ""))
#
# ggarrange(LSR_Fig.15.4b,align = "h") %>%
#      ggexport(filename = paste(c(tles.Fig.15.4b,".tiff"),collapse = ""))
#
# ggarrange(LSR_Fig.15.5a, ncol=1,nrow=1,align = "h")  %>%
#      ggexport(filename = paste(c(tles.Fig.15.5a,".tiff"),collapse = ""))
#
# ggarrange(LSR_Fig.15.5b,align = "h") %>%
#      ggexport(filename = paste(c(tles.Fig.15.5b,".tiff"),collapse = ""))
#
# ggarrange(LSR_Fig.15.6a, ncol=1,nrow=1,align = "h")  %>%
#      ggexport(filename = paste(c(tles.Fig.15.6a,".tiff"),collapse = ""))
#
# ggarrange(LSR_Fig.15.6b,align = "h") %>%
#      ggexport(filename = paste(c(tles.Fig.15.6b,".tiff"),collapse = ""))
#
# ggarrange(LSR_Fig.15.7a, ncol=1,nrow=1,align = "h")  %>%
#      ggexport(filename = paste(c(tles.Fig.15.7a,".tiff"),collapse = ""))
#
# ggarrange(LSR_Fig.15.7b,align = "h") %>%
#      ggexport(filename = paste(c(tles.Fig.15.7b,".tiff"),collapse = ""))
#
# ggarrange(LSR_Fig.15.8a, ncol=1,nrow=1,align = "h")  %>%
#      ggexport(filename = paste(c(tles.Fig.15.8a,".tiff"),collapse = ""))
#
# ggarrange(LSR_Fig.15.8b,align = "h") %>%
#      ggexport(filename = paste(c(tles.Fig.15.8b,".tiff"),collapse = ""))
#
# ggarrange(LSR_Fig.15.9a, ncol=1,nrow=1,align = "h")  %>%
#      ggexport(filename = paste(c(tles.Fig.15.9a,".tiff"),collapse = ""))
#
# ggarrange(LSR_Fig.15.9b,align = "h") %>%
#      ggexport(filename = paste(c(tles.Fig.15.9b,".tiff"),collapse = ""))
#


#make the figure titles
Figure.titles.15 <- as.character(mget(ls(pattern = "tles.Fig.15")))

# following code is commented out
# if you want to print the figure titles then uncomment
#

#
# capture.output(print(as.character(Figure.titles.15),
# print.gap=1,quote = FALSE),file = "Chapter 15, Figure titles")
