#code to generate figures for the priary production preface
#
#NOTA BENE: some of the labels and notes to the Figures may differ
#slightly from what is in the book because of formatting changes made during printing
#

setwd("C:/Users/jmm19/Documents/______ILRI/_______________DATA_PORTAL")


library(dplyr)
library(tidyr)
library(tidyselect)
library(data.table)
library(doBy)
library(stringi)
library(stringr)
library(openxlsx)

library(gdata)
library(ggplot2)
library(ggthemes)
library(ggpubr)
library(grid)
library(gridExtra)
library(graphics)
library(scales)
library(extrafont)
library(extrafontdb)
library(forcats)

library(janitor)

loadfonts(device = "win")
windowsFonts(Times = windowsFont("Times New Roman"))

#first make the figure titles
#
tles.Fig.PII.1 <- c("Fig.PII.1. Primary production research has been a small share of total ILRI spending, 1975-2018")

tles.Fig.PII.2 <- c("Fig.PII.2. ILRI work has been a large share of global publications on rangelands and related problems, 1977-2018")

tles.Fig.PII.3 <- c("Fig.PII.3. ILRI work has been a small share of global publications on planted forages, 1977-2018")

tles.Fig.PII.4 <- c("Fig.PII.4. ILRI has made niche contributions to multidimensional crops research, 1977-2018")

tles.Fig.PII.5 <- c("Fig.PII.5. Frequency of citations of ILRI and global institutions in primary production research by quantile, 1977-2018")

#label the colors ================================
#
col_theil <- c("#E6E600FF")
col_tryps <- c("#ECB176FF")
col_total_ILRAD <- c("#682622DD")
col_management_and_other <- c("#B6DB00FF")
col_animprod_health_gen <- c("#ECB176FF")
col_ILRI <- c("#682622DD")
col_global <- c("#0000FFFF")
col_ECF <- c("#ECB176FF")
col_genetics <- c("#FF3E00FF")
col_fstz <- c("#FF9800FF")
col_feed_forage <- c("#1DB000FF")
col_econ_policy <- c("#F2F2F2FF")
col_live_sys <- c("#FF3000FF")
col_ahg_color <- c("#FF0000FF")
col_manage <- c("#B6DB00FF")
col_ILRI <- c("#682622FF")
col_capdev <- c("#8FFFFFFF")


#create the palettes
pal_pp_fig_1 <- c(col_live_sys,col_econ_policy,col_feed_forage,
                  col_animprod_health_gen,col_capdev,col_management_and_other,col_ILRI)

pal_pp_fig_3 <- c(col_ILRI,col_global)

#
#the spending data for preface PP
#MAKE THE FIGURES FROM THE SPEND_CITES FILES
bib_final_clean <- read.xlsx("BIBLIO_PORTAL.xlsx",sheet = "BIBLIO",colNames = T)

bib_final_clean <- data.table(bib_final_clean)

PrefPPiom <- read.xlsx("FINANCIAL_PORTAL.xlsx",sheet = "Spend_Cites_R", colNames = T)

PrefPPiom <- data.table(filter(PrefPPiom, YEAR > 1974 & YEAR < 2019))

PrefPPiom_df <- select(PrefPPiom,Year = YEAR,
                       `ILRI forage papers` = ILCA_ILRI_FORAGE_PAPERS,
                       `Global forage papers` = GLOBAL_FORAGE_PAPERS,
                       `ILRI grassland papers` = ILCA_ILRI_GRASS_PAPERS,
                       `Selected global grassland papers` = GRASS_GRLABA_PAPERS,
                       `ILRI climate papers within grasslands` = ILRI_CLIM_IN_GRASS_PAPERS,

                       `ILRI forage citations` = ILCA_ILRI_FORAGE_TC,
                       `Global forage citations` = GLOBAL_FORAGE_TC,
                       `ILRI grassland citations` = ILCA_ILRI_GRASS_TC,
                       `Selected global grassland citations` = GRASS_GRLABA_TC,
                       `ILRI climate citations within grasslands` = ILRI_CLIM_IN_GRASS_TC,

                       `Livestock systems` = GROUP_SYSTEMS_SPEND,
                       `Economics and policy` = GROUP_ECON_POLICY_SPEND,
                       `Primary production` = GROUP_PLANT_BIOMASS_SPEND,
                       `Animal science` 	= GROUP_ANIM_SCI_SPEND,
                       `Capacity development` 	= GROUP_CAP_DEV_SPEND,
                       `Management and technical` = GROUP_OTHER_SPEND,
                       `Total spending` = LIFETIME_TOTAL_SPEND,

                       `Global multidimensional crop papers` = MULTID_GLOBAL_PAPERS,
                       `Global multidimensional crop citations` = MULTID_GLOBAL_TC,
                       `ILRI multidimensional crop papers` = ILCA_ILRI_MULTID_PAPERS,
                       `ILRI multidimensional crop citations` = ILCA_ILRI_MULTID_TC,

                       `ILRI all primary production papers` = ILCA_ILRI_ALL_PLANT_BIOMASS_PAPERS,
                       `ILRI all primary production citations` = ILCA_ILRI_ALL_PLANT_BIOMASS_TC,
                       `Global all primary production papers` = GLOBAL_ALL_PLANT_BIOMASS_PAPERS,
                       `Global all primary production citations` = GLOBAL_ALL_PLANT_BIOMASS_TC)


#FIGURE 1
PrefPPiom_df_spend <- select(PrefPPiom_df,Year,`Livestock systems`:`Total spending`)

PrefPPiom_df_spend <- data.table(PrefPPiom_df_spend %>% mutate(Period = case_when(
     Year < 1981 ~ "1975-80",
     Year >= 1981 & Year  <=  1985 ~ "1981-85",
     Year >= 1986 & Year  <=  1990 ~ "1986-90",
     Year >= 1991 & Year  <=  1994 ~ "1991-94",
     Year >= 1995 & Year  <=  2000 ~ "1995-00",
     Year >= 2001 & Year  <=  2005 ~ "2001-05",
     Year >= 2006 & Year  <=  2010 ~ "2006-10",
     TRUE ~ "2011-18")))


PrefPPiom_df_spend$Period <- factor(PrefPPiom_df_spend$Period)

PrefPPiom_df_spend_gather <- pivot_longer(PrefPPiom_df_spend,`Livestock systems`:`Total spending`,
names_to = "Domain", values_to = "Spending")

PrefPbiom_df_spend_gather <- PrefPPiom_df_spend_gather %>%
     mutate(Domain = fct_relevel(Domain,"Total spending","Animal science",
                               "Management and technical","Livestock systems",
                               "Economics and policy","Capacity development",
                               "Primary production"))

PrefPPiom_df_spend_group <- group_by(PrefPPiom_df_spend_gather,Period,Domain) %>%
     summarise(sumDomain = sum(Spending))


(FIGURE_PREFACE_PP_1 <- ggplot(data = PrefPPiom_df_spend_group) +
          geom_col(mapping = aes(x = Period, fill = Domain, y = sumDomain/1000),
                   position = position_dodge()) +
          labs(x = "Period",y = "Spending in millions of 2015 US$",title = tles.Fig.PII.1) +
          scale_fill_manual(name = "Domain",values = pal_pp_fig_1) +
          theme_tufte() +
          theme(text = element_text(size = 11,family = "Times"),legend.position = "bottom") +
          labs(caption = "Source: Constructed by authors from data in ILCA/ILRAD/ILRI Annual Reports and Financial Reports.") +
          theme(plot.caption = element_text(hjust = 0)))


#FEED AND FORAGE

PFORAGE_GLOBAL_TC_PY <- read.xlsx("PREFACE_PP_PORTAL.xlsx",sheet = "PFORAGE_GLOBAL_TC_PY",colNames = T)
PFORAGE_GLOBAL_TC_PY <- filter(PFORAGE_GLOBAL_TC_PY,PY >= 1977 & PY  <=  2018)
PFORAGE_GLOBAL_TC_PY <- select(PFORAGE_GLOBAL_TC_PY,PY,Papers = TC.length,Citations = TC.sum)
PFORAGE_GLOBAL_TC_PY$Institution <- as.factor("Global")

PFORAGE_ILRI_TC_PY <- read.xlsx("PREFACE_PP_PORTAL.xlsx",sheet = "PFORAGE_ILRI_TC_PY",colNames = T)
PFORAGE_ILRI_TC_PY <- filter(PFORAGE_ILRI_TC_PY,PY >= 1977 & PY  <=  2018)
PFORAGE_ILRI_TC_PY <- select(PFORAGE_ILRI_TC_PY,PY,Papers = TC.length,Citations = TC.sum)
PFORAGE_ILRI_TC_PY$Institution <- as.factor("ILRI")

ALL_PFORAGE_TC_PY <- rbind(PFORAGE_GLOBAL_TC_PY,PFORAGE_ILRI_TC_PY)
ALL_PFORAGE_TC_PY <- select(ALL_PFORAGE_TC_PY,PY,Institution,Papers,Citations)
ALL_PFORAGE_TC_PY <- ALL_PFORAGE_TC_PY %>%
     mutate(Institution = fct_relevel(Institution,"ILRI","Global"))

ALL_PFORAGE_TC_PY <- ALL_PFORAGE_TC_PY %>%
     mutate(Period = case_when(
          PY >= 1977 & PY  <=  1980 ~ "1977-80",
          PY >= 1981 & PY  <=  1985 ~ "1981-85",
          PY >= 1986 & PY  <=  1990 ~ "1986-90",
          PY >= 1991 & PY  <=  1994 ~ "1991-94",
          PY >= 1995 & PY  <=  2000 ~ "1995-00",
          PY >= 2001 & PY  <=  2005 ~ "2001-05",
          PY >= 2006 & PY  <=  2010 ~ "2006-10",
          TRUE ~ "2011-18"))

ALL_PFORAGE_TC_PY$Period <- factor(ALL_PFORAGE_TC_PY$Period)
ALL_PFORAGE_TC_PY$PY <- factor(ALL_PFORAGE_TC_PY$PY)

ALL_PFORAGE_TC_PY <- ALL_PFORAGE_TC_PY %>%
     mutate(Group_of_citations = case_when(
          Citations  <=  20 ~ "1-20",
          Citations >= 21 & Citations  <=  40 ~ "21-40",
          Citations >= 41 & Citations  <=  80 ~ "41-80",
          Citations >= 81 & Citations  <=  200 ~ "81-200",
          Citations >= 201 ~ "> 200"))

ALL_PFORAGE_TC_PY$Group_of_citations <- factor(ALL_PFORAGE_TC_PY$Group_of_citations)

ALL_PFORAGE_TC_PY <- select(ALL_PFORAGE_TC_PY,PY,Period,Institution,
                          Group_of_citations,Papers,Citations)

ALL_PFORAGE_TC_PY_gather <- ALL_PFORAGE_TC_PY %>% pivot_longer(Papers:Citations,
names_to = "Bibliometric",values_to = "Value")

ALL_PFORAGE_TC_PY_gather <- ALL_PFORAGE_TC_PY_gather %>%
     mutate(Bibliometric = fct_relevel(Bibliometric,"Papers","Citations"))

ALL_PFORAGE_TC_PY_group <- group_by(ALL_PFORAGE_TC_PY_gather,Institution,Period,Bibliometric) %>%
     summarise(sumBiblio = sum(Value))

pp_ILRI.fig3.papers <- ALL_PFORAGE_TC_PY_group %>% filter(Bibliometric == "Papers" & Institution == "ILRI") %>%
     group_by(Period) %>% summarise(nPapers = sum(sumBiblio))
(pp_ILRI.fig3.papers.total <- sum(pp_ILRI.fig3.papers$nPapers))

pp_global.fig3.papers <- ALL_PFORAGE_TC_PY_group %>% filter(Bibliometric == "Papers" & Institution == "Global") %>%
     group_by(Period) %>% summarise(nPapers = sum(sumBiblio,na.rm = TRUE))
(pp_global.fig3.papers.total <- sum(pp_global.fig3.papers$nPapers))


#tles.Fig.PII.3---------------------------------------
(FIGURE_PREFACE_PP_FORAGE_3 <- ggplot(data = ALL_PFORAGE_TC_PY_group) +
     geom_col(aes(x = Period,y = sumBiblio, fill = Institution), position = position_dodge(),size = 1) +
     facet_wrap(~Bibliometric,scales = "free_y",nrow = 2,ncol = 1) +
     labs(x = "Publication period",y = "Counts",title = tles.Fig.PII.3) +
          scale_y_continuous(labels = scales::comma) +
          scale_fill_manual(name = "Institution", values = pal_pp_fig_3) +
     theme_tufte() +
     theme(text = element_text(size = 11,family = "Times"),legend.position = "bottom") +
     labs(caption = paste(c("Source: Constructed by authors from data at www.scopus.com. ILRI sample = ", pp_ILRI.fig3.papers.total," papers; the global sample = ", pp_global.fig3.papers.total," papers."),collapse = "")) +
     theme(plot.caption = element_text(hjust = 0)))

#GRASS
GRASS_TC_PY_GRLABA <- read.xlsx("PREFACE_PP_PORTAL.xlsx",sheet = "GRASS_GRLABA_TC_PY",colNames = T)
GRASS_TC_PY_GRLABA <- filter(GRASS_TC_PY_GRLABA,PY >= 1977 & PY  <=  2018)
GRASS_TC_PY_GRLABA <- select(GRASS_TC_PY_GRLABA,PY,Papers = TC.length,Citations = TC.sum)
GRASS_TC_PY_GRLABA$Institution <- as.factor("Global")

GRASS_TC_PY_ILRI <- read.xlsx("PREFACE_PP_PORTAL.xlsx",sheet = "GRASS_ILRI_TC_PY",colNames = T)
GRASS_TC_PY_ILRI <- filter(GRASS_TC_PY_ILRI,PY >= 1977 & PY  <=  2018)
GRASS_TC_PY_ILRI <- select(GRASS_TC_PY_ILRI,PY,Papers = TC.length,Citations = TC.sum)
GRASS_TC_PY_ILRI$Institution <- as.factor("ILRI")

ALL_GRASS_TC_PY <- rbind(GRASS_TC_PY_GRLABA,GRASS_TC_PY_ILRI)
ALL_GRASS_TC_PY <- select(ALL_GRASS_TC_PY,PY,Institution,Papers,Citations)

ALL_GRASS_TC_PY <- ALL_GRASS_TC_PY %>% mutate(Institution = fct_relevel(Institution,"ILRI","Global"))

ALL_GRASS_TC_PY <- ALL_GRASS_TC_PY %>%
     mutate(Period = case_when(
          PY >= 1977 & PY  <=  1980 ~ "1977-80",
          PY >= 1981 & PY  <=  1985 ~ "1981-85",
          PY >= 1986 & PY  <=  1990 ~ "1986-90",
          PY >= 1991 & PY  <=  1994 ~ "1991-94",
          PY >= 1995 & PY  <=  2000 ~ "1995-00",
          PY >= 2001 & PY  <=  2005 ~ "2001-05",
          PY >= 2006 & PY  <=  2010 ~ "2006-10",
          TRUE ~ "2011-18"))

ALL_GRASS_TC_PY$Period <- factor(ALL_GRASS_TC_PY$Period)

ALL_GRASS_TC_PY <- ALL_GRASS_TC_PY %>%
     mutate(Group_of_citations = case_when(
          Citations  <=  20 ~ "1-20",
          Citations >= 21 & Citations  <=  40 ~ "21-40",
          Citations >= 41 & Citations  <=  80 ~ "41-80",
          Citations >= 81 & Citations  <=  200 ~ "81-200",
          Citations >= 201 ~ "> 200"))

ALL_GRASS_TC_PY <- select(ALL_GRASS_TC_PY,PY,Period,Institution,
Group_of_citations,Papers,Citations)

ALL_GRASS_TC_PY_gather <- ALL_GRASS_TC_PY %>% pivot_longer(Papers:Citations,
names_to = "Bibliometric",values_to = "Value")

ALL_GRASS_TC_PY_group <- group_by(ALL_GRASS_TC_PY_gather,Institution,Period,Bibliometric) %>%
     summarise(sumBiblio = sum(Value))

ALL_GRASS_TC_PY_group <- ALL_GRASS_TC_PY_group %>%
     mutate(Bibliometric = fct_relevel(Bibliometric,"Papers","Citations"))


pal_pp_fig_2 <- c(col_ILRI,col_global)
names(pal_pp_fig_2) <- levels(ALL_GRASS_TC_PY$Institution)

pp_ILRI.fig2.papers <- ALL_GRASS_TC_PY_group %>% filter(Bibliometric == "Papers" & Institution == "ILRI") %>%
     group_by(Period) %>% summarise(nPapers = sum(sumBiblio))
(pp_ILRI.fig2.papers.total <- sum(pp_ILRI.fig2.papers$nPapers))

pp_global.fig2.papers <- ALL_GRASS_TC_PY_group %>% filter(Bibliometric == "Papers" & Institution == "Global") %>%
     group_by(Period) %>% summarise(nPapers = sum(sumBiblio,na.rm = TRUE))
(pp_global.fig2.papers.total <- sum(pp_global.fig2.papers$nPapers))

#figure 2 on GRASS------------------------------
(FIGURE_PREFACE_PP_GRASS_2 <- ggplot(data = ALL_GRASS_TC_PY_group) +
     geom_col(aes(x = Period,y = sumBiblio, fill = Institution), position = position_dodge(),size = 1) +
     facet_wrap(~Bibliometric,scales = "free_y",nrow = 2,ncol = 1) +
     labs(x = "Publication period", y = "Counts",title = tles.Fig.PII.2) +
          scale_fill_manual(name = "Institution", values = pal_pp_fig_2) +
     scale_y_continuous(labels = scales::comma) +
     theme_tufte() +
          theme(text = element_text(size = 11,family = "Times"),legend.position = "bottom") +
          labs(caption = paste(c("Source: Constructed by authors from data at www.scopus.com. ILRI sample = ", pp_ILRI.fig2.papers.total," papers; the global sample = ", pp_global.fig2.papers.total," papers."),collapse = "")) +
          theme(plot.caption = element_text(hjust = 0)))

#now Fig I.4 on multidimensional crops
MULTID_ILRI_TC_PY <- read.xlsx("PREFACE_PP_PORTAL.xlsx",sheet = "MULTID_ILRI_TC_PY",colNames = T)
MULTID_ILRI_TC_PY <- filter(MULTID_ILRI_TC_PY,PY >= 1977 & PY  <=  2018)
MULTID_ILRI_TC_PY <- select(MULTID_ILRI_TC_PY,PY,Papers = TC.length,Citations = TC.sum)
MULTID_ILRI_TC_PY$Institution <- as.factor("ILRI")

MULTID_GLOBAL_TC_PY <- read.xlsx("PREFACE_PP_PORTAL.xlsx",sheet = "MULTID_GLOBAL_TC_PY",colNames = T)
MULTID_GLOBAL_TC_PY <- filter(MULTID_GLOBAL_TC_PY,PY >= 1977 & PY  <=  2018)
MULTID_GLOBAL_TC_PY <- select(MULTID_GLOBAL_TC_PY,PY,Papers = TC.length,Citations = TC.sum)
MULTID_GLOBAL_TC_PY$Institution <- as.factor("Global")

ALL_MULTID_TC_PY <- rbind(MULTID_ILRI_TC_PY,MULTID_GLOBAL_TC_PY)
ALL_MULTID_TC_PY <- select(ALL_MULTID_TC_PY,PY,Institution,Papers,Citations)
ALL_MULTID_TC_PY <- ALL_MULTID_TC_PY %>%
     mutate(Institution = fct_relevel(Institution,"ILRI","Global"))

ALL_MULTID_TC_PY <- ALL_MULTID_TC_PY %>%
     mutate(Period = case_when(
          PY >= 1977 & PY  <=  1980 ~ "1977-80",
          PY >= 1981 & PY  <=  1985 ~ "1981-85",
          PY >= 1986 & PY  <=  1990 ~ "1986-90",
          PY >= 1991 & PY  <=  1994 ~ "1991-94",
          PY >= 1995 & PY  <=  2000 ~ "1995-00",
          PY >= 2001 & PY  <=  2005 ~ "2001-05",
          PY >= 2006 & PY  <=  2010 ~ "2006-10",
          TRUE ~ "2011-18"))

ALL_MULTID_TC_PY$Period <- factor(ALL_MULTID_TC_PY$Period)

ALL_MULTID_TC_PY <- ALL_MULTID_TC_PY %>%
     mutate(Group_of_citations = case_when(
          Citations  <=  20 ~ "1-20",
          Citations >= 21 & Citations  <=  40 ~ "21-40",
          Citations >= 41 & Citations  <=  80 ~ "41-80",
          Citations >= 81 & Citations  <=  200 ~ "81-200",
          Citations >= 201 ~ "> 200"))

ALL_MULTID_TC_PY <- select(ALL_MULTID_TC_PY,PY,Period,Institution,
Group_of_citations,Papers,Citations)

ALL_MULTID_TC_PY_gather <- ALL_MULTID_TC_PY %>% pivot_longer(Papers:Citations,
names_to = "Bibliometric",values_to = "Value")

ALL_MULTID_TC_PY_group <- group_by(ALL_MULTID_TC_PY_gather,
Institution,Period,Bibliometric) %>% summarise(sumBiblio = sum(Value))

ALL_MULTID_TC_PY_group <- ALL_MULTID_TC_PY_group %>%
     mutate(Bibliometric = fct_relevel(Bibliometric,"Papers","Citations"))

pal_pp_fig_4 <- c(col_ILRI,col_global)
names(pal_pp_fig_4) <- levels(ALL_MULTID_TC_PY$Institution)

pp_ILRI.fig4.papers <- ALL_MULTID_TC_PY_group %>% filter(Bibliometric == "Papers" & Institution == "ILRI") %>%
     group_by(Period) %>% summarise(nPapers = sum(sumBiblio))
(pp_ILRI.fig4.papers.total <- sum(pp_ILRI.fig4.papers$nPapers))

pp_global.fig4.papers <- ALL_MULTID_TC_PY_group %>% filter(Bibliometric == "Papers" & Institution == "Global") %>%
     group_by(Period) %>% summarise(nPapers = sum(sumBiblio,na.rm = TRUE))
(pp_global.fig4.papers.total <- sum(pp_global.fig4.papers$nPapers))

(FIGURE_PREFACE_PP_MULTID_4 <- ggplot(data = ALL_MULTID_TC_PY_group) +
geom_col(aes(x = Period,y = sumBiblio, fill = Institution),
         position = position_dodge(),size = 1) +
facet_wrap(~Bibliometric,scales = "free_y",nrow = 2,ncol = 1) +
labs(x = "Publication period",y = "Counts",title = tles.Fig.PII.4) +
scale_y_continuous(labels = scales::comma) +
scale_fill_manual(name = "Institution", values = pal_pp_fig_4) +
theme_tufte() +
          theme(text = element_text(size = 11,family = "Times"),legend.position = "bottom") +
          labs(caption = paste(c("Source: Constructed by authors from data at www.scopus.com. ILRI sample = ", pp_ILRI.fig4.papers.total," papers; the global sample = ", pp_global.fig4.papers.total," papers."),collapse = "")) +
          theme(plot.caption = element_text(hjust = 0)))


#now get the top 5% shares of all papers with Citations > 9
#start with FORAGES
PFORAGE_GLOBAL <- read.xlsx("PREFACE_PP_PORTAL.xlsx",sheet = "bib_PFORAGE_GLOBAL",colNames = T)
PFORAGE_GLOBAL <- select(PFORAGE_GLOBAL,TI,SR,PY = PY,Citations = TC)
PFORAGE_ILRI <- read.xlsx("PREFACE_PP_PORTAL.xlsx",sheet = "bib_PFORAGE_ILRI",colNames = T)
PFORAGE_ILRI <- select(PFORAGE_ILRI,TI,SR,PY = PY,Citations = TC)
PFORAGE_GLOBAL$Institution <- factor("Global")
PFORAGE_ILRI$Institution <- factor("ILRI")

ALL_PFORAGE <- rbind(PFORAGE_GLOBAL,PFORAGE_ILRI)
ALL_PFORAGE <- select(ALL_PFORAGE,PY,Citations,Institution)

ALL_PFORAGE <- filter(ALL_PFORAGE, PY >= 1977 & PY  <=  2018 & Citations > 9)

ALL_PFORAGE_pr <- quantile(ALL_PFORAGE$Citations, probs = c(0.25,0.5,0.75,0.95))

ALL_PFORAGE <- ALL_PFORAGE %>%
     mutate(Quants = case_when(
          Citations  <=   ALL_PFORAGE_pr[1] ~ "first",
          Citations  <=   ALL_PFORAGE_pr[2] & Citations >= ALL_PFORAGE_pr[1] ~ "second",
          Citations  <=   ALL_PFORAGE_pr[3] & Citations >= ALL_PFORAGE_pr[2] ~ "third",
          Citations  <=   ALL_PFORAGE_pr[4] & Citations >= ALL_PFORAGE_pr[3] ~ "fourth",
          Citations >   ALL_PFORAGE_pr[4]  ~ "top 5%"))

#now get GRASS
bib_GRASS_GLOBAL <- data.table(read.xlsx("PREFACE_PP_PORTAL.xlsx",sheet = "bib_GRASS_GRLABA",colNames = T))
bib_GRASS_GLOBAL <- select(bib_GRASS_GLOBAL,TI,SR,PY = PY,Citations = TC)
bib_GRASS_GLOBAL$Institution <- factor("Global")

bib_GRASS_ILRI <- data.table(read.xlsx("PREFACE_PP_PORTAL.xlsx",sheet = "bib_GRASS_ILRI",colNames = T))
bib_GRASS_ILRI <- select(bib_GRASS_ILRI,TI,SR,PY = PY,Citations = TC)
bib_GRASS_ILRI$Institution <- factor("ILRI")

ALL_GRASS <- rbind(bib_GRASS_GLOBAL,bib_GRASS_ILRI)
ALL_GRASS <- select(ALL_GRASS,PY,Citations,Institution)

ALL_GRASS <- filter(ALL_GRASS, PY >= 1977 & PY  <=  2018)
ALL_GRASS <- filter(ALL_GRASS, PY >= 1977 & PY  <=  2018 & Citations > 9)


ALL_GRASS_pr <- quantile(ALL_GRASS$Citations, probs = c(0.25,0.5,0.75,0.95))

ALL_GRASS <- ALL_GRASS %>%
     mutate(Quants = case_when(
          Citations  <=   ALL_GRASS_pr[1] ~ "first",
          Citations  <=   ALL_GRASS_pr[2] & Citations >= ALL_GRASS_pr[1] ~ "second",
          Citations  <=   ALL_GRASS_pr[3] & Citations >= ALL_GRASS_pr[2] ~ "third",
          Citations  <=   ALL_GRASS_pr[4] & Citations >= ALL_GRASS_pr[3] ~ "fourth",
          Citations >   ALL_GRASS_pr[4]  ~ "top 5%"))

#MULTID

ALL_MULTID_GLOBAL <- read.xlsx("PREFACE_PP_PORTAL.xlsx",sheet = "bib_MULTID_GLOBAL",colNames = T)
ALL_MULTID_GLOBAL <- ALL_MULTID_GLOBAL %>% select(TI,SR,PY,Citations = TC)
ALL_MULTID_GLOBAL$Domain <- factor("Multidimensional")
ALL_MULTID_GLOBAL$Institution <- factor("Global")

ALL_MULTID_ILRI <- read.xlsx("PREFACE_PP_PORTAL.xlsx",sheet = "bib_MULTID_ILRI",colNames = T)
ALL_MULTID_ILRI <- ALL_MULTID_ILRI %>% select(TI,SR,PY,Citations = TC)
ALL_MULTID_ILRI$Domain <- factor("Multidimensional")
ALL_MULTID_ILRI$Institution <- factor("ILRI")

ALL_MULTID <- rbind(ALL_MULTID_ILRI,ALL_MULTID_GLOBAL)
ALL_MULTID <- select(ALL_MULTID,PY,Citations,Institution)
ALL_MULTID <- filter(ALL_MULTID, PY >= 1977 & PY <= 2018)

ALL_MULTID <- filter(ALL_MULTID,PY >= 1977 & PY  <=  2018 & Citations > 9)

ALL_MULTID_pr <- quantile(ALL_MULTID$Citations, probs = c(0.25,0.5,0.75,0.95))
ALL_MULTID <- ALL_MULTID %>%
     mutate(Quants = case_when(
          Citations  <=   ALL_MULTID_pr[1] ~ "first",
          Citations  <=   ALL_MULTID_pr[2] & Citations >= ALL_MULTID_pr[1] ~ "second",
          Citations  <=   ALL_MULTID_pr[3] & Citations >= ALL_MULTID_pr[2] ~ "third",
          Citations  <=   ALL_MULTID_pr[4] & Citations >= ALL_MULTID_pr[3] ~ "fourth",
          Citations  >  ALL_MULTID_pr[4] ~ "top 5%"))

ALL_MULTID$Domain <- factor("Multidimensional")
ALL_PFORAGE$Domain <- factor("Planted forages")

ALL_PFORAGE_MULTID <- rbind(ALL_PFORAGE,ALL_MULTID)

ALL_PFORAGE_MULTID <- filter(ALL_PFORAGE_MULTID,PY >= 1977 & PY  <=  2018 & Citations > 9)

ALL_PFORAGE_MULTID_pr <- quantile(ALL_PFORAGE_MULTID$Citations, probs = c(0.25,0.5,0.75,0.95))
ALL_PFORAGE_MULTID <- ALL_PFORAGE_MULTID %>%
     mutate(Quants = case_when(
          Citations  <=   ALL_PFORAGE_MULTID_pr[1] ~ "first",
          Citations  <=   ALL_PFORAGE_MULTID_pr[2] & Citations >= ALL_PFORAGE_MULTID_pr[1] ~ "second",
          Citations  <=   ALL_PFORAGE_MULTID_pr[3] & Citations >= ALL_PFORAGE_MULTID_pr[2] ~ "third",
          Citations  <=   ALL_PFORAGE_MULTID_pr[4] & Citations >= ALL_PFORAGE_MULTID_pr[3] ~ "fourth",
          Citations  >  ALL_PFORAGE_MULTID_pr[4] ~ "top 5%"))

#do the TOP5% citations
ALL_GRASS$Domain <- factor("Rangeland")
ALL_PFORAGE$Domain <- factor("Planted forages")
ALL_MULTID$Domain <- factor("All multidimensional crops")
#ALL_PFORAGE_MULTID$Domain <- factor("Feed and forage crops")

ALL_PP_TOP5 <- rbind(ALL_GRASS,ALL_PFORAGE,ALL_MULTID) #ALL_PFORAGE_MULTID)
ALL_PP_TOP5$Quants <- factor(ALL_PP_TOP5$Quants)
ALL_PP_TOP5$Institution <- factor(ALL_PP_TOP5$Institution)
ALL_PP_TOP5 <- ALL_PP_TOP5 %>%
     mutate(Institution = fct_relevel(Institution,"ILRI","Global"))

ALL_PP_TOP5 <- ALL_PP_TOP5 %>%
 mutate(Quants = fct_relevel(Quants,"first","second","third","fourth","top 5%"))

ALL_PP_TOP5 <- ALL_PP_TOP5 %>%
     mutate(Period = case_when(
          PY >= 1977 & PY  <=  1980 ~ "1977-80",
          PY >= 1981 & PY  <=  1985 ~ "1981-85",
          PY >= 1986 & PY  <=  1990 ~ "1986-90",
          PY >= 1991 & PY  <=  1994 ~ "1991-94",
          PY >= 1995 & PY  <=  2000 ~ "1995-00",
          PY >= 2001 & PY  <=  2005 ~ "2001-05",
          PY >= 2006 & PY  <=  2010 ~ "2006-10",
          TRUE ~ "2011-18"))

ALL_PP_TOP5$Period <- factor(ALL_PP_TOP5$Period)

ALL_PP_TOP5_group <- group_by(ALL_PP_TOP5,Institution,Domain,Period,Quants) %>%
     summarise(nPapers = length(Citations),nCitations = sum(Citations))

pal_pp_fig_5 <- c(col_ILRI,col_global)
names(pal_pp_fig_5) <- levels(ALL_PP_TOP5_group$Institution)


pp_ILRI.fig5.gs.ps <- ALL_PP_TOP5_group %>% filter(Institution == "ILRI" & Domain == "Rangeland") %>%
     group_by(Period) %>% summarise(nPapers = sum(nPapers))
(pp_ILRI.fig5.gs.ps.tot <- sum(pp_ILRI.fig5.gs.ps$nPapers))

pp_ILRI.fig5.pf.ps <- ALL_PP_TOP5_group %>% filter(Institution == "ILRI" & Domain == "Planted forages") %>%
     group_by(Period) %>% summarise(nPapers = sum(nPapers))
(pp_ILRI.fig5.pf.ps.tot <- sum(pp_ILRI.fig5.pf.ps$nPapers))

pp_ILRI.fig5.mc.ps <- ALL_PP_TOP5_group %>% filter(Institution == "ILRI" & Domain == "All multidimensional crops") %>%
     group_by(Period) %>% summarise(nPapers = sum(nPapers))
(pp_ILRI.fig5.mc.ps.tot <- sum(pp_ILRI.fig5.mc.ps$nPapers))

# pp_ILRI.fig5.ff.ps <- ALL_PP_TOP5_group %>% filter(Institution == "ILRI" & Domain == "Feed and forage crops") %>%
#      group_by(Period) %>% summarise(nPapers = sum(nPapers))
# (pp_ILRI.fig5.ff.ps.tot <- sum(pp_ILRI.fig5.ff.ps$nPapers))

pp_Glo.fig5.gs.ps <- ALL_PP_TOP5_group %>% filter(Institution == "Global" & Domain == "Rangeland") %>%
     group_by(Period) %>% summarise(nPapers = sum(nPapers))
(pp_Glo.fig5.gs.ps.tot <- sum(pp_Glo.fig5.gs.ps$nPapers))

pp_Glo.fig5.pf.ps <- ALL_PP_TOP5_group %>% filter(Institution == "Global" & Domain == "Planted forages") %>%
     group_by(Period) %>% summarise(nPapers = sum(nPapers))
(pp_Glo.fig5.pf.ps.tot <- sum(pp_Glo.fig5.pf.ps$nPapers))

pp_Glo.fig5.mc.ps <- ALL_PP_TOP5_group %>% filter(Institution == "Global" & Domain == "All multidimensional crops") %>%
     group_by(Period) %>% summarise(nPapers = sum(nPapers))
(pp_Glo.fig5.mc.ps.tot <- sum(pp_Glo.fig5.mc.ps$nPapers))

# pp_Glo.fig5.ff.ps <- ALL_PP_TOP5_group %>% filter(Institution == "Global" & Domain == "Feed and forage crops") %>%
#      group_by(Period) %>% summarise(nPapers = sum(nPapers))
# (pp_Glo.fig5.ff.ps.tot <- sum(pp_Glo.fig5.ff.ps$nPapers))

(FIGURE_PREFACE_PP_TOP5_5 <- ggplot(data = ALL_PP_TOP5_group) +
 geom_col(mapping = aes(x = Quants,y  = nPapers, fill = Institution), position = "dodge", size = 1) +
 xlab("Quantiles of merged ILRI and global institutions samples") +
 labs(y = "Counts of papers > 9 citations",title = tles.Fig.PII.5) +
 scale_fill_manual(name = "Institution", values = pal_pp_fig_5) +
 facet_wrap(~Domain,scales = "free_y",nrow = 1,ncol = 3) +
 scale_y_continuous(labels = scales::comma) +
 theme_tufte() +
 theme(text = element_text(size = 11,family = "Times"), legend.position = "bottom") +
 labs(caption = paste(c("Source: Constructed by authors from data at www.scopus.com, for papers published from 1977-2018 with > 9 citations.",
 "\nILRI (Global) rangeland papers, n = ", pp_ILRI.fig5.gs.ps.tot," (",pp_Glo.fig5.gs.ps.tot,"); ",
 "ILRI (Global) planted forages papers, n = ",pp_ILRI.fig5.pf.ps.tot," (",pp_Glo.fig5.pf.ps.tot,"); ",
 "\nILRI (Global) multidimensional crop papers, n = ", pp_ILRI.fig5.mc.ps.tot, " (",pp_Glo.fig5.mc.ps.tot,")."),collapse = "")) +
 theme(plot.caption = element_text(hjust = 0)))


#print all the tibbles -----------------------------
#
options(tibble.print_max = Inf)

#--------------------- RELATED TO FIGURE 1
#ILCA.ILRAD spend
(tibble_ILCA.ILRAD.spend.period <- PrefPPiom_df_spend_gather %>% filter(Year  <=  1994)
%>% group_by(Period,Domain)
%>% summarise(sumSpend = sum(Spending)))

tibble_ILCA.ILRAD.spend.domain <- PrefPPiom_df_spend_gather %>% filter(Year  <=  1994) %>% group_by(Domain) %>% summarise(domainSpend = sum(Spending) / 1000)

for (i in 1:length(tibble_ILCA.ILRAD.spend.domain$Domain)) {
     tibble_ILCA.ILRAD.spend.domain[i,"spendShare"] <- round(100*(tibble_ILCA.ILRAD.spend.domain[i,2] /
          tibble_ILCA.ILRAD.spend.domain[7,2]),digits = 1)}

(tibble_ILCA.ILRAD.spend.domain)

#ILRI spend
(tibble_ILRI.spend.period <- PrefPPiom_df_spend_gather %>%
     group_by(Period,Domain) %>% summarise(sumSpend = sum(Spending)))

tibble_ILRI.spend.domain <- PrefPPiom_df_spend_gather %>% group_by(Domain) %>% summarise(domainSpend = sum(Spending) / 1000)

for (i in 1:length(tibble_ILRI.spend.domain$Domain)) {
     tibble_ILRI.spend.domain[i,"spendShare"] <- round(100*(tibble_ILRI.spend.domain[i,2] /
   tibble_ILRI.spend.domain[7,2]),digits = 1)}

(tibble_ILRI.spend.domain)



#ILCA and ILRAD papers and citations
plant_biomass_ILRI <- data.table(read.xlsx("PREFACE_PP_PORTAL.xlsx",sheet = "bib_PLANT_BIOMASS_ILRI",colNames = T))
plant_biomass_ILRI <- filter(plant_biomass_ILRI,PY >= 1975 & PY <= 2018)

plant_biomass_ILCA_ILRAD <- filter(plant_biomass_ILRI, PY >= 1975 & PY <= 1994)

(tibble_ILCA.ILRAD.papers.cites <- plant_biomass_ILCA_ILRAD %>%
     summarise(nPapers = length(TC),nCitations = sum(TC),
               meanCitations = round(mean(TC),digits = 1),
               medianCitations = round(median(TC),digits = 1)))

(tibble_ILCA.ILRAD.spend.paper <- tibble_ILCA.ILRAD.papers.cites[1] / tibble_ILCA.ILRAD.spend.domain[6,2])

(tibble_ILCA.ILRAD.return.cites <- tibble_ILCA.ILRAD.papers.cites[2] /
          tibble_ILCA.ILRAD.spend.domain[6,2])

#ILRI Primary production papers and citations

(tibble_ILRI.papers.cites <- plant_biomass_ILRI %>%
          summarise(nPapers = length(TC),nCitations = sum(TC),
                    meanCitations = round(mean(TC),digits = 1),
                    medianCitations = round(median(TC),digits = 1)))

(tibble_ILRI.spend.paper <- tibble_ILRI.papers.cites[1] / tibble_ILRI.spend.domain[6,2])

(tibble_ILRI.return.cites <- tibble_ILRI.papers.cites[2] / tibble_ILRI.spend.domain[6,2])


# ILCA.ILRAD planted forages, more tibbles
#RELATED TO FIGURE 3 (ALL_PFORAGE_TC_PY_group)

tibble_ILRI.pforage <- data.table(read.xlsx("PREFACE_PP_PORTAL.xlsx",sheet = "bib_PFORAGE_ILRI",colNames = T))
tibble_ILRI.pforage <- filter(tibble_ILRI.pforage,PY >= 1975 & PY <= 2018)
tibble_ILCA.ILRAD.pforage <- filter(tibble_ILRI.pforage,PY >= 1975 & PY <= 1994)

(tibble_ILCA.ILRAD.pforage.papers.cites <- tibble_ILCA.ILRAD.pforage %>%
          summarise(nPapers = length(TC),nCitations = sum(TC),
                    meanCitations = round(mean(TC),digits = 1),
                    medianCitations = round(median(TC),digits = 1)))

(tibble_ILRI.pforage.papers.cites <- tibble_ILRI.pforage %>%
          summarise(nPapers = length(TC),nCitations = sum(TC),
                    meanCitations = round(mean(TC),digits = 1),
                    medianCitations = round(median(TC),digits = 1)))

(tibble_ILRI.pforage.spend.domain <- PrefPPiom_df_spend_gather %>% group_by(Domain)
     %>% summarise(domainSpend = sum(Spending) / 1000))

for (i in 1:length(tibble_ILRI.spend.domain$Domain)) {
tibble_ILRI.spend.domain[i,"spendShare"] <- round(100*(tibble_ILRI.spend.domain[i,2] /
tibble_ILRI.spend.domain[7,2]),digits = 1)}
(tibble_ILCA.ILRAD.spend.domain)

(tibble_ILRI.pforage.spend.paper <- tibble_ILRI.pforage.papers.cites[1] / tibble_ILRI.spend.domain[6,2])

(tibble_ILRI.pforage.spend.cites <- tibble_ILRI.pforage.papers.cites[2] / tibble_ILRI.spend.domain[6,2])


#top 5 % analysis for planted forages
#
(pforage_tibble <- group_by(ALL_PFORAGE,Institution,Quants) %>%
          summarise(meanTCByInst = mean(Citations),sumTCByInst = sum(Citations),
                    NTCByInst = length(Citations)))
(ILRI.pforage.papers.share.total <- round(100*sum(pforage_tibble$NTCByInst[6:10]) /
          sum(pforage_tibble$NTCByInst[1:10]),digits = 1))
(ILRI.pforage.cites.share.total <- round(100*sum(pforage_tibble$sumTCByInst[6:10]) /
sum(pforage_tibble$sumTCByInst[1:10]),digits = 1))

(ILRI.pforage.papers.share.top5 <- round(100*sum(pforage_tibble$NTCByInst[10]) /
          sum(pforage_tibble$NTCByInst[5],pforage_tibble$NTCByInst[10]),digits = 1))

#------------------------------------------------------
#ILCA.ILRAD rangelands, more tibbles related to FIGURE 2 -----------------
tibble_ILRI.grass <- data.table(read.xlsx("PREFACE_PP_PORTAL.xlsx",sheet = "bib_GRASS_ILRI",colNames = T))
tibble_ILRI.grass <- filter(tibble_ILRI.grass,PY >= 1975 & PY <= 2018)

tibble_ILCA.ILRAD.grass <- filter(tibble_ILRI.grass,PY >= 1975 & PY <= 1994)
nrow(tibble_ILCA.ILRAD.grass)

(tibble_ILCA.ILRAD.grass.papers.cites <- tibble_ILCA.ILRAD.grass %>%
 summarise(nPapers = length(TC),nCitations = sum(TC),
 meanCitations = round(mean(TC),digits = 1),
 medianCitations = round(median(TC),digits = 1)))


(tibble_ILRI.grass.papers.cites <- tibble_ILRI.grass %>%
 summarise(nPapers = length(TC),nCitations = sum(TC),
 meanCitations = round(mean(TC),digits = 1),medianCitations = round(median(TC),digits = 1)))

#ILRI climate with rangelands/grasslands
tibble_ILRI.clim.range.grass <- data.table(read.xlsx("PREFACE_PP_PORTAL.xlsx",sheet = "bib_CLIM_RANGE_GRASS_ILRI",colNames = T))
tibble_ILRI.clim.range.grass <- filter(tibble_ILRI.clim.range.grass,PY >= 1975 & PY <= 2018)

(tibble_ILRI.clim.range.grass.papers.cites <- tibble_ILRI.clim.range.grass %>%
          summarise(nPapers = length(TC),nCitations = sum(TC),
                    meanCitations = round(mean(TC),digits = 1),medianCitations = round(median(TC),digits = 1)))

#top 5 % for rangelands grasslands
(grass_tibble <- group_by(ALL_GRASS,Institution,Quants) %>%
          summarise(meanByInst = mean(Citations),
                    sumByInst = sum(Citations),
                    NByInst = length(Citations)))
(ILRI.grass.papers.share.total <- round(100*sum(grass_tibble$NByInst[6:10]) /
          sum(grass_tibble$NByInst[1:10]),digits = 1))

(ILRI.grass.papers.share.top5 <- round(100*sum(grass_tibble$NByInst[10]) /
          sum(grass_tibble$NByInst[5],grass_tibble$NByInst[10]),digits = 1))

(ILRI.grass.cites.share.top5 <- round(100*sum(grass_tibble$sumByInst[10]) /
sum(grass_tibble$sumByInst[5],grass_tibble$sumByInst[10]),digits = 1))

#ILRI multidimensional crops, more tibbles
#RELATED TO FIGURE 4
#filter and create the files

tibble_ILRI.multid <- data.table(read.xlsx("PREFACE_PP_PORTAL.xlsx",sheet = "bib_MULTID_ILRI",colNames = T))

tibble_ILRI.multid <- filter(tibble_ILRI.multid, PY >= 1975 & PY <= 2018)
tibble_ILCA.ILRAD.multid <- filter(tibble_ILRI.multid, PY >= 1975 & PY <= 1994)
tibble_ALL_MULTID <- filter(ALL_MULTID,PY >= 1975 & PY <= 2018)
tibble_ALL_MULTID.1994 <- filter(ALL_MULTID,PY >= 1975 & PY <= 1994)

# ILCA / ILRAD
(tibble_ILCA.ILRAD.multid.papers.cites <- tibble_ILCA.ILRAD.multid %>%
          summarise(nPapers = length(TC),nCitations = sum(TC),
                    meanCitations = round(mean(TC),digits = 1),medianCitations = round(median(TC),digits = 1)))

(tibble_ALL_MULTID.1994 <- group_by(tibble_ALL_MULTID.1994,Institution,Quants) %>%
 summarise(nPapers = length(Citations),nCitations = sum(Citations),
 meanCitations = mean(Citations),medianCitations = median(Citations)))

(ILCA.ILRAD.multid.papers.share.total <- round(100*sum(tibble_ALL_MULTID.1994$nPapers[1:5]) /
sum(tibble_ALL_MULTID.1994$nPapers[1:10]),digits = 1))

(ILCA.ILRAD.multid.cites.share.total <- round(100*sum(tibble_ALL_MULTID.1994$nCitations[1:5]) /
 sum(tibble_ALL_MULTID.1994$nCitations[1:10]),digits = 1))

(ILCA.ILRAD.multid.papers.share.top5 <- round(100*sum(tibble_ALL_MULTID.1994$nPapers[5]) /
sum(tibble_ALL_MULTID.1994$nPapers[10],tibble_ALL_MULTID.1994$nPapers[5]),digits = 1))


# ILRI
#
AHG_Spend_R <- read.xlsx("FINANCIAL_PORTAL.xlsx",sheet = "Spend_Cites_R",
     startRow = 1,colNames = T,na.strings = "NA")

(tibble_ILRI.multid.papers.cites <- tibble_ILRI.multid %>%
          summarise(nPapers = length(TC),nCitations = sum(TC),meanCitations = round(mean(TC),digits = 1),
          medianCitations = round(median(TC),digits = 1)))

(ILRI.papers.total <- sum(AHG_Spend_R$ILRI_ALL_PAPERS))
(ILRI.cites.total <- sum(AHG_Spend_R$ILRI_ALL_TC))

(ILRI.multid.papers.share.ILRI.total <- round(100*(tibble_ILRI.multid.papers.cites[1] /ILRI.papers.total),digits = 1))
(ILRI.multid.papers.share.ILRI.total <- round(100*(tibble_ILRI.multid.papers.cites[2] /ILRI.cites.total),digits = 1))

(tibble_ALL_MULTID <- group_by(tibble_ALL_MULTID,Institution,Quants) %>%
     summarise(nPapers = length(Citations),nCitations = sum(Citations),
     meanCitations = mean(Citations),medianCitations = median(Citations)))

(ILRI.multid.papers.share.total <- round(100*sum(tibble_ALL_MULTID$nPapers[1:5]) /
sum(tibble_ALL_MULTID$nPapers[1:10]),digits = 1))

(ILRI.multid.cites.share.total <- round(100*sum(tibble_ALL_MULTID$nCitations[1:5]) /
sum(tibble_ALL_MULTID$nCitations[1:10]),digits = 1))

(ILRI.multid.papers.share.top5 <- round(100*sum(tibble_ALL_MULTID$nPapers[5]) /
sum(tibble_ALL_MULTID$nPapers[5],tibble_ALL_MULTID$nPapers[10]),digits = 1))

(ILRI.multid.cites.share.top5 <- round(100*sum(tibble_ALL_MULTID$nCitations[5]) /
sum(tibble_ALL_MULTID$nCitations[5],tibble_ALL_MULTID$nCitations[10]),digits = 1))


#top 5 % analysis for multidimensional crops


#ILRI Primary production crops, more tibbles
#ALL_PP_TOP5 <- rbind(ALL_GRASS,ALL_PFORAGE,ALL_MULTID,ALL_PFORAGE_MULTID)
#RELATED TO FIGURE 6 (TOP 5 group)
tibble_ALL_PP.top5 <- ungroup(ALL_PP_TOP5_group)
tibble_ALL_PP.top5 <- filter(tibble_ALL_PP.top5) #Quants == "top 5%")

(tibble_ALL_PP.top5 <- group_by(ALL_PP_TOP5,Institution,Domain,Quants) %>%
  summarise(nPapersT5 = length(Citations),nCitesT5 = sum(Citations)))

(filter(tibble_ALL_PP.top5, Quants == "top 5%"))


# following code is commented out
# if you want to export the figures to tiff files then uncomment
#


# ggarrange(FIGURE_PREFACE_PP_1, ncol = 1,nrow = 1,align = "hv") %>%
#      ggexport(filename = paste(c(tles.Fig.PII.1,".tiff"),collapse = ""))
#
# ggarrange(FIGURE_PREFACE_PP_GRASS_2, ncol = 1,nrow = 1,align = "hv") %>%
#      ggexport(filename = paste(c(tles.Fig.PII.2,".tiff"),collapse = ""))
#
# ggarrange(FIGURE_PREFACE_PP_FORAGE_3, ncol = 1,nrow = 1,align = "hv") %>%
#      ggexport(filename = paste(c(tles.Fig.PII.3,".tiff"),collapse = ""))
#
# ggarrange(FIGURE_PREFACE_PP_MULTID_4, ncol = 1,nrow = 1,align = "hv") %>%
#      ggexport(filename = paste(c(tles.Fig.PII.4,".tiff"),collapse = ""))
#
# ggarrange(FIGURE_PREFACE_PP_TOP5_5, ncol = 1,nrow = 1,align = "hv") %>%
#      ggexport(filename = paste(c(tles.Fig.PII.5,".tiff"),collapse = ""))

#make a list of figure titles
Fig.titles.PII <- as.character(mget(ls(pattern = "tles.Fig.PII.")))

# following code is commented out
# if you want to print the figure titles then uncomment
#

# capture.output(print(as.character(Fig.titles.PII),
#                      print.gap = 1,quote = FALSE),file = "Preface II, Figure titles")

