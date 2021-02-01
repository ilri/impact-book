#code to generate figures for the preface on livestock systems
#
#NOTA BENE: some of the labels and notes to the Figures may differ
#slightly from what is in the book because of formatting changes made during printing
#
#
#setwd("C:/Users/Jmm19/Documents/______ILRI/_______________DATA_PORTAL")
#
#
library(dplyr)
library(tidyr)
library(tidyselect)
library(data.table)
library(doBy)
library(gdata)
library(ggplot2)
library(stringi)
library(stringr)
library(openxlsx)

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
tles.Fig.PIII.1 <- c("Fig.PIII.1 ILRI economics and policy research has been a modest  share of total ILRI spending, 1975-2018")
tles.Fig.PIII.2 <- c("Fig.PIII.2 ILRI papers are widely cited in livestock systems research in Africa and globally, 1977-2018")
tles.Fig.PIII.3 <- c("Fig.PIII.3 ILRI papers are widely cited in economics and policy research on Africa and global problems related to livestock systems, 1977-2018")
tles.Fig.PIII.4 <- c("Fig.PIII.4 ILRI and other publications in climate change research related to livestock systems have become widely cited since 2000, 1977-2018")
tles.Fig.PIII.5a <- c("Fig.PIII.5a Frequency of papers of ILRI and other institutions in global systems research, 1977-2018")
tles.Fig.PIII.5b <- c("Fig.PIII.5b Frequency of papers of ILRI and other institutions in African systems research, 1977-2018")

#label the colors---------------------------------
#
col_theil <- c("#E6E600FF")
col_tryps <- c("#ECB176FF")
col_total_ILRAD <- c("#682622DD")
col_management_and_other <- c("#B6DB00FF")
col_animprod_health_gen <- c("#ECB176FF")

col_ILRI <- c("#682622DD")
col_global <- c("#0000FFFF")
col_Africa <- c("#1DB000FF")

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


#get the bibliographics data
#
bib_final_clean <- read.xlsx("BIBLIO_PORTAL.xlsx",sheet = "BIBLIO",colNames = T)

bib_final_clean <- data.table(bib_final_clean)

PrefSys <- read.xlsx("FINANCIAL_PORTAL.xlsx",sheet = "Spend_Cites_R")
PrefSys <- data.table(filter(PrefSys, YEAR > 1974 & YEAR < 2019))
PrefSys_df <- select(PrefSys,Year = YEAR,

        `Livestock systems` = GROUP_SYSTEMS_SPEND,
        `Economics and policy` = GROUP_ECON_POLICY_SPEND,
        `Plant sciences`  = GROUP_PLANT_BIOMASS_SPEND,
        `Animal health and genetics` 	 =  GROUP_ANIM_SCI_SPEND,
        `Capacity development` 	 =  GROUP_CAP_DEV_SPEND,
        `Management and technical` = GROUP_OTHER_SPEND,
        `Total spending` = LIFETIME_TOTAL_SPEND,

        `ILRI all papers` = ILRI_ALL_PAPERS,
        `ILRI all citations` = ILRI_ALL_TC,

        `ILRI livestock systems papers` = ILCA_ILRI_FSR_PAPERS,
        `ILRI livestock systems citations` = ILCA_ILRI_FSR_TC,
        `Global livestock systems papers` = FSR_GLOBAL_PAPERS,
        `Global livestock systems citations` = FSR_GLOBAL_TC,
        `Africa livestock systems papers` = FSR_AFRICA_PAPERS,
        `Africa livestock systems citations` = FSR_AFRICA_TC,

      `ILRI policy and economics papers` = ECON_POLICY_ILRI_PAPERS,
      `ILRI policy and economics citations` = ECON_POLICY_ILRI_TC,
      `Global policy and economics papers` = ECON_POLICY_GLOBAL_PAPERS,
      `Global policy and economics citations` = ECON_POLICY_GLOBAL_TC,
      `Africa policy and economics papers` = ECON_POLICY_AFRICA_PAPERS,
      `Africa policy and economics citations` = ECON_POLICY_AFRICA_TC,

      `ILRI climate papers` = ILCA_ILRI_CLIMATE_PAPERS,
      `ILRI climate citations` = ILCA_ILRI_CLIMATE_TC,
      `ILRI climate papers within grasslands` = ILRI_CLIM_IN_GRASS_PAPERS,
      `ILRI climate citations within grasslands` = ILRI_CLIM_IN_GRASS_TC,

`Global climate papers` = CLIM_GLOBAL_PAPERS,
`Global climate citations` = CLIM_GLOBAL_TC,
`Africa climate papers` = CLIM_AFRICA_PAPERS,
`Africa climate citations` = CLIM_AFRICA_TC)

#FIGURE 1 created below

PrefSys_df_spend <- select(PrefSys_df,Year,`Livestock systems`:`Total spending`)

PrefSys_df_spend <- data.table(PrefSys_df_spend %>% mutate(Period = case_when(
     Year < 1981 ~ "1975-80",
     Year >=  1981 & Year <=  1985 ~ "1981-85",
     Year >=  1986 & Year <=  1990 ~ "1986-90",
     Year >=  1991 & Year <=  1994 ~ "1991-94",
     Year >=  1995 & Year <=  2000 ~ "1995-00",
     Year >=  2001 & Year <=  2005 ~ "2001-05",
     Year >=  2006 & Year <=  2010 ~ "2006-10",
     TRUE ~ "2011-18")))
PrefSys_df_spend$Period <- factor(PrefSys_df_spend$Period)

PrefSys_df_spend$`Management, technical, capacity development` <-
 (PrefSys_df_spend$`Management and technical` + PrefSys_df_spend$`Capacity development`)

PrefSys_df_spend <- PrefSys_df_spend %>% select(Year,Period,`Animal health and genetics`,`Plant sciences`,
 `Management, technical, capacity development`,`Economics and policy`,
 `Livestock systems`, `Total spending`)

PrefSys_df_spend_gather <- pivot_longer(PrefSys_df_spend,
 `Animal health and genetics`:`Total spending`,names_to = "Domain",values_to = "Spending")

PrefSys_df_spend_gather$Domain <- factor(PrefSys_df_spend_gather$Domain)

PrefSys_df_spend_gather <- PrefSys_df_spend_gather %>%
     mutate(Domain = fct_relevel(Domain,
"Livestock systems", "Economics and policy","Animal health and genetics",
"Plant sciences","Management, technical, capacity development","Total spending"))

PrefSys_df_spend_group <- group_by(PrefSys_df_spend_gather,Period,Domain) %>% summarise(
     sumDomain = sum(Spending,na.rm = TRUE)/1000)

pal_sys_fig_1 <- c(col_live_sys,col_econ_policy,col_animprod_health_gen,
                   col_feed_forage,col_management_and_other,col_ILRI)

#
#FIGURE 2 CREATED BELOW
#
FSR_ILRI <- read.xlsx("PREFACE_LSR_PORTAL.xlsx",sheet = "bib_FSR_ILRI",colNames = T)
FSR_ILRI <- filter(FSR_ILRI,PY >=  1977 & PY <=  2018)
FSR_ILRI <- select(FSR_ILRI,TI,SR,PY,TC)
FSR_ILRI$Institution <- as.factor("ILRI")

FSR_AFRICA <- read.xlsx("PREFACE_LSR_PORTAL.xlsx",sheet = "bib_FSR_AFRICA",colNames = T)
FSR_AFRICA <- filter(FSR_AFRICA,PY >=  1977 & PY <=  2018)
FSR_AFRICA <- select(FSR_AFRICA,TI,SR,PY,TC)
FSR_AFRICA$Institution <- as.factor("Africa")

FSR_GLOBAL <- read.xlsx("PREFACE_LSR_PORTAL.xlsx",sheet = "bib_FSR_GLOBAL",colNames = T)
FSR_GLOBAL <- filter(FSR_GLOBAL,PY >=  1977 & PY <=  2018)
FSR_GLOBAL <- select(FSR_GLOBAL,TI,SR,PY,TC)
FSR_GLOBAL$Institution <- as.factor("Global")

ALL_FSR <- rbind(FSR_AFRICA,FSR_ILRI,FSR_GLOBAL)

ALL_FSR <- select(ALL_FSR,PY,Institution,TC)
ALL_FSR_TC_PY <- data.frame(summaryBy(TC ~ PY + Institution,
 data = ALL_FSR, FUN = c(length,sum)))

ALL_FSR_TC_PY <- ALL_FSR_TC_PY %>%
  mutate(Period = case_when(
    PY >=  1977 & PY <=  1985 ~ "1977-85",
    PY >=  1986 & PY <=  1990 ~ "1986-90",
    PY >=  1991 & PY <=  1994 ~ "1991-94",
    PY >=  1995 & PY <=  2000 ~ "1995-00",
    PY >=  2001 & PY <=  2005 ~ "2001-05",
    PY >=  2006 & PY <=  2010 ~ "2006-10",
    TRUE ~ "2011-18"))

ALL_FSR_TC_PY <- ALL_FSR_TC_PY %>%
     mutate(Era = case_when(
          PY >=  1995 ~ "after merger into ILRI",
          TRUE ~ "before merger into ILRI"))

ALL_FSR_TC_PY <- ALL_FSR_TC_PY %>%
     mutate(Era = fct_relevel(Era,"before merger into ILRI","after merger into ILRI"))

ALL_FSR_TC_PY$Period <- factor(ALL_FSR_TC_PY$Period)
ALL_FSR_TC_PY$PY <- factor(ALL_FSR_TC_PY$PY)
ALL_FSR_TC_PY$Era <- factor(ALL_FSR_TC_PY$Era)

ALL_FSR_TC_PY <- select(ALL_FSR_TC_PY,PY,Era,Period,
 Institution,Papers = TC.length, Citations = TC.sum)

ALL_FSR_TC_PY_gather <- ALL_FSR_TC_PY %>% pivot_longer(Papers:Citations,
names_to = "Bibliometric",values_to = "Value")

ALL_FSR_TC_PY_gather <- ALL_FSR_TC_PY_gather %>%
     mutate(Institution = fct_relevel(Institution,"ILRI","Africa","Global"))

ALL_FSR_TC_PY_gather <- ALL_FSR_TC_PY_gather %>%
     mutate(Bibliometric = fct_relevel(Bibliometric,"Papers","Citations"))

(ALL_FSR_TC_PY_group <- group_by(ALL_FSR_TC_PY_gather,
Era,Institution,Period,Bibliometric) %>% summarise(sumValue = sum(Value)))

pal_sys_fig_2 <- c(col_ILRI,col_Africa,col_global)
names(pal_sys_fig_2) <- c("ILRI","Africa","Global")

tles.FSR <- list(unique(ALL_FSR_TC_PY_group$Bibliometric),
                 unique(ALL_FSR_TC_PY_group$Era))


#
#ECONOMICS AND POLICY
#
ECON_POLICY_ILRI <- read.xlsx("PREFACE_LSR_PORTAL.xlsx",sheet = "bib_ECON_POLICY_ILRI",colNames = T)
ECON_POLICY_ILRI <- filter(ECON_POLICY_ILRI,PY >=  1977 & PY <=  2018)
ECON_POLICY_ILRI <- select(ECON_POLICY_ILRI,TI,SR,PY,TC)
ECON_POLICY_ILRI$Institution <- as.factor("ILRI")

ECON_POLICY_AFRICA <- read.xlsx("PREFACE_LSR_PORTAL.xlsx",sheet = "bib_ECON_POLICY_AFRICA",colNames = T)
ECON_POLICY_AFRICA <- filter(ECON_POLICY_AFRICA,PY >=  1977 & PY <=  2018)
ECON_POLICY_AFRICA <- select(ECON_POLICY_AFRICA,TI,SR,PY,TC)
ECON_POLICY_AFRICA$Institution <- as.factor("Africa")

ECON_POLICY_GLOBAL <- read.xlsx("PREFACE_LSR_PORTAL.xlsx",sheet = "bib_ECON_POLICY_GLOBAL",colNames = T)
ECON_POLICY_GLOBAL <- filter(ECON_POLICY_GLOBAL,PY >=  1977 & PY <=  2018)
ECON_POLICY_GLOBAL <- select(ECON_POLICY_GLOBAL,TI,SR,PY,TC)
ECON_POLICY_GLOBAL$Institution <- as.factor("Global")

ALL_ECON_POLICY <- rbind(ECON_POLICY_AFRICA,ECON_POLICY_ILRI,ECON_POLICY_GLOBAL)
ALL_ECON_POLICY$PY <- as.integer(ALL_ECON_POLICY$PY)
ALL_ECON_POLICY$TC <- as.integer(ALL_ECON_POLICY$TC)

ALL_ECON_POLICY <- select(ALL_ECON_POLICY,PY,Institution,TC)
ALL_ECON_POLICY_TC_PY <- data.frame(summaryBy(TC ~ PY + Institution, data = ALL_ECON_POLICY, FUN = c(length,sum)))


ALL_ECON_POLICY_TC_PY <- ALL_ECON_POLICY_TC_PY %>%
  mutate(Period = case_when(
    PY >=  1977 & PY <=  1985 ~ "1977-85",
    PY >=  1986 & PY <=  1990 ~ "1986-90",
    PY >=  1991 & PY <=  1994 ~ "1991-94",
    PY >=  1995 & PY <=  2000 ~ "1995-00",
    PY >=  2001 & PY <=  2005 ~ "2001-05",
    PY >=  2006 & PY <=  2010 ~ "2006-10",
    TRUE ~ "2011-18"))

ALL_ECON_POLICY_TC_PY <- ALL_ECON_POLICY_TC_PY %>%
     mutate(Era = case_when(
          PY <=  1994 ~ "before merger into ILRI",
          TRUE ~ "after merger into ILRI"))

ALL_ECON_POLICY_TC_PY <- ALL_ECON_POLICY_TC_PY %>%
     mutate(Era = fct_relevel(Era,"before merger into ILRI","after merger into ILRI"))


ALL_ECON_POLICY_TC_PY$Period <- factor(ALL_ECON_POLICY_TC_PY$Period)
ALL_ECON_POLICY_TC_PY$PY <- factor(ALL_ECON_POLICY_TC_PY$PY)
ALL_ECON_POLICY_TC_PY$Era <- factor(ALL_ECON_POLICY_TC_PY$Era)

ALL_ECON_POLICY_TC_PY <- select(ALL_ECON_POLICY_TC_PY,PY,Era,Period,
 Institution,Papers = TC.length, Citations = TC.sum)

ALL_ECON_POLICY_TC_PY_gather <- ALL_ECON_POLICY_TC_PY %>% pivot_longer(Papers:Citations,
names_to = "Bibliometric",values_to = "Value")

ALL_ECON_POLICY_TC_PY_gather <- ALL_ECON_POLICY_TC_PY_gather %>%
  mutate(Institution = fct_relevel(Institution,"ILRI","Africa","Global"))

ALL_ECON_POLICY_TC_PY_gather <- ALL_ECON_POLICY_TC_PY_gather %>%
     mutate(Bibliometric = fct_relevel(Bibliometric,"Papers","Citations"))

(ALL_ECON_POLICY_TC_PY_group <- group_by(ALL_ECON_POLICY_TC_PY_gather,
Era,Institution,Period,Bibliometric) %>% summarise(sumValue = sum(Value)))

pal_sys_fig_3 <- pal_sys_fig_2
tles.ECON.POLICY <- list(unique(ALL_ECON_POLICY_TC_PY_group$Bibliometric),unique(ALL_ECON_POLICY_TC_PY_group$Era))

(tibble.EP.group <- ALL_ECON_POLICY_TC_PY_group %>% group_by(Era,Institution,Bibliometric) %>%
          summarise(sumV = sum(sumValue)))

#
#-------------------------
#CLIMATE
#

CLIM_ILRI <- read.xlsx("PREFACE_LSR_PORTAL.xlsx",sheet = "bib_CLIM_ILRI",colNames = T)
CLIM_ILRI <- filter(CLIM_ILRI,PY >=  1977 & PY <=  2018)
CLIM_ILRI <- select(CLIM_ILRI,TI,SR,PY,TC)
CLIM_ILRI$Institution <- as.factor("ILRI")

CLIM_AFRICA <- read.xlsx("PREFACE_LSR_PORTAL.xlsx",sheet = "bib_CLIM_AFRICA",colNames = T)
CLIM_AFRICA <- filter(CLIM_AFRICA,PY >=  1977 & PY <=  2018)
CLIM_AFRICA <- select(CLIM_AFRICA,TI,SR,PY,TC)
CLIM_AFRICA$Institution <- as.factor("Africa")

CLIM_GLOBAL <- read.xlsx("PREFACE_LSR_PORTAL.xlsx",sheet = "bib_CLIM_GLOBAL",colNames = T)
CLIM_GLOBAL <- filter(CLIM_GLOBAL,PY >=  1977 & PY <=  2018)
CLIM_GLOBAL <- select(CLIM_GLOBAL,TI,SR,PY,TC)
CLIM_GLOBAL$Institution <- as.factor("Global")

ALL_CLIM <- rbind(CLIM_AFRICA,CLIM_ILRI,CLIM_GLOBAL)

ALL_CLIM <- select(ALL_CLIM,PY,Institution,TC)
ALL_CLIM_TC_PY <- data.frame(summaryBy(TC ~ PY + Institution, data = ALL_CLIM, FUN = c(length,sum)))


ALL_CLIM_TC_PY <- ALL_CLIM_TC_PY %>%
  mutate(Period = case_when(
    PY >=  1977 & PY <=  1985 ~ "1977-85",
    PY >=  1986 & PY <=  1990 ~ "1986-90",
    PY >=  1991 & PY <=  1994 ~ "1991-94",
    PY >=  1995 & PY <=  2000 ~ "1995-00",
    PY >=  2001 & PY <=  2005 ~ "2001-05",
    PY >=  2006 & PY <=  2010 ~ "2006-10",
    TRUE ~ "2011-18"))


ALL_CLIM_TC_PY <- ALL_CLIM_TC_PY %>%
     mutate(Era = case_when(
          PY <=  1994 ~ "before merger into ILRI",
          TRUE ~ "after merger into ILRI"))

ALL_CLIM_TC_PY <- ALL_CLIM_TC_PY %>%
     mutate(Era = fct_relevel(Era,"before merger into ILRI","after merger into ILRI"))


ALL_CLIM_TC_PY$Period <- factor(ALL_CLIM_TC_PY$Period)
ALL_CLIM_TC_PY$PY <- factor(ALL_CLIM_TC_PY$PY)
ALL_CLIM_TC_PY$Era <- factor(ALL_CLIM_TC_PY$Era)

ALL_CLIM_TC_PY <- select(ALL_CLIM_TC_PY,PY,Era,Period,
                                Institution,Papers = TC.length, Citations = TC.sum)

ALL_CLIM_TC_PY_gather <- ALL_CLIM_TC_PY %>% pivot_longer(Papers:Citations,
names_to = "Bibliometric",values_to = "Value")

ALL_CLIM_TC_PY_gather <- ALL_CLIM_TC_PY_gather %>%
     mutate(Institution = fct_relevel(Institution,"ILRI","Africa","Global"))

ALL_CLIM_TC_PY_gather <- ALL_CLIM_TC_PY_gather %>%
     mutate(Bibliometric = fct_relevel(Bibliometric,"Papers","Citations"))

ALL_CLIM_TC_PY_group <- group_by(ALL_CLIM_TC_PY_gather,
Era,Institution,Period,Bibliometric) %>% summarise(sumValue = sum(Value))

tles.CLIM <- list(unique(ALL_CLIM_TC_PY_group$Bibliometric),unique(ALL_CLIM_TC_PY_group$Era))

pal_sys_fig_4 <- pal_sys_fig_2

#----------------------------
#QUANTILES
#
FSR_ILRI <- data.table(read.xlsx("PREFACE_LSR_PORTAL.xlsx",sheet = "bib_FSR_ILRI",colNames = T))
FSR_ILRI <- select(FSR_ILRI,PY = PY,TC = TC)
FSR_ILRI <- filter(FSR_ILRI, PY >=  1977 & PY <= 2018)
FSR_ILRI$Domain <- factor("Livestock systems")
FSR_ILRI$Institution <- factor("ILRI")
FSR_ILRI_pr <- quantile(FSR_ILRI$TC, probs = c(0.25,0.50,0.75,0.95))
FSR_ILRI <- FSR_ILRI %>%
  mutate(Quants = case_when(
    TC <=   FSR_ILRI_pr[1] ~ "first",
    TC <=   FSR_ILRI_pr[2] & TC >=  FSR_ILRI_pr[1] ~ "second",
    TC <=   FSR_ILRI_pr[3] & TC >=  FSR_ILRI_pr[2] ~ "third",
    TC <=   FSR_ILRI_pr[4] & TC >=  FSR_ILRI_pr[3] ~ "fourth",
    TC >   FSR_ILRI_pr[4]  ~ "top 5%"))

FSR_AFRICA <- data.table(read.xlsx("PREFACE_LSR_PORTAL.xlsx",sheet = "bib_FSR_AFRICA",colNames = T))
FSR_AFRICA <- select(FSR_AFRICA,PY = PY,TC = TC)
FSR_AFRICA <- filter(FSR_AFRICA, PY >=  1977 & PY <= 2018)
FSR_AFRICA$Domain <- factor("Livestock systems")
FSR_AFRICA$Institution <- factor("Africa")
FSR_AFRICA_pr <- quantile(FSR_AFRICA$TC, probs = c(0.25,0.50,0.75,0.95))
FSR_AFRICA <- FSR_AFRICA %>%
  mutate(Quants = case_when(
    TC <=   FSR_AFRICA_pr[1] ~ "first",
    TC <=   FSR_AFRICA_pr[2] & TC >=  FSR_AFRICA_pr[1] ~ "second",
    TC <=   FSR_AFRICA_pr[3] & TC >=  FSR_AFRICA_pr[2] ~ "third",
    TC <=   FSR_AFRICA_pr[4] & TC >=  FSR_AFRICA_pr[3] ~ "fourth",
    TC >   FSR_AFRICA_pr[4]  ~ "top 5%"))

FSR_GLOBAL <- data.table(read.xlsx("PREFACE_LSR_PORTAL.xlsx",sheet = "bib_FSR_GLOBAL",colNames = T))
FSR_GLOBAL <- select(FSR_GLOBAL,PY = PY,TC = TC)
FSR_GLOBAL <- filter(FSR_GLOBAL, PY >=  1977 & PY <= 2018)
FSR_GLOBAL$Domain <- factor("Livestock systems")
FSR_GLOBAL$Institution <- factor("Global")
FSR_GLOBAL_pr <- quantile(FSR_GLOBAL$TC, probs = c(0.25,0.50,0.75,0.95))
FSR_GLOBAL <- FSR_GLOBAL %>%
  mutate(Quants = case_when(
    TC <=   FSR_GLOBAL_pr[1] ~ "first",
    TC <=   FSR_GLOBAL_pr[2] & TC >=  FSR_GLOBAL_pr[1] ~ "second",
    TC <=   FSR_GLOBAL_pr[3] & TC >=  FSR_GLOBAL_pr[2] ~ "third",
    TC <=   FSR_GLOBAL_pr[4] & TC >=  FSR_GLOBAL_pr[3] ~ "fourth",
    TC >   FSR_GLOBAL_pr[4]  ~ "top 5%"))

CLIMATE_ILRI <- data.table(read.xlsx("PREFACE_LSR_PORTAL.xlsx",sheet = "bib_CLIM_ILRI",colNames = T))
CLIMATE_ILRI <- select(CLIMATE_ILRI,PY = PY,TC = TC)
CLIMATE_ILRI <- filter(CLIMATE_ILRI, PY >=  1977 & PY <= 2018)
CLIMATE_ILRI$Domain <- factor("Climate")
CLIMATE_ILRI$Institution <- factor("ILRI")
CLIMATE_ILRI_pr <- quantile(CLIMATE_ILRI$TC, probs = c(0.25,0.50,0.75,0.95))
CLIMATE_ILRI <- CLIMATE_ILRI %>%
     mutate(Quants = case_when(
          TC <=   CLIMATE_ILRI_pr[1] ~ "first",
          TC <=   CLIMATE_ILRI_pr[2] & TC >=  CLIMATE_ILRI_pr[1] ~ "second",
          TC <=   CLIMATE_ILRI_pr[3] & TC >=  CLIMATE_ILRI_pr[2] ~ "third",
          TC <=   CLIMATE_ILRI_pr[4] & TC >=  CLIMATE_ILRI_pr[3] ~ "fourth",
          TC >   CLIMATE_ILRI_pr[4]  ~ "top 5%"))

CLIMATE_AFRICA <- data.table(read.xlsx("PREFACE_LSR_PORTAL.xlsx",sheet = "bib_CLIM_AFRICA",colNames = T))
CLIMATE_AFRICA <- select(CLIMATE_AFRICA,PY = PY,TC = TC)
CLIMATE_AFRICA <- filter(CLIMATE_AFRICA, PY >=  1977 & PY <=  2018)
CLIMATE_AFRICA$Domain <- factor("Climate")
CLIMATE_AFRICA$Institution <- factor("Africa")
CLIMATE_AFRICA_pr <- quantile(CLIMATE_AFRICA$TC, probs = c(0.25,0.50,0.75,0.95))
CLIMATE_AFRICA <- CLIMATE_AFRICA %>%
  mutate(Quants = case_when(
    TC <=   CLIMATE_AFRICA_pr[1] ~ "first",
    TC <=   CLIMATE_AFRICA_pr[2] & TC >=  CLIMATE_AFRICA_pr[1] ~ "second",
    TC <=   CLIMATE_AFRICA_pr[3] & TC >=  CLIMATE_AFRICA_pr[2] ~ "third",
    TC <=   CLIMATE_AFRICA_pr[4] & TC >=  CLIMATE_AFRICA_pr[3] ~ "fourth",
    TC >   CLIMATE_AFRICA_pr[4]  ~ "top 5%"))

CLIMATE_GLOBAL <- data.table(read.xlsx("PREFACE_LSR_PORTAL.xlsx",sheet = "bib_CLIM_GLOBAL",colNames = T))
CLIMATE_GLOBAL <- select(CLIMATE_GLOBAL,PY,TC)
CLIMATE_GLOBAL <- filter(CLIMATE_GLOBAL,PY >=  1977 & PY <=  2018)
CLIMATE_GLOBAL$Domain <- factor("Climate")
CLIMATE_GLOBAL$Institution <- factor("Global")
CLIMATE_GLOBAL_pr <- quantile(CLIMATE_GLOBAL$TC, probs = c(0.25,0.50,0.75,0.95))
CLIMATE_GLOBAL <- CLIMATE_GLOBAL %>%
     mutate(Quants = case_when(
          TC <=   CLIMATE_GLOBAL_pr[1] ~ "first",
          TC <=   CLIMATE_GLOBAL_pr[2] & TC >=  CLIMATE_GLOBAL_pr[1] ~ "second",
          TC <=   CLIMATE_GLOBAL_pr[3] & TC >=  CLIMATE_GLOBAL_pr[2] ~ "third",
          TC <=   CLIMATE_GLOBAL_pr[4] & TC >=  CLIMATE_GLOBAL_pr[3] ~ "fourth",
          TC >   CLIMATE_GLOBAL_pr[4]  ~ "top 5%"))

ECONOMICS_ILRI <- data.table(read.xlsx("PREFACE_LSR_PORTAL.xlsx",sheet = "bib_ECON_POLICY_ILRI",colNames = T))
ECONOMICS_ILRI <- select(ECONOMICS_ILRI,PY = PY,TC = TC)
ECONOMICS_ILRI <- filter(ECONOMICS_ILRI, PY >=  1977 & PY <=  2018)
ECONOMICS_ILRI$Domain <- factor("Economics and policy")
ECONOMICS_ILRI$Institution <- factor("ILRI")
ECONOMICS_ILRI_pr <- quantile(ECONOMICS_ILRI$TC, probs = c(0.25,0.50,0.75,0.95))
ECONOMICS_ILRI <- ECONOMICS_ILRI %>%
     mutate(Quants = case_when(
          TC <=   ECONOMICS_ILRI_pr[1] ~ "first",
          TC <=   ECONOMICS_ILRI_pr[2] & TC >=  ECONOMICS_ILRI_pr[1] ~ "second",
          TC <=   ECONOMICS_ILRI_pr[3] & TC >=  ECONOMICS_ILRI_pr[2] ~ "third",
          TC <=   ECONOMICS_ILRI_pr[4] & TC >=  ECONOMICS_ILRI_pr[3] ~ "fourth",
          TC >   ECONOMICS_ILRI_pr[4]  ~ "top 5%"))

ECONOMICS_AFRICA <- data.table(read.xlsx("PREFACE_LSR_PORTAL.xlsx",sheet = "bib_ECON_POLICY_AFRICA",colNames = T))
ECONOMICS_AFRICA <- select(ECONOMICS_AFRICA,PY = PY,TC = TC)
ECONOMICS_AFRICA$PY <- as.integer(ECONOMICS_AFRICA$PY)
ECONOMICS_AFRICA$TC <- as.integer(ECONOMICS_AFRICA$TC)

ECONOMICS_AFRICA <- filter(ECONOMICS_AFRICA, PY >=  1977 & PY <=  2018)
ECONOMICS_AFRICA$Domain <- factor("Economics and policy")
ECONOMICS_AFRICA$Institution <- factor("Africa")
ECONOMICS_AFRICA_pr <- quantile(ECONOMICS_AFRICA$TC, probs = c(0.25,0.50,0.75,0.95))
ECONOMICS_AFRICA <- ECONOMICS_AFRICA %>%
  mutate(Quants = case_when(
    TC <=   ECONOMICS_AFRICA_pr[1] ~ "first",
    TC <=   ECONOMICS_AFRICA_pr[2] & TC >=  ECONOMICS_AFRICA_pr[1] ~ "second",
    TC <=   ECONOMICS_AFRICA_pr[3] & TC >=  ECONOMICS_AFRICA_pr[2] ~ "third",
    TC <=   ECONOMICS_AFRICA_pr[4] & TC >=  ECONOMICS_AFRICA_pr[3] ~ "fourth",
    TC >   ECONOMICS_AFRICA_pr[4]  ~ "top 5%"))

ECONOMICS_GLOBAL <- data.table(read.xlsx("PREFACE_LSR_PORTAL.xlsx",sheet = "bib_ECON_POLICY_GLOBAL",colNames = T))
ECONOMICS_GLOBAL <- select(ECONOMICS_GLOBAL,PY = PY,TC = TC)
ECONOMICS_GLOBAL$PY <- as.integer(ECONOMICS_GLOBAL$PY)
ECONOMICS_GLOBAL$TC <- as.integer(ECONOMICS_GLOBAL$TC)
ECONOMICS_GLOBAL <- filter(ECONOMICS_GLOBAL, PY >=  1977 & PY <=  2018)
ECONOMICS_GLOBAL$Domain <- factor("Economics and policy")
ECONOMICS_GLOBAL$Institution <- factor("Global")
ECONOMICS_GLOBAL_pr <- quantile(ECONOMICS_GLOBAL$TC, probs = c(0.25,0.50,0.75,0.95))
ECONOMICS_GLOBAL <- ECONOMICS_GLOBAL %>%
     mutate(Quants = case_when(
          TC <=   ECONOMICS_GLOBAL_pr[1] ~ "first",
          TC <=   ECONOMICS_GLOBAL_pr[2] & TC >=  ECONOMICS_GLOBAL_pr[1] ~ "second",
          TC <=   ECONOMICS_GLOBAL_pr[3] & TC >=  ECONOMICS_GLOBAL_pr[2] ~ "third",
          TC <=   ECONOMICS_GLOBAL_pr[4] & TC >=  ECONOMICS_GLOBAL_pr[3] ~ "fourth",
          TC >   ECONOMICS_GLOBAL_pr[4]  ~ "top 5%"))

#now rbind the groups
#

ALL_CLIM <- ungroup(ALL_CLIM)
ALL_CLIMATE_AFRICA <- filter(ALL_CLIM, (Institution == "ILRI" | Institution == "Africa") &
             TC > 9)
ALL_CLIMATE_AFRICA <- arrange(ALL_CLIMATE_AFRICA,PY)
CLIMATE_AFRICA_pr10 <- quantile(ALL_CLIMATE_AFRICA$TC, probs = c(0.25,0.50,0.75,0.95))
ALL_CLIMATE_AFRICA <- ALL_CLIMATE_AFRICA %>%
     mutate(Quants = case_when(
          TC <= CLIMATE_AFRICA_pr10[1] ~ "first",
          TC <= CLIMATE_AFRICA_pr10[2] & TC > CLIMATE_AFRICA_pr10[1] ~ "second",
          TC <= CLIMATE_AFRICA_pr10[3] & TC >  CLIMATE_AFRICA_pr10[2] ~ "third",
          TC <= CLIMATE_AFRICA_pr10[4] & TC >  CLIMATE_AFRICA_pr10[3] ~ "fourth",
          TC >  CLIMATE_AFRICA_pr10[4]  ~ "top 5%"))
ALL_CLIMATE_AFRICA$Domain <- "Climate"

ALL_FSR <- ungroup(ALL_FSR)
ALL_FSR_AFRICA <- filter(ALL_FSR, (Institution == "ILRI" | Institution == "Africa") &
               TC > 9)
FSR_AFRICA_pr10 <- quantile(ALL_FSR_AFRICA$TC, probs = c(0.25,0.50,0.75,0.95))
ALL_FSR_AFRICA <- ALL_FSR_AFRICA %>%
     mutate(Quants = case_when(
          TC <= FSR_AFRICA_pr10[1] ~ "first",
          TC <= FSR_AFRICA_pr10[2] & TC > FSR_AFRICA_pr10[1] ~ "second",
          TC <= FSR_AFRICA_pr10[3] & TC >  FSR_AFRICA_pr10[2] ~ "third",
          TC <= FSR_AFRICA_pr10[4] & TC >  FSR_AFRICA_pr10[3] ~ "fourth",
          TC >  FSR_AFRICA_pr10[4]  ~ "top 5%"))
ALL_FSR_AFRICA$Domain <- "Livestock systems"

ALL_ECON_POLICY <- ungroup(ALL_ECON_POLICY)
ALL_ECONOMICS_AFRICA <- filter(ALL_ECON_POLICY, (Institution == "ILRI" | Institution == "Africa") &
               TC > 9)
ECONOMICS_AFRICA_pr10 <- quantile(ALL_ECONOMICS_AFRICA$TC, probs = c(0.25,0.50,0.75,0.95))
ALL_ECONOMICS_AFRICA <- ALL_ECONOMICS_AFRICA %>%
     mutate(Quants = case_when(
          TC <= ECONOMICS_AFRICA_pr10[1] ~ "first",
          TC <= ECONOMICS_AFRICA_pr10[2] & TC > ECONOMICS_AFRICA_pr10[1] ~ "second",
          TC <= ECONOMICS_AFRICA_pr10[3] & TC >  ECONOMICS_AFRICA_pr10[2] ~ "third",
          TC <= ECONOMICS_AFRICA_pr10[4] & TC >  ECONOMICS_AFRICA_pr10[3] ~ "fourth",
          TC >  ECONOMICS_AFRICA_pr10[4]  ~ "top 5%"))
ALL_ECONOMICS_AFRICA$Domain <- "Economics and policy"

ALL_SYSTEMS_ILRI.AFRICA.TOP5 <- rbind(ALL_CLIMATE_AFRICA,ALL_FSR_AFRICA, ALL_ECONOMICS_AFRICA)

#####
ALL_CLIMATE_GLOBAL <- filter(ALL_CLIM, (Institution == "ILRI" | Institution == "Global") &
          TC > 9)
CLIMATE_GLOBAL_pr10 <- quantile(ALL_CLIMATE_GLOBAL$TC, probs = c(0.25,0.50,0.75,0.95))
ALL_CLIMATE_GLOBAL <- ALL_CLIMATE_GLOBAL %>%
     mutate(Quants = case_when(
          TC <= CLIMATE_GLOBAL_pr10[1] ~ "first",
          TC <= CLIMATE_GLOBAL_pr10[2] & TC > CLIMATE_GLOBAL_pr10[1] ~ "second",
          TC <= CLIMATE_GLOBAL_pr10[3] & TC >  CLIMATE_GLOBAL_pr10[2] ~ "third",
          TC <= CLIMATE_GLOBAL_pr10[4] & TC >  CLIMATE_GLOBAL_pr10[3] ~ "fourth",
          TC >  CLIMATE_GLOBAL_pr10[4]  ~ "top 5%"))
ALL_CLIMATE_GLOBAL$Domain <- "Climate"

ALL_FSR_GLOBAL <- filter(ALL_FSR, (Institution == "ILRI" | Institution == "Global") &
                                  TC > 9)
FSR_GLOBAL_pr10 <- quantile(ALL_FSR_GLOBAL$TC, probs = c(0.25,0.50,0.75,0.95))
ALL_FSR_GLOBAL <- ALL_FSR_GLOBAL %>%
     mutate(Quants = case_when(
          TC <= FSR_GLOBAL_pr10[1] ~ "first",
          TC <= FSR_GLOBAL_pr10[2] & TC > FSR_GLOBAL_pr10[1] ~ "second",
          TC <= FSR_GLOBAL_pr10[3] & TC >  FSR_GLOBAL_pr10[2] ~ "third",
          TC <= FSR_GLOBAL_pr10[4] & TC >  FSR_GLOBAL_pr10[3] ~ "fourth",
          TC >  FSR_GLOBAL_pr10[4]  ~ "top 5%"))
ALL_FSR_GLOBAL$Domain <- "Livestock systems"

ALL_ECONOMICS_GLOBAL <- filter(ALL_ECON_POLICY, (Institution == "ILRI" | Institution == "Global") &
      TC > 9)
ECONOMICS_GLOBAL_pr10 <- quantile(ALL_ECONOMICS_GLOBAL$TC, probs = c(0.25,0.50,0.75,0.95))
ALL_ECONOMICS_GLOBAL <- ALL_ECONOMICS_GLOBAL %>%
     mutate(Quants = case_when(
          TC <= ECONOMICS_GLOBAL_pr10[1] ~ "first",
          TC <= ECONOMICS_GLOBAL_pr10[2] & TC > ECONOMICS_GLOBAL_pr10[1] ~ "second",
          TC <= ECONOMICS_GLOBAL_pr10[3] & TC >  ECONOMICS_GLOBAL_pr10[2] ~ "third",
          TC <= ECONOMICS_GLOBAL_pr10[4] & TC >  ECONOMICS_GLOBAL_pr10[3] ~ "fourth",
          TC >  ECONOMICS_GLOBAL_pr10[4]  ~ "top 5%"))
ALL_ECONOMICS_GLOBAL$Domain <- "Economics and policy"

ALL_SYSTEMS_ILRI.GLOBAL.TOP5 <- rbind(ALL_CLIMATE_GLOBAL,ALL_FSR_GLOBAL, ALL_ECONOMICS_GLOBAL)

ALL_SYSTEMS_ILRI.AFRICA.TOP5$Quants <- factor(ALL_SYSTEMS_ILRI.AFRICA.TOP5$Quants)
ALL_SYSTEMS_ILRI.GLOBAL.TOP5$Quants <- factor(ALL_SYSTEMS_ILRI.GLOBAL.TOP5$Quants)
ALL_SYSTEMS_ILRI.AFRICA.TOP5$Domain <- factor(ALL_SYSTEMS_ILRI.AFRICA.TOP5$Domain)
ALL_SYSTEMS_ILRI.GLOBAL.TOP5$Domain <- factor(ALL_SYSTEMS_ILRI.GLOBAL.TOP5$Domain)

ALL_SYSTEMS_ILRI.AFRICA.TOP5 <- ALL_SYSTEMS_ILRI.AFRICA.TOP5 %>%
     mutate(Quants = fct_relevel(Quants,"first","second","third","fourth","top 5%"))
ALL_SYSTEMS_ILRI.GLOBAL.TOP5 <- ALL_SYSTEMS_ILRI.GLOBAL.TOP5 %>%
  mutate(Quants = fct_relevel(Quants,"first","second","third","fourth","top 5%"))
ALL_SYSTEMS_ILRI.AFRICA.TOP5 <- ALL_SYSTEMS_ILRI.AFRICA.TOP5 %>%
     mutate(Institution = fct_relevel(Institution,"ILRI","Africa"))
ALL_SYSTEMS_ILRI.GLOBAL.TOP5 <- ALL_SYSTEMS_ILRI.GLOBAL.TOP5 %>%
  mutate(Institution = fct_relevel(Institution,"ILRI","Global"))

ALL_SYSTEMS_ILRI.AFRICA.TOP5 <- ALL_SYSTEMS_ILRI.AFRICA.TOP5 %>%
     mutate(Era = case_when(
          PY <=  1994 ~ "before merger into ILRI",
          TRUE ~ "after merger into ILRI"))

ALL_SYSTEMS_ILRI.AFRICA.TOP5 <- ALL_SYSTEMS_ILRI.AFRICA.TOP5 %>%
     mutate(Era = fct_relevel(Era,"before merger into ILRI","after merger into ILRI"))

#now do the ILRI.GLOBAL.TOP5 groups -----------------------

ALL_SYSTEMS_ILRI.GLOBAL.TOP5 <- ALL_SYSTEMS_ILRI.GLOBAL.TOP5 %>%
     mutate(Era = case_when(
          PY <=  1994 ~ "before merger into ILRI",
          TRUE ~ "after merger into ILRI"))

ALL_SYSTEMS_ILRI.GLOBAL.TOP5 <- ALL_SYSTEMS_ILRI.GLOBAL.TOP5 %>%
     mutate(Era = fct_relevel(Era,"before merger into ILRI","after merger into ILRI"))

tles.ILRI_AFRICA <- list(unique(ALL_SYSTEMS_ILRI.AFRICA.TOP5$Era),unique(ALL_SYSTEMS_ILRI.AFRICA.TOP5$Domain))
tles.ILRI_GLOBAL <- list(unique(ALL_SYSTEMS_ILRI.GLOBAL.TOP5$Era),unique(ALL_SYSTEMS_ILRI.GLOBAL.TOP5$Domain))

ALL_SYSTEMS_ILRI.AFRICA.TOP5 <- select(ALL_SYSTEMS_ILRI.AFRICA.TOP5,Era,Institution,Domain,Quants,TC)

ALL_SYSTEMS_ILRI.AFRICA.TOP5_group <- group_by(ALL_SYSTEMS_ILRI.AFRICA.TOP5,Era,
     Institution,Domain,Quants) %>% summarise(sumInstitution = length(TC))

ALL_SYSTEMS_ILRI.GLOBAL.TOP5 <- select(ALL_SYSTEMS_ILRI.GLOBAL.TOP5,PY,Era,Institution,Domain,Quants,TC)

(ALL_SYSTEMS_ILRI.GLOBAL.TOP5_group <- group_by(ALL_SYSTEMS_ILRI.GLOBAL.TOP5,
     Institution,Era,Domain,Quants) %>% summarise(sumInstitution = length(TC)))

tles.ILRI_GLOBAL <- list(unique(ALL_SYSTEMS_ILRI.GLOBAL.TOP5_group$Domain),unique(ALL_SYSTEMS_ILRI.GLOBAL.TOP5_group$Era))


pal_sys_fig_5 <- pal_sys_fig_2

sepcomma <- function(x) {
     format(as.numeric(x),big.interval = ',',decimal.mark = '.')
}


#summary data for figure 5a
#

tibble_all_sys_global <- ALL_SYSTEMS_ILRI.GLOBAL.TOP5 %>%
          group_by(Era,Institution,Domain,Quants) %>%
          summarise(sumQuantTC = sum(TC),nQuantTC = length(TC))

#top 5 shares of climate papers in ILCA/ILRAD and ILRI eras
(tibble_all_sys_global_clim <- tibble_all_sys_global %>% filter(Domain ==  "Climate" & Quants ==  "top 5%"))
(ILCA_ILRAD.clim.TOP5.share <- round(100*tibble_all_sys_global_clim[1,6] /
(tibble_all_sys_global_clim[1,6] + tibble_all_sys_global_clim[2,6]),digits = 1))

(ILCA_ILRAD.clim.TOP5.share <- round(100*tibble_all_sys_global_clim[1,5] /
 (tibble_all_sys_global_clim[1,5] + tibble_all_sys_global_clim[2,5]),digits = 1))


(ILRI_clim.top5.share <- round(100*tibble_all_sys_global_clim[3,6] /
 (tibble_all_sys_global_clim[3,6] + tibble_all_sys_global_clim[4,6]),digits = 1))

(ILRI_clim.top5.share <- round(100*tibble_all_sys_global_clim[3,5] /
 (tibble_all_sys_global_clim[3,5] + tibble_all_sys_global_clim[4,5]),digits = 1))

(tibble_all_sys_global_sys <- tibble_all_sys_global %>% filter(Domain ==  "Livestock systems" & Quants ==  "top 5%"))
(ILCA_ILRAD.sys_top5.share <- round(100*tibble_all_sys_global_sys[1,6] /
 (tibble_all_sys_global_sys[1,6] + tibble_all_sys_global_sys[2,6]),digits = 1))

(ILCA_ILRAD.sys_top5.share <- round(100*tibble_all_sys_global_sys[1,5] /
 (tibble_all_sys_global_sys[1,5] + tibble_all_sys_global_sys[2,5]),digits = 1))

(ILRI_sys.top5.share <- round(100*tibble_all_sys_global_sys[3,6] /
(tibble_all_sys_global_sys[3,6] + tibble_all_sys_global_sys[4,6]),digits = 1))

(ILRI_sys.top5.share <- round(100*tibble_all_sys_global_sys[3,5] /
 (tibble_all_sys_global_sys[3,5] + tibble_all_sys_global_sys[4,5]),digits = 1))


(tibble_all_sys_global_ep <- tibble_all_sys_global %>% filter(Domain ==  "Economics and policy" & Quants ==  "top 5%"))
(ILCA_ILRAD.ep.top5.share <- round(100*tibble_all_sys_global_ep[1,6] /
 (tibble_all_sys_global_ep[1,6] + tibble_all_sys_global_ep[2,6]),digits = 1))

(ILCA_ILRAD.clim_top5.share <- round(100*tibble_all_sys_global_ep[1,5] /
 (tibble_all_sys_global_ep[1,5] + tibble_all_sys_global_ep[2,5]),digits = 1))

(ILRI_ep.top5.share <- round(100*tibble_all_sys_global_ep[3,6] /
 (tibble_all_sys_global_ep[3,6] + tibble_all_sys_global_ep[4,6]),digits = 1))

(ILRI_ep.top5.share <- round(100*tibble_all_sys_global_ep[3,5] /
 (tibble_all_sys_global_ep[3,5] + tibble_all_sys_global_ep[4,5]),digits = 1))

tibble.IA.T5 <- ALL_SYSTEMS_ILRI.AFRICA.TOP5_group %>% group_by(Era,Institution,Domain) %>%
          summarise(sumI = sum(sumInstitution))


(tibble_all_sys_africa <- ALL_SYSTEMS_ILRI.AFRICA.TOP5 %>%
          group_by(Era,Institution,Domain,Quants) %>% summarise(QuantTC = length(TC)))

(tibble_all_sys_africa_clim <- tibble_all_sys_africa %>% filter(Domain ==  "Climate" & Quants ==  "top 5%"))
(ILCA_ILRAD.clim_top5.share <- round(100*tibble_all_sys_africa_clim[1,5] /
(tibble_all_sys_africa_clim[1,5] + tibble_all_sys_africa_clim[2,5]),digits = 1))

(ILRI_clim.top5.share <- round(100*tibble_all_sys_africa_clim[3,5] /
                                    (tibble_all_sys_africa_clim[3,5] + tibble_all_sys_africa_clim[4,5]),digits = 1))

(tibble_all_sys_africa_sys <- tibble_all_sys_africa %>% filter(Domain ==  "Livestock systems" & Quants ==  "top 5%"))
(ILCA_ILRAD.sys.top5.share <- round(100*tibble_all_sys_africa_sys[1,5] /
                                         (tibble_all_sys_africa_sys[1,5] + tibble_all_sys_africa_sys[2,5]),digits = 1))
(ILRI_sys.top5.share <- round(100*tibble_all_sys_africa_sys[3,5] /
                                   (tibble_all_sys_africa_sys[3,5] + tibble_all_sys_africa_sys[4,5]),digits = 1))

(tibble_all_sys_africa_ep <- tibble_all_sys_africa %>% filter(Domain ==  "Economics and policy" & Quants ==  "top 5%"))
(ILCA_ILRAD.ep.top5.share <- round(100*tibble_all_sys_africa_ep[1,5] /
                                        (tibble_all_sys_africa_ep[1,5] + tibble_all_sys_africa_ep[2,5]),digits = 1))
(ILRI_ep.top5.share <- round(100*tibble_all_sys_africa_ep[3,5] /
                                  (tibble_all_sys_africa_ep[3,5] + tibble_all_sys_africa_ep[4,5]),digits = 1))

#----------------------------------------------
#now get the numbers for the text
#ILCA return on livestock systems spending
ILCA_ILRAD_sys <- PrefSys %>% filter(YEAR <=  1994)
(ILCA_ILRAD_sys.spend <- round(sum(ILCA_ILRAD_sys$GROUP_SYSTEMS_SPEND),digits = 0))
(ILCA_ILRAD_total.spend <- round(sum(ILCA_ILRAD_sys$LIFETIME_TOTAL_SPEND),digits = 0))
(ILCA_ILRAD_sys.spend.share <- ILCA_ILRAD_sys.spend/sum(ILCA_ILRAD_total.spend))
(ILCA_ILRAD_sys.papers <- sum(ILCA_ILRAD_sys$ILCA_ILRI_FSR_PAPERS))
(ILCA_ILRAD_sys.cites <- sum(ILCA_ILRAD_sys$ILCA_ILRI_FSR_TC))
(ILCA_ILRAD_sys.cites.paper <- ILCA_ILRAD_sys$ILCA_ILRI_FSR_TC/ILCA_ILRAD_sys$ILCA_ILRI_FSR_PAPERS)
(ILCA_ILRAD_sys.cites.mean <- mean(ILCA_ILRAD_sys.cites.paper,na.rm = TRUE))
(ILCA_ILRAD_sys.cites.median <- median(ILCA_ILRAD_sys.cites.paper,na.rm = TRUE))
(ILCA_ILRAD.spend.paper <- 1000*ILCA_ILRAD_sys.papers/ILCA_ILRAD_sys.spend)
(ILCA_ILRAD.spend.cites <- 1000*ILCA_ILRAD_sys.cites/ILCA_ILRAD_sys.spend)

#ILCA.ILRAD spend
tibble_ILCA.ILRAD.sys.spend.period <- PrefSys_df_spend_gather %>% filter(Year <=  1994) %>% group_by(Period,Domain) %>% summarise(sumSpend = sum(Spending))

tibble_ILCA.ILRAD.sys.spend.domain <-
     PrefSys_df_spend_gather %>% filter(Year <=  1994) %>% group_by(Domain)  %>% summarise(domainSpend = sum(Spending)/1000)

for (i in 1:length(tibble_ILCA.ILRAD.sys.spend.domain$Domain)) {
tibble_ILCA.ILRAD.sys.spend.domain[i,"spendShare"] <- round(100*(tibble_ILCA.ILRAD.sys.spend.domain[i,2] /
tibble_ILCA.ILRAD.sys.spend.domain[6,2]),digits = 1)}

(tibble_ILCA.ILRAD.sys.spend.domain)

#ILRI spend------------------------------
tibble_ILRI.sys.spend.period <- PrefSys_df_spend_gather %>%
      group_by(Period,Domain) %>%
     summarise(sumSpend = sum(Spending))

tibble_ILRI.sys.spend.domain <- PrefSys_df_spend_gather %>%
     group_by(Domain) %>%
     summarise(domainSpend = sum(Spending)/1000)

for (i in 1:length(tibble_ILRI.sys.spend.domain$Domain)) {
     tibble_ILRI.sys.spend.domain[i,"spendShare"] <- round(100*(tibble_ILRI.sys.spend.domain[i,2] /
tibble_ILRI.sys.spend.domain[6,2]),digits = 1)}

(tibble_ILRI.sys.spend.domain)

#---------------------------LSR papers -------------------------
#
FSR_ILRI <- data.table(read.xlsx("PREFACE_LSR_PORTAL.xlsx",sheet = "bib_FSR_ILRI",colNames = T))
FSR_ILCA_ILRAD <- filter(FSR_ILRI, PY >= 1975 & PY <=  1994)
FSR_ILCA_ILRAD <- arrange(FSR_ILCA_ILRAD,desc(TC))
(ILCA_ILRAD_sys.papers <- length(FSR_ILCA_ILRAD$TC))
(ILCA_ILRAD_sys.cites <- sum(FSR_ILCA_ILRAD$TC))
(ILCA_ILRAD_sys.cites.mean <- round(mean(FSR_ILCA_ILRAD$TC),digits = 0))
(ILCA_ILRAD_sys.cites.median <- round(median(FSR_ILCA_ILRAD$TC),digits = 0))
FSR_ILCA_ILRAD$cites.rank <- frank(FSR_ILCA_ILRAD,-TC,ties.method = "average")
(ILCA_ILRAD_sys.cites.top10 <- filter(FSR_ILCA_ILRAD,cites.rank <=  10) %>% summarise(sum10cites = sum(TC)))
(ILCA_ILRAD_sys.cites.top10.share <- round(100*ILCA_ILRAD_sys.cites.top10 / ILCA_ILRAD_sys.cites,digits = 1))

(ILRI_sys_spend <- sum(PrefSys$GROUP_SYSTEMS_SPEND))
(ILRI_total_spend <- sum(PrefSys$LIFETIME_TOTAL_SPEND))
(ILRI_sys_spend.share <- ILRI_sys_spend/ILRI_total_spend)
FSR_ILRI <- data.table(read.xlsx("PREFACE_LSR_PORTAL.xlsx",sheet = "bib_FSR_ILRI",colNames = T))
FSR_ILRI <- filter(FSR_ILRI, PY >= 1975 & PY <= 2018)
FSR_ILRI <- arrange(FSR_ILRI,desc(TC))
(ILRI_sys.papers <- length(FSR_ILRI$TC))
(ILRI_sys.cites <- sum(FSR_ILRI$TC))
(ILRI_sys.cites.mean <- round(mean(FSR_ILRI$TC),digits = 0))
(ILRI_sys.cites.median <- round(median(FSR_ILRI$TC),digits = 0))
FSR_ILRI$cites.rank <- frank(FSR_ILRI,-TC,ties.method = "average")
(ILRI_sys.cites.top10 <- filter(FSR_ILRI,cites.rank <=  10) %>% summarise(sum10cites = sum(TC)))
(ILRI_sys.cites.top10.share <- round(100*ILRI_sys.cites.top10 / ILRI_sys.cites,digits = 1))

(tibble.FSR.group <- ALL_FSR_TC_PY_group %>% group_by(Era,Institution,Bibliometric) %>% summarise(sumV = sum(sumValue)))

(tibble.FSR.ILRI.Africa <- (sum(tibble.FSR.group[1,4],tibble.FSR.group[7,4]) /
(sum(tibble.FSR.group[1,4],tibble.FSR.group[3,4],tibble.FSR.group[7,4],tibble.FSR.group[9,4]))))

(tibble.FSR.ILRI.Africa <- (sum(tibble.FSR.group[2,4],tibble.FSR.group[8,4]) /
 (sum(tibble.FSR.group[2,4],tibble.FSR.group[4,4],tibble.FSR.group[8,4],tibble.FSR.group[10,4]))))



#--------------------------climate papers -----------------------
ILRI_clim <- read.xlsx("PREFACE_LSR_PORTAL.xlsx",sheet = "bib_CLIM_ILRI",colNames = T)
ILCA_ILRAD.clim <- filter(ILRI_clim,PY >= 1975 & PY <=  1994)

(ILCA_ILRAD.clim.papers <- length(ILCA_ILRAD.clim$TC))
(ILCA_ILRAD.clim.cites <- sum(ILCA_ILRAD.clim$TC))
(ILCA_ILRAD.clim.cites.paper.mean <- mean(ILCA_ILRAD.clim$TC,na.rm = TRUE))
(ILCA_ILRAD.clim.cites.paper.median <- median(ILCA_ILRAD.clim$TC,na.rm = TRUE))

#note that this filter is different because the text in Preface III is different
ILCA_ILRAD.clim$cites.rank <- frank(ILCA_ILRAD.clim,-TC,ties.method = "average")
(ILCA_ILRAD.clim.cites.top10 <- filter(ILCA_ILRAD.clim,cites.rank <=  10) %>% summarise(sum10cites = sum(TC)))
(ILCA_ILRAD.clim.cites.top10.share <- round(100*(ILCA_ILRAD.clim.cites.top10/ILCA_ILRAD.clim.cites),digits = 0))


ILRI_clim <- filter(ILRI_clim,PY >= 1975 & PY <=  2018)
(ILRI_clim.papers <- length(ILRI_clim$TC))
(ILRI_clim.cites <- sum(ILRI_clim$TC))
(ILRI_clim.cites.paper.mean <- mean(ILRI_clim$TC,na.rm = TRUE))
(ILRI_clim.cites.paper.median <- median(ILRI_clim$TC,na.rm = TRUE))
ILRI_clim$cites.rank <- frank(ILRI_clim,-TC,ties.method = "average")
(ILRI_clim.cites.top10 <- filter(ILRI_clim,cites.rank <=  10) %>% summarise(sum10cites = sum(TC)))
(ILRI_clim.cites.top10.share <- round(100*(ILRI_clim.cites.top10/ILRI_clim.cites),digits = 0))


(tibble.CLIM.group <- ALL_CLIM_TC_PY_group %>% group_by(Era,Institution,Bibliometric) %>%
          summarise(sumV = sum(sumValue)))

(tibble.CLIM.ILRI.Africa <- (sum(tibble.CLIM.group[1,4],tibble.CLIM.group[7,4]) /
                                 (sum(tibble.CLIM.group[1,4],tibble.CLIM.group[3,4],tibble.CLIM.group[7,4],tibble.CLIM.group[9,4]))))

(tibble.CLIM.ILRI.Africa <- (sum(tibble.CLIM.group[2,4],tibble.CLIM.group[8,4]) /
                                 (sum(tibble.CLIM.group[2,4],tibble.CLIM.group[4,4],tibble.CLIM.group[8,4],tibble.CLIM.group[10,4]))))

#--------------------------econ and policy papers----------------------
#repeat the ILCA / ILRAD spending
(tibble_ILCA.ILRAD.sys.spend.domain)

ILRI_ep <- read.xlsx("PREFACE_LSR_PORTAL.xlsx",sheet = "bib_ECON_POLICY_ILRI",colNames = T)
ILCA_ILRAD.ep <- filter(ILRI_ep,PY >= 1975 & PY <= 1994)

(ILCA_ILRAD.ep.papers <- length(ILCA_ILRAD.ep$TC))
ILCA_ILRAD.ep.cites <- sum(ILCA_ILRAD.ep$TC)
(ILCA_ILRAD.ep.cites.paper.mean <- mean(ILCA_ILRAD.ep$TC,na.rm = TRUE))
(ILCA_ILRAD.ep.cites.paper.median <- median(ILCA_ILRAD.ep$TC,na.rm = TRUE))
(ILCA_ILRAD.ep$cites.rank <- frank(ILCA_ILRAD.ep,-TC,ties.method = "average"))
(ILCA_ILRAD.ep.cites.top10 <- filter(ILCA_ILRAD.ep,cites.rank <=  10) %>% summarise(sum10cites = sum(TC)))
(ILCA_ILRAD.ep.cites.top10.share <- round(100*(ILCA_ILRAD.ep.cites.top10/ILCA_ILRAD.ep.cites),digits = 0))


#note that this filter is different because the text in Preface III is different

ILRI_ep <- filter(ILRI_ep, PY >= 1975 & PY <= 2018)
(ILRI_ep.spend <- sum(PrefSys$GROUP_ECON_POLICY_SPEND))
(ILRI_total_spend <- sum(PrefSys$LIFETIME_TOTAL_SPEND))
(ILRI_ep.spend.share <- ILRI_ep.spend/ILRI_total_spend)

(ILRI_ep.papers <- length(ILRI_ep$TC))
ILRI_ep.cites <- sum(ILRI_ep$TC)
(ILRI_ep.cites.paper.mean <- mean(ILRI_ep$TC,na.rm = TRUE))
(ILRI_ep.cites.paper.median <- median(ILRI_ep$TC,na.rm = TRUE))
ILRI_ep$cites.rank <- frank(ILRI_ep,-TC,ties.method = "average")
(ILRI_ep.cites.top10 <- filter(ILRI_ep,cites.rank <=  10) %>% summarise(sum10cites = sum(TC)))
(ILRI_ep.cites.top10.share <- round(100*(ILRI_ep.cites.top10/ILRI_ep.cites),digits = 0))

(ILRI_ep.papers.million <- round(1000*ILRI_ep.papers/ILRI_ep.spend,digits = 0))
(ILRI_ep.cites.million <- round(1000*ILRI_ep.cites/ILRI_ep.spend,digits = 0))

(tibble.FSR.group <- ALL_FSR_TC_PY_group %>% group_by(Era,Institution,Bibliometric) %>% summarise(sumV = sum(sumValue)))

(tibble.FSR.ILRI.Africa <- (sum(tibble.FSR.group[1,4],tibble.FSR.group[7,4]) /
                                 (sum(tibble.FSR.group[1,4],tibble.FSR.group[3,4],tibble.FSR.group[7,4],tibble.FSR.group[9,4]))))

(tibble.FSR.ILRI.Africa <- (sum(tibble.FSR.group[2,4],tibble.FSR.group[8,4]) /
                                 (sum(tibble.FSR.group[2,4],tibble.FSR.group[4,4],tibble.FSR.group[8,4],tibble.FSR.group[10,4]))))


#----------------------all papers ---------------
#----------------------Global -------------------
#
(tibble.IG.T5 <- ALL_SYSTEMS_ILRI.GLOBAL.TOP5_group %>%
      group_by(Era,Institution,Domain) %>% summarise(sumI = sum(sumInstitution)))

(tibble.IG.T5.clim <- round(100*(sum(tibble.IG.T5[1,4],tibble.IG.T5[7,4]) /
(sum(tibble.IG.T5[1,4],tibble.IG.T5[4,4],tibble.IG.T5[7,4],tibble.IG.T5[10,4]))),digits = 1))

(tibble.IG.T5.lsr <- round(100*(sum(tibble.IG.T5[2,4],tibble.IG.T5[8,4]) /
(sum(tibble.IG.T5[2,4],tibble.IG.T5[5,4],tibble.IG.T5[8,4],tibble.IG.T5[11,4]))),digits = 1))

(tibble.IG.T5.ep <- round(100*(sum(tibble.IG.T5[3,4],tibble.IG.T5[9,4]) /
(sum(tibble.IG.T5[3,4],tibble.IG.T5[6,4],tibble.IG.T5[9,4],tibble.IG.T5[12,4]))),digits = 1))

#top 5 percent in Global---------------------------
(tibble.IG.T5.Q5 <- ALL_SYSTEMS_ILRI.GLOBAL.TOP5_group %>% filter(Quants == "top 5%") %>% group_by(Era,Institution,Domain) %>%
          summarise(sumI = sum(sumInstitution)))

(tibble.IG.T5.Q5.clim <- round(100*(sum(tibble.IG.T5.Q5[1,4],tibble.IG.T5.Q5[7,4]) /
(sum(tibble.IG.T5.Q5[1,4],tibble.IG.T5.Q5[4,4],tibble.IG.T5.Q5[7,4],tibble.IG.T5.Q5[10,4]))),digits = 1))

(tibble.IG.T5.Q5.lsr <- round(100*(sum(tibble.IG.T5.Q5[2,4],tibble.IG.T5.Q5[8,4]) /
(sum(tibble.IG.T5.Q5[2,4],tibble.IG.T5.Q5[5,4],tibble.IG.T5.Q5[8,4],tibble.IG.T5.Q5[11,4]))),digits = 1))

(tibble.IG.T5.Q5.ep <- round(100*(sum(tibble.IG.T5.Q5[3,4],tibble.IG.T5.Q5[9,4]) /
(sum(tibble.IG.T5.Q5[3,4],tibble.IG.T5.Q5[6,4],tibble.IG.T5.Q5[9,4],tibble.IG.T5.Q5[12,4]))),digits = 1))

#---------------------Africa


(tibble.IA.T5 <- ALL_SYSTEMS_ILRI.AFRICA.TOP5_group %>% group_by(Era,Institution,Domain) %>%
          summarise(sumI = sum(sumInstitution)))

(tibble.IA.T5.clim <- (sum(tibble.IA.T5[1,4],tibble.IA.T5[7,4]) /
                            (sum(tibble.IA.T5[1,4],tibble.IA.T5[4,4],tibble.IA.T5[7,4],tibble.IA.T5[11,4]))))

(tibble.IA.T5.lsr <- (sum(tibble.IA.T5[2,4],tibble.IA.T5[8,4]) /
                           (sum(tibble.IA.T5[2,4],tibble.IA.T5[5,4],tibble.IA.T5[8,4],tibble.IA.T5[11,4]))))

(tibble.IA.T5.ep <- (sum(tibble.IA.T5[3,4],tibble.IA.T5[9,4]) /
                          (sum(tibble.IA.T5[3,4],tibble.IA.T5[6,4],tibble.IA.T5[9,4],tibble.IA.T5[12,4]))))

#do the figures

(PREFACE_III_FIGURE_1 <- ggplot(data = PrefSys_df_spend_group) +
 geom_col(mapping = aes(x = Period, fill = Domain, y = sumDomain),
 position = position_dodge()) +
 labs(x = "Period",y = "Spending in million of 2015 US$",title = tles.Fig.PIII.1) +
 scale_y_continuous(labels = scales::comma)  +
 scale_fill_manual(name = "Domain", values = pal_sys_fig_1) +
 theme_tufte() +
 theme(text = element_text(size = 11,family = "Times"),legend.position = "bottom") +
 labs(caption = "Source: Constructed by authors from data in ILCA/ILRAD/ILRI Annual Reports and Financial Reports.") +
 theme(plot.caption = element_text(hjust = 0)))


(PREFACE_III_FIGURE_2 <- ggplot(data = ALL_FSR_TC_PY_group) +
    geom_col(mapping = aes(x = Period,y = sumValue, fill = Institution),
             position = position_dodge()) +
    labs(x = "Publication period",y = "Counts by publication period",
    title = tles.Fig.PIII.2) +
    facet_wrap(~Bibliometric+Era,scales = "free",nrow = 2,ncol = 2,labeller = labeller(tles.FSR,.multi_line = FALSE)) +
    scale_fill_manual(name = "Institution", values = pal_sys_fig_2) +
    scale_y_continuous(labels = scales::comma) +
    theme_tufte() +
    theme(text = element_text(size = 11,family = "Times"), legend.position = "bottom") +
    labs(caption = paste(c("Source: Constructed by authors from data at www.scopus.com. ILRI papers: before merger, ",tibble.FSR.group[1,4],"; after merger, ",tibble.FSR.group[7,4],".",
    "\nAfrica papers: ",tibble.FSR.group[3,4],"; ",tibble.FSR.group[9,4],". ",
    "Global papers:",tibble.FSR.group[5,4],"; ",tibble.FSR.group[11,4],"."),collapse = "")) +
    theme(plot.caption = element_text(hjust = 0)))


(PREFACE_III_FIGURE_3 <- ggplot(data = ALL_ECON_POLICY_TC_PY_group) +
    geom_col(mapping = aes(x = Period,y = sumValue, fill = Institution), position = position_dodge()) +
    facet_wrap(~Bibliometric+Era,scales = "free",nrow = 2,ncol = 2,
                 labeller = labeller(tles.ECON.POLICY,.multi_line = FALSE)) +
     labs(x = "Publication period",y = "Counts", title = tles.Fig.PIII.3) +
          scale_fill_manual(name = "Institution", values = pal_sys_fig_3) +
          scale_y_continuous(labels = scales::comma) +
    theme_tufte() +
          theme(text = element_text(size = 11,family = "Times"), legend.position = "bottom") +
          labs(caption = paste(c("Source: Constructed by authors from data at www.scopus.com. ILRI papers: before merger, ",tibble.EP.group[1,4],"; after merger, ",tibble.EP.group[7,4],".",
 "\nAfrica papers: ",tibble.EP.group[3,4],";",tibble.EP.group[9,4],". ",
   "Global papers: ",tibble.EP.group[5,4],";",tibble.EP.group[11,4],"."),collapse = "")) +
          theme(plot.caption = element_text(hjust = 0)))


(PREFACE_III_FIGURE_4 <- ggplot(data = ALL_CLIM_TC_PY_group) +
    geom_col(mapping = aes(x = Period,y = sumValue, fill = Institution),position = position_dodge()) +
    facet_wrap(~Bibliometric+Era,scales = "free",nrow = 2,ncol = 2,
    labeller = labeller(tles.CLIM,.multi_line = FALSE)) +
    labs(x = "Publication period", y = "Counts by publication period",title = tles.Fig.PIII.4) +
    scale_fill_manual(name = "Institution", values = pal_sys_fig_4) +
    scale_y_continuous(labels = scales::comma) +
    theme_tufte() +
          theme(text = element_text(size = 11,family = "Times"), legend.position = "bottom") +
          labs(caption = paste(c("Source: Constructed by authors from data at www.scopus.com.
          ILRI papers: before merger, ",tibble.CLIM.group[1,4],"; after merger, ",tibble.CLIM.group[7,4],".",
          "\nAfrica papers: ",tibble.CLIM.group[3,4],"; ",tibble.CLIM.group[9,4],". ",
          "Global papers: ",tibble.CLIM.group[5,4],"; ",tibble.CLIM.group[11,4],"."),collapse = "")) +
          theme(plot.caption = element_text(hjust = 0)))

(PREFACE_III_FIGURE_5a <- ggplot(data = ALL_SYSTEMS_ILRI.GLOBAL.TOP5_group) +
 geom_col(mapping = aes(x = Quants,y  = sumInstitution, fill = Institution), position = "dodge", size = 1) +
 xlab("Citation quantiles") +
 labs(y = "Counts of papers > 9 citations",title = tles.Fig.PIII.5a) +
 scale_fill_manual(name = "Institution", values = pal_sys_fig_5) +
 facet_wrap(~Domain+Era,scales = "free",nrow = 3,ncol = 2,
 labeller = labeller(tles.ILRI_GLOBAL,.multi_line = FALSE)) +
 scale_y_continuous(labels = scales::comma) +
 theme_tufte() +
 theme(text = element_text(size = 10,family = "Times"), legend.position = "bottom") +
 labs(caption = paste(c("Source: Constructed by authors from data at www.scopus.com.
 ILRI climate papers: before merger, 33; after merger, 289.
 Global climate papers: 31; 1,311.
 ILRI livestock systems: 107;407.  Global livestock systems: 398; 4,478.
 ILRI economics and policy: 56; 447. Global economics and policy: 57; 1,001."),collapse = "")) +
 theme(plot.caption = element_text(hjust = 0)))

(PREFACE_III_FIGURE_5b <- ggplot(data = ALL_SYSTEMS_ILRI.AFRICA.TOP5_group) +
    geom_col(mapping = aes(x = Quants,y  = sumInstitution, fill = Institution), position = "dodge", size = 1) +
    xlab("Citation quantiles") +
    labs(y = "Papers > 9 citations",title = tles.Fig.PIII.5b) +
          scale_fill_manual(name = "Institution", values = pal_sys_fig_5) +
    facet_wrap(~Domain+Era,scales = "free",nrow = 3,ncol = 2,
               labeller = labeller(tles.ILRI_AFRICA,.multi_line = FALSE)) +
    scale_y_continuous(labels = scales::comma) +
    theme_tufte() +
    theme(text = element_text(size = 10,family = "Times"), legend.position = "bottom") +
    labs(caption = paste(c("Source: Constructed by authors from data at www.scopus.com.
    ILRI climate papers: before merger, 33; after merger, 289; Africa climate papers: 6; 468.
    ILRI livestock systems: 107; 407; Africa livestock systems: 83, 1,119.
    ILRI economics and policy: 56, 447. Africa economics and policy: 16, 269"),collapse = "")) +
     theme(plot.caption = element_text(hjust = 0)))

#
#CREATE THE TIFF FILES
#

# following code is commented out
# if you want to export the figures to tiff files then uncomment
#


# ggarrange(PREFACE_III_FIGURE_1, ncol = 1,nrow = 1,align = "h") %>%
#      ggexport(filename = paste(c(tles.Fig.PIII.1,".tiff"),collapse = ""))
#
# ggarrange(PREFACE_III_FIGURE_2, ncol = 1,nrow = 1,align = "h") %>%
#  ggexport(filename = paste(c(tles.Fig.PIII.2,".tiff"),collapse = ""))
#
# ggarrange(PREFACE_III_FIGURE_3, ncol = 1,nrow = 1,align = "h") %>%
#      ggexport(filename = paste(c(tles.Fig.PIII.3,".tiff"),collapse = ""))
#
# ggarrange(PREFACE_III_FIGURE_4, ncol = 1,nrow = 1,align = "h") %>%
#  ggexport(filename = paste(c(tles.Fig.PIII.4,".tiff"),collapse = ""))
#
# ggarrange(PREFACE_III_FIGURE_5a, nrow = 1,ncol = 1,align = "h") %>%
#  ggexport(filename = paste(c(tles.Fig.PIII.5a,".tiff"),collapse = ""))
#
# ggarrange(PREFACE_III_FIGURE_5b, nrow = 1,ncol = 1,align = "h") %>%
#  ggexport(filename = paste(c(tles.Fig.PIII.5b,".tiff"),collapse = ""))


#print the figure labels
Figure.titles.PIII <- as.character(mget(ls(pattern = "tles.Fig.PIII.")))

Figure.titles.PIII <- gsub("^\\.\\]","",Figure.titles.PIII)

# following code is commented out
# if you want to print the figure titles then uncomment
#

capture.output(print(as.character(Figure.titles.PIII),
                     print.gap = 1),file = "Preface III, Figure titles")
