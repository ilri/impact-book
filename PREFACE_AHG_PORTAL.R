#code to generate figures for the Preface on Animal Health and Genetics
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
library(gdata)
library(ggplot2)
library(extrafont)
library(extrafontdb)
library(ggthemes)
library(ggpubr)
library(grid)
library(scales)
library(graphics)
library(stringi)
library(stringr)
library(openxlsx)

library(gridExtra)
library(forcats)

library(janitor)

loadfonts(device = "win")
windowsFonts(Times = windowsFont("Times New Roman"))

#
#the spending data for preface AHG
#MAKE THE FIGURES FROM THE SPEND_CITES FILES
#first make the figure titles
#

tles.AHG.Fig.PI.1 <- c("Fig. PI.1. The decreasing importance of ILRI spending on theileriosis and trypanosomiasis, 1975-2018")

tles.AHG.Fig.PI.2a <- c("Fig.PI.2 (a) The decreasing ILRI share of global publications on trypanosomiasis, 1977-2018")

tles.AHG.Fig.PI.2b <- c("Fig.PI.2 (b) Frequency of citations of ILRI and global publications on trypanosomiasis, 1977-2018")

tles.AHG.Fig.PI.2c <- c("Fig.PI.2 (c) Domination of the field of trypanotolerance by ILRI papers, 1977-2018")

tles.AHG.Fig.PI.3a <- c("Fig.PI.3 (a) ILRI and other publications on Theileriosis and related problems, 1977-2018")

tles.AHG.Fig.PI.3b <- c("Fig.PI.3 (b) Frequency of citations of ILRI and other publications on Theileriosis and related problems, 1977-2018")

tles.AHG.Fig.PI.4a <- c("Fig.PI.4.(a) The domination of ILRI papers in global livestock immunology until recently, 1977-2018")

tles.AHG.Fig.PI.4b <- c("Fig.PI.4.(b) Frequency of citations of ILRI and other publications in immunology, 1977-2018")

tles.AHG.Fig.PI.5a <- c("Fig.PI.5 (a) The rapid increase in ILRI papers on animal genetics, food safety, transboundary diseases and zoonoses after the ILCA/ILRAD merger, 1977-2018")

tles.AHG.Fig.PI.5b <- c("Fig.PI.5 (b) Frequency of citations of ILRI publications on animal genetics, food safety, transboundary diseases and zoonoses, 1977-2018")

tles.AHG.Fig.PI.6 <- c("Fig. PI.6. Frequency of citations of ILRI and global institutions in animal health research by quantile, 1976-2018")

#label the colors-----------------------------
#
col_theil <- c("#E6E600FF")
col_tryps <- c("#ECB176FF")
col_total_ILRAD <- c("#682622DD")
col_management_and_other <- c("#00A600FF")
col_animprod_health_gen <- c("#ECB176FF")
col_ILRI <- c("#682622DD")
col_global <- c("#0000FFFF")
col_ECF <- c("#ECB176FF")
col_genetics <- c("#FF3E00FF")
col_fstz <- c("#FF9800FF")

#object names beginning with "pal_" are color palettes
#

# -----------------------------get the data

AHG_Spend_R <- read.xlsx("FINANCIAL_PORTAL.xlsx",sheet = "Spend_Cites_R",
     startRow = 1,colNames = T,na.strings = "NA")

bib_final_clean <- read.xlsx("BIBLIO_PORTAL.xlsx", sheet = "BIBLIO",
     startRow = 1)

AHG_Spend <- data.table(AHG_Spend_R)
AHG_Spend <- AHG_Spend %>% select(YEAR,
 `Total, all domains`  =  LIFETIME_TOTAL_SPEND,
 `Animal production, health and genetics`  =  ALL_AHG_SPEND,
 `Trypanosomiasis`  =  ALL_TRYPS_SPEND,
 `Theileriosis`  =  ALL_ECF_SPEND)

AHG_Spend_gather <- AHG_Spend %>%
  pivot_longer(c(`Total, all domains`,`Animal production, health and genetics`,
                 `Trypanosomiasis`,`Theileriosis`),
               names_to = "Domain",values_to  =  "Spending")
AHG_Spend_gather$Domain  =  factor(AHG_Spend_gather$Domain)
AHG_Spend_gather$Spending <- AHG_Spend_gather$Spending / 1000

AHG_Spend_gather <- AHG_Spend_gather %>%
  mutate(Domain = fct_relevel(Domain,"Theileriosis","Trypanosomiasis",
 "Animal production, health and genetics","Total, all domains"))

AHG_Spend_gather <- data.table(AHG_Spend_gather %>%
 mutate(Period  =  case_when(
 YEAR >=  1975 & YEAR <=  1980 ~ "1975-80",
 YEAR >=  1981 & YEAR <=  1985 ~ "1981-85",
 YEAR >=  1986 & YEAR <=  1990 ~ "1986-90",
 YEAR >=  1991 & YEAR <=  1994 ~ "1991-94",
 YEAR >=  1995 & YEAR <=  2000 ~ "1995-00",
 YEAR >=  2001 & YEAR <=  2005 ~ "2001-05",
 YEAR >=  2006 & YEAR <=  2010 ~ "2006-10",
 TRUE ~ "2011-18")))

AHG_Spend_gather$Period <- factor(AHG_Spend_gather$Period)
AHG_Spend_gather <- group_by(AHG_Spend_gather,Period,Domain) %>% summarise(sumDomain = sum(Spending))

pal_ahg_fig_1 <- c(col_theil,col_tryps,col_animprod_health_gen,col_total_ILRAD)
names(pal_ahg_fig_1) <- levels(AHG_Spend_gather$Domain)
(ahg_ILRI.spend <- filter(AHG_Spend_gather,Domain == "Total, all domains") %>%
     summarise(total = sum(sumDomain)))
(ahg.spend.total <- round(sum(ahg_ILRI.spend$total),digits = 1))

(FIGURE_PREFACE_AHG_1 <- ggplot(AHG_Spend_gather) +
    geom_col(mapping = aes(x = Period,y  = sumDomain, fill = Domain), position = "dodge", size = 1) +
    labs(x = "Period",y = "Spending in millions of 2015 US$",title = tles.AHG.Fig.PI.1) +
          scale_fill_manual(name = "Domain", values = pal_ahg_fig_1) +
          scale_y_continuous(labels = scales::comma) +
    theme_tufte() +
          labs(caption = paste(c("Constructed by authors from data in ILRI Annual Reports and Financial Reports, various years.
     Total, all domains spending (1975-2018) of ",ahg.spend.total," US$ millions, in 2015 US$ millions."),collapse = "")) +
          theme(plot.caption = element_text(hjust = 0)) +
          theme(text = element_text(size = 11,family = "Times"),legend.position = "bottom"))

#tryps analysis

TRYPS_GLOBAL_TC_PY <- read.xlsx("PREFACE_AHG_PORTAL.xlsx",
     sheet = "TRYPS_GLOBAL_BOTH_TC_PY",colNames = T)

TRYPS_GLOBAL_TC_PY <- filter(TRYPS_GLOBAL_TC_PY,PY >= 1977 & PY <=  2018)
TRYPS_GLOBAL_TC_PY <- select(TRYPS_GLOBAL_TC_PY,PY,Papers = TC.length,Citations = TC.sum)
TRYPS_GLOBAL_TC_PY$Institution <- as.factor("Global")

TRYPS_ILRI_TC_PY <- read.xlsx("PREFACE_AHG_PORTAL.xlsx",sheet = "TRYPS_ILRI_TC_PY",colNames = T)
TRYPS_ILRI_TC_PY <- filter(TRYPS_ILRI_TC_PY,PY >=  1977 & PY <=  2018)
TRYPS_ILRI_TC_PY <- select(TRYPS_ILRI_TC_PY,PY,Papers = TC.length,Citations = TC.sum)
TRYPS_ILRI_TC_PY$Institution <- as.factor("ILRI")

ALL_TRYPS_TC_PY <- rbind(TRYPS_GLOBAL_TC_PY,TRYPS_ILRI_TC_PY)
ALL_TRYPS_TC_PY <- select(ALL_TRYPS_TC_PY,PY,Institution,Papers,Citations)

ALL_TRYPS_TC_PY <- ALL_TRYPS_TC_PY %>%
     mutate(Period = case_when(
          PY >=  1977 & PY <=  1980 ~ "1977-80",
          PY >=  1981 & PY <=  1985 ~ "1981-85",
          PY >=  1986 & PY <=  1990 ~ "1986-90",
          PY >=  1991 & PY <=  1994 ~ "1991-94",
          PY >=  1995 & PY <=  2000 ~ "1995-00",
          PY >=  2001 & PY <=  2005 ~ "2001-05",
          PY >=  2006 & PY <=  2010 ~ "2006-10",
          TRUE ~ "2011-18"))

ALL_TRYPS_TC_PY$Period <- factor(ALL_TRYPS_TC_PY$Period)
ALL_TRYPS_TC_PY$PY <- factor(ALL_TRYPS_TC_PY$PY)

ALL_TRYPS_TC_PY <- ALL_TRYPS_TC_PY %>%
     mutate(Group_of_citations = case_when(
          Citations <=  20 ~ "1-20",
          Citations >=  21 & Citations <=  40 ~ "21-40",
          Citations >=  41 & Citations <=  80 ~ "41-80",
          Citations >=  81 & Citations <=  200 ~ "81-200",
          Citations >=  201 ~ "> 200"))

ALL_TRYPS_TC_PY$Group_of_citations <- factor(ALL_TRYPS_TC_PY$Group_of_citations)

ALL_TRYPS_TC_PY <- select(ALL_TRYPS_TC_PY,PY,Period,Institution,
 Group_of_citations,Papers,Citations)

ALL_TRYPS_TC_PY_gather <- ALL_TRYPS_TC_PY %>% pivot_longer(Papers:Citations,
 names_to = "Bibliometric",values_to = "Value")

ALL_TRYPS_TC_PY_gather <- ALL_TRYPS_TC_PY_gather %>%
        mutate(Institution = fct_relevel(Institution,"ILRI","Global"))

ALL_TRYPS_TC_PY_gather <- group_by(ALL_TRYPS_TC_PY_gather,Institution,Period,Bibliometric) %>%
        summarise(sumBiblio = sum(Value))

pal_ahg_fig_2a <- c(col_ILRI,col_global)
names(pal_ahg_fig_2a) <- levels(ALL_TRYPS_TC_PY$Institution)

ahg_ILRI.fig2.papers <- ALL_TRYPS_TC_PY_gather %>% filter(Bibliometric == "Papers" & Institution == "ILRI") %>%
     group_by(Period) %>% summarise(nPapers = sum(sumBiblio))
(ahg_ILRI.fig2.papers.total <- sum(ahg_ILRI.fig2.papers$nPapers))

ahg_global.fig2.papers <- ALL_TRYPS_TC_PY_gather %>% filter(Bibliometric == "Papers" & Institution == "Global") %>%
     group_by(Period) %>% summarise(nPapers = sum(sumBiblio,na.rm = TRUE))
(ahg_global.fig2.papers.total <- sum(ahg_global.fig2.papers$nPapers))

#figure 2a

(FIGURE_PREFACE_AHG_TRYPS_2a <- ggplot(data = ALL_TRYPS_TC_PY_gather) +
    geom_col(aes(x = Period,y = sumBiblio, fill = Institution), position = position_dodge(),size = 1) +
    facet_wrap(~Bibliometric,scales = "free_y",nrow = 2,ncol = 1) +
    labs(x = "Publication period",y = "Counts",title = tles.AHG.Fig.PI.2a) +
    scale_fill_manual(name = "Institution", values = pal_ahg_fig_2a) +
    scale_y_continuous(labels = scales::comma) +
    theme_tufte() +
    theme(text = element_text(size = 11,family = "Times"),legend.position = "bottom") +
    labs(caption = paste(c("Source: Constructed by authors from data at www.scopus.com. ILRI sample =  ", ahg_ILRI.fig2.papers.total," papers; Global sample =  ", ahg_global.fig2.papers.total," papers."),collapse = "")) +
    theme(plot.caption = element_text(hjust = 0)))

#FIGURE 2b
TRYPS_GLOBAL <- read.xlsx("PREFACE_AHG_PORTAL.xlsx",sheet = "bib_TRYPS_GLOBAL_BOTH",colNames = T)
TRYPS_GLOBAL <- filter(TRYPS_GLOBAL,PY >= 1977 & PY <=  2018)
TRYPS_GLOBAL <- select(TRYPS_GLOBAL,PY,Citations = TC)
TRYPS_GLOBAL$Institution <- as.factor("Global")

TRYPS_ILRI <- read.xlsx("PREFACE_AHG_PORTAL.xlsx",sheet = "bib_TRYPS_ILRI",colNames = T)
TRYPS_ILRI <- filter(TRYPS_ILRI,PY >=  1977 & PY <=  2018)
TRYPS_ILRI <- select(TRYPS_ILRI,PY,Citations = TC)
TRYPS_ILRI$Institution <- as.factor("ILRI")

ALL_TRYPS <- rbind(TRYPS_GLOBAL,TRYPS_ILRI)
ALL_TRYPS$Domain <- factor("Trypanosomiasis")
ALL_TRYPS$PY <- factor(ALL_TRYPS$PY)

ALL_TRYPS <- ALL_TRYPS %>%
     mutate(Group_of_citations = case_when(
          Citations <=  20 ~ "1-20",
          Citations >=  21 & Citations <=  40 ~ "21-40",
          Citations >=  41 & Citations <=  80 ~ "41-80",
          Citations >=  81 & Citations <=  200 ~ "81-200",
          Citations >=  201 ~ "> 200"))

ALL_TRYPS$Group_of_citations <- factor(ALL_TRYPS$Group_of_citations)
ALL_TRYPS <- ALL_TRYPS %>% mutate(Group_of_citations  =
fct_relevel(Group_of_citations,"1-20","21-40","41-80","81-200","> 200"))

ALL_TRYPS <- ALL_TRYPS %>% mutate(Institution = fct_relevel(Institution,"ILRI","Global"))

ALL_TRYPS_group <- group_by(ALL_TRYPS,Institution,Domain,Group_of_citations) %>%
     summarise(sumInstitution = length(Citations))

#figure 2b
#
pal_ahg_fig_2b <- pal_ahg_fig_2a
names(pal_ahg_fig_2b) <- levels(ALL_TRYPS_group$Institution)
sample_size.all.tryps <- length(ALL_TRYPS$Citations)

(FIGURE_PREFACE_AHG_TRYPS_2b <- ggplot(data = ALL_TRYPS_group) +
     geom_col(aes(x = Group_of_citations,y = sumInstitution, fill = Institution),
       position = position_dodge(),size = 1) +
     labs(x = "Ranges of citations per paper",y = "Counts of papers",title = tles.AHG.Fig.PI.2b) +
     scale_fill_manual(name = "Institution", values = pal_ahg_fig_2b) +
     scale_y_continuous(labels = scales::comma) +
     theme_tufte() +
     theme(text = element_text(size = 11,family = "Times"), legend.position = "bottom") +
     labs(caption = paste(c("Source: Constructed by authors from data at www.scopus.com; merged ILRI and Global sample size = ",sample_size.all.tryps," papers."),collapse = "")) +
     theme(plot.caption = element_text(hjust = 0)))

(tibble_ALL_TRYPS_ILRI_group <- ALL_TRYPS_group %>%
     group_by(Institution,Group_of_citations) %>% summarise(sumInst = sum(sumInstitution)))




#NOW DO THE TRYPANOTOLERANCE FIGURE 2c
#
TRYPANOTOLER_GLOBAL_TC_PY <- read.xlsx("PREFACE_AHG_PORTAL.xlsx",sheet = "TRYPANOTOLER_GLOBAL_TC_PY",colNames = T)
TRYPANOTOLER_GLOBAL_TC_PY <- filter(TRYPANOTOLER_GLOBAL_TC_PY,PY >= 1977 & PY <=  2018)
TRYPANOTOLER_GLOBAL_TC_PY <- select(TRYPANOTOLER_GLOBAL_TC_PY,PY,Papers = TC.length,Citations = TC.sum)
TRYPANOTOLER_GLOBAL_TC_PY$Institution <- as.factor("Global")

TRYPANOTOLER_ILRI_TC_PY <- read.xlsx("PREFACE_AHG_PORTAL.xlsx",sheet = "TRYPANOTOLER_ILRI_TC_PY",colNames = T)
TRYPANOTOLER_ILRI_TC_PY <- filter(TRYPANOTOLER_ILRI_TC_PY,PY >= 1977 & PY <=  2018)
TRYPANOTOLER_ILRI_TC_PY <- select(TRYPANOTOLER_ILRI_TC_PY,PY,Papers = TC.length,Citations = TC.sum)
TRYPANOTOLER_ILRI_TC_PY$Institution <- as.factor("ILRI")

ALL_TRYPANOTOLER_TC_PY <- rbind(TRYPANOTOLER_GLOBAL_TC_PY,TRYPANOTOLER_ILRI_TC_PY)
ALL_TRYPANOTOLER_TC_PY <- select(ALL_TRYPANOTOLER_TC_PY,PY,Institution,Papers,Citations)

ALL_TRYPANOTOLER_TC_PY <- ALL_TRYPANOTOLER_TC_PY %>%
  mutate(Period = case_when(
    PY >=  1977 & PY <=  1980 ~ "1977-80",
    PY >=  1981 & PY <=  1985 ~ "1981-85",
    PY >=  1986 & PY <=  1990 ~ "1986-90",
    PY >=  1991 & PY <=  1994 ~ "1991-94",
    PY >=  1995 & PY <=  2000 ~ "1995-00",
    PY >=  2001 & PY <=  2005 ~ "2001-05",
    PY >=  2006 & PY <=  2010 ~ "2006-10",
    TRUE ~ "2011-18"))

ALL_TRYPANOTOLER_TC_PY$Period <- factor(ALL_TRYPANOTOLER_TC_PY$Period)

ALL_TRYPANOTOLER_TC_PY <- ALL_TRYPANOTOLER_TC_PY %>%
  mutate(Group_of_citations = case_when(
    Citations <=  20 ~ "1-20",
    Citations >=  21 & Citations <=  40 ~ "21-40",
    Citations >=  41 & Citations <=  80 ~ "41-80",
    Citations >=  81 & Citations <=  200 ~ "81-200",
    Citations >=  201 ~ "> 200"))

ALL_TRYPANOTOLER_TC_PY <- select(ALL_TRYPANOTOLER_TC_PY,PY,Period,Institution,
                           Group_of_citations,Papers,Citations)

ALL_TRYPANOTOLER_TC_PY_group <- ALL_TRYPANOTOLER_TC_PY %>%
  pivot_longer(Papers:Citations,
 names_to = "Bibliometric",values_to = "Value")

ALL_TRYPANOTOLER_TC_PY_group <- ALL_TRYPANOTOLER_TC_PY_group %>%
  mutate(Institution = fct_relevel(Institution,"ILRI","Global"))

ALL_TRYPANOTOLER_TC_PY_group <- group_by(ALL_TRYPANOTOLER_TC_PY_group,Institution,Period,Bibliometric) %>%
  summarise(sumDomain = sum(Value))

pal_ahg_fig_2c <- pal_ahg_fig_2a
names(pal_ahg_fig_2c) <- levels(ALL_TRYPANOTOLER_TC_PY_group$Institution)

ahg_ILRI.fig2c.papers <- ALL_TRYPANOTOLER_TC_PY_group %>% filter(Bibliometric == "Papers" & Institution == "ILRI") %>%
     group_by(Period) %>% summarise(nPapers = sum(sumDomain))
(ahg_ILRI.fig2c.papers.total <- sum(ahg_ILRI.fig2c.papers$nPapers))

ahg_global.fig2c.papers <- ALL_TRYPANOTOLER_TC_PY_group %>% filter(Bibliometric == "Papers" & Institution == "Global") %>%
     group_by(Period) %>% summarise(nPapers = sum(sumDomain,na.rm = TRUE))
(ahg_global.fig2c.papers.total <- sum(ahg_global.fig2c.papers$nPapers))

(FIGURE_PREFACE_AHG_TRYPANOTOLER_2c <- ggplot(data = ALL_TRYPANOTOLER_TC_PY_group) +
    geom_col(aes(x = Period,y = sumDomain, fill = Institution), position = position_dodge(),size = 1) +
    facet_wrap(~Bibliometric,scales = "free_y",nrow = 2,ncol = 1) +
    labs(x = "Publication period",y = "Counts",title = tles.AHG.Fig.PI.2c) +
    scale_fill_manual(name = "Institution", values = pal_ahg_fig_2c ) +
    scale_y_continuous(labels = scales::comma) +
          theme_tufte() +
          theme(text = element_text(size = 11,family = "Times"),legend.position = "bottom") +
          labs(caption = paste(c("Source: Constructed by authors from data at www.scopus.com. ILRI sample =  ", ahg_ILRI.fig2c.papers.total," papers; the global sample =  ", ahg_global.fig2c.papers.total," papers."),collapse = "")) +
          theme(plot.caption = element_text(hjust = 0)))

#ECF

bib_ECF_GLOBAL <- read.xlsx("PREFACE_AHG_PORTAL.xlsx",sheet = "bib_ECF_GLOBAL",colNames = T)
ECF_GLOBAL_TC_PY <- data.frame(summaryBy(TC ~ PY, data = bib_ECF_GLOBAL, FUN = c(length,sum)))

bib_ECF_ILRI <- read.xlsx("PREFACE_AHG_PORTAL.xlsx",sheet = "bib_ECF_ILRI", colNames = T)
ECF_ILRI_TC_PY <- data.frame(summaryBy(TC ~ PY, data = bib_ECF_ILRI, FUN = c(length,sum)))

ECF_GLOBAL_TC_PY <- filter(ECF_GLOBAL_TC_PY,PY >= 1977 & PY <= 2018)
ECF_GLOBAL_TC_PY <- select(ECF_GLOBAL_TC_PY,PY,Papers = TC.length,Citations = TC.sum)
ECF_GLOBAL_TC_PY$Institution <- as.factor("Global")

ECF_ILRI_TC_PY <- filter(ECF_ILRI_TC_PY,PY >= 1977 & PY <= 2018)
ECF_ILRI_TC_PY <- select(ECF_ILRI_TC_PY,PY,Papers = TC.length,Citations = TC.sum)
ECF_ILRI_TC_PY$Institution <- as.factor("ILRI")

ALL_ECF_TC_PY <- rbind(ECF_GLOBAL_TC_PY,ECF_ILRI_TC_PY)
ALL_ECF_TC_PY <- select(ALL_ECF_TC_PY,PY,Institution,Papers,Citations)

ALL_ECF_TC_PY <- ALL_ECF_TC_PY %>%
        mutate(Period = case_when(
                PY >=  1977 & PY <=  1980 ~ "1977-80",
                PY >=  1981 & PY <=  1985 ~ "1981-85",
                PY >=  1986 & PY <=  1990 ~ "1986-90",
                PY >=  1991 & PY <=  1994 ~ "1991-94",
                PY >=  1995 & PY <=  2000 ~ "1995-00",
                PY >=  2001 & PY <=  2005 ~ "2001-05",
                PY >=  2006 & PY <=  2010 ~ "2006-10",
                TRUE ~ "2011-18"))

ALL_ECF_TC_PY$Period <- factor(ALL_ECF_TC_PY$Period)

ALL_ECF_TC_PY <- ALL_ECF_TC_PY %>%
        mutate(Group_of_citations = case_when(
             Citations <=  20 ~ "1-20",
             Citations >=  21 & Citations <=  40 ~ "21-40",
             Citations >=  41 & Citations <=  80 ~ "41-80",
             Citations >=  81 & Citations <=  200 ~ "81-200",
             Citations >=  201 ~ "> 200"))

ALL_ECF_TC_PY_group <- select(ALL_ECF_TC_PY,PY,Period,Institution,
                    Group_of_citations,Papers,Citations)

ALL_ECF_TC_PY_group <- ALL_ECF_TC_PY_group %>% pivot_longer(Papers:Citations,
 names_to = "Bibliometric",values_to = "Value")

ALL_ECF_TC_PY_group <- ALL_ECF_TC_PY_group %>%
        mutate(Institution = fct_relevel(Institution,"ILRI","Global"))

ALL_ECF_TC_PY_group <- group_by(ALL_ECF_TC_PY_group,Institution,Period,Bibliometric) %>%
        summarise(sumDomain = sum(Value))

pal_ahg_fig_3a <- c(col_ILRI,col_global)
names(pal_ahg_fig_3a) <- levels(ALL_ECF_TC_PY_group$Institution)

ahg_ILRI.fig3a.papers <- ALL_ECF_TC_PY_group %>% filter(Bibliometric == "Papers" & Institution == "ILRI") %>%
     group_by(Period) %>% summarise(nPapers = sum(sumDomain))
(ahg_ILRI.fig3a.papers.total <- sum(ahg_ILRI.fig3a.papers$nPapers))

ahg_global.fig3a.papers <- ALL_ECF_TC_PY_group %>% filter(Bibliometric == "Papers" & Institution == "Global") %>%
     group_by(Period) %>% summarise(nPapers = sum(sumDomain,na.rm = TRUE))
(ahg_global.fig3a.papers.total <- sum(ahg_global.fig3a.papers$nPapers))

#figure 3a on ECF

(FIGURE_PREFACE_AHG_ECF_3a <- ggplot(data = ALL_ECF_TC_PY_group) +
  geom_col(aes(x = Period,y = sumDomain, fill = Institution), position = position_dodge(),size = 1) +
 facet_wrap(~Bibliometric,scales = "free_y",nrow = 2,ncol = 1) +
 labs(x = "Publication period",y = "Counts",title = tles.AHG.Fig.PI.3a) +
  scale_fill_manual(name = "Institution", values = pal_ahg_fig_3a) +
    scale_y_continuous(labels = scales::comma) +
          theme_tufte() +
          theme(text = element_text(size = 11,family = "Times"),legend.position = "bottom") +
          labs(caption = paste(c("Source: Constructed by authors from data at www.scopus.com. ILRI sample =  ", ahg_ILRI.fig3a.papers.total," papers; the global sample =  ", ahg_global.fig3a.papers.total," papers."),collapse = "")) +
          theme(plot.caption = element_text(hjust = 0)))

#now do the figure Fig. PI.3(b)
ECF_GLOBAL <- read.xlsx("PREFACE_AHG_PORTAL.xlsx",sheet = "bib_ECF_GLOBAL",colNames = T)
ECF_GLOBAL <- filter(ECF_GLOBAL,PY >= 1977 & PY <=  2018)
ECF_GLOBAL <- select(ECF_GLOBAL,PY,Citations = TC)
ECF_GLOBAL <- arrange(ECF_GLOBAL,PY)
ECF_GLOBAL$Institution <- as.factor("Global")

ECF_ILRI <- read.xlsx("PREFACE_AHG_PORTAL.xlsx",sheet = "bib_ECF_ILRI",colNames = T)
ECF_ILRI <- filter(ECF_ILRI,PY >=  1977 & PY <=  2018)
ECF_ILRI <- select(ECF_ILRI,PY,Citations = TC)
ECF_ILRI$Institution <- as.factor("ILRI")

ALL_ECF <- rbind(ECF_GLOBAL,ECF_ILRI)
ALL_ECF$Domain <- factor("Theileriosis")
ALL_ECF$PY <- factor(ALL_ECF$PY)

ALL_ECF <- ALL_ECF %>%
  mutate(Group_of_citations = case_when(
    Citations <=  20 ~ "1-20",
    Citations >=  21 & Citations <=  40 ~ "21-40",
    Citations >=  41 & Citations <=  80 ~ "41-80",
    Citations >=  81 & Citations <=  200 ~ "81-200",
    Citations >=  201 ~ "> 200"))

ALL_ECF$Group_of_citations <- factor(ALL_ECF$Group_of_citations)
ALL_ECF <- ALL_ECF %>% mutate(Group_of_citations  =
 fct_relevel(Group_of_citations,"1-20","21-40","41-80","81-200","> 200"))

ALL_ECF <- ALL_ECF %>% mutate(Institution = fct_relevel(Institution,"ILRI","Global"))

ALL_ECF_group <- group_by(ALL_ECF,Institution,Domain,Group_of_citations) %>%
  summarise(sumInstitution = length(Citations))

pal_ahg_fig_3b <- pal_ahg_fig_3a
names(pal_ahg_fig_3b) <- levels(ALL_ECF_TC_PY_group$Institution)
sample_size.all.ECF <- length(ALL_ECF$Citations)

#FIGURE 3b

(FIGURE_PREFACE_AHG_ECF_3b <- ggplot(data = ALL_ECF_group) +
    geom_col(aes(x = Group_of_citations,y = sumInstitution, fill = Institution),
             position = position_dodge(),size = 1) +
    labs(x = "Ranges of citations per paper",y = "Counts of papers",title = tles.AHG.Fig.PI.3b) +
    scale_y_continuous(labels = scales::comma) +
    scale_fill_manual(name = "Institution", values = pal_ahg_fig_3b) +
    theme_tufte() +
    theme(text = element_text(size = 11,family = "Times"), legend.position = "bottom") +
    labs(caption = paste(c("Source: Constructed by authors from data at www.scopus.com;  merged ILRI and Global sample size = ",sample_size.all.ECF," papers."),collapse = "")) +
    theme(plot.caption = element_text(hjust = 0)))

(tibble_ALL_ECF_group <- ALL_ECF_group %>%
          group_by(Institution,Group_of_citations) %>% summarise(sumInst = sum(sumInstitution)))


#now Fig I.4 (a) on other diseases and genetics
##now figure 4a and 4b on immunology
IMMUNO_GLOBAL_TC_PY <- read.xlsx("PREFACE_AHG_PORTAL.xlsx",sheet = "IMMUNO_GLOBAL_TC_PY",colNames = T)
IMMUNO_GLOBAL_TC_PY <- filter(IMMUNO_GLOBAL_TC_PY,PY >= 1977 & PY <=  2018)
IMMUNO_GLOBAL_TC_PY <- select(IMMUNO_GLOBAL_TC_PY,PY,Papers = TC.length,Citations = TC.sum)
IMMUNO_GLOBAL_TC_PY$Institution <- as.factor("Global")

IMMUNO_ILRI_TC_PY <- read.xlsx("PREFACE_AHG_PORTAL.xlsx",sheet = "IMMUNO_ILRI_TC_PY",colNames = T)
IMMUNO_ILRI_TC_PY <- filter(IMMUNO_ILRI_TC_PY,PY >=  1977 & PY <=  2018)
IMMUNO_ILRI_TC_PY <- select(IMMUNO_ILRI_TC_PY,PY,Papers = TC.length,Citations = TC.sum)
IMMUNO_ILRI_TC_PY$Institution <- as.factor("ILRI")

AHG_IMMUNO_TC_PY <- rbind(IMMUNO_GLOBAL_TC_PY,IMMUNO_ILRI_TC_PY)
AHG_IMMUNO_TC_PY <- select(AHG_IMMUNO_TC_PY,PY,Institution,Papers,Citations)

AHG_IMMUNO_TC_PY <- AHG_IMMUNO_TC_PY %>%
     mutate(Period = case_when(
          PY >=  1977 & PY <=  1980 ~ "1977-80",
          PY >=  1981 & PY <=  1985 ~ "1981-85",
          PY >=  1986 & PY <=  1990 ~ "1986-90",
          PY >=  1991 & PY <=  1994 ~ "1991-94",
          PY >=  1995 & PY <=  2000 ~ "1995-00",
          PY >=  2001 & PY <=  2005 ~ "2001-05",
          PY >=  2006 & PY <=  2010 ~ "2006-10",
          TRUE ~ "2011-18"))

AHG_IMMUNO_TC_PY$Period <- factor(AHG_IMMUNO_TC_PY$Period)
AHG_IMMUNO_TC_PY$PY <- factor(AHG_IMMUNO_TC_PY$PY)

AHG_IMMUNO_TC_PY <- AHG_IMMUNO_TC_PY %>%
     mutate(Group_of_citations = case_when(
          Citations <=  20 ~ "1-20",
          Citations >=  21 & Citations <=  40 ~ "21-40",
          Citations >=  41 & Citations <=  80 ~ "41-80",
          Citations >=  81 & Citations <=  200 ~ "81-200",
          Citations >=  201 ~ "> 200"))

AHG_IMMUNO_TC_PY$Group_of_citations <- factor(AHG_IMMUNO_TC_PY$Group_of_citations)

AHG_IMMUNO_TC_PY <- select(AHG_IMMUNO_TC_PY,PY,Period,Institution,
                           Group_of_citations,Papers,Citations)

AHG_IMMUNO_TC_PY_group <- AHG_IMMUNO_TC_PY %>% pivot_longer(Papers:Citations,
                                                            names_to = "Bibliometric",values_to = "Value")

AHG_IMMUNO_TC_PY_group <- AHG_IMMUNO_TC_PY_group %>%
     mutate(Institution = fct_relevel(Institution,"ILRI","Global"))

AHG_IMMUNO_TC_PY_group <- group_by(AHG_IMMUNO_TC_PY_group,Institution,Period,
                                   Group_of_citations,Bibliometric) %>% summarise(sumDomain = sum(Value))

pal_ahg_fig_4a <- c(col_ILRI,col_global)
names(pal_ahg_fig_4a) <- levels(AHG_IMMUNO_TC_PY_group$Institution)

ahg_ILRI.fig4a.papers <- AHG_IMMUNO_TC_PY_group %>% filter(Bibliometric == "Papers" & Institution == "ILRI") %>%
     group_by(Period) %>% summarise(nPapers = sum(sumDomain))
(ahg_ILRI.fig4a.papers.total <- sum(ahg_ILRI.fig4a.papers$nPapers))

ahg_global.fig4a.papers <- AHG_IMMUNO_TC_PY_group %>% filter(Bibliometric == "Papers" & Institution == "Global") %>%
     group_by(Period) %>% summarise(nPapers = sum(sumDomain,na.rm = TRUE))
(ahg_global.fig4a.papers.total <- sum(ahg_global.fig4a.papers$nPapers))

(FIGURE_PREFACE_AHG_IMMUNO_TC_PY_4a <- ggplot(data = AHG_IMMUNO_TC_PY_group) +
          geom_col(aes(x = Period,y = sumDomain,fill = Institution ),
                   position = position_dodge(),size = 1) +
          facet_wrap(~ Bibliometric,scales = "free_y",nrow = 2,ncol = 1) +
          labs(x = "Publication period",y = "Counts",title = tles.AHG.Fig.PI.4a) +
          scale_fill_manual(name = "Institution", values = pal_ahg_fig_4a) +
          scale_y_continuous(labels = scales::comma) +
          theme_tufte() +
          theme(text = element_text(size = 11,family = "Times"), legend.position = "bottom") +
          labs(caption = paste(c("Source: Constructed by authors from data at www.scopus.com. ILRI sample =  ", ahg_ILRI.fig4a.papers.total," papers; the global sample =  ", ahg_global.fig4a.papers.total," papers."),collapse = "")) +
          theme(plot.caption = element_text(hjust = 0)))

#----------------------------------------------------
IMMUNO_GLOBAL <- read.xlsx("PREFACE_AHG_PORTAL.xlsx",sheet = "bib_IMMUNO_GLOBAL",colNames = T)
IMMUNO_GLOBAL <- filter(IMMUNO_GLOBAL,PY >= 1977 & PY <=  2018)
IMMUNO_GLOBAL <- select(IMMUNO_GLOBAL,PY,Citations = TC)
IMMUNO_GLOBAL$Institution <- as.factor("Global")

IMMUNO_ILRI <- read.xlsx("PREFACE_AHG_PORTAL.xlsx", sheet = "bib_IMMUNO_ILRI",colNames = T)
IMMUNO_ILRI <- filter(IMMUNO_ILRI,PY >=  1977 & PY <=  2018)
IMMUNO_ILRI <- select(IMMUNO_ILRI,PY,Citations = TC)
IMMUNO_ILRI$Institution <- as.factor("ILRI")

ALL_IMMUNO <- rbind(IMMUNO_GLOBAL,IMMUNO_ILRI)
ALL_IMMUNO$Domain <- factor("Theileriosis")
ALL_IMMUNO$PY <- factor(ALL_IMMUNO$PY)

ALL_IMMUNO$Citations <- as.numeric(ALL_IMMUNO$Citations)

ALL_IMMUNO <- ALL_IMMUNO %>%
     mutate(Group_of_citations = case_when(
          Citations <=  20 ~ "1-20",
          Citations >=  21 & Citations <=  40 ~ "21-40",
          Citations >=  41 & Citations <=  80 ~ "41-80",
          Citations >=  81 & Citations <=  200 ~ "81-200",
          Citations >=  201 ~ "> 200"))

ALL_IMMUNO$Group_of_citations <- factor(ALL_IMMUNO$Group_of_citations)
ALL_IMMUNO <- ALL_IMMUNO %>% mutate(Group_of_citations  = fct_relevel(Group_of_citations,"1-20","21-40","41-80","81-200","> 200"))

ALL_IMMUNO <- ALL_IMMUNO %>% mutate(Institution = fct_relevel(Institution,"ILRI","Global"))

ALL_IMMUNO_group <- group_by(ALL_IMMUNO,Institution,Domain,Group_of_citations) %>%
     summarise(sumInstitution = length(Citations))

pal_ahg_fig_4b <- c(col_ILRI,col_global)
names(pal_ahg_fig_4b) <- levels(AHG_IMMUNO_TC_PY_group$Institution)
sample_size.all.immuno <- length(ALL_IMMUNO$Citations)

#now do figure 4b

(FIGURE_PREFACE_AHG_IMMUNO_4b <- ggplot(data = ALL_IMMUNO_group) +
          geom_col(aes(x = Group_of_citations,y = sumInstitution, fill = Institution),
                   position = position_dodge(),size = 1) +
          labs(x = "Ranges of citations per paper",y = "Counts of papers",title = tles.AHG.Fig.PI.4b) +
          scale_fill_manual(name = "Institution", values = pal_ahg_fig_4b) +
          scale_y_continuous(labels = scales::comma) +
          theme_tufte() +
          theme(text = element_text(size = 11,family = "Times"), legend.position = "bottom") +
          labs(caption = paste(c("Source: Constructed by authors from data at www.scopus.com;  merged ILRI and Global sample size = ",sample_size.all.immuno," papers."),collapse = "")) +
          theme(plot.caption = element_text(hjust = 0)))

#genetics and other research ---------------------------------------
GENE_ILRI_TC_PY <- read.xlsx("PREFACE_AHG_PORTAL.xlsx",sheet = "GENE_ILRI_TC_PY",colNames = T)
GENE_ILRI_TC_PY <- filter(GENE_ILRI_TC_PY,PY >= 1977 & PY <=  2018)
GENE_ILRI_TC_PY <- select(GENE_ILRI_TC_PY,PY,Papers = TC.length,Citations = TC.sum)
GENE_ILRI_TC_PY$Domain <- as.factor("Genetics")

OTHERDIS_ILRI_TC_PY <- read.xlsx("PREFACE_AHG_PORTAL.xlsx",sheet = "OTHERDIS_ILRI_TC_PY",colNames = T)
OTHERDIS_ILRI_TC_PY <- filter(OTHERDIS_ILRI_TC_PY,PY >= 1977 & PY <=  2018)
OTHERDIS_ILRI_TC_PY <- select(OTHERDIS_ILRI_TC_PY,PY,Papers = TC.length,Citations = TC.sum)
OTHERDIS_ILRI_TC_PY$Domain <- as.factor("Food safety, transboundary, zoonoses")

ALL_GENE_OTHERDIS_TC_PY <- rbind(GENE_ILRI_TC_PY,OTHERDIS_ILRI_TC_PY)
ALL_GENE_OTHERDIS_TC_PY <- select(ALL_GENE_OTHERDIS_TC_PY,PY,Domain,Papers,Citations)

ALL_GENE_OTHERDIS_TC_PY <- ALL_GENE_OTHERDIS_TC_PY %>%
        mutate(Period = case_when(
                PY >=  1977 & PY <=  1980 ~ "1977-80",
                PY >=  1981 & PY <=  1985 ~ "1981-85",
                PY >=  1986 & PY <=  1990 ~ "1986-90",
                PY >=  1991 & PY <=  1994 ~ "1991-94",
                PY >=  1995 & PY <=  2000 ~ "1995-00",
                PY >=  2001 & PY <=  2005 ~ "2001-05",
                PY >=  2006 & PY <=  2010 ~ "2006-10",
                TRUE ~ "2011-18"))

ALL_GENE_OTHERDIS_TC_PY$Period <- factor(ALL_GENE_OTHERDIS_TC_PY$Period)

ALL_GENE_OTHERDIS_TC_PY <- ALL_GENE_OTHERDIS_TC_PY %>%
        mutate(Group_of_citations = case_when(
             Citations <=  20 ~ "1-20",
             Citations >=  21 & Citations <=  40 ~ "21-40",
             Citations >=  41 & Citations <=  80 ~ "41-80",
             Citations >=  81 & Citations <=  200 ~ "81-200",
             Citations >=  201 ~ "> 200"))

ALL_GENE_OTHERDIS_TC_PY <- select(ALL_GENE_OTHERDIS_TC_PY,PY,Period,Domain,
     Group_of_citations,Papers,Citations)

ALL_GENE_OTHERDIS_TC_PY_group <- ALL_GENE_OTHERDIS_TC_PY %>% pivot_longer(Papers:Citations,
 names_to = "Bibliometric",values_to = "Value")

ALL_GENE_OTHERDIS_TC_PY_group <- group_by(ALL_GENE_OTHERDIS_TC_PY_group,
Domain,Period,Bibliometric) %>% summarise(sumDomain = sum(Value))

#HERE HERE
pal_ahg_fig_5a <- c(col_genetics,col_fstz)
names(pal_ahg_fig_5a) <- levels(ALL_GENE_OTHERDIS_TC_PY_group$Domain)

ahg_ILRI.gen.fig5a.papers <- ALL_GENE_OTHERDIS_TC_PY_group %>% filter(Bibliometric == "Papers" & Domain == "Genetics") %>%
     group_by(Period) %>% summarise(nPapers = sum(sumDomain))
(ahg_ILRI.gen.fig5a.papers.total <- sum(ahg_ILRI.gen.fig5a.papers$nPapers))

ahg_ILRI.fstz.fig5a.papers <- ALL_GENE_OTHERDIS_TC_PY_group %>% filter(Bibliometric == "Papers" & Domain == "Food safety, transboundary, zoonoses") %>%
     group_by(Period) %>% summarise(nPapers = sum(sumDomain,na.rm = TRUE))
(ahg_ILRI.fstz.fig5a.papers.total <- sum(ahg_ILRI.fstz.fig5a.papers$nPapers))

#now do  figure 5a

(FIGURE_PREFACE_AHG_GENE_OTHERDIS_5a <- ggplot(data = ALL_GENE_OTHERDIS_TC_PY_group) +
 geom_col(aes(x = Period,y = sumDomain, fill = Domain),position = position_dodge(),size = 1) +
 facet_wrap(~Bibliometric,scales = "free_y",nrow = 2,ncol = 1) +
     labs(x = "Publication period",y = "Counts",title = tles.AHG.Fig.PI.5a) +
     scale_fill_manual(name = "Domain", values = pal_ahg_fig_5a) +
          scale_y_continuous(labels = scales::comma) +
     theme_tufte() +
     theme(text = element_text(size = 10,family = "Times"), legend.position = "bottom") +
          labs(caption = paste(c("Source: Constructed by authors from data at www.scopus.com.
      'Genetics' sample =  ", ahg_ILRI.gen.fig5a.papers.total," papers;
    the 'food safety, transboundary, zoonoses' sample =  ", ahg_ILRI.fstz.fig5a.papers.total," papers."),collapse = "")) +
          theme(plot.caption = element_text(hjust = 0)))

#
#now do figure Fig. PI.5 (b)
#

GENE_ILRI <- read.xlsx("PREFACE_AHG_PORTAL.xlsx",sheet = "bib_GENE_ILRI",colNames = T)
GENE_ILRI <- filter(GENE_ILRI,PY >= 1977 & PY <=  2018)
GENE_ILRI <- select(GENE_ILRI,PY,Citations = TC)
GENE_ILRI$Domain <- as.factor("Genetics")

OTHERDIS_ILRI <- read.xlsx("PREFACE_AHG_PORTAL.xlsx",sheet = "bib_OTHERDIS_ILRI",colNames = T)
OTHERDIS_ILRI <- filter(OTHERDIS_ILRI,PY >=  1977 & PY <=  2018)
OTHERDIS_ILRI <- select(OTHERDIS_ILRI,PY,Citations = TC)
OTHERDIS_ILRI$Domain <- as.factor("Food safety, transboundary, zoonoses")

ALL_GENE_OTHERDIS <- rbind(GENE_ILRI,OTHERDIS_ILRI)
ALL_GENE_OTHERDIS$PY <- factor(ALL_GENE_OTHERDIS$PY)

ALL_GENE_OTHERDIS <- ALL_GENE_OTHERDIS %>%
  mutate(Group_of_citations = case_when(
    Citations <=  20 ~ "1-20",
    Citations >=  21 & Citations <=  40 ~ "21-40",
    Citations >=  41 & Citations <=  80 ~ "41-80",
    Citations >=  81 & Citations <=  200 ~ "81-200",
    Citations >=  201 ~ "> 200"))

ALL_GENE_OTHERDIS$Group_of_citations <- factor(ALL_GENE_OTHERDIS$Group_of_citations)
ALL_GENE_OTHERDIS <- ALL_GENE_OTHERDIS %>% mutate(Group_of_citations  =
 fct_relevel(Group_of_citations,"1-20","21-40","41-80","81-200","> 200"))

ALL_GENE_OTHERDIS_group <- group_by(ALL_GENE_OTHERDIS,Domain,
Group_of_citations) %>% summarise(sumDomain = length(Citations))

pal_ahg_fig_5b <- c(col_genetics,col_fstz)
names(pal_ahg_fig_5b) <- levels(ALL_GENE_OTHERDIS_group$Domain)
sample_size.all.other <- length(ALL_GENE_OTHERDIS$Citations)


(FIGURE_PREFACE_AHG_GENE_OTHERDIS_5b <- ggplot(data = ALL_GENE_OTHERDIS_group) +
    geom_col(aes(x = Group_of_citations,y = sumDomain, fill = Domain),
             position = position_dodge(),size = 1) +
    labs(x = "Ranges of citations per paper by publication period",y = "Counts of papers",title = tles.AHG.Fig.PI.5b) +
    scale_fill_manual(name = "Domain", values = pal_ahg_fig_5b) +
          scale_y_continuous(labels = scales::comma) +
          theme_tufte() +
          theme(text = element_text(size = 11,family = "Times"), legend.position = "bottom") +
          labs(caption = paste(c("Source: Constructed by authors from data at www.scopus.com; merged sample size = ",sample_size.all.other," papers."),collapse = "")) +
    theme(plot.caption = element_text(hjust = 0)))

#now get the top 5% shares of all papers with Citations > 9
#start with TRYPs
#

TRYPS_GLOBAL_BOTH <- read.xlsx("PREFACE_AHG_PORTAL.xlsx", sheet = "bib_TRYPS_GLOBAL_BOTH",colNames = T)
TRYPS_GLOBAL_BOTH <- select(TRYPS_GLOBAL_BOTH,PY = PY,Citations = TC)
TRYPS_ILRI <- read.xlsx("PREFACE_AHG_PORTAL.xlsx",sheet = "bib_TRYPS_ILRI",colNames = T)
TRYPS_ILRI <- select(TRYPS_ILRI,PY = PY,Citations = TC)
TRYPS_GLOBAL_BOTH$Institution <- factor("Global")
TRYPS_ILRI$Institution <- factor("ILRI")

ALL_TRYPS <- rbind(TRYPS_GLOBAL_BOTH,TRYPS_ILRI)
ALL_TRYPS <- filter(ALL_TRYPS, PY >=  1977 & PY <=  2018 & Citations > 9)

ALL_TRYPS_pr <- quantile(ALL_TRYPS$Citations, probs = c(0.25,0.5,0.75,0.95))

ALL_TRYPS <- ALL_TRYPS %>%
  mutate(Quants = case_when(
    Citations <=   ALL_TRYPS_pr[1] ~ "first",
    Citations <=   ALL_TRYPS_pr[2] & Citations >=  ALL_TRYPS_pr[1] ~ "second",
    Citations <=   ALL_TRYPS_pr[3] & Citations >=  ALL_TRYPS_pr[2] ~ "third",
    Citations <=   ALL_TRYPS_pr[4] & Citations >=  ALL_TRYPS_pr[3] ~ "fourth",
    Citations >   ALL_TRYPS_pr[4]  ~ "top 5%"))


#now get ECF
bib_ECF_GLOBAL <- data.table(read.xlsx("PREFACE_AHG_PORTAL.xlsx",sheet = "bib_ECF_GLOBAL",colNames = T))
bib_ECF_ILRI <- data.table(read.xlsx("PREFACE_AHG_PORTAL.xlsx",sheet = "bib_ECF_ILRI", colNames = T))
bib_ECF_GLOBAL$Institution <- factor("Global")
bib_ECF_ILRI$Institution <- factor("ILRI")

ALL_ECF <- rbind(bib_ECF_GLOBAL,bib_ECF_ILRI)
ALL_ECF <- select(ALL_ECF,PY,Citations = TC,Institution)
ALL_ECF <- filter(ALL_ECF, PY >=  1977 & PY <=  2018 & Citations > 9)

ALL_ECF_pr <- quantile(ALL_ECF$Citations, probs = c(0.25,0.5,0.75,0.95))

ALL_ECF <- ALL_ECF %>%
  mutate(Quants = case_when(
    Citations <=   ALL_ECF_pr[1] ~ "first",
    Citations <=   ALL_ECF_pr[2] & Citations >=  ALL_ECF_pr[1] ~ "second",
    Citations <=   ALL_ECF_pr[3] & Citations >=  ALL_ECF_pr[2] ~ "third",
    Citations <=   ALL_ECF_pr[4] & Citations >=  ALL_ECF_pr[3] ~ "fourth",
    Citations >   ALL_ECF_pr[4]  ~ "top 5%"))


#now get IMMUNOLOGY

IMMUNO_ILRI <- read.xlsx("PREFACE_AHG_PORTAL.xlsx",sheet = "bib_IMMUNO_ILRI",colNames = T)
IMMUNO_GLOBAL <- read.xlsx("PREFACE_AHG_PORTAL.xlsx",sheet = "bib_IMMUNO_GLOBAL",colNames = T)
IMMUNO_ILRI <- IMMUNO_ILRI %>% select(PY,Citations = TC)
IMMUNO_ILRI$Citations <- as.integer(IMMUNO_ILRI$Citations)
IMMUNO_GLOBAL <- IMMUNO_GLOBAL %>% select(PY,Citations = TC)
IMMUNO_GLOBAL$Citations <- as.integer(IMMUNO_GLOBAL$Citations)
IMMUNO_ILRI$Institution <- c("ILRI")
IMMUNO_GLOBAL$Institution <- c("Global")

ALL_IMMUNO <- rbind(IMMUNO_ILRI,IMMUNO_GLOBAL)

ALL_IMMUNO <- filter(ALL_IMMUNO,PY >=  1977 & PY <=  2018 & Citations > 9)

ALL_IMMUNO_pr <- quantile(ALL_IMMUNO$Citations, probs = c(0.25,0.5,0.75,0.95))

ALL_IMMUNO <- ALL_IMMUNO %>%
  mutate(Quants = case_when(
    Citations <=   ALL_IMMUNO_pr[1] ~ "first",
    Citations <=   ALL_IMMUNO_pr[2] & Citations >=  ALL_IMMUNO_pr[1] ~ "second",
    Citations <=   ALL_IMMUNO_pr[3] & Citations >=  ALL_IMMUNO_pr[2] ~ "third",
    Citations <=   ALL_IMMUNO_pr[4] & Citations >=  ALL_IMMUNO_pr[3] ~ "fourth",
    Citations  >  ALL_IMMUNO_pr[4] ~ "top 5%"))

#---now get the genetics and other diseases data for ILRI alone
ALL_GENE_OTHER <- read.xlsx("PREFACE_AHG_PORTAL.xlsx",sheet = "bib_GENE_ILRI", colNames = T)
ALL_OTHER_OTHER <- read.xlsx("PREFACE_AHG_PORTAL.xlsx",sheet = "bib_OTHERDIS_ILRI", colNames = T)
ALL_GEN_OTH <- rbind(ALL_GENE_OTHER,ALL_OTHER_OTHER)
ALL_GEN_OTH <- ALL_GEN_OTH %>% select(PY,Citations = TC)
ALL_GEN_OTH$Institution <- factor("ILRI")

ALL_GEN_OTH <- filter(ALL_GEN_OTH, PY >=  1977 & PY <=  2018 & Citations > 9)

ALL_GEN_OTH_pr <- quantile(ALL_GEN_OTH$Citations, probs = c(0.25,0.5,0.75,0.95))

ALL_GEN_OTH <- ALL_GEN_OTH %>%
  mutate(Quants = case_when(
    Citations <=   ALL_GEN_OTH_pr[1] ~ "first",
    Citations <=   ALL_GEN_OTH_pr[2] & Citations >=  ALL_GEN_OTH_pr[1] ~ "second",
    Citations <=   ALL_GEN_OTH_pr[3] & Citations >=  ALL_GEN_OTH_pr[2] ~ "third",
    Citations <=   ALL_GEN_OTH_pr[4] & Citations >=  ALL_GEN_OTH_pr[3] ~ "fourth",
    Citations >   ALL_GEN_OTH_pr[4]  ~ "top 5%"))


#do the TOP5% citations
#

ALL_ECF$Domain <- factor("Theileriosis")
ALL_TRYPS$Domain <- factor("Trypanosomiasis")
ALL_GEN_OTH$Domain <- factor("Genetics, other")
ALL_IMMUNO$Domain <- factor("Immunology")

ALL_TOP5 <- rbind(ALL_ECF,ALL_TRYPS,ALL_GEN_OTH,ALL_IMMUNO)
ALL_TOP5$Quants <- factor(ALL_TOP5$Quants)
ALL_TOP5$Institution <- factor(ALL_TOP5$Institution)
ALL_TOP5 <- ALL_TOP5 %>%
  mutate(Institution = fct_relevel(Institution,
                                 "ILRI","Global"))
ALL_TOP5 <- ALL_TOP5 %>%
  mutate(Quants = fct_relevel(Quants,"first","second","third",
"fourth","top 5%"))

ALL_TOP5 <- ALL_TOP5 %>%
  mutate(Period = case_when(
    PY >=  1976 & PY <=  1980 ~ "1976-80",
    PY >=  1981 & PY <=  1985 ~ "1981-85",
    PY >=  1986 & PY <=  1990 ~ "1986-90",
    PY >=  1991 & PY <=  1994 ~ "1991-94",
    PY >=  1995 & PY <=  2000 ~ "1995-00",
    PY >=  2001 & PY <=  2005 ~ "2001-05",
    PY >=  2006 & PY <=  2010 ~ "2006-10",
    TRUE ~ "2011-18"))

ALL_TOP5$Period <- factor(ALL_TOP5$Period)

ALL_TOP5_group <- group_by(ALL_TOP5,Institution,Domain,Period,Quants) %>%
  summarise(nInstitution = length(Citations))

pal_ahg_fig_6 <- c(col_ILRI,col_global)
names(pal_ahg_fig_6) <- levels(ALL_TOP5_group$Institution)
sample_size.all.top5 <- length(ALL_TOP5$Citations)

(FIGURE_PREFACE_AHG_ALL_TOP5 <- ggplot(data = ALL_TOP5_group) +
geom_col(mapping = aes(x = Quants,y  = nInstitution, fill = Institution), position = "dodge", size = 1) +
xlab("Quantiles of merged ILRI and global institutions sample") +
 labs(y = "Counts of papers > 9 citations",title = tles.AHG.Fig.PI.6) +
    facet_wrap(~Domain,scales = "free_y",nrow = 2,ncol = 2) +
     scale_fill_manual(name = "Institution", values = pal_ahg_fig_6) +
    scale_y_continuous(labels = scales::comma) +
    theme_tufte() +
    theme(text = element_text(size = 11,family = "Times"), legend.position = "bottom") +
    labs(caption = paste(c("Source: Constructed by authors from data at www.scopus.com; sample size = ",sample_size.all.top5," papers, having at least 10 citations per paper."),collapse = "")) +
    theme(plot.caption = element_text(hjust = 0)))

#get the top 5% for TRYPS
#
(tibble_all_TOP5_group <- ALL_TOP5_group %>% group_by(Institution,Domain,Quants) %>%
          summarise(sumInstQuant = sum(nInstitution)))

(tryps_tibble <- group_by(ALL_TRYPS,Institution,Quants) %>%
     summarise(meanByInst = mean(Citations),sumByInst = sum(Citations),
               NByInst = length(Citations)))

(tryps_tibble_cumul <- data.table(Cumul = rbind(cumsum(tryps_tibble[1:5,4]),cumsum(tryps_tibble[6:10,4]))))

tryps_tibble <- data.table(tryps_tibble)
tryps_tibble_cumul <- data.table(tryps_tibble_cumul)

(tryps_tibble_all <- cbind(tryps_tibble,tryps_tibble_cumul))

(ILRI.tryps.papers.share.total <- round(100*sum(tryps_tibble_all$NByInst[6:10]) /
 sum(tryps_tibble_all$NByInst[1:10]),digits = 1))
(ILRI.tryps.papers.share.top5 <- round(100*sum(tryps_tibble_all$NByInst[10]) /
 sum(tryps_tibble_all$NByInst[5],tryps_tibble_all$NByInst[10]),digits = 1))

#get the top 5% for ECF

(ECF_tibble <- group_by(ALL_ECF,Institution,Quants) %>%
          summarise(meanByInst = mean(Citations),sumByInst = sum(Citations),
                    NByInst = length(Citations)))
(ILRI.ECF.papers.share.total <- round(100*sum(ECF_tibble$NByInst[6:10]) /
          sum(ECF_tibble$NByInst[1:10]),digits = 1))

(ILRI.ECF.papers.share.top5 <- round(100*sum(ECF_tibble$NByInst[10]) /
 sum(ECF_tibble$NByInst[5],ECF_tibble$NByInst[10]),digits = 1))

#get the top 5% for immunology

(immuno_tibble <- group_by(ALL_IMMUNO,Institution,Quants) %>%
          summarise(meanByInst = mean(Citations),sumByInst = sum(Citations),
                    NByInst = length(Citations)))

(ILRI.immuno.papers.share.total <- round(100*sum(immuno_tibble$NByInst[6:10]) / sum(immuno_tibble$NByInst[1:10]),digits = 1))

(ILRI.immuno.papers.share.top5 <- round(100*sum(immuno_tibble$NByInst[10]) / sum(immuno_tibble$NByInst[5],immuno_tibble$NByInst[10]),digits = 1))


(tibble_all_TOP5 <- ALL_TOP5 %>%
 group_by(Institution,Domain,Quants) %>% summarise(sumQuantTC = sum(Citations),nQuantTC = length(Citations)))

#top 5 shares of ECF papers in ILCA/ILRAD and ILRI eras
(tibble_all_ecf <- tibble_all_TOP5 %>% filter(Domain == "Theileriosis" & Quants == "top 5%"))

(ecf_top5.shareN <- round(100*tibble_all_ecf[1,5] /
 (tibble_all_ecf[1,5] + tibble_all_ecf[2,5]),digits = 1))

(ecf_top5.shareTC <- round(100*tibble_all_ecf[1,4] /
 (tibble_all_ecf[1,4] + tibble_all_ecf[2,4]),digits = 1))


#top 5 shares of tryps papers
(tibble_all_tryps <- tibble_all_TOP5 %>% filter(Domain == "Trypanosomiasis" & Quants == "top 5%"))

(tryps_top5.shareN <- round(100*tibble_all_tryps[1,5] /
                               (tibble_all_tryps[1,5] + tibble_all_tryps[2,5]),digits = 1))

(tryps_top5.shareTC <- round(100*tibble_all_tryps[1,4] /
                                (tibble_all_tryps[1,4] + tibble_all_tryps[2,4]),digits = 1))

#top 5 shares of immunology papers
(tibble_all_immuno <- tibble_all_TOP5 %>% filter(Domain == "Immunology" & Quants == "top 5%"))

(immuno_top5.shareN <- round(100*tibble_all_immuno[1,5] /
                                 (tibble_all_immuno[1,5] + tibble_all_immuno[2,5]),digits = 1))

(immuno_top5.shareTC <- round(100*tibble_all_immuno[1,4] /
                                  (tibble_all_immuno[1,4] + tibble_all_immuno[2,4]),digits = 1))



#swine and poultry
filter_PIG_POULTRY <- c("SWINE FEVER","TRICHINELLA", "PIG", "SWINE", "TRICHINOSIS",
                     "AVIAN","CYSTERCICOSIS","CHICKEN","POULTRY","PORK","SUID","SCROFA",
                     "GALLUS")
filter_PIG_POULTRY <- str_sort(filter_PIG_POULTRY)

bib_PIG_POULTRY <- filter(bib_final_clean,
 str_detect(TI,paste(c(filter_PIG_POULTRY),collapse = "|")) |
 str_detect(ID,paste(c(filter_PIG_POULTRY),collapse = "|")) |
 str_detect(DE,paste(c(filter_PIG_POULTRY),collapse = "|")) |
 str_detect(AB,paste(c(filter_PIG_POULTRY),collapse = "|")))

bib_PIG_POULTRY$AU <- toupper(trim(bib_PIG_POULTRY$AU))
bib_PIG_POULTRY$TI <- toupper(trim(bib_PIG_POULTRY$TI))
bib_PIG_POULTRY$SR <- toupper(trim(bib_PIG_POULTRY$SR))

bib_PIG_POULTRY$SR <- gsub("-a-b-c-d-e-f-g","",bib_PIG_POULTRY$SR)
bib_PIG_POULTRY$SR <- gsub("-a-b-c-d-e-f","",bib_PIG_POULTRY$SR)
bib_PIG_POULTRY$SR <- gsub("-a-b-c-d-e","",bib_PIG_POULTRY$SR)
bib_PIG_POULTRY$SR <- gsub("-a-b-c-d","",bib_PIG_POULTRY$SR)
bib_PIG_POULTRY$SR <- gsub("-a-b-c","",bib_PIG_POULTRY$SR)
bib_PIG_POULTRY$SR <- gsub("-a-b","",bib_PIG_POULTRY$SR)
bib_PIG_POULTRY$SR <- gsub("-a","",bib_PIG_POULTRY$SR)

bib_PIG_POULTRY <- distinct(bib_PIG_POULTRY,SR,.keep_all = TRUE)
bib_PIG_POULTRY <- distinct(bib_PIG_POULTRY,TI,.keep_all = TRUE)
bib_PIG_POULTRY <- filter(bib_PIG_POULTRY,PY > 1994)

#
#other notes in AHG Preface
options(tibble.print_max = Inf)

#total spending ILCA and ILRAD
ahg_spend_ILCA.ILRAD <- filter(AHG_Spend_R,YEAR <=  1995)

(ahg_spend_ILCA.ILRAD.total <- round(sum(ahg_spend_ILCA.ILRAD$GROUP_ANIM_SCI_SPEND) / 1000,digits = 0))
#(ahg_spend_ILCA.ILRAD.annual <- ahg_spend_ILCA.ILRAD.total/ 20)
(ahg_spend_ILCA.ILRAD.share <- round(100*sum(ahg_spend_ILCA.ILRAD[,"GROUP_ANIM_SCI_SPEND"]) /
          sum(ahg_spend_ILCA.ILRAD[,"LIFETIME_TOTAL_SPEND"]),digits = 0))

#total AHG spending ILRI
(ILRI.spend.total <- round(sum(AHG_Spend_R$LIFETIME_TOTAL_SPEND)/1000,digits = 0))
#(spend_ILRI.total <- sum(AHG_Spend_R$LIFETIME_TOTAL_SPEND))
(ahg_spend_ILRI.total <- round(sum(AHG_Spend_R$GROUP_ANIM_SCI_SPEND)/1000,digits = 0))
(ahg_spend_ILRI_share.total <- round(100*sum(AHG_Spend_R$GROUP_ANIM_SCI_SPEND)/sum(AHG_Spend_R$LIFETIME_TOTAL_SPEND),digits = 1))
(ahg_spend_ILRI.annual <- round(ahg_spend_ILRI.total / 44,digits = 1))

(ahg_spend_ILRI.201118 <- round(100*(AHG_Spend_gather[31,3] / AHG_Spend_gather[32,3]),digits = 1))
(ahg_spend.ecf.tryps.share.all.201118 <- (AHG_Spend_gather[29,3] + AHG_Spend_gather[30,3]) / AHG_Spend_gather[32,3])
(ahg_spend.ecf.tryps.share.ahg.201118 <- (AHG_Spend_gather[29,3] + AHG_Spend_gather[30,3]) / (AHG_Spend_gather[29,3] + AHG_Spend_gather[30,3] + AHG_Spend_gather[31,3]))

#tryps---------------------------

(ILRI.papers.total <- sum(AHG_Spend_R$ILRI_ALL_PAPERS))
(ILRI.cites.total <- sum(AHG_Spend_R$ILRI_ALL_TC))

ILRI.spend.total <- sum(AHG_Spend_R$LIFETIME_TOTAL_SPEND)/1000
(ILRI.tryps.spend.total <- sum(AHG_Spend_R$ALL_TRYPS_SPEND)/1000)
(ILRI.tryps.spend.share.total <- round(100*(ILRI.tryps.spend.total/ILRI.spend.total),digits = 1))
(ILRI.tryps.annual.spend <- ILRI.tryps.spend.total/44)

(ILRI.tryps.papers.total <- length(TRYPS_ILRI$Citations))
(ILRI.tryps.cites.total <- sum(TRYPS_ILRI$Citations))

(ILRI.tryps.paper.cost <- round(1000*ILRI.tryps.spend.total/ILRI.tryps.papers.total, digits = 0))
(ILRI.tryps.cites.millionUSD <- round(ILRI.tryps.cites.total/ILRI.tryps.spend.total,digits = 0))

(ILRI_tryps.cites.mean <- round(mean(TRYPS_ILRI$Citations),digits = 0))
(ILRI_tryps.cites.median <- round(median(TRYPS_ILRI$Citations),digits = 0))

(tryps_ILRI.paper.share.total <- round(100*ILRI.tryps.papers.total/ILRI.papers.total,digits = 1))
(tryps_ILRI.cites.share.total <- round(100*ILRI.tryps.cites.total/ILRI.cites.total, digits = 1))

TRYPS_ILRI$ranks <- frank(TRYPS_ILRI,-Citations)
(ILRI.tryps.cites.top10.total <- filter(TRYPS_ILRI, ranks <=  10) %>% summarise(sumTop10 = sum(Citations)))
(ILRI.tryps.cites.top10.share <- round(100*(ILRI.tryps.cites.top10.total / ILRI.tryps.cites.total),digits = 0))

(global_TRYPS.papers <- length(TRYPS_GLOBAL$Citations))
(global_TRYPS.cites <- sum(TRYPS_GLOBAL$Citations))

(tryps_ILRI.paper.global.share.total <- round(100*ILRI.tryps.papers.total/global_TRYPS.papers,digits = 1))
(tryps_ILRI.cites.global.share.total <- round(100*ILRI.tryps.cites.total/global_TRYPS.cites, digits = 1))

TRYPS_ILRI <- select(TRYPS_ILRI,PY = PY,Citations,Institution = Institution)
TRYPS_GLOBAL <- select(TRYPS_GLOBAL,PY = PY,Citations,Institution = Institution)
TRYPS_GLOBAL$ranks <- frank(TRYPS_GLOBAL,-Citations)


#trypantolerance-----------------------------

global_TRYPANOTOLER <- data.table(read.xlsx("PREFACE_AHG_PORTAL.xlsx",sheet = "bib_TRYPANOTOLER_GLOBAL",colNames = T))
global_TRYPANOTOLER <- filter(global_TRYPANOTOLER, PY >= 1975 & PY <= 2018)
global_TRYPANOTOLER <- select(global_TRYPANOTOLER,PY,TC)
(global_TRYPANOTOLER.papers <- length(global_TRYPANOTOLER$TC))
(global_TRYPANOTOLER.cites <- sum(global_TRYPANOTOLER$TC))
global_TRYPANOTOLER$ranks <- frank(global_TRYPANOTOLER,-TC)

ILRI_TRYPANOTOLER <- data.table(read.xlsx("PREFACE_AHG_PORTAL.xlsx",sheet = "bib_TRYPANOTOLER_ILRI",colNames = T))
ILRI_TRYPANOTOLER <- filter(ILRI_TRYPANOTOLER, PY >= 1975 & PY <= 2018)
ILRI_TRYPANOTOLER <- select(ILRI_TRYPANOTOLER,PY,TC)
(ILRI.trypanotoler.papers <- length(ILRI_TRYPANOTOLER$TC))
(ILRI.trypanotoler.cites <- sum(ILRI_TRYPANOTOLER$TC))
ILRI_TRYPANOTOLER$ranks <- frank(ILRI_TRYPANOTOLER,-TC)
(ILRI.trypanotoler.cites.top10.total <- filter(ILRI_TRYPANOTOLER, ranks <=  10) %>% summarise(sumTop10 = sum(TC)))
(ILRI.trypanotoler.cites.top10.share <- round(100*(ILRI.trypanotoler.cites.top10.total / ILRI.trypanotoler.cites),digits = 0))

(ILRI_trypanotoler.cites.mean <- round(mean(ILRI_TRYPANOTOLER$TC),digits = 0))
(ILRI_trypanotoler.cites.median <- round(median(ILRI_TRYPANOTOLER$TC),digits = 0))
(trypanotoler_ILRI.paper.share.total <- ILRI.trypanotoler.papers/ILRI.papers.total)
(trypanotoler_ILRI.cites.share.total <- ILRI.trypanotoler.cites/ILRI.cites.total)
(trypanotoler_ILRI.paper.global.share.total <- round(100*ILRI.trypanotoler.papers/global_TRYPANOTOLER.papers,digits = 1))
(trypanotoler_ILRI.cites.global.share.total <- round(100*ILRI.trypanotoler.cites/global_TRYPANOTOLER.cites,digits = 1))

#ECF

ILRI_ECF <- read.xlsx("PREFACE_AHG_PORTAL.xlsx",sheet = "bib_ECF_ILRI",colNames = T)
ILRI_ECF <- filter(ILRI_ECF,PY >= 1975 & PY <= 2018)
(ILRI_ECF.papers.total <- length(ILRI_ECF$TC))
(ILRI_ECF.cites.total <- sum(ILRI_ECF$TC))
ILRI_ECF$ranks <- frank(ILRI_ECF,-TC)

bib_ECF_GLOBAL <- read.xlsx("PREFACE_AHG_PORTAL.xlsx",sheet = "bib_ECF_GLOBAL",colNames = T)
bib_ECF_GLOBAL <- filter(bib_ECF_GLOBAL,PY >= 1975 & PY <= 2018)

(global_ECF.papers <- length(bib_ECF_GLOBAL$TC))
(global_ECF.cites <- sum(bib_ECF_GLOBAL$TC))

(ILRI.spend.total <- sum(AHG_Spend_R$LIFETIME_TOTAL_SPEND)) / 1000
(ILRI_ECF.spend.total <- sum(AHG_Spend_R$ALL_ECF_SPEND)) / 1000
(ILRI_ECF.spend.share.total <- round(100*(ILRI_ECF.spend.total/ILRI.spend.total),digits = 1))
(ILRI_ECF.annual.spend <- ILRI_ECF.spend.total/44)
(ILRI_ECF.paper.cost <- round(ILRI_ECF.spend.total/ILRI_ECF.papers.total,digits = 0))
(ILRI_ECF.cites.millionUSD <- round(1000*ILRI_ECF.cites.total/ILRI_ECF.spend.total,digits = 0))

(ILRI_ECF.cites.mean <- round(mean(ILRI_ECF$TC),digits = 0))
(ILRI_ECF.cites.median <- round(median(ILRI_ECF$TC),digits = 0))
(ILRI_ECF.paper.share.total <- round(100*ILRI_ECF.papers.total/ILRI.papers.total,digits = 1))
(ILRI_ECF.cites.share.total <- round(100*ILRI_ECF.cites.total/ILRI.cites.total, digits = 1))
(ILRI_ECF.paper.global.share.total <- round(100*ILRI_ECF.papers.total/global_ECF.papers,digits = 1))
(ILRI_ECF.cites.global.share.total <- round(100*ILRI_ECF.cites.total/global_ECF.cites,digits = 1))

(ILRI_ECF.cites.top10.total <- filter(ILRI_ECF, ranks <=  10) %>% summarise(sumTop10 = sum(TC)))
(ILRI_ECF.cites.top10.share <- round(100*(ILRI_ECF.cites.top10.total / ILRI_ECF.cites.total),digits = 0))


#declining importance of trypanosomiasis and ECF
(ILRI.ECF_TRYPS.spend.AHG.share. <- round(100*(sum(AHG_Spend_gather[31:32,3]) /
 AHG_Spend_gather[30,3]),digits = 0))
(ILRI.ECF_TRYPS.spend.total.share. <- round(100*(sum(AHG_Spend_gather[31:32,3]) /
AHG_Spend_gather[29,3]),digits = 0))


#immunology
IMMUNO_GLOBAL <- read.xlsx("PREFACE_AHG_PORTAL.xlsx",sheet = "bib_IMMUNO_GLOBAL",colNames = T)
IMMUNO_GLOBAL <- filter(IMMUNO_GLOBAL,PY >= 1977 & PY <=  2018)
IMMUNO_GLOBAL <- select(IMMUNO_GLOBAL,PY,TC)
IMMUNO_GLOBAL$TC <- as.integer(IMMUNO_GLOBAL$TC)
IMMUNO_GLOBAL$Institution <- as.factor("Global")

IMMUNO_ILRI <- read.xlsx("PREFACE_AHG_PORTAL.xlsx",sheet = "bib_IMMUNO_ILRI",colNames = T)
IMMUNO_ILRI <- filter(IMMUNO_ILRI,PY >=  1977 & PY <=  2018)
IMMUNO_ILRI <- select(IMMUNO_ILRI,PY,TC)
IMMUNO_ILRI$Institution <- as.factor("ILRI")
IMMUNO_ILRI$ranks <- frank(IMMUNO_ILRI,-TC)

(ILRI.papers.total <- sum(AHG_Spend_R$ILRI_ALL_PAPERS))
(ILRI.cites.total <- sum(AHG_Spend_R$ILRI_ALL_TC))

(IMMUNO_ILRI.papers.total <- length(IMMUNO_ILRI$TC))
(IMMUNO_ILRI.cites.total <- sum(IMMUNO_ILRI$TC))

(IMMUNO_GLOBAL.papers <- length(IMMUNO_GLOBAL$TC))
(IMMUNO_GLOBAL.cites <- sum(IMMUNO_GLOBAL$TC))

(ILRI.spend.total <- sum(AHG_Spend_R$LIFETIME_TOTAL_SPEND))

(IMMUNO_ILRI.cites.mean <- round(mean(IMMUNO_ILRI$TC),digits = 0))
(IMMUNO_ILRI.cites.median <- round(median(IMMUNO_ILRI$TC),digits = 0))
(IMMUNO_ILRI.paper.share.total <- round(100*IMMUNO_ILRI.papers.total/ILRI.papers.total,digits = 1))
(IMMUNO_ILRI.cites.share.total <- round(100*IMMUNO_ILRI.cites.total/ILRI.cites.total, digits = 1))
(IMMUNO_ILRI.paper.global.share.total <- 100*IMMUNO_ILRI.papers.total/IMMUNO_GLOBAL.papers)
(IMMUNO_ILRI.cites.global.share.total <- 100*IMMUNO_ILRI.cites.total/IMMUNO_GLOBAL.cites)

(IMMUNO_ILRI.cites.top10.total <- filter(IMMUNO_ILRI, ranks <=  10) %>% summarise(sumTop10 = sum(TC)))
(IMMUNO_ILRI.cites.top10.share <- round(100*(IMMUNO_ILRI.cites.top10.total / IMMUNO_ILRI.cites.total),digits = 0))


#mouse model of tryps
ahg_mouse <- read.xlsx("PREFACE_AHG_PORTAL.xlsx",sheet = "bib_IMMUNO_ILRI",colNames = T) %>%
     filter(str_detect(AB,paste(c("MOUSE","MURINE"),collapse = "|")))
ahg_mouse10 <- filter(ahg_mouse,TC > 9)
(nrow(ahg_mouse10)/nrow(ahg_mouse))

#genetics,food safety,transboundary, zoonoses
ILRI_OTHERDIS <- read.xlsx("PREFACE_AHG_PORTAL.xlsx",sheet = "bib_OTHERDIS_ILRI",colNames = T)
ILRI_OTHERDIS <- filter(ILRI_OTHERDIS, PY >= 1975 & PY <= 2018)
ILRI_OTHERDIS <- select(ILRI_OTHERDIS,PY = PY,TC = TC)
(fstz_papers.total <- length(ILRI_OTHERDIS$TC))
(fstz_cites.total <- sum(ILRI_OTHERDIS$TC))

(fstz_cites.paper.mean <- round(mean(ILRI_OTHERDIS$TC,na.rm = TRUE),digits = 1))
(fstz_cites.paper.median <- round(median(ILRI_OTHERDIS$TC,na.rm = TRUE), digits = 1))

(fstz_paper.share.total <- 100*round(fstz_papers.total/ILRI.papers.total,digits = 3))
(fstz_cites.share.total <- 100*round(fstz_cites.total/ILRI.cites.total,digits = 3))


#all other diseases
(ILRI_fstz.spend.total <- 1000*ahg_spend_ILRI.total - 1000*ILRI.tryps.spend.total - ILRI_ECF.spend.total)
(ILRI_fstz.spend.papers <- ILRI_fstz.spend.total /  fstz_papers.total)
(ILRI_fstz.spend.cites <-  1000*fstz_cites.total / ILRI_fstz.spend.total)

#sequencing
sequence.papers <- filter(bib_final_clean,str_detect(AB,paste(c("DNA sequenc"),collapse = "|")))
sequence.papers <- filter(sequence.papers, PY < 2000)
nrow(sequence.papers)

#pigs and poultry
pig_poultry_leng <- length(bib_PIG_POULTRY$TC)
pig_poultry_cites <- sum(bib_PIG_POULTRY$TC)
all_leng <- length(bib_final_clean$TC)
all_cites <- sum(bib_final_clean$TC)

pp1 <- pig_poultry_leng / all_leng
pp2 <- pig_poultry_cites / all_cites


# following code is commented out
# if you want to export the figures to tiff files then uncomment
#
# #---------------------------save the figures and their annotations
#
# ggarrange(FIGURE_PREFACE_AHG_1, ncol = 1,nrow = 1,align = "h")  %>%
#      ggexport(filename = "Fig. PI.1. ILRI spending on Animal production, health and genetics, trypanosomiasis and Theileriosis, 1975-2018.tiff")
# #--------------------------
#
# ggarrange(FIGURE_PREFACE_AHG_TRYPS_2a, ncol = 1,nrow = 1,align = "h")  %>%
#      ggexport(filename = "Fig PI.2 (a). ILRI and other publications on trypanosomiasis, 1977-2018.tiff")
#
# #-------------------------
#
# ggarrange(FIGURE_PREFACE_AHG_TRYPS_2b, ncol = 1,nrow = 1,align = "h")  %>%
#      ggexport(filename = "Fig PI.2 (b). Frequency of citations of ILRI and global publications on trypanosomiasis, 1977-2018.tiff")
#
# #-------------------------
#
# ggarrange(FIGURE_PREFACE_AHG_TRYPANOTOLER_2c, ncol = 1,nrow = 1,align = "h")  %>%
#      ggexport(filename = "Fig PI.2 (c). ILRI and global publications on trypanotolerance, 1977-2018.tiff")
#
# #-------------------------
#
# ggarrange(FIGURE_PREFACE_AHG_ECF_3a, ncol = 1,nrow = 1,align = "h")  %>%
#      ggexport(filename = "Fig PI.3. (a) ILRI and other publications on Theileriosis and related problems.tiff")
#
# #----------------------------------
#
# ggarrange(FIGURE_PREFACE_AHG_ECF_3b, ncol = 1,nrow = 1,align = "h")  %>%
#      ggexport(filename = "Fig PI.3. (b) Frequency of citations of ILRI and global publications on Theileriosis and related problems, 1977-2018.tiff")
#
# #---------------------------
#
# ggarrange(FIGURE_PREFACE_AHG_IMMUNO_TC_PY_4a, ncol = 1,nrow = 1,align = "h")  %>%
#      ggexport(filename = "Fig PI.4 (a). ILRI and other publications in immunology, 1977-2018.tiff")
#
# #---------------------------------------------
#
# ggarrange(FIGURE_PREFACE_AHG_IMMUNO_4b, ncol = 1,nrow = 1,align = "h")  %>%
#      ggexport(filename = "Fig PI.4 (b). Frequency of citations of ILRI and global publications in immunology, 1977-2018.tiff")
#
# #---------------------------------------------
#
# ggarrange(FIGURE_PREFACE_AHG_GENE_OTHERDIS_5a, ncol = 1,nrow = 1,align = "h")  %>%
#      ggexport(filename = "Fig PI.5.(a) ILRI publications in genetics, zoonoses, food safety, transboundary diseases.tiff")
#
# #------------------------------------
#
# ggarrange(FIGURE_PREFACE_AHG_GENE_OTHERDIS_5b, ncol = 1,nrow = 1,align = "h")  %>%
#      ggexport(filename = "Fig PI.5. (b) Frequency of citations of ILRI publications in genetics, zoonoses, food safety, transboundary diseases.tiff")
#
# #-------------------------------------------
#
# ggarrange(FIGURE_PREFACE_AHG_ALL_TOP5, ncol = 1,nrow = 1,align = "h")  %>%
#      ggexport(filename = "Fig. PI.6. Frequency of citations of ILRI and global institutions in animal health research by quantile, 1976-2018.tiff")
#
# #-------------------------------------------
#
#

Fig.titles.PI <- as.character(mget(ls(pattern = "tles.AHG.Fig.PI.")))
#
#
## following code is commented out
# if you want to export the figure titles to a file then uncomment it

# capture.output(print(as.character(Fig.titles.PI),
#                      print.gap = 1,quote = FALSE),file = "Preface I, Figure titles")



