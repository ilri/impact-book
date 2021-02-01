#code to generate figures for the CHAPTER 6 on ECF
#
#NOTA BENE: some of the labels and notes to the Figures may differ
#slightly from what is in the book because of formatting changes made during printing
#
#

setwd("C:/Users/Jmm19/Documents/______ILRI/_______________DATA_PORTAL")

#PACKAGES AND LIBRARIES ARE IN THE WORKSPACE

library(bibliometrix)
library(tidyverse)
library(dplyr)
library(tidyr)
library(tidyselect)
library(doBy)
library(ggplot2)
library(ggpubr)
library(ggthemes)
library(gridExtra)
library(grid)
library(stringi)
library(stringr)
library(data.table)
library(rAltmetric)
library(magrittr)
library(purrr)
library(scales)
library(gdata)

library(scales)
library(openxlsx)
library(forcats)
library(janitor)

library(extrafont)
library(extrafontdb)
library(graphics)
loadfonts(device = "win")
windowsFonts(Times = windowsFont("Times New Roman"))

bib_ECF_GLOBAL <- read.xlsx("CHAPTER_6_ECF_PORTAL.xlsx",sheet = "bib_ECF_GLOBAL", colNames = T)

bib_final_clean <- read.xlsx("BIBLIO_PORTAL.xlsx",sheet = "BIBLIO",colNames = T)

#
#set the master ECF filter
#

filter_ECF <- c("THEILERIA PARVA",
                "THEILERIASIS",
                "THEILERIA",
                "THEILERIA PARVA, ANIMAL",
                "THEILERIA PARVA, ANIMALS",
                "THEILERIA PARVA, AMINO ACID SEQUENCE",
                "THEILERIA ANNULATA",
                "THEILERIA PARVA, ANIMALIA",
                "THEILERIASIS, ANIMALIA",
                "THEILERIA MUTANS",
                "THEILERIOSIS",
                "THEILERIA PARVA, BOS TAURUS",
                "THEILERIA TAUROTRAGI",
                "THEILERIA, ANIMAL",
                "THEILERIA LESTOQUARDI",
                "THEILERIA EQUI",
                "THEILERIA PARVA, AMINO ACID MOTIFS",
                "THEILERIA SP.",
                "THEILERIA VELIFERA",
                "THEILERIASIS, ACARI",
                "THEILERIASIS, BOS",
                "MALIGNANT OVINE THEILERIOSIS",
                "THEILERIA ANNULATA, ANIMALIA",
                "THEILERIA BUFFELI",
                "THEILERIA EQUI, ANIMAL",
                "THEILERIA LUWENSHUNI",
                "THEILERIA OVIS",
                "THEILERIA PARVA, ACARI",
                "THEILERIA PARVA, ADENOSINE DIPHOSPHATE RIBOSE",
                "THEILERIA PARVA, ADENOSINE TRIPHOSPHATE",
                "THEILERIA PARVA, ADJUVANTS, IMMUNOLOGIC",
                "THEILERIA PARVA, AFRICA",
                "THEILERIA PARVA, AGING",
                "THEILERIA PARVA, AGRICULTURE",
                "THEILERIA PARVA, ALGORITHMS",
                "THEILERIA PARVA, ALPHA-FETOPROTEINS",
                "THEILERIA PARVA, ANALYSIS OF VARIANCE",
                "THEILERIA PARVA, ANAPLASMA",
                "THEILERIA PARVA, ANAPLASMA MARGINALE",
                "THEILERIA PARVA, ANIMAL HUSBANDRY",
                "THEILERIA PARVA, BACTERIUM ANTIBODY",
                "THEILERIA PARVA, BUFFALO",
                "THEILERIA PARVA, CALVES",
                "THEILERIA PARVA, EUKARYOTA",
                "THEILERIA PARVA, LEISHMANIA BRAZILIENSIS",
                "THEILERIA PARVA, NEUTRALIZING ANTIBODY",
                "THEILERIA PARVA, NUCLEIC ACID PROBE",
                "THEILERIA PARVA, PARASITE ANTIGEN, ARTICLE",
                "THEILERIA SENSU STRICTO",
                "THEILERIA VELIFERA, ANIMALIA",
                "THEILERIA, AMINO ACID SEQUENCE",
                "THEILERIASIS, ALLELES",
                "THEILERIASIS, APICOMPLEXA",
                "THEILERIASIS, BOS TAURUS",
                "THEILERIASIS, BOVINAE",
                "THEILERIASIS, KENYA",
                "THEILERIOSIS, AGRICULTURE",
                "THEILERIOSIS, AMINO ACID SEQUENCE",
                "THEILERIOSIS, BOS",
                "TICK","RHIPICEPHALUS","AMBLYOMMA","T. PARVA")



#now get the ILRI ECF papers
bib_ECF_ILRI_TI <- filter(bib_final_clean, str_detect(TI, paste(c(filter_ECF), collapse = "|")))
bib_ECF_ILRI_ID <- filter(bib_final_clean, str_detect(ID, paste(c(filter_ECF), collapse = "|")))
bib_ECF_ILRI_DE <- filter(bib_final_clean, str_detect(DE, paste(c(filter_ECF), collapse = "|")))
bib_ECF_ILRI_AB <- filter(bib_final_clean, str_detect(AB, paste(c(filter_ECF), collapse = "|")))

bib_ECF_ILRI <- rbind(bib_ECF_ILRI_DE,bib_ECF_ILRI_ID,bib_ECF_ILRI_TI,bib_ECF_ILRI_AB)

bib_ECF_ILRI <- distinct(bib_ECF_ILRI,TI,.keep_all = TRUE)

bib_ECF_IRLI <- filter(bib_ECF_ILRI, PY >= 1975 & PY <= 2018)



#MAKE SOME TABLES
bib_ECF_GLOBAL <- data.table(bib_ECF_GLOBAL)
bib_ECF_GLOBAL$PY <- as.integer(bib_ECF_GLOBAL$PY)
bib_ECF_GLOBAL$TC <- as.integer(bib_ECF_GLOBAL$TC)
bib_ECF_GLOBAL$AU_UN_NR <- as.character(bib_ECF_GLOBAL$AU_UN_NR)

ECF_GLOBAL_TC_PY <- data.frame(summaryBy(TC ~ PY, data = bib_ECF_GLOBAL, FUN = c(length,sum)))

bib_ECF_ILRI <- data.table(bib_ECF_ILRI)
ECF_ILRI_TC_PY <- data.frame(summaryBy(TC ~ PY, data = bib_ECF_ILRI, FUN = c(length,sum)))

#----- FOLLOWING GETS  OUTPUT FIGURES FROM DYNMOD/IMPACT


ECF_KENYA <- data.table(read.xlsx("CHAPTER_6_ECF_PORTAL.xlsx",
sheet = "Adoption", startRow = 1,colNames = T,na.strings = "NA"))

col_ID <- c("#1DB000FF")
col_OD <- c("#B6DB00FF")
col_AP <- c("#E8C285FF")
col_PA <- c("#ECB176FF")
col_WTD <- c("brown")
pal_ECF <- c(col_ID,col_OD,col_AP,col_PA,col_WTD)

#construct  the scenarios : low, base, high
#low scenario
ecf_low <- ECF_KENYA %>% select(Year,contains("Low_")) %>%
     select(Year,"Intensive dairy" = "Low_ID",
            "Open dairy" = "Low_OD",
            "Agropastoral" = "Low_AP",
            "Pastoral" = "Low_PA",
            "Weighted mean" = "Low_WTD")

ecf_low <- ecf_low %>% pivot_longer("Intensive dairy":"Weighted mean",
 names_to = "System",values_to = "Adoption")
ecf_low$System <- factor(ecf_low$System)

ecf_low <- ecf_low %>% mutate(System = fct_relevel(System,
 "Intensive dairy","Open dairy","Agropastoral","Pastoral","Weighted mean"))
ecf_low$Scenario <- factor("Low adoption")

names(pal_ECF) <- levels(ecf_low$System)

pop.cattle <- data.frame("Intensive dairy" = 8.7,
 "Open dairy" = 10.5, "Agropastoral" = 27.3, "Pastoral" = 22.0, "Weighted mean" = 100)


#base scenario
ecf_base <- ECF_KENYA %>% select(Year,contains("Base_")) %>%
select(Year,"Intensive dairy" = "Base_ID",
       "Open dairy" = "Base_OD",
       "Agropastoral" = "Base_AP",
       "Pastoral" = "Base_PA",
       "Weighted mean" = "Base_WTD")

ecf_base <- ecf_base %>% pivot_longer("Intensive dairy":"Weighted mean",
                                    names_to = "System",values_to = "Adoption")
ecf_base$System <- factor(ecf_base$System)
ecf_base$Scenario <- factor("Most likely adoption")

ecf_base <- ecf_base %>% mutate(System = fct_relevel(System,
 "Intensive dairy","Open dairy","Agropastoral","Pastoral","Weighted mean"))

names(pal_ECF) <- levels(ecf_base$System)


#high adoption scenario
ecf_high <- ECF_KENYA %>% select(Year,contains("High_")) %>%
     select(Year,"Intensive dairy" = "High_ID",
            "Open dairy" = "High_OD",
            "Agropastoral" = "High_AP",
            "Pastoral" = "High_PA",
            "Weighted mean" = "High_WTD")

ecf_high <- ecf_high %>% pivot_longer("Intensive dairy":"Weighted mean",
                                      names_to = "System",values_to = "Adoption")
ecf_high$System <- factor(ecf_high$System)
ecf_high$Scenario <- factor("High adoption")

ecf_high <- ecf_high %>% mutate(System = fct_relevel(System,
"Intensive dairy","Open dairy","Agropastoral","Pastoral","Weighted mean"))

names(pal_ECF) <- levels(ecf_high$System)


ecf_all <- rbind(ecf_low,ecf_base,ecf_high)
ecf_all <- arrange(ecf_all,Scenario,Year,System)

(FIGURE_ecf_all <- ggplot(data = ecf_all,aes(x = Year, y = Adoption, colour = System)) +
          geom_line(size = 1.25) +
          facet_wrap(~Scenario,nrow = 1,ncol = 3) +
          scale_fill_manual(values = pal_ECF,aesthetics = "colour") +
          scale_x_continuous(breaks = c(seq(2007,2027,5)),limits = c(2007,2027.21),expand = c(0,0)) +
          labs(y = "% of system farmers adopting",
               title = "Fig. 6.2 Projected ECF vaccine adoption rates by adoption scenario and production system, 2007-2027") +
          scale_y_continuous(labels = scales::comma) +
          theme_tufte() +
          labs(caption = paste(c("Constructed from DynMod model for ECF in Kenya. \n'Intensive dairy' cattle were about ",pop.cattle[1], " % of cattle population in 2007;",
               " 'Open dairy', ",pop.cattle[2],"%;",
               " 'Agropastoral', ",pop.cattle[3],"%;",
               "\n 'Pastoral', ",pop.cattle[4],"%; ",
               "cattle unaffected by ECF were approximately 26% of the national cattle herd of 17.5 m head."),collapse = "")) +
          theme(plot.caption = element_text(hjust = 0)) +
          theme(text = element_text(size = 11), legend.position = "bottom"))



#Note: scenarios defined as follows:
#Baseline: status quo in absence of any ITM adoption from 2007;
#Scenario 1: Most likely mortality, most likely adoption path;
#Scenario 2: Low ECF mortality, most likely adoption path;
#Scenario 3: High ECF mortality, most likely adoption path;
#Scenario 4: Most likely mortality, low adoption path;
#Scenario 5: Low ECF mortality, low adoption path;
#Scenario 6: High ECF mortality, low adoption path;
#Scenario 7: Most likely mortality, high adoption path;
#Scenario 8: Low ECF mortality, high adoption path;
#Scenario 9: High ECF mortality, high adoption path.
#
#
#FIGURE 6.5 BCR -----------------------------------

ecf_bcr <- data.table(read.xlsx("CHAPTER_6_ECF_PORTAL.xlsx",sheet = "BCR", startRow = 1,colNames = T,na.strings = "NA"))

ecf_bcr$Avoided.mortality <- factor(ecf_bcr$Avoided.mortality)
ecf_bcr$Adoption <- factor(ecf_bcr$Adoption)
ecf_bcr <- arrange(ecf_bcr,Adoption)

pal_bcr <- c("#FFAA00FF","#FF6300FF","#FF1C00FF")
ecf_bcr <- ecf_bcr %>% mutate(Adoption = fct_relevel(Adoption,"Low adoption","Most likely adoption","High adoption"))
ecf_bcr <- ecf_bcr %>% mutate(Avoided.mortality =
fct_relevel(Avoided.mortality,"Low mortality","Most likely mortality","High mortality"))

names(pal_bcr) <- levels(ecf_bcr$Avoided.mortality)

(FIGURE_ecf_bcr <- ggplot(data = ecf_bcr) +
     geom_col(mapping = aes(x = Adoption, y = B.C.ratio, fill = Avoided.mortality),
        position = position_dodge()) +
     labs(x = "Vaccine adoption levels", y = "Benefit-cost ratio",
     title = "Fig. 6.5 Benefit cost ratios for ECF vaccination research and development programmes by vaccine adoption levels and mortality avoided") +
     scale_fill_manual(name = "Avoided.mortality", values = pal_bcr) +
     theme_tufte() +
     guides(fill = guide_legend(title = "Mortality avoided")) +
     theme(text = element_text(size = 11),legend.position = "bottom"))

ecf.scen.range <- ecf_bcr %>% group_by(Adoption,Avoided.mortality) %>%
summarise(minBCR = min(B.C.ratio),maxBCR = max(B.C.ratio))


#FIGURE 6.3 HERD PROJECTIONS---------------------

ecf_s <- read.xlsx("CHAPTER_6_ECF_PORTAL.xlsx",sheet = "HerdIndexTranspose", colNames = TRUE,rowNames = FALSE,
skipEmptyRows = TRUE, skipEmptyCols = TRUE,na.strings = "NA")

ecf_gather <- pivot_longer(ecf_s,Baseline:Scenario.9,names_to = "Scenario",values_to = "Value")

keys_likely_adopt <- c("Scenario.1","Scenario.2","Scenario.3")
keys_low_adopt <- c("Scenario.4","Scenario.5","Scenario.6")
keys_high_adopt <- c("Scenario.7","Scenario.8","Scenario.9")

keys_likely_mort <- c("Scenario.1","Scenario.4","Scenario.7")
keys_low_mort <- c("Scenario.2","Scenario.5","Scenario.8")
keys_high_mort <- c("Scenario.3","Scenario.6","Scenario.9")

ecf_gather <- ecf_gather %>% mutate(adopt_level = case_when(
      Scenario %in%  c(keys_likely_adopt) ~ "Most likely adoption",
      Scenario %in%  c(keys_low_adopt) ~ "Low adoption",
      Scenario %in%  c(keys_high_adopt) ~ "High adoption",
      TRUE ~ "Baseline"))

ecf_gather <- ecf_gather %>% mutate(Avoided.mort = case_when(
     Scenario %in%  c(keys_likely_mort) ~ "Most likely mortality",
     Scenario %in%  c(keys_low_mort) ~ "Low mortality",
     Scenario %in%  c(keys_high_mort) ~ "High mortality",
     TRUE ~ "Baseline"))

ecf_gather$Avoided.mort <- factor(ecf_gather$Avoided.mort)
ecf_gather$adopt_level <- factor(ecf_gather$adopt_level)

ecf_gather_s <- filter(ecf_gather,adopt_level != "Baseline" | Avoided.mort != "Baseline")
ecf_gather_b <- filter(ecf_gather,adopt_level == "Baseline" | Avoided.mort == "Baseline")
ecf_gather_b <- ecf_gather_b[-4]
ecf_gather_s <- ecf_gather_s %>% mutate(adopt_level = fct_relevel(adopt_level,
"Low adoption","Most likely adoption","High adoption","Baseline"))

ecf_gather_s <- ecf_gather_s %>% mutate(Avoided.mort = fct_relevel(Avoided.mort,
"Low mortality","Most likely mortality","High mortality","Baseline"))

pal_prod <- pal_bcr

names(pal_prod) <- c("Low mortality","Most likely mortality","High mortality")

(FIGURE_ecf_herd <- ggplot(data = ecf_gather_s) +
          geom_line(mapping = aes(x = Year, y = Value, colour = Avoided.mort),size=1.25) +
          scale_color_manual(name = "Avoided.mort", values = pal_prod) +
          geom_line(aes(x = Year, y = Value), data = ecf_gather_b,
                    colour = ("brown"),size = 1.1, linetype = 2) +
          facet_wrap(~adopt_level,nrow = 1,ncol = 3) +
          scale_x_continuous(breaks = c(seq(2007,2027,5)),limits = c(2007,2027.21),expand = c(0,0)) +
          labs(y = "Cattle herd index = 100",
               title = "Fig. 6.3 Projected cattle herds by vaccine adoption and mortality avoided, 2007-2027") +
          scale_y_continuous(labels = scales::comma) +
         theme_tufte() +
          theme(text = element_text(size = 11),legend.position = "bottom")) +
     labs(caption = paste(c("Constructed from DynMod model for ECF in Kenya. National cattle herd was approximately 17.5 m head in base year.
     Dashed brown line is baseline projection without vaccination"),collapse = "")) +
          theme(plot.caption = element_text(hjust = 0))

#---------------------agricultural production index as f(vaccine)
ecf_prod <- read.xlsx("CHAPTER_6_ECF_PORTAL.xlsx",sheet = "AgRevenueTranspose", colNames = TRUE,rowNames = FALSE,
skipEmptyRows = TRUE, skipEmptyCols = TRUE,na.strings = "NA")
ecf_prod_gather <- pivot_longer(ecf_prod,Baseline:Scenario.9,
                                names_to = "Scenario",
                                values_to = "Value")

keys_likely_adopt <- c("Scenario.1","Scenario.2","Scenario.3")
keys_low_adopt <- c("Scenario.4","Scenario.5","Scenario.6")
keys_high_adopt <- c("Scenario.7","Scenario.8","Scenario.9")

keys_likely_mort <- c("Scenario.1","Scenario.4","Scenario.7")
keys_low_mort <- c("Scenario.2","Scenario.5","Scenario.8")
keys_high_mort <- c("Scenario.3","Scenario.6","Scenario.9")

ecf_prod_gather <- ecf_prod_gather %>% mutate(adopt.level = case_when(
     Scenario %in%  c(keys_likely_adopt) ~ "Most likely adoption",
     Scenario %in%  c(keys_low_adopt) ~ "Low adoption",
     Scenario %in%  c(keys_high_adopt) ~ "High adoption",
     TRUE ~ "Baseline"))

ecf_prod_gather <- ecf_prod_gather %>% mutate(Avoid.mort = case_when(
     Scenario %in%  c(keys_likely_mort) ~ "Most likely mortality",
     Scenario %in%  c(keys_low_mort) ~ "Low mortality",
     Scenario %in%  c(keys_high_mort) ~ "High mortality",
     TRUE ~ "Baseline"))

ecf_prod_gather$adopt.level <- factor(ecf_prod_gather$adopt.level)
ecf_prod_gather$Avoid.mort <- factor(ecf_prod_gather$Avoid.mort)

ecf_prod_gather_s <- filter(ecf_prod_gather,adopt.level != "Baseline" | Avoid.mort != "Baseline")
ecf_prod_gather_b <- filter(ecf_prod_gather,adopt.level == "Baseline" | Avoid.mort == "Baseline")
ecf_prod_gather_b <- ecf_prod_gather_b[-4]
ecf_prod_gather_s <- ecf_prod_gather_s %>% mutate(adopt.level = fct_relevel(adopt.level,
"Low adoption","Most likely adoption","High adoption","Baseline"))

ecf_prod_gather_s <- ecf_prod_gather_s %>% mutate(Avoid.mort = fct_relevel(Avoid.mort,
"Low mortality","Most likely mortality","High mortality","Baseline"))

(FIGURE_ecf_prod <- ggplot(data = ecf_prod_gather_s) +
     geom_line(mapping = aes(x = Year, y = Value, colour = Avoid.mort),size=1.25) +
     scale_color_manual(name = "Avoided.mort", values = pal_prod) +
     geom_line(aes(x = Year, y = Value), data = ecf_prod_gather_b,colour = ("brown"),size = 1.1, linetype = 2) +
     facet_wrap(~adopt.level,nrow = 1,ncol = 3) +
     scale_x_continuous(breaks = c(seq(2007,2027,5)),limits = c(2007,2027.21),expand = c(0,0)) +
     labs(x = "Year", y = "Agricultural production index = 100 in 2007",
     title = "Fig. 6.4 Index of agricultural production in Kenya by vaccine adoption and mortality avoided, 2007-2027") +
     guides(fill = guide_legend(title = "Avoided mortality")) +
     scale_y_continuous(labels = scales::comma) +
     theme_tufte() +
     theme(text = element_text(size = 11),legend.position = "bottom")) +
     labs(caption = paste(c("Constructed from DynMod model for ECF in Kenya.
     Dashed brown line is baseline projection without vaccination"),collapse = "")) +
     theme(plot.caption = element_text(hjust = 0))

# following code is commented out
# if you want to export the figures to tiff files then uncomment
#

# ggarrange(FIGURE_ecf_all, ncol = 1, nrow = 1, align = "h") %>%
#      ggexport(filename = paste0("Fig. 6.2 Projected ECF vaccine adoption rates by adoption scenario and production system, 2007-2027",".tiff"),collapse = "")
#
# ggarrange(FIGURE_ecf_herd, ncol = 1, nrow = 1, align = "h") %>%
#      ggexport(filename = paste0("Fig. 6.3 Projected cattle herds by vaccine adoption and mortality avoided, 2007-2027",".tiff"), collapse = "")
#
# ggarrange(FIGURE_ecf_prod, ncol = 1, nrow = 1, align = "h") %>%
#      ggexport(filename = paste0("Fig. 6.4 Index of agricultural production in Kenya by vaccine adoption and mortality avoided, 2007-2027",".tiff"), collapse = "")
#
# ggarrange(FIGURE_ecf_bcr, ncol = 1, nrow = 1, align = "h") %>%
#      ggexport(filename = paste0("Fig. 6.5 Benefit cost ratios for ECF vaccination research and development programmes by vaccine adoption levels and mortality avoided",".tiff"), collapse = "")
