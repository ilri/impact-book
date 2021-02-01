#code to generate figures for the Introduction
#
#NOTA BENE: some of the labels and notes to the Figures may differ
#slightly from what is in the book because of formatting changes made during printing
#

setwd("C:/Users/Jmm19/Documents/______ILRI/_______________DATA_PORTAL")


library(bibliometrix)
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
library(gridExtra)
library(ggpubr)
library(grid)
library(graphics)
library(scales)
library(stringi)
library(stringr)
library(openxlsx)
library(forcats)
library(janitor)

loadfonts(device = "win")
windowsFonts(Times = windowsFont("Times New Roman"))


#set up the colors

col_feed_forage <- c("#1DB000FF")
col_primprod <- col_feed_forage
col_econ_policy <- c("dodgerblue")
col_live_sys <- c("#E8C285FF")
col_ahg <- c("#FF0000FF")
col_management_and_other <- c("gray")
col_capdev <- c("#00E5FFAA")

col_range <- c("#FFFF00AA")

col_ILRI <- c("#682622FF")
col_ILCA <- col_ILRI
col_ILRAD <- col_ILRI

col_arid <- c("#ECB176FF")
col_semiarid <- c("#E8C285FF")
col_subhumid <- c("#B6DB00FF")
col_humid <- c("#1DB000FF")
col_highlands <- c("#F0C9C0FF")

col_chal_vlow <- c("#FFFFCAFF")
col_chal_low <- c("#FFDD00FF")
col_chal_med <- c("#FF6E00FF")
col_chal_high <- c("#FF0000FF")

col_theil <- c("#E6E600FF")
col_tryps <- c("#ECB176FF")

col_domain_6 <- c(col_feed_forage,col_econ_policy,col_live_sys,
                  col_ahg,col_management_and_other,col_ILRI)

#create the figure titles
tles.Fig.I.1a <- c("Fig.I.1 (a) Area of sub-Saharan Africa by agroecological zone, mid-1970s")
tles.Fig.I.1b <- c("Fig.I.1 (b) Rural population by agroecological zone, mid-1970s")
tles.Fig.I.1c <- c("Fig.I.1 (c) Zonal rural population shares, mid-1970s")
tles.Fig.I.1d <- c("Fig.I.1 (d) Zonal rural population densities, mid-1970s")

tles.Fig.I.2a <- c("Fig.I.2 (a) Numbers of tropical livestock units by agroecological zone, mid-1970s")
tles.Fig.I.2b <- c("Fig.I.2 (b) Tropical livestock unit density by agroecological zone, mid-1970s")
tles.Fig.I.2c <- c("Fig.I.2 (c) Ruminant livestock numbers by agroecological zone, mid-1970s")
tles.Fig.I.2d <- c("Fig.I.2 (d) Zonal ruminant populations as percentages of total, mid-1970s")

tles.Fig.I.3a <- c("Fig.I.3 (a) Agricultural gross domestic product by agroecological zone, mid-1970s")
tles.Fig.I.3b <- c("Fig.I.3 (b) Livestock share of agricultural GDP by agroecological zone, mid-1970s")

tles.Fig.I.4a <- c("Fig.I.4 (a) Area of tsetse fly infestation by agroecological zone")
tles.Fig.I.4b <- c("Fig.I.4 (b) Zonal area shares infested by tsetse")
tles.Fig.I.4c <- c("Fig.I.4 (c) Ruminant TLU and area by tsetse challenge, mid-1970s")
tles.Fig.I.4d <- c("Fig.I.4 (d) Potential zonal gross benefits from tsetse clearance and trypanosomiasis control, mid-1970s")

tles.Fig.I.5  <- c("Fig.I.5 ILRAD spending by research domain and period, 1975-1994")

tles.Fig.I.6  <- c("Fig.I.6 ILRAD publications by research domain and period, 1975-1994")

tles.Fig.I.7  <- c("Fig.I.7 ILCA spending by research domain and period, 1975-1994")

tles.Fig.I.8  <- c("Fig.I.8 ILCA publications by research domain and period, 1975-1994")

tles.Fig.I.9a  <- c("Fig.I.9 (a) ILRI spending by source of funds and period, 1975-2018")
tles.Fig.I.9b  <- c("Fig.I.9 (b) ILRI spending by research domain and period, 1975-2018")

tles.Fig.I.10 <- c("Fig.I.10. ILRI publications by research domain and publication period, 1975-2018")


#read some data from Jahnke,"livestock production systems and livestock development
#in tropical Africa" (1982)
#which can be found at https://www.dphu.org/uploads/attachements/books/books_2344_0.pdf
#
#

Jahnke_data <- data.table(read.xlsx("JAHNKE_PORTAL.xlsx",
sheet = "Jahnke_R",startRow = 1,colNames = T,na.strings = "NA"))
Jahnke_data <- rename(Jahnke_data,"Challenge" = "Challenge_level")

# Sort the data in descending order by area
Jahnke_data <- arrange(Jahnke_data,Order)
Jahnke_data$Zone <- factor(Jahnke_data$Zone)

Jahnke_data <- Jahnke_data %>%
  mutate(Zone = fct_relevel(Zone,"Arid","Semiarid","Subhumid","Humid","Highlands"))

#
#objects beginning with "pal_" are color palettes
#

pal_fig_1_thru_5 <- c(col_arid,col_semiarid,col_subhumid,col_humid,col_highlands)
names(pal_fig_1_thru_5) <- levels(Jahnke_data$Zone)

Jahnke_data$Challenge <- factor(Jahnke_data$Challenge)
Jahnke_data <- Jahnke_data %>%
  mutate(Challenge = fct_relevel(Challenge,"High","Medium","Low","Very Low"))
pal_chall <- c(col_chal_high,col_chal_med,col_chal_low,col_chal_vlow)
names(pal_chall) <- levels(Jahnke_data$Challenge_class)


#make Figures 1 thru 4
(Intro_AEZ_Area <- ggplot(Jahnke_data) +
    geom_col(mapping = aes(x = Zone,y = Zone.Area/1000,fill = Zone)) +
    labs(x = "Agroecological zone", y = "Area in millions of sq km",title = tles.Fig.I.1a) +
    scale_fill_manual(name = "Zone", values = pal_fig_1_thru_5) +
    theme_tufte() +
    theme(text = element_text(size = 11,family = "Times"),legend.position = "none")) +
     labs(caption = "Constructed by authors from data in Jahnke, 1982: Annex tables 1-11.
          Arid zones have rainfall of less than 200 mm annually, with (< 90 days suitable for crop production);
          semi-arid, 200-600 mm (90-179 growing days); subhumid, 600-1200 mm (180-269 growing days);
          and humid, 1000-2000 mm (>= 270 growing days). Jahnke (p. 152) defines 'tropical highlands' as
          having 'a mean daily temperature of less than 20 C during the growing period').") +
     theme(plot.caption = element_text(hjust = 0))

(Intro_AEZ_RuralPops <- ggplot(Jahnke_data) +
    geom_col(mapping = aes(x = Zone,y = RuralPop/1000,fill = Zone)) +
    labs(x = "Agroecological zone", y = "Rural population in millions",title = tles.Fig.I.1b) +
     scale_fill_manual(name = "Zone",values = pal_fig_1_thru_5) +
     theme_tufte() +
    theme(text = element_text(size = 11,family = "Times"),legend.position = "none")) +
     labs(caption = "Constructed by authors from data in Jahnke, 1982: Annex tables 1-11.") +
     theme(plot.caption = element_text(hjust = 0))

(Intro_AEZ_RuralPopShares  <- ggplot(Jahnke_data) +
    geom_col(mapping = aes(x = Zone,y = Pop.prop,fill = Zone)) +
    labs(x = "Agroecological zone",y = "Percentages of total rural population",
         title = tles.Fig.I.1c) +
     scale_fill_manual(name = "Zone",values = pal_fig_1_thru_5) +
    theme_tufte() +
    theme(text = element_text(size = 11,family = "Times"),legend.position = "none")) +
labs(caption = "Constructed by authors from data in Jahnke, 1982: Annex tables 1-11.") +
     theme(plot.caption = element_text(hjust = 0))

(Intro_AEZ_Rural_PopDensity  <- ggplot(Jahnke_data) +
    geom_col(mapping = aes(x = Zone,y = Pop_density,fill = Zone)) +
    labs(x = "Agroecological zone",y = "Rural population density in #/sq km",
         title = tles.Fig.I.1d) +
    scale_fill_manual(name = "Zone", values = pal_fig_1_thru_5) +
    theme_tufte() +
    theme(text = element_text(size = 11,family = "Times"),legend.position = "none")) +
     labs(caption = "Constructed by authors from data in Jahnke, 1982: Annex tables 1-11.") +
     theme(plot.caption = element_text(hjust = 0))

(Intro_AEZ_TLU  <- ggplot(Jahnke_data) +
    geom_col(mapping = aes(x = Zone,y = Zone.TLU/1000,fill = Zone)) +
    labs(x = "Agroecological zone",y = "TLU in millions",title = tles.Fig.I.2a) +
    scale_fill_manual(name = "Zone", values = pal_fig_1_thru_5) +
    theme_tufte() +
    theme(text = element_text(size = 11,family = "Times"),legend.position = "none")) +
     labs(caption = "Constructed by authors from data in Jahnke, 1982: Annex tables 1-11.") +
     theme(plot.caption = element_text(hjust = 0))

(Intro_TLU_Density  <- ggplot(Jahnke_data) +
    geom_col(mapping = aes(x = Zone,y = TLU_density, fill = Zone)) +
    labs(x = "Agroecological zone",y = "TLU density in number/sq km",
         title = tles.Fig.I.2b) +
    scale_fill_manual(name = "Zone", values = pal_fig_1_thru_5) +
    theme_tufte() +
    theme(text = element_text(size = 11,family = "Times"),legend.position = "none"))

(Intro_AEZ_Ruminants  <- ggplot(Jahnke_data) +
    geom_col(mapping = aes(x = Zone, y = Zone.Ruminants/1000, fill = Zone)) +
    labs(x = "Agroecological zone",y = "Head in millions",title = tles.Fig.I.2c) +
          scale_fill_manual(name = "Zone", values = pal_fig_1_thru_5) +
    theme_tufte() +
    theme(text = element_text(size = 11,family = "Times"),legend.position = "none")) +
     labs(caption = "Constructed by authors from data in Jahnke, 1982: Annex tables 1-11.") +
     theme(plot.caption = element_text(hjust = 0))

(Intro_AEZ_Ruminant_Shares  <- ggplot(Jahnke_data) +
    geom_col(mapping = aes(x = Zone, y = Rumin.prop, fill = Zone)) +
    labs(x = "Agroecological zone",y = "Percentages of total ruminant population",
    title = tles.Fig.I.2d) +
     scale_fill_manual(name = "Zone", values = pal_fig_1_thru_5) +
    theme_tufte() +
    theme(text = element_text(size = 11,family = "Times"),legend.position = "none")) +
     labs(caption = "Constructed by authors from data in Jahnke, 1982: Annex tables 1-11.") +
     theme(plot.caption = element_text(hjust = 0))

(Intro_AEZ_Agric_GDP  <- ggplot(Jahnke_data) +
    geom_col(mapping = aes(x = Zone, y = Agric.GDP, fill = Zone)) +
    labs(x = "Agroecological zone", y = "Millions of current US$",title = tles.Fig.I.3a) +
     scale_fill_manual(name = "Zone", values = pal_fig_1_thru_5) +
     scale_y_continuous(labels = scales::comma) +
     theme_tufte() +
     theme(text = element_text(size = 11,family = "Times"),legend.position = "none")) +
     labs(caption = "Constructed by authors from data in Jahnke, 1982: Annex tables 1-11.") +
     theme(plot.caption = element_text(hjust = 0))

(Intro_AEZ_Live_GDP_Shares  <- ggplot(Jahnke_data) +
    geom_col(mapping = aes(x = Zone, y = LiveGDP.AgGDP, fill = Zone)) +
    labs(x = "Agroecological zone", y = "Livestock GDP as percentage of agricultural GDP",title = tles.Fig.I.3b) +
    scale_fill_manual(name = "Zone", values = pal_fig_1_thru_5) +
    theme_tufte() +
    theme(text = element_text(size = 11,family = "Times"),legend.position = "none")) +
labs(caption = "Constructed by authors from data in Jahnke, 1982: Annex tables 1-11.") +
     theme(plot.caption = element_text(hjust = 0))


(Intro_AEZ_Tsetse_Area  <- ggplot(Jahnke_data) +
    geom_col(mapping = aes(x = Zone, y = Tsetse.Area, fill = Zone)) +
    labs(x = "Agroecological zone",y = "Rural area in 000s sq km",
         title = tles.Fig.I.4a) +
     scale_fill_manual(name = "Zone", values = pal_fig_1_thru_5) +
     theme_tufte() +
    scale_y_continuous(labels = scales::comma) +
    theme(text = element_text(size = 11,family = "Times"),legend.position = "none")) +
     labs(caption = "Constructed by authors from data in Jahnke, 1982: Annex tables 1-11.") +
     theme(plot.caption = element_text(hjust = 0))

(Intro_AEZ_Tsetse_Area_Shares  <- ggplot(Jahnke_data) +
    geom_col(mapping = aes(x = Zone, y =  Tsetse.area.prop, fill = Zone)) +
    labs(x = "Agroecological zone",y = "Percentages of rural area infested",
         title = tles.Fig.I.4b) +
    scale_fill_manual(name = "Zone", values = pal_fig_1_thru_5) +
    theme_tufte() +
    scale_y_continuous(labels = scales::comma) +
    theme(text = element_text(size = 11,family = "Times"),legend.position = "none")) +
     labs(caption = "Constructed by authors from data in Jahnke, 1982: Annex tables 1-11.") +
     theme(plot.caption = element_text(hjust = 0))

#figure I.4c is unusual

Jahnke_data_tsetse <- Jahnke_data %>% select(Zone,Challenge,
 `TLU by challenge level in numbers` = TLU.tsetse.risk,
 `Area by challenge level in km2` = Tsetse.Area)
Jahnke_data_tsetse_gather <- Jahnke_data_tsetse %>%
 pivot_longer(`TLU by challenge level in numbers`:
`Area by challenge level in km2`,names_to = "Variable", values_to = "Value")

# Jahnke_data_tsetse_group <- Jahnke_data_tsetse_gather %>% group_by(Challenge) %>%
#      summarise(sumTLU=sum(Zone.TLU),meanDensTLU=sum(Zone.TLU)/sum(Zone.Area))

(Intro_AEZ_Ruminants_Tsetse_Class  <- ggplot(Jahnke_data_tsetse_gather) +
    geom_col(mapping = aes(x = Challenge, y = Value, fill = Challenge)) +
    labs(x = "Tsetse challenge level",y = "Thousands",title = tles.Fig.I.4c) +
    facet_wrap(~Variable,nrow=2,ncol=1,scale = "free_y") +
    scale_fill_manual(name = "Challenge", values = pal_chall) +
    theme_tufte() +
    scale_y_continuous(labels = scales::comma) +
   theme(text = element_text(size = 11,family = "Times"),legend.position = "none")) +
     labs(caption = "Constructed by authors from data in Jahnke, 1982: Annex tables 1-11.") +
     theme(plot.caption = element_text(hjust = 0))

(Intro_AEZ_Gross_Benefits  <- ggplot(Jahnke_data) +
    geom_col(mapping = aes(x = Zone, y = RelValue_tryps, fill = Zone)) +
    labs(x = "Agroecological zone",
         y = "Potential benefits in percentage of zonal livestock GDP",title = tles.Fig.I.4d) +
          scale_fill_manual(name = "Zone", values = pal_fig_1_thru_5) +
            theme_tufte() +
    scale_y_continuous(labels = scales::comma) +
    theme(text = element_text(size = 11,family = "Times"),legend.position = "none")) +
     labs(caption = "Constructed by authors from data in Jahnke, 1982: Annex tables 1-11.") +
     theme(plot.caption = element_text(hjust = 0))

#--------------------------data for figures 5 through 10
#
#NOTA BENE: the financial data are in 2015$
Extract <- read.xlsx("FINANCIAL_PORTAL.xlsx",
sheet = "Spend_Cites_R",startRow = 1,colNames = T,na.strings = "NA")

Extract <- data.table(Extract)

Extract_ILCA <- filter(Extract,YEAR <= 1994) %>% select(YEAR,contains("ILCA"))
Extract_ILCA <- data.table(Extract_ILCA)

Extract_ILRAD <- filter(Extract, YEAR <= 1994) %>% select(YEAR,contains("ILRAD"))
Extract_ILRAD <- data.table(Extract_ILRAD)

Extract_ILRI <- filter(Extract,YEAR >= 1975 & YEAR <= 2018)
Extract_ILRI <- data.table(Extract_ILRI)

#--------------------------Figure 5-----------------------------
#
Spend_ILRAD <- Extract_ILRAD %>% select(Year = YEAR,
Trypanosomiasis = ILRAD_ILRI_TRYPS_SPEND,
 Theileriosis = ILRAD_ILRI_ECF_SPEND,
 `Management and other` = ILRAD_MANAGE_SPEND,
 `Total ILRAD spending` = ILRAD_TOTAL_SPEND)

#group the ILRAD spending data and plot as geom_col
Spend_ILRAD <- data.table(Spend_ILRAD %>%
 mutate(Period = case_when(
 Year < 1981 ~ "1975-80",
 Year >= 1981 & Year <= 1985 ~ "1981-85",
 Year >= 1986 & Year <= 1990 ~ "1986-90",
 Year >= 1991 & Year <= 1995 ~ "1991-94")))

Spend_ILRAD_gather <- pivot_longer(Spend_ILRAD,
Trypanosomiasis:`Total ILRAD spending`,names_to = "Domain",values_to = "Values")

Spend_ILRAD_group <- group_by(Spend_ILRAD_gather,Period,Domain) %>%
  summarise(sumDomain = sum(Values))

Spend_ILRAD_group <- Spend_ILRAD_group %>%
  mutate(Domain = fct_relevel(Domain,"Management and other",
                            "Theileriosis",
                            "Trypanosomiasis",
                            "Total ILRAD spending"))

pal_fig_5 <- c(col_management_and_other,col_theil,col_tryps,col_ILRAD)
names(pal_fig_5) <- levels(Spend_ILRAD_group$Domain)

(sample_ILRAD.spend.total <- round(sum(Spend_ILRAD$`Total ILRAD spending`/1000),digits = 1))

(INTRO_ILRAD_Spend <- ggplot(data = Spend_ILRAD_group) +
    geom_col(mapping = aes(x = Period,y = sumDomain/1000,fill = Domain),
             position = position_dodge(), size = 1) +
    labs(x = "Period totals",y = "Spending in 2015 US$ millions",
    title = tles.Fig.I.5) +
    theme_tufte() +
    scale_fill_manual(name = "Domain", values = pal_fig_5) +
    scale_y_continuous(labels = scales::comma) +
    theme(text = element_text(size = 11,family = "Times"),legend.position = "bottom")) +
     labs(caption = paste(c("Constructed by authors from data in ILRAD Annual Reports, various years. Total ILRAD spending (1975-1994) of ",sample_ILRAD.spend.total," US$ millions, in 2015 US$ millions."),collapse = "")) +
     theme(plot.caption = element_text(hjust = 0))

#
#ILRAD spending-----------------------------------
#

(tibble_ILRAD.spend.domain <- Spend_ILRAD_group %>% group_by(Domain)
     %>% summarise(sumSpend = sum(sumDomain)))

i <- 1

for (i in 1:length(tibble_ILRAD.spend.domain$Domain)) {
tibble_ILRAD.spend.domain[i,"spendShare"] <- round(100*(tibble_ILRAD.spend.domain[i,2] / tibble_ILRAD.spend.domain[4,2]),digits = 1)
}

(tibble_ILRAD.spend.domain)
(tibble_ILRAD.spend.domain[2,3] + tibble_ILRAD.spend.domain[3,3])
(tibble_ILRAD.spend.domain[3,3] / (tibble_ILRAD.spend.domain[2,3] + tibble_ILRAD.spend.domain[3,3]))

#--------------------------Figure 6 ------------------------
Biblio_ILRAD <- Extract_ILRAD %>% select(Year = YEAR,
                         `Trypanosomiasis papers` = ILRAD_ILRI_TRYPS_PAPERS,
                         `Theileriosis papers` = ILRAD_ILRI_ECF_PAPERS,
                         `Total ILRAD papers` = ILRAD_ALL_PAPERS,
                         `Trypanosomiasis citations` = ILRAD_ILRI_TRYPS_TC,
                         `Theileriosis citations` = ILRAD_ILRI_ECF_TC,
                         `Total ILRAD citations` = ILRAD_ALL_TC)

Biblio_ILRAD <- Biblio_ILRAD %>%
  mutate(Period = case_when(
    Year >= 1975 & Year <= 1980 ~ "1975-80",
    Year >= 1981 & Year <= 1985 ~ "1981-85",
    Year >= 1986 & Year <= 1990 ~ "1986-90",
    Year >= 1991 & Year <= 1994 ~ "1991-94",
    TRUE ~ "Other"))
Biblio_ILRAD$Period <- as.factor(Biblio_ILRAD$Period)

Biblio_ILRAD <- select(Biblio_ILRAD,Year,Period,`Trypanosomiasis papers` : `Total ILRAD citations`)
Biblio_ILRAD_p <- select(Biblio_ILRAD,Year,Period,contains("papers"))
Biblio_ILRAD_p$Bibliometric <- factor("Papers")
Biblio_ILRAD_p <- select(Biblio_ILRAD_p,Year,Period,Bibliometric,
                         Trypanosomiasis = `Trypanosomiasis papers`,
                         Theileriosis = `Theileriosis papers`,
                         `Total ILRAD` = `Total ILRAD papers`)
Biblio_ILRAD_p <- pivot_longer(Biblio_ILRAD_p,Trypanosomiasis:`Total ILRAD`,
 names_to = "Domain",values_to = "Values")

Biblio_ILRAD_c <- select(Biblio_ILRAD,Year,Period,contains("citations"))
Biblio_ILRAD_c$Bibliometric <- factor("Citations")
Biblio_ILRAD_c <- select(Biblio_ILRAD_c,Year,Period,Bibliometric,
 Trypanosomiasis = `Trypanosomiasis citations`,
 Theileriosis = `Theileriosis citations`,
 `Total ILRAD` = `Total ILRAD citations`)
Biblio_ILRAD_c <- pivot_longer(Biblio_ILRAD_c,Trypanosomiasis:`Total ILRAD`,
 names_to = "Domain",values_to = "Values")

Biblio_ILRAD <- rbind(Biblio_ILRAD_p,Biblio_ILRAD_c)
Biblio_ILRAD$Domain <- factor(Biblio_ILRAD$Domain)
Biblio_ILRAD <- Biblio_ILRAD %>%  mutate(Domain = fct_relevel(Domain,
                            "Theileriosis",
                            "Trypanosomiasis",
                            "Total ILRAD"))


Biblio_ILRAD_group <- group_by(Biblio_ILRAD,Period,Domain,Bibliometric)


pal_fig_6 <- c("#E6E600FF","#ECB176FF", "#682622DD")
names(pal_fig_6) <- levels(Biblio_ILRAD_group$Domain)
(sample_ILRAD.papers.total <- filter(Biblio_ILRAD,Domain == "Total ILRAD" & Bibliometric == "Papers") %>%
          summarise(nILRADpapers <- sum(Values)))

(INTRO_ILRAD_Biblio <- ggplot(data = Biblio_ILRAD_group) +
 geom_col(aes(x = Period, y = Values, fill = Domain),
          size = 1,position = position_dodge()) +
 facet_wrap(~ Bibliometric,ncol = 1,nrow = 2,scales = "free_y") +
  labs(x = "Publication period", y = "Counts by publication period",title = tles.Fig.I.6) +
  theme_tufte() +
  scale_fill_manual(name = "Domain",values = pal_fig_6) +
  scale_y_continuous(labels = scales::comma) +
  theme(text = element_text(size = 11,family = "Times"),legend.position = "bottom")) +
     labs(caption = paste(c("Constructed by authors from data at www.scopus.com and www.scholar.google.com. Sample size = ",sample_ILRAD.papers.total," papers."),collapse = "")) +
     theme(plot.caption = element_text(hjust = 0))


#-----------------------------Figure 7---------------------------------------

Extract_Spend_ILCA <- select(Extract_ILCA,Year = YEAR,contains("SPEND"))
Extract_Spend_ILCA$PB <- (Extract_Spend_ILCA$ILCA_ILRI_LOCAL_ENV_SPEND +
                     Extract_Spend_ILCA$ILCA_ILRI_PLANT_BIOMASS_SPEND)
Extract_Spend_ILCA$AHG <- Extract_Spend_ILCA$ILCA_ILRI_ANIMAL_HG_SPEND +
                      Extract_Spend_ILCA$ILCA_ILRI_ANIMAL_SCIENCE_SPEND
Extract_Spend_ILCA$MO <- Extract_Spend_ILCA$ILCA_ILRI_CAP_DEV_SPEND +
                    Extract_Spend_ILCA$ILCA_ILRI_MANAGEMENT_SPEND +
                    Extract_Spend_ILCA$ILCA_ILRI_OTHER_SPEND

Spend_ILCA <- select(Extract_Spend_ILCA,Year,
 `Livestock systems` = ILCA_ILRI_SYSTEMS_SPEND,
 `Economics and policy` = ILCA_ILRI_POLICY_SPEND,
 `Primary production` = PB,
 `Animal production, health and genetics` = AHG,
 `Management and other` = MO,
`Total ILCA spending` = ILCA_ILRI_TOTAL_SPEND)

Spend_ILCA <- data.table(Spend_ILCA %>%
 mutate(Period = case_when(
  Year < 1981 ~ "1975-80",
  Year >= 1981 & Year <= 1985 ~ "1981-85",
  Year >= 1986 & Year <= 1990 ~ "1986-90",
  Year >= 1991 & Year <= 1995 ~ "1991-94")))
Spend_ILCA <- select(Spend_ILCA,Year,Period,`Livestock systems`:`Total ILCA spending`)

Spend_ILCA_gather <- Spend_ILCA %>% pivot_longer(`Livestock systems`:`Total ILCA spending`,
 names_to = "Domain",values_to = "Values")

Spend_ILCA_group <- group_by(Spend_ILCA_gather,Period,Domain) %>%
  summarise(sumValue = sum(Values))

Spend_ILCA_group <- Spend_ILCA_group %>%
  mutate(Domain = fct_relevel(Domain,
 "Primary production",
 "Economics and policy","Livestock systems",
 "Animal production, health and genetics",
 "Management and other","Total ILCA spending"))

pal_fig_7 <- c(col_feed_forage,col_econ_policy,col_live_sys,col_ahg,col_management_and_other,col_ILCA)
names(pal_fig_7) <- levels(Spend_ILCA_group$Domain)
(sample_ILCA.spend.total <- round(sum(Spend_ILCA$`Total ILCA spending`/1000),digits = 1))

(INTRO_ILCA_Spend <- ggplot(data = Spend_ILCA_group) +
    geom_col(mapping = aes(x = Period,y = sumValue/1000,
                fill = Domain), position = position_dodge(), size = 1) +
    labs(x = "Period totals",y = "Spending in 2015 US$ millions",title = tles.Fig.I.7) +
            scale_fill_manual(name = "Domain", values = pal_fig_7) +
    scale_y_continuous(labels = scales::comma) +
    theme_tufte() +
    theme(text = element_text(size = 11,family = "Times"),legend.position = "bottom")) +
     labs(caption = paste(c("Constructed by authors from data in ILCA Annual Reports, various years. Total ILCA spending (1975-1994) of ",sample_ILCA.spend.total," US$ millions, in 2015 US$ millions."),collapse = "")) +
     theme(plot.caption = element_text(hjust = 0))

#ILCA spending-----------------------------------

(tibble_ILCA.spend.domain <- Spend_ILCA_group %>% group_by(Domain)
     %>% summarise(sumSpend = sum(sumValue)))

i <- 1

for (i in 1:length(tibble_ILCA.spend.domain$Domain)) {
    tibble_ILCA.spend.domain[i,"spendShare"] <- round(100*(tibble_ILCA.spend.domain[i,2] / tibble_ILCA.spend.domain[6,2]),digits = 1)
}

(Spend_ILCA %>% filter(Year <= 1987) %>% summarise(totalSpend = sum(`Total ILCA spending`)))/13000
(Spend_ILCA %>% filter(Year >= 1988) %>% summarise(totalSpend = sum(`Total ILCA spending`)))/7000

(tibble_ILCA.spend.domain)
(round((100*tibble_ILCA.spend.domain[6,2] / tibble_ILRAD.spend.domain[4,2]),digits = 1)) - 100
(tibble_ILCA.spend.domain[6,2]/20)
(tibble_ILCA.spend.domain[2,3] + tibble_ILCA.spend.domain[3,3])
(tibble_ILCA.spend.domain[3,3] / (tibble_ILCA.spend.domain[2,3] + tibble_ILCA.spend.domain[3,3]))

#------------------------Figure 8 -----------------------------------

Biblio_ILCA <- Extract_ILCA %>% select(Year = YEAR,
 `Total ILCA papers` = ILCA_ALL_PAPERS,
 `Economics and policy papers` = ECON_POLICY_ILCA_PAPERS,
 `Livestock systems papers` = FSR_ILCA_PAPERS,
 `Feed and forage papers` = PFORAGE_ILCA_PAPERS,
 `Rangelands papers` = ILCA_ILRI_GRASS_PAPERS,# + ILCA_ILRI_CLIMATE_PAPERS,
 `Animal science papers` = ANIMAL_ILCA_PAPERS,

 `Total ILCA citations` = ILCA_ALL_TC,
 `Economics and policy citations` = ECON_POLICY_ILCA_TC,
 `Livestock systems citations` = FSR_ILCA_TC,
 `Feed and forage citations` = PFORAGE_ILCA_TC,
 `Rangelands citations` = ILCA_ILRI_GRASS_TC,# + ILCA_ILRI_CLIMATE_TC),
 `Animal science citations` = ANIMAL_ILCA_TC)

Biblio_ILCA <- Biblio_ILCA %>%
  mutate(Period = case_when(
    Year >= 1975 & Year <= 1980 ~ "1975-80",
    Year >= 1981 & Year <= 1985 ~ "1981-85",
    Year >= 1986 & Year <= 1990 ~ "1986-90",
    Year >= 1991 & Year <= 1994 ~ "1991-94",
    TRUE ~ "Other"))
Biblio_ILCA$Period <- as.factor(Biblio_ILCA$Period)

Biblio_ILCA <- select(Biblio_ILCA,Year,Period,`Total ILCA papers`:`Animal science citations`)
Biblio_ILCA_p <- select(Biblio_ILCA,Year,Period,contains("papers"))
Biblio_ILCA_p$Bibliometric <- factor("Papers")
Biblio_ILCA_p <- select(Biblio_ILCA_p,Year,Period,Bibliometric,
                         `Economics and policy` = `Economics and policy papers`,
                         `Livestock systems` = `Livestock systems papers`,
                        `Feed and forage` = `Feed and forage papers`,
                        `Rangelands` = `Rangelands papers`,
                        `Animal science` = `Animal science papers`,
                        `Total ILCA` = `Total ILCA papers`)

Biblio_ILCA_p <- pivot_longer(Biblio_ILCA_p,`Economics and policy`:
`Total ILCA`,names_to = "Domain",values_to = "Values")

Biblio_ILCA_c <- select(Biblio_ILCA,Year,Period,contains("citations"))
Biblio_ILCA_c$Bibliometric <- factor("Citations")
Biblio_ILCA_c <- select(Biblio_ILCA_c,Year,Period,Bibliometric,
                        `Economics and policy` = `Economics and policy citations`,
                        `Livestock systems` = `Livestock systems citations`,
                        `Feed and forage` = `Feed and forage citations`,
                        `Rangelands` = `Rangelands citations`,
                        `Animal science` = `Animal science citations`,
                        `Total ILCA` = `Total ILCA citations`)

Biblio_ILCA_c <- pivot_longer(Biblio_ILCA_c,`Economics and policy`:
`Total ILCA`,names_to = "Domain",values_to = "Values")

Biblio_ILCA <- rbind(Biblio_ILCA_p,Biblio_ILCA_c)
Biblio_ILCA_group <- group_by(Biblio_ILCA,Period,Domain,Bibliometric)
Biblio_ILCA_group$Domain <- factor(Biblio_ILCA_group$Domain)

pal_fig_8 <- c(col_ahg,col_econ_policy,col_feed_forage,col_live_sys,col_range,col_ILRI)
names(pal_fig_8) <- levels(Biblio_ILCA_group$Domain)
(sample_ILCA.papers.total <- filter(Biblio_ILCA,Domain == "Total ILCA" & Bibliometric == "Papers") %>%
          summarise(nILCApapers <- sum(Values)))

(INTRO_ILCA_Biblio <- ggplot(data = Biblio_ILCA_group) +
    geom_col(aes(x = Period, y = Values, fill = Domain), size = 1,position = position_dodge()) +
    facet_wrap(~ Bibliometric,ncol = 1,nrow = 2,scales = "free_y",) +
    labs(x = "Publication period", y = "Counts by publication period",title = tles.Fig.I.8) +
      scale_fill_manual(name = "Domain", values = pal_fig_8) +
     scale_y_continuous(labels = scales::comma) +
     theme_tufte() +
     theme(text = element_text(size = 11,family = "Times"),legend.position = "bottom"))  +
     labs(caption = paste(c("Constructed by authors from data at www.scopus.com and www.scholar.google.com. Sample size = ",sample_ILCA.papers.total," papers."),collapse = "")) +
     theme(plot.caption = element_text(hjust = 0))


#
#newer version of Figure 9a ----------------------------------------
#

Spend_ILRI_source <- select(Extract_ILRI,Year = YEAR,
                            `Unrestricted funds` = ILCA_ILRI_UNRESTRICTED_SPEND,
                            `Restricted funds` = ILCA_ILRI_RESTRICT_SPEND,
                            `Window 1 & Window 2` = ILRI_W1_W2_SPEND,
                            `All sources` = LIFETIME_TOTAL_SPEND)
Spend_ILRI_source$`Restricted funds` <- Spend_ILRI_source$`Restricted funds` -
     Spend_ILRI_source$`Window 1 & Window 2`

Spend_ILRI_source_gather <- Spend_ILRI_source %>%
  pivot_longer(`Unrestricted funds`:`All sources`,
               names_to = "Funds",values_to = "Spending")
Spend_ILRI_source_gather$Funds <- factor(Spend_ILRI_source_gather$Funds)
Spend_ILRI_source_gather <- Spend_ILRI_source_gather %>%
     mutate(Funds = fct_relevel(Funds,"Window 1 & Window 2","Restricted funds",
"Unrestricted funds","All sources"))

Spend_ILRI_source_gather  <- Spend_ILRI_source_gather %>%
     mutate(Era = case_when(
          Year <=  1994 ~ "Before merger into ILRI",
          TRUE ~ "After merger into ILRI"))


Spend_ILRI_source_gather <- Spend_ILRI_source_gather %>%
  mutate(Period = case_when(
  Year >= 1975 & Year <= 1980 ~ "1975-80",
  Year >= 1981 & Year <= 1985 ~ "1981-85",
  Year >= 1986 & Year <= 1990 ~ "1986-90",
  Year >= 1991 & Year <= 1994 ~ "1991-94",
  Year >= 1995 & Year <= 2000 ~ "1995-00",
  Year >= 2001 & Year <= 2005 ~ "2001-05",
  Year >= 2006 & Year <= 2010 ~ "2006-10",
  TRUE ~ "2011-18"))

Spend_ILRI_source_gather <- Spend_ILRI_source_gather %>%
     mutate(Era = fct_relevel(Era,"Before merger into ILRI","After merger into ILRI"))

Spend_ILRI_group <- group_by(Spend_ILRI_source_gather,Era,Period,Funds) %>%
  summarise(sumFunds = sum(Spending))

pal_fig_9a <- c("#BB9D00FF","#8BD000AA","#00A600FF","#682622DD")
names(pal_fig_9a) <- levels(Spend_ILRI_group$Funds)
(ILRI.spend.sources <- filter(Spend_ILRI_group,Funds == "All sources")  %>%
     group_by(Period) %>% summarise(spendILRI.total = round(sum(sumFunds/1000),digits = 1)))
(sample_ILRI.spend.total <- sum(ILRI.spend.sources$spendILRI.total))


(INTRO_ILRI_Spend <- ggplot(data = Spend_ILRI_group) +
    geom_col(mapping = aes(x = Period,y = sumFunds/1000,fill = Funds),
    position = position_dodge(), size = 1) +
    facet_wrap(~Era,scales = "free", nrow = 1, ncol = 2) +
    labs(x = "Period totals",y = "Spending in 2015 US$ millions",title = tles.Fig.I.9a) +
    scale_fill_manual(name = "Funds", values = pal_fig_9a) +
    scale_y_continuous(labels = scales::comma) +
    theme_tufte() +
    theme(text = element_text(size = 11,family = "Times"),legend.position = "bottom")) +
    labs(caption = paste(c("Constructed by authors from data in ILRI Annual Reports and Financial Reports, various years.
                           Lifetime (1975-2018) spending of ",sample_ILRI.spend.total," US$ millions, in 2015 US$ millions."),collapse = "")) +
    theme(plot.caption = element_text(hjust = 0))

#ILRI spending
ILRI_1999 <- filter(Spend_ILRI_source,Year <= 2000)
(ILRI_1999.core <- sum(ILRI_1999$`Unrestricted funds`))
(ILRI_1999.all <- sum(ILRI_1999$`All sources`))
(ILRI_1999.core / ILRI_1999.all)

ILRI_2000 <- filter(Spend_ILRI_source,Year >= 2000 & Year <= 2011)
(ILRI_2000.core <- sum(ILRI_2000$`Unrestricted funds`))
(ILRI_2000.all <- sum(ILRI_2000$`All sources`))
(ILRI_2000.core / ILRI_2000.all)

(Spend_ILRI_source %>% filter(Year <= 2000) %>%
          sum(Spend_ILRI_source$`Unrestricted funds`))
          #sum(Spend_ILRI_source$`All sources`))

#-----------------------Figure 9b------------------------

Spend_ILRI_1975_2018 <- filter(Extract_ILRI,YEAR >= 1975) %>%
     select(Year = YEAR,
            `Total spending` = LIFETIME_TOTAL_SPEND,
            `Economics and policy` = GROUP_ECON_POLICY_SPEND,
            `Livestock systems` = GROUP_SYSTEMS_SPEND,
            `Primary production` = GROUP_PLANT_BIOMASS_SPEND,
             `Animal science` = GROUP_ANIM_SCI_SPEND,
             `Capacity development` = GROUP_CAP_DEV_SPEND,
             `Management and other` = GROUP_OTHER_SPEND)

Spend_ILRI_1975_2018_gather <- Spend_ILRI_1975_2018 %>%
  pivot_longer(`Total spending`:`Management and other`,
               names_to = "Domain",values_to = "Spending")
Spend_ILRI_1975_2018_gather$Domain <- factor(Spend_ILRI_1975_2018_gather$Domain)

Spend_ILRI_1975_2018_gather <- Spend_ILRI_1975_2018_gather %>%
     mutate(Domain = fct_relevel(Domain,
"Primary production","Capacity development",
 "Economics and policy","Livestock systems",
 "Management and other","Animal science",
 "Total spending"))

Spend_ILRI_1975_2018_gather  <- Spend_ILRI_1975_2018_gather %>%
     mutate(Era = case_when(
          Year <=  1994 ~ "Before merger into ILRI",
          TRUE ~ "After merger into ILRI"))

Spend_ILRI_1975_2018_gather <- Spend_ILRI_1975_2018_gather %>%
  mutate(Period = case_when(
            Year >= 1975 & Year <= 1980 ~ "1975-80",
            Year >= 1981 & Year <= 1985 ~ "1981-85",
            Year >= 1986 & Year <= 1990 ~ "1986-90",
            Year >= 1991 & Year <= 1994 ~ "1991-94",
            Year >= 1995 & Year <= 2000 ~ "1995-00",
            Year >= 2001 & Year <= 2005 ~ "2001-05",
            Year >= 2006 & Year <= 2010 ~ "2006-10",
            TRUE ~ "2011-18"))

Spend_ILRI_1975_2018_gather <- Spend_ILRI_1975_2018_gather %>%
     mutate(Era = fct_relevel(Era,"Before merger into ILRI","After merger into ILRI"))

Spend_ILRI_1975_2018_group <- group_by(Spend_ILRI_1975_2018_gather,Era,Period,Domain) %>% summarise(sumDomain = sum(Spending))
pal_fig_9b <- c(col_range,col_capdev,col_econ_policy,col_live_sys,col_management_and_other,col_ahg,col_ILRI)

names(pal_fig_9b) <- levels(Spend_ILRI_1975_2018_group$Domain)

(INTRO_ILRI_Domain <- ggplot(data = Spend_ILRI_1975_2018_group) +
    geom_col(mapping = aes(x = Period,y = sumDomain/1000,fill = Domain),
             position = position_dodge(), size = 1) +
          facet_wrap(~Era,scales = "free", nrow = 1, ncol = 2) +
    labs(x = "Period totals",y = "Spending in 2015 US$ millions",title = tles.Fig.I.9b) +
    scale_fill_manual(name = "Domain",values = pal_fig_9b) +
    scale_y_continuous(labels = scales::comma) +
    theme_tufte() +
    labs(caption = paste(c("Constructed by authors from data in ILRI Annual Reports and Financial Reports, various years.
     Lifetime (1975-2018) spending of ",sample_ILRI.spend.total," US$ millions, in 2015 US$ millions."),collapse = "")) +
    theme(plot.caption = element_text(hjust = 0)) +
    theme(text = element_text(size = 11,family = "Times"),legend.position = "bottom"))

#ILRI spending
(tibble_ILRI.spend.domain <- Spend_ILRI_1975_2018_group %>% group_by(Domain)
     %>% summarise(sumSpend = sum(sumDomain)))

for (i in 1:length(tibble_ILRI.spend.domain$Domain)) {
     tibble_ILRI.spend.domain[i,"spendShare"] <- round(100*(tibble_ILRI.spend.domain[i,2] / tibble_ILRI.spend.domain[7,2]),digits = 1)
}

(Spend_ILRI_1975_2018 %>% filter(Year >= 1988 & Year <= 1994) %>% summarise(totalSpend = sum(`Total spending`)))/7000
(Spend_ILRI_1975_2018 %>% filter(Year >= 1995 & Year <= 2001) %>% summarise(totalSpend = sum(`Total spending`)))/7000

(tibble_ILRI.spend.domain)
(round((100*tibble_ILRI.spend.domain[6,2] / tibble_ILRAD.spend.domain[4,2]),digits = 1)) - 100
(tibble_ILRI.spend.domain[6,2]/20)
(tibble_ILRI.spend.domain[2,3] + tibble_ILRI.spend.domain[3,3])
(tibble_ILRI.spend.domain[3,3] / (tibble_ILRI.spend.domain[2,3] + tibble_ILRI.spend.domain[3,3]))


#--------------------------Figure 10 ------------------------
Biblio_ILRI <- Extract_ILRI %>% select(Year = YEAR,
 `Total ILRI papers` = ILRI_ALL_PAPERS,
 `Economics and policy papers` = ECON_POLICY_ILRI_PAPERS,
 `Livestock systems papers` = ILCA_ILRI_FSR_PAPERS,
 `Rangelands papers` = ILCA_ILRI_GRASS_PAPERS ,
 `Feed and forage papers` = ILCA_ILRI_FORAGE_PAPERS,
 `Climate papers` = ILCA_ILRI_CLIMATE_PAPERS,
 `Animal production, health and genetics papers` = ILCA_ILRI_ANIMAL_PAPERS,
 `Gender papers` = ILCA_ILRI_GENDER_PAPERS,
 `Total ILRI citations` = ILRI_ALL_TC,
 `Economics and policy citations` = ECON_POLICY_ILRI_TC,
`Livestock systems citations` = ILCA_ILRI_FSR_TC,
`Rangelands citations` = ILCA_ILRI_GRASS_TC ,
`Feed and forage citations` = ILCA_ILRI_FORAGE_TC,
`Climate citations` = ILCA_ILRI_CLIMATE_TC,
`Animal production, health and genetics citations` = ILCA_ILRI_ANIMAL_TC,
`Gender citations` = ILCA_ILRI_GENDER_TC)

Biblio_ILRI  <- Biblio_ILRI %>%
     mutate(Era = case_when(
          Year <=  1994 ~ "Before merger into ILRI",
          TRUE ~ "After merger into ILRI"))

Biblio_ILRI <- Biblio_ILRI %>%
     mutate(Period = case_when(
          Year >= 1975 & Year <= 1980 ~ "1975-80",
          Year >= 1981 & Year <= 1985 ~ "1981-85",
          Year >= 1986 & Year <= 1990 ~ "1986-90",
          Year >= 1991 & Year <= 1994 ~ "1991-94",
          Year >= 1995 & Year <= 2000 ~ "1995-00",
          Year >= 2001 & Year <= 2005 ~ "2001-05",
          Year >= 2006 & Year <= 2010 ~ "2006-10",
          TRUE ~ "2011-18"))

Biblio_ILRI <- Biblio_ILRI %>%
     mutate(Era = fct_relevel(Era,"Before merger into ILRI","After merger into ILRI"))


Biblio_ILRI$Period <- as.factor(Biblio_ILRI$Period)

Biblio_ILRI <- select(Biblio_ILRI,Era,Year,Period,`Total ILRI papers`:`Gender citations`)
Biblio_ILRI_p <- select(Biblio_ILRI,Era,Year,Period,contains("papers"))
Biblio_ILRI_p$Bibliometric <- factor("Papers")
Biblio_ILRI_p <- select(Biblio_ILRI_p,Era,Year,Period,Bibliometric,
                        `Economics and policy` = `Economics and policy papers`,
                        `Livestock systems` = `Livestock systems papers`,
                        `Feed and forage` = `Feed and forage papers`,
                        `Rangelands` = `Rangelands papers`,
                        `Animal production, health and genetics` = `Animal production, health and genetics papers`,
                        `Total ILRI` = `Total ILRI papers`)

Biblio_ILRI_p <- pivot_longer(Biblio_ILRI_p,`Economics and policy`:`Total ILRI`,names_to = "Domain",values_to = "Values")

Biblio_ILRI_c <- select(Biblio_ILRI,Era,Year,Period,contains("citations"))
Biblio_ILRI_c$Bibliometric <- factor("Citations")
Biblio_ILRI_c <- select(Biblio_ILRI_c,Era,Year,Period,Bibliometric,
                        `Economics and policy` = `Economics and policy citations`,
                        `Livestock systems` = `Livestock systems citations`,
                        `Feed and forage` = `Feed and forage citations`,
                        `Rangelands` = `Rangelands citations`,
                        `Animal production, health and genetics` = `Animal production, health and genetics citations`,
                        `Total ILRI` = `Total ILRI citations`)

Biblio_ILRI_c <- pivot_longer(Biblio_ILRI_c,`Economics and policy`:`Total ILRI`,names_to = "Domain",values_to = "Values")

Biblio_ILRI <- rbind(Biblio_ILRI_p,Biblio_ILRI_c)

Biblio_ILRI <- Biblio_ILRI %>%
     mutate(Domain = fct_relevel(Domain,
 "Rangelands","Livestock systems","Economics and policy",
"Animal production, health and genetics","Feed and forage","Total ILRI"))

Biblio_ILRI_group <- group_by(Biblio_ILRI,Era,Period,Domain,Bibliometric)
Biblio_ILRI_group$Domain <- factor(Biblio_ILRI_group$Domain)

pal_fig_10 <- c(col_range,col_live_sys,col_econ_policy,col_ahg,col_feed_forage,col_ILRI)
names(pal_fig_10) <- levels(Biblio_ILRI_group$Domain)
(sample_ILRI.papers.total <- filter(Biblio_ILRI_group,Domain == "Total ILRI" & Bibliometric == "Papers") %>%
          summarise(nILRIpapers = sum(Values)))
(ILRI.papers.total <- sum(sample_ILRI.papers.total$nILRIpapers))

(INTRO_ILRI_Biblio <- ggplot(data = Biblio_ILRI_group) +
     geom_col(aes(x = Period, y = Values, fill = Domain),
              size = 1,position = position_dodge()) +
     facet_wrap(~ Bibliometric+Era,ncol = 2,nrow = 2,scales = "free",) +
     labs(x = "Publication period", y = "Counts by publication period",title = tles.Fig.I.10) +
     scale_fill_manual(name = "Domain",values = pal_fig_10) +
     scale_y_continuous(labels = scales::comma) +
     theme_tufte() +
          theme(text = element_text(size = 11,family = "Times"),legend.position = "bottom")) +
     labs(caption = paste(c("Constructed by authors from data in www.scopus.com and www.scholar.google.com. Lifetime (1975-2018) total of ",ILRI.papers.total," papers."),collapse = "")) +
     theme(plot.caption = element_text(hjust = 0))


# following code is commented out
# if you want to export the figures to tiff files then uncomment
#
#------------------------ggarrange Figures 1a thru 1d

# Figure_1a_1d <- ggarrange(Intro_AEZ_Area, Intro_AEZ_RuralPops,
#  Intro_AEZ_RuralPopShares, Intro_AEZ_Rural_PopDensity, ncol = 2,nrow = 2,align = "hv",
#  font.label = list(size = 11,family = "Times",color = "black"),hjust = 0) %>%
#      ggexport(filename = "Fig.I.1a_1d.tiff")
#
# #----------------------------ggarrange Figures 2a thru 2d -------------------
# #
# Figure_2a_2d <- ggarrange(Intro_AEZ_TLU, Intro_TLU_Density,
#  Intro_AEZ_Ruminants, Intro_AEZ_Ruminant_Shares, ncol = 2,nrow = 2,align = "hv",
#  font.label = list(size = 11,family = "Times",color = "black"),hjust = 0) %>%
#      ggexport(filename = "Fig.I.2a_2d.tiff")
#
# #
# #----------------------------ggarrange Figures 3a thru 3b------------------
# #
# Figure_3a_3b <- ggarrange(Intro_AEZ_Agric_GDP, Intro_AEZ_Live_GDP_Shares,
#  ncol = 1,nrow = 2,align = "hv",
#  font.label = list(size = 11,family = "Times",color = "black"),hjust = 0) %>%
# ggexport(filename = "Fig.I.3a_3b.tiff")
#
# #----------------------------ggarrange Figures 4a thru 4d------------------
# Figure_4a_4d <- ggarrange(Intro_AEZ_Tsetse_Area, Intro_AEZ_Tsetse_Area_Shares,
#           Intro_AEZ_Ruminants_Tsetse_Class,Intro_AEZ_Gross_Benefits,
#           ncol = 2,nrow = 2,align = "v",
#           font.label = list(size = 11,family = "Times",color = "black"),hjust = 0) %>%
# ggexport(filename = "Fig.I.4a_4d.tiff")
#
# #----------------------------ggarrange Figure 5------------------
#
# Figure_5 <- ggarrange(INTRO_ILRAD_Spend, ncol = 1, nrow = 1, align = "h",
#  font.label = list(size = 11,family = "Times",color = "black"),hjust = 0) %>%
#  ggexport(filename = paste(c(tles.Fig.I.5,".tiff"),collapse = ""))
#
# #----------------------------ggarrange Figure 6-------------
#
# Figure_6 <- ggarrange(INTRO_ILRAD_Biblio,ncol = 1, nrow = 1, align = "hv",
# font.label = list(size = 11,family = "Times",color = "black"),hjust = 0) %>%
# ggexport(filename = paste(c(tles.Fig.I.6,".tiff"),collapse = ""))
#
#
# #--------------------------ggarrange Figure 7-----------------------------
# #
# Figure_7 <- ggarrange(INTRO_ILCA_Spend,ncol = 1, nrow = 1, align = "hv",
# font.label = list(size = 11,family = "Times",color = "black"),hjust = 0) %>%
# ggexport(filename = paste(c(tles.Fig.I.7,".tiff"),collapse = ""))
#
# #ggarrange Figure 8 ----------------------------
# Figure_8 <- ggarrange(INTRO_ILCA_Biblio,ncol = 1, nrow = 1, align = "hv",
# font.label = list(size = 11,family = "Times",color = "black"),hjust = 0) %>%
# ggexport(filename = paste(c(tles.Fig.I.8,".tiff"),collapse = ""))
#
# #-------------------------ggarrange Figure 9a and 9b
# #
# Figure_9a_9b <- ggarrange(INTRO_ILRI_Spend, INTRO_ILRI_Domain,
#  ncol = 1, nrow = 2, align = "hv",
#  font.label = list(size = 11,family = "Times",color = "black"),hjust = 0) %>%
#      ggexport(filename = "Fig.I.9a_9b.tiff")
#
# #------------------------ggarrange Figure 10----------------------
# #
# Figure_10 <- ggarrange(INTRO_ILRI_Biblio,
#  ncol = 1, nrow = 1, align = "hv",
#  font.label = list(size = 11,family = "Times",color = "black"),hjust = 0) %>%
#      ggexport(filename = paste(c(tles.Fig.I.10,".tiff"),collapse = ""))
#

#-------------------------
#put the the figure titles in a list
#
Fig.titles.Intro <- as.character(mget(ls(pattern = "tles.Fig.I.")))

# following code is commented out
# if you want to print the figure titles then uncomment
#
# capture.output(print(as.character(Fig.titles.Intro),
#                      print.gap = 1,quote = FALSE),file = "Introduction, Figure titles")
#

#end of program

