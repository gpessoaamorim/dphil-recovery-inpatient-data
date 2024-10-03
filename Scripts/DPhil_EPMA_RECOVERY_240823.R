# RECOVERY EPMA DATA ANALYSIS
# GUILHERME AMORIM
# August 2022


# General helper code ------  

## save workspace image -----

# save.image("Workspace/recovery_epma_workspace_image.RData")

# load("Workspace/recovery_epma_workspace_image.RData")

# preventing scientific notation in plots

options(scipen=10000) 



# Libraries -----------

## data loading
library(readr) # read csv files a
library(haven) # read dta/sas files
library(readxl) # read excel files

## data management
library(tidyverse) # general data management and visualization tools
library(magrittr) # pipe %>% operator
library(lubridate) # managing dates
library(DataEditR) # format data tables
library(Rdiagnosislist) # load snomed terminology


## statistics
library(irr) # intraclass coefficient calculations
library(fmsb) # Cohen's kappa
library(grid) # forest plots
library(forestploter) # forest plots: https://cran.r-project.org/web/packages/forestploter/vignettes/forestploter-intro.html
library(epiR) # compute prevalence and bias adjusted kappa (PABAK)
library(crosstable) # crosstables

## plotting
library(viridis) # color scales
library(scales) # adjust scales in plots
library(patchwork) # combining ggplots
library(ggh4x) # extension functions for ggplot
library(tidytext) # reordering within facets
library(gridExtra) # arranging plots together
library(RColorBrewer) # generate color palettes
library(ggrepel) # repel labels in plots


## tables
library(gtsummary) # generate summary tables
library(flextable) # transform tables into word format
library(officer) # export tables to word
source("Tools/customtab.R") # some tools to customize flextables
customtab_defaults() # load the above
library(gtable) # arranging gtables
library(gt) # producing gtables

## system settings ------

### default plotting settings ------


theme_update(text=element_text(family="Mulish"))
update_geom_defaults("text", list(family="Mulish",
                                  size=3))

## oxpop theme

oxpop_blue_panel<- (
  # dark_mode(
  #theme_fivethirtyeight(base_size = 20))+
  theme(plot.background = element_rect(fill = "transparent", color=NA), # the color argument removes white margin
        panel.background = element_rect(fill = "transparent"), # for ppt BG color use #0d1d41
        
        panel.grid.major = element_line(color = "grey90", size=0.1, linetype = "dashed"),
        panel.grid.minor = element_line(color = "grey90", size=0.1, linetype = "dashed"),
        
        strip.background = element_blank(),
        legend.background = element_blank(),
        
        axis.title.y = element_text(family="Mulish",
                                    color = "white"),
        axis.title.x = element_text(family="Mulish",
                                    color = "white"),
        axis.text.y = element_text(family="Mulish",
                                   color = "white"),
        axis.text.x = element_text(family="Mulish",
                                   color = "white"),
        plot.subtitle = element_text(hjust=0),
        plot.caption = element_text(hjust=0),
        
        strip.text = element_text(color="white"),
        
        axis.ticks = element_line(color="white"),
        
        text = element_text(family="Mulish",color = "White", size=25),
        panel.border = element_blank()
  )
)


# Load data -----


## prescription table ------
# extract 79
data_prescription <- read_csv("K:/QNAP/RECOVERY-deidentified/Datasets/INT38_EPMA/0079/nic365354_epma_prescription_202209.txt")

colnames(data_prescription)<-toupper(colnames(data_prescription))

data_prescription%<>%
  rename(Study_ID=STUDY_ID)%>%
  mutate(Study_ID=as.character(Study_ID))

  
### extract 76 ------

data_prescription76 <- read_csv("K:/QNAP/RECOVERY-deidentified/Datasets/INT38_EPMA/0076/nic365354_epma_prescription_202206/nic365354_epma_prescription_202206.txt")

colnames(data_prescription76)<-toupper(colnames(data_prescription76))

data_prescription76%<>%
  rename(Study_ID=STUDY_ID)%>%
  mutate(Study_ID=as.character(Study_ID))

## prescription dmd table ------
# extract 79
data_prescription_dmd <- read_csv("K:/QNAP/RECOVERY-deidentified/Datasets/INT38_EPMA/0079/nic365354_epma_prescription_dmd_202209.txt", 
                                                   col_types = cols(mapped_dmd_code = col_character(), 
                                                                    vtm = col_character(), vmp = col_character(), 
                                                                    amp = col_character()))

colnames(data_prescription_dmd)<-toupper(colnames(data_prescription_dmd))

data_prescription_dmd%<>%
  rename(Study_ID=STUDY_ID)%>%
  mutate(Study_ID=as.character(Study_ID),
         MAPPED_DMD_CODE=as.character(MAPPED_DMD_CODE))


## prescription dosage table ------

# extract 79
data_prescription_dosage <- read_csv("K:/QNAP/RECOVERY-deidentified/Datasets/INT38_EPMA/0079/nic365354_epma_prescription_dosage_202209.txt")

colnames(data_prescription_dosage)<-toupper(colnames(data_prescription_dosage))

data_prescription_dosage%<>%
  rename(Study_ID=STUDY_ID)%>%
  mutate(Study_ID=as.character(Study_ID))




## administration table ----------
# extract 79
data_administration<-read_csv("K:/QNAP/RECOVERY-deidentified/Datasets/INT38_EPMA/0079/nic365354_epma_administration_202209.txt")

colnames(data_administration)<-toupper(colnames(data_administration))


data_administration%<>%
  rename(Study_ID=STUDY_ID)%>%
  mutate(Study_ID=as.character(Study_ID))


### extract 76 ------

data_administration76 <- read_csv("K:/QNAP/RECOVERY-deidentified/Datasets/INT38_EPMA/0076/nic365354_epma_administration_202206/nic365354_epma_administration_202206.txt")%>%
  mutate(Study_ID=as.character(Study_ID))


## administration dmd table ------------
# extract 79
data_administration_dmd <- read_csv("K:/QNAP/RECOVERY-deidentified/Datasets/INT38_EPMA/0079/nic365354_epma_administration_dmd_202209.txt", 
                                    col_types = cols(mapped_dmd_code = col_character(), 
                                                     vtm = col_character(), 
                                                     vmp = col_character(), 
                                                     amp = col_character(), 
                                                     vmpp = col_character(), 
                                                     ampp = col_character()))

colnames(data_administration_dmd)<-toupper(colnames(data_administration_dmd))


data_administration_dmd%<>%
  rename(Study_ID=STUDY_ID)%>%
  mutate(Study_ID=as.character(Study_ID))

## administration dosage table ----------
# extract 79
data_administration_dosage <- read_csv("K:/QNAP/RECOVERY-deidentified/Datasets/INT38_EPMA/0079/nic365354_epma_administration_dosage_202209.txt")

colnames(data_administration_dosage)<-toupper(colnames(data_administration_dosage))

data_administration_dosage%<>%
  rename(Study_ID=STUDY_ID)%>%
  mutate(Study_ID=as.character(Study_ID))

## CRF data ------

### baseline demographics ------
baseline_crf <- read_dta(file="K:/QNAP/RECOVERY-deidentified/Datasets/ADaM datasets/recovery_2212_1/adsl.dta")%>%
  rename(Study_ID = usubjid)%>%
  select(Study_ID,
         siteid,
         age,
         resp_status=ovgr,
         oxygen=oxyfl,
         sex,
         race,
         randdt,
         days_since_admission=hosplen,
         discharge_day = dischdy,
         death_day = dthdy,
         withdrfl,
         diabetes=diabfl,
         cardiac = hdfl,
         liver = liverfl,
         kidney = kidneyfl,
         lung = lungfl,
         renal_replacement=rrtfl,
         remdesivir_baseline=remrfl,
         steroids_baseline=cortrfl,
         macrolides_baseline=macrofl,
         antiplatelets_baseline=antiplfl,
         oral_anticoagulants_baseline=oralacfl,
         LMWH_baseline = vteprgr
         )%>%
  mutate(resp_status=case_when(resp_status=="No oxygen or ventilation" ~ "No oxygen or ventilation",
                               resp_status %in% c("Oxygen requirement with no ventilation",
                                                  "Non-invasive ventilation",
                                                  "Oxygen requirement, non-invasive ventilation unknown") ~ "Oxygen requirement or non-invasive ventilation",
                               resp_status=="Mechanical ventilation or ECMO" ~ "Mechanical ventilation or ECMO"))%>%
  mutate(resp_status=fct_relevel(resp_status,
                                 "No oxygen or ventilation",
                                 "Oxygen requirement or non-invasive ventilation",
                                 "Mechanical ventilation or ECMO"))%>%
  mutate(race=ifelse(is.na(race)| race=="", NA, race))%>%
  mutate(race=str_to_sentence(race))%>%
  mutate(race=factor(race, levels = c("White", "Asian", "Black", "Other", "Mixed", "Unknown")))


#### Remove withdrawals ------

baseline_crf%>%
  filter(withdrfl=="Y")%>%
  distinct(Study_ID)%>%
  .[[1]]->withdrawn_participants



#### Generate randomisation dates ------

rand_dates<-baseline_crf%>%
  select(Study_ID,
         rand_date=randdt,
         withdrfl)%>%
  mutate(Study_ID=as.character(Study_ID))%>%
  filter(withdrfl!="Y")


### CRF drugs (ADaM) -------
# this is only follow-up drugs

drugs_crf_adam<-read_dta(file="K:/QNAP/RECOVERY-deidentified/Datasets/ADaM datasets/recovery_2212_1/addose.dta")%>%
  rename(Study_ID = usubjid)%>%
  mutate(Study_ID=as.character(Study_ID),
         aval=as.character(aval))%>%
  select(Study_ID, param, paramcd, aval)

# aval is the number of days dose received: 1-10 if that was entered on FU form, 0 if FU form says they didn't receive it, or -1 if FU form says they received it but number of days not known. So -1 means that the box for ‘synthetic monoclonal antibodies’ was ticked in section 1 of the FU form, but there is no question corresponding to ‘number of days/doses’.



### CRF drugs (SDTM) -------
drugs_crf_sdtm<-read_dta(file="K:/QNAP/RECOVERY-deidentified/Datasets/ADaM datasets/recovery_2212_1/cm.dta")%>%
  rename(Study_ID = usubjid)%>%
  mutate(Study_ID=as.character(Study_ID))%>%
  left_join(
    read_csv("Tools/data_entry_started_date_v2.csv")%>%
  filter(visitnum==3)%>%
    select(usubjid, visitnum, data_entry_started)%>%
    mutate(usubjid=as.character(usubjid)),
  by=c("Study_ID" = "usubjid",
       "visitnum"))%>%
  mutate(data_entry_started = as.Date(data_entry_started, format="%d/%m/%Y"))%>%
  mutate(cmsttpt= if_else(is.na(cmsttpt) & visitnum==3, data_entry_started, cmsttpt))

drugs_crf_sdtm%>%filter(visitnum==3)%>%filter(is.na(cmsttpt))%>%distinct(Study_ID)%>%write_csv("Intermediate outputs/participants_missing_form_completion_date.csv")


### FU ventilation (ADaM) ------

fu_ventilation_adam<-read_dta(file="K:/QNAP/RECOVERY-deidentified/Datasets/ADaM datasets/recovery_2212_1/advent.dta")%>%
  rename(Study_ID = usubjid)%>%
  mutate(Study_ID=as.character(Study_ID))%>%
  filter(param=="Non-invasive ventilation (Y or N)")%>%
  select(Study_ID, param, avalc)


### FU discharge (ADaM) --------
discharge_crf<-read_dta(file="K:/QNAP/RECOVERY-deidentified/Datasets/ADaM datasets/recovery_2212_1/addistte.dta")%>%
  rename(Study_ID = usubjid)%>%
  mutate(Study_ID=as.character(Study_ID))


### FU death (ADaM) -------
death_crf<-read_dta(file="K:/QNAP/RECOVERY-deidentified/Datasets/ADaM datasets/recovery_2212_1/addthtte.dta")%>%
  rename(Study_ID = usubjid)%>%
  mutate(Study_ID=as.character(Study_ID))




# discharge_censoring_crf<-read_dta(file="K:/QNAP/RECOVERY-deidentified/Datasets/ADaM datasets/recovery_2206_1/version_2022_07_01/adcndis.dta")%>%
#   rename(Study_ID = usubjid)%>%
#   mutate(Study_ID=as.character(Study_ID))

### clinical events (SDTM) -----
# includes FU form and linkage data

clinical_events_sdtm<-read_dta(file="K:/QNAP/RECOVERY-deidentified/Datasets/ADaM datasets/recovery_2212_1/ce.dta")
  
# View(clinical_events_sdtm)


### infection events (FU form) -----

infections_crf<-read_dta(file="K:/QNAP/RECOVERY-deidentified/Datasets/ADaM datasets/recovery_2212_1/adnoncv.dta")%>%
  rename(Study_ID = usubjid)%>%
  mutate(Study_ID=as.character(Study_ID))

### thrombotic events (FU form) -------


thromb_crf<-read_dta(file="K:/QNAP/RECOVERY-deidentified/Datasets/ADaM datasets/recovery_2212_1/adthromb.dta")%>%
  rename(Study_ID = usubjid)%>%
  mutate(Study_ID=as.character(Study_ID))







## site data ----

# recovery sites
Sites <- read_csv("K:/QNAP/RECOVERY-deidentified/Datasets/RECOVERY sites/RECOVERY Sites 2022-03-29.csv")


# trust code, name, and location
  # from https://odsdatapoint.digital.nhs.uk/predefined

trusts <- read_csv("Tools/NHS Trust code and location/etr.csv")


## postcode data -----

#### import maps of all UK countries 
### from https://geoportal.statistics.gov.uk/datasets/ons::countries-december-2021-uk-bfc/explore?showTable=true

postcodes<-read_csv("K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/DPhil_EPMA_RECOVERY/DPhil_EPMA_RECOVERY/Tools/Maps/UK maps/Postcode longitude and latitute/National_Statistics_Postcode_Lookup_UK_Coordinates.csv")%>%
  select(`Postcode 3`,
         Easting,
         Northing)

filename<-"K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/DPhil_EPMA_RECOVERY/DPhil_EPMA_RECOVERY/Tools/Maps/UK countries/Countries_(December_2021)_UK_BGC/CTRY_DEC_2021_UK_BGC.shp"


## HES data -----


# extract 79 (September 2022)

HES_data <- read_csv("K:/QNAP/RECOVERY-deidentified/Datasets/INT02_HESAPC/HES_2022_09_19/DP_INT02_HES_Deidentified_2022-10-25_09-42-26.csv")

colnames(HES_data)

HES_data%<>%
  select(study_number, 
         epikey,
         trust=procode3,
         admidate,
         admimeth,
         admisorc,
         epistart,
         epiend,
         epidur,
         disdate,
         disdest,
         dismeth,
         spelbgin, # 0: not the first episode of spell; 1: first episode of spell that started in previous year: 2: first episode of spell that started in the current year
         speldur,
         spelend, # Y: last episode of spell; N: not last episode
         starts_with("diag"),
         starts_with("opertn"),
         starts_with("opdate"))%>%
  mutate(study_number=as.character(study_number))



## SNOMED codelists (BNF chapters) ------




file.list <- grep(list.files(path="K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/DPhil_EPMA_RECOVERY/DPhil_EPMA_RECOVERY/Tools/Codelists/BNF chapters/", full.names=T),
                  pattern = "dmd.csv",
                  invert = T,
                  value=T)

df = lapply(file.list, function(i){
  x=read_csv(i, col_types = "cccc")
  x$codelist=i
  x
})

bnf_chapters<-bind_rows(df)

bnf_chapters$codelist<-str_sub(bnf_chapters$codelist,-15, -14)

rm(df, file.list)

bnf_chapters%<>%
  mutate(chapter=as.numeric(codelist))

BNF_Snomed_Mapping_data_20230522 <- read_excel("Tools/BNF-SNOMED mapping/BNF Snomed Mapping data 20230522.xlsx", 
                                               col_types = c("skip", "skip", "text", 
                                                             "skip", "text", "text", "skip", "skip", 
                                                             "skip", "skip", "skip"))
bnf_chapters%<>%
  left_join(BNF_Snomed_Mapping_data_20230522, by=c("code"="BNF Code"))

## create BNF chapter labels

bnf_labels <- c("Gastro-Intestinal System",
                "Cardiovascular System",
                "Respiratory System",
                "Central Nervous System",
                "Infections",
                "Endocrine System",
                "Obstetrics, Gynaecology and Urinary-Tract Disorders",
                "Malignant Disease and Immunosuppression",
                "Nutrition and Blood",
                "Musculoskeletal and Joint Diseases",
                "Eye",
                "Ear, Nose and Oropharynx",
                "Skin",
                "Immunological Products and Vaccines",
                "Anaesthesia",
                "Preparations used in Diagnosis",
                "Other Drugs and Preparations",
                "Dressings",
                "Appliances",
                "Incontinence Appliances",
                "Stoma Appliances")

bnf_chapter_labels<-data.frame(distinct(bnf_chapters,chapter), bnf_labels)


rm(bnf_labels)


bnf_chapters%<>%
  select(code, dmd_id = `SNOMED Code`, dmd_name =`DM+D: Product Description`, chapter)

rm(BNF_Snomed_Mapping_data_20230522)

bnf_chapters%<>%
  filter(!is.na(dmd_id))

bnf_chapters%<>%
  filter(dmd_id %in% data_administration_dmd$MAPPED_DMD_CODE | dmd_id %in% data_prescription_dmd)

## load SNOMED terminology terms -----

# October 2022 major release

SNOMED <- loadSNOMED(
  c("K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/SNOMED terminology/uk_sct2cl_35.0.0_20220928000001Z/SnomedCT_InternationalRF2_PRODUCTION_20220731T120000Z/Snapshot",
    
    "K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/SNOMED terminology/uk_sct2cl_35.0.0_20220928000001Z/SnomedCT_UKClinicalRF2_PRODUCTION_20220928T000001Z/Snapshot",
    
    "K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/SNOMED terminology/uk_sct2cl_35.0.0_20220928000001Z/SnomedCT_UKEditionRF2_PRODUCTION_20220928T000001Z/Snapshot",
    
    "K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/SNOMED terminology/uk_sct2dr_35.0.0_20220928000001Z/SnomedCT_UKDrugRF2_PRODUCTION_20220928T000001Z/Snapshot"),
  active_only = F
)



snomed_descriptions<-SNOMED[["DESCRIPTION"]]%>%
  as_tibble()%>%
  select(id, conceptId, term)%>%
  mutate(across(everything(),~as.character(.)))

rm(SNOMED)


## load drug codelists ------

medication_codelists<-read_excel("Tools/Codelists/Medication names codelists.xlsx", 
           col_types = c("text", "skip", "text", 
                         "text", "text", "text", "text", "text", 
                         "text", "text", "text", "text"))












# Initial exploration -------

baseline_crf%>%
  left_join(Sites%>%
              mutate(SiteID=as.character(SiteID)),
            by=c("siteid"="SiteID"))%>%
  filter(Nation=="England")%>%
  distinct(Study_ID)%>%
  nrow()
  # 41762 in England (including withdrawals)



## number of people and records in each table -------

data_prescription%>%
  distinct(Study_ID)%>%
  nrow()
  # 5125 people

data_prescription%>%
  nrow()
# 408574 records


data_prescription_dmd%>%
  distinct(Study_ID)%>%
  nrow()
# 4716 people


data_prescription_dmd%>%
  nrow()
# 337090 records


data_prescription_dosage%>%
  distinct(Study_ID)%>%
  nrow()
# 5125 people


data_prescription_dosage%>%
  nrow()
# 409998 people


data_administration%>%
  distinct(Study_ID)%>%
  nrow()
# 4943 people

data_administration%>%
  nrow()
# 2372641 records


data_administration_dmd%>%
  distinct(Study_ID)%>%
  nrow()
# 4496 people


data_administration_dmd%>%
  nrow()
# 2056430 records



data_administration_dosage%>%
  distinct(Study_ID)%>%
  nrow()
# 4943 people

data_administration_dosage%>%
  nrow()
# 2372959 people

epma_participants_list_prescription_raw<-data_prescription%>%
  distinct(Study_ID)%>%
  .[[1]]
length(epma_participants_list_prescription_raw) # 5125

epma_participants_list_administration_raw<-data_administration%>%
  distinct(Study_ID)%>%
  .[[1]]
length(epma_participants_list_administration_raw) # 4943






## fields --------


### prescription ------

#### Overlook

data_prescription%>%
  colnames()

data_prescription%>%
  slice_head(n=100)%>%
  View()


#### REPORTED DATE AND TIME

data_prescription%>%
  select(REPORTED_DATE_TIME)%>%
  mutate(REPORTED_DATE_TIME = as.Date(REPORTED_DATE_TIME, format=c("%Y-%m-%d")))%>%
  group_by(week = floor_date(REPORTED_DATE_TIME, "week"))%>%
  # mutate(month=paste0(str_sub(REPORTED_DATE_TIME, 1, 8), "01"))%>%
  # mutate(month=as.Date(month, format=c("%Y-%m-%d")))%>%
  count(week)%>%
  
  ggplot(aes(week, n, group=1))+
  geom_point()+
  geom_line()+
  labs(x="Reported date (at weekly level)",
     y="Count",
     title="Timeseries of number of individual records for each reported date (prescription table)",
     subtitle="Aggregated at weekly level")+
  scale_x_date(date_labels = "%Y %b %d",
               date_breaks = "1 month")+
  theme(axis.text.x=element_text(angle=30,
                                 hjust=1),
        panel.grid.minor = element_blank(),
        text=element_text(size=20))+
  scale_y_continuous(limits=c(0, NA))+
  geom_vline(aes(xintercept=as.Date("2022-09-30")), linetype="dashed")+
  annotate(geom="text",
           label="Extract reception date: 30/09/22",
           y=20000,
           x=as.Date("2022-09-30"))
  

ggsave("Outputs/Figures/Exploration/prescription_timeseries_reported_date.png",
       last_plot(),
       width=40,
       height=20,
       units = "cm",
       dpi = "retina",
       scale= 2)
  
  




#### ODS_CODE

data_prescription%>%
  distinct(ODS_CODE)%>%
  nrow()
  # 24 trusts

##### map


#### import maps of all UK countries 
### from https://geoportal.statistics.gov.uk/datasets/ons::countries-december-2021-uk-bfc/explore?showTable=true

postcodes<-read_csv("K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/RECOVERY_gui_analysis/Tools/UK maps/Postcode longitude and latitute/National_Statistics_Postcode_Lookup_UK_Coordinates.csv")

postcodes%<>%
  select(`Postcode 3`,
         Easting,
         Northing)



participants_per_trust<-data_prescription%>%
  select(Study_ID, ODS_CODE)%>%
  mutate(ODS_CODE = str_sub(ODS_CODE, 1,3))%>%
  left_join(trusts%>%
              select(`Organisation Code`,
                     Name,
                     Postcode
              ),
            by=c("ODS_CODE" = "Organisation Code"))%>%
  group_by(`ODS_CODE`, Name, Postcode)%>%
  summarise(Participants = n_distinct(Study_ID))%>%
  mutate(Postcode=str_to_upper(Postcode))%>%
  left_join(postcodes, by=c("Postcode"="Postcode 3"))%>%
  select(ODS_CODE, Name, Participants, Easting, Northing)%>%
  ungroup()
  

filename<-"K:/QNAP/RECOVERY-deidentified/Generalizability and representativeness project/RECOVERY_generalizability_representativeness/Tools/Maps/UK countries/Countries_(December_2021)_UK_BGC/CTRY_DEC_2021_UK_BGC.shp"

# load map to shape file
# trim<-glue::trim

library(rgdal)
countries_map<-raster::shapefile(filename)

countries_map_df<-broom::tidy(countries_map)

# plot the map (using eastings/northings instead of longitude and latitude as usual)

map_plot<-ggplot()+
  geom_polygon(data=countries_map_df%>%
                 filter(id==0),
               aes(x=long, y=lat, group=group), 
               colour="black", 
               fill="grey",
               size=0.05,
               alpha=0.5,
  )+
  geom_point(data=participants_per_trust,
             mapping=aes(x=Easting,
                         y=Northing,
                         size=Participants,
                         fill=Participants
                         ),
             # fill="orange",
             alpha=0.9,
             color="black",
             pch=21)+
  theme_void(base_size=20)+
  theme(text = element_text(family="Mulish",
                            size=20
  ),
  legend.position="right",
  legend.key.height = unit(2, 'cm'),
  plot.caption = element_text(size=20,hjust = 0), 
  plot.title=element_text(hjust=0, size=15),
  plot.title.position = "plot", 
  plot.background = element_rect(fill="white", color=NA)
  )+
  scale_fill_viridis(# option="magma",
                     # direction = -1,
                     breaks=seq(0,900,100),
                     limits=c(0, 900))+
  guides(size="none")+
  labs(title="Geographical distribution of RECOVERY participants included in the EPMA extract")+
  coord_fixed()

map_plot

ggsave("Outputs/Figures/Analysis/EPMA_linkage_map.png",
       last_plot(),
       # width=4,
       # height=6.18,
       dpi = "retina",
       scale= 2)


#### PATIENT_LSOA
# patient location

data_prescription%>%
  distinct(PATIENT_LSOA)%>%
  nrow()
  # 3088 distinct ones

#### PATIENT_LOCATION_CCG_ODS_CODE
# CCG of the patient's address

data_prescription%>%
  distinct(PATIENT_LOCATION_CCG_ODS_CODE)%>%
  nrow()
  # 142 distinct ones


#### PATIENT_GP_PRACTICE_ODS_CODE
# GP practice code

data_prescription%>%
  distinct(PATIENT_GP_PRACTICE_ODS_CODE)%>%
  nrow()
# 1328 distinct ones

#### PATIENT_GP_PRACTICE_ODS_CODE
# GP practice code

data_prescription%>%
  distinct(PATIENT_GP_PRACTICE_ODS_CODE)%>%
  nrow()
# 1328 distinct ones

#### PATIENT_GP_PRACTICE_CCG_ODS_CODE
# CCG of the patient's GP practice

data_prescription%>%
  distinct(PATIENT_GP_PRACTICE_CCG_ODS_CODE)%>%
  nrow()
# 144 distinct ones

#### PATIENT_GP_PRACTICE_LA_DISTRICT
# Local authority of the patient's GP practice

data_prescription%>%
  distinct(PATIENT_GP_PRACTICE_LA_DISTRICT)%>%
  nrow()
# 194 distinct ones

#### PATIENT_AGE

data_prescription%>%
  distinct(Study_ID, PATIENT_AGE)%>%
  ggplot(aes(PATIENT_AGE))+
  geom_histogram(color="black")


#### PATIENT_GENDER

data_prescription%>%
  distinct(Study_ID, PATIENT_GENDER)%>%
  ggplot(aes(PATIENT_GENDER))+
  geom_bar()+
  geom_text(aes(label=after_stat(count)), vjust=-1, stat = "count")



#### TYPE

data_prescription%>%
  mutate(TYPE = case_when(TYPE=="D" ~ "Discharge",
                          TYPE=="I" ~ "Inpatient",
                          TYPE=="O" ~ "Outpatient",
                          TYPE=="STL" ~ "Short-term leave"))%>%
  ggplot(aes(TYPE, fill=TYPE))+
  geom_bar()+
  geom_text(aes(label=paste0(after_stat(count), 
                             " (",
                             round(after_stat(count)/nrow(data_prescription)*100, 1),
                             "%)")), 
            vjust=-1, stat = "count")+
  labs(x="Type",
       fill=NULL,
       y="Number of individual prescriptions",
       title="Distribution of prescription type (prescription table)")+
  theme(legend.position = "none",
        text=element_text(size=20))



ggsave("Outputs/Figures/Exploration/prescription_type_distribution.png", 
       last_plot(),
       width=40, 
       height=20,
       dpi = "retina",
       units="cm")



#### STATUS

data_prescription%>%
  ggplot(aes(STATUS, fill=STATUS))+
  geom_bar()+
  geom_text(aes(label=paste0(after_stat(count), 
                             " (",
                             round(after_stat(count)/nrow(data_prescription)*100, 1),
                             "%)")), 
            vjust=-1, stat = "count")+
  labs(x="Status",
       fill=NULL,
       y="Number of individual prescriptions",
       title="Distribution of prescription status (prescription table)")+
  theme(legend.position = "none",
        text=element_text(size=20))




ggsave("Outputs/Figures/Exploration/prescription_status_distribution.png", 
       last_plot(),
       width=40, 
       height=20,
       dpi = "retina",
       units="cm")

#### FORM

data_prescription%>%
  count(FORM)%>%
  arrange(desc(n))%>%
  View()
  
data_prescription%>%
  count(FORM)%>%
  arrange(desc(n))%>%
  mutate(prop=round(n/sum(n)*100,1))%>%
  mutate(FORM=fct_reorder(FORM, n))%>%
  slice_head(n=30)%>%
  ggplot(aes(x=n, y= FORM, fill=FORM))+
  geom_col()+
  geom_text(aes(label=paste0(n, 
                             " (",
                             prop,
                             "%)")), 
            hjust=-0.1)+
  labs(x="Number of individual records",
       fill=NULL,
       y="Form",
       title="Distribution of prescription form (prescription table)",
       subtitle="Top 30 most frequent formulations")+
  scale_x_continuous(limits=c(0,150000))+
  theme(legend.position = "none",
        text=element_text(size=20))




ggsave("Outputs/Figures/Exploration/prescription_form_distribution.png", 
       last_plot(),
       width=40, 
       height=20,
       dpi = "retina",
       units="cm")




#### INITIAL_AUTHORISED_DATE_TIME

data_prescription%>%
  select(Study_ID, INITIAL_AUTHORISED_DATE_TIME)%>%
  mutate(INITIAL_AUTHORISED_DATE_TIME = as.Date(INITIAL_AUTHORISED_DATE_TIME, format=c("%Y-%m-%d")))%>%
  mutate(month=paste0(str_sub(INITIAL_AUTHORISED_DATE_TIME, 1, 8), "01"))%>%
  mutate(month=as.Date(month, format=c("%Y-%m-%d")))%>%
  group_by(month)%>%
  summarise(Participants=n_distinct(Study_ID),
            Records=n(),
            `Records per participant` = Records/Participants)%>%
  pivot_longer(-month, names_to="key", values_to="value")%>%
  
  ggplot(aes(month, value, group=1))+
  geom_point()+
  geom_line()+
  facet_wrap(~key,
             nrow=3,
             scales="free_y")+
  labs(x="Initial authorised date (at monthly level)",
       y="Count",
       title="Timeseries of number of individual records for each initial prescribed date and time (prescription table)",
       subtitle="Aggregated at monthly level")+
  scale_x_date(date_labels = "%Y %b",
               date_breaks = "1 month")+
  theme(axis.text.x=element_text(angle=30,
                                 hjust=1))+
  scale_y_continuous(limits=c(0, NA))

ggsave("Outputs/Figures/Exploration/prescription_initial_authorised_date_timeseries.png", 
       last_plot(),
       width=40, 
       height=20,
       dpi = "retina",
       units="cm")


#### LAST_AUTHORISED_DATE_TIME

data_prescription%>%
  select(Study_ID,
         PRESCRIPTION_ID_HASHED,
         INITIAL_AUTHORISED_DATE_TIME,
         LAST_AUTHORISED_DATE_TIME)%>%
  group_by(PRESCRIPTION_ID_HASHED)%>%
  summarise(group = if_else(INITIAL_AUTHORISED_DATE_TIME==LAST_AUTHORISED_DATE_TIME,
                             "Equal",
                             "Distinct"))%>%
  ungroup()%>%
  count(group)


data_prescription%>%
  select(Study_ID,
         PRESCRIPTION_ID_HASHED,
         INITIAL_AUTHORISED_DATE_TIME,
         LAST_AUTHORISED_DATE_TIME)%>%


#### ORDER_ID

data_prescription%>%
  count(ORDER_ID)%>%
  arrange(desc(n))


data_prescription%>%
  select(ORDER_ID, Study_ID)%>%
  group_by(ORDER_ID)%>%
  summarise(n=n_distinct(Study_ID))%>%
  View()


#### GROUP_TYPE

data_prescription%>%
  select(GROUP_TYPE)%>%
  # count(GROUP_TYPE)%>%
  mutate(GROUP_TYPE = as.factor(GROUP_TYPE))%>%
  mutate(GROUP_TYPE = case_when (GROUP_TYPE == "E" ~ "Either/or dosage",
                                 GROUP_TYPE == "V" ~ "Variable dose order"))%>%
  
  ggplot(aes(GROUP_TYPE, fill=GROUP_TYPE))+
  geom_bar()+
  geom_text(aes(label=paste0(after_stat(count), 
                             " (",
                             round(after_stat(count)/nrow(data_prescription)*100, 1),
                             "%)")), 
            vjust=-1, stat = "count")+
  labs(x="Group type",
       fill=NULL,
       y="Number of individual prescriptions",
       title="Distribution of group type (prescription table)")+
  theme(legend.position = "none",
        text=element_text(size=20))




ggsave("Outputs/Figures/Exploration/prescription_group_type_distribution.png", 
       last_plot(),
       width=40, 
       height=20,
       dpi = "retina",
       units="cm")

##### either/or

data_prescription%>%
  filter(GROUP_TYPE=="E",
         !is.na(LINK_TO))%>%
  select(Study_ID,
         PRESCRIPTION_ID_HASHED1 = PRESCRIPTION_ID_HASHED,
         MEDICATION1 = MEDICATION_NAME,
         FORM1 = FORM,
         INITIAL_AUTHORISED_DATE_TIME,
         ORDER_ID)%>%
  
  left_join(
    data_prescription%>%
      filter(GROUP_TYPE=="E",
             !is.na(LINK_FROM))%>%
      select(Study_ID,
             PRESCRIPTION_ID_HASHED2 = PRESCRIPTION_ID_HASHED,
             MEDICATION2 = MEDICATION_NAME,
             FORM2 = FORM,
             LINK_FROM),
    by=c("Study_ID", "ORDER_ID"="LINK_FROM")
  )%>%
  arrange(Study_ID,
          ORDER_ID,
          INITIAL_AUTHORISED_DATE_TIME)%>%
  count(MEDICATION1,
        MEDICATION2)%>%
  View()

data_prescription%>%
  filter(GROUP_TYPE=="E",
         !is.na(LINK_TO))%>%
  select(Study_ID,
         PRESCRIPTION_ID_HASHED1 = PRESCRIPTION_ID_HASHED,
         MEDICATION1 = MEDICATION_NAME,
         FORM1 = FORM,
         INITIAL_AUTHORISED_DATE_TIME1 = INITIAL_AUTHORISED_DATE_TIME,
         ORDER_ID)%>%
  
  left_join(
    data_prescription%>%
      filter(GROUP_TYPE=="E",
             !is.na(LINK_FROM))%>%
      select(Study_ID,
             PRESCRIPTION_ID_HASHED2 = PRESCRIPTION_ID_HASHED,
             MEDICATION2 = MEDICATION_NAME,
             INITIAL_AUTHORISED_DATE_TIME2 = INITIAL_AUTHORISED_DATE_TIME,
             FORM2 = FORM,
             LINK_FROM),
    by=c("Study_ID", "ORDER_ID"="LINK_FROM")
  )%>%
  arrange(Study_ID,
          ORDER_ID)%>%
  filter(MEDICATION1 == MEDICATION2)%>%
  # filter(INITIAL_AUTHORISED_DATE_TIME1!=INITIAL_AUTHORISED_DATE_TIME2)%>%
  View()


##### variable dose

data_prescription%>%
  filter(GROUP_TYPE=="V",
         !is.na(LINK_TO))%>%
  select(Study_ID,
         PRESCRIPTION_ID_HASHED1 = PRESCRIPTION_ID_HASHED,
         INITIAL_AUTHORISED_DATE_TIME1 = INITIAL_AUTHORISED_DATE_TIME,
         MEDICATION1 = MEDICATION_NAME,
         FORM1 = FORM,
         INITIAL_AUTHORISED_DATE_TIME1 = INITIAL_AUTHORISED_DATE_TIME,
         ORDER_ID)%>%
  
  left_join(
    data_prescription%>%
      filter(GROUP_TYPE=="V",
             !is.na(LINK_FROM))%>%
      select(Study_ID,
             PRESCRIPTION_ID_HASHED2 = PRESCRIPTION_ID_HASHED,
             MEDICATION2 = MEDICATION_NAME,
             INITIAL_AUTHORISED_DATE_TIME2 = INITIAL_AUTHORISED_DATE_TIME,
             FORM2 = FORM,
             LINK_FROM),
    by=c("Study_ID", "ORDER_ID"="LINK_FROM")
    )%>%
  arrange(Study_ID,
          ORDER_ID)%>%
  filter(MEDICATION1 != MEDICATION2)%>%
  View()



#### VERSION

data_prescription%>%
  count(VERSION)%>%
  View()

data_prescription%>%
  filter(VERSION!=1)%>%
  View()

data_prescription%>%
  group_by(PRESCRIPTION_ID_HASHED)%>%
  summarise(n=n_distinct(VERSION))%>%
  filter(n>1)%>%
  View()




data_prescription%>%
  count(VERSION)%>%
  mutate(VERSION=as.character(VERSION))%>%View()
  ggplot(aes(VERSION, n, size=n))+
  geom_point(alpha=0.1)+
  labs(size="Count",
       y="Count",
      x = "Version number",
      title="Distribution of number of individual records for each version (prescription table)")+
  theme(legend.position="bottom",
        text=element_text(size=20))


ggsave("Outputs/Figures/Exploration/prescription_version_distribution.png", 
       last_plot(),
       width=40, 
       height=20,
       dpi = "retina",
       units="cm")

#### Investigate duplicates

data_prescription%>%
  group_by(Study_ID,
           MEDICATION_NAME,
           INITIAL_AUTHORISED_DATE_TIME)%>%
  group_by(Study_ID,
           MEDICATION_NAME,
           INITIAL_AUTHORISED_DATE_TIME)%>%
  mutate(group = if_else(n()==1,
                            "Unique record",
                            "Duplicate record"))%>%
  ungroup()%>%
  count(group)%>%
  View()








### prescription dmd ------

data_prescription_dmd%>%
  colnames()

#### mapped_dmd_code, mapped_dmd_name, 
data_prescription_dmd%>%
  select(MAPPED_DMD_CODE, MAPPED_DMD_NAME)

#### mapped_dmd_invalid

data_prescription_dmd%>%
  count(MAPPED_DMD_INVALID)


#### mapped_dmd_level
data_prescription_dmd%>%
  select(MAPPED_DMD_LEVEL)%>%
  ggplot(aes(MAPPED_DMD_LEVEL, fill=MAPPED_DMD_LEVEL))+
  geom_bar(stat='count')+
  geom_text(aes(label=paste0(after_stat(count), 
                             " (",
                             round(after_stat(count)/nrow(data_prescription_dmd)*100, 1),
                             "%)")), 
            vjust=-1, stat = "count")+
  labs(x="Mapped DMD level",
       fill=NULL,
       y="Number of individual records",
       title="Distribution of mapped_dmd_level (prescription dmd table)")+
  theme(legend.position = "none",
        text=element_text(size=20))
  
ggsave("Outputs/Figures/Exploration/prescription_dmd_mapped_dmd_level_distribution.png", 
       last_plot(),
       width=40, 
       height=20,
       dpi = "retina",
       units="cm")


#### mapped_dmd_source

data_prescription_dmd%>%
  select(MAPPED_DMD_SOURCE)%>%
  ggplot(aes(MAPPED_DMD_SOURCE, fill=MAPPED_DMD_SOURCE))+
  geom_bar(stat='count')+
  geom_text(aes(label=paste0(after_stat(count), 
                             " (",
                             round(after_stat(count)/nrow(data_prescription_dmd)*100, 1),
                             "%)")), 
            vjust=-1, stat = "count")+
  labs(x="Mapped DMD source",
       fill=NULL,
       y="Number of individual records",
       title="Distribution of mapped_dmd_source (prescription dmd table)")+
  theme(legend.position = "none",
        text=element_text(size=20))

ggsave("Outputs/Figures/Exploration/prescription_dmd_mapped_dmd_source_distribution.png", 
       last_plot(),
       width=40, 
       height=20,
       dpi = "retina",
       units="cm")




#### VTM
#### VTM_name
data_prescription_dmd%>%
  filter(is.na(VTM))%>%
  distinct(VMP_NAME)%>%
  View()


#### VMP
#### VMP_NAME
data_prescription_dmd%>%
  filter(is.na(VMP))%>%
  distinct(MAPPED_DMD_NAME)%>%
  View()

#### AMP
#### AMP_NAME

data_prescription_dmd%>%
  filter(!is.na(AMP))%>%
  distinct(AMP_NAME)%>%
  View()

#### VMPP
#### VMPP_NAME
data_prescription_dmd%>%
  count(VMPP)
  # all blank

#### AMPP
#### AMPP_NAME

data_prescription_dmd%>%
  count(AMPP)
  # all blank



#### investigate dmd coding -----


data_prescription_dmd%>%
  select(MAPPED_DMD_CODE, MAPPED_DMD_NAME, MAPPED_DMD_SOURCE)%>%
  left_join(snomed_descriptions%>%
              select(id, conceptId, term)%>%
              mutate(across(everything(), ~as.character(.))), by=c("MAPPED_DMD_CODE" = "conceptId"))%>%
  group_by(MAPPED_DMD_SOURCE)%>%
  slice_sample(n=100)%>%
  View()











### prescription dose -----

#### Overall

# number of distinct records per prescription id
data_prescription_dosage%>%
  select(PRESCRIPTION_ID_HASHED)%>%
  group_by(PRESCRIPTION_ID_HASHED)%>%
  summarise(n=n())%>%
  ungroup()%>%
  count(n)%>%View()
  
  
data_prescription_dosage%>%
  select(PRESCRIPTION_ID_HASHED)%>%
  group_by(PRESCRIPTION_ID_HASHED)%>%
  summarise(n=n())%>%
  ungroup()%>%
  filter(n>1)%>%
  left_join(data_prescription%>%select(PRESCRIPTION_ID_HASHED, MEDICATION_NAME))%>%
  left_join(data_prescription_dosage%>%select(PRESCRIPTION_ID_HASHED, 
                                              DOSE_QUANTITY_VALUE,
                                              START_DATE_TIME, 
                                              END_DATE_TIME))%>%View()
  
  

#### dosage_sequence
data_prescription_dosage%>%
  count(DOSAGE_SEQUENCE)

#### dose_quantity_value

data_prescription_dosage%>%
  select(DOSE_QUANTITY_VALUE)%>%
  ggplot(aes(DOSE_QUANTITY_VALUE))+
  geom_histogram(color="black")


data_prescription_dosage%>%
  select(DOSE_QUANTITY_VALUE, PRESCRIPTION_ID_HASHED)%>%
  left_join(data_prescription%>%select(PRESCRIPTION_ID_HASHED, MEDICATION_NAME))%>%
  arrange(desc(DOSE_QUANTITY_VALUE))%>%View()


#### alternative_dose_quantity_value
data_prescription_dosage%>%
  select(ALTERNATIVE_DOSE_QUANTITY_VALUE, DOSE_QUANTITY_VALUE, PRESCRIPTION_ID_HASHED)%>%
  left_join(data_prescription%>%select(PRESCRIPTION_ID_HASHED, MEDICATION_NAME))%>%
  arrange(desc(ALTERNATIVE_DOSE_QUANTITY_VALUE))%>%View()

data_prescription_dosage%>%
  count(ALTERNATIVE_DOSE_QUANTITY_VALUE)
  


#### dosage_as_needed_boolean
data_prescription_dosage%>%
  count(DOSAGE_AS_NEEDED_BOOLEAN)%>%
  mutate(proportion=round(n/sum(n)*100, 1))




#### start_date_time

data_prescription_dosage%>%
  select(PRESCRIPTION_ID_HASHED, START_DATE_TIME)%>%
  right_join(data_prescription%>%select(PRESCRIPTION_ID_HASHED, INITIAL_AUTHORISED_DATE_TIME))%>%
  summarise(equal=n_distinct(PRESCRIPTION_ID_HASHED[START_DATE_TIME==INITIAL_AUTHORISED_DATE_TIME]),
            distinct=n_distinct(PRESCRIPTION_ID_HASHED[START_DATE_TIME!=INITIAL_AUTHORISED_DATE_TIME]))%>%
  pivot_longer(everything(),names_to = "Similarity", values_to="n")%>%
  mutate(proportion=round(n/sum(n)*100,1))




### administration -----

data_administration%>%
  colnames()


#### ods_code

data_administration%>%
  select(PRESCRIPTION_ID_HASHED, 
         ODS_CODE_administration=ODS_CODE)%>%
  left_join(data_prescription%>%
              select(PRESCRIPTION_ID_HASHED,
                     ODS_CODE_prescription=ODS_CODE))%>%
  summarise(equal=n_distinct(PRESCRIPTION_ID_HASHED[ODS_CODE_prescription==ODS_CODE_administration]),
            distinct=n_distinct(PRESCRIPTION_ID_HASHED[ODS_CODE_prescription!=ODS_CODE_administration]))%>%
  pivot_longer(everything(),names_to = "Similarity", values_to="n")%>%
  mutate(proportion=round(n/sum(n)*100,1))


#### reported_date_time

data_administration%>%
  select(PRESCRIPTION_ID_HASHED, 
         REPORTED_DATE_TIME_administration=REPORTED_DATE_TIME)%>%
  left_join(data_prescription%>%
              select(PRESCRIPTION_ID_HASHED,
                     REPORTED_DATE_TIME_prescription=REPORTED_DATE_TIME))%>%
  summarise(equal=n_distinct(PRESCRIPTION_ID_HASHED[REPORTED_DATE_TIME_prescription==REPORTED_DATE_TIME_administration]),
            distinct=n_distinct(PRESCRIPTION_ID_HASHED[REPORTED_DATE_TIME_prescription!=REPORTED_DATE_TIME_administration]))%>%
  pivot_longer(everything(),names_to = "Similarity", values_to="n")%>%
  mutate(proportion=round(n/sum(n)*100,1))


#### patient_lsoa


data_administration%>%
  select(PRESCRIPTION_ID_HASHED, 
         PATIENT_LSOA_administration=PATIENT_LSOA)%>%
  left_join(data_prescription%>%
              select(PRESCRIPTION_ID_HASHED,
                     PATIENT_LSOA_prescription=PATIENT_LSOA))%>%
  summarise(equal=n_distinct(PRESCRIPTION_ID_HASHED[PATIENT_LSOA_prescription==PATIENT_LSOA_administration]),
            distinct=n_distinct(PRESCRIPTION_ID_HASHED[PATIENT_LSOA_prescription!=PATIENT_LSOA_administration]))%>%
  pivot_longer(everything(),names_to = "Similarity", values_to="n")%>%
  mutate(proportion=round(n/sum(n)*100,1))


#### PATIENT_LOCATION_CCG_ODS_CODE


data_administration%>%
  select(PRESCRIPTION_ID_HASHED, 
         PATIENT_LOCATION_CCG_ODS_CODE_administration=PATIENT_LOCATION_CCG_ODS_CODE)%>%
  left_join(data_prescription%>%
              select(PRESCRIPTION_ID_HASHED,
                     PATIENT_LOCATION_CCG_ODS_CODE_prescription=PATIENT_LOCATION_CCG_ODS_CODE))%>%
  summarise(equal=n_distinct(PRESCRIPTION_ID_HASHED[PATIENT_LOCATION_CCG_ODS_CODE_prescription==PATIENT_LOCATION_CCG_ODS_CODE_administration]),
            distinct=n_distinct(PRESCRIPTION_ID_HASHED[PATIENT_LOCATION_CCG_ODS_CODE_prescription!=PATIENT_LOCATION_CCG_ODS_CODE_administration]))%>%
  pivot_longer(everything(),names_to = "Similarity", values_to="n")%>%
  mutate(proportion=round(n/sum(n)*100,1))

#### PATIENT_GP_PRACTICE_ODS_CODE


data_administration%>%
  select(PRESCRIPTION_ID_HASHED, 
         PATIENT_GP_PRACTICE_ODS_CODE_administration=PATIENT_GP_PRACTICE_ODS_CODE)%>%
  left_join(data_prescription%>%
              select(PRESCRIPTION_ID_HASHED,
                     PATIENT_GP_PRACTICE_ODS_CODE_prescription=PATIENT_GP_PRACTICE_ODS_CODE))%>%
  summarise(equal=n_distinct(PRESCRIPTION_ID_HASHED[PATIENT_GP_PRACTICE_ODS_CODE_prescription==PATIENT_GP_PRACTICE_ODS_CODE_administration]),
            distinct=n_distinct(PRESCRIPTION_ID_HASHED[PATIENT_GP_PRACTICE_ODS_CODE_prescription!=PATIENT_GP_PRACTICE_ODS_CODE_administration]))%>%
  pivot_longer(everything(),names_to = "Similarity", values_to="n")%>%
  mutate(proportion=round(n/sum(n)*100,1))



#### PATIENT_GP_PRACTICE_CCG_ODS_CODE

data_administration%>%
  select(PRESCRIPTION_ID_HASHED, 
         PATIENT_GP_PRACTICE_CCG_ODS_CODE_administration=PATIENT_GP_PRACTICE_CCG_ODS_CODE)%>%
  left_join(data_prescription%>%
              select(PRESCRIPTION_ID_HASHED,
                     PATIENT_GP_PRACTICE_CCG_ODS_CODE_prescription=PATIENT_GP_PRACTICE_CCG_ODS_CODE))%>%
  summarise(equal=n_distinct(PRESCRIPTION_ID_HASHED[PATIENT_GP_PRACTICE_CCG_ODS_CODE_prescription==PATIENT_GP_PRACTICE_CCG_ODS_CODE_administration]),
            distinct=n_distinct(PRESCRIPTION_ID_HASHED[PATIENT_GP_PRACTICE_CCG_ODS_CODE_prescription!=PATIENT_GP_PRACTICE_CCG_ODS_CODE_administration]))%>%
  pivot_longer(everything(),names_to = "Similarity", values_to="n")%>%
  mutate(proportion=round(n/sum(n)*100,1))

#### PATIENT_GP_PRACTICE_LA_DISTRICT

data_administration%>%
  select(PRESCRIPTION_ID_HASHED, 
         PATIENT_GP_PRACTICE_LA_DISTRICT_administration=PATIENT_GP_PRACTICE_LA_DISTRICT)%>%
  left_join(data_prescription%>%
              select(PRESCRIPTION_ID_HASHED,
                     PATIENT_GP_PRACTICE_LA_DISTRICT_prescription=PATIENT_GP_PRACTICE_LA_DISTRICT))%>%
  summarise(equal=n_distinct(PRESCRIPTION_ID_HASHED[PATIENT_GP_PRACTICE_LA_DISTRICT_prescription==PATIENT_GP_PRACTICE_LA_DISTRICT_administration]),
            distinct=n_distinct(PRESCRIPTION_ID_HASHED[PATIENT_GP_PRACTICE_LA_DISTRICT_prescription!=PATIENT_GP_PRACTICE_LA_DISTRICT_administration]))%>%
  pivot_longer(everything(),names_to = "Similarity", values_to="n")%>%
  mutate(proportion=round(n/sum(n)*100,1))


#### PATIENT_AGE

data_administration%>%
  select(PRESCRIPTION_ID_HASHED, 
         PATIENT_AGE_administration=PATIENT_AGE)%>%
  left_join(data_prescription%>%
              select(PRESCRIPTION_ID_HASHED,
                     PATIENT_AGE_prescription=PATIENT_AGE))%>%
  summarise(equal=n_distinct(PRESCRIPTION_ID_HASHED[PATIENT_AGE_prescription==PATIENT_AGE_administration]),
            distinct=n_distinct(PRESCRIPTION_ID_HASHED[PATIENT_AGE_prescription!=PATIENT_AGE_administration]))%>%
  pivot_longer(everything(),names_to = "Similarity", values_to="n")%>%
  mutate(proportion=round(n/sum(n)*100,1))


data_administration%>%
  select(PRESCRIPTION_ID_HASHED, 
         PATIENT_AGE_administration=PATIENT_AGE,
         Study_ID)%>%
  left_join(data_prescription%>%
              select(PRESCRIPTION_ID_HASHED,
                     PATIENT_AGE_prescription=PATIENT_AGE))%>%
  filter(PATIENT_AGE_administration!=PATIENT_AGE_prescription)%>%
  distinct(Study_ID, PATIENT_AGE_administration, PATIENT_AGE_prescription)%>%
  mutate(age_difference=PATIENT_AGE_administration-PATIENT_AGE_prescription)%>%
  View()


#### PATIENT_GENDER

data_administration%>%
  select(PRESCRIPTION_ID_HASHED, 
         PATIENT_GENDER_administration=PATIENT_GENDER)%>%
  left_join(data_prescription%>%
              select(PRESCRIPTION_ID_HASHED,
                     PATIENT_GENDER_prescription=PATIENT_GENDER))%>%
  summarise(equal=n_distinct(PRESCRIPTION_ID_HASHED[PATIENT_GENDER_prescription==PATIENT_GENDER_administration]),
            distinct=n_distinct(PRESCRIPTION_ID_HASHED[PATIENT_GENDER_prescription!=PATIENT_GENDER_administration]))%>%
  pivot_longer(everything(),names_to = "Similarity", values_to="n")%>%
  mutate(proportion=round(n/sum(n)*100,1))


data_administration%>%
  select(PRESCRIPTION_ID_HASHED, 
         PATIENT_GENDER_administration=PATIENT_GENDER,
         Study_ID)%>%
  left_join(data_prescription%>%
              select(PRESCRIPTION_ID_HASHED,
                     PATIENT_GENDER_prescription=PATIENT_GENDER))%>%
  filter(PATIENT_GENDER_administration!=PATIENT_GENDER_prescription)%>%
  distinct(Study_ID, PATIENT_GENDER_administration, PATIENT_GENDER_prescription)

#### MEDICATION_NAME

data_administration%>%
  select(PRESCRIPTION_ID_HASHED, 
         MEDICATION_NAME_administration=MEDICATION_NAME)%>%
  left_join(data_prescription%>%
              select(PRESCRIPTION_ID_HASHED,
                     MEDICATION_NAME_prescription=MEDICATION_NAME))%>%
  summarise(equal=n_distinct(PRESCRIPTION_ID_HASHED[MEDICATION_NAME_prescription==MEDICATION_NAME_administration]),
            distinct=n_distinct(PRESCRIPTION_ID_HASHED[MEDICATION_NAME_prescription!=MEDICATION_NAME_administration]))%>%
  pivot_longer(everything(),names_to = "Similarity", values_to="n")%>%
  mutate(proportion=round(n/sum(n)*100,1))


#### ADMINISTERED_DATE_TIME
#### SCHEDULED_DATE_TIME
#### RECORDED_DATE_TIME
#### LAST_UPDATED_DATE

data_administration%>%
  select(ADMINISTERED_DATE_TIME)%>%
  arrange(desc(ADMINISTERED_DATE_TIME))%>%
  View()

# scheduled vs initial_authorised_date_time (prescription)

data_administration%>%
  select(PRESCRIPTION_ID_HASHED, SCHEDULED_DATE_TIME)%>%
  left_join(data_prescription%>%
              select(PRESCRIPTION_ID_HASHED,
                     INITIAL_AUTHORISED_DATE_TIME))%>%
  summarise(equal=n_distinct(PRESCRIPTION_ID_HASHED[SCHEDULED_DATE_TIME==INITIAL_AUTHORISED_DATE_TIME]),
            distinct=n_distinct(PRESCRIPTION_ID_HASHED[SCHEDULED_DATE_TIME!=INITIAL_AUTHORISED_DATE_TIME]))%>%
  pivot_longer(everything(),names_to = "Similarity", values_to="n")%>%
  mutate(proportion=round(n/sum(n)*100,1))



# scheduled vs administered 
data_administration%>%
  select(PRESCRIPTION_ID_HASHED,ADMINISTERED_DATE_TIME, SCHEDULED_DATE_TIME)%>%
  summarise(equal=n_distinct(PRESCRIPTION_ID_HASHED[ADMINISTERED_DATE_TIME==SCHEDULED_DATE_TIME]),
            distinct=n_distinct(PRESCRIPTION_ID_HASHED[ADMINISTERED_DATE_TIME!=SCHEDULED_DATE_TIME]))%>%
  pivot_longer(everything(),names_to = "Similarity", values_to="n")%>%
  mutate(proportion=round(n/sum(n)*100,1))  

# administered vs recorded  
data_administration%>%
  select(PRESCRIPTION_ID_HASHED,ADMINISTERED_DATE_TIME, RECORDED_DATE_TIME
)%>%
  summarise(equal=n_distinct(PRESCRIPTION_ID_HASHED[ADMINISTERED_DATE_TIME==RECORDED_DATE_TIME
]),
            distinct=n_distinct(PRESCRIPTION_ID_HASHED[ADMINISTERED_DATE_TIME!=RECORDED_DATE_TIME
]))%>%
  pivot_longer(everything(),names_to = "Similarity", values_to="n")%>%
  mutate(proportion=round(n/sum(n)*100,1))  


# last updated vs recorded 

data_administration%>%
  select(PRESCRIPTION_ID_HASHED,LAST_UPDATED_DATE, RECORDED_DATE_TIME
  )%>%
  summarise(equal=n_distinct(PRESCRIPTION_ID_HASHED[LAST_UPDATED_DATE==RECORDED_DATE_TIME
  ]),
  distinct=n_distinct(PRESCRIPTION_ID_HASHED[LAST_UPDATED_DATE!=RECORDED_DATE_TIME
  ]))%>%
  pivot_longer(everything(),names_to = "Similarity", values_to="n")%>%
  mutate(proportion=round(n/sum(n)*100,1))  



#### administration vs prescription records -----


# prescription ID

## number of records
data_prescription%>%
  filter(!is.na(PRESCRIPTION_ID_HASHED))%>%
  nrow() # 306546 records with complete prescription IDs




## number of distinct IDs
data_prescription%>%
  distinct(PRESCRIPTION_ID_HASHED)%>%
  nrow() # 305896 distinct prescription IDs


  # histogram  
data_prescription%>%
  group_by(Study_ID)%>%
  summarise(n=n_distinct(PRESCRIPTION_ID_HASHED))%>%
  
  ggplot(aes(n))+
  geom_histogram(color="black")+
  theme(text=element_text(size=20))

ggsave("Outputs/Figures/Exploration/distinct_prescription_ids_histogram.png",
       width=50,
       height=30,
       units="cm")

data_prescription%>%
  group_by(Study_ID)%>%
  summarise(n=n_distinct(PRESCRIPTION_ID_HASHED))%>%
  summarise(median=median(n))

## number of distinct records per ID

data_prescription%>%
  group_by(PRESCRIPTION_ID_HASHED)%>%
  summarise(n=n())%>%
  summarise(median=median(n)) # on average only 1 record per prescription id



# administration ID


## number of records
data_administration%>%
  filter(!is.na(ADMINISTRATION_ID_HASHED))%>%
  nrow() # 306546 records with complete administration IDs




## number of distinct records
data_administration%>%
  distinct(ADMINISTRATION_ID_HASHED)%>%
  nrow()


data_administration%>%
  group_by(Study_ID)%>%
  summarise(n=n_distinct(ADMINISTRATION_ID_HASHED))%>%
  
  ggplot(aes(n))+
  geom_histogram(color="black")+
  theme(text=element_text(size=20))

ggsave("Outputs/Figures/Exploration/distinct_administration_ids_histogram.png",
       width=50,
       height=30,
       units="cm")

data_administration%>%
  group_by(Study_ID)%>%
  summarise(n=n_distinct(ADMINISTRATION_ID_HASHED))%>%
  summarise(median=median(n))


## number of distinct records per ID

data_administration%>%
  group_by(ADMINISTRATION_ID_HASHED)%>%
  summarise(n=n())%>%
  summarise(median=median(n)) # on average only 1 record per administration id








# one vs other

## administration records per prescription record (histogram)
data_prescription%>%
  select(PRESCRIPTION_ID_HASHED, Study_ID)%>%
  left_join(data_administration%>%select(PRESCRIPTION_ID_HASHED, 
                                         ADMINISTRATION_ID_HASHED, 
                                         Study_ID),
            by=c("PRESCRIPTION_ID_HASHED"))%>%
  group_by(PRESCRIPTION_ID_HASHED)%>%
  summarise(n=n_distinct(ADMINISTRATION_ID_HASHED))%>%

  ggplot(aes(n))+
  geom_histogram(color="black", binwidth = 5)+
  theme(text=element_text(size=20))+
  coord_cartesian(xlim=c(0,200))

ggsave("Outputs/Figures/Exploration/distinct_administration_ids_per_prescription_ids_histogram.png",
       width=50,
       height=30,
       units="cm")

## administration records per prescription record (median)
data_prescription%>%
  select(PRESCRIPTION_ID_HASHED, Study_ID)%>%
  left_join(data_administration%>%select(PRESCRIPTION_ID_HASHED, 
                                         ADMINISTRATION_ID_HASHED, 
                                         Study_ID),
            by=c("PRESCRIPTION_ID_HASHED"))%>%
  group_by(PRESCRIPTION_ID_HASHED)%>%
  summarise(n=n_distinct(ADMINISTRATION_ID_HASHED))%>%
  summarise(median=median(n))
  # on average only 1 administration record per prescription record


## prescription records vs administration record (after linking with prescription table)
data_administration%>%
  select(ADMINISTRATION_ID_HASHED, PRESCRIPTION_ID_HASHED_ADMINISTRATION=PRESCRIPTION_ID_HASHED, Study_ID_administration=Study_ID)%>%
  filter(!is.na(PRESCRIPTION_ID_HASHED_ADMINISTRATION))%>%
  left_join(data_prescription%>%select(PRESCRIPTION_ID_HASHED_PRESCRIPTION=PRESCRIPTION_ID_HASHED, Study_ID_prescription=Study_ID),
            by=c("PRESCRIPTION_ID_HASHED_ADMINISTRATION"="PRESCRIPTION_ID_HASHED_PRESCRIPTION"),
            keep=T)%>%
  summarise(complete=n_distinct(ADMINISTRATION_ID_HASHED[!is.na(PRESCRIPTION_ID_HASHED_PRESCRIPTION)])/n()*100)
  # 3.6% don't have a corresponding prescription record

data_administration%>%
  select(ADMINISTRATION_ID_HASHED, ADMINISTERED_DATE_TIME, PRESCRIPTION_ID_HASHED, Study_ID_administration=Study_ID)%>%
  filter(!is.na(PRESCRIPTION_ID_HASHED))%>%
  left_join(data_prescription%>%select(PRESCRIPTION_ID_HASHED, STATUS, Study_ID_prescription=Study_ID),
            by="PRESCRIPTION_ID_HASHED")%>%
  count(STATUS)%>%
  mutate(prop=n/sum(n)*100)
# only 67.9% of administration records with a valid administration date have a prescription record with "active" status

## number of administration records per prescription record (after linking tables)
data_administration%>%
  select(ADMINISTRATION_ID_HASHED, PRESCRIPTION_ID_HASHED_ADMINISTRATION=PRESCRIPTION_ID_HASHED, Study_ID_administration=Study_ID)%>%
  filter(!is.na(PRESCRIPTION_ID_HASHED_ADMINISTRATION))%>%
  left_join(data_prescription%>%select(PRESCRIPTION_ID_HASHED_PRESCRIPTION=PRESCRIPTION_ID_HASHED, Study_ID_prescription=Study_ID),
            by=c("PRESCRIPTION_ID_HASHED_ADMINISTRATION"="PRESCRIPTION_ID_HASHED_PRESCRIPTION"),
            keep=T)%>%
  group_by(PRESCRIPTION_ID_HASHED_ADMINISTRATION)%>%
  summarise(n=n_distinct(ADMINISTRATION_ID_HASHED))%>%
  summarise(median=median(n))

















### adminstration dmd ------

data_administration_dmd%>%
  colnames()

#### MAPPED_DMD_CODE

data_administration_dmd%>%
  select(ADMINISTRATION_ID_HASHED, 
         MAPPED_DMD_CODE_administration=MAPPED_DMD_CODE)%>%
  left_join(data_administration%>%select(PRESCRIPTION_ID_HASHED,
                                         ADMINISTRATION_ID_HASHED))%>%
  left_join(data_prescription_dmd%>%
              select(PRESCRIPTION_ID_HASHED,
                     MAPPED_DMD_CODE_prescription=MAPPED_DMD_CODE))%>%
  summarise(equal=n_distinct(PRESCRIPTION_ID_HASHED[MAPPED_DMD_CODE_administration==MAPPED_DMD_CODE_prescription]),
            distinct=n_distinct(PRESCRIPTION_ID_HASHED[MAPPED_DMD_CODE_administration!=MAPPED_DMD_CODE_prescription]))%>%
  pivot_longer(everything(),names_to = "Similarity", values_to="n")%>%
  mutate(proportion=round(n/sum(n)*100,1))


#### MAPPED_DMD_INVALID

data_administration_dmd%>%
  select(ADMINISTRATION_ID_HASHED, 
         MAPPED_DMD_INVALID_administration=MAPPED_DMD_INVALID)%>%
  left_join(data_administration%>%select(PRESCRIPTION_ID_HASHED,
                                         ADMINISTRATION_ID_HASHED))%>%
  left_join(data_prescription_dmd%>%
              select(PRESCRIPTION_ID_HASHED,
                     MAPPED_DMD_INVALID_prescription=MAPPED_DMD_INVALID))%>%
  summarise(equal=n_distinct(PRESCRIPTION_ID_HASHED[MAPPED_DMD_INVALID_administration==MAPPED_DMD_INVALID_prescription]),
            distinct=n_distinct(PRESCRIPTION_ID_HASHED[MAPPED_DMD_INVALID_administration!=MAPPED_DMD_INVALID_prescription]))%>%
  pivot_longer(everything(),names_to = "Similarity", values_to="n")%>%
  mutate(proportion=round(n/sum(n)*100,1))


#### MAPPED_DMD_NAME

data_administration_dmd%>%
  select(ADMINISTRATION_ID_HASHED, 
         MAPPED_DMD_NAME_administration=MAPPED_DMD_NAME)%>%
  left_join(data_administration%>%select(PRESCRIPTION_ID_HASHED,
                                         ADMINISTRATION_ID_HASHED))%>%
  left_join(data_prescription_dmd%>%
              select(PRESCRIPTION_ID_HASHED,
                     MAPPED_DMD_NAME_prescription=MAPPED_DMD_NAME))%>%
  summarise(equal=n_distinct(PRESCRIPTION_ID_HASHED[MAPPED_DMD_NAME_administration==MAPPED_DMD_NAME_prescription]),
            distinct=n_distinct(PRESCRIPTION_ID_HASHED[MAPPED_DMD_NAME_administration!=MAPPED_DMD_NAME_prescription]))%>%
  pivot_longer(everything(),names_to = "Similarity", values_to="n")%>%
  mutate(proportion=round(n/sum(n)*100,1))



#### MAPPED_DMD_LEVEL

data_administration_dmd%>%
  select(ADMINISTRATION_ID_HASHED, 
         MAPPED_DMD_LEVEL_administration=MAPPED_DMD_LEVEL)%>%
  left_join(data_administration%>%select(PRESCRIPTION_ID_HASHED,
                                         ADMINISTRATION_ID_HASHED))%>%
  left_join(data_prescription_dmd%>%
              select(PRESCRIPTION_ID_HASHED,
                     MAPPED_DMD_LEVEL_prescription=MAPPED_DMD_LEVEL))%>%
  summarise(equal=n_distinct(PRESCRIPTION_ID_HASHED[MAPPED_DMD_LEVEL_administration==MAPPED_DMD_LEVEL_prescription]),
            distinct=n_distinct(PRESCRIPTION_ID_HASHED[MAPPED_DMD_LEVEL_administration!=MAPPED_DMD_LEVEL_prescription]))%>%
  pivot_longer(everything(),names_to = "Similarity", values_to="n")%>%
  mutate(proportion=round(n/sum(n)*100,1))

#### MAPPED_DMD_SOURCE

data_administration_dmd%>%
  select(ADMINISTRATION_ID_HASHED, 
         MAPPED_DMD_SOURCE_administration=MAPPED_DMD_SOURCE)%>%
  left_join(data_administration%>%select(PRESCRIPTION_ID_HASHED,
                                         ADMINISTRATION_ID_HASHED))%>%
  left_join(data_prescription_dmd%>%
              select(PRESCRIPTION_ID_HASHED,
                     MAPPED_DMD_SOURCE_prescription=MAPPED_DMD_SOURCE))%>%
  summarise(equal=n_distinct(PRESCRIPTION_ID_HASHED[MAPPED_DMD_SOURCE_administration==MAPPED_DMD_SOURCE_prescription]),
            distinct=n_distinct(PRESCRIPTION_ID_HASHED[MAPPED_DMD_SOURCE_administration!=MAPPED_DMD_SOURCE_prescription]))%>%
  pivot_longer(everything(),names_to = "Similarity", values_to="n")%>%
  mutate(proportion=round(n/sum(n)*100,1))


#### VTM

data_administration_dmd%>%
  select(ADMINISTRATION_ID_HASHED, 
         VTM_administration=VTM)%>%
  left_join(data_administration%>%select(PRESCRIPTION_ID_HASHED,
                                         ADMINISTRATION_ID_HASHED))%>%
  left_join(data_prescription_dmd%>%
              select(PRESCRIPTION_ID_HASHED,
                     VTM_prescription=VTM))%>%
  summarise(equal=n_distinct(PRESCRIPTION_ID_HASHED[VTM_administration==VTM_prescription]),
            distinct=n_distinct(PRESCRIPTION_ID_HASHED[VTM_administration!=VTM_prescription]))%>%
  pivot_longer(everything(),names_to = "Similarity", values_to="n")%>%
  mutate(proportion=round(n/sum(n)*100,1))


#### VTM_NAME

data_administration_dmd%>%
  select(ADMINISTRATION_ID_HASHED, 
         VTM_NAME_administration=VTM_NAME)%>%
  left_join(data_administration%>%select(PRESCRIPTION_ID_HASHED,
                                         ADMINISTRATION_ID_HASHED))%>%
  left_join(data_prescription_dmd%>%
              select(PRESCRIPTION_ID_HASHED,
                     VTM_NAME_prescription=VTM_NAME))%>%
  summarise(equal=n_distinct(PRESCRIPTION_ID_HASHED[VTM_NAME_administration==VTM_NAME_prescription]),
            distinct=n_distinct(PRESCRIPTION_ID_HASHED[VTM_NAME_administration!=VTM_NAME_prescription]))%>%
  pivot_longer(everything(),names_to = "Similarity", values_to="n")%>%
  mutate(proportion=round(n/sum(n)*100,1))


#### VMP

data_administration_dmd%>%
  select(ADMINISTRATION_ID_HASHED, 
         VMP_administration=VMP)%>%
  left_join(data_administration%>%select(PRESCRIPTION_ID_HASHED,
                                         ADMINISTRATION_ID_HASHED))%>%
  left_join(data_prescription_dmd%>%
              select(PRESCRIPTION_ID_HASHED,
                     VMP_prescription=VMP))%>%
  summarise(equal=n_distinct(PRESCRIPTION_ID_HASHED[VMP_administration==VMP_prescription]),
            distinct=n_distinct(PRESCRIPTION_ID_HASHED[VMP_administration!=VMP_prescription]))%>%
  pivot_longer(everything(),names_to = "Similarity", values_to="n")%>%
  mutate(proportion=round(n/sum(n)*100,1))


#### VMP_NAME

data_administration_dmd%>%
  select(ADMINISTRATION_ID_HASHED, 
         VMP_NAME_administration=VMP_NAME)%>%
  left_join(data_administration%>%select(PRESCRIPTION_ID_HASHED,
                                         ADMINISTRATION_ID_HASHED))%>%
  left_join(data_prescription_dmd%>%
              select(PRESCRIPTION_ID_HASHED,
                     VMP_NAME_prescription=VMP_NAME))%>%
  summarise(equal=n_distinct(PRESCRIPTION_ID_HASHED[VMP_NAME_administration==VMP_NAME_prescription]),
            distinct=n_distinct(PRESCRIPTION_ID_HASHED[VMP_NAME_administration!=VMP_NAME_prescription]))%>%
  pivot_longer(everything(),names_to = "Similarity", values_to="n")%>%
  mutate(proportion=round(n/sum(n)*100,1))


#### AMP

data_administration_dmd%>%
  select(ADMINISTRATION_ID_HASHED, 
         AMP_administration=AMP)%>%
  left_join(data_administration%>%select(PRESCRIPTION_ID_HASHED,
                                         ADMINISTRATION_ID_HASHED))%>%
  left_join(data_prescription_dmd%>%
              select(PRESCRIPTION_ID_HASHED,
                     AMP_prescription=AMP))%>%
  summarise(equal=n_distinct(PRESCRIPTION_ID_HASHED[AMP_administration==AMP_prescription]),
            distinct=n_distinct(PRESCRIPTION_ID_HASHED[AMP_administration!=AMP_prescription]))%>%
  pivot_longer(everything(),names_to = "Similarity", values_to="n")%>%
  mutate(proportion=round(n/sum(n)*100,1))


#### AMP_NAME

data_administration_dmd%>%
  select(ADMINISTRATION_ID_HASHED, 
         AMP_administration=AMP)%>%
  left_join(data_administration%>%select(PRESCRIPTION_ID_HASHED,
                                         ADMINISTRATION_ID_HASHED))%>%
  left_join(data_prescription_dmd%>%
              select(PRESCRIPTION_ID_HASHED,
                     AMP_prescription=AMP))%>%
  summarise(equal=n_distinct(PRESCRIPTION_ID_HASHED[AMP_administration==AMP_prescription]),
            distinct=n_distinct(PRESCRIPTION_ID_HASHED[AMP_administration!=AMP_prescription]))%>%
  pivot_longer(everything(),names_to = "Similarity", values_to="n")%>%
  mutate(proportion=round(n/sum(n)*100,1))


#### AMP_NAME

data_administration_dmd%>%
  select(ADMINISTRATION_ID_HASHED, 
         AMP_administration=AMP)%>%
  left_join(data_administration%>%select(PRESCRIPTION_ID_HASHED,
                                         ADMINISTRATION_ID_HASHED))%>%
  left_join(data_prescription_dmd%>%
              select(PRESCRIPTION_ID_HASHED,
                     AMP_prescription=AMP))%>%
  summarise(equal=n_distinct(PRESCRIPTION_ID_HASHED[AMP_administration==AMP_prescription]),
            distinct=n_distinct(PRESCRIPTION_ID_HASHED[AMP_administration!=AMP_prescription]))%>%
  pivot_longer(everything(),names_to = "Similarity", values_to="n")%>%
  mutate(proportion=round(n/sum(n)*100,1))

#### investigate dmd coding -----



data_administration_dmd%>%
  select(MAPPED_DMD_CODE, MAPPED_DMD_NAME, MAPPED_DMD_SOURCE)%>%
  left_join(snomed_descriptions%>%
              select(id, conceptId, term)%>%
              mutate(across(everything(), ~as.character(.))), by=c("MAPPED_DMD_CODE" = "conceptId"))%>%
  group_by(MAPPED_DMD_SOURCE)%>%
  slice_sample(n=100)%>%
  View()


rm(snomed_descriptions)



### administration dose --------

data_administration_dosage%>%colnames()

##### ROUTEINTEXT

data_administration_dosage%>%
  count(ROUTEINTEXT)


##### DOSAGEAS_NEEDEDBOOLEAN

data_administration_dosage%>%
  count(DOSAGEAS_NEEDEDBOOLEAN)





## completeness -----


### prescription

completeness_table<-
  data_prescription%>%
  select(-Study_ID)%>%
  as_tibble()%>%
  summarise_all(funs(sum(complete.cases(.))/nrow(data_prescription)*100))%>%
  t()%>%
  as.data.frame()%>%
  mutate(value=V1, .keep=c("unused"))

completeness_table$field<-
  row.names(completeness_table)

completeness_table$field%<>%
  factor(levels=data_prescription%>%colnames())

row.names(completeness_table)<-NULL


completeness_plot<-
  completeness_table%>%
  ggplot(aes(x=as.factor(field), y=value))+
  geom_bar(stat='identity')+
  xlab("Field")+
  ylab("Completeness rate (%)")+
  theme(axis.text.x = element_text(angle = 30,hjust=1),
        plot.margin = unit(c(1,0,0,2), "cm"))+
  geom_text(stat='identity', aes(label=round(value, 1)), vjust=-0.5, size=4, hjust=0.5)+
  labs(title="Individual field completeness (prescription table)")+
  scale_y_continuous(expand = expansion(c(0,0.1)))

completeness_plot

ggsave("Outputs/Figures/Exploration/completeness_prescription.png", 
       last_plot(),
       width=40, 
       dpi = "retina",
       units="cm")


### prescription dmd

completeness_table<-
  data_prescription_dmd%>%
  select(-Study_ID)%>%
  as_tibble()%>%
  summarise_all(funs(sum(complete.cases(.))/nrow(data_prescription_dmd)*100))%>%
  t()%>%
  as.data.frame()%>%
  mutate(value=V1, .keep=c("unused"))

completeness_table$field<-
  row.names(completeness_table)

completeness_table$field%<>%
  factor(levels=data_prescription_dmd%>%colnames())

row.names(completeness_table)<-NULL


completeness_plot<-
  completeness_table%>%
  ggplot(aes(x=as.factor(field), y=value))+
  geom_bar(stat='identity')+
  xlab("Field")+
  ylab("Completeness rate (%)")+
  theme(axis.text.x = element_text(angle = 30,hjust=1),
        plot.margin = unit(c(1,0,0,2), "cm"))+
  geom_text(stat='identity', aes(label=round(value, 1)), vjust=-0.5, size=4, hjust=0.5)+
  labs(title="Individual field completeness (prescription dmd table)")+
  scale_y_continuous(expand = expansion(c(0,0.1)))


completeness_plot

ggsave("Outputs/Figures/Exploration/completeness_prescription_dmd.png", 
       last_plot(),
       width=40, 
       dpi = "retina",
       units="cm")


### prescription dose

completeness_table<-
  data_prescription_dosage%>%
  select(-Study_ID)%>%
  as_tibble()%>%
  summarise_all(funs(sum(complete.cases(.))/nrow(data_prescription_dosage)*100))%>%
  t()%>%
  as.data.frame()%>%
  mutate(value=V1, .keep=c("unused"))

completeness_table$field<-
  row.names(completeness_table)

completeness_table$field%<>%
  factor(levels=data_prescription_dosage%>%colnames())

row.names(completeness_table)<-NULL


completeness_plot<-
  completeness_table%>%
  ggplot(aes(x=as.factor(field), y=value))+
  geom_bar(stat='identity')+
  xlab("Field")+
  ylab("Completeness rate (%)")+
  theme(axis.text.x = element_text(angle = 30,hjust=1),
        plot.margin = unit(c(1,0,0,2), "cm"))+
  geom_text(stat='identity', aes(label=round(value, 1)), vjust=-0.5, size=4, hjust=0.5)+
  labs(title="Individual field completeness (prescription dose table)")+
  scale_y_continuous(expand = expansion(c(0,0.1)))


completeness_plot

ggsave("Outputs/Figures/Exploration/completeness_prescription_dose.png", 
       last_plot(),
       width=40, 
       dpi = "retina",
       units="cm")


### administration

completeness_table<-
  data_administration%>%
  select(-Study_ID)%>%
  as_tibble()%>%
  summarise_all(funs(sum(complete.cases(.))/nrow(data_administration)*100))%>%
  t()%>%
  as.data.frame()%>%
  mutate(value=V1, .keep=c("unused"))

completeness_table$field<-
  row.names(completeness_table)

completeness_table$field%<>%
  factor(levels=data_administration%>%colnames())

row.names(completeness_table)<-NULL


completeness_plot<-
  completeness_table%>%
  ggplot(aes(x=as.factor(field), y=value))+
  geom_bar(stat='identity')+
  xlab("Field")+
  ylab("Completeness rate (%)")+
  theme(axis.text.x = element_text(angle = 30,hjust=1),
        plot.margin = unit(c(1,0,0,2), "cm"))+
  geom_text(stat='identity', aes(label=round(value, 1)), vjust=-0.5, size=4, hjust=0.5)+
  labs(title="Individual field completeness (administration table)")+
  scale_y_continuous(expand = expansion(c(0,0.1)))


completeness_plot

ggsave("Outputs/Figures/Exploration/completeness_administration.png", 
       last_plot(),
       width=40, 
       dpi = "retina",
       units="cm")


### administration dmd

completeness_table<-
  data_administration_dmd%>%
  select(-Study_ID)%>%
  as_tibble()%>%
  summarise_all(funs(sum(complete.cases(.))/nrow(data_administration_dmd)*100))%>%
  t()%>%
  as.data.frame()%>%
  mutate(value=V1, .keep=c("unused"))

completeness_table$field<-
  row.names(completeness_table)

completeness_table$field%<>%
  factor(levels=data_administration_dmd%>%colnames())

row.names(completeness_table)<-NULL


completeness_plot<-
  completeness_table%>%
  ggplot(aes(x=as.factor(field), y=value))+
  geom_bar(stat='identity')+
  xlab("Field")+
  ylab("Completeness rate (%)")+
  theme(axis.text.x = element_text(angle = 30,hjust=1),
        plot.margin = unit(c(1,0,0,2), "cm"))+
  geom_text(stat='identity', aes(label=round(value, 1)), vjust=-0.5, size=4, hjust=0.5)+
  labs(title="Individual field completeness (administration dmd table)")+
  scale_y_continuous(expand = expansion(c(0,0.1)))


completeness_plot

ggsave("Outputs/Figures/Exploration/completeness_administration_dmd.png", 
       last_plot(),
       width=40, 
       dpi = "retina",
       units="cm")


### administration dose

completeness_table<-
  data_administration_dosage%>%
  select(-Study_ID)%>%
  as_tibble()%>%
  summarise_all(funs(sum(complete.cases(.))/nrow(data_administration_dosage)*100))%>%
  t()%>%
  as.data.frame()%>%
  mutate(value=V1, .keep=c("unused"))

completeness_table$field<-
  row.names(completeness_table)

completeness_table$field%<>%
  factor(levels=data_administration_dosage%>%colnames())

row.names(completeness_table)<-NULL


completeness_plot<-
  completeness_table%>%
  ggplot(aes(x=as.factor(field), y=value))+
  geom_bar(stat='identity')+
  xlab("Field")+
  ylab("Completeness rate (%)")+
  theme(axis.text.x = element_text(angle = 30,hjust=1),
        plot.margin = unit(c(1,0,0,2), "cm"))+
  geom_text(stat='identity', aes(label=round(value, 1)), vjust=-0.5, size=4, hjust=0.5)+
  labs(title="Individual field completeness (administration dose table)")+
  scale_y_continuous(expand = expansion(c(0,0.1)))


completeness_plot

ggsave("Outputs/Figures/Exploration/completeness_administration_dose.png", 
       last_plot(),
       width=40, 
       dpi = "retina",
       units="cm")


# rm(data_prescription_dosage, data_administration_dosage)
rm(completeness_plot, completeness_table)







# DATA TRANSFORMATIONS --------



## apply censoring rules ------



### Restrict to people recruited in England -------

baseline_crf_england<-
  baseline_crf%>%
  mutate(siteid=as.integer(siteid))%>%
  filter(!Study_ID %in% withdrawn_participants)%>%
  left_join(Sites, by=c("siteid"="SiteID"))%>%
  filter(Nation=="England")%>%
  mutate(Study_ID=as.character(Study_ID))

nrow(baseline_crf_england) # 41493 (after excluding withdrawals, but before applying right censoring)


## Transform baseline data (drug flags) -----------

pre_rand_drugs_crf<-
  drugs_crf_sdtm%>%
  filter(Study_ID%in%epma_participants_list_administration_raw |
           Study_ID%in%epma_participants_list_prescription_raw)%>%
  filter(visitnum==1)%>%
  select(Study_ID, 
         Complete = cmpresp, 
         Drug=cmtrt, 
         Flag=cmoccur, 
         rand_date=cmsttpt)%>%
  filter(Drug %in% c("Macrolide",
                     "Systemic corticosteroids (e.g. dexamethasone, prednisolone, hydrocortisone)",
                     "Remdesivir",
                     "Warfarin or direct oral anticoagulant",
                     "Antiplatelet therapy",
                     "Tocilizumab or sarilumab"))%>%
  filter(Flag %in% c("Y", "N"))%>%
  mutate(Flag=if_else(Flag=="Y", 1, 0))%>%
  mutate(Drug=if_else(Drug=="Macrolide", "Macrolides", Drug))%>%
  mutate(Drug=if_else(Drug=="Systemic corticosteroids (e.g. dexamethasone, prednisolone, hydrocortisone)", "Corticosteroids", Drug))%>%
  mutate(Drug=if_else(Drug=="Warfarin or direct oral anticoagulant", "Oral anticoagulants", Drug))%>%
  mutate(Drug=if_else(Drug=="Antiplatelet therapy", "Antiplatelets", Drug))
  
pre_rand_drugs_crf%<>%
  rbind(
    baseline_crf_england%>%
      filter(Study_ID%in%epma_participants_list_administration_raw | 
               Study_ID%in%epma_participants_list_prescription_raw)%>%
      select(Study_ID, oxygen)%>%
      mutate(Drug="Oxygen")%>%
      rename(Flag=oxygen)%>%
      mutate(Flag=if_else(Flag=="Y", 1, 0))%>%
      mutate(Complete="Y")%>%
      left_join(rand_dates%>%select(Study_ID, rand_date)%>%mutate(Study_ID=as.character(Study_ID))))%>%
  
  rbind(
    baseline_crf_england%>%
      filter(Study_ID%in%epma_participants_list_administration_raw | 
               Study_ID%in%epma_participants_list_prescription_raw)%>%
      select(Study_ID, LMWH_baseline)%>%
      mutate(Drug="LMWH")%>%
      rename(Flag=LMWH_baseline)%>%
      mutate(Flag=case_when(Flag=="Standard low molecular weight heparin" | Flag == "Higher dose low molecular weight heparin" ~ 1, 
                            Flag =="None" ~ 0))%>%
      filter(!is.na(Flag))%>%
      mutate(Complete="Y")%>%
      left_join(rand_dates%>%select(Study_ID, rand_date)%>%mutate(Study_ID=as.character(Study_ID))))
    
    
pre_rand_drugs_crf%>%
  distinct(Drug)%>%View()






## Transform FU data (drug flags) -------------


crf_post_rand_drug_flags<-
  drugs_crf_adam%>%
  filter(Study_ID %in% epma_participants_list_prescription_raw | 
           Study_ID %in% epma_participants_list_administration_raw)%>%
  filter(paramcd!="NOADDTRT")%>%
  mutate(Drug=case_when(paramcd=="ASPIRIN" ~ "Aspirin",
                        paramcd=="CORTICO" ~ "Corticosteroids",
                        paramcd=="HOCHLQ" ~ "Hydroxychloroquine",
                        paramcd=="LOPRIT" ~ "Lopinavir-ritonavir",
                        paramcd=="NOADDTRT" ~ "Usual care",
                        paramcd=="AZITHMAC" ~ "Macrolides",
                        # paramcd=="MACRO" ~ "Other macrolides",
                        paramcd=="TOCSAR" ~ "Tocilizumab or sarilumab",
                        paramcd=="REMDES" ~ "Remdesivir",
                        paramcd=="BARIC" ~ "Baricitinib",
                        paramcd=="COLCHIC" ~ "Colchicine",
                        paramcd=="REGEN" ~ "REGN antibodies",
                        # paramcd=="INOTROPE" ~ "Inotropes"
                        ))%>%
  filter(!is.na(Drug))%>%
  mutate(days_number = ifelse(str_detect(param, "Number of days") & aval!="-1", aval, NA),
         dose_number = ifelse(str_detect(param, "doses") & aval!="-1", aval, NA),
         days_proportion_aspirin = ifelse(str_detect(param, "Proportion") & aval!="-1", aval, NA),
         flag=if_else(!aval %in% c(0,NA), 1, 0) # flag is positive if the number of days received is 1-10 or -1 (missing but CRF confirms drug was given); flag is negative if the number of days received is missing or 0 (CRF states they didn't receive it)
  )%>%
  mutate(days_proportion_aspirin = case_when(days_proportion_aspirin=="1" ~ "≥90%",
                                             days_proportion_aspirin=="2" ~ "≥50-90%",
                                             days_proportion_aspirin=="3" ~ "0-50%"))%>%
  select(Study_ID, Drug, days_number, dose_number, days_proportion_aspirin, flag)%>%
  mutate(across(everything(),~as.character(.)))%>%
  pivot_longer(-c(Study_ID, Drug), names_to="Variable", values_to="Value")%>%
  filter(!is.na(Value))




## Transform EPMA data (drug flags) -------


### review medication names (to select those of interest) ------


data_administration%>%
  distinct(MEDICATION_NAME)%>%
  arrange(MEDICATION_NAME)%>%
  View()


### choose relevant formulations  ------

data_prescription%>%
  count(FORM)%>%
  arrange(desc(n))%>%
  View()

systemic_formulations<-c("Tablets",
         "Injection",
         "Capsules",
         "NA",
         "tablets",
         "Oral solution",
         "injection",
         "Pre-filled Syringe",
         "Intravenous Infusion",
         "Dispersible Tablets",
         "Caplets",
         "Liquid",
         "Sachets",
         "capsules",
         "Solution",
         "Effervescent Tablets",
         "Syringe",
         "Infusion",
         "Modified Release Tablets",
         "Suspension",
         "Soluble Tablets",
         "Pre-Filled Syringe",
         "tablets.",
         "Powder",
         "Chewable Tablets",
         "Oral Powder",
         "Oral Suspension",
         "Syrup",
         "Intravenous Injection",
         "Modified Release Capsules",
         "Modified-Release Tablets",
         "dispersible tablets",
         "Modified-Release Capsules",
         "Sustained Release Tablets",
         "Patch",
         "Suppositories",
         "Injection for Infusion",
         "Pre-filled syringe",
         "Solution for Infusion",
         "Bag",
         "infusion",
         "Sublingual Spray",
         "Patches",
         "Spray",
         "liquid.",
         "Orodispersible Tablet",
         "Chewable Tablet",
         "Enteric Coated Tablets",
         "MR tablets",
         "Subcutaneous Injection",
         "Pre-Filled Pen",
         "Sustained Release Capsules",
         "Oral Solution",
         "Oral Liquid",
         "Liquid Food",
         "Suspension.",
         "Pre-filled Pen",
         "Solution for Injection",
         "Gastro-resistant Capsules",
         "Powder for Infusion",
         "Oral Sugar-Free Solution",
         "Mixture",
         "Enteric Coated Capsules",
         "Prolonged Release Tablets",
         "Linctus",
         "sachet",
         "chewable tablets",
         "Film Coated Tablets",
         "Orodispersible Tablets",
         "Injection Vial - 10 ml",
         "Oral Gel",
         "FlexPen",
         "Oral Spray",
         "Sachet",
         "Syrup..",
         "suspension",
         "Ampoule",
         "Pump Spray",
         "MR capsules",
         "effervescent tablets",
         "Controlled Release Tablets",
         "Granules",
         "enteric coated tablets",
         "Powder for Solution",
         "Syrup.",
         "Solostar Pen",
         "oral solution",
         "Pre-filled disposable device",
         "Injection Powder",
         "Modified-release Tablets",
         "Buccal Tablets",
         "patch",
         "Powder for Solution Infusion",
         "Elixir",
         "Sugar-free Solution",
         "Powder for Soln for Infusion",
         "Cartridge",
         "buccal tablets",
         "Sugar-free powder",
         "Sugar-Free Oral Solution",
         "Oral SF Solution",
         "oral liquid",
         "Cartridges",
         "Sugar-Free Linctus",
         "Gastro-resistant Tablets",
         "Prolonged-Release Tablets",
         "Gastro-Resistant Capsules",
         "Oral Solution Sugar Free",
         "Injection Ampoule - 1 mL",
         "Sugar-Free Suspension",
         "Sugar Free Suspension",
         "Oral Drops",
         "10mL Vial",
         "suppositories",
         "syringe",
         "Oral Sugar-Free Suspension",
         "Injection Pen - 3 ml",
         "Subcutaneous Infusion",
         "Injection Ampoule",
         "syrup",
         "Enteric-Coated Tablets",
         "orodispersible tablets",
         "7 day patches",
         "Vial",
         "AS PER CHART",
         "Instruction ONLY",
         "See additional chart",
         "sachets",
         "Sublingual Tablets",
         "Double Strength Tablets",
         "Intramuscular Injection",
         "Sachets.",
         "Chart",
         "SR tabs",
         "depot injection",
         "Conc for Soln for Infusion",
         "Modified-release Capsules",
         "Injection Solution",
         "Controlled Release Capsules",
         "Pre-filled Pens",
         "3mL Flexpen",
         "3ml Cartridge",
         "Linctus BP",
         "oral powder",
         "Sugar Free Solution",
         "liquid",
         "Disposable Pens",
         "Caplet",
         "3mL Kwikpen",
         "Oral Syrup",
         "oral spray",
         "Oral Sugar-Free Syrup",
         "Tablets (Brown)",
         "7 Day Patch",
         "Injection Vial - 2 ml",
         "Oro-Dispersible Tablets",
         "3mL Solostar Pen",
         "granule sachet",
         "Lozenges",
         "Sugar-Free Liquid",
         "Sugar Free Soluble Tablets",
         "Spray Solution",
         "Powder for Soln for Injection",
         "Solution For Infusion",
         "KwikPen",
         "Multidose Vials",
         "Pre-Filled syringe",
         "Prolonged-release tablets",
         "sublingual tablets",
         "Sugar Free Oral Solution",
         "Oral Sugar-Free Liquid",
         "-Injection",
         "caplets",
         "Chewable tablets",
         "Crushable Tablets",
         "Oral Powder Sugar Free",
         "Oral solution sachets",
         "Oral Sugar/Alcohol-Free Liquid",
         "Oral S/F Linctus",
         "Prolonged-Release Capsule",
         "3mL Flextouch Pen",
         "Dissolving Tablet",
         "Oral soln",
         "Penfill",
         "Penfill Cartridge - 3 ml",
         "Modified Release Granules",
         "Penfils",
         "Preservative-Free Injection",
         "Prolonged Release Injection",
         "mg",
         "Oily injection",
         "Oral S/F Solution",
         "Orodispersible Film",
         "Sugar Free Syrup",
         "EC Capsule",
         "Pre-Filled Pen - 3ml",
         "Slow Release Tablets",
         "Capsules ( 100 Pack)",
         "Gastro-Resistant Tablets",
         "Immediate-Release Capsules",
         "Injection (with diluent)",
         "linctus",
         "Pre Filled Syringe",
         "Solution For Injection",
         "Gum",
         "Injection (Unlicensed)",
         "Modified Release Tablet",
         "Rectal Tube",
         "Sugar-free Syrup",
         "Chewing Gum",
         "E/C tabs",
         "Mixture BP",
         "Oral SF Suspension",
         "Orodispersible tablet",
         "powder",
         "Pre-filled pen",
         "Rectal Solution",
         "Rectal Foam",
         "foam enema",
         "Enema",
         "Ready-To-Use Enema",
         "enema",
         "Micro-enema",
         "Disposable Enema",
         "Foam Enema",
         "Micro-Enema",
         "Sachets Sugar Free",
         "Sugar-free Linctus",
         "Sugar-Free Oral Suspension",
         "Transdermal Matrix Patch",
         "Modified release sachets",
         "Pre-filled Injection",
         "Slow Release Capsules",
         "Solution for Infusion bag",
         "Sugar-Free Syrup",
         "Sugar free suspension",
         "Auto-injector",
         "Auto injector",
         "Innolet - 3ml",
         "MUPS Tablets",
         "Solostar prefilled pen",
         "Soluble Tabs",
         "CAPLETS",
         "Film coated tablet",
         "Immediate Release Tablets",
         "Injections",
         "oral lyophilisates",
         "Oral Sugar Free Solution",
         "Oral Suspension S/F",
         "Oromucosal Gel Sugar Free",
         "PFP",
         "Powders",
         "Rectal Tubes",
         "Soak Tablets",
         "subcutaneous infusion",
         "ampoule",
         "Chewable Tablet S/F",
         "chewable. tablets",
         "Concentrate soln. for infusion",
         "Effervescent Granules",
         "Foam Enema",
         "Gastro Resistant Tablet",
         "Injection Pen",
         "Medicated Chewing Gum",
         "Micro-Enema",
         "Modified Release Suspension",
         "Oral Granules",
         "Oral Lyophilisates",
         "Oral Powder Sachets",
         "Oral Solution (S/F Sachets)",
         "Oral Solution Sachets",
         "Oral solution sugar-free",
         "Oromucosal Solution",
         "Powder for Injection",
         "Pre-filled Injector",
         "Pre-loaded Pen",
         "Prolonged Release Capsule",
         "Prolonged Release Granules",
         "rectal solution",
         "S/F Sachets",
         "Solution Tablets",
         "Sugar-free mixture",
         "Sugar Free Linctus",
         "Tablets ( 28 Pack)",
         "Twice-Weekly Patch",
         "Caplet(s)",
         "Carpuject Cartridges",
         "Conc for Solution for Infusion",
         "Discs",
         "Elixir.",
         "Gastro-resistant Granules",
         "I/M inj",
         "Injection*",
         "Liquid for enteral tubes ONLY",
         "MINI-JET",
         "Modified-Release Granules",
         "Mouthspray",
         "Optipen Cartridges",
         "Oral Gel S/F",
         "Oral S/F Suspension",
         "Oral Sachet",
         "Oral Suspension sugar free",
         "Oromucosal prefilled syringe",
         "Oromucosal Spray",
         "PCA Smith Cadd Cassette",
         "Powder For Solution Injection",
         "Powder Sachets",
         "Rectal solution",
         "Sugar-Free Powder",
         "Sugar free liquid",
         "Sugar Free Oral Suspension",
         "Wafers")

oxygen_formulations<-c("Oxygen",
                       "Inhalation",
                       "Medical Gas",
                       "mask",
                       "Gas",
                       "AS PER CHART",
                       "Reminder",
                       "See note")



### derive medication flags ----

#### prescription records -------

##### drugs -------
prescription_drug_records<-
  data_prescription%>%
  select(PRESCRIPTION_ID_HASHED, Study_ID, MEDICATION_NAME, FORM, INITIAL_AUTHORISED_DATE_TIME, STATUS, TYPE)%>%
  # filter(FORM %in% systemic_formulations)%>% # restrict to systemic formulations
  mutate(date=as.Date(INITIAL_AUTHORISED_DATE_TIME, format="%T-%m-%d"))%>%
  distinct(Study_ID, MEDICATION_NAME,PRESCRIPTION_ID_HASHED, date, FORM, STATUS, TYPE)%>%
  
  # Extract drug flags
  mutate(Aspirin=if_else(str_detect(MEDICATION_NAME, regex("aspirin", ignore_case=T)), 1, 0),
         Corticosteroids = if_else(str_detect(MEDICATION_NAME, regex("dexamethasone|prednisolone|hydrocortisone|methylprednisolone", ignore_case = T)), "1", "0"),
         Baricitinib =if_else(str_detect(MEDICATION_NAME, regex("baricitinib", ignore_case=T)), 1, 0),
         `Tocilizumab or sarilumab`= if_else(str_detect(MEDICATION_NAME, regex("tocilizumab|sarilumab", ignore_case=T)), 1, 0),
         Hydroxychloroquine  = if_else(str_detect(MEDICATION_NAME, regex("Hydroxychloroquine", ignore_case=T)), 1, 0),
         `Lopinavir-ritonavir`= if_else(str_detect(MEDICATION_NAME, regex("LOPINAVIR", ignore_case=T)), 1, 0),
         Azithromycin= if_else(str_detect(MEDICATION_NAME, regex("azithromycin", ignore_case=T)), 1, 0),
         `Macrolides`= if_else(str_detect(MEDICATION_NAME, regex("azithromycin|erythromycin|clarithromycin", ignore_case=T)), 1, 0),
         # `Other macrolides`= if_else(str_detect(MEDICATION_NAME, regex("erythromycin|clarithromycin", ignore_case=T)), 1, 0),
         Remdesivir= if_else(str_detect(MEDICATION_NAME, regex("remdesivir", ignore_case=T)), 1, 0),
         Colchicine= if_else(str_detect(MEDICATION_NAME, regex("colchicine", ignore_case=T)), 1, 0),
         `REGN antibodies`= if_else(str_detect(MEDICATION_NAME, regex("REGN|casirivimab|imdevimab|ronapreve", ignore_case=T)), 1, 0),
         `Oral anticoagulants`= if_else(str_detect(MEDICATION_NAME, regex("warfarin|rivaroxaban|apixaban|dabigatran|edoxaban|ACENOCOUMAROL|Phenindione|Phenprocoumon", ignore_case=T)), 1, 0),
         Antiplatelets= if_else(str_detect(MEDICATION_NAME, regex("aspirin|clopidogrel|ticagrelor|dipyridamole|eptifibatide|prasugrel|ticlopidine", ignore_case=T)), 1, 0),
         Immunoglobulin = if_else(str_detect(MEDICATION_NAME, regex("immunoglob|ivig|iv ig", ignore_case=T)), 1, 0),
         LMWH = if_else(str_detect(MEDICATION_NAME, regex("dalteparin|tinzaparin|enoxaparin", ignore_case=T)), 1, 0))%>%
  
  
  
  select(-MEDICATION_NAME)%>%
  mutate(across(everything(), ~as.character(.)))%>%
  pivot_longer(-c(Study_ID, date, PRESCRIPTION_ID_HASHED, STATUS, FORM, TYPE), names_to = "Drug", values_to = "Flag")%>%
  distinct(Study_ID, date, Drug, Flag, STATUS, FORM, TYPE, .keep_all=T)%>%
  mutate(Table="Prescription",
         ADMINISTRATION_ID_HASHED = NA)


##### oxygen -------

oxygen_prescription_records<-
  data_prescription%>%
  select(PRESCRIPTION_ID_HASHED, Study_ID, MEDICATION_NAME, FORM,STATUS,TYPE, INITIAL_AUTHORISED_DATE_TIME)%>%
  mutate(date=as.Date(INITIAL_AUTHORISED_DATE_TIME, format="%T-%m-%d"))%>%
  distinct(Study_ID, MEDICATION_NAME, PRESCRIPTION_ID_HASHED, FORM, date, STATUS, TYPE)%>%
  mutate(Oxygen=if_else(str_detect(MEDICATION_NAME, regex("oxygen", ignore_case=T)), 1, 0))%>%
  mutate(across(everything(), ~as.character(.)))%>%
  select(-MEDICATION_NAME)%>%
  pivot_longer(-c(Study_ID, date, PRESCRIPTION_ID_HASHED, STATUS, FORM, TYPE), names_to = "Drug", values_to = "Flag")%>%
  distinct(Study_ID, date, Drug, Flag, STATUS,FORM,TYPE,.keep_all=T)%>%
  mutate(Table="Prescription",
         ADMINISTRATION_ID_HASHED= NA)













#### administration records -------

##### drugs --------
administration_drug_records<-
  data_administration%>%
  select(PRESCRIPTION_ID_HASHED, ADMINISTRATION_ID_HASHED, Study_ID, MEDICATION_NAME, ADMINISTERED_DATE_TIME)%>%
  left_join(data_prescription%>%select(PRESCRIPTION_ID_HASHED, FORM, STATUS, TYPE))%>%
  # filter(FORM %in% systemic_formulations)%>% # restrict to systemic formulations
  mutate(date=as.Date(ADMINISTERED_DATE_TIME, format="%T-%m-%d"))%>%
  distinct(Study_ID, MEDICATION_NAME, PRESCRIPTION_ID_HASHED, date, FORM,STATUS,TYPE, ADMINISTRATION_ID_HASHED)%>%
  
  # Extract drug flags
  mutate(Aspirin=if_else(str_detect(MEDICATION_NAME, regex("aspirin", ignore_case=T)), 1, 0),
         Corticosteroids = if_else(str_detect(MEDICATION_NAME, regex("dexamethasone|prednisolone|hydrocortisone|methylprednisolone", ignore_case = T)), "1", "0"),
         Baricitinib =if_else(str_detect(MEDICATION_NAME, regex("baricitinib", ignore_case=T)), 1, 0),
         `Tocilizumab or sarilumab`= if_else(str_detect(MEDICATION_NAME, regex("tocilizumab|sarilumab", ignore_case=T)), 1, 0),
         Hydroxychloroquine  = if_else(str_detect(MEDICATION_NAME, regex("Hydroxychloroquine", ignore_case=T)), 1, 0),
         `Lopinavir-ritonavir`= if_else(str_detect(MEDICATION_NAME, regex("LOPINAVIR|KALETRA", ignore_case=T)), 1, 0),
         Azithromycin= if_else(str_detect(MEDICATION_NAME, regex("azithromycin", ignore_case=T)), 1, 0),
         `Macrolides`= if_else(str_detect(MEDICATION_NAME, regex("azithromycin|erythromycin|clarithromycin", ignore_case=T)), 1, 0),
         # `Other macrolides`= if_else(str_detect(MEDICATION_NAME, regex("erythromycin|clarithromycin", ignore_case=T)), 1, 0),
         Remdesivir= if_else(str_detect(MEDICATION_NAME, regex("remdesivir", ignore_case=T)), 1, 0),
         Colchicine= if_else(str_detect(MEDICATION_NAME, regex("colchicine", ignore_case=T)), 1, 0),
         `REGN antibodies`= if_else(str_detect(MEDICATION_NAME, regex("REGN|casirivimab|imdevimab|ronapreve", ignore_case=T)), 1, 0),
         `Oral anticoagulants`= if_else(str_detect(MEDICATION_NAME, regex("warfarin|rivaroxaban|apixaban|dabigatran|edoxaban|ACENOCOUMAROL|Phenindione|Phenprocoumon", ignore_case=T)), 1, 0),
         Antiplatelets= if_else(str_detect(MEDICATION_NAME, regex("aspirin|clopidogrel|ticagrelor|dipyridamole|eptifibatide|prasugrel|ticlopidine", ignore_case=T)), 1, 0),
         Immunoglobulin = if_else(str_detect(MEDICATION_NAME, regex("immunoglob|ivig|iv ig", ignore_case=T)), 1, 0),
         LMWH = if_else(str_detect(MEDICATION_NAME, regex("dalteparin|tinzaparin|enoxaparin", ignore_case=T)), 1, 0))%>%
  
  
  
  select(-MEDICATION_NAME)%>%
  mutate(across(everything(), ~as.character(.)))%>%
  pivot_longer(-c(Study_ID, date, PRESCRIPTION_ID_HASHED, FORM, ADMINISTRATION_ID_HASHED, STATUS, TYPE), names_to = "Drug", values_to = "Flag")%>%
  distinct(Study_ID, date, Drug, Flag, STATUS,FORM, TYPE,.keep_all=T)%>%
  mutate(Table="Administration")
         
         
        
##### oxygen --------

oxygen_administration_records<-
  data_administration%>%
  select(PRESCRIPTION_ID_HASHED, ADMINISTRATION_ID_HASHED, Study_ID, MEDICATION_NAME, ADMINISTERED_DATE_TIME)%>%
  mutate(date=as.Date(ADMINISTERED_DATE_TIME, format="%T-%m-%d"))%>%
  left_join(data_prescription%>%select(PRESCRIPTION_ID_HASHED, FORM, STATUS, TYPE))%>%
  distinct(Study_ID, MEDICATION_NAME, PRESCRIPTION_ID_HASHED, date, FORM, STATUS, ADMINISTRATION_ID_HASHED, TYPE)%>%
  mutate(Oxygen=if_else(str_detect(MEDICATION_NAME, regex("oxygen", ignore_case=T)), 1, 0))%>%
  mutate(across(everything(), ~as.character(.)))%>%
  select(-MEDICATION_NAME)%>%
  pivot_longer(-c(Study_ID, date, PRESCRIPTION_ID_HASHED, FORM, STATUS, ADMINISTRATION_ID_HASHED, TYPE), names_to = "Drug", values_to = "Flag")%>%
  distinct(Study_ID, date, Drug, Flag, STATUS, FORM, TYPE,.keep_all=T)%>%
  mutate(Table="Administration")


### Merge all ------

epma_drug_flags<-
  prescription_drug_records%>%
  rbind(administration_drug_records,
        oxygen_administration_records,
        oxygen_prescription_records)

rm(prescription_drug_records, administration_drug_records, oxygen_administration_records, oxygen_prescription_records)

# View(epma_drug_flags)

epma_drug_flags%>%
  distinct(Drug)





## Generate analysis periods -------

### Duration of admission for baseline assessments ------
# need to restrict EPMA records to time period of the present admission (before randomisation)

censoring_dates<-

# take date of admission and death/discharge from crf
baseline_crf_england%>%
  filter(Study_ID%in%epma_participants_list_administration_raw | 
           Study_ID%in%epma_participants_list_prescription_raw)%>% # restrict to people with raw EPMA data
  select(Study_ID, randdt, days_since_admission, discharge_day, death_day)%>%
  mutate(admidate_crf = randdt-days_since_admission,
         disdate_crf=randdt+discharge_day-1,
         disdeath_crf=randdt+death_day-1)%>%
  
  # take date of admission and discharge from HES
  left_join(
    HES_data%>%
      select(study_number, admidate, disdate)%>%
      filter(study_number%in%epma_participants_list_administration_raw | 
               study_number%in%epma_participants_list_prescription_raw)%>%
      left_join(rand_dates%>%mutate(Study_ID=as.character(Study_ID)), by=c("study_number"="Study_ID"))%>%
      filter(admidate<=rand_date & (disdate>=rand_date | is.na(disdate)))%>%
      rename(admidate_hes = admidate,
             disdate_hes=disdate)%>%
      mutate(study_number=as.character(study_number)),
    by=c("Study_ID"="study_number"))%>%
  
  # use crf dates unless missing, then HES, then randomisation date
  ungroup()%>%
  mutate(admission_date= case_when(!is.na(admidate_crf) ~ admidate_crf,
                                   !is.na(admidate_hes) ~ admidate_hes),
         
         discharge_date = if_else(!is.na(disdate_crf), disdate_crf, disdate_hes),
         death_or_discharge_date= if_else(disdeath_crf<discharge_date & !is.na(disdeath_crf), disdeath_crf, discharge_date))%>%
  mutate(admission_date = if_else(is.na(admission_date), randdt, admission_date))%>%
  
  select(Study_ID, 
         rand_date=randdt,
         admission_date,
         admidate_crf,
         admidate_hes
         # death_or_discharge_date
         )%>%



### 28 days after randomisation (or discharge/death) ------


left_join(
  drugs_crf_sdtm%>%
  filter(visitnum=="3")%>% # select follow-up forms
  distinct(Study_ID, 
           right_censoring_date_death_discharge_28d=cmsttpt, # this is the day of discharge, death, or completion of the FU form (whichever is earliest)
           censoring_study_day=cmsttpdy)%>%
  left_join(rand_dates%>%mutate(Study_ID=as.character(Study_ID)))%>%
  select(-rand_date))%>%
  
  mutate(admission_study_day = as.integer(difftime(admission_date, rand_date, units="days"))+1)%>%
  mutate(admission_study_day = if_else(admission_study_day==0, -1, admission_study_day))%>%
  distinct(Study_ID, .keep_all = T)

distinct(censoring_dates, Study_ID) # 5134 (this is before applying right-censoring, so different from 5116 in the consort diagram; the 18 extra people are those recruited June onwards)
 
# ANALYSIS -------


## CONSORT diagram -------


###  people included in the raw administration table ----- 
length(epma_participants_list_administration_raw) # 4943

###  people included in the raw prescription table ------ 
length(epma_participants_list_prescription_raw) # 5125

epma_participants_list_raw<-union(epma_participants_list_administration_raw, epma_participants_list_prescription_raw)
length(epma_participants_list_raw) # 5150

### people with administration data in the admission of randomisation ------

participants_with_data_in_randomisation_admission_administration<-
  data_administration%>%
  select(Study_ID, 
         administered_date = ADMINISTERED_DATE_TIME,
         scheduled_date = SCHEDULED_DATE_TIME,
         recorded_date = REPORTED_DATE_TIME,
         last_updated_date = LAST_UPDATED_DATE)%>%
  mutate(date = case_when(is.na(administered_date) ~ scheduled_date,
                          is.na(scheduled_date) ~ recorded_date,
                          is.na(recorded_date) ~ last_updated_date,
                          !is.na(administered_date) ~ administered_date))%>%
  left_join(
    censoring_dates%>%
      select(Study_ID,
             admission_date,
             right_censoring_date_death_discharge_28d))%>%
  group_by(Study_ID)%>%
  filter(any(date>=admission_date),
         any(date<=right_censoring_date_death_discharge_28d))%>%
  distinct(Study_ID)%>%
  .[[1]]

length(participants_with_data_in_randomisation_admission_administration) # 4222(before right censoring) 

### people with prescription data in the admission of randomisation ------

participants_with_data_in_randomisation_admission_prescription<-
  data_prescription%>%
  select(Study_ID, 
         date = INITIAL_AUTHORISED_DATE_TIME)%>%
  left_join(
    censoring_dates%>%
      select(Study_ID,
             admission_date,
             right_censoring_date_death_discharge_28d))%>%
  group_by(Study_ID)%>%
  filter(any(date>=admission_date),
         any(date<=right_censoring_date_death_discharge_28d))%>%
  distinct(Study_ID)%>%
  .[[1]]

length(participants_with_data_in_randomisation_admission_prescription) # 4291 (before right censoring)

length(setdiff(participants_with_data_in_randomisation_admission_prescription,
               participants_with_data_in_randomisation_admission_administration))
# 91 people in prescription but not administration

length(setdiff(participants_with_data_in_randomisation_admission_administration,
               participants_with_data_in_randomisation_admission_prescription)) # 22 in admission but not prescription

### people with any data in the admission of randomisation -----

participants_with_data_in_randomisation_admission<-union(participants_with_data_in_randomisation_admission_administration, participants_with_data_in_randomisation_admission_prescription)

length(participants_with_data_in_randomisation_admission) # 4313 (before applying right censoring)



### CONSORT diagram counts -----

length(withdrawn_participants) # 296 withdrawals

nrow(baseline_crf) # 48255 people in the raw CRF data (excluding withdrawals), until 12/12/22

nrow(baseline_crf_england) # 41493 in England (after excluding withdrawals, but before applying right censoring)



baseline_crf%>%
  left_join(Sites%>%mutate(SiteID=as.character(SiteID)), by=c("siteid"="SiteID"))%>%
  # nrow() # 48255 up until 12/12/2022
  filter(Nation=="England")%>%
  # nrow() # 41762 in England
  filter(age>15)%>%
  # nrow() # 41348 aged 16+
  filter(withdrfl!="Y")%>%
  # nrow() # 41214 excluding withdrawals
  filter(randdt<"2022-06-01")%>% 
  # nrow()  # 40860 recruited before June 2022 (data stability lag)

# filter(Study_ID %in% epma_participants_list_administration_raw)%>%
# nrow() # 4911 with administration data
# filter(Study_ID %in% epma_participants_list_prescription_raw)%>%
# nrow() # 5091 with prescription data
# filter(Study_ID %in% epma_participants_list_prescription_raw |
#          Study_ID %in% epma_participants_list_administration_raw)%>%
# nrow() # 5116 with any data



# filter(Study_ID %in% participants_with_data_in_randomisation_admission)%>%
# nrow() # 4313 with EPMA data during admission of randomisation (any table)
# filter(Study_ID %in% participants_with_data_in_randomisation_admission_prescription)%>%
# nrow() # 4291 with EPMA data during admission of randomisation (prescription table)
filter(Study_ID %in% participants_with_data_in_randomisation_admission_administration)%>%
  nrow() # 4222 with EPMA data during admission of randomisation (administration table)









length(epma_participants_list_prescription_raw) # 5125 in the raw prescription table
length(epma_participants_list_administration_raw) # 4943 in the raw administration table
length(union(epma_participants_list_administration_raw, epma_participants_list_prescription_raw)) # 5150 in any of the raw tables

length(participants_with_data_in_randomisation_admission_prescription) # 4291 
length(participants_with_data_in_randomisation_admission_administration) # 4222
length(union(participants_with_data_in_randomisation_admission_prescription, participants_with_data_in_randomisation_admission_administration)) # 4313 in any of the raw tables restricting to admission of randomisation


### apply above restrictions to the data------


baseline_crf_england%<>%
  # left_join(Sites%>%mutate(SiteID=as.character(SiteID)), by=c("siteid"="SiteID"))%>%
  filter(Nation=="England")%>%
  filter(age>15)%>%
  filter(withdrfl!="Y")%>%
  filter(randdt<"2022-06-01")

nrow(baseline_crf_england) # 40860

study_population<-baseline_crf_england%>%
  distinct(Study_ID)%>%
  filter(Study_ID %in% participants_with_data_in_randomisation_admission)%>%
  .[[1]]

length(study_population) # 4313

epma_drug_flags%<>%
  filter(Study_ID %in% study_population)

## 1. Cohort characteristics -------

colnames(baseline_crf)














### 1.1 Table - baseline characteristics grouped by linkage status -------

#### check distributions for age
baseline_crf_england%>%
  select(Study_ID, age)%>%
  mutate(`EPMA data availability` = if_else(Study_ID %in% epma_participants_list_raw, "Available", "Not available"))%>%
  ggplot(aes(age, fill=`EPMA data availability`, group=`EPMA data availability`))+
  geom_density(alpha=0.5, color="black")

#### check distributions for days since admission
baseline_crf_england%>%
  select(Study_ID, days_since_admission)%>%
  mutate(`EPMA data availability` = if_else(Study_ID %in% epma_participants_list_prescription, "Available", "Not available"))%>%
  filter(days_since_admission<50)%>%
  group_by(`EPMA data availability`)%>%
  count(days_since_admission)%>%
  mutate(prop=n/sum(n)*100)%>%
  ggplot(aes(days_since_admission, y=prop, fill=`EPMA data availability`, group=`EPMA data availability`))+
  geom_bar(position=position_dodge(), color="black", stat="identity")

##### at any time point -------

#### produce table
baseline_crf_england%>%
  mutate(`EPMA data availability` = if_else(Study_ID %in% epma_participants_list_raw, "Available", "Not available"))%>%
  select(age,
         resp_status,
         sex,
         race,
         days_since_admission,
         diabetes,
         cardiac,
         liver,
         kidney,
         lung,
         renal_replacement,
         `EPMA data availability`)%>%

  tbl_summary(by="EPMA data availability",
              label = list(sex~"Female",
                           resp_status~"Respiratory support at randomisation",
                           race~"Race",
                           renal_replacement ~ "Baseline renal replacement therapy"),
              value = list(diabetes~"Y",
                           cardiac~"Y",
                           liver~"Y",
                           kidney~"Y",
                           lung~"Y",
                           renal_replacement~"Y",
                           sex~"F"),
              statistic = list(age ~ "{mean} ({sd})"),
              missing = "no")->baseline_cohort_characteristics_table
  # add_p(
  #   list(age~"t.test"),
  #   test.args=list(age~list(var.equal=T)))
baseline_cohort_characteristics_table%>%
  as_flex_table()%>%
  add_header_row(top=T, values=c("", "EPMA data availability"), colwidths = c(1,2))

baseline_cohort_characteristics_table%>%
  custom_tab_gtsummary(header="Cohort characteristics grouped by EPMA data availability",
                       footer=NULL)

sect_properties <- prop_section(page_size = page_size(
  orient = "portrait",
  # width = 15,
  #height = 11.7
),
type = "continuous",
page_margins = page_mar())

baseline_cohort_characteristics_table

save_as_docx(baseline_cohort_characteristics_table%>%
               as_flex_table()%>%
               add_header_row(top=T, values=c("", "EPMA data availability"), colwidths = c(1,2)),
             path="Outputs/Tables/baseline_cohort_characteristics_table.docx")

save_as_docx(baseline_cohort_characteristics_table%>%
               custom_tab_gtsummary(header="Cohort characteristics grouped by EPMA data availability",
                                    footer=NULL),
             path="Outputs/Tables/baseline_cohort_characteristics_table.docx")


#### restrict to people with EPMA data available in the admission of randomisation -----


baseline_crf_england%>%
  mutate(`EPMA data availability` = if_else(Study_ID %in% participants_with_data_in_randomisation_admission, "Available", "Not available"))%>%
  select(age,
         resp_status,
         sex,
         race,
         days_since_admission,
         diabetes,
         cardiac,
         liver,
         kidney,
         lung,
         renal_replacement,
         `EPMA data availability`)%>%
  
  tbl_summary(by="EPMA data availability",
              label = list(sex~"Female",
                           resp_status~"Respiratory support at randomisation",
                           race~"Race",
                           renal_replacement ~ "Baseline renal replacement therapy"),
              value = list(diabetes~"Y",
                           cardiac~"Y",
                           liver~"Y",
                           kidney~"Y",
                           lung~"Y",
                           renal_replacement~"Y",
                           sex~"F"),
              statistic = list(age ~ "{mean} ({sd})"),
              missing = "no")->baseline_cohort_characteristics_table
# add_p(
#   list(age~"t.test"),
#   test.args=list(age~list(var.equal=T)))
baseline_cohort_characteristics_table%>%
  as_flex_table()%>%
  add_header_row(top=T, values=c("", "EPMA data availability"), colwidths = c(1,2))

baseline_cohort_characteristics_table%>%
  custom_tab_gtsummary(header="Cohort characteristics grouped by EPMA data availability",
                       footer=NULL)

sect_properties <- prop_section(page_size = page_size(
  orient = "portrait",
  # width = 15,
  #height = 11.7
),
type = "continuous",
page_margins = page_mar())

baseline_cohort_characteristics_table

save_as_docx(baseline_cohort_characteristics_table%>%
               as_flex_table()%>%
               add_header_row(top=T, values=c("", "EPMA data availability"), colwidths = c(1,2)),
             path="Outputs/Tables/baseline_cohort_characteristics_rand_admission_table.docx")

# save_as_docx(baseline_cohort_characteristics_table%>%
#                custom_tab_gtsummary(header="Cohort characteristics grouped by EPMA data availability",
#                                     footer=NULL),
#              path="Outputs/Tables/baseline_cohort_characteristics_rand_admission_table.docx")







### 1.2 Map-------

#### at any time point ------

baseline_crf_england%>%
  mutate(`EPMA data availability` = if_else(Study_ID %in% epma_participants_list_raw, "Available", "Not available"))%>%
  group_by(siteid, SiteName, Postcode, `EPMA data availability`)%>%
  summarise(Participants = n_distinct(Study_ID))%>%
  group_by(siteid, SiteName, Postcode)%>%
  mutate(Proportion=round(Participants/sum(Participants)*100,1))%>%
  mutate(Postcode=str_to_upper(Postcode))%>%
  left_join(postcodes, by=c("Postcode"="Postcode 3"))%>%
  select(siteid, SiteName, Participants, `EPMA data availability`, Proportion, Easting, Northing)%>%
  mutate(SiteID=as.integer(siteid))->counts_per_trust
  

library(rgdal)
countries_map<-raster::shapefile(filename)

countries_map_df<-broom::tidy(countries_map)

map<-ggplot()+
  geom_polygon(data=countries_map_df%>%
                 filter(id==0),
               aes(x=long, 
                   y=lat, 
                   group=group,
               ), 
               colour="black", 
               fill="grey",
               size=0.05,
               alpha=0.5,
  )+
  geom_point(data=counts_per_trust%>%
               group_by(SiteName, Postcode,Easting, Northing)%>%
               summarise(Participants_total=sum(Participants),
                         `Proportion with available EPMA data` = round(Participants[`EPMA data availability`=="Available"]/sum(Participants)*100, 1)),
             mapping=aes(x=Easting,
                         y=Northing,
                         size=`Proportion with available EPMA data`,
                         fill=`Proportion with available EPMA data`,
                         # text=paste0("Site: ", SiteName,
                         #             "\n",
                         #             "RECOVERY participants: ", Participants_total,
                         #             "\n",
                         #             "Proportion with missing EPMA data: ", `Proportion with missing EPMA data`, "%"
                         # )
                         ),
             alpha=0.7,
             color="black",
             pch=21)+
  theme_void(base_size=20)+
  # oxpop_blue_panel+
  theme(text = element_text(family="Mulish", 
                            size=20
                            # color="white"
  ),
  legend.position="right",
  legend.key.height = unit(2, 'cm'),
  # panel.spacing.x = unit(2, "cm"),
  plot.caption = element_text(size=10,hjust = 0), 
  plot.title=element_text(hjust=0, size=20),
  plot.title.position = "plot", 
  # plot.subtitle.position="plot",
  # plot.caption.position =  "plot",
  plot.background = element_rect(fill="white", color=NA)
  )+
  scale_fill_viridis(option="magma", direction = -1,
                     # breaks=seq(0,15,5),
                     limits=c(0, NA)
  )+
  # scale_color_viridis(
  #   # breaks=seq(0,200,20)
  #   )+
  guides(size="none")+
  labs(
    title="Geographical distribution of RECOVERY participants recruited in England with available EPMA data",
    caption="Bubble size and color depict proportion of participants with available EPMA data among all participants in each Trust",
    size="Proportion (%)",
    fill="Proportion (%)"
  )+
  coord_fixed()

map


ggsave("Outputs/Figures/Analysis/EPMA_linkage_map.png",
       last_plot(),
       # width=4,
       height=35,
       width=60,
       units="cm",
       dpi = "retina",
       # scale= 2.5
)

#### in admission of randomisation -------


baseline_crf_england%>%
  mutate(`EPMA data availability` = if_else(Study_ID %in% participants_with_data_in_randomisation_admission, "Available", "Not available"))%>%
  group_by(siteid, SiteName, Postcode, `EPMA data availability`)%>%
  summarise(Participants = n_distinct(Study_ID))%>%
  group_by(siteid, SiteName, Postcode)%>%
  mutate(Proportion=round(Participants/sum(Participants)*100,1))%>%
  mutate(Postcode=str_to_upper(Postcode))%>%
  left_join(postcodes, by=c("Postcode"="Postcode 3"))%>%
  select(siteid, SiteName, Participants, `EPMA data availability`, Proportion, Easting, Northing)%>%
  mutate(SiteID=as.integer(siteid))->counts_per_trust


library(rgdal)
countries_map<-raster::shapefile(filename)

countries_map_df<-broom::tidy(countries_map)

map<-ggplot()+
  geom_polygon(data=countries_map_df%>%
                 filter(id==0),
               aes(x=long, 
                   y=lat, 
                   group=group,
               ), 
               colour="black", 
               fill="grey",
               size=0.05,
               alpha=0.5,
  )+
  geom_point(data=counts_per_trust%>%
               group_by(SiteName, Postcode,Easting, Northing)%>%
               summarise(Participants_total=sum(Participants),
                         `Proportion with available EPMA data` = round(Participants[`EPMA data availability`=="Available"]/sum(Participants)*100, 1)),
             mapping=aes(x=Easting,
                         y=Northing,
                         size=`Proportion with available EPMA data`,
                         fill=`Proportion with available EPMA data`,
                         # text=paste0("Site: ", SiteName,
                         #             "\n",
                         #             "RECOVERY participants: ", Participants_total,
                         #             "\n",
                         #             "Proportion with missing EPMA data: ", `Proportion with missing EPMA data`, "%"
                         # )
             ),
             alpha=0.7,
             color="black",
             pch=21)+
  theme_void(base_size=20)+
  # oxpop_blue_panel+
  theme(text = element_text(family="Mulish", 
                            size=20
                            # color="white"
  ),
  legend.position="right",
  legend.key.height = unit(2, 'cm'),
  # panel.spacing.x = unit(2, "cm"),
  plot.caption = element_text(size=10,hjust = 0), 
  plot.title=element_text(hjust=0, size=20),
  plot.title.position = "plot", 
  # plot.subtitle.position="plot",
  # plot.caption.position =  "plot",
  plot.background = element_rect(fill="white", color=NA)
  )+
  scale_fill_viridis(option="magma", direction = 1,
                     # breaks=seq(0,15,5),
                     limits=c(0, NA),
                     
  )+
  # scale_color_viridis(
  #   # breaks=seq(0,200,20)
  #   )+
  guides(size="none")+
  labs(
#    title="Geographical distribution of RECOVERY participants recruited in England with available EPMA data",
    # caption="Bubble size and color depict proportion of participants with available EPMA data among all participants in each Trust",
    size="Proportion (%)",
    fill="Proportion (%)"
  )+
  coord_fixed()

map


ggsave("Outputs/Figures/Analysis/EPMA_linkage_map_rand_admission.png",
       last_plot(),
       # width=4,
       height=35,
       width=30,
       units="cm",
       dpi = "retina",
       scale= 0.5
)









#### Number of sites with available EPMA data ------

data_administration%>%
  distinct(ODS_CODE)%>%
  nrow()
  # 26 trusts

data_prescription%>%
  distinct(ODS_CODE)%>%
  nrow()
# 26 trusts

Sites%>%
  filter(Nation=="England")%>%
  distinct(SiteID)%>%
  nrow()
  # 139 in the study


# rm(postcodes, countries_map, countries_map_df, trusts, counts_per_trust, participants_sites, participants_per_trust, sites,map, map_plot)




### 1.3 Recruitment date -------
data_prescription%>%
  select(Study_ID)%>%
  left_join(rand_dates)%>%
  arrange(desc(rand_date))%>%
  slice_head(n=1)

data_administration%>%
  select(Study_ID)%>%
  left_join(rand_dates)%>%
  arrange(desc(rand_date))%>%
  slice_head(n=1)

baseline_crf_england%>%
  left_join(rand_dates)%>%
  filter(rand_date<="2022-08-26")%>%
  filter(withdrfl!="Y")%>%
  distinct(Study_ID)%>%
  nrow()

# plot
baseline_crf_england%>%
  mutate(`EPMA data availability` = if_else(Study_ID %in% epma_participants_list_raw, "Available", "Not available"))%>%
  filter(randdt<"2022-06-01")%>%
  mutate(month=as.Date(paste0(str_sub(randdt,1,8), "01")), format="%Y-%m-%d")%>%
  group_by(month, `EPMA data availability`)%>%
  summarise(Participants=n_distinct(Study_ID))%>%
  ggplot(aes(month, Participants, color = `EPMA data availability`, group=`EPMA data availability`))+
  geom_point()+
  geom_line()+
  scale_x_date(date_breaks = "1 month",
               date_labels = "%Y %b",
               # expand = expansion(c(0,0.1))
               # limits=c(NA, as.Date("2022-07-01"))
               )+
  # geom_vline(aes(xintercept=as.Date("2022-09-30")),
  #            linetype="dashed")+
  # annotate(geom="text",
  #          x=as.Date("2022-09-30"),
  #          y=6000,
  #          label="Extract reception\n(30-09-2022)")+
  theme(text=element_text(size=20),
        axis.text.x = element_text(angle=30,
                                   vjust=1,
                                   hjust=1),
        legend.position = "bottom")+
  labs(x="Month of randomisation",
       title="Distribution of randomisation dates for participants with and without EPMA data (at monthly level)")


ggsave("Outputs/Figures/Analysis/epma_status_rand_dates_timeseries.png",
       width=60,
       height = 30,
       units = "cm",
       dpi="retina")




## 2. Data utility -------


### 2.2 BNF chapter coverage (prescription and administration) -------

bnf_chapters_counts<-
  data_administration%>%
  ungroup()%>%
  select(Study_ID, ADMINISTRATION_ID_HASHED, MEDICATION_NAME)%>%
  left_join(
      data_administration_dmd%>%
        ungroup()%>%
        select(Study_ID, MAPPED_DMD_CODE, ADMINISTRATION_ID_HASHED)%>%
        mutate(Study_ID=as.character(Study_ID)))%>%
  filter(Study_ID %in% study_population)%>%
  ungroup()%>%
  left_join(bnf_chapters%>%select(dmd_id, chapter), by=c("MAPPED_DMD_CODE"="dmd_id"))%>%View()
  group_by(chapter)%>%
  summarise(Participants=n_distinct(Study_ID))%>%
  mutate(Proportion=round(Participants/length(study_population)*100,0))%>%
  left_join(bnf_chapter_labels)%>%
  mutate(Table="Administration")%>%
  
  rbind(
    data_prescription%>%
      ungroup()%>%
      select(Study_ID, PRESCRIPTION_ID_HASHED, MEDICATION_NAME)%>%
        left_join(
          data_prescription_dmd%>%
            ungroup()%>%
            select(Study_ID, MAPPED_DMD_CODE, PRESCRIPTION_ID_HASHED)%>%
            mutate(Study_ID=as.character(Study_ID)))%>%
        filter(Study_ID %in% study_population)%>%
      mutate(MAPPED_DMD_CODE=as.character(MAPPED_DMD_CODE))%>%
      left_join(bnf_chapters%>%select(dmd_id, chapter), by=c("MAPPED_DMD_CODE"="dmd_id"))%>%
    group_by(chapter)%>%
        summarise(Participants=n_distinct(Study_ID))%>%
        mutate(Proportion=round(Participants/length(study_population)*100,0))%>%
        left_join(bnf_chapter_labels)%>%
        mutate(Table="Prescription")
      )
  

bnf_chapters_counts%>%
  mutate(bnf_labels=as.character(bnf_labels))%>%
  mutate(bnf_labels=replace_na(bnf_labels, "Not applicable"))%>%
  mutate(bnf_labels=as.factor(bnf_labels))%>%
  mutate(bnf_labels=reorder(bnf_labels, chapter))%>%
  mutate(Table=factor(Table, levels=c("Prescription", "Administration")))%>%
  
  ggplot(aes(bnf_labels, Participants, fill=Table))+
  geom_col(position="dodge",
           width=0.5)+
  geom_text(aes(label=paste0(Participants, "\n(", Proportion, "%)")),
            vjust=-1,
            position=position_dodge(width=0.9),
            size=4)+
  theme(text=element_text(size=20),
        axis.text.x = element_text(angle=30,
                                   vjust=1,
                                   hjust=1),
        legend.position = "bottom",
        axis.title.x=element_blank(),
        plot.margin = unit(c(1,0,0,2), "cm"))+
  labs(y="Number of distinct participants (% of total)",
       x="BNF chapters",
       fill="EPMA dataset table")+
  scale_y_continuous(expand=expansion(c(0,0.2)))

ggsave("Outputs/Figures/Analysis/bnf_chapters_coverage.png",
       dpi="retina",
       width=60,
       height=30,
       units="cm")

ggsave("Outputs/Figures/Analysis/bnf_chapters_coverage.tiff",
       dpi="retina",
       width=60,
       height=30,
       units="cm")


# slides

bnf_chapters_counts%>%
  mutate(bnf_labels=as.character(bnf_labels))%>%
  mutate(bnf_labels=replace_na(bnf_labels, "Not applicable"))%>%
  mutate(bnf_labels=as.factor(bnf_labels))%>%
  mutate(bnf_labels=reorder(bnf_labels, chapter))%>%
  mutate(Table=factor(Table, levels=c("Prescription", "Administration")))%>%
  
  ggplot(aes(bnf_labels, Participants, fill=Table))+
  geom_col(position="dodge",
           width=0.5)+
  geom_text(aes(label=paste0(Participants, "\n(", Proportion, "%)")),
            vjust=-1,
            position=position_dodge(width=0.9),
            size=4,
            color="white")+
  oxpop_blue_panel+
  theme(text=element_text(size=20),
        axis.text.x = element_text(angle=30,
                                   vjust=1,
                                   hjust=1),
        legend.position = "bottom",
        axis.title.x=element_blank(),
        plot.margin = unit(c(1,0,0,2), "cm"))+
  labs(y="Number of distinct participants (% of total)",
       x="BNF chapters",
       fill="EPMA dataset table")+
  scale_y_continuous(expand=expansion(c(0,0.2)))

ggsave("Outputs/Figures/Slides/bnf_chapters_coverage.png",
       dpi="retina",
       width=60,
       height=30,
       units="cm")



# rm(data_prescription_dmd, data_administration_dmd)

# rm(bnf_labels, bnf_chapter_labels, bnf_chapters, bnf_chapters_counts)

#### look at records with no BNF chapter ----

data_administration%>%
  left_join(
    data_administration_dmd%>%
      select(Study_ID, MAPPED_DMD_CODE, ADMINISTRATION_ID_HASHED)%>%
      mutate(Study_ID=as.character(Study_ID),
             MAPPED_DMD_CODE=as.character(MAPPED_DMD_CODE)))%>%
  filter(Study_ID %in% epma_participants_list_raw)%>%
  select(ADMINISTRATION_ID_HASHED, MAPPED_DMD_CODE, MEDICATION_NAME)%>%
  left_join(bnf_chapters, by=c("MAPPED_DMD_CODE"="dmd_id"))%>%
  filter(is.na(chapter))%>%
  distinct(
    # ADMINISTRATION_ID_HASHED, 
           MAPPED_DMD_CODE, MEDICATION_NAME)%>%
  arrange(MEDICATION_NAME)%>%
  View()


# save missing dmd codes 
data_administration%>%
  ungroup()%>%
  select(Study_ID, ADMINISTRATION_ID_HASHED, MEDICATION_NAME)%>%
  left_join(
    data_administration_dmd%>%
      ungroup()%>%
      select(Study_ID, MAPPED_DMD_CODE, ADMINISTRATION_ID_HASHED)%>%
      mutate(Study_ID=as.character(Study_ID)))%>%
  filter(Study_ID %in% study_population)%>%
  ungroup()%>%
  left_join(bnf_chapters%>%select(dmd_id, chapter), by=c("MAPPED_DMD_CODE"="dmd_id"))%>%filter(is.na(MAPPED_DMD_CODE))%>%select(ADMINISTRATION_ID_HASHED, MEDICATION_NAME, MAPPED_DMD_CODE)->missing_dmd_administration

write_csv(missing_dmd_administration, "Intermediate outputs/missing_dmd_administration_table.csv")

### 2.3 Specific drug coverage ------


drug_labels_order<-c(
  "Corticosteroids",
  "Hydroxychloroquine",
  "Lopinavir-ritonavir",
  "Azithromycin",
  "Macrolides",
  "Tocilizumab or sarilumab",
  "Colchicine",
  "Aspirin",
  "Antiplatelets",
  "Oral anticoagulants",
  "LMWH",
  "Baricitinib",
  "REGN antibodies",
  "Remdesivir",
  "Immunoglobulin",
  "Oxygen"
)

epma_drug_flags%>%
  filter(Study_ID %in% study_population)%>%
  group_by(Drug, Table)%>%
  summarise(Participants=n_distinct(Study_ID[Flag=="1"]))%>%
  mutate(Proportion=round(Participants/length(study_population)*100,0))%>%
  
  mutate(Table=factor(Table, levels=c("Prescription", "Administration")))%>%
  filter(Drug%in%drug_labels_order)%>%
  mutate(Drug=as.factor(Drug))%>%
  mutate(Drug=fct_relevel(Drug, drug_labels_order))%>%
  
  ggplot(aes(Drug, Participants, fill=Table))+
  geom_col(position="dodge",
           width=0.5)+
  geom_text(aes(label=paste0(Participants, "\n(", Proportion, "%)")),
            vjust=-1,
            position=position_dodge(width=0.9),
            size=5)+
  theme(text=element_text(size=20),
        axis.text.x = element_text(angle=30,
                                   vjust=1,
                                   hjust=1),
        legend.position = "bottom",
        axis.title.x=element_blank(),
        plot.margin = unit(c(1,0,0,2), "cm"))+
  labs(y="Number of distinct participants (% of total)",
       x="Drug group",
       fill="EPMA dataset table")+
  scale_y_continuous(expand=expansion(c(0,0.2)))

ggsave("Outputs/Figures/Analysis/drugs_coverage.png",
       dpi="retina",
       width=60,
       height=30,
       units="cm")


ggsave("Outputs/Figures/Analysis/drugs_coverage.tiff",
       dpi="retina",
       width=60,
       height=30,
       units="cm")


# slides

epma_drug_flags%>%
  filter(Study_ID %in% study_population)%>%
  group_by(Drug, Table)%>%
  summarise(Participants=n_distinct(Study_ID[Flag=="1"]))%>%
  mutate(Proportion=round(Participants/length(study_population)*100,0))%>%
  
  mutate(Table=factor(Table, levels=c("Prescription", "Administration")))%>%
  filter(Drug%in%drug_labels_order)%>%
  mutate(Drug=as.factor(Drug))%>%
  mutate(Drug=fct_relevel(Drug, drug_labels_order))%>%
  
  ggplot(aes(Drug, Participants, fill=Table))+
  geom_col(position="dodge",
           width=0.5)+
  geom_text(aes(label=paste0(Participants, "\n(", Proportion, "%)")),
            vjust=-1,
            color="white",
            position=position_dodge(width=0.9),
            size=5)+
  oxpop_blue_panel+
  theme(text=element_text(size=20),
        axis.text.x = element_text(angle=30,
                                   vjust=1,
                                   hjust=1),
        legend.position = "bottom",
        axis.title.x=element_blank(),
        plot.margin = unit(c(1,0,0,2), "cm"))+
  labs(y="Number of distinct participants (% of total)",
       x="Drug group",
       fill="EPMA dataset table")+
  scale_y_continuous(expand=expansion(c(0,0.2)))

ggsave("Outputs/Figures/Slides/drugs_coverage.png",
       dpi="retina",
       width=60,
       height=30,
       units="cm")


### 2.3 data stability/timeliness ---------


data_administration%>%
  select(Study_ID,
         REPORTED_DATE_TIME,
         ADMINISTERED_DATE_TIME)%>%
  View()
# data received 02/09/22; last item is for 28/09 (?) (if excluding records with date 2030) and last report date is 29/09



extract_76_date<-"2022-06-28"

extract_79_date<-"2022-09-30"

# extract rand date date for people in extract 76
data_prescription76%>%
  distinct(Study_ID)%>%
  left_join(censoring_dates)%>%
  slice_max(rand_date)%>%
  distinct(rand_date)%>%
  .[[1]]->last_rand_date_76_prescription # 18-05-2022

last_rand_date_76_prescription

data_prescription%>%
  select(Study_ID)%>%
  left_join(censoring_dates)%>%
  slice_max(rand_date)%>%
  distinct(rand_date)%>%
  .[[1]]



data_administration76%>%
  select(Study_ID)%>%
  left_join(censoring_dates)%>%
  slice_max(rand_date)%>%
  distinct(rand_date)%>%
  .[[1]]->last_rand_date_76_administration

last_rand_date_76_administration# 18-05-2022

# counts
data_prescription%>%
  select(Study_ID, MEDICATION_NAME, date = INITIAL_AUTHORISED_DATE_TIME)%>%
  mutate(date=as.Date(date, format="%Y-%m-%d"))%>%
  mutate(month=paste0(str_sub(date, 1, 8), "01"))%>%
  left_join(censoring_dates%>%select(Study_ID, rand_date))%>%
  filter(rand_date<=last_rand_date_76_prescription,
         !Study_ID %in% withdrawn_participants)%>%
  group_by(month)%>%
  summarise(Records=n(),
            Participants=n_distinct(Study_ID),
            Drugs = n_distinct(MEDICATION_NAME))%>%
  mutate(Extract="79",
         Table="Prescription")%>%
  as_tibble()->counts_prescription_79

data_prescription76%>%
  select(Study_ID, MEDICATION_NAME, date = INITIAL_AUTHORISED_DATE_TIME)%>%
  mutate(date=as.Date(date, format="%Y-%m-%d"))%>%
  mutate(month=paste0(str_sub(date, 1, 8), "01"))%>%
  left_join(censoring_dates%>%select(Study_ID, rand_date))%>%
  filter(rand_date<=last_rand_date_76_prescription,
         !Study_ID %in% withdrawn_participants)%>%
  group_by(month)%>%
  summarise(Records=n(),
            Participants=n_distinct(Study_ID),
            Drugs = n_distinct(MEDICATION_NAME))%>%
  mutate(Extract="76",
         Table="Prescription")%>%
  as_tibble()->counts_prescription_76


data_administration76%>%
  select(Study_ID, MEDICATION_NAME, date = ADMINISTERED_DATE_TIME)%>%
  mutate(date=as.Date(date, format="%Y-%m-%d"))%>%
  mutate(month=paste0(str_sub(date, 1, 8), "01"))%>%
  left_join(censoring_dates%>%select(Study_ID, rand_date))%>%
  filter(rand_date<=last_rand_date_76_prescription,
         !Study_ID %in% withdrawn_participants)%>%
  group_by(month)%>%
  summarise(Records=n(),
            Participants=n_distinct(Study_ID),
            Drugs = n_distinct(MEDICATION_NAME))%>%
  mutate(Extract="76",
         Table="Administration")%>%
  as_tibble()->counts_administration_76

data_administration%>%
  select(Study_ID, MEDICATION_NAME, date = ADMINISTERED_DATE_TIME)%>%
  mutate(date=as.Date(date, format="%Y-%m-%d"))%>%
  mutate(month=paste0(str_sub(date, 1, 8), "01"))%>%
  left_join(censoring_dates%>%select(Study_ID, rand_date))%>%
  filter(rand_date<=last_rand_date_76_prescription,
         !Study_ID %in% withdrawn_participants)%>%
  group_by(month)%>%
  summarise(Records=n(),
            Participants=n_distinct(Study_ID),
            Drugs = n_distinct(MEDICATION_NAME))%>%
  mutate(Extract="79",
         Table="Administration")%>%
  as_tibble()->counts_administration_79


stability_counts_merged<-rbind(counts_administration_76,
                               counts_administration_79,
                               counts_prescription_76,
                               counts_prescription_79)



#### plot -----

stability_counts_merged%>%
  # filter(month>="2018-01-01" & month<="2021-12-01")%>%
  rename("Distinct drug terms"=Drugs)%>%
  pivot_longer(-c(month, Extract, Table), names_to = "key", values_to="value")%>%
  mutate(key=fct_relevel(key, c("Participants", "Records", "Distinct drug terms")),
         Extract=fct_relevel(Extract, c("76", "79")),
         Extract=case_when(Extract == "76" ~ "June 2022",
                           Extract=="79"~ "September 2022"),
         Table=fct_relevel(Table, c("Prescription", "Administration")))%>%  
  mutate(month=as.Date(month))%>%
  filter(month<="2023-01-01")%>%
  ggplot(aes(month, 
             value, 
             fill=Extract, 
             group=Extract,
             shape=Extract))+
  geom_area(position="identity", 
            alpha=0.5, 
            color="black"
  )+
  geom_point(aes(shape=Extract), alpha=0.8, color="black")+
  facet_grid(cols = vars(Table),
             rows=vars(key),
             scales="free",
             labeller = label_wrap_gen(width = 15),
             switch="y")+
  # facet_wrap(Table~key,
  #            scales="free",
  #            ncol=2,
  #            labeller = label_wrap_gen(width = 15),
  #            # switch="y",
  #            # strip.position = "left"
  #            )+
  geom_vline(aes(xintercept=as.Date(extract_79_date)), linetype="dashed", color="#F8766D", size=1)+
  geom_vline(aes(xintercept=as.Date(extract_76_date)), linetype="dashed", color="#619CFF", size=1)+
  labs(x="Month",
       y="Counts",
       title="EPMA data stability across different extracts (split by Prescription vs Administration table)",
       color="Extract",
       shape="Extract",
       size="Extract",
       fill="Extract",
       # caption="Plot restricted to participants randomised on or before the 18/05/2022 (latest randomisation date for participants included in the first extract).
       # Vertical dashed lines placed at the date of receipt of each extract"
  )+
  theme(legend.position = "bottom",
        text = element_text(family="Mulish", size=20),
        plot.caption = element_text(hjust = 0),
        plot.caption.position =  "plot",
        plot.title.position = "plot",
        axis.text.x = element_text(angle = 90,hjust=0,vjust=0.1),
        # panel.grid.minor = element_blank(),
        strip.placement = "outside")+
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y", 
               # limits=c(as.Date("2018-01-01"), NA)
  )+
  scale_shape_manual(breaks=c("June 2022", "September 2022"), values= c(23, 24))+
  scale_y_continuous(limits=c(0, NA))+
  scale_fill_manual(breaks=c("June 2022", "September 2022"), values=c("#619CFF", "#F8766D"))+
  scale_color_manual(breaks=c("June 2022", "September 2022"), values=c("#619CFF",  "#F8766D"))+
  geom_text(.%>%
              filter(Table=="Prescription",
                     key=="Participants")%>%
              slice_head(n=1),
            mapping=aes(x=as.Date(extract_76_date),
                        y=1200,
                        label="28-06-22"),
            color="#619CFF")+
  geom_text(.%>%
              filter(Table=="Prescription",
                     key=="Participants")%>%
              slice_head(n=1),
            mapping=aes(x=as.Date(extract_79_date),
                        y=1400,
                        label="02-09-22"),
            color="#F8766D")

ggsave("Outputs/Figures/Analysis/data_stability.png",
       last_plot(),
       width=20,
       height=10,
       dpi="retina")

# slides


stability_counts_merged%>%
  # filter(month>="2018-01-01" & month<="2021-12-01")%>%
  rename("Distinct drug terms"=Drugs)%>%
  pivot_longer(-c(month, Extract, Table), names_to = "key", values_to="value")%>%
  mutate(key=fct_relevel(key, c("Participants", "Records", "Distinct drug terms")),
         Extract=fct_relevel(Extract, c("76", "79")),
         Extract=case_when(Extract == "76" ~ "June 2022",
                           Extract=="79"~ "September 2022"),
         Table=fct_relevel(Table, c("Prescription", "Administration")))%>%  
  mutate(month=as.Date(month))%>%
  filter(month<="2023-01-01")%>%
  ggplot(aes(month, 
             value, 
             fill=Extract, 
             group=Extract,
             shape=Extract))+
  geom_area(position="identity", 
            alpha=0.5, 
            color="black"
  )+
  geom_point(aes(shape=Extract), alpha=0.8, color="black")+
  facet_grid(cols = vars(Table),
             rows=vars(key),
             scales="free",
             labeller = label_wrap_gen(width = 15),
             switch="y")+
  # facet_wrap(Table~key,
  #            scales="free",
  #            ncol=2,
  #            labeller = label_wrap_gen(width = 15),
  #            # switch="y",
  #            # strip.position = "left"
  #            )+
  geom_vline(aes(xintercept=as.Date(extract_79_date)), linetype="dashed", color="#F8766D", size=1)+
  geom_vline(aes(xintercept=as.Date(extract_76_date)), linetype="dashed", color="#619CFF", size=1)+
  labs(x="Month",
       y="Counts",
       # title="EPMA data stability across different extracts (split by Prescription vs Administration table)",
       color="Extract",
       shape="Extract",
       size="Extract",
       fill="Extract",
       # caption="Plot restricted to participants randomised on or before the 18/05/2022 (latest randomisation date for participants included in the first extract).
       # Vertical dashed lines placed at the date of receipt of each extract"
  )+
  theme(legend.position = "bottom",
        text = element_text(family="Mulish", size=20),
        plot.caption = element_text(hjust = 0),
        plot.caption.position =  "plot",
        plot.title.position = "plot",
        axis.text.x = element_text(angle = 90,hjust=0,vjust=0.1),
        # panel.grid.minor = element_blank(),
        strip.placement = "outside",
        panel.spacing = unit(2, "cm"))+
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y", 
               # limits=c(as.Date("2018-01-01"), NA)
  )+
  scale_shape_manual(breaks=c("June 2022", "September 2022"), values= c(23, 24))+
  scale_y_continuous(limits=c(0, NA))+
  scale_fill_manual(breaks=c("June 2022", "September 2022"), values=c("#619CFF", "#F8766D"))+
  scale_color_manual(breaks=c("June 2022", "September 2022"), values=c("#619CFF",  "#F8766D"))+
  geom_text(.%>%
              filter(Table=="Prescription",
                     key=="Participants")%>%
              slice_head(n=1),
            mapping=aes(x=as.Date(extract_76_date),
                        y=1200,
                        label="28-06-22"),
            color="#619CFF")+
  oxpop_blue_panel+
  geom_text(.%>%
              filter(Table=="Prescription",
                     key=="Participants")%>%
              slice_head(n=1),
            mapping=aes(x=as.Date(extract_79_date),
                        y=1400,
                        label="02-09-22"),
            color="#F8766D")

ggsave("Outputs/Figures/Slides/data_stability.png",
       last_plot(),
       width=20,
       height=10,
       dpi="retina")



# compare additions/deletions/changes

#### participants ----- 
##### prescription ------

data_prescription%>%
  select(Study_ID, MEDICATION_NAME, date = INITIAL_AUTHORISED_DATE_TIME)%>%
  left_join(censoring_dates%>%select(Study_ID, rand_date))%>%
  filter(rand_date<"2022-05-01",
         date<"2022-05-01")%>%
  distinct(Study_ID)%>%
  .[[1]]->individuals_extract_79_prescription

length(individuals_extract_79_prescription) # 4837 people in extract 79 (same dates as extract 76)

data_prescription76%>%
  select(Study_ID, MEDICATION_NAME, date = INITIAL_AUTHORISED_DATE_TIME)%>%
  left_join(censoring_dates%>%select(Study_ID, rand_date))%>%
  filter(rand_date<"2022-05-01",
         date<"2022-05-01")%>%
  distinct(Study_ID)%>%
  .[[1]]->individuals_extract_76_prescription


length(individuals_extract_76_prescription)
# 4837 people in extract 76

setdiff(individuals_extract_76_prescription, individuals_extract_79_prescription)
# no deletions
setdiff(individuals_extract_79_prescription, individuals_extract_76_prescription)
# no additions

##### administration ------

data_administration%>%
  select(Study_ID, MEDICATION_NAME, contains("date"))%>%
  rename(administered_date = ADMINISTERED_DATE_TIME,
         scheduled_date = SCHEDULED_DATE_TIME,
         recorded_date = RECORDED_DATE_TIME,
         last_updated_date = LAST_UPDATED_DATE)%>%
  mutate(date = case_when(is.na(administered_date) ~ scheduled_date,
                          is.na(scheduled_date) ~ recorded_date,
                          is.na(recorded_date) ~ last_updated_date,
                          !is.na(administered_date) ~ administered_date))%>%
  left_join(censoring_dates%>%select(Study_ID, rand_date))%>%
  filter(rand_date<"2022-05-01",
         date<"2022-05-01")%>%  
  distinct(Study_ID)%>%
  .[[1]]->individuals_extract_79_administration

length(individuals_extract_79_administration)
# 4855 people in extract 79 (same dates as extract 76)

data_administration76%>%
  select(Study_ID, MEDICATION_NAME, contains("date"))%>%
  rename(administered_date = ADMINISTERED_DATE_TIME,
         scheduled_date = SCHEDULED_DATE_TIME,
         recorded_date = RECORDED_DATE_TIME,
         last_updated_date = LAST_UPDATED_DATE)%>%
  mutate(date = case_when(is.na(administered_date) ~ scheduled_date,
                          is.na(scheduled_date) ~ recorded_date,
                          is.na(recorded_date) ~ last_updated_date,
                          !is.na(administered_date) ~ administered_date))%>%
  left_join(censoring_dates%>%select(Study_ID, rand_date))%>%
  filter(rand_date<"2022-05-01",
         date<"2022-05-01")%>%  
  distinct(Study_ID)%>%
  .[[1]]->individuals_extract_76_administration


length(individuals_extract_76_administration)
# 4855 people in extract 76

setdiff(individuals_extract_76_administration, individuals_extract_79_administration)
# no deletions
setdiff(individuals_extract_79_administration, individuals_extract_76_administration)
# no additions






#### records ----

##### prescription ------

data_prescription%>%
  select(Study_ID, PRESCRIPTION_ID_HASHED, MEDICATION_NAME, date = INITIAL_AUTHORISED_DATE_TIME)%>%
  left_join(censoring_dates%>%select(Study_ID, rand_date))%>%
  filter(rand_date<"2022-05-01",
         date<"2022-05-01")->records_prescription_79

nrow(records_prescription_79) # 384855 records in extract 79 (same dates as extract 76)
records_prescription_79%>%distinct(PRESCRIPTION_ID_HASHED)%>%nrow() # 384189 distinct prescription IDs

data_prescription76%>%
  select(Study_ID, PRESCRIPTION_ID_HASHED, MEDICATION_NAME, date = INITIAL_AUTHORISED_DATE_TIME)%>%
  left_join(censoring_dates%>%select(Study_ID, rand_date))%>%
  filter(rand_date<"2022-05-01",
         date<"2022-05-01")->records_prescription_76

nrow(records_prescription_76) # 384855 records in extract 79 (same dates as extract 76)
records_prescription_76%>%distinct(PRESCRIPTION_ID_HASHED)%>%nrow() # 384189 distinct prescription IDs

records_prescription_76%>%
  rename(MEDICATION_NAME_76 = MEDICATION_NAME,
         date_76 = date)%>%
  full_join(records_prescription_79%>%
              rename(MEDICATION_NAME_79 = MEDICATION_NAME,
                     date_79 = date))%>%
  mutate(across(contains("date"),  ~as.Date(., format="%Y-%m-%d")))%>%
  group_by(PRESCRIPTION_ID_HASHED)%>%
  summarise(different_drug=if_else(MEDICATION_NAME_79!=MEDICATION_NAME_76, "Y", "N"),
            different_date = if_else(date_76!=date_79, "Y", "N"),
            datediff=difftime(date_79, date_76))%>%
  distinct(PRESCRIPTION_ID_HASHED, different_drug, different_date, .keep_all = T)%>%
  ungroup()%>%View()


setdiff(records_prescription_79$PRESCRIPTION_ID_HASHED, records_prescription_76$PRESCRIPTION_ID_HASHED)


setdiff(records_prescription_76$PRESCRIPTION_ID_HASHED, records_prescription_79$PRESCRIPTION_ID_HASHED)











##### administration -------


data_administration%>%
  select(Study_ID, ADMINISTRATION_ID_HASHED, MEDICATION_NAME, contains("date"))%>%
  left_join(censoring_dates%>%select(Study_ID, rand_date))%>%
  filter(rand_date<"2022-05-01",
         SCHEDULED_DATE_TIME <"2022-05-01" |
           ADMINISTERED_DATE_TIME <"2022-05-01" |
           RECORDED_DATE_TIME<"2022-05-01"|
           LAST_UPDATED_DATE <"2022-05-01")->records_administration_79

nrow(records_administration_79) # 2238777 records in extract 79 (same dates as extract 76)
records_administration_79%>%distinct(ADMINISTRATION_ID_HASHED)%>%nrow() # 2238630 distinct prescription IDs

data_administration76%>%
  select(Study_ID, ADMINISTRATION_ID_HASHED, MEDICATION_NAME, contains("date"))%>%
  left_join(censoring_dates%>%select(Study_ID, rand_date))%>%
  filter(rand_date<"2022-05-01",
         SCHEDULED_DATE_TIME <"2022-05-01" |
           ADMINISTERED_DATE_TIME <"2022-05-01" |
           RECORDED_DATE_TIME<"2022-05-01"|
           LAST_UPDATED_DATE <"2022-05-01")->records_administration_76

nrow(records_administration_76) # 2238176 records in extract 79 (same dates as extract 76)
records_administration_76%>%distinct(ADMINISTRATION_ID_HASHED)%>%nrow() # 2238029 distinct prescription IDs

records_administration_76%>%
  rename(MEDICATION_NAME_76 = MEDICATION_NAME,
         administered_date_76 = ADMINISTERED_DATE_TIME,
         scheduled_date_76 = SCHEDULED_DATE_TIME,
         recorded_date_76 = RECORDED_DATE_TIME,
         last_updated_date_76 = LAST_UPDATED_DATE)%>%
  full_join(records_administration_79%>%
              rename(MEDICATION_NAME_79 = MEDICATION_NAME,
                     administered_date_79 = ADMINISTERED_DATE_TIME,
                     scheduled_date_79 = SCHEDULED_DATE_TIME,
                     recorded_date_79 = RECORDED_DATE_TIME,
                     last_updated_date_79 = LAST_UPDATED_DATE
              ),
            by=c("ADMINISTRATION_ID_HASHED"))%>%
  mutate(across(contains("date"),  ~as.Date(., format="%Y-%m-%d")))%>%
  group_by(ADMINISTRATION_ID_HASHED)%>%
  summarise(different_drug=if_else(MEDICATION_NAME_79!=MEDICATION_NAME_76, "Y", "N"),
            different_date_administered = if_else(administered_date_76!=administered_date_79, "Y", "N"),
            different_date_scheduled = if_else(scheduled_date_76!=scheduled_date_79, "Y", "N"),
            different_date_recorded = if_else(recorded_date_76!=recorded_date_79, "Y", "N"),
            different_date_last_updated = if_else(last_updated_date_76!=last_updated_date_79, "Y", "N"))%>%
  distinct(PRESCRIPTION_ID_HASHED, different_drug, different_date_administered,different_date_scheduled, different_date_recorded,different_date_last_updated,.keep_all = T)%>%
  ungroup()%>%View() # 3 different records



length(setdiff(records_administration_79%>%distinct(ADMINISTRATION_ID_HASHED)%>%.[[1]], records_administration_76%>%distinct(ADMINISTRATION_ID_HASHED)%>%.[[1]]))


setdiff(records_administration_76%>%distinct(ADMINISTRATION_ID_HASHED)%>%.[[1]], records_administration_79%>%distinct(ADMINISTRATION_ID_HASHED)%>%.[[1]])














#### drug terms -----

##### prescription -----

records_prescription_76%>%
  distinct(MEDICATION_NAME)%>%
  nrow() # 13027

records_prescription_79%>%
  distinct(MEDICATION_NAME)%>%
  nrow() # 13027


setdiff(
  records_prescription_76%>%
    distinct(MEDICATION_NAME)%>%
    .[[1]],
  records_prescription_79%>%
    distinct(MEDICATION_NAME)%>%
    .[[1]]) # 0 deletions


setdiff(
  records_prescription_79%>%
    distinct(MEDICATION_NAME)%>%
    .[[1]],
  records_prescription_76%>%
    distinct(MEDICATION_NAME)%>%
    .[[1]]) # 0 additions

##### administration ------

records_administration_76%>%
  distinct(MEDICATION_NAME)%>%
  nrow() # 11898

records_administration_79%>%
  distinct(MEDICATION_NAME)%>%
  nrow() # 11899


setdiff(
  records_administration_76%>%
    distinct(MEDICATION_NAME)%>%
    .[[1]],
  records_administration_79%>%
    distinct(MEDICATION_NAME)%>%
    .[[1]]) # 0 deletions


setdiff(
  records_administration_79%>%
    distinct(MEDICATION_NAME)%>%
    .[[1]],
  records_administration_76%>%
    distinct(MEDICATION_NAME)%>%
    .[[1]]) # 1 additions




#### investigate outstanding discrepancies --------

records_prescription_76%>%
  mutate(extract=76)%>%
  rbind(
    records_prescription_79%>%
      mutate(extract=79))%>%
  mutate(month=as.Date(paste0(strftime(date, "%Y-%m"), "-01"),
                       format="%Y-%m-%d"))%>%
  filter(month=="2020-04-01" | month=="2021-01-01")%>%
  group_by(extract, month)%>%
  summarise(participants=n_distinct(Study_ID),
            records = n())%>%
  View()
  

### Date fields timeseries ---------


data_prescription%>%
  select(REPORTED_DATE_TIME, INITIAL_AUTHORISED_DATE_TIME)%>%
  mutate(`Reported date` = as.Date(REPORTED_DATE_TIME, format=c("%Y-%m-%d")),
         `Record date`= as.Date(INITIAL_AUTHORISED_DATE_TIME, format=c("%Y-%m-%d")))%>%
  pivot_longer(c(`Reported date`, `Record date`), names_to="Field", values_to="Date")%>%
  mutate(Table = "Prescription table")%>%
  select(Field, Date, Table)%>%
  
  rbind(
    data_administration%>%
      select(REPORTED_DATE_TIME, ADMINISTERED_DATE_TIME)%>%
      mutate(`Reported date` = as.Date(REPORTED_DATE_TIME, format=c("%Y-%m-%d")),
             `Record date`= as.Date(ADMINISTERED_DATE_TIME, format=c("%Y-%m-%d")))%>%
      pivot_longer(c(`Reported date`, `Record date`), names_to="Field", values_to="Date")%>%
      mutate(Table = "Administration table")%>%
      select(Field, Date, Table))%>%
  
  
  mutate(week = floor_date(Date, "week"))%>%
  filter(Date<=as.Date("2023-01-01"))%>%
  group_by(week, Table, Field)%>%
  # mutate(month=paste0(str_sub(REPORTED_DATE_TIME, 1, 8), "01"))%>%
  # mutate(month=as.Date(month, format=c("%Y-%m-%d")))%>%
  count(week)%>%
  
  ggplot(aes(week, n, group=1))+
  geom_point()+
  geom_line()+
  facet_grid(Table~Field, scales="free",
             switch="y")+
  labs(x="Date (at weekly level)",
       y="Count",
       # title="Timeseries of number of individual records for each reported date (prescription table)",
       # subtitle="Aggregated at weekly level"
       )+
  scale_x_date(date_labels = "%Y %b",
               date_breaks = "2 months")+
  theme(axis.text.x=element_text(angle=30,
                                 hjust=1),
        panel.grid.minor = element_blank(),
        text=element_text(size=20))+
  scale_y_continuous(limits=c(0, NA))
  # annotate(geom="text",
  #          #label="Extract reception date: 02/09/22",
  #          y=20000,
  #          x=as.Date("2022-09-02"))



ggsave("Outputs/Figures/Analysis/date_fields_timeseries.png",
       last_plot(),
       width=60,
       height=30,
       units = "cm",
       dpi="retina")





#### restrict derived flags to the study population (after right censoring) --------

pre_rand_drugs_crf%<>%
  filter(Study_ID %in% study_population)

crf_post_rand_drug_flags%<>%
  filter(Study_ID %in% study_population)

epma_drug_flags%<>%
  filter(Study_ID %in% study_population)



censoring_dates%<>%
  filter(rand_date<"2022-06-01")











## 3. Sensitivity analyses -------


pre_rand_drugs_crf

pre_rand_drugs_crf%>%
  distinct(Drug)

epma_drug_flags%>%
  distinct(Drug)





###  administration vs prescription tables ------

#### calculations -----

#### EPMA flags

baseline_epma_flags<-
  epma_drug_flags%>%
  filter(Drug %in% 
           (pre_rand_drugs_crf%>%
              distinct(Drug)%>%.[[1]]))%>%
  left_join(censoring_dates%>%mutate(Study_ID=as.character(Study_ID)))%>%
  filter(!is.na(date))%>%
  group_by(Study_ID)%>%
  filter(date>=admission_date,
         date<=right_censoring_date_death_discharge_28d)%>% # restrict to people who have had a record within their randomisation admission  group_by(Study_ID, Drug, Table)%>%
  group_by(Study_ID, Drug, Table)%>%
  summarise(EPMA=if_else(any(Flag=="1" & date<=rand_date & date>=admission_date), 1, 0))%>% # positive flag if record occurs before randomisation but after the current date of admission (therefore in current admission)
  ungroup()

# fill with 0s for people included in comparison but with no computable flags from data
baseline_epma_flags_by_table<-
  baseline_epma_flags%>%
  filter(Table=="Administration")%>%
  right_join(
    expand.grid(Study_ID=intersect(study_population, participants_with_data_in_randomisation_admission_administration),
                Table="Administration",
                Drug=baseline_epma_flags%>%distinct(Drug)%>%.[[1]])
  )%>%
  mutate(EPMA=replace_na(EPMA, 0))%>%
  rbind(
    baseline_epma_flags%>%
      filter(Table=="Prescription")%>%
      right_join(
        expand.grid(Study_ID=intersect(study_population, participants_with_data_in_randomisation_admission_prescription),
                    Table="Prescription",
                    Drug=baseline_epma_flags%>%distinct(Drug)%>%.[[1]])
      )%>%
      mutate(EPMA=replace_na(EPMA, 0)))%>%
  rbind(
    baseline_epma_flags%>%
      select(-Table)%>%
      right_join(
        expand.grid(Study_ID=study_population,
                    Drug=baseline_epma_flags%>%distinct(Drug)%>%.[[1]])
      )%>%
      group_by(Study_ID, Drug)%>%
      summarise(EPMA=if_else(any(EPMA==1), 1, 0))%>%
      ungroup()%>%
      mutate(Table="Any"))


#### Join flags from both sources and prepare for calculations

baseline_combined_flags_sensitivity_tables<-pre_rand_drugs_crf%>%
  rename(CRF=Flag)%>%
  filter(Study_ID%in%baseline_epma_flags_by_table$Study_ID)%>%
  left_join(baseline_epma_flags_by_table)%>%
  left_join(baseline_crf_england%>%select(Study_ID, resp_status))%>%
  select(Study_ID, Drug, EPMA, CRF, Table, resp_status)%>%
  mutate(EPMA=replace_na(EPMA, 0))%>%
  select(-resp_status)%>%
  group_by(Study_ID, Drug, Table)%>%
  summarise(EPMA=if_else(any(EPMA==1), 1, 0),
            CRF=if_else(any(CRF==1), 1, 0))%>%
  ungroup()


#### Calculate agreement --------

kappa_baseline_sensitivity_analysis_tables<-list()

drugs<-baseline_combined_flags_sensitivity_tables%>%
  distinct(Drug)%>%
  # filter(Drug!="Oxygen")%>%
  .[[1]]

tables<-baseline_combined_flags_sensitivity_tables%>%distinct(Table)%>%.[[1]]

for (t in tables) {
  
  for (i in drugs) {
    
    baseline_combined_flags_sensitivity_tables%>%
      filter(Drug==i,
             Table==t)%>%
      select(EPMA, CRF)%>%
      mutate(across(everything(), as.integer))%>%
      mutate(across(c(EPMA, CRF), ~replace_na(.x, 0)))->table
    
    matrix<-as.matrix(table(table))
    
    
    # try(k<-Kappa.test(table$EPMA, table$CRF))
    
    try(k<-epi.kappa(matrix, method="cohen", alternative = "greater", conf.level=0.95))
    
    if(exists("k"))
    {
      
      kappa_baseline_sensitivity_analysis_tables[[t]][[i]]<-k
      
      # kappa_baseline_sensitivity_analysis_tables[[t]][[i]]<-append(kappa_baseline_sensitivity_analysis_tables[[t]][[i]], pabak)
      
      
    }
    else{
      kappa_baseline_sensitivity_analysis_tables[[t]][[i]]<-list()
    }
    
    
    
    rm(i, table, k, matrix
       #,  pabak
    )
    
  }
  rm(t)
}



# View(kappa_baseline_sensitivity_analysis_tables)    



##### Unlist results for table


kappa_baseline_sensitivity_analysis_tables_results<-data.frame()

drugs<-baseline_combined_flags_sensitivity_tables%>%distinct(Drug)%>%.[[1]]



for (t in tables) {
  
  for (i in drugs) {
    
    Table<-t
    
    Drug<-i
    
    try(
      table<-(data.frame(
        Table = Table,
        Drug = Drug,
        kappa = unlist(kappa_baseline_sensitivity_analysis_tables[[t]][[i]]$kappa$est),
        se = unlist(kappa_baseline_sensitivity_analysis_tables[[t]][[i]]$kappa$se),
        kappa_CI_lower = unlist(kappa_baseline_sensitivity_analysis_tables[[t]][[i]]$kappa$lower),
        kappa_CI_upper = unlist(kappa_baseline_sensitivity_analysis_tables[[t]][[i]]$kappa$upper),
        
        pabak = unlist(kappa_baseline_sensitivity_analysis_tables[[t]][[i]]$pabak$est),
        pabak_CI_lower = unlist(kappa_baseline_sensitivity_analysis_tables[[t]][[i]]$pabak$lower),
        pabak_CI_upper = unlist(kappa_baseline_sensitivity_analysis_tables[[t]][[i]]$pabak$upper),
        
        p_value = unlist(kappa_baseline_sensitivity_analysis_tables[[t]][[i]]$z$p.value)
      ))
    )
    
    try(kappa_baseline_sensitivity_analysis_tables_results%<>%bind_rows(table))
    rm(table, i)
  }
  rm(t, Table)
}


# View(kappa_baseline_sensitivity_analysis_tables_results)  



#### aggregate summary (all drugs) ------


for (t in tables) {
  
  baseline_combined_flags_sensitivity_tables%>%
    filter(Table==t)%>%
    select(EPMA, CRF)%>%
    mutate(across(everything(), as.integer))->table
  
  matrix<-as.matrix(table(table))
  
  
  try(k<-epi.kappa(matrix, method="cohen", alternative = "greater", conf.level=0.95))
  
  if(exists("k"))
  {
    
    kappa_baseline_sensitivity_analysis_tables[[t]][["Combined"]]<-k
    
  }
  else{
    kappa_baseline_sensitivity_analysis_tables[[t]][["Combined"]]<-list()
  }
  
  rm(t, table, k, matrix)
  
}

# View(kappa_baseline_sensitivity_analysis_tables)

# unlist results in table

for (t in tables){
  
  try(
    table<-(data.frame(
      Table = t,
      Drug = "Combined",
      kappa = unlist(kappa_baseline_sensitivity_analysis_tables[[t]][["Combined"]]$kappa$est),
      se = unlist(kappa_baseline_sensitivity_analysis_tables[[t]][["Combined"]]$kappa$se),
      kappa_CI_lower = unlist(kappa_baseline_sensitivity_analysis_tables[[t]][["Combined"]]$kappa$lower),
      kappa_CI_upper = unlist(kappa_baseline_sensitivity_analysis_tables[[t]][["Combined"]]$kappa$upper),
      
      pabak = unlist(kappa_baseline_sensitivity_analysis_tables[[t]][["Combined"]]$pabak$est),
      pabak_CI_lower = unlist(kappa_baseline_sensitivity_analysis_tables[[t]][["Combined"]]$pabak$lower),
      pabak_CI_upper = unlist(kappa_baseline_sensitivity_analysis_tables[[t]][["Combined"]]$pabak$upper),
      
      p_value = unlist(kappa_baseline_sensitivity_analysis_tables[[t]][["Combined"]]$z$p.value)
    ))
  )
  
  try(kappa_baseline_sensitivity_analysis_tables_results%<>%bind_rows(table))
  
}

rm(Drug)
# View(kappa_baseline_sensitivity_analysis_tables_results)


kappa_baseline_sensitivity_analysis_tables_results%<>%
  arrange(Drug)%>%
  mutate(Judgement = case_when(kappa<0~"No agreement",
                               kappa>= 0 & kappa <0.2 ~ "Slight agreement",
                               kappa >=0.2 & kappa<0.4 ~ "Fair agreement",
                               kappa>=0.4 & kappa <0.6 ~"Moderate agreement",
                               kappa>=0.6 & kappa<0.8 ~ "Substantial agreement",
                               kappa>=0.8 ~ "Almost perfect agreement"))


#### counts ------
baseline_combined_flags_sensitivity_tables%>%
  group_by(Table, Drug)%>%
  summarise(pairs = n_distinct(Study_ID),
            positives = length(CRF[CRF=="1"]),
            negatives = length(CRF[CRF=="0"]),
            true_positives = length(CRF[CRF=="1" & EPMA=="1"]),
            true_negatives = length(CRF[CRF=="0" & EPMA=="0"]),
            false_positives = length(CRF[CRF=="0" & EPMA =="1"]),
            false_negatives = length(CRF[CRF =="1" & EPMA == "0"]),
            Sensitivity = round(true_positives/positives*100, 1),
            Specificity = round(true_negatives/negatives*100, 1),
            PPV = round(true_positives/(true_positives+false_positives)*100, 1),
            NPV = round(true_negatives/(true_negatives+false_negatives)*100, 1),
            Both = sum(EPMA=="1" & CRF=="1"),
            CRF_only = sum(EPMA=="0" & CRF=="1"),
            EPMA_only = sum(EPMA=="1" & CRF=="0"),
            Neither = sum(EPMA=="0" & CRF=="0"))%>%
  rbind(
    
    
    baseline_combined_flags_sensitivity_tables%>%
      group_by(Table)%>%
      summarise(pairs = n(),
                positives = length(CRF[CRF=="1"]),
                negatives = length(CRF[CRF=="0"]),
                true_positives = length(CRF[CRF=="1" & EPMA=="1"]),
                true_negatives = length(CRF[CRF=="0" & EPMA=="0"]),
                false_positives = length(CRF[CRF=="0" & EPMA =="1"]),
                false_negatives = length(CRF[CRF =="1" & EPMA == "0"]),
                Sensitivity = round(true_positives/positives*100, 1),
                Specificity = round(true_negatives/negatives*100, 1),
                PPV = round(true_positives/(true_positives+false_positives)*100, 1),
                NPV = round(true_negatives/(true_negatives+false_negatives)*100, 1),
                Both = sum(EPMA=="1" & CRF=="1"),
                CRF_only = sum(EPMA=="0" & CRF=="1"),
                EPMA_only = sum(EPMA=="1" & CRF=="0"),
                Neither = sum(EPMA=="0" & CRF=="0"))%>%
      mutate(Drug="Combined"))%>%
  
  
  select(-c(positives, negatives, true_positives, true_negatives, false_positives, false_negatives))%>%
  rename(Individuals=pairs)->baseline_drugs_participant_counts_sensitivity_tables

# View(baseline_drugs_participant_counts_sensitivity_tables)

#### Forest plot with counts -----
kappa_baseline_sensitivity_analysis_tables_results%<>%
  left_join(baseline_drugs_participant_counts_sensitivity_tables,
            by=c("Table", "Drug"))%>%
  mutate(Drug=fct_relevel(Drug, 
                          "Corticosteroids",
                          "Macrolides",
                          "Tocilizumab or sarilumab",
                          "Remdesivir",
                          "Antiplatelets",
                          "Oral anticoagulants",
                          "LMWH",
                          "Oxygen",
                          "Combined"
  ))%>%
  
  arrange(Drug)

kappa_baseline_sensitivity_analysis_tables_results%>%
  mutate(Table=factor(Table, levels=c("Prescription", "Administration", "Any")))%>%
  arrange(Drug, Table)%>%
  mutate(`Kappa (95% CI)` = paste0(round(kappa,2), " (", round(kappa_CI_lower, 2), " to ", round(kappa_CI_upper, 2), ")"))%>%
  mutate(`PABAK (95% CI)` = paste0(round(pabak,2), " (", round(pabak_CI_lower, 2), " to ", round(pabak_CI_upper, 2), ")"))%>%
  select(Drug, Table, Individuals, Both, CRF_only, EPMA_only, Neither, Sensitivity, Specificity, PPV, NPV, `Kappa (95% CI)`, `PABAK (95% CI)`, kappa, se, kappa_CI_lower, kappa_CI_upper, pabak, pabak_CI_lower, pabak_CI_upper)%>%
  add_row(.before=1)%>%
  add_row(.before=5)%>%
  add_row(.before=9)%>%
  add_row(.before=13)%>%
  add_row(.before=17)%>%
  add_row(.before=21)%>%
  add_row(.before=25)%>%
  add_row(.before=29)%>%
  add_row(.before=33)%>%
  rename(Total=Individuals,
         `CRF only` = CRF_only,
         `Both sources` = Both,
         `EPMA only` = EPMA_only,
         `Neither source` = Neither)%>%
  add_column(`Kappa`=NA, .after = "NPV")%>%
  add_column(`PABAK`=NA, .after = "Kappa (95% CI)")%>%
  mutate(`Kappa`=paste(rep(" ", 20), collapse = " "))%>%
  mutate(`PABAK`=paste(rep(" ", 20), collapse = " "))%>%
  mutate(Size=Total)%>%
  mutate(Drug=rep(kappa_baseline_sensitivity_analysis_tables_results%>%distinct(Drug)%>%arrange(Drug)%>%.[[1]],each=4))%>%
  # mutate(Drug=as.factor(Drug))%>%
  # mutate(Table=ifelse(is.na(Table), Drug, Table))%>%View()
  # select(-Drug)%>%View()
  mutate(Drug=as.character(Drug))%>%
  mutate(Table=if_else(Table%in%c("Administration", "Prescription", "Any"), paste("   ", Table), Drug))%>%
  rename(`Drug (by dataset table used)`=Table)%>%
  select(-Drug)%>%
  mutate(across(-c(Size, se), ~ifelse(is.na(.), " ",.)))%>%
  mutate(across(c(kappa_CI_lower, kappa_CI_upper, kappa, pabak, pabak_CI_lower, pabak_CI_upper), ~as.numeric(.)))->forest_plot_table_sensitivity_tables



forest_plot_table_sensitivity_tables%>%
  slice_tail(n=3)%>%
  rename(`Data table`=1)->forest_plot_table_sensitivity_tables_combined

forest_plot_table_sensitivity_tables%>%
  slice_head(n=-4)->forest_plot_table_sensitivity_tables_by_drug

##### combined ------


forest_plot_sensitivity_tables_combined<-forest(forest_plot_table_sensitivity_tables_combined[,1:14],
                                       est=list(forest_plot_table_sensitivity_tables_combined$kappa,
                                                forest_plot_table_sensitivity_tables_combined$pabak),
                                       lower=list(forest_plot_table_sensitivity_tables_combined$kappa_CI_lower,
                                                  forest_plot_table_sensitivity_tables_combined$pabak_CI_lower),
                                       upper=list(forest_plot_table_sensitivity_tables_combined$kappa_CI_upper,
                                                  forest_plot_table_sensitivity_tables_combined$pabak_CI_upper),
                                       sizes=list(forest_plot_table_sensitivity_tables_combined$se^2,
                                                  forest_plot_table_sensitivity_tables_combined$se^2),
                                       ci_column = c(11,13),
                                       xlim=c(-1,1),
                                       arrow_lab = c("Negative agreement", "Positive agreement"),
                                       ref_line = 0)%>%
  insert_text(.,
              text = "Counts (participant/drug pairs)",
              col = 2:6,
              part = "header",
              gp = gpar(fontface = "bold"))%>%
  insert_text(.,
              text = "Agreement metrics",
              col = 7:13,
              part = "header",
              gp = gpar(fontface = "bold"))%>%
  gtable_add_grob(grobs=segmentsGrob(
    x0 = unit(0,"npc"), # horizontal origin
    y0 = unit(0,"npc"), # vertical origin
    x1 = unit(0,"npc"), # horizontal end
    y1 = unit(1,"npc"), # vertical end
    gp = gpar(lwd = 2.0)), # width
    t = 4,   # top end
    b = 7,  # bottom end
    l = 8,  # left end
    r = 8 # right end
  )

forest_plot_sensitivity_tables_combined

ggsave(filename="Outputs/Figures/Analysis/forestplot_sensitivity_tables_baseline_combined.png", 
       plot=forest_plot_sensitivity_tables_combined,
       dpi = "retina",
       units = "cm",
       width = get_wh(plot=forest_plot_sensitivity_tables_combined, unit="cm"),
       limitsize = F
       # height = 30
)



#### alternative table format 

sect_properties <- prop_section(
  page_size = page_size(orient = "landscape",
                        width = 15, 
                        #height = 11.7
  ),
  type = "continuous",
  page_margins = page_mar())

forest_plot_table_sensitivity_tables_combined%>%
  select(`Data table`,
         Total,
         `Both sources`,
         `CRF only`,
         `EPMA only`,
         `Neither source`,
         Sn=Sensitivity,
         Sp=Specificity,
         PPV,
         NPV,
         `Kappa (95% CI)`,
         `PABAK (95% CI)`)%>%
  flextable()%>%
  add_header_row(values=c("", "Participants/drug pairs in each source (%)", "Agreement metrics"), colwidths = c(1, 5, 6))%>%
  save_as_docx(path="Outputs/Tables/sensitivity_tables_baseline_combined.docx",
               pr_section = sect_properties)


##### by drug ------

forest_plot_sensitivity_tables_by_drug<-forest(forest_plot_table_sensitivity_tables_by_drug[,1:14],
                                       est=list(forest_plot_table_sensitivity_tables_by_drug$kappa,
                                                forest_plot_table_sensitivity_tables_by_drug$pabak),
                                       lower=list(forest_plot_table_sensitivity_tables_by_drug$kappa_CI_lower,
                                                  forest_plot_table_sensitivity_tables_by_drug$pabak_CI_lower),
                                       upper=list(forest_plot_table_sensitivity_tables_by_drug$kappa_CI_upper,
                                                  forest_plot_table_sensitivity_tables_by_drug$pabak_CI_upper),
                                       sizes=list(forest_plot_table_sensitivity_tables_by_drug$se^2,
                                                  forest_plot_table_sensitivity_tables_by_drug$se^2),
                                       ci_column = c(11,13),
                                       xlim=c(-1,1),
                                       arrow_lab = c("Negative agreement", "Positive agreement"),
                                       ref_line = 0)%>%
  insert_text(.,
              text = "Counts (participants)",
              col = 2:6,
              part = "header",
              gp = gpar(fontface = "bold"))%>%
  insert_text(.,
              text = "Agreement metrics",
              col = 7:13,
              part = "header",
              gp = gpar(fontface = "bold"))%>%
  gtable_add_grob(grobs=segmentsGrob(
    x0 = unit(0,"npc"), # horizontal origin
    y0 = unit(0,"npc"), # vertical origin
    x1 = unit(0,"npc"), # horizontal end
    y1 = unit(1,"npc"), # vertical end
    gp = gpar(lwd = 2.0)), # width
    t = 4,   # top end
    b = 36,  # bottom end
    l = 8,  # left end
    r = 8 # right end
  )

forest_plot_sensitivity_tables_by_drug

ggsave(filename="Outputs/Figures/Analysis/forestplot_sensitivity_tables_baseline_by_drug.png", 
       plot=forest_plot_sensitivity_tables_by_drug,
       dpi = "retina",
       units = "cm",
       width = get_wh(plot=forest_plot_sensitivity_tables_by_drug, unit="cm"),
       limitsize = F
       # height = 30
)


#### alternative table format 
sect_properties <- prop_section(
  page_size = page_size(orient = "landscape",
                        width = 15, 
                        #height = 11.7
  ),
  type = "continuous",
  page_margins = page_mar())

forest_plot_table_sensitivity_tables_by_drug%>%
  select(Drug,
         Total,
         `Both sources`,
         `CRF only`,
         `EPMA only`,
         `Neither source`,
         Sn=Sensitivity,
         Sp=Specificity,
         PPV,
         NPV,
         `Kappa (95% CI)`,
         `PABAK (95% CI)`)%>%
  flextable()%>%
  add_header_row(values=c("", "Participants/drug pairs in each source (%)", "Agreement metrics"), colwidths = c(1, 5, 6))%>%
  save_as_docx(path="Outputs/Tables/sensitivity_tables_baseline_combined.docx",
               pr_section = sect_properties)


###  type (inpatient/etc) ------


#### calculations -------

baseline_epma_flags_type<-
  epma_drug_flags%>%
  filter(Drug %in% 
           (pre_rand_drugs_crf%>%
              distinct(Drug)%>%.[[1]]))%>%
  left_join(censoring_dates%>%mutate(Study_ID=as.character(Study_ID)))%>%
  filter(!is.na(date))%>%
  group_by(Study_ID)%>%
  filter(date>=admission_date,
         date<=right_censoring_date_death_discharge_28d)%>% # restrict to people who have had a record within their randomisation admission  mutate(STATUS=replace_na(STATUS, "Unknown"))%>%
  group_by(Study_ID, Drug, TYPE)%>%
  summarise(EPMA=if_else(any(Flag=="1" & date<=rand_date & date>=admission_date), 1, 0))%>% # positive flag if record occurs before randomisation but after the current date of admission (therefore in current admission)
  ungroup()

# summarise flags
baseline_flags_sensitivity_type<-
  baseline_epma_flags_type%>%
  filter(TYPE=="I")%>%
  mutate(TYPE="Inpatient")%>%
  
  rbind(baseline_epma_flags_type%>%
          mutate(TYPE="Any"))%>%
  mutate(EPMA=replace_na(EPMA, 0))%>%
  rename(Type=TYPE)%>%
  group_by(Study_ID, Drug, Type)%>%
  ungroup()%>%
  right_join(
    expand.grid(Study_ID=study_population,
                Type=c("Inpatient", "Any"),
                Drug=baseline_epma_flags_type%>%distinct(Drug)%>%.[[1]])
  )%>%
  mutate(EPMA=replace_na(EPMA, 0))



#### Join flags from both sources and prepare for calculations

baseline_combined_flags_sensitivity_type<-pre_rand_drugs_crf%>%
  rename(CRF=Flag)%>%
  filter(Study_ID%in%baseline_flags_sensitivity_type$Study_ID)%>%
  left_join(baseline_flags_sensitivity_type)%>%
  select(Study_ID, Drug, EPMA, CRF, Type)%>%
  mutate(EPMA=replace_na(EPMA, 0))%>%
  group_by(Study_ID, Drug, Type)%>%
  summarise(EPMA=if_else(any(EPMA==1), 1, 0),
            CRF=if_else(any(CRF==1), 1, 0))%>%
  ungroup()



#### Calculate agreement --------


kappa_baseline_sensitivity_type<-list()

drugs<-baseline_combined_flags_sensitivity_type%>%
  distinct(Drug)%>%
  .[[1]]

types<-c("Inpatient", "Any")


for (i in drugs){  
  
  for(s in types){
    
    baseline_combined_flags_sensitivity_type%>%
      filter(Drug==i,
             Type==s)%>%
      select(EPMA, CRF)%>%
      mutate(across(everything(), as.integer))->table
    
    matrix<-as.matrix(table(table))
    
    
    try(k<-epi.kappa(matrix, method="cohen", alternative = "greater", conf.level=0.95))
    
    if(exists("k"))
    {
      
      kappa_baseline_sensitivity_type[[i]][[s]]<-k
      
    }
    else{
      kappa_baseline_sensitivity_type[[i]][[s]]<-list()
    }
    
    rm(s, table, k, matrix)
    
  }
  rm(i)
}

View(kappa_baseline_sensitivity_type)

# unlist results in table


kappa_baseline_sensitivity_type_results<-data.frame()

for (i in drugs) {
  
  for (s in types){
    
    try(
      table<-(data.frame(
        Drug = i,
        Type = s,
        kappa = unlist(kappa_baseline_sensitivity_type[[i]][[s]]$kappa$est),
        se = unlist(kappa_baseline_sensitivity_type[[i]][[s]]$kappa$se),
        kappa_CI_lower = unlist(kappa_baseline_sensitivity_type[[i]][[s]]$kappa$lower),
        kappa_CI_upper = unlist(kappa_baseline_sensitivity_type[[i]][[s]]$kappa$upper),
        
        pabak = unlist(kappa_baseline_sensitivity_type[[i]][[s]]$pabak$est),
        pabak_CI_lower = unlist(kappa_baseline_sensitivity_type[[i]][[s]]$pabak$lower),
        pabak_CI_upper = unlist(kappa_baseline_sensitivity_type[[i]][[s]]$pabak$upper),
        
        p_value = unlist(kappa_baseline_sensitivity_type[[i]][[s]]$z$p.value)
      ))
    )
    
    try(kappa_baseline_sensitivity_type_results%<>%bind_rows(table))
    rm(Status, s, table)
  }
  rm(Drug, i)
}


# View(kappa_baseline_sensitivity_type_results)  


#### aggregate summary (all drugs) ------


for(s in types){
  
  baseline_combined_flags_sensitivity_type%>%
    filter(Type==s)%>%
    select(EPMA, CRF)%>%
    mutate(across(everything(), as.integer))->table
  
  matrix<-as.matrix(table(table))
  
  
  try(k<-epi.kappa(matrix, method="cohen", alternative = "greater", conf.level=0.95))
  
  if(exists("k"))
  {
    
    kappa_baseline_sensitivity_type[["Combined"]][[s]]<-k
    
  }
  else{
    kappa_baseline_sensitivity_type[["Combined"]][[s]]<-list()
  }
  
  rm(s, table, k, matrix)
  
}

View(kappa_baseline_sensitivity_type)

# unlist results in table

for (s in types){
  
  try(
    table<-(data.frame(
      Drug = "Combined",
      Type = s,
      kappa = unlist(kappa_baseline_sensitivity_type[["Combined"]][[s]]$kappa$est),
      se = unlist(kappa_baseline_sensitivity_type[["Combined"]][[s]]$kappa$se),
      kappa_CI_lower = unlist(kappa_baseline_sensitivity_type[["Combined"]][[s]]$kappa$lower),
      kappa_CI_upper = unlist(kappa_baseline_sensitivity_type[["Combined"]][[s]]$kappa$upper),
      
      pabak = unlist(kappa_baseline_sensitivity_type[["Combined"]][[s]]$pabak$est),
      pabak_CI_lower = unlist(kappa_baseline_sensitivity_type[["Combined"]][[s]]$pabak$lower),
      pabak_CI_upper = unlist(kappa_baseline_sensitivity_type[["Combined"]][[s]]$pabak$upper),
      
      p_value = unlist(kappa_baseline_sensitivity_type[["Combined"]][[s]]$z$p.value)
    ))
  )
  
  try(kappa_baseline_sensitivity_type_results%<>%bind_rows(table))
  rm(s, table)
}

# View(kappa_baseline_sensitivity_type_results)


#### counts ------
baseline_combined_flags_sensitivity_type%>%
  group_by(Drug, Type)%>%
  summarise(pairs = n_distinct(Study_ID),
            positives = length(CRF[CRF=="1"]),
            negatives = length(CRF[CRF=="0"]),
            true_positives = length(CRF[CRF=="1" & EPMA=="1"]),
            true_negatives = length(CRF[CRF=="0" & EPMA=="0"]),
            false_positives = length(CRF[CRF=="0" & EPMA =="1"]),
            false_negatives = length(CRF[CRF =="1" & EPMA == "0"]),
            Sensitivity = round(true_positives/positives*100, 1),
            Specificity = round(true_negatives/negatives*100, 1),
            PPV = round(true_positives/(true_positives+false_positives)*100, 1),
            NPV = round(true_negatives/(true_negatives+false_negatives)*100, 1),
            Both = sum(EPMA=="1" & CRF=="1"),
            CRF_only = sum(EPMA=="0" & CRF=="1"),
            EPMA_only = sum(EPMA=="1" & CRF=="0"),
            Neither = sum(EPMA=="0" & CRF=="0"))%>%
  rbind(
    baseline_combined_flags_sensitivity_type%>%
      group_by(Type)%>%
      summarise(pairs = n(),
                positives = length(CRF[CRF=="1"]),
                negatives = length(CRF[CRF=="0"]),
                true_positives = length(CRF[CRF=="1" & EPMA=="1"]),
                true_negatives = length(CRF[CRF=="0" & EPMA=="0"]),
                false_positives = length(CRF[CRF=="0" & EPMA =="1"]),
                false_negatives = length(CRF[CRF =="1" & EPMA == "0"]),
                Sensitivity = round(true_positives/positives*100, 1),
                Specificity = round(true_negatives/negatives*100, 1),
                PPV = round(true_positives/(true_positives+false_positives)*100, 1),
                NPV = round(true_negatives/(true_negatives+false_negatives)*100, 1),
                Both = sum(EPMA=="1" & CRF=="1"),
                CRF_only = sum(EPMA=="0" & CRF=="1"),
                EPMA_only = sum(EPMA=="1" & CRF=="0"),
                Neither = sum(EPMA=="0" & CRF=="0"))%>%
      mutate(Drug="Combined")
  )%>%
  select(-c(positives, negatives, true_positives, true_negatives, false_positives, false_negatives))%>%
  rename(Individuals=pairs)->baseline_drugs_participant_counts_sensitivity_type

# View(baseline_drugs_participant_counts_sensitivity_type)





#### forest plot with counts -------


kappa_baseline_sensitivity_type_results%>%
  
  left_join(baseline_drugs_participant_counts_sensitivity_type,by=c("Drug", "Type"))%>%

  mutate(Drug=fct_relevel(Drug, 
                          "Corticosteroids",
                          "Macrolides",
                          "Tocilizumab or sarilumab",
                          "Remdesivir",
                          "Antiplatelets",
                          "Oral anticoagulants",
                          "LMWH",
                          "Oxygen",
                          "Combined"
  ))%>%
  mutate(Type=fct_relevel(Type, "Inpatient", "Any"))%>%
  arrange(Drug, Type)%>%
  mutate(`Kappa (95% CI)` = paste0(round(kappa,2), " (", round(kappa_CI_lower, 2), " to ", round(kappa_CI_upper, 2), ")"))%>%
  mutate(`PABAK (95% CI)` = paste0(round(pabak,2), " (", round(pabak_CI_lower, 2), " to ", round(pabak_CI_upper, 2), ")"))%>%
  
  select(Drug, Type, Individuals, Both, CRF_only, EPMA_only, Neither, Sensitivity, Specificity, PPV, NPV, `Kappa (95% CI)`, `PABAK (95% CI)`, kappa, se, kappa_CI_lower, kappa_CI_upper, pabak, pabak_CI_lower, pabak_CI_upper)%>%
  add_row(Drug="Corticosteroids", .before=1)%>%
  add_row(Drug="Macrolides",.before=4)%>%
  add_row(Drug="Tocilizumab or sarilumab",.before=7)%>%
  add_row(Drug="Remdesivir",.before=10)%>%
  add_row(Drug="Antiplatelets",.before=13)%>%
  add_row(Drug="Oral anticoagulants",.before=16)%>%
  add_row(Drug="LMWH",.before=19)%>%
  add_row(Drug="Oxygen",.before=22)%>%
  add_row(Drug="Combined",.before=25)%>%
  rename(Total=Individuals,
         `CRF only` = CRF_only,
         `Both sources` = Both,
         `EPMA only` = EPMA_only,
         `Neither source` = Neither)%>%
  add_column(`Kappa`=NA, .after = "NPV")%>%
  add_column(`PABAK`=NA, .after = "Kappa (95% CI)")%>%
  mutate(`Kappa`=paste(rep(" ", 20), collapse = " "))%>%
  mutate(`PABAK`=paste(rep(" ", 20), collapse = " "))%>%
  mutate(Size=Total)%>%
  mutate(Drug=as.character(Drug))%>%
  mutate(Type=if_else(!is.na(Type), paste("   ", Type), Drug))%>%
  rename(`Drug (by prescription status)`=Type)%>%
  select(-Drug)%>%
  mutate(across(-c(Size,se), ~ifelse(is.na(.), " ",.)))%>%
  mutate(across(c(kappa_CI_lower, kappa_CI_upper, kappa, pabak, pabak_CI_lower, pabak_CI_upper), ~as.numeric(.)))->forest_plot_table_baseline_sensitivity_type



forest_plot_table_baseline_sensitivity_type%>%
  slice_tail(n=2)%>%
  rename(`Prescription type`=1)->forest_plot_table_baseline_sensitivity_type_combined

forest_plot_table_baseline_sensitivity_type%>%
  slice_head(n=-3)->forest_plot_table_baseline_sensitivity_type_by_drug











##### combined ------


forest_plot_sensitivity_type_combined<-forest(forest_plot_table_baseline_sensitivity_type_combined[,1:14],
                                             est=list(forest_plot_table_baseline_sensitivity_type_combined$kappa,
                                                      forest_plot_table_baseline_sensitivity_type_combined$pabak),
                                             lower=list(forest_plot_table_baseline_sensitivity_type_combined$kappa_CI_lower,
                                                        forest_plot_table_baseline_sensitivity_type_combined$pabak_CI_lower),
                                             upper=list(forest_plot_table_baseline_sensitivity_type_combined$kappa_CI_upper,
                                                        forest_plot_table_baseline_sensitivity_type_combined$pabak_CI_upper),
                                             sizes=list(forest_plot_table_baseline_sensitivity_type_combined$se^2,
                                                        forest_plot_table_baseline_sensitivity_type_combined$se^2),
                                             ci_column = c(11,13),
                                             xlim=c(-1,1),
                                             arrow_lab = c("Negative agreement", "Positive agreement"),
                                             ref_line = 0)%>%
  insert_text(.,
              text = "Counts (participant/drug pairs)",
              col = 2:6,
              part = "header",
              gp = gpar(fontface = "bold"))%>%
  insert_text(.,
              text = "Agreement metrics",
              col = 7:13,
              part = "header",
              gp = gpar(fontface = "bold"))%>%
  gtable_add_grob(grobs=segmentsGrob(
    x0 = unit(0,"npc"), # horizontal origin
    y0 = unit(0,"npc"), # vertical origin
    x1 = unit(0,"npc"), # horizontal end
    y1 = unit(1,"npc"), # vertical end
    gp = gpar(lwd = 2.0)), # width
    t = 4,   # top end
    b = 6,  # bottom end
    l = 8,  # left end
    r = 8 # right end
  )

forest_plot_sensitivity_type_combined

ggsave(filename="Outputs/Figures/Analysis/forestplot_baseline_sensitivity_type_combined.png", 
       plot=forest_plot_sensitivity_type_combined,
       dpi = "retina",
       units = "cm",
       width = get_wh(plot=forest_plot_sensitivity_type_combined, unit="cm"),
       limitsize = F
       # height = 30
)

##### by drug -----


forest_plot_sensitivity_type_by_drug<-forest(forest_plot_table_baseline_sensitivity_type_by_drug[,1:14],
                                       est=list(forest_plot_table_baseline_sensitivity_type_by_drug$kappa,
                                                forest_plot_table_baseline_sensitivity_type_by_drug$pabak),
                                       lower=list(forest_plot_table_baseline_sensitivity_type_by_drug$kappa_CI_lower,
                                                  forest_plot_table_baseline_sensitivity_type_by_drug$pabak_CI_lower),
                                       upper=list(forest_plot_table_baseline_sensitivity_type_by_drug$kappa_CI_upper,
                                                  forest_plot_table_baseline_sensitivity_type_by_drug$pabak_CI_upper),
                                       sizes=list(forest_plot_table_baseline_sensitivity_type_by_drug$se^2,
                                                  forest_plot_table_baseline_sensitivity_type_by_drug$se^2),
                                       ci_column = c(11,13),
                                       xlim=c(-1,1),
                                       arrow_lab = c("Negative agreement", "Positive agreement"),
                                       ref_line = 0)%>%
  insert_text(.,
              text = "Counts (participants)",
              col = 2:6,
              part = "header",
              gp = gpar(fontface = "bold"))%>%
  insert_text(.,
              text = "Agreement metrics",
              col = 7:13,
              part = "header",
              gp = gpar(fontface = "bold"))%>%
  gtable_add_grob(grobs=segmentsGrob(
    x0 = unit(0,"npc"), # horizontal origin
    y0 = unit(0,"npc"), # vertical origin
    x1 = unit(0,"npc"), # horizontal end
    y1 = unit(1,"npc"), # vertical end
    gp = gpar(lwd = 2.0)), # width
    t = 4,   # top end
    b = 28,  # bottom end
    l = 8,  # left end
    r = 8 # right end
  )

forest_plot_sensitivity_type_by_drug

ggsave(filename="Outputs/Figures/Analysis/forestplot_baseline_sensitivity_type_by_drug.png", 
       plot=forest_plot_sensitivity_type_by_drug,
       dpi = "retina",
       units = "cm",
       width = get_wh(plot=forest_plot_sensitivity_type_by_drug, unit="cm"),
       limitsize = F
       # height = 30
)

















###  status ------

#### calculations -------

baseline_epma_flags_status<-
  epma_drug_flags%>%
  filter(Drug %in% 
           (pre_rand_drugs_crf%>%
              distinct(Drug)%>%.[[1]]))%>%
  left_join(censoring_dates%>%mutate(Study_ID=as.character(Study_ID)))%>%
  filter(!is.na(date))%>%
  group_by(Study_ID)%>%
  filter(date>=admission_date,
         date<=right_censoring_date_death_discharge_28d)%>% # restrict to people who have had a record within their randomisation admission  mutate(STATUS=replace_na(STATUS, "Unknown"))%>%
  group_by(Study_ID, Drug, STATUS)%>%
  summarise(EPMA=if_else(any(Flag=="1" & date<=rand_date & date>=admission_date), 1, 0))%>% # positive flag if record occurs before randomisation but after the current date of admission (therefore in current admission)
  ungroup()

# summarise flags
baseline_flags_sensitivity_status<-
  baseline_epma_flags_status%>%
  filter(STATUS=="Active")%>%
  
  rbind(baseline_epma_flags_status%>%
          mutate(STATUS="Any"))%>%
  mutate(EPMA=replace_na(EPMA, 0))%>%
  rename(Status=STATUS)%>%
  group_by(Study_ID, Drug, Status)%>%
  ungroup()%>%
  right_join(
    expand.grid(Study_ID=study_population,
                Status=c("Active", "Any"),
                Drug=baseline_epma_flags_status%>%distinct(Drug)%>%.[[1]])
  )%>%
  mutate(EPMA=replace_na(EPMA, 0))



#### Join flags from both sources and prepare for calculations

baseline_combined_flags_sensitivity_status<-pre_rand_drugs_crf%>%
  rename(CRF=Flag)%>%
  filter(Study_ID%in%baseline_flags_sensitivity_status$Study_ID)%>%
  left_join(baseline_flags_sensitivity_status)%>%
  select(Study_ID, Drug, EPMA, CRF, Status)%>%
  mutate(EPMA=replace_na(EPMA, 0))%>%
  group_by(Study_ID, Drug, Status)%>%
  summarise(EPMA=if_else(any(EPMA==1), 1, 0),
            CRF=if_else(any(CRF==1), 1, 0))%>%
  ungroup()



#### Calculate agreement --------


kappa_baseline_sensitivity_status<-list()

drugs<-baseline_combined_flags_sensitivity_status%>%
  distinct(Drug)%>%
  .[[1]]

status<-c("Active", "Any")


for (i in drugs){  
  
  for(s in status){
    
    baseline_combined_flags_sensitivity_status%>%
      filter(Drug==i,
             Status==s)%>%
      select(EPMA, CRF)%>%
      mutate(across(everything(), as.integer))->table
    
    matrix<-as.matrix(table(table))
    
    
    try(k<-epi.kappa(matrix, method="cohen", alternative = "greater", conf.level=0.95))
    
    if(exists("k"))
    {
      
      kappa_baseline_sensitivity_status[[i]][[s]]<-k
      
    }
    else{
      kappa_baseline_sensitivity_status[[i]][[s]]<-list()
    }
    
    rm(s, table, k, matrix)
    
  }
  rm(i)
}

View(kappa_baseline_sensitivity_status)

# unlist results in table


kappa_baseline_sensitivity_status_results<-data.frame()

for (i in drugs) {
  
  for (s in status){
    
    try(
      table<-(data.frame(
        Drug = i,
        Status = s,
        kappa = unlist(kappa_baseline_sensitivity_status[[i]][[s]]$kappa$est),
        se = unlist(kappa_baseline_sensitivity_status[[i]][[s]]$kappa$se),
        kappa_CI_lower = unlist(kappa_baseline_sensitivity_status[[i]][[s]]$kappa$lower),
        kappa_CI_upper = unlist(kappa_baseline_sensitivity_status[[i]][[s]]$kappa$upper),
        
        pabak = unlist(kappa_baseline_sensitivity_status[[i]][[s]]$pabak$est),
        pabak_CI_lower = unlist(kappa_baseline_sensitivity_status[[i]][[s]]$pabak$lower),
        pabak_CI_upper = unlist(kappa_baseline_sensitivity_status[[i]][[s]]$pabak$upper),
        
        p_value = unlist(kappa_baseline_sensitivity_status[[i]][[s]]$z$p.value)
      ))
    )
    
    try(kappa_baseline_sensitivity_status_results%<>%bind_rows(table))
    rm(Status, s, table)
  }
  rm(Drug, i)
}


# View(kappa_baseline_sensitivity_status_results)  


#### aggregate summary (all drugs) ------


for(s in status){
    
    baseline_combined_flags_sensitivity_status%>%
      filter(Status==s)%>%
      select(EPMA, CRF)%>%
      mutate(across(everything(), as.integer))->table
    
    matrix<-as.matrix(table(table))
    
    
    try(k<-epi.kappa(matrix, method="cohen", alternative = "greater", conf.level=0.95))
    
    if(exists("k"))
    {
      
      kappa_baseline_sensitivity_status[["Combined"]][[s]]<-k
      
    }
    else{
      kappa_baseline_sensitivity_status[["Combined"]][[s]]<-list()
    }
    
    rm(s, table, k, matrix)
    
  }

View(kappa_baseline_sensitivity_status)

# unlist results in table

for (s in status){
    
    try(
      table<-(data.frame(
        Drug = "Combined",
        Status = s,
        kappa = unlist(kappa_baseline_sensitivity_status[["Combined"]][[s]]$kappa$est),
        se = unlist(kappa_baseline_sensitivity_status[["Combined"]][[s]]$kappa$se),
        kappa_CI_lower = unlist(kappa_baseline_sensitivity_status[["Combined"]][[s]]$kappa$lower),
        kappa_CI_upper = unlist(kappa_baseline_sensitivity_status[["Combined"]][[s]]$kappa$upper),
        
        pabak = unlist(kappa_baseline_sensitivity_status[["Combined"]][[s]]$pabak$est),
        pabak_CI_lower = unlist(kappa_baseline_sensitivity_status[["Combined"]][[s]]$pabak$lower),
        pabak_CI_upper = unlist(kappa_baseline_sensitivity_status[["Combined"]][[s]]$pabak$upper),
        
        p_value = unlist(kappa_baseline_sensitivity_status[["Combined"]][[s]]$z$p.value)
      ))
    )
    
    try(kappa_baseline_sensitivity_status_results%<>%bind_rows(table))
    rm(s, table)
  }



#### counts ------
baseline_combined_flags_sensitivity_status%>%
  group_by(Drug, Status)%>%
  summarise(pairs = n_distinct(Study_ID),
            positives = length(CRF[CRF=="1"]),
            negatives = length(CRF[CRF=="0"]),
            true_positives = length(CRF[CRF=="1" & EPMA=="1"]),
            true_negatives = length(CRF[CRF=="0" & EPMA=="0"]),
            false_positives = length(CRF[CRF=="0" & EPMA =="1"]),
            false_negatives = length(CRF[CRF =="1" & EPMA == "0"]),
            Sensitivity = round(true_positives/positives*100, 1),
            Specificity = round(true_negatives/negatives*100, 1),
            PPV = round(true_positives/(true_positives+false_positives)*100, 1),
            NPV = round(true_negatives/(true_negatives+false_negatives)*100, 1),
            Both = sum(EPMA=="1" & CRF=="1"),
            CRF_only = sum(EPMA=="0" & CRF=="1"),
            EPMA_only = sum(EPMA=="1" & CRF=="0"),
            Neither = sum(EPMA=="0" & CRF=="0"))%>%
  rbind(
    baseline_combined_flags_sensitivity_status%>%
      group_by(Status)%>%
      summarise(pairs = n(),
                positives = length(CRF[CRF=="1"]),
                negatives = length(CRF[CRF=="0"]),
                true_positives = length(CRF[CRF=="1" & EPMA=="1"]),
                true_negatives = length(CRF[CRF=="0" & EPMA=="0"]),
                false_positives = length(CRF[CRF=="0" & EPMA =="1"]),
                false_negatives = length(CRF[CRF =="1" & EPMA == "0"]),
                Sensitivity = round(true_positives/positives*100, 1),
                Specificity = round(true_negatives/negatives*100, 1),
                PPV = round(true_positives/(true_positives+false_positives)*100, 1),
                NPV = round(true_negatives/(true_negatives+false_negatives)*100, 1),
                Both = sum(EPMA=="1" & CRF=="1"),
                CRF_only = sum(EPMA=="0" & CRF=="1"),
                EPMA_only = sum(EPMA=="1" & CRF=="0"),
                Neither = sum(EPMA=="0" & CRF=="0"))%>%
      mutate(Drug="Combined")
    )%>%
  select(-c(positives, negatives, true_positives, true_negatives, false_positives, false_negatives))%>%
  rename(Individuals=pairs)->baseline_drugs_participant_counts_sensitivity_status

# View(baseline_drugs_participant_counts_sensitivity_status)





#### forest plot with counts -------


kappa_baseline_sensitivity_status_results%>%
  left_join(baseline_drugs_participant_counts_sensitivity_status,by=c("Drug", "Status"))%>%
  mutate(Drug=fct_relevel(Drug, 
                          "Corticosteroids",
                          "Macrolides",
                          "Tocilizumab or sarilumab",
                          "Remdesivir",
                          "Antiplatelets",
                          "Oral anticoagulants",
                          "LMWH",
                          "Oxygen",
                          "Combined"
  ))%>%
  arrange(Drug, Status)%>%
  mutate(`Kappa (95% CI)` = paste0(round(kappa,2), " (", round(kappa_CI_lower, 2), " to ", round(kappa_CI_upper, 2), ")"))%>%
  mutate(`PABAK (95% CI)` = paste0(round(pabak,2), " (", round(pabak_CI_lower, 2), " to ", round(pabak_CI_upper, 2), ")"))%>%
  
  select(Drug, Status, Individuals, Both, CRF_only, EPMA_only, Neither, Sensitivity, Specificity, PPV, NPV, `Kappa (95% CI)`, `PABAK (95% CI)`, kappa, se, kappa_CI_lower, kappa_CI_upper, pabak, pabak_CI_lower, pabak_CI_upper)%>%
  add_row(Drug="Corticosteroids", .before=1)%>%
  add_row(Drug="Macrolides",.before=4)%>%
  add_row(Drug="Tocilizumab or sarilumab",.before=7)%>%
  add_row(Drug="Remdesivir",.before=10)%>%
  add_row(Drug="Antiplatelets",.before=13)%>%
  add_row(Drug="Oral anticoagulants",.before=16)%>%
  add_row(Drug="LMWH",.before=19)%>%
  add_row(Drug="Oxygen",.before=22)%>%
  add_row(Drug="Combined",.before=25)%>%
  rename(Total=Individuals,
         `CRF only` = CRF_only,
         `Both sources` = Both,
         `EPMA only` = EPMA_only,
         `Neither source` = Neither)%>%
  add_column(`Kappa`=NA, .after = "NPV")%>%
  add_column(`PABAK`=NA, .after = "Kappa (95% CI)")%>%
  mutate(`Kappa`=paste(rep(" ", 20), collapse = " "))%>%
  mutate(`PABAK`=paste(rep(" ", 20), collapse = " "))%>%
  mutate(Size=Total)%>%
  mutate(Drug=as.character(Drug))%>%
  mutate(Status=if_else(!is.na(Status), paste("   ", Status), Drug))%>%
  rename(`Drug (by prescription status)`=Status)%>%
  select(-Drug)%>%
  mutate(across(-c(Size,se), ~ifelse(is.na(.), " ",.)))%>%
  mutate(across(c(kappa_CI_lower, kappa_CI_upper, kappa, pabak, pabak_CI_lower, pabak_CI_upper), ~as.numeric(.)))->forest_plot_table_baseline_sensitivity_status





forest_plot_table_baseline_sensitivity_status%>%
  slice_tail(n=2)%>%
  rename(`Prescription status`=1)->forest_plot_table_baseline_sensitivity_status_combined

forest_plot_table_baseline_sensitivity_status%>%
  slice_head(n=-3)->forest_plot_table_baseline_sensitivity_status_by_drug








##### combined -----

forest_plot_sensitivity_status_combined<-forest(forest_plot_table_baseline_sensitivity_status_combined[,1:14],
                                               est=list(forest_plot_table_baseline_sensitivity_status_combined$kappa,
                                                        forest_plot_table_baseline_sensitivity_status_combined$pabak),
                                               lower=list(forest_plot_table_baseline_sensitivity_status_combined$kappa_CI_lower,
                                                          forest_plot_table_baseline_sensitivity_status_combined$pabak_CI_lower),
                                               upper=list(forest_plot_table_baseline_sensitivity_status_combined$kappa_CI_upper,
                                                          forest_plot_table_baseline_sensitivity_status_combined$pabak_CI_upper),
                                               sizes=list(forest_plot_table_baseline_sensitivity_status_combined$se^2,
                                                          forest_plot_table_baseline_sensitivity_status_combined$se^2),
                                               ci_column = c(11,13),
                                               xlim=c(-1,1),
                                               arrow_lab = c("Negative agreement", "Positive agreement"),
                                               ref_line = 0)%>%
  insert_text(.,
              text = "Counts (participant/drug pairs)",
              col = 2:6,
              part = "header",
              gp = gpar(fontface = "bold"))%>%
  insert_text(.,
              text = "Agreement metrics",
              col = 7:13,
              part = "header",
              gp = gpar(fontface = "bold"))%>%
  gtable_add_grob(grobs=segmentsGrob(
    x0 = unit(0,"npc"), # horizontal origin
    y0 = unit(0,"npc"), # vertical origin
    x1 = unit(0,"npc"), # horizontal end
    y1 = unit(1,"npc"), # vertical end
    gp = gpar(lwd = 2.0)), # width
    t = 4,   # top end
    b = 6,  # bottom end
    l = 8,  # left end
    r = 8 # right end
  )

forest_plot_sensitivity_status_combined

ggsave(filename="Outputs/Figures/Analysis/forestplot_baseline_sensitivity_status_combined.png", 
       plot=forest_plot_sensitivity_status_combined,
       dpi = "retina",
       units = "cm",
       width = get_wh(plot=forest_plot_sensitivity_status_combined, unit="cm"),
       limitsize = F
       # height = 30
)





##### by drug -------


forest_plot_sensitivity_status_by_drug<-forest(forest_plot_table_baseline_sensitivity_status_by_drug[,1:14],
                    est=list(forest_plot_table_baseline_sensitivity_status_by_drug$kappa,
                             forest_plot_table_baseline_sensitivity_status_by_drug$pabak),
                    lower=list(forest_plot_table_baseline_sensitivity_status_by_drug$kappa_CI_lower,
                               forest_plot_table_baseline_sensitivity_status_by_drug$pabak_CI_lower),
                    upper=list(forest_plot_table_baseline_sensitivity_status_by_drug$kappa_CI_upper,
                               forest_plot_table_baseline_sensitivity_status_by_drug$pabak_CI_upper),
                    sizes=list(forest_plot_table_baseline_sensitivity_status_by_drug$se^2,
                               forest_plot_table_baseline_sensitivity_status_by_drug$se^2),
                    ci_column = c(11,13),
                    xlim=c(-1,1),
                    arrow_lab = c("Negative agreement", "Positive agreement"),
                    ref_line = 0)%>%
  insert_text(.,
              text = "Counts (participants)",
              col = 2:6,
              part = "header",
              gp = gpar(fontface = "bold"))%>%
  insert_text(.,
              text = "Agreement metrics",
              col = 7:13,
              part = "header",
              gp = gpar(fontface = "bold"))%>%
  gtable_add_grob(grobs=segmentsGrob(
    x0 = unit(0,"npc"), # horizontal origin
    y0 = unit(0,"npc"), # vertical origin
    x1 = unit(0,"npc"), # horizontal end
    y1 = unit(1,"npc"), # vertical end
    gp = gpar(lwd = 2.0)), # width
    t = 4,   # top end
    b = 28,  # bottom end
    l = 8,  # left end
    r = 8 # right end
  )

forest_plot_sensitivity_status_by_drug

ggsave(filename="Outputs/Figures/Analysis/forestplot_baseline_sensitivity_status_by_drug.png", 
       plot=forest_plot_sensitivity_status_by_drug,
       dpi = "retina",
       units = "cm",
       width = get_wh(plot=forest_plot_sensitivity_status_by_drug, unit="cm"),
       limitsize = F
       # height = 30
)






###  formulations -------

#### calculations ------

baseline_epma_flags_formulations<-
  epma_drug_flags%>%
  filter(Drug %in% 
           (pre_rand_drugs_crf%>%
              distinct(Drug)%>%.[[1]]))%>%
  left_join(censoring_dates%>%mutate(Study_ID=as.character(Study_ID)))%>%
  filter(!is.na(date))%>%
  group_by(Study_ID)%>%
  filter(date>=admission_date & 
           date<=right_censoring_date_death_discharge_28d)%>% # restrict to people who have had a record within their randomisation admission
  mutate(FORM=case_when(is.na(FORM) ~ "Unknown",
                        FORM %in% systemic_formulations ~ "Systemic",
                        FORM %in% oxygen_formulations ~ "Inhaled",
                        !is.na(FORM) & !FORM %in% systemic_formulations & !FORM %in% oxygen_formulations ~ "Other"))%>%
  group_by(Study_ID, Drug, Table, FORM)%>%
  summarise(EPMA=if_else(any(Flag=="1" & date<=rand_date & date>=admission_date), 1, 0))%>% # positive flag if record occurs before randomisation but after the current date of admission (therefore in current admission)
  ungroup()

# create summarised flags for each formulation
baseline_flags_sensitivity_formulations<-
  
  # drugs, systemic formulations
  baseline_epma_flags_formulations%>%
  filter(Drug != "Oxygen",
         FORM =="Systemic")%>%
  select(-FORM,-Table)%>%
  group_by(Study_ID, Drug)%>%
  summarise(EPMA=if_else(any(EPMA==1), 1, 0))%>%
  mutate(Formulation_group="Systemic")%>%
  
  # drugs, any formulation
  rbind(baseline_epma_flags_formulations%>%
          filter(Drug != "Oxygen")%>%
          group_by(Study_ID, Drug)%>%
          summarise(EPMA=if_else(any(EPMA==1), 1, 0))%>%
          mutate(Formulation_group="Any"))%>%
  
  # drugs, systemic or unknown formulation
  rbind(
    baseline_epma_flags_formulations%>%
      filter(Drug != "Oxygen",
             FORM %in% c("Systemic", "Unknown"))%>%
      group_by(Study_ID, Drug)%>%
      summarise(EPMA=if_else(any(EPMA==1), 1, 0))%>%
      mutate(Formulation_group="Systemic or unknown")
    )%>%
  
  
  # oxygen, inhaled formulation
  rbind(
    baseline_epma_flags_formulations%>%
      filter(Drug == "Oxygen",
             FORM == "Inhaled")%>%
      group_by(Study_ID, Drug)%>%
      summarise(EPMA=if_else(any(EPMA==1), 1, 0))%>%
      mutate(Formulation_group="Inhaled"))%>%
  
  # oxygen, any formulation
  rbind(
    baseline_epma_flags_formulations%>%
      filter(Drug == "Oxygen")%>%
      group_by(Study_ID, Drug)%>%
      summarise(EPMA=if_else(any(EPMA==1), 1, 0))%>%
      mutate(Formulation_group="Any"))%>%
  
  # oxygen, inhaled or unknown formulation
  rbind(
    baseline_epma_flags_formulations%>%
      filter(Drug == "Oxygen",
             FORM %in% c("Inhaled", "Unknown"))%>%
      group_by(Study_ID, Drug)%>%
      summarise(EPMA=if_else(any(EPMA==1), 1, 0))%>%
      mutate(Formulation_group="Inhaled or unknown"))%>%
  filter(Formulation_group!="Unknown")
  


# fill with 0 for people with absent flags

baseline_flags_sensitivity_formulations%<>%
  right_join(
    expand.grid(Study_ID=study_population,
                Drug=pre_rand_drugs_crf%>%distinct(Drug)%>%.[[1]],
                Formulation_group=c("Systemic", "Systemic or unknown", "Inhaled", "Inhaled or unknown", "Any"))%>%
      mutate(Study_ID=as.character(Study_ID)),
    by=c("Study_ID", "Drug", "Formulation_group")
  )%>%
  mutate(EPMA=replace_na(EPMA, 0))

# combine flags from two sources

baseline_combined_flags_with_formulations<-
  pre_rand_drugs_crf%>%
  filter(Study_ID%in%baseline_flags_sensitivity_formulations$Study_ID)%>%
  rename(CRF=Flag)%>%
  left_join(baseline_flags_sensitivity_formulations,
            by=c("Study_ID", "Drug")
  )%>%
  select(Study_ID, Drug, EPMA, CRF, Formulation_group)%>%
  mutate(EPMA=replace_na(EPMA, 0))%>%
  ungroup()





#### Calculate agreement --------


kappa_baseline_sensitivity_formulations<-list()

drugs<-baseline_combined_flags_with_formulations%>%
  distinct(Drug)%>%
  # filter(Drug!="Oxygen")%>%
  .[[1]]

formulations<-baseline_combined_flags_with_formulations%>%distinct(Formulation_group)%>%.[[1]]


for (i in drugs){  
  
  for(f in formulations){
    
    baseline_combined_flags_with_formulations%>%
      filter(Drug==i,
             Formulation_group==f)%>%
      select(EPMA, CRF)%>%
      mutate(across(everything(), as.integer))%>%
      mutate(across(c(EPMA, CRF), ~replace_na(.x, 0)))->table
    
    matrix<-as.matrix(table(table))
    
    
    try(k<-epi.kappa(matrix, method="cohen", alternative = "greater", conf.level=0.95))
    
    if(exists("k"))
    {
      
      kappa_baseline_sensitivity_formulations[[i]][[f]]<-k
      
    }
    else{
      kappa_baseline_sensitivity_formulations[[i]][[f]]<-list()
    }
    
    rm(f, table, matrix, k)
    
  }
  rm(i)
}

View(kappa_baseline_sensitivity_formulations)

# unlist results in table


kappa_baseline_sensitivity_formulations_results<-data.frame()

for (i in drugs) {
  
  for (f in formulations){
    
    try(
      table<-(data.frame(
        Drug = i,
        Formulation = f,
        kappa = unlist(kappa_baseline_sensitivity_formulations[[i]][[f]]$kappa$est),
        se = unlist(kappa_baseline_sensitivity_formulations[[i]][[f]]$kappa$se),
        kappa_CI_lower = unlist(kappa_baseline_sensitivity_formulations[[i]][[f]]$kappa$lower),
        kappa_CI_upper = unlist(kappa_baseline_sensitivity_formulations[[i]][[f]]$kappa$upper),
        
        pabak = unlist(kappa_baseline_sensitivity_formulations[[i]][[f]]$pabak$est),
        pabak_CI_lower = unlist(kappa_baseline_sensitivity_formulations[[i]][[f]]$pabak$lower),
        pabak_CI_upper = unlist(kappa_baseline_sensitivity_formulations[[i]][[f]]$pabak$upper),
        
        p_value = unlist(kappa_baseline_sensitivity_formulations[[i]][[f]]$z$p)
      ))
    )
    
    try(kappa_baseline_sensitivity_formulations_results%<>%bind_rows(table))
    rm(f, table)
  }
  rm(Drug, i)
}


View(kappa_baseline_sensitivity_formulations_results)  


#### aggregate summary (all drugs) ------

formulations<-c("Any", "Systemic or unknown", "Systemic")


for (f in formulations){
  
  baseline_combined_flags_with_formulations%>%
    filter(Drug!="Oxygen")%>%
    filter(Formulation_group==f)%>%
    select(EPMA, CRF)%>%
    mutate(across(everything(), as.integer))->table
  
  matrix<-as.matrix(table(table))
  
  
  try(k<-epi.kappa(matrix, method="cohen", alternative = "greater", conf.level=0.95))
  
  if(exists("k"))
  {
    
    kappa_baseline_sensitivity_formulations[["Combined"]][[f]]<-k
    
  }
  else{
    kappa_baseline_sensitivity_formulations[["Combined"]][[f]]<-list()
  }
  
  rm(f, table, k, matrix)
  
}

View(kappa_baseline_sensitivity_formulations)

# unlist results in table

for (f in formulations){
  
  try(
    table<-(data.frame(
      Drug = "Combined",
      Formulation = f,
      kappa = unlist(kappa_baseline_sensitivity_formulations[["Combined"]][[f]]$kappa$est),
      se = unlist(kappa_baseline_sensitivity_formulations[["Combined"]][[f]]$kappa$se),
      kappa_CI_lower = unlist(kappa_baseline_sensitivity_formulations[["Combined"]][[f]]$kappa$lower),
      kappa_CI_upper = unlist(kappa_baseline_sensitivity_formulations[["Combined"]][[f]]$kappa$upper),
      
      pabak = unlist(kappa_baseline_sensitivity_formulations[["Combined"]][[f]]$pabak$est),
      pabak_CI_lower = unlist(kappa_baseline_sensitivity_formulations[["Combined"]][[f]]$pabak$lower),
      pabak_CI_upper = unlist(kappa_baseline_sensitivity_formulations[["Combined"]][[f]]$pabak$upper),
      
      p_value = unlist(kappa_baseline_sensitivity_formulations[["Combined"]][[f]]$z$p.value)
    ))
  )
  
  try(kappa_baseline_sensitivity_formulations_results%<>%bind_rows(table))
  
}



#### counts ------
baseline_combined_flags_with_formulations%>%
  rename(Formulation=Formulation_group)%>%
  group_by(Drug, Formulation)%>%
  summarise(pairs = n_distinct(Study_ID),
            positives = length(CRF[CRF=="1"]),
            negatives = length(CRF[CRF=="0"]),
            true_positives = length(CRF[CRF=="1" & EPMA=="1"]),
            true_negatives = length(CRF[CRF=="0" & EPMA=="0"]),
            false_positives = length(CRF[CRF=="0" & EPMA =="1"]),
            false_negatives = length(CRF[CRF =="1" & EPMA == "0"]),
            Sensitivity = round(true_positives/positives*100, 1),
            Specificity = round(true_negatives/negatives*100, 1),
            PPV = round(true_positives/(true_positives+false_positives)*100, 1),
            NPV = round(true_negatives/(true_negatives+false_negatives)*100, 1),
            Both = sum(EPMA=="1" & CRF=="1"),
            CRF_only = sum(EPMA=="0" & CRF=="1"),
            EPMA_only = sum(EPMA=="1" & CRF=="0"),
            Neither = sum(EPMA=="0" & CRF=="0"))%>%
  rbind(
    baseline_combined_flags_with_formulations%>%
      rename(Formulation=Formulation_group)%>%
      
      filter(Drug!="Oxygen",
             Formulation !="Inhaled",
             Formulation !="Inhaled or unknown")%>%
      group_by(Formulation)%>%
      summarise(pairs = n(),
                positives = length(CRF[CRF=="1"]),
                negatives = length(CRF[CRF=="0"]),
                true_positives = length(CRF[CRF=="1" & EPMA=="1"]),
                true_negatives = length(CRF[CRF=="0" & EPMA=="0"]),
                false_positives = length(CRF[CRF=="0" & EPMA =="1"]),
                false_negatives = length(CRF[CRF =="1" & EPMA == "0"]),
                Sensitivity = round(true_positives/positives*100, 1),
                Specificity = round(true_negatives/negatives*100, 1),
                PPV = round(true_positives/(true_positives+false_positives)*100, 1),
                NPV = round(true_negatives/(true_negatives+false_negatives)*100, 1),
                Both = sum(EPMA=="1" & CRF=="1"),
                CRF_only = sum(EPMA=="0" & CRF=="1"),
                EPMA_only = sum(EPMA=="1" & CRF=="0"),
                Neither = sum(EPMA=="0" & CRF=="0"))%>%
      mutate(Drug="Combined"))%>%
  
  
  
  select(-c(positives, negatives, true_positives, true_negatives, false_positives, false_negatives))%>%
  rename(Individuals=pairs)->baseline_drugs_participant_counts_sensitivity_formulations

# View(baseline_drugs_participant_counts_sensitivity_formulations)




#### forest plot with counts -------


kappa_baseline_sensitivity_formulations_results%>%
  left_join(baseline_drugs_participant_counts_sensitivity_formulations,by=c("Drug", "Formulation"))%>%

  mutate(Drug=fct_relevel(Drug, 
                          "Corticosteroids",
                          "Macrolides",
                          "Tocilizumab or sarilumab",
                          "Remdesivir",
                          "Antiplatelets",
                          "Oral anticoagulants",
                          "LMWH",
                          "Oxygen"
  ))%>%
  mutate(Formulation=fct_relevel(Formulation, "Systemic", 
                                 "Inhaled",
                                 "Systemic or unknown",
                                 "Inhaled or unknown",
                                 "Any"))%>%
  arrange(Drug, Formulation)%>%
  mutate(`Kappa (95% CI)` = paste0(round(kappa,2), " (", round(kappa_CI_lower, 2), " to ", round(kappa_CI_upper, 2), ")"))%>%
  mutate(`PABAK (95% CI)` = paste0(round(pabak,2), " (", round(pabak_CI_lower, 2), " to ", round(pabak_CI_upper, 2), ")"))%>%
  select(Drug, Formulation, Individuals, Both, CRF_only, EPMA_only, Neither, Sensitivity, Specificity, PPV, NPV, `Kappa (95% CI)`, `PABAK (95% CI)`, kappa, se, kappa_CI_lower, kappa_CI_upper, pabak, pabak_CI_lower, pabak_CI_upper)%>%
  add_row(Drug="Corticosteroids", .before=1)%>%
  add_row(Drug="Macrolides",.before=5)%>%
  add_row(Drug="Tocilizumab or sarilumab",.before=9)%>%
  add_row(Drug="Remdesivir",.before=13)%>%
  add_row(Drug="Antiplatelets",.before=17)%>%
  add_row(Drug="Oral anticoagulants",.before=21)%>%
  add_row(Drug="LMWH",.before=25)%>%
  add_row(Drug="Oxygen",.before=29)%>%
  add_row(Drug="Combined",.before=33)%>%
  rename(Total=Individuals,
         `CRF only` = CRF_only,
         `Both sources` = Both,
         `EPMA only` = EPMA_only,
         `Neither source` = Neither)%>%
  add_column(`Kappa`=NA, .after = "NPV")%>%
  add_column(`PABAK`=NA, .after = "Kappa (95% CI)")%>%
  mutate(`Kappa`=paste(rep(" ", 20), collapse = " "))%>%
  mutate(`PABAK`=paste(rep(" ", 20), collapse = " "))%>%
  mutate(Size=Total)%>%
  mutate(Drug=as.character(Drug))%>%
  mutate(Formulation=if_else(!is.na(Formulation), paste("   ", Formulation), Drug))%>%
  rename(`Drug (by formulation)`=Formulation)%>%
  select(-Drug)%>%
  mutate(across(-Size, ~ifelse(is.na(.), " ",.)))%>%
  mutate(across(c(kappa_CI_lower, kappa_CI_upper, kappa, pabak, pabak_CI_lower, pabak_CI_upper, se), ~as.numeric(.)))->forest_plot_table_baseline_sensitivity_formulation




forest_plot_table_baseline_sensitivity_formulation%>%
  slice_tail(n=3)%>%
  rename(`Prescription formulation`=1)->forest_plot_table_baseline_sensitivity_formulation_combined

forest_plot_table_baseline_sensitivity_formulation%>%
  slice_head(n=-4)->forest_plot_table_baseline_sensitivity_formulation_by_drug







##### combined ------


forest_plot_sensitivity_formulations_combined<-forest(forest_plot_table_baseline_sensitivity_formulation_combined[,1:14],
                                                     est=list(forest_plot_table_baseline_sensitivity_formulation_combined$kappa,
                                                              forest_plot_table_baseline_sensitivity_formulation_combined$pabak),
                                                     lower=list(forest_plot_table_baseline_sensitivity_formulation_combined$kappa_CI_lower,
                                                                forest_plot_table_baseline_sensitivity_formulation_combined$pabak_CI_lower),
                                                     upper=list(forest_plot_table_baseline_sensitivity_formulation_combined$kappa_CI_upper,
                                                                forest_plot_table_baseline_sensitivity_formulation_combined$pabak_CI_upper),
                                                     sizes=list(forest_plot_table_baseline_sensitivity_formulation_combined$se^2,
                                                                forest_plot_table_baseline_sensitivity_formulation_combined$se^2),
                                                     ci_column = c(11,13),
                                                     xlim=c(-1,1),
                                                     arrow_lab = c("Negative agreement", "Positive agreement"),
                                                     ref_line = 0)%>%
  insert_text(.,
              text = "Counts (participant/drug pairs)",
              col = 2:6,
              part = "header",
              gp = gpar(fontface = "bold"))%>%
  insert_text(.,
              text = "Agreement metrics",
              col = 7:13,
              part = "header",
              gp = gpar(fontface = "bold"))%>%
  gtable_add_grob(grobs=segmentsGrob(
    x0 = unit(0,"npc"), # horizontal origin
    y0 = unit(0,"npc"), # vertical origin
    x1 = unit(0,"npc"), # horizontal end
    y1 = unit(1,"npc"), # vertical end
    gp = gpar(lwd = 2.0)), # width
    t = 4,   # top end
    b = 7,  # bottom end
    l = 8,  # left end
    r = 8 # right end
  )

forest_plot_sensitivity_formulations_combined

ggsave(filename="Outputs/Figures/Analysis/forestplot_baseline_sensitivity_formulations_combined.png", 
       plot=forest_plot_sensitivity_formulations_combined,
       dpi = "retina",
       units = "cm",
       width = get_wh(plot=forest_plot_sensitivity_formulations_combined, unit="cm"),
       limitsize = F
       # height = 30
)

##### by drug --------


forest_plot_sensitivity_formulations_by_drug<-forest(forest_plot_table_baseline_sensitivity_formulation_by_drug[,1:14],
                                             est=list(forest_plot_table_baseline_sensitivity_formulation_by_drug$kappa,
                                                      forest_plot_table_baseline_sensitivity_formulation_by_drug$pabak),
                                             lower=list(forest_plot_table_baseline_sensitivity_formulation_by_drug$kappa_CI_lower,
                                                        forest_plot_table_baseline_sensitivity_formulation_by_drug$pabak_CI_lower),
                                             upper=list(forest_plot_table_baseline_sensitivity_formulation_by_drug$kappa_CI_upper,
                                                        forest_plot_table_baseline_sensitivity_formulation_by_drug$pabak_CI_upper),
                                             sizes=list(forest_plot_table_baseline_sensitivity_formulation_by_drug$se^2,
                                                        forest_plot_table_baseline_sensitivity_formulation_by_drug$se^2),
                                             ci_column = c(11,13),
                                             xlim=c(-1,1),
                                             arrow_lab = c("Negative agreement", "Positive agreement"),
                                             ref_line = 0)%>%
  insert_text(.,
              text = "Counts (participants)",
              col = 2:6,
              part = "header",
              gp = gpar(fontface = "bold"))%>%
  insert_text(.,
              text = "Agreement metrics",
              col = 7:13,
              part = "header",
              gp = gpar(fontface = "bold"))%>%
  gtable_add_grob(grobs=segmentsGrob(
    x0 = unit(0,"npc"), # horizontal origin
    y0 = unit(0,"npc"), # vertical origin
    x1 = unit(0,"npc"), # horizontal end
    y1 = unit(1,"npc"), # vertical end
    gp = gpar(lwd = 2.0)), # width
    t = 4,   # top end
    b = 36,  # bottom end
    l = 8,  # left end
    r = 8 # right end
  )

forest_plot_sensitivity_formulations_by_drug

ggsave(filename="Outputs/Figures/Analysis/forestplot_baseline_sensitivity_formulations_by_drug.png", 
       plot=forest_plot_sensitivity_formulations_by_drug,
       dpi = "retina",
       units = "cm",
       width = get_wh(plot=forest_plot_sensitivity_formulations_by_drug, unit="cm"),
       limitsize = F
       # height = 30
)











































###  date of randomisation -------

#### calculations -----

baseline_epma_flags_sensitivity_rand_dates<-
  epma_drug_flags%>%
  filter(Drug %in% 
           (pre_rand_drugs_crf%>%
              distinct(Drug)%>%.[[1]]))%>%
  left_join(censoring_dates%>%mutate(Study_ID=as.character(Study_ID)))%>%
  group_by(Study_ID)%>%
  filter(!is.na(date))%>%
  filter(any(date>=admission_date & date<=right_censoring_date_death_discharge_28d))%>% # restrict to people who have had a record within their randomisation admission
  group_by(Study_ID, Drug)%>%
  summarise(EPMA=if_else(any(Flag=="1" & date<=rand_date & date>=admission_date), 1, 0))%>% # positive flag if record occurs before randomisation but after the current date of admission (therefore in current admission)
  mutate(Rand_date_rule="Include")%>%
  ungroup()%>%
  
  rbind(
    epma_drug_flags%>%
      filter(Drug %in% 
               (pre_rand_drugs_crf%>%
                  distinct(Drug)%>%.[[1]]))%>%
      left_join(censoring_dates%>%mutate(Study_ID=as.character(Study_ID)))%>%
      group_by(Study_ID)%>%
      filter(!is.na(date))%>%
      filter(any(date>=admission_date & date<=right_censoring_date_death_discharge_28d))%>% # restrict to people who have had a record within their randomisation admission
      group_by(Study_ID, Drug)%>%
      summarise(EPMA=if_else(any(Flag=="1" & date<rand_date & date>=admission_date), 1, 0))%>% # positive flag if record occurs before randomisation but after the current date of admission (therefore in current admission)
      mutate(Rand_date_rule="Exclude")%>%
      ungroup()
    )

#### fill with 0 for people with no records

baseline_epma_flags_sensitivity_rand_dates%<>%
  right_join(
    expand.grid(Study_ID=study_population,
                Drug=pre_rand_drugs_crf%>%distinct(Drug)%>%.[[1]],
                Rand_date_rule=c("Include", "Exclude")))%>%
  mutate(EPMA=replace_na(EPMA,0))





#### Join flags from both sources and prepare for calculations

baseline_combined_flags_sensitivity_rand_dates<-pre_rand_drugs_crf%>%
  filter(Study_ID %in% baseline_epma_flags_sensitivity_rand_dates$Study_ID)%>%
  rename(CRF=Flag)%>%
  left_join(baseline_epma_flags_sensitivity_rand_dates)%>%
  select(Study_ID, Drug, EPMA, CRF, Rand_date_rule)%>%
  ungroup()
  


#### Calculate agreement --------


kappa_baseline_sensitivity_rand_dates<-list()

drugs<-baseline_combined_flags_sensitivity_rand_dates%>%
  distinct(Drug)%>%
  # filter(Drug!="Oxygen")%>%
  .[[1]]

Rand_date_rules<-c("Include", "Exclude")


for (i in drugs){  
  
  for(r in Rand_date_rules){
    
    baseline_combined_flags_sensitivity_rand_dates%>%
      filter(Drug==i,
             Rand_date_rule==r)%>%
      select(EPMA, CRF)%>%
      mutate(across(everything(), as.integer))%>%
      mutate(across(c(EPMA, CRF), ~replace_na(.x, 0)))->table
    
    matrix<-as.matrix(table(table))
    
    
    try(k<-epi.kappa(matrix, method="cohen", alternative = "greater", conf.level=0.95))
    
    if(exists("k"))
    {
      
      kappa_baseline_sensitivity_rand_dates[[i]][[r]]<-k
      
    }
    else{
      kappa_baseline_sensitivity_rand_dates[[i]][[r]]<-list()
    }
    
    rm(r, table, matrix,k)
    
  }
  rm(i)
}

# View(kappa_baseline_sensitivity_rand_dates)

# unlist results in table


kappa_baseline_sensitivity_rand_dates_results<-data.frame()

for (i in drugs) {
  
  for (r in Rand_date_rules){
    
    try(
      table<-(data.frame(
        Drug = i,
        Rand_date_rule = r,
        kappa = unlist(kappa_baseline_sensitivity_rand_dates[[i]][[r]]$kappa$est),
        se = unlist(kappa_baseline_sensitivity_rand_dates[[i]][[r]]$kappa$se),
        kappa_CI_lower = unlist(kappa_baseline_sensitivity_rand_dates[[i]][[r]]$kappa$lower),
        kappa_CI_upper = unlist(kappa_baseline_sensitivity_rand_dates[[i]][[r]]$kappa$upper),
        
        pabak = unlist(kappa_baseline_sensitivity_rand_dates[[i]][[r]]$pabak$est),
        pabak_CI_lower = unlist(kappa_baseline_sensitivity_rand_dates[[i]][[r]]$pabak$lower),
        pabak_CI_upper = unlist(kappa_baseline_sensitivity_rand_dates[[i]][[r]]$pabak$upper),
        
        p_value = unlist(kappa_baseline_sensitivity_rand_dates[[i]][[r]]$z$p)
      ))
    )
    
    try(kappa_baseline_sensitivity_rand_dates_results%<>%bind_rows(table))
    rm(Rand_date_rule, r, table)
  }
  rm(Drug, i)
}


# View(kappa_baseline_sensitivity_rand_dates_results)  




#### aggregate summary (all drugs) ------



for (r in Rand_date_rules){
  
  baseline_combined_flags_sensitivity_rand_dates%>%
    filter(Rand_date_rule==r)%>%
    select(EPMA, CRF)%>%
    mutate(across(everything(), as.integer))%>%
    mutate(across(c(EPMA, CRF), ~replace_na(.x, 0)))->table
  
  matrix<-as.matrix(table(table))
  
  
  try(k<-epi.kappa(matrix, method="cohen", alternative = "greater", conf.level=0.95))
  
  if(exists("k"))
  {
    
    kappa_baseline_sensitivity_rand_dates[["Combined"]][[r]]<-k
    
  }
  else{
    kappa_baseline_sensitivity_rand_dates[["Combined"]][[r]]<-list()
  }
  
  rm(r, table, matrix,k)
  
}

# View(kappa_baseline_sensitivity_rand_dates)

# unlist results in table

for (r in Rand_date_rules){
  
  try(
    table<-(data.frame(
      Drug = "Combined",
      Rand_date_rule = r,
      kappa = unlist(kappa_baseline_sensitivity_rand_dates[["Combined"]][[r]]$kappa$est),
      se = unlist(kappa_baseline_sensitivity_rand_dates[["Combined"]][[r]]$kappa$se),
      kappa_CI_lower = unlist(kappa_baseline_sensitivity_rand_dates[["Combined"]][[r]]$kappa$lower),
      kappa_CI_upper = unlist(kappa_baseline_sensitivity_rand_dates[["Combined"]][[r]]$kappa$upper),
      
      pabak = unlist(kappa_baseline_sensitivity_rand_dates[["Combined"]][[r]]$pabak$est),
      pabak_CI_lower = unlist(kappa_baseline_sensitivity_rand_dates[["Combined"]][[r]]$pabak$lower),
      pabak_CI_upper = unlist(kappa_baseline_sensitivity_rand_dates[["Combined"]][[r]]$pabak$upper),
      
      p_value = unlist(kappa_baseline_sensitivity_rand_dates[["Combined"]][[r]]$z$p.value)
    ))
  )
  
  try(kappa_baseline_sensitivity_rand_dates_results%<>%bind_rows(table))
  
  rm(r, table)
}

View(kappa_baseline_sensitivity_rand_dates_results)

#### counts ------
baseline_combined_flags_sensitivity_rand_dates%>%
  group_by(Drug, Rand_date_rule)%>%
  summarise(pairs = n_distinct(Study_ID),
            positives = length(CRF[CRF=="1"]),
            negatives = length(CRF[CRF=="0"]),
            true_positives = length(CRF[CRF=="1" & EPMA=="1"]),
            true_negatives = length(CRF[CRF=="0" & EPMA=="0"]),
            false_positives = length(CRF[CRF=="0" & EPMA =="1"]),
            false_negatives = length(CRF[CRF =="1" & EPMA == "0"]),
            Sensitivity = round(true_positives/positives*100, 1),
            Specificity = round(true_negatives/negatives*100, 1),
            PPV = round(true_positives/(true_positives+false_positives)*100, 1),
            NPV = round(true_negatives/(true_negatives+false_negatives)*100, 1),
            Both = sum(EPMA=="1" & CRF=="1"),
            CRF_only = sum(EPMA=="0" & CRF=="1"),
            EPMA_only = sum(EPMA=="1" & CRF=="0"),
            Neither = sum(EPMA=="0" & CRF=="0"))%>%
  
  rbind(
    baseline_combined_flags_sensitivity_rand_dates%>%
      group_by(Rand_date_rule)%>%
      summarise(pairs = n(),
                positives = length(CRF[CRF=="1"]),
                negatives = length(CRF[CRF=="0"]),
                true_positives = length(CRF[CRF=="1" & EPMA=="1"]),
                true_negatives = length(CRF[CRF=="0" & EPMA=="0"]),
                false_positives = length(CRF[CRF=="0" & EPMA =="1"]),
                false_negatives = length(CRF[CRF =="1" & EPMA == "0"]),
                Sensitivity = round(true_positives/positives*100, 1),
                Specificity = round(true_negatives/negatives*100, 1),
                PPV = round(true_positives/(true_positives+false_positives)*100, 1),
                NPV = round(true_negatives/(true_negatives+false_negatives)*100, 1),
                Both = sum(EPMA=="1" & CRF=="1"),
                CRF_only = sum(EPMA=="0" & CRF=="1"),
                EPMA_only = sum(EPMA=="1" & CRF=="0"),
                Neither = sum(EPMA=="0" & CRF=="0"))%>%
      mutate(Drug="Combined")
    )%>%
  
  
  select(-c(positives, negatives, true_positives, true_negatives, false_positives, false_negatives))%>%
  rename(Individuals=pairs)->baseline_drugs_participant_counts_sensitivity_rand_dates_rules

# View(baseline_drugs_participant_counts_sensitivity_rand_dates_rules)




#### forest plot with counts -------


kappa_baseline_sensitivity_rand_dates_results%>%
  left_join(baseline_drugs_participant_counts_sensitivity_rand_dates_rules,by=c("Drug", "Rand_date_rule"))%>%
  mutate(Drug=fct_relevel(Drug, 
                          "Corticosteroids",
                          "Macrolides",
                          "Tocilizumab or sarilumab",
                          "Remdesivir",
                          "Antiplatelets",
                          "Oral anticoagulants",
                          "LMWH",
                          "Oxygen",
                          "Combined"
  ))%>%
  arrange(Drug, Rand_date_rule)%>%
  mutate(`Kappa (95% CI)` = paste0(round(kappa,2), " (", round(kappa_CI_lower, 2), " to ", round(kappa_CI_upper, 2), ")"))%>%
  mutate(`PABAK (95% CI)` = paste0(round(pabak,2), " (", round(pabak_CI_lower, 2), " to ", round(pabak_CI_upper, 2), ")"))%>%
  select(Drug, Rand_date_rule, Individuals, Both, CRF_only, EPMA_only, Neither, Sensitivity, Specificity, PPV, NPV, `Kappa (95% CI)`, `PABAK (95% CI)`, kappa, se, kappa_CI_lower, kappa_CI_upper, pabak, pabak_CI_lower, pabak_CI_upper)%>%
  add_row(Drug="Corticosteroids", .before=1)%>%
  add_row(Drug="Macrolides",.before=4)%>%
  add_row(Drug="Tocilizumab or sarilumab",.before=7)%>%
  add_row(Drug="Remdesivir",.before=10)%>%
  add_row(Drug="Antiplatelets",.before=13)%>%
  add_row(Drug="Oral anticoagulants",.before=16)%>%
  add_row(Drug="LMWH",.before=19)%>%
  add_row(Drug="Oxygen",.before=22)%>%
  add_row(Drug="Combined",.before=25)%>%
  rename(Total=Individuals,
         `CRF only` = CRF_only,
         `Both sources` = Both,
         `EPMA only` = EPMA_only,
         `Neither source` = Neither)%>%
  add_column(`Kappa`=NA, .after = "NPV")%>%
  add_column(`PABAK`=NA, .after = "Kappa (95% CI)")%>%
  mutate(`Kappa`=paste(rep(" ", 20), collapse = " "))%>%
  mutate(`PABAK`=paste(rep(" ", 20), collapse = " "))%>%
  mutate(Size=Total)%>%
  mutate(Drug=as.character(Drug))%>%
  mutate(Rand_date_rule=if_else(!is.na(Rand_date_rule), paste("   ", Rand_date_rule), Drug))%>%
  rename(`Drug\n(by randomisation date handling rule)`=Rand_date_rule)%>%
  select(-Drug)%>%
  mutate(across(-Size, ~ifelse(is.na(.), " ",.)))%>%
  mutate(across(c(kappa_CI_lower, kappa_CI_upper, kappa, pabak, pabak_CI_lower, pabak_CI_upper, se), ~as.numeric(.)))->forest_plot_table_baseline_sensitivity_rand_date_rules



forest_plot_table_baseline_sensitivity_rand_date_rules%>%
  slice_tail(n=2)%>%
  rename(`Data in the day of randomisation`=1)->forest_plot_table_baseline_sensitivity_rand_date_rules_combined

forest_plot_table_baseline_sensitivity_rand_date_rules%>%
  slice_head(n=-3)->forest_plot_table_baseline_sensitivity_rand_date_rules_by_drug











##### combined ----


forest_plot_rand_dates_combined<-forest(forest_plot_table_baseline_sensitivity_rand_date_rules_combined[,1:14],
                                       est=list(forest_plot_table_baseline_sensitivity_rand_date_rules_combined$kappa,
                                                forest_plot_table_baseline_sensitivity_rand_date_rules_combined$pabak),
                                       lower=list(forest_plot_table_baseline_sensitivity_rand_date_rules_combined$kappa_CI_lower,
                                                  forest_plot_table_baseline_sensitivity_rand_date_rules_combined$pabak_CI_lower),
                                       upper=list(forest_plot_table_baseline_sensitivity_rand_date_rules_combined$kappa_CI_upper,
                                                  forest_plot_table_baseline_sensitivity_rand_date_rules_combined$pabak_CI_upper),
                                       sizes=list(forest_plot_table_baseline_sensitivity_rand_date_rules_combined$se^2,
                                                  forest_plot_table_baseline_sensitivity_rand_date_rules_combined$se^2),
                                       ci_column = c(11,13),
                                       xlim=c(-1,1),
                                       arrow_lab = c("Negative agreement", "Positive agreement"),
                                       ref_line = 0)%>%
  insert_text(.,
              text = "Counts (participant/drug pairs)",
              col = 2:6,
              part = "header",
              gp = gpar(fontface = "bold"))%>%
  insert_text(.,
              text = "Agreement metrics",
              col = 7:13,
              part = "header",
              gp = gpar(fontface = "bold"))%>%
  gtable_add_grob(grobs=segmentsGrob(
    x0 = unit(0,"npc"), # horizontal origin
    y0 = unit(0,"npc"), # vertical origin
    x1 = unit(0,"npc"), # horizontal end
    y1 = unit(1,"npc"), # vertical end
    gp = gpar(lwd = 2.0)), # width
    t = 4,   # top end
    b = 6, # bottom end
    l = 8,  # left end
    r = 8 # right end
  )

forest_plot_rand_dates_combined

ggsave(filename="Outputs/Figures/Analysis/forestplot_baseline_sensitivity_rand_date_rules_combined.png", 
       plot=forest_plot_rand_dates_combined,
       dpi = "retina",
       units = "cm",
       width = get_wh(plot=forest_plot_rand_dates_combined, unit="cm"),
       limitsize = F
       # height = 30
)



##### by drug ------

forest_plot_rand_dates_by_drug<-forest(forest_plot_table_baseline_sensitivity_rand_date_rules_by_drug[,1:14],
                    est=list(forest_plot_table_baseline_sensitivity_rand_date_rules_by_drug$kappa,
                             forest_plot_table_baseline_sensitivity_rand_date_rules_by_drug$pabak),
                    lower=list(forest_plot_table_baseline_sensitivity_rand_date_rules_by_drug$kappa_CI_lower,
                               forest_plot_table_baseline_sensitivity_rand_date_rules_by_drug$pabak_CI_lower),
                    upper=list(forest_plot_table_baseline_sensitivity_rand_date_rules_by_drug$kappa_CI_upper,
                               forest_plot_table_baseline_sensitivity_rand_date_rules_by_drug$pabak_CI_upper),
                    sizes=list(forest_plot_table_baseline_sensitivity_rand_date_rules_by_drug$se^2,
                               forest_plot_table_baseline_sensitivity_rand_date_rules_by_drug$se^2),
                    ci_column = c(11,13),
                    xlim=c(-1,1),
                    arrow_lab = c("Negative agreement", "Positive agreement"),
                    ref_line = 0)%>%
  insert_text(.,
              text = "Counts (participants)",
              col = 2:6,
              part = "header",
              gp = gpar(fontface = "bold"))%>%
  insert_text(.,
              text = "Agreement metrics",
              col = 7:13,
              part = "header",
              gp = gpar(fontface = "bold"))%>%
  gtable_add_grob(grobs=segmentsGrob(
    x0 = unit(0,"npc"), # horizontal origin
    y0 = unit(0,"npc"), # vertical origin
    x1 = unit(0,"npc"), # horizontal end
    y1 = unit(1,"npc"), # vertical end
    gp = gpar(lwd = 2.0)), # width
    t = 4,   # top end
    b = 28,  # bottom end
    l = 8,  # left end
    r = 8 # right end
  )

forest_plot_rand_dates_by_drug

ggsave(filename="Outputs/Figures/Analysis/forestplot_baseline_sensitivity_rand_date_rules_by_drug.png", 
       plot=forest_plot_rand_dates_by_drug,
       dpi = "retina",
       units = "cm",
       width = get_wh(plot=forest_plot_rand_dates_by_drug, unit="cm"),
       limitsize = F
       # height = 30
)


## administration date fields ------

#### initial transformations ------

##### generate new EPMA flags with all available date fields

##### drugs
administration_drug_records<-
  data_administration%>%
  select(PRESCRIPTION_ID_HASHED, 
         ADMINISTRATION_ID_HASHED, 
         Study_ID, 
         MEDICATION_NAME, 
         ADMINISTERED_DATE_TIME, 
         RECORDED_DATE_TIME,
         SCHEDULED_DATE_TIME, 
         LAST_UPDATED_DATE)%>%
  mutate(administered_date=as.Date(ADMINISTERED_DATE_TIME, format="%T-%m-%d"),
         recorded_date=as.Date(RECORDED_DATE_TIME, format="%T-%m-%d"),
         scheduled_date=as.Date(SCHEDULED_DATE_TIME, format="%T-%m-%d"),
         last_updated_date=as.Date(LAST_UPDATED_DATE, format="%T-%m-%d"))%>%
  distinct(Study_ID, MEDICATION_NAME, PRESCRIPTION_ID_HASHED, administered_date, recorded_date, scheduled_date, last_updated_date, ADMINISTRATION_ID_HASHED)%>%
  mutate(any_date = case_when(is.na(administered_date) ~ scheduled_date,
                              is.na(scheduled_date) ~ recorded_date,
                              is.na(recorded_date) ~ last_updated_date,
                              !is.na(administered_date) ~ administered_date))%>%
  # Extract drug flags
  mutate(Aspirin=if_else(str_detect(MEDICATION_NAME, regex("aspirin", ignore_case=T)), 1, 0),
         Corticosteroids = if_else(str_detect(MEDICATION_NAME, regex("dexamethasone|prednisolone|hydrocortisone|methylprednisolone", ignore_case = T)), "1", "0"),
         Baricitinib =if_else(str_detect(MEDICATION_NAME, regex("baricitinib", ignore_case=T)), 1, 0),
         `Tocilizumab or sarilumab`= if_else(str_detect(MEDICATION_NAME, regex("tocilizumab|sarilumab", ignore_case=T)), 1, 0),
         Hydroxychloroquine  = if_else(str_detect(MEDICATION_NAME, regex("Hydroxychloroquine", ignore_case=T)), 1, 0),
         `Lopinavir-ritonavir`= if_else(str_detect(MEDICATION_NAME, regex("LOPINAVIR|KALETRA", ignore_case=T)), 1, 0),
         Azithromycin= if_else(str_detect(MEDICATION_NAME, regex("azithromycin", ignore_case=T)), 1, 0),
         `Macrolides`= if_else(str_detect(MEDICATION_NAME, regex("azithromycin|erythromycin|clarithromycin", ignore_case=T)), 1, 0),
         # `Other macrolides`= if_else(str_detect(MEDICATION_NAME, regex("erythromycin|clarithromycin", ignore_case=T)), 1, 0),
         Remdesivir= if_else(str_detect(MEDICATION_NAME, regex("remdesivir", ignore_case=T)), 1, 0),
         Colchicine= if_else(str_detect(MEDICATION_NAME, regex("colchicine", ignore_case=T)), 1, 0),
         `REGN antibodies`= if_else(str_detect(MEDICATION_NAME, regex("REGN|casirivimab|imdevimab|ronapreve", ignore_case=T)), 1, 0),
         `Oral anticoagulants`= if_else(str_detect(MEDICATION_NAME, regex("warfarin|rivaroxaban|apixaban|dabigatran|edoxaban|ACENOCOUMAROL|Phenindione|Phenprocoumon", ignore_case=T)), 1, 0),
         Antiplatelets= if_else(str_detect(MEDICATION_NAME, regex("aspirin|clopidogrel|ticagrelor|dipyridamole|eptifibatide|prasugrel|ticlopidine", ignore_case=T)), 1, 0),
         Immunoglobulin = if_else(str_detect(MEDICATION_NAME, regex("immunoglob|ivig|iv ig", ignore_case=T)), 1, 0),
         LMWH = if_else(str_detect(MEDICATION_NAME, regex("dalteparin|tinzaparin|enoxaparin", ignore_case=T)), 1, 0))%>%
  
  select(-c(MEDICATION_NAME))%>%
  mutate(across(everything(), ~as.character(.)))%>%
  pivot_longer(-c(Study_ID, administered_date, recorded_date, scheduled_date, last_updated_date, any_date, PRESCRIPTION_ID_HASHED, ADMINISTRATION_ID_HASHED), names_to = "Drug", values_to = "Flag")%>%
  distinct(Study_ID, administered_date, recorded_date, scheduled_date, last_updated_date, any_date, Drug, Flag, .keep_all=T)



##### oxygen

oxygen_administration_records<-
  data_administration%>%
  select(PRESCRIPTION_ID_HASHED, 
         ADMINISTRATION_ID_HASHED, 
         Study_ID, 
         MEDICATION_NAME, 
         ADMINISTERED_DATE_TIME, 
         RECORDED_DATE_TIME,
         SCHEDULED_DATE_TIME, 
         LAST_UPDATED_DATE)%>%
  mutate(administered_date=as.Date(ADMINISTERED_DATE_TIME, format="%T-%m-%d"),
         recorded_date=as.Date(RECORDED_DATE_TIME, format="%T-%m-%d"),
         scheduled_date=as.Date(SCHEDULED_DATE_TIME, format="%T-%m-%d"),
         last_updated_date=as.Date(LAST_UPDATED_DATE, format="%T-%m-%d"))%>%
  distinct(Study_ID, MEDICATION_NAME, PRESCRIPTION_ID_HASHED, administered_date, recorded_date, scheduled_date, last_updated_date, ADMINISTRATION_ID_HASHED)%>%
  
  mutate(any_date = case_when(is.na(administered_date) ~ scheduled_date,
                              is.na(scheduled_date) ~ recorded_date,
                              is.na(recorded_date) ~ last_updated_date,
                              !is.na(administered_date) ~ administered_date))%>%
  
  
  mutate(Oxygen=if_else(str_detect(MEDICATION_NAME, regex("oxygen", ignore_case=T)), 1, 0))%>%
  mutate(across(everything(), ~as.character(.)))%>%
  select(-MEDICATION_NAME)%>%
  pivot_longer(-c(Study_ID, administered_date, recorded_date, scheduled_date, last_updated_date, any_date, PRESCRIPTION_ID_HASHED, ADMINISTRATION_ID_HASHED), names_to = "Drug", values_to = "Flag")%>%
  distinct(Study_ID, administered_date, recorded_date, scheduled_date, last_updated_date, any_date, Drug, Flag, .keep_all=T)


### Merge all

epma_drug_flags_with_all_dates<-rbind(administration_drug_records,
        oxygen_administration_records)%>%
  filter(Study_ID %in% study_population)

rm(administration_drug_records, oxygen_administration_records)



baseline_epma_flags_sensitivity_all_dates<-
  epma_drug_flags_with_all_dates%>%
  filter(Drug %in% 
           (pre_rand_drugs_crf%>%
              distinct(Drug)%>%.[[1]]))%>%
  pivot_longer(c(administered_date, recorded_date, scheduled_date, last_updated_date, any_date), names_to="Field", values_to="date")%>%
  left_join(censoring_dates%>%mutate(Study_ID=as.character(Study_ID)))%>%
  group_by(Study_ID)%>%
  filter(any(date>=admission_date & date<=right_censoring_date_death_discharge_28d))%>% # restrict to people who have had a record within their randomisation admission
  group_by(Study_ID, Drug, Field)%>%
  summarise(EPMA=if_else(any(Flag=="1" & date<=rand_date & date>=admission_date), 1, 0))%>% # positive flag if record occurs before randomisation but after the current date of admission (therefore in current admission)
  right_join(
    expand.grid(Study_ID=study_population,
                Drug=pre_rand_drugs_crf%>%distinct(Drug)%>%.[[1]],
                Field=c("administered_date", "recorded_date", "scheduled_date", "last_updated_date", "any_date"))%>%
      mutate(Study_ID=as.character(Study_ID)),
    by=c("Study_ID", "Drug", "Field"))%>%
  mutate(EPMA=replace_na(EPMA, 0))%>%
  ungroup()
  


#### Join flags from both sources and prepare for calculations

baseline_combined_flags_sensitivity_all_dates<-pre_rand_drugs_crf%>%
  rename(CRF=Flag)%>%
  left_join(baseline_epma_flags_sensitivity_all_dates)%>%
  select(Study_ID, Drug, EPMA, CRF, Field)


#### Calculate agreement --------


kappa_baseline_sensitivity_date_fields<-list()

drugs<-baseline_combined_flags_sensitivity_all_dates%>%
  distinct(Drug)%>%
  # filter(Drug!="Oxygen")%>%
  .[[1]]

Date_fields<-c("administered_date", "recorded_date", "scheduled_date", "last_updated_date", "any_date")


for (i in drugs){  
  
  for(d in Date_fields){
    
    baseline_combined_flags_sensitivity_all_dates%>%
      filter(Drug==i,
             Field==d)%>%
      select(EPMA, CRF)%>%
      mutate(across(everything(), as.integer))%>%
      mutate(across(c(EPMA, CRF), ~replace_na(.x, 0)))->table
    
    matrix<-as.matrix(table(table))
    
    
    try(k<-epi.kappa(matrix, method="cohen", alternative = "greater", conf.level=0.95))
    
    if(exists("k"))
    {
      
      kappa_baseline_sensitivity_date_fields[[i]][[d]]<-k
      
    }
    else{
      kappa_baseline_sensitivity_date_fields[[i]][[d]]<-list()
    }
    
    rm(r, table, matrix,k)
    
  }
  rm(i)
}

View(kappa_baseline_sensitivity_date_fields)

# unlist results in table


kappa_baseline_sensitivity_date_fields_results<-data.frame()

for (i in drugs) {
  
  for (d in Date_fields){
    
    try(
      table<-(data.frame(
        Drug = i,
        Field = d,
        kappa = unlist(kappa_baseline_sensitivity_date_fields[[i]][[d]]$kappa$est),
        se = unlist(kappa_baseline_sensitivity_date_fields[[i]][[d]]$kappa$se),
        kappa_CI_lower = unlist(kappa_baseline_sensitivity_date_fields[[i]][[d]]$kappa$lower),
        kappa_CI_upper = unlist(kappa_baseline_sensitivity_date_fields[[i]][[d]]$kappa$upper),
        
        pabak = unlist(kappa_baseline_sensitivity_date_fields[[i]][[d]]$pabak$est),
        pabak_CI_lower = unlist(kappa_baseline_sensitivity_date_fields[[i]][[d]]$pabak$lower),
        pabak_CI_upper = unlist(kappa_baseline_sensitivity_date_fields[[i]][[d]]$pabak$upper),
        
        p_value = unlist(kappa_baseline_sensitivity_date_fields[[i]][[d]]$z$p)
      ))
    )
    
    try(kappa_baseline_sensitivity_date_fields_results%<>%bind_rows(table))
    
    rm(d, table)
  }
  rm(i)
}


View(kappa_baseline_sensitivity_date_fields_results)  


#### aggregate summary (all drugs) ------



for (d in Date_fields){
  
  baseline_combined_flags_sensitivity_all_dates%>%
    filter(Field==d)%>%
    select(EPMA, CRF)%>%
    mutate(across(everything(), as.integer))->table
  
  matrix<-as.matrix(table(table))
  
  
  try(k<-epi.kappa(matrix, method="cohen", alternative = "greater", conf.level=0.95))
  
  if(exists("k"))
  {
    
    kappa_baseline_sensitivity_date_fields[["Combined"]][[d]]<-k
    
  }
  else{
    kappa_baseline_sensitivity_date_fields[["Combined"]][[d]]<-list()
  }
  
  rm(d, table, k, matrix)
  
}

View(kappa_baseline_sensitivity_date_fields)

# unlist results in table

for (d in Date_fields){
  
  try(
    table<-(data.frame(
      Drug = "Combined",
      Field = d,
      kappa = unlist(kappa_baseline_sensitivity_date_fields[["Combined"]][[d]]$kappa$est),
      se = unlist(kappa_baseline_sensitivity_date_fields[["Combined"]][[d]]$kappa$se),
      kappa_CI_lower = unlist(kappa_baseline_sensitivity_date_fields[["Combined"]][[d]]$kappa$lower),
      kappa_CI_upper = unlist(kappa_baseline_sensitivity_date_fields[["Combined"]][[d]]$kappa$upper),
      
      pabak = unlist(kappa_baseline_sensitivity_date_fields[["Combined"]][[d]]$pabak$est),
      pabak_CI_lower = unlist(kappa_baseline_sensitivity_date_fields[["Combined"]][[d]]$pabak$lower),
      pabak_CI_upper = unlist(kappa_baseline_sensitivity_date_fields[["Combined"]][[d]]$pabak$upper),
      
      p_value = unlist(kappa_baseline_sensitivity_date_fields[["Combined"]][[d]]$z$p.value)
    ))
  )
  
  try(kappa_baseline_sensitivity_date_fields_results%<>%bind_rows(table))
  
}

#### counts ------
baseline_combined_flags_sensitivity_all_dates%>%
  group_by(Drug, Field)%>%
  summarise(pairs = n_distinct(Study_ID),
            positives = length(CRF[CRF=="1"]),
            negatives = length(CRF[CRF=="0"]),
            true_positives = length(CRF[CRF=="1" & EPMA=="1"]),
            true_negatives = length(CRF[CRF=="0" & EPMA=="0"]),
            false_positives = length(CRF[CRF=="0" & EPMA =="1"]),
            false_negatives = length(CRF[CRF =="1" & EPMA == "0"]),
            Sensitivity = round(true_positives/positives*100, 1),
            Specificity = round(true_negatives/negatives*100, 1),
            PPV = round(true_positives/(true_positives+false_positives)*100, 1),
            NPV = round(true_negatives/(true_negatives+false_negatives)*100, 1),
            Both = sum(EPMA=="1" & CRF=="1"),
            CRF_only = sum(EPMA=="0" & CRF=="1"),
            EPMA_only = sum(EPMA=="1" & CRF=="0"),
            Neither = sum(EPMA=="0" & CRF=="0"))%>%
  select(-c(positives, negatives, true_positives, true_negatives, false_positives, false_negatives))%>%
  rbind(
    baseline_combined_flags_sensitivity_all_dates%>%
      mutate(Drug="Combined")%>%
  group_by(Drug, Field)%>%
  summarise(pairs = n(),
            positives = length(CRF[CRF=="1"]),
            negatives = length(CRF[CRF=="0"]),
            true_positives = length(CRF[CRF=="1" & EPMA=="1"]),
            true_negatives = length(CRF[CRF=="0" & EPMA=="0"]),
            false_positives = length(CRF[CRF=="0" & EPMA =="1"]),
            false_negatives = length(CRF[CRF =="1" & EPMA == "0"]),
            Sensitivity = round(true_positives/positives*100, 1),
            Specificity = round(true_negatives/negatives*100, 1),
            PPV = round(true_positives/(true_positives+false_positives)*100, 1),
            NPV = round(true_negatives/(true_negatives+false_negatives)*100, 1),
            Both = sum(EPMA=="1" & CRF=="1"),
            CRF_only = sum(EPMA=="0" & CRF=="1"),
            EPMA_only = sum(EPMA=="1" & CRF=="0"),
            Neither = sum(EPMA=="0" & CRF=="0"))%>%
  select(-c(positives, negatives, true_positives, true_negatives, false_positives, false_negatives)))%>%
  rename(Individuals=pairs)->baseline_drugs_participant_counts_sensitivity_date_fields

# View(baseline_drugs_participant_counts_sensitivity_date_fields)

baseline_drugs_participant_counts_sensitivity_date_fields%<>%
  filter(!is.na(Field))


#### forest plot with counts -------


kappa_baseline_sensitivity_date_fields_results%>%
  left_join(baseline_drugs_participant_counts_sensitivity_date_fields,by=c("Drug", "Field"))%>%

  mutate(Drug=fct_relevel(Drug, 
                          "Corticosteroids",
                          "Macrolides",
                          "Tocilizumab or sarilumab",
                          "Remdesivir",
                          "Antiplatelets",
                          "Oral anticoagulants",
                          "LMWH",
                          "Oxygen"
  ))%>%
  mutate(Field=case_when(Field=="administered_date" ~ "Administered",
                         Field=="recorded_date" ~ "Recorded",
                         Field=="scheduled_date" ~ "Scheduled",
                         Field=="last_updated_date" ~ "Last updated",
                         Field=="any_date" ~ "Any*"))%>%
  mutate(Field=factor(Field,levels=c("Scheduled", "Administered", "Recorded", "Last updated", "Any*")))%>%
  arrange(Drug, Field)%>%
  mutate(`Kappa (95% CI)` = paste0(round(kappa,2), " (", round(kappa_CI_lower, 2), " to ", round(kappa_CI_upper, 2), ")"))%>%
  mutate(`PABAK (95% CI)` = paste0(round(pabak,2), " (", round(pabak_CI_lower, 2), " to ", round(pabak_CI_upper, 2), ")"))%>%
  select(Drug, Field, Individuals, Both, CRF_only, EPMA_only, Neither, Sensitivity, Specificity, PPV, NPV, `Kappa (95% CI)`, `PABAK (95% CI)`, kappa, se, kappa_CI_lower, kappa_CI_upper, pabak, pabak_CI_lower, pabak_CI_upper)%>%
  add_row(Drug="Corticosteroids", .before=1)%>%
  add_row(Drug="Macrolides",.before=7)%>%
  add_row(Drug="Tocilizumab or sarilumab",.before=13)%>%
  add_row(Drug="Remdesivir",.before=19)%>%
  add_row(Drug="Antiplatelets",.before=25)%>%
  add_row(Drug="Oral anticoagulants",.before=31)%>%
  add_row(Drug="LMWH",.before=37)%>%
  add_row(Drug="Oxygen",.before=43)%>%
  add_row(Drug="Combined",.before=49)%>%
  rename(Total=Individuals,
         `CRF only` = CRF_only,
         `Both sources` = Both,
         `EPMA only` = EPMA_only,
         `Neither source` = Neither)%>%
  add_column(`Kappa`=NA, .after = "NPV")%>%
  add_column(`PABAK`=NA, .after = "Kappa (95% CI)")%>%
  mutate(`Kappa`=paste(rep(" ", 20), collapse = " "))%>%
  mutate(`PABAK`=paste(rep(" ", 20), collapse = " "))%>%
  mutate(Size=Total)%>%
  mutate(Drug=as.character(Drug))%>%
  mutate(Field=if_else(!is.na(Field), paste("   ", Field), Drug))%>%
  rename(`Drug\n(by administration date field)`=Field)%>%
  select(-Drug)%>%
  mutate(across(-Size, ~ifelse(is.na(.), " ",.)))%>%
  mutate(across(c(kappa_CI_lower, kappa_CI_upper, kappa, pabak, pabak_CI_lower, pabak_CI_upper, se), ~as.numeric(.)))->forest_plot_table_baseline_sensitivity_date_fields


## separate combined and by drug

forest_plot_table_baseline_sensitivity_date_fields%>%
  slice_tail(n=5)%>%
  rename(`Date field`=1)->forest_plot_table_baseline_sensitivity_date_fields_combined

forest_plot_table_baseline_sensitivity_date_fields%>%
  slice_head(n=-6)->forest_plot_table_baseline_sensitivity_date_fields_by_drug



##### combined plot ------
forest_plot_date_fields_combined<-forest(forest_plot_table_baseline_sensitivity_date_fields_combined[,1:14],
                                est=list(forest_plot_table_baseline_sensitivity_date_fields_combined$kappa,
                                         forest_plot_table_baseline_sensitivity_date_fields_combined$pabak),
                                lower=list(forest_plot_table_baseline_sensitivity_date_fields_combined$kappa_CI_lower,
                                           forest_plot_table_baseline_sensitivity_date_fields_combined$pabak_CI_lower),
                                upper=list(forest_plot_table_baseline_sensitivity_date_fields_combined$kappa_CI_upper,
                                           forest_plot_table_baseline_sensitivity_date_fields_combined$pabak_CI_upper),
                                sizes=list(forest_plot_table_baseline_sensitivity_date_fields_combined$se^2,
                                           forest_plot_table_baseline_sensitivity_date_fields_combined$se^2),
                                ci_column = c(11,13),
                                xlim=c(-1,1),
                                arrow_lab = c("Negative agreement", "Positive agreement"),
                                ref_line = 0,
                                footnote = "*Using any non-missing date in a hierarchical sequence: 1) administered 2) scheduled 3) recorded 4) last updated",
                                theme = forest_theme(footnote_cex=1))%>%
  insert_text(.,
              text = "Counts (participant/drug pairs)",
              col = 2:6,
              part = "header",
              gp = gpar(fontface = "bold"))%>%
  insert_text(.,
              text = "Agreement metrics",
              col = 7:13,
              part = "header",
              gp = gpar(fontface = "bold"))%>%
  gtable_add_grob(grobs=segmentsGrob(
    x0 = unit(0,"npc"), # horizontal origin
    y0 = unit(0,"npc"), # vertical origin
    x1 = unit(0,"npc"), # horizontal end
    y1 = unit(1,"npc"), # vertical end
    gp = gpar(lwd = 2.0)), # width
    t = 4,   # top end
    b = 9,  # bottom end
    l = 8,  # left end
    r = 8 # right end
  )

forest_plot_date_fields_combined

ggsave(filename="Outputs/Figures/Analysis/forestplot_baseline_sensitivity_date_fields_combined.png", 
       plot=forest_plot_date_fields_combined,
       dpi = "retina",
       units = "cm",
       width = get_wh(plot=forest_plot_date_fields_combined, unit="cm"),
       limitsize = F
       # height = 30
)


##### by drug -----


forest_plot_date_fields_by_drug<-forest(forest_plot_table_baseline_sensitivity_date_fields_by_drug[,1:14],
                                         est=list(forest_plot_table_baseline_sensitivity_date_fields_by_drug$kappa,
                                                  forest_plot_table_baseline_sensitivity_date_fields_by_drug$pabak),
                                         lower=list(forest_plot_table_baseline_sensitivity_date_fields_by_drug$kappa_CI_lower,
                                                    forest_plot_table_baseline_sensitivity_date_fields_by_drug$pabak_CI_lower),
                                         upper=list(forest_plot_table_baseline_sensitivity_date_fields_by_drug$kappa_CI_upper,
                                                    forest_plot_table_baseline_sensitivity_date_fields_by_drug$pabak_CI_upper),
                                         sizes=list(forest_plot_table_baseline_sensitivity_date_fields_by_drug$se^2,
                                                    forest_plot_table_baseline_sensitivity_date_fields_by_drug$se^2),
                                         ci_column = c(11,13),
                                         xlim=c(-1,1),
                                         arrow_lab = c("Negative agreement", "Positive agreement"),
                                         ref_line = 0,
                                         footnote = "*Using any non-missing date in a hierarchical sequence: 1) administered 2) scheduled 3) recorded 4) last updated",
                                         theme = forest_theme(footnote_cex=1))%>%
  insert_text(.,
              text = "Counts (participants)",
              col = 2:6,
              part = "header",
              gp = gpar(fontface = "bold"))%>%
  insert_text(.,
              text = "Agreement metrics",
              col = 7:13,
              part = "header",
              gp = gpar(fontface = "bold"))%>%
  gtable_add_grob(grobs=segmentsGrob(
    x0 = unit(0,"npc"), # horizontal origin
    y0 = unit(0,"npc"), # vertical origin
    x1 = unit(0,"npc"), # horizontal end
    y1 = unit(1,"npc"), # vertical end
    gp = gpar(lwd = 2.0)), # width
    t = 4,   # top end
    b = 52,  # bottom end
    l = 8,  # left end
    r = 8 # right end
  )

forest_plot_date_fields_by_drug

ggsave(filename="Outputs/Figures/Analysis/forestplot_baseline_sensitivity_date_fields_by_drug.png", 
       plot=forest_plot_date_fields_by_drug,
       dpi = "retina",
       units = "cm",
       width = get_wh(plot=forest_plot_date_fields_by_drug, unit="cm"),
       limitsize = F
       # height = 30
)







## 4. Agreement  --------

### 4.1 Baseline exposure ------

##### data transformations -----

baseline_epma_flags_overall<-
  epma_drug_flags%>%
  filter(Drug %in% 
           (pre_rand_drugs_crf%>%
              distinct(Drug)%>%.[[1]]))%>% # restrict to drugs of interest (from the CRF)
  
  left_join(censoring_dates%>%mutate(Study_ID=as.character(Study_ID)))%>%
  filter(date>=admission_date,
         date<=right_censoring_date_death_discharge_28d)%>% # restrict to people who have had a record within their randomisation admission
  group_by(Study_ID, Drug)%>%
  summarise(EPMA=if_else(any(Flag=="1" & date>=admission_date & date<=rand_date), 1, 0))%>% # positive flag if record occurs before or in the day of randomisation but after the current date of admission (therefore in current admission)
  mutate(EPMA=replace_na(EPMA, 0))%>%
  ungroup()

# fill with 0 for people with absent flags

baseline_epma_flags_overall%<>%
  right_join(
    expand.grid(Study_ID=study_population,
                Drug=pre_rand_drugs_crf%>%distinct(Drug)%>%.[[1]])%>%
      mutate(Study_ID=as.character(Study_ID)),
    by=c("Study_ID", "Drug")
  )%>%
  mutate(EPMA=replace_na(EPMA, 0))


#### Join flags from both sources and prepare for calculations

baseline_combined_flags<-pre_rand_drugs_crf%>%
  filter(Study_ID %in% baseline_epma_flags_overall$Study_ID)%>% # restrict CRF data to people with available EPMA data for the admission of randomisation
  rename(CRF=Flag)%>%
  left_join(baseline_epma_flags_overall)%>%
  left_join(baseline_crf_england%>%select(Study_ID, resp_status))%>%
  select(Study_ID, Drug, EPMA, CRF, resp_status)%>%
  group_by(Study_ID, Drug, resp_status)%>%
  summarise(EPMA=if_else(any(EPMA==1), 1, 0),
            CRF=if_else(any(CRF==1), 1, 0))%>%
  ungroup()




##### calculate agreement ------

# by resp status, except oxygen

kappa_baseline<-list()

drugs<-baseline_combined_flags%>%
  distinct(Drug)%>%
  filter(Drug!="Oxygen")%>%
  .[[1]]

resp_status<-baseline_combined_flags%>%distinct(resp_status)%>%mutate(resp_status=as.character(resp_status))%>%.[[1]]

for (i in drugs) {
  
  for (r in resp_status) {
    
    baseline_combined_flags%>%
      filter(Drug==i,
             resp_status==r)%>%
      select(EPMA, CRF)%>%
      mutate(across(everything(), as.integer))%>%
      mutate(across(c(EPMA, CRF), ~replace_na(.x, 0)))->table
    
    matrix<-as.matrix(table(table))
    
    try(k<-epi.kappa(matrix, method="cohen", alternative = "greater", conf.level=0.95))
    
    if(exists("k"))
    {
      
      kappa_baseline[[i]][[r]]<-k
      
    }
    else{
      kappa_baseline[[i]][[r]]<-list()
    }
    
    rm(r, table, k, matrix)
    
  }
  rm(i)
}




View(kappa_baseline)    



##### Unlist results for table

kappa_baseline_results<-data.frame()


for (i in drugs) {
  Drug<-i
  
  for (r in resp_status){
    
    
    resp <- r
    
    try(
      table<-(
        data.frame(
          Drug = Drug,
          resp_status = resp,
          kappa = unlist(kappa_baseline[[i]][[r]]$kappa$est),
          se = unlist(kappa_baseline[[i]][[r]]$kappa$se),
          kappa_CI_lower = unlist(kappa_baseline[[i]][[r]]$kappa$lower),
          kappa_CI_upper = unlist(kappa_baseline[[i]][[r]]$kappa$upper),
          
          pabak = unlist(kappa_baseline[[i]][[r]]$pabak$est),
          pabak_CI_lower = unlist(kappa_baseline[[i]][[r]]$pabak$lower),
          pabak_CI_upper = unlist(kappa_baseline[[i]][[r]]$pabak$upper),
          
          p_value = unlist(kappa_baseline[[i]][[r]]$z$p.value)
        ))
    )
    
    try(kappa_baseline_results%<>%bind_rows(table))
    rm(resp, r, table)
  }
  rm(Drug, i)
}


View(kappa_baseline_results)


###### add aggregates (all resp statuses) ------

drugs<-baseline_combined_flags%>%
  distinct(Drug)%>%
  .[[1]]



# calculate agreement

for (i in drugs){  
  r<-"Aggregate"
  
  
  baseline_combined_flags%>%
    filter(Drug==i)%>%
    select(EPMA, CRF)%>%
    mutate(across(everything(), as.integer))%>%
    mutate(across(c(EPMA, CRF), ~replace_na(.x, 0)))->table
  
  matrix<-as.matrix(table(table))
  
  try(k<-epi.kappa(matrix, method="cohen", alternative = "greater", conf.level=0.95))
  
  if(exists("k"))
  {
    
    kappa_baseline[[i]][[r]]<-k
    
  }
  else{
    kappa_baseline[[i]][[r]]<-list()
  }
  
  rm(i, table, k)
  
}


# unlist results in table


for (i in drugs) {
  r<-"Aggregate"
  
  try(
    table<-(data.frame(
      Drug = i,
      resp_status = "Aggregate",
      kappa = unlist(kappa_baseline[[i]][[r]]$kappa$est),
      se = unlist(kappa_baseline[[i]][[r]]$kappa$se),
      kappa_CI_lower = unlist(kappa_baseline[[i]][[r]]$kappa$lower),
      kappa_CI_upper = unlist(kappa_baseline[[i]][[r]]$kappa$upper),
      
      pabak = unlist(kappa_baseline[[i]][[r]]$pabak$est),
      pabak_CI_lower = unlist(kappa_baseline[[i]][[r]]$pabak$lower),
      pabak_CI_upper = unlist(kappa_baseline[[i]][[r]]$pabak$upper),
      
      p_value = unlist(kappa_baseline[[i]][[r]]$z$p.value)
    ))
  )
  
  try(kappa_baseline_results%<>%bind_rows(table))
}



View(kappa_baseline_results)  

##### aggregate summary (all drugs) ------

###### by resp status ------



  for (r in resp_status) {
    
    baseline_combined_flags%>%
      filter(resp_status==r,
             Drug!="Oxygen")%>%
      select(EPMA, CRF)%>%
      mutate(across(everything(), as.integer))%>%
      mutate(across(c(EPMA, CRF), ~replace_na(.x, 0)))->table
    
    matrix<-as.matrix(table(table))
    
    try(k<-epi.kappa(matrix, method="cohen", alternative = "greater", conf.level=0.95))
    
    if(exists("k"))
    {
      
      kappa_baseline[["Combined (excluding oxygen)"]][[r]]<-k
      
    }
    else{
      kappa_baseline[["Combined (excluding oxygen)"]][[r]]<-list()
    }
    
    rm(r, table, k, matrix)
    
  }
 



View(kappa_baseline)    



##### Unlist results for table

  for (r in resp_status){
    
    
    resp <- r
    
    try(
      table<-(
        data.frame(
          Drug = "Combined (excluding oxygen)",
          resp_status = resp,
          kappa = unlist(kappa_baseline[["Combined (excluding oxygen)"]][[r]]$kappa$est),
          se = unlist(kappa_baseline[["Combined (excluding oxygen)"]][[r]]$kappa$se),
          kappa_CI_lower = unlist(kappa_baseline[["Combined (excluding oxygen)"]][[r]]$kappa$lower),
          kappa_CI_upper = unlist(kappa_baseline[["Combined (excluding oxygen)"]][[r]]$kappa$upper),
          
          pabak = unlist(kappa_baseline[["Combined (excluding oxygen)"]][[r]]$pabak$est),
          pabak_CI_lower = unlist(kappa_baseline[["Combined (excluding oxygen)"]][[r]]$pabak$lower),
          pabak_CI_upper = unlist(kappa_baseline[["Combined (excluding oxygen)"]][[r]]$pabak$upper),
          
          p_value = unlist(kappa_baseline[["Combined (excluding oxygen)"]][[r]]$z$p.value)
        ))
    )
    
    try(kappa_baseline_results%<>%bind_rows(table))
    rm(resp, r, table)
  }

View(kappa_baseline_results)


















###### aggregate (all resp statuses) --------

baseline_combined_flags%>%
    select(EPMA, CRF)%>%
    mutate(across(everything(), as.integer))->table
  
matrix<-as.matrix(table(table))
  
  
kappa_baseline[["Combined"]]<-epi.kappa(matrix, method="cohen", alternative = "greater", conf.level=0.95)

rm(matrix, table)    


baseline_combined_flags%>%
  filter(Drug!="Oxygen")%>%
  select(EPMA, CRF)%>%
  mutate(across(everything(), as.integer))->table

matrix<-as.matrix(table(table))


kappa_baseline[["Combined (excluding oxygen)"]]<-epi.kappa(matrix, method="cohen", alternative = "greater", conf.level=0.95)

rm(matrix, table)    








View(kappa_baseline)

# unlist results in table

table<-(data.frame(
      Drug = "Combined",
      resp_status="Aggregate",
      kappa = unlist(kappa_baseline[["Combined"]]$kappa$est),
      se = unlist(kappa_baseline[["Combined"]]$kappa$se),
      kappa_CI_lower = unlist(kappa_baseline[["Combined"]]$kappa$lower),
      kappa_CI_upper = unlist(kappa_baseline[["Combined"]]$kappa$upper),
      
      pabak = unlist(kappa_baseline[["Combined"]]$pabak$est),
      pabak_CI_lower = unlist(kappa_baseline[["Combined"]]$pabak$lower),
      pabak_CI_upper = unlist(kappa_baseline[["Combined"]]$pabak$upper),
      
      p_value = unlist(kappa_baseline[["Combined"]]$z$p.value)
    ))

kappa_baseline_results%<>%bind_rows(table)

rm(table)


table<-(data.frame(
  Drug = "Combined (excluding oxygen)",
  resp_status="Aggregate",
  kappa = unlist(kappa_baseline[["Combined (excluding oxygen)"]]$kappa$est),
  se = unlist(kappa_baseline[["Combined (excluding oxygen)"]]$kappa$se),
  kappa_CI_lower = unlist(kappa_baseline[["Combined (excluding oxygen)"]]$kappa$lower),
  kappa_CI_upper = unlist(kappa_baseline[["Combined (excluding oxygen)"]]$kappa$upper),
  
  pabak = unlist(kappa_baseline[["Combined (excluding oxygen)"]]$pabak$est),
  pabak_CI_lower = unlist(kappa_baseline[["Combined (excluding oxygen)"]]$pabak$lower),
  pabak_CI_upper = unlist(kappa_baseline[["Combined (excluding oxygen)"]]$pabak$upper),
  
  p_value = unlist(kappa_baseline[["Combined (excluding oxygen)"]]$z$p.value)
))

kappa_baseline_results%<>%bind_rows(table)

rm(table)

View(kappa_baseline_results)
























##### counts ------

# counts by resp status
baseline_combined_flags%>%
  filter(Drug!="Oxygen")%>%
  group_by(Drug, resp_status)%>%
  summarise(pairs = n_distinct(Study_ID),
            positives = length(CRF[CRF=="1"]),
            negatives = length(CRF[CRF=="0"]),
            true_positives = length(CRF[CRF=="1" & EPMA=="1"]),
            true_negatives = length(CRF[CRF=="0" & EPMA=="0"]),
            false_positives = length(CRF[CRF=="0" & EPMA =="1"]),
            false_negatives = length(CRF[CRF =="1" & EPMA == "0"]),
            Sensitivity = round(true_positives/positives*100, 1),
            Specificity = round(true_negatives/negatives*100, 1),
            PPV = round(true_positives/(true_positives+false_positives)*100, 1),
            NPV = round(true_negatives/(true_negatives+false_negatives)*100, 1),
            Both = sum(EPMA=="1" & CRF=="1"),
            CRF_only = sum(EPMA=="0" & CRF=="1"),
            EPMA_only = sum(EPMA=="1" & CRF=="0"),
            Neither = sum(EPMA=="0" & CRF=="0"))%>%
  select(-c(positives, negatives, true_positives, true_negatives, false_positives, false_negatives))%>%
  
# aggregate counts by drug across all resp statuses
  rbind(
    baseline_combined_flags%>%
      mutate(resp_status="Aggregate")%>%
      group_by(Drug, resp_status)%>%
      summarise(pairs = n_distinct(Study_ID),
                positives = length(CRF[CRF=="1"]),
                negatives = length(CRF[CRF=="0"]),
                true_positives = length(CRF[CRF=="1" & EPMA=="1"]),
                true_negatives = length(CRF[CRF=="0" & EPMA=="0"]),
                false_positives = length(CRF[CRF=="0" & EPMA =="1"]),
                false_negatives = length(CRF[CRF =="1" & EPMA == "0"]),
                Sensitivity = round(true_positives/positives*100, 1),
                Specificity = round(true_negatives/negatives*100, 1),
                PPV = round(true_positives/(true_positives+false_positives)*100, 1),
                NPV = round(true_negatives/(true_negatives+false_negatives)*100, 1),
                Both = sum(EPMA=="1" & CRF=="1"),
                CRF_only = sum(EPMA=="0" & CRF=="1"),
                EPMA_only = sum(EPMA=="1" & CRF=="0"),
                Neither = sum(EPMA=="0" & CRF=="0"))%>%
      select(-c(positives, negatives, true_positives, true_negatives, false_positives, false_negatives)))%>%
  
# combined counts across all drugs by resp status (excludes oxygen)
  rbind(
    baseline_combined_flags%>%
      filter(Drug!="Oxygen")%>%
      mutate(Drug="Combined (excluding oxygen)")%>%
      group_by(Drug,resp_status)%>%
      summarise(pairs = n(),
                positives = length(CRF[CRF=="1"]),
                negatives = length(CRF[CRF=="0"]),
                true_positives = length(CRF[CRF=="1" & EPMA=="1"]),
                true_negatives = length(CRF[CRF=="0" & EPMA=="0"]),
                false_positives = length(CRF[CRF=="0" & EPMA =="1"]),
                false_negatives = length(CRF[CRF =="1" & EPMA == "0"]),
                Sensitivity = round(true_positives/positives*100, 1),
                Specificity = round(true_negatives/negatives*100, 1),
                PPV = round(true_positives/(true_positives+false_positives)*100, 1),
                NPV = round(true_negatives/(true_negatives+false_negatives)*100, 1),
                Both = sum(EPMA=="1" & CRF=="1"),
                CRF_only = sum(EPMA=="0" & CRF=="1"),
                EPMA_only = sum(EPMA=="1" & CRF=="0"),
                Neither = sum(EPMA=="0" & CRF=="0"))%>%
      select(-c(positives, negatives, true_positives, true_negatives, false_positives, false_negatives)))%>%
  
# counts for all drugs combined, across all resp statuses (including oxygen)
  rbind(
    baseline_combined_flags%>%
      mutate(Drug="Combined",
             resp_status="Aggregate")%>%
      group_by(Drug,resp_status)%>%
      summarise(pairs = n(),
                positives = length(CRF[CRF=="1"]),
                negatives = length(CRF[CRF=="0"]),
                true_positives = length(CRF[CRF=="1" & EPMA=="1"]),
                true_negatives = length(CRF[CRF=="0" & EPMA=="0"]),
                false_positives = length(CRF[CRF=="0" & EPMA =="1"]),
                false_negatives = length(CRF[CRF =="1" & EPMA == "0"]),
                Sensitivity = round(true_positives/positives*100, 1),
                Specificity = round(true_negatives/negatives*100, 1),
                PPV = round(true_positives/(true_positives+false_positives)*100, 1),
                NPV = round(true_negatives/(true_negatives+false_negatives)*100, 1),
                Both = sum(EPMA=="1" & CRF=="1"),
                CRF_only = sum(EPMA=="0" & CRF=="1"),
                EPMA_only = sum(EPMA=="1" & CRF=="0"),
                Neither = sum(EPMA=="0" & CRF=="0"))%>%
      select(-c(positives, negatives, true_positives, true_negatives, false_positives, false_negatives)))%>%
  

# counts for all drugs combined, across all resp statuses (excluding oxygen)
  rbind(
    baseline_combined_flags%>%
      filter(Drug!="Oxygen")%>%
      mutate(Drug="Combined (excluding oxygen)",
             resp_status="Aggregate")%>%
      group_by(Drug,resp_status)%>%
      summarise(pairs = n(),
                positives = length(CRF[CRF=="1"]),
                negatives = length(CRF[CRF=="0"]),
                true_positives = length(CRF[CRF=="1" & EPMA=="1"]),
                true_negatives = length(CRF[CRF=="0" & EPMA=="0"]),
                false_positives = length(CRF[CRF=="0" & EPMA =="1"]),
                false_negatives = length(CRF[CRF =="1" & EPMA == "0"]),
                Sensitivity = round(true_positives/positives*100, 1),
                Specificity = round(true_negatives/negatives*100, 1),
                PPV = round(true_positives/(true_positives+false_positives)*100, 1),
                NPV = round(true_negatives/(true_negatives+false_negatives)*100, 1),
                Both = sum(EPMA=="1" & CRF=="1"),
                CRF_only = sum(EPMA=="0" & CRF=="1"),
                EPMA_only = sum(EPMA=="1" & CRF=="0"),
                Neither = sum(EPMA=="0" & CRF=="0"))%>%
      select(-c(positives, negatives, true_positives, true_negatives, false_positives, false_negatives)))%>%
  
  rename(Individuals=pairs)->baseline_drugs_participant_counts

# View(baseline_drugs_participant_counts)

kappa_baseline_results%<>%
  arrange(Drug)%>%
  mutate(Judgement = case_when(kappa<0~"No agreement",
                               kappa>= 0 & kappa <0.2 ~ "Slight agreement",
                               kappa >=0.2 & kappa<0.4 ~ "Fair agreement",
                               kappa>=0.4 & kappa <0.6 ~"Moderate agreement",
                               kappa>=0.6 & kappa<0.8 ~ "Substantial agreement",
                               kappa>=0.8 ~ "Almost perfect agreement"))



##### forest plot with counts -------

kappa_baseline_results%>%
  left_join(baseline_drugs_participant_counts)%>%
  mutate(resp_status=fct_relevel(resp_status,
                                 "No oxygen or ventilation",
                                 "Oxygen requirement or non-invasive ventilation",
                                 "Mechanical ventilation or ECMO",
                                 "Aggregate"))%>%
  filter(Drug!="Oxygen" | resp_status=="Aggregate")%>%
  mutate(Drug=fct_relevel(Drug, 
                          "Corticosteroids",
                          "Macrolides",
                          "Tocilizumab or sarilumab",
                          "Remdesivir",
                          "Antiplatelets",
                          "Oral anticoagulants",
                          "LMWH",
                          "Oxygen",
                          "Combined",
                          "Combined (excluding oxygen)"
  ))%>%
  arrange(Drug, resp_status)%>%
  mutate(`Kappa (95% CI)` = paste0(round(kappa,2), " (", round(kappa_CI_lower, 2), " to ", round(kappa_CI_upper, 2), ")"))%>%
  mutate(`PABAK (95% CI)` = paste0(round(pabak,2), " (", round(pabak_CI_lower, 2), " to ", round(pabak_CI_upper, 2), ")"))%>% 
  select(Drug, resp_status, Individuals, Both, CRF_only, EPMA_only, Neither, Sensitivity, Specificity, PPV, NPV, `Kappa (95% CI)`, `PABAK (95% CI)`, kappa, se, kappa_CI_lower, kappa_CI_upper, pabak, pabak_CI_lower, pabak_CI_upper)%>%
  add_row(Drug="Corticosteroids", .before=1)%>%
  add_row(Drug="Macrolides",.before=6)%>%
  add_row(Drug="Tocilizumab or sarilumab",.before=11)%>%
  add_row(Drug="Remdesivir",.before=16)%>%
  add_row(Drug="Antiplatelets",.before=21)%>%
  add_row(Drug="Oral anticoagulants",.before=26)%>%
  add_row(Drug="LMWH",.before=31)%>%
  add_row(Drug="Oxygen",.before=36)%>%
  add_row(Drug="Combined",.before=38)%>%
  add_row(Drug="Combined (excluding oxygen)",.before=43)%>%
  rename(Total=Individuals,
         `CRF only` = CRF_only,
         `Both sources` = Both,
         `EPMA only` = EPMA_only,
         `Neither source` = Neither)%>%
  add_column(`Kappa`=NA, .after = "NPV")%>%
  add_column(`PABAK`=NA, .after = "Kappa (95% CI)")%>%  
  mutate(`Kappa`=paste(rep(" ", 20), collapse = " "))%>%
  mutate(`PABAK`=paste(rep(" ", 20), collapse = " "))%>%  
  mutate(Size=Total)%>%
  mutate(Drug=as.character(Drug))%>%
  mutate(resp_status=if_else(!is.na(resp_status), paste("   ", resp_status), Drug))%>%
  rename(`Drug (by respiratory status)`=resp_status)%>%
  select(-Drug)%>%
  mutate(across(-c(Size), ~ifelse(is.na(.), " ",.)))%>%
  mutate(across(c(kappa_CI_lower, kappa_CI_upper, kappa, pabak, pabak_CI_lower, pabak_CI_upper, se), ~as.numeric(.)))->forest_plot_table_baseline


###### overall ---------

forest_plot_table_baseline_overall<-
  forest_plot_table_baseline%>%
  filter(!`Drug (by respiratory status)`%in%c("    No oxygen or ventilation",
                                              "    Oxygen requirement or non-invasive ventilation",
                                              "    Mechanical ventilation or ECMO"))%>%
  rename(Drug=`Drug (by respiratory status)`)%>%
  filter(Drug=="    Aggregate")%>%
  mutate(Drug=c("Corticosteroids",
                "Macrolides",
                "Tocilizumab or sarilumab",
                "Remdesivir",
                "Antiplatelets",
                "Oral anticoagulants",
                "LMWH",
                "Oxygen",
                "Combined",
                "Combined\n(excluding oxygen)"))



forest_plot_baseline_overall<-forest(forest_plot_table_baseline_overall[,1:14],
                                         est=list(forest_plot_table_baseline_overall$kappa,
                                                  forest_plot_table_baseline_overall$pabak),
                                         lower=list(forest_plot_table_baseline_overall$kappa_CI_lower,
                                                    forest_plot_table_baseline_overall$pabak_CI_lower),
                                         upper=list(forest_plot_table_baseline_overall$kappa_CI_upper,
                                                    forest_plot_table_baseline_overall$pabak_CI_upper),
                                         sizes=list(forest_plot_table_baseline_overall$se^2,
                                                    forest_plot_table_baseline_overall$se^2),
                                         ci_column = c(11,13),
                                         xlim=c(-1,1),
                                         arrow_lab = c("Negative agreement", "Positive agreement"),
                                         ref_line = 0,
                                     # is_summary = c(rep(FALSE, nrow(forest_plot_table_baseline_overall)-2), TRUE, TRUE)
                                     )%>%
  insert_text(.,
              text = "Counts (participants or participant/drug pairs)",
              col = 2:6,
              part = "header",
              gp = gpar(fontface = "bold"))%>%
  insert_text(.,
              text = "Agreement metrics",
              col = 7:13,
              part = "header",
              gp = gpar(fontface = "bold"))%>%
  edit_plot(row=c(9,10),
            which="text",
            gp=gpar(fontface="bold"))%>%
  gtable_add_grob(grobs=segmentsGrob(
    x0 = unit(0,"npc"), # horizontal origin
    y0 = unit(0,"npc"), # vertical origin
    x1 = unit(0,"npc"), # horizontal end
    y1 = unit(1,"npc"), # vertical end
    gp = gpar(lwd = 2.0)), # width
    t = 4,   # top end
    b = 14,  # bottom end
    l = 8,  # left end
    r = 8 # right end
  )

forest_plot_baseline_overall

ggsave(filename="Outputs/Figures/Analysis/forestplot_baseline_overall.png", 
       plot=forest_plot_baseline_overall,
       dpi = "retina",
       units = "cm",
       width = get_wh(plot=forest_plot_baseline_overall, unit="cm"),
       limitsize = F
       # height = 30
)

##### alternative table format
sect_properties <- prop_section(
  page_size = page_size(orient = "landscape",
                        width = 15, 
                        #height = 11.7
  ),
  type = "continuous",
  page_margins = page_mar())

forest_plot_table_baseline_overall%>%
  select(Drug,
         Total,
         `Both sources`,
         `CRF only`,
         `EPMA only`,
         `Neither source`,
         Sn=Sensitivity,
         Sp=Specificity,
         PPV,
         NPV,
         `Kappa (95% CI)`,
         `PABAK (95% CI)`)%>%
  flextable()%>%
  add_header_row(values=c("", "Participants/drug pairs in each source (%)", "Agreement metrics"), colwidths = c(1, 5, 6))%>%
  save_as_docx(path="Outputs/Tables/baseline_agreement_overall.docx",
               pr_section = sect_properties)
  


###### resp status -------

forest_plot_table_baseline_resp_status<-
  forest_plot_table_baseline%>%
  filter(`Drug (by respiratory status)`!="    Aggregate",
           `Drug (by respiratory status)` !="Oxygen",
         `Drug (by respiratory status)` !="Combined (excluding oxygen)")





forest_plot_baseline_resp_status<-forest(forest_plot_table_baseline_resp_status[,1:14],
                    est=list(forest_plot_table_baseline_resp_status$kappa,
                             forest_plot_table_baseline_resp_status$pabak),
                    lower=list(forest_plot_table_baseline_resp_status$kappa_CI_lower,
                               forest_plot_table_baseline_resp_status$pabak_CI_lower),
                    upper=list(forest_plot_table_baseline_resp_status$kappa_CI_upper,
                               forest_plot_table_baseline_resp_status$pabak_CI_upper),
                    sizes=list(forest_plot_table_baseline_resp_status$se^2,
                               forest_plot_table_baseline_resp_status$se^2),
                    ci_column = c(11,13),
                    xlim=c(-1,1),
                    arrow_lab = c("Negative agreement", "Positive agreement"),
                    ref_line = 0,
                    # is_summary = c(rep(FALSE, nrow(forest_plot_table_baseline_resp_status)-3), TRUE)
                    )%>%
  insert_text(.,
              text = "Counts (participants or participant/drug pairs)",
              col = 2:6,
              part = "header",
              gp = gpar(fontface = "bold"))%>%
  insert_text(.,
              text = "Agreement metrics",
              col = 7:13,
              part = "header",
              gp = gpar(fontface = "bold"))%>%
  edit_plot(row=c(29,30,31,32),
            which="text",
            gp=gpar(fontface="bold"))%>%
  gtable_add_grob(grobs=segmentsGrob(
    x0 = unit(0,"npc"), # horizontal origin
    y0 = unit(0,"npc"), # vertical origin
    x1 = unit(0,"npc"), # horizontal end
    y1 = unit(1,"npc"), # vertical end
    gp = gpar(lwd = 2.0)), # width
    t = 4,   # top end
    b = 36,  # bottom end
    l = 8,  # left end
    r = 8 # right end
  )

forest_plot_baseline_resp_status

ggsave(filename="Outputs/Figures/Analysis/forestplot_baseline_resp_status.png", 
       plot=forest_plot_baseline_resp_status,
       dpi = "retina",
       units = "cm",
       width = get_wh(plot=forest_plot_baseline_resp_status, unit="cm"),
       limitsize = F
       # height = 30
)


# alternative table format

forest_plot_table_baseline_resp_status%>%
  select(`Drug (by respiratory status)`,
         Total,
         `Both sources`,
         `CRF only`,
         `EPMA only`,
         `Neither source`,
         Sn=Sensitivity,
         Sp=Specificity,
         PPV,
         NPV,
         `Kappa (95% CI)`,
         `PABAK (95% CI)`)%>%
  filter(`Drug (by respiratory status)`!="Combined (excluding oxygen)")%>%
  flextable()%>%
  add_header_row(values=c("", "Participants/drug pairs in each source (%)", "Agreement metrics"), colwidths = c(1, 5, 6))%>%
  width(j=1, 8, "cm")%>%
  width(j=c(11,12), 4, "cm")%>%
  #padding(i=c(1,5,9,13,17,21,25), j=1, padding.left=2, part="body")
  save_as_docx(path="Outputs/Tables/baseline_agreement_by_resp_status.docx",
               pr_section = sect_properties)


##### investigate participants missed by EPMA (steroids)------

baseline_combined_flags%>%
  filter(EPMA=="0", 
         CRF=="1", 
         Drug=="Corticosteroids")%>%
  distinct(Study_ID)%>%.[[1]]->discordant_ids_steroids 
# 102 people on steroids in CRF but not EPMA 

# check records for people with positive flags in EPMA data who don't appear in the overall counts

## extract people who do have steroid records in EPMA but are not being captured
epma_drug_flags%>%
  filter(Table=="Administration")%>%
  left_join(censoring_dates)%>%
  filter(Study_ID%in%discordant_ids_steroids)%>%
  filter(Drug=="Corticosteroids")%>%
  arrange(Study_ID)%>%
  filter(Flag=="1")%>%
  distinct(Study_ID)%>%
  .[[1]]->participants_with_steroids_records 
  # 157 of these have steroids recorded (at any time point)

## check relationships of those records with randomisation date
epma_drug_flags%>%
  filter(Table=="Administration")%>%
  left_join(censoring_dates)%>%
  filter(Study_ID%in%participants_with_steroids_records)%>%
  filter(Drug=="Corticosteroids")%>%
  arrange(Study_ID)%>%
  filter(Flag=="1")%>%
  distinct(Study_ID, PRESCRIPTION_ID_HASHED, admission_date, rand_date, date)%>%
  select(Study_ID,PRESCRIPTION_ID_HASHED, admission_date, rand_date, date)%>%
  mutate(days_after_rand=as.numeric(difftime(date, rand_date, units= "days")))%>%
  mutate(days_after_admission=as.numeric(difftime(date, admission_date, units= "days")))%>%
  filter(days_after_rand<=0)%>%
  arrange(days_after_rand)%>%
  View() # none have records after admission date and before randomisation date



##### investigate records for people inaccuratley captured by EPMA (steroids) -------


baseline_combined_flags%>%
  filter(EPMA=="1", 
         CRF=="0", 
         Drug=="Corticosteroids")%>%
  distinct(Study_ID)%>%.[[1]]->discordant_ids_steroids

epma_drug_flags%>%
  left_join(censoring_dates)%>%
  filter(Study_ID%in%discordant_ids_steroids)%>%
  filter(Drug=="Corticosteroids")%>%
  arrange(Study_ID)%>%
  filter(Flag=="1")%>%
  distinct(Study_ID, PRESCRIPTION_ID_HASHED, admission_date, rand_date, date, STATUS)%>%
  select(Study_ID,PRESCRIPTION_ID_HASHED, admission_date, rand_date, date, STATUS)%>%
  mutate(days_after_rand=as.numeric(difftime(date, rand_date, units= "days")))%>%
  # filter(days_after_rand<0)%>%
  arrange(days_after_rand)%>%
  View()

# inactive prescriptions?
epma_drug_flags%>%
  left_join(censoring_dates)%>%
  filter(Study_ID%in%discordant_ids_steroids)%>%
  filter(Drug=="Corticosteroids")%>%
  arrange(Study_ID)%>%
  filter(Flag=="1")%>%
  mutate(days_after_rand=as.numeric(difftime(date, rand_date, units= "days")))%>%
  filter(days_after_rand<0)%>%
  group_by(Study_ID)%>%
  summarise(any_active=if_else(any(STATUS=="Active"), 1, 0))%>%
  View()
  # only 49/102 have active records








# infections?
HES_data%>%
  filter(study_number %in% discordant_ids_steroids)%>%
  left_join(censoring_dates, by=c("study_number" = "Study_ID"))%>%
  filter(epistart>=admission_date, epiend<=right_censoring_date_death_discharge_28d)%>%
  select(study_number, starts_with("diag"))%>%
  pivot_longer(-study_number, names_to = "position", values_to="code")%>%
  group_by(code)%>%
  summarise(n=n_distinct(study_number))%>%
  View()


# tables?
data_prescription%>%
  filter(PRESCRIPTION_ID_HASHED %in% 
           (epma_drug_flags%>%
  filter(Drug=="Corticosteroids",
         Flag=="1",
         Study_ID %in% discordant_ids_steroids)%>%
  select(PRESCRIPTION_ID_HASHED)%>%
    .[[1]]))%>%
  select(Study_ID, PRESCRIPTION_ID_HASHED, date=INITIAL_AUTHORISED_DATE_TIME, MEDICATION_NAME)%>%
  mutate(Table="Prescription")%>%
  rbind(
    data_administration%>%
      filter(ADMINISTRATION_ID_HASHED %in% 
               (epma_drug_flags%>%
                  filter(Drug=="Corticosteroids",
                         Flag=="1",
                         Study_ID %in% discordant_ids_steroids)%>%
                  select(ADMINISTRATION_ID_HASHED)%>%
                  .[[1]]))%>%
      select(Study_ID, PRESCRIPTION_ID_HASHED, date=ADMINISTERED_DATE_TIME, MEDICATION_NAME)%>%
      mutate(Table="Administration"))%>%
  left_join(censoring_dates)%>%
  mutate(date=as.Date(date, format="%Y-%m-%d"))%>%
  filter(date>=admission_date,
         date<=rand_date)%>% 
  group_by(Study_ID)%>%
  summarise(prescription=if_else(any(Table=="Prescription"), 1, 0),
            administration = if_else(any(Table=="Administration"), 1, 0),
            both = if_else(any(Table=="Administration") & any(Table=="Prescription"), 1, 0),
            prescription_only = if_else(!any(Table=="Administration") & any(Table=="Prescription"), 1, 0))%>%
  ungroup()%>%
  # filter(both==0)%>%
  View()
  # 9/102 have only prescription records
  

#### barchart plot -----

baseline_drugs_participant_counts%>%
  filter(resp_status=="Aggregate",
         Drug!="Combined",
         Drug!="Combined (excluding oxygen)")%>%
  select(Drug, EPMA_only, CRF_only, Both, Neither)%>%
  mutate(Either = EPMA_only+CRF_only+Both,
         EPMA = EPMA_only+Both,
         CRF = CRF_only+Both,
         Total=Either+Neither)%>%
  select(Drug, Either, CRF, EPMA, Total)%>%
  pivot_longer(-Drug, names_to = "Group", values_to = "Count")%>%
  group_by(Drug)%>%
  mutate(prop = round(Count/Count[Group=="Total"]*100,1))%>%
  filter(Group!="Neither",
         Group!="Total")%>%
  mutate(Group=fct_relevel(Group, 
                           # "Neither", 
                           "Either",
                           "CRF", "EPMA"
                           ))->counts_for_baseline_barchart
  
counts_for_baseline_barchart%>%
  ggplot(aes(Drug, prop, fill=Group))+
  geom_col(position = "dodge")+
  geom_text(aes(label=paste0(prop, "%")),
            position = position_dodge(width = .9),
            vjust=-1,
            size=15/2.54)+
  labs(y="Proportion of total participants",
       fill="Data source",
       subtitle="Baseline exposure")+
  scale_y_continuous(
    expand = expansion(c(0,0.2)),
    limits=c(0,100),
    breaks = seq(0,100,20))+
  theme(legend.position = "none",
        text=element_text(family="Mulish",
                          size=20),
        axis.title.x=element_blank(),
        axis.text.x = element_text(angle=30, 
                                   hjust=1, 
                                   # vjust=-1
                                   ))->barchart1
barchart1

# slides

counts_for_baseline_barchart%>%
  ggplot(aes(Drug, prop, fill=Group))+
  geom_col(position = "dodge")+
  geom_text(aes(label=paste0(prop, "%")),
            position = position_dodge(width = .9),
            vjust=-1,
            size=15/2.54,
            color="white")+
  labs(y="Proportion of total participants",
       fill="Data source",
       # subtitle="Baseline exposure"
       )+
  scale_y_continuous(
    expand = expansion(c(0,0.2)),
    limits=c(0,100),
    breaks = seq(0,100,20))+
  oxpop_blue_panel+
  theme(legend.position = "right",
        text=element_text(family="Mulish",
                          size=20),
        axis.title.x=element_blank(),
        axis.text.x = element_text(angle=30, 
                                   hjust=1, 
                                   # vjust=-1
        ))


ggsave("Outputs/Figures/Slides/barchart_baseline_exposure.png",
       width=60,
       height=35,
       dpi="retina",
       units = "cm")  






### 4.2 Post-randomisation exposure ------

drugs_crf_sdtm%>%
  distinct(cmtrt, epoch)%>%
  View()

drugs_crf_adam%>%
  distinct(param)


#### 4.2.1 Binary ------

# check who could have had the aspirin question answered (based on when the form started being used)

drugs_crf_adam%>%
  filter(param=="Proportion of days aspirin received")%>%
  filter(!is.na(aval))%>%
  distinct(Study_ID)%>%
  left_join(drugs_crf_sdtm)%>%
  filter(visitnum==3)%>%
  slice_min(cmsttpt)%>%
  distinct(cmsttpt) # 25-01-2020


drugs_crf_sdtm%>%
  filter(visitnum==3)%>%
  filter(cmsttpt>="2020-01-05")%>%
  distinct(Study_ID)%>%
  .[[1]]->aspirin_participant_list




##### extract EPMA flags -------

post_randomisation_epma_binary_flags<-
  epma_drug_flags%>%
  left_join(censoring_dates%>%select(-c(admidate_crf, admidate_hes, admission_study_day, censoring_study_day)))%>%
  filter(Drug %in% (crf_post_rand_drug_flags%>%distinct(Drug)%>%.[[1]]))%>%
  group_by(Study_ID)%>%
  filter(any(date>=admission_date & date<=right_censoring_date_death_discharge_28d))%>% # restrict to people who have had a record within their randomisation admission
  group_by(Study_ID, Drug)%>%
  summarise(EPMA=if_else(any(Flag=="1" & date>rand_date & date<=right_censoring_date_death_discharge_28d), 1, 0))%>% # positive flag if there any relevant record occurring after randomisation but before the censoring date
  right_join(
    expand.grid(Study_ID=study_population,
                Drug=crf_post_rand_drug_flags%>%distinct(Drug)%>%.[[1]])%>%
      mutate(Study_ID=as.character(Study_ID)),
    by=c("Study_ID", "Drug"))%>%
  mutate(EPMA=replace_na(EPMA, 0))%>%
  ungroup()
    

##### combine with CRF data ------


post_rand_binary_combined_flags<-crf_post_rand_drug_flags%>%
  filter(Study_ID %in% study_population,
         Drug!="Aspirin")%>% # restrict to people with EPMA data
  rbind(
    crf_post_rand_drug_flags%>%
      filter(Drug=="Aspirin")%>%
      filter(Study_ID %in% aspirin_participant_list,
             Study_ID %in% study_population)
  )%>%
  filter(Variable=="flag")%>%
  select(-Variable)%>%
  rename(CRF=Value)%>%
  left_join(post_randomisation_epma_binary_flags)%>%
  left_join(baseline_crf_england%>%select(Study_ID, resp_status))%>%
  select(Study_ID, Drug, EPMA, CRF, resp_status)%>%
  group_by(Study_ID, Drug, resp_status)%>%
  summarise(EPMA=if_else(any(EPMA==1), 1, 0),
            CRF=if_else(any(CRF==1), 1, 0))%>%
  mutate(EPMA=replace_na(EPMA,0))%>%
  ungroup()


##### calculate agreeemnt  -------

# by resp status

kappa_post_rand<-list()

drugs<-post_rand_binary_combined_flags%>%
  distinct(Drug)%>%
  .[[1]]

resp_status<-post_rand_binary_combined_flags%>%distinct(resp_status)%>%      filter(!is.na(resp_status))%>%
mutate(resp_status=as.character(resp_status))%>%.[[1]]

for (i in drugs) {
  
  for (r in resp_status) {
    
    post_rand_binary_combined_flags%>%
      filter(Drug==i,
             resp_status==r)%>%
      select(EPMA, CRF)%>%
      mutate(across(everything(), as.integer))%>%
      mutate(across(c(EPMA, CRF), ~replace_na(.x, 0)))->table
    
    matrix<-as.matrix(table(table))
    
    try(k<-epi.kappa(matrix, method="cohen", alternative = "greater", conf.level=0.95))
    
    if(exists("k"))
    {
      
      kappa_post_rand[[i]][[r]]<-k
      
    }
    else{
      kappa_post_rand[[i]][[r]]<-list()
    }
    
    rm(r, table, k, matrix)
    
  }
  rm(i)
}


# View(kappa_post_rand)    



###### Unlist results for table ------

kappa_post_rand_results<-data.frame()


for (i in drugs) {
  Drug<-i
  
  for (r in resp_status){
    
    
    resp <- r
    
    try(
      table<-(
        data.frame(
          Drug = Drug,
          resp_status = resp,
          kappa = unlist(kappa_post_rand[[i]][[r]]$kappa$est),
          se = unlist(kappa_post_rand[[i]][[r]]$kappa$se),
          kappa_CI_lower = unlist(kappa_post_rand[[i]][[r]]$kappa$lower),
          kappa_CI_upper = unlist(kappa_post_rand[[i]][[r]]$kappa$upper),
          
          pabak = unlist(kappa_post_rand[[i]][[r]]$pabak$est),
          pabak_CI_lower = unlist(kappa_post_rand[[i]][[r]]$pabak$lower),
          pabak_CI_upper = unlist(kappa_post_rand[[i]][[r]]$pabak$upper),
          
          p_value = unlist(kappa_post_rand[[i]][[r]]$z$p.value)
        ))
    )
    
    try(kappa_post_rand_results%<>%bind_rows(table))
    rm(resp, r, table)
  }
  rm(Drug, i)
}


# View(kappa_post_rand_results)


###### add aggregates  ------

# calculate agreement

for (i in drugs){  
  r<-"Aggregate"
  
  
  post_rand_binary_combined_flags%>%
    filter(Drug==i)%>%
    select(EPMA, CRF)%>%
    mutate(across(everything(), as.integer))%>%
    mutate(across(c(EPMA, CRF), ~replace_na(.x, 0)))->table
  
  matrix<-as.matrix(table(table))
  
  try(k<-epi.kappa(matrix, method="cohen", alternative = "greater", conf.level=0.95))
  
  if(exists("k"))
  {
    
    kappa_post_rand[[i]][[r]]<-k
    
  }
  else{
    kappa_post_rand[[i]][[r]]<-list()
  }
  
  rm(i, table, k, matrix)
  
}




###### unlist results in table -----


for (i in drugs) {
  r<-"Aggregate"
  
  try(
    table<-(data.frame(
      Drug = i,
      resp_status = "Aggregate",
      kappa = unlist(kappa_post_rand[[i]][[r]]$kappa$est),
      se = unlist(kappa_post_rand[[i]][[r]]$kappa$se),
      kappa_CI_lower = unlist(kappa_post_rand[[i]][[r]]$kappa$lower),
      kappa_CI_upper = unlist(kappa_post_rand[[i]][[r]]$kappa$upper),
      
      pabak = unlist(kappa_post_rand[[i]][[r]]$pabak$est),
      pabak_CI_lower = unlist(kappa_post_rand[[i]][[r]]$pabak$lower),
      pabak_CI_upper = unlist(kappa_post_rand[[i]][[r]]$pabak$upper),
      
      p_value = unlist(kappa_post_rand[[i]][[r]]$z$p.value)
    ))
  )
  
  try(kappa_post_rand_results%<>%bind_rows(table))
  
  rm(i)
}



# View(kappa_post_rand_results)  



###### combined (all drugs) ------


for (r in resp_status) {
  
  post_rand_binary_combined_flags%>%
    filter(resp_status==r)%>%
    select(EPMA, CRF)%>%
    mutate(across(everything(), as.integer))%>%
    mutate(across(c(EPMA, CRF), ~replace_na(.x, 0)))->table
  
  matrix<-as.matrix(table(table))
  
  try(k<-epi.kappa(matrix, method="cohen", alternative = "greater", conf.level=0.95))
  
  if(exists("k"))
  {
    
    kappa_post_rand[["Combined"]][[r]]<-k
    
  }
  else{
    kappa_post_rand[["Combined"]][[r]]<-list()
  }
  
  rm(r, table, k, matrix)
  
}

for (r in resp_status){
  
  
  resp <- r
  
  try(
    table<-(
      data.frame(
        Drug = "Combined",
        resp_status = resp,
        kappa = unlist(kappa_post_rand[["Combined"]][[r]]$kappa$est),
        se = unlist(kappa_post_rand[["Combined"]][[r]]$kappa$se),
        kappa_CI_lower = unlist(kappa_post_rand[["Combined"]][[r]]$kappa$lower),
        kappa_CI_upper = unlist(kappa_post_rand[["Combined"]][[r]]$kappa$upper),
        
        pabak = unlist(kappa_post_rand[["Combined"]][[r]]$pabak$est),
        pabak_CI_lower = unlist(kappa_post_rand[["Combined"]][[r]]$pabak$lower),
        pabak_CI_upper = unlist(kappa_post_rand[["Combined"]][[r]]$pabak$upper),
        
        p_value = unlist(kappa_post_rand[["Combined"]][[r]]$z$p.value)
      ))
  )
  
  try(kappa_post_rand_results%<>%bind_rows(table))
  rm(resp, r, table)
}



  post_rand_binary_combined_flags%>%
    select(EPMA, CRF)%>%
    mutate(across(everything(), as.integer))%>%
    mutate(across(c(EPMA, CRF), ~replace_na(.x, 0)))->table
  
  matrix<-as.matrix(table(table))
  
  k<-epi.kappa(matrix, method="cohen", alternative = "greater", conf.level=0.95)
  kappa_post_rand[["Combined"]][["Aggregate"]]<-k
    
 rm(k, matrix, table)

  table<-(
      data.frame(
        Drug = "Combined",
        resp_status = "Aggregate",
        kappa = unlist(kappa_post_rand[["Combined"]][["Aggregate"]]$kappa$est),
        se = unlist(kappa_post_rand[["Combined"]][["Aggregate"]]$kappa$se),
        kappa_CI_lower = unlist(kappa_post_rand[["Combined"]][["Aggregate"]]$kappa$lower),
        kappa_CI_upper = unlist(kappa_post_rand[["Combined"]][["Aggregate"]]$kappa$upper),
        
        pabak = unlist(kappa_post_rand[["Combined"]][["Aggregate"]]$pabak$est),
        pabak_CI_lower = unlist(kappa_post_rand[["Combined"]][["Aggregate"]]$pabak$lower),
        pabak_CI_upper = unlist(kappa_post_rand[["Combined"]][["Aggregate"]]$pabak$upper),
        
        p_value = unlist(kappa_post_rand[["Combined"]][["Aggregate"]]$z$p.value)
      ))
  
kappa_post_rand_results%<>%bind_rows(table)
  rm(table)



###### counts ------
  
kappa_post_rand_results%<>%
    arrange(Drug)%>%
    mutate(Judgement = case_when(kappa<0~"No agreement",
                                 kappa>= 0 & kappa <0.2 ~ "Slight agreement",
                                 kappa >=0.2 & kappa<0.4 ~ "Fair agreement",
                                 kappa>=0.4 & kappa <0.6 ~"Moderate agreement",
                                 kappa>=0.6 & kappa<0.8 ~ "Substantial agreement",
                                 kappa>=0.8 ~ "Almost perfect agreement"))
  
  
post_rand_binary_combined_flags%>%
  group_by(Drug, resp_status)%>%
  summarise(pairs = n_distinct(Study_ID),
            positives = length(CRF[CRF=="1"]),
            negatives = length(CRF[CRF=="0"]),
            true_positives = length(CRF[CRF=="1" & EPMA=="1"]),
            true_negatives = length(CRF[CRF=="0" & EPMA=="0"]),
            false_positives = length(CRF[CRF=="0" & EPMA =="1"]),
            false_negatives = length(CRF[CRF =="1" & EPMA == "0"]),
            Sensitivity = round(true_positives/positives*100, 1),
            Specificity = round(true_negatives/negatives*100, 1),
            PPV = round(true_positives/(true_positives+false_positives)*100, 1),
            NPV = round(true_negatives/(true_negatives+false_negatives)*100, 1),
            Both = sum(EPMA=="1" & CRF=="1"),
            CRF_only = sum(EPMA=="0" & CRF=="1"),
            EPMA_only = sum(EPMA=="1" & CRF=="0"),
            Neither = sum(EPMA=="0" & CRF=="0"))%>%
  select(-c(positives, negatives, true_positives, true_negatives, false_positives, false_negatives))%>%
  rbind(
    post_rand_binary_combined_flags%>%
      mutate(resp_status="Aggregate")%>%
      group_by(Drug, resp_status)%>%
      summarise(pairs = n_distinct(Study_ID),
                positives = length(CRF[CRF=="1"]),
                negatives = length(CRF[CRF=="0"]),
                true_positives = length(CRF[CRF=="1" & EPMA=="1"]),
                true_negatives = length(CRF[CRF=="0" & EPMA=="0"]),
                false_positives = length(CRF[CRF=="0" & EPMA =="1"]),
                false_negatives = length(CRF[CRF =="1" & EPMA == "0"]),
                Sensitivity = round(true_positives/positives*100, 1),
                Specificity = round(true_negatives/negatives*100, 1),
                PPV = round(true_positives/(true_positives+false_positives)*100, 1),
                NPV = round(true_negatives/(true_negatives+false_negatives)*100, 1),
                Both = sum(EPMA=="1" & CRF=="1"),
                CRF_only = sum(EPMA=="0" & CRF=="1"),
                EPMA_only = sum(EPMA=="1" & CRF=="0"),
                Neither = sum(EPMA=="0" & CRF=="0"))%>%
      select(-c(positives, negatives, true_positives, true_negatives, false_positives, false_negatives))  )%>%
  rbind(
    post_rand_binary_combined_flags%>%
      mutate(Drug="Combined")%>%
      group_by(Drug,resp_status)%>%
      summarise(pairs = n(),
                positives = length(CRF[CRF=="1"]),
                negatives = length(CRF[CRF=="0"]),
                true_positives = length(CRF[CRF=="1" & EPMA=="1"]),
                true_negatives = length(CRF[CRF=="0" & EPMA=="0"]),
                false_positives = length(CRF[CRF=="0" & EPMA =="1"]),
                false_negatives = length(CRF[CRF =="1" & EPMA == "0"]),
                Sensitivity = round(true_positives/positives*100, 1),
                Specificity = round(true_negatives/negatives*100, 1),
                PPV = round(true_positives/(true_positives+false_positives)*100, 1),
                NPV = round(true_negatives/(true_negatives+false_negatives)*100, 1),
                Both = sum(EPMA=="1" & CRF=="1"),
                CRF_only = sum(EPMA=="0" & CRF=="1"),
                EPMA_only = sum(EPMA=="1" & CRF=="0"),
                Neither = sum(EPMA=="0" & CRF=="0"))%>%
      select(-c(positives, negatives, true_positives, true_negatives, false_positives, false_negatives)))%>%
  rbind(
    post_rand_binary_combined_flags%>%
      mutate(Drug="Combined",
             resp_status="Aggregate")%>%
      group_by(Drug,resp_status)%>%
      summarise(pairs = n(),
                positives = length(CRF[CRF=="1"]),
                negatives = length(CRF[CRF=="0"]),
                true_positives = length(CRF[CRF=="1" & EPMA=="1"]),
                true_negatives = length(CRF[CRF=="0" & EPMA=="0"]),
                false_positives = length(CRF[CRF=="0" & EPMA =="1"]),
                false_negatives = length(CRF[CRF =="1" & EPMA == "0"]),
                Sensitivity = round(true_positives/positives*100, 1),
                Specificity = round(true_negatives/negatives*100, 1),
                PPV = round(true_positives/(true_positives+false_positives)*100, 1),
                NPV = round(true_negatives/(true_negatives+false_negatives)*100, 1),
                Both = sum(EPMA=="1" & CRF=="1"),
                CRF_only = sum(EPMA=="0" & CRF=="1"),
                EPMA_only = sum(EPMA=="1" & CRF=="0"),
                Neither = sum(EPMA=="0" & CRF=="0"))%>%
      select(-c(positives, negatives, true_positives, true_negatives, false_positives, false_negatives)))%>%
  rename(Individuals=pairs)%>%
  ungroup()->post_rand_participant_counts

# View(post_rand_participant_counts)




##### forest plot with counts -------


kappa_post_rand_results%>%
  ungroup()%>%
  left_join(post_rand_participant_counts)%>%
  mutate(resp_status=fct_relevel(resp_status,
                                 "No oxygen or ventilation",
                                 "Oxygen requirement or non-invasive ventilation",
                                 "Mechanical ventilation or ECMO",
                                 "Aggregate"))%>%
  filter(Drug!="Oxygen" | resp_status=="Aggregate")%>%
  mutate(Drug=fct_relevel(Drug, 
                          "Corticosteroids",
                          "Tocilizumab or sarilumab",
                          "Macrolides",
                          "Remdesivir",
                          "Hydroxychloroquine",
                          "Lopinavir-ritonavir",
                          "Colchicine",
                          "Baricitinib",
                          "Aspirin",
                          "REGN antibodies"
  ))%>%
  arrange(Drug, resp_status)%>%
  mutate(`Kappa (95% CI)` = paste0(round(kappa,2), " (", round(kappa_CI_lower, 2), " to ", round(kappa_CI_upper, 2), ")"))%>%
  mutate(`PABAK (95% CI)` = paste0(round(pabak,2), " (", round(pabak_CI_lower, 2), " to ", round(pabak_CI_upper, 2), ")"))%>% 
  select(Drug, resp_status, Individuals, Both, CRF_only, EPMA_only, Neither, Sensitivity, Specificity, PPV, NPV, `Kappa (95% CI)`, `PABAK (95% CI)`, kappa, se, kappa_CI_lower, kappa_CI_upper, pabak, pabak_CI_lower, pabak_CI_upper)%>%
  add_row(Drug="Corticosteroids", .before=1)%>%
  add_row(Drug="Tocilizumab or sarilumab",.before=6)%>%
  add_row(Drug="Macrolides",.before=11)%>%
  add_row(Drug="Remdesivir",.before=16)%>%
  add_row(Drug="Hydroxychloroquine",.before=21)%>%
  add_row(Drug="Lopinavir-ritonavir",.before=26)%>%
  add_row(Drug="Colchicine",.before=30)%>%
  add_row(Drug="Bariticinib",.before=35)%>%
  add_row(Drug="Aspirin",.before=40)%>%
  add_row(Drug="REGN antibodies",.before=45)%>%
  add_row(Drug="Combined",.before=50)%>%
  rename(Total=Individuals,
         `CRF only` = CRF_only,
         `Both sources` = Both,
         `EPMA only` = EPMA_only,
         `Neither source` = Neither)%>%
  add_column(`Kappa`=NA, .after = "NPV")%>%
  add_column(`PABAK`=NA, .after = "Kappa (95% CI)")%>%  
  mutate(`Kappa`=paste(rep(" ", 20), collapse = " "))%>%
  mutate(`PABAK`=paste(rep(" ", 20), collapse = " "))%>%  
  mutate(Size=Total)%>%
  mutate(Drug=as.character(Drug))%>%
  mutate(resp_status=if_else(!is.na(resp_status), paste("   ", resp_status), Drug))%>%
  rename(`Drug (by respiratory status)`=resp_status)%>%
  select(-Drug)%>%
  mutate(across(-c(Size,se), ~ifelse(is.na(.), " ",.)))%>%
  mutate(across(c(kappa_CI_lower, kappa_CI_upper, kappa, pabak, pabak_CI_lower, pabak_CI_upper), ~as.numeric(.)))->forest_plot_table_post_rand





###### overall ------


forest_plot_table_post_rand_overall<-
  forest_plot_table_post_rand%>%
  filter(!`Drug (by respiratory status)`%in%c("    No oxygen or ventilation",
                                              "    Oxygen requirement or non-invasive ventilation",
                                              "    Mechanical ventilation or ECMO"))%>%
  rename(Drug=`Drug (by respiratory status)`)%>%
  filter(Drug=="    Aggregate")%>%
  mutate(Drug=c("Corticosteroids",
                "Tocilizumab or sarilumab",
                "Macrolides",
                "Remdesivir",
                "Hydroxychloroquine",
                "Lopinavir-ritonavir",
                "Colchicine",
                "Baricitinib",
                "Aspirin",
                "REGN antibodies",
                "Combined"))



forest_plot_post_rand_overall<-forest(forest_plot_table_post_rand_overall[,1:14],
                                     est=list(forest_plot_table_post_rand_overall$kappa,
                                              forest_plot_table_post_rand_overall$pabak),
                                     lower=list(forest_plot_table_post_rand_overall$kappa_CI_lower,
                                                forest_plot_table_post_rand_overall$pabak_CI_lower),
                                     upper=list(forest_plot_table_post_rand_overall$kappa_CI_upper,
                                                forest_plot_table_post_rand_overall$pabak_CI_upper),
                                     sizes=list(forest_plot_table_post_rand_overall$se^2,
                                                forest_plot_table_post_rand_overall$se^2),
                                     ci_column = c(11,13),
                                     xlim=c(-1,1),
                                     arrow_lab = c("Negative agreement", "Positive agreement"),
                                     ref_line = 0)%>%
  insert_text(.,
              text = "Counts (participants or participant/drug pairs)",
              col = 2:6,
              part = "header",
              gp = gpar(fontface = "bold"))%>%
  insert_text(.,
              text = "Agreement metrics",
              col = 7:13,
              part = "header",
              gp = gpar(fontface = "bold"))%>%
  edit_plot(row=c(11),
            which="text",
            gp=gpar(fontface="bold"))%>%
  gtable_add_grob(grobs=segmentsGrob(
    x0 = unit(0,"npc"), # horizontal origin
    y0 = unit(0,"npc"), # vertical origin
    x1 = unit(0,"npc"), # horizontal end
    y1 = unit(1,"npc"), # vertical end
    gp = gpar(lwd = 2.0)), # width
    t = 4,   # top end
    b = 15,  # bottom end
    l = 8,  # left end
    r = 8 # right end
  )

forest_plot_post_rand_overall


ggsave(filename="Outputs/Figures/Analysis/forestplot_post_rand_binary_overall.png", 
       plot=forest_plot_post_rand_overall,
       dpi = "retina",
       units = "cm",
       width = get_wh(plot=forest_plot_post_rand_overall, unit="cm"),
       limitsize = F
       # height = 30
)



# alternative table

forest_plot_table_post_rand_overall%>%
  select(Drug,
         Total,
         `Both sources`,
         `CRF only`,
         `EPMA only`,
         `Neither source`,
         Sn=Sensitivity,
         Sp=Specificity,
         PPV,
         NPV,
         `Kappa (95% CI)`,
         `PABAK (95% CI)`)%>%
  flextable()%>%
  add_header_row(values=c("", "Participants/drug pairs in each source (%)", "Agreement metrics"), colwidths = c(1, 5, 6))%>%
  save_as_docx(path="Outputs/Tables/follow_up_agreement_overall.docx",
               pr_section = sect_properties)









###### by resp status--------


forest_plot_table_post_rand_resp_status<-
  forest_plot_table_post_rand%<>%
  filter(`Drug (by respiratory status)`!="    Aggregate")



forest_plot_post_rand_resp_status<-forest(forest_plot_table_post_rand_resp_status[,1:14],
                    est=list(forest_plot_table_post_rand_resp_status$kappa,
                             forest_plot_table_post_rand_resp_status$pabak),
                    lower=list(forest_plot_table_post_rand_resp_status$kappa_CI_lower,
                               forest_plot_table_post_rand_resp_status$pabak_CI_lower),
                    upper=list(forest_plot_table_post_rand_resp_status$kappa_CI_upper,
                               forest_plot_table_post_rand_resp_status$pabak_CI_upper),
                    sizes=list(forest_plot_table_post_rand_resp_status$se^2,
                               forest_plot_table_post_rand_resp_status$se^2),
                    ci_column = c(11,13),
                    xlim=c(-1,1),
                    arrow_lab = c("Negative agreement", "Positive agreement"),
                    ref_line = 0)%>%
  insert_text(.,
              text = "Counts (participants or participant/drug pairs)",
              col = 2:6,
              part = "header",
              gp = gpar(fontface = "bold"))%>%
  insert_text(.,
              text = "Agreement metrics",
              col = 7:13,
              part = "header",
              gp = gpar(fontface = "bold"))%>%
  edit_plot(row=c(40,41,42,43),
            which="text",
            gp=gpar(fontface="bold"))%>%
  gtable_add_grob(grobs=segmentsGrob(
    x0 = unit(0,"npc"), # horizontal origin
    y0 = unit(0,"npc"), # vertical origin
    x1 = unit(0,"npc"), # horizontal end
    y1 = unit(1,"npc"), # vertical end
    gp = gpar(lwd = 2.0)), # width
    t = 4,   # top end
    b = 47,  # bottom end
    l = 8,  # left end
    r = 8 # right end
  )

forest_plot_post_rand_resp_status


ggsave(filename="Outputs/Figures/Analysis/forestplot_post_rand_binary_resp_status.png", 
       plot=forest_plot_post_rand_resp_status,
       dpi = "retina",
       units = "cm",
       width = get_wh(plot=forest_plot_post_rand_resp_status, unit="cm"),
       limitsize = F
       # height = 30
)

# alternative table


forest_plot_table_post_rand_resp_status%>%
  select(`Drug (by respiratory status)`,
         Total,
         `Both sources`,
         `CRF only`,
         `EPMA only`,
         `Neither source`,
         Sn=Sensitivity,
         Sp=Specificity,
         PPV,
         NPV,
         `Kappa (95% CI)`,
         `PABAK (95% CI)`)%>%
  filter(`Drug (by respiratory status)`!="Combined (excluding oxygen)")%>%
  flextable()%>%
  add_header_row(values=c("", "Participants/drug pairs in each source (%)", "Agreement metrics"), colwidths = c(1, 5, 6))%>%
  width(j=1, 8, "cm")%>%
  width(j=c(11,12), 4, "cm")%>%
  #padding(i=c(1,5,9,13,17,21,25), j=1, padding.left=2, part="body")
  save_as_docx(path="Outputs/Tables/follow_up_agreement_by_resp_status.docx",
               pr_section = sect_properties)

##### barchart plot -----

post_rand_participant_counts%>%
  filter(resp_status=="Aggregate",
         Drug!="Combined",
         Drug!="Combined (excluding oxygen)")%>%
  select(Drug, EPMA_only, CRF_only, Both, Neither)%>%
  mutate(Either = EPMA_only+CRF_only+Both,
         EPMA = EPMA_only+Both,
         CRF = CRF_only+Both,
         Total=Either+Neither)%>%
  select(Drug, Either, CRF, EPMA, Total)%>%
  pivot_longer(-Drug, names_to = "Group", values_to = "Count")%>%
  group_by(Drug)%>%
  mutate(prop = round(Count/Count[Group=="Total"]*100,1))%>%
  filter(Group!="Neither",
         Group!="Total")%>%
  mutate(Group=fct_relevel(Group, 
                           # "Neither", 
                           "Either",
                           "CRF", "EPMA"
  ))->counts_for_follow_up_barchart
  
counts_for_follow_up_barchart%>%
  ggplot(aes(Drug, prop, fill=Group))+
  geom_col(position = "dodge")+
  geom_text(aes(label=paste0(prop, "%")),
            position = position_dodge(width = .9),
            vjust=-1,
            size=15/2.54)+
  labs(y="Proportion of total participants",
       fill="Data source",
       title="Follow-up exposure")+
  scale_y_continuous(
    # expand = expansion(c(0,0.2)),
                     limits=c(0,100),
                     breaks = seq(0,100,20))+
  theme(legend.position = "bottom",
        text=element_text(family="Mulish",
                          size=20),
        axis.title.x=element_blank(),
        axis.text.x = element_text(angle=30, 
                                   hjust=1, 
                                   # vjust=-1
        )) ->barchart2

barchart1/barchart2

ggsave(filename="Outputs/Figures/Analysis/barchart_counts.png", 
       dpi = "retina",
       units = "cm",
       width=60,
       height=30,
       limitsize = F
)


# slides

counts_for_follow_up_barchart%>%
  ggplot(aes(Drug, prop, fill=Group))+
  geom_col(position = "dodge")+
  geom_text(aes(label=paste0(prop, "%")),
            position = position_dodge(width = .9),
            vjust=-1,
            size=15/2.54,
            color="white")+
  labs(y="Proportion of total participants",
       fill="Data source",
       # title="Follow-up exposure"
       )+
  scale_y_continuous(
    # expand = expansion(c(0,0.2)),
    limits=c(0,100),
    breaks = seq(0,100,20))+
  oxpop_blue_panel+
  theme(legend.position = "right",
        text=element_text(family="Mulish",
                          size=20),
        axis.title.x=element_blank(),
        axis.text.x = element_text(angle=30, 
                                   hjust=1, 
                                   # vjust=-1
        ))

ggsave(filename="Outputs/Figures/Slides/barchart_follow_up.png", 
       dpi = "retina",
       units = "cm",
       width=60,
       height=30,
       limitsize = F
)


##### investigate records for people missed by EPMA ------

# lopinavir
post_rand_binary_combined_flags%>%
  filter(EPMA=="0", 
         CRF=="1", 
         Drug=="Lopinavir-ritonavir")%>%
  distinct(Study_ID)%>%.[[1]]->discordant_ids_lopi 
# 117 people  in CRF but not EPMA 


## check source data 

data_administration%>%
  select(Study_ID, ADMINISTRATION_ID_HASHED, PRESCRIPTION_ID_HASHED, MEDICATION_NAME, SCHEDULED_DATE_TIME, ADMINISTERED_DATE_TIME, RECORDED_DATE_TIME, LAST_UPDATED_DATE)%>%
  filter(Study_ID %in% discordant_ids_lopi)%>%
  left_join(censoring_dates)%>%
  View() # none

data_prescription%>%
  select(Study_ID, PRESCRIPTION_ID_HASHED, MEDICATION_NAME, INITIAL_AUTHORISED_DATE_TIME)%>%
  filter(Study_ID %in% discordant_ids_lopi)%>%
  left_join(censoring_dates)%>%
  View() # only 1 person




# steroids
post_rand_binary_combined_flags%>%
  filter(EPMA=="0", 
         CRF=="1", 
         Drug=="Corticosteroids")%>%
  distinct(Study_ID)%>%.[[1]]->discordant_ids_steroids_post_rand 

## check source data 

data_administration%>%
  select(Study_ID, ADMINISTRATION_ID_HASHED, PRESCRIPTION_ID_HASHED, MEDICATION_NAME, SCHEDULED_DATE_TIME, ADMINISTERED_DATE_TIME, RECORDED_DATE_TIME, LAST_UPDATED_DATE)%>%
  filter(Study_ID %in% discordant_ids_steroids_post_rand)%>%
  left_join(censoring_dates)%>%
  View() # none

data_prescription%>%
  select(Study_ID, PRESCRIPTION_ID_HASHED, MEDICATION_NAME, INITIAL_AUTHORISED_DATE_TIME)%>%
  filter(Study_ID %in% discordant_ids_steroids_post_rand)%>%
  left_join(censoring_dates)%>%
  View() # only 1 person



## check relationships of those records with randomisation date
epma_drug_flags%>%
  left_join(censoring_dates)%>%
  filter(Study_ID%in%discordant_ids_steroids_post_rand)%>%
  filter(Drug=="Corticosteroids")%>%
  arrange(Study_ID)%>%
  filter(Flag=="1")%>%
  distinct(Study_ID, PRESCRIPTION_ID_HASHED, admission_date, rand_date, date)%>%
  select(Study_ID,PRESCRIPTION_ID_HASHED, admission_date, rand_date, date)%>%
  mutate(days_after_rand=as.numeric(difftime(date, rand_date, units= "days")))%>%
  filter(days_after_rand>0)%>%
  arrange(days_after_rand)%>%
  View()


##### investigate records for people missed by CRF ------
# steroids

post_rand_binary_combined_flags%>%
  filter(EPMA=="1", 
         CRF=="0", 
         Drug=="Corticosteroids")%>%
  distinct(Study_ID)%>%.[[1]]->discordant_ids_steroids_post_rand

## check source data 

data_administration%>%
  select(Study_ID, ID = ADMINISTRATION_ID_HASHED, MEDICATION_NAME, Date = ADMINISTERED_DATE_TIME)%>%
  filter(Study_ID %in% discordant_ids_steroids_post_rand)%>%
  mutate(Table="Administration")%>%
  rbind(
    data_prescription%>%
      select(Study_ID, ID = PRESCRIPTION_ID_HASHED, MEDICATION_NAME, Date=INITIAL_AUTHORISED_DATE_TIME)%>%
      filter(Study_ID %in% discordant_ids_steroids_post_rand)%>%
      mutate(Table="Prescription"))%>%
  mutate(date=as.Date(Date, format="%Y-%m-%d"))%>%
  left_join(censoring_dates)%>%
  filter(str_detect(MEDICATION_NAME, regex("dexamethasone|prednisolone|hydrocortisone|methylprednisolone", ignore_case = T)))%>%
  filter(date>=rand_date,
         date<=right_censoring_date_death_discharge_28d)%>%
  # group_by(Study_ID, Table)%>%
  # summarise(n=n())%>%
  # filter(n>1)%>%
  # filter(Table=="Prescription")%>%
  # distinct(Study_ID)%>%
  View() # none


data_prescription%>%
      select(Study_ID, ID = PRESCRIPTION_ID_HASHED, MEDICATION_NAME, Date=INITIAL_AUTHORISED_DATE_TIME, STATUS)%>%
      filter(Study_ID %in% discordant_ids_steroids_post_rand)%>%
  mutate(date=as.Date(Date, format="%Y-%m-%d"))%>%
  left_join(censoring_dates)%>%
  filter(str_detect(MEDICATION_NAME, regex("dexamethasone|prednisolone|hydrocortisone|methylprednisolone", ignore_case = T)))%>%
  filter(date>=rand_date,
         date<=right_censoring_date_death_discharge_28d)%>%
  # group_by(Study_ID, Table)%>%
  # summarise(n=n())%>%
  # filter(n>1)%>%
  filter(STATUS=="Active")%>%
  distinct(Study_ID)%>%
  View() # none





















##### extract records for people with toci or REGN missed by CRF ------

post_rand_binary_combined_flags%>%
  filter(Drug=="Tocilizumab or sarilumab" | Drug == "REGN antibodies")%>%
  filter(CRF=="0", EPMA=="1")%>%
  select(-resp_status)->participants_missed_toci_regn

epma_drug_flags%>%
  filter(Study_ID %in% participants_missed_toci_regn$Study_ID,
         Drug=="Tocilizumab or sarilumab" | Drug == "REGN antibodies")%>%
  inner_join(participants_missed_toci_regn)%>%
  filter(EPMA==1, CRF==0, Flag==1)%>%
  left_join(data_prescription%>%select(PRESCRIPTION_ID_HASHED, MEDICATION_NAME_PRESCRIPTION=MEDICATION_NAME))%>%
  left_join(data_administration%>%select(ADMINISTRATION_ID_HASHED, MEDICATION_NAME_ADMINISTRATION=MEDICATION_NAME))%>%
  select(Study_ID, Drug, MEDICATION_NAME_PRESCRIPTION, MEDICATION_NAME_ADMINISTRATION, date, FORM, STATUS, TYPE)%>%
  left_join(censoring_dates%>%select(Study_ID, rand_date, right_censoring_date_death_discharge_28d))%>%
  filter(date>rand_date,
         date<=right_censoring_date_death_discharge_28d)%>%
  View() # some records for inactive/unused prescriptions


epma_drug_flags%>%
  filter(Study_ID %in% participants_missed_toci_regn$Study_ID,
         Drug=="Tocilizumab or sarilumab" | Drug == "REGN antibodies")%>%
  inner_join(participants_missed_toci_regn)%>%
  filter(EPMA==1, CRF==0, Flag==1)%>%
  left_join(data_prescription%>%select(PRESCRIPTION_ID_HASHED, MEDICATION_NAME_PRESCRIPTION=MEDICATION_NAME))%>%
  left_join(data_administration%>%select(ADMINISTRATION_ID_HASHED, MEDICATION_NAME_ADMINISTRATION=MEDICATION_NAME))%>%
  select(Study_ID, Drug, MEDICATION_NAME_PRESCRIPTION, MEDICATION_NAME_ADMINISTRATION, date, FORM, STATUS, TYPE)%>%
  left_join(censoring_dates%>%select(Study_ID, rand_date, right_censoring_date_death_discharge_28d))%>%
  filter(date>rand_date,
         date<=right_censoring_date_death_discharge_28d)%>%
  mutate(STATUS=replace_na(STATUS, "Missing"))%>%
  group_by(Study_ID, Drug)%>%
  summarise(any_active=if_else(any(STATUS=="Active"), 1, 0))%>%
  View() # 22/65 have active prescriptions


epma_drug_flags%>%
  
  filter(Study_ID %in% participants_missed_toci_regn$Study_ID,
         Drug=="Tocilizumab or sarilumab" | Drug == "REGN antibodies")%>%
  inner_join(participants_missed_toci_regn)%>%
  filter(EPMA==1, CRF==0, Flag==1)%>%
  filter(Table=="Administration")%>%
  left_join(data_administration%>%select(ADMINISTRATION_ID_HASHED, MEDICATION_NAME_ADMINISTRATION=MEDICATION_NAME))%>%
  select(Study_ID, Drug, MEDICATION_NAME_ADMINISTRATION, date)%>%
  left_join(censoring_dates%>%select(Study_ID, rand_date, right_censoring_date_death_discharge_28d))%>%
  filter(date>rand_date,
         date<=right_censoring_date_death_discharge_28d)%>%
  group_by(Study_ID, Drug)%>%
  summarise(any_active=if_else(any(!is.na(date)), 1, 0))%>%
  View() # 26/65 have administration records with valid administered dates


write_csv(participants_missed_toci_regn, "K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/DPhil_EPMA_RECOVERY/DPhil_EPMA_RECOVERY/Intermediate outputs/participants_missed_toci_regn.csv")



##### cross-check agreement vs Sn/Sp -------

kappa_baseline_results%>%
  left_join(baseline_drugs_participant_counts)%>%
  select(Drug, resp_status, pabak, kappa, Sensitivity, Specificity)%>%
  pivot_longer(c(kappa, pabak), names_to="metric", values_to="Value")%>%
  filter(resp_status=="Aggregate",
         !str_detect(Drug, "Combined"))%>%
  mutate(Event="Baseline")%>%
  rbind(
    kappa_post_rand_results%>%
      left_join(post_rand_participant_counts)%>%
      select(Drug, resp_status, pabak, kappa, Sensitivity, Specificity)%>%
      pivot_longer(c(kappa, pabak), names_to="metric", values_to="Value")%>%
      filter(resp_status=="Aggregate",
             !str_detect(Drug, "Combined"))%>%
      mutate(Event="Follow-up"))%>%
  mutate(Value=as.numeric(Value))%>%
  mutate(metric=if_else(metric=="kappa", "Kappa", "PABAK"))%>%
  
  ggplot(aes(Sensitivity, Specificity, size=Value, color=Drug))+
  geom_point(aes(alpha=Value),
    # show.legend = F
             )+
  scale_x_continuous(limits=c(0,100))+
  scale_y_continuous(limits=c(0,100))+
  geom_text_repel(aes(label=paste0(Drug, "\nK: ", round(Value, 2))),
                  alpha=1,
                  size=5,
                  show.legend = F,
                  force_pull = 10)+
  geom_abline(slope = 1, intercept=0, linetype="dashed")+
  facet_wrap(metric~Event)+
  theme(legend.position = "bottom",
        text=element_text(size=25))+
  scale_color_discrete(guide="none")+
  labs(size="Agreement metric value",
       alpha="Agreement metric value",
       subtitle="Split by event (baseline vs follow-up) and agreement metric (Kappa vs PABAK)")
  

ggsave(filename="Outputs/Figures/Analysis/sn_vs_sp_scatter.png", 
       dpi = "retina",
       units = "cm",
       width=60,
       height=40,
       limitsize = F
)



#### 4.2.2 Days/proportion of days exposed--------






##### extract EPMA counts -------

post_rand_epma_quantitative_exposure<-
  epma_drug_flags%>%
  left_join(censoring_dates%>%select(-c(admidate_crf, admidate_hes, admission_study_day, censoring_study_day)))%>%
  group_by(Study_ID)%>%
  filter(any(date>=admission_date) & any(date<=right_censoring_date_death_discharge_28d))%>% # restrict to people who have had a record within their randomisation admission
  filter(Drug %in% (crf_post_rand_drug_flags%>%distinct(Drug)%>%.[[1]]))%>% 
  
  group_by(Study_ID, Drug)%>%
  summarise(days_number = n_distinct(date[Flag==1 & date>rand_date & date<=right_censoring_date_death_discharge_28d]),
            dose_number = n_distinct(date[Flag==1 & Drug%in%c("Tocilizumab or sarilumab") & date>rand_date & date<=right_censoring_date_death_discharge_28d]))%>%
  
  mutate(dose_number=ifelse(dose_number>2,2,dose_number))%>%
  
  mutate(days_number=if_else(days_number>10 & Drug!="Aspirin", 10L, days_number))%>% # to align with possible answers in the CRF
  
  left_join(censoring_dates%>%
              select(Study_ID,
                     analysis_period_duration=censoring_study_day)%>%
              mutate(Drug="Aspirin"),
            by=c("Study_ID", "Drug")
            )%>%
  
  mutate(days_proportion=as.integer(days_number/analysis_period_duration*100))%>%
  
  select(-analysis_period_duration)%>%

  mutate(across(everything(), ~as.character(.)))%>%
  mutate(across(everything(), ~replace_na(., "0")))%>% # assign 0 to absent records
  pivot_longer(-c(Study_ID, Drug), names_to = "Variable", values_to = "EPMA")%>%
  mutate(EPMA=as.numeric(EPMA))%>%
  mutate(EPMA_proportion=case_when(Drug=="Aspirin" & Variable=="days_proportion" & EPMA >=90 ~ "≥90%",
                        Drug=="Aspirin" & Variable=="days_proportion" & EPMA >=50 & EPMA <90 ~ "≥50-90%",
                        Drug=="Aspirin" & Variable=="days_proportion" & EPMA >0 & EPMA<50 ~ "0-50%"))%>%
  mutate(EPMA=as.character(EPMA))%>%
  mutate(EPMA=if_else(is.na(EPMA_proportion), EPMA, EPMA_proportion))%>%
  select(-EPMA_proportion)
  

##### combine with CRF data -------

combined_post_rand_quantitative_exposure<-
  crf_post_rand_drug_flags%>%
  filter(Study_ID %in% participants_with_data_in_randomisation_admission,
         Drug!="Aspirin")%>% # restrict to people with administration data in the admission of randomisation
  rbind(
    crf_post_rand_drug_flags%>%
      filter(Drug=="Aspirin")%>%
      filter(Study_ID %in% aspirin_participant_list,
             Study_ID %in% participants_with_data_in_randomisation_admission)
  )%>%
  filter(Study_ID %in% participants_with_data_in_randomisation_admission)%>% # restrict to people who have available EPMA data
  filter(Variable!="flag")%>%
  mutate(Variable=if_else(Variable=="days_proportion_aspirin", "days_proportion", Variable))%>%
  rename(CRF=Value)%>%
  rbind(
    crf_post_rand_drug_flags%>%
      filter(Variable=="flag",
             Drug=="Aspirin",
              Value=="0")%>%
      mutate(Variable="days_proportion")%>%
      rename(CRF=Value)
  )%>%
  
  
  left_join( # by using left join we restrict the comparison to people who have the corresponding question answered in the CRF
    post_rand_epma_quantitative_exposure,
  by=c("Study_ID", "Drug", "Variable")
  )%>%
  mutate(across(c(EPMA, CRF), ~replace_na(., "0")))%>%
  filter((Variable == "days_number" & CRF!=0 & EPMA!=0 )|
           (Variable!="days_number" & CRF !=0 & EPMA!=0))

##### calculate intraclass correlation coefficient ------

drugs<-combined_post_rand_quantitative_exposure%>%
  filter(Variable=="days_number")%>%
  distinct(Drug)%>%
  .[[1]]

icc_result<-list()

for (i in drugs){
  
  icc_result[[i]]<-
    combined_post_rand_quantitative_exposure%>%
    ungroup()%>%
    filter(Variable=="days_number",
           Drug==i)%>%
    select(-c(Variable, Study_ID, Drug))%>%
    mutate(across(everything(), ~as.numeric(.)))%>%
    # filter(CRF>0, EPMA>0)%>%
    icc(model="twoway", # two way model as we are interested in differences between sources and not just between subjects
        type="agreement", # agreement type defined as absolute agreement vs consistency as we are more interested in quantifying absolute agreement (difference in days) rather than consistency between measurements
        unit="single") # unit measurement type is one single measurement in each source for each subject
    # ref: Shrout, P.E., & Fleiss, J.L. (1979), Intraclass correlation: uses in assessing rater reliability. Psychological Bulletin, 86, 420-428.
  
  # icc_result_table%<>%bind_rows(list(icc_result))
  
  rm(i)
  
}

# build table with results

icc_results_table<-data_frame() # create empty dataframe

for (i in drugs) { 
  
  name<-i
  
  try(
    table<-(data.frame( # this will extract the individual parameters from each element of the results list (for each drug group) and store them as new variables
      Drug = name,
      pairs = unlist(icc_result[[i]]$subjects),
      icc = unlist(icc_result[[i]]$value),
      CI_lower = unlist(icc_result[[i]]$lbound),
      CI_upper = unlist(icc_result[[i]]$ubound),
      p_value = unlist(icc_result[[i]]$p.value)
    ))
  )
  
  try(icc_results_table%<>%bind_rows(table)) # bind results to the results table
  
  
  rm(table, name, i)
  
}

icc_results_table%<>%
  mutate(CI=paste0(round(CI_lower,1), " - ", round(CI_upper,1)))



##### BA plot (number of days) -----

bland_altman_plot_data<-
  combined_post_rand_quantitative_exposure%>%
  ungroup()%>%
  filter(Variable=="days_number")%>%
  select(-Variable)%>%
  rowwise()%>%
  mutate(across(c(CRF, EPMA), ~as.numeric(.)))%>%
  # filter(CRF>0, EPMA>0)%>%
  mutate(mean=(CRF+EPMA)/2,
         difference = CRF-EPMA)%>%
  ungroup()
  
bland_altman_plot_summary_stats<-
  bland_altman_plot_data%>%
  ungroup()%>%
  group_by(Drug)%>%
  summarise(mean_diff=mean(difference),
            lower_bound=mean_diff-1.96*sd(difference),
            upper_bound=mean_diff+1.96*sd(difference))


bland_altman_plot_data%>%
  mutate(across(c(CRF, EPMA, mean, difference), ~as.numeric(.)))%>%
  filter(CRF>0 & EPMA>0)%>%
  ggplot(aes(mean, difference, color=Drug))+
  # geom_histogram(aes(x=mean,y=NA,group=Drug))
  geom_point(alpha=0.5,
             size=4)+
  geom_hline(data=bland_altman_plot_summary_stats,
             aes(group=Drug, yintercept=mean_diff),
             linetype="dashed") +
  geom_hline(yintercept=0, color="black") +
  geom_hline(data=bland_altman_plot_summary_stats,
             aes(group=Drug, yintercept=lower_bound), 
             color = "red", linetype="dashed") +
  geom_hline(data=bland_altman_plot_summary_stats,
             aes(group=Drug, yintercept=upper_bound), 
             color = "red", linetype="dashed")+
  facet_wrap(~Drug)+
  geom_text(data=bland_altman_plot_summary_stats,
            aes(group=Drug, y=mean_diff+0.5,x=7.5,
                label = paste0("Mean difference: ", round(mean_diff, 2))),
            color="black",
            hjust=0)+
  geom_text(data=bland_altman_plot_summary_stats,
            aes(group=Drug, y=lower_bound-0.5,x=7.5,
                label=paste0("95% CI: ", round(lower_bound, 2))), 
                color="red",
            hjust=0)+
  geom_text(data=bland_altman_plot_summary_stats,
          aes(group=Drug, y=upper_bound+0.5,x=7.5,
              label=paste0("95% CI: ", round(upper_bound, 2))), 
          color="red",
          hjust=0)+
  geom_text(.%>%filter(Drug=="Baricitinib")%>%slice_head(n=1),
            mapping=aes(x= 1, y=10), 
            label="CRF days > EPMA days",
            color="black")+
  geom_text(.%>%filter(Drug=="Baricitinib")%>%slice_head(n=1),
            mapping=aes(x= 1, y=-10), 
            label="CRF days < EPMA days",
            color="black")+
  geom_text(data=icc_results_table,
            mapping=aes(group=Drug, y=10,x=7.5,
                label=paste0("ICC: ", round(icc,1), " (", CI,")\nPairs: ", pairs)),
            color="black",
            hjust=0)+
  ggtitle("Bland-Altman Plot for number of days of exposure after randomisation in the CRF vs EPMA data") +
  ylab("Difference\n(CRF minus EPMA)") +
  xlab("Average")+
  theme(legend.position = "none",
        text = element_text(family="Mulish",
                            size=20),
        plot.caption = element_text(hjust = 0),
        plot.caption.position =  "plot",
        plot.title.position = "plot",
        panel.grid.minor = element_blank())+
  scale_x_continuous(breaks=seq(0,10,1), limits=c(0,10))+
  scale_y_continuous(breaks=seq(-10,10,2), limits=c(-10,10))
  

ggsave("Outputs/Figures/Analysis/bland_altman_plot_post_rand_exposure.png",
       dpi="retina",
       width=60,
       height=30,
       units="cm")


###### check ICC and average differences (for table)-----



# join aggregate summaries for ICC

icc_result[["Combined"]]<-
  combined_post_rand_quantitative_exposure%>%
  ungroup()%>%
  filter(Variable=="days_number")%>%
  select(-c(Variable, Study_ID, Drug))%>%
  mutate(across(everything(), ~as.numeric(.)))%>%
  # filter(CRF>0, EPMA>0)%>%
  icc(model="twoway", # two way model as we are interested in differences between sources and not just between subjects
      type="agreement", # agreement type defined as absolute agreement vs consistency as we are more interested in quantifying absolute agreement (difference in days) rather than consistency between measurements
      unit="single") # unit measurement type is one single measurement in each source for each subject
# ref: Shrout, P.E., & Fleiss, J.L. (1979), Intraclass correlation: uses in assessing rater reliability. Psychological Bulletin, 86, 420-428.

icc_results_table%<>%bind_rows(
  data.frame( # this will extract the individual parameters from each element of the results list (for each drug group) and store them as new variables
    Drug = "Combined",
    pairs = unlist(icc_result[["Combined"]]$subjects),
    icc = unlist(icc_result[["Combined"]]$value),
    CI_lower = unlist(icc_result[["Combined"]]$lbound),
    CI_upper = unlist(icc_result[["Combined"]]$ubound),
    p_value = unlist(icc_result[["Combined"]]$p.value)
  ))

# join aggregate summaries for average difference


bland_altman_plot_summary_stats%<>%
  bind_rows(
  bland_altman_plot_data%>%
  group_by(Drug="Combined")%>%
    summarise(mean_diff=mean(difference),
            lower_bound=mean_diff-1.96*sd(difference),
            upper_bound=mean_diff+1.96*sd(difference))
  )


View(icc_results_table)

View(bland_altman_plot_summary_stats)

##### heatmap (number of days) ------

bland_altman_plot_data%>%
  filter(CRF>0 & EPMA>0)%>%
  group_by(Drug, CRF, EPMA)%>%
  summarise(n=n_distinct(Study_ID))%>%
  group_by(Drug)%>%
  mutate(prop=round(n/sum(n)*100,1))%>%
  # mutate(across(everything(), ~as.numeric(.)))%>%
  ggplot(aes(x=CRF, y=EPMA, fill=prop))+
  geom_tile(
    # aes(alpha=n)
    )+
  facet_wrap2(~Drug,
             scales="free")+
  # scale_fill_viridis(limits=c(0, 300))+
  scale_fill_gradient(
    # breaks=seq(0,100,20),
                      limits=c(0, 30),
                      low="white",
                      high="red")+
  scale_y_reverse(breaks=seq(1,10,1),
                   # trans="reverse"
                  )+
  scale_x_continuous(breaks=seq(1,10,1),
                     position = "top")+
  geom_text(aes(label=n),
            size=8
            )+
  # geom_text(data=.%>%
  #             filter(Drug=="Corticosteroids",
  #                    CRF==10,
  #                    EPMA==10),
  #           size=2,
  #           aes(label=n))+
  labs(x="Number of days (CRF)",
       y="Number of days (EPMA)",
       fill="Proportion of total for each drug (%)")+
  theme(text=element_text(size=30,
                          family="Mulish"),
        strip.placement = "outside",
        legend.position = "bottom")

ggsave("Outputs/Figures/Analysis/heatmap_post_rand_exposure.png",
       width=60,
       height=35,
       dpi="retina",
       units = "cm")  

# slides

bland_altman_plot_data%>%
  filter(CRF>0 & EPMA>0)%>%
  group_by(Drug, CRF, EPMA)%>%
  summarise(n=n_distinct(Study_ID))%>%
  group_by(Drug)%>%
  mutate(prop=round(n/sum(n)*100,1))%>%
  # mutate(across(everything(), ~as.numeric(.)))%>%
  ggplot(aes(x=CRF, y=EPMA, fill=prop))+
  geom_tile(
    # aes(alpha=n)
  )+
  facet_wrap2(~Drug,
              scales="free")+
  # scale_fill_viridis(limits=c(0, 300))+
  scale_fill_gradient(
    # breaks=seq(0,100,20),
    limits=c(0, 30),
    low="white",
    high="red")+
  scale_y_reverse(breaks=seq(1,10,1),
                  # trans="reverse"
  )+
  scale_x_continuous(breaks=seq(1,10,1),
                     position = "top")+
  geom_text(aes(label=n),
            size=8,
            color="black"
  )+
  # geom_text(data=.%>%
  #             filter(Drug=="Corticosteroids",
  #                    CRF==10,
  #                    EPMA==10),
  #           size=2,
  #           aes(label=n))+
  labs(x="Number of days (CRF)",
       y="Number of days (EPMA)",
       fill="Proportion of total for each drug (%)")+
  oxpop_blue_panel+
  theme(text=element_text(size=20,
                          family="Mulish"),
        strip.placement = "outside",
        legend.position = "bottom")

ggsave("Outputs/Figures/Slides/heatmap_post_rand_exposure.png",
       width=60,
       height=35,
       dpi="retina",
       units = "cm")  













##### heatmap (number of doses/proportion of days) ------

combined_post_rand_quantitative_exposure%>%
  ungroup()%>%
  filter(Variable!="days_number")%>%
  filter(Drug %in% c("Tocilizumab or sarilumab",
                     "Aspirin"))%>%
  filter(Drug=="Aspirin")%>%
  group_by(Drug, CRF, EPMA)%>%
  summarise(n=n_distinct(Study_ID))%>%
  ungroup()%>%
  mutate(prop=round(n/sum(n)*100,0))%>%
  ungroup()%>%
  mutate(across(c(`CRF`, `EPMA`), ~factor(., levels=c("0-50%",
                                                     "≥50-90%",
                                                     "≥90%"))))%>%
  mutate(EPMA=fct_rev(EPMA))%>%
  
  ggplot(aes(x=CRF, y=EPMA, fill=prop))+
  geom_tile()+
  scale_x_discrete(position="top")+
  scale_fill_gradient(breaks=seq(0,100,20),
                     limits=c(0, 100),
                     low="white",
                     high="red")+
  geom_text(aes(label=paste0(n, " (", prop, "%)")),
    size=20/2.53)+
  labs(x="CRF",
       y="EPMA",
       subtitle="Aspirin (proportion of days received)",
       fill="Participants (%)")+
  theme(text=element_text(size=30,
                          family="Mulish"),
        legend.position = "none",
        legend.key.size = unit(2, "cm"),
        strip.placement = "outside")->p1



combined_post_rand_quantitative_exposure%>%
  ungroup()%>%
  filter(Variable!="days_number")%>%
  filter(Drug %in% c("Tocilizumab or sarilumab",
                     "Aspirin"))%>%
  filter(Drug!="Aspirin")%>%
  mutate(across(c(CRF, EPMA), ~if_else(.==2, "2 or more", "1")))%>%
  group_by(Drug, CRF, EPMA)%>%
  summarise(n=n_distinct(Study_ID))%>%
  ungroup()%>%
  mutate(prop=round(n/sum(n)*100,0))%>%
  mutate(EPMA=fct_rev(EPMA))%>%
  
  ggplot(aes(x=CRF, y=EPMA, fill=prop))+
  geom_tile()+

  scale_fill_gradient(breaks=seq(0,100,20),
                     limits=c(0, 100),
                     low="white",
                     high="red")+
  scale_x_discrete(position="top")+
  geom_text(aes(label=paste0(n, " (", prop, "%)")),
    size=20/2.53)+
  labs(x="CRF",
       y="EPMA",
       subtitle="Tocilizumab/sarilumab (number of doses)",
       fill="Participants (%)")+
  theme(text=element_text(size=30,
                          family="Mulish"),
        legend.position = "bottom",
        legend.key.size = unit(2, "cm"),
        strip.placement = "outside")->p2


heatmap_doses<-p1+plot_spacer()+p2+plot_layout(widths = c(4,1,4))

heatmap_doses



ggsave("Outputs/Figures/Analysis/heatmap_post_rand_exposure_doses.png",
       width=70,
       height=35,
       dpi="retina",
       units = "cm")  

# slides


combined_post_rand_quantitative_exposure%>%
  ungroup()%>%
  filter(Variable!="days_number")%>%
  filter(Drug %in% c("Tocilizumab or sarilumab",
                     "Aspirin"))%>%
  filter(Drug=="Aspirin")%>%
  group_by(Drug, CRF, EPMA)%>%
  summarise(n=n_distinct(Study_ID))%>%
  ungroup()%>%
  mutate(prop=round(n/sum(n)*100,0))%>%
  ungroup()%>%
  mutate(across(c(`CRF`, `EPMA`), ~factor(., levels=c("0-50%",
                                                      "≥50-90%",
                                                      "≥90%"))))%>%
  mutate(EPMA=fct_rev(EPMA))%>%
  
  ggplot(aes(x=CRF, y=EPMA, fill=prop))+
  geom_tile()+
  scale_x_discrete(position="top")+
  scale_fill_gradient(breaks=seq(0,100,20),
                      limits=c(0, 100),
                      low="white",
                      high="red")+
  geom_text(aes(label=paste0(n, " (", prop, "%)")),
            size=15/2.53)+
  labs(x="CRF",
       y="EPMA",
       subtitle="Aspirin (proportion of days received)",
       fill="Participants (%)")+
  oxpop_blue_panel+
  theme(text=element_text(size=20,
                          family="Mulish"),
        legend.position = "none",
        legend.key.size = unit(2, "cm"),
        strip.placement = "outside")->p1



combined_post_rand_quantitative_exposure%>%
  ungroup()%>%
  filter(Variable!="days_number")%>%
  filter(Drug %in% c("Tocilizumab or sarilumab",
                     "Aspirin"))%>%
  filter(Drug!="Aspirin")%>%
  mutate(across(c(CRF, EPMA), ~if_else(.==2, "1 or more", "1")))%>%
  group_by(Drug, CRF, EPMA)%>%
  summarise(n=n_distinct(Study_ID))%>%
  ungroup()%>%
  mutate(prop=round(n/sum(n)*100,0))%>%
  mutate(EPMA=fct_rev(EPMA))%>%
  
  ggplot(aes(x=CRF, y=EPMA, fill=prop))+
  geom_tile()+
  
  scale_fill_gradient(breaks=seq(0,100,20),
                      limits=c(0, 100),
                      low="white",
                      high="red")+
  scale_x_discrete(position="top")+
  geom_text(aes(label=paste0(n, " (", prop, "%)")),
            size=15/2.53)+
  labs(x="CRF",
       y="EPMA",
       subtitle="Tocilizumab/sarilumab (number of doses)",
       fill="Participants (%)")+
  oxpop_blue_panel+
  theme(text=element_text(size=20,
                          family="Mulish"),
        legend.position = "bottom",
        legend.key.size = unit(2, "cm"),
        strip.placement = "outside",
        plot.margin = unit(c(0,0,0,40), "pt"))->p2


heatmap_doses<-p1+
  # plot_spacer()+
  p2+
  plot_layout(widths = c(4,
                         # 1,
                         4))+
  plot_annotation(theme=theme(plot.background = element_rect(fill="transparent", color=NA)))


ggsave("Outputs/Figures/Slides/heatmap_post_rand_exposure_doses.png",
       width=60,
       height=35,
       dpi="retina",
       units = "cm")  


# ### alternative color scheme
# 
# 
# combined_post_rand_quantitative_exposure%>%
#   ungroup()%>%
#   filter(Variable!="days_number")%>%
#   filter(Drug %in% c("Tocilizumab or sarilumab",
#                      "Aspirin"))%>%
#   filter(Drug=="Aspirin")%>%
#   group_by(Drug, CRF, EPMA)%>%
#   summarise(n=n_distinct(Study_ID))%>%
#   ungroup()%>%
#   mutate(prop=round(n/sum(n)*100,0))%>%
#   ungroup()%>%
#   mutate(across(c(`CRF`, `EPMA`), ~factor(., levels=c("0-50%",
#                                                       "≥50-90%",
#                                                       "≥90%"))))%>%
#   mutate(EPMA=fct_rev(EPMA))%>%
#   
#   ggplot(aes(x=CRF, y=EPMA, fill=prop))+
#   geom_tile()+
#   scale_x_discrete(position="top")+
#   scale_fill_viridis(breaks=seq(0,100,20),
#                       limits=c(0, 100))+
#   geom_text(aes(label=paste0(n, " (", prop, "%)")),
#             size=15/2.53,
#             color="white")+
#   labs(x="CRF",
#        y="EPMA",
#        subtitle="Aspirin (proportion of days received)",
#        fill="Participants (%)")+
#   theme(text=element_text(size=20,
#                           family="Mulish"),
#         legend.position = "none",
#         legend.key.size = unit(2, "cm"),
#         strip.placement = "outside")->p1
# 
# 
# 
# combined_post_rand_quantitative_exposure%>%
#   ungroup()%>%
#   filter(Variable!="days_number")%>%
#   filter(Drug %in% c("Tocilizumab or sarilumab",
#                      "Aspirin"))%>%
#   filter(Drug!="Aspirin")%>%
#   group_by(Drug, CRF, EPMA)%>%
#   summarise(n=n_distinct(Study_ID))%>%
#   ungroup()%>%
#   mutate(prop=round(n/sum(n)*100,0))%>%
#   mutate(EPMA=fct_rev(EPMA))%>%
#   
#   ggplot(aes(x=CRF, y=EPMA, fill=prop))+
#   geom_tile()+
#   
#   scale_fill_viridis(breaks=seq(0,100,20),
#                       limits=c(0, 100))+
#   scale_x_discrete(position="top")+
#   geom_text(aes(label=paste0(n, " (", prop, "%)")),
#             size=15/2.53,
#             color="white")+
#   labs(x="CRF",
#        y="EPMA",
#        subtitle="Tocilizumab/sarilumab (number of doses)",
#        fill="Participants (%)")+
#   theme(text=element_text(size=20,
#                           family="Mulish"),
#         legend.position = "bottom",
#         legend.key.size = unit(2, "cm"),
#         strip.placement = "outside")->p2
# 
# 
# heatmap_doses<-p1+p2
# 
# heatmap_doses
# 
# 
# 
# ggsave("Outputs/Figures/Analysis/heatmap_post_rand_exposure_doses_viridis.png",
#        width=60,
#        height=35,
#        dpi="retina",
#        units = "cm")  








###### investigate participants with aspirin >90% CRF but 50-90% EPMA -----

combined_post_rand_quantitative_exposure%>%
  filter(Drug=="Aspirin",
         EPMA=="≥50-90%",
         CRF=="≥90%")%>%
  distinct(Study_ID)%>%
  .[[1]]->discordant_ids_aspirin_follow_up


data_administration%>%
  select(Study_ID, MEDICATION_NAME, contains("date"))%>%
  filter(Study_ID %in% discordant_ids_aspirin_follow_up)%>%
  left_join(censoring_dates)%>%
  select(Study_ID, admission_date, rand_date, MEDICATION_NAME, ADMINISTERED_DATE_TIME,  LAST_UPDATED_DATE, right_censoring_date_death_discharge_28d, censoring_study_day)%>%
  left_join(post_rand_epma_quantitative_exposure%>%
            filter(Drug=="Aspirin",
                   Variable=="days_number"))%>%
  View()
  # data seems to be correct in ~10 cases reviewed manually

###### calculate kappas ------

# aspirin

  epi.kappa(combined_post_rand_quantitative_exposure%>%
              ungroup()%>%
              filter(Drug=="Aspirin")%>%
              select(-Study_ID, -Variable, -Drug)%>%
              table()%>%
              as.matrix(), 
            method="cohen", 
            alternative = "greater", 
            conf.level=0.95)

# tocilizumab
epi.kappa(combined_post_rand_quantitative_exposure%>%
            ungroup()%>%
            filter(Drug=="Tocilizumab or sarilumab")%>%
            select(-Study_ID, -Variable, -Drug)%>%
            table()%>%
            as.matrix(), 
          method="cohen", 
          alternative = "greater", 
          conf.level=0.95)












## 4.3 Sensitivity/specificity over time --------

sn_sp_over_time<-
baseline_combined_flags%>%
  select(-resp_status)%>%
  left_join(censoring_dates%>%select(Study_ID, rand_date))%>%
  mutate(Event="Baseline")%>%
  rbind(
    post_rand_binary_combined_flags%>%
      select(-resp_status)%>%
      left_join(censoring_dates%>%select(Study_ID, rand_date))%>%
      mutate(Event="Follow=up"))%>%
  mutate(month=as.Date(paste0(strftime(rand_date, "%Y-%m"), "-01"),
                       format="%Y-%m-%d"))%>%
    mutate(Period = case_when(month>="2020-03-01" & month <="2020-05-01" ~ "Mar 20 - May 20",
                              month>="2020-06-01" & month <="2020-08-01" ~ "Jun 20 - Aug 20",
                              month>="2020-09-01" & month <="2020-11-01" ~ "Sep 20 - Nov 20",
                              month>="2020-12-01" & month <="2021-02-01" ~ "Dec 20 - Feb 21",
                              month>="2021-03-01" & month <="2021-05-01" ~ "Mar 21 - May 21",
                              month>="2021-06-01" & month <="2021-08-01" ~ "Jun 21 - Aug 21",
                              month>="2021-09-01" & month <="2021-11-01" ~ "Sep 21 - Nov 21",
                              
                              month>="2021-12-01" & month <="2022-02-01" ~ "Dec 21 - Feb 22",
                              month>="2022-03-01" & month <="2022-05-01" ~ "Mar 22 - May 22"))%>%
  mutate(Period=factor(Period, levels=c("Mar 20 - May 20",
                                        "Jun 20 - Aug 20",
                                        "Sep 20 - Nov 20",
                                        "Dec 20 - Feb 21",
                                        "Mar 21 - May 21",
                                        "Jun 21 - Aug 21",
                                        "Sep 21 - Nov 21",
                                        "Dec 21 - Feb 22",
                                        "Mar 22 - May 22")))%>%
  mutate(Period=fct_relevel(Period, "Mar 20 - May 20",
                            "Jun 20 - Aug 20",
                            "Sep 20 - Nov 20",
                            "Dec 20 - Feb 21",
                            "Mar 21 - May 21",
                            "Jun 21 - Aug 21",
                            "Sep 21 - Nov 21",
                            "Dec 21 - Feb 22",
                            "Mar 22 - May 22"))%>%
  group_by(Drug, Period, Event)%>%
  summarise(participants = n_distinct(Study_ID),
            positives = length(CRF[CRF=="1"]),
            negatives = length(CRF[CRF=="0"]),
            true_positives = length(CRF[CRF=="1" & EPMA=="1"]),
            true_negatives = length(CRF[CRF=="0" & EPMA=="0"]),
            false_positives = length(CRF[CRF=="0" & EPMA =="1"]),
            false_negatives = length(CRF[CRF =="1" & EPMA == "0"]),
            Sensitivity = true_positives/positives,
            Specificity = true_negatives/negatives,
            PPV = round(true_positives/(true_positives+false_positives)*100, 1),
            NPV = round(true_negatives/(true_negatives+false_negatives)*100, 1),
            Both = sum(EPMA=="1" & CRF=="1"),
            CRF_only = sum(EPMA=="0" & CRF=="1"),
            EPMA_only = sum(EPMA=="1" & CRF=="0"),
            Neither = sum(EPMA=="0" & CRF=="0"))%>%
  select(Drug, Period, Event, Sensitivity, Specificity, participants)%>%
  pivot_longer(c(Sensitivity, Specificity), values_to = "Value", names_to = "Statistic")%>%
  mutate(SE = sqrt(Value*(1-Value)/participants))
  
  # plot
sn_sp_over_time%>%
  filter(Event=="Baseline")%>%
  ggplot(aes(Period, Value, color=Statistic, group=Statistic))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=Value-SE,
                    ymax=Value+SE),
                width=0.2)+
  facet_grid(~Drug)+
  scale_y_continuous(
    labels = scales::percent,
                  breaks=seq(0,1,0.1)
                     )+
  theme(legend.position = "none",
        text=element_text(size=20),
        axis.text.x=element_text(angle=30, hjust=1, vjust=1, size=10))+

  # geom_rect(data=.%>%filter(Drug=="Hydroxychloroquine"),
  #           aes(xmin=as.Date('2020-03-01', format="%Y-%m-%d"),
  #               xmax=as.Date('2020-06-05', format="%Y-%m-%d"),
  #               ymin=-Inf,
  #               ymax=Inf),
  #           fill="light green",
  #           color=NA,
  #           alpha=0.01)+
  # geom_rect(data=.%>%filter(Drug=="Tocilizumab or sarilumab"),
  #           aes(xmin=as.Date('2020-04-01', format="%Y-%m-%d"),
  #               xmax=as.Date('2021-02-11', format="%Y-%m-%d"),
  #               ymin=-Inf,
  #               ymax=Inf),
  #           fill="light green",
  #           color=NA,
  #           alpha=0.01)+
  # geom_rect(data=.%>%filter(Drug=="Baricitinib"),
  #           aes(xmin=as.Date('2021-02-02', format="%Y-%m-%d"),
  #               xmax=as.Date('2022-03-03', format="%Y-%m-%d"),
  #               ymin=-Inf,
  #               ymax=Inf),
  #           fill="light green",
  #           color=NA,
  #           alpha=0.01)+
  # geom_rect(data=.%>%filter(Drug=="Antiplatelets"),
  #           aes(xmin=as.Date('2020-11-01', format="%Y-%m-%d"),
  #               xmax=as.Date('2021-03-21', format="%Y-%m-%d"),
  #               ymin=-Inf,
  #               ymax=Inf),
  #           fill="light green",
  #           color=NA,
  #           alpha=0.01)+
  # geom_rect(data=.%>%filter(Drug=="Macrolides"),
  #           aes(xmin=as.Date('2020-04-07', format="%Y-%m-%d"),
  #               xmax=as.Date('2020-11-27', format="%Y-%m-%d"),
  #               ymin=-Inf,
  #               ymax=Inf),
  #           fill="light green",
  #           color=NA,
  #           alpha=0.01)+
  # scale_x_date()+
  labs(subtitle="Baseline exposure",
                              # title="Sensitivy and specificity of EPMA data vs CRF data over time"
            )->p1

p1

sn_sp_over_time%>%
  filter(Event=="Follow=up")%>%
  ggplot(aes(Period, Value, color=Statistic, group=Statistic))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=Value-SE,
                    ymax=Value+SE),
                width=0.2)+
  facet_grid(~Drug,
             labeller = label_wrap_gen(width = 15))+
  scale_y_continuous(
    labels = scales::percent,
    breaks=seq(0,1,0.1)
  )+  
  theme(legend.position = "bottom",
        text=element_text(size=20),
        axis.text.x=element_text(angle=30, hjust=1, vjust=1, size=10))+

  # geom_rect(data=.%>%filter(Drug=="Hydroxychloroquine"),
  #           aes(xmin=as.Date('2020-03-01', format="%Y-%m-%d"),
  #               xmax=as.Date('2020-06-05', format="%Y-%m-%d"),
  #               ymin=-Inf,
  #               ymax=Inf),
  #           fill="light green",
  #           color=NA,
  #           alpha=0.01)+
  # geom_rect(data=.%>%filter(Drug=="Tocilizumab or sarilumab"),
  #           aes(xmin=as.Date('2020-04-01', format="%Y-%m-%d"),
  #               xmax=as.Date('2021-02-11', format="%Y-%m-%d"),
  #               ymin=-Inf,
  #               ymax=Inf),
  #           fill="light green",
  #           color=NA,
  #           alpha=0.01)+
  # geom_rect(data=.%>%filter(Drug=="Baricitinib"),
  #           aes(xmin=as.Date('2021-02-02', format="%Y-%m-%d"),
  #               xmax=as.Date('2022-03-03', format="%Y-%m-%d"),
  #               ymin=-Inf,
  #               ymax=Inf),
  #           fill="light green",
  #           color=NA,
  #           alpha=0.01)+
  # geom_rect(data=.%>%filter(Drug=="Antiplatelets"),
  #           aes(xmin=as.Date('2020-11-01', format="%Y-%m-%d"),
  #               xmax=as.Date('2021-03-21', format="%Y-%m-%d"),
  #               ymin=-Inf,
  #               ymax=Inf),
  #           fill="light green",
  #           color=NA,
  #           alpha=0.01)+
  # geom_rect(data=.%>%filter(Drug=="Macrolides"),
  #           aes(xmin=as.Date('2020-04-07', format="%Y-%m-%d"),
  #               xmax=as.Date('2020-11-27', format="%Y-%m-%d"),
  #               ymin=-Inf,
  #               ymax=Inf),
  #           fill="light green",
  #           color=NA,
  #           alpha=0.01)+
  # scale_x_date()+
  labs(subtitle="Follow-up exposure")->p2


p1/p2



ggsave("Outputs/Figures/Analysis/sensitivity_specificity_time_series.png",
       width=60,
       height=35,
       dpi="retina",
       units = "cm")  

ggsave("Outputs/Figures/Analysis/sensitivity_specificity_time_series.tiff",
       width=60,
       height=35,
       dpi="retina",
       units = "cm")  


# slides 

sn_sp_over_time%>%
  filter(Event=="Baseline")%>%
  ggplot(aes(Period, Value, color=Statistic, group=Statistic))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=Value-SE,
                    ymax=Value+SE),
                width=0.2)+
  facet_grid(~Drug,
             labeller = label_wrap_gen(width = 15))+
  scale_y_continuous(
    labels = scales::percent,
    breaks=seq(0,1,0.1)
  )+
  oxpop_blue_panel+
  theme(legend.position = "none",
        text=element_text(size=20),
        axis.text.x=element_text(angle=30, hjust=1, vjust=1, size=10),
        panel.spacing.x = unit(0.4,"cm"),
        legend.background=element_blank(),
        legend.key=element_blank())+
  labs(subtitle="Baseline exposure",
       # title="Sensitivy and specificity of EPMA data vs CRF data over time"
  )->p1


sn_sp_over_time%>%
  filter(Event=="Follow=up")%>%
  ggplot(aes(Period, Value, color=Statistic, group=Statistic))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=Value-SE,
                    ymax=Value+SE),
                width=0.2)+
  facet_grid(~Drug,
             labeller = label_wrap_gen(width = 15))+
  scale_y_continuous(
    labels = scales::percent,
    breaks=seq(0,1,0.1)
  )+  
  oxpop_blue_panel+
  theme(legend.position = "bottom",
        text=element_text(size=20),
        axis.text.x=element_text(angle=30, hjust=1, vjust=1, size=10),
        panel.spacing.x = unit(0.4,"cm"),
        legend.background=element_blank())+
   labs(subtitle="Follow-up exposure")+
  guides(fill=NA)->p2


p1/p2&plot_annotation(theme=theme(plot.background = element_rect(fill="transparent", color=NA)))



ggsave("Outputs/Figures/Slides/sensitivity_specificity_time_series.png",
       width=60,
       height=35,
       dpi="retina",
       units = "cm")  



## 5. Use case scenarios -----

## 5.1 Trends along time --------

### extract EPMA records --------

epma_drug_flags%>%
  select(Study_ID,
         Drug,
         Flag,
         date)%>%
  filter(Drug%in% c("Corticosteroids",
                    "Tocilizumab or sarilumab",
                    "Hydroxychloroquine",
                    "Baricitinib"))%>%
  left_join(censoring_dates)%>%
  left_join(baseline_crf_england%>%select(Study_ID, resp_status))%>%
  mutate(month=as.Date(paste0(strftime(rand_date, "%Y-%m"), "-01"),
                       format="%Y-%m-%d"))%>%
  group_by(Study_ID, resp_status,Drug, month)%>%
  summarise(Flag=if_else(any(Flag=="1"& date>rand_date & date<right_censoring_date_death_discharge_28d), 1,0))%>% # restricting to records on or after day of randomisation and up until death, discharge, or date follow-up form was completed, whichever occurred earlier
  ungroup()%>%
  mutate(Source="EPMA")%>%
  mutate(Flag=replace_na(Flag, 0))->epma_flags_timeseries

### join aggregate counts (regardless of resp status) -----

epma_flags_timeseries%<>%
  rbind(
    epma_drug_flags%>%
      select(Study_ID,
             Drug,
             Flag,
             date)%>%
      filter(Drug%in% c("Corticosteroids",
                        "Tocilizumab or sarilumab",
                        "Hydroxychloroquine",
                        "Baricitinib"))%>%
      left_join(censoring_dates)%>%
      left_join(baseline_crf_england%>%select(Study_ID, resp_status))%>%
      mutate(month=as.Date(paste0(strftime(rand_date, "%Y-%m"), "-01"),
                           format="%Y-%m-%d"))%>%
      mutate(resp_status="Aggregate")%>%
      group_by(Study_ID, resp_status,Drug, month)%>%
      summarise(Flag=if_else(any(Flag=="1"& date>rand_date & date<right_censoring_date_death_discharge_28d), 1,0))%>% # restricting to records on or after day of randomisation and up until death, discharge, or date follow-up form was completed, whichever occurred earlier
      ungroup()%>%
      mutate(Source="EPMA")%>%
      mutate(Flag=replace_na(Flag, 0)))

### extract CRF data -----

fu_crf_flags_timeseries<-crf_post_rand_drug_flags%>%
  filter(Drug %in% c("Corticosteroids", 
                     "Baricitinib",
                     "Tocilizumab or sarilumab",
                     "Hydroxychloroquine"))%>%
  left_join(baseline_crf_england%>%
              select(Study_ID, resp_status, randdt)%>%
              mutate(Study_ID=as.character(Study_ID)))%>%
  filter(Variable=="flag")%>%
  rename(Flag=Value)%>%
  mutate(Source="CRF",
         month=as.Date(paste0(strftime(randdt, "%Y-%m"), "-01"),
                              format="%Y-%m-%d"))%>%
  select(-Variable, -randdt)

fu_crf_flags_timeseries%<>%
  rbind(
    crf_post_rand_drug_flags%>%
      filter(Drug %in% c("Corticosteroids", 
                         "Baricitinib",
                         "Tocilizumab or sarilumab",
                         "Hydroxychloroquine"))%>%
      left_join(baseline_crf_england%>%
                  select(Study_ID, resp_status, randdt)%>%
                  mutate(Study_ID=as.character(Study_ID)))%>%
      filter(Variable=="flag")%>%
      rename(Flag=Value)%>%
      mutate(Source="CRF",
             month=as.Date(paste0(strftime(randdt, "%Y-%m"), "-01"),
                           format="%Y-%m-%d"))%>%
      select(-Variable, -randdt)%>%
      mutate(resp_status="Aggregate"))




### calculate proportions ---------

timeseries_summarised_for_plotting<-
  epma_flags_timeseries%>%
  rbind(fu_crf_flags_timeseries)%>%
  mutate(Drug=fct_relevel(Drug, "Corticosteroids", "Hydroxychloroquine", "Tocilizumab or sarilumab", "Baricitinib"))%>%
  # mutate(resp_status=case_when(resp_status=="No oxygen or ventilation" ~ "No oxygen or ventilation",
  #                              resp_status %in% c("Oxygen requirement with no ventilation",
  #                                                 "Non-invasive ventilation",
  #                                                 "Oxygen requirement, non-invasive ventilation unknown") ~ "Oxygen requirement or non-invasive ventilation",
  #                              resp_status=="Mechanical ventilation or ECMO" ~ "Mechanical ventilation or ECMO"))%>%
  mutate(resp_status=fct_relevel(resp_status,
                                 "No oxygen or ventilation",
                                 "Oxygen requirement or non-invasive ventilation",
                                 "Mechanical ventilation or ECMO"))%>%
  group_by(month, Drug, Source, resp_status)%>%
  summarise(Participants = n_distinct(Study_ID),
            Participants_with_drug_of_interest = n_distinct(Study_ID[Flag=="1"]),
            Proportion = Participants_with_drug_of_interest/Participants,
            SE = sqrt(Proportion*(1-Proportion)/Participants))

### plot ---------

#### oxpop theme -----

timeseries_summarised_for_plotting%>%
  filter(!is.na(resp_status))%>%
  mutate(resp_status=factor(resp_status, levels=c("Aggregate", "No oxygen or ventilation", "Oxygen requirement or non-invasive ventilation","Mechanical ventilation or ECMO")))%>%
  ggplot(aes(month, Proportion, color=Source, 
             ))+
  geom_rect(data=.%>%filter(Drug=="Corticosteroids"),
            aes(xmin=ymd('2020-03-01'),
                xmax=ymd('2020-06-16'),
                ymin=-Inf,
                ymax=Inf), 
            fill="light green",
            color=NA,
            alpha=0.01)+
  geom_rect(data=.%>%filter(Drug=="Hydroxychloroquine"),
            aes(xmin=ymd('2020-03-01'),
                xmax=ymd('2020-06-05'),
                ymin=-Inf,
                ymax=Inf), 
            fill="light green",
            color=NA,
            alpha=0.01)+
  geom_rect(data=.%>%filter(Drug=="Tocilizumab or sarilumab"),
            aes(xmin=ymd('2020-04-01'),
                xmax=ymd('2021-02-11'),
                ymin=-Inf,
                ymax=Inf), 
            fill="light green",
            color=NA,
            alpha=0.01)+
  geom_rect(data=.%>%filter(Drug=="Baricitinib"),
            aes(xmin=ymd('2021-02-02'),
                xmax=ymd('2022-03-03'),
                ymin=-Inf,
                ymax=Inf), 
            fill="light green",
            color=NA,
            alpha=0.01)+
  geom_point()+
  geom_line()+
  geom_vline(data=
               expand_grid(tibble(Drug=c("Corticosteroids", 
                                         "Hydroxychloroquine", 
                                         "Tocilizumab or sarilumab", 
                                         "Baricitinib"),
                                  Date = as.Date(c("2020-06-16",
                                                   "2020-06-05",
                                                   "2021-02-11",
                                                   "2022-03-03"))),
                           resp_status=c("No oxygen or ventilation",
                                         "Oxygen requirement or non-invasive ventilation",
                                         "Mechanical ventilation or ECMO")),
             aes(xintercept=Date),
             linetype="dashed",
             color="white")+
  geom_errorbar(aes(ymin=Proportion-SE,
                    ymax=Proportion+SE))+
  # geom_text(aes(label=paste0(
  #   # Participants_with_drug_of_interest 
  #                            # , "\n(", 
  #   round(Proportion*100,0) 
  #   #,"%)"
  #                            )),
  #           vjust=-0.5,
  #           size=5/2.54,
  #           color="black")+
  geom_text(data=
              expand_grid(tibble(Drug=c("Corticosteroids", 
                                        "Hydroxychloroquine", 
                                        "Tocilizumab or sarilumab", 
                                        "Baricitinib"),
                                 Date = as.Date(c("2020-06-16",
                                                  "2020-06-05",
                                                  "2021-02-11",
                                                  "2022-03-03"))),
                          resp_status=c(
                            # "No oxygen or ventilation"
                            # ,           "Oxygen requirement or non-invasive ventilation",
                            "Mechanical ventilation or ECMO"
                            )
                          ),
            aes(x=Date,label=Date),
            y=1,
            size=7/2.54,
            color="white")+
 
  facet_grid(as.factor(resp_status)~as.factor(Drug), 
             # ncol = 4,
             scales="free_y",
             switch="y",
             labeller = label_wrap_gen(width = 20)
             # strip.position = "left"
             
  )+
  scale_x_date(date_breaks = "2 months",
               date_labels = "%Y %b",
               limits=as.Date(c(NA, "2022-04-30"))
               )+
  scale_y_continuous(labels=label_number(scale=1e2),
                     expand=expansion(c(0,0.1)))+
  oxpop_blue_panel+
  theme(text = element_text(size=10,
                            family="Mulish"),
        axis.text.x = element_text(angle=90,
                                   vjust=0.5),
        axis.text.y=element_text(size=10),
        # axis.title=element_text(size=15),
        legend.position = "bottom",
        plot.caption = element_text(hjust=0,
                                    size=12),
        strip.placement = "outside",
        legend.background=element_blank(),
        # panel.grid.minor = element_blank(),
        legend.key = element_rect(fill = NA)
  )+
  labs(
    # title="Use of RECOVERY trial medications during the admission of randomisation",
    # subtitle="Among RECOVERY trial participants",
    x="Month of randomisation",
    y="Proportion of monthly randomisations with drug record (%)",
    #    caption="
    # Text labels show the number of admissions with each drug of interest and the proportion within that month. Error bars depict standard errors. Vertical dashed lines show the date in which each result was published in the RECOVERY website.
    # Sources: Hospital Episode Statistics (HES), Electronic Prescribing and Administration (EPMA).
    # Analysis restricted to RECOVERY trial participants recruited in England for whom EPMA data was available (~12%) and right-censored at the end of 2021 (due to current HES data availability).
    # HES methodology:
    # - Each HES record was considered as a separate admission (transfers were not aggregated into superspells and therefore each spell was counted indepedently).
    # - Unfinished HES records (missing discharge date) were excluded; after that, records with the same admission and discharge dates were considered duplicates and only counted once. 
    # - Admissions for COVID-19 were defined as HES records in which U071 ('COVID-19: Virus identified') or U072 ('COVID-19: Virus not identified') were coded in the primary diagnostic position.
    # - Finally, a single admission and discharge date for each period meeting the above condition was extracted as a distinct admission for COVID-19.
    # EPMA methodology:
    # - Medications prescribed were identified using the name recorded in the electronic prescribing system (i.e. no drug terminology mapping was employed).
    # - Only one prescription per admission was required.
    # - Prescription records were extracted regardless of whether the medication was administered"
  )->results_plot_randomisation

ggsave(plot=results_plot_randomisation,
       filename="timeseries_epma_administration_vs_crf_oxpop.png",
       path="Outputs/Figures/Slides",
       dpi="retina",
       width=30,
       height=15,
       units="cm")

#### regular plot -----

##### by resp status --------

timeseries_summarised_for_plotting%>%
  filter(!is.na(resp_status))%>%
  mutate(resp_status=factor(resp_status, levels=c("Aggregate", "No oxygen or ventilation", "Oxygen requirement or non-invasive ventilation","Mechanical ventilation or ECMO")))%>%
  ggplot(aes(month, Proportion, color=Source, 
  ))+
  geom_rect(data=.%>%filter(Drug=="Corticosteroids"),
            aes(xmin=ymd('2020-03-01'),
                xmax=ymd('2020-06-16'),
                ymin=-Inf,
                ymax=Inf), 
            fill="light green",
            color=NA,
            alpha=0.01)+
  geom_rect(data=.%>%filter(Drug=="Hydroxychloroquine"),
            aes(xmin=ymd('2020-03-01'),
                xmax=ymd('2020-06-05'),
                ymin=-Inf,
                ymax=Inf), 
            fill="light green",
            color=NA,
            alpha=0.01)+
  geom_rect(data=.%>%filter(Drug=="Tocilizumab or sarilumab"),
            aes(xmin=ymd('2020-04-01'),
                xmax=ymd('2021-02-11'),
                ymin=-Inf,
                ymax=Inf), 
            fill="light green",
            color=NA,
            alpha=0.01)+
  geom_rect(data=.%>%filter(Drug=="Baricitinib"),
            aes(xmin=ymd('2021-02-02'),
                xmax=ymd('2022-03-03'),
                ymin=-Inf,
                ymax=Inf), 
            fill="light green",
            color=NA,
            alpha=0.01)+
  geom_point()+
  geom_line()+
  geom_vline(data=
               expand_grid(tibble(Drug=c("Corticosteroids", 
                                         "Hydroxychloroquine", 
                                         "Tocilizumab or sarilumab", 
                                         "Baricitinib"),
                                  Date = as.Date(c("2020-06-16",
                                                   "2020-06-05",
                                                   "2021-02-11",
                                                   "2022-03-03"))),
                           resp_status=c("No oxygen or ventilation",
                                         "Oxygen requirement or non-invasive ventilation",
                                         "Mechanical ventilation or ECMO",
                                         "Aggregate")),
             aes(xintercept=Date),
             linetype="dashed",
             color="black")+
  geom_errorbar(aes(ymin=if_else(Proportion-1.96*SE<0,0, Proportion-1.96*SE,0) ,
                    ymax=if_else(Proportion+1.96*SE>1, 1, Proportion+1.96*SE)))+
  # geom_text(aes(label=paste0(
  #   # Participants_with_drug_of_interest 
  #                            # , "\n(", 
  #   round(Proportion*100,0) 
  #   #,"%)"
  #                            )),
  #           vjust=-0.5,
  #           size=5/2.54,
  #           color="black")+
  geom_text(data=
              expand_grid(tibble(Drug=c("Corticosteroids", 
                                        "Hydroxychloroquine", 
                                        "Tocilizumab or sarilumab", 
                                        "Baricitinib"),
                                 Date = as.Date(c("2020-06-16",
                                                  "2020-06-05",
                                                  "2021-02-11",
                                                  "2022-03-03"))),
                          resp_status=c(
                            # "No oxygen or ventilation"
                            # ,           "Oxygen requirement or non-invasive ventilation",
                            "Mechanical ventilation or ECMO"
                          )
              ),
            aes(x=Date,label=Date),
            y=1,
            size=7/2.54,
            color="black")+
  
  facet_grid(as.factor(resp_status)~as.factor(Drug), 
             # ncol = 4,
             scales="free_y",
             switch="y",
             labeller = label_wrap_gen(width = 15)
             # strip.position = "left"
             
  )+
  scale_x_date(date_breaks = "2 months",
               date_labels = "%Y %b",
               limits=as.Date(c(NA, "2022-04-30"))
  )+
  scale_y_continuous(labels=label_number(scale=1e2),
                     expand=expansion(c(0,0.1)))+
  # oxpop_blue_panel+
  theme(text = element_text(size=10,
                            family="Mulish"),
        axis.text.x = element_text(angle=90,
                                   vjust=0.5),
        axis.text.y=element_text(size=10),
        # axis.title=element_text(size=15),
        legend.position = "bottom",
        plot.caption = element_text(hjust=0,
                                    size=12),
        strip.placement = "outside",
        legend.background=element_blank(),
        # panel.grid.minor = element_blank(),
        legend.key = element_rect(fill = NA)
  )+
  labs(
    # title="Use of RECOVERY trial medications during the admission of randomisation",
    # subtitle="Among RECOVERY trial participants",
    x="Month of randomisation",
    y="Proportion of monthly randomisations with drug record (%)",
    #    caption="
    # Text labels show the number of admissions with each drug of interest and the proportion within that month. Error bars depict standard errors. Vertical dashed lines show the date in which each result was published in the RECOVERY website.
    # Sources: Hospital Episode Statistics (HES), Electronic Prescribing and Administration (EPMA).
    # Analysis restricted to RECOVERY trial participants recruited in England for whom EPMA data was available (~12%) and right-censored at the end of 2021 (due to current HES data availability).
    # HES methodology:
    # - Each HES record was considered as a separate admission (transfers were not aggregated into superspells and therefore each spell was counted indepedently).
    # - Unfinished HES records (missing discharge date) were excluded; after that, records with the same admission and discharge dates were considered duplicates and only counted once. 
    # - Admissions for COVID-19 were defined as HES records in which U071 ('COVID-19: Virus identified') or U072 ('COVID-19: Virus not identified') were coded in the primary diagnostic position.
    # - Finally, a single admission and discharge date for each period meeting the above condition was extracted as a distinct admission for COVID-19.
    # EPMA methodology:
    # - Medications prescribed were identified using the name recorded in the electronic prescribing system (i.e. no drug terminology mapping was employed).
    # - Only one prescription per admission was required.
    # - Prescription records were extracted regardless of whether the medication was administered"
  )->results_plot_randomisation

results_plot_randomisation


ggsave(plot=results_plot_randomisation,
       filename="timeseries_epma_administration_vs_crf_by_resp_status.png",
       path="Outputs/Figures/Analysis",
       dpi="retina",
       width=30,
       height=15,
       units="cm")

##### aggregate -------


timeseries_summarised_for_plotting%>%
  filter(resp_status=="Aggregate")%>%
  ggplot(aes(month, Proportion, color=Source, 
  ))+
  geom_rect(data=.%>%filter(Drug=="Corticosteroids"),
            aes(xmin=ymd('2020-03-01'),
                xmax=ymd('2020-06-16'),
                ymin=-Inf,
                ymax=Inf), 
            fill="light green",
            color=NA,
            alpha=0.01)+
  geom_rect(data=.%>%filter(Drug=="Hydroxychloroquine"),
            aes(xmin=ymd('2020-03-01'),
                xmax=ymd('2020-06-05'),
                ymin=-Inf,
                ymax=Inf), 
            fill="light green",
            color=NA,
            alpha=0.01)+
  geom_rect(data=.%>%filter(Drug=="Tocilizumab or sarilumab"),
            aes(xmin=ymd('2020-04-01'),
                xmax=ymd('2021-02-11'),
                ymin=-Inf,
                ymax=Inf), 
            fill="light green",
            color=NA,
            alpha=0.01)+
  geom_rect(data=.%>%filter(Drug=="Baricitinib"),
            aes(xmin=ymd('2021-02-02'),
                xmax=ymd('2022-03-03'),
                ymin=-Inf,
                ymax=Inf), 
            fill="light green",
            color=NA,
            alpha=0.01)+
  geom_point()+
  geom_line()+
  geom_vline(data=
               expand_grid(tibble(Drug=c("Corticosteroids", 
                                         "Hydroxychloroquine", 
                                         "Tocilizumab or sarilumab", 
                                         "Baricitinib"),
                                  Date = as.Date(c("2020-06-16",
                                                   "2020-06-05",
                                                   "2021-02-11",
                                                   "2022-03-03")))),
             aes(xintercept=Date),
             linetype="dashed",
             color="black")+
  geom_errorbar(aes(ymin=Proportion-1.96*SE,
                    ymax=Proportion+1.96*SE))+
  # geom_text(aes(label=paste0(
  #   # Participants_with_drug_of_interest 
  #                            # , "\n(", 
  #   round(Proportion*100,0) 
  #   #,"%)"
  #                            )),
  #           vjust=-0.5,
  #           size=5/2.54,
  #           color="black")+
  geom_text(data=
              expand_grid(tibble(Drug=c("Corticosteroids", 
                                        "Hydroxychloroquine", 
                                        "Tocilizumab or sarilumab", 
                                        "Baricitinib"),
                                 Date = as.Date(c("2020-06-16",
                                                  "2020-06-05",
                                                  "2021-02-11",
                                                  "2022-03-03"))),
                          resp_status=c(
                            # "No oxygen or ventilation"
                            # ,           "Oxygen requirement or non-invasive ventilation",
                            "Mechanical ventilation or ECMO"
                          )
              ),
            aes(x=Date,label=Date),
            y=1,
            size=7/2.54,
            color="black")+
  
  facet_grid(~as.factor(Drug), 
             # ncol = 4,
             scales="free_y",
             switch="y",
             labeller = label_wrap_gen(width = 30)
             # strip.position = "left"
             
  )+
  scale_x_date(date_breaks = "2 months",
               date_labels = "%Y %b",
               limits=as.Date(c(NA, "2022-04-30"))
  )+
  scale_y_continuous(labels=label_number(scale=1e2),
                     expand=expansion(c(0,0.1)))+
  # oxpop_blue_panel+
  theme(text = element_text(size=10,
                            family="Mulish"),
        axis.text.x = element_text(angle=90,
                                   vjust=0.5),
        axis.text.y=element_text(size=10),
        # axis.title=element_text(size=15),
        legend.position = "bottom",
        plot.caption = element_text(hjust=0,
                                    size=12),
        strip.placement = "outside",
        legend.background=element_blank(),
        # panel.grid.minor = element_blank(),
        legend.key = element_rect(fill = NA)
  )+
  labs(
    # title="Use of RECOVERY trial medications during the admission of randomisation",
    # subtitle="Among RECOVERY trial participants",
    x="Month of randomisation",
    y="Proportion of monthly randomisations with drug record (%)",
    #    caption="
    # Text labels show the number of admissions with each drug of interest and the proportion within that month. Error bars depict standard errors. Vertical dashed lines show the date in which each result was published in the RECOVERY website.
    # Sources: Hospital Episode Statistics (HES), Electronic Prescribing and Administration (EPMA).
    # Analysis restricted to RECOVERY trial participants recruited in England for whom EPMA data was available (~12%) and right-censored at the end of 2021 (due to current HES data availability).
    # HES methodology:
    # - Each HES record was considered as a separate admission (transfers were not aggregated into superspells and therefore each spell was counted indepedently).
    # - Unfinished HES records (missing discharge date) were excluded; after that, records with the same admission and discharge dates were considered duplicates and only counted once. 
    # - Admissions for COVID-19 were defined as HES records in which U071 ('COVID-19: Virus identified') or U072 ('COVID-19: Virus not identified') were coded in the primary diagnostic position.
    # - Finally, a single admission and discharge date for each period meeting the above condition was extracted as a distinct admission for COVID-19.
    # EPMA methodology:
    # - Medications prescribed were identified using the name recorded in the electronic prescribing system (i.e. no drug terminology mapping was employed).
    # - Only one prescription per admission was required.
    # - Prescription records were extracted regardless of whether the medication was administered"
  )->results_plot_randomisation

results_plot_randomisation

ggsave(plot=results_plot_randomisation,
       filename="timeseries_epma_administration_vs_crf_aggregate.png",
       path="Outputs/Figures/Analysis",
       dpi="retina",
       width=30,
       height=15,
       units="cm")
































# investigate baricitinib randomisations timeseries

# baseline_crf%>%
#   select(usubjid, randdt, trt05p)%>%
#   mutate(month=paste0(str_sub(randdt, 1, 8), "01"))%>%
#   filter(trt05p %in% c("DA-USUAL", "DB-BARI"))%>%
#   group_by(month, trt05p)%>%
#   summarise(participants=n_distinct(usubjid))%>%
#   ggplot(aes(month, participants, color=trt05p, group=trt05p))+
#   geom_point()+
#   geom_line()


## 5.2 agreement by Trust ------

### initial transformations ----

rm(Drug)

drug_counts_by_site<-
  baseline_combined_flags%>%
  select(-resp_status,
         -Drug)%>%
  left_join(baseline_crf_england%>%
              select(Study_ID,
                     SiteName))%>%
  left_join(sites%>%select(SiteName))

### calculate agreement ----


kappa_sites<-list()

sites<-drug_counts_by_site%>%
  distinct(SiteName)%>%
  .[[1]]

for (i in sites) {
  
  drug_counts_by_site%>%
      filter(SiteName==i)%>%
      select(EPMA, CRF)%>%
      mutate(across(everything(), as.integer))%>%
      mutate(across(c(EPMA, CRF), ~replace_na(.x, 0)))->table
    
    matrix<-as.matrix(table(table))
    
    try(k<-epi.kappa(matrix, method="cohen", alternative = "greater", conf.level=0.95))
    
    if(exists("k"))
    {
      
      kappa_sites[[i]]<-k
      
    }
    else{
      kappa_sites[[i]]<-list()
    }
    
    rm(i, table, k, matrix)
    
  }

##### Unlist results for table

kappa_sites_results<-data.frame()


for (i in sites) {
  Site<-i
  
    try(
      table<-(
        data.frame(
          Site = Site,
          kappa = unlist(kappa_sites[[i]]$kappa$est),
          se = unlist(kappa_sites[[i]]$kappa$se),
          kappa_CI_lower = unlist(kappa_sites[[i]]$kappa$lower),
          kappa_CI_upper = unlist(kappa_sites[[i]]$kappa$upper),
          
          pabak = unlist(kappa_sites[[i]]$pabak$est),
          pabak_CI_lower = unlist(kappa_sites[[i]]$pabak$lower),
          pabak_CI_upper = unlist(kappa_sites[[i]]$pabak$upper),
          
          p_value = unlist(kappa_sites[[i]]$z$p.value)
        ))
    )
    
    try(kappa_sites_results%<>%bind_rows(table))
    rm(i, Site, table)
  }

### counts ------
drug_counts_by_site%>%
  rename(Site=SiteName)%>%
  group_by(Site)%>%
  summarise(pairs = n_distinct(Study_ID),
            positives = length(CRF[CRF=="1"]),
            negatives = length(CRF[CRF=="0"]),
            true_positives = length(CRF[CRF=="1" & EPMA=="1"]),
            true_negatives = length(CRF[CRF=="0" & EPMA=="0"]),
            false_positives = length(CRF[CRF=="0" & EPMA =="1"]),
            false_negatives = length(CRF[CRF =="1" & EPMA == "0"]),
            Sensitivity = round(true_positives/positives*100, 1),
            Specificity = round(true_negatives/negatives*100, 1),
            PPV = round(true_positives/(true_positives+false_positives)*100, 1),
            NPV = round(true_negatives/(true_negatives+false_negatives)*100, 1),
            Both = sum(EPMA=="1" & CRF=="1"),
            CRF_only = sum(EPMA=="0" & CRF=="1"),
            EPMA_only = sum(EPMA=="1" & CRF=="0"),
            Neither = sum(EPMA=="0" & CRF=="0"),
            `Unconfirmed records` = false_negatives/positives,
            `Missing records` = false_positives/length(EPMA[EPMA=="1"]))%>%
  select(-c(positives, negatives, true_positives, true_negatives, false_positives, false_negatives))%>%
  rename(Individuals=pairs)->drug_counts_by_site_counts

drug_counts_by_site%>%
  select(EPMA, CRF)%>%
  table()


### plot ----
drug_counts_by_site_counts%>%
  select(Site, Individuals, `Unconfirmed records`, `Missing records`)%>%
  filter(Individuals>100)%>%
  # mutate(across(c(Unconfirmed, Missing), ~./100))%>%
  pivot_longer(c(`Unconfirmed records`, `Missing records`), names_to="Variable", values_to="Value")%>%
  mutate(lower = Value - 1.96*sqrt(Value*(Value)/Individuals),
         upper = Value + 1.96*sqrt(Value*(1-Value)/Individuals))%>%
  select(-Individuals)%>%
  rbind(kappa_sites_results%>%
          left_join(drug_counts_by_site_counts%>%
                      select(Site, Individuals))%>%
        filter(Individuals>100)%>%
          select(
                Site,
                Value=kappa,
                     lower=kappa_CI_lower,
                     upper=kappa_CI_upper)%>%
              mutate(Variable="Kappa"))%>%
    # rbind(kappa_sites_results%>%
    #         left_join(drug_counts_by_site_counts%>%
    #                     select(Site, Individuals))%>%
    #         filter(Individuals>100)%>%
    #         select(
    #           Site,
    #           Value=pabak,
    #           lower=pabak_CI_lower,
    #           upper=pabak_CI_upper)%>%
    #         mutate(Variable="PABAK"))%>%
  arrange(Site)%>%
  mutate(Site=rep(c(1:14), each=3))%>%
  mutate(across(c(Value, upper, lower), ~round(.,2)))%>%
  mutate(Variable=as.factor(Variable),
         Site_facet=as.character(Site),
         Site=reorder_within(Site, Value, Variable))%>%
  mutate(Value=if_else(Variable!="Kappa", Value*100, Value),
         lower=if_else(Variable!="Kappa", lower*100, lower),
         upper=if_else(Variable!="Kappa", upper*100, upper),
         )%>%
  group_by(Variable)%>%
  mutate(Average=mean(Value))->agreement_by_trust_for_plotting
  
agreement_by_trust_for_plotting%>%
  filter(Variable=="Kappa")%>%
  ggplot(aes(as.factor(Site),
             Value, 
             fill=Site_facet
             ))+
  geom_col(width=0.8)+
  geom_hline(aes(yintercept=Average),
             linetype="dashed")+
  facet_wrap(~Variable,
             nrow=1,
             scales="free")+
  geom_errorbar(aes(ymin=lower,
                    ymax=upper,
                    width=0.3))+

  geom_text(.%>%filter(!Variable%in% c("Unconfirmed records", "Missing records")),
          mapping=aes(Site, y=1, label=paste0(Value,
                                      "\n(",
                                      lower,
                                      " - ",
                                      upper,
                                      ")")))+
  geom_text(data=.%>%
              group_by(Variable)%>%
              slice_max(Site),
            aes(Site, 0.6, label=paste0("Average: ", round(Average, 1))))+

  scale_x_reordered() +

  scale_y_continuous(expand = expansion(c(0,0.2)),
                     limits=c(0,1),
                     breaks=seq(0,1,0.2)
                     )+
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        text=element_text(size=20, family="Mulish"))+
  scale_fill_manual(values = colorRampPalette(c("#a83232", "#2ac938"))(14),
                    limits=c(2,4,9,6,11,1,10,12,14,7,5,8,13,3))->p1

p1
    

agreement_by_trust_for_plotting%>%
  filter(Variable!="Kappa")%>%
  ggplot(aes(Site,
             Value, 
             fill=Site_facet
  ))+
  geom_col(width=0.8)+
  geom_hline(aes(yintercept=Average),
             linetype="dashed")+
  facet_wrap(~Variable,
             nrow=3,
             scales="free")+
  geom_errorbar(aes(ymin=lower,
                    ymax=upper,
                    width=0.3))+
  geom_text(
    .%>%filter(Variable %in% c("Unconfirmed records", "Missing records")),
    mapping=aes(Site, y=60, label=paste0(Value,
                                                "%\n(",
                                                lower,
                                                "% - ",
                                                upper,
                                                "%)")))+
  geom_text(data=.%>%
              group_by(Variable)%>%
              slice_min(Site),
            aes(Site, Average*1.2, label=paste0("Average: ", round(Average, 1))))+
  
  scale_x_reordered(limits=rev) +
  
  scale_y_continuous(expand = expansion(c(0,0.2)),
                     limits=c(0,60),
                     breaks=seq(0,60,20)
  )+
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        text=element_text(size=20, family="Mulish"))+
  scale_fill_manual(values = colorRampPalette(c("#a83232", "#2ac938"))(14),
                    limits=c(2,4,9,6,11,1,10,12,14,7,5,8,13,3))->p2
p2

agreement_plot<-
# grid.arrange(patchworkGrob(
  p1/p2+plot_layout(heights=c(1,2))
  # ),
#              left="Value")

agreement_plot

ggsave("Outputs/Figures/Analysis/agreement_by_trust.png",
       agreement_plot,
       dpi="retina",
       width=60,
       height=30,
       units="cm")

















## 5.3 clinical events --------


### 5.3.1 infections -----

# extract events

## extract medication records (antibacterials, antivirals, antifungals)

data_administration%>%
  group_by(MEDICATION_NAME)%>%
  summarise(n=n())%>%
  arrange(desc(n))%>%
  write_csv("Intermediate outputs/Medication names.csv")




infections_crf%>%
  filter(Study_ID %in% participants_with_data_in_randomisation_admission)%>%
  distinct(Study_ID)%>%
  nrow()
  # 1377 people with infection records and EPMA data for comparison


## extract antibacterial flags

antimicrobials<-
  medication_codelists%>%
  filter(Antibacterial=="Y" | Antiviral =="Y" | Antifungal == "Y")%>%
  distinct(MEDICATION_NAME)%>%
  .[[1]]


## extract drug flags from EPMA and join with CRF flags
infection_flags_combined<-
  infections_crf%>%
  filter(Study_ID %in% participants_with_data_in_randomisation_admission)%>% # restrict CRF flags to people with EPMA data for comparison
  group_by(Study_ID)%>%
  summarise(CRF_infection_flag=if_else(any(avalc=="Y"), "Y", "N"))%>%
  left_join(
    data_prescription%>%
      select(Study_ID,
             MEDICATION_NAME,
             date=INITIAL_AUTHORISED_DATE_TIME)%>%
      rbind(
        data_administration%>%
          select(Study_ID,
                 MEDICATION_NAME,
                 date = ADMINISTERED_DATE_TIME))%>%
      filter(Study_ID %in% participants_with_data_in_randomisation_admission)%>%
      left_join(censoring_dates%>%
                  select(Study_ID,
                         rand_date,
                         admission_date,
                         right_censoring_date_death_discharge_28d))%>%
      group_by(Study_ID)%>%
      summarise(EPMA_antimicrobial_flag_post_rand = if_else(any(MEDICATION_NAME %in% antimicrobials & date >rand_date & date<= right_censoring_date_death_discharge_28d), "Y", "N"),
                EPMA_antimicrobial_flag_pre_rand = if_else(any(MEDICATION_NAME %in% antimicrobials & date <=rand_date & date>= admission_date), "Y", "N"))%>%
      mutate(EPMA_antimicrobial_flag_pre_rand=replace_na(EPMA_antimicrobial_flag_pre_rand, "N"),
             EPMA_antimicrobial_flag_post_rand=replace_na(EPMA_antimicrobial_flag_post_rand, "N"))%>%
    mutate(EPMA_antimicrobial_flag = if_else(EPMA_antimicrobial_flag_post_rand=="Y"  & EPMA_antimicrobial_flag_pre_rand == "N", "Y", "N")))
    
View(infection_flags_combined)  


# check most common antimicrobials reported

infections_crf%>%
  filter(Study_ID %in% participants_with_data_in_randomisation_admission)%>% # restrict CRF flags to people with EPMA data for comparison
  group_by(Study_ID)%>%
  summarise(CRF_infection_flag=if_else(any(avalc=="Y"), "Y", "N"))%>%
  left_join(
    data_prescription%>%
      select(Study_ID,
             MEDICATION_NAME,
             date=INITIAL_AUTHORISED_DATE_TIME)%>%
      rbind(
        data_administration%>%
          select(Study_ID,
                 MEDICATION_NAME,
                 date = ADMINISTERED_DATE_TIME))%>%
      filter(Study_ID %in% participants_with_data_in_randomisation_admission)%>%
      left_join(censoring_dates%>%
                  select(Study_ID,
                         rand_date,
                         right_censoring_date_death_discharge_28d))%>%
      mutate(EPMA_antimicrobial_flag = if_else(MEDICATION_NAME %in% antimicrobials & date >=rand_date & date<= right_censoring_date_death_discharge_28d, "Y", "N")))%>%

  group_by(CRF_infection_flag,
           EPMA_antimicrobial_flag,
           MEDICATION_NAME)%>%
  summarise(n=n())%>%
  filter(EPMA_antimicrobial_flag=="Y",
         CRF_infection_flag=="N")%>%
  View()
# records seem adequate





## remove people with pre-randomisation non-covid infections

View(clinical_events_sdtm)

pre_rand_infections_participant_list<-clinical_events_sdtm%>%
  mutate(Study_ID=as.character(usubjid))%>%
  filter(cecat=="NON-CORONAVIRUS INFECTION")%>%
  left_join(rand_dates)%>%
  filter(cestdtc<=rand_date)%>%
  distinct(Study_ID)%>%
  .[[1]]


## compute table
infection_flags_combined%>%
  filter(!Study_ID %in%pre_rand_infections_participant_list)%>%
  select(CRF_infection_flag, EPMA_antimicrobial_flag)%>%
  table()



## plot


infection_flags_combined%>%
  filter(!Study_ID %in%pre_rand_infections_participant_list)%>%
  select(CRF=CRF_infection_flag, EPMA=EPMA_antimicrobial_flag)%>%
  group_by(CRF, EPMA)%>%
  summarise(n=n())%>%
  ungroup()%>%
  mutate(prop=round(n/sum(n)*100,1))%>%
  mutate(EPMA=fct_rev(EPMA))%>%
  
  
  ggplot(aes(x=CRF, y=EPMA, fill=prop))+
  geom_tile()+
  scale_x_discrete(position="top")+
  scale_fill_gradient(breaks=seq(0,100,20),
                      limits=c(0, 100),
                      low="white",
                      high="red")+
  geom_text(aes(label=paste0(n, " (", prop, "%)")),
            size=30/2.53)+
  labs(x="CRF",
       y="EPMA",
       fill="Participants (%)")+
  theme(text=element_text(size=30,
                          family="Mulish"),
        legend.position = "bottom",
        legend.key.size = unit(2, "cm"),
        strip.placement = "outside")

ggsave("Outputs/Figures/Analysis/infection_events_heatmap.png",
       dpi="retina",
       width=60,
       height=30,
       units="cm")








### 5.3.2 vascular events (abandoned) ----

View(thromb_crf)


aspirin<-
  medication_codelists%>%
  filter(Aspirin=="Y")%>%
  distinct(MEDICATION_NAME)%>%
  .[[1]]

p2y12<-
  medication_codelists%>%
  filter(P2Y12=="Y")%>%
  distinct(MEDICATION_NAME)%>%
  .[[1]]

fondaparinux<-
  medication_codelists%>%
  filter(Fondaparinux=="Y")%>%
  distinct(MEDICATION_NAME)%>%
  .[[1]]


mi_flags_combined<-
  thromb_crf%>%
  filter(param=="Myocardial infarction",
         avalc=="Y")%>%
  filter(Study_ID %in% participants_with_data_in_randomisation_admission)%>% # restrict CRF flags to people with EPMA data for comparison
  group_by(Study_ID)%>%
  summarise(CRF_mi_flag=if_else(any(avalc=="Y"), "Y", "N"))%>%
  left_join(
    
    
    data_administration%>%
      select(Study_ID,
             MEDICATION_NAME,
             administered_date = ADMINISTERED_DATE_TIME,
             scheduled_date = SCHEDULED_DATE_TIME,
             recorded_date = REPORTED_DATE_TIME,
             last_updated_date = LAST_UPDATED_DATE)%>%
      mutate(date = case_when(is.na(administered_date) ~ scheduled_date,
                              is.na(scheduled_date) ~ recorded_date,
                              is.na(recorded_date) ~ last_updated_date,
                              !is.na(administered_date) ~ administered_date))%>%
      filter(Study_ID %in% participants_with_data_in_randomisation_admission)%>%
      left_join(censoring_dates%>%
                  select(Study_ID,
                         rand_date,
                         right_censoring_date_death_discharge_28d))%>%
      group_by(Study_ID)%>%
      summarise(EPMA_antithrombotic_flag = if_else(
        any(MEDICATION_NAME %in% aspirin & date >=rand_date & date<= right_censoring_date_death_discharge_28d) &
        any(MEDICATION_NAME %in% p2y12 & date >=rand_date & date<= right_censoring_date_death_discharge_28d) &
          any(MEDICATION_NAME %in% fondaparinux & date >=rand_date & date<= right_censoring_date_death_discharge_28d), "Y", "N")))
  # too few events to be considered

















# Supplementary analyses ------

## CRF data audit ------

CRF_data_audit_tocilizumab_comparison <- read_excel("Tools/CRF data audit/CRF data audit tocilizumab comparison.xlsx")

CRF_data_audit_tocilizumab_comparison%>%
  filter(`Participant ID` %in% epma_participants_list_raw) # only 1 included in the EPMA data

CRF_data_audit_tocilizumab_comparison%>%
  filter(`Participant ID` %in% study_population) # but not during the admission of randomisation

# calculations by drug
CRF_data_audit_tocilizumab_comparison%>%
  mutate(across(!c('Participant ID', Site), ~replace_na(., "N")))%>%
  pivot_longer(-c(`Participant ID`, Site), names_to="Drug", values_to="Source")%>%
  left_join(drugs_crf_sdtm%>%
              select(Study_ID, cmtrt, visitnum, cmoccur)%>%
              filter(cmtrt%in%c("Corticosteroid",
                                "Azithromycin or other macrolides",
                                "Colchicine",
                                "Lopinavir-ritonavir",
                                "Tocilizumab or sarilumab",
                                "Hydroxychloroquine",
                                "Remdesivir",
                                "Synthetic monocloncal antibodies (REGN10933+REGN10987)",
                                "Colchicine"),
                     visitnum=="3")%>%
              mutate(`Participant ID`=as.integer(Study_ID),
                     CRF=cmoccur)%>%
              select(`Participant ID`,
                     Drug=cmtrt,
                     CRF))%>%
  filter(!is.na(CRF))%>%
  group_by(Drug)%>%
  summarise(
    pairs = n_distinct(`Participant ID`),
    positives = length(Source[Source=="Y"]),
    negatives = length(Source[Source=="N"]),
    true_positives = length(CRF[CRF=="Y" & Source=="Y"]),
    true_negatives = length(CRF[CRF=="N" & Source=="N"]),
    false_positives = length(CRF[CRF=="Y" & Source =="N"]),
    false_negatives = length(CRF[CRF =="N" & Source == "Y"]),
    Sensitivity = round(true_positives/positives*100, 1),
    Specificity = round(true_negatives/negatives*100, 1),
    PPV = round(true_positives/(true_positives+false_positives)*100, 1),
    NPV = round(true_negatives/(true_negatives+false_negatives)*100, 1),
    Both = sum(Source=="Y" & CRF=="Y"),
    CRF_only = sum(Source=="N" & CRF=="Y"),
    Source_only = sum(Source=="Y" & CRF=="N"),
    Neither = sum(Source=="N" & CRF=="N"))%>%
    View()


# aggregate summary
CRF_data_audit_tocilizumab_comparison%>%
  mutate(across(!c('Participant ID', Site), ~replace_na(., "N")))%>%
  pivot_longer(-c(`Participant ID`, Site), names_to="Drug", values_to="Source")%>%
  left_join(drugs_crf_sdtm%>%
              select(Study_ID, cmtrt, visitnum, cmoccur)%>%
              filter(cmtrt%in%c("Corticosteroid",
                                "Azithromycin or other macrolides",
                                "Colchicine",
                                "Lopinavir-ritonavir",
                                "Tocilizumab or sarilumab",
                                "Hydroxychloroquine",
                                "Remdesivir",
                                "Synthetic monocloncal antibodies (REGN10933+REGN10987)",
                                "Colchicine"),
                     visitnum=="3")%>%
              mutate(`Participant ID`=as.integer(Study_ID),
                     CRF=cmoccur)%>%
              select(`Participant ID`,
                     Drug=cmtrt,
                     CRF))%>%
  filter(!is.na(CRF))%>%
  summarise(
    pairs = n_distinct(`Participant ID`),
    positives = length(Source[Source=="Y"]),
    negatives = length(Source[Source=="N"]),
    true_positives = length(CRF[CRF=="Y" & Source=="Y"]),
    true_negatives = length(CRF[CRF=="N" & Source=="N"]),
    false_positives = length(CRF[CRF=="Y" & Source =="N"]),
    false_negatives = length(CRF[CRF =="N" & Source == "Y"]),
    Sensitivity = round(true_positives/positives*100, 1),
    Specificity = round(true_negatives/negatives*100, 1),
    PPV = round(true_positives/(true_positives+false_positives)*100, 1),
    NPV = round(true_negatives/(true_negatives+false_negatives)*100, 1),
    Both = sum(Source=="Y" & CRF=="Y"),
    CRF_only = sum(Source=="N" & CRF=="Y"),
    Source_only = sum(Source=="Y" & CRF=="N"),
    Neither = sum(Source=="N" & CRF=="N"))%>%
  View()






