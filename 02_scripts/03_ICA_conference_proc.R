#0. Preparation ----

# Load packages
pacman::p_load(haven, #Read sav database
               tidyverse, #Data manipulation
               janitor, #Clean columns
               sjlabelled, #Labels manipulation
               ltm, #Cronbach alpha
               summarytools)


# Read 
raw_2015 <- read_sav("01_input/01_raw_data/2015_survey_raw.sav") %>% clean_names()
raw_2023 <- read_sav("01_input/01_raw_data/2023_survey_raw.sav") %>% clean_names()

# Filter zones measured in both studies
raw_2023 <- raw_2023 %>% filter(tipo_zona==2)
comunas_2023<-get_labels(raw_2023$zona_rural) %>% toupper()
raw_2015 <- raw_2015 %>% filter(to_label(comuna)%in%comunas_2023)

# 99 to NA
raw_2015[raw_2015==99]<-NA
raw_2023[raw_2023==99]<-NA

#1. Construction of database ----

apply(raw_2023, 2, function(x) which(x == 99))

## Home Income (only 2023)
ingresos <- raw_2023%>%dplyr::select(starts_with("g3_")) %>% mutate_all(as.integer)
raw_2023$ingresos <- coalesce(ingresos$g3_1,ingresos$g3_2,ingresos$g3_3,
                              ingresos$g3_4,ingresos$g3_5,ingresos$g3_6,
                              ingresos$g3_7,ingresos$g3_8,ingresos$g3_9,
                              ingresos$g3_10)

## Innovative personality both
innovative_variables_2015 <- raw_2015 %>% dplyr::select(p046_01, p046_02, p046_03)%>%
                             mutate(across(everything(),~ifelse(.==99,NA,.)))
raw_2015$innovative <- innovative_variables_2015 %>% mutate_all(as.integer)%>%
                       rowMeans(na.rm=T)
cronbach.alpha(innovative_variables_2015, CI=TRUE) #Alpha of index


innovative_variables_2023 <- raw_2023 %>% dplyr::select(starts_with("f2_"))%>%
                             mutate(across(everything(),~ifelse(.==99,NA,.)))
raw_2023$innovative <- innovative_variables_2023 %>% mutate_all(as.integer)%>%
                       rowMeans(na.rm=T)
cronbach.alpha(innovative_variables_2023, CI=TRUE) #Alpha of index

#Social networks
raw_2015$soc_net <- raw_2015%>%dplyr::select(starts_with("p047_"))%>%
                    mutate_all(as.integer)%>%
                    mutate(across(everything(),~case_when(.==2~0,
                                                          .==1~1,
                                                          TRUE ~ NA_real_)))%>%
                    rowSums(na.rm=T)

raw_2023$soc_net <- raw_2023%>%dplyr::select(starts_with("f6_"))%>%
  mutate_all(as.integer)%>%
  mutate(across(everything(),~case_when(.==2~0,
                                        .==1~1,
                                        TRUE~NA_real_)))%>%
  rowSums(na.rm=T)

#Dichotomize user of internet (only 2023)
raw_2015 <- raw_2015 %>% mutate(p018=ifelse(p018==2,0,p018))
raw_2023 <- raw_2023 %>% mutate(b6=ifelse(b6==2,0,b6))


# Seleccionar variables
beyond_2015 <- raw_2015 %>% dplyr::select(
                                age=edad, #Age
                                education=p070, #Education household
                                sex=sexo, #Gender
                                income=p075, #Income
                                innovative, #Innovative
                                transport=p053, #Transport frequency
                                user=p018, #Internet user
                                soc_net #Social Network
                                )

beyond_2023 <- raw_2023 %>% dplyr::select(
                                age=a2, #Age
                                education=g6, #Education household
                                sex=a1, #Gender
                                income=ingresos, #Income
                                innovative, #Innovative
                                transport=f14, #Transport frequency
                                user=b6, #Internet user
                                soc_net #Social Network
                                )

#2. Description of variables

dfSummary(beyond_2015, 
          plain.ascii  = FALSE,
          style        = 'grid',
          graph.magnif = 0.85,
          varnumbers = FALSE,
          valid.col    = FALSE,
          tmp.img.dir  = "/tmp",
          method="render")

dfSummary(beyond_2023, 
          plain.ascii  = FALSE,
          style        = 'grid',
          graph.magnif = 0.85,
          varnumbers = FALSE,
          valid.col    = FALSE,
          tmp.img.dir  = "/tmp",
          method="render")

#3. Modelo logístico

glm_2015 <- glm(user~age+sex+innovative+education+income+transport+soc_net,
                data = beyond_2015,family = "binomial")

glm_2023 <- glm(user~age+sex+innovative+education+income+transport+soc_net,
                data = beyond_2023,family = "binomial")

texreg::htmlreg(l=list(glm_2015,glm_2023),
                custom.coef.names=c("Intercept",
                  
                                    #Personal factors
                                    "Age (continuos)","Sex (1=Woman)",
                                    "Innovative personality (1-4)",
                                    
                                    #Positional factors
                                    "Education of head of household (1-6)",
                                    
                                    #Material resources
                                    "House income", "Transport conectivity (1-6)",
                                    
                                    #Social resources
                                    "Social network"
                                    ),
                custom.model.names = c("2015","2023"))
