# 0. Preparation ----

pacman::p_load(
  tidyverse, #Data manipulation
  haven, #Read sav
  janitor, #Clean names
  ltm, #Cronbach alpha
  skimr, #Descriptive table
  gt, #Render tables
  labelled, #col_labes
  corrplot, #Corrplot
  sjPlot #Tab models
  )

raw_2023 <- read_sav("../01_input/01_raw_data/2023_survey_raw.sav") %>% clean_names()

# 1. Index creation ----

# Insecurity with technologies

raw_2023 <- raw_2023 %>% mutate_at(.vars=vars(starts_with("c1_")),
                                   .funs=list(~ifelse(.==99,NA,.)))

c1 <-cronbach.alpha(raw_2023 %>% dplyr::select(starts_with("c1_")) %>%
                      drop_na(), CI=TRUE) #Alpha of index

raw_2023$i_tech_insecurity <- raw_2023 %>% dplyr::select(starts_with("c1_"))%>%
  mutate_all(as.integer)%>%rowMeans(na.rm=T)

# Technological Self-efficacy


raw_2023 <- raw_2023 %>% mutate_at(.vars=vars(starts_with("c2_")),
                                   .funs=list(~ifelse(.==99,NA,.)))

c2 <-cronbach.alpha(raw_2023 %>% dplyr::select(starts_with("c2_")) %>%
                 drop_na(), CI=TRUE) #Alpha of index

raw_2023$i_tech_self_efficacy <- raw_2023 %>% dplyr::select(starts_with("c2_"))%>%
                                   mutate_all(as.integer)%>%rowMeans(na.rm=T)

# Digital skills

raw_2023 <- raw_2023 %>% mutate_at(.vars=vars(starts_with("c3_")),
                                   .funs=list(~ifelse(.==2,0,.))) # Recode dummy
## Operational skills
raw_2023$i_operational_skills <- raw_2023 %>% dplyr::select(starts_with("c3_1"))%>%
                               mutate(across(everything(),~ifelse(.==99,NA,.)))%>%
                               mutate_all(as.integer)%>% rowSums(na.rm = T)

## Informative skills
raw_2023$i_informative_skills <- raw_2023 %>% dplyr::select(starts_with("c3_2"))%>%
                               mutate(across(everything(),~ifelse(.==99,NA,.)))%>%
                               mutate_all(as.integer)%>% rowSums(na.rm = T)

## Social skills
raw_2023$i_social_skills <- raw_2023 %>% dplyr::select(starts_with("c3_3"))%>%
                          mutate(across(everything(),~ifelse(.==99,NA,.)))%>%
                          mutate_all(as.integer)%>% rowSums(na.rm = T)

## Creative skills
raw_2023$i_creative_skills <- raw_2023 %>% dplyr::select(starts_with("c3_4"))%>%
                            mutate(across(everything(),~ifelse(.==99,NA,.)))%>%
                            mutate_all(as.integer)%>% rowSums(na.rm = T)

## Digital skills (total)
raw_2023$i_digital_skills <- raw_2023$operational_skills <- raw_2023 %>% dplyr::select(starts_with("c3"))%>%
                           mutate(across(everything(),~ifelse(.==99,NA,.)))%>%
                           mutate_all(as.integer)%>% rowSums(na.rm = T)

# Digital Literacy

raw_2023 <- raw_2023 %>% mutate(across(starts_with("c4_"),~ifelse(.==99,NA,.)))

c4 <- cronbach.alpha(raw_2023 %>% dplyr::select(starts_with("c4_")) %>%
                 drop_na(), CI=TRUE) #Alpha of index

raw_2023$i_digital_literacy <- raw_2023 %>% dplyr::select(starts_with("c4_"))%>%
                             mutate_all(as.integer)%>%rowMeans(na.rm=T)

# Algorithmic awareness

raw_2023 <- raw_2023 %>% mutate(across(starts_with("d2"),~ifelse(.==99,NA,.)))

d2 <- cronbach.alpha(raw_2023 %>% dplyr::select(starts_with("d2")) %>%
                 drop_na(), CI=TRUE) #Alpha of index

raw_2023$i_algorithmic_awareness <- raw_2023 %>% dplyr::select(starts_with("d2"))%>%
                             mutate_all(as.integer)%>%rowMeans(na.rm=T)

# Innovative personality

raw_2023 <- raw_2023 %>% mutate_at(.vars=vars(starts_with("f2_")),
                                   .funs=list(~ifelse(.==99,NA,.)))

f2 <-cronbach.alpha(raw_2023 %>% dplyr::select(starts_with("f2_")) %>%
                      drop_na(), CI=TRUE) #Alpha of index

raw_2023$i_innovative_personality <- raw_2023 %>% dplyr::select(starts_with("f2_"))%>%
  mutate_all(as.integer)%>%rowMeans(na.rm=T)

# Social capital

raw_2023$i_social_capital <- raw_2023 %>% dplyr::select(starts_with("f6_"))%>%
  mutate(across(everything(),~ifelse(.==99,NA,.)))%>%
  mutate_all(as.integer)%>% rowSums(na.rm = T)


# List of Cronbach's alphas

Cronbach<-c(c1["alpha"],c2["alpha"],NA,NA,NA,NA,NA,
            c4["alpha"],d2["alpha"],f2["alpha"],NA)

ci<-c(c1["ci"],c2["ci"],NA,NA,NA,NA,NA,c4["ci"],d2["ci"],f2["ci"],NA)

# 2. Descriptive table ----

raw_2023 %>% dplyr::select(starts_with("i_")) %>% 
  mutate_all(~ifelse(.==99,NA,.)) %>% skim() %>% as.data.frame()%>%
  mutate(Label= c("Inseguridad tecnológica",
                  "Autoeficacia tecnológica",
                  "Habilidades digitales operativas",
                  "Habilidades digitales informativas",
                  "Habilidades digitales sociales",
                  "Habilidades digitales creativas",
                  "Habilidades digitales (total)",
                  "Alfabetización digital",
                  "Consciencia Algorítmica",
                  "Personalidad innovadora",
                  "Capital social"
                  ),
         Estimation=c("By means",
                      "By means",
                      "Summative",
                      "Summative",
                      "Summative",
                      "Summative",
                      "Summative",
                      "By means",
                      "By means",
                      "By means",
                      "Summative"),
         
         Rango=c("[1-3]",
                 "[1-3]",
                 "[0-6]",
                 "[0-5]",
                 "[0-6]",
                 "[0-3]",
                 "[0-21]",
                 "[1-5]",
                 "[1-5]",
                 "[1-4]",
                 "[1-13]")
         )%>%
  
    dplyr::select(Variable=skim_variable,
                Label,
                Estimation,
                Rango,
                Mean=numeric.mean,
                SD=numeric.sd,
                N_Missing=n_missing,
                Complete_rate=complete_rate,
                Hist=numeric.hist) %>%
  
  mutate(Mean=round(Mean,2),
         SD=round(SD,2),
         Complete_rate=paste0(round(Complete_rate*100,2),"%"),
         Cronbach=Cronbach,
         "C.I. [2.5%;97.5%]"=ci)

