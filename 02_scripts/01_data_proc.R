## 0. Preparing ----

pacman::p_load(
  tidyverse, #Data manipulation
  haven, #Read sav
  janitor, #Clean names
  labelled, #col_labes
  corrplot, #Corrplot
  sjlabelled #to_label
)

raw_2023 <- read_sav("01_input/01_raw_data/2023_survey_raw.sav") |> 
              clean_names() #Open data

# 1. Delete useless variables and rename wrong columns ----

data <- raw_2023 |> dplyr::select(-c(folio,
                                     gpslat,
                                     gpslong,
                                     starts_with("edad"),
                                     starts_with("parentesco"),
                                     starts_with("total"))) |> 
  
                    rename(c5_5=c5_6,
                           c5_6=c5_7) #There's not c5_5 in raw data, so we create to mantain order
                    

# 2. Index creation ----

# Anxiety with technologies

data <- data %>% mutate_at(.vars=vars(starts_with("c1_")),
                           .funs=list(~ifelse(.==99,NA,.)))

data$i_tech_insecurity <- data %>% dplyr::select(starts_with("c1_"))%>%
  mutate_all(as.integer)%>%rowMeans(na.rm=T)

# Technological Self-efficacy


data <- data %>% mutate_at(.vars=vars(starts_with("c2_")),
                           .funs=list(~ifelse(.==99,NA,.)))

data$i_tech_self_efficacy <- data %>% dplyr::select(starts_with("c2_"))%>%
  mutate_all(as.integer)%>%rowMeans(na.rm=T)

# Digital skills

data <- data %>% 
  mutate_at(
    .vars = vars(c3_2_3, c3_2_4),
    .funs = list(
      ~case_when(. == 1 ~ 2,
                 . == 2 ~ 1,
                 TRUE ~ .)
    )) #Change direction of contradictory items

data <- data %>% mutate_at(.vars=vars(starts_with("c3_")),
                           .funs=list(~ifelse(.==2,0,.))) # Recode dummy
## Operational skills
data$i_operational_skills <- data %>% dplyr::select(starts_with("c3_1"))%>%
  mutate(across(everything(),~ifelse(.==99,NA,.)))%>%
  mutate_all(as.integer)%>% rowSums(na.rm = T)

## Informative skills
data$i_informative_skills <- data %>% dplyr::select(starts_with("c3_2"))%>%
  mutate(across(everything(),~ifelse(.==99,NA,.)))%>%
  mutate_all(as.integer)%>% rowSums(na.rm = T)

## Social skills
data$i_social_skills <- data %>% dplyr::select(starts_with("c3_3"))%>%
  mutate(across(everything(),~ifelse(.==99,NA,.)))%>%
  mutate_all(as.integer)%>% rowSums(na.rm = T)

## Creative skills
data$i_creative_skills <- data %>% dplyr::select(starts_with("c3_4"))%>%
  mutate(across(everything(),~ifelse(.==99,NA,.)))%>%
  mutate_all(as.integer)%>% rowSums(na.rm = T)

## Digital skills (total)
data$i_digital_skills <- data %>% dplyr::select(starts_with("c3"))%>%
  mutate(across(everything(),~ifelse(.==99,NA,.)))%>%
  mutate_all(as.integer)%>% rowSums(na.rm = T)

# Digital Literacy

data <- data %>% mutate(across(starts_with("c4_"),~ifelse(.==99,NA,.)))

data$i_digital_literacy <- data %>% dplyr::select(starts_with("c4_"))%>%
  mutate_all(as.integer)%>%rowMeans(na.rm=T)

# Algorithmic awareness

data <- data %>% mutate(across(starts_with("d2"),~ifelse(.==99,NA,.)))

data$i_algorithmic_awareness <- data %>% dplyr::select(starts_with("d2"))%>%
  mutate_all(as.integer)%>%rowMeans(na.rm=T)

# Innovative personality

data <- data %>% mutate_at(.vars=vars(starts_with("f2_")),
                           .funs=list(~ifelse(.==99,NA,.)))

data$i_innovative_personality <- data %>% dplyr::select(starts_with("f2_"))%>%
  mutate_all(as.integer)%>%rowMeans(na.rm=T)

# Social capital

data$i_social_capital <- data %>% dplyr::select(starts_with("f6_"))%>%
  mutate(across(everything(),~ifelse(.==99,NA,.)))%>%
  mutate_all(as.integer)%>% rowSums(na.rm = T)

# 3. Export data ----

#Sav
write_sav(data,"01_input/02_proc_data/digital_inclusion_2023.sav")

#RData
save(data,file = "01_input/02_proc_data/digital_inclusion_2023.RData")





