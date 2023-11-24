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

data$tech_anxiety <- data %>% 
                      mutate_at(.vars=vars(starts_with("c1_")),
                                .funs=list(~ifelse(.==99,NA,.))) %>%
                      dplyr::select(starts_with("c1_")) %>%
                      mutate_all(as.numeric) %>%
                      rowMeans(na.rm=T)

# Technological Self-efficacy


data$tech_self_efficacy <- data %>% mutate_at(.vars=vars(starts_with("c2_")),
                                              .funs=list(~ifelse(.==99,NA,.))) %>%
                                    dplyr::select(starts_with("c2_"))%>%
                                    mutate_all(as.numeric) %>%
                                    rowMeans(na.rm=T)

# Digital skills

data_skills <- data %>% mutate_at(.vars=vars(starts_with("c3_")),
                                  .funs=list(~ifelse(.==2,0,.))) # Recode dummy
## Operational skills
data$tech_operational_skills <- data_skills %>% 
                            dplyr::select(starts_with("c3_1")) %>%
                            mutate(across(everything(),
                                          ~ifelse(.==99,NA,.))) %>%
                            mutate_all(as.numeric) %>%
                            rowSums(na.rm = T)

## Informative skills
data$tech_informative_skills <- data_skills %>% 
                            dplyr::select(starts_with("c3_2"))%>%
                            #Change direction of contradictory items
                            mutate_at(
                            .vars = vars(c3_2_3, c3_2_4),
                            .funs = list(
                            ~case_when(. == 0 ~ 1,
                                       . == 0 ~ 1,
                                       TRUE ~ .)
                                       )) %>% 
                            mutate(across(everything(),
                                          ~ifelse(.==99,NA,.))) %>%
                            mutate_all(as.numeric) %>% 
                            rowSums(na.rm = T)

## Social skills
data$tech_social_skills <- data_skills %>% 
                       dplyr::select(starts_with("c3_3")) %>%
                       mutate(across(everything(),~ifelse(.==99,NA,.))) %>%
                       mutate_all(as.numeric) %>% 
                       rowSums(na.rm = T)

## Creative skills
data$tech_creative_skills <- data_skills %>% 
                         dplyr::select(starts_with("c3_4")) %>%
                         mutate(across(everything(),
                                       ~ifelse(.==99,NA,.))) %>%
                         mutate_all(as.numeric) %>% 
                         rowSums(na.rm = T)

## Digital skills (total)
data$digital_skills <- data_skills %>% 
                          dplyr::select(starts_with("c3")) %>%
                          mutate(across(everything(),
                                        ~ifelse(.==99,NA,.))) %>%
                          mutate_all(as.numeric) %>% 
                          rowSums(na.rm = T)

# Digital Literacy

data$digital_literacy <- data %>%
                          dplyr::select(starts_with("c4_")) %>%
                          mutate_all(as.numeric) %>%
                          mutate(across(everything(),
                                        ~ifelse(.==99,NA,.))) %>% 
                          rowMeans(na.rm=T)

# Algorithmic awareness

data$algorithmic_awareness <- data %>% 
                               mutate(across(starts_with("d2"),
                                             ~ifelse(.==99,1,.))) %>%
                               dplyr::select(starts_with("d2")) %>%
                               mutate_all(as.numeric) %>% 
                               rowMeans(na.rm=T)

# Innovative personality

data$innovative_personality <- data %>% 
                                mutate_at(.vars=vars(starts_with("f2_")),
                                          .funs=list(~ifelse(.==99,NA,.))) %>% 
                                dplyr::select(starts_with("f2_")) %>%
                                mutate_all(as.numeric) %>% 
                                rowMeans(na.rm=T)

# Social capital

data$social_capital <- data %>% 
                          dplyr::select(starts_with("f6_")) %>%
                          mutate(across(everything(),
                                        ~ifelse(.==99,NA,.))) %>%
                          mutate(across(everything(),
                                        ~ifelse(.==2,0,.))) %>%
                          mutate_all(as.numeric) %>% 
                          rowSums(na.rm = T)

# 3. Export data ----

#Sav
write_sav(data,"01_input/02_proc_data/digital_inclusion_2023.sav")

#RData
save(data,file = "01_input/02_proc_data/digital_inclusion_2023.RData")





