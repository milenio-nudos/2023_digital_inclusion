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

# Technological Self-efficacy


raw_2023 <- raw_2023 %>% mutate_at(.vars=vars(starts_with("c2_")),
                                   .funs=list(~ifelse(.==99,NA,.)))

c2 <-cronbach.alpha(raw_2023 %>% dplyr::select(starts_with("c2_")) %>%
                 drop_na(), CI=TRUE) #Alpha of index

raw_2023$i_tech_self_efficacy <- raw_2023 %>% dplyr::select(starts_with("c2_"))%>%
                                   mutate_all(as.integer)%>%rowMeans(na.rm=T)

# Digital skills

raw_2023 <- raw_2023 %>% mutate_at(.vars=vars(starts_with("c3_")),
                                   .funs=list(~ifelse(.==2,0,.)))
## Operational skills
raw_2023$i_operational_skills <- raw_2023 %>% dplyr::select(starts_with("c3_1"))%>%
                               mutate(across(everything(),~ifelse(.==99,NA,.)))%>%
                               mutate_all(as.integer)%>% rowSums(na.rm = T)

c3_1 <- cronbach.alpha(raw_2023 %>% dplyr::select(starts_with("c3_1")) %>%
                 drop_na(), CI=TRUE)

## Informative skills
raw_2023$i_informative_skills <- raw_2023 %>% dplyr::select(starts_with("c3_2"))%>%
                               mutate(across(everything(),~ifelse(.==99,NA,.)))%>%
                               mutate_all(as.integer)%>% rowSums(na.rm = T)

c3_2<- cronbach.alpha(raw_2023 %>% dplyr::select(starts_with("c3_2")) %>%
                 drop_na(), CI=TRUE)

## Social skills
raw_2023$i_social_skills <- raw_2023 %>% dplyr::select(starts_with("c3_3"))%>%
                          mutate(across(everything(),~ifelse(.==99,NA,.)))%>%
                          mutate_all(as.integer)%>% rowSums(na.rm = T)

c3_3 <- cronbach.alpha(raw_2023 %>% dplyr::select(starts_with("c3_3")) %>%
                 drop_na(), CI=TRUE)

## Creative skills
raw_2023$i_creative_skills <- raw_2023 %>% dplyr::select(starts_with("c3_4"))%>%
                            mutate(across(everything(),~ifelse(.==99,NA,.)))%>%
                            mutate_all(as.integer)%>% rowSums(na.rm = T)

c3_4 <- cronbach.alpha(raw_2023 %>% dplyr::select(starts_with("c3_4")) %>%
                 drop_na(), CI=TRUE)

## Digital skills (total)
raw_2023$i_digital_skills <- raw_2023$operational_skills <- raw_2023 %>% dplyr::select(starts_with("c3"))%>%
                           mutate(across(everything(),~ifelse(.==99,NA,.)))%>%
                           mutate_all(as.integer)%>% rowSums(na.rm = T)

c3 <- cronbach.alpha(raw_2023 %>% dplyr::select(starts_with("c3")) %>%
                 drop_na(), CI=TRUE)

# Digital Literacy

## Social media literacy

raw_2023 <- raw_2023 %>% mutate_at(.vars=vars(c4_12,c4_13,c4_15,c4_16,c4_17,c4_18),
                                   .funs=list(~ ifelse(.==99,NA,.)))

c4_a <- cronbach.alpha(raw_2023 %>% dplyr::select(c4_12,c4_13,c4_15,c4_16,c4_17,c4_18) %>%
                 drop_na(), CI=TRUE) #Alpha of index

raw_2023$i_social_media_literacy <- raw_2023 %>% 
  dplyr::select(c4_12,c4_13,c4_15,c4_16,c4_17,c4_18)%>%
  mutate_all(as.integer)%>%rowMeans(na.rm=T)

## Algorithmic literacy

raw_2023 <- raw_2023 %>% mutate_at(.vars=vars(c4_10,c4_9,c4_14,c4_1,d1),
                                   .funs=list(~ifelse(.==99,NA,.)))

c4_b <- cronbach.alpha(raw_2023 %>% dplyr::select(c4_10,c4_9,c4_14,c4_1,d1) %>%
                 drop_na(), CI=TRUE) #Alpha of index

raw_2023$i_algorithmic_literacy <- raw_2023 %>% dplyr::select(starts_with("c4_"))%>%
  mutate_all(as.integer)%>%rowMeans(na.rm=T)

## Digital literacy (total)

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


# List of Cronbach's alphas

Cronbach<-c(c2["alpha"],c3["alpha"],c3_1["alpha"],c3_2["alpha"],c3_3["alpha"],c3_4["alpha"],
  c4_a["alpha"],c4_b["alpha"],c4["alpha"],d2["alpha"])

ci<-c(c2["ci"],c3["ci"],c3_1["ci"],c3_2["ci"],c3_3["ci"],c3_4["ci"],
            c4_a["ci"],c4_b["ci"],c4["ci"],d2["ci"])

# 2. Descriptive table ----

raw_2023 %>% dplyr::select(starts_with("i_")) %>% 
  mutate_all(~ifelse(.==99,NA,.)) %>% skim() %>% as.data.frame()%>%
  mutate(Label= c("Self-efficacy index",
                  "Operational skills index",
                  "Informative skills index",
                  "Social skills index",
                  "Creative skills index",
                  "Digital skills index (total)",
                  "Social media literacy index",
                  "Algorithmic literacy index",
                  "Digital literacy index (total)",
                  "Algorithmic awareness index"
                  ),
         Estimation=c("By means",
                      "Sumative",
                      "Sumative",
                      "Sumative",
                      "Sumative",
                      "Sumative",
                      "By means",
                      "By means",
                      "By means",
                      "By means"))%>%
    dplyr::select(Variable=skim_variable,
                Label,
                Estimation,
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

