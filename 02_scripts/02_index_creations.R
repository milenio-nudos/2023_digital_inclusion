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

# Extreme political position index

raw_2023<-raw_2023 %>% 
  mutate(i_extreme_politics=case_when(f18%in%c(1,10)~5,
                                      f18%in%c(2,9)~4,
                                      f18%in%c(3,8)~3,
                                      f18%in%c(4,7)~2,
                                      f18%in%c(5,6)~1))

# List of Cronbach's alphas

Cronbach<-c(c2["alpha"],c3["alpha"],c3_1["alpha"],c3_2["alpha"],c3_3["alpha"],c3_4["alpha"],
  c4_a["alpha"],c4_b["alpha"],c4["alpha"],d2["alpha"],NA,NA,NA,NA,NA,NA,NA,NA)

ci<-c(c2["ci"],c3["ci"],c3_1["ci"],c3_2["ci"],c3_3["ci"],c3_4["ci"],
            c4_a["ci"],c4_b["ci"],c4["ci"],d2["ci"],NA,NA,NA,NA,NA,NA,NA,NA)

# 2. Descriptive table ----

raw_2023 %>% dplyr::select(starts_with(c("i_","c5")),f5_2) %>% 
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
                  "Algorithmic awareness index",
                  "Political polar position index",
                  "I have shared information that subsequently turned out to be false or misleading",
                  "I have criticised someone for spreading false or misleading information.",
                  "When I have received false or misleading news, I have and made it clear that it is false.",
                  "I have blocked or unfollowed people because they they post false information",
                  "I have believed news that later turned out to be false or misleading",
                  "I have seen or read false or misleading news",
                  "I feel I have a good understanding of important public issues in Chile."
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
                      "By means",
                      "Polar recodification",
                      "Raw data",
                      "Raw data",
                      "Raw data",
                      "Raw data",
                      "Raw data",
                      "Raw data",
                      "Raw data"))%>%
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
         "C.I. [2.5%;97.5%]"=ci)%>%

  gt() %>%
  sub_missing(missing_text = "")


# 3. Correlations ----

model_data <- raw_2023 %>% dplyr::select(starts_with(c("i_","c5")),f5_2,
                                         gender=a1,age=a2,ses=nse)%>%
              mutate(gender=ifelse(gender==2,1,0))%>%
              mutate_all(as.numeric)

var_label(model_data)<-c("Self-efficacy index",
                     "Operational skills index",
                     "Informative skills index",
                     "Social skills index",
                     "Creative skills index",
                     "Digital skills index (total)",
                     "Social media literacy index",
                     "Algorithmic literacy index",
                     "Digital literacy index (total)",
                     "Algorithmic awareness index",
                     "Political polar position index",
                     "I have shared information that subsequently turned out to be false or misleading",
                     "I have criticised someone for spreading false or misleading information.",
                     "When I have received false or misleading news, I have and made it clear that it is false.",
                     "I have blocked or unfollowed people because they they post false information",
                     "I have believed news that later turned out to be false or misleading",
                     "I have seen or read false or misleading news",
                     "I feel I have a good understanding of important public issues in Chile",
                     "Gender (Women=1)",
                     "Age",
                     "Socioeconomical Status")

corr_matrix<-cor(model_data%>%dplyr::select(-c(i_extreme_politics,gender,age,ses))%>%
                   drop_na())

testRes = cor.mtest(model_data%>%dplyr::select(-c(i_extreme_politics,gender,age,ses))%>%
                      drop_na(), conf.level = 0.95)


corrplot(corr_matrix, method = 'circle', type = 'lower',p.mat = testRes$p, insig = 'blank',
         addCoef.col = "black",order = 'AOE', diag=FALSE,number.cex = 0.5)

tab_corr(model_data)

# 4. Models ----

# Algorithmic awareness + control
c5_1_a<-lm(c5_1 ~ i_algorithmic_awareness + age + gender + ses, data = model_data)
c5_2_a<-lm(c5_2 ~ i_algorithmic_awareness + age + gender + ses, data = model_data)
c5_3_a<-lm(c5_3 ~ i_algorithmic_awareness + age + gender + ses, data = model_data)
c5_4_a<-lm(c5_4 ~ i_algorithmic_awareness + age + gender + ses, data = model_data)
c5_6_a<-lm(c5_6 ~ i_algorithmic_awareness + age + gender + ses, data = model_data)
c5_7_a<-lm(c5_7 ~ i_algorithmic_awareness + age + gender + ses, data = model_data)

# Digital literacy + control
c5_1_b<-lm(c5_1 ~ i_digital_literacy + age + gender + ses, data = model_data)
c5_2_b<-lm(c5_2 ~ i_digital_literacy + age + gender + ses, data = model_data)
c5_3_b<-lm(c5_3 ~ i_digital_literacy + age + gender + ses, data = model_data)
c5_4_b<-lm(c5_4 ~ i_digital_literacy + age + gender + ses, data = model_data)
c5_6_b<-lm(c5_6 ~ i_digital_literacy + age + gender + ses, data = model_data)
c5_7_b<-lm(c5_7 ~ i_digital_literacy + age + gender + ses, data = model_data)

# Great model
c5_1_c<-lm(c5_1 ~ i_digital_literacy + i_digital_skills +
             i_algorithmic_awareness + i_tech_self_efficacy +
             age + gender + ses, data = model_data)
c5_2_c<-lm(c5_2 ~ i_digital_literacy + i_digital_skills +
             i_algorithmic_awareness + i_tech_self_efficacy +
             age + gender + ses, data = model_data)
c5_3_c<-lm(c5_3 ~ i_digital_literacy + i_digital_skills +
             i_algorithmic_awareness + i_tech_self_efficacy +
             age + gender + ses, data = model_data)
c5_4_c<-lm(c5_4 ~ i_digital_literacy + i_digital_skills +
             i_algorithmic_awareness + i_tech_self_efficacy +
             age + gender + ses, data = model_data)
c5_6_c<-lm(c5_6 ~ i_digital_literacy + i_digital_skills +
             i_algorithmic_awareness + i_tech_self_efficacy +
             age + gender + ses, data = model_data)
c5_7_c<-lm(c5_7 ~ i_digital_literacy + i_digital_skills +
             i_algorithmic_awareness + i_tech_self_efficacy +
             age + gender + ses, data = model_data)

# Tables
tab_model(c5_1_a, c5_2_a,c5_3_a,c5_4_a,c5_6_a,c5_7_a, 
          show.ci = FALSE, auto.label = TRUE,
          p.style = "stars",collapse.se = TRUE,
          show.re.var = FALSE,show.icc = FALSE,
          show.obs = FALSE,show.ngroups = FALSE)

tab_model(c5_1_b, c5_2_b,c5_3_b,c5_4_b,c5_6_b,c5_7_b, 
          show.ci = FALSE, auto.label = TRUE,
          p.style = "stars",collapse.se = TRUE,
          show.re.var = FALSE,show.icc = FALSE,
          show.obs = FALSE,show.ngroups = FALSE)

tab_model(c5_1_c, c5_2_c,c5_3_c,c5_4_c,c5_6_c,c5_7_c, 
          show.ci = FALSE, auto.label = TRUE,
          p.style = "stars",collapse.se = TRUE,
          show.re.var = FALSE,show.icc = FALSE,
          show.obs = FALSE,show.ngroups = FALSE)

