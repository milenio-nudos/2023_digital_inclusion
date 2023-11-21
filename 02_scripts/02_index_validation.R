# 0. Preparation ----

pacman::p_load(
  tidyverse, #Data manipulation
  ltm, #Get Cronbach
  skimr, #Descriptive table
  gt, #Render tables
  labelled #to_numeric
)

# load("01_input/02_proc_data/digital_inclusion_2023.RData") #Put as text for render manual


# 1. Estimation

# Create alpha objects
c1 <-cronbach.alpha(data %>% dplyr::select(starts_with("c1_")) %>%
                      drop_na(), CI=TRUE) #Alpha of index

c2 <-cronbach.alpha(data %>% dplyr::select(starts_with("c2_")) %>%
                      drop_na(), CI=TRUE) #Alpha of index

c4 <- cronbach.alpha(data %>% dplyr::select(starts_with("c4_")) %>%
                       drop_na(), CI=TRUE) #Alpha of index

d2 <- cronbach.alpha(data %>% dplyr::select(starts_with("d2")) %>%
                       drop_na(), CI=TRUE) #Alpha of index

f2 <-cronbach.alpha(data %>% dplyr::select(starts_with("f2_")) %>%
                      drop_na(), CI=TRUE) #Alpha of index


# List of Cronbach's alphas

Cronbach<-c(c1["alpha"],c2["alpha"],NA,NA,NA,NA,NA,
            c4["alpha"],d2["alpha"],f2["alpha"],NA,NA)

ci<-c(c1["ci"],c2["ci"],NA,NA,NA,NA,NA,c4["ci"],d2["ci"],f2["ci"],NA,NA)

# 2. Descriptive Table

data %>% dplyr::select(starts_with("i_"),nse) %>% mutate(nse=as.numeric(nse)) %>%
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
                  "Capital social",
                  "Nivel Socioeconómico"
  ),
  Estimation=c("Por promedio",
               "Por promedio",
               "Sumativo",
               "Sumativo",
               "Sumativo",
               "Sumativo",
               "Sumativo",
               "Por promedio",
               "Por promedio",
               "Por promedio",
               "Sumativo",
               "Indicador"),
  
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
          "[1-13]",
          "[Categórica]")
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


## Code continues in manual...