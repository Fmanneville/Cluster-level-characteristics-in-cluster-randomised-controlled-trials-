# Packages and database ####
library(gtsummary)
library(lavaan)
library(dplyr)
library(knitr)
library(ggplot2)
library(questionr)
library(PROscorerTools)
library(haven)
library(labelled)
library(psych)
library(data.table)
library(readr)
library(forcats)
library(finalfit)
library(naniar)
library(sas7bdat)
library(plyr)
library(tidyverse)
library(readr)
library(readxl)
library(fastDummies)
library(DescTools)
X <- read_excel("X:XXXXXXXXX/Table_cluster_cRCTs.xls")

ESCIENT <- X[X$Reviewer == "Consensus", ]
summary(ESCIENT)

#### DATAMANAGEMENT ####
  #### NCT Number ####
ESCIENT <- ESCIENT %>%
  dplyr::mutate(
    NCT_number_2cl = ifelse(
      trimws(tolower(`NCT NUMBER`)) == "none",
      "no",
      "yes"
    )
  )

  #### Objective ####

ESCIENT <- dummy_cols(ESCIENT, 
                          select_columns = "OBJECTIVE",
                          split = ",",
                          remove_selected_columns = FALSE)

  #### Characteristics of the intervention ####

ESCIENT <- dummy_cols(ESCIENT, 
                      select_columns = "CHARACTERISTICS OF THE INTERVENTION",
                      split = ",",
                      remove_selected_columns = FALSE)


  #### Methodological quality of RCT ####

ESCIENT <- dummy_cols(ESCIENT, 
                      select_columns = "METHODOLOGICAL QUALITY OF RCT",
                      split = ",",
                      remove_selected_columns = FALSE)
ESCIENT <- ESCIENT %>%
  mutate(`METHODOLOGICAL QUALITY OF RCT_Accounted for clustering in analysis` = replace_na(`METHODOLOGICAL QUALITY OF RCT_Accounted for clustering in analysis`, 0))
ESCIENT <- ESCIENT %>%
  mutate(`METHODOLOGICAL QUALITY OF RCT_Accounted for clustering in sample size` = replace_na(`METHODOLOGICAL QUALITY OF RCT_Accounted for clustering in sample size`, 0))
ESCIENT <- ESCIENT %>%
  mutate(`METHODOLOGICAL QUALITY OF RCT_Allocated minimum of four clusters per arm` = replace_na(`METHODOLOGICAL QUALITY OF RCT_Allocated minimum of four clusters per arm`, 0))
ESCIENT <- ESCIENT %>%
  mutate(`METHODOLOGICAL QUALITY OF RCT_Used restricted randomization` = replace_na(`METHODOLOGICAL QUALITY OF RCT_Used restricted randomization`, 0))

  #### Study coverage ####
ESCIENT <- ESCIENT %>%
  mutate(
    `study coverage` = case_when(
      `POP & SAMPLING recruitment area` %in%
        c("Cities", "City", "Country", "District",
          "Hospital", "Region", "School", "University") ~ "national",
      
      `POP & SAMPLING recruitment area` == "Countries" ~ "international",
      
      `POP & SAMPLING recruitment area` == "NA" ~ "Not reported"
    )
  )

  #### Nature of the cluster ####
n_clinical <- sum(grepl("Clinical", ESCIENT$`POP & SAMPLING - CLUSTER LEVEL nature of the cluster as random unit`))
n_clinical
prop.table(table(grepl("Clinical", ESCIENT$`POP & SAMPLING - CLUSTER LEVEL nature of the cluster as random unit`)))
n_nonclinical <- sum(grepl("Non-clinical", ESCIENT$`POP & SAMPLING - CLUSTER LEVEL nature of the cluster as random unit`))
n_nonclinical
prop.table(table(grepl("Non-clinical", ESCIENT$`POP & SAMPLING - CLUSTER LEVEL nature of the cluster as random unit`)))
    #step 1: Creation of a table from data
data <- tribble(
  ~Type, ~Subcategory, ~Count,
  "Clinical", "Hospital units, wards", 4,
  "Clinical", "Individual health professionals", 6,
  "Clinical", "Medical practices or clinics", 39,
  "Clinical", "Nursing homes or wards", 6,
  "Clinical", "Other (C)", 16,
  "Non Clinical", "Residential areas", 44,
  "Non Clinical", "Sport teams, clubs, churches, other social groups", 12,
  "Non Clinical", "Schools or classrooms", 51,
  "Non Clinical", "Worksites", 8,
  "Non Clinical", "Other (NC)", 19
)
    # Step 2: Calculation of % of each nature
data <- data %>%
  mutate(count_tot = ifelse(Type == "Clinical", 69, 132))
data$count_tot <- as.numeric(data$count_tot)
data$pourcentage <- round((data$Count / data$count_tot) * 100, 2)


  #### Transformation of categorical variables into continuous variables ####
ESCIENT$number_cluster <- as.numeric(as.character(ESCIENT$`RESULTS Total number of cluster included`))

  #### Creation of binary variables (cluster level, description clusters, participants, outcomes)####
ESCIENT$crit_select_cluster <- ifelse(ESCIENT$`POP & SAMPLING - CLUSTER LEVEL Selection criteria of the clusters` == '0', 0, 1)
ESCIENT$crit_select_participant <- ifelse(ESCIENT$`POP & SAMPLING - IND LEVEL number of formal selection criteria of the participants` == '0', 0, 1)
ESCIENT$description_cluster <- ifelse(ESCIENT$`RESULTS Number of characteristics of clusters included` == '0', 0, 1)
ESCIENT$description_participant <- ifelse(ESCIENT$`RESULTS Number of characteristics of participants` == '0', 0, 1)
ESCIENT$description_outcome <- ifelse(ESCIENT$`RESULTS Number of characteristics of participants (outcomes)` == '0', 0, 1)

ESCIENT$rando_p <- na_if(ESCIENT$`RESULTS Nature of characteristics of clusters included for randomization purpose`, "NA")
ESCIENT$rando_p_bis <- ifelse(is.na(ESCIENT$rando_p), NA, ifelse(ESCIENT$rando_p == 0, 0, 1))
ESCIENT$desc_p <- na_if(ESCIENT$`RESULTS Nature of characteristics of clusters included for descriptive purpose`, "NA")
ESCIENT$desc_p_bis <- ifelse(is.na(ESCIENT$desc_p), NA, ifelse(ESCIENT$desc_p == 0, 0, 1))
ESCIENT$purpose <- case_when(
  ESCIENT$rando_p_bis == 1 & ESCIENT$desc_p_bis == 1 ~ "rando_desc",
  ESCIENT$rando_p_bis == 1                ~ "rando",
  ESCIENT$desc_p_bis  == 1                ~ "desc",
  TRUE                         ~ NA_character_
)
  #### Creation of ordinal variables#### 

          # selection criteria of clusters
ESCIENT$nb_crit_select_cluster <- factor(
  ESCIENT$`POP & SAMPLING - CLUSTER LEVEL Selection criteria of the clusters`,
  ordered = TRUE
)
ESCIENT <- ESCIENT %>%
  mutate(
    nb_crit_select_cluster_na = ifelse(as.numeric(as.character(nb_crit_select_cluster)) == 0,
                                NA,
                                as.numeric(as.character(nb_crit_select_cluster)))
  )
ESCIENT$nb_crit_select_cluster_naQ <- as.character(ESCIENT$nb_crit_select_cluster_na)

          # selection criteria of participants
ESCIENT$nb_crit_select_part <- factor(
  ESCIENT$`POP & SAMPLING - IND LEVEL number of formal selection criteria of the participants`,
  ordered = TRUE
)
ESCIENT <- ESCIENT %>%
  mutate(
    nb_crit_select_part_na = ifelse(as.numeric(as.character(nb_crit_select_part)) == 0,
                                       NA,
                                       as.numeric(as.character(nb_crit_select_part)))
  )
ESCIENT$nb_crit_select_part_naQ <- as.character(ESCIENT$nb_crit_select_part_na)

         # characteristics of clusters
ESCIENT$nb_cstq_cluster <- factor(
  ESCIENT$`RESULTS Number of characteristics of clusters included`,
  ordered = TRUE
)
ESCIENT <- ESCIENT %>%
  mutate(
    nb_cstq_cluster_na = ifelse(as.numeric(as.character(nb_cstq_cluster)) == 0,
                       NA,
                       as.numeric(as.character(nb_cstq_cluster)))
  )
ESCIENT$nb_cstq_cluster_naQ <- as.character(ESCIENT$nb_cstq_cluster_na)

        # characteristics of participants
ESCIENT$nb_cstq_part <- factor(
  ESCIENT$`RESULTS Number of characteristics of participants`,
  ordered = TRUE
)
ESCIENT <- ESCIENT %>%
  mutate(
    nb_cstq_part_na = ifelse(as.numeric(as.character(nb_cstq_part)) == 0,
                                NA,
                                as.numeric(as.character(nb_cstq_part)))
  )
ESCIENT$nb_cstq_part_b <- factor(
  ESCIENT$`RESULTS Number of characteristics of participants (outcomes)`,
  ordered = TRUE
)
ESCIENT <- ESCIENT %>%
  mutate(
    nb_cstq_part_b_na = ifelse(as.numeric(as.character(nb_cstq_part_b)) == 0,
                                NA,
                                as.numeric(as.character(nb_cstq_part_b)))
  )


#### ANALYSES ####
  #### Description of variables####
      #Table 1#
theme_gtsummary_language("fr", decimal.mark = ",", big.mark = " ")
ESCIENT %>%
  dplyr::select('YEAR OF PUBLICATION',  
                'NCT_number_2cl',
                'subject_area',
                'CHARACTERISTICS OF THE INTERVENTION_Individual - cluster',
                'CHARACTERISTICS OF THE INTERVENTION_Professional - cluster',
                'CHARACTERISTICS OF THE INTERVENTION_External - cluster',
                'CHARACTERISTICS OF THE INTERVENTION_Cluster - cluster',
                'study coverage',
                'number_cluster',
                'POP & SAMPLING - CLUSTER LEVEL number of levels',
                'POP & SAMPLING - CLUSTER LEVEL nature of the cluster as random unit',
                'TYPE OF ANALYSIS',
                'METHODOLOGICAL QUALITY OF RCT_Used restricted randomization',
                'METHODOLOGICAL QUALITY OF RCT_Allocated minimum of four clusters per arm',
                'METHODOLOGICAL QUALITY OF RCT_Accounted for clustering in sample size',
                'METHODOLOGICAL QUALITY OF RCT_Accounted for clustering in analysis',
                'METHODOLOGICAL QUALITY OF RCT_NA') %>%
  tbl_summary(digits = list(all_categorical()~c(0,1),all_continuous()~c(1,1)),
              missing = 'always',statistic = list(all_continuous() ~ 
                                                    "{mean} (±{sd}) | Médiane : {median} (Q1: {p25}, Q3: {p75}) | Min-Max : ({min} - {max})"
              )
  ) 

      #Table 2#            
theme_gtsummary_language("fr", decimal.mark = ",", big.mark = " ")
ESCIENT %>%
  dplyr::select('crit_select_cluster',
                'nb_crit_select_cluster_naQ',
                'nb_crit_select_cluster_na',
                'crit_select_participant',
                'nb_crit_select_part_naQ',
                'nb_crit_select_part_na',
                'description_cluster',
                'purpose',
                'nb_cstq_cluster_naQ',
                'nb_cstq_cluster_na',
                'description_participant',
                'nb_cstq_part_na',
                'description_outcome',
                'nb_cstq_part_b_na') %>%
  tbl_summary(
    type = list(
      nb_crit_select_cluster_na ~ "continuous"
    ),
    digits = list(
      all_categorical() ~ c(0, 1),
      all_continuous() ~ c(1, 1)
    ),
    missing = "always",
    statistic = list(
      all_continuous() ~
        "{mean} (±{sd}) | Médiane : {median} (Q1: {p25}, Q3: {p75}) | Min-Max : ({min} - {max})"
    )
  )

  #### Description of variables by year of publication####

theme_gtsummary_language("fr", decimal.mark = ",", big.mark = " ")

ESCIENT %>%
  dplyr::select(
    'YEAR OF PUBLICATION',
    'crit_select_cluster',
    'nb_crit_select_cluster_naQ',
    'nb_crit_select_cluster_na',
    'description_cluster',
    'purpose',
    'nb_cstq_cluster_naQ',
    'nb_cstq_cluster_na'
  ) %>%
  tbl_summary(
    by = `YEAR OF PUBLICATION`,
    
    type = list(
      nb_crit_select_cluster_na ~ "continuous"
    ),
    
    digits = list(
      all_categorical() ~ c(0, 1),
      all_continuous() ~ c(1, 1)
    ),
    
    missing = "always",
    
    statistic = list(
      all_continuous() ~
        "{mean} (±{sd}) | Médiane : {median} (Q1: {p25}, Q3: {p75}) | Min-Max : ({min} - {max})"
    )
  )

      # trend test

          #Binary outcomes
ESCIENT$year <- as.numeric(as.character(ESCIENT$`YEAR OF PUBLICATION`))

modele <- glm(crit_select_cluster	 ~ year, 
              data = ESCIENT, 
              family = binomial)
summary(modele)
exp(cbind(
  OR = coef(modele),
  confint(modele)
))
CochranArmitageTest(table(ESCIENT$year, ESCIENT$crit_select_cluster))


modele <- glm(description_cluster	 ~ year, 
              data = ESCIENT, 
              family = binomial)
summary(modele)
exp(cbind(
  OR = coef(modele),
  confint(modele)
))
CochranArmitageTest(table(ESCIENT$year, ESCIENT$description_cluster))

          #Continuous outcomes
modele <- lm(nb_crit_select_cluster_na ~ year, data = ESCIENT)
summary(modele)


modele <- lm(nb_cstq_cluster_na ~ year, data = ESCIENT)
summary(modele)

#### FIGURES####
  #### Methodological quality####

data <- data.frame(
  combinaison = c(
    "None",
    "Accounted for clustering in analysis",
    "Accounted for clustering in sample size",
    "Accounted for clustering in sample size,Accounted for clustering in analysis",
    "Allocated minimum of four clusters per arm",
    "Allocated minimum of four clusters per arm,Accounted for clustering in analysis",
    "Allocated minimum of four clusters per arm,Accounted for clustering in sample size",
    "Allocated minimum of four clusters per arm,Accounted for clustering in sample size,Accounted for clustering in analysis",
    "Used restricted randomization",
    "Used restricted randomization,Accounted for clustering in analysis",
    "Used restricted randomization,Accounted for clustering in sample size",
    "Used restricted randomization,Allocated minimum of four clusters per arm",
    "Used restricted randomization,Allocated minimum of four clusters per arm,Accounted for clustering in analysis",
    "Used restricted randomization,Allocated minimum of four clusters per arm,Accounted for clustering in sample size",
    "Used restricted randomization,Allocated minimum of four clusters per arm,Accounted for clustering in sample size,Accounted for clustering in analysis"
  ),
  valeur = c(17, 3, 4, 2, 12, 14, 6, 21, 1, 2, 2, 8, 27, 7, 74)
)

library(stringr)
library(eulerr)

comb_data <- data.frame(
  Accounted_analysis = str_detect(data$combinaison, "Accounted for clustering in analysis"),
  Accounted_sample = str_detect(data$combinaison, "Accounted for clustering in sample size"),
  Allocated_clusters = str_detect(data$combinaison, "Allocated minimum of four clusters per arm"),
  Restricted_randomization = str_detect(data$combinaison, "Used restricted randomization"),
  value = data$valeur
)


library(dplyr)
library(tidyr)
library(ComplexUpset)
library(ggplot2)

vars <- c(
  "Accounted_analysis",
  "Accounted_sample",
  "Allocated_clusters",
  "Restricted_randomization"
)

#row_missing <- data.frame(
#  Accounted_analysis = TRUE,
#  Accounted_sample = TRUE,
#  Allocated_clusters = FALSE,
#  Restricted_randomization = TRUE,
#  value = 1
#)

#comb_data_full <- dplyr::bind_rows(comb_data, row_missing)

# Déplier: répète chaque combinaison 'value' fois
comb_expanded <- comb_data %>%
  tidyr::uncount(weights = value)

comb_expanded <- comb_expanded %>%
  dplyr::rename(
    "Statistical analysis" = Accounted_analysis,
    "Sample size calculation" = Accounted_sample,
    "≥ 4 clusters per arm" = Allocated_clusters,
    "Restricted randomization" = Restricted_randomization
  )

vars <- c(
  "Statistical analysis",
  "Sample size calculation",
  "≥ 4 clusters per arm",
  "Restricted randomization"
)


library(ggplot2)
library(grid)

# Remplace le plot_theme "cassé" par une version compatible ggplot2
plot_theme <- function() {
  theme_minimal(base_size = 12) +
    theme(
      axis.ticks = element_line(),
      axis.ticks.length = unit(2, "pt"),   
      panel.grid.minor = element_blank()
    )
}

# Basic usage 
upset(
  comb_expanded,
  intersect = vars,
  name = "Criteria",
  width_ratio = 0.2,
  wrap = TRUE,
  sort_intersections = "descending",
  sort_sets = "descending",
  min_size = 1,
  matrix = intersection_matrix(
    segment = geom_blank()
  ),
  themes = upset_default_themes(
    text = element_text(size = 16),
    axis.title = element_text(size = 20),
    axis.text  = element_text(size = 20),
    strip.text = element_text(size = 20),
    plot.title = element_text(size = 20)
  ),
  base_annotations = list(
    "Number" = intersection_size(
      text_mapping = aes(label = paste0(
        !!get_size_mode("exclusive_intersection"), 
        "\n(",
        !!upset_text_percentage(digits=1),
        ")"
      )),
      bar_number_threshold = 1,
      text = list(vjust = -0.5, size=5.5),
    )+
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.30))) 
  ),
  set_sizes=(
    upset_set_size()
    + ylab ('Number')
    + geom_text(aes(label=..count..), colour= "white", size=6, hjust=-0.2, stat='count')
  )
) +
  plot_theme() &
  theme(
    text = element_text(size = 16),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14)
  )

  #### Intervention typology####

library(dplyr)
library(tidyr)
library(ComplexUpset)
library(ggplot2)

data <- data.frame(
  combinaison = c(
    "Cluster - cluster",
    "External - cluster",
    "External - cluster,Cluster - cluster",
    "Individual - cluster",
    "Individual - cluster,Cluster - cluster",
    "Individual - cluster,External - cluster",
    "Individual - cluster,External - cluster,Cluster - cluster",
    "Individual - cluster,Professional - cluster",
    "Individual - cluster,Professional - cluster,Cluster - cluster",
    "Individual - cluster,Professional - cluster,External - cluster",
    "Individual - cluster,Professional - cluster,External - cluster,Cluster - cluster",
    "Professional - cluster",
    "Professional - cluster,Cluster - cluster",
    "Professional - cluster,Cluster - cluster,External - cluster",
    "Professional - cluster,External - cluster",
    "Professional - cluster,External - cluster,Cluster - cluster"
  ),
  valeur = c(2,6,5,3,22,16,21,13,31,8,13,9,30,1,9,11)
)

# 2. Créer les variables binaires 
comb_data <- data.frame(
  Individual = str_detect(data$combinaison, "Individual"),
  Professional = str_detect(data$combinaison, "Professional"),
  External = str_detect(data$combinaison, "External"),
  Cluster = str_detect(data$combinaison, "Cluster - cluster"),
  value = data$valeur
)



# Déplier: répète chaque combinaison 'value' fois
comb_expanded <- comb_data %>%
  tidyr::uncount(weights = value)

comb_expanded <- comb_expanded %>%
  dplyr::rename(
    "Individual-cluster" = Individual,
    "Professional-cluster" = Professional,
    "External-cluster" = External,
    "Cluster-cluster" = Cluster
  )

vars <- c(
  "Individual-cluster",
  "Professional-cluster",
  "External-cluster",
  "Cluster-cluster"
)

library(ggplot2)
library(grid)

# Remplace le plot_theme "cassé" par une version compatible ggplot2
plot_theme <- function() {
  theme_minimal(base_size = 12) +
    theme(
      axis.ticks = element_line(),
      axis.ticks.length = unit(2, "pt"),   # <-- IMPORTANT: unit(), pas un nombre
      panel.grid.minor = element_blank()
    )
}

# Basic usage 
upset(
  comb_expanded,
  intersect = vars,
  name = "Type of intervention",
  width_ratio = 0.2,
  wrap = TRUE,
  sort_intersections = "descending",
  sort_sets = "descending",
  min_size = 1,
  matrix = intersection_matrix(
    segment = geom_blank()
  ),
  themes = upset_default_themes(
    text = element_text(size = 16),
    axis.title = element_text(size = 20),
    axis.text  = element_text(size = 20),
    strip.text = element_text(size = 20),
    plot.title = element_text(size = 20)
  ),
  base_annotations = list(
    "Number" = intersection_size(
      text_mapping = aes(label = paste0(
        !!get_size_mode("exclusive_intersection"), 
        "\n(",
        !!upset_text_percentage(digits=1),
        ")"
      )),
      bar_number_threshold = 1,
      text = list(vjust = -0.5, size=6),
    )+
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.30))) 
  ),
  set_sizes=(
    upset_set_size()
    + ylab ('Number')
    + geom_text(aes(label=..count..), colour= "white", size=6, hjust=-0.2, stat='count')
  )
) +
  plot_theme() &
  theme(
    text = element_text(size = 16),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14)
  )




