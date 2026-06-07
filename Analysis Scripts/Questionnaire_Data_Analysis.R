#Create copy of original file with relevant variables
#----
install.packages("tidyverse")
library(tidyverse)

# Set path to your data
file_path <- "Data_File_Path"

# Define folder and file name separately
output_folder <- "Output_path"
output_file <- "Survey Data.csv"
output_path <- file.path(output_folder, output_file)

# Read data
survey_data <- read_csv(file_path)

# Select relevant columns
selected_data <- survey_data %>%
  select(
    record_id,
    redcap_event_name,
    starts_with("ptq"),
    starts_with("rpa"),
    starts_with("erq"),
    starts_with("ffmq"),
    starts_with("panas"),
    starts_with("LARSS")
  )

# Export the selected data
write_csv(selected_data, output_path)

cat("Survey Data successfully saved to:", output_path)

#

#----
#Match record ID to subject number using matching file
#----
library(tidyverse)

# Paths
survey_data_path <- output_path
mapping_file_path <- "Participant_ID_File" #file that contains the particpant's ID across differnet systems

# Load files
survey_data <- read_csv(survey_data_path)
mapping_data <- read_csv(mapping_file_path)

# Ensure record_id is character
survey_data <- survey_data %>%
  mutate(record_id = as.character(record_id))

# Clean mapping data
mapping_data_clean <- mapping_data %>%
  filter(!is.na(`Record ID`) & `Record ID` != "") %>%
  mutate(
    `Record ID` = as.character(`Record ID`),
    subject_number = as.numeric(gsub("s", "", `overkoepelend nummer`))
  ) %>%
  select(`Record ID`, subject_number) %>%
  distinct(`Record ID`, .keep_all = TRUE)


# Perform safe merge (adding subject_number)
survey_data_updated <- survey_data %>%
  left_join(mapping_data_clean, by = c("record_id" = "Record ID"))

# Final assert: row count must not change
if (nrow(survey_data_updated) != nrow(survey_data_filtered)) {
  stop(" ERROR: Number of rows changed after merging! Check your mapping file.")
} else {
  cat(" Number of rows remained exactly the same after merge.\n")
}

# Save updated data
write_csv(survey_data_updated, survey_data_path)


#----
#Create Variables needed for further analysis
#----
library(tidyverse)

# Load the updated Survey Data (with subject_number)
behavioral_data_path <- "Behavioral_Data_File_Path"
survey_data <- read_csv(survey_data_path)

# Create session_nr variable
survey_data <- survey_data %>%
  mutate(session_nr = case_when(
    redcap_event_name == "preintervention_1_arm_2" ~ 1,
    redcap_event_name == "periintervention_1_arm_2" ~ 2,
    redcap_event_name == "preintervention_2_arm_2" ~ 3,
    redcap_event_name == "periintervention_2_arm_2" ~ 4,
    TRUE ~ NA_real_  # For any other or missing values
  ))

# Check how many cases per session_nr
print(table(survey_data$session_nr, useNA = "ifany"))

# Save the updated file
write_csv(survey_data, survey_data_path)

cat(" 'session_nr' successfully created and saved to:", survey_data_path, "\n")


#Create Pre-post variable
survey_data <- survey_data %>%
  mutate(session_pre_post = case_when(
    session_nr %in% c(1, 3) ~ 1,
    session_nr %in% c(2, 4) ~ 2,
    TRUE ~ NA_real_
  ))

write_csv(survey_data, survey_data_path)


# Add intervention variable

library(tidyverse)

# Load data
survey_data <- read_csv(survey_data_path)
behavioral_data <- read_csv(behavioral_data_path)

cat(" Loaded Survey Data - Rows:", nrow(survey_data), " Columns:", ncol(survey_data), "\n")
cat(" Loaded Behavioral Data - Rows:", nrow(behavioral_data), " Columns:", ncol(behavioral_data), "\n")

#  Reduce behavioral_data to unique rows 
behavioral_unique <- behavioral_data %>%
  select(subject_nr, session_nr, intervention) %>%
  distinct()

cat(" Behavioral Data deduplicated - Rows:", nrow(behavioral_unique), "\n")

# Sanity check for accidental multiple interventions per participant per session
still_duplicated <- behavioral_unique %>%
  group_by(subject_nr, session_nr) %>%
  filter(n() > 1)

if(nrow(still_duplicated) > 0){
  cat(" ERROR: Still multiple interventions for the same subject & session:\n")
  print(still_duplicated)
  stop("Fix required before merging.")
}

# Merge safely
survey_data_merged <- survey_data %>%
  left_join(
    behavioral_unique,
    by = c("subject_number" = "subject_nr", "session_nr" = "session_nr")
  )

cat(" Merged data - Rows:", nrow(survey_data_merged), "\n")
cat("Columns after merge:", ncol(survey_data_merged), "\n")

#  peek at result
print(survey_data_merged %>% select(subject_number, session_nr, intervention) %>% head(10))

#  check of unmatched
unmatched <- survey_data_merged %>% filter(is.na(intervention))
if(nrow(unmatched) > 0){
  cat(" Warning: Records with missing intervention after merge:\n")
  print(unmatched %>% select(subject_number, session_nr))
} else {
  cat(" All records have intervention.\n")
}

# Manually set intervention for subject 4 for session 1 & 2 (was missing before)
survey_data_merged <- survey_data_merged %>%
  mutate(
    intervention = case_when(
      subject_number == 4 & session_nr %in% c(1, 2) ~ 2,
      TRUE ~ intervention
    )
  )

# If intervention known for session 1 or 2, assign to 3 & 4
intervention_12_known <- survey_data_merged %>%
  filter(session_nr %in% c(1, 2), !is.na(intervention)) %>%
  distinct(subject_number, intervention) %>%
  group_by(subject_number) %>%
  filter(n() == 1) %>%
  ungroup()

survey_data_merged <- survey_data_merged %>%
  left_join(intervention_12_known, by = "subject_number", suffix = c("", "_from12")) %>%
  mutate(
    intervention = case_when(
      is.na(intervention) & session_nr %in% c(3, 4) & intervention_from12 == 1 ~ 2,
      is.na(intervention) & session_nr %in% c(3, 4) & intervention_from12 == 2 ~ 1,
      TRUE ~ intervention
    )
  ) %>%
  select(-intervention_from12)

# If intervention known for session 3 or 4, assign to 1 & 2 ===
intervention_34_known <- survey_data_merged %>%
  filter(session_nr %in% c(3, 4), !is.na(intervention)) %>%
  distinct(subject_number, intervention) %>%
  group_by(subject_number) %>%
  filter(n() == 1) %>%
  ungroup()

survey_data_merged <- survey_data_merged %>%
  left_join(intervention_34_known, by = "subject_number", suffix = c("", "_from34")) %>%
  mutate(
    intervention = case_when(
      is.na(intervention) & session_nr %in% c(1, 2) & intervention_from34 == 1 ~ 2,
      is.na(intervention) & session_nr %in% c(1, 2) & intervention_from34 == 2 ~ 1,
      TRUE ~ intervention
    )
  ) %>%
  select(-intervention_from34)

# Final check: no NA allowed 
remaining_na <- survey_data_merged %>% filter(is.na(intervention))

if(nrow(remaining_na) > 0){
  cat(" ERROR: Still missing intervention values after all corrections:\n")
  print(remaining_na %>% select(subject_number, session_nr))
  stop("Fix required.")
} else {
  cat(" All intervention values fully assigned and no NAs remain.\n")
}

write_csv(survey_data_merged, survey_data_path)

library(dplyr)

# List of subjects
included_subjects <- c(
  108, 10, 112, 13, 140, 147, 156, 158, 15, 163, 174,
  202, 207, 208, 215, 220, 222, 22, 24, 262, 270, 298, 305, 306,
  307, 340, 344, 37, 380, 3, 417, 428, 472, 4, 510, 521, 530, 532,
  53, 54, 562, 56, 57, 590, 65, 67, 7, 81, 8, 91
)

# 
filtered_data <- survey_data_merged %>%
  filter(subject_number %in% included_subjects)

# Check how many participants remain
cat(" Number of included participants:", n_distinct(filtered_data$subject_number), "\n")

# Optionally, save it
write_csv(filtered_data, survey_data_path)

#add group_nr

library(dplyr)
library(readr)

survey_data <- read_csv(survey_data_path)
behavioral_data <- read_csv(behavioral_data_path)

# Ensure IDs are numeric
survey_data <- survey_data %>%
  mutate(subject_number = as.numeric(subject_number))

behavioral_data <- behavioral_data %>%
  mutate(subject_nr = as.numeric(subject_nr))

#  Check for duplicates in behavioral_data (must be only 1 group_nr per subject_nr)
group_check <- behavioral_data %>%
  group_by(subject_nr) %>%
  summarise(n_group = n_distinct(group_nr), .groups = "drop") %>%
  filter(n_group > 1)

if (nrow(group_check) > 0) {
  stop(" ERROR: Some participants in behavioral_data have multiple group_nr assigned!\n", print(group_check))
} else {
  cat("All subject_nr in behavioral_data have only one group_nr.\n")
}

#  Extract mapping
group_mapping <- behavioral_data %>%
  distinct(subject_nr, group_nr)

#  Add the manual correction for subject_number 4 -> error in file naming before
manual_group <- tibble(subject_nr = 4, group_nr = 1)

# Combine mapping with manual override (and remove any possible duplicates, safety)
group_mapping_combined <- bind_rows(group_mapping, manual_group) %>%
  distinct(subject_nr, .keep_all = TRUE)

# Safe merge into survey_data
survey_data_updated <- survey_data %>%
  left_join(group_mapping_combined, by = c("subject_number" = "subject_nr"))

#  Final check — no unmatched allowed
unmatched <- survey_data_updated %>%
  filter(is.na(group_nr))

if (nrow(unmatched) > 0) {
  stop(" ERROR: Some subject_number in survey_data could not be matched even after manual correction:\n", print(unmatched$subject_number))
} else {
  cat(" All survey_data participants successfully matched to group_nr (including manual case for subject 4).\n")
}

# Save updated data
write_csv(survey_data_updated, survey_data_path)

cat(" Survey Data updated with group_nr added and saved successfully.\n")
#----
#Creating averages for all questionnaires
#----

library(readr)
library(dplyr)

# Load the Survey Data
survey_data <- read_csv(survey_data_path)

#ERQ

survey_data <- survey_data %>%
  mutate(
    ERQ_Cog_Re = if_else(
      if_any(c(erq1, erq3, erq5, erq7, erq8, erq10), is.na),
      NA_real_,
      rowSums(select(., erq1, erq3, erq5, erq7, erq8, erq10), na.rm = FALSE)
    ),
    ERQ_Exp_Sup = if_else(
      if_any(c(erq2, erq4, erq6, erq9), is.na),
      NA_real_,
      rowSums(select(., erq2, erq4, erq6, erq9), na.rm = FALSE)
    )
  )


#PTQ

survey_data <- survey_data %>%
  mutate(
    PTQ_Total = if_else(
      if_any(all_of(paste0("ptq", 1:15)), is.na),
      NA_real_,
      rowSums(select(., all_of(paste0("ptq", 1:15))), na.rm = FALSE)
    )
  )


#FFMQ

ffmq_direct <- c("ffmq1", "ffmq6", "ffmq11", "ffmq15", "ffmq20", "ffmq26", "ffmq31", "ffmq36",
                 "ffmq2", "ffmq7", "ffmq27", "ffmq32", "ffmq37",
                 "ffmq4", "ffmq9", "ffmq19", "ffmq21", "ffmq24", "ffmq29", "ffmq33")

ffmq_reversed <- c("ffmq12", "ffmq16", "ffmq22", "ffmq5", "ffmq8", "ffmq13", "ffmq18", "ffmq23",
                   "ffmq28", "ffmq34", "ffmq38", "ffmq3", "ffmq10", "ffmq14", "ffmq17", "ffmq25",
                   "ffmq30", "ffmq35", "ffmq39")


survey_data <- survey_data %>%
  mutate(across(all_of(ffmq_reversed), ~ 6 - ., .names = "rev_{.col}"))


survey_data <- survey_data %>%
  mutate(
    FFMQ_Total = if_else(
      if_any(all_of(c(ffmq_direct, paste0("rev_", ffmq_reversed))), is.na),
      NA_real_,
      rowSums(select(., all_of(ffmq_direct), all_of(paste0("rev_", ffmq_reversed))), na.rm = FALSE)
    )
  )

#PANAS

panas_pos_items <- c("panas1", "panas3", "panas5", "panas9", "panas10", 
                     "panas12", "panas14", "panas16", "panas17", "panas19")

panas_neg_items <- c("panas2", "panas4", "panas6", "panas7", "panas8", 
                     "panas11", "panas13", "panas15", "panas18", "panas20")

survey_data <- survey_data %>%
  mutate(
    PANAS_Pos = if_else(
      if_any(all_of(panas_pos_items), is.na),
      NA_real_,
      rowSums(select(., all_of(panas_pos_items)), na.rm = FALSE)
    ),
    PANAS_Neg = if_else(
      if_any(all_of(panas_neg_items), is.na),
      NA_real_,
      rowSums(select(., all_of(panas_neg_items)), na.rm = FALSE)
    )
  )

#LARSS

larss_uncontrollability_items <- c("larss14", "larss16", "larss18", "larss1", "larss4", "larss20")

#  Calculate the sum 
survey_data <- survey_data %>%
  mutate(
    LARSS_Uncontrollability = if_else(
      if_any(all_of(larss_uncontrollability_items), is.na),
      NA_real_,
      rowSums(select(., all_of(larss_uncontrollability_items)), na.rm = FALSE)
    )
  )


#RPA

rpa_self_focus_items <- c("rpa3", "rpa4", "rpa5", "rpa13")
rpa_dampening_items <- c("rpa6", "rpa9", "rpa10", "rpa11", "rpa14", "rpa15", "rpa17")
rpa_emotion_focus_items <- c("rpa1", "rpa2", "rpa7", "rpa8", "rpa16")

#  Calculate the subscales 
survey_data <- survey_data %>%
  mutate(
    RPA_Self_focus = if_else(
      if_any(all_of(rpa_self_focus_items), is.na),
      NA_real_,
      rowSums(select(., all_of(rpa_self_focus_items)), na.rm = FALSE)
    ),
    RPA_Dampening = if_else(
      if_any(all_of(rpa_dampening_items), is.na),
      NA_real_,
      rowSums(select(., all_of(rpa_dampening_items)), na.rm = FALSE)
    ),
    RPA_Emotion_focus = if_else(
      if_any(all_of(rpa_emotion_focus_items), is.na),
      NA_real_,
      rowSums(select(., all_of(rpa_emotion_focus_items)), na.rm = FALSE)
    )
  )

#   quick check 
survey_data %>%
  select(subject_number, session_nr, RPA_Self_focus, RPA_Dampening, RPA_Emotion_focus) %>%
  head()

#  Save the updated data 
write_csv(survey_data, survey_data_path)

cat("Survey Data updated daa saved successfully.\n")
#----
##Descriptive Statistics per group
#----
survey_data <- read_csv(survey_data_path)
library(dplyr)

# Specifying the variables
vars <- c(
  "ERQ_Cog_Re", "ERQ_Exp_Sup", "PTQ_Total", "FFMQ_Total",
  "PANAS_Pos", "PANAS_Neg", "LARSS_Uncontrollability",
   "RPA_Dampening", "RPA_Emotion_focus"
)

# 1) By group_nr: mean, sd, and non‐missing N for each variable
summary_by_group <- survey_data %>% filter(session_nr == 1) %>%
  group_by(group_nr) %>%
  summarise(
    across(
      all_of(vars),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd   = ~ sd(.x,   na.rm = TRUE),
        n    = ~ sum(!is.na(.x))
      ),
      .names = "{.col}_{.fn}"
    )
  )

# 2) Count total observations (rows) by intervention
obs_by_intervention <- survey_data %>%
  group_by(intervention) %>%
  summarise(n_obs = n(), .groups = "drop")

# 3) Count total observations (rows) by session_pre_post
obs_by_session_pre_post <- survey_data %>%
  group_by(session_pre_post) %>%
  summarise(n_obs = n(), .groups = "drop")

# View results
summary_by_group

#----
#Baseline differences between ND & rMDD group
#----
#Mann-Whitney test for baseline comparisons
survey_data <- read_csv(survey_data_path)

survey_data$session_pre_post <- as.factor(survey_data$session_pre_post)
survey_data$intervention <- as.factor(survey_data$intervention)
survey_data$group_nr <- as.factor(survey_data$group_nr)

library(dplyr)
library(BayesFactor)

s1 <- survey_data %>% 
  filter(session_nr == 1) %>%
  mutate(group_nr = as.numeric(as.character(group_nr)))

# Duplicate check 
dup_check <- s1 %>% count(subject_number) %>% filter(n > 1)
if (nrow(dup_check) > 0) {
  warning("Duplicate subjects: ", paste(dup_check$subject_number, collapse = ", "))
}

vars <- c(
  "ERQ_Cog_Re", "ERQ_Exp_Sup", "PTQ_Total", "FFMQ_Total",
  "PANAS_Pos", "PANAS_Neg", "LARSS_Uncontrollability",
  "RPA_Dampening", "RPA_Emotion_focus"
)
  
run_mwu <- function(v, data) {
  x_rMDD <- data %>% filter(group_nr == 2, !is.na(.data[[v]])) %>% pull(.data[[v]])
  x_ND   <- data %>% filter(group_nr == 1, !is.na(.data[[v]])) %>% pull(.data[[v]])
  
  n_rMDD <- length(x_rMDD)
  n_ND   <- length(x_ND)
  
  mw <- wilcox.test(x_rMDD, x_ND, exact = FALSE, alternative = "two.sided")
  U  <- as.numeric(mw$statistic)
  
  all_vals <- c(x_rMDD, x_ND)
  all_grps <- factor(c(rep("rMDD", n_rMDD), rep("ND", n_ND)))
  rnks     <- rank(all_vals)
  bf_obj   <- ttestBF(x = rnks[all_grps == "rMDD"],
                      y = rnks[all_grps == "ND"])
  BF10 <- exp(as.numeric(bf_obj@bayesFactor$bf))
  
  tibble(
    Variable    = v,
    n_rMDD      = n_rMDD,
    n_ND        = n_ND,
    Median_rMDD = median(x_rMDD),
    IQR_rMDD    = IQR(x_rMDD),
    Median_ND   = median(x_ND),
    IQR_ND      = IQR(x_ND),
    U           = round(U, 1),
    p_raw       = mw$p.value,
    BF10        = BF10
  )
}

results <- bind_rows(lapply(vars, run_mwu, data = s1))

results <- results %>%
  mutate(
    p_fdr = p.adjust(p_raw, method = "fdr"),
    BF_interpretation = case_when(
      BF10 >= 100  ~ "extreme evidence for H1",
      BF10 >= 30   ~ "very strong evidence for H1",
      BF10 >= 10   ~ "strong evidence for H1",
      BF10 >= 3    ~ "moderate evidence for H1",
      BF10 >= 1    ~ "anecdotal evidence for H1",
      BF10 >= 1/3  ~ "anecdotal evidence for H0",
      BF10 >= 1/10 ~ "moderate evidence for H0",
      TRUE         ~ "strong or greater evidence for H0"
    )
  )

print(results %>%
        select(Variable, n_rMDD, n_ND,
               Median_rMDD, IQR_rMDD, Median_ND, IQR_ND,
               U, p_raw, p_fdr, BF10, BF_interpretation) %>%
        mutate(U = format(U, nsmall = 1)),
      n = Inf)

#----
#RQ1
#----

#model comparison via AIC was done manually as descriped in the manuscript with terms only remaining in the model if its removal leads to ΔAIC increase of 2

survey_data <- read_csv(survey_data_path)

survey_data$session_pre_post <- as.factor(survey_data$session_pre_post)
survey_data$intervention <- as.factor(survey_data$intervention)
survey_data$group_nr <- as.factor(survey_data$group_nr)

#ERQ Cognitive Reappraisal
model_ERQ_Cog_Re2 <- lmer(ERQ_Cog_Re ~ 
                            #session_pre_post +
                            #intervention +
                            #session_pre_post * intervention + 
                            (1 | subject_number), data = survey_data,REML = FALSE)

AIC(model_ERQ_Cog_Re2)
anova(model_ERQ_Cog_Re2)

#ERQ Expressive Suppression
model_ERQ_Exp_Sup2 <- lmer(ERQ_Exp_Sup ~ 
                             #session_pre_post +
                             #intervention +
                             #session_pre_post : intervention + 
                             (1 | subject_number), data = survey_data, REML = FALSE)


AIC(model_ERQ_Exp_Sup2)
anova(model_ERQ_Exp_Sup2)

#PTQ
model_PTQ_Total2 <- lmer(PTQ_Total ~ 
                           session_pre_post +
                           #intervention +
                           #session_pre_post : intervention + 
                           (1 | subject_number), data = survey_data, REML = FALSE)

AIC(model_PTQ_Total2)
anova(model_PTQ_Total2)


emm_session <- emmeans(model_PTQ_Total2, ~ session_pre_post)
c1 <-pairs(emm_session, adjust = "none")

#FFMQ
model_FFMQ_Total2 <- lmer(FFMQ_Total ~ 
                            session_pre_post +
                            intervention +
                            #session_pre_post : intervention + 
                            (1 | subject_number), data = survey_data, REML = FALSE)
AIC(model_FFMQ_Total2)
anova(model_FFMQ_Total2)



emm_session <- emmeans(model_FFMQ_Total2, ~ session_pre_post)
c2 <- pairs(emm_session, adjust = "none")

#PANAS Positive Subscale
model_PANAS_Pos2 <- lmer(PANAS_Pos ~ 
                           #session_pre_post +
                           #intervention +
                           #session_pre_post * intervention + 
                           (1 | subject_number), data = survey_data, REML = FALSE)
AIC(model_PANAS_Pos2)
anova(model_PANAS_Pos2)

#PANAS Negative Subscale
model_PANAS_Neg2 <- lmer(PANAS_Neg ~ 
                           #session_pre_post +
                           #intervention +
                           #session_pre_post * intervention + 
                           (1 | subject_number), data = survey_data, REML = FALSE)

AIC(model_PANAS_Neg2)
anova(model_PANAS_Neg2)

#LARSS Uncontrollabilty subscale
model_LARSS_Uncontrollability2 <- lmer(LARSS_Uncontrollability ~    
                                         #session_pre_post +
                                         #intervention +
                                         #session_pre_post * intervention + 
                                         (1 | subject_number), data = survey_data, REML = FALSE)
AIC(model_LARSS_Uncontrollability2)
anova(model_LARSS_Uncontrollability2)


# RPA Dampening Subscale
model_RPA_Dampening2 <- lmer(RPA_Dampening ~ 
                               session_pre_post +
                               #intervention +
                               #session_pre_post * intervention + 
                               (1 | subject_number), data = survey_data, REML = FALSE)

AIC(model_RPA_Dampening2)
anova(model_RPA_Dampening2)

emm_session <- emmeans(model_RPA_Dampening2, ~ session_pre_post)
c3 <- pairs(emm_session, adjust = "none")

#RPA EMOTION FOCUS
model_RPA_Emotion_focus2 <- lmer(RPA_Emotion_focus ~ 
                                   session_pre_post +
                                   #intervention +
                                   #session_pre_post * intervention + 
                                   (1 | subject_number), data = survey_data, REML = FALSE)

AIC(model_RPA_Emotion_focus2)
anova(model_RPA_Emotion_focus2)


emm_session <- emmeans(model_RPA_Emotion_focus2, ~ session_pre_post)
c4 <- pairs(emm_session, adjust = "none")


#FDR correction of contrasts
contr_list <- list(
  c1 = c1, c2 = c2, c3 = c3, c4 = c4
)

all_contrasts <- bind_rows(lapply(names(contr_list), function(nm) {
  as.data.frame(contr_list[[nm]]) %>%
    mutate(contrast_set = nm)
}))


all_contrasts <- all_contrasts %>%
  mutate(p_fdr = p.adjust(p.value, method = "BH"))

library(dplyr)

all_contrasts <- all_contrasts %>%
  mutate(
    p_value_4dp = formatC(p.value, format = "f", digits = 4),
    p_fdr_4dp   = formatC(p_fdr,   format = "f", digits = 4)
  )


all_contrasts


#----
#RQ 2 
#----

library(lme4)
library(lmerTest)
library(dplyr)
library(readr)

survey_data <- read_csv(survey_data_path)

survey_data$session_pre_post <- as.factor(survey_data$session_pre_post)
survey_data$intervention <- as.factor(survey_data$intervention)
survey_data$group_nr <- as.factor(survey_data$group_nr)

#ERQ Cognitive Reappraisal
model_ERQ_Cog_Re2 <- lmer(ERQ_Cog_Re ~ 
                            #session_pre_post +
                            #intervention +
                            group_nr +
                            #session_pre_post * group_nr +
                            #intervention : group_nr+
                            #session_pre_post * intervention + 
                            #session_pre_post * intervention * group_nr +
                            (1 | subject_number), data = survey_data,REML = FALSE)

AIC(model_ERQ_Cog_Re2)
anova(model_ERQ_Cog_Re2)

#ERQ Expressive Suppression
model_ERQ_Exp_Sup2 <- lmer(ERQ_Exp_Sup ~ 
                             #session_pre_post +
                             #intervention +
                             group_nr +
                             #session_pre_post : group_nr +
                             #intervention :  group_nr+
                             #session_pre_post : intervention + 
                             #session_pre_post : intervention : group_nr +
                             (1 | subject_number), data = survey_data, REML = FALSE)


AIC(model_ERQ_Exp_Sup2)
anova(model_ERQ_Exp_Sup2)

#PTQ
model_PTQ_Total2 <- lmer(PTQ_Total ~ 
                           session_pre_post +
                           #intervention +
                           group_nr +
                           #session_pre_post : group_nr +
                           #intervention : group_nr+
                           #session_pre_post : intervention + 
                           #session_pre_post : intervention : group_nr +
                           (1 | subject_number), data = survey_data, REML = FALSE)

AIC(model_PTQ_Total2)
anova(model_PTQ_Total2)


emm_session <- emmeans(model_PTQ_Total2, ~ session_pre_post)
c1 <-pairs(emm_session, adjust = "none")

#FFMQ
model_FFMQ_Total2 <- lmer(FFMQ_Total ~ 
                            session_pre_post +
                            intervention +
                            group_nr +
                            #session_pre_post : group_nr +
                            #intervention : group_nr+
                            #session_pre_post : intervention + 
                            #session_pre_post : intervention : group_nr +
                            (1 | subject_number), data = survey_data, REML = FALSE)
AIC(model_FFMQ_Total2)
anova(model_FFMQ_Total2)


emm_session <- emmeans(model_FFMQ_Total2, ~ session_pre_post)
c2 <- pairs(emm_session, adjust = "none")


#PANAS Positive Subscale
model_PANAS_Pos2 <- lmer(PANAS_Pos ~ 
                           #session_pre_post +
                           #intervention +
                           group_nr +
                           #session_pre_post * group_nr +
                           #intervention * group_nr+
                           #session_pre_post * intervention + 
                           #session_pre_post * intervention * group_nr +
                           (1 | subject_number), data = survey_data, REML = FALSE)
AIC(model_PANAS_Pos2)
anova(model_PANAS_Pos2)

#PANAS Negative Subscale
model_PANAS_Neg2 <- lmer(PANAS_Neg ~ 
                           #session_pre_post +
                           #intervention +
                           group_nr +
                           #session_pre_post * group_nr +
                           #intervention * group_nr+
                           #session_pre_post * intervention + 
                           #session_pre_post * intervention * group_nr +
                           (1 | subject_number), data = survey_data, REML = FALSE)

AIC(model_PANAS_Neg2)
anova(model_PANAS_Neg2)

#LARSS Uncontrollabilty subscale
model_LARSS_Uncontrollability2 <- lmer(LARSS_Uncontrollability ~    
                                         #session_pre_post +
                                         intervention +
                                         group_nr +
                                         #session_pre_post * group_nr +
                                         intervention * group_nr+
                                         #session_pre_post * intervention + 
                                         #session_pre_post * intervention * group_nr +
                                         (1 | subject_number), data = survey_data, REML = FALSE)
AIC(model_LARSS_Uncontrollability2)
anova(model_LARSS_Uncontrollability2)


# RPA Dampening Subscale
model_RPA_Dampening2 <- lmer(RPA_Dampening ~ 
                               session_pre_post +
                               #intervention +
                               #group_nr +
                               #session_pre_post * group_nr +
                               #intervention * group_nr+
                               #session_pre_post * intervention + 
                               #session_pre_post * intervention * group_nr +
                               (1 | subject_number), data = survey_data, REML = FALSE)

AIC(model_RPA_Dampening2)
anova(model_RPA_Dampening2)

emm_group <- emmeans(model_RPA_Dampening2, ~ session_pre_post)
c3 <- pairs(emm_group, adjust = "none")

#RPA EMOTION FOCUS
model_RPA_Emotion_focus2 <- lmer(RPA_Emotion_focus ~ 
                                   session_pre_post +
                                   intervention +
                                   group_nr +
                                   #session_pre_post * group_nr +
                                   intervention * group_nr+
                                   #session_pre_post * intervention + 
                                   #session_pre_post * intervention * group_nr +
                                   (1 | subject_number), data = survey_data, REML = FALSE)

AIC(model_RPA_Emotion_focus2)
anova(model_RPA_Emotion_focus2)


emm_session <- emmeans(model_RPA_Emotion_focus2, ~ session_pre_post)
c4 <- pairs(emm_session, adjust = "none")


#FDR correction of contrasts
contr_list <- list(
  c1 = c1, c2 = c2,  c3 = c3, c4 = c4
)

all_contrasts <- bind_rows(lapply(names(contr_list), function(nm) {
  as.data.frame(contr_list[[nm]]) %>%
    mutate(contrast_set = nm)
}))


all_contrasts <- all_contrasts %>%
  mutate(p_fdr = p.adjust(p.value, method = "BH"))

library(dplyr)

all_contrasts <- all_contrasts %>%
  mutate(
    p_value_4dp = formatC(p.value, format = "f", digits = 4),
    p_fdr_4dp   = formatC(p_fdr,   format = "f", digits = 4)
  )


all_contrasts



library(lme4)
library(lmerTest)
library(dplyr)
library(readr)


#----
#Assumption Checks
#----
check_lmm_assumptions <- function(model, model_name = "") {
  
  cat("\n=============================\n")
  cat("Model:", model_name, "\n")
  cat("=============================\n")
  
  par(mfrow = c(1, 2))
  
  # Residuals vs fitted
  plot(
    fitted(model),
    resid(model),
    xlab = "Fitted values",
    ylab = "Residuals",
    main = "Residuals vs Fitted"
  )
  abline(h = 0, lty = 2)
  
  # Q-Q plot
  qqnorm(resid(model), main = "Normal Q-Q")
  qqline(resid(model))
  
  par(mfrow = c(1, 1))
}

check_lmm_assumptions(model_PTQ_Total2, "PTQ Total")
check_lmm_assumptions(model_ERQ_Cog_Re2, "model_ERQ_Cog_Re2")
check_lmm_assumptions(model_ERQ_Exp_Sup2, "model_ERQ_Exp_Sup2")
check_lmm_assumptions(model_FFMQ_Total2, "model_FFMQ_Total2")
check_lmm_assumptions(model_PANAS_Pos2, "model_PANAS_Pos2")
check_lmm_assumptions(model_PANAS_Neg2, "model_PANAS_Neg2")
check_lmm_assumptions(model_LARSS_Uncontrollability2, "model_LARSS_Uncontrollability2")
check_lmm_assumptions(model_RPA_Dampening2, "model_RPA_Dampening2")
check_lmm_assumptions(model_RPA_Emotion_focus2, "model_RPA_Emotion_focus2")
