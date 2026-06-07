#CREATES ONE BIG DATAFILE WITH ALL NECESSARY DATA
#----

install.packages("readxl")
library(dplyr)

# Set input and output folders
input_folder <- "Input Folder" # Folder containing all Task csv files
output_file <- "Output_file_path"

# Get list of all raw .csv files
file_list <- list.files(path = input_folder, pattern = "\\.csv$", full.names = TRUE)

all_trials <- list()

for (file_path in file_list) {
  
  # Read file
  data <- read.csv(file_path, header = TRUE, stringsAsFactors = FALSE)
  
  # Extract session number from filename
  file_name <- basename(file_path)
  session_nr <- as.numeric(sub(".*_m([0-9]+).*", "\\1", file_name))
  
  # Define condition from pptrigger_value (3 = neutral, 2 = negative, 4 = positive)
  data$condition <- ifelse(data$pptrigger_value == 11, 2,
                           ifelse(data$pptrigger_value == 12, 3,
                                  ifelse(data$pptrigger_value == 13, 4, NA)))
  
  # Define group number (1 = HC, 2 = rMDD)
  data$group_nr <- ifelse(data$group == "HC", 1,
                          ifelse(data$group == "rMDD", 2, NA))
  
  
  trial_data <- data %>%
    filter(condition %in% c(2, 3, 4)) %>%
    mutate(
      subject_nr = as.character(subject_nr),
      session_nr = session_nr,
      valence_rating = slider_percent1,
      attending_rating = slider_percent2
    ) %>%
    select(subject_nr, session_nr, group_nr, condition, valence_rating, attending_rating)
  
  all_trials[[length(all_trials) + 1]] <- trial_data
}

# Combine all files
combined_data <- bind_rows(all_trials)

# Save final CSV
write.csv(combined_data, output_file, row.names = FALSE)

# Optional: preview first few rows
head(combined_data)

#----
#ADDS INTERVENTION ORDER TO DATAFRAME
#----
install.packages("readxl")

library(dplyr)
library(readxl)

# Load behavioral data
behavior_data <- read.csv(output_file, 
                          stringsAsFactors = FALSE)

# Load intervention order from Excel
intervention_data <- read_excel("Intervention_Order.xlsx") #FIle containing the intervention order

# Ensure consistent column types for merging
behavior_data$subject_nr <- as.character(behavior_data$subject_nr)
behavior_data$session_nr <- as.numeric(behavior_data$session_nr)
intervention_data$subject_nr <- as.character(intervention_data$subject_nr)
intervention_data$session <- as.numeric(intervention_data$session)

# Merge: add intervention into behavioral data
behavior_data <- behavior_data %>%
  left_join(intervention_data[, c("subject_nr", "session", "intervention")],
            by = c("subject_nr" = "subject_nr", "session_nr" = "session"))

# add session_pre_post variable
behavior_data$session_pre_post <- ifelse(behavior_data$session_nr %in% c(1, 3), 1,
                                         ifelse(behavior_data$session_nr %in% c(2, 4), 2, NA))

# Remove rows without intervention info (not present in intervention file)
behavior_data <- behavior_data %>% filter(!is.na(intervention))

# Save the updated behavioral data 
write.csv(behavior_data, output_file, 
          row.names = FALSE) 

#correct wrong group assignment in data

# Load the behavioral data
behavior_data <- read.csv(output_file,
                          stringsAsFactors = FALSE)
# Make the corrections
behavior_data <- behavior_data %>%
  mutate(group_nr = case_when(
    subject_nr == 147 & session_nr == 1 ~ 2,
    subject_nr == 207 & session_nr == 2 ~ 2,
    subject_nr == 262 & session_nr == 4 ~ 2,
    subject_nr == 380 & session_nr == 2 ~ 2,
    TRUE ~ group_nr  # Keep the original value otherwise
  ))

# check if the corrections applied correctly
behavior_data %>%
  filter(subject_nr %in% c(147, 207, 262, 380), session_nr %in% c(1, 2, 4)) %>%
  select(subject_nr, session_nr, group_nr)

write_csv(behavior_data, output_file)
# Load the behavioral data
behavior_data <- read.csv(output_file,
                          stringsAsFactors = FALSE)

# Add numeric session_pre_post variable
behavior_data <- behavior_data %>%
  mutate(session_pre_post = case_when(
    session_nr %in% c(1, 3) ~ 1,
    session_nr %in% c(2, 4) ~ 2,
    TRUE ~ NA_real_
  ))

# Save the updated file 
write.csv(behavior_data, output_file,
          row.names = FALSE)


#----
#Model Testing RQ 1
#----
install.packages("readr")
install.packages("dplyr")
library(lme4)
library(lmerTest)
library(dplyr)
library("readr")

data <- read_csv(output_file)

data$condition <- as.factor(data$condition)
data$condition <- relevel(data$condition, ref = "3")
data$session_pre_post <- as.factor(data$session_pre_post)
data$group_nr <- as.factor(data$group_nr)
data$intervention <- as.factor(data$intervention)



model_valence2 <- lmer(
  valence_rating ~ 
    session_pre_post + 
    #intervention + 
    condition + 
    #session_pre_post:intervention + 
    session_pre_post:condition + 
    #intervention:condition + 
    #session_pre_post:intervention:condition + 
    (1 | subject_nr), 
  data = data, REML = FALSE
)


AIC(model_valence2)
anova(model_valence2)
summary(model_valence2)
emm_condition <- emmeans(model_valence2, ~ condition)
c1 <- pairs(emm_condition, adjust = "none")
emm_session_condition <- emmeans(model_valence2, ~ session_pre_post | condition)
c2 <- contrast(emm_session_condition, method = "pairwise", adjust = "none")


#Attending Model 

data <- read_csv(output_file)

data$condition <- as.factor(data$condition)
data$condition <- relevel(data$condition, ref = "3")
data$session_pre_post <- as.factor(data$session_pre_post)
data$group_nr <- as.factor(data$group_nr)
data$intervention <- as.factor(data$intervention)

library(readr)
library(dplyr)
library(lme4)


model_attending2 <- lmer(
  attending_rating ~ 
    session_pre_post + 
    intervention + 
    condition + 
    session_pre_post:intervention + 
    #session_pre_post:condition + 
    #intervention:condition + 
    #session_pre_post:intervention:condition + 
    
    (1 | subject_nr), 
  data = data, REML = FALSE
)


AIC(model_attending2)
anova(model_attending2)

emm_session_pre_post <- emmeans(model_attending2, ~ session_pre_post)
c3 <- pairs(emm_session_pre_post, adjust = "none")
emm_condition <- emmeans(model_attending2, ~ condition)
c4 <- pairs(emm_condition, adjust = "none")
emm_session_by_intervention <- emmeans(model_attending2, ~ session_pre_post| intervention)
c5 <- contrast(emm_session_by_intervention, method = "pairwise", adjust = "none")



#FDR COrrection RQ1
library(dplyr)

contr_list <- list(
  c1 = c1, c2 = c2, c3 = c3,
  c4 = c4, c5 = c5
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


install.packages("readr")
install.packages("dplyr")
library(lme4)
library(lmerTest)
library(dplyr)
library("readr")

data <- read_csv(output_file)

data$condition <- as.factor(data$condition)
data$condition <- relevel(data$condition, ref = "3")
data$session_pre_post <- as.factor(data$session_pre_post)
data$group_nr <- as.factor(data$group_nr)
data$intervention <- as.factor(data$intervention)

model_valence2 <- lmer(
  valence_rating ~ 
    session_pre_post + 
    intervention + 
    condition + 
    group_nr + 
    session_pre_post:intervention + 
    session_pre_post:condition + 
    session_pre_post:group_nr + 
    #intervention:condition + 
    intervention:group_nr + 
    condition:group_nr + 
    #session_pre_post:intervention:condition + 
    session_pre_post*intervention*group_nr + 
    #session_pre_post:condition:group_nr + 
    #intervention:condition:group_nr + 
    #session_pre_post*intervention*condition*group_nr + 
    (1 | subject_nr), 
  data = data, REML = FALSE
)


AIC(model_valence2)
anova(model_valence2)
summary(model_valence2)
emm_condition <- emmeans(model_valence2, ~ condition)
c1 <- pairs(emm_condition, adjust = "none")
emm_session_condition <- emmeans(model_valence2, ~ session_pre_post | condition)
c2 <- contrast(emm_session_condition, method = "pairwise", adjust = "none")
emm_cond_group <- emmeans(model_valence2, ~ group_nr | condition)
c3 <- contrast(emm_cond_group, method = "pairwise", adjust = "none")
emm_int_cond_group <- emmeans(model_valence2, ~ session_pre_post|  intervention * group_nr )
c4 <- contrast(emm_int_cond_group, method = "pairwise", adjust = "none")
emm_inter_group <- emmeans(model_valence2, ~  intervention |  group_nr)
c5 <- contrast(emm_inter_group, method = "pairwise", adjust = "none")

#Attending Model 



data <- read_csv(output_file)

data$condition <- as.factor(data$condition)
data$condition <- relevel(data$condition, ref = "3")
data$session_pre_post <- as.factor(data$session_pre_post)
data$group_nr <- as.factor(data$group_nr)
data$intervention <- as.factor(data$intervention)



model_attending2 <- lmer(
  attending_rating ~ 
    session_pre_post + 
    intervention + 
    condition + 
    group_nr + 
    session_pre_post:intervention + 
    #session_pre_post:condition + 
    session_pre_post:group_nr + 
    intervention:condition + 
    intervention:group_nr + 
    condition:group_nr + 
    #session_pre_post:intervention:condition + 
    session_pre_post*intervention*group_nr + 
    #session_pre_post:condition:group_nr + 
    #intervention:condition:group_nr + 
    #session_pre_post*intervention*condition*group_nr + 
    (1 | subject_nr), 
  data = data, REML = FALSE
)


AIC(model_attending2)
anova(model_attending2)

emm_session_pre_post <- emmeans(model_attending2, ~ session_pre_post)
c6 <- pairs(emm_session_pre_post, adjust = "none")
emm_condition <- emmeans(model_attending2, ~ condition)
c7 <- pairs(emm_condition, adjust = "none")
emm_session_by_intervention <- emmeans(model_attending2, ~ session_pre_post| intervention)
c8 <- contrast(emm_session_by_intervention, method = "pairwise", adjust = "none")
emm_int_cond_group <- emmeans(model_attending2, ~ session_pre_post|  intervention * group_nr )
c9 <- contrast(emm_int_cond_group, method = "pairwise", adjust = "none")


#FDR COrrection RQ2
library(dplyr)

contr_list <- list(
  c1 = c1, c2 = c2, c3 = c3, c4 = c4, c5 = c5 ,c6 = c6,
  c7 = c7, c8 = c8, c9 = c9
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
#---- Assumption testing
#----
check_lmm_assumptions <- function(model, model_name = "") {
  
  cat("\n=============================\n")
  cat("Model:", model_name, "\n")
  cat("=============================\n")
  
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar), add = TRUE)
  
  par(mfrow = c(1, 2), mar = c(4, 4, 3, 1) + 0.1)
  
  # Residuals vs fitted
  plot(
    fitted(model),
    resid(model),
    xlab = "Fitted values",
    ylab = "Residuals",
    main = "Residuals vs Fitted",
    pch = 16, cex = 0.6
  )
  abline(h = 0, lty = 2)
  
  # Q–Q plot
  qqnorm(resid(model), main = "Normal Q–Q", pch = 16, cex = 0.6)
  qqline(resid(model))
  
  invisible(TRUE)
}

check_lmm_assumptions(model_valence2,  "ERT – Valence ratings")
check_lmm_assumptions(model_attending2, "ERT – Attending ratings")