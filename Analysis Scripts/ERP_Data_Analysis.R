#create full copy of dataframe with all variables of interest
#----
library(tidyverse)
library(readxl)
library(readr)
library(stringr)

file_path <- "ERP File Path" # CSV File that is produced by the Preprocessing Pipeline
output_folder <- "output folder"
output_file <- "ERP_Data_full.csv"
output_path <- file.path(output_folder, output_file)

# Read all columns as text
ERP_data <- read_csv(file_path)

head(ERP_data)

ERP_data <- ERP_data %>%
  mutate(
    ERP = case_when(
      Window == "Early_LPP" ~ 1L,
      Window == "Mid_LPP"   ~ 2L,
      Window == "Late_LPP"  ~ 3L,
      Window == "P3"        ~ 4L,
      TRUE ~ NA_integer_
    )
  )

# Select columns of interest
selected_data <- ERP_data %>%
  select('ERPset',
         'ERP',
         'binlabel',
         'bini',
         'chlabel',
         'value',
         'chindex'
  )

head(selected_data)

# Extract session number 
selected_data <- selected_data %>%
  mutate(
    session_nr = as.numeric(str_extract(ERPset, "_m([1-4])") %>% str_remove_all("_m")),
    session_pre_post = case_when(
      session_nr %in% c(1, 3) ~ 1,
      session_nr %in% c(2, 4) ~ 2,
      TRUE ~ NA_real_
    )
  )

write.csv2(selected_data, output_path, row.names = FALSE)

#----
# Create relevant variables for analysis
#----
file_path <- "ERP Data CSV File"

data<- read_csv2(file_path)

#condition variable
data <- data %>% rename(condition = bini)
data <- data %>% rename(ERP_amplitude = value)


#group_nr variable 
data <- data %>%
  mutate(
    group_nr = as.numeric(str_extract(ERPset, "_g([1-2])") %>% str_remove_all("_g")),
  )

#subject_nr variable
data <- data %>%
  mutate(
    subject_number = as.numeric(str_extract(ERPset, "(?<=ERT_s)\\d+"))
  )

write.csv2(data, output_path, row.names = FALSE)

#intervention from the survey data file
library(dplyr)
library(readr)

# Load the survey data that contains subject_nr, session_nr, and intervention
survey_data_path <- "Survey_Data_Path"
survey_data <- read_csv(survey_data_path)

# Check that each subject/session has exactly one intervention
interv_check <- survey_data %>%
  distinct(subject_number, session_nr, intervention) %>%
  group_by(subject_number, session_nr) %>%
  summarize(n_int = n(), .groups = "drop") %>%
  filter(n_int > 1)
if (nrow(interv_check) > 0) {
  stop("ERROR: Multiple interventions found for subject/session:\n",
       paste0(interv_check$subject_number, "/", interv_check$session_nr, collapse = "; "))
}

# Build clean mapping table of subject/session to intervention
interv_map <- survey_data %>%
  distinct(subject_number, session_nr, intervention)

# Merge intervention into  ERP data 
erp_df2 <- data %>%
  left_join(interv_map, by = c("subject_number", "session_nr"))

#  Check for any unmatched rows
unmatched <- erp_df2 %>% filter(is.na(intervention))
if (nrow(unmatched) > 0) {
  stop("ERROR: Missing intervention for these subject/session combinations:\n",
       paste0(unmatched$subject_nr, "/", unmatched$session_nr, collapse = "; "))
}


# overwrite 
data <- erp_df2

write.csv2(data, output_path, row.names = FALSE)
#----
#RQ1
#----
library(lme4)
library(lmerTest)
library(dplyr)
library(readr)



file_path <- "output path"

#Early LPP
data <- read_csv2(file_path)
data <- data %>% filter(ERP==1)
data <- data %>% filter(subject_number != "24") # was mistakenly not removed after epoch rejection as they did not reatin enough epochs, thus filtered out manually from each analysis

# Convert the correct variables to factors
data$condition <- as.factor(data$condition)
data$condition <- relevel(data$condition, ref = "3")
data$session_pre_post <- as.factor(data$session_pre_post)
data$group_nr <- as.factor(data$group_nr)
data$intervention <- as.factor(data$intervention)
data <- data %>%
  mutate(ERP_amplitude = as.numeric(ERP_amplitude))


model_early_LPP2 <- lmer(
  ERP_amplitude ~ 
    session_pre_post + 
    intervention + 
    condition + 
    session_pre_post *intervention + 
    #session_pre_post:condition + 
    #intervention:condition + 
    #session_pre_post:intervention:condition + 
    (1 | subject_number), 
  data = data, REML = FALSE
)


AIC(model_early_LPP2)
anova(model_early_LPP2)

emm_session_pre_post <- emmeans(model_early_LPP2, ~ session_pre_post)
c1 <- pairs(emm_session_pre_post, adjust = "none")
emm_condition <- emmeans(model_early_LPP2, ~ condition)
c2 <- pairs(emm_condition, adjust = "none")
emm_session_by_intervention <- emmeans(model_early_LPP2, ~ session_pre_post| intervention)
c3 <- contrast(emm_session_by_intervention, method = "pairwise",adjust = "none")

#Mid LPP
data <- read_csv2(file_path)
data <- data %>% filter(ERP==2)
data <- data %>% filter(subject_number != "24")
# Convert the correct variables to factors
data$condition <- as.factor(data$condition)
data$condition <- relevel(data$condition, ref = "3")
data$session_pre_post <- as.factor(data$session_pre_post)
data$group_nr <- as.factor(data$group_nr)
data$intervention <- as.factor(data$intervention)
data <- data %>%
  mutate(ERP_amplitude = as.numeric(ERP_amplitude))


model_mid_LPP2 <- lmer(
  ERP_amplitude ~ 
    session_pre_post + 
    intervention + 
    condition + 
    session_pre_post*intervention + 
    #session_pre_post:condition + 
    #intervention:condition + 
    #session_pre_post:intervention:condition + 
    
    (1 | subject_number), 
  data = data, REML = FALSE
)


AIC(model_mid_LPP2)
anova(model_mid_LPP2)

emm_session_pre_post <- emmeans(model_mid_LPP2, ~ session_pre_post)
c4 <- pairs(emm_session_pre_post, adjust = "none")
emm_condition <- emmeans(model_mid_LPP2, ~ condition)
c5 <- pairs(emm_condition,adjust = "none")
emm_session_by_intervention <- emmeans(model_mid_LPP2, ~ session_pre_post| intervention)
c6 <- contrast(emm_session_by_intervention, method = "pairwise",adjust = "none")

#Late LPP
data <- read_csv2(file_path)
data <- data %>% filter(ERP==3)
data <- data %>% filter(subject_number != "24")
# Convert the correct variables to factors
data$condition <- as.factor(data$condition)
data$condition <- relevel(data$condition, ref = "3")
data$session_pre_post <- as.factor(data$session_pre_post)
data$group_nr <- as.factor(data$group_nr)
data$intervention <- as.factor(data$intervention)
data <- data %>%
  mutate(ERP_amplitude = as.numeric(ERP_amplitude))


model_late_LPP2 <- lmer(
  ERP_amplitude ~ 
    #session_pre_post + 
    #intervention + 
    condition + 
    #session_pre_post:intervention + 
    #session_pre_post:condition + 
    #intervention:condition + 
    #session_pre_post:intervention:condition + 
    
    (1 | subject_number), 
  data = data, REML = FALSE
)

AIC(model_late_LPP2)
anova(model_late_LPP2)

emm_condition <- emmeans(model_late_LPP2, ~ condition)
c7 <- pairs(emm_condition, adjust = "none")


data <- read_csv2(file_path)
data <- data %>% filter(ERP==4)
data <- data %>% filter(subject_number != "24")
# Convert the correct variables to factors
data$condition <- as.factor(data$condition)
data$condition <- relevel(data$condition, ref = "3")
data$session_pre_post <- as.factor(data$session_pre_post)
data$group_nr <- as.factor(data$group_nr)
data$intervention <- as.factor(data$intervention)
data <- data %>%
  mutate(ERP_amplitude = as.numeric(ERP_amplitude))


model_P3_2 <- lmer(
  ERP_amplitude ~ 
    session_pre_post + 
    #intervention + 
    #condition + 
    #session_pre_post:intervention + 
    #session_pre_post:condition + 
    #intervention:condition + 
    #session_pre_post:intervention:condition + 
    (1 | subject_number), 
  data = data, REML = FALSE
)


AIC(model_P3_2)
anova(model_P3_2)

emm_session_pre_post <- emmeans(model_P3_2, ~ session_pre_post)
c8 <- pairs(emm_session_pre_post, adjust = "none")

#FDR correction RQ1
library(dplyr)

contr_list <- list(
  c1 = c1, c2 = c2, c3 = c3, c4 = c4, c5 = c5, c6 = c6,c7 = c7,
  c8 = c8
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
#RQ2
#----

file_path <- output_file

#Early LPP
data <- read_csv2(file_path)
data <- data %>% filter(ERP==1)
data <- data %>% filter(subject_number != "24") # was mistakenly not removed after epoch rejection thus filtered out manually from each analysis
# Convert the correct variables to factors
data$condition <- as.factor(data$condition)
data$condition <- relevel(data$condition, ref = "3")
data$session_pre_post <- as.factor(data$session_pre_post)
data$group_nr <- as.factor(data$group_nr)
data$intervention <- as.factor(data$intervention)
data <- data %>%
  mutate(ERP_amplitude = as.numeric(ERP_amplitude))

model_early_LPP2 <- lmer(
  ERP_amplitude ~ 
    session_pre_post + 
    intervention + 
    condition + 
    group_nr + 
    session_pre_post *intervention + 
    #session_pre_post:condition + 
    #session_pre_post:group_nr + 
    #intervention:condition + 
    #intervention:group_nr + 
    condition*group_nr + 
    #session_pre_post:intervention:condition + 
    #session_pre_post:intervention:group_nr + 
    #session_pre_post:condition:group_nr + 
    #intervention:condition:group_nr + 
    #session_pre_post:intervention:condition:group_nr + 
    (1 | subject_number), 
  data = data, REML = FALSE
)

AIC(model_early_LPP2)
anova(model_early_LPP2)


emm_session_pre_post <- emmeans(model_early_LPP2, ~ session_pre_post)
c1 <- pairs(emm_session_pre_post, adjust = "none")
emm_condition <- emmeans(model_early_LPP2, ~ condition)
c2 <- pairs(emm_condition, adjust = "none")
emm_condition_by_group <- emmeans(model_early_LPP2, ~  group_nr | condition )
c3 <- contrast(emm_condition_by_group, method = "pairwise",adjust = "none")
emm_session_by_intervention <- emmeans(model_early_LPP2, ~ session_pre_post| intervention)
c4 <- contrast(emm_session_by_intervention, method = "pairwise",adjust = "none")


#Mid LPP
data <- read_csv2(file_path)
data <- data %>% filter(ERP==2)
data <- data %>% filter(subject_number != "24")
# Convert the correct variables to factors
data$condition <- as.factor(data$condition)
data$condition <- relevel(data$condition, ref = "3")
data$session_pre_post <- as.factor(data$session_pre_post)
data$group_nr <- as.factor(data$group_nr)
data$intervention <- as.factor(data$intervention)
data <- data %>%
  mutate(ERP_amplitude = as.numeric(ERP_amplitude))


model_mid_LPP2 <- lmer(
  ERP_amplitude ~ 
    session_pre_post + 
    intervention + 
    condition + 
    #group_nr + 
    session_pre_post*intervention + 
    #session_pre_post:condition + 
    #session_pre_post:group_nr + 
    #intervention:condition + 
    #intervention*group_nr + 
    #condition:group_nr + 
    #session_pre_post:intervention:condition + 
    # session_pre_post:intervention:group_nr + 
    #session_pre_post:condition:group_nr + 
    #intervention:condition:group_nr + 
    #session_pre_post:intervention:condition:group_nr + 
    (1 | subject_number), 
  data = data, REML = FALSE
)


AIC(model_mid_LPP2)
anova(model_mid_LPP2)

emm_session_pre_post <- emmeans(model_mid_LPP2, ~ session_pre_post)
c5 <- pairs(emm_session_pre_post, adjust = "none")
emm_condition <- emmeans(model_mid_LPP2, ~ condition)
c6 <- pairs(emm_condition,adjust = "none")
emm_session_by_intervention <- emmeans(model_mid_LPP2, ~ session_pre_post| intervention)
c7 <- contrast(emm_session_by_intervention, method = "pairwise",adjust = "none")

#Late LPP
data <- read_csv2(file_path)
data <- data %>% filter(ERP==3)
data <- data %>% filter(subject_number != "24")
# Convert the correct variables to factors
data$condition <- as.factor(data$condition)
data$condition <- relevel(data$condition, ref = "3")
data$session_pre_post <- as.factor(data$session_pre_post)
data$group_nr <- as.factor(data$group_nr)
data$intervention <- as.factor(data$intervention)
data <- data %>%
  mutate(ERP_amplitude = as.numeric(ERP_amplitude))


model_late_LPP2 <- lmer(
  ERP_amplitude ~ 
    #session_pre_post + 
    #intervention + 
    condition + 
    #group_nr + 
    #session_pre_post:intervention + 
    #session_pre_post:condition + 
    #session_pre_post:group_nr + 
    #intervention:condition + 
    #intervention:group_nr + 
    #condition:group_nr + 
    #session_pre_post:intervention:condition + 
    #session_pre_post:intervention:group_nr + 
    #session_pre_post:condition:group_nr + 
    #intervention:condition:group_nr + 
    #session_pre_post:intervention:condition:group_nr + 
    (1 | subject_number), 
  data = data, REML = FALSE
)

AIC(model_late_LPP2)
anova(model_late_LPP2)

emm_condition <- emmeans(model_late_LPP2, ~ condition)
c8 <- pairs(emm_condition, adjust = "none")


data <- read_csv2(file_path)
data <- data %>% filter(ERP==4)
data <- data %>% filter(subject_number != "24")
# Convert the correct variables to factors
data$condition <- as.factor(data$condition)
data$condition <- relevel(data$condition, ref = "3")
data$session_pre_post <- as.factor(data$session_pre_post)
data$group_nr <- as.factor(data$group_nr)
data$intervention <- as.factor(data$intervention)
data <- data %>%
  mutate(ERP_amplitude = as.numeric(ERP_amplitude))



model_P3_2 <- lmer(
  ERP_amplitude ~ 
    session_pre_post + 
    #intervention + 
    #condition + 
    #group_nr + 
    #session_pre_post:intervention + 
    #session_pre_post:condition + 
    #session_pre_post:group_nr + 
    #intervention:condition + 
    #intervention:group_nr + 
    #condition:group_nr + 
    #session_pre_post:intervention:condition + 
    #session_pre_post:intervention:group_nr + 
    #session_pre_post:condition:group_nr + 
    #intervention:condition:group_nr + 
    #session_pre_post:intervention:condition:group_nr + 
    (1 | subject_number), 
  data = data, REML = FALSE
)


AIC(model_P3_2)
anova(model_P3_2)

emm_session_pre_post <- emmeans(model_P3_2, ~ session_pre_post)
c9 <- pairs(emm_session_pre_post, adjust = "none")



#FDR correction RQ2
library(dplyr)

contr_list <- list(
  c1 = c1, c2 = c2, c3 = c3, c4 = c4, c5 = c5, c6 = c6,
  c7 = c7, c8 = c8, c8 = c8
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
#Assumption checks: Normality, Homoscedasticity and Outliers
#----
library(tidyverse)
library(lme4)

check_lmm_assumptions <- function(model, model_name = "") {
  cat("\n=============================\n")
  cat("Model:", model_name, "\n")
  cat("=============================\n")
  
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar), add = TRUE)
  
  par(mfrow = c(1, 2), mar = c(4, 4, 3, 1) + 0.1)
  
  plot(
    fitted(model),
    resid(model),
    xlab = "Fitted values",
    ylab = "Residuals",
    main = "Residuals vs Fitted",
    pch = 16, cex = 0.6
  )
  abline(h = 0, lty = 2)
  
  qqnorm(resid(model), main = "Normal Q-Q", pch = 16, cex = 0.6)
  qqline(resid(model))
  
  invisible(TRUE)
}

file_path <- output_file

data <- read_csv2(file_path) %>%
  filter(ERP == 1,subject_number != "24") %>%
  mutate(
    condition = relevel(as.factor(condition), ref = "3"),
    session_pre_post = as.factor(session_pre_post),
    group_nr = as.factor(group_nr),
    intervention = as.factor(intervention),
    ERP_amplitude = as.numeric(ERP_amplitude)
  )

check_lmm_assumptions(model_early_LPP2, "ERP 1: Early LPP")

model <- model_early_LPP2

df <- model.frame(model) %>%
  mutate(
    subject_number = as.integer(as.character(subject_number)),
    residuals = resid(model),
    fitted    = fitted(model)
  ) %>%
  filter(subject_number != 24)

ggplot(df, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuals vs fitted values (subject 24 excluded)",
    x = "Fitted values",
    y = "Residuals"
  ) +
  theme_bw()


data <- read_csv2(file_path) %>%
  filter(ERP == 2, subject_number != "24") %>%
  mutate(
    condition = relevel(as.factor(condition), ref = "3"),
    session_pre_post = as.factor(session_pre_post),
    group_nr = as.factor(group_nr),
    intervention = as.factor(intervention),
    ERP_amplitude = as.numeric(ERP_amplitude)
  )
check_lmm_assumptions(model_mid_LPP2, "ERP 2: MId LPP")

model <- model_mid_LPP2

df <- model.frame(model) %>%
  mutate(
    subject_number = as.integer(as.character(subject_number)),
    residuals = resid(model),
    fitted    = fitted(model)
  ) %>%
  filter(subject_number != 24)

ggplot(df, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuals vs fitted values (subject 24 excluded)",
    x = "Fitted values",
    y = "Residuals"
  ) +
  theme_bw()

data <- read_csv2(file_path) %>%
  filter(ERP == 3,subject_number != "24") %>%
  mutate(
    condition = relevel(as.factor(condition), ref = "3"),
    session_pre_post = as.factor(session_pre_post),
    group_nr = as.factor(group_nr),
    intervention = as.factor(intervention),
    ERP_amplitude = as.numeric(ERP_amplitude)
  )
check_lmm_assumptions(model_late_LPP2, "ERP 3 :Late LPP")

model <- model_late_LPP2

df <- model.frame(model) %>%
  mutate(
    subject_number = as.integer(as.character(subject_number)),
    residuals = resid(model),
    fitted    = fitted(model)
  ) %>%
  filter(subject_number != 24)

ggplot(df, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuals vs fitted values (subject 24 excluded)",
    x = "Fitted values",
    y = "Residuals"
  ) +
  theme_bw()

data <- read_csv2(file_path) %>%
  filter(ERP == 4,subject_number != "24") %>%
  mutate(
    condition = relevel(as.factor(condition), ref = "3"),
    session_pre_post = as.factor(session_pre_post),
    group_nr = as.factor(group_nr),
    intervention = as.factor(intervention),
    ERP_amplitude = as.numeric(ERP_amplitude)
  )
check_lmm_assumptions(model_P3_2, "ERP 4 :P3")


model <- model_P3_2

df <- model.frame(model) %>%
  mutate(
    subject_number = as.integer(as.character(subject_number)),
    residuals = resid(model),
    fitted    = fitted(model)
  ) %>%
  filter(subject_number != 24)

ggplot(df, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuals vs fitted values (subject 24 excluded)",
    x = "Fitted values",
    y = "Residuals"
  ) +
  theme_bw()