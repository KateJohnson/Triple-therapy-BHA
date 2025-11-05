library(remotes)
install_github("rachaelmountain/epicR", ref="triple-therapy-BHA")
library(epicR)
library(tidyverse)

### MEDICATION ADHERENCE ####
# Medication adherence = HIGH
  
  # Settings
  settings <- get_default_settings()
  settings$record_mode <- 0
  settings$n_base_agents <- 1e7
  init_session(settings = settings)
  input <- get_input(closed_cohort = 1)$values
  time_horizon <-20
  input$global_parameters$time_horizon <- time_horizon
  
  input$medication$medication_adherence <- 0.7 # set medication adherence
  input$medication$triple_therapy_early_initiation <- 0 # standard of care scenario
  input$medication$triple_therapy_early_initiation_criteria[1:2] <- c(1, 0) # GOLD>=1, no minimum symptoms sceneario
  
  # Run model
  run(input = input)
  output<- Cget_output()
  output_ex <- Cget_output_ex()
  terminate_session()
  
  # Results
  res_med_high <- data.frame(Scenario = "0.7 PDC",
                             cohort=output$n_cohort,
                             exacs=sum(output_ex$n_exac_by_ctime_severity),
                             exacs_sev=sum(output_ex$n_exac_by_ctime_severity[,c(3,4)]),
                             life_yrs=output$cumul_time,
                             qalys=output$total_qaly) %>% 
    # express per 100 patients
    mutate(exacs_100P = exacs/cohort*100,
           exacs_sev_100P = exacs_sev/cohort*100,
           life_yrs_100P = output$cumul_time/cohort*100,
           qalys_100P = output$total_qaly/cohort*100)
  
  ####
  # Medication adherence = MEDIUM
  
  # Settings
  settings <- get_default_settings()
  settings$record_mode <- 0
  settings$n_base_agents <- 1e7
  init_session(settings = settings)
  input <- get_input(closed_cohort = 1)$values
  time_horizon <-20
  input$global_parameters$time_horizon <- time_horizon
  
  input$medication$medication_adherence <- 0.5 # set medication adherence
  input$medication$triple_therapy_early_initiation <- 0 # standard of care scenario
  input$medication$triple_therapy_early_initiation_criteria[1:2] <- c(1, 0) # GOLD>=1, no minimum symptoms sceneario
  
  # Run model
  run(input = input)
  output<- Cget_output()
  output_ex <- Cget_output_ex()
  terminate_session()
  
  # Results
  res_med_med<- data.frame(Scenario = "0.5 PDC",
                             cohort=output$n_cohort,
                             exacs=sum(output_ex$n_exac_by_ctime_severity),
                             exacs_sev=sum(output_ex$n_exac_by_ctime_severity[,c(3,4)]),
                             life_yrs=output$cumul_time,
                             qalys=output$total_qaly) %>% 
    # express per 100 patients
    mutate(exacs_100P = exacs/cohort*100,
           exacs_sev_100P = exacs_sev/cohort*100,
           life_yrs_100P = output$cumul_time/cohort*100,
           qalys_100P = output$total_qaly/cohort*100)
  
  
  ### 
  # Medication adherence = LOW
  
  # Settings
  settings <- get_default_settings()
  settings$record_mode <- 0
  settings$n_base_agents <- 1e7
  init_session(settings = settings)
  input <- get_input(closed_cohort = 1)$values
  time_horizon <-20
  input$global_parameters$time_horizon <- time_horizon
  
  input$medication$medication_adherence <- 0.3 # set medication adherence
  input$medication$triple_therapy_early_initiation <- 0 # standard of care scenario
  input$medication$triple_therapy_early_initiation_criteria[1:2] <- c(1, 0) # GOLD>=1, no minimum symptoms sceneario
  
  # Run model
  run(input = input)
  output<- Cget_output()
  output_ex <- Cget_output_ex()
  terminate_session()
  
  # Results
  res_med_low <- data.frame(Scenario = "0.3 PDC",
                             cohort=output$n_cohort,
                             exacs=sum(output_ex$n_exac_by_ctime_severity),
                             exacs_sev=sum(output_ex$n_exac_by_ctime_severity[,c(3,4)]),
                             life_yrs=output$cumul_time,
                             qalys=output$total_qaly) %>% 
    # express per 100 patients
    mutate(exacs_100P = exacs/cohort*100,
           exacs_sev_100P = exacs_sev/cohort*100,
           life_yrs_100P = output$cumul_time/cohort*100,
           qalys_100P = output$total_qaly/cohort*100)
  
  
  
  ### Plot results ####
  res_med <- rbind(res_med_high, res_med_med, res_med_low) %>%
    select(Scenario, exacs_100P, exacs_sev_100P, life_yrs_100P, qalys_100P) %>% 
    pivot_longer(cols=exacs_100P:qalys_100P, names_to="outcome") %>% 
    group_by(outcome) %>% 
    mutate(ref_PDC = value[Scenario=="0.7 PDC"],
           diff_ref_PDC = value - ref_PDC) %>% 
    ungroup() %>%
    mutate(diff_ref_PDC = na_if(diff_ref_PDC, 0),
           Scenario = fct_relevel(Scenario,"0.7 PDC","0.5 PDC", "0.3 PDC")) 
  


facet_labels <- c(
  "exacs_100P" = "Exacerbations",
  "exacs_sev_100P" = "Severe Exacerbations",
  "life_yrs_100P" = "Life Years",
  "qalys_100P" = "QALYs")


res_med %>% 
  ggplot(aes(y=value, x=Scenario, fill=Scenario)) + 
  geom_bar(position="dodge", stat="identity", width=0.75) + 
  geom_text(aes(label = round(diff_ref_PDC,2)), 
            position = position_dodge(width = 0.75), 
            vjust = -0.3, size = 3) + 
  facet_wrap(~ outcome, scales="free",
             labeller=labeller(outcome= facet_labels)) + 
  theme_bw() + guides(fill = guide_legend(title = NULL)) +
  scale_fill_manual("Subgroup", values=c("skyblue","thistle","lightgreen")) +
  guides(fill = guide_legend(title = NULL)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.10)))

ggsave("PDC_outcomes_plot.tiff", width=7, height=5, dpi=300)

