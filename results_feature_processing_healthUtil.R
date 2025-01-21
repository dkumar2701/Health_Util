library(tidyverse)
library(ggprism)
library(shapviz)
library(progress)
library(data.table)

data_name = "Decompression_Labs_01-21-25"
set.seed(1)

csv_files = list.files(pattern="\\.csv$")
data_list = lapply(csv_files, read.csv)
names(data_list) <- tools::file_path_sans_ext(csv_files)

# Fusion: 79 features, 3812 pts, Decompression: 76 features, 10302 pts
# New fusion cohort: 79 feautres, 3581 pts
# Newest Fusion: 2890 x 79, Decompression: 10128 x 76
# Newest Labs: 2890 x 106, Decompression: 10128 x 103
######### SAVED DATA HERE CALLED "CSV.RDATA" ###########



# Filter for data frames with names ending in "_model_results" --> 1 tbl per model, 50 rows for 5 splits x 10 reps
model_results_list <- data_list[grep("_model_results$", names(data_list))]

# Merge all selected data frames vertically
All_model_results <- do.call(rbind, model_results_list)

# Filter for data frames with names ending in "_auc_data" ---> Data to make AUC curves for each of 50 reps per model
auc_results_list <- data_list[grep("_auc_data$", names(data_list))]

# Merge all selected data frames vertically
All_auc_results <- do.call(rbind, auc_results_list)

# Filter for data frames with names ending in "_feature_importance". --> 1tbl per model mean_abs_shap per feature per rep
feature_results_list <- data_list[grep("_feature_importance$", names(data_list))]

# Merge all selected data frames vertically
All_feature_results <- do.call(rbind, feature_results_list)

# Filter for data frames with names ending in "_feature_importance_full"
shapley_results_list <- data_list[grep("_feature_importance_full$", names(data_list))]

# Merge all selected data frames vertically
All_shapley_results <- do.call(rbind, shapley_results_list)


# Reading in core output files

results_file <- All_model_results #1 tbl per model, 50 rows for 5 splits x 10 reps, 8 models
features_file <- All_feature_results # 8 models, mean_abs_shap per feature per rep
shapley_file <- All_shapley_results # All shap results data -> #patients*#features*10 x feature_actual_value (the number passed in) & shap & identifiers

# Assuming your data frame is named df
unique_combinations <- shapley_file %>%
  distinct(ir_id, surgery_start_datetime) %>%  # Select distinct combinations
  count()  # Count the number of rows

# View the count
unique_combinations

########### Save as readyData.rData #############




# Aggregating results for table generation
# Cut down to only the number of models (9), summarize over all reps
summarized_results <- results_file %>%
  group_by(Procedure, Outcome, Features, Model) %>%
  summarize(
    AUC_mean = round(mean(AUC), digits = 3),
    AUC_std = sd(AUC), 
    Acc_mean = mean(Accuracy),
    Acc_std = sd(Accuracy),
    loss_mean = round(mean(`Brier.Loss`), digits = 2),
    loss_std = sd(`Brier.Loss`),
    sn_mean = mean(Sensitivity), 
    sp_mean = mean(Specificity),
    sn_std = sd(Sensitivity), 
    sp_std = sd(Specificity),
    n_mean = mean(n),
    label_n_mean = mean(label_n),
    label_n_std = sd(label_n),
    n_std = sd(n),
    count = n()
  ) %>%
  mutate(
    ci_auc = round((AUC_std * 1.96 / sqrt(50)), digits = 2), 
    ci_sn = round((sn_std * 1.96 / sqrt(50)), digits = 2),
    ci_sp = round((sp_std * 1.96 / sqrt(50)), digits = 2)  
    
    
  )

#sort
summarized_results <- summarized_results[order(-summarized_results$AUC_mean), ]
write_csv(summarized_results, paste(data_name, "_summarized_results.csv", sep = "_"))

features_sum_n_groups <- count(distinct(features_file, Procedure, Outcome, Features, Model, Feature_Name))
print(features_sum_n_groups)
pb <- progress_bar$new(
  format = "[:bar] :percent :elapsed :eta",
  total = features_sum_n_groups,
  clear = FALSE
)

#Get mean and CI shaps over all 50 reps/folds per feature, row_n = #features
summarized_features <- features_file %>%
  group_by(Procedure, Outcome, Features, Model, Feature_Name) %>%
  group_modify(~ {
    pb$tick()  # Update progress bar after processing each group
    
    .x %>%
      mutate(
        Feature_Mean_Abs_Value = as.numeric(Feature_Mean_Abs_Value)
      ) %>%
      summarize(
        Feature_Value_Mean = mean(Feature_Mean_Abs_Value),
        Feature_Value_SD = sd(Feature_Mean_Abs_Value)
      ) %>%
      mutate(
        ci_feature = Feature_Value_SD * 1.96 / sqrt(50),
        ci_upper = Feature_Value_Mean + ci_feature,
        ci_lower = Feature_Value_Mean - ci_feature,
        multiple = ci_upper * ci_lower,
        sig = case_when(
          multiple <= 0 ~ 0,
          multiple > 0 ~ 1
        )
      )
  })

write_csv(summarized_features, paste(data_name, "_summarized_features.csv", sep = "_"))


###################summarized Shap Creation######################

sumShap_ngroups <- count(distinct(shapley_file, ir_id, surgery_start_datetime, Procedure, Outcome, Model, Feature_Name))
sumShap_ngroups
#summarize shaps for each patient, averaged over their 10 reps. N_rows is #features * #patients
# Step 2: Initialize the progress bar
pb <- progress_bar$new(
  format = "[:bar] :percent | Elapsed: :elapsed | ETA: :eta",
  total = sumShap_ngroups,
  clear = FALSE
)

# Convert to data.table
shapley_file_table <- as.data.table(shapley_file)

# Summarize data
summarized_SHAP <- shapley_file_table[
  , {
    pb$tick()  # Update progress bar after processing each group
    list(
      mean_SHAP = mean(Feature_Value),
      sd_SHAP = sd(Feature_Value),
      mean_real_value = mean(Feature_Actual_Value),
      sd_real_value = sd(Feature_Actual_Value)
    )
  }, by = .(ir_id, surgery_start_datetime, Procedure, Outcome, Model, Feature_Name)
]





# summarized_SHAP <- shapley_file %>% #Calculate summary data for each patient, analyzing over the 10 reps done
#   group_by(ir_id, surgery_start_datetime, Procedure, Outcome, Model, Feature_Name) %>%
#   summarize(
#     mean_SHAP = mean(Feature_Value),
#     sd_SHAP = sd(Feature_Value),
#     mean_real_value = mean(Feature_Actual_Value),
#     sd_real_value = sd(Feature_Actual_Value)
#   )


#############Save in AllSummarized.RData #########################

# Function to isolate best function from results file based on a tiered approach IGNORE THIS IGNORE THIS 
extract_best_feature_file <- function(sum_results, sum_features, procedure_of_interest, outcome_of_interest){
  
  if (outcome_of_interest != "outpatient_resource_sum"){
    
    filtered_results_round1 <- sum_results %>%
      filter(Outcome == outcome_of_interest) %>%
      filter(Procedure == procedure_of_interest) %>%
      mutate(
        AUC_mean = round(AUC_mean, digits = 2),
        ci_auc = round(ci_auc, digits = 2)
      ) %>%
      slice_max(AUC_mean, with_ties = TRUE)
    
    model_of_interest = filtered_results_round1$Model
    
    
    if (nrow (filtered_results_round1 > 1)){
      
      filtered_results_round2 <- filtered_results_round1 %>%
        slice_min(ci_auc, with_ties = TRUE)
      
      model_of_interest = filtered_results_round2$Model
      
      
      if (nrow (filtered_results_round2 > 1)){
        
        filtered_results_round3 <- filtered_results_round2 %>%
          slice_min(loss_mean, with_ties = TRUE)
        
        model_of_interest = filtered_results_round3$Model
        
        if (nrow (filtered_results_round3 > 1)){
          
          filtered_results_round4 <- filtered_results_round3 %>%
            arrange(factor(Model, levels = c("ENet", "RF", "XGBoost", "NN"))) %>%
            head(1)
          
          
          model_of_interest = filtered_results_round4$Model
        }
      }
    }
  }
  else if (outcome_of_interest == "outpatient_resource_sum"){
    
    filtered_results_round1 <- sum_results %>%
      filter(Outcome == outcome_of_interest) %>%
      filter(Procedure == procedure_of_interest) %>%
      mutate(
        MAE_mean = round(MAE_mean, digits = 2),
        ci_mae = round(ci_mae, digits = 2)
      ) %>%
      slice_min(MAE_mean, with_ties = TRUE)
    
    model_of_interest = filtered_results_round1$Model
    
    if (nrow (filtered_results_round1 > 1)){
      
      filtered_results_round2 <- filtered_results_round1 %>%
        slice_min(ci_mae, with_ties = TRUE)
      
      model_of_interest = filtered_results_round2$Model
      
      if (nrow (filtered_results_round2 > 1)){
        
        filtered_results_round3 <- filtered_results_round2 %>%
          arrange(factor(Model, levels = c("ENet", "RF", "NN"))) %>%
          head(1)
        
        
        model_of_interest = filtered_results_round3$Model
        
        
      }
      
      
    }
    
    
    
    
    
    
  }
  
  filtered_features <- sum_features %>%
    filter(Outcome == outcome_of_interest) %>%
    filter(Procedure == procedure_of_interest) %>%
    filter(Model == model_of_interest)
  
  print(model_of_interest)
  
  return(filtered_features)
  
}


# Iterative loop to produce feature importance and SHAP summary plots


#procedures = c("Fusion")
procedures = c("Decompression")
#procedures = c("PLF", "PLIF_TLIF", "AXDLIF")
#procedures = c("PLF")
#outcomes = c("reoperation_90")
#outcomes = c("post_lam_syndrome", "ed_uc_binary", "outpatient_resource_binary_75")
#outcomes = c("outpatient_resource_sum")
outcomes = c("ltc_postlaminectomy_syndrome")

label_vec <- c(
  ir_id = "Patient ID",
  surgery_start_datetime = "Surgery Start Time",
  age_at_procedure = "Age at Procedure",
  gender_female = "Gender: Female",
  BMI = "Body Mass Index (BMI)",
  elix_obesity = "Elix: Obesity",
  elix_aids_hiv = "Elix: AIDS/HIV",
  elix_lymphoma = "Elix: Lymphoma",
  elix_paralysis = "Elix: Paralysis",
  elix_psychoses = "Elix: Psychoses",
  elix_depression = "Elix: Depression",
  elix_drug_abuse = "Elix: Drug Abuse",
  elix_weight_loss = "Elix: Weight Loss",
  elix_coagulopathy = "Elix: Coagulopathy",
  elix_alcohol_abuse = "Elix: Alcohol Abuse",
  elix_liver_disease = "Elix: Liver Disease",
  elix_renal_failure = "Elix: Renal Failure",
  elix_hypothyroidism = "Elix: Hypothyroidism",
  elix_valvular_disease = "Elix: Valvular Disease",
  elix_blood_loss_anemia = "Elix: Blood Loss Anemia",
  elix_deficiency_anemia = "Elix: Deficiency Anemia",
  elix_metastatic_cancer = "Elix: Metastatic Cancer",
  elix_cardiac_arrhythmia = "Elix: Cardiac Arrhythmia",
  elix_rheumatoid_arhritis = "Elix: Rheumatoid Arthritis",
  elix_diabetes_complicated = "Elix: Complicated Diabetes",
  elix_diabetes_uncomplicated = "Elix: Uncomplicated Diabetes",
  elix_congestive_heart_failure = "Elix: Congestive Heart Failure",
  elix_hypertension_complicated = "Elix: Complicated Hypertension",
  elix_chronic_pulmonary_disease = "Elix: Chronic Pulmonary Disease",
  elix_solid_tumor_wo_metastasis = "Elix: Solid Tumor (Without Metastasis)",
  elix_hypertension_uncomplicated = "Elix: Uncomplicated Hypertension",
  elix_other_neurological_disorder = "Elix: Other Neurological Disorder",
  "elix_peripheral vascular_disorder" = "Elix: Peripheral Vascular Disorder",
  elix_pulmonary_circulation_disorder = "Elix: Pulmonary Circulation Disorder",
  elix_fluid_and_electrolyte_disorders = "Elix: Fluid and Electrolyte Disorders",
  elix_peptic_ulcer_disease_excluding_bleeding = "Elix: Peptic Ulcer Disease (Excluding Bleeding)",
  validation_cur_tobacco = "Current Tobacco Use Validation",
  validation_fmr_tobacco = "Former Tobacco Use Validation",
  shx_cervical_fusion = "History of Cervical Fusion",
  shx_cervical_surgery = "History of Cervical Surgery",
  shx_thoracolumbar_fusion = "History of Thoracolumbar Fusion",
  shx_thoracolumbar_surgery = "History of Thoracolumbar Surgery",
  shx_unspecified_spine_fusion = "History of Unspecified Spine Fusion",
  shx_unspecified_spine_surgery = "History of Unspecified Spine Surgery",
  lumbar_stenosis = "Lumbar Stenosis",
  lumbar_spondy = "Lumbar Spondylosis",
  lumbar_disc_disorders = "Lumbar Disc Disorders",
  cervical_disc_diorders = "Cervical Disc Disorders",
  cervical_stenosis = "Cervical Stenosis",
  cervical_spondy = "Cervical Spondylosis",
  cervical_disc_herniation = "Cervical Disc Herniation",
  lumbar_disc_herniation = "Lumbar Disc Herniation",
  preop_med_90days_ace_inhibitor = "Preoperative ACE Inhibitor Use (90 Days)",
  preop_med_90days_arb = "Preoperative ARB Use (90 Days)",
  preop_med_90days_antidepressant = "Preoperative Antidepressant Use (90 Days)",
  preop_med_90days_beta_2_agonist = "Preoperative Beta-2 Agonist Use (90 Days)",
  preop_med_90days_beta_blocker = "Preoperative Beta Blocker Use (90 Days)",
  preop_med_90days_benzodiazepine = "Preoperative Benzodiazepine Use (90 Days)",
  preop_med_90days_immunosuppresant = "Preoperative Immunosuppressant Use (90 Days)",
  preop_med_90days_nsaid = "Preoperative NSAID Use (90 Days)",
  preop_med_90days_opioid = "Preoperative Opioid Use (90 Days)",
  preop_med_90days_anti_psychotic = "Preoperative Antipsychotic Use (90 Days)",
  preop_med_90days_neuromodulator = "Preoperative Neuromodulator Use (90 Days)",
  preop_med_90days_biphosphonate = "Preoperative Biphosphonate Use (90 Days)",
  preop_med_90days_loop_diuretic = "Preoperative Loop Diuretic Use (90 Days)",
  preop_med_90days_thiazide_diuretic = "Preoperative Thiazide Diuretic Use (90 Days)",
  preop_med_90days_cinacalcet = "Preoperative Cinacalcet Use (90 Days)",
  preop_med_90days_insulin = "Preoperative Insulin Use (90 Days)",
  preop_med_90days_oral_diabetes = "Preoperative Oral Diabetes Medication Use (90 Days)",
  preop_med_90days_calcium_supplement = "Preoperative Calcium Supplement Use (90 Days)",
  preop_med_90days_vit_d_supplement = "Preoperative Vitamin D Supplement Use (90 Days)",
  OR_duration_hours = "OR Duration (Hours)",
  anesthesia_duration_hours = "Anesthesia Duration (Hours)",
  cpt_multilevel = "Multilevel Procedure",
  cpt_instrumentation = "Instrumentation",
  cohort_query_microdisc = "Microdiscectomy",
  anesthesia_type_General = "Anesthesia Type: General",
  procedure_setting_Inpatient = "Procedure Setting: Inpatient",
  cpt_anterior_approach = "Anterior Approach",
  cpt_lateral_approach = "Lateral Approach",
  cpt_posterior_approach = "Posterior Approach",
  ltc_postlaminectomy_syndrome = "Outcome: Post-Laminectomy Syndrome"
)




options(warn = 1)
#WHAT IS THE BEST MODEL? CHOOSE MANUALLY TODO
best_model_name <- "RF"
# Only one procedure and outcome #TODO

procedure <- procedures[1]
outcome <- outcomes[1]
#for (procedure in procedures){
#  for (outcome in outcomes){
    
    print(procedure)
    print(outcome)
    
    prefix = paste(procedure, "/feature_files/", sep = "")
    
    date_features = "12_2_24_features" #TODO
    
    date_SHAP = "12_2_24_SHAP" #TODO
    
    feature_file <- summarized_features[summarized_features$Model == best_model_name, ]
    SHAP_file <- summarized_SHAP[summarized_SHAP$Model == best_model_name, ] %>%
      ungroup()
    
    model_name_dat <- feature_file %>% head(1)
    model_name = model_name_dat$Model
    
    print(model_name)
    
    if (model_name != "ASA" && nrow(feature_file) > 0){
      
      trimmed_feature_file <- feature_file %>%
        ungroup() %>%
        filter(sig != 0) %>%
        mutate(
          Feature_Value_Abs = abs(Feature_Value_Mean),
          ci_up = Feature_Value_Abs + ci_feature,
          ci_down = Feature_Value_Abs - ci_feature
        ) %>%
        arrange(desc(Feature_Value_Abs)) %>%
        head(10) 
      
      order_vec <- rev(trimmed_feature_file$Feature_Name)
      
      
      ggplot(data = trimmed_feature_file) +
        geom_bar(aes(x = reorder(Feature_Name, Feature_Value_Abs), y = Feature_Value_Abs), stat = "identity", alpha = 0.7, width = 0.5) +
        geom_errorbar(aes(x = Feature_Name, ymin = ci_down, ymax = ci_up), width = 0.5) +
        coord_flip() +
        labs(x = "", y = "") +
        theme(axis.text = element_text(size = 15)) +
        scale_x_discrete(labels = label_vec) + 
        theme_prism()
      
      suffix = ".png"
      output_file_save <- paste0(prefix, paste(procedure, outcome, model_name, date_features, suffix, sep = "_"))
      
      ggsave(output_file_save, units = "in", width = 7, height = 7, device = "png", dpi = 300)
      
      
      feature_data <- SHAP_file %>%
        select(-c(Outcome, Model, Procedure, mean_SHAP, sd_SHAP, sd_real_value)) %>%
        pivot_wider(id_cols = c(ir_id, surgery_start_datetime), names_from = Feature_Name, values_from = mean_real_value) %>%
        select(-c(ir_id, surgery_start_datetime)) %>%
        select(all_of(order_vec)) %>%
        as.matrix()
      
      
      shap_data <-  SHAP_file %>%
        select(-c(Outcome, Model, Procedure, sd_SHAP, mean_real_value, sd_real_value)) %>%
        pivot_wider(id_cols = c(ir_id, surgery_start_datetime), names_from = Feature_Name, values_from = mean_SHAP) %>%
        select(-c(ir_id, surgery_start_datetime)) %>%
        select(all_of(order_vec)) %>%
        as.matrix()
      
      # Compute the symmetric x-axis range
      max_abs_value <- max(abs(shap_data))
      shap_plotter <- shapviz(shap_data, feature_data)
      
      if (procedure == "Decompression"){
        print("test")
        size_bees = 0.000000000000001
        alpha_bees = 0.5
        shape_bees = "."
      } else if (procedure == "Fusion"){
        size_bees = 0.00005
        alpha_bees = 0.6
        shape_bees = 19
      }
      
      sv_importance(shap_plotter, kind = "beeswarm", size = size_bees, alpha = alpha_bees, shape = 19) +
        theme_prism() + 
        theme(axis.text = element_text(face = "bold"))+
        #theme(panel.grid.major.x = element_line(colour = "black")) +
        scale_y_discrete (labels = label_vec, limits = order_vec)
      
      suffix = ".png"
      output_file_save <- paste0(prefix, paste(procedure, outcome, model_name, date_SHAP, suffix, sep = "_"))
      
      ggsave(output_file_save, device = "png", units = "in", width = 15, height = 10)
      
      
      
      
    }
    
    
    
    
    
    
#  }
#}
############## AD HOC #################

# LF 
# Generating ad hoc analyses

ad_hoc_file <- summarized_SHAP %>%
  filter(Outcome == "ed_uc_binary") %>%
  filter(Model == "ENet")


feature_data <- ad_hoc_file %>%
  ungroup() %>%
  select(-c(Outcome, Model, Procedure, mean_SHAP, sd_SHAP, sd_real_value)) %>%
  pivot_wider(id_cols = c(ir_id, surgery_start_datetime), names_from = Feature_Name, values_from = mean_real_value) %>%
  select(-c(ir_id, surgery_start_datetime))

shap_data <-  ad_hoc_file %>%
  ungroup() %>%
  select(-c(Outcome, Model, Procedure, sd_SHAP, mean_real_value, sd_real_value)) %>%
  pivot_wider(id_cols = c(ir_id, surgery_start_datetime), names_from = Feature_Name, values_from = mean_SHAP) %>%
  select(-c(ir_id, surgery_start_datetime)) %>%
  as.matrix()


shap_plotter <- shapviz(shap_data, feature_data)


sv_dependence(shap_plotter, "platelet_count") + 
  theme_prism() +
  xlab("Preoperative Platelet Count") + 
  ylab("Mean SHAP Value") + 
  scale_x_continuous(limits = c(0, 1000), breaks = c(0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)) + 
  scale_y_continuous(limits = c(-100, 100), breaks = c(-100, -75, -50, -25, 0, 25, 50, 75, 100)) + 
  geom_hline(yintercept = 0, linetype =  "dashed", color = "red")

#theme(axis.text = element_text(face = "bold"))+
#theme(panel.grid.major.x = element_line(colour = "black")) 


ggsave(output_file_save, device = "png", units = "in", width = 7, height = 7, dpi = 300)



sv_dependence(shap_plotter, "glucose") + 
  theme_prism() +
  xlab("Preoperative Serum Glucose") + 
  ylab("Mean SHAP Value") +
  scale_x_continuous(limits = c(0, 1000), breaks = c(0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)) + 
  scale_y_continuous(limits = c(-100, 100), breaks = c(-100, -75, -50, -25, 0, 25, 50, 75, 100)) + 
  geom_hline(yintercept = 0, linetype =  "dashed", color = "red")



############## save allData.RData ######################




