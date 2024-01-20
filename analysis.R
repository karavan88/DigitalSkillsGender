#-------------------------------------------------------------------
# Project: Gender Gap in Digital Skills 
# Sector: UNICEF Education Data & Analytics
# Author: Garen Avanesian
# Date: 20 Oct 2023; 
#-------------------------------------------------------------------



control_file_main <- 
  read_excel(file.path(metaData, "Control File Master.xlsx"), sheet = "control_file", skip = 1) %>% 
  drop_na(country) 

control_file_main1 = 
  control_file_main %>%
  #we need to exclude the Roma datasets
  filter(!str_detect(country, "Roma|Kashmir")) %>%
  #filter(iso3 %in% iso3) %>%
  filter(`ICT men` == 1 & survey == "MICS6") 

countries_to_run =
  control_file_main1 %>%
  pull(country) 


ict_data_rep = list()
lp_skill = list()
fe_skill = list()
fe_skill_wealth = list()
fe_skill_no_educ = list()
mlm_data = list()
mlm_dataset = list()

for (i in 1:length(countries_to_run)) {
  
  current_country = countries_to_run[i]
  
  file = readRDS(paste0(inputData, "/", current_country, ".rds"))
  
  
  
  ### WE NEED TO CREATE KEY VARIABLES ###
  
  Country = control_file_main$country[control_file_main$country == current_country]
  
  unicef_region = control_file_main$unicef_region[control_file_main$country == current_country]
  
  Source = paste0(control_file_main$survey[control_file_main$country == current_country])
  
  Year = paste0(control_file_main$coverage_year[control_file_main$country == current_country])
  
  
  # assign variable names and create the values for them by each country
  
  Attending_Variable = paste0(control_file_main$attending_this_year[control_file_main$country == current_country])
  
  
  Age_Variable = paste0(control_file_main$age[control_file_main$country == current_country])
  
  Sex_Variable = paste0(control_file_main$sex[control_file_main$country == current_country])
  Sex_Male_Value = as.numeric(paste0(control_file_main$sex_male[control_file_main$country == current_country]))
  
  Area_Variable = paste0(control_file_main$area[control_file_main$country == current_country])
  Area_Urban_Value = as.numeric(paste0(control_file_main$area_urban[control_file_main$country == current_country]))
  
  Wealth_Variable = paste0(control_file_main$wq[control_file_main$country == current_country])
  PC_Variable = paste0(control_file_main$computer[control_file_main$country == current_country])
  Internet_Variable = paste0(control_file_main$internet[control_file_main$country == current_country])
  
  HH_Weight_Variable = paste0(control_file_main$hhweight[control_file_main$country == current_country])
  Female_Weight_Variable = paste0(control_file_main$female_weight[control_file_main$country == current_country])
  Male_Weight_Variable = paste0(control_file_main$male_weight[control_file_main$country == current_country])
  
  #### ICT skills of women and men 15-24
  ICT_wm_copy_move_var = paste0(control_file_main$copy_move_women[control_file_main$country == current_country])
  ICT_wm_copy_paste_var = paste0(control_file_main$copy_paste_women[control_file_main$country == current_country])
  ICT_wm_email_var = paste0(control_file_main$email_women[control_file_main$country == current_country])
  ICT_wm_formula_var = paste0(control_file_main$formula_women[control_file_main$country == current_country])
  ICT_wm_install_device_var = paste0(control_file_main$install_device_women[control_file_main$country == current_country])
  ICT_wm_software_var = paste0(control_file_main$software_women[control_file_main$country == current_country])
  ICT_wm_presentation_var = paste0(control_file_main$presentation_women[control_file_main$country == current_country])
  ICT_wm_transfer_file_var = paste0(control_file_main$transfer_file_women[control_file_main$country == current_country])
  ICT_wm_programming_var = paste0(control_file_main$programming_women[control_file_main$country == current_country])
  
  ICT_mn_copy_move_var = paste0(control_file_main$copy_move_men[control_file_main$country == current_country])
  ICT_mn_copy_paste_var = paste0(control_file_main$copy_paste_men[control_file_main$country == current_country])
  ICT_mn_email_var = paste0(control_file_main$email_men[control_file_main$country == current_country])
  ICT_mn_formula_var = paste0(control_file_main$formula_men[control_file_main$country == current_country])
  ICT_mn_install_device_var = paste0(control_file_main$install_device_men[control_file_main$country == current_country])
  ICT_mn_software_var = paste0(control_file_main$software_men[control_file_main$country == current_country])
  ICT_mn_presentation_var = paste0(control_file_main$presentation_men[control_file_main$country == current_country])
  ICT_mn_transfer_file_var = paste0(control_file_main$transfer_file_men[control_file_main$country == current_country])
  ICT_mn_programming_var = paste0(control_file_main$programming_men[control_file_main$country == current_country])
  
  file = as.data.frame(file)
  
  
  base_file = 
    file %>%
    mutate(Sex_Male = ifelse(.[, Sex_Variable] == Sex_Male_Value, 1, 0),
           Area_U = ifelse(ISO3 == "ARG", 1, .[, Area_Variable])) %>% #ARG does not have a rural sample
    mutate(Area_Urban = ifelse(Area_U == Area_Urban_Value, 1, 0),
           Wealth_Quintile = as.factor(.[, Wealth_Variable]),
           PC = .[, PC_Variable],
           Internet = .[, Internet_Variable],
           Age = .[, Age_Variable],
           HH_Weight = .[, HH_Weight_Variable],
           Female_Weight = .[, Female_Weight_Variable],
           Male_Weight = .[, Male_Weight_Variable],
           School_Attendance = .[, Attending_Variable],
           Source = Source,
           Year = Year,
           Country = Country,
           UNICEF_Region = unicef_region) %>%
    filter(Age >= 15 & Age <= 24) %>%
    ##### we will need to replace these variables
    mutate(Sex = ifelse(Sex_Male == 1, "Male", "Female"),
           Area = ifelse(Area_Urban == 1, "Urban", "Rural"),
           PC = ifelse(PC == 1,1,0),
           Internet = ifelse(Internet == 1,1,0),
           School_Attendance = ifelse(School_Attendance == 1, "2. Attends", "1. Does not attend"),
           Personal_Weight = ifelse(is.na(Female_Weight), Male_Weight, Female_Weight),
           attain = ifelse(ED4 == 2, 0, 1),
           att = ED5A + 1,
           educ_att = ifelse(attain == 0, 0, att))
  
  
  
  base_file = 
    base_file %>%
    mutate(
      #ICT skills women
      ICT_wm_copy_move = .[, ICT_wm_copy_move_var],
      ICT_wm_copy_paste = .[, ICT_wm_copy_paste_var], 
      ICT_wm_email = .[, ICT_wm_email_var],
      ICT_wm_formula = .[, ICT_wm_formula_var],
      ICT_wm_install_device = .[, ICT_wm_install_device_var],
      ICT_wm_software = .[, ICT_wm_software_var],
      ICT_wm_presentation = .[, ICT_wm_presentation_var],
      ICT_wm_transfer_file = .[, ICT_wm_transfer_file_var],
      ICT_wm_programming = .[, ICT_wm_programming_var],
      
      #ICT skills men
      ICT_mn_copy_move = .[, ICT_mn_copy_move_var],
      ICT_mn_copy_paste = .[, ICT_mn_copy_paste_var],
      ICT_mn_email = .[, ICT_mn_email_var],
      ICT_mn_formula = .[, ICT_mn_formula_var],
      ICT_mn_install_device = .[, ICT_mn_install_device_var],
      ICT_mn_software = .[, ICT_mn_software_var],
      ICT_mn_presentation = .[, ICT_mn_presentation_var],
      ICT_mn_transfer_file = .[, ICT_mn_transfer_file_var],
      ICT_mn_programming = .[, ICT_mn_programming_var]) 
  
  
  
  ICT_basic_women_vars = c("ICT_wm_copy_move","ICT_wm_copy_paste","ICT_wm_email", 
                           "ICT_wm_transfer_file")
  
  ICT_std_adv_women_vars = c("ICT_wm_formula","ICT_wm_install_device","ICT_wm_software",
                             "ICT_wm_presentation", "ICT_wm_programming")
  
  ICT_basic_men_vars = c("ICT_mn_copy_move","ICT_mn_copy_paste","ICT_mn_email", 
                         "ICT_mn_transfer_file")
  
  ICT_std_adv_men_vars = c("ICT_mn_formula", "ICT_mn_install_device","ICT_mn_software",
                           "ICT_mn_presentation", "ICT_mn_programming")
  
  ICT = 
    base_file %>%
    dplyr::select(LN, Sex, wmweight, mnweight, Personal_Weight, HL6, HH1, HH2, Age,PC,Internet,
                  starts_with("ICT_wm"), starts_with("ICT_mn"),School_Attendance, educ_att,
                  ISO3, Country, Area, Wealth_Quintile, UNICEF_Region, Year, Source) %>%
    drop_na(Personal_Weight) %>%
    mutate(ID = paste0(HH1, HH2)) %>%
    mutate(ICT_wm_copy_move = case_when(ICT_wm_copy_move==1 ~ 1, TRUE ~ 0),
           ICT_wm_copy_paste = case_when(ICT_wm_copy_paste==1 ~ 1, TRUE ~ 0),
           ICT_wm_email = case_when(ICT_wm_email==1 ~ 1, TRUE ~ 0),
           ICT_wm_formula = case_when(ICT_wm_formula==1 ~ 1, TRUE ~ 0),
           ICT_wm_install_device = case_when(ICT_wm_install_device==1 ~ 1, TRUE ~ 0),
           ICT_wm_software = case_when(ICT_wm_software==1 ~ 1, TRUE ~ 0),
           ICT_wm_presentation = case_when(ICT_wm_presentation ==1 ~ 1, TRUE ~ 0),
           ICT_wm_transfer_file = case_when(ICT_wm_transfer_file ==1 ~ 1, TRUE ~ 0),
           ICT_wm_programming = case_when(ICT_wm_programming ==1 ~ 1, TRUE ~ 0)) %>%
    mutate(ICT_mn_copy_move = case_when(ICT_mn_copy_move ==1 ~ 1, TRUE ~ 0),
           ICT_mn_copy_paste = case_when(ICT_mn_copy_paste ==1 ~ 1, TRUE ~ 0),
           ICT_mn_email = case_when(ICT_mn_email ==1 ~ 1, TRUE ~ 0),
           ICT_mn_formula = case_when(ICT_mn_formula ==1 ~ 1, TRUE ~ 0),
           ICT_mn_install_device = case_when(ICT_mn_install_device ==1 ~ 1, TRUE ~ 0),
           ICT_mn_software = case_when(ICT_mn_software==1 ~ 1, TRUE ~ 0),
           ICT_mn_presentation = case_when(ICT_mn_presentation == 1 ~ 1, TRUE ~ 0),
           ICT_mn_transfer_file = case_when(ICT_mn_transfer_file ==1 ~ 1, TRUE ~ 0),
           ICT_mn_programming = case_when(ICT_mn_programming==1 ~ 1, TRUE ~ 0)) %>%
    #create any skill wm and mn
    mutate(ICT_wm_any_skill = ifelse(ICT_wm_copy_move == 1 | ICT_wm_copy_paste == 1 | ICT_wm_email == 1 | 
                                       ICT_wm_formula == 1 | ICT_wm_install_device == 1 | ICT_wm_software == 1 | 
                                       ICT_wm_presentation == 1 | ICT_wm_transfer_file == 1 | ICT_wm_programming == 1, 1, 0),
           ICT_mn_any_skill = ifelse(ICT_mn_copy_move == 1 | ICT_mn_copy_paste == 1 | ICT_mn_email == 1 | 
                                       ICT_mn_formula == 1 | ICT_mn_install_device == 1 | ICT_mn_software == 1 | 
                                       ICT_mn_presentation == 1 | ICT_mn_transfer_file == 1 | ICT_mn_programming == 1, 1, 0)) %>%
    #check if any variable in ICT skills equals to 1 and their sum is more than 0
    mutate(ICT_wm_basic_skill = ifelse(ICT_wm_copy_move == 1 | ICT_wm_copy_paste == 1 | ICT_wm_email == 1 | 
                                         ICT_wm_transfer_file == 1, 1, 0),
           ICT_wm_std_adv_skill = ifelse(ICT_wm_formula == 1 | ICT_wm_install_device == 1 | ICT_wm_software == 1 | 
                                           ICT_wm_presentation == 1 | ICT_wm_programming == 1, 1, 0),
           ICT_mn_basic_skill = ifelse(ICT_mn_copy_move == 1 | ICT_mn_copy_paste == 1 | ICT_mn_email == 1 | 
                                         ICT_mn_transfer_file == 1, 1, 0),
           ICT_mn_std_adv_skill = ifelse(ICT_mn_formula == 1 | ICT_mn_install_device == 1 | ICT_mn_software == 1 | 
                                           ICT_mn_presentation == 1 | ICT_mn_programming == 1, 1, 0)) %>%
    mutate(ICT_wm_no_skill = ifelse(ICT_wm_any_skill == 0, 1, 0),
           ICT_mn_no_skill = ifelse(ICT_mn_any_skill == 0, 1, 0)) %>%
    #create one variable for each skill level
    mutate(ICT_no_skill = ifelse(Sex == "Male", ICT_mn_no_skill, ICT_wm_no_skill),
           ICT_any_skill = ifelse(Sex == "Male", ICT_mn_any_skill, ICT_wm_any_skill),
           ICT_basic_skill = ifelse(Sex == "Male", ICT_mn_basic_skill, ICT_wm_basic_skill),
           ICT_std_adv_skill = ifelse(Sex == "Male", ICT_mn_std_adv_skill, ICT_wm_std_adv_skill)) %>%
    dummy_cols(select_columns = "Age")
  
  ict_results = 
    ICT %>%
    as_survey_design(weight = Personal_Weight) %>%
    group_by(ISO3, Country, UNICEF_Region, Source, Year, Sex) %>%
    summarise(ICT_no_skill = survey_mean(ICT_no_skill, vartype = "ci"),
              ICT_any_skill = survey_mean(ICT_any_skill, vartype = "ci"),
              ICT_basic_skill = survey_mean(ICT_basic_skill, vartype = "ci"),
              ICT_std_adv_skill = survey_mean(ICT_std_adv_skill, vartype = "ci"),
              PC = survey_mean(PC, vartype = "ci"),
              Internet = survey_mean(Internet, vartype = "ci")) # %>%
  # select(-contains("_se"))
  
  
  
  
  fe_skill_data = 
    ICT %>%
    arrange(ID) %>%
    dplyr::select(ID, Sex, LN, HH1, HH2, starts_with("Age"), Area, 
                  School_Attendance, educ_att, ICT_any_skill, Wealth_Quintile, Country) %>%
    group_by(ID) %>%
    filter(all(c("Male", "Female") %in% Sex)) %>%
    ungroup() %>%
    mutate(Bottom20 = ifelse(Wealth_Quintile == "1", 1, 0),
           Top20 = ifelse(Wealth_Quintile == "5", 1, 0),
           educ_att = factor(educ_att)) %>%
    drop_na()
  
  # we need to see how many boys and girls from the same household are in each country
  qual_check =
    fe_skill_data %>%
    mutate(n = 1) %>%
    group_by(Sex) %>%
    summarise(N = sum(n)) %>%
    pivot_wider(names_from = Sex, values_from = N) %>%
    rename(N_Female = Female, N_Male = Male)
  
  #we also need to create a dataset to further carry out an MLM
  mlm_dataset[[i]] = fe_skill_data
  
  
  
  fe_lp_skill = plm(ICT_any_skill ~ Sex + factor(educ_att) + Age_16 + Age_17 + Age_18 + Age_19 + Age_20 + Age_21 + Age_22 + Age_23 + Age_24,
                    index = "ID", data = fe_skill_data )
  
  #produce the tidy output with the clustered standard errors
  fe_skill_tidy = 
    broom::tidy(coeftest(fe_lp_skill, vcov = vcovHC(fe_lp_skill, cluster = "group"))) %>% 
    mutate(Country = current_country, 
           p.value = round(p.value, 2),
           Model = "FE LP") %>% 
    bind_cols(qual_check)
  
  fe_skill[[i]] = fe_skill_tidy
  
  ict_table = 
    ict_results %>%
    pivot_wider(names_from = Sex,
                values_from = c(ICT_no_skill, ICT_no_skill_low, ICT_no_skill_upp,
                                ICT_any_skill, ICT_any_skill_low, ICT_any_skill_upp,
                                ICT_basic_skill, ICT_basic_skill_low, ICT_basic_skill_upp,
                                ICT_std_adv_skill, ICT_std_adv_skill_low, ICT_std_adv_skill_upp,
                                PC, PC_low, PC_upp,
                                Internet, Internet_low, Internet_upp)) %>%
    mutate(Sex_Male_LP_FE = fe_skill_tidy$estimate[fe_skill_tidy$term == "SexMale"],
           Sig_LP_FE = round(fe_skill_tidy$p.value[fe_skill_tidy$term == "SexMale"], 6))
  
  
  ict_data_rep[[i]] = ict_table
  
  
  print(current_country)
  
}

ict_report = data.frame(Reduce(rbind, ict_data_rep))


# Descriptives


# Regressions

fe_lp_regressions <- 
  data.frame(Reduce(rbind, fe_skill)) %>%
  mutate(Sig = ifelse(p.value < 0.1, "Sig", "Not sig"))



# Multilevel Model Analysis

pooled_data = 
  data.frame(Reduce(rbind, mlm_dataset)) %>%  
  mutate(Sex_Male = ifelse(Sex == "Male", 1, 0)) %>%
  filter(!Country %in% c("Turks and Caicos Islands", "Tuvalu"))



mlm_pooled = lmer(ICT_any_skill ~ Sex_Male + factor(educ_att) + Wealth_Quintile + Area +
                    Age_16 + Age_17 + Age_18 + Age_19 + Age_20 + 
                    Age_21 + Age_22 + Age_23 + Age_24 +
                    (1 |Country/ID)+  (1 + Sex_Male || Country:Wealth_Quintile), 
                  REML = F, control = lmerControl(optimizer = "Nelder_Mead"),
                  data = pooled_data)

coef_data = 
  coef(mlm_pooled)$`Country:Wealth_Quintile` %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  dplyr::select(rowname, Sex_Male) %>%
  separate(rowname, c("country", "wealth"), ":")


mlm_chart <-
  ggplot(coef_data, aes(Sex_Male*-1, fct_reorder(wealth, desc(wealth)))) +
  geom_point(size = 6, color = "lightblue") +
  geom_vline(xintercept = 0) +
  geom_text(aes(label = round(Sex_Male*-100, 1)), color = "black", size = 2) +
  theme_bw() +
  scale_y_discrete(labels = c("Richest", "Rich","Middle", "Poor", "Poorest" )) +
  scale_x_continuous(labels = scales::percent) +
  facet_wrap(country~.) +
  theme(legend.position = "bottom") +
  xlab("") +
  ylab("")



