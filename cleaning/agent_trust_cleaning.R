library(tidyverse)
library(psych)


raw_df <- read_csv("trust1_raw.csv") %>%
  filter(Finished == "TRUE")

flow_id <- read_csv("trust1_flow_id.csv") ##make sense of the qualtrics randomization codings


cleaner <- function(df, key){
  
  #Pet Attitude Scale items to reverse
  #NOTE - we adapted the scale and only used 17/18 total items
  cols_to_reverse <- c("PAS_RATE_4", "PAS_RATE_6", "PAS_RATE_9", "PAS_RATE_12", "PAS_RATE_14", "PAS_RATE_16")
  
  ##Parse apart the qualtrics randomizaiton columns
  df_separated <- df %>%
    separate(FL_419_DO, into = c("first_u", "zecond_u", "third_u", "fourth_u"), sep = "\\|") %>% #parse out the two order columns
    separate(FL_426_DO, into = c("first_d", "zecond_d", "third_d", "fourth_d"), sep = "\\|") %>%
    mutate(across(everything(), ~ if_else(. %in% key$flow_id, 
                                          key$key[match(. , key$flow_id)], .))) %>% #replace values based on the flow_id key
    rename_with(~ if_else(. %in% key$flow_id, 
                          key$key[match(., key$flow_id)], .), .cols = everything()) %>%  #replace colnames based on the flow_id key
    mutate(across(starts_with("PAS_RATE_"), ~ str_remove(., "[^0-9.].*")), 
           across(starts_with("PAS_RATE_"), ~ as.numeric(as.character(.)))) %>%
    mutate(across(all_of(cols_to_reverse), 
                  ~ (as.numeric(as.character(.)) - 8) * -1)) %>% # Convert to numeric and reverse code
    mutate(PAS_sum = rowSums(select(.,PAS_RATE_1:PAS_RATE_17)), 
           across(starts_with("BASE_ATTENTION_"), ~as.numeric(.)), 
           across(starts_with("PUSH_ATTENTION_"), ~as.numeric(.))) %>%
    mutate(base_attn = rowSums(select(.,BASE_ATTENTION_1:BASE_ATTENTION_4)),
           push_attn = rowSums(select(.,PUSH_ATTENTION_1:PUSH_ATTENTION_4))) %>%
    mutate(across(everything(), ~ na_if(., -1))) %>%
    mutate(original_sample = if_else(str_starts(StartDate, "3"), "YES",
                                     if_else(str_starts(StartDate, "2"), "NO", NA_character_))) %>%
    select(ParticipantID, original_sample, S1B_RATE_1:Knowledgeable, Experience_Rate, Push_Direction, B_Scenario, R_Scenario, E_Scenario, C_Scenario, 
           first_u:push_attn)
  
  ##If we go by original attention check exclusion criteria, we exclude > 15% of participants
  #let's go by problem based on pre-registration
  
  df_separated2 <- df_separated %>%
    mutate(across(starts_with("S1"), 
                  ~ if_else(BASE_ATTENTION_1 == 1 & PUSH_ATTENTION_1 == 1, ., NA_character_),
                  .names = "{.col}"), 
           across(starts_with("S2"), 
                  ~ if_else(BASE_ATTENTION_2 == 1 & PUSH_ATTENTION_2 == 1, ., NA_character_),
                  .names = "{.col}"), 
           across(starts_with("S3"), 
                  ~ if_else(BASE_ATTENTION_3 == 1 & PUSH_ATTENTION_3 == 1, ., NA_character_),
                  .names = "{.col}"), 
           across(starts_with("S4"), 
                  ~ if_else(BASE_ATTENTION_4 == 1 & PUSH_ATTENTION_4 == 1, ., NA_character_),
                  .names = "{.col}")
    )
  
  
    return(df_separated2)
  
}

separated_df <- cleaner(raw_df, flow_id)


by_subscale <- function(df_separated){


##Average across the MDMT V1 subscales (4 items per subscale)

##reliable subscale = 1 (reliable), 5 (predictable), 9 (someone you can count on), 13 (consistent)

##capable subscale = 3 (capable), 7 (skilled), 11 (competent), 15 (meticulous)

##ethical subscale = 4 (ethical), 8 (respectable), 12 (principled), 16 (has integrity)

##sincere subscale = 2 (sincere), 6 (genuine), 10 (candid), 14 (Authentic)

baseline_scores <- df_separated %>%
  pivot_longer(cols = starts_with("S"), 
               names_to = c("scenario", "question"), 
               names_sep = "_RATE_") %>%
  mutate(value = as.numeric(value)) %>%
  mutate(reliable = case_when(question %in% c("1", "5", "9", "13") ~ value, TRUE ~ NA_real_), 
         capable = case_when(question %in% c("3", "7", "11", "15") ~ value, TRUE ~ NA_real_), 
         ethical = case_when(question %in% c("4", "8", "12", "16") ~ value, TRUE ~ NA_real_), 
         sincere = case_when(question %in% c("2", "6", "10", "14") ~ value, TRUE ~ NA_real_)) %>%
  group_by(scenario, ParticipantID) %>%
  summarise(reliable_score = mean(reliable, na.rm = TRUE),
            capable_score = mean(capable, na.rm = TRUE),
            ethical_score = mean(ethical, na.rm = TRUE),
            sincere_score = mean(sincere, na.rm = TRUE)) 

push_scores <- df_separated %>%
  select(!starts_with("PUSH_ATTENTION")) %>%
  select(!push_attn) 

push_scores2 <- push_scores %>%
  pivot_longer(cols = starts_with("PUSH_"), 
               names_to = c("scenario", "question"), 
               names_sep = "_Rate_") %>%
  filter(scenario !="Push_Direction") %>%
  mutate(value = as.numeric(value)) %>%
  mutate(reliable = case_when(question %in% c("1", "5", "9", "13") ~ value, TRUE ~ NA_real_), 
         capable = case_when(question %in% c("3", "7", "11", "15") ~ value, TRUE ~ NA_real_), 
         ethical = case_when(question %in% c("4", "8", "12", "16") ~ value, TRUE ~ NA_real_), 
         sincere = case_when(question %in% c("2", "6", "10", "14") ~ value, TRUE ~ NA_real_)) %>%
  group_by(scenario, ParticipantID) %>%
  summarise(push_reliable_score = mean(reliable, na.rm = TRUE),
            push_capable_score = mean(capable, na.rm = TRUE),
            push_ethical_score = mean(ethical, na.rm = TRUE),
            push_sincere_score = mean(sincere, na.rm = TRUE))
  


for_combining_b <- baseline_scores %>%
  mutate(scenario_num = str_sub(scenario, start = 2, end = 2),
    scenario = str_sub(scenario, start = 3)) %>%
  filter(!is.na(reliable_score)&!is.na(capable_score) & !is.na(ethical_score) &
           !is.na(sincere_score))


for_combining_p <- push_scores2 %>%
  mutate(scenario = str_sub(scenario, start = 6)) %>%
  filter(!is.na(push_reliable_score)&!is.na(push_capable_score) & !is.na(push_ethical_score) &
           !is.na(push_sincere_score))


scores <- right_join(for_combining_b, for_combining_p, 
                     by = c("ParticipantID", "scenario")) %>%
  mutate(delta_R = push_reliable_score - reliable_score, 
         delta_C = push_capable_score - capable_score, 
         delta_E = push_ethical_score - ethical_score, 
         delta_S = push_sincere_score - sincere_score)

direction <- df_separated %>%
  select(ParticipantID, Push_Direction)

for_analysis <- left_join(scores, direction, by = "ParticipantID") %>%
  pivot_longer(cols = starts_with("delta")) %>%
  mutate(measured_dimension = str_sub(name, start = 7), 
         pushed_dimension = replace(scenario, scenario == "B", "S"), 
         change_magnitude = abs(value)) %>%
  mutate(measured_dimension = as.factor(measured_dimension), 
         pushed_dimension = as.factor(pushed_dimension), 
         ParticipantID = as.factor(ParticipantID), 
         Push_Direction = as.factor(Push_Direction))

temp <-df_separated%>%
  select(PAS_sum, ParticipantID, Knowledgeable, Experience_Rate, original_sample)


temp2 <- df_separated %>%
  select(ParticipantID, first_d:fourth_d, first_u:fourth_u) %>%
  mutate(
    first  = coalesce(first_d, first_u),
    second = coalesce(zecond_d, zecond_u),
    third  = coalesce(third_d, third_u),
    fourth = coalesce(fourth_d, fourth_u)
  ) %>%
  select(ParticipantID, first:fourth) %>%
  pivot_longer(cols = !ParticipantID, 
               names_to = "scenario_order", 
               values_to = "scenario") %>%
  pivot_wider(
              names_from = "scenario", 
              values_from = "scenario_order")

finalized <- merge(for_analysis, temp)

finalized2 <- merge(finalized, temp2) %>%
filter(!is.na(reliable_score)&!is.na(capable_score) & !is.na(ethical_score) &
         !is.na(sincere_score)) %>%
  mutate(scenario_order = case_when(
    scenario == "B" ~ B,
    scenario == "E" ~ E,
    scenario == "C" ~ C,
    scenario == "R" ~ R,
    TRUE ~ NA_character_
  )) %>%
  mutate(scenario = replace(scenario, scenario == "B", "S")) %>%
  select(ParticipantID:original_sample, scenario_order)

return(finalized2)

}

analysis_df <- by_subscale(separated_df)

  
write_csv(analysis_df, "trust_exp1_data.csv")

#-------------------------------------------------------

flow_id2 <- read_csv("trust2_flow_id.csv")

exp2 <- read_csv("trust2_raw.csv") %>%
  filter(exclude == "N") %>%
  filter(T_Scenario != 1) %>%
  separate(FL_419_DO, into = c("first_u", "zecond_u", "third_u", "fourth_u", "fifth_u"), sep = "\\|") %>% #parse out the two order columns
  separate(FL_745_DO, into = c("first_d", "zecond_d", "third_d", "fourth_d", "fifth_d"), sep = "\\|") %>%
  mutate(across(first_u:fifth_u, ~ flow_id2$key[match(., flow_id2$flow_id)])) %>%
  mutate(across(first_d:fifth_d, ~ flow_id2$key[match(., flow_id2$flow_id)])) %>%
  select(!c(B, R, C, E, T)) %>%
  mutate(
    first  = coalesce(first_d, first_u),
    second = coalesce(zecond_d, zecond_u),
    third  = coalesce(third_d, third_u),
    fourth = coalesce(fourth_d, fourth_u), 
    fifth = coalesce(fifth_d, fifth_u)
  ) %>%
  pivot_longer(cols = first:fifth, names_to = "position", values_to = "value") %>%
  pivot_wider(names_from = value, values_from = position) 



##original version had a typo in scenario 1T - replace participants who saw that version AND collect remainder of the data
exp2_replacement <- read_csv("replacement_trust_data.csv") %>%
  filter(exclude == "N") %>%
  separate(FL_419_DO, into = c("first_u", "zecond_u", "third_u", "fourth_u", "fifth_u"), sep = "\\|") %>% #parse out the two order columns
  separate(FL_745_DO, into = c("first_d", "zecond_d", "third_d", "fourth_d", "fifth_d"), sep = "\\|") %>%
  mutate(across(first_u:fifth_u, ~ flow_id2$key[match(., flow_id2$flow_id)])) %>%
  mutate(across(first_d:fifth_d, ~ flow_id2$key[match(., flow_id2$flow_id)])) %>%
  select(!c(B, R, C, E, T)) %>%
  mutate(
    first  = coalesce(first_d, first_u),
    second = coalesce(zecond_d, zecond_u),
    third  = coalesce(third_d, third_u),
    fourth = coalesce(fourth_d, fourth_u), 
    fifth = coalesce(fifth_d, fifth_u)
  ) %>%
  pivot_longer(cols = first:fifth, names_to = "position", values_to = "value") %>%
  pivot_wider(names_from = value, values_from = position) 

fixer <- function(df, df2){
  
exp2_complete <- bind_rows(df, df2) %>%
  mutate(across(everything(), ~ na_if(., -1)))


exp2_inc <- exp2_complete %>%
  mutate(across(starts_with("BASE_ATTENTION_"), ~as.numeric(.)), 
                across(starts_with("PUSH_ATTENTION_"), ~as.numeric(.))) %>%
  mutate(base_attn = rowSums(select(.,BASE_ATTENTION_1:BASE_ATTENTION_5), na.rm = TRUE),
         push_attn = rowSums(select(.,PUSH_ATTENTION_1:PUSH_ATTENTION_5), na.rm = TRUE)) %>%
  filter(push_attn >= 1 & base_attn >= 1)


exp2_num <- exp2_inc %>%
  mutate(across(starts_with("BASE_"), ~as.numeric(.)))


df_separated2 <- exp2_num %>%
  mutate(across(starts_with("BASE_1"), 
                ~ if_else(BASE_ATTENTION_1 == 1 & PUSH_ATTENTION_1 == 1, ., as.double(NA)),
                .names = "{.col}"), 
         across(starts_with("BASE_2"), 
                ~ if_else(BASE_ATTENTION_2 == 1 & PUSH_ATTENTION_2 == 1, ., as.double(NA)),
                .names = "{.col}"), 
         across(starts_with("BASE_3"), 
                ~ if_else(BASE_ATTENTION_3 == 1 & PUSH_ATTENTION_3 == 1, ., as.double(NA)),
                .names = "{.col}"), 
         across(starts_with("BASE_4"), 
                ~ if_else(BASE_ATTENTION_4 == 1 & PUSH_ATTENTION_4 == 1, ., as.double(NA)),
                .names = "{.col}"),
         across(starts_with("BASE_5"), 
                ~ if_else(BASE_ATTENTION_5 == 1 & PUSH_ATTENTION_5 == 1, ., as.double(NA)),
                .names = "{.col}"))


baseline_scores <- df_separated2 %>%
  pivot_longer(cols = starts_with("BASE_"), 
               names_to = c("type", "scenario", "question"), 
               names_sep = "_") %>%
  mutate(value = as.numeric(value)) %>%
  mutate(reliable = case_when(question %in% c("Reliable", "Dependable", "Consistent") ~ value, TRUE ~ NA_real_), 
         ethical = case_when(question %in% c("Ethical", "Principled", "Moral") ~ value, TRUE ~ NA_real_), 
         competent = case_when(question %in% c("Competent", "Capable", "Skilled") ~ value, TRUE ~ NA_real_), 
         transparent = case_when(question %in% c("Transparent", "Genuine", "Authentic") ~ value, TRUE ~ NA_real_),
         benevolent = case_when(question %in% c("Benevolent", "Kind", "Considerate" ) ~ value, TRUE ~ NA_real_)) %>%
  group_by(scenario, ParticipantID) %>%
  summarise(reliable_score = mean(reliable, na.rm = TRUE),
            competent_score = mean(competent, na.rm = TRUE),
            ethical_score = mean(ethical, na.rm = TRUE),
            transparent_score = mean(transparent, na.rm = TRUE),
            benevolent_score = mean(benevolent, na.rm = TRUE)) %>%
  filter(!is.na(reliable_score)&!is.na(competent_score) & !is.na(ethical_score) &
           !is.na(transparent_score) & !is.na(benevolent_score)) %>%
  distinct() %>%
  mutate(
    scenario_num = str_extract(scenario, "\\d+") %>% as.integer(),
    scenario = str_extract(scenario, "[A-Za-z]")
  )

  

push_scores <- df_separated2 %>%
  pivot_longer(cols = starts_with("PUSH_RATE"), 
               names_to = c("type","category", "scenario", "question"), 
               names_sep = "_") %>%
  mutate(value = as.numeric(value)) %>%
  mutate(reliable = case_when(question %in% c("Reliable", "Dependable", "Consistent") ~ value, TRUE ~ NA_real_), 
         ethical = case_when(question %in% c("Ethical", "Principled", "Moral") ~ value, TRUE ~ NA_real_), 
         competent = case_when(question %in% c("Competent", "Capable", "Skilled") ~ value, TRUE ~ NA_real_), 
         transparent = case_when(question %in% c("Transparent", "Genuine", "Authentic") ~ value, TRUE ~ NA_real_),
         benevolent = case_when(question %in% c("Benevolent", "Kind", "Considerate" ) ~ value, TRUE ~ NA_real_)) %>%
  group_by(scenario, ParticipantID) %>%
  summarise(Agent = Agent,Experience = Experience, Knowledge = Knowledge,Push_Direction,
    push_reliable_score = mean(reliable, na.rm = TRUE),
            push_competent_score = mean(competent, na.rm = TRUE),
            push_ethical_score = mean(ethical, na.rm = TRUE),
            push_transparent_score = mean(transparent, na.rm = TRUE),
            push_benevolent_score = mean(benevolent, na.rm = TRUE)) %>%
  filter(!is.na(push_reliable_score)&!is.na(push_competent_score) & !is.na(push_ethical_score) &
           !is.na(push_transparent_score) & !is.na(push_benevolent_score)) %>%
  distinct()


scores2 <- right_join(baseline_scores, push_scores, 
                     by = c("ParticipantID", "scenario")) %>%
  ungroup() %>%
  mutate(delta_R = push_reliable_score - reliable_score, 
         delta_C = push_competent_score - competent_score, 
         delta_E = push_ethical_score - ethical_score, 
         delta_T = push_transparent_score - transparent_score, 
         delta_B = push_benevolent_score - benevolent_score) %>%
  pivot_longer(cols = starts_with("delta")) %>%
  mutate(measured_dimension = str_sub(name, start = 7), 
         pushed_dimension = scenario, 
         change_magnitude = abs(value)) 



  

scenario_order <- df_separated2 %>%
  select(ParticipantID, E:B)



final_df2<- scores2 %>%
  left_join(scenario_order, by = "ParticipantID") %>%
  rowwise() %>%
  mutate(scenario_order = get(scenario)) %>%
  ungroup() %>%
  select(!c(B, E, R, T, C)) %>%
  mutate(measured_dimension = as.factor(measured_dimension), 
         pushed_dimension = as.factor(pushed_dimension), 
         ParticipantID = as.factor(ParticipantID), 
         Push_Direction = as.factor(Push_Direction)) %>%
  unique() %>%
  filter(!is.na(scenario_num))

return(final_df2)

}


analysis2_df <- fixer(exp2, exp2_replacement)

write_csv(analysis2_df, "trust_exp2_data.csv")

