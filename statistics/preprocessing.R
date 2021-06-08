# libraries
library(tidyverse)

### Load Data (Main)
#----------------------------------------------------------

data <- read.csv("../data/rawdata_10May.csv",
                 na.string = c(" ", "NA"),
                 stringsAsFactors = FALSE)[-c(1, 2), -c(2:3, 5, 8:17, 21:22)] %>% 
  
  # Filter only completed data
  filter(Q17.3 == "I confirm that I have read and understood which news information are fake and which are real.") %>% 
  
  # Rename
  rename(
    ID = id,
    Date = StartDate,
    Duration = Duration..in.seconds.,
    Consent = Q1.2,
    Order_Questionnaires = FL_9_DO,
    Order_News = NEWSHEADLINES_DO,
    System_Screen = Q2.2_Resolution,
    System_Device = Q2.2_Operating.System,
    AttentionCheck_1 = Q10.12,
    AttentionCheck_2 = Q14.1,
    Gov_Effective = Q14.2_1, 
    Gov_Trust = Q14.2_2,
    Gov_Transparency = Q14.2_3,
    Compliance_WashHands = Q14.3_1,
    Compliance_StayHome = Q14.3_2,
    Compliance_Distancing = Q14.3_3,
    Compliance_Face = Q14.3_4,
    Compliance_Mask = Q14.3_5,
    Belief_WashHands = Q14.4_1,
    Belief_StayHome = Q14.4_2,
    Belief_Distancing = Q14.4_3,
    Belief_Face = Q14.4_4,
    Belief_Mask = Q14.4_5,
    Gender = Q15.1,
    Age = Q15.2_3,
    Country_Residence = Q15.3,
    Country_Citizenship = Q15.4,
    English_Fluency = Q15.5_1,
    Student = Q15.6,
    Education_Achieved = Q15.7,
    Education_Pursuing = Q15.8,
    Ethnicity = Q15.10,
    Religion_Type = Q15.11,
    Religion_Faith = Q15.12_1,
    Religion_Engagement = Q15.13_4,
    Household_Income = Q15.14_4,
    Household_Members = Q15.14_5,
    COVID_Diagnosis = Q16.1,
    COVID_IncomeStability = Q16.2_1,
    COVID_MentalHealth = Q16.2_2,
    COVID_PhysicalHealth = Q16.2_3,
    COVID_Relationships = Q16.2_4,
    COVID_SpiritualHealth = Q16.2_5
  ) %>% 
  
  # Keep only 1 unique (first) response for ID with 2 responses
  distinct(ID, .keep_all = TRUE) %>%

  # Remove duplicated IP Addresses 
  distinct(IPAddress, .keep_all = TRUE) %>%
  
  # Combine OTHERS responses
  mutate(
    Education_Achieved = ifelse(Education_Achieved != "Others", Education_Achieved, Q15.7_8_TEXT),
    Education_Pursuing = ifelse(Education_Pursuing != "Others", Education_Pursuing, Q15.8_10_TEXT),
    Ethnicity = ifelse(Ethnicity != "Others", Ethnicity, Q15.10_4_TEXT),
    Religion_Type = ifelse(Religion_Type != "Others", Religion_Type, Q15.11_6_TEXT),
  ) %>%
  
  mutate(
    Incentive = FALSE
  ) %>%
  
  # Mutate other columns
  mutate(
    Income = round(as.numeric(Household_Income)/as.numeric(Household_Members), 2),
    Duration = as.numeric(Duration)/60,
    Age = 2020 - as.numeric(Age)
  ) %>% 
  mutate(Education_A = ifelse(Education_Achieved == "PSLE", -6,
                     ifelse(Education_Achieved == "O/N Levels", -2,
                     ifelse(Education_Achieved %in% c("A Levels", "Diploma"), 0,
                     ifelse(Education_Achieved == "Bachelors Degree", 4,
                     ifelse(Education_Achieved == "Masters Degree or similar", 6,
                     ifelse(Education_Achieved == "Doctoral Degree or similar", 10, NA))))))) %>%
  mutate(Education_P = ifelse(Education_Pursuing == "O/N Levels", -4,
                       ifelse(Education_Pursuing %in% c("A Levels", "Diploma"), -2,
                       ifelse(Education_Pursuing == "Bachelors Degree", 2,
                       ifelse(Education_Pursuing == "Masters Degree or similar", 5,
                       ifelse(Education_Pursuing == "Doctoral Degree or similar", 8, NA)))))) %>% 
  mutate(Education = coalesce(Education_A, Education_P)) %>% 

  # Screen resolution
  separate(System_Screen, c("Screen_Width", "Screen_Height"), sep = "x") %>%
  mutate(System_Screen = as.numeric(Screen_Width) * as.numeric(Screen_Height)) %>%

  # Device
  mutate(System_Device = ifelse(str_detect(System_Device, "iPhone|Android"), "Phone",
                                ifelse(str_detect(System_Device, "Windows|Macintosh|CrOS"), "Computer", "Tablet"))) %>%

  # Date of completion
  mutate(Date = as.Date(Date, format = "%d/%m/%Y")) %>% 
  
  # Remove irrelevant columns
  select(-ends_with("_TEXT"), -starts_with("Household_"), -starts_with("Screen_"),
         -Education_A, -Education_P, -Q17.3, -Q1.3, -IPAddress)






### Load Data (SONA)
#----------------------------------------------------------
data_SONA <- read.csv("../data/rawdata_SONA.csv",
                      na.string = c("", "NA"),
                      stringsAsFactors = FALSE)[-c(1, 2), -c(2:3, 5, 8:17, 19:20)] %>% 

  # Filter only completed data
  filter(Q16.3 == "I confirm that I have read and understood which news information are fake and which are real.") %>%
  
  
  # Rename
  rename(
    ID = id,
    Date = StartDate,
    Duration = Duration..in.seconds.,
    Consent = Q1.2,
    Order_Questionnaires = FL_8_DO,
    Order_News = NEWSHEADLINES_DO,
    System_Screen = Q2.2_Resolution,
    System_Device = Q2.2_Operating.System,
    AttentionCheck_1 = Q9.12,
    AttentionCheck_2 = Q13.1,
    Gov_Effective = Q13.2_1, 
    Gov_Trust = Q13.2_2,
    Gov_Transparency = Q13.2_3,
    Compliance_WashHands = Q13.3_1,
    Compliance_StayHome = Q13.3_2,
    Compliance_Distancing = Q13.3_3,
    Compliance_Face = Q13.3_4,
    Compliance_Mask = Q13.3_5,
    Belief_WashHands = Q13.4_1,
    Belief_StayHome = Q13.4_2,
    Belief_Distancing = Q13.4_3,
    Belief_Face = Q13.4_4,
    Belief_Mask = Q13.4_5,
    Gender = Q14.1,
    Age = Q14.2_1,
    Country_Residence = Q14.3,
    Country_Citizenship = Q14.4,
    English_Fluency = Q14.5_1,
    Student = Q14.6,
    Education_Achieved = Q14.7,
    Education_Pursuing = Q14.8,
    Ethnicity = Q14.10,
    Religion_Type = Q14.11,
    Religion_Faith = Q14.12_1,
    Religion_Engagement = Q14.13_4,
    Household_Income = Q14.14_4,
    Household_Members = Q14.14_5,
    COVID_Diagnosis = Q15.1,
    COVID_IncomeStability = Q15.2_1,
    COVID_MentalHealth = Q15.2_2,
    COVID_PhysicalHealth = Q15.2_3,
    COVID_Relationships = Q15.2_4,
    COVID_SpiritualHealth = Q15.2_5
  ) %>% 
  
  # Keep only 1 unique (first) response for ID with 2 responses
  distinct(ID, .keep_all = TRUE) %>%
  
  # Remove duplicated IP Addresses 
  distinct(IPAddress, .keep_all = TRUE) %>%
  
  # Combine OTHERS responses
  mutate(
    Education_Achieved = ifelse(Education_Achieved != "Others", Education_Achieved, Q14.7_8_TEXT),
    Education_Pursuing = ifelse(Education_Pursuing != "Others", Education_Pursuing, Q14.8_10_TEXT),
    Ethnicity = ifelse(Ethnicity != "Others", Ethnicity, Q14.10_4_TEXT),
    Religion_Type = ifelse(Religion_Type != "Others", Religion_Type, Q14.11_6_TEXT),
  ) %>% 
  
  mutate(
    Incentive = TRUE,
  ) %>%
  
  # Mutate other columns
  mutate(
    Income = round(as.numeric(Household_Income)/as.numeric(Household_Members), 2),
    Duration = as.numeric(Duration)/60,
    Age = 2020 - as.numeric(Age)
  ) %>% 
  # mutate(Education_A = ifelse(Education_Achieved == "PSLE", -6,
  #                             ifelse(Education_Achieved == "O/N Levels", -2,
  #                                    ifelse(Education_Achieved %in% c("A Levels", "Diploma"), 0,
  #                                           ifelse(Education_Achieved == "Bachelors Degree", 4,
  #                                                  ifelse(Education_Achieved == "Masters Degree or similar", 6,
  #                                                         ifelse(Education_Achieved == "Doctoral Degree or similar", 10, NA))))))) %>%
  mutate(Education = ifelse(Education_Pursuing == "O/N Levels", -4,
                              ifelse(Education_Pursuing %in% c("A Levels", "Diploma"), -2,
                                     ifelse(Education_Pursuing == "Bachelors Degree", 2,
                                            ifelse(Education_Pursuing == "Masters Degree or similar", 5,
                                                   ifelse(Education_Pursuing == "Doctoral Degree or similar", 8, NA)))))) %>% 

  # Screen resolution
  separate(System_Screen, c("Screen_Width", "Screen_Height"), sep = "x") %>%
  mutate(System_Screen = as.numeric(Screen_Width) * as.numeric(Screen_Height)) %>%
  
  # Device
  mutate(System_Device = ifelse(str_detect(System_Device, "iPhone|Android"), "Phone",
                                ifelse(str_detect(System_Device, "Windows|Macintosh|CrOS"), "Computer", "Tablet"))) %>%
  
  # Date of completion
  mutate(Date = as.Date(Date)) %>% 
  
  # Remove irrelevant columns
  select(-ends_with("_TEXT"), -starts_with("Household_"), -starts_with("Screen_"), -Q16.3, -IPAddress)





### Load Data (2nd Round of Dissemination, after 10 June)
#----------------------------------------------------------

data_2 <- read.csv("../data/rawdata_9July.csv",
                   na.string = c("", "NA"),
                   stringsAsFactors = FALSE)[-c(1, 2), -c(2:3, 5, 8:17, 21:22)] %>% 
  
  # Filter only completed data
  filter(Q17.3 == "I confirm that I have read and understood which news information are fake and which are real.") %>%
  
  # Rename
  rename(
    ID = id,
    Date = StartDate,
    Duration = Duration..in.seconds.,
    Consent = Q1.2,
    Order_Questionnaires = FL_9_DO,
    Order_News = NEWSHEADLINES_DO,
    System_Screen = Q2.2_Resolution,
    System_Device = Q2.2_Operating.System,
    AttentionCheck_1 = Q10.12,
    AttentionCheck_2 = Q14.1,
    Gov_Effective = Q14.2_1, 
    Gov_Trust = Q14.2_2,
    Gov_Transparency = Q14.2_3,
    Compliance_WashHands = Q14.3_1,
    Compliance_StayHome = Q14.3_2,
    Compliance_Distancing = Q14.3_3,
    Compliance_Face = Q14.3_4,
    Compliance_Mask = Q14.3_5,
    Belief_WashHands = Q14.4_1,
    Belief_StayHome = Q14.4_2,
    Belief_Distancing = Q14.4_3,
    Belief_Face = Q14.4_4,
    Belief_Mask = Q14.4_5,
    Gender = Q15.1,
    Age = Q15.2_3,
    Country_Residence = Q15.3,
    Country_Citizenship = Q15.4,
    English_Fluency = Q15.5_1,
    Student = Q15.6,
    Education_Achieved = Q15.7,
    Education_Pursuing = Q15.8,
    Ethnicity = Q15.10,
    Religion_Type = Q15.11,
    Religion_Faith = Q15.12_1,
    Religion_Engagement = Q15.13_4,
    Household_Income = Q15.14_4,
    Household_Members = Q15.14_5,
    COVID_Diagnosis = Q16.1,
    COVID_IncomeStability = Q16.2_1,
    COVID_MentalHealth = Q16.2_2,
    COVID_PhysicalHealth = Q16.2_3,
    COVID_Relationships = Q16.2_4,
    COVID_SpiritualHealth = Q16.2_5
  ) %>% 
  
  # Keep only 1 unique (first) response for ID with 2 responses
  distinct(ID, .keep_all = TRUE) %>%

  # Remove duplicated IP Addresses 
  distinct(IPAddress, .keep_all = TRUE) %>%
  
  # Combine OTHERS responses
  mutate(
    Education_Achieved = ifelse(Education_Achieved != "Others", Education_Achieved, Q15.7_8_TEXT),
    Education_Pursuing = ifelse(Education_Pursuing != "Others", Education_Pursuing, Q15.8_10_TEXT),
    Ethnicity = ifelse(Ethnicity != "Others", Ethnicity, Q15.10_4_TEXT),
    Religion_Type = ifelse(Religion_Type != "Others", Religion_Type, Q15.11_6_TEXT),
  ) %>% 
  
  mutate(
    Incentive = TRUE,
  ) %>%

  # Mutate other columns
  mutate(
    Income = round(as.numeric(Household_Income)/as.numeric(Household_Members), 2),
    Duration = as.numeric(Duration)/60,
    Age = 2020 - as.numeric(Age)
  ) %>% 
  mutate(Education_A = ifelse(Education_Achieved == "PSLE", -6,
                              ifelse(Education_Achieved == "O/N Levels", -2,
                                     ifelse(Education_Achieved %in% c("A Levels", "Diploma"), 0,
                                            ifelse(Education_Achieved == "Bachelors Degree", 4,
                                                   ifelse(Education_Achieved == "Masters Degree or similar", 6,
                                                          ifelse(Education_Achieved == "Doctoral Degree or similar", 10, NA))))))) %>%
  mutate(Education_P = ifelse(Education_Pursuing == "O/N Levels", -4,
                              ifelse(Education_Pursuing %in% c("A Levels", "Diploma"), -2,
                                     ifelse(Education_Pursuing == "Bachelors Degree", 2,
                                            ifelse(Education_Pursuing == "Masters Degree or similar", 5,
                                                   ifelse(Education_Pursuing == "Doctoral Degree or similar", 8, NA)))))) %>% 
  mutate(Education = coalesce(Education_A, Education_P)) %>% 

  # Manual mutate due to typos
  mutate(Age = ifelse(Age < 0, NA,
                      ifelse(Age > 1000, 2020 - Age, Age))) %>% 

  # Screen resolution
  separate(System_Screen, c("Screen_Width", "Screen_Height"), sep = "x") %>%
  mutate(System_Screen = as.numeric(Screen_Width) * as.numeric(Screen_Height)) %>%
  
  # Device
  mutate(System_Device = ifelse(str_detect(System_Device, "iPhone|Android"), "Phone",
                                ifelse(str_detect(System_Device, "Windows|Macintosh|CrOS"), "Computer", "Tablet"))) %>%
  
  # Date of completion
  mutate(Date = as.Date(Date)) %>% 
  
  # Remove irrelevant columns
  select(-ends_with("_TEXT"), -starts_with("Household_"), -starts_with("Screen_"),
         -Education_A, -Education_P, -Q17.3, -Q1.3, -IPAddress)


## Extract emails for lucky draw
#----------------------------------------------------------
# emails <- data.frame(data_2$email_1) %>%
#   drop_na() %>% 
#   rename(Emails = data_2.email_1)
# write.csv(emails, "../luckydraw/emails.csv", row.names = FALSE, na = "")




### Append Data
library(dplyr)
colnames(data_SONA) <- colnames(data)
data_2 <- select(data_2, -email_1, -email_2)

df <- bind_rows(data, data_SONA, data_2)





### Import questionnaire labels
#----------------------------------------------------------

master <- read.csv("question_labels.csv", stringsAsFactors = FALSE, na.string = "") %>%
  rename(Questionnaire = ï..Questionnaire) %>% 
  mutate(Item_Name = paste0(Questionnaire, "_", Item))

# Rename df cols to questionnaire labels
## CMQ
names(df)[str_detect(names(df), "Q6.[:digit:]")] <- master$Item_Name[str_detect(master$Item_Name, "CMQ")]
## PBS
names(df)[str_detect(names(df), "Q7.[:digit:]")] <- master$Item_Name[str_detect(master$Item_Name, "PBS")]
## GPTS
names(df)[str_detect(names(df), "Q8.[:digit:]")] <- master$Item_Name[str_detect(master$Item_Name, "GPTS")]
## BSCS
names(df)[str_detect(names(df), "Q9.[:digit:]")] <- master$Item_Name[str_detect(master$Item_Name, "BSCS")]
## SPQ
names(df)[str_detect(names(df), "Q10.[:digit:]")] <- master$Item_Name[str_detect(master$Item_Name, "SPQ")]
## IUS
names(df)[str_detect(names(df), "Q11.[:digit:]")] <- master$Item_Name[str_detect(master$Item_Name, "IUS")]
## MAIA2
names(df)[str_detect(names(df), "Q12.[:digit:]")] <- master$Item_Name[str_detect(master$Item_Name, "MAIA2")]

# Manually append News items labels
names(df)[str_detect(names(df), "Q3.[:digit:]")] <- paste0("News_", master$Item[1:66], "_", master$Category[1:66]) 

# Manually append News-related labels
Consumption_labels <- c("Frequency_General", "Frequency_COVID", "Source_Newspaper", "Source_SocialNetworks", "Source_OfficialSites", "Source_SharedLinks")
names(df)[str_detect(names(df), "Q5.[:digit:]")] <- Consumption_labels
  
# Manually append PANAS labels
PANAS_labels <- c("Distressed", "Upset", "Guilty", "Scared", "Hostile", "Irritable", "Ashamed", "Nervous", "Jittery", "Afraid")
PANAS_state <- paste0(master$Item_Name[str_detect(master$Item_Name, "PANAS_1")], "_", PANAS_labels) %>% 
  str_replace("PANAS_1", "PANAS_State")
PANAS_trait <- paste0(master$Item_Name[str_detect(master$Item_Name, "PANAS_2")], "_", PANAS_labels) %>% 
  str_replace("PANAS_2", "PANAS_Trait")

names(df)[str_detect(names(df), "Q13.[:digit:]")] <- c(PANAS_state, PANAS_trait)







### Merge dataset with Nottingham Trent Uni (last update: 27Oct2020)
#------------------------------------------

data_NTU <- read.csv("../data/rawdata_27Oct_NTU.csv",
                     na.string = c("", "NA"),
                     stringsAsFactors = FALSE)[,-c(2:3, 5, 8:17, 20:21)] %>% 

  # Filter only completed data
  filter(Q167 == "I confirm that I have read and understood which news information are fake and which are real.") %>%
  
  # Rename
  rename(
    ID = Q174,
    Date = StartDate,
    Duration = Duration..in.seconds.,
    Consent = Q4,
    # Order_Questionnaires = FL_9_DO,
    # Order_News = NEWSHEADLINES_DO,
    System_Screen = Q164_Resolution,
    System_Device = Q164_Operating.System,
    AttentionCheck_1 = Q61,
    AttentionCheck_2 = Q155,
    Gov_Effective = Q156_1, 
    Gov_Trust = Q156_2,
    Gov_Transparency = Q156_3,
    Compliance_WashHands = Q157_1,
    Compliance_StayHome = Q157_2,
    Compliance_Distancing = Q157_3,
    Compliance_Face = Q157_4,
    Compliance_Mask = Q157_5,
    Belief_WashHands = Q158_1,
    Belief_StayHome = Q158_2,
    Belief_Distancing = Q158_3,
    Belief_Face = Q158_4,
    Belief_Mask = Q158_5,
    Gender = Q6,
    Age = Q7_3,
    Country_Residence = Q8,
    Country_Citizenship = Q9,
    English_Fluency = Q10_1,
    Student = Q11,
    Education_Achieved = Q12,
    Education_Pursuing = Q13,
    Ethnicity = Q15,
    Religion_Type = Q16,
    Religion_Faith = Q17_1,
    Religion_Engagement = Q18_4,
    Household_Income = Q19_4,
    Household_Members = Q19_5,
    COVID_Diagnosis = Q161,
    COVID_IncomeStability = Q162_1,
    COVID_MentalHealth = Q162_2,
    COVID_PhysicalHealth = Q162_3,
    COVID_Relationships = Q162_4,
    COVID_SpiritualHealth = Q162_5
  ) %>% 
  
  # Keep only 1 unique (first) response for ID with 2 responses
  distinct(ID, .keep_all = TRUE) %>%
  
  # Remove duplicated IP Addresses 
  distinct(IPAddress, .keep_all = TRUE) %>%
  
  # Combine OTHERS responses
  mutate(
    Education_Achieved = ifelse(Education_Achieved != "Others", Education_Achieved, Q12_8_TEXT),
    Education_Pursuing = ifelse(Education_Pursuing != "Others", Education_Pursuing, Q13_10_TEXT),
    Ethnicity = ifelse(Ethnicity != "Others", Ethnicity, Q15_8_TEXT),
    Religion_Type = ifelse(Religion_Type != "Others", Religion_Type, Q16_9_TEXT),
  ) %>%

  mutate(
    Incentive = TRUE
  ) %>%

  # Mutate other columns
  mutate(
    Income = round(((as.numeric(Household_Income)/as.numeric(Household_Members)) * 1.76), 2),
    Duration = as.numeric(Duration)/60,
    Age = 2020 - as.numeric(Age)
  ) %>%
  mutate(Education_A = ifelse(Education_Achieved == "PSLE", -6,
                              ifelse(Education_Achieved == "O/N Levels", -2,
                                     ifelse(Education_Achieved %in% c("A Levels", "Diploma"), 0,
                                            ifelse(Education_Achieved == "Bachelors Degree", 4,
                                                   ifelse(Education_Achieved == "Masters Degree or similar", 6,
                                                          ifelse(Education_Achieved == "Doctoral Degree or similar", 10, NA))))))) %>%
  mutate(Education_P = ifelse(Education_Pursuing == "O/N Levels", -4,
                              ifelse(Education_Pursuing %in% c("A Levels", "Diploma"), -2,
                                     ifelse(Education_Pursuing == "Bachelors Degree", 2,
                                            ifelse(Education_Pursuing == "Masters Degree or similar", 5,
                                                   ifelse(Education_Pursuing == "Doctoral Degree or similar", 8, NA)))))) %>%
  mutate(Education = coalesce(Education_A, Education_P)) %>%

  # Manual mutate due to typos
  mutate(Age = ifelse(Age > 1000, 2020 - Age, Age)) %>% 

  # Screen resolution
  separate(System_Screen, c("Screen_Width", "Screen_Height"), sep = "x") %>%
  mutate(System_Screen = as.numeric(Screen_Width) * as.numeric(Screen_Height)) %>%

  # Device
  mutate(System_Device = ifelse(str_detect(System_Device, "iPhone|Android"), "Phone",
                                ifelse(str_detect(System_Device, "Windows|Macintosh|CrOS"), "Computer", "Tablet"))) %>%

  # Date of completion
  mutate(Date = as.Date(Date)) %>%

  # Remove irrelevant columns
  select(-ends_with("_TEXT"), -starts_with("Household_"), -starts_with("Screen_"),
         -Education_A, -Education_P, -Q167, -IPAddress)




### Import questionnaire labels for NTU data
#----------------------------------------------------------
master <- read.csv("question_labels_NTU.csv", stringsAsFactors = FALSE, na.string = "") %>%
  rename(Questionnaire = ï..Questionnaire) %>%
  mutate(Item_Name = paste0(Questionnaire, "_", Item))

# Manually append News items labels
names(data_NTU)[str_detect(names(data_NTU), "[:digit:]_[:digit:]")][1:66] <- paste0("News_", master$Item[1:66], "_", master$Category[1:66]) 

# Manually append News-related labels
names(data_NTU)[str_detect(names(data_NTU), "Q48_[:digit:]")] <- c("Frequency_General", "Frequency_COVID")
names(data_NTU)[str_detect(names(data_NTU), "Q49_[:digit:]")] <- c("Source_Newspaper", "Source_SocialNetworks", "Source_OfficialSites", "Source_SharedLinks")

# Manually append PANAS labels
names(data_NTU)[str_detect(names(data_NTU), "Q159_[:digit:]")] <- PANAS_state
names(data_NTU)[str_detect(names(data_NTU), "Q160_[:digit:]")] <- PANAS_trait


# Rename df cols to questionnaire labels
## Remove questions that NTU doesn't have 
data_NTU <- data_NTU %>% 
  select(-starts_with("Q17"))

names(data_NTU)[str_detect(names(data_NTU), "Q[:digit:]")] <- master$Item_Name[!str_detect(master$Item_Name, "News") & !str_detect(master$Item_Name, "PANAS")]


### Merge NTU-NTU Data
#----------------------------------------------------------
# Temporarily remove order column first
data_SG <-  select(df, -starts_with("Order")) %>% 
  add_column(Dataset = "SG")
data_NTU <- data_NTU %>% 
  add_column(Dataset = "UK")

# Indicate UK or SG Dataset
df <- bind_rows(data_SG, data_NTU)


# Final data
#----------------------------------------------------------

df[str_detect(names(df), "News_|Gov_|Compliance_|Belief_|CMQ|PBS|GPTS|BSCS|IUS|MAIA2|PANAS")]<- sapply(df[str_detect(names(df), "News_|Gov_|Compliance_|Belief_|CMQ|PBS|GPTS|BSCS|IUS|MAIA2|PANAS")],as.numeric)


df <- df %>% 
  mutate(COVID_IncomeStability = as.numeric(COVID_IncomeStability),
         COVID_MentalHealth = as.numeric(COVID_MentalHealth),
         COVID_PhysicalHealth = as.numeric(COVID_PhysicalHealth),
         COVID_Relationships = as.numeric(COVID_Relationships),
         COVID_SpiritualHealth = as.numeric(COVID_SpiritualHealth),
         Religion_Faith = as.numeric(Religion_Faith),
         Religion_Engagement = as.numeric(Religion_Engagement),
         English_Fluency = as.numeric(English_Fluency))

write.csv(df, "data2.csv", row.names = FALSE, na = "")

