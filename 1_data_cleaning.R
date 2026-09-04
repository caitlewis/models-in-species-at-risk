#### MODELS FOR SAR PROJECT 
#### CLEANING DATA

#### SCRIPT OBJECTIVE 
# In this script I clean and reshape the final, full data-set 

#### OBJECTS CREATED 

#clean_data.RDS # final data in wide format without SC or flags 
#reshaped_data.RDS # final data in long format, cleaned


#### LOAD LIBRARIES 
library(plyr)
library(tidyverse)
library(udpipe)


#### LOAD OBJECTS
data <- read.csv("data/final_dataset_may_28_26.csv",
                 stringsAsFactors = TRUE)

str(data$doc_type)

#### SPECIAL CONCERN SPECIES ####

# NOTE: using COSEWIC status vs SARA Status
#NOTE FROM JL: removing status appraisals and rapid reviews

# create new df by filtering based on status 
special_concern <- data %>% select(person_assigned,
                                   species,
                                   common_name,
                                   cosewic_status, 
                                   year_published,
                                   status_appraisal_rapid_review,
                                   taxonomic_group,
                                   criteria_used_listing_1:report_sections_additional_model_referenced) %>%
  filter(cosewic_status == "Special Concern",
         person_assigned != "Diego", # removing Diego's assigned reports 
         status_appraisal_rapid_review ==0) %>% # removing status appraisals and rapid reviews
  mutate(cosewic_status = as.factor(cosewic_status),
         taxonomic_group = as.factor(taxonomic_group),
         year_published = as.numeric(year_published))

#### Descriptive statistics ####
# there were:
nrow(special_concern) # reports with special concerned species
# with publishing year ranging from:
range(special_concern$year_published)

# Number of reports per taxonomic group
data.frame(Number_of_reports= 
             summary(special_concern$taxonomic_group))

hist(special_concern$year_published,
     main= "Publishing Year of Special Concern Reports
     in Dataset",
     xlab= "Year published")

#### ENDANGERED AND THREATENED SPECIES ####

data_no_sc <- data %>% dplyr::select(
  person_assigned,
  species,
  common_name,
  cosewic_status,
  year_published,
  taxonomic_group,
  status_appraisal_rapid_review,
  criteria_used_listing_1:report_sections_additional_model_referenced
) %>%
  # filtering OUT special concern reports
  filter(cosewic_status != "Special Concern", 
         # Removing Diego's assigned reports 
         person_assigned != "Diego",
         status_appraisal_rapid_review ==0) %>% # removing status appraisals and rapid reviews) 
  # Make sure year is numeric
  mutate(year_published = as.numeric(year_published))


#Create a unique identifier column with a reportID for each species-year
#Note from JL: not all species are unique because there are some species with multiple DUs but have the same species name (e.g. beluga whale cumberland sound population vs. beluga whale st. lawrence estuary population are both Delphinapterus leucas)). So I have changed the code below so that it uses common name, not species
data_no_sc <- data.frame(data_no_sc)
#data_no_sc$reportID <- unique_identifier(data_no_sc, fields = c("species", "year_published"), start_from = 1) <- JL: this is the old version
data_no_sc$reportID <- unique_identifier(data_no_sc, fields = c("common_name", "year_published"), start_from = 1)

# Create another unique identifier for each designatable-unit (DU)
## You need this to reshape the dataset, and for random-intercept
## This is different from reportID bc we have subpopulations within species

data_no_sc['DU'] <- row.names(data_no_sc) 

# Randomly select between duplicate species 
#Note from JL: no need to do this now, as I have removed duplicate species directly from the database; so I'm just assigning subsample as data_no_sc
# subsample <- ddply(data_no_sc, .(species), 
#                    function(species) {
#                      species[sample(nrow(species), size=1),]
#                    })
subsample<-data_no_sc #Addition from JL

# We originally had:
nrow(data_no_sc) # reports...

# After randomly selecting duplicate species we have:
nrow(subsample) # reports...


#rename the columns # I could clean this up using grep fn
subsample <- subsample %>% dplyr::select(-common_name, #addition from JL
                                         -person_assigned, 
                                         -X1_notes_on_model,
                                         -X2_notes_on_model,
                                         -X3_notes_on_model,
                                         -X4_notes_on_model,
                                         -X1_rationale_reference,
                                         -X2_rationale_reference,
                                         -X3_rationale_reference,
                                         -X4_rationale_reference) %>%
  dplyr::rename(type_x1 = X1_type_of_model_used,
                type_x2 = X2_type_of_model_used,
                type_x3= X3_type_of_model_used,
                type_x4= X4_type_of_model_used,
                used_x1= X1_model_used_listing_decision,
                used_x2= X2_model_used_listing_decision,
                used_x3= X3_model_used_listing_decision,
                used_x4= X4_model_used_listing_decision,
                species_specific_x1 = X1_model_species_specific,
                species_specific_x2 = X2_model_species_specific,
                species_specific_x3= X3_model_species_specific,
                species_specific_x4= X4_model_species_specific,
                uncertainty_reported_x1 = X1_uncertainty_reported,
                uncertainty_reported_x2 = X2_uncertainty_reported,
                uncertainty_reported_x3= X3_uncertainty_reported,
                uncertainty_reported_x4= X4_uncertainty_reported,
                uncertainty_degree_x1 = X1_degree_of_uncertainty,
                uncertainty_degree_x2= X2_degree_of_uncertainty,
                uncertainty_degree_x3= X3_degree_of_uncertainty,
                uncertainty_degree_x4 = X4_degree_of_uncertainty,
                criteria_category_x1 = X1_criteria_model_used_to_inform,
                criteria_category_x2= X2_criteria_model_used_to_inform,
                criteria_category_x3= X3_criteria_model_used_to_inform,
                criteria_category_x4= X4_criteria_model_used_to_inform,
  ) %>%
  mutate(DU = as.integer(DU))


# Remove blank rows 
remove <- filter(subsample, criteria_used_listing_2 == "")
remove <- remove$species
subsample <- subsample %>% filter(!species %in% remove)

# Check that was_model_mentioned_anywhere else is either Y or N (shouldn't be NA)
subsample <- subsample %>%
  mutate(
    was_model_mentioned_anywhere_else_fixed = if_else(is.na(was_model_mentioned_anywhere_else), "N",
                                                      as.character(was_model_mentioned_anywhere_else)))


subsample <- subsample %>% dplyr::select(-was_model_mentioned_anywhere_else) %>%
  mutate(was_model_mentioned_anywhere_else_fixed = as.factor(was_model_mentioned_anywhere_else_fixed))



#take a look at the structure, make sure everything looks okay
str(subsample)
summary(subsample)

# There are still some flags that need to be addressed...

#### RESHAPE DATA 

#use reshape function, with unique identifier "DU" to elongate the table. We want the type_x columns
#aggregated, and the used_X columns aggregated. 
#This function creates a useless time column. But we can use it to verify that there's 4
#rows per DU (sort by DU, each "time" row represents a possible model). 

#the vectors refer to indexed columns (i.e. c(5...) is referring to column #5)
#SEE: https://stackoverflow.com/questions/12466493/reshaping-multiple-sets-of-measurement-columns-wide-format-into-single-columns

#select necessary columns
reshaped_data <- subsample %>%
  dplyr::select(species, cosewic_status, year_published,
                taxonomic_group, 
                # model one:
                type_x1, used_x1, 
                species_specific_x1, uncertainty_reported_x1,
                uncertainty_degree_x1, criteria_category_x1,
                # model two:
                type_x2, used_x2,
                species_specific_x2, uncertainty_reported_x2,
                uncertainty_degree_x2, criteria_category_x2,
                # model three:
                type_x3, used_x3,
                species_specific_x3, uncertainty_reported_x3,
                uncertainty_degree_x3, criteria_category_x3,
                # model four
                type_x4, used_x4,
                species_specific_x4, uncertainty_reported_x4,
                uncertainty_degree_x4, criteria_category_x4,
  )

# reshape 
colnames(reshaped_data)

# Create a list of column names that you want to lengthen
Type <- colnames(reshaped_data[c(5,11,17,23)])
Used <- colnames(reshaped_data[c(6,12,18,24)])
Uncertainty_reported <- colnames(reshaped_data[c(8,14,20,26)])
Species_specific <- colnames(reshaped_data[c(7,13,19,25)])
Uncertainty_degree <- colnames(reshaped_data[c(9,15,21,27)])
Criteria_category <- colnames(reshaped_data[c(10,16,22,28)])


reshaped_data <- reshape(
  reshaped_data,
  idvar = "DU",
  direction = "long",
  varying = list(Type= Type,
                 Used = Used,
                 Criteria_category= Criteria_category,
                 Uncertainty_reported = Uncertainty_reported,
                 Uncertainty_degree= Uncertainty_degree,
                 Species_specific= Species_specific
  ),
  v.names = c("Type",
              "Used",
              "Criteria_category",
              "Uncertainty_reported",
              "Uncertainty_degree", 
              "Species_specific")
)

# Clean up reshaped data 

summary(reshaped_data$Used)
summary(reshaped_data$Type)
summary(reshaped_data$Uncertainty_reported)

summary(reshaped_data$Uncertainty_degree) #HIGH, LOW, MED, NRP or NA

summary(reshaped_data$Criteria_category) 
# will need to separate out list, widen so that there's a column for each letter 

str(reshaped_data)


reshaped_data <- reshaped_data %>% 
  
  # Remove useless time column
  dplyr::select(-time) %>%
  
  # Fix the different spellings, etc...
  mutate(Type= as.factor(ifelse(grepl('dist|Dist', Type), 'distributional',
                                ifelse(grepl('abun|Abun', Type), 'abundance',
                                       ifelse(grepl('demo|Demo',Type), 'demographic',
                                              ifelse(grepl('pop',Type), 'population',
                                                     ifelse(grepl('OTH|other',Type), 'other', Type)))))),
         Uncertainty_reported = as.factor(ifelse(grepl('Y', Uncertainty_reported), 'Y',
                                                 ifelse(grepl('NRP',Uncertainty_reported), 'NRP',
                                                        ifelse(grepl('N',Uncertainty_reported), 'N', 
                                                               Uncertainty_reported)))),
         Uncertainty_degree = as.factor(ifelse(grepl('HIGH', Uncertainty_degree),'high',
                                               ifelse(grepl('MED',Uncertainty_degree),'medium',
                                                      ifelse(grepl('LOW',Uncertainty_degree),'low',
                                                             ifelse(grepl('NRP|GRAPH',Uncertainty_degree),'NRP', Uncertainty_degree))))))


reshaped_data <- separate_wider_delim(reshaped_data,
                                      Criteria_category,
                                      delim = ",",
                                      names= c("criteria_1", "criteria_2", "criteria_3", "criteria_4", "criteria_5"),
                                      too_few = "align_start",
                                      cols_remove = TRUE)




# Remove those blank rows 
remove <- reshaped_data %>% filter(Type == 1)
remove <- remove$DU

# Finalized dataset 
reshaped_data <- reshaped_data %>% filter(!DU %in% remove)

#reshaped_data is now the full, reshaped dataset with year duplicates deleted and values corrected
#write.csv(reshaped_data, "clean_data_may_28_26.csv", row.names = FALSE)

#######################################
###         Save Objects          #####
#######################################

saveRDS(reshaped_data, file = "R_objects/reshaped_data.RDS")
saveRDS(subsample, file = "R_objects/clean_data.RDS")


