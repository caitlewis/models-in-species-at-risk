#### MODELS FOR SAR PROJECT 
#### Descriptive statistics 


#### LOAD LIBRARIES 
library(tidyverse)
library(PNWColors)

#### LOAD OBJECTS
data <- readRDS('R_objects/clean_data.RDS') # report-level statistics 

#data <- read_csv("data and code/clean_data_may_28_26.csv")


reshaped_data <- readRDS('R_objects/reshaped_data.RDS') # model-level statistics 

head(data)
head(reshaped_data)

#### REPORT-LEVEL DESCRIPTIVE STATS ####

nrow(data) # total reports in analysis 

# create a data frame of reports where at least one of the models was used to inform criteria table

#JL addition: lump lichens and mosses together
data$taxonomic_group <- as.character(data$taxonomic_group)
data$taxonomic_group[data$taxonomic_group == "Mosses"] <- "Mosses and Lichens"
data$taxonomic_group[data$taxonomic_group == "Lichens"] <- "Mosses and Lichens"

## How many REPORTS used AT LEAST one model to inform the criteria table?
models_in_criteria <- data %>% dplyr::filter(!is.na(type_x1))
nrow(models_in_criteria) # ... reports

# create a data frame of reports where at least one model was used in a LISTING decision
models_used_listing <- models_in_criteria[models_in_criteria$used_x1 == 'Y' |
                                            models_in_criteria$used_x2 == 'Y' |
                                            models_in_criteria$used_x3 == 'Y' |
                                            models_in_criteria$used_x4 == 'Y', ]
models_used_listing <- models_used_listing %>% filter(!is.na(type_x1)) #filter out weird NA values

## How many REPORTS used AT LEAST one model in a listing decision?
nrow(models_used_listing) # ... reports

## How many REPORTS used AT LEAST one model in criteria AND mentioned them elsewhere in report?
nrow(models_in_criteria[models_in_criteria$was_model_mentioned_anywhere_else_fixed == "Y",]) #... reports





# Create a new data frame that shows:
## Number of reports
## Number of reports that mentioned any model
## Number of reports that used model to inform criteria
## Number of reports that used a model to inform a listing decision

## create column that shows if report used at least one model in listing decision
data <- data %>% 
  mutate(reports_used_listing= ifelse(used_x1=="Y"|used_x2=="Y"|used_x3=="Y"|used_x4=="Y", 1,0))
#### and transform NA values to zero's
data$reports_used_listing[is.na(data$reports_used_listing)] <-0

data %>% filter(reports_used_listing == 1) %>% nrow

#JL: commenting out the below code
# ## create column that shows if report used at least one model in criteria table (if type_x1 col is na, then make it zero, and if not, the report used at least one model)
# data <- data %>% mutate(reports_used_criteria= ifelse(is.na(data$type_x1),0,1))

#JL: the below code selects reports where at least one model in criteria table, but NOT used in listing decision
data <- data %>%
  mutate(
    reports_used_criteria = if_else(
      !is.na(type_x1) &
        (is.na(used_x1) | used_x1 == "N") &
        (is.na(used_x2) | used_x2 == "N") &
        (is.na(used_x3) | used_x3 == "N") &
        (is.na(used_x4) | used_x4 == "N"),
      1,0))

data %>% filter(reports_used_criteria == 1) %>% nrow


## create column that shows if report used both a model in criteria table 
## OR a model was mentioned elsewhere 
####(if there is a yes to "was model mentioned anywhere else" OR 
#### if that first type_x1 column did NOT have an NA value)
data <- data %>% mutate(model_mentioned_anywhere = 
                          ifelse(was_model_mentioned_anywhere_else_fixed =="Y"|
                                   !is.na(data$type_x1), 1,0))

# This is the correct "was model mentioned anywhere column...
data %>% filter(model_mentioned_anywhere == 1) %>% nrow

# column for reports where model was used elsewhere but NOT for criteria or listing decision 
data <- data %>% mutate(model_mentioned_only_anywhere= 
                          ifelse(was_model_mentioned_anywhere_else_fixed =="Y" &
                                   is.na(data$type_x1),1,0))


data %>% filter(model_mentioned_only_anywhere == 1) %>% nrow

# column for reports where model was used for
#just criteria table, NOT listing decision NOR mentioned anywhere else 
data <- data %>% 
  mutate(reports_used_only_criteria= 
           ifelse(!is.na(data$type_x1) & 
                    was_model_mentioned_anywhere_else_fixed =="N" & 
                    used_x1!="Y",1,0)) # does NOT equal Y

# column for reports where no model was used, anywhere
data <- data %>% mutate(reports_no_models= 
                          ifelse(is.na(data$type_x1) & was_model_mentioned_anywhere_else_fixed =="N",1,0))


table(data$was_model_mentioned_anywhere_else_fixed,data$was_model_mentioned_anywhere_else)
sum(data$reports_no_models)

#### Taxonomic group breakdown ####

taxonomic_breakdown <- data %>% 
  dplyr::group_by(taxonomic_group) %>%
  dplyr::summarise(sum_model_mentioned_only_anywhere = sum(model_mentioned_only_anywhere),
                   sum_reports_used_criteria= sum(reports_used_criteria),
                   sum_reports_used_listing= sum(reports_used_listing),
                   total_reports_where_model_mentioned= sum(model_mentioned_anywhere),
                   .groups= 'drop')
head(taxonomic_breakdown) 

# a proportional version of the above table, out of the total reports where a model was mentioned
# anywhere in the report
prop_taxonomic_breakdown <- taxonomic_breakdown %>%
  mutate(prop_reports_used_criteria = sum_reports_used_criteria/total_reports_where_model_mentioned,
         prop_reports_used_listing = sum_reports_used_listing/total_reports_where_model_mentioned,
         prop_models_mentioned = sum_model_mentioned_only_anywhere/total_reports_where_model_mentioned)
prop_taxonomic_breakdown

#sum(prop_tax_breakdown$sum_model_mentioned_anywhere)
# Edit:
sum(prop_taxonomic_breakdown$total_reports_where_model_mentioned) # there were .... reports where a model was mentioned somewhere 

model_legend <- c(paste('Model mentioned \nbut not used, n=', sum(prop_taxonomic_breakdown$sum_model_mentioned_only_anywhere)), 
                  paste('Model used to inform criteria \nbut not recommendation, n=', sum(prop_taxonomic_breakdown$sum_reports_used_criteria)),
                  paste('Model used for listing \nrecommendation, n=', sum(prop_taxonomic_breakdown$sum_reports_used_listing)))



# proportion of reports stacked bar chart (by tax group)
prop_taxonomic_breakdown %>%
  dplyr::select(taxonomic_group, prop_reports_used_criteria, prop_reports_used_listing, prop_models_mentioned) %>%
  pivot_longer(-taxonomic_group) %>%
  ggplot(aes(x= taxonomic_group,
             y= value,
             fill = name)) +
  theme_classic()+
  geom_col(position= position_stack()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 12),
        legend.text = element_text(size=11),
        legend.title= element_text(size=12),
        axis.title.x = element_text(margin = margin(t = 20)),
        legend.spacing.y = unit(0.5, "cm"),   # vertical space between items
        legend.key.height = unit(1, "cm")) +     # height of each legend key + # move x axis title down
  labs(y= "Proportion of Reports", x="Taxonomic group", 
       fill = "Reports containing at least one model") +
  scale_fill_manual(labels=model_legend,
                    values= c("#eba07e","#b0cbe7","#5d74a5"))

#Category 1
#model used for listing recommendation
#Category 2
#model used to inform criteria but not recommendation
#Category 3
#model mentioned but not used

# raw numbers
prop_taxonomic_breakdown %>%
  dplyr::select(taxonomic_group, sum_model_mentioned_only_anywhere:sum_reports_used_listing) %>%
  pivot_longer(-taxonomic_group) %>%
  ggplot(aes(x= taxonomic_group, y=value, fill= name)) +
  geom_col(position = position_stack()) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(y= "Number of Reports", x="Taxonomic group", 
       fill = "Reports containing at least one model") +
  scale_fill_discrete(labels=model_legend)


##FIGURE 4
# final graph

model_legend <- c(paste('Reports with model mentioned \nbut not used, n=', sum(prop_taxonomic_breakdown$sum_model_mentioned_only_anywhere)), 
                  paste('Reports with model used to inform criteria \nbut not recommendation, n=', sum(prop_taxonomic_breakdown$sum_reports_used_criteria)),
                  paste('Reports with model used for listing \nrecommendation, n=', sum(prop_taxonomic_breakdown$sum_reports_used_listing)))


raw_taxonomic_brakdown = prop_taxonomic_breakdown %>%
  dplyr::select(taxonomic_group, sum_model_mentioned_only_anywhere:sum_reports_used_listing) %>%
  pivot_longer(-taxonomic_group) %>%
  ggplot(aes(x= reorder(taxonomic_group, -value) , y=value, fill= name)) +
  geom_col(position = position_stack()) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) + # this removes padding at y-axis (first number in parantheses)
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 12),
        legend.text = element_text(size=12),
        legend.title= element_text(size=12),
        axis.title.x = element_text(margin = margin(t = 20), size = 12), 
        # move x axis title down
        axis.title.y= element_text(size=12),
        legend.spacing.y = unit(0.5, "cm"),   # vertical space between items
        legend.key.height = unit(1, "cm")) +     # height of each legend key )
  labs(y= "Number of Status Reports", x="Taxonomic group", 
       fill = "") +
  scale_fill_manual(labels=model_legend,
                    values= c("#eba07e","#b0cbe7","#5d74a5"))

plot(raw_taxonomic_brakdown)
# ggsave("plots/taxonomic_breakdown_modelcategories_barplot.png", plot=raw_taxonomic_brakdown,         width=8, height = 6, units= "in", dpi= 300)

#Category 1
#model used for listing recommendation
#Category 2
#model used to inform criteria but not recommendation
#Category 3
#model mentioned but not used





sum(taxonomic_breakdown$total_reports_where_model_mentioned) 
sum(taxonomic_breakdown$sum_model_mentioned_only_anywhere) 
sum(taxonomic_breakdown$sum_reports_used_criteria)
sum(taxonomic_breakdown$sum_reports_used_listing) 


#### BY TAXONOMIC GROUP
#What is the proportion of reports that do not mention a model versus 
# the proportion of reports that do mention at least one model?

# make a raw number dataframe 
total_tax_breakdown <- data %>% group_by(taxonomic_group) %>%
  dplyr::summarise(sum_model_mentioned_anywhere = sum(model_mentioned_anywhere),
                   sum_reports_used_criteria= sum(reports_used_criteria),
                   sum_reports_used_listing= sum(reports_used_listing),
                   total_reports= n_distinct(reportID),
                   .groups= 'drop')

# a proportional version of the above data frame 
prop_tax_breakdown <- total_tax_breakdown %>%
  mutate(prop_reports_used_criteria = sum_reports_used_criteria/total_reports,
         prop_reports_used_listing = sum_reports_used_listing/total_reports,
         reports_with_no_models_mentioned = total_reports - sum_model_mentioned_anywhere,
         reports_with_no_models_mentioned = reports_with_no_models_mentioned/total_reports)


reports_with_models <- prop_tax_breakdown %>%
  dplyr::select(taxonomic_group, total_reports, sum_model_mentioned_anywhere)

reports_with_models$proportion_reports_with_models <- reports_with_models$sum_model_mentioned_anywhere/reports_with_models$total_reports

reports_with_models$proportion_reports_with_no_models <- 1-(reports_with_models$sum_model_mentioned_anywhere/reports_with_models$total_reports)

reports_with_models <- reports_with_models[c(1,4,5)] %>%
  pivot_longer(-taxonomic_group)

#original stacked barchart wasn't in the right order, so flipping the bars by 
#specifying order of factor (so that "no models" is on top)
reports_with_models$name <- factor(reports_with_models$name, levels = c('proportion_reports_with_no_models', 'proportion_reports_with_models'))

reports_with_models$taxonomic_group = fct_recode(reports_with_models$taxonomic_group,
                                                 'Amphibians'= "Amphibians",
                                                 'Arthropods'=  "Arthropods",
                                                 'Birds'= "Birds",
                                                 "Freshwater fishes"='Fishes (freshwater)',
                                                 "Marine fishes"=  'Fishes (marine)',
                                                 'Mosses and Lichens'= "Mosses and Lichens",
                                                 "Marine mammals"= 'Mammals (marine)',
                                                 "Terrestrial mammals"='Mammals (terrestrial)',
                                                 'Molluscs'="Molluscs",
                                                 'Reptiles'= "Reptiles",
                                                 "Vascular plants"='Vascular Plants')

# reports_with_models$taxonomic_group <- factor(reports_with_models$taxonomic_group,
#                                               levels= c("Amphibians",
#                                                         "Arthropods",
#                                                         "Birds",
#                                                         "Freshwater fishes",
#                                                         "Marine fishes",
#                                                         "Lichens",
#                                                         "Terrestrial mammals",
#                                                         "Molluscs",
#                                                         "Mosses",
#                                                         "Reptiles",
#                                                         "Vascular plants"))


#FIGURE 3
#make a stacked barchart with adjusted dataframe 
reports_that_mention_model_or_not = 
  
  ggplot(reports_with_models, aes(x= taxonomic_group, y = value, fill= name))+
  geom_bar(position='stack', stat='identity') +
  theme_classic()+
  labs(y= "Proportion of Reports", x="Taxonomic group", 
       fill = "Any model present \nin status report?") +
  theme(axis.text.y = element_text(size=12),
        legend.text = element_text(size=12),
        legend.title= element_text(size=12),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 12),# left align taxonomic groups 
        axis.title.x = element_text(size=12, margin = margin(t = 20)), # move x axis title down
        axis.title.y = element_text(size=12)) +
  scale_x_discrete(labels= c('Amphibians'= paste0("Amphibians",' (n= ',prop_tax_breakdown$total_reports[1], ')'),
                             'Arthropods'=  paste0("Arthropods",' (n= ',prop_tax_breakdown$total_reports[2], ')'),
                             'Birds'= paste0("Birds",' (n= ',prop_tax_breakdown$total_reports[3], ')'),
                             "Freshwater fishes"=paste0("Freshwater fishes",' (n= ',prop_tax_breakdown$total_reports[4], ')'),
                             "Marine fishes"= paste0("Marine fishes",' (n= ',prop_tax_breakdown$total_reports[5], ')'),
                             "Marine mammals"= paste0("Marine mammals",' (n= ',prop_tax_breakdown$total_reports[6], ')'),
                             "Terrestrial mammals"=paste0("Terrestrial mammals",' (n= ',prop_tax_breakdown$total_reports[7], ')'),
                             'Molluscs'=paste0("Molluscs",' (n= ',prop_tax_breakdown$total_reports[8], ')'),
                             'Mosses and Lichens'=paste0("Mosses and Lichens",' (n= ',prop_tax_breakdown$total_reports[9], ')'),
                             'Reptiles'= paste0("Reptiles",' (n= ',prop_tax_breakdown$total_reports[10], ')'),
                             "Vascular plants"=paste0("Vascular plants",' (n= ',prop_tax_breakdown$total_reports[11], ')')))+
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) + 
  scale_fill_manual(values = c(proportion_reports_with_no_models = '#b0cbe7',
                               proportion_reports_with_models = '#a8554e'),
                    labels = c(proportion_reports_with_no_models = paste0("No"," (n=",sum(prop_tax_breakdown$total_reports) - sum(prop_tax_breakdown$sum_model_mentioned_anywhere),')'),
                               proportion_reports_with_models = paste0("Yes"," (n=", 
                                                                       sum(prop_tax_breakdown$sum_model_mentioned_anywhere),")")))

plot(reports_that_mention_model_or_not)                  

# ggsave("plots/reports_mentioning_models_barplot.png", plot=reports_that_mention_model_or_not,      width=8, height = 6, units= "in", dpi= 300)
# # # 



#### Publishing year breakdown ####


library(cowplot)

# a dataframe with just year and number of total reports 
year<- data %>%
  dplyr::group_by(year_published) %>%
  dplyr::summarise(number_reports=n())


#group by YEAR, and then sum the new columns you just created
year_breakdown <- data %>% group_by(year_published) %>%
  dplyr::summarise(sum_model_mentioned_only_anywhere = sum(model_mentioned_only_anywhere),
                   sum_reports_used_criteria= sum(reports_used_criteria),
                   sum_reports_used_listing= sum(reports_used_listing),
                   total_reports_where_model_mentioned= sum(model_mentioned_anywhere),
                   total_reports = n_distinct(reportID),
                   .groups= 'drop')

year_breakdown 

sum(year_breakdown$sum_reports_used_criteria) 
sum(year_breakdown$sum_model_mentioned_only_anywhere) 
sum(year_breakdown$sum_reports_used_criteria) 
sum(year_breakdown$sum_reports_used_listing) 
sum(year_breakdown$total_reports_where_model_mentioned) 


#create new columns based on proportion of columns you just created
year_breakdown <- year_breakdown %>%
  mutate(prop_reports_used_criteria = sum_reports_used_criteria/total_reports_where_model_mentioned,
         prop_reports_used_listing = sum_reports_used_listing/total_reports_where_model_mentioned,
         prop_models_mentioned = sum_model_mentioned_only_anywhere/total_reports_where_model_mentioned)

range(year_breakdown$year_published)



#Category 1
#model used for listing recommendation
#Category 2
#model used to inform criteria but not recommendation
#Category 3
#model mentioned but not used


#Figure 2
# Proportional breakdown of model use in reports over time 

model_legend <- c(paste('Reports with model mentioned but not used, n=', sum(year_breakdown$sum_model_mentioned_only_anywhere)), 
                  paste('Reports with model used to inform criteria but not recommendation, n=', sum(year_breakdown$sum_reports_used_criteria)),
                  paste('Reports with model used for listing recommendation, n=', sum(year_breakdown$sum_reports_used_listing)))

year_breakdown_proportional <- year_breakdown %>%
  dplyr::select(year_published, prop_reports_used_criteria:prop_models_mentioned) %>%
  pivot_longer(-year_published) %>%
  ggplot(aes(x= year_published, y= value, color = name, fill = name)) +
  geom_point() +
  #geom_smooth(se= F, method = 'loess', span = 1) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.25) + 
  theme_classic()+
  theme(axis.text.y = element_text(size=12),
        legend.text = element_text(size=12),
        legend.title= element_text(size=12),
        axis.text.x = element_text(size=12),
        axis.title.x= element_text(size=12),
        axis.title.y= element_text(size=12))+
  labs(y= "Proportion of Reports",
       x= "Year",
       color = "Model use in report") +
  scale_color_manual(labels= model_legend,
                     values=c("#eba07e","#b0cbe7","#5d74a5" )) +
  scale_fill_manual(values = c("#f6c6b3", "#d6e4f3", "#9fb0cf")) +
  guides(fill = "none")# lighter shades


# Raw data breakdwon of model use in reports over time 
year_breakdown_raw = year_breakdown %>%
  dplyr::select(year_published, sum_model_mentioned_only_anywhere:sum_reports_used_listing) %>%
  pivot_longer(-year_published) %>%
  ggplot(aes(x= year_published, y= value, color = name, fill = name)) +
  geom_point() +
  #geom_smooth(se= F, method = 'loess', span = 1) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.25) + 
  theme_classic()+
  theme(axis.text.y = element_text(size=14),
        legend.text = element_text(size=14),
        legend.title= element_text(size=14),
        axis.text.x = element_text(size=14),
        axis.title.x= element_text(size=14),
        axis.title.y= element_text(size=14))+
  labs(y= "Number of Status Reports",
       x= "Year",
       color="") +
  scale_color_manual(labels= model_legend,
                     values=c("#eba07e","#b0cbe7","#5d74a5" )) +
  scale_fill_manual(values = c("#f6c6b3", "#d6e4f3", "#9fb0cf")) +
  guides(fill = "none")# lighter shades


legend <- get_legend(year_breakdown_raw)

year_breakdown_raw_clean <- year_breakdown_raw + theme(legend.position = "none")
year_breakdown_proportional_clean <- year_breakdown_proportional + theme(legend.position = "none")





plot_grid = plot_grid(
  plot_grid(year_breakdown_raw_clean, year_breakdown_proportional_clean, ncol = 2),
  legend,
  ncol = 1,
  rel_heights = c(1, 0.3)
)
plot_grid
 ggsave("plots/year_breakdown_model_categories_scatterplot.png", plot_grid,
         width=10, height = 5, units= "in", dpi= 300)
# # # 

#### BY YEAR
# What is the proportion of reports that do not mention a model versus 
# the proportion of reports that do mention at least one model?

total_year_breakdown <- year_breakdown %>%
  dplyr::select(year_published, total_reports_where_model_mentioned, total_reports) %>%
  mutate(prop_reports_with_models= total_reports_where_model_mentioned/total_reports) 

title <- paste0('Proportion of status reports that mentioned a model (n=', 
                sum(total_year_breakdown$total_reports_where_model_mentioned),
                ') out of total reports in sample (n=',
                sum(total_year_breakdown$total_reports),')')



##FIGURE 1
total_year_breakdown_plot= total_year_breakdown %>%
  dplyr::select(year_published, prop_reports_with_models) %>%
  pivot_longer(-year_published) %>%
  ggplot(aes(x= year_published, y= value)) + 
  geom_point() +
  theme_classic()+
  geom_smooth(se= F, method = 'loess', span = 1,color = "black") +
  labs(y= "Proportion of Status Reports \nThat Include a Model",
       x= 'Year',
       title= "") 

plot(total_year_breakdown_plot)
 ggsave("plots/total_reports_over_year_scatterplot.png", plot=total_year_breakdown_plot,
          width=8, height = 5, units= "in", dpi= 300)






#### MODEL-LEVEL DESCRIPTIVE STATS ####

head(reshaped_data)
colnames(reshaped_data)

# rename columns and make sure things are factors 
reshaped_data <- 
  within(reshaped_data,{
    sara_status = factor(cosewic_status)
    year = factor(year_published)
    taxonomic_group = factor(taxonomic_group)
    Type = factor(Type)
    Uncertainty_degree = factor(Uncertainty_degree)
    Uncertainty_reported = factor(Uncertainty_reported)
  })

values = c('Y'= 'Yes', 'N'='No')


# NEW STUFF

# Was model species-specific? 
#Figure S7
reshaped_data %>% filter(Species_specific %in% c("Y","N")) %>%
  filter(!is.na(Used)) %>%
  # reorder ensures largest column is closest to the y-axis
  ggplot(aes(x= reorder(Species_specific, Species_specific, function(x)-length(x)), 
             fill= Used)) +
  geom_bar(stat = 'count') + 
  theme_bw() +
  #scale_fill_discrete(labels= values)+
  scale_x_discrete(labels= values) +
  labs(x= "Was the model species-specific?",
       y= "Frequency",
       fill= "Was the model applied to \nlisting recommendation?") +
  theme_classic() +
  scale_x_discrete(labels= values) +
  scale_fill_manual(values=c('#b0cbe7', '#a8554e'), labels= c("N"="No",
                                                              "Y"= "Yes"))




# This is the reverse of the above graph...
reshaped_data %>% filter(Species_specific %in% c("Y","N"),
                         !is.na(Used)) %>%
  # reorder ensures largest column is closest to the y-axis
  ggplot(aes(x= reorder(Used, Used, function(x)-length(x)), 
             fill= Species_specific)) +
  geom_bar(stat = 'count') + 
  theme_bw() +
  scale_fill_discrete(labels= values)+
  scale_x_discrete(labels= values) +
  labs(x= "Was the model used in the listing decision?",
       y= "Frequency",
       fill= "Was the model species-specific?")

# Was uncertainty reported for the model? And to what degree of uncertainty was it?
reshaped_data %>% filter(!is.na(Uncertainty_reported),
                         Uncertainty_reported != "NRP",
                         !is.na(Uncertainty_degree)) %>%
  # reorder ensures largest column is closest to the y-axis
  ggplot(aes(x= reorder(Uncertainty_reported, Uncertainty_reported, function(x)-length(x)), 
             fill= reorder(Uncertainty_degree, Uncertainty_degree, function(x)+length(x)))) +
  theme_bw() +
  scale_x_discrete(labels= values) +
  geom_bar(stat = 'count') + 
  labs(x= "Was uncertainty reported for the model?",
       y= "Frequency",
       fill= "Degree of Uncertainty")


uncertainty_listing = reshaped_data %>% filter(!is.na(Uncertainty_reported),
                                               Uncertainty_reported != "NRP",
                                               !is.na(Uncertainty_degree)) %>%
  # reorder ensures largest column is closest to the y-axis
  ggplot(aes(x= reorder(Used, Used, function(x)-length(x)),
             fill= reorder(Uncertainty_degree, Uncertainty_degree, function(x)+length(x)))) +
  theme_classic() +
  theme(axis.text.y = element_text(size=12),
        legend.text = element_text(size=11),
        legend.title= element_text(size=12),
        axis.text.x = element_text(size=12),
        axis.title.x = element_text(size= 12, margin = margin(t = 20)),
        axis.title.y= element_text(size=12))+
  scale_x_discrete(labels= values) +
  scale_fill_manual(values=c("#41476b", "#9e6374", "#de9b71", "#fbdfa2"),
                    labels= c("low"="Low",
                              "high"= "High",
                              "medium"="Medium",
                              "NRP"= "None reported"))+
  geom_bar(stat = 'count') + 
  labs(x= "Model used in the ultimate listing decision?",
       y= "Number of quantitative models",
       fill= "Degree of uncertainty in the model")

#  ggsave("plots/uncertainty_effects_listing_barplot.png", plot=uncertainty_listing ,
#            width=10, height = 5, units= "in", dpi= 300)
# #  # 



#### BY TAXONOMIC GROUP
# model type used to inform criteria for each taxonomic group, rounded to 2 digits
#Figure 5

#Note from JL: there is one data entry that says "population" under the model type. I reviewed the status report and it is a PVA so I am updating in the google doc and here, just in case.

reshaped_data$Type[reshaped_data$Type == "population"] <- "demographic"

type_used_criteria_by_tax <- 
  data.frame(round(prop.table(table(reshaped_data$taxonomic_group, reshaped_data$Type),1),2)) 

type_used_criteria_by_tax_wide <- type_used_criteria_by_tax %>%
  pivot_wider(names_from = Var2, values_from= Freq)

# Same as above but with raw numbers
type_used_criteria_by_tax_raw <- data.frame(
  table(reshaped_data$taxonomic_group, reshaped_data$Type)) %>%
  pivot_wider(names_from = Var2, values_from = Freq) 

#Addition from JL: get number of models per group
type_used_criteria_by_tax_raw<- type_used_criteria_by_tax_raw %>%
  mutate(total_models=abundance + distributional + other + demographic) 

# How many models of each type were used to inform criteria?
legend <- reshaped_data %>% filter(!is.na(Type)) %>%
  dplyr::group_by(Type) %>%
  dplyr::summarise(number_models=n()) %>%
  mutate(
    total= sum(number_models),
    proportion= number_models/total) 


model_legend <- c('demographic'= paste0('demographic (n=', legend$number_models[2],")"),
                  'distributional'= paste0('distributional (n=', legend$number_models[3],')'),
                  'abundance'= paste0('abundance (n=', legend$number_models[1],')'),
                  'other'= paste0('other (n=', legend$number_models[4], ')'))

#... reorder factor for the following barchart
type_used_criteria_by_tax$Var2 <- factor(type_used_criteria_by_tax$Var2, levels= 
                                           c('demographic',
                                             'distributional',
                                             'abundance',
                                             'other'))

type_model_by_tax = type_used_criteria_by_tax %>% 
  filter(!is.na(Var2)) %>%
  ggplot(aes(x= Var1, y= Freq, fill=Var2)) +
  geom_bar(position = 'stack', stat='identity') +
  theme_classic()+
  scale_x_discrete(labels= c('Amphibians'= paste0("Amphibians",' (n= ',type_used_criteria_by_tax_raw$total_models[1], ')'),
                             'Arthropods'=  paste0("Arthropods",' (n= ',type_used_criteria_by_tax_raw$total_models[2], ')'),
                             'Birds'= paste0("Birds",' (n= ',type_used_criteria_by_tax_raw$total_models[3], ')'),
                             "Fishes (freshwater)"=paste0("Fishes (freshwater)",' (n= ',type_used_criteria_by_tax_raw$total_models[4], ')'),
                             "Fishes (marine)"= paste0("Fishes (marine)",' (n= ',type_used_criteria_by_tax_raw$total_models[5], ')'),
                             'Lichens'= paste0("Lichens",' (n= ',type_used_criteria_by_tax_raw$total_models[6], ')'),
                             "Mammals (marine)"= paste0("Mammals (marine)",' (n= ',type_used_criteria_by_tax_raw$total_models[7], ')'),
                             "Mammals (terrestrial)"=paste0("Mammals (terrestrial)",' (n= ',type_used_criteria_by_tax_raw$total_models[8], ')'),
                             'Molluscs'=paste0("Molluscs",' (n= ',type_used_criteria_by_tax_raw$total_models[9], ')'),
                             'Mosses'=paste0("Mosses",' (n= ',type_used_criteria_by_tax_raw$total_models[10], ')'),
                             'Reptiles'= paste0("Reptiles",' (n= ',type_used_criteria_by_tax_raw$total_models[11], ')'),
                             "Vascular Plants"=paste0("Vascular plants",' (n= ',type_used_criteria_by_tax_raw$total_models[12], ')')))+
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) + # this removes padding at y-axis (first number in parantheses)
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 12),# left align taxonomic groups 
        axis.text.y = element_text(size=12),
        legend.text = element_text(size=12),
        legend.title= element_text(size=12),
        axis.title.x = element_text(size= 12, margin = margin(t = 20)),
        axis.title.y= element_text(size=12),
        plot.margin = margin(t = 20, r = 10, b = 10, l = 20))+
  labs(y= "Proportion of Models", x= "Taxonomic Group", fill="Model type") +
  scale_fill_manual(labels= model_legend,
                    values= c("#41476b", "#9e6374", "#de9b71", "#fbdfa2"))

plot(type_model_by_tax)
ggsave("plots/types_of_models_used_for_taxonomic_groups_barplot.png", plot=type_model_by_tax,
              width=8, height = 6, units= "in", dpi= 300)
# 


# How many total MODELS were used for criteria table?
nrow(filter(reshaped_data, !(is.na(Type)))) # filter out NAs to get this value 

#breakdown of unique models within each taxonomic group 
reshaped_data %>% 
  group_by(taxonomic_group) %>%
  summarize(number_of_models= n()) 

reshaped_data %>% 
  group_by(Type) %>%
  summarize(number_of_models= n())


## number of unique reports 
data %>%
  summarize(number_of_reports = n_distinct(DU))



#JL's additions to replicate Caitlin's Figures S4 and S5

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)


#Figure S4


model_data <- data %>%
  select(
    reportID,
    starts_with("type_x"),
    starts_with("criteria_category_x")
  ) %>%
  pivot_longer(
    cols = -reportID,
    names_to = c(".value", "model_number"),
    names_pattern = "(type|criteria_category)_x(\\d+)"
  )

model_data <- model_data %>%
  mutate(
    type = str_to_lower(as.character(type)),
    type = str_trim(type),
    criteria_category = as.character(criteria_category)
  )

table(model_data$type, useNA = "ifany")

model_criteria <- model_data %>%
  filter(
    !is.na(type),
    !is.na(criteria_category)
  ) %>%
  separate_rows(criteria_category, sep = "\\s*,\\s*") %>%
  mutate(
    criteria_category = str_trim(criteria_category)
  ) %>%
  filter(criteria_category %in% c("A", "B", "C", "D", "E"))


plot_data <- model_data %>%
  filter(
    !is.na(type),
    !is.na(criteria_category)
  ) %>%
  mutate(
    type = case_when(
      type %in% c("distribution", "distributional") ~ "Distributional",
      type == "abundance" ~ "Abundance",
      type == "demographic" ~ "Demographic",
      type == "other" ~ "Other",
      TRUE ~ type
    )
  ) %>%
  separate_rows(criteria_category, sep = "\\s*,\\s*") %>%
  mutate(
    criteria_category = str_trim(criteria_category),
    type = factor(
      type,
      levels = c(
        "Abundance",
        "Demographic",
        "Distributional",
        "Other"
      )
    ),
    criteria_category = factor(
      criteria_category,
      levels = c("A", "B", "C", "D", "E")
    )
  ) %>%
  filter(criteria_category %in% c("A", "B", "C", "D", "E")) %>%
  count(type, criteria_category, name = "n")





#Figure S5
# Put model type, criterion category, and "used" status
# on the same row for each individual model
model_use_data <- data %>%
  select(
    reportID,
    matches("^(type|criteria_category|used)_x[1-4]$")
  ) %>%
  pivot_longer(
    cols = -reportID,
    names_to = c(".value", "model_number"),
    names_pattern = "(type|criteria_category|used)_x(\\d+)"
  ) %>%
  filter(!is.na(type)) %>%
  mutate(
    type = str_to_lower(str_trim(as.character(type))),
    criteria_category = as.character(criteria_category),
    used = str_trim(as.character(used)),
    type = case_when(
      type %in% c("distribution", "distributional") ~ "distributional",
      TRUE ~ type
    )
  )
#Then create the Figure S5 counts using only models with used == "Y":
plot_S5_model_specific <- model_use_data %>%
  filter(
    used == "Y",
    !is.na(criteria_category)
  ) %>%
  separate_rows(
    criteria_category,
    sep = "\\s*,\\s*"
  ) %>%
  mutate(
    criteria_category = str_trim(criteria_category)
  ) %>%
  filter(
    criteria_category %in% c("A", "B", "C", "D", "E")
  ) %>%
  count(
    type,
    criteria_category,
    name = "n"
  ) %>%
  arrange(type, criteria_category)

plot_S5_model_specific


sum(plot_S5_model_specific$n)

model_use_data %>%
  filter(used == "Y") %>%
  count(type)


