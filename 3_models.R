#### MODELS FOR SAR PROJECT 
#### Modeling 

#### CREATED: May 23 2025
#### LAST EDIT:  April 27 2026


#### OBJECTS CREATED 

#### LOAD LIBRARIES
library(tidyverse)
library(reshape)
library(udpipe)
library(forcats)
library(glmmTMB)
library(lme4)
library(gtsummary) # for making nice tables


#### LOAD OBJECTS
data <- readRDS('R_objects/clean_data.RDS') # report-level statistics 
reshaped_data <- readRDS('R_objects/reshaped_data.RDS') # model-level statistics 


summary(data$taxonomic_group)
summary(reshaped_data$taxonomic_group)
# EDIT MAR 30: for some reason, lichens and mosses are separate. Fixed it below. 

# Group lichens and mosses together in both datasets 
data <- data %>%
  mutate(taxonomic_group = fct_recode(taxonomic_group, "Lichens and Mosses" = "Lichens",
                                      "Lichens and Mosses" = "Mosses"))

reshaped_data <- reshaped_data %>%
  mutate(taxonomic_group = fct_recode(taxonomic_group, "Lichens and Mosses" = "Lichens",
                                      "Lichens and Mosses" = "Mosses"))

summary(data$taxonomic_group)
summary(reshaped_data$taxonomic_group)


#### MODEL ONE ####
#From JL: Regression 3 (Table S6 in initial manuscript, renumbered to Table 2 in final manuscript when we removed the previous Table S5): model applied to listing decision (n=53)
# What factors (year, taxonomic group, model_type) contribute to a quantitative model being used by COSEWIC in a species-at-risk listing decision? 

# MODEL COMPONENTS 
# Dataset = only models used in criteria table, only EN and TH species  
# y variable = Binary (0(N)/1(Y)), was the model used in the criteria 
#table also used in the listing decision?

#fixed effects
# taxonomic group (categorical, factor)
## model type (categorical, factor)
### year (numerical)

#random effects
#intercept = reportID 
#a unique identifier that refers to each individaul SAR document (pairs sp with yr)

## STEP ONE: CREATE BINARY RESPONSE VARIABLE 

#convert "Used" to 0/1 binary and store in new variable called y. =NA if model wasn't used 
reshaped_data$used_binary <- ifelse(reshaped_data$Used == "Y", 1,0)

# add a reportID column
reshaped_data$reportID <- unique_identifier(reshaped_data, fields = c("species", "year_published"), start_from = 1)

# Filter out all the models that did NOT inform criteria 
criteria_informing_models <- reshaped_data %>% filter(!is.na(Type)) %>%
  within({taxonomic_group = factor(taxonomic_group)
  type = factor(Type)
  used_binary= factor(used_binary)
  reportID= factor(reportID)
  year = as.numeric(year_published)})

# fix the factors so that our reference categories go first
criteria_informing_models$taxonomic_group = factor(
  criteria_informing_models$taxonomic_group,
  levels =
    c("Birds",
      "Arthropods",
      "Amphibians",
      "Fishes (freshwater)",
      "Molluscs",
      "Fishes (marine)",
      "Mammals (terrestrial)",
      "Mammals (marine)",
      "Reptiles",
      "Lichens and Mosses",
      "Vascular Plants"
    )
)

criteria_informing_models$type = factor(
  criteria_informing_models$type,
  levels =
    c(
      "abundance",
      "demographic",
      "distributional",
      "population",
      "other"
    )
)

# Rename levels using fct_recode(factor, "new" = "old")

criteria_informing_models$type=  fct_recode(
  criteria_informing_models$type,
  "Abundance" = "abundance",
  "Demographic" = "demographic",
  "Distributional" = "distributional",
  "Population" = "population",
  "Other" = "other"
)

# Do our predictors have any colinearity or possible interactions?

# taxonomic group and model type
criteria_informing_models %>%
  ggplot(aes(x= taxonomic_group, y= type)) +
  stat_sum(aes(size=..n.., group= 1)+
             scale_size_area(max_size = 10),)+
  labs(x= "Taxonomic group",
       y= "Model Type",
       title = "Relationship between taxonomic group and model type") +
  theme(axis.text.x= element_text(angle= 90, hjust = 0, vjust = 0, size = 10))

# taxonomic group and year
melt(criteria_informing_models[,c("taxonomic_group", "year_published")], id.vars = "taxonomic_group") %>%
  ggplot(aes(factor(taxonomic_group), 
             y= value,
             fill= factor(taxonomic_group))) +
  geom_boxplot() +
  facet_wrap(~variable, scales= 'free_y') + 
  theme(axis.text.x= element_text(angle= 90, hjust = 0, vjust = 0, size = 10))

# model type and year
melt(criteria_informing_models[,c("Type", "year_published")],
     id.vars = "Type") %>%
  ggplot(aes(factor(Type), y=value, fill=factor(Type))) +
  geom_boxplot() + 
  facet_wrap(~variable, scales = "free_y") +
  labs(x = 'Model type',
       y = 'Year published')


#two-way contingency table of categorical predictors and binary
#responses to make sure there's no 0 cells
xtabs(~used_binary + taxonomic_group, data=criteria_informing_models)
# amphibians, molluscs, marine fishes, vascular plants have zeros

xtabs(~used_binary + Type, data=criteria_informing_models)
# population type models have a zero

# must drop molluscs, amphibians, vascular plants, marine fish, and population models
#JL got rid of this because we aren't using taxonomic group as a predictor in the model anymore, and "population" was a typo that has since been fixed

#criteria_informing_models <- criteria_informing_models %>% 
#  filter(!taxonomic_group %in% c("Molluscs", "Amphibians", "Fishes (marine)", "Vascular Plants")) %>%
#  filter(type != "population")


# Is there a relationship between model type and taxonomic group?
criteria_informing_models %>% ggplot(aes(x= taxonomic_group,
                                         y= type)) +
  stat_sum(aes(size=..n.., group = 1) +
             scale_size_area(max_size= 10))+
  labs(x= "Taxonomic group",
       y= "Model type",
       title= "The distribution of model type across taxonomic groups among\
       models used to inform quantitative criteria") +
  theme(axis.text.x= element_text(angle= 90, hjust = 0, vjust = 0, size = 10))


# Is there a relationship between taxonomic grouping and year?
melt(criteria_informing_models[, c("taxonomic_group", "year_published")], id.vars = "taxonomic_group") %>%
  ggplot(aes(factor(taxonomic_group), y= value)) +
  geom_boxplot() +
  facet_wrap(~variable, scales= "free_y") +
  labs(y= "Year published",
       x= "Taxonomic group") + 
  theme(axis.text.x= element_text(angle= 90, hjust = 0, vjust = 0, size = 10))

# Or model type and year? 
melt(criteria_informing_models[,c("Type", "year_published")],
     id.vars = "Type") %>%
  ggplot(aes(factor(Type), y= value))+
  geom_boxplot() +
  facet_wrap(~variable, scales= 'free_y') +
  labs(y= "Year published",
       x= "Model type")

#### BUILDING MODEL


## With random effect
# model_model = glmer(used_binary ~ taxonomic_group + 
#                       type + year_published +
#                       (1|reportID),
#                     data= criteria_informing_models,
#                     family= binomial(link= "logit"))

# ERROR about convergence: Error: (maxstephalfit) PIRLS step-halvings failed to reduce deviance in pwrssUpdate

## Without random effect 
# This is the model used in the report
model_model = glm(
  used_binary ~ taxonomic_group +
    year_published + type,
  data = criteria_informing_models,
  family = binomial(link = "logit")
)

nrow(criteria_informing_models)
criteria_informing_models %>% filter(used_binary ==1) %>% nrow

summary(model_model)

#JL addition: get rid of taxonomic group
library(lme4)

model_model <- glmer(
  used_binary ~ year_published + type + 
    (1|reportID),
  data = criteria_informing_models,
  family = binomial(link = "logit")
)

#glmer is unstable, try glmmTMB instead
model_model <- glmmTMB(
  used_binary ~ year_published + type +
    (1|reportID),
  data = criteria_informing_models,
  family = binomial())


#add taxonomic group but get convergence issues
model_model <- glmmTMB(
  used_binary ~ year_published + type +
    (1|reportID) + (1|taxonomic_group),
  data = criteria_informing_models,
  family = binomial())

summary(model_model)


nrow(criteria_informing_models)
criteria_informing_models %>% filter(used_binary ==1) %>% nrow

summary(model_model)


# Compare model with just intercept with full model 
#Test stat is different between residual deviance for model with predictors and
#null model. 


#Find the "test statistic" (the difference in deviance for the two models)
#with(model_model, null.deviance - deviance)

#degrees of freedom
#with(model_model, df.null-df.residual)

#p-value=
#with(model_model, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))


#JL addition: null.deviance and deviance are components of a glm object, but not of a glmmTMB object. For mixed models, the equivalent comparison is done using a likelihood ratio test between the full model and a null model.
# null model (intercept + random effect)
m_null <- glmmTMB(
  used_binary ~ 1 +
    (1 | reportID),
  data = criteria_informing_models,
  family = binomial()
)

anova(m_null, model_model)



# model_model_glm <- model_model %>%
#   tbl_regression(label= list(
#     year_published ~ "Year published",
#     taxonomic_group ~ "Taxonomic group",
#     type ~ "Model type"),
#     exponentiate = TRUE) %>%
#   modify_header(label~"**Predictor**")

# model_model_glm %>%
#   as_gt() %>%
#   gt::gtsave(filename = "listing_decision_glm.png")

#JL addition: get rid of taxonomic group
library(gt)
model_model_glmm <- model_model %>%
  tbl_regression(
    label = list(
      year_published ~ "Year published",
      type ~ "Model type"
    ),
    exponentiate = TRUE
  ) %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(groupname_col = "")
  ) %>%
  modify_header(
    label ~ "**Predictor**",
    estimate ~ "**OR**"
  ) %>%
  as_gt()

# remove all existing source notes
model_model_glmm$`_source_notes` <- NULL

# add your own
model_model_glmm <- model_model_glmm %>%
  gt::tab_source_note(
    source_note = gt::md(
      "**Abbreviations:** CI = Confidence Interval; OR = Odds Ratio."
    )
  )

model_model_glmm %>%
  #as_gt() %>%
  gt::gtsave(filename = "listing_decision_glmm.png")


# odds_ratios_model_model = round(exp(cbind(OR= coef(model_model), confint(model_model))),digits = 3)
#don't usually interpret the intercept OR 


#odds_ratios_model_model = as.data.frame(odds_ratios_model_model)
#odds_ratios_model_model = rownames_to_column(odds_ratios_model_model, var= "Terms")

# #odds_ratios_model_model$Terms = recode(odds_ratios_model_model$Term, 
#                                        "(Intercept)" = "Intercept",
#                                        "taxonomic_groupArthropods"= "Arthropods",
#                                        "taxonomic_groupFishes (freshwater)"= "Freshwater fishes",
#                                        "taxonomic_groupMammals (terrestrial)" = "Terrestrial mammals",
#                                        "taxonomic_groupFishes (marine)" = "Marine fishes",
#                                        
#                                        "taxonomic_groupMammals (marine)"= "Marine mammals",
#                                        "taxonomic_groupReptiles"= "Reptiles",
#                                        "taxonomic_groupLichens and Mosses"= "Lichens and Mosses",
#                                        "year_published"= "Year published",
#                                        "typeDemographic"= "Demographic (type)",
#                                        "typeDistributional"="Distributional (type)",
#                                        "typePopulation"="Population (type)",
#                                        "typeOther"= "Other (type)")

# removing intercept, bc typically don't interpret this 
#odds_ratios_model_model = odds_ratios_model_model[-1,]

# ggplot(odds_ratios_model_model, aes(x= OR, y= Terms)) +
#   geom_point() + theme_classic()+
#   geom_errorbarh(aes(xmin= `2.5 %`, xmax = `97.5 %`),height= 0.2) +
#   geom_vline(xintercept=1, linetype= "dashed") + 
#   theme(axis.text.y = element_text(size = 12),
#         axis.text.x = element_text(size= 12),
#         axis.title.x = element_text(size = 12),
#         axis.title.y= element_text(size=12)) +
#   labs(x= "Odds Ratio (95% confidence interval)")







#### MODEL TWO ####
#From JL: Regression 1 (Table S4 in original manuscript; now Table 1): report applied to criteria OR listing decisions (n=43)
# What factors (year, taxonomic group) contribute to at least 1 quantitative model being used by COSEWIC in a species-at-risk criterion? 

# MODEL COMPONENTS 
# Dataset = original cleaned, wide-framed data set without special concern species
# y variable = Binary (0(N)/1(Y)), was there at least one model used for SAR criterion? 
## No/0 : reports_used_criteria = 0
## Y/1: reports_used_criteria = 1

#predictors
# taxonomic group (categorical, factor)
# year (numerical)

#no random effect needed, because now we are looking at individual reports not individual models

# Create binary response variable
# reminder:
# if type_x1 has a value, than the report had at least one model used to inform criteria
# if type_x1 is NA, than all other type_x columns will be NA too
data <- data %>% mutate(reports_used_criteria= ifelse(is.na(data$type_x1),0,1))


models_in_reports = within(data, {
  taxonomic_group = factor(taxonomic_group)
  reports_used_criteria = factor(reports_used_criteria)
  year_published = as.numeric(year_published)
})

# re-order the taxonomic group factor so that reference category will be birds

#order taxonomic_group so that the reference category is birds
models_in_reports$taxonomic_group <- factor(
  models_in_reports$taxonomic_group,
  levels =
    c("Birds",
      "Arthropods",
      "Amphibians",
      "Fishes (freshwater)",
      "Molluscs",
      "Fishes (marine)",
      "Mammals (terrestrial)",
      "Mammals (marine)",
      "Reptiles",
      "Lichens and Mosses",
      "Vascular Plants"
    )
)


# Look for potential interactions between predictors

# relationship between taxonomic group and year?
melt(models_in_reports[,c("taxonomic_group", 
                          "year_published")],
     id.vars = "taxonomic_group") %>%
  ggplot(aes(factor(taxonomic_group), y= value,
             fill=factor(taxonomic_group)))+
  geom_boxplot() + 
  facet_wrap(~variable, scales= 'free_y') +
  labs(fill= NULL,
       x= "Taxonomic group",
       y= "Year published",
       title= "Relationship between taxonomic group and publish year in SAR reports") +
  theme(axis.text.x= element_text(angle= 90, hjust = 0, vjust = 0, size = 10))

#two-way contingency table of categorical predictors and binary
#responses to make sure there's no 0 cells
xtabs(~reports_used_criteria + taxonomic_group, data=models_in_reports)

#  mosses need to be dropped-- EDIT MARCH 30, no longer true bc combined with Lichens 
# models_in_reports = models_in_reports %>% filter(!taxonomic_group %in% c("Mosses"))

#### BUILDING MODEL
# Rename variables for final table
report_model = glm(reports_used_criteria ~ taxonomic_group + year_published,
                   data= models_in_reports,
                   family= binomial(link = "logit"))

summary(report_model)

tbl_glm <- report_model %>%
  tbl_regression(label= list(
    year_published ~ "Year published",
    taxonomic_group ~ "Taxonomic group"),
    exponentiate = TRUE) %>%
  modify_header(label~"**Predictor**", estimate ~ "**Odds Ratio**")

tbl_glm

tbl_glm %>%
  as_gt() %>%
  gt::gtsave(filename = "criteria_table_report_glm.png")


# we have significant terms within the taxonomic group 
# the overall effect of taxonomic group is signifiant

## ODDS RATIOS
#exponentiate coefficients and interpret as odds-ratios
##use cbind to bind coefficients and confidence intervals into columns
exp(coef(report_model))#just odds ratios


odds_ratios = round(exp(cbind(OR= coef(report_model), confint(report_model))),digits = 5)
#don't usually interpret the intercept OR 

odds_ratios

# compare full model to Null model (just intercept)
#Find the "test statistic" (the difference in deviance for the two models)
with(report_model, null.deviance - deviance)

#degrees of freedom: 9
with(report_model, df.null-df.residual)

#p-value=
with(report_model, pchisq(null.deviance - deviance, 
                          df.null - df.residual, lower.tail = FALSE))

ll.null <- report_model$null.deviance/-2
ll.proposed <- report_model$deviance/-2
(ll.null - ll.proposed)/ ll.null #0.2667876 #(R^2 value?)
1-pchisq(2*(ll.proposed-ll.null), df=(length(report_model$coefficients)-1))
#P value is almost 0 (very tiny)


odds_ratios = as.data.frame(odds_ratios)
odds_ratios = rownames_to_column(odds_ratios, var= "Terms")

odds_ratios$Terms = recode(odds_ratios$Term, 
                           "(Intercept)" = "Intercept",
                           "taxonomic_groupArthropods"= "Arthropods",
                           "taxonomic_groupAmphibians"= "Amphibians",
                           "taxonomic_groupFishes (freshwater)"= "Freshwater fishes",
                           "taxonomic_groupMolluscs"= "Molluscs",
                           "taxonomic_groupFishes (marine)" = "Marine fishes",
                           "taxonomic_groupMammals (terrestrial)" = "Terrestrial mammals",
                           "taxonomic_groupMammals (marine)"= "Marine mammals",
                           "taxonomic_groupReptiles"= "Reptiles",
                           "taxonomic_groupLichens and Mosses"= "Lichens and Mosses",
                           "taxonomic_groupVascular Plants"= "Vascular plants",
                           "year_published"= "Year published")

# removing intercept, bc typically don't interpret this 
odds_ratios = odds_ratios[-1,]

ggplot(odds_ratios, aes(x= OR, y= Terms)) +
  geom_point() + theme_classic()+
  geom_errorbarh(aes(xmin= `2.5 %`, xmax = `97.5 %`),height= 0.2) +
  geom_vline(xintercept=1, linetype= "dashed") + 
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size= 12),
        axis.title.x = element_text(size = 12),
        axis.title.y= element_text(size=12)) +
  labs(x= "Odds Ratio (95% confidence interval)")





#### MODEL THREE ####
#From JL: Regression 2 (Previously Table S5 in the initial manuscript; removed from final manuscript): report applied to listing decisions only (n=34) <- this was just to check that the 9 extra models didn't affect our conclusions much, kind of a sensitivity analysis
# What factors (year, taxonomic group) contribute to at least 1 quantitative model being used by COSEWIC in a species-at-risk LISTING decision?  

# MODEL COMPONENTS 
# Dataset = original cleaned, wide-framed data set without special concern species
# y variable = Binary (0(N)/1(Y)), was there at least one model used for SAR criterion? 
## No/0 : reports_used_listing = 0
## Y/1: reports_used_listing = 1

#predictors
# taxonomic group (categorical, factor)
# year (numerical)

#no random effect needed, because now we are looking at individual reports not individual models

# Create a binary response variable in data
data <- data %>% 
  mutate(reports_used_listing= ifelse(used_x1=="Y"|used_x2=="Y"|used_x3=="Y"|used_x4=="Y", 1,0))
data$reports_used_listing[is.na(data$reports_used_listing)] <-0

reports_listing = within(data, {
  taxonomic_group= factor(taxonomic_group)
  y = factor(reports_used_listing)
})

# reorder so reference category is at the top
reports_listing$taxonomic_group = factor(
  reports_listing$taxonomic_group,
  levels =
    c("Birds",
      "Arthropods",
      "Amphibians",
      "Fishes (freshwater)",
      "Molluscs",
      "Fishes (marine)",
      "Mammals (terrestrial)",
      "Mammals (marine)",
      "Reptiles",
      "Lichens and Mosses",
      "Vascular Plants"
    )
)
# Look for potential collinearity or other interaction between predictors
# taxonomic group and year

melt(reports_listing[,c("taxonomic_group", "year_published")],
     id.vars = "taxonomic_group") %>%
  ggplot(aes(factor(taxonomic_group), y= value,
             fill= factor(taxonomic_group)))+
  geom_boxplot() +
  facet_wrap(~variable, scales= "free_y") +
  labs(fill= NULL,
       x= "Taxonomic group",
       y= "Year published") + 
  theme(axis.text.x= element_text(angle= 90, hjust = 0, vjust = 0, size = 10))

# Check for zero values
xtabs(~y + taxonomic_group, data=reports_listing)

# remove moss EDIT MARCH 30: no longer necessary when lumping lichens and mosses
#reports_listing = reports_listing %>% filter(taxonomic_group != "Mosses")

listing_report_model = glm(y~ taxonomic_group + year_published,
                           data= reports_listing,
                           family= binomial(link= "logit"))

nrow(reports_listing)
reports_listing %>% filter(y==1) %>% nrow

summary(listing_report_model)
# publishing year is significant

# LRT
# compare full model to Null model (just intercept)
#Find the "test statistic" (the difference in deviance for the two models)
with(listing_report_model, null.deviance - deviance)

#degrees of freedom: 
with(listing_report_model, df.null-df.residual)

#p-value=
with(listing_report_model, pchisq(null.deviance - deviance, 
                                  df.null - df.residual, lower.tail = FALSE))


#### Making a table ####
listing_report_model_tbl <- listing_report_model %>%
  tbl_regression(label= list(
    year_published ~ "Year published",
    taxonomic_group ~ "Taxonomic group"),
    exponentiate = TRUE) %>%
  modify_header(label~"**Predictor**")

listing_report_model_tbl

#### Making a graph ####


odds_ratios_model_3 = round(exp(cbind(OR= coef(listing_report_model), confint(listing_report_model))),digits = 3)

odds_ratios_model_3 = as.data.frame(odds_ratios_model_3)
odds_ratios_model_3 = rownames_to_column(odds_ratios_model_3, var= "Terms")

odds_ratios_model_3$Terms = recode(odds_ratios_model_3$Term, 
                                   "(Intercept)" = "Intercept",
                                   "taxonomic_groupArthropods"= "Arthropods",
                                   "taxonomic_groupAmphibians"= "Amphibians",
                                   "taxonomic_groupFishes (freshwater)"= "Freshwater fishes",
                                   "taxonomic_groupMolluscs"= "Molluscs",
                                   "taxonomic_groupFishes (marine)" = "Marine fishes",
                                   "taxonomic_groupMammals (terrestrial)" = "Terrestrial mammals",
                                   "taxonomic_groupMammals (marine)"= "Marine mammals",
                                   "taxonomic_groupReptiles"= "Reptiles",
                                   "taxonomic_groupLichens and Mosses"= "Lichens and Mosses",
                                   "taxonomic_groupVascular Plants"= "Vascular plants",
                                   "year_published"= "Year published")

# removing intercept, bc typically don't interpret this 
odds_ratios_model_3 = odds_ratios_model_3[-1,]

ggplot(odds_ratios_model_3, aes(x= OR, y= Terms)) +
  geom_point() + theme_classic()+
  geom_errorbarh(aes(xmin= `2.5 %`, xmax = `97.5 %`),height= 0.2) +
  geom_vline(xintercept=1, linetype= "dashed") + 
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size= 12),
        axis.title.x = element_text(size = 12),
        axis.title.y= element_text(size=12)) +
  labs(x= "Odds Ratio (95% confidence interval)")








