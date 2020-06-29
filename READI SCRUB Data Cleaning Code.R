#install.packages("remotes")
#remotes::install_github("ropensci/qualtRics")
library(qualtRics)
library(sjlabelled)
library(memisc)
library(plyr); library(dplyr)

rm(list=ls())
setwd("~/Google Drive/Research/READI COVID Raw Data and Cleaning Code")


# Retrieve Qualtrics surveys ----------------------------------------------


surveys <- all_surveys()

# Retrieve a single survey

# * W1 ORU ----------------------------------------------------------------

d1 <- fetch_survey(surveyID = "SV_bjSOoBhwULqALY1",
                  unanswer_recode = -99,include_display_order = F,
                  force_request = T, breakout_sets = T,
                  time_zone = Sys.timezone(), label = T, fileEncoding = "UTF-8")

d1_breakouts <- d1 %>% dplyr::select(matches("INFO_NEED_[0-9]*$"),
                                     matches("PLEDGE_[0-9]*$")) # INFO_NEED_4 was not measured in ORU W1 (see DD)
d1 <- d1 %>% dplyr::select(-matches("INFO_NEED_[0-9]*$"),
                           -matches("PLEDGE_[0-9]*$"))

# * W1 MASTER -------------------------------------------------------------

d2 <- fetch_survey(surveyID = "SV_57p0waWGUCBGePj",
                   unanswer_recode = -99,include_display_order = F,
                   force_request = T, breakout_sets = T,
                   time_zone = Sys.timezone(), label = T, fileEncoding = "UTF-8")

d2_breakouts <- d2 %>% dplyr::select(matches("INFO_NEED_[0-9]*$"), matches("PLEDGE_[0-9]*$"))
d2 <- d2 %>% dplyr::select(-matches("INFO_NEED_[0-9]*$"), -matches("PLEDGE_[0-9]*$"))

# * W2 ORU ----------------------------------------------------------------

d3 <- fetch_survey(surveyID = "SV_3LhcxaSInKqdSZf",
                   unanswer_recode = -99,include_display_order = F,
                   force_request = T, breakout_sets = T,
                   time_zone = Sys.timezone(), label = T, fileEncoding = "UTF-8")

d3_breakouts <- d3 %>% dplyr::select(matches("bar_fogg_\\w+_[0-9]*$"), matches("INFO_NEED_W2_[0-9]*$"), matches("cald_atsi_[0-9]*$"), matches("manipulation_check_[0-9]*$"))
d3 <- d3 %>% dplyr::select(-matches("bar_fogg_\\w+_[0-9]*$"), -matches("INFO_NEED_W2_[0-9]*$"), -matches("cald_atsi_[0-9]*$"), -matches("manipulation_check_[0-9]*$"))

# * W2 MASTER -------------------------------------------------------------

d4 <- fetch_survey(surveyID = "SV_8d1Q951KB41wsaV",
                   unanswer_recode = -99,include_display_order = F,
                   force_request = T, breakout_sets = T,
                   time_zone = Sys.timezone(), label = T)

d4_breakouts <- d4 %>% dplyr::select(matches("bar_fogg_\\w+_[0-9]*$"), matches("INFO_NEED_W2_[0-9]*$"), matches("cald_atsi_[0-9]*$"), matches("manipulation_check_[0-9]*$"))
d4 <- d4 %>% dplyr::select(-matches("bar_fogg_\\w+_[0-9]*$"), -matches("INFO_NEED_W2_[0-9]*$"), -matches("cald_atsi_[0-9]*$"), -matches("manipulation_check_[0-9]*$"))

# * W3 ORU ------------------------------------------------------------------

d5 <- fetch_survey(surveyID = "SV_6gk7JwsvhryTjfL",
                   unanswer_recode = -99, include_display_order = F,
                   force_request = T, breakout_sets = T,
                   time_zone = Sys.timezone(), label = T, fileEncoding = "UTF-8")

d5_breakouts <- d5 %>% dplyr::select(matches("bar_fogg_\\w+_[0-9]*$"), 
                                     matches("INFO_NEED_W2_[0-9]*$"),
                                     matches("cald_atsi_[0-9]*$"),
                                     matches("manipulation_check_[0-9]*$"))
d5 <- d5 %>% dplyr::select(-matches("bar_fogg_\\w+_[0-9]*$"),
                           -matches("INFO_NEED_W2_[0-9]*$"),
                           -matches("cald_atsi_[0-9]*$"),
                           -matches("manipulation_check_[0-9]*$"))

# * W4 ORU ------------------------------------------------------------------


d6 <- fetch_survey(surveyID = "SV_7aLWgS2MPpryWQB",
                   unanswer_recode = -99, include_display_order = F,
                   force_request = T, breakout_sets = T,
                   time_zone = Sys.timezone(), label = T, fileEncoding = "UTF-8")

d6_breakouts <- d6 %>% dplyr::select(matches("w4_school_neg_[0-9]*$"),
                                     matches("w4_school_pos_[0-9]*$"),
                                     matches("bar_fogg_\\w+_[0-9]*$"),
                                     matches("w4_school_travelmode_[0-9]*$"),
                                     matches("w4_school_type_[0-9]*$"),
                                     matches("w4_wkex_inperson_[0-9]*$"),
                                     matches("w4_wkex_remote_[0-9]*$"),
                                     matches("w4_wkex_trans_[0-9]*$"),
                                     matches("w4_wkex_trans_int_[0-9]*$"),
                                     matches("w4_wkex_trans_precov_[0-9]*$"),
                                     matches("w4_beh_inperson.*_[0-9]*$"),
                                     matches("w4_beh_medical.*_[0-9]*$"))

d6 <- d6 %>% dplyr::select(-matches("w4_school_neg_[0-9]*$"),
                           -matches("w4_school_pos_[0-9]*$"),
                           -matches("bar_fogg_\\w+_[0-9]*$"),
                           -matches("w4_school_travelmode_[0-9]*$"),
                           -matches("w4_school_type_[0-9]*$"),
                           -matches("w4_wkex_inperson_[0-9]*$"),
                           -matches("w4_wkex_remote_[0-9]*$"),
                           -matches("w4_wkex_trans_[0-9]*$"),
                           -matches("w4_wkex_trans_int_[0-9]*$"),
                           -matches("w4_wkex_trans_precov_[0-9]*$"),
                           -matches("w4_beh_inperson.*_[0-9]*$"),
                           -matches("w4_beh_medical.*_[0-9]*$"))

# * W5 ORU ------------------------------------------------------------------


d7 <- fetch_survey(surveyID = "SV_ezDxu1Uhp2iMd6t",
                   unanswer_recode = -99, include_display_order = F,
                   force_request = T, breakout_sets = T,
                   time_zone = Sys.timezone(), label = T, fileEncoding = "UTF-8")

#code to check that the checkboxes below actuall pull the right checkboxes
test_grepl <- function(match_this){names(d7)[grepl(match_this, names(d7))]}
#test_grepl("w5_symptoms_.*_[0-9]*$")

d7_breakouts <- d7 %>% dplyr::select(matches("bar_fogg_\\w+_[0-9]*$"),
                                     matches("drv_ffd_[0-9]*$"),
                                     matches("w5_pastwk.*yn_[0-9]*$"),
                                     matches("w5_hyp_.*_[0-9]*$"),
                                     matches("w5_test_.*_[0-9]*$"),
                                     matches("w5_symptoms_.*_[0-9]*$"),
                                     matches("w5_rules_distance.*[0-9]*$"))
d7 <- d7 %>% dplyr::select(-matches("bar_fogg_\\w+_[0-9]*$"),
                           -matches("drv_ffd_[0-9]*$"),
                           -matches("w5_pastwk.*yn_[0-9]*$"),
                           -matches("w5_hyp_.*_[0-9]*$"),
                           -matches("w5_test_.*_[0-9]*$"),
                           -matches("w5_symptoms_.*_[0-9]*$"),
                           -matches("w5_rules_distance.*[0-9]*$"))


# Join Qualtrics survey datasets ------------------------------------------

# * Recode checkboxes -----------------------------------------------------

make_miss_false <- function(col_to_na){
  #col_to_na <- d6_boxes[, 1]
  col_to_na[col_to_na=="99"] <- "-99"
  col_to_na[col_to_na=="No"] <- "-99"
  col_to_na[col_to_na=="FALSE"] <- "-99"
  col_to_na[col_to_na!="-99"] <- "TRUE"
  col_to_na[col_to_na=="-99"] <- "FALSE"
  sjlabelled::as_factor(col_to_na)
}

d1_breakouts <- as.data.frame(lapply(d1_breakouts, make_miss_false))
d2_breakouts <- as.data.frame(lapply(d2_breakouts, make_miss_false))
d3_breakouts <- as.data.frame(lapply(d3_breakouts, make_miss_false))
d4_breakouts <- as.data.frame(lapply(d4_breakouts, make_miss_false))
d5_breakouts <- as.data.frame(lapply(d5_breakouts, make_miss_false))
d6_breakouts <- as.data.frame(lapply(d6_breakouts, make_miss_false))
d7_breakouts <- as.data.frame(lapply(d7_breakouts, make_miss_false))

# * Join checkbox / non-checkbox data by wave -----------------------------

d1 <- cbind(d1, d1_breakouts)
d2 <- cbind(d2, d2_breakouts)
d3 <- cbind(d3, d3_breakouts)
d4 <- cbind(d4, d4_breakouts)
d5 <- cbind(d5, d5_breakouts)
d6 <- cbind(d6, d6_breakouts)
d7 <- cbind(d7, d7_breakouts)

# Comment out fetching Brazil until we need it
# brazil <- fetch_survey(surveyID = "SV_23NTI2qGvCT8v0V",
#                    unanswer_recode = -99, include_display_order = F,
#                    force_request = T, breakout_sets = F,
#                    time_zone = Sys.timezone(), label = F, convert = FALSE)


# * Add origin and wave metadata ------------------------------------------

d1$origin <- paste(c("oru_export_",gsub("-","_",as.character(Sys.Date()))), collapse = "")
d2$origin <- paste(c("master_export_",gsub("-","_",as.character(Sys.Date()))), collapse = "")
d3$origin <- paste(c("oru_export_",gsub("-","_",as.character(Sys.Date()))), collapse = "")
d4$origin <- paste(c("master_export_",gsub("-","_",as.character(Sys.Date()))), collapse = "")
d5$origin <- paste(c("oru_export_",gsub("-","_",as.character(Sys.Date()))), collapse = "")
d6$origin <- paste(c("oru_export_",gsub("-","_",as.character(Sys.Date()))), collapse = "")
d7$origin <- paste(c("oru_export_",gsub("-","_",as.character(Sys.Date()))), collapse = "")
#brazil$origin <- paste(c("brazil_export_",gsub("-","_",as.character(Sys.Date()))), collapse = "")
d1$wave <- 1
d2$wave <- 1
d3$wave <- 2
d4$wave <- 2
d5$wave <- 3
d6$wave <- 4
d7$wave <- 5
#brazil$wave <- 1
# hadley's style https://style.tidyverse.org/syntax.html#object-names
names(d1) <- tolower(names(d1))
names(d2) <- tolower(names(d2))
names(d3) <- tolower(names(d3))
names(d4) <- tolower(names(d4))
names(d5) <- tolower(names(d5))
names(d6) <- tolower(names(d6))
names(d7) <- tolower(names(d7))


# * Fix per-wave typos before matching --------------------------------------


#Rename two fields so they match old data sets and are processed the same way
d5 <- dplyr::rename(d5,likely_app = intentions_download)
d5$area_code_1_text[is.na(d5$area_code_1_text)] <- d5$ausonly_postcode[is.na(d5$area_code_1_text)]
d5$state[!is.na(d6$ausonly_state)] <- d5$ausonly_state[!is.na(d6$ausonly_state)]
d6$state[!is.na(d6$ausonly_state)] <- d6$ausonly_state[!is.na(d6$ausonly_state)]
d5 <- dplyr::rename(d5, intentions_download = intentions_download_1)

#fix alcohol typo (if not already)
# names(d3)[names(d3)=="othb_alocohol"] <- "othb_alcohol" # Fixed @@AS 2020-06-21
# names(d4)[names(d4)=="othb_alocohol"] <- "othb_alcohol"
d2 <- dplyr::rename(d2, conf_natleaders = conf_leaders,
                    conf_stateleaders = conf_7)


# * Join waves ------------------------------------------------------------


d <- rbind.fill(d1, d2)
d <- rbind.fill(d, d3)
d <- rbind.fill(d, d4)
d <- rbind.fill(d, d5)
d <- rbind.fill(d, d6)

#find new variables to add that aren't checkboxes 
new_vars <- function(newdf, new_checkboxes, olddf){
  #newdf <- d7
  #new_checkboxes <- d7_breakouts
  #olddf <- d
  old_names <- (names(newdf)%in%names(d) | names(newdf)%in%names(new_checkboxes))
  new_names <- names(newdf)[!old_names]
  new_names
}
clean_me <- new_vars(d7, d7_breakouts, d)

d <- rbind.fill(d, d7)
#d <- ?rbind.fill(d, brazil)
d <- d[colSums(is.na(d))!=dim(d)[1]] #qualtrics has exported some deleted variables so remove the completely empty ones

##pulling straight out of qualtrics creates a few ordered factors but the code below 
##was built for processing the values from qualtrics csvs so convert them back :(
ordered_factors <- grepl("ordered",sapply(d, class))
d[, ordered_factors] <- lapply(d[, ordered_factors], as_character)

# dclasses <- unlist(sapply(d, class))
# dclasses <- dclasses[order(names(dclasses))]
# 
# oldclasses <- unlist(sapply(saved_attributes, class))
# oldclasses <- oldclasses[order(names(oldclasses))]


saved_attributes <- d
#d <- saved_attributes

#for all questions except interventions, make unanswered NA
intervention_qns <- grepl("_pledge", names(d))
intervention_qns[1:8] <- T #remove date from the process

make_miss_na <- function(col_to_na){
  col_to_na[col_to_na=="-99"] <- NA
  col_to_na[col_to_na=="99"] <- NA
  col_to_na
}
d[, !intervention_qns] <- lapply(d[, !intervention_qns], make_miss_na)

d$email <- as_character(d$email)
d$recipientemail <- as_character(d$recipientemail)
d$responseid <- as_character(d$responseid)

d <- dplyr::rename(d, aware_factors_pos = aware_factors,
                   aware_factors_neg = unaware_factors,
                   capa_factors_pos = capa_factors,
                   capa_factors_neg = capa_factors_d,
                   opp_factors_pos = opp_factors,
                   opp_factors_neg = opp_factors_d,
                   motiv_factors_pos = motiv_factors,
                   motiv_factors_neg = motiv_factors_neg)

generic_conversion <- function(var_to_change, ordered_levels, ord){
  var_to_change <- gsub("[[:punct:]]", "", var_to_change) #sometimes labels aren't matching due to differences in ascii so remove punctuation
  ordered_levels <- gsub("[[:punct:]]", "", ordered_levels)
  var_to_change <- gsub("\\s+", " ", var_to_change)
  ordered_levels <- gsub("\\s+", " ", ordered_levels)
  #removing punctuation sometimes leads to duplicate spaces
  var_to_change[var_to_change=="-99"] <- NA
  var_to_change[var_to_change==-99] <- NA
  var_to_change <- factor(var_to_change, ordered = ord,
                          levels = ordered_levels)
  return(var_to_change)
}

## Match wave 1 and wave 2 participant IDs
library(readxl)
#link_matching_table <- readxl::read_excel("SCRUB Survey - ORU list W2_repeat respondents.xlsx")
link_matching_table <- readxl::read_excel("SCRUB - Longitudinal data matching - 2020-05-13.xlsx", sheet = 2)
link_matching_table$W3_Email <- gsub("@test.com","",link_matching_table$W3_Email)
d$recipientemail[is.na(d$recipientemail)] <- "test@test.com"
d$recipientemail <- gsub("@test.com","",d$recipientemail)
d$id <- d$responseid #using date as ID here rather than cleaning file so data is still usable

d$id[d$recipientemail!="test"&!is.na(d$recipientemail)] <- d$recipientemail[d$recipientemail!="test"&!is.na(d$recipientemail)]

link_matching_table$W3_Email[is.na(link_matching_table$W3_Email)] <- link_matching_table$W1_RespID[is.na(link_matching_table$W3_Email)]

link_matching_table$W3_Email[is.na(link_matching_table$W3_Email)] <- link_matching_table$W2_RespID[is.na(link_matching_table$W3_Email)]

rows_to_overwrite_id <- which(d$responseid%in%link_matching_table$W1_RespID)

#dplyr::select(d[rows_to_overwrite_id[1],], id, responseid)
for(i in 1:length(rows_to_overwrite_id)){
  #i <- 1
  d$id[rows_to_overwrite_id[i]] <- link_matching_table$W3_Email[which(link_matching_table$W1_RespID==d[rows_to_overwrite_id[i],"responseid"])]
}

rows_to_overwrite_id <- which(d$responseid%in%link_matching_table$W2_RespID)
#dplyr::select(d[rows_to_overwrite_id[1],], id, responseid)
for(i in 1:length(rows_to_overwrite_id)){
  d$id[rows_to_overwrite_id[i]] <- link_matching_table$W3_Email[which(link_matching_table$W2_RespID==d[rows_to_overwrite_id[i],"responseid"])]
}

# remove non-consenters
d$consent <- factor(d$consent)
d <- dplyr::filter(d, consent != 1)
d <- dplyr::select(d, -consent, -q_url)

#remove preview responses and those less than 5 minutes
d <- dplyr::filter(d, status != "Survey Preview")
#d <- dplyr::filter(d, status != "test")
d <- dplyr::filter(d, `duration (in seconds)` >= (5*60) )
d <- dplyr::filter(d, `duration (in seconds)` <= (24*60*60) )

#find when emails have been used more than once
d$email <- stringr::str_to_lower(d$email)
d$recipientemail <- stringr::str_to_lower(d$recipientemail)


#If the email they provided matches another row (either email they provided or the recipient email) then give all records the same id
for(i in unique(d$email[!is.na(d$email)&d$email!="test"])){
  #i <- unique(d$email)[4]
  d[(!is.na(d$email) & d$email == i) | d$recipientemail == i,"id"] <- d[(!is.na(d$email) & d$email == i) | d$recipientemail == i,"id"][1]
}


# remove testing responses
d <- d %>% filter(responseid != "R_b96aPTGwlYJg2hX", # Luca
                  responseid != "R_2pY5a7K2w4XqKtS") # Kate



#table(d$othb_cancel_travel)

#just reversing the order so new data is easier to inspect
d <- arrange(d, desc(startdate))

# remove unnecessary meta-data
head(d)

d <- dplyr::select(d, -enddate:-userlanguage)
d <- dplyr::select(d, -contains("email")) 

# convert always -- never variables
convert_always_never <- function(var_to_change){
  var_to_change <- factor(var_to_change, ordered = T,
                             levels = c("Never", "Rarely",
                                        "Sometimes", "Often", "Always"))
}

beh_questions <- c(grep("^beh_",names(d)),grep("w4_int_beh_",names(d)))
d[, beh_questions] <- lapply(d[, beh_questions], convert_always_never)

ff_physcontact <- grep("ff_physcontact", names(d))
d[, ff_physcontact] <- lapply(d[, ff_physcontact], convert_always_never)

# convert always -- very often variables
convert_very_often_never <- function(var_to_change){
  var_to_change <- factor(var_to_change, ordered = T,
                          levels = c("Never", "Rarely", "Occasionally",
                                     "Sometimes", "Often", "Very often"))
}
acpq <- grep("acpq_",names(d))
d[, acpq] <- lapply(d[, acpq], convert_very_often_never)

# convert awareness variables
convert_awareness <- function(var_to_change){
  var_to_change <- factor(var_to_change, ordered = T,
                             levels = c("Completely unaware","Unaware",
                                        "Somewhat Unaware","Neutral",
                                        "Somewhat aware","Aware",
                                        "Completely aware"))
}

aware_questions <- grep("aware_",names(d))
keep_factors_as_text <- grep("factors",names(d))
## don't do the ones we want to keep as text
aware_questions <- aware_questions[!(aware_questions %in% keep_factors_as_text)]
d[, aware_questions] <- lapply(d[, aware_questions], convert_awareness)

# convert capability variables
convert_capa <- function(var_to_change){
  var_to_change <- factor(var_to_change, ordered = T,
                             levels = c("Very difficult","Difficult",
                                        "Somewhat difficult","Neutral",
                                        "Somewhat easy","Easy","Very easy"))
}

capa_questions <- grep("capa_",names(d))
## don't do the ones we want to keep as text
capa_questions <- capa_questions[!(capa_questions %in% keep_factors_as_text)]
d[, capa_questions] <- lapply(d[, capa_questions], convert_capa)

# convert opportunity variables
convert_opp <- function(var_to_change){
  var_to_change <- factor(var_to_change, ordered = T,
                             levels = c("Hindered a lot","Mostly hindered",
                                        "Hindered a little","Neutral",
                                        "Helped a little","Mostly helped",
                                        "Helped a lot"))
}

opp_questions <- grep("opp_",names(d))
## don't do the ones we want to keep as text
opp_questions <- opp_questions[!(opp_questions %in% keep_factors_as_text)]
d[, opp_questions] <- lapply(d[, opp_questions], convert_opp)

# convert motivation variables
convert_motiv <- function(var_to_change){
  var_to_change <- factor(var_to_change, ordered = T,
                             levels = c("Very unmotivated","Unmotivated",
                                        "Somewhat unmotivated","Neutral",
                                        "Somewhat motivated","Motivated",
                                        "Very motivated"))
}

motiv_questions <- grep("motiv_",names(d))
## don't do the ones we want to keep as text
motiv_questions <- motiv_questions[!(motiv_questions %in% keep_factors_as_text)]
d[, motiv_questions] <- lapply(d[, motiv_questions], convert_motiv)

# fix SWB 4
# d <- dplyr::rename(d, swb4 = swb4_1) # @@AS fixed 2020-06-21
d$swb4 <- gsub("Click to write Scale Point ","",d$swb4)
keep_numbers_only <- function(x) as.numeric(gsub("([0-9]+).*$", "\\1", x))
# convert subjective wellbeing to numeric
swb_questions <- grep("swb",names(d))
d[, swb_questions] <- lapply(d[, swb_questions], keep_numbers_only)
d[, swb_questions] <- lapply(d[, swb_questions], as.numeric)
# convert to same scale 0-10
d$swb4[d$wave==1] <- d$swb4[d$wave==1] - 1

d$hh_size <- keep_numbers_only(d$hh_size)
d$ses_ladder <- keep_numbers_only(d$ses_ladder)

cald_atsi <- grepl("cald_atsi", names(d))
d[, cald_atsi] <- lapply(d[, cald_atsi],make_miss_false)

# convert worry questions into ordered factors
worry_labs <- "1 - Don't worry at all (1)	2 (2)	3 - Worry a little (3)	4 (4)	5 - Worry a fair bit (5)	6 (6)	7 - Worry a lot (7)"

convert_labs_to_list <- function(labs_to_convert){
  labs_to_convert <- gsub("\\([0-9]+\\)", "",labs_to_convert)
  labs_to_convert <- gsub(" $", "",labs_to_convert)
  strsplit(labs_to_convert," \t")
}

worry_labs <- unlist(convert_labs_to_list(worry_labs))
convert_worry <- function(var_to_change){
  var_to_change <- factor(var_to_change, ordered = T,
                             levels = worry_labs)
}

worry_questions <- grep("worry_",names(d))
## don't do the ones we want to keep as text
keep_factors_as_text <- grep("_text",names(d))
worry_questions <- worry_questions[!(worry_questions %in% keep_factors_as_text)]
d[, worry_questions] <- lapply(d[, worry_questions], convert_worry)

### exploratory analyses: sort(colMeans(sapply(d[, worry_questions], as.numeric), na.rm = T))
infected_labs <- "o	Yes, confirmed with a test  (1) 
o	I think so, and I did a test and I am waiting for the result  (2) 
o	I think so, but I can't get tested  (3) 
o	I think so, but I don't want to do a test  (4) 
o	No  (5) 
o	Don't know  (99)"

convert_radio_to_labs <- function(labs_to_convert){
  labs_to_convert <- gsub("o	", "",labs_to_convert)
  labs_to_convert <- gsub("\\([0-9]+\\)", "",labs_to_convert)
  labs_to_convert <- strsplit(labs_to_convert," \n")
  remove_final_space <- function(lab){gsub(" $", "",lab)}
  labs_to_convert <- lapply(labs_to_convert, remove_final_space)
  labs_to_convert <- lapply(labs_to_convert, remove_final_space)
  labs_to_convert <- lapply(labs_to_convert, remove_final_space)
  unlist(labs_to_convert)
}

d$infected <- factor(d$infected,
                        levels = convert_radio_to_labs(infected_labs))

infected_social_labs <- "o	Yes, confirmed with a test  (1) 
o	Yes probably, but not confirmed with a test  (2) 
o	No  (3) 
o	Don't know  (99)"

d$infected_social <- factor(d$infected_social,
                               levels = convert_radio_to_labs(infected_social_labs))

knowledge_labs <- "o	1 - Very poor knowledge  (1) 
o	2  (2) 
o	3  (3) 
o	4  (4) 
o	5  (5) 
o	6  (6) 
o	7 - Very good knowledge  (7)"

w4_rules_know <- grepl("_knowledge", names(d))
d[, w4_rules_know] <- lapply(d[, w4_rules_know],
                          generic_conversion,
                          convert_radio_to_labs(knowledge_labs), T)

d$w5_rules_visit <- generic_conversion(d$w5_rules_visit,
                                       convert_radio_to_labs("o	No visitors  (0) 
o	Up to 5 visitors  (5) 
o	Up to 10 visitors  (10) 
o	Up to 20 visitors  (20) 
o	Up to 75 visitors  (75) 
o	Up to 100 visitors  (100) 
o	Other limit on visitors  (998) 
o	No limit on visitors  (999) 
o	Don't know  (99) "), T)

d$w5_rules_patrons <- generic_conversion(d$w5_rules_patrons,
                                       convert_radio_to_labs("o	No customers  (0) 
o	Up to 10 customers  (10) 
o	Up to 20 customers  (20) 
o	Up to 50 customers  (50) 
o	Other limit on customers  (998) 
o	No limit on customers  (999) 
o	Don't know  (99)"), T)

notest_labs <- "o	Didn't think I had COVID-19  (1) 
o	I thought my symptoms were too mild to get tested  (2) 
o	Didn't know where to get tested  (3) 
o	I thought I needed to pay for the test  (4) 
o	I didn't want to wait to get tested  (5) 
o	I couldn't miss work to get tested  (6) 
o	Couldn't get to a testing facility  (7) 
o	I was worried about contracting COVID-19 in the process of getting tested  (8) 
o	I was worried about passing COVID-19 on to others in the process of getting tested  (9) 
o	The test is painful/uncomfortable  (10) 
o	Didn't want to know whether I had COVID-19  (11) 
o	There are not many cases in my area  (12) 
o	Didn't want to take up resources needed for others  (13) 
o	Other (please specify)  (14)"
d$w5_hyp_notest_m <- generic_conversion(d$w5_hyp_notest_m,
                                        convert_radio_to_labs(notest_labs), F)
d$w5_symptoms_notest_m <- generic_conversion(d$w5_symptoms_notest_m,
                                        convert_radio_to_labs(notest_labs), F)


symp_labs <- "Related to COVID-19 (1)	Not related to COVID-19 (2)	Don't know"

symp_labs <- unlist(convert_labs_to_list(symp_labs))
convert_symp <- function(var_to_change){
  var_to_change <- factor(var_to_change,
                             levels = symp_labs)
}

symp_questions <- grep("symp_",names(d))

d[, symp_questions] <- lapply(d[, symp_questions], convert_symp)


treatment_labs <- "o	There is a drug to treat COVID-19.  (1) 
o	There is a vaccine for COVID-19.  (2) 
o	There is both a drug for the treatment and a vaccine for COVID-19.  (3) 
o	There is currently no drug treatment or vaccine for COVID-19.  (4) 
o	Don't know  (99)"

d$treatment <- factor(d$treatment,
                         levels = convert_radio_to_labs(treatment_labs))

transmission_labs <- "o	COVID-19 can spread from one person to another person.  (1) 
o	COVID-19 can spread from animals to humans only.  (100) 
o	COVID-19 cannot spread from one carrier to another.  (101) 
o	Don't know  (99)"

d$transmission <- factor(d$transmission,
                            levels = convert_radio_to_labs(transmission_labs))

symptom_labs <- convert_labs_to_list("Mild (1)	Moderate (2)	Severe (3)	Did not experience this symptom (99)")
w5_symp <- grepl("w5_test_symp*", names(d))

d[, w5_symp] <- lapply(d[, w5_symp],
                       generic_conversion,
                       unlist(symptom_labs), T)

prob_labs <- "o	1 - Extremely unlikely  (1) 
o	2 - Unlikely  (2) 
o	3 - A little unlikely  (3) 
o	4 - Neutral  (4) 
o	5 - A little likely  (5) 
o	6 - Likely  (6) 
o	7 - Extremely likely  (7)"

d$prob <- factor(d$prob, ordered = T,
                    levels = convert_radio_to_labs(prob_labs))


prob_question <- grep("prob_",names(d))

get_num_before_percentage <- function(response){
  all_responses <- response
  valid_rows <- !is.na(response)
  response <- response[valid_rows]
  response <- unlist(regmatches(response, gregexpr("\\d+(\\.\\d+){0,1}%", response)))
  response <- gsub("\\%","",response)
  response <- as.numeric(response)
  all_responses[valid_rows] <- response
  as.numeric(all_responses)
}
d[, prob_question] <- sapply(d[, prob_question], get_num_before_percentage)

#get number of days
d$othb_pa_1 <- keep_numbers_only(d$othb_pa_1)
d$othb_pa_2 <- keep_numbers_only(d$othb_pa_2)
d$othb_pa_3[d$othb_pa_3>24] <- NA
d$othb_alcohol <- keep_numbers_only(d$othb_alcohol)
d$othb_alcohol_alt <- keep_numbers_only(d$othb_alcohol_alt)

#strip the number for each response
likely_qns <- grep("likely_", names(d))
d[, likely_qns] <- lapply(d[, likely_qns], keep_numbers_only)


d$w5_pastwk_ff_occ <- keep_numbers_only(d$w5_pastwk_ff_occ)
d$w5_int_ff_occ <- keep_numbers_only(d$w5_int_ff_occ)

#change predictions about time period to ordered factors
prediction_labs <- c("Less than one month",
                     "One month", "Two months", 
                     "Three months", "Four months", 
                     "Five months", "6-12 months", 
                     "12-18 months", "18-24 months",
                     "More than 24 months (2 years)")
convert_preds <- function(var_to_change){
  var_to_change <- factor(var_to_change, ordered = T,
                          levels = prediction_labs)
}
pred_vars <- grep("pred_", names(d))
d[, pred_vars] <- lapply(d[, pred_vars], convert_preds)

# change severity, prep, efficacy, following to s-disagree to s-agree
sev_rows <- which(names(d)=="severity"):(which(names(d)=="severity")+3)
agree_labs <- "Strongly disagree (1)	Disagree (2)	Somewhat disagree (3)	Neither agree nor disagree (4)	Somewhat agree (5)	Agree (6)	Strongly agree (7)"

agree_labs <- unlist(convert_labs_to_list(agree_labs))
convert_agree <- function(var_to_change){
  var_to_change <- factor(var_to_change, ordered = T,
                             levels = agree_labs)
}
d[, sev_rows] <- lapply(d[, sev_rows], convert_agree)

agree_labs_5 <- "Strongly disagree (1)	Disagree (2)	Neither agree nor disagree (3)	Agree (4)	Strongly agree (5)"
agree_labs_5 <- unlist(convert_labs_to_list(agree_labs_5))
convert_agree_5 <- function(var_to_change){
  var_to_change <- factor(var_to_change, ordered = T,
                          levels = agree_labs_5)
}
drv_questions <- grepl("drv_",names(d)) & !grepl("text",names(d))
d[, drv_questions] <- lapply(d[, drv_questions], convert_agree_5)

other_labs <- "Not at all (1)	A little (2)	A moderate amount (3)	A lot (4)	A great deal (5)	N/A (99)"
other_labs <- unlist(convert_labs_to_list(other_labs))
convert_other <- function(var_to_change){
  var_to_change[var_to_change=="None at all"] <- "Not at all"
  var_to_change[var_to_change=="Never"] <- "Not at all"
  var_to_change[var_to_change=="Rarely"] <- "A little"
  var_to_change[var_to_change=="Sometimes"] <- "A moderate amount"
  var_to_change[var_to_change=="Often"] <- "A lot"
  var_to_change[var_to_change=="Always"] <- "A great deal"
  var_to_change[var_to_change==-99] <- NA
  var_to_change <- factor(var_to_change, ordered = T,
                             levels = other_labs)
}
# qualtrics was not published before releasing survey to participants,
# so response scale is different for these two sets of questions
names(d)[grepl("othb_",names(d))]
other_questions <- grepl("othb_",names(d)) & !grepl("_pa_",names(d)) & !grepl("_alc",names(d))
d[, other_questions] <- lapply(d[, other_questions], convert_other)

# d$info_need_symptoms <- grepl("Symptoms", d$info_need) @@AS: with breakout boxes these sets should no longer be neccessary.
# d$info_need_stories <- grepl("stories", d$info_need)
# d$info_need_science <- grepl("Scientific", d$info_need)
# d$info_need_actions <- grepl("personally", d$info_need)
# d$info_need_risk_group <- grepl("group", d$info_need)
# d$info_need_education <- grepl("education", d$info_need)
# d$info_need_travel <- grepl("travel", d$info_need)
# d$info_need_my_risk <- grepl("personal risk", d$info_need)
# d$info_need_testing <- grepl("tested", d$info_need)
# d$info_need_none <- grepl("don", d$info_need)
# 
# info_need_items <- grepl("info_need_", names(d))
# info_need_items <- xor(info_need_items, grepl("info_need_w2", names(d))) 
# d[is.na(d$info_need), info_need_items] <- NA
#   
# d <- dplyr::select(d, -info_need)
# d$info_need_w2_government_tv <- grepl("Government-funded television channels", d$info_need_w2)
# attr(d$info_need_w2_government_tv, "label") <- "Government-funded television channels"
# 
# d$info_need_w2_government_radio <- grepl("Government-funded radio stations", d$info_need_w2)
# attr(d$info_need_w2_government_radio, "label") <- "Government-funded radio channels"
# 
# d$info_need_w2_commercial_radio <- grepl("Commercial radio stations", d$info_need_w2)
# attr(d$info_need_w2_commercial_radio, "label") <- "Commercial radio stations"
# 
# d$info_need_w2_commercial_tv <- grepl("Commercial television stations", d$info_need_w2)
# attr(d$info_need_w2_commercial_tv, "label") <- "Commercial television stations"
# 
# d$info_need_w2_commercial_online <- grepl("Commercial online news", d$info_need_w2)
# attr(d$info_need_w2_commercial_online, "label") <- "Commercial online news"
# 
# d$info_need_w2_newspapers <- grepl("Daily or weekly newspapers", d$info_need_w2)
# attr(d$info_need_w2_newspapers, "label") <- "Daily or weekly newspapers"
# 
# d$info_need_w2_family_friends <- grepl("Family and friends", d$info_need_w2)
# attr(d$info_need_w2_family_friends, "label") <- "Family and friends"
# 
# d$info_need_w2_coworkers <- grepl("Co-workers", d$info_need_w2)
# attr(d$info_need_w2_coworkers, "label") <- "Co-workers"
# 
# d$info_need_w2_healthcare <- grepl("Consultation with health care workers", d$info_need_w2)
# attr(d$info_need_w2_healthcare, "label") <- "Consultation with health care workers"
# 
# d$info_need_w2_authority_websites <- grepl("Government or health authority websites", d$info_need_w2)
# attr(d$info_need_w2_authority_websites, "label") <- "Government or health authority websites"
# 
# d$info_need_w2_search_engines <- grepl("Search engines", d$info_need_w2)
# attr(d$info_need_w2_search_engines, "label") <- "Search engines"
# 
# d$info_need_w2_social_media <- grepl("Social media", d$info_need_w2)
# attr(d$info_need_w2_social_media, "label") <- "Social media"
# 
# info_need_items <- grepl("info_need_w2", names(d))
# d[is.na(d$info_need_w2), info_need_items] <- NA
# d <- dplyr::select(d, -info_need_w2)

informed_questions <- grep("informed_",names(d))
keep_factors_as_text <- grep("_text",names(d))
informed_questions <- informed_questions[!(informed_questions %in% keep_factors_as_text)]
d[, informed_questions] <- lapply(d[, informed_questions], convert_always_never)

follow_questions <- grep("follow_",names(d))
names(d)[follow_questions]
follow_questions <- follow_questions[!(follow_questions %in% keep_factors_as_text)]
d[, follow_questions] <- lapply(d[, follow_questions], convert_always_never)

confidence_labs <- "Very low confidence (1)	2 (2)	3 (3)	4 (4)	5 (5)	6 (6)	Very high confidence (7)"

confidence_labs <- unlist(convert_labs_to_list(confidence_labs))
convert_confidence <- function(var_to_change){
  var_to_change <- factor(var_to_change, ordered = T,
                             levels = confidence_labs)
}
confidence_questions <- grep("conf_",names(d))
keep_factors_as_text <- grep("_text",names(d))
## don't do the ones we want to keep as text
confidence_questions <- confidence_questions[!(confidence_questions %in% keep_factors_as_text)]

d[, confidence_questions] <- lapply(d[, confidence_questions], convert_confidence)
d$gender <- factor(d$gender, levels = c("Male", "Female", "Other (specify)"))

education_labs <- "o	Some high school  (1) 
o	Completed high school  (2) 
o	Associate's degree or technical education  (3) 
o	Undergraduate degree  (4) 
o	Master's degree  (5) 
o	Doctorate  (6)"
d$education <- factor(d$education, ordered = T,
                         levels = convert_radio_to_labs(education_labs))

w3_rules_knowledge_labs <- "o	1 - Very poor knowledge  (1) 
o	2  (2) 
o	3  (3) 
o	4  (4) 
o	5  (5) 
o	6  (6) 
o	7 - Very good knowledge  (7) "
d$w3_rules_knowledge <- factor(d$w3_rules_knowledge, ordered = T,
                      levels = convert_radio_to_labs(w3_rules_knowledge_labs))
knowledge_items <- grepl("w4_rules_", names(d))
d[, knowledge_items] <- lapply(d[, knowledge_items],
                          factor, ordered = T,
                          levels = convert_radio_to_labs(w3_rules_knowledge_labs))

d$w3_rules_self <- convert_always_never(d$w3_rules_self)

loop_radios <- "o	work at your job  (1) 
o	purchase prepared food or drink from a restaurant or cafe  (5) 
o	receive health or medical care  (6) 
o	socialise with friends or family  (7) 
o	purchase goods or services  (8) 
o	participate in public activities  (9) "
loop_radios <- convert_radio_to_labs(loop_radios)

w4_test_items <- grep("w4_test", names(d))
d[, w4_test_items] <- lapply(d[, w4_test_items], as_numeric)
d$ed_work_inperson <- as_numeric(d$ed_work_inperson)
d$ed_work_remote <- as_numeric(d$ed_work_remote)

#attach the labels then convert to te best format
pastwk_beh_vars <- grep("w3_pastwk_beh$", names(d))
d[, pastwk_beh_vars] <- lapply(d[, pastwk_beh_vars], keep_numbers_only)
set_label(d[, pastwk_beh_vars]) <- paste("Think back to last week, from Monday 4 May to Sunday 10 May. On how many of those days did you ",
                                         loop_radios,
                                         "?", sep = "")


pastwk_inperson <- grep("w3_pastwk_inperson$", names(d))
d[, pastwk_inperson] <- lapply(d[, pastwk_inperson], keep_numbers_only)
set_label(d[, pastwk_inperson]) <- paste("Last week, on how many days did you",
                                         loop_radios,
                                         "in person?")

pastwk_remote <- grep("w3_pastwk_remote$", names(d))
d[, pastwk_remote] <- lapply(d[, pastwk_remote], keep_numbers_only)
set_label(d[, pastwk_remote]) <- paste("Last week, on how many days did you",
                                         loop_radios,
                                         "remotely?")

risky_vars <- grepl("w3_risk|w4_wkex_risk|.*_w3_risk", names(d))

risky_labs <- "o	1 - Not at all risky  (1) 
o	2  (2) 
o	3  (3) 
o	4  (4) 
o	5  (5) 
o	6  (6) 
o	7 - Extremely risky  (7)"

labels_to_fix <- grepl(".*_w3_risk", names(d))
risky_questions_w4 <- get_label(d[, names(d)[labels_to_fix]])

d[, risky_vars] <- lapply(d[, risky_vars],
                          generic_conversion,
                          convert_radio_to_labs(risky_labs), T)

set_label(d[, names(d)[labels_to_fix]]) <- paste("Considering the COVID-19 situation, how risky is it for you to",
                                       loop_radios,
                                       "?")
set_label(d[, names(d)[grepl("w4_wkex_risk", names(d))]]) <- paste("How risky is...?",
                                                                  get_label(d[, grepl("w4_wkex_risk", names(d))]), sep = " ")

intention_vars <- grep("w3_int$", names(d))
d[, intention_vars] <- lapply(d[, intention_vars], keep_numbers_only)
set_label(d[, intention_vars]) <- paste("Now think about the next 7 days after today. On how many of those days do you intend to ",
                                    loop_radios,
                                    "?", sep = "")

vic_vars <- grep("w3_postvic$", names(d))
d[, vic_vars] <- lapply(d[, vic_vars],
                          factor)
set_label(d[, vic_vars]) <- paste("Changes in COVID-19 rules and regulations took effect in Victoria from the start of Wednesday 13 May. Did you",
                                        loop_radios,
                                        "in person since then?")

loop_merge_cols <- grepl("^[0-9]_w3",names(d))|grepl("^[0-9]_w4",names(d))
names(d)[loop_merge_cols] <- gsub("1_","work_",names(d)[loop_merge_cols])
names(d)[loop_merge_cols] <- gsub("5_","cafe_",names(d)[loop_merge_cols])
names(d)[loop_merge_cols] <- gsub("6_","medical_",names(d)[loop_merge_cols])
names(d)[loop_merge_cols] <- gsub("7_","socialise_",names(d)[loop_merge_cols])
names(d)[loop_merge_cols] <- gsub("8_","shopping_",names(d)[loop_merge_cols])
names(d)[loop_merge_cols] <- gsub("9_","activities_",names(d)[loop_merge_cols])

#attach the labels then convert to te best format
pastwk_beh_vars <- grepl("w4_pastwk_inperson", names(d))
set_label(d[, pastwk_beh_vars]) <- paste("What proportion of time did you ",
                                         loop_radios[-3],
                                         " in person compared to doing it remotely?", sep = "")
pastwk_beh_vars <- grepl("w4_int_inperson", names(d))
set_label(d[, pastwk_beh_vars]) <- paste("What proportion of that time do you intend to ",
                                         loop_radios[-3],
                                         " in person compared to doing it remotely?", sep = "")


pt_days <- c(grep("^w3_pastwk_beh_pub", names(d)),
             grep("^w3_pt_pastwk_beh", names(d)),
             grep("w4_pastwk_ffsocial", names(d)),
             grep("w4_int_ffsocial", names(d)))
transports <- c("Public transport","Active transport","Private transport")
d[, pt_days] <- lapply(d[, pt_days], keep_numbers_only)
pt_days <- pt_days[1:3]
set_label(d[, pt_days]) <- paste("Think back to last week, from Monday 4 May to Sunday 10 May. On how many of those days did you use the following methods of transport?",
                                 transports)
set_label(d$socialise_w4_pastwk_ffsocial) <- "On how many of those days did you see family and friends in person in a private space (e.g. your home)?"
set_label(d$socialise_w4_int_ffsocial) <- "On how many of those days do you intend to see family and friends in person in a private space (e.g. your home)?"

pt_risk <- grep("^w3_pt_risk", names(d))
d[, pt_risk] <- lapply(d[, pt_risk],
                          generic_conversion,
                          convert_radio_to_labs(risky_labs), T)
set_label(d[, pt_risk]) <- paste("Considering the COVID-19 situation, how risky is it for you to use the following methods of transport?",
                                 transports)

pt_intent <- grep("^w3_pt_int", names(d))
d[, pt_intent] <- lapply(d[, pt_intent], keep_numbers_only)
set_label(d[, pt_intent]) <- paste("Now think about the next 7 days after today. On how many of those days do you intend to use the following methods of transport?",
                                 transports)

pt_postvic <- grep("^w3_pt_postvic", names(d))
d[, pt_postvic] <- lapply(d[, pt_postvic], factor)
set_label(d[, pt_postvic]) <- paste("Changes in COVID-19 rules and regulations took effect in Victoria from the start of Wednesday 13 May. Did you use any of these methods of transport since then?",
                                   transports)

set_label(d$w3_pastwk_beh_govtpay) <- "Think back to last week, from Monday 4 May to Sunday 10 May. In this period, did you access government payments related to COVID-19 (e.g., JobKeeper, JobSeeker)?"
d$w3_pastwk_beh_govtpay <- factor(d$w3_pastwk_beh_govtpay)
set_label(d$w3_pastwk_beh_tested) <- "Think back to last week, from Monday 4 May to Sunday 10 May. In this period, did you get tested for COVID-19?" 
d$w3_pastwk_beh_tested <- factor(d$w3_pastwk_beh_tested)

pt_measures <- "If public transport stations and vehicles were sanitised multiple times per day (1) 
If public transport stations and vehicles were modified to reduce maximum passenger density (e.g., blocking off rows of seats) (4) 
If public transport stations and vehicles were modified to increase availability of hand sanitiser (5) 
If fewer people used public transport (e.g., 50% of normal density) (6) 
If my family, friends, or work colleagues started using public transport again (8) 
If the government made it clear that it wanted people to use public transport (9) 
If the government allowed more public activities (e.g., restaurants, bars, cinemas) (11) 
If most people chose to wear a face mask on public transport (13) 
Attention check, 3 equals attentive"

likely_labs <- "Much less likely (1)	Less likely (2)	Have no impact (3)	More likely (4)	Much more likely (5)"

measure_items <- grep("^w3_pt_measures", names(d))
d[, measure_items] <- lapply(d[, measure_items],
                       generic_conversion,
                       unlist(convert_labs_to_list(likely_labs)),
                       T)
d$w3_pt_peak <- factor(d$w3_pt_peak)
set_label(d[, measure_items]) <- paste("Consider the following public transport COVID-19 measures. Rate each of the measures in terms of whether it would make you more likely or less likely to use public transport, if enacted.",
                                    convert_radio_to_labs(pt_measures))

measure_items <- grep("w4_wkex_measures", names(d))
measure_labels <- get_label(d[, names(d)[measure_items]])
d[, measure_items] <- lapply(d[, measure_items],
                             generic_conversion,
                             unlist(convert_labs_to_list(likely_labs)),
                             T)
set_label(d[, names(d)[measure_items]]) <- paste("Rate each of the measures in terms of whether it would make you more likely or less likely to attend work IN PERSON, if enacted.",
                                                 measure_labels, sep = " ")

peak_measures <- "If I could change school or childcare drop-off hours for my children (1) 
If I could change my work or education hours to use public transport at quieter times  (4) 
If my employer directed me to start or end work at off-peak times (e.g., earlier or later) (5) 
If travel was discounted in off-peak hours (6) 
If I had access to real time information about which services were more crowded (7) "
peak_items <- grep("^w3_pt_peak_measures", names(d))
d[, peak_items] <- lapply(d[, peak_items],
                             generic_conversion,
                             unlist(convert_labs_to_list(likely_labs)),
                             T)

set_label(d[, peak_items]) <- paste("Consider the following public transport COVID-19 measures. Rate each of the measures in terms of whether it would make you more likely or less likely to use public transport, if enacted.",
                                       convert_radio_to_labs(peak_measures))


pt_diagnos <- c("It is a good idea for me to use public transport right now",
                "People important to me are using public transport right now",
                "I have a choice about whether I use public transport right now")
diag_items <- grep("^w3_pt_diag", names(d))
d[, diag_items] <- lapply(d[, diag_items],
                          convert_agree_5)
set_label(d[, diag_items]) <- pt_diagnos

econ_measures <- "If shops and public places were sanitised multiple times per day (1) 
If shops and public places were modified to increase availability of hand sanitiser (5) 
If fewer people visited shops and public places (e.g., 50% of normal density) (6) 
If my family, friends, or work colleagues started visiting shops and public places again (8) 
If the government made it clear that it wanted people to visit shops and public places (9) 
If the government allowed more public activities (e.g., restaurants, bars, cinemas) (11) 
If most people chose to wear a face mask when visiting shops and public places (13) 
Attention check, 3 equals attentive"
econ_items <- grep("^w3_econ_measures", names(d))
d[, econ_items] <- lapply(d[, econ_items],
                          generic_conversion,
                          unlist(convert_labs_to_list(likely_labs)),
                          T)
set_label(d[, econ_items]) <- paste("Consider the following economic COVID-19 measures. Rate each of the measures in terms of whether it would make you more likely or less likely to visit shops and public places, if enacted.",
                                       convert_radio_to_labs(econ_measures))

econ_diags <- grep("^w3_econ_diag", names(d))
econ_diag_labs <- c("It is a good idea for me to spend money right now",
                    "People important to me are spending money right now", 
                    "I have a choice about whether I spend money right now")
d[, econ_diags] <- lapply(d[, econ_diags],
                          convert_agree_5)
set_label(d[, econ_diags]) <- econ_diag_labs

interventions <- grep("^apptxt", names(d))[-1]
d[, interventions] <- lapply(d[, interventions], is.na)
d[, interventions] <- !d[, interventions]


d$cald_english <-factor(d$cald_english, ordered = T,
                        levels = c("Not at all",
                                   "Not well",
                                   "Well",
                                   "Very well"))


#re-order the factors for household income
d$hh_income <- factor(d$hh_income, ordered = T,
                      levels = names(table(d$hh_income))[c(18,1,8,11:17,2:7,9:10)])

#get numbers for need support
need_support_items <- grepl("need_support", names(d))
d[, need_support_items] <- lapply(d[, need_support_items], keep_numbers_only)

#### Manipulation checks for wave 2 #### @@ AS no longer needed due to split out as separate items
# d$manip_check_info <- grepl("tracking", d$manipulation_check) # @@AS: manipulation_check_1
# d$manip_check_safe <- grepl("protected", d$manipulation_check) # @@AS: manipulation_check_4
# d$manip_check_no_safe <- grepl("long-term", d$manipulation_check) # @@AS: manipulation_check_5
# d$manip_check_aut <- grepl("activate", d$manipulation_check) # @@AS: manipulation_check_2
# d$manip_check_cont <- grepl("must", d$manipulation_check) # @@AS: manipulation_check_3

#check if allocation matches attention check
#if in either autonomy support conditions and clicked choice manip check but not control
#or in either control condition and clicked control manip check but not choice

#convert conditions to boolean
d$cont_safe <- !is.na(d$cont_safe)
d$cont_nosafe <- !is.na(d$cont_nosafe)
d$aut_safe <- !is.na(d$aut_safe)
d$aut_nosafe <- !is.na(d$aut_nosafe)

#convert main-effects to boolean
d$aut_int <- d$aut_nosafe | d$aut_safe
d$safe_int <- d$cont_safe | d$aut_safe

#check maniplations for each main effect
# d$autonomy_manip_check_passed <- (d$aut_int & d$manip_check_aut & !d$manip_check_cont) | # @@ AS these vars don't exist in my edits
#   (!d$aut_int & !d$manip_check_aut & d$manip_check_cont)

d$autonomy_manip_check_passed <- (d$aut_int & d$manipulation_check_2 & !d$manipulation_check_3) | # @@ AS alternative manip check
  (!d$aut_int & !d$manipulation_check_2 & d$manipulation_check_3)

# d$safety_manip_check_passed <- (d$safe_int & d$manip_check_safe & !d$manip_check_no_safe) |# @@ AS these vars don't exist in my edits
#   (!d$safe_int & !d$manip_check_safe & d$manip_check_no_safe) 

d$safety_manip_check_passed <- (d$safe_int & d$manipulation_check_4 & !d$manipulation_check_5) |
  (!d$safe_int & !d$manipulation_check_4 & d$manipulation_check_5) 


d$intentions_total <- d$intentions_download + d$intentions_share + d$intentions_share

manip_check_items <- grepl("manipulation_check_", names(d))
d[d$wave != 2, manip_check_items] <- NA

aut_control_items <- grep("aut|cont|safe_", names(d))
d[d$wave != 2, aut_control_items] <- NA

pledge_items <- grep("pledge_", names(d))
d[d$wave != 1, pledge_items] <- NA

library(readr)
d$area_code <- parse_number(d$area_code_1_text)
d$area_code <- as.numeric(d$area_code)
c <- d
which_aus <- which(d$country == "Australia")
d <- d[which_aus, ]
table(d$state)

d$state[grepl("2000",tolower(d$state))] <- "New South Wales"
d$state[grepl("nsw",tolower(d$state))] <- "New South Wales"
d$state[grepl("ydney",d$state)] <- "New South Wales"
d$state[grepl("vic",tolower(d$state))] <- "Victoria"
d$state[grepl("melb",tolower(d$state))] <- "Victoria"
d$state[grepl("wa",tolower(d$state))] <- "Western Australia"
d$state[grepl("sa",tolower(d$state))] <- "South Australia"
d$state[grepl("qld",tolower(d$state))] <- "Queensland"
d$state[grepl("risban",tolower(d$state))] <- "Queensland"
d$state[grepl("queensland",tolower(d$state))] <- "Queensland"
d$state[grepl("tas",tolower(d$state))] <- "Tasmania"
d$state[grepl("nt",tolower(d$state))] <- "Northern Territory"
d$state[grepl("vic",tolower(d$area_code_1_text))] <- "Victoria"
d$state[grepl("qld",tolower(d$area_code_1_text))] <- "Queensland"
d$state[grepl("parkville",tolower(d$state))] <- "Victoria"
d$state[grepl("act",tolower(d$state))] <- "Australian Capital Territory"
d$state[grepl("penrith",tolower(d$state))] <- "New South Wales"
d$state[grepl("perth",tolower(d$state))] <- "Western Australia"
d$area_code[grepl("2839",tolower(d$state))] <- 2839
d$area_code[grepl("3173",tolower(d$state))] <- 3173
d$state[d$area_code %in% 1000:1999] <- "New South Wales"
d$state[d$area_code %in% 2000:2599] <- "New South Wales"
d$state[d$area_code %in% 2619:2899] <- "New South Wales"
d$state[d$area_code %in% 2921:2999] <- "New South Wales"
d$state[d$area_code %in% 0200:0299] <- "Australian Capital Territory"
d$state[d$area_code %in% 2600:2618] <- "Australian Capital Territory"
d$state[d$area_code %in% 2900:2920] <- "Australian Capital Territory"
d$state[d$area_code %in% 3000:3999] <- "Victoria"
d$state[d$area_code %in% 8000:8999] <- "Victoria"
d$state[d$area_code %in% 4000:4999] <- "Queensland"
d$state[d$area_code %in% 5000:5999] <- "South Australia"
d$state[d$area_code %in% 6000:6999] <- "Western Australia"
d$state[d$area_code %in% 7000:7999] <- "Tasmania"
d$state[d$area_code %in% 0800:0999] <- "Northern Territory"
table(d$state)

c[which_aus, ] <- d
d <- c

d$state_aus <- factor(d$state, levels = c("Australian Capital Territory",
                                          "New South Wales",
                                          "Northern Territory",
                                          "Queensland",
                                          "South Australia",
                                          "Tasmania",
                                          "Victoria",
                                          "Western Australia"))

lcri <- grep("lcri_|mod_peb_comf", names(d))
d[, lcri] <- lapply(d[, lcri],
                    generic_conversion,
                    unlist(convert_labs_to_list("Strongly disagree (1)	Disagree (2)	Neutral (3)	Agree (4)	Strongly agree (5)")),
                    T)
d$mod_peb_lcb_temp <- generic_conversion(d$mod_peb_lcb_temp,
                   convert_radio_to_labs("o	Never  (1) 
o	Rarely  (2) 
o	Sometimes  (3) 
o	Often  (4) 
o	Very often  (5) 
o	Always  (6)"),T)

d$mod_peb_lci_fridge <- generic_conversion(d$mod_peb_lci_fridge,
                                           convert_radio_to_labs("o	Definitely do not intend  (1) 
o	Probably do not intend  (2) 
o	Undecided  (3) 
o	Probably do intend  (4) 
o	Definitely do intend  (5) 
o	Already have one  (77)"),T)

d$mod_peb_lci_washing <- generic_conversion(d$mod_peb_lci_washing,
                                           convert_radio_to_labs("o	Definitely do not intend  (1) 
o	Probably do not intend  (2) 
o	Undecided  (3) 
o	Probably do intend  (4) 
o	Definitely do intend  (5) 
o	Already have one  (77)"),T)

d$mod_peb_lci_heat <- generic_conversion(d$mod_peb_lci_heat,
                                           convert_radio_to_labs("o	Definitely do not intend  (1) 
o	Probably do not intend  (2) 
o	Undecided  (3) 
o	Probably do intend  (4) 
o	Definitely do intend  (5) 
o	I already have an energy efficient heating system with 5-6 star energy rating  (77) 
o	I have a reverse cycle heating and cooling system  (88)"),T)

d$mod_peb_lci_cool <- generic_conversion(d$mod_peb_lci_cool,
                                         convert_radio_to_labs("o	Definitely do not intend  (1) 
o	Probably do not intend  (2) 
o	Undecided  (3) 
o	Probably do intend  (4) 
o	Definitely do intend  (5) 
o	I already have an energy efficient cooling system with 5-6 star energy rating  (77) 
o	I have a reverse cycle heating and cooling system  (88)"),T)

# Factor variables that are coming in as characters ------------------------
vars_to_factor <- c("health_hcw",
                    "apptxt_prebeh",
                    "chronic",
                    "health",
                    "w5_test_gen",
                    "food_space",
                    "food_check",
                    "w5_covid",
                    "w4_test_homewait",
                    "w5_test_pst_week",
                    "country",
                    "screen_job",
                    "residency",
                    "children",
                    "cald_cob",
                    "cald_lote",
                    "pregnant",
                    "ausonly_state",
                    "state",
                    "w3_pt_use",
                    "w4_school_child_ch",
                    "w4_school_child_mode",
                    "w4_school_adult_ch",
                    "w4_school_adult_mode",
                    "w4_wkex_permit",
                    "long_optin",
                    "origin",
                    "w5_test_reason_main",
                    "w5_test_location",
                    names(d)[grepl("pol_",names(d))],
                    names(d)[grepl("employ", names(d)) & !grepl("_arr", names(d))],
                    names(d)[grepl("food_[a-z]*_pastwk", names(d))],
                    names(d)[grepl("food_[a-z]*_precovid", names(d))])


d[, vars_to_factor] <- lapply(d[, vars_to_factor], factor)

d$state_aus[is.na(d$state_aus)] <- d$ausonly_state[is.na(d$state_aus)]

library(readxl)
regions <- read_xls("CG_POSTCODE_2017_RA_2016.xls", 4, skip = 5)
regions <- regions[!is.na(regions$POSTCODE_2017...1),]
d$region_aus <- as_numeric(NA)
d$region_aus_type <- as_character(NA)
d$region_aus[which_aus] <- regions$RA_CODE_2016[match(d[which_aus,names(d)=="area_code"],
                                                      regions$POSTCODE_2017...1)]
d$region_aus <- as_numeric(d$region_aus)
d$region_aus_type[which_aus] <- regions$RA_NAME_2016[match(d[which_aus,names(d)=="area_code"],
                                                regions$POSTCODE_2017...1)]
d$region_aus_type <- as_factor(d$region_aus_type)
# @@AS I somehow broke this inhab_labels, possibly due to an PC/MAC encoding issue for "less
# than or equal to" (it shows as garbled characters on my screen)
inhab_labels <- "≤ 5,000 people  (1) 
o	5,001 - 20,000  (2) 
o	20,001 - 100,000  (3) 
o	100,001 - 500,000  (4) 
o	> 500,000 people  (5)"

d$inhabitants <- factor(d$inhabitants, ordered = T,
                           levels = convert_radio_to_labs(inhab_labels))

sac_cols <- grep("_sac$",names(d))

sac_items <- as.data.frame(d[, sac_cols])
sac_col_to_extract <- which(!is.na(sac_items), arr.ind = T)
sac_col_to_extract <- sac_col_to_extract[order(sac_col_to_extract[, 1]), ]
will_you_stand_against_corona <- as.character(rep(NA, dim(d)[1]))
for(i in 1:dim(sac_col_to_extract)[1]){
  will_you_stand_against_corona[sac_col_to_extract[i,1]] <- sac_items[sac_col_to_extract[i,1], sac_col_to_extract[i,2]]
}
d$will_you_stand_against_corona <- factor(will_you_stand_against_corona,
                                             levels = c("Yes", "No",
                                                        "I have already signed an online pledge",
                                                        "-99"))
d <- d[, -sac_cols]

d <- dplyr::select(d, -contains("_pledge")) %>%
  dplyr::select(-contains("all_commits")) %>%
  dplyr::select(-source, -sc0, -demo_skip)

ordered_factors <- grepl("ordered",sapply(d, class))
#preserve pred_ variables as factors
ordered_factors <- xor(ordered_factors, grepl("pred_", names(d)))
#preserve education variable as factor
ordered_factors <- ordered_factors & !(names(d)=="education")

d[, ordered_factors] <- lapply(d[, ordered_factors], sjlabelled::as_numeric)

d$agegroup <- NA
d$agegroup <- factor(
  d$agegroup,
  levels = c(1,2,3,4,5,6,7),
  labels = c("18-29",
             "30-39",
             "40-49",
             "50-59",
             "60-69",
             "70-79",
             "80 and over")
)
d$agegroup[which(d$age >= 18 & d$age <= 29)] <- "18-29"
d$agegroup[which(d$age >= 30 & d$age <= 39)] <- "30-39"
d$agegroup[which(d$age >= 40 & d$age <= 49)] <- "40-49"
d$agegroup[which(d$age >= 50 & d$age <= 59)] <- "50-59"
d$agegroup[which(d$age >= 60 & d$age <= 69)] <- "60-69"
d$agegroup[which(d$age >= 70 & d$age <= 79)] <- "70-79"
d$agegroup[which(d$age >= 80)] <- "80 and over"


names(d)[grep("w4_beh_", names(d))] <- gsub("_1", "_i_did",names(d)[grep("w4_beh_", names(d))])
names(d)[grep("w4_beh_", names(d))] <- gsub("_2", "_i_will",names(d)[grep("w4_beh_", names(d))])

logical_variables <- grepl("logical",sapply(d, class))
d[, logical_variables] <- lapply(d[, logical_variables], sjlabelled::as_numeric)
## remove incorrect attention check people
#table(d$attn_check_3)
d <- dplyr::filter(d, is.na(attn_check_1) | attn_check_1=="Never")
d <- dplyr::filter(d, is.na(attn_check_3) | attn_check_3=="2")
d <- dplyr::filter(d, is.na(attn_check_3_1) | attn_check_3_1=="3")


##Re-apply names to variables
attributes_saved <- names(d)%in%names(saved_attributes) #find variables with saved attributes
null_attributes <- get_label(d)=="" | is.null(get_label(d))#find null attributes
get_these_attributes <- which(attributes_saved & null_attributes)
for(i in get_these_attributes){
  #i <- get_these_attributes[1]
  set_label(d[,i]) <- get_label(saved_attributes[,names(d)[i]])
  #get_label(d[,i])
}
#Manually reset a few troublesome labels
set_label(d$swb4) <- " Overall, how anxious did you feel yesterday?"
set_label(d$othb_treat_alt) <- "Used natural or alternative medicines to prevent or treat COVID-19"
set_label(d$othb_treat_conv) <- "Used prescribed medicines to prevent or treat COVID-19"
set_label(d$beh_distance) <- "Keep physical distance from people in public, school, or workplace"

w4_ints <- grepl("w4_txtffd_", names(d))
d[, w4_ints] <- lapply(d[, w4_ints],make_miss_false)

#Checking what's still a text variable:
#names(d)[sapply(d, is.character)]

#table(d$agegroup)
#round(prop.table(table(d$beh_stayhome))*100)
#round(prop.table(table(d$beh_touch))*100)

library(osfr)
osf_auth()
scrub <- osf_retrieve_node("q7gck")
# file_name <- paste(c("~/Google Drive/Research/READI/READI COVID19/Coronavirus living survey/05. Data and Analysis/READI COVID Cleaned Data and Survey/",
#                      gsub("-","_",as.character(Sys.Date())),"_readi_covid_cleaned_sensitive.RDS"), collapse = "") #dynamic updating with dates
file_name_sensitive <- "latest_readi_covid_cleaned_sensitive.RDS" #using version control
file_path_sensitive <- "~/Google Drive/Research/READI/READI COVID19/Coronavirus living survey/05. Data and Analysis/READI COVID Cleaned Data and Survey/"
saveRDS(d, paste(c(file_path_sensitive,file_name_sensitive),collapse=""))
saveRDS(d, paste(c("~/Google Drive/Research/READI/READI COVID19/Coronavirus living survey/05. Data and Analysis/READI COVID Cleaned Data and Survey/jc/test_shiny/",file_name_sensitive),collapse=""))
scrub %>% osf_upload(paste(c(file_path_sensitive,file_name_sensitive),collapse=""), conflicts = "overwrite", verbose = T)

library(psych)
#file_name <- paste(c(gsub("-","_",as.character(Sys.Date())),"_descriptives_of_each_variable.csv"), collapse = "") #dynamic updating with dates
file_name_descriptives <- "latest_descriptives_of_each_variable.csv"
write.csv(psych::describe(d), file_name_descriptives)
public_dat <- osf_retrieve_node("u5x3r") #%>% osf_ls_files(pattern = "Data")
public_dat %>% osf_upload(file_name_descriptives, conflicts = "overwrite", verbose = T)

#Create data dictionary
d_dict <- as.data.frame(names(d))
d_dict$upper <- toupper(names(d))
d_dict$field_label <- gsub("^ ","",as.matrix(lapply(d, function(var){unlist(as.character(attr(var,'label')[1]))})))

d_dict_choice_labels <- function(var){
  unlist(as.character(paste(paste(as.character(attr(var, 'labels')), names(attr(var, 'labels')), sep = ", "), collapse = " | ")))
}
d_dict$choices <- as.character(unlist(lapply(d, d_dict_choice_labels)))
#file_name <- paste(c(gsub("-","_",as.character(Sys.Date())),"_data_dictionary_draft_raw_data.csv"), collapse = "") #dynamic updating with date in filename
file_name_d_dict <- "latest_data_dictionary_draft_raw_data.csv"
write.csv(d_dict, file_name_d_dict)
public_dat %>% osf_upload(file_name_d_dict, conflicts = "overwrite", verbose = T)

#save distribution of countries
country_completion <- table(d$country, d$wave)
country_completion <- country_completion[order(rowSums(country_completion), decreasing = T), ]
#file_name <- paste(c(gsub("-","_",as.character(Sys.Date())),"_country_completion_rates.xlsx"), collapse = "") #dynamic updating with date in filename
file_name_completion <- "latest_country_completion_rates.xlsx"
library(writexl)
writexl::write_xlsx(as.data.frame.matrix(country_completion), file_name_completion)
public_dat %>% osf_upload(file_name_completion, conflicts = "overwrite", verbose = T)

#exporting data for various teams
open_science <- d[, !sapply(d, is.character)] %>% dplyr::select(-area_code)
#file_name <- paste(c(gsub("-","_",as.character(Sys.Date())),"_open_science_data.csv"), collapse = "") #dynamic updating with date in filename
file_name_open_csv <- "latest_open_science_data.csv"
write.csv(open_science, file_name_open_csv)
open_science_path <- osf_retrieve_node("zt3f7") #%>% osf_ls_files(pattern = "Data")

open_science_path %>% osf_upload(file_name_open_csv, conflicts = "overwrite", verbose = T)
#file_name <- paste(c(gsub("-","_",as.character(Sys.Date())),"_open_science_data.RDS"), collapse = "") #dynamic updating with date in filename
file_name_open_rds <- "latest_open_science_data.RDS"
saveRDS(open_science, file_name_open_rds)
open_science_path %>% osf_upload(file_name_open_rds, conflicts = "overwrite", verbose = T)
write.csv(psych::describe(open_science),"checking_open_science.csv")