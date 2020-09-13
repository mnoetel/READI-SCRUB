library(tidyverse)

d <- read_rds("G:/Shared drives/BWA-SCRUB (EXT)/05. Data and Analysis/Cleaning code/SCRUB ORU W34567 - 2020-08-19.RDS")
d8_raw <- read_rds("G:/Shared drives/BWA-SCRUB (EXT)/05. Data and Analysis/Wave 8 - DPC/Cleaned raw data/SCRUB ORU W8 - 2020-09-04.RDS")

d_67 <- d %>% select(startdate, 
               matches("^beh"),
               matches("^pred_num"),
               matches("^swb"),
               matches("^worry"),
               -matches("worry_other"),
               gender, 
               age,
               agegroup,
               wave,
               origin,
               ausonly_state,
               country) %>%
  filter(wave == 6 | wave == 7) %>%
  copy_labels(d)
         

d8 <- d8_raw %>% select(startdate, 
                     matches("^beh"),
                     matches("^pred_num"),
                     matches("^swb"),
                     matches("^worry"),
                     -matches("worry_other"),
                     gender, 
                     age,
                     agegroup,
                     wave,
                     origin,
                     ausonly_state,
                     country) %>%
  copy_labels(d8_raw)

d <- bind_rows(d_67, d8)

write_rds(d, "SCRUB ORU W678 dashboard vars - 2020-09-07.rds")
