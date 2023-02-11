# Loading the libraries ----
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("haven","admiral","dplyr","tidyr","metacore","metatools","xportr","stringr","readxl","labelled")

ipak(packages)

# Loading ADSL & VS ----

adsl <- read_xpt("adam/adsl.xpt") %>% convert_blanks_to_na()

# Deriving ADVS ----

adtte <- adsl %>%
  select(AGE, AGEGR1, AGEGR1N, RACE, RACEN, SAFFL, SEX,
         SITEID, STUDYID, USUBJID, TRT01A, TRT01AN, TRT01P,
         TRTDURD, TRTEDT, TRTSDT, RFSTDTC)





