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
lb <- read_xpt("sdtm/lb.xpt") %>% convert_blanks_to_na()


adsl_vars <- vars(SITEID, STUDYID, USUBJID, TRT01A,
                  TRT01AN, TRT01P, TRT01PN, TRTSDT, TRTEDT,
                  RACE, RACEN, SAFFL, SEX, AGE,AGEGR1, AGEGR1N,
                  COMP24FL, DSRAEFL)

# Deriving ADLBH ----

adlbh <- lb %>%
  derive_vars_merged(
    dataset_add = adsl,
    new_vars = adsl_vars,
    by_vars = vars(STUDYID, USUBJID)
  ) %>%
  mutate(
    TRTAN = TRT01AN,
    TRTA = TRT01A,
    TRTPN = TRT01PN,
    TRTP = TRT01P
  ) %>%
  derive_vars_dt(
    new_vars_prefix = "AEN",
    dtc = AEENDTC
  )
