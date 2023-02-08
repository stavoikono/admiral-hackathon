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
ae <- read_xpt("sdtm/ae.xpt") %>% convert_blanks_to_na()

# Deriving ADVS ----

adsl_vars <- vars(RACE, RACEN, SAFFL, SEX, SITEID, STUDYID, USUBJID, TRT01A,
                  TRT01AN, TRTSDT, TRTEDT,  AGE,AGEGR1, AGEGR1N)



adae <- ae %>%
  derive_vars_merged(
    dataset_add = adsl,
    new_vars = adsl_vars,
    by_vars = vars(STUDYID, USUBJID)
  ) %>%
  mutate(
    TRTAN = TRT01AN,
    TRTA = TRT01A
  ) %>%
  derive_vars_dt(
    new_vars_prefix = "AEN",
    dtc = AEENDTC
  )
