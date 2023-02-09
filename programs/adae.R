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
  ) %>%
  derive_vars_dt(
    new_vars_prefix = "AST",
    dtc = AESTDTC,
    highest_imputation = "D",
    min_dates = vars(TRTSDT)
  ) %>%
  rowwise() %>%
  mutate(
    ADURN = as.numeric(AENDT - ASTDT + 1)
  ) %>%
  ungroup() %>%
  mutate(ADURU = if_else(!is.na(ADURN),"DAY", NA_character_)) %>%
  derive_vars_dy(
    reference_date = TRTSDT,
    source_vars = vars(TRTSDT, AENDT, ASTDT)
  ) %>%
  mutate(TRTEMFL2 = case_when(is.na(ASTDT) ~ "N",
                              ASTDT >= TRTSDT ~"Y",
                              TRUE ~ "N")
  ) %>%
  #derive_var_trtemfl(new_var = TRTEMFL,start_date = ASTDT, end_date = AENDT, trt_start_date = TRTSDT)
  mutate(
    CQ01NAM =
      if_else(
        (str_detect(AEDECOD,paste0(c("APPLICATION","DERMATITIS", "ERYTHEMA", "BLISTER"),collapse = "|")) |
          (AEBODSYS=="SKIN AND SUBCUTANEOUS TISSUE DISORDERS")) &
          !AEDECOD %in% c('COLD SWEAT', 'HYPERHIDROSIS', 'ALOPECIA'),
        'DERMATOLOGIC EVENTS',NA_character_
  ))



