# Loading the libraries ----
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("haven","admiral","dplyr","tidyr","metacore","metatools","xportr","stringr","readxl")

ipak(packages)


# Loading ADSL & AE ----

adsl <- read_xpt("adam/adsl.xpt") %>% convert_blanks_to_na()
ae <- read_xpt("sdtm/ae.xpt") %>% convert_blanks_to_na()

# Deriving ADAE ----

adsl_vars <- vars(RACE, SAFFL, SEX, SITEID, STUDYID, USUBJID, TRT01A,
                  TRT01AN, TRTSDT, TRTEDT,  AGE,AGEGR1, AGEGR1N)



adae <- ae %>%
  derive_vars_merged(
    dataset_add = adsl,
    new_vars = adsl_vars,
    by_vars = vars(STUDYID, USUBJID)
  ) %>%
  mutate(
    TRTAN = TRT01AN,
    TRTA = TRT01A,
    RACEN = case_when(RACE=="AMERICAN INDIAN OR ALASKA NATIVE" ~ 6,
                      RACE=="BLACK OR AFRICAN AMERICAN" ~ 2,
                      RACE=="WHITE" ~ 1,
                      TRUE ~ NA_real_)
  ) %>%
  derive_vars_dt(
    new_vars_prefix = "AEN",
    dtc = AEENDTC
  ) %>%
  derive_vars_dt(
    new_vars_prefix = "AST",
    dtc = AESTDTC,
    flag_imputation = "date",
    highest_imputation = "D",
    min_dates = vars(TRTSDT)
  ) %>%
  rowwise() %>%
  mutate(
    ADURN = as.numeric(difftime(AENDT,ASTDT, units = "days")) + 1
  ) %>%
  ungroup() %>%
  mutate(ADURU = if_else(!is.na(ADURN),"DAY", NA_character_)) %>%
  derive_vars_dy(
    reference_date = TRTSDT,
    source_vars = vars(TRTSDT, AENDT, ASTDT)
  ) %>%
  mutate(
    TRTEMFL = case_when(
      ASTDT >= TRTSDT ~"Y",
      TRUE ~ NA_character_)
  ) %>%
  mutate(
    CQ01NAM =
      if_else(
        (str_detect(AEDECOD,paste0(c("APPLICATION","DERMATITIS", "ERYTHEMA", "BLISTER"),collapse = "|")) |
          (AEBODSYS=="SKIN AND SUBCUTANEOUS TISSUE DISORDERS")) &
          !AEDECOD %in% c('COLD SWEAT', 'HYPERHIDROSIS', 'ALOPECIA'),
        'DERMATOLOGIC EVENTS',NA_character_
  ))


## Derive AOCC flag variables ----
adae <- adae %>%
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = vars(USUBJID),
      order = vars(USUBJID, ASTDT, AESEQ),
      new_var = AOCC01FL,
      mode = "first"
    ),
    filter = CQ01NAM=="DERMATOLOGIC EVENTS" & TRTEMFL=="Y"
  ) %>%
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = vars(USUBJID),
      order = vars(USUBJID, ASTDT, AESEQ),
      new_var = AOCC02FL,
      mode = "first"
    ),
    filter = AESER=="Y" & TRTEMFL=="Y"
  ) %>%
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = vars(USUBJID, AEBODSYS),
      order = vars(USUBJID,AEBODSYS, ASTDT, AESEQ),
      new_var = AOCC03FL,
      mode = "first"
    ),
    filter = AESER=="Y" & TRTEMFL=="Y"
  ) %>%
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = vars(USUBJID, AEBODSYS,AEDECOD),
      order = vars(USUBJID,AEBODSYS,AEDECOD, ASTDT, AESEQ),
      new_var = AOCC04FL,
      mode = "first"
    ),
    filter = AESER=="Y" & TRTEMFL=="Y"
  ) %>%
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = vars(USUBJID),
      order = vars(USUBJID,ASTDT, AESEQ),
      new_var = AOCCFL,
      mode = "first"
    ),
    filter = TRTEMFL=="Y"
  ) %>%
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = vars(USUBJID, AEBODSYS,AEDECOD),
      order = vars(USUBJID, AEBODSYS, AEDECOD, ASTDT, AESEQ),
      new_var = AOCCPFL,
      mode = "first"
    ),
    filter = TRTEMFL=="Y"
  ) %>%
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = vars(USUBJID, AEBODSYS),
      order = vars(USUBJID, AEBODSYS, ASTDT, AESEQ),
      new_var = AOCCSFL,
      mode = "first"
    ),
    filter = TRTEMFL=="Y"
  )


# Formatting ADAE for extraction ----

adae_spec <- readxl::read_xlsx("metadata/specs.xlsx", sheet = "Variables")  %>%
  filter(Dataset=="ADAE") %>%
  dplyr::rename(type = "Data Type") %>%
  rlang::set_names(tolower) %>%
  mutate(format = str_to_lower(format))

adae <- adae %>%
  select(adae_spec$variable) %>%
  xportr_label(adae_spec, domain = "ADAE") %>%
  xportr_format(adae_spec, domain = "ADAE") %>%
  xportr_length(adae_spec, domain = "ADAE") %>%
  xportr_write(path = "adam/ADAE.xpt",
               label = "Adverse Events Analysis Dataset")

