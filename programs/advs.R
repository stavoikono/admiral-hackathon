# Loading the libraries ----
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("haven","admiral","dplyr","tidyr","metacore","metatools","xportr","stringr","readxl")

ipak(packages)

# Loading ADSL & VS ----

adsl <- read_xpt("adam/adsl.xpt") %>% convert_blanks_to_na()
vs <- read_xpt("sdtm/vs.xpt") %>% convert_blanks_to_na()

# Deriving ADVS ----

adsl_vars <- vars(STUDYID, USUBJID, SITEID, AGE,AGEGR1, AGEGR1N,RACE, RACEN,
                  SAFFL, SEX,TRTSDT, TRTEDT, TRT01A, TRT01P, TRT01AN, TRT01PN)

advs <- vs %>%
  derive_vars_merged(
    dataset_add = adsl,
    new_vars = adsl_vars,
    by_vars = vars(STUDYID, USUBJID)
  ) %>%
  mutate(
    TRTPN = TRT01PN,
    TRTP = TRT01P,
    TRTAN = TRT01AN,
    TRTA = TRT01A,
    PARAMCD = VSTESTCD,
    PARAM = VSTEST,
    BASETYPE = VSTPT,
    AVAL = VSSTRESN,
    ATPT = VSTPT,
    ADY = VSDY,
    ABLFL = VSBLFL
  ) %>%
  derive_vars_dt(
    new_vars_prefix = "A",
    dtc = VSDTC
  )


advs <- advs %>%
  mutate(
    ATPTN = case_when(ATPT == "AFTER LYING DOWN FOR 5 MINUTES" ~ 5,
                      ATPT == "AFTER STANDING FOR 1 MINUTE" ~ 1,
                      ATPT == "AFTER STANDING FOR 3 MINUTES" ~ 3,
                      TRUE ~ NA_real_)
  ) %>%
  derive_vars_merged(
    vs,
    by_vars = vars(USUBJID,VSSEQ),
    new_vars = vars(BASE = VSSTRESN),
    filter_add = VSBLFL=="Y"
  ) %>%
  # derive_var_base(
  #   by_vars = vars(STUDYID, USUBJID, PARAMCD, BASETYPE),
  #   source_var = AVAL,
  #   new_var = BASE,
  #   filter = VSBLFL=="Y"
  # ) %>%
  rowwise() %>%
  mutate(
    CHG = AVAL - BASE,
    PCHG = 100*(CHG/BASE)
  ) %>%
  ungroup() %>%
  mutate(
    PARAMN = case_when(PARAMCD == "SYSBP" ~ 1,
                       PARAMCD == "DIABP" ~ 2,
                       PARAMCD == "PULSE" ~ 3,
                       PARAMCD == "WEIGHT" ~ 4,
                       PARAMCD == "HEIGHT" ~ 5,
                       PARAMCD == "TEMP" ~ 6,
                       TRUE ~ NA_real_)
  ) %>%
  mutate(
    AVISIT = case_when(
      str_detect(VISIT, "SCREEN") ~ NA_character_,
      str_detect(VISIT, "UNSCHED") ~ NA_character_,
      str_detect(VISIT, "RETRIEVAL") ~ NA_character_,
      str_detect(VISIT, "AMBUL") ~ NA_character_,
      !is.na(VISIT) ~ str_to_title(VISIT)
    ),
    AVISITN = as.numeric(case_when(
      VISIT == "BASELINE" ~ "0",
      str_detect(VISIT, "WEEK") ~ str_trim(str_replace(VISIT, "WEEK", ""))
    )),
    ANL01FL = case_when(
      str_detect(VISIT, "SCREEN") ~ NA_character_,
      str_detect(VISIT, "UNSCHED") ~ NA_character_,
      str_detect(VISIT, "RETRIEVAL") ~ NA_character_,
      str_detect(VISIT, "AMBUL") ~ NA_character_,
      !is.na(VISIT) ~ "Y"
    )
  )


# Formatting ADVS for extraction ----

advs_spec <- readxl::read_xlsx("metadata/specs.xlsx", sheet = "Variables")  %>%
  filter(Dataset=="ADVS") %>%
  dplyr::rename(type = "Data Type") %>%
  rlang::set_names(tolower) %>%
  mutate(format = str_to_lower(format))

advs <- advs %>%
  select(advs_spec$variable) %>%
  xportr_label(advs_spec, domain = "ADVS") %>%
  xportr_format(advs_spec, domain = "ADVS") %>%
  xportr_length(advs_spec, domain = "ADVS") %>%
  xportr_write(path = "adam/ADVS.xpt",
               label = "Vital Signs Analysis Dataset")
