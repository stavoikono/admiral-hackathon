# Loading the libraries ----
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("haven","admiral","dplyr","tidyr","metacore","metatools","xportr","stringr","readxl")

ipak(packages)

# Loading ADSL & LB ----

adsl <- read_xpt("adam/adsl.xpt") %>% convert_blanks_to_na()
lb <- read_xpt("sdtm/lb.xpt") %>% convert_blanks_to_na()

adsl_vars <- vars(AGE,AGEGR1, AGEGR1N,COMP24FL,DSRAEFL,RACE,SAFFL,SEX,STUDYID,
                  USUBJID, SUBJID,TRTSDT, TRTEDT, TRT01A, TRT01P, TRT01AN, TRT01PN)

# Deriving ADLBC ----

adlbc <- lb %>%
  filter(LBCAT == "CHEMISTRY") %>%
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
    A1HI = LBSTNRHI,
    A1LO = LBSTNRLO,
    ADY = LBDY,
    AVAL = LBSTRESN
  ) %>%
  mutate(
    RACEN = case_when(RACE=="AMERICAN INDIAN OR ALASKA NATIVE" ~ 6,
                      RACE=="BLACK OR AFRICAN AMERICAN" ~ 2,
                      RACE=="WHITE" ~ 1,
                      TRUE ~ NA_real_)
  ) %>%
  derive_vars_dt(
    new_vars_prefix = "A",
    dtc = LBDTC
  ) %>%
  mutate(
    ABLFL = if_else(VISITNUM==1,"Y",NA_character_),
    PARCAT1 = "CHEM",
    R2A1HI = AVAL/A1HI,
    R2A1LO = AVAL/A1LO
  ) %>%
  filter(!VISIT %in% c("AMBUL ECG REMOVAL","RETRIEVAL")) %>%
  mutate(
    AVISIT = case_when(
      str_detect(VISIT, "SCREEN") ~ "Baseline",
      str_detect(VISIT, "SCREEN|UNSCHED|RETRIEVAL|AMBUL") ~ NA_character_,
      !is.na(VISIT) ~ str_to_title(VISIT),
      TRUE ~ NA_character_
    ),
    AVISITN = as.numeric(case_when(
      VISIT == "SCREENING 1" ~ "0",
      str_detect(VISIT, "WEEK") ~ str_trim(str_replace(VISIT, "WEEK", ""))
    ))
  ) %>%
  mutate(
    PARAMCD = LBTESTCD,
    PARAM = paste0(LBTEST," (",LBSTRESU,")"),
    PARAMN =  case_when(PARAMCD=="SODIUM" ~ 18,
                        PARAMCD=="K" ~ 19,
                        PARAMCD=="CL" ~ 20,
                        PARAMCD=="BILI" ~ 21,
                        PARAMCD=="ALP" ~ 22,
                        PARAMCD=="GGT" ~ 23,
                        PARAMCD=="ALT" ~ 24,
                        PARAMCD=="AST" ~ 25,
                        PARAMCD=="BUN" ~ 26,
                        PARAMCD=="CREAT" ~ 27,
                        PARAMCD=="URATE" ~ 28,
                        PARAMCD=="PHOS" ~ 29,
                        PARAMCD=="CA" ~ 30,
                        PARAMCD=="GLUC" ~ 31,
                        PARAMCD=="PROT" ~ 32,
                        PARAMCD=="ALB" ~ 33,
                        PARAMCD=="CHOL" ~ 34,
                        PARAMCD=="CK" ~ 35)
  ) %>%
  derive_extreme_records(
    by_vars = vars(STUDYID, USUBJID, PARAMCD),
    order = vars(AVISITN),
    mode = "last",
    filter = !is.na(AVISIT) & AVISITN>=2 & AVISITN<=24,
    set_values_to = vars(
      AVISIT = "End of Treatment",
      AVISITN = 99
    )
  ) %>%
  rowwise() %>%
  mutate(
    ANRIND = case_when(AVAL > 1.5 * A1HI ~ "H",
                       AVAL < 0.5 * A1LO ~ "L",
                       AVAL < 1.5 * A1HI & AVAL > 0.5 * A1LO ~ "N",
                       is.na(AVAL) ~ "N",
                       TRUE ~ "N")
  ) %>%
  ungroup()


## Baseline measurements ----
base_meas <- adlbc %>%
  group_by(USUBJID,PARAMCD) %>%
  summarise(
    BR2A1HI = R2A1HI[VISITNUM==1],
    BR2A1LO = R2A1LO[VISITNUM==1],
    BASE = AVAL[VISITNUM==1],
    BNRIND = ANRIND[VISITNUM==1]) %>%
  ungroup() %>%
  distinct()

adlbc <- adlbc %>%
  left_join(base_meas) %>%
  rowwise() %>%
  mutate(
    ALBTRVAL = max((LBSTRESN-0.5*LBSTNRLO),(1.5*LBSTNRHI-LBSTRESN),na.rm = T)) %>%
  ungroup() %>%
  mutate(
    CHG = case_when(AVISIT=="Baseline"~ NA_real_,
                    TRUE ~ AVAL - BASE))

## AENTMTFL flag ----
aentmtfl <- adlbc %>%
  filter(VISITNUM>1 & VISITNUM<=12 & !is.na(AVISITN)) %>%
  group_by(USUBJID, PARAMCD) %>%
  summarise(
    VISITNUM = max(VISITNUM, na.rm = T)
  ) %>%
  ungroup() %>%
  mutate(
    AENTMTFL = case_when(VISITNUM<=12 ~ "Y",
                         TRUE ~ NA_character_)
  )

adlbc <- adlbc %>%
  left_join(aentmtfl)

## ANL01FL flag ----
anl01fl <- adlbc %>%
  filter(VISITNUM>1 & VISITNUM <=12 & !is.na(AVISITN)) %>%
  group_by(USUBJID, PARAMCD) %>%
  mutate(
    ALBTRVALMAX = max(ALBTRVAL, na.rm = T)
  ) %>%
  ungroup() %>%
  select(USUBJID,PARAMCD,LBSEQ,ALBTRVAL,ALBTRVALMAX) %>%
  filter(is.finite(ALBTRVALMAX)) %>%
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = vars(USUBJID,PARAMCD),
      order = vars(LBSEQ),
      new_var = ANL01FL,
      mode = "first"
    ),
    filter = ALBTRVAL == ALBTRVALMAX
  ) %>%
  filter(!is.na(ANL01FL)) %>%
  distinct(USUBJID,PARAMCD,LBSEQ,ANL01FL)

adlbc <- adlbc %>%
  left_join(anl01fl)



# Formatting ADLBC for extraction ----

adlbc_spec <- readxl::read_xlsx("metadata/specs.xlsx", sheet = "Variables")  %>%
  filter(Dataset=="ADLBC") %>%
  dplyr::rename(type = "Data Type") %>%
  rlang::set_names(tolower) %>%
  mutate(format = str_to_lower(format))

adlbc <- adlbc %>%
  select(adlbc_spec$variable) %>%
  xportr_label(adlbc_spec, domain = "ADLBC") %>%
  xportr_format(adlbc_spec, domain = "ADLBC") %>%
  xportr_length(adlbc_spec, domain = "ADLBC") %>%
  xportr_write(path = "adam/ADLBC.xpt",
               label = "Analysis Dataset Lab Blood Chemistry")


