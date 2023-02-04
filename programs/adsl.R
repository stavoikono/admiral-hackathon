# Loading the libraries ----
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("haven","admiral","dplyr","tidyr","metacore","metatools","xportr","stringr","readxl")

ipak(packages)


# Loading SDTM ----
dm <- read_xpt("sdtm/dm.xpt") %>% convert_blanks_to_na()
ex <- read_xpt("sdtm/ex.xpt") %>% convert_blanks_to_na()
vs <- read_xpt("sdtm/vs.xpt") %>% convert_blanks_to_na()
ds <- read_xpt("sdtm/ds.xpt") %>% convert_blanks_to_na()
sv <- read_xpt("sdtm/sv.xpt") %>% convert_blanks_to_na()
qs <- read_xpt("sdtm/qs.xpt") %>% convert_blanks_to_na()
mh <- read_xpt("sdtm/mh.xpt") %>% convert_blanks_to_na()
sc <- read_xpt("sdtm/sc.xpt") %>% convert_blanks_to_na()


# Custom functions ----

trtn <- function(x){
  case_when(x=="Placebo" ~ 0,
            x=="Xanomeline Low Dose" ~ 1,
            x=="Xanomeline High Dose" ~ 2)
}

comp_func <- function(x){
  ifelse(!is.na(x),"Y","N")
}

format_eosstt <- function(DSDECOD) {
  ifelse(DSDECOD=="COMPLETED","COMPLETED","DISCONTINUED")
}

# Deriving ADSL ----

adsl <- dm %>%
  filter(ARM!="Screen Failure") %>%
  select(-DOMAIN) %>%
  mutate(TRT01P = ARM,
         TRT01A = TRT01P,
         SITEGR1 = SITEID,
         AGEGR1 = case_when(AGE < 65 ~ "<65",
                            AGE >=65 & AGE<=80 ~ "65-80",
                            TRUE ~ ">80",),
         AGEGR1N = case_when(AGE < 65 ~ 1,
                            AGE >=65 & AGE<=80 ~ 2,
                            TRUE ~ 3),
         RACEN = case_when(RACE=="AMERICAN INDIAN OR ALASKA NATIVE" ~ 1,
                           RACE=="ASIAN" ~ 2,
                           RACE=="BLACK OR AFRICAN AMERICAN" ~ 3,
                           RACE=="WHITE" ~ 6),
         TRT01AN = trtn(TRT01A),
         TRT01PN = trtn(TRT01P)
  ) %>%
  derive_vars_merged(
    dataset_add = vs,
    by_vars = vars(STUDYID, USUBJID),
    new_vars = vars(HEIGHTBL = VSSTRESN),
    filter_add = VSTESTCD=="HEIGHT" & VISITNUM==1
  ) %>%
  derive_vars_merged(
    dataset_add = vs,
    by_vars = vars(STUDYID, USUBJID),
    new_vars = vars(WEIGHTBL = VSSTRESN),
    filter_add = VSTESTCD=="WEIGHT" & VISITNUM==3
  ) %>%
  mutate(BMIBL = compute_bmi(HEIGHTBL,WEIGHTBL),
         BMIBLGR1 = case_when(BMIBL < 25 ~ "<25",
                              BMIBL >=25 & BMIBL < 30 ~ "25-<30",
                              BMIBL >=30 ~ ">=30")
  ) %>%
  derive_vars_dt(
    dtc = RFENDTC,
    new_vars_prefix = "RFEN"
  )



adsl <- adsl %>%
  derive_var_merged_summary(
    dataset_add = qs,
    by_vars = vars(USUBJID),
    new_var = MMSETOT,
    analysis_var = QSORRES,
    summary_fun = function(x) sum(as.numeric(x), na.rm = TRUE),
    filter_add = QSCAT == "MINI-MENTAL STATE"
  ) %>%
  derive_vars_merged(
    dataset_add = sc,
    by_vars = vars(STUDYID, USUBJID),
    new_vars = vars(EDUCLVL = SCSTRESN),
    filter_add = SCTESTCD=="EDLEVEL"
  )


## Deriving completion flags ----

vs_comp <- vs %>%
  left_join(adsl %>% select(USUBJID,RFENDT)) %>%
  mutate(COMP8FL = if_else(VISITNUM==8 & VSDTC<=RFENDT,"Y","N"),
         COMP16FL = if_else(VISITNUM==10 & VSDTC<=RFENDT,"Y","N"),
         COMP24FL = if_else(VISITNUM==12 & VSDTC<=RFENDT,"Y","N")
         ) %>%
  distinct(USUBJID,COMP8FL,COMP16FL,COMP24FL,VISITNUM) %>%
  filter(VISITNUM %in% c(8,10,12))


adsl <- adsl %>%
  left_join(vs_comp %>%
              filter(VISITNUM==8) %>%
              select(USUBJID,COMP8FL)
            ) %>%
  left_join(vs_comp %>%
              filter(VISITNUM==10) %>%
              select(USUBJID,COMP16FL)
            ) %>%
  left_join(vs_comp %>%
              filter(VISITNUM==12) %>%
              select(USUBJID,COMP24FL)
            ) %>%
  mutate(COMP8FL = if_else(is.na(COMP8FL),"N",COMP8FL),
         COMP16FL = if_else(is.na(COMP16FL),"N",COMP16FL),
         COMP24FL = if_else(is.na(COMP24FL),"N",COMP24FL))


rm(vs_comp)






adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = sv,
    by_vars = vars(STUDYID, USUBJID),
    new_vars = vars(VISIT1DT = SVSTDTC),
    filter_add = VISITNUM==1
  ) %>%
  derive_vars_disposition_reason(
    dataset_ds = ds,
    new_var = DCSREAS,
    reason_var = DSDECOD,
    filter_ds = DSCAT == "DISPOSITION EVENT"
  ) %>%
  derive_var_disposition_status(
    dataset_ds = ds,
    new_var = EOSSTT,
    status_var = DSDECOD,
    format_new_var = format_eosstt,
    filter_ds = DSCAT == "DISPOSITION EVENT"
  ) %>%
  derive_vars_merged(
    dataset_add = ds,
    by_vars = vars(STUDYID, USUBJID),
    new_vars = vars(DSDECOD),
    filter_add = DSCAT == "DISPOSITION EVENT"
  ) %>%
  mutate(DISCONFL = ifelse(DCSREAS!="COMPLETED","Y",NA_character_),
         DSRAEFL = ifelse(DCSREAS!="ADVERSE EVENT","Y",NA_character_)) %>%
  derive_vars_merged(
    dataset_add = mh,
    by_vars = vars(USUBJID),
    new_vars = vars(DISONSDT = MHSTDTC),
    filter_add = MHCAT == "PRIMARY DIAGNOSIS"
  ) %>%
  mutate(VISIT1DT = as.Date(VISIT1DT),
         DISONSDT = as.Date(DISONSDT)) %>%
  derive_vars_duration(
    new_var = DURDIS,
    out_unit = "months",
    start_date = VISIT1DT,
    end_date = DISONSDT
  ) %>%
  derive_vars_merged(
    dataset_add = ds,
    by_vars = vars(USUBJID),
    new_vars = vars(VISNUMEN=VISITNUM),
    filter_add = DSTERM == "PROTOCOL COMPLETED"
  )

## Deriving treatment start date, end date and duration ----

ex_ext <- ex %>%
  select(USUBJID,VISITNUM, EXENDTC) %>%
  mutate(EXENDTC = as.Date(EXENDTC)) %>%
  group_by(USUBJID) %>%
  slice_max(VISITNUM) %>%
  ungroup() %>%
  left_join( adsl %>% select(USUBJID, RFENDT)) %>%
  rowwise() %>%
  mutate(EXENDTC = if_else(VISITNUM >=3 & is.na(EXENDTC),RFENDT,EXENDTC)) %>%
  ungroup() %>%
  select(USUBJID, TRTEDT = EXENDTC)


adsl <- adsl %>%
  left_join(ex_ext) %>%
  derive_vars_merged(
    dataset_add = sv,
    by_vars = vars(STUDYID, USUBJID),
    new_vars = vars(TRTSDT = SVSTDTC),
    filter_add = VISITNUM==3
  ) %>%
  mutate(TRTSDT = as.Date(TRTSDT)) %>%
  derive_var_trtdurd()

rm(ex_ext)

## Deriving ITTFL, SAFFL and EFFFL ----

efffl <- qs %>%
  filter(VISITNUM>3) %>%
  group_by(USUBJID) %>%
  summarise(adas = sum(QSCAT=="ALZHEIMER'S DISEASE ASSESSMENT SCALE"),
            cibic = sum(QSCAT=="CLINICIAN'S INTERVIEW-BASED IMPRESSION OF CHANGE (CIBIC+)")) %>%
  ungroup() %>%
  filter(adas>0 & cibic>0) %>%
  mutate(efffl_fl = "Y") %>%
  select(USUBJID, efffl_fl)


adsl <- adsl %>%
  mutate(ITTFL = ifelse(!is.na(ARMCD),"Y","N"),
         SAFFL = ifelse(ITTFL == "Y" & !is.na(TRTSDT),"Y","N")
  ) %>%
  left_join(efffl) %>%
  mutate(efffl_fl = if_else(is.na(efffl_fl),"N","Y")) %>%
  mutate(EFFFL = if_else(SAFFL=="Y" & efffl_fl=="Y","Y","N")) %>%
  select(-efffl_fl)

rm(efffl)


# Export ADSL ----

var_order <- read_excel("metadata/specs.xlsx",
                    sheet = "Variables") %>%
  filter(Dataset=="ADSL") %>%
  pull(Variable)

adsl <- adsl %>%
  select(all_of(var_order))

xportr_write(adsl, "adam/adsl.xpt")
