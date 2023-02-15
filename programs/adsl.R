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
            x=="Xanomeline Low Dose" ~ 54,
            x=="Xanomeline High Dose" ~ 81)
}

comp_func <- function(x){
  if_else(!is.na(x),"Y","N")
}

format_eosstt <- function(DSDECOD) {
  if_else(DSDECOD=="COMPLETED","COMPLETED","DISCONTINUED")
}

# Deriving ADSL ----

pooled_sites_id <- dm %>%
  filter(ARM!="Screen Failure") %>%
  count(SITEID, ARM,  sort = T) %>%
  filter(n<3) %>%
  pull(SITEID) %>%
  unique()

adsl <- dm %>%
  filter(ARM!="Screen Failure") %>%
  select(-DOMAIN) %>%
  mutate(
    TRT01P = ARM,
    TRT01A = TRT01P,
    SITEGR1 = if_else(SITEID %in% pooled_sites_id,"900",SITEID),
    AGEGR1 = case_when(AGE < 65 ~ "<65",
                       AGE >=65 & AGE<=80 ~ "65-80",
                       TRUE ~ ">80"),
    AGEGR1N = case_when(AGE < 65 ~ 1,
                        AGE >=65 & AGE<=80 ~ 2,
                        TRUE ~ 3),
    RACEN = case_when(RACE=="AMERICAN INDIAN OR ALASKA NATIVE" ~ 1,
                      RACE=="BLACK OR AFRICAN AMERICAN" ~ 3,
                      RACE=="WHITE" ~ 6,
                      TRUE ~ NA_real_),
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
  mutate(WEIGHTBL=round(WEIGHTBL,1),
         HEIGHTBL=round(HEIGHTBL,1)) %>%
  mutate(
    BMIBL = compute_bmi(HEIGHTBL,WEIGHTBL),
    BMIBL = round(BMIBL,1),
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

vs_comp <- sv %>%
  left_join(
    dm %>%
      select(USUBJID,RFENDTC)
  ) %>%
  mutate(
    COMP8FL = if_else(VISITNUM==8 & SVSTDTC<=RFENDTC,"Y","N"),
    COMP16FL = if_else(VISITNUM==10 & SVSTDTC<=RFENDTC,"Y","N"),
    COMP24FL = if_else(VISITNUM==12 & SVSTDTC<=RFENDTC,"Y","N")
  ) %>%
  distinct(USUBJID,COMP8FL,COMP16FL,COMP24FL,VISITNUM) %>%
  filter(VISITNUM %in% c(8,10,12))


adsl <- adsl %>%
  left_join(
    vs_comp %>%
      filter(VISITNUM==8) %>%
      select(USUBJID,COMP8FL)
  ) %>%
  left_join(
    vs_comp %>%
      filter(VISITNUM==10) %>%
      select(USUBJID,COMP16FL)
  ) %>%
  left_join(
    vs_comp %>%
      filter(VISITNUM==12) %>%
      select(USUBJID,COMP24FL)
  ) %>%
  mutate(
    COMP8FL = if_else(is.na(COMP8FL),"N",COMP8FL),
    COMP16FL = if_else(is.na(COMP16FL),"N",COMP16FL),
    COMP24FL = if_else(is.na(COMP24FL),"N",COMP24FL)
  )


rm(vs_comp)






adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = sv,
    by_vars = vars(STUDYID, USUBJID),
    new_vars = vars(VISIT1DT = SVSTDTC),
    filter_add = VISITNUM==1
  ) %>%
  mutate(
    VISIT1DT = as.Date(VISIT1DT)
  ) %>%
  derive_vars_disposition_reason(
    dataset_ds = ds,
    new_var = DCSREAS,
    reason_var = DSDECOD,
    filter_ds = DSCAT == "DISPOSITION EVENT"
  ) %>%
  mutate(
    DCSREAS = str_to_title(DCSREAS),
    DCSREAS = case_when(DCSREAS=="Lost To Follow-Up" ~ "Lost to Follow-up",
                        DCSREAS=="Lack Of Efficacy" ~ "Lack of Efficacy",
                        DCSREAS=="Study Terminated By Sponsor" ~ "Sponsor Decision",
                        DCSREAS=="Withdrawal By Subject" ~ "Withdrew Consent",
                        #DCSREAS=="Protocol Violation" ~ "I/E Not Met",
                        #DCSREAS=="I/E Not Met" ~ "Protocol Violation",
                        TRUE ~ DCSREAS)) %>%
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
    new_vars = vars(DCDECOD = DSDECOD ),
    filter_add = DSCAT == "DISPOSITION EVENT"
  ) %>%
  mutate(
    DISCONFL = if_else(DCSREAS!="COMPLETED","Y",NA_character_),
    DSRAEFL = if_else(DCSREAS=="Adverse Event","Y",NA_character_)
  ) %>%
  derive_vars_merged(
    dataset_add = mh,
    by_vars = vars(USUBJID),
    new_vars = vars(DISONSDT = MHSTDTC),
    filter_add = MHCAT == "PRIMARY DIAGNOSIS"
  ) %>%
  mutate(
    DISONSDT = as.Date(DISONSDT)
  ) %>%
  derive_vars_duration(
    new_var = DURDIS,
    out_unit = "months",
    start_date = DISONSDT,
    end_date = VISIT1DT
  ) %>%
  mutate(
    DURDIS =  round(DURDIS,1),
    DURDSGR1 = if_else(DURDIS<12,"<12",">=12")
  ) %>%
  derive_vars_merged(
    dataset_add = ds,
    by_vars = vars(USUBJID),
    new_vars = vars(VISNUMEN=VISITNUM),
    filter_add = DSCAT == "DISPOSITION EVENT"
  ) %>%
  mutate(
    VISNUMEN = if_else(VISNUMEN==13,12,VISNUMEN)
  )

## Deriving treatment start date, end date and duration ----

ex_ext <- ex %>%
  select(USUBJID,VISITNUM, EXENDTC) %>%
  mutate(
    EXENDTC = as.Date(EXENDTC)
  ) %>%
  group_by(USUBJID) %>%
  slice_max(VISITNUM) %>%
  ungroup() %>%
  left_join(
    adsl %>% select(USUBJID, RFENDT)
  ) %>%
  mutate(EXENDTC = if_else(VISITNUM >=3 & is.na(EXENDTC),RFENDT,EXENDTC)) %>%
  select(USUBJID, TRTEDT = EXENDTC)


adsl <- adsl %>%
  left_join(
    ex_ext
  ) %>%
  derive_vars_merged(
    dataset_add = sv,
    by_vars = vars(STUDYID, USUBJID),
    new_vars = vars(TRTSDT = SVSTDTC),
    filter_add = VISITNUM==3
  ) %>%
  mutate(
    TRTSDT = as.Date(TRTSDT)
  ) %>%
  derive_var_trtdurd()

rm(ex_ext)

## Deriving ITTFL, SAFFL and EFFFL ----

efffl <- qs %>%
  filter(VISITNUM>3) %>%
  group_by(USUBJID) %>%
  summarise(
    adas = sum(QSCAT=="ALZHEIMER'S DISEASE ASSESSMENT SCALE"),
    cibic = sum(QSCAT=="CLINICIAN'S INTERVIEW-BASED IMPRESSION OF CHANGE (CIBIC+)")
  ) %>%
  ungroup() %>%
  filter(adas>0 & cibic>0) %>%
  mutate(efffl_fl = "Y") %>%
  select(USUBJID, efffl_fl)


adsl <- adsl %>%
  mutate(
    ITTFL = if_else(!is.na(ARMCD),"Y","N"),
    SAFFL = if_else(ITTFL == "Y" & !is.na(TRTSDT),"Y","N")
  ) %>%
  left_join(
    efffl
  ) %>%
  mutate(
    efffl_fl = if_else(is.na(efffl_fl),"N","Y"),
    EFFFL = if_else(SAFFL=="Y" & efffl_fl=="Y","Y","N")
  ) %>%
  select(-efffl_fl)

rm(efffl)

## Deriving CUMDOSE

df_cumdose <- adsl %>%
  derive_vars_merged(
    dataset_add = sv,
    by_vars = vars(STUDYID, USUBJID),
    new_vars = vars(VISIT4DT = SVSTDTC),
    filter_add = VISITNUM==4
  ) %>%
  derive_vars_merged(
    dataset_add = sv,
    by_vars = vars(STUDYID, USUBJID),
    new_vars = vars(VISIT12DT = SVSTDTC),
    filter_add = VISITNUM==12
  ) %>%
  left_join(
    vs %>%
      filter(!VISITNUM %in% c(101,201)) %>%
      group_by(USUBJID) %>%
      slice_max(VISITNUM) %>%
      ungroup() %>%
      select(USUBJID,VISITNUM,VSDTC) %>%
      distinct()
  ) %>%
  select(USUBJID, TRT01PN, VISITNUM, TRTSDT, VISIT4DT, VISIT12DT, TRTEDT) %>%
  filter(TRT01PN==81) %>%
  mutate(VISIT4DT = as.Date(VISIT4DT),
         VISIT12DT = as.Date(VISIT12DT)) %>%
  mutate(VISIT4DT = if_else(VISITNUM<=4 & VISITNUM>=3,TRTEDT,VISIT4DT),
         VISIT12DT = if_else(VISITNUM<=12 & VISITNUM>4,TRTEDT,VISIT12DT)) %>%
  mutate(INTERVAL1 = if_else(VISITNUM>=3,as.numeric(difftime(VISIT4DT,TRTSDT,units = "days")) + 1,0),
         INTERVAL2 = if_else(VISITNUM>4, as.numeric(difftime(VISIT12DT,VISIT4DT,units = "days")), 0),
         INTERVAL3 = if_else(VISITNUM>12, as.numeric(difftime(TRTEDT, VISIT12DT,units = "days")), 0)) %>%
  mutate(CUMDOSE = 54*INTERVAL1 + 81*INTERVAL2 + 54*INTERVAL3) %>%
  select(USUBJID,TRT01PN,CUMDOSE)

adsl <- adsl %>%
  left_join(df_cumdose) %>%
  rowwise() %>%
  mutate(CUMDOSE = if_else(is.na(CUMDOSE),TRT01PN*TRTDURD,CUMDOSE),
         AVGDD = round(CUMDOSE/TRTDURD,1)) %>%
  ungroup()


rm(df_cumdose)

adsl <- adsl %>%
  mutate(DCSREAS=ifelse(
    USUBJID %in% c("01-703-1175","01-705-1382","01-708-1372"),"I/E Not Met",DCSREAS))

# Formatting ADSL for extraction ----

adsl_spec <- readxl::read_xlsx("metadata/specs.xlsx", sheet = "Variables")  %>%
  filter(Dataset=="ADSL") %>%
  dplyr::rename(type = "Data Type") %>%
  rlang::set_names(tolower) %>%
  mutate(format = str_to_lower(format)) %>%
  mutate(format = if_else(format=="date9.","date",NA_character_))

adsl <- adsl %>%
  select(adsl_spec$variable) %>%
  xportr_label(adsl_spec, domain = "ADSL") %>%
  xportr_format(adsl_spec, domain = "ADSL") %>%
  xportr_length(adsl_spec, domain = "ADSL") %>%
  xportr_write(path = "adam/ADSL.xpt",
               label = "Subject-Level Analysis Dataset")
