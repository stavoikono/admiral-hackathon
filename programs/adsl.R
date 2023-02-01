# Loading the libraries ----
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("haven","admiral","dplyr","tidyr","metacore","metatools","xportr","stringr")

ipak(packages)


# Loading SDTM ----
dm <- read_xpt("sdtm/dm.xpt") %>% convert_blanks_to_na()
ex <- read_xpt("sdtm/ex.xpt") %>% convert_blanks_to_na()
vs <- read_xpt("sdtm/vs.xpt") %>% convert_blanks_to_na()
ds <- read_xpt("sdtm/ds.xpt") %>% convert_blanks_to_na()
sv <- read_xpt("sdtm/sv.xpt") %>% convert_blanks_to_na()
qs <- read_xpt("sdtm/qs.xpt") %>% convert_blanks_to_na()
mh <- read_xpt("sdtm/mh.xpt") %>% convert_blanks_to_na()

#xportr_write(adsl, "adam/adsl.xpt")

# Deriving ADSL ----

trtn <- function(x){
  case_when(x=="Placebo" ~ 1,
            x=="Xanomeline Low Dose" ~ 2,
            x=="Xanomeline High Dose" ~ 3,
            TRUE ~ as.numeric(NA))
}



adsl <- dm %>%
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
         TRT01PN = trtn(TRT01P)) %>%
  derive_vars_merged(
    dataset_add = sv,
    by_vars = vars(STUDYID, USUBJID),
    new_vars = vars(VISIT1DT = SVSTDTC),
    filter_add = VISITNUM==1
  ) %>%
  derive_vars_merged(
    dataset_add = sv,
    by_vars = vars(STUDYID, USUBJID),
    new_vars = vars(TRTSDT = SVSTDTC),
    filter_add = VISITNUM==3
  ) %>%
  # PLACE HOLDER FOR
  # derive_vars_merged(
  #   dataset_add = sv,
  #   by_vars = vars(STUDYID, USUBJID),
  #   new_vars = vars(TRTSDT = SVSTDTC),
  #   filter_add = VISITNUM==3
  # ) %>%
  mutate(ITTFL = ifelse(!is.na(ARMCD),"Y","N"),
         SAFFL = ifelse(ITTFL == "Y" & !is.na(TRTSDT),"Y","N")
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
  )


adsl <- adsl %>%
  #derive_vars_dtm_to_dt(source_vars = vars(TRTSDTM, TRTEDTM)) %>%
  derive_var_trtdurd()




adsl <- adsl %>%
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
                              BMIBL >=30 ~ ">=30"))




# VS -- NEED TO BE FIXED

vs_8 <- vs %>% filter(VISITNUM==8) %>% distinct(USUBJID, VSDTC) %>% dplyr::rename(VSDTC8 = VSDTC)
vs_10 <- vs %>% filter(VISITNUM==10) %>% distinct(USUBJID, VSDTC) %>% dplyr::rename(VSDTC10 = VSDTC)
vs_12 <- vs %>% filter(VISITNUM==12) %>% distinct(USUBJID, VSDTC) %>% dplyr::rename(VSDTC12 = VSDTC)

comp_func <- function(VSDTC){
  ifelse(!is.na(VSDTC),"Y","N")
}

adsl <- adsl %>%
  derive_var_merged_cat(
    dataset_add = vs,
    by_vars = vars(STUDYID, USUBJID),
    new_var = COMP8FL,
    source_var = VSDTC,
    cat_fun = comp_func,
    filter_add = VISITNUM==8,
    mode = "last"
  ) adsl %>%
  derive_var_merged_exist_flag(
    dataset_add = vs,
    by_vars = vars(STUDYID, USUBJID),
    filter_add = VISITNUM==8,
    new_var = COMP8FL,
    condition = !is.na(VSDTC)
  ) %>%
  # left_join(vs_8) %>%
  # left_join(vs_10) %>%
  # left_join(vs_12) %>%
  mutate(COMP8FL = ifelse(!is.na(VSDTC8) & VSDTC8<= RFENDTC,"Y","N"),
         COMP16FL = ifelse(!is.na(VSDTC10) & VSDTC10<= RFENDTC,"Y","N"),
         COMP24FL = ifelse(!is.na(VSDTC12) & VSDTC12<= RFENDTC,"Y","N")) %>%
  select(-VSDTC8,-VSDTC10,-VSDTC12)




format_eosstt <- function(DSDECOD) {
  ifelse(DSDECOD=="COMPLETED","COMPLETED","DISCONTINUED")
}

adsl2 <- adsl %>%
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
  )





