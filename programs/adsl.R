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
dm <- read_xpt("sdtm/dm.xpt")
ex <- read_xpt("sdtm/ex.xpt")
vs <- read_xpt("sdtm/vs.xpt")
ds <- read_xpt("sdtm/ds.xpt")

# specs <- readxl::read_excel("metadata/specs.xlsx",
#                     sheet = "Variables") %>%
#   filter(Dataset=="ADSL")
#
# specs_new <- specs %>%
#   mutate(indm = ifelse(Variable %in% names(dm) ,T, F),
#          inex = ifelse(Variable %in% names(ex), T, F))


#xportr_write(adsl, "adam/adsl.xpt")

# Deriving ADSL ----

ex_ext <- ex %>%
  derive_vars_dtm(
    dtc = EXSTDTC,
    new_vars_prefix = "EXST"
  ) %>%
  derive_vars_dtm(
    dtc = EXENDTC,
    new_vars_prefix = "EXEN",
    time_imputation = "last"
  )


adsl <- dm %>%
  select(-DOMAIN) %>%
  mutate(TRT01P = ARM,
         TRT01A = ACTARM,
         SITEGR1 = SITEID,
         AGEGR1 = case_when(AGE < 65 ~ "<65",
                            AGE >=65 & AGE<=80 ~ "65-80",
                            TRUE ~ ">80"),
         AGEGR1N = case_when(AGE < 65 ~ 1,
                            AGE >=65 & AGE<=80 ~ 2,
                            TRUE ~ 3))


adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = (EXDOSE > 0 |
                    (EXDOSE == 0 &
                       str_detect(EXTRT, "PLACEBO"))) & !is.na(EXSTDTM),
    new_vars = vars(TRTSDTM = EXSTDTM, TRTSTMF = EXSTTMF),
    order = vars(EXSTDTM, EXSEQ),
    mode = "first",
    by_vars = vars(STUDYID, USUBJID)
  ) %>%
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = (EXDOSE > 0 |
                    (EXDOSE == 0 &
                       str_detect(EXTRT, "PLACEBO"))) & !is.na(EXENDTM),
    new_vars = vars(TRTEDTM = EXENDTM, TRTETMF = EXENTMF),
    order = vars(EXENDTM, EXSEQ),
    mode = "last",
    by_vars = vars(STUDYID, USUBJID)
  )


adsl <- adsl %>%
  derive_vars_dtm_to_dt(source_vars = vars(TRTSDTM, TRTEDTM)) %>%
  derive_var_trtdurd() %>%
  derive_vars_dt(
    dtc = RFENDTC,
    new_vars_prefix = "RFEN"
  )

adsl <- adsl %>%
  mutate(TRT01AN = case_when(TRT01A=="Placebo" ~ 1,
                             TRT01A=="Xanomeline Low Dose" ~ 2,
                             TRT01A=="Xanomeline High Dose" ~ 3,
                             TRUE ~ as.numeric(NA)),
         TRT01PN = case_when(TRT01P=="Placebo" ~ 1,
                             TRT01P=="Xanomeline Low Dose" ~ 2,
                             TRT01P=="Xanomeline High Dose" ~ 3,
                             TRUE ~ as.numeric(NA)))


adsl <- adsl %>%
  left_join(
    vs %>% filter(VSTESTCD=="HEIGHT" & VISITNUM==1) %>% select(USUBJID, HEIGHTBL = VSSTRESN)
  ) %>%
  left_join(
    vs %>% filter(VSTESTCD=="WEIGHT" & VISITNUM==3) %>% select(USUBJID, WEIGHTBL = VSSTRESN)
  ) %>%
  mutate(BMIBL = compute_bmi(HEIGHTBL,WEIGHTBL),
         BMIBLGR1 = case_when(BMIBL < 25 ~ "<25",
                              BMIBL >=25 & BMIBL < 30 ~ "25-<30",
                              BMIBL >=30 ~ ">=30"))


adsl <- adsl %>%
  mutate(RACEN = case_when(RACE=="AMERICAN INDIAN OR ALASKA NATIVE" ~ 1,
                           RACE=="ASIAN" ~ 2,
                           RACE=="BLACK OR AFRICAN AMERICAN" ~ 3,
                           RACE=="WHITE" ~ 6)) %>%
  mutate(ITTFL = ifelse(!is.na(ARMCD),"Y","N")) %>%
  mutate(SAFFL = ifelse(ITTFL == "Y" & !is.na(TRTSDT),"Y","N"))


vs_8 <- vs %>% filter(VISITNUM==8) %>% distinct(USUBJID, VSDTC) %>% dplyr::rename(VSDTC8 = VSDTC)
vs_10 <- vs %>% filter(VISITNUM==10) %>% distinct(USUBJID, VSDTC) %>% dplyr::rename(VSDTC10 = VSDTC)
vs_12 <- vs %>% filter(VISITNUM==12) %>% distinct(USUBJID, VSDTC) %>% dplyr::rename(VSDTC12 = VSDTC)

adsl <- adsl %>%
  left_join(vs_8) %>%
  left_join(vs_10) %>%
  left_join(vs_12) %>%
  mutate(COMP8FL = ifelse(!is.na(VSDTC8) & VSDTC8<= RFENDTC,"Y","N"),
         COMP16FL = ifelse(!is.na(VSDTC10) & VSDTC10<= RFENDTC,"Y","N"),
         COMP24FL = ifelse(!is.na(VSDTC12) & VSDTC12<= RFENDTC,"Y","N")) %>%
  select(-VSDTC8,-VSDTC10,-VSDTC12)


adsl <- adsl %>%
  left_join(ds %>% filter(DSCAT=="DISPOSITION EVENT") %>% select(USUBJID,DSDECOD)) %>%
  mutate(EOSSTT=ifelse(DSDECOD=="COMPLETED","COMPLETED","DISCONTINUED"))


