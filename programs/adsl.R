library(haven)
library(admiral)
library(dplyr)
library(tidyr)
library(metacore)
library(metatools)
library(xportr)
library(stringr)

dm <- read_xpt("sdtm/dm.xpt")
ex <- read_xpt("sdtm/ex.xpt")
vs <- read_xpt("sdtm/vs.xpt")

# specs <- readxl::read_excel("metadata/specs.xlsx",
#                     sheet = "Variables") %>%
#   filter(Dataset=="ADSL")
#
# specs_new <- specs %>%
#   mutate(indm = ifelse(Variable %in% names(dm) ,T, F),
#          inex = ifelse(Variable %in% names(ex), T, F))


#xportr_write(adsl, "adam/adsl.xpt")

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
  derive_var_trtdurd()

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

