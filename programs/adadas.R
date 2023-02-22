# Loading the libraries ----
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("haven","admiral","dplyr","tidyr","metacore","metatools","xportr","stringr","readxl")

ipak(packages)

# Loading ADSL & QS ----

adsl <- read_xpt("adam/adsl.xpt") %>% convert_blanks_to_na()
qs <- read_xpt("sdtm/qs.xpt") %>% convert_blanks_to_na()



adsl_vars <- vars(STUDYID, USUBJID, SITEID,SITEGR1, AGE,AGEGR1, AGEGR1N,RACE,ITTFL,EFFFL,
                  COMP24FL, SEX, TRTSDT, TRTEDT, TRT01P, TRT01PN )

adadas <- qs %>%
  filter(QSCAT=="ALZHEIMER'S DISEASE ASSESSMENT SCALE") %>%
  filter((VISITNUM==3 & QSDY<=1 ) |
           (VISITNUM==8 & QSDY>=2 & QSDY<=84 ) |
           (VISITNUM==10 & QSDY>=85 & QSDY<=140 ) |
           (VISITNUM==12 & QSDY>=141)) %>%
  derive_vars_merged(
    dataset_add = adsl,
    new_vars = adsl_vars,
    by_vars = vars(STUDYID, USUBJID)
  ) %>%
  mutate(
    TRTP = TRT01P,
    TRTPN = TRT01PN,
    PARAMCD = QSTESTCD,
    PARAM =  stringr::str_to_title(QSTEST),
    PARAMN = str_sub(PARAMCD,-2,-1),
    PARAMN = if_else(PARAMN=="OT","15",PARAMN),
    PARAMN = as.numeric(PARAMN),
    AVAL = QSSTRESN,
    ABLFL = QSBLFL,
    RACEN = case_when(RACE=="AMERICAN INDIAN OR ALASKA NATIVE" ~ 6,
                      RACE=="BLACK OR AFRICAN AMERICAN" ~ 2,
                      RACE=="WHITE" ~ 1,
                      TRUE ~ NA_real_)
  ) %>%
  mutate(
    AVISIT = case_when(
      str_detect(VISIT, "SCREEN|UNSCHED|RETRIEVAL|AMBUL") ~ NA_character_,
      !is.na(VISIT) ~ str_to_title(VISIT),
      TRUE ~ NA_character_
    ),
    AVISITN = as.numeric(case_when(
      VISIT == "BASELINE" ~ "0",
      str_detect(VISIT, "WEEK") ~ str_trim(str_replace(VISIT, "WEEK", "")),
      TRUE ~ NA_character_
    ))
  ) %>%
  derive_vars_dt(
    new_vars_prefix = "A",
    dtc = QSDTC
  ) %>%
  rowwise() %>%
  mutate(ADY = case_when(ADT >= TRTSDT ~ as.numeric(difftime(ADT,TRTSDT,"days"))+1,
                         ADT < TRTSDT ~ as.numeric(difftime(TRTSDT,ADT,"days")),
                         TRUE ~ NA_real_)
  ) %>%
  ungroup() %>%
  mutate(
    AWLO = case_when(VISITNUM == 8 ~ 2,
                     VISITNUM == 10 ~ 85,
                     VISITNUM == 12 ~ 141,
                     TRUE ~NA_real_),
    AWHI = case_when(VISITNUM == 3 ~ 1,
                     VISITNUM == 8 ~ 84,
                     VISITNUM == 10 ~ 140,
                     TRUE ~NA_real_),
    AWRANGE = case_when(VISITNUM == 3 ~ "<=1",
                        VISITNUM == 8  ~ "2-84",
                        VISITNUM == 10 ~ "85-140",
                        VISITNUM == 12 ~ ">140",
                        TRUE ~NA_character_),
    AWTARGET = case_when(VISITNUM == 3 ~ 1,
                         VISITNUM == 8 ~ 56,
                         VISITNUM == 10 ~ 112,
                         VISITNUM == 12 ~ 168,
                         TRUE ~NA_real_),
    AWU = "DAYS",
    AWTDIFF = case_when(QSDY>=AWTARGET ~ QSDY - AWTARGET,
                        QSDY<AWTARGET ~ AWTARGET - QSDY)
  ) %>%
  derive_var_base(
    by_vars = vars(USUBJID,PARAMCD),
    source_var = QSSTRESN,
    new_var = BASE,
    filter = QSBLFL=="Y"
  ) %>%
  rowwise() %>%
  mutate(CHG = case_when(AVISIT=="Baseline"~ NA_real_,
                         TRUE ~ AVAL - BASE),
         PCHG = 100*(CHG/BASE)) %>%
  ungroup() %>%
  mutate(DTYPE = NA_character_,
         ANL01FL = "Y")


# Formatting ADADAS for extraction ----

adadas_spec <- readxl::read_xlsx("metadata/specs.xlsx", sheet = "Variables")  %>%
  filter(Dataset=="ADADAS") %>%
  dplyr::rename(type = "Data Type") %>%
  rlang::set_names(tolower) %>%
  mutate(format = str_to_lower(format))

adadas <- adadas %>%
  select(adadas_spec$variable) %>%
  xportr_label(adadas_spec, domain = "ADADAS") %>%
  xportr_format(adadas_spec, domain = "ADADAS") %>%
  xportr_length(adadas_spec, domain = "ADADAS") %>%
  xportr_write(path = "adam/ADADAS.xpt",
               label = "ADAS-Cog Analysis")


