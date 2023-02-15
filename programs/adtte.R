# Loading the libraries ----
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("haven","admiral","dplyr","tidyr","metacore","metatools","xportr","stringr","readxl")

ipak(packages)

# Loading ADSL & ADAE ----

adsl <- read_xpt("adam/adsl.xpt") %>% convert_blanks_to_na()
adae <- read_xpt("adam/adae.xpt") %>% convert_blanks_to_na()

# Deriving ADTTE ----

adtte <- adsl %>%
  derive_vars_merged(
    adae,
    new_vars = vars(ASTDT, TRTEMFL,AESEQ),
    by_vars = vars(STUDYID, USUBJID),
    filter_add = !is.na(CQ01NAM) & AOCC01FL=="Y"
  ) %>%
  mutate(
    RACEN = case_when(RACE=="AMERICAN INDIAN OR ALASKA NATIVE" ~ 6,
                      RACE=="BLACK OR AFRICAN AMERICAN" ~ 2,
                      RACE=="WHITE" ~ 1,
                      TRUE ~ NA_real_)
  ) %>%
  dplyr::rename(
    TRTDUR = TRTDURD,
    TRTP = TRT01P,
    TRTA = TRT01A,
    TRTAN = TRT01AN
  ) %>%
  derive_vars_dt(
    new_vars_prefix = "START",
    dtc = RFSTDTC
  ) %>%
  mutate(
    PARAM = "Time to First Dermatologic Event",
    PARAMCD = "TTDE",
    ADT = if_else(!is.na(ASTDT) & ASTDT >= TRTSDT, ASTDT, as.Date(RFENDTC))
  ) %>%
  mutate(
    AVAL = as.numeric(difftime(ADT,STARTDT, units = "days")) + 1,
    CNSR = if_else(is.na(TRTEMFL),1,0),
    EVNTDESC = if_else(CNSR == 0, "Dematologic Event Occured", "Study Completion Date")
  ) %>%
  mutate(
    SRCDOM = case_when(ADT == ASTDT ~ "ADAE",
                       TRUE ~ "ADSL"),
    SRCVAR = case_when(ADT == ASTDT ~"ASTDT",
                       TRUE ~ "RFENDT"),
    SRCSEQ = if_else(SRCDOM == "ADAE", AESEQ, NA_real_)
  )


# Formatting ADSL for extraction ----

adtte_spec <- readxl::read_xlsx("metadata/specs.xlsx", sheet = "Variables")  %>%
  filter(Dataset=="ADTTE") %>%
  dplyr::rename(type = "Data Type") %>%
  rlang::set_names(tolower) %>%
  mutate(format = str_to_lower(format))

adtte <- adtte %>%
  select(adtte_spec$variable) %>%
  xportr_label(adtte_spec, domain = "ADTTE") %>%
  xportr_format(adtte_spec, domain = "ADTTE") %>%
  xportr_length(adtte_spec, domain = "ADTTE") %>%
  xportr_write(path = "adam/ADTTE.xpt",
               label = "AE Time To 1st Derm. Event Analysis")
