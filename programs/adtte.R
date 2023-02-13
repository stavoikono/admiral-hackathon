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
adae <- read_xpt("adam/adae.xpt") %>% convert_blanks_to_na()

# Deriving ADVS ----

adtte <- adae %>%
  derive_vars_merged(
    dataset_add = adsl,
    new_vars = vars(TRTDURD,RFENDTC,TRT01P),
    by_vars = vars(STUDYID, USUBJID)
  ) %>%
  dplyr::rename(TRTDUR = TRTDURD, TRTP = TRT01P) %>%
  derive_vars_dt(
    new_vars_prefix = "START",
    dtc = RFENDTC
  ) %>%
  mutate(
    PARAM = "Time to First Dermatologic Event",
    PARAMCD = "TTDE",
    ADT = if_else(!is.na(ASTDT) & ASTDT > TRTSDT, ASTDT, as.Date(RFENDTC))
  ) %>%
  #derive_vars_duration(new_var = AVAL,start_date = STARTDT,end_date = ADT) %>%
  mutate(
    AVAL = as.numeric(difftime(ADT,STARTDT, units = "days")) + 1,
    CNSR = if_else(TRTEMFL == "Y", 0, 1),
    EVNTDESC = if_else(CNSR == 0, "Dematologic Event Occured", "Study Completion Date"),
    SRCDOM = if_else(ADT == ASTDT, "ADAE", "ADSL"),
    SRCVAR = if_else(ADT == ASTDT, "ASTDT", "RFENDT"),
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
