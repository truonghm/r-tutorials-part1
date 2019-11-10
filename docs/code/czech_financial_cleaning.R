# clearing environment
rm(list = ls())
gc()

# loading libraries
library(dplyr, quietly = TRUE)
library(lubridate, quietly = TRUE)
library(stringr, quietly = TRUE)
library(DT, quietly = TRUE) # load df in Viewer quadrant

# set up path
file_path <- "data/czech_financial_raw/"
file_list <- list.files(path=file_path)

# importing data

df_list <- lapply(file_list, 
                  function (x) {
                    read.table (file=paste0(file_path, x),
                                header=TRUE,
                                sep=";",
                                na.strings=c("","NA") )
                    }
                  )

names(df_list) <- gsub(".asc", "_df", file_list)

# account df

df_list$account_df <- df_list$account_df %>%
  mutate(frequency = factor(case_when(frequency == "POPLATEK MESICNE" ~ "MONTHLY",
                                      frequency == "POPLATEK TYDNE" ~ "WEEKLY",
                                      frequency == "POPLATEK PO OBRATU" ~ "AFTER_TRANSACTION",
                                      TRUE ~ "UNKNOWN"))) %>%
  mutate(date = ymd(date)) %>%
  rename(acc_date = date,
         acc_frequency = frequency)


# client df

df_list$client_df <- df_list$client_df %>%
  mutate(YY = as.numeric(substr(birth_number, 1, 2))+1900,
         MM = as.numeric(substr(birth_number, 3, 4)),
         DD = as.numeric(substr(birth_number, 5, 6))) %>%
  mutate(client_birthday = case_when(MM > 50 ~ as.Date(paste(YY, MM-50, DD, sep="-"),"%Y-%m-%d"),
                                     TRUE ~ as.Date(paste(YY, MM, DD, sep="-"),"%Y-%m-%d")),
         client_gender = factor(case_when(MM > 50 ~ 'F',
                                          TRUE ~ 'M'))) %>%
  select(client_id, district_id, client_birthday, client_gender)


# disposition df

df_list$disp_df <- df_list$disp_df %>%
  rename(disp_type = type)

# order df

df_list$order_df <- df_list$order_df %>%
  mutate(k_symbol = factor(case_when(k_symbol == "POJISTNE" ~ "INSURANCE_PAYMENT",
                                     k_symbol == "SIPO" ~ "HOUSEHOLD_PAYMENT",
                                     k_symbol == "UVER" ~ "LOAN_PAYMENT",
                                     k_symbol == "LEASING" ~ "LEASING_PAYMENT",
                                     TRUE ~ "UNKNOWN"))) %>%
  rename(order_k_symbol = k_symbol,
         order_to_bank = bank_to,
         order_to_account = account_to,
         order_amount = amount)


# transaction df

df_list$trans_df <- df_list$trans_df %>%
  mutate(type = factor(case_when(type == "PRIJEM" ~ "CREDIT",
                                 type == "VYDAJ" ~ "WITHDRAWAL",
                                 TRUE ~ "UNKNOWN")),
         operation = factor(case_when(operation == "VYBER KARTOU" ~ "CC_WITHDRAWAL",
                                      operation == "VKLAD" ~ "CREDIT_IN_CASH",
                                      operation == "PREVOD Z UCTU" ~ "COLL_FROM_OTHER_BANK",
                                      operation == "VYBER" ~ "WITHDRAWAL_IN_CASH",
                                      operation == "PREVOD NA UCET" ~ "REMITTANCE_TO_OTHER_BANK",
                                      TRUE ~ "UNKNOWN")),
         k_symbol = factor(case_when(k_symbol == "POJISTNE" ~ "INSURANCE_PAYMENT",
                                     k_symbol == "SLUZBY" ~ "PAYMENT_FOR_STATEMENT",
                                     k_symbol == "UROK" ~ "INTEREST_CREDITED",
                                     k_symbol == "SANKC. UROK" ~ "SANCTION_INTEREST",
                                     k_symbol == "SIPO" ~ "HOUSEHOLD_PAYMENT",
                                     k_symbol == "DUCHOD" ~ "OLD_AGE_PENSION",
                                     k_symbol == "UVER" ~ "LOAN_PAYMENT",
                                     TRUE ~ "UNKNOWN")),
         date = ymd(date)) %>%
  rename(trans_type = type,
         trans_operation = operation,
         trans_k_symbol =k_symbol,
         trans_date = date,
         trans_amount = amount,
         trans_balance_after = balance,
         trans_to_bank = bank,
         trans_to_account = account)


# loan df

df_list$loan_df <- df_list$loan_df %>%
  mutate(date = ymd(date)) %>%
  rename(loan_date = date,
         loan_amount = amount,
         loan_duration = duration,
         loan_payments = payments,
         loan_status = status)


# card df

df_list$card_df <- df_list$card_df %>%
  mutate(issued = as_date(ymd_hms(issued))) %>%
  rename(card_type = type,
         card_issued_date = issued)


# demographic data

df_list$district_df <- df_list$district_df %>%
  rename(district_id=A1,
         district_name=A2,
         region=A3,
         population=A4,
         num_munipalities_gt499=A5,
         num_munipalities_500to1999=A6,
         num_munipalities_2000to9999=A7,
         num_munipalities_gt10000=A8,
         num_cities=A9,
         urban_ratio=A10,
         avg_salary=A11,
         unemp_rate95=A12,
         unemp_rate96=A13,
         entrep_rate_per1000=A14,
         num_crimes95=A15,
         num_crimes96=A16)

sapply(names(df_list), 
       function (x) {
         write.csv(df_list[[x]], 
                   file=paste0("data/czech_financial_formatted/", x, ".csv") )
         }
       )

