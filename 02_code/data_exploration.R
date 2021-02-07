
filepath <- "01_data/comdirect.xml"

filepath_example <- "01_data/test.xml"

# PACKAGES
library(XML)
library(dplyr)

# FUNCTIONS ----
# currently stored in script until package will be created

# DATA IMPORT ----
read_pp_data <- function(file){
  # parsing xml file
  parsed_xml <- XML::xmlParse(file)
  
  # turn xml file to list
  out <- XML::xmlToList(parsed_xml)
  
  return(out)
}

# SECRUITY RESHAPING ----
reshape_secruity_price <- function(x){
  # turning vector to data frame
  out <- as.data.frame(x) %>% 
    # turning rownames to column
    tibble::rownames_to_column() %>% 
    # reshaping data
    tidyr::spread(key = 1, value = 2) %>%
    # renaming columns
    dplyr::rename(date = t, price = v) %>% 
    dplyr::mutate(date = lubridate::ymd(date),
                  price = as.double(price) / 10000) %>% 
    tibble::as_tibble()
  
  return(out)
} 


get_secruity_data <- function(x, secruity = 1){
  secruity_data <- x$securities[secruity][["security"]]
  sec_label <- glue::glue("security[{secruity}]")
  
  # META
  name <- secruity_data[["name"]]
  ticker <- secruity_data[["tickerSymbol"]]
  wkn <- secruity_data[["wkn"]]
  currency <- secruity_data[["currencyCode"]]
  
  # PRICE
  price_data <- purrr::map(.x = secruity_data[["prices"]],
                           .f = reshape_secruity_price) %>% 
    dplyr::bind_rows()
  
  # OUTPUT
  out <- price_data %>% 
    dplyr::mutate(name = name, 
                  ticker = ticker,
                  wkn = wkn,
                  currency = currency,
                  label = as.character(sec_label))
  
}


# TRANSACTION EXPORT ----
# helper function for sec extraction
extract_sec_transaction_information <- function(x){
  
  date <- x[["date"]]
  currency <- x[["currencyCode"]]
  amount <- as.double(x[["amount"]]) / 100
  sec <- x[["security"]]
  sec_clean <- stringr::str_extract(sec, "(?<=securities/).+")
  sec_fin <-ifelse(sec_clean == "security", "security[1]", sec_clean)
  shares <- as.double(x[["shares"]]) / 1000000 
  type <- x[["type"]]
  
  out <- tibble::tibble(date = date, 
                        currency = currency, 
                        type = type,
                        amount = amount,
                        secruity = sec_fin,
                        shares = shares)
  
  
  return(out)
  
}

# RESHAPE SECRUITIES ----
reshape_sec_transactions <- function(x){
  # META DATE
  transaction_date <- x[["date"]]
  
  # get all transactions
  all_trans <- x[["crossEntry"]]$portfolio$transactions
  
  # extract length of each transaction
  trans_vec_all <- purrr::map(.x = all_trans,
                              .f = length) %>% 
    unlist()
  
  # subsetting transactions and ignoring references
  all_trans_subset <- all_trans[as.vector(which(trans_vec_all > 1))]
  
  # reshaping data and get data frame
  sec_transactions <- purrr::map(.x = all_trans_subset,
                                 .f = extract_sec_transaction_information) %>% 
    dplyr::bind_rows()
  
  # date to date format
  out <- sec_transactions %>% 
    dplyr::mutate(date = dplyr::case_when(
      stringr::str_detect(date, "date") ~ stringr::str_remove(transaction_date, "(?=T).+"),
      TRUE ~ stringr::str_remove(date, "(?=T).+"),
    )) %>% 
    dplyr::mutate(date = lubridate::ymd(date))
  
  return(out)
  
}

# RESHAPE DIVIDENDS ----
reshape_div_transactions <- function(x){
  
  sec <- stringr::str_extract(x[["security"]], "(?<=securities/).+")
  sec_label <-ifelse(sec == "security", "security[1]", sec)
  amount <- as.double(x[["amount"]]) / 100
  date <- stringr::str_remove(x[["date"]], "(?=T).+")
  shares <- as.double(x[["shares"]]) / 1000000 
  
  out <- tibble::tibble(date = lubridate::ymd(date), 
                        currency = x[["currencyCode"]], 
                        dividend = amount,
                        secruity = sec_label,
                        shares = shares)
  
  return(out)
  
}

# DEPOSIT REMOVE RESHAPE ----
reshape_dr_transactions <- function(x){
  amount <- as.double(x[["amount"]]) / 100
  date <- stringr::str_remove(x[["date"]], "(?=T).+")
  type <-x[["type"]]
  
  
  out <- tibble::tibble(date = lubridate::ymd(date), 
                        currency = x[["currencyCode"]], 
                        amount = amount,
                        type = type)
}

# WRAPPER ACCOUNT DATA ----
extract_acc_data <- function(acc_data){
  
  # TEST
  # acc_data[["account"]]
  
  # extract account name for which transactions should be extracted
  acc_name <- acc_data[["name"]]
  transactions <- acc_data[["transactions"]]
  
  # get all transactions
  transactions <- acc_data[["transactions"]]
  
  
  trans_vec <- purrr::map(.x = transactions,
                          .f = length) %>% 
    unlist()
  
  subset_transactions <- transactions[as.vector(which(trans_vec > 1))]
  
  # extract all transaction types
  transaction_types <- purrr::map(.x = subset_transactions,
                                  .f = ~ .x[["type"]]) %>% 
    unlist() %>% as.vector()
  
  # BUY
  buy_index <- which(transaction_types == "BUY")
  sec_transactions <- subset_transactions[buy_index]
  
  # reshaping sec transactions
  sec_data <- purrr::map(.x = sec_transactions,
                         .f = reshape_sec_transactions) %>% 
    dplyr::bind_rows() %>% 
    dplyr::mutate(account_name = acc_name)
  
  # DIVIDENDS
  div_index <- which(transaction_types == "DIVIDENDS")
  div_transactions <- subset_transactions[div_index]
  
  x <- div_transactions[[1]]
  
  div_data <- purrr::map(.x = div_transactions,
                         .f = reshape_div_transactions) %>% 
    dplyr::bind_rows() %>% 
    dplyr::mutate(account_name = acc_name)
  
  # DEPOSIT & REMOVAL 
  dr_index <- which(transaction_types %in% c("DEPOSIT", "REMOVAL"))
  dr_transactions <- subset_transactions[dr_index]
  
  dr_data <- purrr::map(.x = dr_transactions,
                        .f = reshape_dr_transactions) %>% 
    dplyr::bind_rows() %>% 
    dplyr::mutate(amount = dplyr::case_when(
      type == "REMOVAL" ~ amount*-1,
      T ~ amount
    )) %>% 
    dplyr::mutate(account_name = acc_name)
  
  out <- list(secruities = sec_data,
              dividends = div_data,
              deposit_removal = dr_data)
  
  return(out)
  
}


# WRAPPER WHOLE TRANSACTION DATA ----
get_transaction_data <- function(data){
  # get length of account entries
  acc_length <- purrr::map(.x = data$accounts,
                           .f = length) %>% 
    unlist()
  
  # get index of needed accounts
  acc_index <- which(acc_length > 1)
  
  # subset account data
  acc_data <- data$accounts[acc_index]
  
  
  # acc_data[["account"]]
  acc_extractions <- purrr::map(.x = acc_data,
                                .f = extract_acc_data)
  
  all_sec <- purrr::map(.x = acc_extractions,
                        .f = ~ .x[["secruities"]]) %>% 
    dplyr::bind_rows()
  
  all_div <- purrr::map(.x = acc_extractions,
                        .f = ~ .x[["dividends"]]) %>% 
    dplyr::bind_rows()
  
  all_dr <- purrr::map(.x = acc_extractions,
                       .f = ~ .x[["deposit_removal"]]) %>% 
    dplyr::bind_rows()
  
  out <- list(secruities = all_sec,
              dividends = all_div,
              deposit_removal = all_dr)
  
  return(out)
  
}


# reading data
data_xml <- read_pp_data(filepath)


# ALL TRANS DATA ----
transactions <- get_transaction_data(data = data_xml)

# manual check
transactions$secruities %>% 
  dplyr::arrange(desc(date))

# ALL SEC DATA ----
# testing function
secruity_data <- purrr::map(.x = 1:length(data_xml$securities),
                            .f = ~get_secruity_data(data_xml, secruity = .x)) %>% 
  dplyr::bind_rows()

secruity_data %>% 
  dplyr::select(name, ticker, wkn, currency, label) %>% 
  dplyr::distinct() %>% 
  as.data.frame()







