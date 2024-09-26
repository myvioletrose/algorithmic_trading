# set environment
readRenviron("~/algorithmic_trading/config/.env")

# load tictoc
if(!require(pacman)){install.packages("pacman"); require(pacman)}
pacman::p_load(tictoc)

# set environment, proxy servers if running the script at Bloomberg office desktop
if(tolower(Sys.getenv("RSTUDIO_USER_IDENTITY")) == "jng410"){
    proxy <- "proxy.bloomberg.com:81"
    Sys.setenv(
        https_proxy = proxy,
        http_proxy = proxy
    )
}

# set local environment variables using global environment
PACKAGE_PATH <- Sys.getenv("PACKAGE_PATH")
STOCK_DIM_PATH <- Sys.getenv("STOCK_DIM_PATH")
WATCHLIST_TEMPLATE_PATH <- Sys.getenv("WATCHLIST_TEMPLATE_PATH")
WATCHLIST_PATH <- Sys.getenv("WATCHLIST_PATH")
DECRYPT_FUNC <- Sys.getenv("DECRYPT_FUNC")
DATA_DIRECTORY <- Sys.getenv("DATA_DIRECTORY")
FUNCTION_DIRECTORY <- Sys.getenv("FUNCTION_DIRECTORY")
PLOT_DIRECTORY <- Sys.getenv("PLOT_DIRECTORY")
PROJECT_HOME_DIRECTORY <- Sys.getenv("PROJECT_HOME_DIRECTORY")
DIAGNOSTIC_FOLDER <- Sys.getenv("DIAGNOSTIC_FOLDER")
TA_FOLDER <- Sys.getenv("TA_FOLDER")
ALPHA_VANTAGE_API <- Sys.getenv("ALPHA_VANTAGE_API")
PASSWORD <- Sys.getenv("PASSWORD")
SENDER <- Sys.getenv("SENDER")
USERNAME <- Sys.getenv("USERNAME")
DB <- Sys.getenv("DB")
HOST_DB <- Sys.getenv("HOST_DB")
DB_PORT <- Sys.getenv("DB_PORT")
DB_USER <- Sys.getenv("DB_USER")
DB_PASSWORD <- Sys.getenv("DB_PASSWORD")
ETL_PROTOCOL <- Sys.getenv("ETL_PROTOCOL")
EXECUTE_PW <- Sys.getenv("EXECUTE_PW")

# set project home directory
setwd(PROJECT_HOME_DIRECTORY)

# install CandleStickPattern manually
#options(download.file.method = "wininet")
#devtools::install_github("kochiuyu/CandleStickPattern")

# load packages
packages <- read.csv(PACKAGE_PATH, header = FALSE)
pacman::p_load(char = as.vector(packages$V1), install = FALSE)

# source all functions
sapply(paste(FUNCTION_DIRECTORY, grep(pattern = "\\.[Rr]$", list.files(FUNCTION_DIRECTORY), value = TRUE), sep = "/"), function(x) source(x)) %>% invisible()
