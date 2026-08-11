# obtain meaningful error messages
Sys.setlocale(category = "LC_ALL", locale = "en_US.UTF-8")
Sys.setlocale(category = "LC_PAPER", locale = "en_US.UTF-8")
Sys.setlocale(category = "LC_MEASUREMENT", locale = "en_US.UTF-8")
Sys.setlocale(category = "LC_MESSAGES", locale = "en_US.UTF-8")

# load rsconnect-package
library(rsconnect)

# deploy
# waterLevel
appDir <- "shinyapps/waterLevel"

files <- listDeploymentFiles(
    appDir = appDir,
    appFiles = NULL,
    appFileManifest = NULL,
    error_call = caller_env()
)

rsconnect::deployApp(
    appDir = appDir,
    appFiles = files,
    appName = "waterLevel",
    appTitle = "waterLevel",
    launch.browser = FALSE,
    logLevel = "verbose",
    forceUpdate = TRUE,
    appId = 4,
    appVisibility = "public",
    account = "arnd.weber@bafg.de",
    server = "shiny.bafg.de")

# waterLevelPegelonline
appDir <- "shinyapps/waterLevelPegelonline"

files <- listDeploymentFiles(
    appDir = appDir,
    appFiles = NULL,
    appFileManifest = NULL,
    error_call = caller_env()
)

rsconnect::deployApp(
    appDir = appDir,
    appFiles = files,
    appName = "waterLevelPegelonline",
    appTitle = "waterLevelPegelonline",
    launch.browser = FALSE,
    logLevel = "verbose",
    forceUpdate = TRUE,
    appId = 5,
    appVisibility = "public",
    account = "arnd.weber@bafg.de",
    server = "shiny.bafg.de")

