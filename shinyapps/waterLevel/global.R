# load the necessary packages
library(shiny)
library(shinyjs)
library(shiny.i18n)
library(hyd1d)

# rivers
rivers <- c("ELBE", "RHINE")
df.from_to <- data.frame(river    = rivers, 
                         from     = c(0, 336.2),
                         to       = c(585.7, 865.7),
                         from_val = c(257, 336.2),
                         to_val   = c(262, 362.4))

# https://stat.ethz.ch/R-manual/R-devel/library/base/html/chartr.html
simpleCap <- function(x) {
    paste0(toupper(substring(x, 1, 1)), tolower(substring(x, 2)))
}

# https://groups.google.com/forum/#!topic/shiny-discuss/gj-PDGH0KuM
alignRight <- function(el) {
    htmltools::tagAppendAttributes(el,
                                   style="margin-left:auto;margin-right:none;"
    )
}

# translation
translator <- Translator$new(translation_json_path = "translation.json")

# JavaScript to determine browser language
jscode <- paste0("var language =  window.navigator.userLanguage || window.navi",
                 "gator.language;Shiny.onInputChange('lang', language);console",
                 ".log(language);")
de <- function(x) {
    if (is.null(x)) {return(FALSE)}
    if (startsWith(x, "de")) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

