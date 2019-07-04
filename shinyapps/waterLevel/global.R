# load the necessary packages
library(shiny)
library(hyd1d)

# set english locale to produce english plot labels
Sys.setlocale(category = "LC_MESSAGES", locale = "en_US.utf8")

#https://stackoverflow.com/questions/47750273/shiny-application-get-browser-language-settings
#https://github.com/chrislad/multilingualShinyApp

# rivers
rivers <- c("ELBE", "RHEIN")
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

