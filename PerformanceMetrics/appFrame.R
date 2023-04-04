
library(shiny)

## These placeholder values go into the footer HTML files.
## They will be replaced by passed-in (or default) URL strings.

AFCONTACTURL <- "CONTACTURLGOESHERE"
AFDISPLAYAPPSURL <- "DISPLAYAPPSURLGOESHERE"

appFrameFooterFixed <- function(
    displayAppsURL="http://dataexplorer.nwfsc.noaa.gov",
    contactURL="http://www.nwfsc.noaa.gov/contact/feedback.cfm") {
   
    html <- includeHTML(file.path('www','footerFixed.html'))

    html <- gsub(x=html, pattern=AFCONTACTURL, replacement=contactURL)
    html <- gsub(x=html, pattern=AFDISPLAYAPPSURL,
                 replacement=displayAppsURL)
    
    tagList(html, includeCSS(file.path('www','footer.css')))
}

appFrameFooterScrolling <- function(
    displayAppsURL="http://dataexplorer.nwfsc.noaa.gov",
    contactURL="http://www.nwfsc.noaa.gov/contact/feedback.cfm") {

    html <- includeHTML(file.path('www','footerScrolling.html'))

    html <- gsub(x=html, pattern=AFCONTACTURL, replacement=contactURL)
    html <- gsub(x=html, pattern=AFDISPLAYAPPSURL,
                 replacement=displayAppsURL)
    
    tagList(html, includeCSS(file.path('www','footer.css')))
}

appFrameHeaderFixed <- function(overlapHeight=7) {
    tagList(includeHTML(file.path('www','headerFixed.html')),
            includeCSS(file.path('www','header.css')),
            div(style=paste("height:",
                    overlapHeight,
                    "em",
                    sep=''))
            )
}

appFrameHeaderScrolling <- function(overlapHeight=7) {
    tagList(includeHTML(file.path('www','headerScrolling.html')),
            includeCSS(file.path('www','header.css')),
            div(style=paste("height:",
                    overlapHeight,
                    "em",
                    sep=''))

            )
}


