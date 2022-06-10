
library(shiny)

## These placeholder values go into the footer HTML files.
## They will be replaced by passed-in (or default) URL strings.

AFCONTACTURL <- "CONTACTURLGOESHERE"
AFDISPLAYAPPSURL <- "DISPLAYAPPSURLGOESHERE"

appFrameFooterFixed <- function(
    displayAppsURL="http://dataexplorer.nwfsc.noaa.gov",
    contactURL="http://www.nwfsc.noaa.gov/contact/feedback.cfm") {
   
    html <- includeHTML(system.file('www','footerFixed.html',
                                    package='appFrame'))

    html <- gsub(x=html, pattern=AFCONTACTURL, replacement=contactURL)
    html <- gsub(x=html, pattern=AFDISPLAYAPPSURL,
                 replacement=displayAppsURL)
    
    tagList(html, includeCSS(system.file('www','footer.css',
                                         package='appFrame')))
}

appFrameFooterScrolling <- function(
    displayAppsURL="http://dataexplorer.nwfsc.noaa.gov",
    contactURL="http://www.nwfsc.noaa.gov/contact/feedback.cfm") {

    html <- includeHTML(system.file('www','footerScrolling.html',
                                    package='appFrame'))

    html <- gsub(x=html, pattern=AFCONTACTURL, replacement=contactURL)
    html <- gsub(x=html, pattern=AFDISPLAYAPPSURL,
                 replacement=displayAppsURL)
    
    tagList(html, includeCSS(system.file('www','footer.css',
                                         package='appFrame')))
}

appFrameHeaderFixed <- function(overlapHeight=7) {
    browser()
    tagList(includeHTML(system.file('www','headerFixed.html',
                                    package='appFrame')),
            includeCSS(system.file('www','header.css',
                                   package='appFrame')),
            div(style=paste("height:",
                    overlapHeight,
                    "em",
                    sep=''))
            )
}

appFrameHeaderScrolling <- function(overlapHeight=7) {
    tagList(includeHTML(system.file('www','headerScrolling.html',
                                    package='appFrame')),
            includeCSS(system.file('www','header.css',
                                   package='appFrame')),
            div(style=paste("height:",
                    overlapHeight,
                    "em",
                    sep=''))

            )
}


