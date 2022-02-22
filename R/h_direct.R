## h_direct
## @include funmaintext.R


#' @title Open a Web Page in the your browser
#' @description
#' \code{h_framasoft} gives access to several free web services (as in speech and in
#' half pint of beer) that are good alternatives to GAFA services.
#'
#' \code{h_academie} is a bookmark to the Academie francaise's dictionary.
#'
#' \code{h_lexilogos} gives access to hundreds of dictionaries in many languages.
#'
#' \code{h_deepl}, \code{h_googletranslate}, \code{h_interglot}, \code{h_reverso},
#' \code{h_linguee}, \code{h_promt}, \code{h_reverso}, \code{h_systran} provide
#' translation engines. \code{h_linguee} returns examples with long sentences.
#'
#' \code{h_yacy} is a decentralized peer-to-peer web search software.
#'
#' \code{h_etz} is a bookmark to the EveryTimeZone website.
#'
#' \code{h_tad} and \code{h_tadsm} are bookmarks to timeanddate.com, a website
#' dedicated to date and time conversion plus timezone management.
#'
#' \code{h_meteoblue} and \code{h_windy} are bookmarks to the Meteoblue and Windy
#' meteo and weather websites.
#'
#' @examples
#' if (interactive()) {
#' h_linguee()
#' h_lexilogos()
#' }
#' @name h_direct
NULL

#' @export
#' @rdname h_direct
h_academie <- function() {
    msg <- "Open Dictionnaire de l'Academie francaise in your browser"
    url <- "https://www.dictionnaire-academie.fr"
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_direct
h_deepl <- function() {
    msg <- "Open DeepL online-translator in your browser"
    url <- "https://www.deepl.com/translator"
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_direct
h_etz <- function() {
    msg <- "Open EveryTimeZone in your browser"
    url <- "https://everytimezone.com"
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_direct
h_framasoft <- function() {
    msg <- "Open Framasoft web services in your browser"
    url <- "https://degooglisons-internet.org/en/list/"
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_direct
h_framasoft0 <- function() {
    msg <- "Open Framasoft web services in your browser"
    url <- "https://framasoft.org/en/"
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_direct
h_googletranslate <- function() {
    msg <- "Open Google Translate in your browser"
    url <- "https://translate.google.com"
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_direct
h_interglot <- function() {
    msg <- "Open Interglot translation services in your browser"
    url <- "https://www.interglot.com/dictionary"
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_direct
h_lexilogos <- function() {
    msg <- "Open Lexilogos in your browser"
    url <- "https://www.lexilogos.com/english/index.htm"
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_direct
h_linguee <- function() {
    msg <- "Open Linguee in your browser"
    url <- "https://www.linguee.com"
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_direct
h_meteoblue <- function() {
    msg <- "Open Meteoblue website in your browser"
    url <- "https://www.meteoblue.com"
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_direct
h_promt <- function() {
    msg <- "Open PROMT online-translator in your browser"
    url <- "https://www.online-translator.com"
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_direct
h_reverso <- function() {
    msg <- "Open Reverso translation services in your browser"
    url <- "http://www.reverso.net/text_translation.aspx?lang=FR"
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_direct
h_systran <- function() {
    msg <- "Open Systran translation services in your browser"
    url <- "https://translate.systran.net/translationTools/text"
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_direct
h_tad <- function() {
    msg <- "Open Time-and-Date in your browser"
    url <- "https://www.timeanddate.com/worldclock/fixedform.html"
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_direct
h_tadsm <- function() {
    msg <- "Open Time-and-Date Site Map in your browser"
    url <- "https://www.timeanddate.com/sitemap.html"
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_direct
h_windy <- function() {
    msg <- "Open Windy weather map in your browser"
    url <- "https://www.windy.com"
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_direct
h_yacy <- function() {
    msg <- "Open YaCy software in your browser"
    url <- "https://yacy.net"
    trybrowseURL(url, msgT = msg)
}



