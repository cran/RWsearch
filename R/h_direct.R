## h_direct
## @include funmaintext.R


#' @title Open a Web Page in the Browser
#' @description 
#' \code{h_academie} gives access to the Academie francaise's dictionary.
#' 
#' \code{h_etz} gives access to the EveryTimeZone website.
#' 
#' \code{h_framasoft} gives access to several free web services (as in speech and in 
#' half pint of beer) that are good alternatives to GAFA services. 
#' 
#' \code{h_lexilogos} gives access to hundreds of dictionaries in many languages.
#' 
#' \code{h_googletranslate}, \code{h_interglot}, \code{h_reverso}, \code{h_linguee}, 
#' \code{h_promt}, \code{h_reverso}, \code{h_systran} provide translation engines. 
#' \code{h_linguee} returns examples with long sentences.
#' 
#' \code{h_tad} and \code{h_tadsm} gives access to a website dedicated to date 
#' and time conversion plus timezone management.
#' 
#' \code{h_yacy} is a decentralized peer-to-peer web search software.
#' 
#' Using the regular R format "w1 w2 w3" rather than w1, w2, w3 makes sense as most
#' engines collapse the words into character chains "w1 w2 w3", "w1+w2+w3" or "w1-w2-w3".
#' 
#' @examples
#' \donttest{
#' h_linguee()
#' h_lexilogos()
#' }
#' @name h_direct
NULL

#' @export
#' @rdname h_direct
h_academie <- function() {
    message("Open Dictionnaire de l'Academie francaise in browser")
utils::browseURL("https://www.dictionnaire-academie.fr")
}

#' @export
#' @rdname h_direct
h_etz <- function() {
    message("Open EveryTimeZone in browser")
utils::browseURL("https://everytimezone.com")
}

#' @export
#' @rdname h_direct
h_framasoft <- function() {
    message("Open Framasoft web services in browser")
utils::browseURL("https://degooglisons-internet.org/en/list/")
}

#' @export
#' @rdname h_direct
h_framasoft0 <- function() {
    message("Open Framasoft web services in browser")
utils::browseURL("https://framasoft.org/en/")
}

#' @export
#' @rdname h_direct
h_googletranslate <- function() {
    message("Open Google Translate in browser")
utils::browseURL("https://translate.google.com")
}

#' @export
#' @rdname h_direct
h_interglot <- function() {
    message("Open Interglot translation services in browser")
utils::browseURL("https://www.interglot.com/dictionary")
}

#' @export
#' @rdname h_direct
h_lexilogos <- function() {
    message("Open Lexilogos in browser")
utils::browseURL("https://www.lexilogos.com/english/index.htm")
}

#' @export
#' @rdname h_direct
h_linguee <- function() {
    message("Open Linguee in browser")
utils::browseURL("https://www.linguee.com")
}

#' @export
#' @rdname h_direct
h_promt <- function() {
    message("Open PROMT online-translator in browser")
utils::browseURL("https://www.online-translator.com")
}

#' @export
#' @rdname h_direct
h_reverso <- function() {
    message("Open Reverso translation services in browser")
utils::browseURL("http://www.reverso.net/text_translation.aspx?lang=FR")
}

#' @export
#' @rdname h_direct
h_systran <- function() {
    message("Open Systran translation services in browser")
utils::browseURL("https://translate.systran.net/translationTools/text")
}

#' @export
#' @rdname h_direct
h_tad <- function() {
    message("Open Time-and-Date in browser")
utils::browseURL("https://www.timeanddate.com/worldclock/fixedform.html")
}

#' @export
#' @rdname h_direct
h_tadsm <- function() {
    message("Open Time-and-Date Site Map in browser")
utils::browseURL("https://www.timeanddate.com/sitemap.html")
}

#' @export
#' @rdname h_direct
h_yacy <- function() {
    message("Open YaCy software in browser")
utils::browseURL("https://yacy.net/en/")
}



