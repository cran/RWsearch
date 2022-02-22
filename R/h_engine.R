## h_engine
## @include h_direct.R


## @title Hidden Functions to Adapt Messages and URL to Web Search Engine Syntax.
## @description
## Collapse a text string with (" " "+" "-") to the format required by the engine API.
## \code{"word1 word2 word3"} or \code{"word1+word2+word3"} or \code{"word1-word2-word3"}.
fme <- function(txt, words) {
    words <- paste(words, collapse = " ")
    words <- gsub(",", " ", words)
    words <- gsub("  ", " ", words)
    words <- gsub("  ", " ", words)
    message(paste(txt, words))
}

fbr <- function(engine, words, word2 = "", word3 = "", coll = "+", encode = FALSE) {
    words <- paste(words, collapse = " ")
    words <- gsub(",", " ", words)
    words <- gsub("  ", " ", words)
    words <- gsub("  ", " ", words)
    words <- gsub(" ", coll, words)
    words <- paste0(words, word2, word3)
    url <- if (encode) { paste0(engine, utils::URLencode(words))
           } else      { paste0(engine, words) }
    trybrowseURL(url)
}


## NEW IN v_4.9.14
## fmsg <-> fme, furl <-> fbr
fmsg <- function(txt, words) {
    words <- paste(words, collapse = " ")
    words <- gsub(",", " ", words)
    words <- gsub("  ", " ", words)
    words <- gsub("  ", " ", words)
    paste(txt, words)
}

furl <- function(engine, words, word2 = "", word3 = "", coll = "+", encode = FALSE) {
    words <- paste(words, collapse = " ")
    words <- gsub(",", " ", words)
    words <- gsub("  ", " ", words)
    words <- gsub("  ", " ", words)
    words <- gsub(" ", coll, words)
    words <- paste0(words, word2, word3)
    url <- if (encode) { paste0(engine, utils::URLencode(words))
           } else      { paste0(engine, words) }
    url
}


#' @title Explore the Web with Various Search Engines
#' @description
#' Launch the default browser and search in: ABC Bourse (short
#' stock names), arXiv (vectorized), Ask, Baidu, Blackle, Bing, Bing Map (bmap),
#' Boursorama (short stocknames), CNRTL (French dictionary),
#' Collins English Dictionary, CPAN and metaCPAN (Perl),
#' Crossref (DOI and bibliographic metadata), CTAN (Latex), Daum, DailyMotion (dm),
#' DOI, DuckDuckGo (ddg), Ecosia, Egerin, Evene (citations), Exalead, Excite,
#' Gigablast, GitHub, GitLab, Google Map (gmap), Google, Google Scholar (gscholar),
#' IANA TLD root domain database, IANA WHOIS service, Info, Khoj, Les Echos,
#' La Tribune (lt), Lilo, Lycos, Mappy Map, Merriam-Webster (mw, English dictionary),
#' Nabble, Nate, Naver (see N2H4 package), Orcid, Open Street Map, OSM Nominatim,
#' Parsijoo, PeerTube, Peru, Pipilika,
#' Qwant (qw + qwfr), R-bloggers, Rdocumentation (rdoc), Rdocumentation task views (rdoctv),
#' Rdrr, Reverso dictionnary, Rseek, Sapo, Searx, Sogou, SSRN and SSRN Author (vectorized),
#' Stackoverflow (so), Startpage (ex-Ixquick), Twitter (+ twfr),
#' L'Usine Nouvelle (un), ViaMichelin Map and Routes (via), Les Verbes, Vimeo, Wego (Here maps),
#' Wikipedia (wp + wpfr), Yahoo, Yahoo Finance, Yandex, Yooz, Youtube (yt).
#'
#' \code{h_zbib} is a bookmark to ZoteroBib, a service that returns the complete
#' bibliographic reference from a fragment of information: URL, ISBN, DOI, PMID,
#' arXiv id or title and generates a MD5 number to retrieve it later.t
#'
#' Using the regular R format "w1 w2 w3" rather than w1, w2, w3 makes sense as most
#' functions collapse the words into character chains "w1 w2 w3", "w1+w2+w3" or "w1-w2-w3".
#'
#' Visit \url{https://en.wikipedia.org/wiki/Web_search_engine} for a list of web search
#' engines.
#' @param   ...      any format recognized by \code{\link{cnsc}}, except list.
#'                   A vector of packages.
#' @param   char     (name to) a character vector. Use this argument if
#'                   \code{...} fails or if you call the function from another function.
#' @param   lang     character. The language accepted by the search engine, usually
#'                   "en", "de", "es", "fr", "jp", etc.
#' @examples
#' if (interactive()) {
#' h_yt("Serge Gainsbourg Ne dis rien")
#' h_so(R, deep, neural, network)
#' h_osm("Le Chateau d'Oleron")
#' h_mw(recension)
#' h_arxiv(c(1212.4320, 1605.08732))
#' h_doi("10.1016/j.ejor.2013.06.029")
#' }
#' @name h_engine
NULL

#' @export
#' @rdname h_engine
h_abcbourse <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("ABC Bourse results for:", words)
    url <- furl("https://www.abcbourse.com/graphes/display.aspx?s=", words)
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_ask <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Bing results for:", words)
    url <- furl("https://www.ask.com/web?q=", words)
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_arxiv <- function(..., char = NULL) {
    funTF <- function(TF, char) ifelse(TF, sprintf("%.4f", char), sprintf("%.5f", char))
    if (tryCatch(is.numeric(...),  condition = function(cond) FALSE)) char <- (...)
    if (tryCatch(is.numeric(char), condition = function(cond) FALSE)) {
        TF   <- (char %/% 1) < 1501
        char <- funTF(TF, char)
    }
    words <- if (is.null(char)) cnscinfun() else char
    for (word in words) {
        msg <- fmsg("arXiv pages for:", word)
        url <- furl("https://arxiv.org/abs/", word)
        trybrowseURL(url, msgT = msg)
    }
}

#' @export
#' @rdname h_engine
h_arxivpdf <- function(..., char = NULL) {
    funTF <- function(TF, char) ifelse(TF, sprintf("%.4f", char), sprintf("%.5f", char))
    if (tryCatch(is.numeric(...),  condition = function(cond) FALSE)) char <- (...)
    if (tryCatch(is.numeric(char), condition = function(cond) FALSE)) {
        TF   <- (char %/% 1) < 1501
        char <- funTF(TF, char)
    }
    words <- if (is.null(char)) cnscinfun() else char
    for (word in words) {
        msg <- fmsg("arXiv pages for:", word)
        url <- furl("https://arxiv.org/pdf/", word)
        trybrowseURL(url, msgT = msg)
    }
}

#' @export
#' @rdname h_engine
h_baidu <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Baidu results for:", words)
    url <- furl("https://www.baidu.com/s?ie=utf-8&wd=", words)
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_blackle <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Blackle results for:", words)
    url <- furl("http://www.blackle.com/results/?q=", words)
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_bing <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Bing results for:", words)
    url <- furl("https://www.bing.com/search?q=", words)
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_biorxiv <- function(..., char = NULL) {
    funTF <- function(TF, char) ifelse(TF, sprintf("%.4f", char), sprintf("%.5f", char))
    if (tryCatch(is.numeric(...),  condition = function(cond) FALSE)) char <- (...)
    if (tryCatch(is.numeric(char), condition = function(cond) FALSE)) {
        TF   <- (char %/% 1) < 1501
        char <- funTF(TF, char)
    }
    words <- if (is.null(char)) cnscinfun() else char
    for (word in words) {
        msg <- fmsg("bioRxiv pages for:", word)
        url <- furl("https://www.biorxiv.org/content/10.1101/", word)
        trybrowseURL(url, msgT = msg)
    }
}

#' @export
#' @rdname h_engine
h_biorxivpdf <- function(..., char = NULL) {
    funTF <- function(TF, char) ifelse(TF, sprintf("%.4f", char), sprintf("%.5f", char))
    if (tryCatch(is.numeric(...),  condition = function(cond) FALSE)) char <- (...)
    if (tryCatch(is.numeric(char), condition = function(cond) FALSE)) {
        TF   <- (char %/% 1) < 1501
        char <- funTF(TF, char)
    }
    words <- if (is.null(char)) cnscinfun() else char
    for (word in words) {
        msg <- fmsg("bioRxiv pages for:", words)
        url <- furl("https://www.biorxiv.org/content/", word, word2 = ".full.pdf")
        trybrowseURL(url, msgT = msg)
    }
}

#' @export
#' @rdname h_engine
h_bmap <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Bing results for:", words)
    url <- furl("https://www.bing.com/maps?q=", words, word2 = "&FORM=HDRSC4", coll = " ")
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_boursorama <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Boursorama results for:", words)
    url <- furl("https://www.boursorama.com/cours/", words)
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_cnrtl <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Resultats CNRTL pour :", words)
    url <- furl("http://www.cnrtl.fr/definition/", words, coll = "-")
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_collins <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Collins Dictionary results for:", words)
    url <- furl("https://www.collinsdictionary.com/spellcheck/english?q=", words)
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_cpan <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    if (words== "") {
        msg <- fmsg("Open CPAN:", "")
        url <- furl("https://www.cpan.org", "")
        trybrowseURL(url, msgT = msg)
    } else {
        msg <- fmsg("metaCPAN results for:", words)
        url <- furl("https://metacpan.org/search?&q=", words)
        trybrowseURL(url, msgT = msg)
    }
}

#' @export
#' @rdname h_engine
h_crossref <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Crossref results for:", words)
    url <- furl("http://search.crossref.org/?q=", words)
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_ctan <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("CTAN results for:", words)
    url <- furl("https://www.ctan.org/search?phrase=", words)
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_daum <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Daum results for:", words)
    url <- furl("https://search.daum.net/search?w=tot&q=", words)
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_ddg <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("DuckDuckGo results for:", words)
    url <- furl("https://duckduckgo.com/?q=", words)
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_dm <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Daily Motion results for:", words)
    url <- furl("https://www.dailymotion.com/search/", words, coll = " ")
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_doi <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("DOI redirection for:", words)
    url <- furl("https://doi.org/", words)
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_ecosia <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Ecosia results for:", words)
    url <- furl("https://www.ecosia.org/search?q=", words)
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_egerin <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Egerin results for:", words)
    url <- furl("http://egerin.com/user/searchresult?query=", words)
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_estrep <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Resultats Est Republicain pour:", words)
    url <- furl("https://www.estrepublicain.fr/search?q=", words, word2 = "&x=1&y=1")
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_evene <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Resultats pour les citations Evene :", words)
    url <- furl("http://evene.lefigaro.fr/citations/mot.php?mot=", words)
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_exalead <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Exalead results for:", words)
    url <- furl("http://www.exalead.com/search/web/results/?q", words)
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_excite <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Excite results for:", words)
    url <- furl("http://msxml.excite.com/search/web?q=", words)
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_framabee <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Excite results for:", words)
    url <- furl("https://framabee.org/?q=", words, , word2 = "&categories=general&language=fr", encode = TRUE)
    trybrowseURL(url, msgT = msg)
}


#' @export
#' @rdname h_engine
h_gigablast <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Gigablast results for:", words)
    url <- furl("https://www.gigablast.com/search?q=", words)
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_github <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("GitHub results for:", words)
    url <- furl("https://github.com/search?q=", words)
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_gitlab <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("GitLab results for:", words)
    url <- furl("https://gitlab.com/search?search=", words)
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_gmap <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Google Map results for:", words)
    url <- furl("https://www.google.com/maps/place/", words)
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_google <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Google results for:", words)
    url <- furl("https://www.google.com/search?q=", words)
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_gscholar <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Google Scholar results for:", words)
    url <- furl("https://scholar.google.com/scholar?q=", words)
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_ianaTLD <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    if (is.null(words)) {
        msg <- fmsg("Open IANA TLD database in browser")
        url <- furl("https://www.iana.org/domains/root/db")
        trybrowseURL(url, msgT = msg)
    } else {
        msg <- fmsg("IANA results for:", tolower(words))
        url <- furl("https://www.iana.org/domains/root/db/", tolower(words), word2 = ".html")
        trybrowseURL(url, msgT = msg)
    }
}

#' @export
#' @rdname h_engine
h_ianaWHOIS <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    if (is.null(words)) {
        msg <- fmsg("Open IANA WHOIS service in browser")
        url <- furl("https://www.iana.org/whois")
        trybrowseURL(url, msgT = msg)
    } else {
        msg <- fmsg("IANA WHOIS results for:", words)
        url <- furl("https://www.iana.org/whois?q=", words)
        trybrowseURL(url, msgT = msg)
    }
}

#' @export
#' @rdname h_engine
h_info <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Info results for:", words)
    url <- furl("http://www.info.com/serp?q=", words)
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_ixquick <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Startpage-Ixquick results for:", words)
    url <- furl("https://www.startpage.com/do/search?query=", words)
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_khoj <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Khoj results for:", words)
    url <- furl("http://khoj.org/search?cx=partner-pub-5005186563660475%3A3559433543&cof=FORID%3A10&ie=UTF-8&q=", words)
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_lesechos <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Les Echos results for:", words)
    url <- furl("https://recherche.lesechos.fr/recherche.php?exec=1&texte=", words)
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_lilo <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Lilo results for:", words)
    url <- furl("https://search.lilo.org/searchweb.php?q=", words, coll = " ")
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_lt <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("La Tribune results for:", words)
    url <- furl("https://www.latribune.fr/recherche.html?q=", words)
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_lycos <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Lycos results for:", words)
    url <- furl("http://search1.lycos.com/web/?q=", words)
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_mappy <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Mappy results for:", words)
    url <- furl("https://fr.mappy.com/#/1/M2/TSearch/S", words)
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_mw <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Merriam-Webster results for:", words)
    url <- furl("https://www.merriam-webster.com/dictionary/", words, coll = " ")
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_nate <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Nate results for:", words)
    url <- furl("https://search.daum.net/nate?thr=sbma&w=tot&q=", words)
}

#' @export
#' @rdname h_engine
h_naver <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Nabble results for:", words)
    url <- furl("https://search.naver.com/search.naver?where=nexearch&sm=top_hty&fbm=1&ie=utf8&query=", words)
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_orcid<- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Orcid ID results for:", words)
    url <- furl("https://orcid.org/orcid-search/quick-search/?searchQuery=", words)
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_osm <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Open Street Map results for:", words)
    url <- furl("https://www.openstreetmap.org/search?query=", words)
}

#' @export
#' @rdname h_engine
h_osmn <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Open Street Map results for:", words)
    url <- furl("https://nominatim.openstreetmap.org/search.php?q=", words,
                          word2 = "&polygon_geojson=1&viewbox=")
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_parsijoo <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Parsijoo results for:", words)
    url <- furl("http://parsijoo.ir/web?q=", words)
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_peertube <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("PeerTube results for:", words)
    url <- furl("https://www.peertube.fr/search?search=", words, coll = " ")
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_peru <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Peru results for:", words)
    url <- furl("http://peru.buscamas.pe/", words)
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_pipilika <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Pipilika results for:", words)
    url <- furl("https://www.pipilika.com/search?q=", words)
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_qwant <- function(..., char = NULL, lang = "en") {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Qwant results for:", words)
    url <- furl("https://www.qwant.com/?q=", words, word2= "&l=", word3 = lang)
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_qwfr <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Resultats Qwant pour :", words)
    url <- furl("https://www.qwant.com/?q=", words, word2 = "&l=fr")
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_reverso_d <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Reverso results for:", words)
    url <- furl("https://dictionnaire.reverso.net/anglais-definition/", words)
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_sapo <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Sapo results for:", words)
    url <- furl("https://www.sapo.pt/pesquisa?q=", words)
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_searx <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Searx results for:", words)
    url <- furl("https://searx.me/?q=", words)
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_so <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Stackoverflow results for:", words)
    url <- furl("https://stackoverflow.com/search?q=", words)
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_sogou <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Sogou results for:", words)
    url <- furl("https://www.sogou.com/web?query=", words)
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_ssrn <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    for (word in words) {
        msg <- fmsg("SSRN page(s) for:", word)
        url <- furl("https://ssrn.com/abstract=", word)
        trybrowseURL(url, msgT = msg)
    }
}

#' @export
#' @rdname h_engine
h_ssrnauth <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    for (word in words) {
        msg <- fmsg("SSRN Author page(s) for:", word)
        url <- furl("http://papers.ssrn.com/sol3/cf_dev/AbsByAuth.cfm?per_id=", word)
        trybrowseURL(url, msgT = msg)
    }
}

#' @export
#' @rdname h_engine
h_startpage <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Startpage-Ixquick results for:", words)
    url <- furl("https://www.startpage.com/do/search?query=", words)
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_twfr <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Twitter results for:", words)
    url <- furl("https://twitter.com/search?q=", words, word2 = "&lang=fr")
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_twitter <- function(..., char = NULL, lang = "en") {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Twitter results for:", words)
    url <- furl("https://twitter.com/search?q=", words, word2 = "&lang=", word3 = lang)
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_un <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("L'Usine Nouvelle results for:", words)
    url <- furl("https://www.usinenouvelle.com/recherche=", words)
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_verbes <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Conjugaison Les Verbes :", words)
    url <- furl("http://www.les-verbes.com/conjuguer.php?verbe=", words, word2 = "&submit.x=0&submit.y=0")
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_via <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("ViaMichelin results for:", words)
    url <- furl("https://www.viamichelin.com/web/Maps?address=", words)
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_vimeo <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Vimeo results for:", words)
    url <- furl("https://vimeo.com/search?q=", words)
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_wego <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Wego/Here results for:", words)
    url <- furl("https://wego.here.com/search/", tolower(words), coll = "-")
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_wp <- function(..., char = NULL, lang = "en") {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Wikipedia results for:", words)
    address <- paste0("https://", lang, ".wikipedia.org/w/index.php?search=")
    url <- furl(address, words)
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_wpfr <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Resultats Wikipedia pour : ", words)
    url <- furl("https://fr.wikipedia.org/w/index.php?search=", words)
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_yahoo <- function(..., char = NULL, lang = "en") {
    words <- if (is.null(char)) cnscinfun() else char
    msg   <- fmsg("Yahoo results for: ", words)
    if (lang == "en") {
        url <- furl("https://search.yahoo.com/search?p=", words)
        trybrowseURL(url, msgT = msg)
    } else {
        address <- paste0("https://", lang, ".search.yahoo.com/search?p=")
        url <- furl(address, words)
        trybrowseURL(url, msgT = msg)
    }
}

#' @export
#' @rdname h_engine
h_yahoofin <- function(..., char = NULL, lang = "en") {
    words <- if (is.null(char)) cnscinfun() else char
    msg   <- fmsg("Yahoo Finance results for: ", words)
    if (lang == "en") {
        address <- paste0("https://finance.yahoo.com/quote/", words, "?p=")
        url <- furl(address, words)
        trybrowseURL(url, msgT = msg)
    } else {
        address <- paste0("https://", lang,
                          ".finance.yahoo.com/quote/", words, "?p=")
        url <- furl(address, words)
        trybrowseURL(url, msgT = msg)
    }
}

#' @export
#' @rdname h_engine
h_yandex <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Yandex results for:", words)
    url <- furl("https://www.yandex.com/search/?text=", words)
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_yooz <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Yooz results for:", words)
    url <- furl("https://yooz.ir/search/?q=", words)
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_yt <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    msg <- fmsg("Youtube results for:", words)
    url <- furl("https://www.youtube.com/results?search_query=", words)
    trybrowseURL(url, msgT = msg)
}

#' @export
#' @rdname h_engine
h_zbib <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    if (is.null(words)) {
        msg <- fmsg("Open ZoteroBib")
        url <- furl("https://zbib.org")
        trybrowseURL(url, msgT = msg)
    } else {
        msg <- fmsg("Open ZoteroBib for:", words)
        url <- furl("https://zbib.org/", words)
        trybrowseURL(url, msgT = msg)
    }
}



