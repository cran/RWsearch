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
utils::browseURL(url)
}




#' @title Explore the Web with Various Search Engines
#' @description 
#' Launch the default browser and search in: 1bis Map (BottinCarto), ABC Bourse (short 
#' stock names), arXiv (vectorized), Ask, Baidu, Blackle, Bing, Bing Map (bmap), 
#' Boursorama (short stocknames), CNRTL (French dictionary), 
#' Collins English Dictionary, CPAN and metaCPAN (Perl), 
#' Crossref (DOI and bibliographic metadata), CTAN (Latex), Daum, DailyMotion (dm), 
#' DOI, DuckDuckGo (ddg), Ecosia, Egerin, Evene (citations), Exalead, Excite, 
#' Gigablast, GitHub, GitLab, Google Map (gmap), Google, Google Scholar (gscholar), Info, 
#' Khoj, Les Echos, La Tribune (lt), Lilo, Lycos, Mappy Map, Merriam-Webster (mw, English dictionary), 
#' Nabble, Nate, Naver (see N2H4 package), Orcid, Open Street Map, OSM Nominatim, 
#' Parsijoo, PeerTube, Peru, Pipilika, 
#' Qwant (qw + qwfr), R-bloggers, Rdocumentation (rdoc), Rdocumentation task views (rdoctv), 
#' Rdrr, Reverso dictionnary, Rseek, Sapo, Searx, Sogou, SSRN and SSRN Author (vectorized),
#' Stackoverflow (so), Startpage (ex-Ixquick), Twitter (+ twfr), 
#' L'Usine Nouvelle (un), ViaMichelin Map and Routes (via), Les Verbes, Vimeo, Wego (Here maps), 
#' Wikipedia (wp + wpfr), Yahoo, Yahoo Finance, Yandex, Yooz, Youtube (yt). 
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
#' \donttest{
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
h_1bis <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("1bis results for:", words)
    fbr("http://maps.bottincarto.com/1bis/map/map.asp?&scale=200000&city=", words, encode = TRUE)
}
## Ne marche pas.

#' @export
#' @rdname h_engine
h_abcbourse <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("ABC Bourse results for:", words)
    fbr("https://www.abcbourse.com/graphes/display.aspx?s=", words)
}

#' @export
#' @rdname h_engine
h_ask <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Bing results for:", words)
    fbr("https://www.ask.com/web?q=", words)
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
    fme("arXiv pages for:", words)
    for (word in words) fbr("https://arxiv.org/abs/", word)
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
    fme("arXiv pages for:", words)
    for (word in words) fbr("https://arxiv.org/pdf/", word)
}

#' @export
#' @rdname h_engine
h_baidu <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Baidu results for:", words)
    fbr("https://www.baidu.com/s?ie=utf-8&wd=", words)
}

#' @export
#' @rdname h_engine
h_blackle <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Blackle results for:", words)
    fbr("http://www.blackle.com/results/?q=", words)
}

#' @export
#' @rdname h_engine
h_bing <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Bing results for:", words)
    fbr("https://www.bing.com/search?q=", words)
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
    fme("bioRxiv pages for:", words)
    for (word in words) fbr("https://www.biorxiv.org/content/10.1101/", word)
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
    fme("bioRxiv pages for:", words)
    for (word in words) fbr("https://www.biorxiv.org/content/", word, word2 = ".full.pdf")
}

#' @export
#' @rdname h_engine
h_bmap <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Bing results for:", words)
    fbr("https://www.bing.com/maps?q=", words, word2 = "&FORM=HDRSC4", coll = " ")
}

#' @export
#' @rdname h_engine
h_boursorama <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Boursorama results for:", words)
    fbr("https://www.boursorama.com/cours/", words)
}

#' @export
#' @rdname h_engine
h_cnrtl <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Resultats CNRTL pour :", words)
    fbr("http://www.cnrtl.fr/definition/", words, coll = "-")
}

#' @export
#' @rdname h_engine
h_collins <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Collins Dictionary results for:", words)
    fbr("https://www.collinsdictionary.com/spellcheck/english?q=", words)
}

#' @export
#' @rdname h_engine
h_cpan <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
	if (words== "") {
		fme("Open CPAN:", "")
		fbr("https://www.cpan.org", "")
	} else {
		fme("metaCPAN results for:", words)
		fbr("https://metacpan.org/search?&q=", words)
	}
}

#' @export
#' @rdname h_engine
h_crossref <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Crossref results for:", words)
    fbr("http://search.crossref.org/?q=", words)
}

#' @export
#' @rdname h_engine
h_ctan <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("CTAN results for:", words)
    fbr("https://www.ctan.org/search?phrase=", words)
}

#' @export
#' @rdname h_engine
h_daum <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Daum results for:", words)
    fbr("https://search.daum.net/search?w=tot&q=", words)
}

#' @export
#' @rdname h_engine
h_ddg <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("DuckDuckGo results for:", words)
    fbr("https://duckduckgo.com/?q=", words)
}

#' @export
#' @rdname h_engine
h_dm <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Daily Motion results for:", words)
    fbr("https://www.dailymotion.com/search/", words, coll = " ")
}

#' @export
#' @rdname h_engine
h_doi <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("DOI redirection for:", words)
    fbr("https://doi.org/", words)
}

#' @export
#' @rdname h_engine
h_ecosia <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Ecosia results for:", words)
    fbr("https://www.ecosia.org/search?q=", words)
}

#' @export
#' @rdname h_engine
h_egerin <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Egerin results for:", words)
    fbr("http://egerin.com/user/searchresult?query=", words)
}

#' @export
#' @rdname h_engine
h_estrep <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Resultats Est Republicain pour:", words)
    fbr("https://www.estrepublicain.fr/search?q=", words, word2 = "&x=1&y=1")
}

#' @export
#' @rdname h_engine
h_evene <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Resultats pour les citations Evene :", words)
    fbr("http://evene.lefigaro.fr/citations/mot.php?mot=", words)
}

#' @export
#' @rdname h_engine
h_exalead <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Exalead results for:", words)
    fbr("http://www.exalead.com/search/web/results/?q", words)
}

#' @export
#' @rdname h_engine
h_excite <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Excite results for:", words)
    fbr("http://msxml.excite.com/search/web?q=", words)
}

#' @export
#' @rdname h_engine
h_framabee <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Excite results for:", words)
    fbr("https://framabee.org/?q=", words, , word2 = "&categories=general&language=fr", encode = TRUE)
}


#' @export
#' @rdname h_engine
h_gigablast <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Gigablast results for:", words)
    fbr("https://www.gigablast.com/search?q=", words)
}

#' @export
#' @rdname h_engine
h_github <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("GitHub results for:", words)
    fbr("https://github.com/search?q=", words)
}

#' @export
#' @rdname h_engine
h_gitlab <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("GitLab results for:", words)
    fbr("https://gitlab.com/search?search=", words)
}

#' @export
#' @rdname h_engine
h_gmap <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Google Map results for:", words)
    fbr("https://www.google.com/maps/place/", words)
}

#' @export
#' @rdname h_engine
h_google <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Google results for:", words)
    fbr("https://www.google.com/search?q=", words)
}

#' @export
#' @rdname h_engine
h_gscholar <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Google Scholar results for:", words)
    fbr("https://scholar.google.com/scholar?q=", words)
}

#' @export
#' @rdname h_engine
h_info <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Info results for:", words)
    fbr("http://www.info.com/serp?q=", words)
}

#' @export
#' @rdname h_engine
h_ixquick <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Startpage-Ixquick results for:", words)
    fbr("https://www.startpage.com/do/search?query=", words)
}

#' @export
#' @rdname h_engine
h_khoj <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Khoj results for:", words)
    fbr("http://khoj.org/search?cx=partner-pub-5005186563660475%3A3559433543&cof=FORID%3A10&ie=UTF-8&q=", words)
}

#' @export
#' @rdname h_engine
h_lesechos <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Les Echos results for:", words)
    fbr("https://recherche.lesechos.fr/recherche.php?exec=1&texte=", words)
}

#' @export
#' @rdname h_engine
h_lilo <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Lilo results for:", words)
    fbr("https://search.lilo.org/searchweb.php?q=", words, coll = " ")
}

#' @export
#' @rdname h_engine
h_lt <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("La Tribune results for:", words)
    fbr("https://www.latribune.fr/recherche.html?q=", words)
}

#' @export
#' @rdname h_engine
h_lycos <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Lycos results for:", words)
    fbr("http://search1.lycos.com/web/?q=", words)
}

#' @export
#' @rdname h_engine
h_mappy <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Mappy results for:", words)
    fbr("https://fr.mappy.com/#/1/M2/TSearch/S", words)
}

#' @export
#' @rdname h_engine
h_mw <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Merriam-Webster results for:", words)
    fbr("https://www.merriam-webster.com/dictionary/", words, coll = " ")
}

#' @export
#' @rdname h_engine
h_nate <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Nate results for:", words)
    fbr("https://search.daum.net/nate?thr=sbma&w=tot&q=", words)
}

#' @export
#' @rdname h_engine
h_naver <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Nabble results for:", words)
    fbr("https://search.naver.com/search.naver?where=nexearch&sm=top_hty&fbm=1&ie=utf8&query=", words)
}

#' @export
#' @rdname h_engine
h_orcid<- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Orcid ID results for:", words)
    fbr("https://orcid.org/orcid-search/quick-search/?searchQuery=", words)
}

#' @export
#' @rdname h_engine
h_osm <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Open Street Map results for:", words)
    fbr("https://www.openstreetmap.org/search?query=", words)
}

#' @export
#' @rdname h_engine
h_osmn <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Open Street Map results for:", words)
    fbr("https://nominatim.openstreetmap.org/search.php?q=", words, 
                          word2 = "&polygon_geojson=1&viewbox=")
}

#' @export
#' @rdname h_engine
h_parsijoo <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Parsijoo results for:", words)
    fbr("http://parsijoo.ir/web?q=", words)
}

#' @export
#' @rdname h_engine
h_peertube <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("PeerTube results for:", words)
    fbr("https://www.peertube.fr/search?search=", words, coll = " ")
}

#' @export
#' @rdname h_engine
h_peru <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Peru results for:", words)
    fbr("http://peru.buscamas.pe/", words)
}

#' @export
#' @rdname h_engine
h_pipilika <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Pipilika results for:", words)
    fbr("https://www.pipilika.com/search?q=", words)
}

#' @export
#' @rdname h_engine
h_qwant <- function(..., char = NULL, lang = "en") {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Qwant results for:", words)
    fbr("https://www.qwant.com/?q=", words, word2= "&l=", word3 = lang)
}

#' @export
#' @rdname h_engine
h_qwfr <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Resultats Qwant pour :", words)
    fbr("https://www.qwant.com/?q=", words, word2 = "&l=fr")
}

#' @export
#' @rdname h_engine
h_reverso_d <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Reverso results for:", words)
    fbr("https://dictionnaire.reverso.net/anglais-definition/", words)
}

#' @export
#' @rdname h_engine
h_sapo <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Sapo results for:", words)
    fbr("https://www.sapo.pt/pesquisa?q=", words)
}

#' @export
#' @rdname h_engine
h_searx <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Searx results for:", words)
    fbr("https://searx.me/?q=", words)
}

#' @export
#' @rdname h_engine
h_so <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Stackoverflow results for:", words)
    fbr("https://stackoverflow.com/search?q=", words)
}

#' @export
#' @rdname h_engine
h_sogou <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Sogou results for:", words)
    fbr("https://www.sogou.com/web?query=", words)
}

#' @export
#' @rdname h_engine
h_ssrn <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("SSRN page(s) for:", words)
    for (word in words) fbr("https://ssrn.com/abstract=", word)
}

#' @export
#' @rdname h_engine
h_ssrnauth <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("SSRN Author page(s) for:", words)
    for (word in words) fbr("http://papers.ssrn.com/sol3/cf_dev/AbsByAuth.cfm?per_id=", word)
}

#' @export
#' @rdname h_engine
h_startpage <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Startpage-Ixquick results for:", words)
    fbr("https://www.startpage.com/do/search?query=", words)
}

#' @export
#' @rdname h_engine
h_twfr <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Twitter results for:", words)
    fbr("https://twitter.com/search?q=", words, word2 = "&lang=fr")
}

#' @export
#' @rdname h_engine
h_twitter <- function(..., char = NULL, lang = "en") {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Twitter results for:", words)
    fbr("https://twitter.com/search?q=", words, word2 = "&lang=", word3 = lang)
}

#' @export
#' @rdname h_engine
h_un <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("L'Usine Nouvelle results for:", words)
    fbr("https://www.usinenouvelle.com/recherche=", words)
}

#' @export
#' @rdname h_engine
h_verbes <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Conjugaison Les Verbes :", words)
    fbr("http://www.les-verbes.com/conjuguer.php?verbe=", words, word2 = "&submit.x=0&submit.y=0")
}

#' @export
#' @rdname h_engine
h_via <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("ViaMichelin results for:", words)
    fbr("https://www.viamichelin.com/web/Maps?address=", words)
}

#' @export
#' @rdname h_engine
h_vimeo <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Vimeo results for:", words)
    fbr("https://vimeo.com/search?q=", words)
}

#' @export
#' @rdname h_engine
h_wego <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Wego/Here results for:", words)
    fbr("https://wego.here.com/search/", tolower(words), coll = "-")
}

#' @export
#' @rdname h_engine
h_wp <- function(..., char = NULL, lang = "en") {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Wikipedia results for:", words)
    address <- paste0("https://", lang, ".wikipedia.org/w/index.php?search=")
    fbr(address, words)
}

#' @export
#' @rdname h_engine
h_wpfr <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Resultats Wikipedia pour : ", words)
    fbr("https://fr.wikipedia.org/w/index.php?search=", words)
}

#' @export
#' @rdname h_engine
h_yahoo <- function(..., char = NULL, lang = "en") {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Yahoo results for: ", words)
    if (lang == "en") {
        fbr("https://search.yahoo.com/search?p=", words)
    } else {
        address <- paste0("https://", lang, ".search.yahoo.com/search?p=")
        fbr(address, words)
    }
}

#' @export
#' @rdname h_engine
h_yahoofin <- function(..., char = NULL, lang = "en") {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Yahoo Finance results for: ", words)
    if (lang == "en") {
        address <- paste0("https://finance.yahoo.com/quote/", words, "?p=", words)
        fbr(address)
    } else {
        address <- paste0("https://", lang, ".finance.yahoo.com/quote/", words, "?p=", words)
        fbr(address)
    }
}

#' @export
#' @rdname h_engine
h_yandex <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Yandex results for:", words)
    fbr("https://www.yandex.com/search/?text=", words)
}

#' @export
#' @rdname h_engine
h_yooz <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Yooz results for:", words)
    fbr("https://yooz.ir/search/?q=", words)
}

#' @export
#' @rdname h_engine
h_yt <- function(..., char = NULL) {
    words <- if (is.null(char)) cnscinfun() else char
    fme("Youtube results for:", words)
    fbr("https://www.youtube.com/results?search_query=", words)
}



