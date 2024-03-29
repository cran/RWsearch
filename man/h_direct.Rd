% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/h_direct.R
\name{h_direct}
\alias{h_direct}
\alias{h_academie}
\alias{h_deepl}
\alias{h_etz}
\alias{h_framasoft}
\alias{h_framasoft0}
\alias{h_googletranslate}
\alias{h_interglot}
\alias{h_lexilogos}
\alias{h_linguee}
\alias{h_meteoblue}
\alias{h_promt}
\alias{h_reverso}
\alias{h_systran}
\alias{h_tad}
\alias{h_tadsm}
\alias{h_windy}
\alias{h_yacy}
\title{Open a Web Page in the your browser}
\usage{
h_academie()

h_deepl()

h_etz()

h_framasoft()

h_framasoft0()

h_googletranslate()

h_interglot()

h_lexilogos()

h_linguee()

h_meteoblue()

h_promt()

h_reverso()

h_systran()

h_tad()

h_tadsm()

h_windy()

h_yacy()
}
\description{
\code{h_framasoft} gives access to several free web services (as in speech and in
half pint of beer) that are good alternatives to GAFA services.

\code{h_academie} is a bookmark to the Academie francaise's dictionary.

\code{h_lexilogos} gives access to hundreds of dictionaries in many languages.

\code{h_deepl}, \code{h_googletranslate}, \code{h_interglot}, \code{h_reverso},
\code{h_linguee}, \code{h_promt}, \code{h_reverso}, \code{h_systran} provide
translation engines. \code{h_linguee} returns examples with long sentences.

\code{h_yacy} is a decentralized peer-to-peer web search software.

\code{h_etz} is a bookmark to the EveryTimeZone website.

\code{h_tad} and \code{h_tadsm} are bookmarks to timeanddate.com, a website
dedicated to date and time conversion plus timezone management.

\code{h_meteoblue} and \code{h_windy} are bookmarks to the Meteoblue and Windy
meteo and weather websites.
}
\examples{
if (interactive()) {
h_linguee()
h_lexilogos()
}
}
