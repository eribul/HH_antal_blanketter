
library(dplyr)

##########################################################################################
#                                                                                        #
#                Gör lokala anpassningar då skriptet inte körs från INCA                 #
#                                                                                        #
##########################################################################################

is.inca <- function() unname(Sys.info()["nodename"] == "EXT-R27-PROD")

if (!is.inca()) {
  setwd("~/Documents/huvud_hals/atal_blanketter")
  rm(list = setdiff(ls(), "is.inca"))
  load("data/df.rda")
   param <- list(
      year_from     = "2008",
      year_to       = "2014",
      Diagnos       = c("Läpp",  "Munhåla",  "Oropharynx",  "Nasopharynx",  "Hypopharynx",  "Näsa/bihåla",
                        "Larynx",  "Spottkörtlar",  "Ofullständigt angiven",  "Mal lgl på hals"),
      # Svarar på frågan "Visa uppgifter för:"
      urval         = c("hela riket (per region)") # Kan också vara "patienter anmälda av min klinik"
  )
}



##########################################################################################
#                                                                                        #
#                                Allmänna hjälpfunktioner                                #
#                                                                                        #
##########################################################################################


not_blank <- function(x) !is.na(x) & x != ""

# Jämför inloggad enhet mot utvald enhet
compare_unit <- function(sjh, klk = NULL, df) {
    compare_sjh <- df$userparentunitcode == df[[sjh]]
    if (!is.null(klk)) {
        compare_sjh & df$userunitcode == df[[klk]]
    } else{
        compare_sjh
    }
}


# Justera region-benämningen för att särredovisa den egna kliniken
# df = data.frame med userpos-data
# sjh = textsträng med namn på sjukhuskolumn i df
# klk = textsträng med namn på klinik i df
adjust_region <- function(df, sjh, klk) {
    ifelse(compare_unit(sjh, klk, df),
           "Din klinik",
           ifelse(df$region_namn == df$userregionname,
                  paste(df$region_namn, "(exkl. din klinik)"),
                  df$region_namn))
}





##########################################################################################
#                                                                                        #
#                                Snygga till df och param                                #
#                                                                                        #
##########################################################################################

# Snygga till df
names(df)             <- tolower(names(df))
isfac                 <- sapply(df, is.factor)
df[isfac]             <- lapply(df[isfac], as.character)

# Fixa till param
param$year_from       <- as.numeric(param$year_from)
param$year_to         <- as.numeric(param$year_to)

# Om vi bara ska betrakta data för den egna kliniken
if (param$urval == "patienter anmälda av min klinik") {
    df <- df[compare_unit("a_anmsjh", "a_anmkli", df), ]
    df$region_namn <- paste( unique(df$userparentunitname), unique(df$userunitname), sep = " - ")
}



# Grundläggande variabelförändringar
df <- df %>%
    mutate(
        a_diadat_ar   = substr(as.character(a_diadat), 1, 4),
        region_namn   = gsub("Region ", "", region_namn),
        a_icd10_grupp = gsub("[[:digit:]][[:space:]]", "", df$a_icd10_grupp)
    ) %>%
    # Fallen måste vara diagnostiserade fr o m 2009 t o m pågående år
    filter(
        region_namn   != "Demo",
        a_diadat_ar   %in% param$year_from:param$year_to,
        a_icd10_värde != "00",
        a_icd10_grupp %in% param$Diagnos | param$Diagnos == "Alla")



# Kortare label om alla diagnoser valda
alla_diagnoser        <- c("Läpp",  "Munhåla",  "Oropharynx",  "Nasopharynx",  "Hypopharynx",  "Näsa/bihåla",
                           "Larynx",  "Spottkörtlar",  "Ofullständigt angiven",  "Mal lgl på hals")
if (all(alla_diagnoser %in% param$Diagnos)) param$Diagnos <- "Alla"





##########################################################################################
#                                                                                        #
#                          Funktioner för att skapa JSON-objekt                          #
#                                                                                        #
##########################################################################################

# Skapa titel i Json-format
# js_name = namnet på variabeln som kommer användas i json
# txt = texten som json-variabeln ska innehålla
js_title <- function(txt, js_name) paste0("var title_", js_name, " = ", "'", txt, "'", ";")


# Skapa kategorivariabel i.e. år till JS format
# x = vektor att ta nivåer ifrån
# js_name = namnet på den variabel som skapas
js_category <- function(x, js_name) {
    x <- paste(levels(as.factor(x)), collapse = "','")
    paste0("var category_", js_name, " = ['", x, "'];" )
}



# Exportera datastrukturen till json-format
# x = tabell med rader = region och kolumner = diagnosår
# där varje cell är ett antal eller en andel
js_serie <- function(x, js_name) {
    if (nrow(x) != 0) {
        tbl <- as.matrix(x)
        data <- data.frame(name = rownames(tbl),
                           data = paste0("[", apply(tbl, 1, paste, collapse = ","), "]"),
                           stringsAsFactors = FALSE)
        data <- jsonlite::toJSON(data)
    } else {
        data <- "[]"
    }
    data <- paste0("var ser_", js_name, " = ", data, ";")
    data <- gsub('\\"\\[', '[', data)
    data <- gsub('\\]\\"', ']', data)
    data
}



# Skapa objekt med information i JSON-format
# df1 = data.frame (som måste innehålla vissa variabler)
# df2 = ev ytterligare data.frame som i så fall utgör nämnare
#       vid beräkning av tck
# region = namn på regionvariabel
# year = namn på årsvariabel (kan vara datum som konverteras)
# js_name = namn på indikatorn som kommer användas i JSON
js_object <- function(df1, df2 = NULL, js_name, region = "region_namn", year = "a_diadat_ar", main = "") {

    # hjälpfunktion för att skapa namn på elementen i objektslistan
    add_jsname <- function(prefix, comma = TRUE) paste0(prefix, "_", js_name, if (comma) ", ")

    # Hjälpfunktion för att rensa bort fall med saknat år
    table_df <- function(x) {
        x[[year]] <- substr(as.character(x[[year]]), 1, 4)
        x <- filter(x, not_blank(x[[year]]))
        table(x[[region]], x[[year]])
    }

    # Ska vi skapa antal eller tck
    js_fun <- if (is.null(df2)) "Stapeldiagram" else "Tckdiagram"

    # SKapa en textsträng med funktionsanrop som tolkas av js
    fun_s <- paste0(js_fun, "('", add_jsname("container", FALSE), "', ", add_jsname("title"),
                    "subtitle_urval, ", add_jsname("category"), "x_axis_diaar, y_axis_antal, ",
                    add_jsname("ser", FALSE), ");")


    df <- if (is.null(df2)) {
            table_df(df1)
        } else {
            apply(round(table_df(merge(df1, df2)) / table_df(df2) * 100, 0), 1:2, min, 100)
        }

    list(
        serie         = js_serie(df, js_name),
        categories    = js_category(colnames(df), js_name),
        title         = js_title(main, js_name),
        function_call = fun_s
    )
}


##########################################################################################
#                                                                                        #
#                                 Gemensamma textobjekt                                  #
#                                                                                        #
##########################################################################################

# Gemensamma textobjekt för antalsgrafer
subtitle_urval_text <-
    paste0(
        "(Urval: ",
        with(param, paste0(
            "Dianosår: [", year_from, "-", year_to, "]. ",
            "Diagnoser: [", paste(Diagnos, collapse = ", "), "].")
        ),")")

general_labels <- list(
    subtitle_urval =
        paste0( "var subtitle_urval = ", "'", subtitle_urval_text, "'", ";"),
    y_axis_antal  = "var y_axis_antal = 'Antal blanketter';",
    x_axis_diaar  = "var x_axis_diaar = 'Diagnosår';"
)


##########################################################################################
#                                                                                        #
#                                Stapeldiagram för antal                                 #
#                                                                                        #
##########################################################################################

# Antal anmälningar
ant_anm_df  <- df %>%
    filter(
        not_blank(a_anmkli) |
        not_blank(a_lakare) |
        not_blank(a_anmsjh) |
        not_blank(a_inrappenhet) |
        not_blank(a_initav)
    )

ant_anm <- ant_anm_df %>%
    js_object(js_name = "ant_anm", main = "Antal anmälningsblanketter")


# Antal kirurgiblanketter - blanketten avser kirurgi om ngn av de tre variablerna
# ifyllda
ant_kir_df  <-
    df %>%
    filter(
        not_blank(b_behenlplankirhals_beskrivning) |
        not_blank(b_behenlplankirprim_beskrivning) |
        not_blank(b_op1sjh))

ant_kir <- ant_kir_df %>%
    js_object(
        js_name =  "ant_kir",
        main = "Antal kirurgiblanketter")


# Antal onkologiska blanketter
ant_onk_df  <-
    df %>%
    # Behandlingsblanketten avser onkologisk behandling om ngn av följande uppgifter finns
    filter(
        not_blank(b_stralsjh) |
        not_blank(b_behenlplanbrachy_beskrivning) |
        not_blank(b_behenlplanextstr_beskrivning) |
        not_blank(b_behenlplanmedtumb_beskrivning) |
        not_blank(b_stralstart) |
        not_blank(b_brachystart) |
        not_blank(b_behmed) |
        not_blank(b_behingen)
        )
ant_onk <- ant_onk_df %>%
    js_object(
        js_name = "ant_onk",
        main = "Antal onkologiblanketter")



# Antal uppföljningar
ant_uppf_df <- filter(df,
                      not_blank(u_rappdatuppf) |
                      not_blank(u_uppfsjh) |
                      not_blank(u_ctrlslutdat) |
                      not_blank(u_uppfsjhkli))

ant_uppf <- js_object(ant_uppf_df, js_name = "ant_uppf", main = "Antal uppföljningsblanketter")



##########################################################################################
#                                                                                        #
#                            Andelsdiagram för täckningsgrad                             #
#                                                                                        #
##########################################################################################

# Kirurgiblanketter
tck_kir  <- js_object(df1 = ant_kir_df,
                      df2 =
                          filter(df,
                            a_bebehkirha_beskrivning == "Ja" |
                            a_bebehkirpri_beskrivning == "Ja"),
                      js_name = "tck_kir",
                      main = "Intern täckningsgrad för kirurgiblanketter")

# Onkologiblanketter
tck_onk  <- js_object(df1 = ant_onk_df,
                      df2 =
                          filter(df,
                                 a_bebehbrac_beskrivning == "Ja" |
                                 a_bebehext_beskrivning == "Ja" |
                                 a_bebehann_beskrivning == "Ja" |
                                 a_bebehmed_beskrivning == "Ja"),
                      js_name = "tck_onk",
                      main = "Intern täckningsgrad för onkologiblanketten")

# Uppföljningsblanketter
tck_uppf <- js_object(df1 = ant_uppf_df,
                      df2 = df,
                      js_name = "tck_uppf",
                      main = "Intern täckningsgrad för uppföljningsblanketter")




##########################################################################################
#                                                                                        #
#                                  Generera output.html                                  #
#                                                                                        #
##########################################################################################

# Create file "output.html with text från ...
# ... = texts
create_output <- function(...) {

    # Ladda in del1 av HTML-filen
    file <- "prefix.html"
    if (is.inca()) file <- paste0("D:/R-Scripts/Väst/oc5buer/huvud-_och_halscancer/antalblanketter/", file)
    del1 <- scan(file, what = "", sep = "\n", quiet = TRUE, fileEncoding = "UTF-8")

    # Tydlig informationstext ifall bara klinikens patienter inkluderas
    .informationstext <- if (param$urval == "patienter anmälda av min klinik") {
        paste('<p style="color:red"><strong>',
              'Endast fall där kliniken rapporterat anmälan inkluderas!</strong><br>',
              'T ex: täckningsgrad för kirurgiblanketten avser andel tumörer',
              'där anmälan gjorts av den egna kliniken och där kirurgiblanketten sedan också registrerats',
              'av godtycklig enhet. </p>'
        )
    } else ""

    # Registrets namn (används i rubriker)
    registernamn     <- "Svenskt kvalitetsregister för huvud- och halscancer (SweHNCR)"
    subtitle         <- paste(registernamn, subtitle_urval_text, sep = "<br>")
    info_register    <- paste0("document.getElementById('register').innerHTML='", registernamn, "';")
    info_typ         <- "document.getElementById('typ av rapport').innerHTML='Antal blanketter och intern täckningsgrad';"
    info_info        <- paste0("document.getElementById('information').innerHTML='", subtitle, "';")
    informationstext <- paste0("document.getElementById('informationstext').innerHTML='", .informationstext, "';")

    file.create("output.html")
    outfile <- file("output.html","w", encoding = "UTF-8")
    on.exit(close(outfile))
    add_to_outfile <- function(txt) cat(paste("\n", txt),file = outfile, append = TRUE)

    # Lista med textobjekt som ska skrivas till output-filen
    txt_list <- list(del1, "<script>", general_labels, ..., info_register, info_typ, info_info,
                     informationstext, "</script> </body> </html>")
    lapply(txt_list, add_to_outfile)
}

# Skapa output-filen med de js-objekt vi skapat ovan
create_output(ant_anm, ant_kir, ant_onk, ant_uppf, tck_kir, tck_onk, tck_uppf)
