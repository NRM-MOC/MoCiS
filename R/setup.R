##' Process the raw data for analysis with \code{mocis}.
##'
##' For internal use only.
##' @title setup
##' @param d A data.frame.
##' @param file The file name. Taken from \code{mocis}.
##' @param vars Which variables should be analyzed.
##' @param add.vars Additional variables to include.
##' @param GRU For selection among birds.
##' @param locs Analyze only specific locales. Not used.
##' @param yearvar Variable containing the years.
##' @param nyears How many years should be present in the series before doing
##'     regression analysis. Taken from \code{mocis}.
##' @param locvar "LOC"
##' @param loc A vector of locales to subset the analysis on. Experimental.
##' @param agesel A list describing the age selection. Defaults to age_select().
##' @param co_met A list describing the metal conversion. Defaults to convert_met().
##' @param co_pcb A list describing the PCB conversion. Defaults to convert_pcb().
##' @param co_dde A list describing the DDE conversion. Defaults to convert_dde().
##' @param type Type of file.
##' @param srt Type of file.
##' @return A list with each locale as the elements. Each locale is a list with
##'     the species as the elements. Each species is a list with a
##'     \code{data.frame} containing each contaminant as the elements.
##' @author Erik Lampa
##' @export
setup <- function(d, file, vars = NULL, add.vars,
                GRU = GRU,
                locs = NULL, yearvar = yearvar, nyears = nyears,
                locvar = "LOC", loc = NULL, agesel = agesel,
                co_met = convert_met(), co_pcb = convert_pcb(),
                co_dde = convert_dde(), type, srt) {
    
    ## Setup the data set for analysis. Performs age selection and lab
    ## conversion

    ## Certain additional variables are needed for certain file types
    if (type %in% c("hav", "uria") & is.null(add.vars) & srt != "dx") {
        add.vars <- c("YWD", "LOC", "ACCNR", "TOTL", "TOTV", "ALDR",
                     "MTPRC", "LTPRC", "LVKT", "GENUS", "LOKAL", "SEX",
                     "FPRC", "NKOO", "EKOO", "GRU", "TPRC", "SKLV")
    }
    if (type == "hav" & is.null(add.vars) & srt == "dx") {
        add.vars <- c("YWD", "LOC", "ACCNR", "TOTL", "TOTV", "ALDR",
                      "MTPRC", "LTPRC", "LVKT", "GENUS", "LOKAL", "SEX",
                      "FPRC", "NKOO", "EKOO", "FPRCPP", "FPRCU",
                      "TPRC", "SKLV")
    }
    if (type == "uria" & is.null(add.vars) & srt == "dx") {
        add.vars <- c("YWD", "LOC", "ACCNR", "TOTL", "TOTV", "ALDR",
                      "GENUS", "LOKAL", "SEX",
                      "NKOO", "EKOO", "FPRCU")
    }

    if (type == "limn" & is.null(add.vars) & !srt %in% c("clc", "dx")) {
        add.vars <- c("YWD", "LOC", "ACCNR", "TOTL", "TOTV", "GENUS",
                      "NKOO", "EKOO", "LTPRC", "FPRC", "SKLV")
    }
    if (type == "limn" & is.null(add.vars) & srt %in% c("clc", "bfr")) {
        add.vars <- c("YWD", "LOC", "ACCNR", "TOTL", "TOTV", "GENUS",
                      "NKOO", "EKOO", "FPRC", "SKLV")
    }
    if (type == "limn" & is.null(add.vars) & srt == "dx") {
        add.vars <- c("YWD", "LOC", "ACCNR", "TOTL", "TOTV", "GENUS",
                      "NKOO", "EKOO", "FPRC", "FPRCPP", "FPRCU", "SKLV")
    }

    if (is.null(vars)) stop("Specify at least one variable.",
                            call. = FALSE)
    ## If duplicates are present in the vars vector, remove duplicates
    dup <- duplicated(vars)
    if (length(dup[dup == TRUE]) >= 1) {
        vars <- vars[!dup]
    }
    ## If a subset of locales is desired
    if (!is.null(loc)) {
        d <- subset(d, locvar %in% locs)
    }
    ## For birds, select based on GRU
    GRU.arg <- GRU # Can't compare GRU with GRU in subset!!
    if (type == "uria") {
        # d <- subset(d, GRU == GRU)
        d <- subset(d, (GRU == GRU.arg)|is.na(GRU)) # GRU values for pooled samples before 1993 missing
    }
    ## PCB10 and CB138 are needed to calculate PCBSUM
    if ("PCBSUM" %in% vars) {
        add.vars <- c(add.vars, "PCB10", "CB138")
    }
    ## Error message if a variable is not found in data
    vars.tmp <- unlist(lapply(vars, function(x) {
        grep(paste0("\\b", x, "\\b"), names(d), value = TRUE)
    }))
    not.in.data <- which(!(vars.tmp %in% names(d)))
    if (length(not.in.data) > 0) {
        stop(paste("Variable(s)", paste(vars[not.in.data], collapse = ", "),
                   "not found in file."), call. = FALSE)
    }
    ## Just a message if an additional variable is not found
    add.vars.tmp <- unlist(sapply(add.vars, function(x) grep(x, names(d), fixed
    = TRUE, value = TRUE)))
    not.in.data.add <- which(!(add.vars %in% names(d)))
    if (length(not.in.data.add) > 0) {
        message(paste("Additional variable(s)", paste(add.vars[not.in.data.add], collapse = ", "),
                      "not found in file."))
        add.vars <- as.character(add.vars.tmp[add.vars.tmp %in% names(d)])
    }
    ## if ("DDT" %in% vars) d$DDT <- NULL

    ## Rename DDE, DDD and DDT to DDEX, DDDX, DDTX to avoid them being overwritten
    if (any(c("DDE", "DDD", "DDT") %in% vars)) {
        dde.inds <- which(grepl("DDE", names(d)))
        ddd.inds <- which(grepl("DDD", names(d)))
        ddt.inds <- which(grepl("DDT", names(d)))
        if (length(dde.inds) != 0) {
            names(d)[dde.inds] <- paste0(names(d)[dde.inds], "X")
        }
        if (length(ddd.inds) != 0) {
            names(d)[ddd.inds] <- paste0(names(d)[ddd.inds], "X")
        }
        if (length(ddt.inds) != 0) {
            names(d)[ddt.inds] <- paste0(names(d)[ddt.inds], "X")
        }
    }
    ## Generate the year from YWD
    if (yearvar == "YWD") {
        d$YEAR <- as.character(d[[yearvar]])
        d$YEAR <- ifelse(nchar(d$YEAR) == 2, d$YEAR,
                  ifelse(nchar(d$YEAR) == 3, "0",
                  ifelse(nchar(d$YEAR) == 4, substr(d$YEAR, 1, 1),
                  ifelse(nchar(d$YEAR) == 5, substr(d$YEAR, 1, 2), d$YEAR))))
        d$YEAR <- ifelse(as.numeric(d$YEAR) < 67,
                         2000 + as.numeric(d$YEAR),
                         1900 + as.numeric(d$YEAR))
    } else d$YEAR <- d[[yearvar]]
    ## Remove HGFST from the uriamet data
    if (type == "uria" & srt == "met") d$HGFST <- NULL
    ## Convert to numeric
    if (srt == "clc" & "PCBSUM" %in% vars) {
        d$PCB10 <- as.numeric(d$PCB10)
        d$PCB10[d$PCB10 <= -9] <- NA
        d$CB138 <- as.numeric(d$CB138)
        d$CB138[d$CB138 <= -9] <- NA
        ## Perform PCBSUM calculation
        if (type == "hav") {
            d <- merge(d, co_pcb, by = c("GENUS", "LOC"), all.x = TRUE)
            d$PCBSUM <- ifelse(d$YEAR <= 1988,
                               d$PCB10/d$r1, d$CB138/(d$r1 * d$r2))
        }
        if (type == "uria" & srt == "clc") {
            co_pcb$LOC <- NULL
            d <- merge(d, co_pcb, by = "GENUS", all.x = TRUE)
            d$PCBSUM <- ifelse(d$YEAR <= 1988,
                               d$PCB10/d$r1, d$CB138/(d$r1 * d$r2))
        }
    }
    ## If locales are written with lowercase characters
    d[[locvar]] <- toupper(d[[locvar]])

    ## First, split the data on locales
    d <- split(d, d[,locvar])
    ## Then on species
    d <- lapply(d, function(x) {
        split(x, f = x$GENUS)
    })
    ## These functions are probably not used...
    ok_years <- function(x, nyears) {
        ifelse(length(unique(x$YEAR)) < nyears, 0, 1)
    }
    ok_vars <- function(x) {
        ifelse(any(!is.na(x[,ncol(x)])), 1, 0)
    }
    ## Remove empty locales
    rem <- sapply(d, function(x) length(x) == 0)
    d <- d[!rem]

    ## Main workhorse
    d <- lapply(d, function(x) {
        lapply(x, function(y) {               
            if (srt == "dx" & "TCDDEQV" %in% vars) {
               vars <- c("TCDDEQVW", vars)
            }
            ## Loop over variables
            a <- lapply(vars, function(v) {
                ## KOND for fish
                if (v == "KOND" & any(c("CLUP", "PERC", "ZOAR", "GADU",
                                        "SALV", "ESOX", "RUTI") %in% y$GENUS)) {

                    aa <- y[, c(add.vars, "YEAR")]
                    aa$TOTV[as.numeric(aa$TOTV) <= -9] <- NA
                    aa$TOTL[as.numeric(aa$TOTL) <= -9] <- NA
                    aa$KOND <- 100 * as.numeric(aa$TOTV)/as.numeric(aa$TOTL)^3
                }
                ## KOND for bluemussel
                if (v == "KOND" & unique(y$GENUS) == "MYTI") {
                    aa <- y[, c(add.vars, "YEAR")]
                    if (exists("aa$SKLV")) {
                        aa$KOND <- (as.numeric(aa$TOTV) -
                                    as.numeric(aa$SKLV))/as.numeric(aa$SKLV)
                    } else aa$KOND <- NA
                }
                ## If FPRC are among the variables, remove it and put it last in
                ## the data
                if (v == "FPRC") {
                    FPRC <- y$FPRC
                    aa <- y[, c(add.vars, "YEAR")]
                    aa$FPRC <- NULL
                    aa$FPRC <- FPRC
                }
                ## Generate TCDDEQVW
                if (v == "TCDDEQVW" & type != "uria") {
                    aa <- y[, c(add.vars, "YEAR", "TCDDEQV")]
                    aa$TCDDEQV <- as.numeric(aa$TCDDEQV)
                    aa$TCDDEQV[aa$TCDDEQV < 0] <- NA
                    aa$TCDDEQVW <- aa$TCDDEQV * as.numeric(aa$FPRC)/100
                    aa$TCDDEQV <- NULL
                }
                if (v == "TCDDEQVW" & type == "uria") {
                    aa <- y[, c(add.vars, "YEAR", "TCDDEQV")]
                    aa$TCDDEQV <- as.numeric(aa$TCDDEQV)
                    aa$TCDDEQV[aa$TCDDEQV < 0] <- NA
                    aa$TCDDEQVW <- aa$TCDDEQV * as.numeric(aa$FPRCU)/100
                    aa$TCDDEQV <- NULL
                }                
                if (!v %in% c("KOND", "TCDDEQVW", "FPRC")) {
                    ## Extract the other variables
                    inds <- grep(paste0("^", v), names(y), value = TRUE)
                    if (v == "DDT") {
                        inds <- grep("DDTX|DDTKX", inds, value = TRUE)
                    }
                    if (v == "DDTS") {
                        inds <- grep("DDTSNYX|DDTSHX", inds, value = TRUE)
                    }
                    ## Make sure that CU and not CUCD is used
                    if (v == "CU" & "CUCD" %in% inds) {
                        inds <- inds[-which(inds == "CUCD")]
                    }
                    ## Make sure that AL and not ALDR is used
                    if (v == "AL" & "ALDR" %in% inds) {
                        inds <- inds[-which(inds == "ALDR")]
                    }
                    ## If CUCD is in the vars vector, make sure to use it and
                    ## not CU
                    if (v == "CUCD") inds <- inds[which(inds == "CUCD")]
                    for (i in 1:length(inds)) {
                        ## Make it numeric and set -9 and below to NA
                        y[[inds[i]]] <- as.numeric(y[[inds[i]]])
                        y[[inds[i]]] <- ifelse(y[[inds[i]]] <= -9 |
                                               y[[inds[i]]] == 0,
                                               NA, y[[inds[i]]])
                        y[[inds[i]]]
                    }
                    ## Return a data fram with additional variables and the
                    ## variable of interest
                    aa <- y[,c(add.vars, "YEAR", inds)]
                    if (type == "limn" & length(inds) == 1) {
                        ## For limnic data
                        names(aa)[names(aa) == inds] <- v
                    }
                }
                aa
            })
            names(a) <- vars
            ## Oh, here is ok_vars. Check if all values are NA
            which_ok <- lapply(a, function(x) {
                ok_vars(x)
            })
            ## Just keep variables with data
            a[which_ok == 1]
        })
    })
    if (srt %in% c("met", "clc", "other") & type != "limn") {
        d <- lapply(d, function(x) {
            lapply(x, function(y) {
                a <- lapply(seq_along(y), function(i) {
                    if (!type %in% c("limn", "uria")) {
                        ## Age selection
                        y[[i]] <- subset_age(data = y[[i]],
                                             agesel = agesel,
                                             what = names(y)[i],
                                             genus = unique(y[[i]]$GENUS),
                                             where = unique(y[[i]][[locvar]]))
                    }
                    remnams <- grep("^LOKAL|^ALDR|^SEX|^HGFST", names(y[[i]]))
                    nams <- names(y[[i]][-c(remnams)])
                    inds <- grep(paste(vars, collapse = "|"), nams, value =
                                                                        TRUE)
                    if (names(y)[i] != "KOND" &
                        (srt == "met" |
                         grepl("DDE", names(y[i])) |
                         grepl("DDD", names(y[i])) |
                         grepl("DDT", names(y[i])))) {
                        if (exists("y[[i]]$TPRC")) {
                            y[[i]]$TPRC <- as.numeric(y[[i]]$TPRC)
                            y[[i]]$TPRC[y[[i]]$LTPRC < 0] <- NA
                        }
                        if ("FPRC" %in% inds) inds <- inds[-which(inds ==
                                                                  "FPRC")]
                        ## And conversion between labs
                        y[[i]][[names(y)[i]]] <- convert(y[[i]],
                                                         inds = inds,
                                                         what = names(y)[i],
                                                         genus = unique(y[[i]]$GENUS),
                                                         srt = srt,
                                                         type = type,
                                                         loc =
                                                             unique(y[[i]][[locvar]]),
                                                         co_met = co_met,
                                                         co_dde = co_dde)
                        if (unique(y[[i]]$GENUS) == "MYTI" &
                            names(y)[i] != "HG" &
                            srt == "met") {
                            y[[i]][[names(y)[i]]] <- y[[i]][[names(y)[i]]] *
                                y[[i]]$TPRC/100
                        }
                        y[[i]]
                    } else y[[i]]
                })
                names(a) <- names(y)
                a
            })
        })
    }
    ## Now the same for limnic data
    if (type == "limn") {
        d <- lapply(d, function(x) {
            lapply(x, function(y) {
                a <- lapply(seq_along(y), function(i) {
                    remnams <- grep("^LOKAL|^ALDR|^SEX|^HGFST", names(y[[i]]))
                    if (length(remnams) > 0) {
                        nams <- names(y[[i]][-c(remnams)])
                    } else nams <- names(y[[i]])
                    inds <- grep(paste(vars, collapse = "|"), nams, value =
                                                                        TRUE)
                    if (names(y)[i] != "KOND" &
                         (grepl("DDE", names(y[i])) |
                         grepl("DDD", names(y[i])) |
                         grepl("DDT", names(y[i])))) {
                        if ("FPRC" %in% inds) inds <- inds[-which(inds ==
                                                                  "FPRC")]
                        y[[i]][[names(y)[i]]] <- convert(y[[i]],
                                                         inds = inds,
                                                         what = names(y)[i],
                                                         genus = unique(y[[i]]$GENUS),
                                                         srt = srt,
                                                         type = type,
                                                         loc =
                                                             unique(y[[i]][[locvar]]),
                                                         co_met = co_met,
                                                         co_dde = co_dde)
                        y[[i]]
                    } else y[[i]]
                })
                names(a) <- names(y)
                a
            })
        })
        ## Return the entire list
    } else d
}
