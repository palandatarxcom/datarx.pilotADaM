#'
#' gen_ht_t14_101
#' Generate Table 14-1.01 Summary of Population
#'
#' @param adsl ADaM ADSL data set
#'
#' @return a huxtable table
#' @export
#'
#' @examples
#'
gen_ht_t14_101 <- function(adsl) {
  # Create the total values upfront for quicker summary ----
  adsl_ <- adsl |>
    union(adsl |>
            mutate(TRT01P = 'Total',
                   TRT01PN = 99)) |>
    mutate(
      COMPL = ifelse(DCDECOD == "COMPLETED", "Y", "N")
    )

  # Calculate the header Ns ----
  header_n <- get_header_n(adsl_)

  # Column headers
  column_headers <- header_n |> select(TRT01PN, labels) |>
    tidyr::pivot_wider(names_from = TRT01PN, values_from = labels)

  # Intent to treat ----
  itt <- sum_subgrp(adsl_, ITTFL, order_var=STUDYID, include.n=FALSE, header_n = header_n) |>
    mutate(rowlbl1 = "Intent-To-Treat (ITT)")

  # Safety ----
  safety <- sum_subgrp(adsl_, SAFFL, order_var=STUDYID, include.n=FALSE, header_n = header_n) |>
    mutate(rowlbl1 = "Safety")

  # Efficacy ----
  efficacy <- sum_subgrp(adsl_, EFFFL, order_var=STUDYID, include.n=FALSE, header_n = header_n) |>
    mutate(rowlbl1 = "Efficacy")

  # Commpleters Week 24 ----
  compl_24 <- sum_subgrp(adsl_, COMP24FL, order_var=STUDYID, include.n=FALSE, header_n = header_n) |>
    mutate(rowlbl1 = "Complete Week 24")

  # Study completers ----
  compl <- sum_subgrp(adsl_, COMPL, order_var=STUDYID, include.n=FALSE, header_n = header_n) |>
    mutate(rowlbl1 = "Complete Study")

  # Pull the body together
  body <- rbind(itt, safety, efficacy, compl_24, compl) |>
    filter(rowlbl2 == "Y") |>
    select(-rowlbl2)

  # Cleanup
  rm(itt, safety, efficacy, compl_24, compl)

  # Attach the header
  final <- bind_rows(column_headers, body) |>
    select(rowlbl1, `0`, `54`, `81`, `99`)

  # Make the table
  ht <- as_hux(final, add_colnames = FALSE) |>
    set_bold(1, 1:ncol(final), TRUE) |>
    set_align(1, 1:ncol(final), 'center') |>
    set_valign(1, 1:ncol(final), 'bottom') |>
    set_bottom_border(1, 1:ncol(final), 1) |>
    set_width(1.1) |>
    set_escape_contents(FALSE) |>
    set_col_width(c(.4, .15, .15, .15, .15)) |>
    set_wrap(TRUE)
}


#'
#' gen_ht_t14_102
#' Generate Table 14-1.02 Summary of End of Study Data
#'
#' @param adsl ADaM ADSL data set
#'
#' @return a huxtable table
#' @export
#'
#' @examples
#'
gen_ht_t14_102 <- function(adsl) {
  adsl_ <- adsl

  adsl_$COMP24FL <- ordered(adsl_$COMP24FL, c("Y", "N", NA))
  adsl_$ARM <- ordered(adsl_$ARM, c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"))
  adsl_$DCREASCD <- ordered(adsl_$DCSREAS, c("Adverse Event",
                                           "Death",
                                           "Lack of Efficacy",
                                           "Lost to Follow-up",
                                           "Withdrew Consent",
                                           "Physician Decision",
                                           "I/E Not Met",
                                           "Protocol Violation",
                                           "Sponsor Decision"))

  # Completion Status Table ----
  comp_stat <- adsl_ |>
    group_by(COMP24FL, ARM) |>
    summarise(n = n())

  #Make data.frame for table, unnamed so the cols are named correctly
  comp_df <- data.frame(
    "Placebo" = n_pct(unlist(comp_stat[c(1,4), "n"]), sum(unlist(comp_stat[c(1,4), "n"])), mark_lt=FALSE),
    "Xanomeline Low Dose" = n_pct(unlist(comp_stat[c(2,5), "n"]), sum(unlist(comp_stat[c(2,5), "n"])), mark_lt=FALSE),
    "Xanomeline High Dose" = n_pct(unlist(comp_stat[c(3,6), "n"]), sum(unlist(comp_stat[c(3,6), "n"])), mark_lt=FALSE),
    "Total" = c(n_pct(sum(comp_stat[1:3, "n"]), sum(comp_stat[,"n"]), mark_lt=FALSE),
                n_pct(sum(comp_stat[4:6, "n"]), sum(comp_stat[,"n"]), mark_lt=FALSE)),
    row.names = c("\tCompleted Week 24", "\tEarly Termination (prior to Week 24)"),
    #Stop data.frame from adding periods
    check.names = FALSE, stringsAsFactors = FALSE
  )
  # Add tabs to row.names

  # Add missing row.
  comp_df["\tMissing", ] <- "  0 (  0%)"

  # p-value
  comp_p <- fish_p(adsl_, COMP24FL, ARM)
  comp_df <- attach_p(comp_df, comp_p)

  # Reason for Early Termination Table ----
  ## By ARM
  term_reas <- adsl_ |>
    filter(COMP24FL == "N") |>
    group_by(DCREASCD, ARM) |>
    # complete(nesting(DCREASCD, ARM)) |>
    summarise(n = n())

  ## Total
  term_reas_tot <- adsl_ |>
    filter(COMP24FL == "N", !is.na(DCDECOD)) |>
    group_by(DCREASCD) |>
    # complete(nesting(DCREASCD, ARM)) |>
    summarise(n = n())

  term_df <- data.frame(
    "Placebo" = n_pct(unlist(term_reas[seq(1, 27, 3), "n"]), sum(adsl_ |> filter(ARM == "Placebo") |> summarise(n = n())), mark_lt=FALSE),
    "Xanomeline Low Dose" = n_pct(unlist(term_reas[seq(2, 27, 3), "n"]), sum(adsl_ |> filter(ARM == "Xanomeline Low Dose") |> summarise(n = n())), mark_lt=FALSE),
    "Xanomeline High Dose" = n_pct(unlist(term_reas[seq(3, 27, 3), "n"]), sum(adsl_ |> filter(ARM == "Xanomeline High Dose") |> summarise(n = n())), mark_lt=FALSE),
    "Total" = n_pct(unlist(term_reas_tot[, "n"]), sum(adsl_ |> summarise(n = n())), mark_lt=FALSE),
    row.names = c(
      "\tAdverse Event",
      "\tDeath",
      "\tLack of Efficacy[2]",
      "\tLost to Follow-up",
      "\tSubject decided to withdraw",
      "\tPhysician decided to withdraw subject",
      "\tProtocol criteria not met",
      "\tProtocol violation",
      "\tSponsor decision"
    ),
    #Stop data.frame from adding periods
    check.names = FALSE, stringsAsFactors = FALSE
  )
  term_df["\tMissing", ] <- "  0 (  0%)"

  # p-value
  term_p_1 <- adsl_ |>
    select(ARM, DCREASCD) |>
    mutate(loefl = ifelse(DCREASCD %in% "Adverse Event", 1, 0)) |>
    fish_p(loefl, ARM, width = 6)
  term_df <- attach_p(term_df, term_p_1)

  term_p_2 <- adsl_ |>
    select(ARM, DCREASCD) |>
    mutate(loefl = ifelse(DCREASCD %in% "Lack of Efficacy", 1, 0)) |>
    fish_p(ARM ,loefl, width = 6)

  term_df["\tLack of Efficacy[2]",] <- attach_p(term_df[3,], term_p_2)


  ## Add Table lables
  comp_df <- add_column(comp_df, " " = row.names(comp_df), .before = 1)
  comp_df <- add_row(comp_df, " " = "Completion Status:", .before = 1)
  comp_df <- add_row(comp_df, " " = "", .before = 1)

  term_df <- add_column(term_df, " " = row.names(term_df), .before = 1)
  term_df <- add_row(term_df, " " = "Reason for Early Termination (prior to Week 24):", .before = 1)
  term_df <- add_row(term_df, " " = "", .before = 1)

  combinedTable <- rbind(comp_df, term_df)
  # Rename to get rid of period separation
  names(combinedTable)

  headers <- adsl_ |>
    group_by(ARM) |>
    summarise(N = n())
  headers_2 <- adsl_ |>
    summarise(N = n()) |>
    mutate(ARM = "Total")
  headers_3 <- rbind(headers, headers_2) |>
    mutate(labels = str_replace_all(str_wrap(glue::glue('{ARM} (N={N})'), width=10), "\n", function(x) "\\line "))
  headers_4 <- c(" ", headers_3$labels, "p-value [1]")
  names(combinedTable) <- headers_4

  ht <- combinedTable |>
    huxtable::as_hux(add_colnames=TRUE) |>
    huxtable::set_wrap(FALSE)

  huxtable::bottom_border(ht)[1, ] <- 1
  huxtable::bold(ht)[1, ] <- TRUE
  huxtable::align(ht)[1, ] <- 'center'
  huxtable::align(ht)[, 6] <- "center"
  huxtable::width(ht) <- 1.5
  huxtable::escape_contents(ht) <- FALSE
  huxtable::col_width(ht) <- c(.4, .12, .12, .12, .12, .12)
  huxtable::bottom_padding(ht) <- 0
  huxtable::top_padding(ht) <- 0
  huxtable::valign(ht)[1,] <- "bottom"
  ht[8,2] <- ""
  ht <- huxtable::merge_cells(ht, 8, 1:2)
}


#'
#' gen_ht_t14_103
#' Generate Table 14-1.03 Summary of Number of Subjects By Site
#'
#' @param adsl ADaM ADSL data set
#'
#' @return a huxtable table
#' @export
#'
#' @examples
#'
gen_ht_t14_103 <- function(adsl) {
  adsl$SITEGR1 <- ordered(adsl$SITEGR1, c(
    "Pooled\\line Id",
    "701",
    "703",
    "704",
    "705",
    "708",
    "709",
    "710",
    "713",
    "716",
    "718",
    "900",
    "TOTAL"
  ))
  adsl$SITEID <- ordered(adsl$SITEID, c(
    "Site\\line Id",
    "701",
    "703",
    "704",
    "705",
    "708",
    "709",
    "710",
    "713",
    "716",
    "718",
    "702",
    "706",
    "707",
    "711",
    "714",
    "715",
    "717",
    ""
  ))
  adsl$ITTFL <- ordered(adsl$ITTFL, c("Y", "N"))
  adsl$EFFFL <- ordered(adsl$EFFFL, c("Y", "N"))
  adsl$COMP24FL <- ordered(adsl$COMP24FL, c("Y", "N"))

  adsl_grp1 <- adsl |>
    select(SITEGR1, SITEID, TRT01P, ITTFL) |>
    group_by(SITEGR1, SITEID, TRT01P, ITTFL) |>
    filter(ITTFL == "Y") |>
    summarise(n = n())
  adsl_grp1[,4] <- "ITTFL"
  names(adsl_grp1)[4] <- "FLFL"

  adsl_grp2 <- adsl |>
    select(SITEGR1, SITEID, TRT01P, EFFFL) |>
    group_by(SITEGR1, SITEID, TRT01P, EFFFL) |>
    filter(EFFFL == "Y") |>
    summarise(n = n())
  adsl_grp2[,4] <- "EFFFL"
  names(adsl_grp2)[4] <- "FLFL"

  adsl_grp3 <- adsl |>
    select(SITEGR1, SITEID, TRT01P, COMP24FL) |>
    group_by(SITEGR1, SITEID, TRT01P, COMP24FL) |>
    filter(COMP24FL == "Y") |>
    summarise(n = n())
  adsl_grp3[,4] <- "COMP24FL"
  names(adsl_grp3)[4] <- "FLFL"

  adsl_grp4 <- adsl |>
    select(SITEGR1, SITEID, ITTFL) |>
    group_by(SITEGR1, SITEID, ITTFL) |>
    filter(ITTFL == "Y") |>
    summarise(n = n())
  adsl_grp4[,3] <- "ITTFL"
  names(adsl_grp4)[3] <- "FLFL"
  adsl_grp4$TRT01P <- "Total"

  adsl_grp5 <- adsl |>
    select(SITEGR1, SITEID, EFFFL) |>
    group_by(SITEGR1, SITEID, EFFFL) |>
    filter(EFFFL == "Y") |>
    summarise(n = n())
  adsl_grp5[,3] <- "EFFFL"
  names(adsl_grp5)[3] <- "FLFL"
  adsl_grp5$TRT01P <- "Total"

  adsl_grp6 <- adsl |>
    select(SITEGR1, SITEID, COMP24FL) |>
    group_by(SITEGR1, SITEID, COMP24FL) |>
    filter(COMP24FL == "Y") |>
    summarise(n = n())
  adsl_grp6[,3] <- "COMP24FL"
  names(adsl_grp6)[3] <- "FLFL"
  adsl_grp6$TRT01P <- "Total"

  all <- rbind(adsl_grp1, adsl_grp2, adsl_grp3, adsl_grp4, adsl_grp5, adsl_grp6)
  all$FLFL <- ordered(all$FLFL, c("ITTFL", "EFFFL", "COMP24FL"))
  all$TRT01P <- ordered(all$TRT01P, c(
    "Placebo",
    "Xanomeline Low Dose",
    "Xanomeline High Dose",
    "Total"
  ))

  df <- all |>
    arrange(SITEGR1, SITEID, TRT01P, FLFL) |>
    pivot_wider(id_cols = c(SITEGR1, SITEID), names_from = c(TRT01P, FLFL), values_from = c(n), values_fill = list(n = 0)) |>
    ungroup() |>
    as.data.frame()

  # Stack the total row to the bottom of the data frame

  df <-rbind(df,
             data.frame(
               SITEGR1 = "TOTAL",
               SITEID = "",
               t(apply(df[,3:ncol(df)], 2, sum)), check.names = FALSE
             )
  )

  names(df) <- c(
    "Pooled\\line Id",
    "Site\\line Id",
    rep(c("ITT", "Eff", "Com"), 4)
  )

  df[2:(nrow(df) + 1),] <- df[1:nrow(df),]
  df[1,] <- as.list(names(df))
  df <- df |>
    add_row("Pooled\\line Id" = "", .before = 1) |>
    add_row("Pooled\\line Id" = "", .before = 1)


  # Add Headers
  headers <- adsl |>
    group_by(ARM) |>
    summarise(N = n()) |>
    mutate(labels = str_replace_all(str_wrap(glue('{ARM} (N={N})'), width=10), "\n", function(x) "\\line "))
  headers[4,] <- list(
    ARM = "Total",
    N = nrow(adsl),
    labels = paste0("Total\\line(N=", nrow(adsl), ")")
  )

  df[1, 3] <- headers[1, "labels"]
  df[1, 6] <- headers[3, "labels"]
  df[1, 9] <- headers[2, "labels"]
  df[1, 12] <- headers[4, "labels"]

  ht <- df |>
    as_hux(add_colnames=FALSE) |>
    merge_cells(1, 3:5) |>
    set_bottom_border(2, 3:5, 1) |>
    merge_cells(1, 6:8) |>
    set_bottom_border(2, 6:8, 1) |>
    merge_cells(1, 9:11) |>
    set_bottom_border(2, 9:11, 1) |>
    merge_cells(1, 12:14) |>
    set_bottom_border(2, 12:14, 1) |>
    set_escape_contents(FALSE) |>
    set_width(1.1) |>
    set_bottom_border(3, 1:14, 1) |>
    set_align(3, 1:14, "center") |>
    set_valign(3, 1:14, "bottom") |>
    set_align(4:nrow(df), 3:ncol(df), "right") |>
    set_align(1:nrow(df), 1:2, "center") |>
    set_align(1, 1:14, "center") |>
    set_valign(1, 1:14, "bottom") |>
    set_col_width(1:ncol(df), value = c(0.1, 0.1, rep(0.07, 12))) |>
    set_wrap(FALSE)
}


#' gen_ht_t14_201
#' Generate Table 14-2.01 Summary of Demographic and Baseline Characteristics
#'
#' @param adsl ADaM ADSL dataset
#'
#' @return a huxtable table
#' @export
#'
#' @examples
gen_ht_t14_201 <- function(adsl) {
  adsl <- adsl |>
    filter(ITTFL == "Y") |>
    mutate(
      RACE_DISPLAY = case_when(
        ETHNIC == 'HISPANIC OR LATINO' ~ 'Hispanic',
        RACE == 'WHITE' ~ 'Caucasian',
        RACE == 'BLACK OR AFRICAN AMERICAN' ~ 'African Descent',
        RACE == 'AMERICAN INDIAN OR ALASKA NATIVE' ~ 'Other',
      ),
      RACEN_DISPLAY = case_when(
        ETHNIC == 'HISPANIC OR LATINO' ~ 3,
        RACE == 'WHITE' ~ 1,
        RACE == 'BLACK OR AFRICAN AMERICAN' ~ 2,
        RACE == 'AMERICAN INDIAN OR ALASKA NATIVE' ~ 4,
      ),
      SEX =
        case_when(
          SEX == 'M' ~ 'Male',
          SEX == 'F' ~ 'Female'
        ),
      SEXN =
        case_when(
          SEX == 'Male' ~ 1,
          SEX == 'Female' ~ 2
        ),
      DURDSGR1N =
        case_when(
          DURDSGR1 == '<12' ~ 1,
          DURDSGR1 == '>=12' ~ 2
        ),
      DURDSGR1 = paste(DURDSGR1, 'months'),
      BMIBLGR1N =
        case_when(
          BMIBLGR1 == '<25' ~ 1,
          BMIBLGR1 == '25-<30' ~ 2,
          BMIBLGR1 == '>=30' ~ 3
        ),
      AGEGR1 = paste(AGEGR1, 'yrs')
    )

  # get_meta(adsl)

  # Create the total values upfront for quicker summary ----
  adsl_ <- adsl |>
    bind_rows(adsl |>
                mutate(TRT01P = 'Total',
                       TRT01PN = 99))


  # Get the header N's ----
  header_n <- get_header_n(adsl_)

  ## Exploring Age ----

  # Descriptive stats
  age_1 <- adsl_ |> desc_stats(AGE)
  age_p <- adsl |> aov_p(AGE ~ TRT01P) # anova

  age_1 <- attach_p(age_1, age_p)

  # Categorical n counts
  age_2 <- adsl_ |> sum_subgrp(AGEGR1, AGEGR1N, include.n=FALSE, header_n=header_n)

  agegrp_p <- adsl |> chi_p(AGEGR1, TRT01P)
  age_2 <- attach_p(age_2, agegrp_p)

  age <- rbind(age_1, age_2) |>
    mutate(rowlbl1 = "Age (y)")

  rm(age_1, age_2, age_p, agegrp_p)

  ## Exploring sex ----
  sex = adsl_ |>
    sum_subgrp(SEX, SEXN, header_n=header_n)

  sex_p <- adsl |> chi_p(SEX, TRT01P)

  sex <- attach_p(sex, sex_p) |>
    mutate(rowlbl1 = 'Sex')

  rm(sex_p)

  ## Exploring race ----
  race = adsl_ |>
    sum_subgrp(RACE_DISPLAY, RACEN_DISPLAY, header_n=header_n)

  race_p <- adsl |> chi_p(RACE_DISPLAY, TRT01P)

  race <- attach_p(race, race_p) |>
    mutate(rowlbl1 = "Race (Origin)")

  rm(race_p)

  ## Exploring MMSE ---
  mmse <- adsl_ |> desc_stats(MMSETOT)

  mmse_p <- adsl |> aov_p(MMSETOT ~ TRT01P)

  mmse <- attach_p(mmse, mmse_p) |>
    mutate(rowlbl1 = 'MMSE')

  rm(mmse_p)

  ## Exploring disease duration ----

  # Descriptive
  durdis_1 <- adsl_ |> desc_stats(DURDIS)
  durdis_1p <- adsl |> aov_p(DURDIS ~ TRT01P)
  durdis_1 <- attach_p(durdis_1, durdis_1p)

  # Categorical
  durdis_2 <- adsl_ |> sum_subgrp(DURDSGR1, DURDSGR1N, include.n=FALSE, header_n=header_n)
  durdis_2p <- adsl |> chi_p(DURDSGR1, TRT01P)
  durdis_2 <- attach_p(durdis_2, durdis_2p)

  durdis <- durdis_1 |>
    union(durdis_2) |>
    mutate(rowlbl1 = 'Duration of disease ') |>
    pad_row()

  rm(durdis_1, durdis_2, durdis_1p, durdis_2p)

  ## Years of education ----
  educlvl <- adsl_ |> desc_stats(EDUCLVL)

  educlvl_p <- adsl |> aov_p(EDUCLVL ~ TRT01P)

  educlvl <- attach_p(educlvl, educlvl_p) |>
    mutate(rowlbl1 = 'Years of education')

  rm(educlvl_p)

  ## Baseline weight ----
  weightbl <- adsl_ |> desc_stats(WEIGHTBL)

  weightbl_p <- adsl |> aov_p(WEIGHTBL ~ TRT01P)

  weightbl <- attach_p(weightbl, weightbl_p)|>
    mutate(rowlbl1 = 'Baseline weight(kg)')

  rm(weightbl_p)

  ## Baseline height ----
  heightbl <- adsl_ |> desc_stats(HEIGHTBL)

  heightbl_p <- adsl |> aov_p(HEIGHTBL ~ TRT01P)

  heightbl <- attach_p(heightbl, heightbl_p) |>
    mutate(rowlbl1 = 'Baseline height(cm)')

  rm(heightbl_p)

  ## Baseline BMI ----

  # Descriptive
  bmi_1 <- adsl_ |> desc_stats(BMIBL)
  bmi_1p <- adsl |> aov_p(BMIBL ~ TRT01P)
  bmi_1 <- attach_p(bmi_1, bmi_1p)

  # Categorical
  bmi_2 <- adsl_ |> sum_subgrp(BMIBLGR1, BMIBLGR1N, include.n=FALSE, header_n=header_n)
  bmi_2p <- adsl |> chi_p(BMIBLGR1, TRT01P)
  bmi_2 <- attach_p(bmi_2, bmi_2p)

  bmi <- rbind(bmi_1, bmi_2) |>
    mutate(rowlbl1 = 'Baseline BMI')

  rm(bmi_1, bmi_2, bmi_1p, bmi_2p)

  ## Stack together final tables ---
  final <- rbind(age, sex, race, mmse, durdis, educlvl, weightbl, heightbl, bmi) |>
    group_by(rowlbl1) |>
    mutate(ord1 = row_number()) |>
    ungroup() |>
    mutate(rowlbl1 = ifelse(ord1 == 1, rowlbl1, ""))

  rm(age, sex, race, mmse, durdis, educlvl, weightbl, heightbl, bmi)

  # Make and attach column headers
  header_n_v <- header_n |> select(TRT01PN, labels) |>
    pivot_wider(names_from = TRT01PN, values_from = labels) |>
    mutate(
      rowlbl1 = '',
      rowlbl2 = '',
      p = 'p-value\\line [1]'
    )

  final <- bind_rows(header_n_v, final) |>
    select(rowlbl1, rowlbl2, `0`, `54`, `81`, `99`, p)

  ## Table build
  ht <- huxtable::as_hux(final, add_colnames = FALSE)

  huxtable::bottom_border(ht)[1, ] <- 1
  huxtable::valign(ht)[1, ] <- 'bottom'
  huxtable::bold(ht)[1, ] <- TRUE
  huxtable::align(ht)[1, ] <- 'center'
  huxtable::width(ht) <- 1.5
  huxtable::escape_contents(ht) <- FALSE
  huxtable::col_width(ht) <- c(.2, .2, .12, .12, .12, .12, .12)
  huxtable::bottom_padding(ht) <- 0
  huxtable::top_padding(ht) <- 0

  return(ht)
}


#' gen_ht_t14_301
#' Generate Table 14-3.01 Primary Endpoint Analysis: ADAS Cog (11) - Change from Baseline to Week 24 - LOCF
#'
#' @param adadas
#'
#' @return a huxtable
#' @export
#'
#' @examples
gen_ht_t14_301 <- function(adadas) {
  adas <- adam_adadas |>
    filter(EFFFL == "Y" & ITTFL=='Y' & PARAMCD == 'ACTOT' & ANL01FL == 'Y')

  # Calculate the header Ns ----
  header_n <- adas |>
    distinct(USUBJID, TRTP, TRTPN) |>
    get_header_n(TRTP, TRTPN)

  column_headers <- header_n |>
    select(-N) |>
    pivot_wider(names_from = TRTPN, values_from=labels) |>
    mutate(rowlbl1 = '')

  # Run each group ----
  summary_portion <- bind_rows(summary_data(adas, AVAL, 0 , 'Baseline'),
                               summary_data(adas, AVAL, 24, 'Week 24'),
                               summary_data(adas, CHG,  24, 'Change from Baseline')) |>
    pad_row()

  ## Gather the model data ----
  model_portion <- efficacy_models(adas, 'CHG', 24)

  final <- bind_rows(column_headers, summary_portion, model_portion) |>
    select(rowlbl1, `0`, `54`, `81`)

  # Create the table ----
  ht <- huxtable::as_hux(final, add_colnames = FALSE) |>
    huxtable::set_bold(1, 1:ncol(final), TRUE) |>
    huxtable::set_align(1, 1:ncol(final), 'center') |>
    huxtable::set_valign(1, 1:ncol(final), 'bottom') |>
    huxtable::set_bottom_border(1, 1:ncol(final), 1) |>
    huxtable::set_width(1.2) |>
    huxtable::set_escape_contents(FALSE) |>
    huxtable::set_col_width(c(.5, 1/6, 1/6, 1/6))

  return(ht)
}

