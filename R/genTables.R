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


#' gen_ht_t14_302
#' Generate Table 14-3.02 Primary Endpoint Analysis: CIBIC+ - Summary at Week 24 - LOCF
#'
#' @param adcibc
#'
#' @return a huxtable
#' @export
#'
#' @examples
gen_ht_t14_302 <- function(adcibc) {
  cibc <- adcibc |>
    filter(EFFFL == "Y" & ITTFL=='Y' & PARAMCD == 'CIBICVAL' & ANL01FL == 'Y')

  # Calculate the header Ns ----
  header_n <- cibc |>
    distinct(USUBJID, TRTP, TRTPN) |>
    get_header_n(TRTP, TRTPN)

  column_headers <- header_n |>
    select(-N) |>
    tidyr::pivot_wider(names_from = TRTPN, values_from=labels) |>
    mutate(rowlbl1 = '')

  # Run each group ----
  summary_portion <- summary_data(cibc, AVAL, 24, 'Week 24') |>
    pad_row()

  # Gather the model data ----
  model_portion <- efficacy_models(cibc, 'AVAL', 24)

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


#' gen_ht_t14_303
#' Generate Table 14-3.03 ADAS Cog (11) - Change from Baseline to Week 8 - LOCF
#'
#' @param adadas
#'
#' @return
#' @export
#'
#' @examples
gen_ht_t14_303 <- function(adadas) {
  adas <- adadas |>
    filter(EFFFL == "Y" & ITTFL=='Y' & PARAMCD == 'ACTOT' & ANL01FL == 'Y')

  # Calculate the header Ns ----
  header_n <- adas |>
    distinct(USUBJID, TRTP, TRTPN) |>
    get_header_n(TRTP, TRTPN)

  column_headers <- header_n |>
    select(-N) |>
    tidyr::pivot_wider(names_from = TRTPN, values_from=labels) |>
    mutate(rowlbl1 = '')

  # Run each group ----
  summary_portion <- bind_rows(summary_data(adas, AVAL, 0, 'Baseline'),
                               summary_data(adas, AVAL, 8, 'Week 8'),
                               summary_data(adas, CHG,  8, 'Change from Baseline')) |>
    pad_row()

  # Gather the model data ----
  model_portion <- efficacy_models(adas, 'CHG', 8)

  final <- bind_rows(column_headers, summary_portion, model_portion) |>
    select(rowlbl1, `0`, `54`, `81`)

  # Make the table ----
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


#' gen_ht_t14_304
#' Generate Table 14-3.04 CIBIC+ - Summary at Week 8 - LOCF
#'
#' @param adcibc
#'
#' @return a huxtable
#' @export
#'
#' @examples
gen_ht_t14_304 <- function(adcibc) {
  cibc <- adcibc |>
    filter(EFFFL == "Y" & ITTFL=='Y' & PARAMCD == 'CIBICVAL' & ANL01FL == 'Y')

  # Calculate the header Ns ----
  header_n <- cibc |>
    distinct(USUBJID, TRTP, TRTPN) |>
    get_header_n(TRTP, TRTPN)

  column_headers <- header_n |>
    select(-N) |>
    tidyr::pivot_wider(names_from = TRTPN, values_from=labels) |>
    mutate(rowlbl1 = '')

  # Run each group ----
  summary_portion <- summary_data(cibc, AVAL, 8, 'Week 8') |>
    pad_row()

  # Gather the model data ----
  model_portion <- efficacy_models(cibc, 'AVAL', 8)

  final <- bind_rows(column_headers, summary_portion, model_portion) |>
    select(rowlbl1, `0`, `54`, `81`)

  # Make the table ----
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


#' gen_ht_t14_305
#' Generate Table 14-3.05 ADAS Cog (11) - Change from Baseline to Week 16 - LOCF
#'
#' @param adadas
#'
#' @return a huxtable
#' @export
#'
#' @examples
gen_ht_t14_305 <- function(adadas) {
  adas <- adadas |>
    filter(EFFFL == "Y" & ITTFL=='Y' & PARAMCD == 'ACTOT' & ANL01FL == 'Y')

  # Calculate the header Ns ----
  header_n <- adas |>
    distinct(USUBJID, TRTP, TRTPN) |>
    get_header_n(TRTP, TRTPN)

  column_headers <- header_n |>
    select(-N) |>
    tidyr::pivot_wider(names_from = TRTPN, values_from=labels) |>
    mutate(rowlbl1 = '')

  # Run each group ----
  summary_portion <- bind_rows(summary_data(adas, AVAL,  0, 'Baseline'),
                               summary_data(adas, AVAL, 16, 'Week 16'),
                               summary_data(adas, CHG,  16, 'Change from Baseline')) |>
    pad_row()

  # Gather the model data ----
  model_portion <- efficacy_models(adas, 'CHG', 16)

  final <- bind_rows(column_headers, summary_portion, model_portion) |>
    select(rowlbl1, `0`, `54`, `81`)

  # Make the table ----
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


#' gen_ht_t14_306
#' Generate Table 14-3.06 CIBIC+ - Summary at Week 16 - LOCF
#'
#' @param adcibc
#'
#' @return a huxtable
#' @export
#'
#' @examples
gen_ht_t14_306 <- function(adcibc) {
  cibc <- adcibc |>
    filter(EFFFL == "Y" & ITTFL=='Y' & PARAMCD == 'CIBICVAL' & ANL01FL == 'Y')

  # Calculate the header Ns ----
  header_n <- cibc |>
    distinct(USUBJID, TRTP, TRTPN) |>
    get_header_n(TRTP, TRTPN)

  column_headers <- header_n |>
    select(-N) |>
    tidyr::pivot_wider(names_from = TRTPN, values_from=labels) |>
    mutate(rowlbl1 = '')

  # Run each group ----
  summary_portion <- summary_data(cibc, AVAL, 16, 'Week 16') |>
    pad_row()

  # Gather the model data ----
  model_portion <- efficacy_models(cibc, 'VAL', 16)

  final <- bind_rows(column_headers, summary_portion, model_portion) |>
    select(rowlbl1, `0`, `54`, `81`)

  # Make the table ----
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


#' gen_ht_t14_307
#' Generate Table 14-3.07 ADAS Cog (11) - Change from Baseline to Week 24 - Completers at Wk 24-Observed Cases-Windowed
#'
#' @param adadas
#'
#' @return a huxtable
#' @export
#'
#' @examples
gen_ht_t14_307 <- function(adadas) {
  adas <- adadas |>
    filter(COMP24FL == "Y" & EFFFL=='Y' & PARAMCD == 'ACTOT' & ANL01FL == 'Y' & DTYPE != 'LOCF')

  # Calculate the header Ns ----
  header_n <- adas |>
    distinct(USUBJID, TRTP, TRTPN) |>
    get_header_n(TRTP, TRTPN)

  column_headers <- header_n |>
    select(-N) |>
    tidyr::pivot_wider(names_from = TRTPN, values_from=labels) |>
    mutate(rowlbl1 = '')

  # Run each group ----
  summary_portion <- bind_rows(summary_data(adas, AVAL, 0, 'Baseline'),
                               summary_data(adas, AVAL, 24, 'Week 24'),
                               summary_data(adas, CHG,  24, 'Change from Baseline')) |>
    pad_row()

  # Gather the model data ----
  model_portion <- efficacy_models(adas, 'CHG', 24)

  final <- bind_rows(column_headers, summary_portion, model_portion) |>
    select(rowlbl1, `0`, `54`, `81`)

  # Make the table ----
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


#' gen_ht_t14_308
#' Generate Table 14-3.08 ADAS Cog (11) - Change from Baseline to Week 24 in Male Subjects - LOCF
#'
#' #' @param adadas
#'
#' @return a huxtable
#' @export
#'
#' @examples
gen_ht_t14_308 <- function(adadas) {
  adas <- adadas |>
    filter(EFFFL=='Y' & PARAMCD == 'ACTOT' & ANL01FL == 'Y' & SEX == "M")

  # Calculate the header Ns ----
  header_n <- adas |>
    distinct(USUBJID, TRTP, TRTPN) |>
    get_header_n(TRTP, TRTPN)

  column_headers <- header_n |>
    select(-N) |>
    pivot_wider(names_from = TRTPN, values_from=labels) |>
    mutate(rowlbl1 = '')

  # Run each group ----
  summary_portion <- bind_rows(summary_data(adas, AVAL, 0, 'Baseline'),
                               summary_data(adas, AVAL, 24, 'Week 24'),
                               summary_data(adas, CHG,  24, 'Change from Baseline')) |>
    pad_row()

  # Gather the model data ----
  model_portion <- efficacy_models(adas, 'CHG', 24)

  final <- bind_rows(column_headers, summary_portion, model_portion) |>
    select(rowlbl1, `0`, `54`, `81`)

  # Make the table ----
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


#' gen_ht_t14_309
#' Generate Table 14-3.09 ADAS Cog (11) - Change from Baseline to Week 24 in Female Subjects - LOCF
#'
#' @param adadas
#'
#' @return
#' @export
#'
#' @examples
gen_ht_t14_309 <- function(adadas) {
  adas <- adadas |>
    filter(EFFFL == "Y" & PARAMCD == 'ACTOT' & ANL01FL == 'Y' & SEX == "F")

  # Calculate the header Ns ----
  header_n <- adas |>
    distinct(USUBJID, TRTP, TRTPN) |>
    get_header_n(TRTP, TRTPN)

  column_headers <- header_n |>
    select(-N) |>
    pivot_wider(names_from = TRTPN, values_from=labels) |>
    mutate(rowlbl1 = '')

  # Run each group ----
  summary_portion <- bind_rows(summary_data(adas, AVAL, 0,  'Baseline'),
                               summary_data(adas, AVAL, 24, 'Week 24'),
                               summary_data(adas, CHG,  24, 'Change from Baseline')) |>
    pad_row()

  # Gather the model data ----
  model_portion <- efficacy_models(adas, 'CHG', 24)

  final <- bind_rows(column_headers, summary_portion, model_portion) |>
    select(rowlbl1, `0`, `54`, `81`)

  # Make the table ----
  ht <- huxtable::as_hux(final, add_colnames = FALSE) |>
    huxtable::set_bold(1, 1:ncol(final), TRUE) |>
    huxtable::set_align(1, 1:ncol(final), 'center') |>
    huxtable::set_valign(1, 1:ncol(final), 'bottom') |>
    huxtable::set_bottom_border(1, 1:ncol(final), 1) |>
    huxtable::set_width(1.2) |>
    huxtable::set_escape_contents(FALSE) |>
    huxtable::set_col_width(c(.5, 1/6, 1/6, 1/6))

  return (ht)
}


#' gen_ht_t14_310
#' Generate Table 14-3.10 ADAS Cog (11) - Mean and Mean Change from Baseline over Time
#' @param adadas
#'
#' @return a huxtable
#' @export
#'
#' @examples
gen_ht_t14_310 <- function(adadas) {
  adas <- adadas |>
    filter(EFFFL == "Y" & PARAMCD == "ACTOT" & ITTFL == "Y" & AVISITN %in% c(0, 8, 16, 24) & ANL01FL=="Y") |>
    mutate(SET = "LOCF") |>
    select(TRTPN, TRTP, AVISIT, AVISITN, AVAL, BASE, CHG, DTYPE, SET)

  # Dataframe to merge display order of visits ----
  visits <- tibble(
    ORD = rep(c(1:8), 3),
    AVISIT = rep(c("Baseline", "Week 8 (Windowed)", "Week 16 (Windowed)", "Week 24 (Windowed)",
                   "Week 8 LOCF", "Week 16 LOCF", "Week 24 LOCF", ''), 3),
    TRTPN = c(rep(c(0), 8), rep(54, 8), rep(81, 8))
  )

  # Join the LOCF and Windowed sets together ----
  step1 <- adas |>
    bind_rows(adas |>
                filter(AVISITN != 0 & DTYPE != 'LOCF') |>
                mutate(SET="WINDOWED")
    ) |>

    mutate(
      # Format AVISIT for display
      AVISIT =
        case_when(
          SET == 'WINDOWED' & AVISITN != 0 ~ paste(AVISIT, '(Windowed)'),
          SET == 'LOCF' & AVISITN != 0 ~ paste(AVISIT, 'LOCF'),
          TRUE ~ AVISIT
        ),
      # Display of TRTP
      TRTP =
        case_when(
          TRTPN == 0 ~ 'Placebo',
          TRTPN == 54 ~ 'Xan.Low',
          TRTPN == 81 ~ 'Xan.High'
        )
    )

  # Get all summaries for aval ----
  aval <- step1 |>
    group_by(TRTPN, TRTP, AVISITN, AVISIT, SET) |>
    summarize(
      n = num_fmt(n(), int_len=2, size=2),
      mean = num_fmt(mean(AVAL), digits=1, int_len=2, size=4),
      sd = num_fmt(sd(AVAL), digits=2, int_len=2, size=5),
      md = num_fmt(median(AVAL), digits=1, int_len=2, size=4),
      mn = num_fmt(min(AVAL), int_len=2, size=4),
      mx = num_fmt(max(AVAL), int_len=2, size=4),
    ) |>
    full_join(visits, by=c("TRTPN", "AVISIT"))

  # Get all summaries for chg ----
  chg <- step1 |>
    group_by(TRTPN, TRTP, AVISITN, AVISIT, SET) |>
    filter(AVISITN != 0) |>
    summarize(
      meanc = num_fmt(mean(CHG), digits=1, int_len=1, size=4),
      sdc = num_fmt(sd(CHG), digits=2, int_len=1, size=4),
      mdc = num_fmt(median(CHG), digits=1, int_len=1, size=4),
      mnc = num_fmt(min(CHG), int_len=3, size=4),
      mxc = num_fmt(max(CHG), int_len=2, size=4),
      bmn = num_fmt(mean(BASE), digits=1, int_len=2, size=4),
      bsd = num_fmt(sd(BASE), digits=2, int_len=2, size=5)
    )

  # Join to AVAL and CHG results together to create final table ----
  final <- left_join(aval, chg, by=c('TRTPN', 'TRTP', 'AVISITN', 'AVISIT', 'SET')) |>
    arrange(TRTPN, ORD) |>
    ungroup() |>
    mutate(TRTP = ifelse(ORD==1, TRTP, '')) |>
    select(TRTP, AVISIT, n, mean, sd, md, mn, mx, bmn, bsd, meanc, sdc, mdc, mnc, mxc)

  # Create the column headers
  header <- tibble(
    TRTP=character(2),
    AVISIT=character(2),
    n=c('', 'nc'),
    mean=c('', 'Mean'),
    sd=c('', 'Std'),
    md=c('', 'Med.'),
    mn=c('', 'Min.'),
    mx=c('', 'Max.'),
    bmn=c('', 'Bsln\\line Mean'),
    bsd=c('', 'Bsln\\line Std'),
    meanc=c('---Change from baseline---', 'Mean'),
    sdc=c('', 'Std'),
    mdc=c('', 'Med.'),
    mnc=c('', 'Min.'),
    mxc=c('', 'Max.')
  )

  # Make the table
  ht <- huxtable::as_hux(bind_rows(header, final), add_colnames = FALSE) |>
    huxtable::merge_cells(1, 11:15) |> # Span header for Change from Baseline
    huxtable::set_bold(1:2, 1:ncol(final), TRUE) |> # Bold the header
    huxtable::set_align(1:2, 1:ncol(final), 'center') |> # Align the header
    huxtable::set_align(1:(nrow(final) +2), 3:ncol(final), 'center') |> # Center all of the numeric cells
    huxtable::set_valign(1:2, 1:ncol(final), 'bottom') |> # Attach the column headers to the bottom of the cell
    huxtable::set_bottom_border(2, 1:ncol(final), 1) |> # Bottom border under column header
    huxtable::set_width(1.5) |> # Take up the whole width of the page
    huxtable::set_escape_contents(FALSE) |> # Allow RTF strings
    huxtable::set_col_width(c(.09, .19, .05, .06, .06, .06, .05, .05, .06, .06, .06, .05, .05, .05, .06)) # Column widths as a ratio

  return(ht)
}


#' gen_ht_t14_311
#'
#' @param adadas
#' Generate Table 14-3.11 ADAS Cog (11) - Repeated Measures Analysis of Change from Baseline to Week 24
#'
#' @return
#' @export
#'
#' @examples
gen_ht_t14_311 <- function(adadas) {
  adas <- adadas |>
    filter(EFFFL == "Y" & PARAMCD == 'ACTOT' & ANL01FL == 'Y' & DTYPE != 'LOCF' & AVISITN > 0)

  # Calculate the header Ns ----
  header_n <- adas |>
    distinct(USUBJID, TRTP, TRTPN) |>
    get_header_n(TRTP, TRTPN)

  column_headers <- header_n |>
    select(-N) |>
    tidyr::pivot_wider(names_from = TRTPN, values_from=labels) |>
    mutate(rowlbl1 = '')

  # Gather the model data ----
  model_portion <- efficacy_models(adas, 'CHG', 24, model_type='repeated')

  final <- bind_rows(column_headers, model_portion) |>
    select(rowlbl1, `0`, `54`, `81`)

  # Take off footnote references ----
  final[4,1] <- "p-value(Xan - Placebo)"
  final[8,1] <- "p-value(Xan High - Xan Low)"

  # Make the table ----
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


#' gen_ht_t14_312
#' Generate Table 14-3.12 Mean NPI-X Total Score from Week 4 through Week 24 - Windowed
#'
#' @param adnpix
#'
#' @return a huxtable
#' @export
#'
#' @examples
gen_ht_t14_312 <- function(adnpix) {
  npix <- adnpix %>%
    filter(EFFFL == 'Y' & ITTFL == 'Y' & PARAMCD == 'NPTOTMN') %>%
    mutate(CHG = AVAL - BASE)

  # Calculate the header Ns ----
  header_n <- npix %>%
    distinct(USUBJID, TRTP, TRTPN) %>%
    get_header_n(TRTP, TRTPN)

  column_headers <- header_n %>%
    select(-N) %>%
    pivot_wider(names_from = TRTPN, values_from=labels) %>%
    mutate(rowlbl1 = '')

  summary_portion <- bind_rows(summary_data(npix, AVAL, 0 , 'Baseline'),
                               summary_data(npix, AVAL,  98, 'Mean of Weeks 4-24')) %>%
    pad_row()

  # Gather the model data ----
  model_portion <- efficacy_models(npix, 'CHG', 98)

  final <- bind_rows(column_headers, summary_portion, model_portion) %>%
    select(rowlbl1, `0`, `54`, `81`)

  # Create the table ----
  ht <- huxtable::as_hux(final, add_colnames = FALSE) %>%
    huxtable::set_bold(1, 1:ncol(final), TRUE) %>%
    huxtable::set_align(1, 1:ncol(final), 'center') %>%
    huxtable::set_valign(1, 1:ncol(final), 'bottom') %>%
    huxtable::set_bottom_border(1, 1:ncol(final), 1) %>%
    huxtable::set_width(1.2) %>%
    huxtable::set_escape_contents(FALSE) %>%
    huxtable::set_col_width(c(.5, 1/6, 1/6, 1/6))

  return(ht)
}


#' gen_ht_t14_313
#' Generate Table 14-3.13 CIBIC+ - Categorical Analysis - LOCF
#'
#' @param adadcibc
#'
#' @return a huxtable
#' @export
#'
#' @examples
gen_ht_t14_313 <- function(adadcibc) {
  ord <- tibble(
    AVALC = rep(c('n',
                  'Marked improvement',
                  'Moderate improvement',
                  'Minimal improvement',
                  'No Change',
                  'Minimal worsening',
                  'Moderate worsening',
                  'Marked worsening', ''),3),
    ord = rep(c(0:8),3),
    AVISITN = c(rep(8, 9), rep(16,9), rep(24,9)),
    AVISIT = c(rep('Week 8', 9), rep('Week 16', 9), rep('Week 24', 9))
  )

  # Read in the CBIC dataset ----
  cbic <- adadcibc |>
    filter(EFFFL == 'Y' & ITTFL == 'Y', AVISITN %in% c(8, 16, 24) & ANL01FL=='Y') |>
    # Create a character version of AVAL for display
    mutate(
      AVALC = ord[2:8, ]$AVALC[AVAL], # The codelist is already in this dataframe so using that
    )

  # Calculate the header Ns ----
  header_n <- cbic |>
    distinct(USUBJID, TRTP, TRTPN) |>
    get_header_n(TRTP, TRTPN)

  column_headers <- header_n |>
    select(-N) |>
    pivot_wider(names_from = TRTPN, values_from=labels) |>
    mutate(AVISIT = '',
           AVALC = 'Assessment',
           p = 'p-value\\line [1]')

  # Get the summary N counts for each group ----
  ns <- cbic |>
    group_by(TRTPN, AVISITN, AVISIT) |>
    summarize(N = n()) |>
    ungroup()

  counts <- cbic |>
    # Summarize the categorical counts
    group_by(TRTPN, AVISITN, AVISIT, AVALC) |>
    summarize(n = n()) |>
    ungroup() |>
    # Merge in the group N's for summary
    merge(ns, by=c('TRTPN', 'AVISITN', 'AVISIT', 'AVISIT')) |>
    rowwise() |>
    # Format the n (%)
    mutate(npct=n_pct(n, N, n_width=2)) |>
    select(-n, -N) |>
    # Transpose out by treatment group
    tidyr::pivot_wider(names_from = TRTPN, values_from=npct) |>
    # Bind with the N rows
    bind_rows(
      # Need for tranpose and format
      ns |>
        rowwise() |>
        # Format the N counts and add the row label
        mutate(
          AVALC = 'n',
          Nc = num_fmt(N, size=9, int_len=2)
        ) |>
        select(-N) |>
        # Transpose out by group
        tidyr::pivot_wider(names_from = TRTPN, values_from=Nc)
    ) |>
    # Join to add 0's
    full_join(
      ord, by=c('AVISITN', 'AVISIT', 'AVALC')
    ) |>
    # Fill the 0s
    ## There is a bug here that causes vctrs to fail.
    tidyr::replace_na(list(`0`=' 0       ', `54` = ' 0       ', `81`=' 0       ')) |>
    # Clean up the rows that should be blank
    mutate(
      `0` = ifelse(AVALC=='', '', `0`),
      `54` = ifelse(AVALC=='', '', `54`),
      `81` = ifelse(AVALC=='', '', `81`),
      AVISIT = ifelse(ord==0, AVISIT, '')
    ) |>
    # Sort
    arrange(AVISITN, ord)


  # P-values ----
  # !!! NOTE: To obtain the same p-value used in SAS for this display, a modification had to be made to the vcdExtra library.
  #           Please refer to this github issue: https://github.com/friendly/vcdExtra/issues/3
  #           And you can access our fork of the library here: https://github.com/mstackhouse/vcdExtra
  counts['p'] <- character(nrow(counts))

  counts[(counts$AVISITN==8 & counts$ord==0),'p'] <- cbic |>
    filter(AVISITN == 8) |>
    cmh_p(AVAL ~ TRTP | SITEGR1) |>
    num_fmt(digits=4, size=5, int_len=1)

  counts[(counts$AVISITN==16 & counts$ord==0),'p']  <- cbic |>
    filter(AVISITN == 16) |>
    cmh_p(AVAL ~ TRTP | SITEGR1) |>
    num_fmt(digits=4, size=5, int_len=1)

  counts[(counts$AVISITN==24 & counts$ord==0),'p'] <- cbic |>
    filter(AVISITN == 24) |>
    cmh_p(AVAL ~ TRTP | SITEGR1) |>
    num_fmt(digits=4, size=5, int_len=1)

  final <- bind_rows(column_headers, counts) |>
    select(AVISIT, AVALC, `0`,`54`,`81`, p)

  # Make the table ----
  ht <- huxtable::as_hux(final, add_colnames = FALSE) |>
    huxtable::set_bold(1, 1:ncol(final), TRUE) |>
    huxtable::set_align(1, 1:ncol(final), 'center') |>
    huxtable::set_align(1,2, 'left') |>
    huxtable::set_valign(1, 1:ncol(final), 'bottom') |>
    huxtable::set_bottom_border(1, 1:ncol(final), 1) |>
    huxtable::set_width(1.2) |>
    huxtable::set_escape_contents(FALSE) |>
    huxtable::set_col_width(c(1/8, 3/8, 1/8, 1/8, 1/8, 1/8))

  return(ht)
}


#' gen_ht_t14_401
#' Generate Table 14-4.01 Summary of Planned Exposure to Study Drug, as of End of Study
#'
#' @param adsl
#'
#' @return a huxtable
#' @export
#'
#' @examples
gen_ht_t14_401 <- function(adsl) {
  # Subset for completers
  adsl_complt <- adsl |>
    filter(COMP24FL == 'Y') |>
    select(TRT01P, TRT01PN, AVGDD, CUMDOSE) |>
    mutate(cat = 1, TRTPCD = paste(TRT01PN, '_C', sep=''))

  # Subset for safety
  adsl_safety <- adsl |>
    filter(SAFFL == 'Y') |>
    select(TRT01P, TRT01PN, AVGDD, CUMDOSE) |>
    mutate(cat = 2, TRTPCD = paste(TRT01PN, '_S', sep=''))

  # Stack the two together
  adsl_ = bind_rows(adsl_safety, adsl_complt)
  rm(adsl_safety, adsl_complt) # Clean-up

  # Header N counts and column headers
  header <- adsl_ |>
    group_by(TRTPCD, TRT01P, TRT01PN, cat) |>
    summarize(N = n()) |>
    mutate(
      labels = str_replace_all(str_wrap(glue('{TRT01P} (N={N})'), width=10), "\n", function(x) "\\line ")
    ) |>
    ungroup() |>
    arrange(cat, TRT01PN) |>
    select(TRTPCD, labels) |>
    tidyr::pivot_wider(names_from=TRTPCD, values_from=labels)

  # Calculate average daily dose summary stats ----
  avgdd <- adsl_ |> desc_stats(AVGDD, group=TRTPCD, int_len=5) |>
    mutate(rowlbl1 = 'Average daily dose (mg)')

  # Calculate cumulative dose at end of study ----
  cumdose <- adsl_ |> desc_stats(CUMDOSE, group=TRTPCD, int_len=5) |>
    mutate(rowlbl1 = 'Cumulative dose at end of study [2]')

  # Spanner - want this to be the top left cell of the cells that will merge
  spanner <- tibble(`0_C` = 'Completers at Week 24', `0_S` = 'Safety Population [1]')

  # Join it all together, order columns, clean grouped cells
  final <- bind_rows(spanner, header, avgdd, cumdose) |>
    select(rowlbl1, rowlbl2, `0_C`, `54_C`, `81_C`, `0_S`, `54_S`, `81_S`) |>
    group_by(rowlbl1) |>
    mutate(ord1 = row_number()) |>
    ungroup() |>
    mutate(rowlbl1 = ifelse(ord1 == 1, rowlbl1, "")) |>
    select(-ord1)

  ht <- huxtable::as_hux(final, add_colnames = FALSE) |>
    huxtable::merge_cells(1, 3:5) |>
    huxtable::merge_cells(1, 6:8)

  huxtable::bottom_border(ht)[1, 3] <- 1
  huxtable::bottom_border(ht)[1, 6] <- 1
  huxtable::bottom_border(ht)[2, ] <- 1
  huxtable::valign(ht)[1:2, ] <- 'bottom'
  huxtable::bold(ht)[1:2, ] <- TRUE
  huxtable::align(ht)[1:2, ] <- 'center'
  huxtable::width(ht) <- 1.5
  huxtable::escape_contents(ht) <- FALSE
  huxtable::col_width(ht) <- c(.36, .07, .1, .11, .11, .1, .11, .11)
  huxtable::bottom_padding(ht) <- 0
  huxtable::top_padding(ht) <- 0

  return(ht)
}


