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

  # Calculate the header Ns
  header_n <- get_header_n(adsl_)

  # Column headers
  column_headers <- header_n |> select(TRT01PN, labels) |>
    tidyr::pivot_wider(names_from = TRT01PN, values_from = labels)

  # Intent to treat
  itt <- sum_subgrp(adsl_, ITTFL, order_var=STUDYID, include.n=FALSE, header_n = header_n) |>
    mutate(rowlbl1 = "Intent-To-Treat (ITT)")

  # Safety
  safety <- sum_subgrp(adsl_, SAFFL, order_var=STUDYID, include.n=FALSE, header_n = header_n) |>
    mutate(rowlbl1 = "Safety")

  # Efficacy
  efficacy <- sum_subgrp(adsl_, EFFFL, order_var=STUDYID, include.n=FALSE, header_n = header_n) |>
    mutate(rowlbl1 = "Efficacy")

  # Commpleters Week 24
  compl_24 <- sum_subgrp(adsl_, COMP24FL, order_var=STUDYID, include.n=FALSE, header_n = header_n) |>
    mutate(rowlbl1 = "Complete Week 24")

  # Study completers
  compl <- sum_subgrp(adsl_, COMPL, order_var=STUDYID, include.n=FALSE, header_n = header_n) |>
    mutate(rowlbl1 = "Complete Study")

  # Pull the body together
  body <- rbind(itt, safety, efficacy, compl_24, compl) |>
    filter(rowlbl2 == "Y") |>
    select(-rowlbl2)

  # Cleanup
  # rm(itt, safety, efficacy, compl_24, compl)

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

  return(ht)
}

#' gen_ht_t14_102
#' Generate Table 14-1.02 Summary of End of Study Data
#'
#' @param adsl ADaM ADSL data set
#'
#' @return
#' @export
#'
#' @examples
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

  #### Completion Status Table
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

  #### Reason for Early Termination Table
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
  # Rename to get rid of period seperation
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

