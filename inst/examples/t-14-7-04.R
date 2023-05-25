### Table 14-7.04: Summary of Concomitant Medications (Number of Subjects)

here::i_am("inst/examples/t-14-7-01.R")
library(here)
library(datarx.pilotADaM)
library(pharmaRTF)


ht <- gen_ht_t14_704(sdtm_cm, adam_adsl)


# Write into doc object and pull titles/footnotes from excel file
doc <- rtf_doc(ht) |> titles_and_footnotes_from_df(
  from.file = here::here("inst", "examples", "titles.xlsx"),
  reader=example_custom_reader,
  table_number='14-7.04') |>
  set_font_size(10) |>
  set_ignore_cell_padding(TRUE) |>
  set_header_height(1) |>
  set_column_header_buffer(1,0) |>
  set_footer_height(1)

write_rtf(doc, file=here::here("output", "14-7.04.rtf"))

