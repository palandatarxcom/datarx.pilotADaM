### Table 14-7.03 pg 148 Summary of Weight Change from Baseline at End of Treatment


here::i_am("inst/examples/t-14-7-03.R")
library(here)
library(datarx.pilotADaM)
library(pharmaRTF)





ht <- gen_ht_t14_703(adam_advs, adam_adsl)


# Write into doc object and pull titles/footnotes from excel file
doc <- rtf_doc(ht) |> titles_and_footnotes_from_df(
  from.file = here::here("inst", "examples", "titles.xlsx"),
  reader=example_custom_reader,
  table_number='14-7.03') |>
  set_font_size(10) |>
  set_ignore_cell_padding(TRUE) |>
  set_header_height(1) |>
  set_column_header_buffer(1,0)

write_rtf(doc, file=here::here("output", "14-7.03.rtf"))

