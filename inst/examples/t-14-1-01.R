# t-14-1-01.R
#   CDISC Pilot Table 14-1.01

here::i_am("inst/examples/t-14-1-01.R")
library(here)
library(datarx.pilotADaM)
library(pharmaRTF)

ht <- gen_ht_t14_101(adam_adsl)

# Write into doc object and pull titles/footnotes from excel file
doc <- rtf_doc(ht) |> titles_and_footnotes_from_df(
  from.file=here::here("inst","examples", "titles.xlsx"),
  reader=example_custom_reader,
  table_number='14-1.01') |>
  set_font_size(10) |>
  set_ignore_cell_padding(TRUE) |>
  set_column_header_buffer(top=1)

# Write out the RTF
write_rtf(doc, file=here::here("output", '14-1.01.rtf'))
