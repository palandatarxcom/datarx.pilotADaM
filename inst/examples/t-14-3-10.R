# t-14-3-09.R
#   CDISC Pilot Table 14-3.10

here::i_am("inst/examples/t-14-3-10.R")
library(here)
library(pharmaRTF)
library(datarx.pilotADaM)


ht <- gen_ht_t14_310(adam_adadas)

# Write into doc object and pull titles/footnotes from excel file
doc <- rtf_doc(ht, header_rows=2) |> titles_and_footnotes_from_df(
  from.file = here::here("inst", "examples", "titles.xlsx"),
  reader=example_custom_reader,
  table_number='14-3.10') |>
  set_font_size(10) |>
  set_ignore_cell_padding(TRUE) |>
  set_column_header_buffer(top=1)

# Write out the RTF
write_rtf(doc, file=here::here("output", "14-3.10.rtf"))

