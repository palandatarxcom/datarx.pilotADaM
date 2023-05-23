# t-14-4-01.R
#   CDISC Pilot Table 14-4.01

here::i_am("inst/examples/t-14-4-01.R")
library(here)
library(pharmaRTF)
library(datarx.pilotADaM)


ht <- gen_ht_t14_401(adam_adsl)

# Write into doc object and pull titles/footnotes from excel file
doc <- rtf_doc(ht, header_rows = 2) |> titles_and_footnotes_from_df(
  from.file = here::here("inst", "examples", "titles.xlsx"),
  reader = example_custom_reader,
  table_number = '14-4.01') |>
  set_column_header_buffer(top=1) |>
  set_font_size(10)

# Write out the RTF
write_rtf(doc, file=here::here("output", "14-4.01.rtf"))
