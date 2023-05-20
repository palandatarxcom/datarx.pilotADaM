# t-14-1-03.R
#   CDISC Pilot Table 14-1.03

here::i_am("inst/examples/t-14-1-03.R")
library(here)
library(datarx.pilotADaM)
library(pharmaRTF)

ht <- gen_ht_t14_103(adam_adsl)

doc <- rtf_doc(ht, header_rows = 2) %>% titles_and_footnotes_from_df(
  from.file=here::here("inst", "examples", "titles.xlsx"),
  reader=example_custom_reader,
  table_number='14-1.03') %>%
  set_font_size(10) %>%
  set_ignore_cell_padding(TRUE) %>%
  set_column_header_buffer(top = 1)

write_rtf(doc, file=here::here("output", "14-1.03.rtf"))

