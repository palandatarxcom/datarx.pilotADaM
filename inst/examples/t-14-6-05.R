## 14-6.05

here::i_am("inst/examples/t-14-6-05.R")
library(here)
library(pharmaRTF)
library(datarx.pilotADaM)



ht <- gen_ht_t14_605(adam_adlbc, adam_adlbh, adam_adsl)

# Write into doc object and pull titles/footnotes from excel file
doc <- rtf_doc(ht, header_rows = 3) |>
  titles_and_footnotes_from_df(
    from.file = here::here("inst", "examples", "titles.xlsx"),
    reader=example_custom_reader,
    table_number='14-6.05') |>
  set_font_size(10) |>
  set_ignore_cell_padding(TRUE) |>
  set_column_header_buffer(top = 1) |>
  set_header_height(1) |>
  set_footer_height(1.3)

# Write out the RTF
write_rtf(doc, file=here::here("output", "14-6.05.rtf"))

