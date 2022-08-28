# Code to prepare palettes data file
london_palettes <- list(
  Cholmondeley = list(
    palette = c("#241e1e", "#6a6463", "#605251", "#832b13", "#988364", "#bab3ac","#d1cfd1"),
    colourblind = TRUE),
  Copley = list(
    palette = c("#3d2c24", "#8a1b17", "#c33423", "#6f7274", "#7c9284", "#ece9eb"),
    colourblind = FALSE),
  Blake = list(
    palette = c("#284552", "#889463", "#dbca88", "#c08a74", "#e0cead"),
    colourblind = FALSE),
  Turner = list(
    palette = c("#31271d", "#564a34", "#555238", "#7c582e", "#917a47", "#cbbd8e"),
    colourblind = FALSE),
  Millais = list(
    palette = c("#2e4040", "#24325c", "#7b2223", "#bdb08f", "#938638", "#4f6b39", "#5d733e", "#617b5e"),
    colourblind = FALSE),
  Sargent = list(
    palette = c("#155a86", "#b8b99c", "#ccb8ba", "#e0a45d", "#e29871", "#b99bc1", "#bc708a", "#aa6b41"),
    colourblind = FALSE),
  Waterhouse = list(
    palette = c("#2b312f", "#5c4136", "#af9549", "#a48a7d", "#c1baa8", "#60353b", "#9e2f37"),
    colourblind = FALSE),
  Grant = list(
    palette = c("#9e6952", "#d1b083", "#e2cfaf", "#9ba791", "#939c77", "#909296"),
    colourblind = FALSE),
  Lowry = list(
    palette = c("#696b68", "#d1c9be", "#bb9e8b", "#a32c2b"),
    colourblind = FALSE),
  Bacon = list(
    palette = c("#9b5531", "#d15b13", "#c0b09d", "#403734"),
    colourblind = TRUE),
  Hockney = list(
    palette = c("#e3d4bf", "#cdada2", "#49754c", "#579fb8", "#e2e2e0"),
    colourblind = FALSE)
)

usethis::use_data(london_palettes, internal = TRUE, overwrite = TRUE)
