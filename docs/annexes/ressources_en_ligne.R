
# cartes 4G agence du numérique
# https://www.amenagement-numerique.gouv.fr/accord-mobile

# export pdf
# https://github.com/yihui/xaringan/wiki/Export-Slides-to-PDF

library(webshot)
#install_phantomjs()

file_name <- paste0("file://", normalizePath("./docs/index.html"))
# export pdf
webshot(file_name, "./docs/pdf/presentation_couvnum_29042019.pdf")
webshot("https://observatoire-territoires.github.io/couvnum/#10", "./docs/pdf/presentation_couvnum_29042019.pdf")
webshot(url = "https://observatoire-territoires.github.io/couvnum/#5", file = "./docs/pdf/widget.png")

webshot("./docs.index.html", "./docs/pdf/presentation_couvnum_29042019.pdf")

