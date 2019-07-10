
geo_EPCI_poly_FRMET <-
  creation_carto_supracomm(CARTO_COMM = COMMG_COG2016_METDOM,
                           CODE_COMMUNE = "INSEE_COM",
                           COG_IN = 2016,
                           COG_NIVGEO = 2018,
                           NIVGEO = "EPCI",
                           FORMAT = "poly",
                           SG = 0.05)


# stats ope mieux-disant par EPCI
EPCI_COUVMOB_4G_4OPE_012019.stat.FR.indics.max <-
  COMM_COUVMOB_4G_4OPE_072017.stat.FRMET.indics %>% mutate(an = 2017) %>%
  rbind.data.frame(COMM_COUVMOB_4G_4OPE_012019.stat.FRMET.indics %>% mutate(an = 2019) ) %>%
  rbind.data.frame(COMM_COUVMOB_4G_4OPE_012019.stat.FRDOM.indics %>% mutate(an = 2019)) %>%
  #rbind.data.frame(COMM_COUVMOB_4G_4OPE_012019.stat.FRDOM.indics) %>%
  ajout_nivgeo_supracomm(TABLE = . ,
                         CODE_COMMUNE = "CODE_INSEE",
                         NIVGEO = "EPCI",
                         COG_IN = 2016,
                         COG_NIVGEO = 2018) %>%
  group_by(EPCI,techno, OPE, an) %>%
  summarise_if(is.numeric, sum) %>%
  mutate(pct.superficie = superficie / TOT.superficie) %>%
  ungroup() %>%
  group_by(EPCI,techno, an) %>%
  # indicateur de l'opérateur mieux-disant par techno / par an
  filter(pct.superficie == max(pct.superficie)) %>%
  distinct(EPCI, .keep_all = TRUE) %>%
  ungroup() %>%
  select(EPCI,OPE,techno, an,  superf_tot = TOT.superficie, superf_opemax = superficie ) %>%
  mutate(pct_superf_opemax = superf_opemax / superf_tot) %>%
  ajout_libelles_nivgeo(TABLE = .,
                        NIVGEO_IN ="EPCI",
                        COG_NIVGEO = 2018) %>%
  select(EPCI, LIB_EPCI, an ,OPE, pct_superf_opemax, superf_tot) %>%
  mutate(an = as.character(an)) %>%
  mutate(OPE = factor(OPE, levels = c("ORF","SFR","BYT","FRE") ) ) %>%
  mutate(OPE = recode(OPE, "ORF" = "Orange", "SFR" = "SFR", "BYT" = "Bytel","FRE" = "Free"))





EPCI_COUVMOB_4G_4OPE_EVOL.stat.FRMET.indics.max <-
  EPCI_COUVMOB_4G_4OPE_012019.stat.FR.indics.max %>%
  select(EPCI, an, pct_superf_opemax) %>%
  spread(an , pct_superf_opemax) %>%
  rename(pct.superficie.012019 = `2019`,pct.superficie.072017 = `2017`) %>%
  mutate(diff.pct.superficie.072017_012019 = pct.superficie.012019 - pct.superficie.072017)



# fonction pour gérer accents
conv_accents <- function(x) {
  x <- gsub(pattern = "è", replacement = "&egrave;", x = x)
  x <- gsub(pattern = "é", replacement = "&eacute;", x = x)
  x <- gsub(pattern = "ê", replacement = "&ecirc;", x = x)
  x <- gsub(pattern = "ë", replacement = "&euml;", x = x)
  x <- gsub(pattern = "î", replacement = "&icirc;", x = x)
  x <- gsub(pattern = "ï", replacement = "&iuml;", x = x)
  x <- gsub(pattern = "û", replacement = "&ucirc;", x = x)
  x <- gsub(pattern = "ü", replacement = "&uuml;", x = x)
  x <- gsub(pattern = "ô", replacement = "&ocirc;", x = x)
  x <- gsub(pattern = "à", replacement = "&agrave;", x = x)
  x <- gsub(pattern = "â", replacement = "&acirc;", x = x)
  x <- gsub(pattern = "ç", replacement = "&ccedil;", x = x)
  x <- gsub(pattern = "è", replacement = "&Egrave;", x = x)
  x <- gsub(pattern = "é", replacement = "&Eacute;", x = x)
  x <- gsub(pattern = "ê", replacement = "&Ecirc;", x = x)
  x <- gsub(pattern = "ë", replacement = "&Euml;", x = x)
  x <- gsub(pattern = "î", replacement = "&Icirc;", x = x)
  x <- gsub(pattern = "ï", replacement = "&Iuml;", x = x)
  x <- gsub(pattern = "û", replacement = "&Ucirc;", x = x)
  x <- gsub(pattern = "ü", replacement = "&Uuml;", x = x)
  x <- gsub(pattern = "ô", replacement = "&Ocirc;", x = x)
  x <- gsub(pattern = "à", replacement = "&Agrave;", x = x)
  x <- gsub(pattern = "â", replacement = "&Acirc;", x = x)
  x <- gsub(pattern = "ç", replacement = "&Ccedil;", x = x)
  x <- gsub(pattern = "'", replacement = "&#39;", x = x)
  
  return(x)
}






# création carto ggiraph

library(ggiraph)
library(stringi)

ggi_choro <-
  ggplot() +
  geom_sf_interactive(data = geo_EPCI_poly_FRMET %>% 
                        full_join(EPCI_COUVMOB_4G_4OPE_012019.stat.FR.indics.max %>%
                                    filter(an %in% '2019') %>%
                                    left_join(EPCI_COUVMOB_4G_4OPE_EVOL.stat.FRMET.indics.max %>%
                                    select(EPCI,pct.superficie.012019, pct.superficie.072017 ), by = "EPCI"),
                                  by = c("EPCI" ="EPCI" ) ) %>%
                        # mutate(tip = paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                        #                     "<font size=2.5 color=white family=Roboto>","<b>",conv_accents(LIB_EPCI) ,"</b>","<br>",
                        #                     "superficie totale :", format(round(superf_tot / 1000000,-1), nsmall=0, big.mark=" "), " km2","<br>",
                        #                     "<b>",percent(pct_superf_opemax, accuracy = 0.01) ,"</b>"," de la superficie est couverte " , "<br>",
                        #                     "par ","<i>",OPE ,"</i>",", (operateur mieux-disant sur ce territoire)","</font>") ),
                        mutate(tip = paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                                            "<font size=2.5 color=white family=Roboto>","<b>",conv_accents(LIB_EPCI) ,"</b>","<br>",
                                            "Part de la surface couverte par l operateur mieux-disant","<br>",
                                            "janvier 2019 : ", "<b>",percent(pct.superficie.012019, accuracy = 0.1) ,"</b>","<br>",
                                            "juillet 2017 : ", "<b>",percent(pct.superficie.072017, accuracy = 0.1) ,"</b>",
                                            "</font>") ),
                      # mutate(tip = paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                      #                     "<font size=2.5 color=white family=Roboto>","<b>",conv_accents(LIB_EPCI) ,"</b>","<br>",
                      #                     "superficie totale :", format(round(superf_tot / 1000000,-1), nsmall=0, big.mark=" "), " km2","<br>",
                      #                     "<b>",percent(pct_superf_opemax, accuracy = 0.01) ,"</b>"," de la superficie est couverte " , "<br>","</font>") ),
                      aes(fill=cut(pct_superf_opemax*100, breaks = c(0,20,30,50,80,95,100.01)),
                          tooltip = tip,
                          data_id = EPCI), color = NA) +
  geom_sf(data = geo_DEP_poly_FRMETDOM, color = "grey65", fill = NA ,size = 0.05) +
  scale_fill_brewer(palette = "Reds", name = "%") +
  guides(fill = guide_legend(reverse=T)) +
  labs(
    title = "Part de la surface couverte en 4G",
    subtitle = "par l'opérateur mieux-disant, par commune",
    caption = "Source : Arcep, opérateurs mobiles, janvier 2019"
  ) +   
  theme_ipsum() + 
  theme(legend.position = "right",
        axis.line=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank() )+
  coord_sf(crs = st_crs(2154), datum = NA)



tooltip_css <- "background-color:#272B30;padding:2px;font-size: 120%;color: white;opacity:0.2"
#x <- girafe(ggobj = ggi_choro, width = 1, height_svg = 6 )
x <- girafe(ggobj = ggi_choro)
x <- girafe_options(x, 
                    opts_tooltip(use_fill = FALSE, offx = 10, offy = -10, use_cursor_pos = TRUE, css = tooltip_css),
                    opts_hover(css = "stroke:black;r:5pt;"),
                    opts_selection(type = "none"),
                    opts_zoom(max = 2),
                    opts_toolbar(position = "topright", saveaspng = FALSE) )

widgetframe::frameWidget(x)

## evolution 

ggi_choro_evol <-
ggplot() +
  geom_sf_interactive(data = geo_EPCI_poly_FRMET %>% 
                        full_join(EPCI_COUVMOB_4G_4OPE_012019.stat.FR.indics.max %>%
                                    filter(an %in% '2019'), 
                                  by = c("EPCI" ="EPCI" ) ) %>%
                        left_join(EPCI_COUVMOB_4G_4OPE_EVOL.stat.FRMET.indics.max, by = "EPCI") %>%
                        mutate(tip = paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                                            "<font size=2.5 color=white family=Roboto>","<b>",conv_accents(LIB_EPCI) ,"</b>","<br>",
                                            "superficie totale :", format(round(superf_tot / 1000000,-1), nsmall=0, big.mark=" "), " km2","<br>",
                                            "<b>",percent(pct_superf_opemax, accuracy = 0.01) ,"</b>"," de la superficie est couverte " , "<br>",
                                            "par ","<i>",OPE ,"</i>",", (operateur mieux-disant sur ce territoire)","</font>") ),
                      aes(fill=cut(diff.pct.superficie.072017_012019*100, breaks = c(-100,-50,-25,-15,-7,-3,0,3,7,15,25,50,100)),
                          tooltip = tip,
                          data_id = EPCI), color = NA) +
  geom_sf(data = geo_DEP_poly_FRMETDOM, color = "grey65", fill = NA ,size = 0.05) +
  scale_fill_manual(name = "%",
                    na.value = "grey",
                    values = c(rev(colorRampPalette(brewer.pal(8, "Reds"))(6)) ,colorRampPalette(brewer.pal(8, "Greens"))(6) )  , drop = FALSE) +
  guides(fill = guide_legend(reverse=T)) +
  labs(
    title = "Evolution de la part de la surface couverte en 4G par l'opérateur mieux-disant",
    subtitle = "Entre juillet 2017 et janvier 2019, par commune",
    caption = "Source : Arcep, opérateurs mobiles, janvier 2019"
  ) +   
  theme_ipsum() + 
  theme(legend.position = "right",
        axis.line=element_blank(),
        #plot.title = element_text(size = 10),
        axis.title=element_blank(),
        axis.text=element_blank() )+
  coord_sf(crs = st_crs(2154), datum = NA)


##############################
#### ggiraph avec 2 cartes liées

library(patchwork)
x <- girafe( code = print(ggi_choro + ggi_choro_evol + plot_layout(nrow  = 1, widths  = c(2, 2)) ),
             width_svg = 20, 
             height_svg = 10)
x <- girafe_options(x,
                    opts_hover(css = "fill:red;r:2.5pt;size:5pt;stroke:red;color:black;opacity:0.3") ,
                    #opts_hover(css = "fill:black;r:0.25pt;size:5;stroke:black") ,
                    opts_tooltip(use_fill = FALSE) , 
                    opts_toolbar(saveaspng = FALSE) )
#x
widgetframe::frameWidget(x,width = "100%")

###################################
####### evolution ope mieux disant par région



REG_COUVMOB_4G_4OPE_012019.stat.FR.indics.max <-
  COMM_COUVMOB_4G_4OPE_072017.stat.FRMET.indics %>% mutate(an = 2017) %>%
  rbind.data.frame(COMM_COUVMOB_4G_4OPE_012019.stat.FRMET.indics %>% mutate(an = 2019) ) %>%
  rbind.data.frame(COMM_COUVMOB_4G_4OPE_012019.stat.FRDOM.indics %>% mutate(an = 2019)) %>%
  #rbind.data.frame(COMM_COUVMOB_4G_4OPE_012019.stat.FRDOM.indics) %>%
  ajout_nivgeo_supracomm(TABLE = . ,
                         CODE_COMMUNE = "CODE_INSEE",
                         NIVGEO = "REG",
                         COG_IN = 2016,
                         COG_NIVGEO = 2018) %>%
  group_by(REG,techno, OPE, an) %>%
  summarise_if(is.numeric, sum) %>%
  mutate(pct.superficie = superficie / TOT.superficie) %>%
  ungroup() %>%
  group_by(REG,techno, an) %>%
  # indicateur de l'opérateur mieux-disant par techno / par an
  filter(pct.superficie == max(pct.superficie)) %>%
  distinct(REG, .keep_all = TRUE) %>%
  ungroup() %>%
  select(REG,OPE,techno, an,  superf_tot = TOT.superficie, superf_opemax = superficie ) %>%
  mutate(pct_superf_opemax = superf_opemax / superf_tot) %>%
  ajout_libelles_nivgeo(TABLE = .,
                        NIVGEO_IN ="REG",
                        COG_NIVGEO = 2018) %>%
  select(REG, LIB_REG, an ,OPE, pct_superf_opemax, superf_tot) %>%
  mutate(an = as.numeric(an)) %>%
  mutate(OPE = factor(OPE, levels = c("ORF","SFR","BYT","FRE") ) ) %>%
  mutate(OPE = recode(OPE, "ORF" = "Orange", "SFR" = "SFR", "BYT" = "Bytel","FRE" = "Free")) %>%
  filter(!LIB_REG %in% 'Guyane')


library(scales)
library(hrbrthemes)
library(directlabels)
library(ggrepel)

ggi_slope_reg <-
ggplot(data=REG_COUVMOB_4G_4OPE_012019.stat.FR.indics.max %>%
         left_join( REG_COUVMOB_4G_4OPE_012019.stat.FR.indics.max %>%
                     select(REG, an, pct_superf_opemax) %>%
                     spread(an , pct_superf_opemax) %>%
                     rename(pct.superficie.012019 = `2019`,pct.superficie.072017 = `2017`) %>%
                     mutate(diff.pct.superficie.072017_012019 = pct.superficie.012019 - pct.superficie.072017),
                    
                    by = "REG") %>%
         mutate(tip = paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                             "<font size=2.5 color=white family=Roboto>","<b>",conv_accents(LIB_REG) ,"</b>","<br>",
                            "en juillet 2017 : ", "<b>",percent(pct.superficie.072017, accuracy = 0.01) ,"</b>" , "<br>",
                            "en janvier 2019 : ","<b>",percent(pct.superficie.012019, accuracy = 0.01) ,"</b>","</font>") ),
       aes(x=an, y = pct_superf_opemax, 
           group = LIB_REG,
           #label=paste0( round(pct_ind * 100,0), "%") ,
           color = pct_superf_opemax  ) ) +
  #geom_line(size = 1.6) +
  geom_line_interactive(aes(data_id = REG, tooltip = tip ), 
                        size = 1.6) +
  
  # geom_dl(aes(label = paste0( round(pct.superficie * 100,0), "%")), 
  #         method = list(dl.combine("first.points", "last.points"),
  #                                             cex = 1.5)) +
  # geom_text(data=REG_COUVMOB_4G_4OPE_012019.stat.FR.indics.max %>% filter(an ==2017),
  #           aes(x=an, y = pct_superf_opemax,label=LIB_REG),
  #           #position = position_stack(vjust = 0.5),
  #           #hjust = 2,
  #           size = 3) +
  geom_text_repel(data=REG_COUVMOB_4G_4OPE_012019.stat.FR.indics.max %>% filter(an ==2017),
                  aes(x=an, y = pct_superf_opemax,
                      color = pct_superf_opemax,
                      label=LIB_REG),
    size = 3,
    segment.color = NA,
    nudge_x      = -0.15,
    direction    = "y",
    hjust        = 1,
    segment.size = 0
  ) +
  geom_text(data=REG_COUVMOB_4G_4OPE_012019.stat.FR.indics.max %>% filter(an ==2017),
            aes(x=an, y = pct_superf_opemax,
                color = pct_superf_opemax,
                label=paste0( round(pct_superf_opemax * 100,0), "%")),
            #position = position_stack(vjust = 0.5),
            hjust = 1,
            size = 2) +
  geom_text(data=REG_COUVMOB_4G_4OPE_012019.stat.FR.indics.max %>% filter(an ==2019),
            aes(x=an, y = pct_superf_opemax,
                color = pct_superf_opemax,
                label=paste0( round(pct_superf_opemax * 100,0), "%")),
            #position = position_stack(vjust = 0.5),
            hjust = -0.3,
            size = 2) +
  # geom_dl(data=REG_COUVMOB_4G_4OPE_012019.stat.FR.indics.max %>% filter(an ==2017),
  #         aes(label = LIB_REG),
  #         method = list(dl.combine("first.points"),cex = 0.5)) +
  scale_y_continuous(labels = percent_format(accuracy = 1), name = '') +
  scale_x_continuous(name = '', limits = c(2016.3,2019), breaks = c(2017,2019), expand=c(0, 0.5)) +
  scale_color_distiller(palette = "RdBu", direction = 1) +
 # scale_color_manual(values=c( "#84312D","#FD817F","#9DC86C", "#439B45"), name = 'Type de densité',guide=guide_legend(reverse=F)) +
  theme_ipsum() +
  theme(axis.text.y = element_blank(),
        legend.position = "none",
        strip.text.x = element_text(face = "bold"),
        strip.background = element_rect(color = "grey"),
        panel.grid = element_line(linetype = "longdash")) +
  #coord_flip() +
  labs(
    title = "Part de la surface couverte en 4G par l'opérateur mieux-disant",
    subtitle = "par région administrative",
    caption = "Source : Arcep, opérateurs mobiles, juillet 2017/janvier 2019"
  )



tooltip_css <- "background-color:#272B30;padding:2px;font-size: 120%;color: white;opacity:0.2"
#x <- girafe(ggobj = ggi_choro, width = 1, height_svg = 6 )
x <- girafe(ggobj = ggi_slope_reg)
x <- girafe_options(x, 
                    opts_tooltip(use_fill = FALSE, offx = 10, offy = -10, use_cursor_pos = TRUE, css = tooltip_css),
                    opts_hover(css = "stroke:black;r:5pt;"),
                    opts_selection(type = "none"),
                    opts_zoom(max = 2),
                    opts_toolbar(position = "topright", saveaspng = FALSE) )

widgetframe::frameWidget(x)
