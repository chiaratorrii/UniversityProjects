library(tidyverse)
library(readxl)
library(rpart)
library(rpart.plot)
library(tidymodels)
library(randomForest)
library(rgdal)
library(tmap)
library(sf)
library(tmaptools)
library(extrafont)



bes <- read_excel("Indicatori_per_regione_sesso.xlsx") %>% 
  mutate(SCORE = as.numeric(SCORE))

# -- scelta indicatori
bes = bes %>% 
  filter(SESSO == "Totale") %>% 
  filter(!TERRITORIO %in% c("Nord", "Centro", "Mezzogiorno", "Italia")) %>% 
  filter(INDICATORE %in% c(
    "Indice di salute mentale (SF36)",
    "Eccesso di peso",
    "Fumo",
    "Alcol",
    "Sedentarietà",
    "Uscita precoce dal sistema di istruzione e formazione",
    "Giovani che non lavorano e non studiano (NEET)",
    "Partecipazione alla formazione continua",
    "Competenze digitali elevate",
    "Partecipazione culturale fuori casa",
    "Trasformazioni da lavori instabili a lavori stabili",
    "Dipendenti con bassa paga",
    "Ricchezza netta media pro capite",
    "Reddito disponibile lordo pro capite",
    "Soddisfazione per il lavoro svolto",
    "Rischio di povertà",
    "Povertà assoluta",
    "Soddisfazione per le relazioni familiari",
    "Soddisfazione per le relazioni amicali",
    "Partecipazione sociale",
    "Partecipazione civica e politica",
    "Partecipazione elettorale",
    "Soddisfazione per la propria vita",
    "Giudizio negativo sulle prospettive future"
  )) %>% 
  filter(PERIODO == "indicatore annuale") %>% 
  select(-c(SESSO, UNITA_MISURA))

# -- wider + variabili numeriche
bes = bes %>% 
  pivot_wider(names_from = INDICATORE, values_from = SCORE) %>% 
  select(c(
    TERRITORIO,
    `Giovani che non lavorano e non studiano (NEET)`,
    "Reddito disponibile lordo pro capite",
    "Soddisfazione per la propria vita",
    "Partecipazione alla formazione continua",
    "Dipendenti con bassa paga",
    "Partecipazione civica e politica",
    "Indice di salute mentale (SF36)"
  )) %>% 
  rename(neet = `Giovani che non lavorano e non studiano (NEET)`)  %>% 
  filter(!TERRITORIO %in% c("Bolzano/Bozen", "Trento"))

bes = bes %>% 
  arrange(TERRITORIO) %>% 
  mutate(Regione = c(
    "ABR",
    "BAS",
    "CAL",
    "CAM",
    "ER",
    "FVG",
    "LAZ",
    "LIG",
    "LOM",
    "MAR",
    "MOL",
    "PIE",
    "PUG",
    "SAR",
    "SIC",
    "TOS",
    "TAA",
    "UMB",
    "VA",
    "VEN"
  ),
  Area= c(
    "Sud",
    "Sud",
    "Sud",
    "Sud",
    "Nord",
    "Nord",
    "Centro",
    "Nord",
    "Nord",
    "Centro",
    "Sud",
    "Nord",
    "Sud",
    "Sud",
    "Sud",
    "Centro",
    "Nord",
    "Centro",
    "Nord",
    "Nord"
  ))

bes$Area=as.factor(bes$Area)

cor = data.frame(corr = NA,
                 var = NA)
for (i in 1:ncol(bes)) {
  cor[i, 1] = cor(bes$neet, bes[, i])
  cor[i, 2] = colnames(bes[i])
}

cor = cor %>% 
  arrange(desc(corr))

cor = data.frame(cor,
                 area = c(
                   "Istr e form",
                   "Ben econ",
                   "Lavoro",
                   "Salute",
                   "Istr e form",
                   "Salute",
                   "Salute",
                   "Ben soggettivo",
                   "Salute",
                   "Ben soggettivo",
                   "Lavoro",
                   "Salute",
                   "Lavoro",
                   "Rel sociali",
                   "Rel sociali",
                   "Istr e form",
                   "Politica",
                   "Rel sociali",
                   "Rel sociali",
                   "Istr e form",
                   "Ben econ",
                   "Istr e form"
                 ))


cor2 = cor %>% 
  filter(var %in% c(
    "Reddito disponibile lordo pro capite",
    "Soddisfazione per la propria vita",
    "Partecipazione alla formazione continua",
    "Dipendenti con bassa paga",
    "Partecipazione civica e politica",
    "Indice di salute mentale (SF36)"
  ))


# -- scatter
theme_set(theme_minimal())
loadfonts(device = "win", quiet = T)
theme_update(text = element_text(family = "Century Gothic",size = 12))

bes %>% 
  ggplot(aes(`Dipendenti con bassa paga`, neet)) +
  geom_point(aes(col=Area), show.legend = F, size=4.5) +
  labs(x="Dipendenti con bassa paga (%)",
       y="NEET (%)") +
  scale_color_manual(values=c("chartreuse3", "gold2", "steelblue3"))+
  geom_text(aes(label=Regione), nudge_x = 0.8, nudge_y = 0, check_overlap = F,
             size=2.4)
             
  
bes %>% 
  ggplot(aes(`Indice di salute mentale (SF36)`, neet)) +
  geom_point(aes(col=Area), show.legend = F, size=4.5) +
  labs(x="Indice di salute mentale (0-100)",
       y="NEET (%)") +
  scale_color_manual(values=c("chartreuse3", "gold2", "steelblue3"))+
  geom_text(aes(label=Regione), nudge_x = 0.3, nudge_y = 0, check_overlap = F,
            size=2.4)   

bes %>% 
  ggplot(aes(`Soddisfazione per la propria vita`, neet)) +
  geom_point(aes(col=Area), show.legend = F, size=4.5) +
  labs(x="Soddisfazione per la propria vita (%)",
       y="NEET (%)") +
  scale_color_manual(values=c("chartreuse3", "gold2", "steelblue3"))+
  geom_text(aes(label=Regione), nudge_x = 1.5, nudge_y = 0, check_overlap = F,
            size=2.4)

bes %>% 
  ggplot(aes(`Partecipazione alla formazione continua`, neet)) +
  geom_point(aes(col=Area), show.legend = F, size=4.5) +
  labs(x="Partecipazione alla formazione continua (%)",
       y="NEET (%)") +
  scale_color_manual(values=c("chartreuse3", "gold2", "steelblue3"))+
  geom_text(aes(label=Regione), nudge_x = 0.3, nudge_y = 0, check_overlap = F,
            size=2.4)

bes %>% 
  ggplot(aes(`Partecipazione civica e politica`, neet)) +
  geom_point(aes(col=Area), show.legend = F, size=4.5) +
  labs(x="Partecipazione civica e politica (%)",
       y="NEET (%)") +
  scale_color_manual(values=c("chartreuse3", "gold2", "steelblue3"))+
  geom_text(aes(label=Regione), nudge_x = 1, nudge_y = 0, check_overlap = F,
            size=2.4)

bes %>% 
  ggplot(aes(`Reddito disponibile lordo pro capite`, neet)) +
  geom_point(aes(col=Area), show.legend = F, size=4.5) +
  labs(x="Reddito disponibile lordo pro capite (€)",
       y="NEET (%)") +
  scale_color_manual(values=c("chartreuse3", "gold2", "steelblue3"))+
  geom_text(aes(label=Regione), nudge_x =500, nudge_y = 0, check_overlap = F,
            size=2.4)




# -- heat map
regions = readOGR("Reg_2016_WGS84_g.shp")
plot(regions)

regions@data

bes = bes %>% 
  mutate(TERRITORIO = 
           case_when(
             TERRITORIO  == "Trentino-Alto Adige/Südtirol" ~ "Trentino-Alto Adige",
             TERRITORIO == "Friuli-Venezia Giulia" ~ "Friuli Venezia Giulia",
             TERRITORIO == "Valle d'Aosta/Vallée d'Aoste" ~ "Valle D'Aosta",
             TRUE ~ TERRITORIO 
           )
  )


regions@data = left_join(regions@data,
                         bes,
                         by = c("REGIONE" = "TERRITORIO"))
view(regions@data)


# - mappa finale
tm_shape(regions) +
  tm_borders() +
  tm_fill("neet", palette = "Reds",
          breaks = seq(10,40,5),
          style = "pretty",
          legend.is.portrait = F) +
  tm_layout(main.title="NEET per regione (%)",
            main.title.size = 1.3,
            main.title.fontfamily = "Century Gothic",
            main.title.position = "center",
            legend.outside = T,
            legend.outside.position ="bottom",
            legend.outside.size = 0.1,
            legend.title.size = 0.01,
            legend.text.size = 0.5,
            legend.text.fontfamily = "Century Gothic",
            frame=F)

tm_shape(regions) +
  tm_borders() +
  tm_fill("neet", palette = "Reds",
          breaks = seq(10,40,5),
          style = "cont",
          legend.is.portrait = F) +
  tm_layout(main.title="Neet (%) per regione",
            main.title.size = 1.3,
            main.title.fontfamily = "Calibri",
            main.title.position = "center",
            legend.outside = T,
            legend.outside.position ="bottom",
            legend.outside.size = 0.15,
            legend.title.size = 0.01,
            legend.text.size = 4,
            frame=F)
  
