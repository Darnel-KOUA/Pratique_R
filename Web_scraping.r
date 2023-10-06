# Library pour extraires les pages web
install.packages("tidyverse")
install.packages("rvest")
install.packages("lubridate")
install.packages("progress")


library(lubridate)
library(rvest)
library(tidyverse)
library(dplyr)
library(tibble)
library(stringi)
library(progress)
#Création de la fonction getPays

getPays <- function(url) { # nolint
  base <- "https://www.historique-meteo.net"
  data <- tibble(Lien = character(), Nom_pays = character())
  
  page_meteo <- read_html(url)
  resultats <- page_meteo %>% html_elements("li.item-thumbs")
  for (res in resultats){
    link <- res %>% html_element("a") %>% html_attr("href")
    link_pays <- paste(base, link, sep = "")
    name <- res %>% html_element("div") %>% html_text()
    data <- rbind(data, c(link_pays, name))
  }
  # Noms des colonnes
  colnames(data) <- c("Lien", "Nom_Pays")
  
  # Enregistrer les données sous format CSV
  write.csv(data, file = "pays_afrique.csv", row.names = FALSE)
  
  # Il renvoit un dataframe
  return(data)
}

#test
url <- "https://www.historique-meteo.net/afrique"
test <- getPays(url)
head(test)

# Création de la fonction getRegion

getRegion <- function(url_pays) { #nolint
  base <- "https://www.historique-meteo.net"
  data <- list() # nolint
  
  i <- 0
  page_meteo <- read_html(url_pays)
  region_baliz <- page_meteo %>% html_elements("div.list-group")
  region <- region_baliz[2] %>% html_elements("a")
  for (reg in region) {
    i <- i + 1
    link <- reg %>% html_attr("href")
    link_region <- paste(base, link, sep = "")
    name_region <- reg %>% html_element("b") %>% html_text()
    data[[i]] <- c(link_region, name_region)
  }
  
  #retourne un une liste
  return(data)
}

#Test
url <- "https://www.historique-meteo.net/afrique/congo"
region <- getRegion(url)
View(region)

#Une fonction pour convertir les strings en format nom_var

slugify <- function(input_string) {
  # Convertir la chaîne en minuscules
  input_string <- tolower(input_string)
  
  # Remplacer les caractères accentués par leurs équivalents non accentués
  slug <- stringi::stri_trans_general(input_string, "Latin-ASCII")
  
  # Remplacer les caractères spéciaux par des tirets
  slug <- gsub("[^a-z0-9]+", "-", slug)
  
  # Supprimer les tirets en début et fin de chaîne
  slug <- gsub("^-+|-+$", "", slug)
  
  # Retourner le slug résultant
  return(slug)
}


# Création de la fonction getDataDay

getDataDay <- function(url) {#nolint
  page_meteo <- read_html(url)
  day <- page_meteo %>% html_element("div.col-lg-8")
  
  # On recupère le tableau qui contient les données
  table <- day %>% html_table()
  kpi <- slugify(table$X1[-c(1, length(table$X1))])
  val <- table$X4[-c(1, length(table$X4))]
  valeur <- gsub(paste0("[", "°km/h%ChPa", "]"), "", val)
  data <- list(kpi, valeur) # nolint
  
  #Retourner une liste
  return(data)
}

# Test
url <- "https://www.historique-meteo.net/afrique/cote-d-ivoire/abidjan/2023/01/12/" # nolint
data <- getDataDay(url)
View(data)

#Création de la fonction getData

getData <- function(url, years) { # nolint
  nbre_mois = c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12') # nolint
  data <- tibble() #Le DataFrame qui va recevoir les donnees
  
  #KPI de base
  KPI <- c("temperature-maximale", "temperature-minimale", "vitesse-du-vent", "temperature-du-vent", "precipitations", # nolint
           "humidite", "visibilite", "couverture-nuageuse", "indice-de-chaleur", "point-de-rosee", "pression", "heure-du-lever-du-soleil",  # nolint
           "heure-du-coucher-du-soleil", "duree-du-jour") # nolint
  
  total <- length(getRegion(url))
  pb <- progress_bar$new(
    format = "  Chargement des données [:bar] :percent in :elapsed", # nolint
    total = total, clear = FALSE, width = 100)
  
  for (reg in getRegion(url)) {
    pb$tick()
    Sys.sleep(1 / total)
    
    for (year in years) {
      for (mois in nbre_mois) {
        date <- ymd(paste(year, mois, "01", sep = "-"))
        nbr_jours <- days_in_month(date) # nolint
        for (jour in 1:nbr_jours) {
          
          if (nchar(as.character(jour)) == 1) {
            j <- paste(0, jour, sep = "")
            date <- paste(year, mois, j, sep = "/")
            link <- paste(reg[1], date, sep = "")
            valeur <- unlist(getDataDay(link)[2])
            kpi <- unlist(getDataDay(link)[1]) # nolint
          }else {
            date <- paste(year, mois, jour, sep = "/")
            link <- paste(reg[1], date, sep = "")
            valeur <- unlist(getDataDay(link)[2])
            kpi <- unlist(getDataDay(link)[1])
          }
          
          # y a des KPI qui ne sont pas prelevés pour tous les jours.
          # L'idée c'est de recuperer la liste des kpi la plus longue et # nolint
          # Affecter "None" si la variable n'est pas renseignée.
          
          if (length(kpi) >= length(KPI)) {
            KPI <- kpi # nolint
          }else {
            for (i in KPI) {
              if (!(i %in% kpi) == TRUE) {
                index_kpi <- which(KPI == i)
                valeur <- append(valeur, NA, after = index_kpi - 1) # nolint
              }
            }
          }
          
          ligne <- c(date, reg[2], valeur)
          data <- rbind(data, ligne)
          #break
        }
        # Pour l'annee en cours s'arreter en juin
        if (year == 2023 && mois == "06") {
          break
        }
        #break
      }
      #break
    }
    break
  }
  
  #Nom des colonnes du DataFrame
  colonnes <- c("date", "region", KPI)
  colnames(data) <- colonnes
  
  #Convertir certainnes  colonnea en type numerique
  var_num = c("temperature-maximale", "temperature-minimale", "vitesse-du-vent", "temperature-du-vent", "precipitations", # nolint
              "humidite", "visibilite", "couverture-nuageuse", "indice-de-chaleur", "point-de-rosee", "pression") # nolint
  for (var in var_num) {
    data[[var]] <- as.numeric(data[[var]])
  }
  
  #Enregistrer les data dans un fichier
  write.csv2(data, file = "meteo_nom_pays.csv", row.names = FALSE)
  
  #retourne un data frame
  return(data)
}


# Test
url <- "https://www.historique-meteo.net/afrique/gabon"
years <- c(2023)
data <- getData(url, years)
View(data)
data %>%
  group_by(date) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

