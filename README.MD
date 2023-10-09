# WEBSCRAPING SUR R

Le web scraping est devenu un outil inestimable pour les professionnels de la statistique et du machine learning, permettant d'accéder rapidement à de vastes quantités de données à partir de sites Web et de les transformer en informations exploitables. Dans ce travail de machine learning 2, nous allons nous concentrer sur le web scraping d'un site de météo pour collecter des données météorologiques en temps réel à partir de différentes sources en ligne. L'objectif de cette étude est d'exploiter ces données afin d'analyser la dynamique d'évolution de certains KPI pour diverses localités.

Ce projet est divisé en trois parties:

-   Dans première partie nous construisons deux fonctions, une appelée **getPays()** qui prend en argument l'url d'un continent et renvois les url des pays et leur nom et l'autre appelée **getRegion()** qui pour un pays donné renvoit les url et noms des regions du pays.

-   Dans la deuxième partie nous construisons une fonction nommée **getDayData()** qui collecte tous les KPI disponibles sur la plateforme pour une région donnée.

-   Enfin dans la troisième partie, nous contruisons la fonction **getData** qui prend en paramètre l'url pays, les années pour lesquelles l'on souhaite avoir les données et fournit un dataframe qui contient l'ensemble des KPIs pour l'ensenble des régions du pays choisi tout en enregistrant les données receuillis sous format csv.
