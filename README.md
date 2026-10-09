# Voorspelling huizenprijzen Nederland

# Projectomschrijving

Dit project onderzoekt de ontwikkeling van huizenprijzen en inkomens in Nederland.
Met behulp van historische gegevens van het CBS, De Nederlandsche Bank (DNB) en 
Allescijfers.nl wordt gekeken naar de relatie tussen woningprijzen, inkomens,
hypotheekrentes en bevolkingsgroei.

Op basis van historische gegevens uit de periode 2009 tot en met 2024 wordt een 
voorspelling gemaakt van de woningmarkt tot en met 2030.

# Ontwikkelomgeving

Dit project is ontwikkeld in:

R versie 4.5.1
RStudio
Programmeertaal:

R

# Projectstructuur

# Rapport_voorspelling.Rmd

Het R Markdown-bestand waarmee het uiteindelijke rapport wordt gegenereerd. 
Dit bestand bevat zowel de tekstuele toelichting als de code die nodig is om de 
analyses te reproduceren.

# scripts/

Deze map bevat alle scripts die gebruikt worden binnen het project.
Het bestand main.R voert de scripts in de juiste volgorde uit:

01_import.R
02_cleaning.R
03_eda.R
04_analyse.R
05_linear_model.R
Deze scripts verzorgen onder andere:

Data-import
Data cleaning
Imputatie van ontbrekende waarden
Exploratory Data Analysis (EDA)
Modellering
Forecasting

# data
Deze map bevat zowel de ruwe als de verwerkte datasets die binnen het project worden gebruikt.

Rapport_voorspelling.pdf
Het definitieve rapport dat is gegenereerd vanuit het R Markdown-bestand.

Gebruikte methoden
Binnen dit project zijn onder andere de volgende methoden toegepast:

Data cleaning
MissForest imputatie
Exploratory Data Analysis (EDA)
Correlatieanalyse
Variance Inflation Factor (VIF)
Lineaire regressie
ARIMA forecasting
Analyse van woningbetaalbaarheid
Belangrijkste bevindingen
De analyse laat zien dat de huizenprijzen in Nederland de afgelopen jaren aanzienlijk sneller zijn gestegen dan de inkomens.

Het gebruikte voorspellingsmodel suggereert dat deze ontwikkeling zich mogelijk voortzet richting 2030, waardoor de betaalbaarheid van koopwoningen verder onder druk komt te staan.

Databronnen
CBS StatLine
De Nederlandsche Bank (DNB)
Allescijfers.nl
Opmerking
Het uitvoeren van alle scripts en het genereren van het rapport kan enkele minuten duren, afhankelijk van de beschikbare hardware.