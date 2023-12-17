# zadanie 1 ------------------------------------------------------------------------------------------------------------
# 1. Załaduj dane Fleiss1993bin znajdujące się w bibliotece meta
library(meta)
data(Fleiss1993bin)
head(Fleiss1993bin)
# tutaj mamy zmienne binarne, w liscie 4 mieliśmy zmienne ciągłe

# zadanie 2 -------------------------------------------------------------------------------------------------------------
# Przeprowadź meta analizę jako miary efektu używając ilorazu szans, ryzyka 
# względnego oraz różnicy ryzyk. Porównaj otrzymane wyniki.
#?metabin

# generalnie w metaanalizie mierzymy efekt jaki wynika z każdej z publikacji
# można stosować różne miary pomiaru tego efektu
# tutaj używamy różnych miar (OR, RR, RD)
# tutaj używamy totalnie innych metod oceny efektów niż w liście 4...
# ...ponieważ tu mamy zmienne kategoryczne a nie zmiennie ciągłe

# co jest ciekawe to, że widać, że jedna publikacja wykazała efekt odwrotny do reszty..
# ... patrz na boxploty

meta_analysisOR = metabin(event.e = d.asp, n.asp, d.plac, n.plac, data = Fleiss1993bin, sm = "OR", studlab = study)
summary(meta_analysisOR)
forest(meta_analysisOR)

meta_analysisRR = metabin(event.e = d.asp, n.asp, d.plac, n.plac, data = Fleiss1993bin, sm = "RR", studlab = study)
summary(meta_analysisRR)
forest(meta_analysisRR)


meta_analysisRD = metabin(event.e = d.asp, n.asp, d.plac, n.plac, data = Fleiss1993bin, sm = "RD", studlab = study)
summary(meta_analysisRD)
forest(meta_analysisRD)

# patrząc na I^2 można postarać się określić jednorodność wyników badań
# im niższe I^2 tym lepiej
# zdecydowanie najwyższe I^2 mamy dla RD czyli rożnicy ryzyk


# zadanie 3 -------------------------------------------------------------------------------------------
# Stwórz meta analizę używając modelu z efektem stałym, używając ilorazu szans. 
# Sprawdź, czy metoda używa to wyznaczenia ilorazu szans ma wpływ na wyniki. Użyj 
# metod: odwrotnej wariancji, Peto i Mantele-Hanszele wplywa na wyniki


# generalnie w tej metanalizie by default mamy zawsze 2 modele: stały oraz mieszany (efekty stałe + losowe)
# tutaj nie bierzemy pod uwagę modelu mieszanego (random = FALSE)
# jako miary efektu badanego używamy tutaj OR
# są różne metody estymacji OR
# tutaj porównujemy 3 metody wyznaczanie OR (odwrotnej wariancji, Peto i Mantele-Hanszele wplywa na wyniki)

analysis_inverse = metabin(event.e = d.asp, n.asp, d.plac, n.plac, data = Fleiss1993bin, sm = "OR", random = FALSE, method = "Inverse", studlab = study)
summary(analysis_inverse)

analysis_peto = metabin(event.e = d.asp, n.asp, d.plac, n.plac, data = Fleiss1993bin, sm = "OR", random = FALSE, method = "Peto", studlab = study)
summary(analysis_peto)

# w sumie nie ma istotnych różnic między tymi trzema metodami
analysis_MH = metabin(event.e = d.asp, n.asp, d.plac, n.plac, data = Fleiss1993bin, sm = "OR", random = FALSE, method = "MH", studlab = study)
summary(analysis_MH)
