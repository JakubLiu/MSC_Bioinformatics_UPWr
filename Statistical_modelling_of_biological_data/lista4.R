#install.packages('meta')
library(meta)
library(data.table)

# zadanie 1 -------------------------------------------------------------------------------------------------
#Wczytaj dane meta1, znajdujące się na stronie przedmiotu. Jakie informacje one 
#zawierają, z danymi jakiego typu mamy do czynienia?
# -------------------------------------------------------------------------------------------------------------

meta1 <- fread('http://theta.edu.pl/wp-content/uploads/2023/11/meta1.csv')
head(meta1)
# dane zawierają informacje o badaniach
# rok i autor badania
# rozmiar, średnia i odchylenie standardowe próby badanej (Ne, Me, Se)
# rozmiar, średnia i odchylenie standardowe próby kontrolnej (Nc, Mc, Sc)

# zadanie 2 ------------------------------------------------------------------------------------------------
# Dla wczytanych danych, wykonaj metaanalizę, używając różnicy średnich.
# ---------------------------------------------------------------------------------------------------------

meta_analysis <- metacont(Ne,Me,Se,Nc,Mc,Sc,data = meta1, studlab = paste(author,year))
summary(meta_analysis)
# różnica średnich jest domyślna, więc nie definiujemy tutaj tego

# zadanie 3 ---------------------------------------------------------------------------------------------
# Zobrazuj otrzymane wyniki i zinterpretuj je
# ----------------------------------------------------------------------------------------------------

forest(meta_analysis)
# mamy 2 modele, jeden efektów stałych a drugi mieszany
# Po lewej stronie mamy podsumowanie naszych danych wejściowych
# na wykresie mamy boxploty efektu dla danej publikacji
# czyli np. jak robimy metanaliza o wadze psów to te boxploty to wagi psów z poszczególnych publikacji
# im większy kwadrat boxplotu tym większa jest waga danej publikacji w metanalizie
# po prawej mamy różnice średniej oraz przedział ufności tej różnicy
# po prawej mamy też jaki % ogólnej wariancji jest wyjaśniany przez tą publikacje
#tau2 - wariancja pomiedzy poszczegolnymi badaniami
#I2 - zmiennosc jaka moze byc wyjasniona losowoscia (jak <25% to mozna stwierdzic ze dane jednorodne)
# tau^2 i I^2 to miary jednorodność, im wyższe I^2 tym wyższa niejednorodność
# jesli I^2 < 25% to można mówić, że wyniki w badań są jednorodne
#p-value --> H0 - nie ma roznic; prawdziwa wartosc efektu jest taka sama we wszystkich badaniach
# jak widać na wykresie z boxplotami, wszystkie publikacje wykazały efekt w "tą samą stronę"


# zadanie 4 ----------------------------------------------------------------------------------------
# Wykonaj metaanalizę używając standaryzowanej różnicy średnich, sprawdź czy sposób 
# estymacji standaryzowanej różnicy wpływa na wyniki.
# -------------------------------------------------------------------------------------------------

meta_analysis2 <- metacont(Ne,Me,Se,Nc,Mc,Sc,data = meta1, studlab = paste(author,year),sm="SMD")
summary(meta_analysis2)
forest(meta_analysis2)
# końcowa ocena się nie zmienia, ponieważ p-value jest nadal nieistotne


# zadanie 5 ----------------------------------------------------------------------------------------------
# Sprawdź w dokumentacji biblioteki jakie metody można użyć do estymacji wariancji 
# pomiędzy badaniami (tau). Wybierz trzy metody i porównaj ich rezultaty
# -------------------------------------------------------------------------------------------------------

taus <- c("PM", "EB", "HS")

for(tau in taus){
  meta_analysis <- metacont(Ne,Me,Se,Nc,Mc,Sc,data=meta1, studlab = paste(author,year),method.tau=tau,sm="SMD")
  print(summary(meta_analysis))
  print("---------------------------------------------------------------------------")
}