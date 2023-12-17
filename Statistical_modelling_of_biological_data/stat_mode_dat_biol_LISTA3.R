# --------------------------------------------------------------------
# zadanie 1
#Wczytaj dane “gbsg” z biblioteki survival. Sprawdź w dokumentacji co opisują 
#poszczególne zmienne
# --------------------------------------------------------------------
library(survival)
#head(gbsg)


# ---------------------------------------------------------------
# zadanie 2
# Usuń z danych kolumnę z id pacjentów
# -------------------------------------------------------------
gbsg$pid <- NULL
#head(gbsg)


# ----------------------------------------------------------------------
# zadanie 3
# Za pomocą testu log-rank sprawdź, czy istnieją różnice w krzywych przeżycia w 
#zależności od stopnia nowotworu.
# ----------------------------------------------------------------------

log_rank <- survdiff(Surv(gbsg$rfstime, gbsg$status) ~ gbsg$grade)
survfit(Surv(gbsg$rfstime, gbsg$status) ~ gbsg$grade)
plot(survfit(Surv(gbsg$rfstime, gbsg$status) ~ gbsg$grade))
# Surv(gbsg$rfstime, gbsg$status) ---> nasza krzywa przeżycia
# badamy zależnośc naszej krzywej przeżycia od tego ----->  ~ gbsg$grade


# --------------------------------------------------------------------------
#Stosując krokową metodę doboru zmiennych do modelu, stwórz model 
#proporcjonalnego hazardu Cox’a. Zinterpretuj otrzymany model.
# ---------------------------------------------------------------------
model_orig = coxph(Surv(gbsg$rfstime, gbsg$status) ~., data = gbsg)  # model ze wszystkimi zmiennymi
summary(model_orig)

# tak ręcznie można robić stepwise
#model2 <- coxph(Surv(gbsg$rfstime, gbsg$status) ~. -hormon -pgr, data = gbsg)
#summary(model2)

library(MASS)
# to nam zrobi wybór istotnych zmiennych poprzez odejmowanie oraz dodawanie zmiennych
stepwise_model <- stepAIC(model_orig, direction = "both")
summary(stepwise_model)
# interpretacja:
# wszystkie zmienne w finalnym modelu istotnie wpływają na czas przeżycia
# zmienne oprócz zmiennej "pgr" oraz "hormon" pozytywnie wpływają na czas przeżycia

# model Coxa mówi nam czy dana zmienna istotnie zwiększa czy zmniejsza funkcję hazardu
# jak sie popatrzy na summry() tego modelu, to w kolumnie exp(coef) mówi nam, że jeżeli...
# ... dana zmienna wzrośnie o 1, to funkcja hazardu wzrośnie o exp(coef)

# ---------------------------------------------------------------------------------------------------
# zadanie 5
#Nanieś na wykres prawdopodobieństwo przeżycia wyestymowane za pomocą modelu 
#stworzonego w poprzednim zadaniu.
# --------------------------------------------------------------------------------------------------
plot(survfit(stepwise_model), xlab = 'czas (dni)',ylab = "odsetek żywych")


# ------------------------------------------------------------------------------
# zadanie 6
#Przeczytaj dokumentacje funkcji cox.zph(). Do czego ona służy.
# ------------------------------------------------------------------------------

# Funkcja ta sprawdza czy spełnione są założenie modelu Coxa.
# To założenie to tyle, że funkcja hazardu powinna być niezależna od czasu.


# ----------------------------------------------------------------------
# zadanie 7
# Zastosuj funkcję cox.zph() do stworzonego modelu. Zinterpretuj otrzymane wyniki
# ----------------------------------------------------------------------

test_assumption <- cox.zph(model_orig)
test_assumption

# jeśli p-value jest istotne to dla danej zmiennej to założenie wzgledem tej zmiennje...
# ... nie jest spełnione
# na dole mamy GLOBAL i to mówi czy ogólnie założenie jest spełnione
