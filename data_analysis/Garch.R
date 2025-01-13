# ------------------------------------------------------
# Installation (si nécessaire) des packages requis
# ------------------------------------------------------
# install.packages("quantmod")
# install.packages("rugarch")
# install.packages("tseries")
# install.packages("forecast")
# install.packages("PerformanceAnalytics")
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("tidyverse")

# ------------------------------------------------------
# Chargement des bibliothèques
# ------------------------------------------------------
library(quantmod)
library(rugarch)
library(tseries)
library(forecast)
library(PerformanceAnalytics)
library(ggplot2)
library(dplyr)
library(tidyverse)

# ------------------------------------------------------
# Nettoyage + seed
# ------------------------------------------------------
rm(list = ls())
set.seed(13012025)

# ------------------------------------------------------
# Paramètres et importation de GOOGL (Open prices)
# ------------------------------------------------------
ticker     <- "GOOGL"
train_from <- "2007-01-01"
train_to   <- "2017-12-31"
test_from  <- "2018-01-01"
test_to    <- "2018-12-31"

# Téléchargement via quantmod
getSymbols(ticker, src = "yahoo", from = train_from, to = test_to)

# Nommage des colonnes
names(GOOGL) <- c("open","high","low","close","volume","adjusted")

# On se focalise sur le prix d'ouverture
price_xts <- GOOGL$open

# ------------------------------------------------------
# Construction Train / Test (sur les prix d'ouverture)
# ------------------------------------------------------
train_price <- price_xts[paste0(train_from,"/",train_to)]
test_price  <- price_xts[paste0(test_from,"/",test_to)]

# ------------------------------------------------------
# Conversion en rendements log (%) pour l'ajustement GARCH
# ------------------------------------------------------
train_log_ret <- na.omit(100 * (log(train_price) - log(lag(train_price))))
test_log_ret  <- na.omit(100 * (log(test_price ) - log(lag(test_price ))))

# ------------------------------------------------------
# 1) Recherche du Meilleur Modèle GARCH
#    - On fait varier p,q dans {1,2} 
#    - On teste plusieurs distributions (norm, std, ged, sstd)
#    - On retient celui qui minimise le BIC
# ------------------------------------------------------
pq_grid    <- expand.grid(p = 1:2, q = 1:2,
                          dist = c("norm","std","ged","sstd"),
                          KEEP.OUT.ATTRS = FALSE)
results_df <- data.frame(p = numeric(),
                         q = numeric(),
                         dist = character(),
                         bic = numeric(),
                         stringsAsFactors = FALSE)

# Boucle de recherche
for(i in seq_len(nrow(pq_grid))) {
  p_ <- pq_grid$p[i]
  q_ <- pq_grid$q[i]
  dist_ <- as.character(pq_grid$dist[i])
  spec_tmp <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(p_, q_)),
    mean.model     = list(armaOrder = c(1, 0)),
    distribution.model = dist_
  )
  
  
  # On essaie d'ajuster le modèle et on capture d'éventuelles erreurs
  fit_ok <- TRUE
  fit_res <- tryCatch(
    ugarchfit(spec = spec_tmp, data = train_log_ret, silent = TRUE),
    error   = function(e) { fit_ok <<- FALSE; return(NULL) }
  )
  
  if(fit_ok) {
    bic_val <- infocriteria(fit_res)[2]  # [1] = AIC, [2] = BIC
    results_df <- rbind(results_df, data.frame(
      p = p_, q = q_, dist = dist_, bic = bic_val,
      stringsAsFactors = FALSE
    ))
  }
}

# On récupère le meilleur modèle (min BIC)
best_row    <- results_df[which.min(results_df$bic), ]
best_p      <- best_row$p
best_q      <- best_row$q
best_dist   <- best_row$dist

cat("Meilleur modèle GARCH trouvé (selon le BIC) :\n")
cat("  GARCH(", best_p, ",", best_q, ") avec distribution =", best_dist,
    " | BIC =", round(best_row$bic, 4), "\n")

# ------------------------------------------------------
# 2) Ajustement du Meilleur Modèle GARCH sur le train
# ------------------------------------------------------
best_spec <- ugarchspec(
  mean.model     = list(armaOrder = c(1,0)),
  variance.model = list(model = "sGARCH", garchOrder = c(best_p, best_q)),
  distribution.model = best_dist
)

best_fit <- ugarchfit(data = train_log_ret, spec = best_spec)
cat("\nRésumé du meilleur modèle GARCH retenu :\n")
print(best_fit)

# ------------------------------------------------------
# 3) Fonction de prédiction (n.ahead) sur les PRIX et calcul des indicateurs
# ------------------------------------------------------
predict_prices_garch <- function(garch_fit, train_prices, test_prices, n.ahead) {
  
  # 1) Prévision sur n jours (rendements)
  forc <- ugarchforecast(garch_fit, n.ahead = n.ahead)
  
  # 2) Extraction des rendements prévus (moyenne)
  pred_returns <- as.numeric(forc@forecast$seriesFor)
  
  # 3) Dernier prix observé (fin du train)
  last_train_price <- as.numeric(tail(train_prices, 1))
  
  # 4) Reconstruction pas à pas par exponentiation cumulée des rendements
  predicted_prices_vec <- last_train_price * exp(cumsum(pred_returns)/100)
  
  # 5) Prix réels (n jours)  
  real_prices <- head(test_prices, n.ahead)
  
  # 6) Alignement sur la plus petite taille en cas de longueur différente
  nb <- min(length(predicted_prices_vec), length(real_prices))
  
  # 7) Calculs des différents indicateurs
  # RMSE
  rmse_val <- sqrt(mean((as.numeric(real_prices[1:nb]) - 
                           predicted_prices_vec[1:nb])^2, na.rm=TRUE))
  # MAE [3]
  mae_val  <- mean(abs(as.numeric(real_prices[1:nb]) - 
                         predicted_prices_vec[1:nb]), na.rm=TRUE)
  # MSE [4]
  mse_val  <- mean((as.numeric(real_prices[1:nb]) - 
                      predicted_prices_vec[1:nb])^2, na.rm=TRUE)
  
  list(
    prices_pred = predicted_prices_vec,
    rmse        = rmse_val,
    mae         = mae_val,
    mse         = mse_val
  )
}

# ------------------------------------------------------
# 4) Exemples de prévision : 5, 22 et 250 jours
# ------------------------------------------------------
pred5   <- predict_prices_garch(best_fit, train_price, test_price, n.ahead = 5)
pred22  <- predict_prices_garch(best_fit, train_price, test_price, n.ahead = 22)
pred250 <- predict_prices_garch(best_fit, train_price, test_price, n.ahead = 250)

pred5   <- predict_prices_garch(best_fit, train_price, test_price, n.ahead = 5)
pred22  <- predict_prices_garch(best_fit, train_price, test_price, n.ahead = 22)
pred250 <- predict_prices_garch(best_fit, train_price, test_price, n.ahead = 250)

cat("\nPour 5 jours : RMSE =", pred5$rmse, 
    "| MAE =", pred5$mae, 
    "| MSE =", pred5$mse, "\n")
cat("Pour 22 jours : RMSE =", pred22$rmse, 
    "| MAE =", pred22$mae, 
    "| MSE =", pred22$mse, "\n")
cat("Pour 250 jours : RMSE =", pred250$rmse, 
    "| MAE =", pred250$mae, 
    "| MSE =", pred250$mse, "\n")

# ------------------------------------------------------
# 5) Visualisation rapide des prévisions vs. prix réels
# ------------------------------------------------------
plot_pred <- function(n, pred) {
  df_pred <- data.frame(
    date       = index(head(test_price, n)),
    real_price = as.numeric(head(test_price, n)),
    pred_price = pred$prices_pred
  )
  ggplot(df_pred, aes(x = date)) +
    geom_line(aes(y = real_price, color = "Prix réel")) +
    geom_line(aes(y = pred_price, color = "Prix prédit (GARCH)")) +
    ggtitle(paste0("Prévision sur ", n, " jours (Prix d'ouverture)")) +
    xlab("Date") + ylab("Prix d'ouverture") +
    scale_color_manual(values = c("blue","red")) +
    theme_minimal()
}

print(plot_pred(5, pred5))
print(plot_pred(22, pred22))
print(plot_pred(250, pred250))


# Ajouter des NA pour égaliser les longueurs
pred5_adjusted <- `length<-`(pred5$prices_pred, 250)
pred22_adjusted <- `length<-`(pred22$prices_pred, 250)


# Créer le data.frame avec les colonnes ajustées
create_garch <- data.frame(
  pred5 = pred5_adjusted,
  pred22 = pred22_adjusted,
  pred250 = pred250_adjusted
)

# Affichage pour vérifier le résultat
print(create_garch)


write.csv(create_garch, "forecast_predictions_garch.csv", row.names = FALSE)

