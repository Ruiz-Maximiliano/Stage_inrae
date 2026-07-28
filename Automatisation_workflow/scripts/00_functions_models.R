# ============================================================
# 00_functions_models.R — Fonctions de modèles et prédiction
#
# compute_lime_explanation()/add_lime_explanations() (basées sur le lime.R de
# Paul) remplacent l'usage de compute_shap()/.shapley_exact() dans
# 02_hebdomadaire.R — ces deux dernières restent définies ici (encore
# utilisées par d'autres scripts, ex. backfills SHAP existants) mais ne sont
# plus appelées par le pipeline hebdomadaire normal.
#
# CE QUE FAIT CE CODE :
#   Définit les fonctions liées aux modèles de présence/abondance : calcul
#   SHAP (exact et via treeshap), calcul LIME (explications locales via le
#   package lime), prédiction combinée two-part avec propagation
#   d'incertitude, et les utilitaires d'affichage de progression utilisés par
#   ces calculs (potentiellement longs). Ce fichier ne fait RIEN tout seul —
#   il ne fait que DÉFINIR des fonctions, appelées depuis
#   00_train_models.R, 02_hebdomadaire.R,
#   07_seasonal_forecast_predictions.R, etc. après un
#   source(here("scripts", "00_functions_models.R")).
#
# PARAMÈTRES D'ENTRÉE (à fournir) :
#   Aucun au niveau fichier — chaque fonction reçoit ses propres paramètres en
#   argument (voir le tag [ENTRÉE] dans la documentation de chaque fonction).
#
# PARAMÈTRES CRÉÉS PAR CE CODE :
#   compute_shap(), compute_lime_explanation(), add_lime_explanations(),
#   predict_two_part_uncertainty(), .shapley_exact(), .format_duree(),
#   .log_progression() (internes).
# ============================================================

#' Formate une durée en secondes en texte lisible (ex. "1 h 12 min", "45 s")
#'
#' @description
#' Utilisée par .shapley_exact() et predict_two_part_uncertainty() pour
#' afficher le temps écoulé / temps restant estimé (ETA) pendant les calculs
#' longs (backfill historique de plusieurs heures).
#'
#' @param secondes Durée en secondes (numérique, peut être NA/Inf)  [ENTRÉE]
#' @return Texte formaté (heures/minutes/secondes selon la grandeur) [SORTIE]
.format_duree <- function(secondes) {
  if (!is.finite(secondes) || secondes < 0) return("? ")
  if (secondes < 60) return(sprintf("%.0f s", secondes))
  if (secondes < 3600) return(sprintf("%.0f min %.0f s", secondes %/% 60, secondes %% 60))
  sprintf("%.0f h %.0f min", secondes %/% 3600, (secondes %% 3600) %/% 60)
}

#' Affiche un message de progression — log_print() si disponible, sinon cat()
#'
#' @description
#' Utilise log_print() (package logr, ouvert dans 02_hebdomadaire.R) quand
#' disponible, pour que la progression reste dans logs/log/ — sinon retombe
#' sur cat() (ex. 07_seasonal_forecast_predictions.R, qui n'utilise pas logr).
#'
#' @param msg Message texte à afficher                              [ENTRÉE]
.log_progression <- function(msg) {
  # tryCatch : logr::log_print() plante avec "Log is not open." si le package
  # logr est chargé (attaché d'un script précédent dans la même session R)
  # mais qu'aucun log_open() n'a été appelé dans CETTE session — on retombe
  # proprement sur cat() dans ce cas.
  ok <- FALSE
  if (exists("log_print", mode = "function")) {
    ok <- isTRUE(tryCatch({ log_print(msg); TRUE }, error = function(e) FALSE))
  }
  if (!ok) cat(msg, "\n")
}

#' Calcule les valeurs de Shapley EXACTES par énumération de toutes les
#' coalitions de prédicteurs
#'
#' @description
#' Implémentation maison, sans dépendance externe (pas de fastshap), de la
#' définition mathématique exacte de la valeur de Shapley :
#'   phi_j = somme sur toutes les coalitions S de {1..p}\\{j} de
#'           [ |S|! (p-|S|-1)! / p! ] * ( v(S U {j}) - v(S) )
#' où v(S) = prédiction moyenne du modèle quand seules les variables de S
#' prennent la vraie valeur de l'observation, et les autres variables sont
#' tirées d'un échantillon de référence (background). C'est EXACT (pas une
#' approximation Monte Carlo) car on énumère les 2^p coalitions complètes —
#' utilisée uniquement pour les forêts de probabilité, où treeshap ne
#' fonctionne pas (voir compute_shap()), et viable car p reste petit (au
#' plus 5 prédicteurs dans ce pipeline ; pour p plus grand, ce calcul
#' deviendrait trop coûteux et il faudrait une méthode approchée).
#'
#' Le calcul des observations à expliquer est découpé en lots (batch_size)
#' pour borner la mémoire utilisée par chaque appel à pred_wrapper() —
#' important pour un backfill historique de plusieurs milliers de lignes.
#'
#' @param model          Modèle (caret ou ranger) sur lequel pred_wrapper
#'                       sait faire des prédictions                    [ENTRÉE]
#' @param X_df           data.frame des observations à expliquer        [ENTRÉE]
#' @param background     data.frame de référence (valeurs "absentes")   [ENTRÉE]
#' @param feature_names   noms des colonnes/prédicteurs (longueur p)     [ENTRÉE]
#' @param pred_wrapper    fonction(object, newdata) -> vecteur numérique
#'                        de prédictions (une valeur par ligne)          [ENTRÉE]
#' @param max_background Taille max. de l'échantillon de référence tiré
#'                       au hasard dans background (limite le coût calcul,
#'                       défaut 100)                                    [ENTRÉE]
#' @param batch_size     Nombre max. de lignes à expliquer traitées en une
#'                       fois (limite la mémoire, défaut 2000)          [ENTRÉE]
#' @return data.frame p colonnes (une par variable de feature_names),
#'         une ligne par observation de X_df — valeurs de Shapley exactes [SORTIE]
.shapley_exact <- function(model, X_df, background, feature_names, pred_wrapper,
                            max_background = 100, batch_size = 2000) {

  p <- length(feature_names)
  n <- nrow(X_df)

  # Échantillon de référence limité à max_background lignes : chaque coalition
  # S nécessite n * n_bg prédictions, limiter n_bg évite l'explosion du coût
  # quand le jeu d'entraînement est grand.
  if (nrow(background) > max_background) {
    set.seed(123)
    background <- background[sample(nrow(background), max_background), , drop = FALSE]
  }
  n_bg <- nrow(background)

  # Toutes les coalitions possibles de {1..p}, codées en bits (0 = absent, 1 = présent)
  # ex. pour p=2 : {}, {1}, {2}, {1,2}  →  4 coalitions = 2^p
  all_subsets <- lapply(0:(2^p - 1), function(mask) {
    which(bitwAnd(mask, 2^(0:(p - 1))) > 0)
  })

  # Cache des valeurs v(S) pour chaque coalition S, calculées une seule fois
  # pour TOUTES les observations (vectorisé par lot) plutôt que ligne par ligne.
  v_S <- vector("list", length(all_subsets))
  n_subsets <- length(all_subsets)

  # Chaque itération traite un data.frame de n * n_bg lignes — pour un backfill
  # historique (n = milliers de lignes), une seule itération peut prendre
  # plusieurs minutes. Affichage de l'avancement (coalition i/n_subsets) +
  # temps écoulé + ETA pour pouvoir suivre un run long sans deviner.
  t_debut_shap <- Sys.time()

  # Découpage des n observations à expliquer en lots de batch_size : chaque
  # appel à pred_wrapper() ne traite jamais plus de batch_size * n_bg lignes
  # à la fois, quel que soit n — évite de dépasser la limite mémoire par
  # vecteur de R sur un backfill de plusieurs milliers de lignes. Résultat
  # mathématiquement identique (mêmes moyennes par observation), juste
  # calculé en morceaux bornés en mémoire.
  batch_starts <- seq(1, n, by = batch_size)

  for (i in seq_along(all_subsets)) {
    S <- all_subsets[[i]]
    v_i <- numeric(n)

    for (b_start in batch_starts) {
      b_end   <- min(b_start + batch_size - 1, n)
      idx_obs <- b_start:b_end
      n_batch <- length(idx_obs)

      # Construit un data.frame de n_bg * n_batch lignes (borné, jamais tout
      # n d'un coup) : pour chaque observation du lot, on répète le
      # background n_bg fois et on remplace les colonnes de S par la vraie
      # valeur de l'observation (les autres colonnes restent celles du
      # background = "valeur absente/marginalisée").
      bg_rep <- background[rep(seq_len(n_bg), times = n_batch), , drop = FALSE]

      if (length(S) > 0) {
        obs_rep <- X_df[rep(idx_obs, each = n_bg), feature_names[S], drop = FALSE]
        bg_rep[, feature_names[S]] <- obs_rep
      }

      preds <- pred_wrapper(model, bg_rep)
      # Moyenne par observation (bloc de n_bg lignes consécutives = 1 observation)
      v_i[idx_obs] <- vapply(seq_len(n_batch), function(k) {
        mean(preds[((k - 1) * n_bg + 1):(k * n_bg)])
      }, numeric(1))
    }

    v_S[[i]] <- v_i

    ecoule <- as.numeric(difftime(Sys.time(), t_debut_shap, units = "secs"))
    eta    <- (ecoule / i) * (n_subsets - i)
    .log_progression(sprintf(
      "  SHAP coalition %d/%d (n=%d obs x n_bg=%d, %d lot(s) de %d) — écoulé %s — restant ~%s",
      i, n_subsets, n, n_bg, length(batch_starts), batch_size,
      .format_duree(ecoule), .format_duree(eta)
    ))
  }

  # Combine les v(S) en valeurs de Shapley via la formule de pondération exacte
  shap_mat <- matrix(0, nrow = n, ncol = p)
  colnames(shap_mat) <- feature_names

  for (j in seq_len(p)) {
    subsets_without_j <- which(vapply(all_subsets, function(S) !(j %in% S), logical(1)))

    for (idx in subsets_without_j) {
      S      <- all_subsets[[idx]]
      s_size <- length(S)

      # Trouve l'indice de la coalition S U {j}
      mask_with_j <- 0
      if (s_size > 0) mask_with_j <- sum(2^(S - 1))
      mask_with_j <- mask_with_j + 2^(j - 1)
      idx_with_j  <- mask_with_j + 1  # +1 car all_subsets est indexé à partir de mask=0

      weight <- factorial(s_size) * factorial(p - s_size - 1) / factorial(p)
      shap_mat[, j] <- shap_mat[, j] + weight * (v_S[[idx_with_j]] - v_S[[idx]])
    }
  }

  as.data.frame(shap_mat)
}

#' Calcule les valeurs SHAP pour un modèle ranger ou caret/ranger
#'
#' @description
#' Fonction générique SHAP utilisable avec n'importe quel modèle ranger
#' ou caret entraîné avec méthode ranger. Gère deux cas :
#'   - Forêt de probabilité (chaque feuille renvoie un vecteur de
#'     probabilités par classe) : non supportée par treeshap::ranger.unify(),
#'     donc calcul des valeurs de Shapley exactes par énumération complète
#'     des coalitions (.shapley_exact(), sans dépendance externe).
#'   - Forêt de régression/classification par vote : treeshap standard.
#'
#' @param model        Modèle ranger ou caret (méthode ranger)        [ENTRÉE]
#' @param X_data       data.frame ou matrix des prédicteurs           [ENTRÉE]
#' @param model_type   "ranger" (défaut) ou "caret_ranger"            [ENTRÉE]
#' @param X_background data.frame de référence pour le calcul exact (forêts
#'                     de probabilité uniquement) — idéalement les
#'                     données d'entraînement. NULL = utilise X_data  [ENTRÉE]
#' @param max_background Taille max. de l'échantillon de référence
#'                     (forêts de probabilité uniquement, défaut 100) [ENTRÉE]
#' @return data.frame avec colonnes shap_<var>, shap_dominant_var,
#'         shap_dominant_val pour chaque observation                [SORTIE]
#'
#' @examples
#' shap_abund <- compute_shap(rf_abundance_q, X_abundance, model_type = "ranger")
#' shap_pres  <- compute_shap(mod_presence, X_presence, model_type = "caret_ranger",
#'                             X_background = res_train$X_presence)
compute_shap <- function(model, X_data, model_type = c("ranger", "caret_ranger"),
                          X_background = NULL, max_background = 100) {

  model_type <- match.arg(model_type)
  X_df       <- as.data.frame(X_data)

  # Extraire le modèle ranger sous-jacent si caret
  ranger_mod <- if (model_type == "caret_ranger") model$finalModel else model

  if (!inherits(ranger_mod, "ranger")) {
    stop("Le modèle doit être un objet ranger (ou caret avec méthode ranger)")
  }

  # treeshap requiert keep.inbag = TRUE à l'entraînement
  if (is.null(ranger_mod$inbag.counts)) {
    warning("Le modèle ranger n'a pas été entraîné avec keep.inbag = TRUE. ",
            "Les valeurs SHAP pourraient être incorrectes.")
  }

  # is_proba_forest : TRUE si le ranger sous-jacent est une forêt DE
  # PROBABILITÉ (chaque feuille stocke un vecteur de probabilités par classe,
  # pas une seule valeur numérique) — non supporté par treeshap::ranger.unify().
  is_proba_forest <- isTRUE(ranger_mod$treetype == "Probability estimation")

  if (is_proba_forest) {

    feature_names <- colnames(X_df)
    background    <- if (!is.null(X_background)) as.data.frame(X_background) else X_df
    background    <- background[, feature_names, drop = FALSE]

    pred_wrapper <- function(object, newdata) {
      predict(object, newdata = newdata, type = "prob")$Presence
    }

    result <- tryCatch({
      shap_df <- .shapley_exact(
        model = model, X_df = X_df, background = background,
        feature_names = feature_names, pred_wrapper = pred_wrapper,
        max_background = max_background
      )
      colnames(shap_df) <- paste0("shap_", colnames(shap_df))
      shap_cols <- colnames(shap_df)

      shap_df %>%
        dplyr::mutate(
          shap_dominant_var = gsub("shap_", "", shap_cols[
            apply(abs(.[, shap_cols, drop = FALSE]), 1, which.max)
          ]),
          shap_dominant_val = apply(.[, shap_cols, drop = FALSE], 1,
                                    function(x) x[which.max(abs(x))])
        )
    }, error = function(e) {
      warning("compute_shap (calcul exact) : impossible de calculer les valeurs ",
              "SHAP — ", conditionMessage(e), ". Colonnes SHAP absentes pour ce modèle.")
      NULL
    })

    return(result)
  }

  # Chemin normal (treeshap) — régression ou classification par vote (non probabiliste)
  result <- tryCatch({

    unified   <- treeshap::ranger.unify(ranger_mod, X_df)
    ts_result <- treeshap::treeshap(unified, X_df)
    shap_df   <- as.data.frame(ts_result$shaps)
    colnames(shap_df) <- paste0("shap_", colnames(shap_df))
    shap_cols <- colnames(shap_df)

    shap_df %>%
      dplyr::mutate(
        shap_dominant_var = gsub("shap_", "", shap_cols[
          apply(abs(.[, shap_cols, drop = FALSE]), 1, which.max)
        ]),
        shap_dominant_val = apply(.[, shap_cols, drop = FALSE], 1,
                                  function(x) x[which.max(abs(x))])
      )

  }, error = function(e) {
    warning("compute_shap : impossible de calculer les valeurs SHAP — ",
            conditionMessage(e), ". Colonnes SHAP absentes pour ce modèle.")
    NULL
  })

  return(result)
}

#' Prédiction combinée présence + abondance avec propagation d'incertitude
#'
#' @description
#' Combine un modèle de présence (classification probabiliste) et un modèle
#' d'abondance (quantile RF) pour produire une prédiction d'abondance combinée
#' avec incertitude propagée par simulation Monte Carlo :
#'   1. tire z_sim ~ Bernoulli(p)                (présence ou non, n_sim fois)
#'   2. tire a_log_sim ~ Normale(mu, sd)          (abondance log, n_sim fois)
#'   3. y_sim = z_sim * exp(a_log_sim)            (abondance finale si présent, 0 sinon)
#' sd est approximé à partir de l'intervalle 90% [q05,q95] en supposant une
#' loi normale (1.645 = quantile normal à 95%, donc q95-q05 couvre 2*1.645
#' écarts-types). C'est de là que viennent pred_combined_mean/q05/q50/q95/sd,
#' publiées comme combined_abundance_* dans 02_hebdomadaire.R. Utilisée à la
#' fois à l'entraînement (00_train_models.R, sur les données complètes) et
#' lors du pipeline hebdomadaire (02_hebdomadaire.R, sur les nouvelles données).
#'
#' @param newdata              data.frame contenant les prédicteurs            [ENTRÉE]
#' @param mod_presence         Modèle caret de présence (classification)       [ENTRÉE]
#' @param rf_abundance_q       Modèle ranger quantile d'abondance              [ENTRÉE]
#' @param predictors_presence  Vecteur des noms de prédicteurs (présence)      [ENTRÉE]
#' @param predictors_abundance Vecteur des noms de prédicteurs (abondance)     [ENTRÉE]
#' @param n_sim                Nombre de simulations Monte Carlo (défaut 2000) [ENTRÉE]
#' @return newdata enrichi des colonnes de prédiction présence/abondance/incertitude [SORTIE]
predict_two_part_uncertainty <- function(newdata, mod_presence, rf_abundance_q,
                                          predictors_presence, predictors_abundance,
                                          n_sim = 2000) {

  pred_presence <- predict(mod_presence,
    newdata = newdata[, predictors_presence, drop = FALSE], type = "prob")

  p <- pred_presence$Presence

  pred_q <- predict(rf_abundance_q,
    data = newdata[, predictors_abundance, drop = FALSE],
    type = "quantiles", quantiles = c(0.05, 0.5, 0.95))$predictions

  out <- newdata %>%
    dplyr::mutate(
      pred_presence_prob    = p,
      pred_presence_var     = p * (1 - p),
      pred_presence_entropy = -(p * log(pmax(p, 1e-8)) + (1 - p) * log(pmax(1 - p, 1e-8))),
      pred_log_abundance_q05 = pred_q[, 1],
      pred_log_abundance_q50 = pred_q[, 2],
      pred_log_abundance_q95 = pred_q[, 3],
      pred_abundance_q05     = exp(pred_log_abundance_q05),
      pred_abundance_q50     = exp(pred_log_abundance_q50),
      pred_abundance_q95     = exp(pred_log_abundance_q95),
      pred_expected_abundance = pred_presence_prob * pred_abundance_q50
    )

  # Pour un backfill historique (milliers de lignes x n_sim tirages chacune),
  # cette boucle peut prendre plusieurs minutes. Affichage périodique (tous
  # les 5% ou au moins toutes les 2000 lignes) plutôt qu'à chaque ligne, pour
  # ne pas ralentir le calcul avec trop d'I/O de log.
  n_total_sim  <- nrow(out)
  pas_affichage <- max(1, min(2000L, ceiling(n_total_sim / 20)))
  t_debut_sim  <- Sys.time()

  sim_res <- purrr::map_dfr(seq_len(nrow(out)), function(i) {
    p_i  <- out$pred_presence_prob[i]
    mu_i <- out$pred_log_abundance_q50[i]
    sd_i <- pmax((out$pred_log_abundance_q95[i] - out$pred_log_abundance_q05[i]) / (2 * 1.645), 1e-6)
    y_sim <- rbinom(n_sim, 1, p_i) * exp(rnorm(n_sim, mu_i, sd_i))

    if (i %% pas_affichage == 0 || i == n_total_sim) {
      ecoule <- as.numeric(difftime(Sys.time(), t_debut_sim, units = "secs"))
      eta    <- (ecoule / i) * (n_total_sim - i)
      .log_progression(sprintf(
        "  Simulation Monte Carlo : ligne %d/%d (%.0f%%) — écoulé %s — restant ~%s",
        i, n_total_sim, 100 * i / n_total_sim, .format_duree(ecoule), .format_duree(eta)
      ))
    }

    tibble::tibble(row_id = i,
           pred_combined_mean = mean(y_sim),
           pred_combined_q05  = quantile(y_sim, 0.05),
           pred_combined_q50  = quantile(y_sim, 0.50),
           pred_combined_q95  = quantile(y_sim, 0.95),
           pred_combined_sd   = sd(y_sim))
  })

  out %>%
    dplyr::mutate(row_id = seq_len(dplyr::n())) %>%
    dplyr::left_join(sim_res, by = "row_id") %>%
    dplyr::mutate(pred_thresholded = ifelse(pred_presence_prob > 0.5, pred_abundance_q50, 0))
}

#' Calcule les explications LIME d'un sous-ensemble de prédictions pour UN modèle
#'
#' @description
#' Reprend la logique de lime.R (Paul) : découpe df en lots (batch_size, pour
#' borner la mémoire/parallélisme via furrr, comme .shapley_exact() le fait
#' pour SHAP), appelle lime::explain() sur chaque lot, puis reconstruit un
#' data.frame large (une colonne par prédicteur) joint sur (codgeo, date).
#'
#' Le join id <-> explication se fait par la colonne "case" de lime::explain()
#' (numérotée 1..n À L'INTÉRIEUR DE CHAQUE LOT, pas globalement) — d'où le
#' découpage en lots IDENTIQUE (même ordre de lignes) pour les identifiants
#' (codgeo, date) et pour les prédicteurs, tous deux tirés du même df d'entrée.
#'
#' @param df           data.frame déjà filtré pour CE modèle (ex. pred_presence_prob
#'                      < 0.5 pour le modèle de présence) — doit contenir codgeo,
#'                      date, et les colonnes de `predictors`                [ENTRÉE]
#' @param explainer     Objet lime::lime() construit sur les données d'entraînement [ENTRÉE]
#' @param predictors    Vecteur des noms de prédicteurs de CE modèle              [ENTRÉE]
#' @param lime_col_map  Vecteur nommé prédicteur -> colonne de sortie, ex.
#'                      c(TM_0_8 = "lime_TM", UM_5_11 = "lime_UM")               [ENTRÉE]
#' @param label         Nom de la classe à expliquer (modèles de classification
#'                      seulement, ex. "Presence") — NULL pour la régression     [ENTRÉE]
#' @param n_permutations,n_features,feature_select,dist_fun,kernel_width
#'                      Transmis tels quels à lime::explain()                    [ENTRÉE]
#' @param batch_size    Taille des lots (mémoire/parallélisme furrr, défaut 1000) [ENTRÉE]
#' @return data.frame codgeo, date, lime_TM, lime_UM, lime_RR (NA pour les
#'         colonnes absentes de lime_col_map — modèle qui n'utilise pas cette
#'         variable), ou NULL si df est vide                                    [SORTIE]
compute_lime_explanation <- function(df, explainer, predictors, lime_col_map,
                                      label = NULL, n_permutations = 200,
                                      n_features = 6, feature_select = "highest_weights",
                                      dist_fun = "gower", kernel_width = NULL,
                                      batch_size = 1000) {

  if (nrow(df) == 0) return(NULL)

  x_batches <- df %>%
    dplyr::select(dplyr::all_of(predictors)) %>%
    dplyr::group_by(dplyr::row_number() %/% batch_size) %>%
    dplyr::group_map(~.x)

  id_batches <- df %>%
    dplyr::select(codgeo, date) %>%
    dplyr::group_by(dplyr::row_number() %/% batch_size) %>%
    dplyr::group_map(~ dplyr::mutate(.x, case = as.character(seq_len(nrow(.x)))))

  explain_args <- list(
    explainer      = explainer,
    n_permutations = n_permutations,
    dist_fun       = dist_fun,
    kernel_width   = kernel_width,
    n_features     = n_features,
    feature_select = feature_select
  )
  if (!is.null(label)) explain_args$labels <- label

  explanations <- furrr::future_map(
    x_batches,
    ~ do.call(lime::explain, c(list(x = .x), explain_args)),
    .options = furrr::furrr_options(seed = TRUE)
  )

  result <- purrr::map2_dfr(id_batches, explanations, ~ dplyr::left_join(.x, .y, by = "case")) %>%
    dplyr::select(-c(case:model_prediction, feature_value, feature_desc, data, prediction)) %>%
    tidyr::pivot_wider(names_from = feature, values_from = feature_weight)

  # Renomme selon lime_col_map (ex. TM_0_8 -> lime_TM), et garantit les 3
  # colonnes lime_TM/lime_UM/lime_RR même quand un prédicteur n'existe pas
  # pour ce modèle (ex. lime_RR = NA pour le modèle de présence, qui n'utilise pas RR).
  # fix : dplyr::rename(!!!x) attend names(x) = NOUVEAU nom, values(x) = ANCIEN nom
  # (c'est rename(nouveau = ancien)) — l'inverse de lime_col_map (ancien -> nouveau,
  # plus lisible pour l'appelant) — on inverse donc le vecteur ici, juste pour l'appel.
  result <- result %>% dplyr::rename(!!!stats::setNames(names(lime_col_map), lime_col_map))
  for (col in setdiff(c("lime_TM", "lime_UM", "lime_RR"), names(result))) {
    result[[col]] <- NA_real_
  }
  result %>% dplyr::select(codgeo, date, lime_TM, lime_UM, lime_RR)
}

#' Calcule les explications LIME pour les 2 modèles (présence + abondance) et
#' les joint à df_meteo_predictions
#'
#' @description
#' Équivalent LIME du bloc SHAP de 02_hebdomadaire.R : chaque ligne est
#' expliquée par UN SEUL des deux modèles selon le seuil pred_presence_prob
#' (comme lime.R de Paul) — contrairement au SHAP spatial, qui expliquait un
#' modèle combiné unique, LIME explique séparément le modèle qui a
#' effectivement produit la prédiction affichée pour cette ligne :
#'   - pred_presence_prob <  0.5 -> expliquée par le modèle de PRÉSENCE
#'     (TM_0_8, UM_5_11 -> lime_TM, lime_UM ; lime_RR = NA)
#'   - pred_presence_prob >= 0.5 -> expliquée par le modèle d'ABONDANCE
#'     (TM_0_4, UM_0_11, RR_1_5 -> lime_TM, lime_UM, lime_RR)
#'
#' @param df_meteo_predictions data.frame avec codgeo, date, pred_presence_prob,
#'                              et les colonnes de predictors_presence/abundance [ENTRÉE]
#' @param explainer_presence,explainer_abundance  Objets lime::lime() (un par modèle) [ENTRÉE]
#' @param predictors_presence,predictors_abundance  Vecteurs de noms de prédicteurs [ENTRÉE]
#' @param n_permutations,n_features,batch_size  Transmis à compute_lime_explanation() [ENTRÉE]
#' @return df_meteo_predictions enrichi des colonnes lime_TM, lime_UM, lime_RR [SORTIE]
add_lime_explanations <- function(df_meteo_predictions,
                                   explainer_presence, explainer_abundance,
                                   predictors_presence, predictors_abundance,
                                   n_permutations = 200, n_features = 6,
                                   batch_size = 1000) {

  explanation_presence <- compute_lime_explanation(
    df_meteo_predictions %>% dplyr::filter(pred_presence_prob < 0.5),
    explainer     = explainer_presence,
    predictors    = predictors_presence,
    lime_col_map  = c(TM_0_8 = "lime_TM", UM_5_11 = "lime_UM"),
    label          = "Presence",
    n_permutations = n_permutations,
    n_features     = n_features,
    batch_size     = batch_size
  )

  explanation_abundance <- compute_lime_explanation(
    df_meteo_predictions %>% dplyr::filter(pred_presence_prob >= 0.5),
    explainer     = explainer_abundance,
    predictors    = predictors_abundance,
    lime_col_map  = c(TM_0_4 = "lime_TM", UM_0_11 = "lime_UM", RR_1_5 = "lime_RR"),
    label          = NULL,
    n_permutations = n_permutations,
    n_features     = n_features,
    batch_size     = batch_size
  )

  explanation_lime <- dplyr::bind_rows(explanation_presence, explanation_abundance)

  dplyr::left_join(df_meteo_predictions, explanation_lime, by = c("codgeo", "date"))
}
