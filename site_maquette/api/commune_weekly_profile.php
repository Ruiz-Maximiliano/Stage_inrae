<?php
/**
 * GET api/commune_weekly_profile.php?codgeo=34XXX
 *
 * Pour une commune donnée, renvoie 53 points (semaine 1 à 53) avec :
 *   - la valeur de l'année en cours (réel jusqu'à aujourd'hui, forecast après)
 *   - la moyenne historique 10 ans et 2 ans (vues matérialisées mean_10y / mean_2y,
 *     rafraîchies par le pipeline R via refresh_mean_views() dans 00_functions.R)
 *
 * Sert à tracer le graphique "année en cours vs moyennes historiques par semaine
 * calendaire", avec un repère vertical sur la semaine en cours.
 *
 * Schéma de mean_10y / mean_2y (voir 00_functions.R, refresh_mean_views()) :
 *   codgeo, week (1..53, = FLOOR((DOY-1)/7)+1), combined_abundance_q50, ...,
 *   pred_presence_prob, n_obs — une ligne par commune x semaine calendaire,
 *   moyennée sur les N années précédant la dernière année présente dans db_layer.
 */

header('Content-Type: application/json; charset=utf-8');
require __DIR__ . '/db.php';

const TABLE      = 'albopictus_ruiz_test';
const TABLE_10Y  = 'mean_10y';
const TABLE_2Y   = 'mean_2y';

function fail(int $code, string $msg): void {
    http_response_code($code);
    echo json_encode(['error' => $msg], JSON_UNESCAPED_UNICODE);
    exit;
}

// Semaine calendaire — MÊME formule que refresh_mean_views() côté SQL, pour que
// les numéros de semaine de l'année en cours s'alignent avec ceux des vues 10y/2y.
function calendarWeek(string $date): int {
    $doy = (int) date('z', strtotime($date)) + 1; // date('z') = jour de l'année, 0-indexé
    return intdiv($doy - 1, 7) + 1;
}

$codgeo = $_GET['codgeo'] ?? null;
if (!$codgeo || !preg_match('/^\d+$/', $codgeo)) {
    fail(400, "Paramètre 'codgeo' requis (numérique).");
}

try {
    $pdo = get_pg_connection();

    // Année de référence = année de la dernière date connue dans la table de
    // prédictions (cohérent avec la logique de refresh_mean_views()).
    $yearRow = $pdo->query('SELECT EXTRACT(YEAR FROM MAX(date))::int AS y FROM ' . TABLE)->fetch();
    $currentYear = $yearRow && $yearRow['y'] !== null ? (int) $yearRow['y'] : (int) date('Y');

    // Année en cours, par semaine, pour cette commune (lundis seulement — voir
    // note dans predictions.php sur les lignes journalières parasites).
    // mean_temperature ajoutée (demande utilisateur : courbe de température
    // dans le graphique de profil hebdo) — mean_10y/mean_2y n'ont PAS cette
    // colonne (vues matérialisées limitées à combined_abundance_q50, voir
    // refresh_mean_views()), donc pas de moyenne historique de température
    // disponible ici, seulement l'année en cours.
    // combined_abundance_q05/q95 ajoutées (demande utilisateur : intervalle de
    // prédiction, case à cocher) — mêmes colonnes déjà utilisées par
    // zone_report.php, confirmant qu'elles existent bien sur TABLE. Comme pour
    // la température, pas d'équivalent sur mean_10y/mean_2y (vues limitées à
    // combined_abundance_q50) : l'intervalle n'existe que pour l'année en cours.
    $stmt = $pdo->prepare("
        SELECT date, combined_abundance_q50, combined_abundance_q05, combined_abundance_q95, mean_temperature
        FROM " . TABLE . "
        WHERE codgeo = :codgeo
          AND EXTRACT(YEAR FROM date) = :year
          AND EXTRACT(DOW FROM date) = 1
        ORDER BY date
    ");
    $stmt->execute(['codgeo' => $codgeo, 'year' => $currentYear]);

    $today = date('Y-m-d');
    $currentByWeek = [];
    $lastRealWeek = null; // dernière semaine "réelle" (non-forecast) trouvée dans les données
    while ($r = $stmt->fetch()) {
        $w = calendarWeek($r['date']);
        $isForecast = $r['date'] > $today;
        $currentByWeek[$w] = [
            'value'       => $r['combined_abundance_q50'] !== null ? (float) $r['combined_abundance_q50'] : null,
            'q05'         => $r['combined_abundance_q05'] !== null ? (float) $r['combined_abundance_q05'] : null,
            'q95'         => $r['combined_abundance_q95'] !== null ? (float) $r['combined_abundance_q95'] : null,
            'temperature' => $r['mean_temperature']        !== null ? (float) $r['mean_temperature']        : null,
            'is_forecast' => $isForecast,
        ];
        if (!$isForecast) $lastRealWeek = $w;
    }

    $stmt10 = $pdo->prepare('SELECT week, combined_abundance_q50 FROM ' . TABLE_10Y . ' WHERE codgeo = :codgeo');
    $stmt10->execute(['codgeo' => $codgeo]);
    $mean10 = [];
    while ($r = $stmt10->fetch()) {
        $mean10[(int) $r['week']] = $r['combined_abundance_q50'] !== null ? (float) $r['combined_abundance_q50'] : null;
    }

    $stmt2 = $pdo->prepare('SELECT week, combined_abundance_q50 FROM ' . TABLE_2Y . ' WHERE codgeo = :codgeo');
    $stmt2->execute(['codgeo' => $codgeo]);
    $mean2 = [];
    while ($r = $stmt2->fetch()) {
        $mean2[(int) $r['week']] = $r['combined_abundance_q50'] !== null ? (float) $r['combined_abundance_q50'] : null;
    }

    // BUG corrigé — la ligne "aujourd'hui" du graphique doit tomber exactement
    // là où les données basculent réel → prévision, pas sur un calcul de
    // semaine indépendant à partir de la date du jour : le binning "semaine
    // calendaire" (FLOOR(DOY/7), ancré au 1er janvier) ne tombe pas forcément
    // dans le même bin que le lundi réellement utilisé par les données si
    // "aujourd'hui" n'est pas lui-même un lundi — d'où le décalage visible
    // mais irrégulier (dépend du jour de la semaine courant).
    $todayWeek = $lastRealWeek ?? calendarWeek($today);

    $weeks = [];
    for ($w = 1; $w <= 53; $w++) {
        $weeks[] = [
            'week'        => $w,
            'current'     => $currentByWeek[$w]['value']       ?? null,
            // q05/q95 : oubliés dans le output ci-dessous lors du 1er passage —
            // étaient déjà dans le SELECT/$currentByWeek plus haut, mais jamais
            // recopiés ici, donc jamais renvoyés par l'API (bug corrigé).
            'q05'         => $currentByWeek[$w]['q05']         ?? null,
            'q95'         => $currentByWeek[$w]['q95']         ?? null,
            'temperature' => $currentByWeek[$w]['temperature'] ?? null,
            'is_forecast' => $currentByWeek[$w]['is_forecast'] ?? null,
            'mean_10y'    => $mean10[$w] ?? null,
            'mean_2y'     => $mean2[$w]  ?? null,
        ];
    }

    echo json_encode([
        'codgeo'       => $codgeo,
        'current_year' => $currentYear,
        'today_week'   => $todayWeek,
        'weeks'        => $weeks,
    ], JSON_UNESCAPED_UNICODE);

} catch (Throwable $e) {
    fail(500, $e->getMessage());
}
