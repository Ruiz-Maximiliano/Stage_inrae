<?php
/**
 * GET api/region_max_abundance.php
 *
 * Renvoie UN SEUL nombre : le maximum "robuste" d'abondance (combined_abundance_q50)
 * observé sur TOUTE la région (toutes communes) et TOUTE la profondeur
 * d'historique disponible côté BD — pas seulement la fenêtre de 2 ans chargée
 * par le mapa (cachedWfsData / scatterScales.abMax, limitée par
 * HISTORY_YEARS_BACK côté front).
 *
 * Pourquoi ça existe (bug rapporté par l'utilisateur — "todo por sobre 25
 * individuos es 100%") :
 *   Le graphique de comuna (commune-chart.js) normalise l'"Activity Index" en
 *   %, avec comme dénominateur scatterScales.abMax — calculé côté front à
 *   partir de cachedWfsData, qui ne couvre QUE les 2 dernières années (limite
 *   volontaire du slider temporel, voir HISTORY_YEARS_BACK dans config.js).
 *   Mais le même graphique affiche aussi mean_10y/mean_2y — des vues
 *   matérialisées BACKEND qui, elles, couvrent jusqu'à 10 ans d'historique
 *   (voir commune_weekly_profile.php). Si un pic d'abondance a eu lieu il y a
 *   3-10 ans dans N'IMPORTE QUELLE commune, il n'entre JAMAIS dans le calcul
 *   de scatterScales.abMax (le front ne charge jamais ces données), donc le
 *   "maximum régional" utilisé comme référence 100% était en réalité trop bas
 *   — d'où l'aplatissement à 100% de valeurs pourtant courantes.
 *
 *   Cet endpoint corrige ça en interrogeant DIRECTEMENT la BD pour le vrai
 *   maximum, sur les 3 sources qui alimentent le graphique (table de
 *   production courante + les 2 vues historiques), sans dépendre de ce que le
 *   navigateur a déjà chargé. Résultat : UN SEUL nombre, stable, vraiment
 *   global — même référence pour toutes les communes (comparabilité
 *   préservée), plus besoin du "filet de sécurité" par commune.
 *
 * CORRECTIF — MAX() brut remplacé par le 99e percentile (2e bug rapporté par
 *   l'utilisateur : le premier déploiement de cet endpoint renvoyait 774.6,
 *   alors que le seuil "Élevé" du tableau de bord est ≥10 ind/piège et que les
 *   pics communaux connus tournent autour de 20-45 — 774.6 est quasiment
 *   certainement UNE seule ligne aberrante (erreur de saisie/pipeline) plutôt
 *   qu'un vrai pic régional. Un MAX() brut est par nature fragile face à UN
 *   SEUL outlier : toutes les valeurs normales s'écrasaient alors près de 0%
 *   ("todo en el piso"). Le 99e percentile (percentile_cont, sur l'union des
 *   3 sources) donne la même référence "grosso modo la valeur la plus haute
 *   jamais observée", mais ignore le 1% le plus extrême — donc robuste à une
 *   poignée de lignes aberrantes, sans avoir besoin de trouver/corriger la
 *   ligne fautive dans la BD à la main. Les quelques valeurs réelles qui
 *   dépasseraient encore ce seuil s'affichent simplement à 100% (comportement
 *   attendu, pas un bug).
 *
 * Coût : négligeable sur des tables/vues déjà indexées par le pipeline R —
 * appelé une seule fois au chargement du dashboard et mis en cache côté
 * navigateur (voir CLIENT_CACHE_TTL_MS, même mécanisme que baseGeoJSON/meta).
 */

header('Content-Type: application/json; charset=utf-8');
require __DIR__ . '/db.php';

const TABLE      = 'albopictus_ruiz_test';
const TABLE_10Y  = 'mean_10y';
const TABLE_2Y   = 'mean_2y';
const PERCENTILE = 0.99; // exclut le 1% le plus extrême (outliers) du calcul du "maximum régional"

function fail(int $code, string $msg): void {
    http_response_code($code);
    echo json_encode(['error' => $msg], JSON_UNESCAPED_UNICODE);
    exit;
}

try {
    $pdo = get_pg_connection();

    $stmt = $pdo->prepare("
        SELECT percentile_cont(:p) WITHIN GROUP (ORDER BY v) AS max_abundance
        FROM (
            SELECT combined_abundance_q50 AS v FROM " . TABLE . "
            WHERE combined_abundance_q50 IS NOT NULL
            UNION ALL
            SELECT combined_abundance_q50 FROM " . TABLE_10Y . "
            WHERE combined_abundance_q50 IS NOT NULL
            UNION ALL
            SELECT combined_abundance_q50 FROM " . TABLE_2Y . "
            WHERE combined_abundance_q50 IS NOT NULL
        ) t
    ");
    $stmt->execute(['p' => PERCENTILE]);
    $row = $stmt->fetch();

    $max = ($row && $row['max_abundance'] !== null) ? (float) $row['max_abundance'] : null;

    echo json_encode(['max_abundance' => $max, 'percentile' => PERCENTILE], JSON_UNESCAPED_UNICODE);

} catch (Throwable $e) {
    fail(500, $e->getMessage());
}
