<?php
/**
 * GET api/lime_range.php
 *
 * Renvoie UN SEUL nombre : l'amplitude "robuste" (max_abs) de l'influence
 * LIME (lime_TM/lime_UM/lime_RR — Température/Précipitations/Humidité) sur
 * TOUTE la région et TOUTE l'historique disponible côté BD.
 *
 * Pourquoi ça existe : le graphique LIME (buildLimeChart, right-panel.js)
 * recalculait avant son échelle d'abscisses à CHAQUE rendu (min/max = ±110%
 * de la plus grande barre de la semaine/commune affichée) — donc la même
 * longueur de barre pouvait représenter une influence très différente d'une
 * semaine à l'autre, rendant les graphiques impossibles à comparer entre eux.
 * Demande : une échelle FIXE, basée sur le min/max d'influence LIME observé
 * pour tout le département.
 *
 * MAX() brut évité — même leçon que region_max_abundance.php : une seule
 * ligne aberrante suffirait à écraser l'échelle pour tout le monde. On prend
 * donc le 1er/99e percentile (sur l'union des 3 colonnes, toutes communes/
 * semaines confondues) plutôt que le vrai min/max, et on garde le plus grand
 * des deux écarts absolus pour une échelle symétrique autour de 0 (cohérent
 * avec le rendu actuel : barres rouges = influence positive, bleues = négative).
 */

header('Content-Type: application/json; charset=utf-8');
require __DIR__ . '/db.php';

const TABLE      = 'albopictus_ruiz_test';
const PERCENTILE = 0.01; // et son miroir 0.99 — exclut le 1% le plus extrême de chaque côté

function fail(int $code, string $msg): void {
    http_response_code($code);
    echo json_encode(['error' => $msg], JSON_UNESCAPED_UNICODE);
    exit;
}

try {
    $pdo = get_pg_connection();

    $stmt = $pdo->prepare("
        SELECT
            percentile_cont(:p_low)  WITHIN GROUP (ORDER BY v) AS p_low,
            percentile_cont(:p_high) WITHIN GROUP (ORDER BY v) AS p_high
        FROM (
            SELECT \"lime_TM\" AS v FROM " . TABLE . " WHERE \"lime_TM\" IS NOT NULL
            UNION ALL
            SELECT \"lime_UM\" FROM " . TABLE . " WHERE \"lime_UM\" IS NOT NULL
            UNION ALL
            SELECT \"lime_RR\" FROM " . TABLE . " WHERE \"lime_RR\" IS NOT NULL
        ) t
    ");
    $stmt->execute(['p_low' => PERCENTILE, 'p_high' => 1 - PERCENTILE]);
    $row = $stmt->fetch();

    $pLow  = ($row && $row['p_low']  !== null) ? (float) $row['p_low']  : 0.0;
    $pHigh = ($row && $row['p_high'] !== null) ? (float) $row['p_high'] : 0.0;
    $maxAbs = max(abs($pLow), abs($pHigh));

    echo json_encode(['max_abs' => $maxAbs > 0 ? $maxAbs : null], JSON_UNESCAPED_UNICODE);

} catch (Throwable $e) {
    fail(500, $e->getMessage());
}
