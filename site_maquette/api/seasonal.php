<?php
/**
 * GET api/seasonal.php?codgeo=34XXX   (une commune, pour le modal profil hebdo)
 *     ou api/seasonal.php?all=1        (toutes les communes, pour le mode
 *                                        carte "Saisonnier" du time-scale)
 *
 * Prévisions SAISONNIÈRES (jusqu'à ~6 mois d'horizon) depuis la table de
 * TEST test_seasonal_ruiz (voir pipeline_test/scripts/
 * 07_seasonal_forecast_predictions.R). Contrairement à albopictus_ruiz_test
 * (forecast météo réel, horizon court ~2 semaines), ces valeurs viennent
 * d'un forecast saisonnier (climatologie probabiliste CFS/ECMWF), donc
 * nettement moins fiable à long terme — la table est explicitement une
 * table de TEST côté pipeline, jamais la table de production. Pas de LIME
 * ici (le pipeline saute volontairement ce calcul pour cette table
 * exploratoire, voir le script R, point 5).
 *
 * Cet endpoint ne fait AUCUNE hypothèse sur la présence de la table : si
 * test_seasonal_ruiz n'existe pas encore (script saisonnier jamais lancé
 * sur cette instance), on renvoie { available: false } plutôt qu'une 500.
 *
 * Réponse (mode ?codgeo=) :
 *   { available: bool, codgeo, last_update, weeks: [
 *       { date, date_fin, mean_temperature, mean_rainfall, mean_humidity,
 *         abundance_q50, abundance_q05, abundance_q95,
 *         horizon_jours, level_risk, trend, class_trend }
 *   ] }
 *
 * Réponse (mode ?all=1) :
 *   { available: bool, last_update, communes: [
 *       { codgeo, weeks: [ { même forme que ci-dessus, sans date_fin/level_risk/
 *         class_trend/horizon_jours pour rester léger } ] }
 *   ] }
 */

header('Content-Type: application/json; charset=utf-8');
require __DIR__ . '/db.php';

const TABLE = 'test_seasonal_ruiz';

function fail(int $code, string $msg): void {
    http_response_code($code);
    echo json_encode(['error' => $msg], JSON_UNESCAPED_UNICODE);
    exit;
}

$codgeo = $_GET['codgeo'] ?? null;
$all    = isset($_GET['all']);

if (!$all && (!$codgeo || !preg_match('/^\d+$/', $codgeo))) {
    fail(400, "Paramètre 'codgeo' requis (numérique), ou 'all=1' pour toutes les communes.");
}

try {
    $pdo = get_pg_connection();

    // Table de TEST : peut ne pas exister sur toutes les instances / tous les
    // moments (delete_layer=TRUE à chaque run côté R). On vérifie avant de
    // requêter pour ne pas transformer une absence de table en erreur 500.
    $check = $pdo->query("SELECT to_regclass('" . TABLE . "') AS t")->fetch();
    if (!$check || $check['t'] === null) {
        echo json_encode($all
            ? ['available' => false, 'communes' => []]
            : ['available' => false, 'codgeo' => $codgeo, 'weeks' => []], JSON_UNESCAPED_UNICODE);
        exit;
    }

    if ($all) {
        // Mode carte : toutes communes, toutes dates — on garde la charge utile
        // légère (pas de libgeo/level_risk/class_trend, non utilisés côté carte).
        $stmt = $pdo->query("
            SELECT codgeo, date, mean_temperature, mean_rainfall, mean_humidity,
                   combined_abundance_q50, trend, last_update
            FROM " . TABLE . "
            ORDER BY codgeo, date
        ");
        $byCommune = [];
        $lastUpdate = null;
        while ($r = $stmt->fetch()) {
            if ($lastUpdate === null) $lastUpdate = $r['last_update'];
            $code = $r['codgeo'];
            if (!isset($byCommune[$code])) $byCommune[$code] = ['codgeo' => $code, 'weeks' => []];
            $byCommune[$code]['weeks'][] = [
                'date'             => $r['date'],
                'mean_temperature' => $r['mean_temperature']       !== null ? (float) $r['mean_temperature']       : null,
                'mean_rainfall'    => $r['mean_rainfall']          !== null ? (float) $r['mean_rainfall']          : null,
                'mean_humidity'    => $r['mean_humidity']          !== null ? (float) $r['mean_humidity']          : null,
                'abundance_q50'    => $r['combined_abundance_q50'] !== null ? (float) $r['combined_abundance_q50'] : null,
                'trend'            => $r['trend']                 !== null ? (float) $r['trend']                 : null,
            ];
        }

        echo json_encode([
            'available'   => !empty($byCommune),
            'last_update' => $lastUpdate,
            'communes'    => array_values($byCommune),
        ], JSON_UNESCAPED_UNICODE);
        exit;
    }

    $stmt = $pdo->prepare("
        SELECT date, date_fin, mean_temperature, mean_rainfall, mean_humidity,
               combined_abundance_q50, combined_abundance_q05, combined_abundance_q95,
               horizon_jours, level_risk, trend, class_trend, last_update
        FROM " . TABLE . "
        WHERE codgeo = :codgeo
        ORDER BY date
    ");
    $stmt->execute(['codgeo' => $codgeo]);

    $weeks = [];
    $lastUpdate = null;
    while ($r = $stmt->fetch()) {
        if ($lastUpdate === null) $lastUpdate = $r['last_update'];
        $weeks[] = [
            'date'             => $r['date'],
            'date_fin'         => $r['date_fin'],
            'mean_temperature' => $r['mean_temperature']       !== null ? (float) $r['mean_temperature']       : null,
            'mean_rainfall'    => $r['mean_rainfall']          !== null ? (float) $r['mean_rainfall']          : null,
            'mean_humidity'    => $r['mean_humidity']          !== null ? (float) $r['mean_humidity']          : null,
            'abundance_q50'    => $r['combined_abundance_q50'] !== null ? (float) $r['combined_abundance_q50'] : null,
            'abundance_q05'    => $r['combined_abundance_q05'] !== null ? (float) $r['combined_abundance_q05'] : null,
            'abundance_q95'    => $r['combined_abundance_q95'] !== null ? (float) $r['combined_abundance_q95'] : null,
            'horizon_jours'    => $r['horizon_jours'] !== null ? (int) $r['horizon_jours'] : null,
            'level_risk'       => $r['level_risk'],
            'trend'            => $r['trend'] !== null ? (float) $r['trend'] : null,
            'class_trend'      => $r['class_trend'],
        ];
    }

    if (empty($weeks)) {
        echo json_encode(['available' => false, 'codgeo' => $codgeo, 'weeks' => []], JSON_UNESCAPED_UNICODE);
        exit;
    }

    echo json_encode([
        'available'   => true,
        'codgeo'      => $codgeo,
        'last_update' => $lastUpdate,
        'weeks'       => $weeks,
    ], JSON_UNESCAPED_UNICODE);

} catch (Throwable $e) {
    fail(500, $e->getMessage());
}
