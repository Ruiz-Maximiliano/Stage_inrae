<?php
/**
 * GET api/zone_report.php?codgeo[]=34001&codgeo[]=34002...   (une ou plusieurs communes)
 *     ou api/zone_report.php?dept=34                          (département entier)
 *
 * Données agrégées (moyenne simple entre communes — même logique que le
 * "All Regions (Average)" du rapport ZanZemap fourni en référence) pour
 * générer le "Rapport de zone d'intérêt" côté dashboard-regional.html.
 * Le formatage en Markdown se fait côté JS (buildMarkdownReport) — cet
 * endpoint ne renvoie que les données.
 *
 * Réponse (voir docstrings inline pour le détail de chaque bloc) :
 *   { zone, current_year, today_week, current_week, weeks: [...53] }
 */

header('Content-Type: application/json; charset=utf-8');
require __DIR__ . '/db.php';

const TABLE     = 'albopictus_ruiz_test';
const TABLE_10Y = 'mean_10y';
const TABLE_2Y  = 'mean_2y';

function fail(int $code, string $msg): void {
    http_response_code($code);
    echo json_encode(['error' => $msg], JSON_UNESCAPED_UNICODE);
    exit;
}

function calendarWeek(string $date): int {
    $doy = (int) date('z', strtotime($date)) + 1;
    return intdiv($doy - 1, 7) + 1;
}

// ── Résolution de la zone (codes commune explicites, ou département entier) ──
$codgeoParam = $_GET['codgeo'] ?? null; // tableau si ?codgeo[]=...&codgeo[]=...
$deptParam   = $_GET['dept']   ?? null;

$explicitCodes = [];
if (is_array($codgeoParam)) {
    foreach ($codgeoParam as $c) {
        if (preg_match('/^\d{4,5}$/', $c)) $explicitCodes[] = $c;
    }
}

if (empty($explicitCodes) && (!$deptParam || !preg_match('/^\d{2,3}$/', $deptParam))) {
    fail(400, "Fournir 'codgeo[]' (une ou plusieurs communes) ou 'dept' (département entier).");
}

try {
    $pdo = get_pg_connection();

    if (!empty($explicitCodes)) {
        $placeholders = implode(',', array_map(fn($i) => ":c$i", array_keys($explicitCodes)));
        $params = [];
        foreach ($explicitCodes as $i => $c) $params["c$i"] = $c;
        $whereZone = "codgeo IN ($placeholders)";
        $zoneLabel = count($explicitCodes) === 1 ? 'Commune sélectionnée' : count($explicitCodes) . ' communes sélectionnées';
    } else {
        $whereZone = "LEFT(codgeo, 2) = :dept AND LENGTH(codgeo) > 2";
        $params = ['dept' => $deptParam];
        $zoneLabel = 'Département ' . $deptParam . ' (ensemble des communes)';
    }

    // Noms + codes des communes incluses (une ligne par codgeo, la plus récente)
    $stmtNames = $pdo->prepare("
        SELECT DISTINCT ON (codgeo) codgeo, libgeo
        FROM " . TABLE . "
        WHERE $whereZone
        ORDER BY codgeo, date DESC
    ");
    $stmtNames->execute($params);
    $communes = $stmtNames->fetchAll();
    if (!$communes) fail(404, "Aucune commune trouvée pour cette zone.");
    $zoneCodes = array_column($communes, 'codgeo');

    // Population / superficie — pas en BD, viennent de data/communes_meta.json (source INSEE)
    $metaPath = __DIR__ . '/../data/communes_meta.json';
    $popTotal = null;
    $supTotal = null;
    if (is_file($metaPath)) {
        $meta = json_decode(file_get_contents($metaPath), true) ?: [];
        $metaByCode = [];
        foreach ($meta as $m) $metaByCode[$m['codgeo']] = $m;
        $popTotal = 0; $supTotal = 0;
        foreach ($zoneCodes as $c) {
            $popTotal += (float) ($metaByCode[$c]['population'] ?? 0);
            $supTotal += (float) ($metaByCode[$c]['superficie_km2'] ?? 0);
        }
    }

    // Année de référence (cohérent avec commune_weekly_profile.php / refresh_mean_views())
    $yearRow = $pdo->query('SELECT EXTRACT(YEAR FROM MAX(date))::int AS y FROM ' . TABLE)->fetch();
    $currentYear = $yearRow && $yearRow['y'] !== null ? (int) $yearRow['y'] : (int) date('Y');
    $today = date('Y-m-d');

    // ── Année en cours, agrégée par semaine (moyenne simple entre communes) ──
    $stmt = $pdo->prepare("
        SELECT
            date,
            AVG(combined_abundance_q50) AS abundance_q50,
            AVG(combined_abundance_q05) AS abundance_q05,
            AVG(combined_abundance_q95) AS abundance_q95,
            AVG(pred_presence_prob)     AS presence_prob,
            AVG(mean_temperature)       AS temperature,
            AVG(mean_rainfall)          AS rainfall,
            AVG(mean_humidity)          AS humidity,
            AVG(trend)                  AS trend,
            AVG(\"lime_TM\")               AS lime_tm,
            AVG(\"lime_UM\")               AS lime_um,
            AVG(\"lime_RR\")               AS lime_rr
        FROM " . TABLE . "
        WHERE $whereZone
          AND EXTRACT(YEAR FROM date) = :year
          AND EXTRACT(DOW FROM date) = 1
        GROUP BY date
        ORDER BY date
    ");
    $stmt->execute($params + ['year' => $currentYear]);

    $currentByWeek = [];
    $lastRealWeek = null; // dernière semaine "réelle" (non-forecast) trouvée dans les données
    while ($r = $stmt->fetch()) {
        $w = calendarWeek($r['date']);
        $isForecast = $r['date'] > $today;
        $row = [
            'date'          => $r['date'],
            'abundance_q50' => $r['abundance_q50'] !== null ? round((float) $r['abundance_q50'], 2) : null,
            'abundance_q05' => $r['abundance_q05'] !== null ? round((float) $r['abundance_q05'], 2) : null,
            'abundance_q95' => $r['abundance_q95'] !== null ? round((float) $r['abundance_q95'], 2) : null,
            'presence_prob' => $r['presence_prob'] !== null ? round((float) $r['presence_prob'], 3) : null,
            'temperature'   => $r['temperature']   !== null ? round((float) $r['temperature'], 1)   : null,
            'rainfall'      => $r['rainfall']      !== null ? round((float) $r['rainfall'], 1)      : null,
            'humidity'      => $r['humidity']      !== null ? round((float) $r['humidity'], 1)      : null,
            'trend'         => $r['trend']         !== null ? round((float) $r['trend'], 1)         : null,
            'lime_TM'       => $r['lime_tm']       !== null ? round((float) $r['lime_tm'], 4)       : null,
            'lime_UM'       => $r['lime_um']       !== null ? round((float) $r['lime_um'], 4)       : null,
            'lime_RR'       => $r['lime_rr']       !== null ? round((float) $r['lime_rr'], 4)       : null,
            'is_forecast'   => $isForecast,
        ];
        $currentByWeek[$w] = $row;
        if (!$isForecast) $lastRealWeek = $w;
    }
    // BUG corrigé — voir commune_weekly_profile.php : "aujourd'hui" doit être
    // la dernière semaine réelle trouvée dans les données, pas un calcul
    // indépendant à partir de la date du jour (décalage possible si
    // aujourd'hui n'est pas un lundi, le binning DOY/7 étant ancré au 1er
    // janvier et non aux lundis réels des données).
    $todayWeek = $lastRealWeek ?? calendarWeek($today);
    $currentWeekRow = $todayWeek !== null ? ($currentByWeek[$todayWeek] ?? null) : null;

    // ── Moyennes historiques 10 ans / 2 ans, agrégées par semaine ──
    $stmt10 = $pdo->prepare("SELECT week, AVG(combined_abundance_q50) AS v FROM " . TABLE_10Y . " WHERE $whereZone GROUP BY week");
    $stmt10->execute($params);
    $mean10 = [];
    while ($r = $stmt10->fetch()) $mean10[(int) $r['week']] = $r['v'] !== null ? round((float) $r['v'], 2) : null;

    $stmt2 = $pdo->prepare("SELECT week, AVG(combined_abundance_q50) AS v FROM " . TABLE_2Y . " WHERE $whereZone GROUP BY week");
    $stmt2->execute($params);
    $mean2 = [];
    while ($r = $stmt2->fetch()) $mean2[(int) $r['week']] = $r['v'] !== null ? round((float) $r['v'], 2) : null;

    function riskLevel(?float $v): ?string {
        if ($v === null) return null;
        if ($v < 3)  return 'Faible';
        if ($v < 10) return 'Modéré';
        return 'Élevé';
    }

    $weeks = [];
    for ($w = 1; $w <= 53; $w++) {
        $monthLabel = (new DateTime("$currentYear-01-01"))->modify('+' . (($w - 1) * 7) . ' days')->format('M');
        $cur = $currentByWeek[$w]['abundance_q50'] ?? null;
        $weeks[] = [
            'week'        => $w,
            'month'       => $monthLabel,
            'current'     => $cur,
            'temperature' => $currentByWeek[$w]['temperature'] ?? null,
            'is_forecast' => $currentByWeek[$w]['is_forecast'] ?? null,
            'mean_2y'     => $mean2[$w]  ?? null,
            'mean_10y'    => $mean10[$w] ?? null,
            'level'       => riskLevel($cur),
        ];
    }

    echo json_encode([
        'zone' => [
            'label'             => $zoneLabel,
            'communes_included' => $communes,
            'n_communes'        => count($communes),
            'population_total'  => $popTotal !== null ? round($popTotal) : null,
            'superficie_totale_km2' => $supTotal !== null ? round($supTotal, 1) : null,
        ],
        'current_year' => $currentYear,
        'today_week'   => $todayWeek,
        'current_week' => $currentWeekRow,
        'weeks'        => $weeks,
    ], JSON_UNESCAPED_UNICODE);

} catch (Throwable $e) {
    fail(500, $e->getMessage());
}
