<?php
/**
 * GET api/predictions.php?from=YYYY-MM-DD&to=YYYY-MM-DD
 *
 * Devuelve las predicciones semanales (clima + abundancia) de la tabla
 * "albopictus_ruiz_test" en Postgres, agrupadas por commune, SOLO para el
 * rango de fechas pedido — así el frontend carga la data de a tramos
 * ("recientes primero, histórico después") en vez de todo junto.
 *
 * Forma de la respuesta (mismos nombres de campo que data/albo_weekly_herault.json
 * para no tener que reescribir el render del mapa/gráficos):
 *   {
 *     "from": "...", "to": "...",
 *     "communes": [
 *       { "codgeo": "34001", "libgeo": "Abeilhan", "weeks": [
 *           { "date", "date_fin", "temperature", "rainfall", "humidity",
 *             "abundance", "sd_abundance", "presence_prob", "level_risk", "trend" }
 *       ]}
 *     ]
 *   }
 */

header('Content-Type: application/json; charset=utf-8');
require __DIR__ . '/db.php';

const TABLE = 'albopictus_ruiz_test'; // tabla de producción (ver pipeline_test/config_charge.R)

function fail(int $code, string $msg): void {
    http_response_code($code);
    echo json_encode(['error' => $msg], JSON_UNESCAPED_UNICODE);
    exit;
}

$from = $_GET['from'] ?? null;
$to   = $_GET['to']   ?? null;

$dateRe = '/^\d{4}-\d{2}-\d{2}$/';
if (!$from || !$to || !preg_match($dateRe, $from) || !preg_match($dateRe, $to)) {
    fail(400, "Paramètres 'from' et 'to' requis, format YYYY-MM-DD.");
}

try {
    $pdo = get_pg_connection();

    // NOTE — filtre "lundi uniquement" (EXTRACT(DOW FROM date) = 1) :
    // le run hebdomadaire normal du pipeline (pipeline_test/scripts/02_hebdomadaire.R,
    // section "point 5") écrit désormais une ligne par JOUR (pas seulement le lundi)
    // dans la fenêtre récente/forecast — voir .fenetre_a_predire (by="day") et le
    // dernier bloc du filtre meteo2 (else { filter(date %in% .fenetre_a_predire) },
    // sans le weekday==1 appliqué dans les 2 autres branches). Résultat : les
    // semaines proches d'aujourd'hui apparaissaient jour par jour côté site.
    // On filtre ici en lecture pour ne garder qu'une ligne par semaine (lundi),
    // en attendant que le filtre weekday==1 soit rétabli dans cette branche du
    // pipeline (et que les lignes journalières déjà écrites soient nettoyées).
    $stmt = $pdo->prepare("
        SELECT
            codgeo, libgeo, date, date_fin,
            mean_temperature, mean_rainfall, mean_humidity,
            combined_abundance_q50, combined_abundance_sd,
            pred_presence_prob, level_risk, trend,
            \"lime_TM\" AS lime_tm, \"lime_UM\" AS lime_um, \"lime_RR\" AS lime_rr,
            \"TM_0_4\" AS tm_0_4, \"TM_0_8\" AS tm_0_8
        FROM " . TABLE . "
        WHERE date >= :from AND date <= :to
          AND EXTRACT(DOW FROM date) = 1
        ORDER BY codgeo, date
    ");
    $stmt->execute(['from' => $from, 'to' => $to]);

    $byCommune = [];
    while ($r = $stmt->fetch()) {
        $code = $r['codgeo'];
        if (!isset($byCommune[$code])) {
            $byCommune[$code] = [
                'codgeo' => $code,
                'libgeo' => $r['libgeo'],
                'weeks'  => [],
            ];
        }
        $byCommune[$code]['weeks'][] = [
            'date'          => $r['date'],
            'date_fin'      => $r['date_fin'],
            'temperature'   => $r['mean_temperature']       !== null ? (float) $r['mean_temperature']       : null,
            'rainfall'      => $r['mean_rainfall']          !== null ? (float) $r['mean_rainfall']          : null,
            'humidity'      => $r['mean_humidity']          !== null ? (float) $r['mean_humidity']          : null,
            'abundance'     => $r['combined_abundance_q50'] !== null ? (float) $r['combined_abundance_q50'] : null,
            'sd_abundance'  => $r['combined_abundance_sd']  !== null ? (float) $r['combined_abundance_sd']  : null,
            'presence_prob' => $r['pred_presence_prob']     !== null ? (float) $r['pred_presence_prob']     : null,
            'level_risk'    => $r['level_risk'],
            'trend'         => $r['trend'],
            'lime_TM'       => $r['lime_tm'] !== null ? (float) $r['lime_tm'] : null,
            'lime_UM'       => $r['lime_um'] !== null ? (float) $r['lime_um'] : null,
            'lime_RR'       => $r['lime_rr'] !== null ? (float) $r['lime_rr'] : null,
            'TM_0_4'        => $r['tm_0_4'] !== null ? (float) $r['tm_0_4'] : null,
            'TM_0_8'        => $r['tm_0_8'] !== null ? (float) $r['tm_0_8'] : null,
        ];
    }

    echo json_encode([
        'from'     => $from,
        'to'       => $to,
        'communes' => array_values($byCommune),
    ], JSON_UNESCAPED_UNICODE);

} catch (Throwable $e) {
    fail(500, $e->getMessage());
}
