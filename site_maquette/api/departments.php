<?php
/**
 * GET api/departments.php
 *
 * Liste dynamique des départements réellement présents dans la table de
 * prédictions (LEFT(codgeo,2)) — sert à peupler le sélecteur de département
 * côté site sans coder en dur "34" : si un jour le pipeline charge d'autres
 * départements (11, 12, 13, 30, 81 — voir pipeline_test/README.md), ils
 * apparaissent automatiquement ici.
 *
 * Réponse : [{"dept":"34","n_communes":343}, ...]
 */

header('Content-Type: application/json; charset=utf-8');
require __DIR__ . '/db.php';

const TABLE = 'albopictus_ruiz_test';

try {
    $pdo = get_pg_connection();

    // LENGTH(codgeo) > 2 : exclut d'éventuelles lignes agrégées département
    // (codgeo à 2 caractères) — on ne veut compter que de vraies communes.
    $rows = $pdo->query("
        SELECT LEFT(codgeo, 2) AS dept, COUNT(DISTINCT codgeo) AS n_communes
        FROM " . TABLE . "
        WHERE LENGTH(codgeo) > 2
        GROUP BY LEFT(codgeo, 2)
        ORDER BY dept
    ")->fetchAll();

    $out = array_map(fn($r) => ['dept' => $r['dept'], 'n_communes' => (int) $r['n_communes']], $rows);
    echo json_encode($out, JSON_UNESCAPED_UNICODE);

} catch (Throwable $e) {
    http_response_code(500);
    echo json_encode(['error' => $e->getMessage()], JSON_UNESCAPED_UNICODE);
}
