<?php
/**
 * Conexión a la base PostgreSQL "taconet_albopictus" (alwaysdata.net).
 *
 * IMPORTANTE — credenciales:
 *   Este archivo corre del lado del servidor (PHP/MAMP). El navegador nunca
 *   ve estas credenciales: solo recibe el JSON que devuelven los endpoints
 *   en api/*.php. Aun así, si este proyecto se sube a un repositorio público,
 *   este archivo NO debe commitearse (ver .gitignore) — mismo criterio que
 *   config.R en el pipeline (pipeline_test/config_charge.R).
 *
 *   Mismas credenciales usadas por el pipeline R (scripts/09_export_predictions_csv.R
 *   con config_charge.R activo).
 *
 * IMPORTANTE — respuestas siempre JSON:
 *   Si MAMP tiene display_errors=On (default en muchas instalaciones), un simple
 *   warning/notice de PHP (ej. array key indefinida) se imprime como HTML ANTES
 *   del echo json_encode(...) de cada endpoint — el navegador recibe entonces
 *   "<br />\n<b>Warning</b>: ..." pegado al JSON, y r.json() falla con
 *   "Unexpected token '<' ... is not valid JSON". Para que esto no vuelva a
 *   pasar pase lo que pase en el php.ini de MAMP:
 *     1) apagamos display_errors acá (no vuelve a imprimirse HTML suelto),
 *     2) convertimos cualquier warning/notice en excepción, así el propio
 *        try/catch de cada endpoint (predictions.php, commune_weekly_profile.php)
 *        lo atrapa y devuelve un JSON de error legible en vez de silenciarlo.
 */
ini_set('display_errors', '0');
error_reporting(E_ALL);
set_error_handler(function ($errno, $errstr, $errfile, $errline) {
    throw new ErrorException($errstr, 0, $errno, $errfile, $errline);
});

// Filet de sécurité pour les requêtes qui lisent plusieurs années d'historique
// (ex. predictions.php avec une grande fenêtre from/to) : une erreur fatale
// PHP (timeout, mémoire) N'EST PAS interceptée par set_error_handler ni par
// try/catch — elle imprime son propre HTML même avec display_errors à 0 dans
// certaines configs MAMP, ce qui casse le JSON côté navigateur. On augmente
// donc les limites par défaut ici plutôt que de compter uniquement sur le
// découpage des requêtes côté JS.
ini_set('memory_limit', '512M');
set_time_limit(120);

function get_pg_connection(): PDO {
    static $pdo = null;

    if ($pdo === null) {
        $host     = 'postgresql-taconet.alwaysdata.net';
        $port     = 5432;
        $dbname   = 'taconet_albopictus';
        $user     = 'taconet';
        $password = 'HHKcue51';

        $dsn = "pgsql:host={$host};port={$port};dbname={$dbname}";

        try {
            $pdo = new PDO($dsn, $user, $password, [
                PDO::ATTR_ERRMODE            => PDO::ERRMODE_EXCEPTION,
                PDO::ATTR_DEFAULT_FETCH_MODE  => PDO::FETCH_ASSOC,
            ]);
        } catch (PDOException $e) {
            // No repetimos el mensaje crudo de PDO (podría incluir el DSN) —
            // devolvemos algo genérico y logueamos el detalle en el server.
            error_log('Erreur connexion PostgreSQL: ' . $e->getMessage());
            throw new RuntimeException(
                "Impossible de se connecter à la base de données. " .
                "Vérifiez que l'extension PHP pdo_pgsql est activée dans MAMP."
            );
        }
    }

    return $pdo;
}
