// ─────────────────────────────────────────────────────────────────────────
// API — tous les appels fetch() vers les endpoints PHP (api/predictions.php,
// api/seasonal.php, api/zone_report.php) + la mise en forme des réponses en
// lignes "plates" utilisées par le reste du dashboard (communesToFlat).
// ─────────────────────────────────────────────────────────────────────────


async function fetchPredictions(from, to){
  const r = await fetch(`api/predictions.php?from=${from}&to=${to}`);
  if(!r.ok){
    let msg = 'HTTP ' + r.status;
    try { msg = (await r.json()).error || msg; } catch(_) {}
    throw new Error(msg);
  }
  return r.json();
}


// Charge (une seule fois, avec cache navigateur) le vrai maximum régional
// d'abondance — voir api/region_max_abundance.php pour le pourquoi (le calcul
// front, scatterScales.abMax, ne voit que 2 ans de données et sous-estimait
// le maximum réel, aplatissant à 100% des valeurs pourtant courantes dans le
// graphique de comuna). Mis en cache comme baseGeoJSON/meta (même TTL) : ce
// nombre ne bouge quasiment jamais d'une visite à l'autre.
async function fetchRegionMaxAbundance(){
  // Clé versionnée ("_v2") : le 1er déploiement de region_max_abundance.php
  // utilisait un MAX() brut (renvoyait 774.6, un outlier — voir le percentile
  // dans region_max_abundance.php) et ce mauvais chiffre est resté en cache
  // navigateur jusqu'à 24h (CLIENT_CACHE_TTL_MS) chez les utilisateurs qui
  // avaient déjà chargé le dashboard entre-temps. Changer la clé force un
  // fetch frais partout, sans devoir attendre l'expiration du vieux cache.
  const cacheKey = 'cache_regionMaxAbundance_34_v2';
  const cached = cacheGet(cacheKey);
  if (cached !== null && cached !== undefined) { regionMaxAbundance = cached; return; }
  try {
    const r = await fetch('api/region_max_abundance.php');
    if (!r.ok) throw new Error('HTTP ' + r.status);
    const data = await r.json();
    if (typeof data.max_abundance === 'number' && data.max_abundance > 0) {
      regionMaxAbundance = data.max_abundance;
      cacheSet(cacheKey, regionMaxAbundance);
    }
  } catch (e) {
    // Pas bloquant : commune-chart.js retombe sur scatterScales.abMax + le
    // filet de sécurité par commune si ce fetch échoue (ex. endpoint pas
    // encore déployé, BD indisponible).
    console.warn('region_max_abundance.php indisponible, repli sur le calcul par fenêtre chargée :', e.message);
  }
}


// Charge (une seule fois, avec cache navigateur) l'amplitude fixe du
// graphique LIME — voir api/lime_range.php pour le pourquoi (échelle qui se
// recalculait avant à chaque rendu, rendant les graphiques d'une semaine/
// commune à l'autre impossibles à comparer entre eux).
async function fetchLimeAxisRange(){
  const cacheKey = 'cache_limeAxisMax_34';
  const cached = cacheGet(cacheKey);
  if (cached !== null && cached !== undefined) { limeAxisMax = cached; return; }
  try {
    const r = await fetch('api/lime_range.php');
    if (!r.ok) throw new Error('HTTP ' + r.status);
    const data = await r.json();
    if (typeof data.max_abs === 'number' && data.max_abs > 0) {
      limeAxisMax = data.max_abs;
      cacheSet(cacheKey, limeAxisMax);
    }
  } catch (e) {
    // Pas bloquant : buildLimeChart() retombe sur l'ancien calcul dynamique
    // (échelle qui s'adapte à chaque rendu) si ce fetch échoue.
    console.warn('lime_range.php indisponible, repli sur l\'échelle dynamique :', e.message);
  }
}


function communesToFlat(apiCommunes){
  const rows = [];
  apiCommunes.forEach(c => {
    const props = communeProps[c.codgeo] || {};
    c.weeks.forEach(w => {
      rows.push({
        codgeo: c.codgeo,
        libgeo: c.libgeo,
        dep: '34',
        date: w.date,
        date_fin: w.date_fin,
        mean_temperature: w.temperature,
        mean_rainfall: w.rainfall,
        mean_humidity: w.humidity,
        mean_abundance_albopictus: w.abundance,
        sd_abundance_albopictus: w.sd_abundance,
        trend: w.trend,
        lime_TM: w.lime_TM,
        lime_UM: w.lime_UM,
        lime_RR: w.lime_RR,
        TM_0_4: w.TM_0_4,
        TM_0_8: w.TM_0_8,
        population: props.population,
        superficie_km2: props.superficie_km2
      });
    });
  });
  return rows;
}


// Charge (une seule fois, avec cache) la prévision saisonnière pour TOUTES
// les communes (api/seasonal.php?all=1), pour le mode carte "Saisonnier".
// Contrairement au forecast météo court terme (albopictus_ruiz_test, fiable
// ~2 semaines), ceci vient de test_seasonal_ruiz (climatologie probabiliste,
// jusqu'à ~6 mois) — table de TEST, pas de production.
async function ensureSeasonalMapLoaded(){
  if (cachedSeasonalMapFlat !== null) return; // déjà chargé (même si vide)
  try {
    const r = await fetch('api/seasonal.php?all=1');
    const data = await r.json();
    if (!data.available || !data.communes || !data.communes.length) {
      cachedSeasonalMapFlat = [];
      seasons = [];
      return;
    }
    const rows = [];
    data.communes.forEach(c => {
      const props = communeProps[c.codgeo] || {};
      c.weeks.forEach(w => {
        rows.push({
          codgeo: c.codgeo,
          date: w.date,
          mean_temperature: w.mean_temperature,
          mean_rainfall: w.mean_rainfall,
          mean_humidity: w.mean_humidity,
          mean_abundance_albopictus: w.abundance_q50,
          trend: w.trend,
          population: props.population,
          superficie_km2: props.superficie_km2
        });
      });
    });
    cachedSeasonalMapFlat = rows;
    seasons = [...new Set(rows.map(r => r.date))].sort();
  } catch (e) {
    console.error('Erreur chargement seasonal.php?all=1 :', e);
    cachedSeasonalMapFlat = [];
    seasons = [];
  }
}


// Récupère et parse la réponse de zone_report.php à la main (pas r.json()
// direct) : ça permet, si le PHP a laissé fuiter un warning/notice en HTML
// AVANT le JSON (bug récurrent — display_errors mal configuré côté MAMP,
// ou erreur fatale hors du try/catch de l'endpoint), de voir le début du
// texte brut renvoyé au lieu d'un opaque "Unexpected token '<'... is not
// valid JSON" — ça pointe direct vers la ligne PHP fautive.
async function fetchZoneReportJSON(qs) {
  const r = await fetch(`api/zone_report.php?${qs}`);
  const raw = await r.text();
  let data;
  try {
    data = JSON.parse(raw);
  } catch (parseErr) {
    console.error('Réponse BRUTE (non-JSON) de zone_report.php :\n', raw);
    const snippet = raw.trim().slice(0, 300);
    throw new Error(
      'Le serveur a renvoyé une réponse invalide (probablement un warning/erreur PHP mélangé au JSON — voir la console pour le texte complet). Début de la réponse :\n\n' + snippet
    );
  }
  if (!r.ok) throw new Error(data.error || ('HTTP ' + r.status));
  return data;
}
