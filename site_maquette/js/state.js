// ─────────────────────────────────────────────────────────────────────────
// STATE — toutes les variables globales mutables (let) qui représentent
// "où on en est" dans le dashboard : semaine/mois/saison courante, données
// en cache, commune sélectionnée, état des toggles. Regroupées ici pour que
// ce soit facile de voir d'un coup d'œil tout l'état partagé entre fichiers.
// ─────────────────────────────────────────────────────────────────────────

// ─────────────────────────────────────────────────────────────────────────────

let weeks=[], weekIdx=0, curVar='abundance', geoLayer=null, playTimer=null, map, curDep='34', prevWeekVals=null;

// 'commune' (défaut, comportement inchangé) ou 'department' : colore chaque
// commune avec la moyenne de SON département au lieu de sa propre valeur —
// un "zoom arrière" simple, sans polygones départementaux séparés à charger.
let mapScope = 'commune';

let baseGeoJSON = null;

let cachedWfsData = [];

let communeProps = {}; // Mappe codgeo -> {população, superficie_km2}

let scatterScales = {};       // plages fixes calculées sur toutes les semaines

// Vrai maximum d'abondance RÉGIONAL (toutes communes, toute la profondeur
// d'historique BD — pas seulement les 2 ans chargés côté front) — voir
// api/region_max_abundance.php. null = pas encore chargé (fetch en cours ou
// échoué) : dans ce cas, commune-chart.js retombe sur scatterScales.abMax +
// le filet de sécurité par commune (ancien comportement).
let regionMaxAbundance = null;

// Amplitude fixe de l'échelle du graphique LIME (voir api/lime_range.php et
// fetchLimeAxisRange() dans api.js) — null = pas encore chargé, buildLimeChart
// (right-panel.js) retombe alors sur l'ancien calcul dynamique par rendu.
let limeAxisMax = null;

let scatterFixedScale = true; // true = axes fixes, false = adaptatif

const charts={};


// ── Échelle temporelle Semaine/Mois ──────────────────────────────────────────
// 'week' (défaut, comportement inchangé) ou 'month' : le slider avance alors
// mois par mois, chaque "pas" agrégeant (moyenne) toutes les semaines de
// cachedWfsData tombant dans ce mois-là, par commune.
let timeScale = 'week';

let months = [], monthIdx = 0;


// ── Échelle temporelle "Saisonnier" (3e mode, cycle avec Semaine/Mois) ──────
// Contrairement à Semaine/Mois (dérivés de cachedWfsData, déjà en mémoire),
// la prévision saisonnière vient d'un endpoint séparé (api/seasonal.php?all=1,
// table de test test_seasonal_ruiz) et n'est chargée qu'à la demande (1er
// basculement vers ce mode), pour ne pas payer ce coût aux utilisateurs qui
// ne l'utilisent jamais.
let seasons = [], seasonIdx = 0;

let cachedSeasonalMapFlat = null; // null = jamais chargé ; [] = chargé mais vide/indisponible

let dataLoadedFrom = null;

let fullHistoryLoaded = false;


// Géométrie des départements = fusion RÉELLE des polygones communaux (via
// turf.dissolve), calculée une seule fois et mise en cache : les frontières
// communales ne changent jamais, seules les données affichées changent d'une
// semaine/commune à l'autre. Un simple habillage visuel (contour transparent
// ou de la couleur du remplissage) laissait un fin liseré blanc entre
// communes adjacentes (artefact d'anti-aliasing entre deux tracés SVG
// distincts) — la seule façon de l'éliminer complètement est de n'avoir
// QU'UN SEUL polygone par département, comme si c'était une commune unique.
let departmentGeoJSON = null;


// Dernières features affichées (semaine/mois/saison en cours) — mémorisées
// pour pouvoir re-rendre le LIME/l'explication de tendance sans refetch quand
// la sélection de commune change (voir renderRightPanel/openCommuneWeeklyModal).
let lastStatsFeatures = [];

// ── CHART MODAL ──────────────────────────────────────────────────────────────
let modalChart = null;


// ── MODAL COMMUNE : profil hebdomadaire vs moyennes historiques (10a/2a) ──────
let communeWeeklyChart = null;

let wideChartView = false; // "vue élargie" : plus de px par semaine + défilement horizontal, pour lire plus facilement quand la saisonnière allonge l'axe


let currentModalCodgeo = null; // commune actuellement ouverte dans le modal (pour le rapport 1-commune)

let currentProfile = null;     // dernier profile chargé (pour re-render sans refetch, ex. toggle seasonal)

let seasonalData = null;       // réponse de api/seasonal.php pour la commune ouverte (cache)

let seasonalEnabled = false;   // état du toggle "prévision saisonnière"


// Légende à cases à cocher (#cw-legend) : chaque <input> porte un
// data-ds="<index de dataset>" — "Année en cours" ET "Prévision" ont 2 cases
// séparées mais pointent vers le MÊME dataset (0), vu que le trait plein/
// pointillé est juste un style de segment de la même série (voir
// segment.borderDash plus haut), pas 2 datasets distincts — cocher/décocher
// l'une des deux doit donc aussi refléter l'état sur l'autre (voir
// syncLegendToggleUI).
//
// cwDatasetVisibility mémorise l'état choisi par l'utilisateur pour qu'il
// SURVIVE à la recréation du chart (communeWeeklyChart.destroy() + new Chart)
// qui a lieu à chaque appel de renderCommuneWeeklyChart — notamment quand on
// coche/décoche "Prévision saisonnière", qui sans ça effaçait les cases que
// l'utilisateur venait de décocher. Remise à [true,true,true] uniquement à
// l'ouverture d'une NOUVELLE commune/département (voir openCommuneWeeklyModal
// / openDeptWeeklyModal) — pas à chaque re-render du même modal.
let cwDatasetVisibility = [true, false, false, false]; // par défaut, seule "Année en cours" est visible — l'utilisateur active les moyennes 2/10 ans + la température à la demande (index 3 = Température)

// Case "Prévision" du modal commune — INDÉPENDANTE de cwDatasetVisibility[0]
// ("Année en cours"). BUG CORRIGÉ : les deux cases pointaient avant vers le
// MÊME dataset Chart.js (data-ds="0"), donc décocher "Prévision" masquait
// TOUTE la ligne (réel + prévision) via setDatasetVisibility, au lieu de
// juste cacher le tronçon prévision (les ~2 semaines de forecast court terme,
// voir FORECAST_DAYS). Cette variable pilote maintenant un filtrage séparé
// des points is_forecast dans renderCommuneWeeklyChart (voir commune-chart.js),
// pendant que cwDatasetVisibility[0] continue de gérer l'affichage/masquage
// de TOUTE la série via la case "Année en cours".
let cwForecastVisible = true;

// Case "Intervalle de prédiction" (bande q05-q95 autour de l'année en cours) —
// demande utilisateur : ajouter l'intervalle de prédiction aux courbes, avec
// sa propre case à cocher. Piloté à part (pas via cwDatasetVisibility) car
// c'est une PAIRE de datasets (borne haute/basse formant un fill), pas une
// simple courbe — voir renderCommuneWeeklyChart dans commune-chart.js.
let cwIntervalVisible = false;
