// ─────────────────────────────────────────────────────────────────────────
// CONFIG — constantes statiques : endpoints, palette de couleurs/seuils par
// variable cartographiée (abondance/tendance), textes du panneau "Modèle &
// seuils" (RISK_ZONE_INFO), fenêtres de temps (RECENT_DAYS/FORECAST_DAYS/
// HISTORY_YEARS_BACK). Rien ici n'appelle de fonction — que des littéraux.
// ─────────────────────────────────────────────────────────────────────────


const DATA_SOURCE='wfs';

const WFS='./omees/proxy.php';

const LAYER='omees:albopictus_climate_suitability_weekly';

const C={codgeo:'codgeo',libgeo:'libgeo',dep:'dep',date:'date',abund:'mean_abundance_albopictus',temp:'mean_temperature',rain:'mean_rainfall',hum:'mean_humidity'};

const TREND_STABLE_THRESHOLD = 20; // % — aligné sur class_trend du pipeline R (trend>20 Hausse, <-20 Baisse)

const VARS={
  abundance:{col:'abund',label:'Activity Index',unit:'',colors:['#4caf50','#8bc34a','#ff9800','#f44336','#b71c1c'],breaks:[0,2,5,10,20],grad:'linear-gradient(to right,#4caf50,#8bc34a,#ff9800,#f44336,#b71c1c)'},
  // Tendance : 3 catégories discrètes (pas un dégradé continu) — Baisse si
  // < -TREND_STABLE_THRESHOLD, Stable entre les deux, Hausse si >
  // +TREND_STABLE_THRESHOLD. BUG CORRIGÉ — ces breaks étaient à ±10 alors que
  // trendCategory() (le texte affiché) utilise TREND_STABLE_THRESHOLD=20 :
  // résultat, la couleur de la carte/légende ne correspondait pas au texte
  // "Stable"/"Hausse"/"Baisse" affiché (ex. +15% = texte "Stable" mais
  // couleur "Hausse"). Les deux utilisent maintenant le même seuil.
  trend:{col:'trend',label:'Tendance',unit:'%',
    colors:['#2166ac','#9ca3af','#b2182b'],
    breaks:[-Infinity,-TREND_STABLE_THRESHOLD,TREND_STABLE_THRESHOLD],
    catLabels:['Baisse','Stable','Hausse'],
    grad:'linear-gradient(to right,#2166ac 0%,#2166ac 33.33%,#9ca3af 33.33%,#9ca3af 66.66%,#b2182b 66.66%,#b2182b 100%)'}
};

const DEP_PALETTE=['#00d4ff','#7c3aed','#f97316','#22c55e','#f59e0b','#ec4899','#a78bfa'];


// ── GESTION DES ÉCHELLES ──────────────────────────────────────────────────────
// Ancien toggle "Échelle: Brute/km²/hab." (bouton header + ABUND_SCALES/
// ABUND_SCALE_ORDER/dynBreaks/ABUND_COLOR_RAMP/ABUND_GRAD/cycleAbundScale())
// RETIRÉ — demande utilisateur, une seule échelle (brute) désormais partout.

 
// ─── CHARGEMENT DONNÉES (BD Postgres en direct, par tranches de dates) ───────
// Charge d'abord les métadonnées + une fenêtre récente (rapide), puis le reste
// de l'historique en arrière-plan. Repli sur le fichier JSON local si l'API
// n'est pas disponible (ex. pdo_pgsql pas encore activé dans MAMP).
const RECENT_DAYS   = 40 * 7; // ~40 semaines (couvre les 30 dernières utilisées par les tendances)

const FORECAST_DAYS = 14;

// Plafond de la navigation temporelle (slider semaine/mois) — voir
// HISTORY_FLOOR dans utils.js : limité au 1er janvier de l'année EN COURS
// (pas une fenêtre de N années en arrière) — demande utilisateur, pour que le
// mapa/panneau gauche ne montrent que l'année en cours (sys. date).


// Cache navigateur (localStorage) pour les données qui ne changent quasiment
// jamais d'une visite à l'autre : frontières communales (géométrie externe,
// geo.api.gouv.fr) et méta communes (population/superficie). Évite de
// re-télécharger + re-parser ces payloads à chaque chargement de page. TTL
// 24h : si les données changent côté source, le cache expire tout seul.
const CLIENT_CACHE_TTL_MS = 24 * 60 * 60 * 1000;

const CW_PX_PER_WEEK_WIDE = 26; // largeur minimale par semaine en mode élargi


// Mesures par niveau de risque — BROUILLON, à affiner avec l'équipe/ARS.
const RISK_ZONE_INFO = {
  'Faible': {
    color: '#4caf50',
    text: "Aucune action prioritaire. Poursuivre la surveillance de routine (relevé des pièges). Sensibilisation ponctuelle de la population à l'élimination des eaux stagnantes."
  },
  'Modéré': {
    color: '#ffc107',
    text: "Vigilance accrue. Renforcer la communication auprès des habitants. Inspecter les gîtes larvaires potentiels dans les zones concernées. Anticiper une intervention si la tendance se maintient."
  },
  'Élevé': {
    color: '#e53935',
    text: "Intervention recommandée. Démoustication ciblée sur les zones identifiées. Alerte renforcée aux habitants et professionnels de santé. Coordination avec les services de lutte anti-vectorielle (ARS). Suivi rapproché semaine par semaine."
  }
};
