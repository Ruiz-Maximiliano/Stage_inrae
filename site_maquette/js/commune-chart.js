// ─────────────────────────────────────────────────────────────────────────
// COMMUNE-CHART — le graphique "année en cours vs moyennes historiques"
// affiché dans le modal commune : construction du chart Chart.js, bandes
// de risque cliquables ("crochets"), légende à cases à cocher, survol.
// ─────────────────────────────────────────────────────────────────────────


function toggleWideChartView(){
  wideChartView = !wideChartView;
  const btn = document.getElementById('cw-widen-btn');
  btn.textContent = wideChartView ? '↔ Vue élargie' : '↔ Vue compacte';
  btn.classList.toggle('active', wideChartView);
  if (currentProfile) renderCommuneWeeklyChart(currentProfile);
}


// Réapplique le style des polygones du mode "Commune" (contour jaune sur la
// commune sélectionnée) sans reconstruire toute la couche — resetStyle()
// relit le style à travers la fonction "style" d'origine, qui lit
// currentModalCodgeo (variable externe, donc toujours à jour).
function refreshMapSelectionHighlight(){
  if (geoLayer && geoLayer.eachLayer) geoLayer.eachLayer(l => geoLayer.resetStyle(l));
}


// Survol d'une ligne (Année en cours / Moyenne 2 ans / Moyenne 10 ans) dans le
// graphique commune : la ligne survolée redevient bien épaisse/opaque, les
// autres passent en transparence — pour distinguer une ligne des autres
// quand elles se croisent, sans avoir à cliquer sur la légende.
function handleCommuneChartHover(evt, chart) {
  const native = evt.native || evt;
  if (!native) return;
  // mode 'nearest' + intersect:false (plutôt que 'dataset'+intersect:true) :
  // détecte la ligne la plus proche du curseur même ENTRE deux points (pas
  // seulement pile sur un point, ce qui serait trop précis vu leur petite
  // taille) — beaucoup plus proche de "survoler la ligne" au sens large.
  const hits = chart.getElementsAtEventForMode(native, 'nearest', { intersect: false, axis: 'xy' }, false);
  const hoveredIdx = hits.length ? hits[0].datasetIndex : null;
  let changed = false;
  chart.data.datasets.forEach((ds, i) => {
    if (!ds._baseBorderColor) return; // ignore les datasets qui n'ont pas ces valeurs "repos" (aucun pour l'instant)
    const isHovered = hoveredIdx === i;
    const dim = hoveredIdx !== null && !isHovered;
    // Atténuation relevée de 0.15 à 0.55 — les lignes non survolées (surtout
    // Moyenne 2/10 ans) devenaient quasi invisibles pendant le survol de
    // "Année en cours", trop loin de leur rendu toujours pleinement opaque
    // dans le rapport PDF (qui n'a pas de survol). Elles restent plus
    // discrètes que la ligne survolée sans disparaître.
    const wantColor = dim ? withAlpha(ds._baseBorderColor, 0.55) : ds._baseBorderColor;
    const wantPointBg = dim ? withAlpha(ds._basePointBg, 0.55) : ds._basePointBg;
    const wantWidth = isHovered ? ds._baseBorderWidth + 1.5 : ds._baseBorderWidth;
    if (ds.borderColor !== wantColor) { ds.borderColor = wantColor; ds.backgroundColor = wantColor; changed = true; }
    if (ds.pointBackgroundColor !== wantPointBg) { ds.pointBackgroundColor = wantPointBg; changed = true; }
    if (ds.borderWidth !== wantWidth) { ds.borderWidth = wantWidth; changed = true; }
  });
  if (changed) chart.update('none'); // pas d'animation : évite le scintillement au mousemove
}


// "Crochets" de risque — bandes VERTICALES qui colorent les semaines (pas les
// valeurs) selon la phase de risque, regroupées par tranches consécutives de
// même niveau. Au-delà de l'horizon de prévision (plus de valeur "current"),
// on retombe sur la moyenne historique (10 ans, puis 2 ans) comme estimation
// "attendue" — marquée visuellement (hachures/contour pointillé) et signalée
// comme telle dans le brouillon de mesures. Chaque bande est cliquable (géré
// par chartjs-plugin-annotation).
// Extrait dans une fonction à part (au lieu d'être inline dans
// buildRiskBandAnnotations) pour que le clic (voir onClick du chart plus bas,
// qui interroge directement points[idx]) utilise EXACTEMENT le même calcul
// que le dessin des bandes — plus aucun risque que les deux divergent.
function computeRiskPoints(currentData, mean10Data, mean2Data) {
  return currentData.map((v, i) => {
    if (v !== null && v !== undefined && !isNaN(v)) return { level: riskLevelFromValue(v), expected: false };
    const fallback = (mean10Data[i] !== null && mean10Data[i] !== undefined) ? mean10Data[i]
                    : (mean2Data[i] !== null && mean2Data[i] !== undefined) ? mean2Data[i] : null;
    return { level: riskLevelFromValue(fallback), expected: true };
  });
}


function buildRiskBandAnnotations(weeks, currentData, mean10Data, mean2Data) {
  const points = computeRiskPoints(currentData, mean10Data, mean2Data);

  const annotations = {};
  let i = 0;
  while (i < points.length) {
    const { level: lvl, expected } = points[i];
    if (lvl === null) { i++; continue; }
    let j = i;
    while (j + 1 < points.length && points[j + 1].level === lvl && points[j + 1].expected === expected) j++;
    const info = RISK_ZONE_INFO[lvl];
    // BUG DES "ZONES BLANCHES" CORRIGÉ — sur un axe catégoriel Chart.js, un
    // nombre passé tel quel en xMin/xMax (via chartjs-plugin-annotation) est
    // interprété comme un INDEX 0-based direct (scale.getPixelForValue saute
    // scale.parse() car typeof value === 'number'), PAS comme une recherche
    // du label correspondant. Utiliser weeks[i].week (valeur 1..53, "1-based")
    // au lieu de l'index de tableau (0-based) décalait donc CHAQUE bande d'une
    // semaine vers la droite par rapport aux vrais points de données — d'où
    // le clic sur une zone blanche déclenchant l'alerte de la bande voisine.
    // Séparément, positionner xMin/xMax exactement sur les index des points
    // (centres de colonnes) laissait un demi-colonne non couvert ENTRE deux
    // bandes adjacentes (même bug indépendamment de l'alignement) : d'où les
    // zones blanches visibles. Fix : utiliser directement les index de
    // tableau i/j (déjà 0-based, alignés sur les vrais points), étendus de
    // ±0.5 pour que deux bandes voisines se touchent exactement, sans trou.
    annotations['riskBand_' + i + '_' + j] = {
      type: 'box',
      xMin: i - 0.5,
      xMax: j + 0.5,
      yMin: 0,
      yMax: 100,
      // Contour + remplissage renforcés (avant : bandes "réelles" sans aucun
      // contour, rien ne signalait qu'elles étaient cliquables — demande
      // utilisateur "messages préventifs" : bords en couleur + plus de
      // brillance pour que ce soit évident au premier coup d'œil).
      backgroundColor: hexToRgba(info.color, expected ? 0.10 : 0.20),
      borderWidth: expected ? 1 : 1.5,
      borderColor: hexToRgba(info.color, expected ? 0.5 : 0.8),
      borderDash: expected ? [3, 3] : undefined,
      drawTime: 'beforeDatasetsDraw'
      // Pas de "click" ici — le hit-test du plugin d'annotation sur un axe
      // catégoriel à labels numériques s'est déjà révélé peu fiable une fois
      // (voir note ci-dessus sur les "zones blanches"), et un second bug de
      // ce type provoquait des clics qui ouvraient le niveau de risque de la
      // MAUVAISE bande. Le clic est maintenant géré une seule fois, de façon
      // centralisée, dans le onClick du chart (voir plus bas) : il calcule
      // l'index exact via scale.getValueForPixel() — l'inverse mathématique
      // EXACT de la fonction utilisée pour dessiner ces bandes — et relit
      // points[idx] (computeRiskPoints), donc toujours rigoureusement
      // cohérent avec ce qui est affiché à l'écran.
    };
    i = j + 1;
  }
  return annotations;
}


function showRiskZoneInfo(level, expected) {
  const info = RISK_ZONE_INFO[level];
  if (!info) return;
  let modal = document.getElementById('risk-zone-modal');
  if (!modal) {
    modal = document.createElement('div');
    modal.id = 'risk-zone-modal';
    modal.style.cssText = 'position:fixed;inset:0;z-index:9500;background:rgba(15,23,42,.6);display:flex;align-items:center;justify-content:center';
    modal.innerHTML = `
      <div style="background:var(--surface);border-radius:12px;padding:18px 20px;max-width:360px;box-shadow:0 8px 30px rgba(0,0,0,.35)">
        <div id="rz-title" style="font-weight:700;margin-bottom:8px;display:flex;align-items:center;gap:6px"></div>
        <div id="rz-text" style="font-size:13px;color:var(--text-muted);line-height:1.5"></div>
        <div id="rz-expected" style="font-size:11px;color:var(--text-muted);font-style:italic;margin-top:8px"></div>
        <button onclick="document.getElementById('risk-zone-modal').remove()" style="margin-top:12px;width:100%;padding:6px;border-radius:6px;border:1px solid var(--border);background:none;cursor:pointer;font-size:12px">Fermer</button>
      </div>`;
    modal.addEventListener('click', e => { if (e.target === modal) modal.remove(); });
    document.body.appendChild(modal);
  }
  modal.querySelector('#rz-title').innerHTML = `<span style="display:inline-block;width:12px;height:12px;border-radius:50%;background:${info.color}"></span> Risque ${level}${expected ? ' (attendu)' : ''}`;
  modal.querySelector('#rz-expected').textContent = expected
    ? 'Estimation basée sur la moyenne historique (10 ans / 2 ans) — au-delà de l\'horizon de prévision disponible.'
    : '';
  modal.querySelector('#rz-text').textContent = info.text;
}


// opts.inPlace : n'est utilisé QUE par onSeasonalToggle() (commune-modal.js)
// — voir plus bas (juste avant la construction du chart) pour le pourquoi :
// évite le "reload" visible du graphique à chaque coche/décoche de
// "Prévision saisonnière".
function renderCommuneWeeklyChart(profile, opts) {
  const inPlace = !!(opts && opts.inPlace && communeWeeklyChart);

  // Vrai "aujourd'hui" (calendrier serveur, PHP date()) — sert UNIQUEMENT à
  // borner la prévision saisonnière plus bas (ne jamais recouvrir le passé/le
  // forecast court terme déjà connu), pas à positionner le marqueur visuel.
  const realTodayWeek = profile.today_week;

  // Marqueur rouge + badge "Aujourd'hui" : suit la date actuellement affichée
  // par la navigation temporelle du panneau gauche (semaine/mois/saisonnier,
  // voir selectedDateStr() dans data-loader.js) plutôt que la vraie date du
  // jour — demande utilisateur : en déplaçant le slider, la ligne doit se
  // déplacer avec pour montrer la date affichée, pas rester clouée sur
  // aujourd'hui. Repli sur la vraie date du jour si indisponible (ex. modal
  // ouvert avant la 1ère navigation).
  const selDate = selectedDateStr() || isoDate(new Date());
  const isRealToday = selDate === isoDate(new Date());
  const markerWeek = calendarWeekJS(selDate);
  const { start: wkStart, end: wkEnd } = weekDateRange(profile.current_year, markerWeek);
  const todayLabel = isRealToday
    ? `Aujourd'hui (${fmtShortNum(new Date(selDate + 'T00:00:00Z'))})`
    : `Date affichée (${fmtShortNum(new Date(selDate + 'T00:00:00Z'))})`;
  const weekRangeLabel = `S${markerWeek} (${fmtShortNum(wkStart)}–${fmtShortNum(wkEnd)})`;

  // Année en cours : segmentée réel (plein) / prévision (pointillé), sans relier
  // le trou éventuel entre le dernier point réel et le premier point de prévision.
  // weeksExt/currentData sont volontairement des tableaux EXTENSIBLES (via
  // .push plus bas) : la prévision saisonnière, quand activée, peut à la fois
  // combler des trous dans l'année en cours ET prolonger l'axe au-delà de la
  // semaine 53 (voir bloc seasonal ci-dessous).
  let weeksExt = profile.weeks.map(w => ({ ...w, isSeasonal: false }));
  let currentData = weeksExt.map(w => w.current);
  let mean2Data = weeksExt.map(w => w.mean_2y);
  let mean10Data = weeksExt.map(w => w.mean_10y);
  // Température de l'année en cours (demande utilisateur : courbe de
  // température dans le graphique) — pas de moyenne 2/10 ans disponible pour
  // la température (voir api/commune_weekly_profile.php), donc une seule
  // ligne, comme "Année en cours" pour l'abondance.
  let tempData = weeksExt.map(w => w.temperature ?? null);

  // Prévision saisonnière (test_seasonal_ruiz, jusqu'à ~6 mois) FUSIONNÉE dans
  // LA MÊME série "Année en cours" (continuation de la ligne bleue pointillée,
  // pas un 4e jeu de données séparé) :
  //  1) comble les semaines encore vides après le forecast court terme, dans
  //     l'année en cours (weeks 1..53) ;
  //  2) si l'horizon dépasse la fin de l'année civile, ÉTEND l'axe SANS
  //     continuer à compter (54, 55...) — BUG CORRIGÉ ("régler pb semaine
  //     calendaire > 52") : on revient à la semaine 1, 2, 3... de l'année
  //     suivante (même numérotation que le début du graphique), avec un
  //     repère visuel (ligne pointillée + étiquette d'année, voir
  //     yearBoundaryIdx plus bas) pour ne pas confondre ces semaines avec
  //     celles de l'année en cours qui les précèdent dans le graphique.
  let yearBoundaryIdx = -1; // index dans weeksExt où bascule l'année suivante (-1 = pas de bascule à afficher)
  if (seasonalEnabled && seasonalData && seasonalData.available) {
    const extra = [];
    seasonalData.weeks.forEach(w => {
      if (!w.date) return;
      const wYear = parseInt(w.date.slice(0, 4), 10);
      const wk = calendarWeekJS(w.date);
      if (wYear <= profile.current_year) {
        // Même année que le profil : comble les trous entre le forecast
        // court terme et la fin de l'année en cours.
        if (wk <= realTodayWeek) return; // ne recouvre jamais le passé / le forecast déjà connu (vraie date du jour)
        const idx = weeksExt.findIndex(x => x.week === wk && !x.isNextYear);
        if (idx !== -1 && (currentData[idx] === null || currentData[idx] === undefined)) {
          currentData[idx] = w.abundance_q50;
          weeksExt[idx] = { ...weeksExt[idx], isSeasonal: true, is_forecast: true };
        }
        // Température : comble aussi les trous, indépendamment de l'abondance
        // (une semaine peut avoir une temp connue mais pas d'abondance, ou
        // vice-versa selon la disponibilité des données saisonnières).
        if (idx !== -1 && (tempData[idx] === null || tempData[idx] === undefined) && w.mean_temperature !== null && w.mean_temperature !== undefined) {
          tempData[idx] = w.mean_temperature;
        }
        return;
      }
      // Année suivante : semaine 1, 2, 3... de cette nouvelle année (pas de
      // numérotation continue) — voir isNextYear/nextYear, utilisés plus bas
      // pour positionner la ligne de bascule d'année.
      extra.push({ week: wk, isSeasonal: true, is_forecast: true, isNextYear: true, nextYear: wYear, current: w.abundance_q50, temperature: w.mean_temperature ?? null, mean_2y: null, mean_10y: null });
    });
    // Tri chronologique (année puis semaine) — assure l'ordre même si
    // seasonalData.weeks n'était pas déjà trié, et même si l'horizon
    // traversait plus d'une frontière d'année.
    extra.sort((a, b) => (a.nextYear - b.nextYear) || (a.week - b.week));
    extra.forEach(e => {
      weeksExt.push(e);
      currentData.push(e.current);
      tempData.push(e.temperature);
      mean2Data.push(null);
      mean10Data.push(null);
    });
    yearBoundaryIdx = weeksExt.findIndex(w => w.isNextYear);
  }

  const labels = weeksExt.map(w => w.week);

  // Normalisation en "Activity Index" 0-100% (style ZanZemap) — RÉGION ENTIÈRE,
  // pas par graphique : dénominateur de base = le maximum d'abondance RÉEL
  // observé sur toute la région (regionMaxAbundance, voir
  // api/region_max_abundance.php et fetchRegionMaxAbundance() dans api.js).
  //
  // BUG CORRIGÉ (rapporté par l'utilisateur — "todo por sobre 25 individuos es
  // 100%") : le dénominateur utilisait avant scatterScales.abMax, calculé
  // côté front à partir de cachedWfsData — qui ne couvre QUE les 2 dernières
  // années (HISTORY_YEARS_BACK, limite volontaire du slider). Mais ce même
  // graphique affiche mean_10y/mean_2y, des vues BACKEND couvrant jusqu'à 10
  // ans — un pic vieux de 3-10 ans dans N'IMPORTE QUELLE commune n'entrait
  // donc jamais dans scatterScales.abMax, qui restait artificiellement bas
  // (~25 dans le cas rapporté) : tout ce qui le dépassait s'aplatissait à
  // 100%, et le "filet de sécurité" par commune (ancien ownPeak) rendait en
  // plus le dénominateur DIFFÉRENT d'une commune à l'autre — plus de vraie
  // comparabilité régionale, contrairement à l'intention initiale.
  // region_max_abundance.php interroge directement la BD (table de
  // production + mean_10y + mean_2y, toutes communes) pour le vrai maximum —
  // UN SEUL nombre, stable, fetché une fois au chargement du dashboard.
  //
  // Repli si regionMaxAbundance n'est pas encore prêt (fetch en cours/échoué,
  // endpoint pas encore déployé côté hosting) : on retombe sur l'ancien calcul
  // (scatterScales.abMax + pic propre de la commune) plutôt que de planter.
  let REGION_MAX_ABUNDANCE;
  if (regionMaxAbundance) {
    REGION_MAX_ABUNDANCE = regionMaxAbundance;
  } else {
    const ownPeak = arrMax([...currentData, ...mean2Data, ...mean10Data].filter(v => v !== null && !isNaN(v)).concat(0));
    REGION_MAX_ABUNDANCE = Math.max((scatterScales && scatterScales.abMax) || 0, ownPeak, 1);
  }
  const toIndex = v => (v === null || v === undefined || isNaN(v)) ? null : Math.min(100, (v / REGION_MAX_ABUNDANCE) * 100);
  // Case "Prévision" décochée : on masque UNIQUEMENT le tronçon prévision
  // (points weeksExt[i].is_forecast — court terme ET saisonnier le cas
  // échéant) en les mettant à null, PAS tout le dataset. weeksExt/currentData
  // restent intacts (spanGaps:true fait que la ligne s'arrête proprement au
  // dernier point réel, sans rien connecter après puisque ces points sont
  // toujours en fin de tableau — voir plus haut). Les bandes de risque
  // (computeRiskPoints) et les autres séries ne sont pas affectées : seule la
  // ligne "Année en cours" perd son tronçon prévision à l'écran.
  const currentIdx = currentData.map((v, i) => (!cwForecastVisible && weeksExt[i].is_forecast) ? null : toIndex(v));
  const mean2Idx = mean2Data.map(toIndex);
  const mean10Idx = mean10Data.map(toIndex);

  // Température normalisée par département (demande utilisateur), pour être
  // affichable sur la même échelle 0-100% que l'Activity Index : 0% = tempMin
  // régional, 100% = tempMax régional (scatterScales, calculé sur
  // cachedWfsData — TOUTES les communes de l'Hérault, année en cours depuis
  // le plafonnement de la navigation temporelle — voir HISTORY_FLOOR dans
  // utils.js). PAS le même genre de normalisation que l'abondance (pas de
  // notion de "% du max jamais atteint" pertinente pour une température) :
  // ici c'est une mise à l'échelle min-max classique, région entière.
  const tempMin = scatterScales && typeof scatterScales.tempMin === 'number' ? scatterScales.tempMin : 0;
  const tempMax = scatterScales && typeof scatterScales.tempMax === 'number' ? scatterScales.tempMax : 40;
  const tempSpan = (tempMax - tempMin) || 1; // filet anti division par zéro
  const toTempIndex = v => (v === null || v === undefined || isNaN(v)) ? null : Math.min(100, Math.max(0, ((v - tempMin) / tempSpan) * 100));
  const tempIdx = tempData.map((v, i) => (!cwForecastVisible && weeksExt[i].is_forecast) ? null : toTempIndex(v));

  // Vue élargie : donne une largeur MINIMALE au conteneur du graphique (au
  // lieu de width:100%), pour que #cw-canvas-wrap devienne scrollable
  // horizontalement plutôt que d'écraser toutes les semaines dans la largeur
  // du panneau — surtout utile quand la saisonnière allonge l'axe.
  //
  // BUG CORRIGÉ ("ça recharge tout le graphique quand j'active la
  // prévision") : le bouton manuel "vue compacte/élargie" a été retiré de
  // l'UI (wishlist précédente), donc wideChartView restait bloqué à false —
  // le conteneur gardait une largeur FIXE (100%) même quand la prévision
  // saisonnière ajoutait ~26 semaines de plus. Chart.js devait alors
  // compresser TOUTES les semaines (anciennes + nouvelles) dans cette même
  // largeur, donc CHAQUE point existant changeait de position pixel d'un
  // coup — visuellement indiscernable d'un rechargement complet, même si la
  // mise à jour "en place" (inPlace, voir plus bas) évitait bien le
  // destroy/recreate. Élargi automatiquement dès que l'axe dépasse les 53
  // semaines habituelles, pour que les semaines déjà affichées ne bougent
  // plus — seul le nouveau tronçon s'ajoute, avec défilement horizontal.
  const cwInner = document.getElementById('cw-canvas-inner');
  if (cwInner) {
    if (wideChartView || weeksExt.length > 53) {
      const wrapWidth = document.getElementById('cw-canvas-wrap').clientWidth;
      cwInner.style.width = Math.max(wrapWidth, weeksExt.length * CW_PX_PER_WEEK_WIDE) + 'px';
    } else {
      cwInner.style.width = '100%';
    }
  }

  // inPlace : NE PAS détruire l'instance existante (voir plus bas, juste
  // avant la construction du chart, pour le pourquoi).
  if (!inPlace && communeWeeklyChart) { communeWeeklyChart.destroy(); communeWeeklyChart = null; }
  const ctx = document.getElementById('chart-commune-weekly');

  const baseDatasets = [
        {
          label: `Année ${profile.current_year}`,
          data: currentIdx,
          rawData: currentData,
          rawUnit: ' ind/piège',
          // hidden: reprend l'état de visibilité mémorisé (cwDatasetVisibility)
          // plutôt que de toujours repartir "visible" — sans ça, activer/
          // désactiver "Prévision saisonnière" recréait le chart de zéro et
          // effaçait les cases décochées par l'utilisateur juste avant.
          hidden: !cwDatasetVisibility[0],
          borderColor: 'rgba(26,84,144,1)',
          backgroundColor: 'rgba(26,84,144,1)',
          pointBackgroundColor: 'rgba(26,84,144,1)',
          pointBorderColor: '#fff',
          pointBorderWidth: 1.5,
          spanGaps: true,
          tension: 0.25,
          pointRadius: 2.5,
          borderWidth: 2,
          order: 1,
          clip: false,
          // Valeurs "repos" mémorisées pour le survol (voir onHover plus bas) :
          // la ligne survolée redevient pleinement opaque + plus épaisse, les
          // autres passent en transparence pour mieux distinguer celle qu'on
          // regarde.
          _baseBorderColor: 'rgba(26,84,144,1)',
          _basePointBg: 'rgba(26,84,144,1)',
          _baseBorderWidth: 2,
          segment: {
            // Trait plein pour les données réelles ; pointillé serré pour la
            // prévision météo court terme ; pointillé plus lâche (moins
            // dense) pour le tronçon issu de la prévision saisonnière — même
            // ligne/couleur, juste un style qui reflète la fiabilité décroissante.
            borderDash: ctx => {
              const w = weeksExt[ctx.p1DataIndex];
              if (!w) return undefined;
              if (w.isSeasonal) return [2, 5];
              if (w.is_forecast) return [6, 4];
              return undefined;
            }
          }
        },
        {
          label: 'Moyenne 2 ans',
          data: mean2Idx,
          rawData: mean2Data,
          rawUnit: ' ind/piège',
          hidden: !cwDatasetVisibility[1],
          borderColor: 'rgba(8,145,178,1)',      // cyan froid — plein, plus de transparence
          backgroundColor: 'rgba(8,145,178,1)',
          pointBackgroundColor: 'rgba(8,145,178,1)',
          pointBorderColor: '#fff',
          pointBorderWidth: 1.5,
          borderWidth: 2,
          // Pointillé plus dense (moins de vide entre les tirets) + trait un
          // peu plus épais qu'avant : la couleur était déjà pleinement
          // opaque, mais un tireté trop espacé ([2,3] = ~40% d'encre) donne
          // une impression de transparence à l'œil — corrigé ici.
          borderDash: [4, 2],
          pointRadius: 3,
          spanGaps: true,
          tension: 0,
          order: 3,
          clip: false,
          _baseBorderColor: 'rgba(8,145,178,1)',
          _basePointBg: 'rgba(8,145,178,1)',
          _baseBorderWidth: 2
        },
        {
          label: 'Moyenne 10 ans',
          data: mean10Idx,
          rawData: mean10Data,
          rawUnit: ' ind/piège',
          hidden: !cwDatasetVisibility[2],
          borderColor: 'rgba(124,58,237,1)',       // violet froid — plein, plus de transparence
          backgroundColor: 'rgba(124,58,237,1)',
          pointBackgroundColor: 'rgba(124,58,237,1)',
          pointBorderColor: '#fff',
          pointBorderWidth: 1.5,
          borderWidth: 2,
          borderDash: [3, 2],
          pointRadius: 3,
          spanGaps: true,
          tension: 0,
          order: 2,
          clip: false,
          _baseBorderColor: 'rgba(124,58,237,1)',
          _basePointBg: 'rgba(124,58,237,1)',
          _baseBorderWidth: 2
        },
        {
          // Température (demande utilisateur) — normalisée région entière
          // (voir tempIdx plus haut), affichée sur le MÊME axe 0-100% que
          // l'Activity Index pour rester dans un seul graphique. Pas de
          // moyenne 2/10 ans dispo pour la température (voir tempData plus
          // haut) — une seule ligne, comme "Année en cours".
          label: 'Température',
          data: tempIdx,
          rawData: tempData,
          rawUnit: '°C',
          hidden: !cwDatasetVisibility[3],
          borderColor: 'rgba(249,115,22,1)',      // orange chaud — distinct des 3 autres lignes
          backgroundColor: 'rgba(249,115,22,1)',
          pointBackgroundColor: 'rgba(249,115,22,1)',
          pointBorderColor: '#fff',
          pointBorderWidth: 1.5,
          borderWidth: 1.5,
          borderDash: [1, 2], // pointillé fin — se distingue des lignes d'abondance (pleines/tiretées plus larges)
          pointRadius: 2,
          spanGaps: true,
          tension: 0.25,
          order: 4,
          clip: false,
          _baseBorderColor: 'rgba(249,115,22,1)',
          _basePointBg: 'rgba(249,115,22,1)',
          _baseBorderWidth: 1.5
        }
  ];

  // Construit toujours les options complètes (mêmes soit la 1ère fois, soit
  // pour une mise à jour en place) — évite de dupliquer ce gros bloc.
  const chartOptions = {
      responsive: true,
      maintainAspectRatio: false,
      interaction: { mode: 'index', intersect: false },
      // Replace le badge HTML si le panneau change de largeur (resize fenêtre).
      onResize: () => positionTodayBadge(markerWeek, todayLabel, weekRangeLabel),
      // Survol d'une ligne précise (mode 'dataset', indépendant du mode
      // 'index' ci-dessus utilisé pour le tooltip) : voir handleCommuneChartHover.
      onHover: (evt, _els, chart) => handleCommuneChartHover(evt, chart),
      // Clic sur une bande de risque ("crochets") — géré ICI plutôt que via le
      // "click" de chaque annotation box (voir buildRiskBandAnnotations) :
      // scale.getValueForPixel() est l'inverse mathématique EXACT de
      // scale.getPixelForValue() utilisée pour positionner ces mêmes boîtes,
      // donc l'index trouvé ici correspond toujours pixel-perfect à la bande
      // visuellement affichée sous le curseur — pas de hit-test séparé du
      // plugin d'annotation qui pourrait diverger du rendu.
      onClick: (evt, _els, chart) => {
        const canvasPos = Chart.helpers.getRelativePosition(evt, chart);
        const xScale = chart.scales.x;
        const rawIdx = xScale.getValueForPixel(canvasPos.x);
        if (rawIdx === undefined || rawIdx === null) return;
        const idx = Math.round(rawIdx);
        if (idx < 0 || idx >= weeksExt.length) return;
        const pt = computeRiskPoints(currentData, mean10Data, mean2Data)[idx];
        if (pt && pt.level) showRiskZoneInfo(pt.level, pt.expected);
      },
      // Petit padding (quelques px, pas la place réservée au label avant) —
      // juste assez pour que le rayon des points ne soit pas coupé par le
      // bord de la zone de tracé quand une valeur touche exactement 0 ou 100.
      layout: { padding: { top: 10, bottom: 8 } },
      scales: {
        x: { title: { display: true, text: yearBoundaryIdx !== -1 ? 'Semaine calendaire (numérotation reprise à 1 après la ligne pointillée — voir année)' : 'Semaine calendaire' } },
        y: { title: { display: true, text: 'Activity Index (%) — Température normalisée (%)' }, min: 0, max: 100 }
      },
      plugins: {
        legend: { display: false },
        tooltip: {
          callbacks: {
            label: (ctx) => {
              const raw = ctx.dataset.rawData?.[ctx.dataIndex];
              const idx = ctx.parsed.y;
              const w = weeksExt[ctx.dataIndex];
              // datasetIndex 0 (Année en cours) et 3 (Température) ont toutes
              // deux un tronçon prévision/saisonnier — la balise s'applique
              // aux deux, pas seulement à l'abondance.
              const tag = w?.isSeasonal ? ' [saisonnier, moins fiable]' : (w?.is_forecast ? ' [prévision]' : '');
              // rawUnit par dataset (ind/piège pour l'abondance, °C pour la
              // température) — au lieu du "ind/piège" fixe d'avant, faux pour
              // la température.
              return `${ctx.dataset.label}: ${idx !== null ? idx.toFixed(0) + '%' : '—'}` +
                     (raw !== null && raw !== undefined ? ` (${raw.toFixed(1)}${ctx.dataset.rawUnit ?? ''})` : '') +
                     ((ctx.datasetIndex === 0 || ctx.datasetIndex === 3) ? tag : '');
            }
          }
        },
        annotation: {
          annotations: {
            // "Crochets" de risque — bandes verticales par tranche de semaines
            // (colore le TEMPS, pas la valeur), cliquables via chartjs-plugin-annotation.
            ...buildRiskBandAnnotations(weeksExt, currentData, mean10Data, mean2Data),
            // Pas de label ici — la ligne "aujourd'hui" reste dessinée dans le
            // canvas (pas de risque de rognage), mais le TEXTE rouge est un
            // vrai <div> HTML positionné au-dessus (#cw-today-badge, voir plus
            // bas) : ça permet de le faire dépasser du graphique sans jamais
            // toucher à sa taille/son padding interne.
            todayLine: {
              type: 'line',
              // markerWeek est une valeur "1-based" (semaine 1..53) — sur l'axe
              // catégoriel, l'index réel du point est markerWeek-1 (voir note
              // détaillée dans buildRiskBandAnnotations plus haut).
              xMin: markerWeek - 1,
              xMax: markerWeek - 1,
              borderColor: '#e53935',
              borderWidth: 2,
              borderDash: [4, 4]
            },
            // Bascule d'année de la prévision saisonnière (voir yearBoundaryIdx
            // plus haut) — n'existe que si l'horizon saisonnier dépasse la fin
            // de l'année civile ET que la numérotation des semaines repart à 1
            // à cet endroit-là (sinon deux "semaine 1" côte à côte seraient
            // ambiguës sans ce repère).
            ...(yearBoundaryIdx !== -1 ? { yearBoundaryLine: {
              type: 'line',
              xMin: yearBoundaryIdx - 0.5,
              xMax: yearBoundaryIdx - 0.5,
              borderColor: '#64748b',
              borderWidth: 1.5,
              borderDash: [6, 3],
              label: {
                display: true,
                content: String(weeksExt[yearBoundaryIdx].nextYear),
                position: 'start',
                backgroundColor: 'rgba(100,116,139,0.9)',
                color: '#fff',
                font: { size: 9, weight: '600' },
                padding: 4
              }
            } } : {})
          }
        }
      }
  };

  // inPlace (seasonal toggle uniquement, voir opts en tête de fonction) :
  // met à jour l'instance Chart.js EXISTANTE au lieu de la détruire/recréer —
  // demande utilisateur ("pas recharger la courbe mais ajouter le morceau
  // qui manque") : décocher/cocher "Prévision saisonnière" faisait
  // disparaître puis réapparaître tout le graphique (destroy() vide le
  // canvas, new Chart() le redessine de zéro), au lieu de juste étendre/
  // raccourcir la ligne existante. Chart.js relit options.onClick/onHover/
  // onResize à chaque évènement (pas besoin de recréer l'instance pour les
  // changer) — seuls data/scales/annotations doivent être réassignés à la
  // main avant .update().
  if (inPlace) {
    communeWeeklyChart.data.labels = labels;
    communeWeeklyChart.data.datasets = baseDatasets;
    communeWeeklyChart.options.onResize = chartOptions.onResize;
    communeWeeklyChart.options.onHover = chartOptions.onHover;
    communeWeeklyChart.options.onClick = chartOptions.onClick;
    communeWeeklyChart.options.scales.x.title.text = chartOptions.scales.x.title.text;
    communeWeeklyChart.options.plugins.annotation.annotations = chartOptions.plugins.annotation.annotations;
    communeWeeklyChart.options.plugins.tooltip.callbacks.label = chartOptions.plugins.tooltip.callbacks.label;
    // 'none' : coupe l'animation de transition par défaut de Chart.js —
    // celle-ci redessine progressivement TOUT le canvas image par image
    // (requestAnimationFrame), ce qui reste visuellement proche d'un
    // "rechargement" même sans destroy()/new Chart(). Avec 'none', la mise à
    // jour est instantanée (un seul repaint), ce qui correspond à "ajouter le
    // morceau qui manque" sans effet de reconstruction.
    communeWeeklyChart.update('none');
  } else {
    communeWeeklyChart = new Chart(ctx, { type: 'line', data: { labels, datasets: baseDatasets }, options: chartOptions });
  }

  positionTodayBadge(markerWeek, todayLabel, weekRangeLabel);
  syncLegendToggleUI();
}


// Positionne le badge HTML "Aujourd'hui"/"Date affichée" au-dessus du canvas,
// aligné horizontalement sur weekNum (via l'échelle X du chart, donc toujours
// juste même si la largeur du panneau change).
function positionTodayBadge(weekNum, todayLabel, weekRangeLabel) {
  const badge = document.getElementById('cw-today-badge');
  if (!communeWeeklyChart || !badge) return;
  const px = communeWeeklyChart.scales.x.getPixelForValue(weekNum - 1); // idem : index 0-based
  badge.textContent = `${todayLabel} · ${weekRangeLabel}`;
  badge.style.left = px + 'px';
  badge.style.display = 'block';
}


// Case "Prévision" du modal commune — indépendante de "Année en cours" (voir
// cwForecastVisible dans state.js pour le pourquoi). Recrée le chart (comme
// le fait déjà onSeasonalToggle) plutôt que de manipuler chart.data en place :
// le filtrage is_forecast se fait au moment de la construction de currentIdx
// dans renderCommuneWeeklyChart, donc le plus simple/fiable est de relancer
// ce même chemin de rendu.
function toggleForecastVisibility(checkbox) {
  cwForecastVisible = checkbox.checked;
  // inPlace:true — même correctif que "Prévision saisonnière" (voir
  // onSeasonalToggle) : sans ça, ce toggle repassait par le chemin
  // destroy()+new Chart(), qui vide le canvas un instant avant de tout
  // redessiner — visuellement une "recharge" de la ligne, alors que rien ne
  // change ici que la visibilité de quelques points (pas besoin de tout
  // reconstruire l'instance Chart.js).
  if (currentProfile) renderCommuneWeeklyChart(currentProfile, { inPlace: true });
}


function toggleDatasetVisibility(checkbox) {
  if (!communeWeeklyChart) return;
  const idx = parseInt(checkbox.dataset.ds, 10);
  if (isNaN(idx)) return;
  cwDatasetVisibility[idx] = checkbox.checked;
  communeWeeklyChart.setDatasetVisibility(idx, checkbox.checked);
  communeWeeklyChart.update();
  syncLegendToggleUI();
}


// Reflète l'état réel du chart (visible/masqué) sur TOUTES les cases de la
// légende, y compris les 2 qui partagent le dataset 0 — appelée après chaque
// (re)création du chart pour que la légende affiche cwDatasetVisibility (pas
// un état "tout visible" par défaut).
function syncLegendToggleUI() {
  if (!communeWeeklyChart) return;
  document.querySelectorAll('#cw-legend input[data-ds]').forEach(cb => {
    const idx = parseInt(cb.dataset.ds, 10);
    cb.checked = communeWeeklyChart.isDatasetVisible(idx);
  });
}
