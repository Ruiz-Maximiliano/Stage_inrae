// ─────────────────────────────────────────────────────────────────────────
// DATA-LOADER — chargement des données (BD Postgres, avec repli fichier
// local) et navigation temporelle (slider Semaine/Mois/Saisonnier) :
// initDashboard, loadWeek/loadMonth/loadSeason, Précédent/Suivant/Play.
// ─────────────────────────────────────────────────────────────────────────


function recomputeWeeksAndScales(){
  const wSet = new Set(cachedWfsData.map(p => (p[C.date]||'').substring(0,10)).filter(Boolean));
  weeks = [...wSet].sort();
  const mSet = new Set(weeks.map(w => w.substring(0,7)));
  months = [...mSet].sort();

  const _ab=cachedWfsData.map(p=>parseFloat(p[C.abund])).filter(v=>!isNaN(v));
  const _tp=cachedWfsData.map(p=>parseFloat(p[C.temp])).filter(v=>!isNaN(v));
  const _hm=cachedWfsData.map(p=>parseFloat(p[C.hum])).filter(v=>!isNaN(v));
  scatterScales = {
    abMax  : _ab.length ? Math.ceil(arrMax(_ab) * 1.05) : 20,
    tempMin: _tp.length ? Math.floor(arrMin(_tp)) - 1   : 0,
    tempMax: _tp.length ? Math.ceil(arrMax(_tp))  + 1   : 40,
    humMin : _hm.length ? Math.floor(arrMin(_hm)) - 1   : 0,
    humMax : _hm.length ? Math.ceil(arrMax(_hm))  + 1   : 100,
  };
  setEl('h-weeks', weeks.length);
}


async function loadRemainingHistoryInBackground(){
  if (fullHistoryLoaded || !dataLoadedFrom) return;
  // Chargé par tranches d'~1 an — voir le commentaire équivalent dans
  // index.html : une seule requête sur 20+ ans peut dépasser la mémoire/le
  // temps max de PHP et renvoyer une erreur fatale non-JSON côté MAMP.
  try {
    let cursorTo = new Date(new Date(dataLoadedFrom).getTime() - 86400000);
    const floor = new Date(HISTORY_FLOOR);
    while (cursorTo >= floor) {
      const to = isoDate(cursorTo);
      let chunkFromDate = new Date(cursorTo);
      chunkFromDate.setDate(chunkFromDate.getDate() - 364);
      if (chunkFromDate < floor) chunkFromDate = floor;
      const from = isoDate(chunkFromDate);

      const older = await fetchPredictions(from, to);
      if (!older.communes.length) {
        // Tranche vide : on a atteint le vrai début des données (typiquement
        // après 1-2 itérations si la BD ne couvre que quelques mois/années),
        // pas la peine de continuer à interroger la BD tranche par tranche
        // jusqu'en 2000 — c'était la cause principale de lenteur au chargement
        // (~25 aller-retours BD séquentiels dans le vide avant ce correctif).
        break;
      }
      cachedWfsData = [...communesToFlat(older.communes), ...cachedWfsData];

      const curKey = timeScale==='month' ? months[monthIdx] : weeks[weekIdx];
      recomputeWeeksAndScales();
      const sl = document.getElementById('time-slider');
      if (timeScale==='month') {
        sl.max = Math.max(months.length - 1, 0);
        monthIdx = months.indexOf(curKey);
        if (monthIdx < 0) monthIdx = months.length - 1;
        sl.value = monthIdx;
      } else {
        sl.max = Math.max(weeks.length - 1, 0);
        weekIdx = weeks.indexOf(curKey);
        if (weekIdx < 0) weekIdx = weeks.length - 1;
        sl.value = weekIdx;
      }

      if (from === HISTORY_FLOOR) break;
      cursorTo = new Date(chunkFromDate.getTime() - 86400000);
    }
    fullHistoryLoaded = true;
  } catch(e) {
    console.warn('Historique complet pas encore chargé, on continue sans :', e.message);
  }
}


async function initDashboard(){
  setLoading('Chargement frontières (Hérault)...');
  try {
    baseGeoJSON = cacheGet('cache_baseGeoJSON_34');
    if (baseGeoJSON) {
      console.log('[cache] frontières communales chargées depuis le cache navigateur (pas de requête réseau).');
    } else {
      const rGeo = await fetch('https://geo.api.gouv.fr/departements/34/communes?format=geojson&geometry=contour');
      baseGeoJSON = await rGeo.json();
      cacheSet('cache_baseGeoJSON_34', baseGeoJSON);
    }
    setEl('h-communes', baseGeoJSON.features.length);

    setLoading('Chargement des communes...');
    let meta = cacheGet('cache_communes_meta');
    if (meta) {
      console.log('[cache] métadonnées communes chargées depuis le cache navigateur.');
    } else {
      const rMeta = await fetch('data/communes_meta.json');
      if(!rMeta.ok) throw new Error('Métadonnées communes introuvables (HTTP ' + rMeta.status + ')');
      meta = await rMeta.json();
      cacheSet('cache_communes_meta', meta);
    }
    communeProps = {};
    meta.forEach(m => { communeProps[m.codgeo] = {population: m.population, superficie_km2: m.superficie_km2}; });

    // Lancé en arrière-plan (pas de await) : indépendant du reste du chargement,
    // pas la peine de retarder l'affichage de la carte pour ça. Voir
    // fetchRegionMaxAbundance() — remplit regionMaxAbundance dès que prêt, lu
    // par commune-chart.js à l'ouverture du modal d'une commune.
    fetchRegionMaxAbundance();
    fetchLimeAxisRange();

    try {
      setLoading('Chargement des prédictions récentes (BD)...');
      const to = isoDate(new Date(Date.now() + FORECAST_DAYS * 86400000));
      // Plafonné à HISTORY_FLOOR (1er janvier de l'année en cours, voir
      // utils.js) : sans ça, RECENT_DAYS (~40 semaines) déborde sur l'année
      // précédente une bonne partie de l'année (ex. en février, 280 jours en
      // arrière tombe en mai de l'année d'avant) — contraire à la demande
      // "seulement l'année en cours" sur le mapa/panneau gauche.
      const recentFrom = isoDate(new Date(Date.now() - RECENT_DAYS * 86400000));
      const from = recentFrom < HISTORY_FLOOR ? HISTORY_FLOOR : recentFrom;
      // Diagnostic — pour vérifier si l'horodatage du navigateur et la fenêtre
      // de requête envoyée à la BD correspondent bien à ce qu'on attend.
      console.log('[predictions] horloge navigateur (Date.now()) :', new Date().toString());
      console.log('[predictions] fenêtre demandée à la BD :', from, '→', to, '(FORECAST_DAYS =', FORECAST_DAYS, ')');
      const recent = await fetchPredictions(from, to);
      const datesRecus = [...new Set(recent.communes.flatMap(c => c.weeks.map(w => w.date)))].sort();
      console.log('[predictions] dates reçues de la BD — min:', datesRecus[0], '| max:', datesRecus[datesRecus.length - 1], '| total dates distinctes:', datesRecus.length);
      cachedWfsData = communesToFlat(recent.communes);
      dataLoadedFrom = from;
    } catch(apiErr) {
      console.warn('BD indisponible, repli sur le fichier local :', apiErr.message);
      setLoading('BD indisponible — chargement du fichier local...');
      const rLocal = await fetch('data/albo_weekly_herault.json');
      if(!rLocal.ok) throw new Error('Fichier local introuvable (HTTP ' + rLocal.status + ')');
      const grouped = await rLocal.json();
      if(!grouped.length) throw new Error('Aucune donnée trouvée dans le fichier local.');
      cachedWfsData = [];
      grouped.forEach(commune => {
        communeProps[commune.codgeo] = {population: commune.population, superficie_km2: commune.superficie_km2};
        commune.weeks.forEach(w => cachedWfsData.push({
          codgeo: commune.codgeo, libgeo: commune.libgeo, dep: commune.dep,
          date: w.date, date_fin: w.date_fin,
          mean_temperature: w.temperature, mean_rainfall: w.rainfall, mean_humidity: w.humidity,
          mean_abundance_albopictus: w.abundance, sd_abundance_albopictus: w.sd_abundance,
          trend: null,
          lime_TM: null, lime_UM: null, lime_RR: null,
          population: commune.population, superficie_km2: commune.superficie_km2
        }));
      });
      fullHistoryLoaded = true;
    }

    if(!cachedWfsData.length) throw new Error('Aucune donnée de prédiction disponible.');

    autoDetect(cachedWfsData[0]);

    recomputeWeeksAndScales();
    const sl = document.getElementById('time-slider');
    sl.max = weeks.length-1;
    // Démarrer sur la semaine ACTUELLE (comme la météo : on commence
    // aujourd'hui et on voit ce qui vient), pas sur la toute dernière semaine
    // chargée (qui inclut jusqu'à FORECAST_DAYS de prévision future) — même
    // logique que le bouton "Aujourd'hui" (jumpToToday) : dernière semaine <=
    // aujourd'hui, ou la plus récente dispo si aucune ne qualifie.
    const todayStr = isoDate(new Date());
    weekIdx = weeks.length - 1;
    for (let i = weeks.length - 1; i >= 0; i--) {
      if (weeks[i] <= todayStr) { weekIdx = i; break; }
    }
    sl.value = weekIdx;

    await loadWeek(weekIdx);
    hideLoading();

    // Le reste de l'historique (si on vient de la BD) se charge après, sans bloquer l'affichage
    loadRemainingHistoryInBackground();
  } catch(e) { showError(e.message); console.error(e); }
}

 
async function loadWeek(idx){
  const week=weeks[idx];
  // Semaine future (au-delà d'aujourd'hui, forecast court terme) : on le dit
  // clairement à côté de la date, plutôt que de laisser croire que c'est du
  // réel — utile maintenant que la page démarre sur la semaine actuelle et
  // qu'on peut naviguer vers les 1-2 semaines de prévision juste après.
  const weekTag = week && week > isoDate(new Date()) ? ' (prévision)' : '';
  setEl('current-date',fmtDate(week)+weekTag); setEl('week-info','Sem. '+(idx+1)+' / '+weeks.length);
  document.getElementById('time-slider').value=idx;
  try {
    const statsFeatures = cachedWfsData.filter(p => (p[C.date]||'').substring(0,10)===week).map(p => ({
       type: 'Feature', geometry: null, properties: {
         code: p[C.codgeo], nom: p[C.libgeo],
         mean_abundance_albopictus: parseFloat(p[C.abund]),
         mean_temperature: parseFloat(p[C.temp]),
         mean_humidity: parseFloat(p[C.hum]),
         mean_rainfall: parseFloat(p[C.rain]),
         mean_trend: parseFloat(p.trend),
         lime_TM: parseFloat(p.lime_TM),
         lime_UM: parseFloat(p.lime_UM),
         lime_RR: parseFloat(p.lime_RR),
         tm_0_4: parseFloat(p.TM_0_4),
         tm_0_8: parseFloat(p.TM_0_8),
         dep: p[C.dep],
         population: p.population||null,
         superficie_km2: p.superficie_km2||null
       }
    }));

    renderMap(statsFeatures);
    renderStats(statsFeatures);
    renderRightPanel(statsFeatures);
    refreshCommuneChartMarker();

  } catch(e) { console.error(e); }
}


// Même pipeline de rendu que loadWeek(), mais agrège (moyenne) par commune
// toutes les semaines de cachedWfsData tombant dans le mois demandé.
async function loadMonth(idx){
  const month = months[idx];
  const monthTag = month && month > isoDate(new Date()).substring(0,7) ? ' (prévision)' : '';
  setEl('current-date', fmtMonth(month)+monthTag); setEl('week-info', 'Mois ' + (idx + 1) + ' / ' + months.length);
  document.getElementById('time-slider').value = idx;
  try {
    const rowsInMonth = cachedWfsData.filter(p => (p[C.date] || '').substring(0, 7) === month);
    const byCommune = {};
    rowsInMonth.forEach(p => {
      const code = p[C.codgeo];
      if (!byCommune[code]) byCommune[code] = { rows: [], nom: p[C.libgeo], dep: p[C.dep], population: p.population, superficie_km2: p.superficie_km2 };
      byCommune[code].rows.push(p);
    });
    const meanOf = (rows, key) => { const vals = rows.map(r => parseFloat(r[key])).filter(v => !isNaN(v)); return vals.length ? mean(vals) : NaN; };

    const statsFeatures = Object.entries(byCommune).map(([code, g]) => ({
      type: 'Feature', geometry: null, properties: {
        code, nom: g.nom,
        mean_abundance_albopictus: meanOf(g.rows, C.abund),
        mean_temperature: meanOf(g.rows, C.temp),
        mean_humidity: meanOf(g.rows, C.hum),
        mean_rainfall: meanOf(g.rows, C.rain),
        mean_trend: meanOf(g.rows, 'trend'),
        lime_TM: meanOf(g.rows, 'lime_TM'),
        lime_UM: meanOf(g.rows, 'lime_UM'),
        lime_RR: meanOf(g.rows, 'lime_RR'),
        tm_0_4: meanOf(g.rows, 'TM_0_4'),
        tm_0_8: meanOf(g.rows, 'TM_0_8'),
        dep: g.dep,
        population: g.population || null,
        superficie_km2: g.superficie_km2 || null
      }
    }));

    renderMap(statsFeatures);
    renderStats(statsFeatures);
    renderRightPanel(statsFeatures);
    refreshCommuneChartMarker();

  } catch (e) { console.error(e); }
}


// Même pipeline de rendu que loadWeek()/loadMonth(), pour un "instantané"
// (date) de la prévision saisonnière. Pas de LIME pour le saisonnier (le
// pipeline saute volontairement ce calcul ici, voir 07_seasonal_forecast_
// predictions.R, point 5 — coût jugé trop élevé pour une table exploratoire).
async function loadSeason(idx){
  const date = seasons[idx];
  setEl('current-date', fmtDate(date) + ' (saisonnier)');
  setEl('week-info', 'Prév. saisonnière ' + (idx + 1) + ' / ' + seasons.length);
  document.getElementById('time-slider').value = idx;
  try {
    const rowsAtDate = cachedSeasonalMapFlat.filter(p => p.date === date);
    // api/seasonal.php?all=1 ne renvoie pas libgeo (payload allégé) — on
    // récupère le nom depuis baseGeoJSON, déjà chargé pour afficher la carte.
    const nameByCode = {};
    if (baseGeoJSON) baseGeoJSON.features.forEach(f => { nameByCode[f.properties.code] = f.properties.nom; });
    const statsFeatures = rowsAtDate.map(p => ({
      type: 'Feature', geometry: null, properties: {
        code: p.codgeo, nom: nameByCode[p.codgeo] || '',
        mean_abundance_albopictus: parseFloat(p.mean_abundance_albopictus),
        mean_temperature: parseFloat(p.mean_temperature),
        mean_humidity: parseFloat(p.mean_humidity),
        mean_rainfall: parseFloat(p.mean_rainfall),
        mean_trend: parseFloat(p.trend),
        lime_TM: NaN, lime_UM: NaN, lime_RR: NaN,
        tm_0_4: NaN, tm_0_8: NaN, // pas calculés pour la prévision saisonnière (voir 07_seasonal_forecast_predictions.R)
        dep: '34',
        population: p.population || null,
        superficie_km2: p.superficie_km2 || null
      }
    }));

    renderMap(statsFeatures);
    renderStats(statsFeatures);
    renderRightPanel(statsFeatures);
    refreshCommuneChartMarker();

  } catch (e) { console.error(e); }
}

// ── Dispatch Semaine/Mois/Saisonnier : évite de dupliquer prevWeek/nextWeek/togglePlay ──
function curTimeList(){ return timeScale==='month' ? months : (timeScale==='season' ? seasons : weeks); }

function curTimeIdx(){ return timeScale==='month' ? monthIdx : (timeScale==='season' ? seasonIdx : weekIdx); }


// Date "YYYY-MM-DD" actuellement affichée par la navigation temporelle du
// panneau gauche (semaine/mois/saisonnier) — lue par renderCommuneWeeklyChart
// (commune-chart.js) pour positionner la ligne rouge "Aujourd'hui"/le badge
// sur la date QU'ON EST EN TRAIN DE REGARDER plutôt que sur la vraie date du
// jour, quand on navigue dans le temps avec le slider. months[] est au format
// "YYYY-MM" (pas un jour précis) — on prend le 1er du mois par convention.
function selectedDateStr(){
  if (timeScale === 'month') {
    const m = months[monthIdx];
    return m ? m + '-01' : null;
  }
  if (timeScale === 'season') {
    return seasons[seasonIdx] || null;
  }
  return weeks[weekIdx] || null;
}

function loadCurrent(idx){
  if (timeScale==='month') { monthIdx = idx; loadMonth(idx); }
  else if (timeScale==='season') { seasonIdx = idx; loadSeason(idx); }
  else { weekIdx = idx; loadWeek(idx); }
}


// Bouton "Aujourd'hui" (barre de navigation temporelle) : saute directement à
// la semaine/mois actuel(le), sans avoir à faire défiler le slider ou cliquer
// Suiv plusieurs fois. En mode "season", il n'y a que du futur (voir
// ensureSeasonalMapLoaded) — on saute juste à l'échéance la plus proche.
function jumpToToday(){
  const list = curTimeList();
  if (!list.length) return;
  let idx;
  if (timeScale === 'season') {
    idx = 0;
  } else {
    const todayStr = isoDate(new Date());
    // Comparaison de chaînes "YYYY-MM-DD" (ou "YYYY-MM" en mode mois, qui
    // reste correcte : un préfixe se compare toujours "avant" une chaîne plus
    // longue commençant pareil) : dernière entrée <= aujourd'hui.
    idx = list.length - 1;
    for (let i = list.length - 1; i >= 0; i--) {
      if (list[i] <= todayStr) { idx = i; break; }
    }
  }
  document.getElementById('time-slider').value = idx;
  loadCurrent(idx);
}

async function toggleTimeScale(){
  const order = ['week', 'month', 'season'];
  const next = order[(order.indexOf(timeScale) + 1) % order.length];
  const btn = document.getElementById('time-scale-btn');
  const sl  = document.getElementById('time-slider');

  if (next === 'month'){
    timeScale = 'month';
    btn.textContent = '🗓️ Mois';
    sl.max = Math.max(months.length-1, 0);
    const curMonth = (weeks[weekIdx]||'').substring(0,7);
    monthIdx = Math.max(months.indexOf(curMonth), 0);
    sl.value = monthIdx;
    loadMonth(monthIdx);
  } else if (next === 'season'){
    // Chargement à la demande (1ère fois) — table de test, peut être vide.
    btn.disabled = true; btn.textContent = 'Chargement...';
    await ensureSeasonalMapLoaded();
    btn.disabled = false;
    if (!seasons.length){
      alert("Aucune prévision saisonnière disponible pour l'instant (table de test test_seasonal_ruiz vide ou absente).");
      timeScale = 'week';
      btn.textContent = '🗓️ Semaine';
      sl.max = Math.max(weeks.length-1, 0);
      sl.value = weekIdx;
      loadWeek(weekIdx);
      return;
    }
    timeScale = 'season';
    btn.textContent = ' Saisonnier';
    sl.max = Math.max(seasons.length-1, 0);
    seasonIdx = 0;
    sl.value = seasonIdx;
    loadSeason(seasonIdx);
  } else {
    timeScale = 'week';
    btn.textContent = '🗓️ Semaine';
    sl.max = Math.max(weeks.length-1, 0);
    sl.value = weekIdx;
    loadWeek(weekIdx);
  }
}


function setVar(btn){document.querySelectorAll('.var-btn').forEach(b=>b.classList.remove('active'));btn.classList.add('active');curVar=btn.dataset.var;if(curTimeList().length)loadCurrent(curTimeIdx());}

function onSlider(v){loadCurrent(parseInt(v));}

function prevWeek(){const i=curTimeIdx();if(i>0)loadCurrent(i-1);}

function nextWeek(){const i=curTimeIdx();if(i<curTimeList().length-1)loadCurrent(i+1);}

function togglePlay(){
  const btn=document.getElementById('play-btn');
  if(playTimer){clearInterval(playTimer);playTimer=null;btn.textContent='Play';btn.classList.remove('active');}
  else{btn.textContent='Pause';btn.classList.add('active');
    playTimer=setInterval(()=>{const i=curTimeIdx();if(i<curTimeList().length-1){loadCurrent(i+1);}else{clearInterval(playTimer);playTimer=null;btn.textContent='Play';btn.classList.remove('active');}},700);}
}
