// ─────────────────────────────────────────────────────────────────────────
// MAP — tout ce qui touche à la carte Leaflet : initialisation, mode
// Commune vs Département (avec la fusion de géométrie turf.js pour un
// département "tout lisse"), rendu des polygones, tooltip au survol.
// ─────────────────────────────────────────────────────────────────────────

function setMapScope(scope, btn){
  mapScope = scope;
  document.getElementById('scope-btn-commune').classList.toggle('active', scope === 'commune');
  document.getElementById('scope-btn-department').classList.toggle('active', scope === 'department');
  if (curTimeList().length) loadCurrent(curTimeIdx());
}

 
function initMap(){
  map=L.map('map',{center:[43.6,3.4],zoom:9,preferCanvas:true});
  L.tileLayer('https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png',{attribution:'CartoDB',subdomains:'abcd',maxZoom:19}).addTo(map);
}

 
function autoDetect(p){
  const k=Object.keys(p),find=(...t)=>k.find(key=>t.some(x=>key.toLowerCase().includes(x)));
  C.abund=find('abundance','abondance')||C.abund;C.temp=find('temperature','temp')||C.temp;
  C.rain=find('rainfall','rain','precipit')||C.rain;C.hum=find('humidity','humidite')||C.hum;
  C.date=find('date','week','semaine')||C.date;C.libgeo=find('libgeo','libelle','nom_com','nom')||C.libgeo;
  C.codgeo=find('codgeo','code_insee','insee')||C.codgeo;
}


function ensureDepartmentGeoJSON(){
  if (departmentGeoJSON !== null) return departmentGeoJSON; // déjà calculé (ou déjà tenté et échoué -> false)
  if (typeof turf === 'undefined') {
    console.error("Turf.js n'a pas pu se charger (CDN) — impossible de fusionner les communes par département.");
    departmentGeoJSON = false;
    return false;
  }
  try {
    const withDep = {
      type: 'FeatureCollection',
      features: baseGeoJSON.features.map(f => ({
        type: 'Feature',
        geometry: f.geometry,
        properties: { dep: (f.properties.code || '').substring(0, 2) }
      }))
    };
    // turf.dissolve exige des Polygon (pas des MultiPolygon) — plusieurs
    // communes côtières (îles/étangs détachés) sont des MultiPolygon dans la
    // géométrie source. turf.flatten() les éclate en plusieurs features
    // Polygon (même propriété "dep" dupliquée sur chaque morceau) avant de
    // dissoudre : les morceaux contigus fusionnent, un morceau isolé (île)
    // reste une feature à part mais garde la même couleur/le même clic
    // (même "dep"), donc aucun impact visuel.
    const flattened = turf.flatten(withDep);
    departmentGeoJSON = turf.dissolve(flattened, { propertyName: 'dep' });
  } catch (e) {
    console.error('Erreur turf.dissolve (fusion des communes par département) :', e);
    departmentGeoJSON = false;
  }
  return departmentGeoJSON;
}


function renderMap(statsFeatures){
  if (mapScope === 'department') { renderDepartmentMap(statsFeatures); return; }

  const cfg=VARS[curVar];
  const statByCode = {};
  statsFeatures.forEach(f => statByCode[f.properties.code] = f.properties);

  const mapData = {
    type:'FeatureCollection',
    features: baseGeoJSON.features.map(f => {
       const stat = statByCode[f.properties.code] || {};
       const props = communeProps[f.properties.code] || {};
       return {
         type:'Feature',
         geometry: f.geometry,
         properties: {
             code: f.properties.code,
             nom: f.properties.nom,
             mean_abundance_albopictus: stat.mean_abundance_albopictus??null,
             mean_temperature: stat.mean_temperature??null,
             mean_humidity: stat.mean_humidity??null,
             mean_rainfall: stat.mean_rainfall??null,
             mean_trend: stat.mean_trend??null,
             population: props.population||null,
             superficie_km2: props.superficie_km2||null
         }
       };
    })
  };

  const isTrend = curVar==='trend';
  const col = cfg.col==='abund'?'mean_abundance_albopictus':'mean_trend';
  document.getElementById('legend-bar').style.background=cfg.grad;
  const fmt3=v=>typeof v==='number'?v<0.001?v.toExponential(1):v<10?v.toFixed(3):v.toFixed(1):v;
  if (isTrend) {
    // 3 catégories discrètes — labels textuels au lieu de min/mid/max numériques.
    setEl('leg-min',cfg.catLabels[0]);setEl('leg-mid',cfg.catLabels[1]);setEl('leg-max',cfg.catLabels[2]);
  } else {
    setEl('leg-min',fmt3(cfg.breaks[0]));setEl('leg-mid',fmt3(cfg.breaks[2]));setEl('leg-max','>'+fmt3(cfg.breaks[4]));
  }
  setEl('legend-label',cfg.label);

  if(geoLayer)map.removeLayer(geoLayer);
  geoLayer=L.geoJSON(mapData,{
    style:f=>{
      const v=parseFloat(f.properties[col]);
      const inZone=f.properties.code === currentModalCodgeo;
      return{fillColor:gk(v,cfg),weight:inZone?3:0.6,color:inZone?'#f59e0b':'#fff',opacity:1,fillOpacity:isNaN(v)?0.15:0.75};
    },
    onEachFeature:(f,layer)=>{layer.on({
      mouseover:e=>{e.target.setStyle({weight:2,color:'#1a5490',fillOpacity:.9});showTooltip(f,e.latlng);},
      mouseout:e=>{geoLayer.resetStyle(e.target);map.closePopup();},
      // Click normal = ouvre le profil hebdomadaire ET sélectionne la commune
      // pour le rapport (contour jaune ci-dessus, remplace toute sélection
      // précédente) — plus de mode de sélection séparé à activer.
      click:e=>{ openCommuneWeeklyModal(f.properties.code, f.properties.nom); }
    });}
  }).addTo(map);
}


// Mode "Département" : UN SEUL polygone par département (voir
// ensureDepartmentGeoJSON), traité exactement comme une commune — même style
// de contour/hover/clic qu'une commune individuelle, juste avec la géométrie
// fusionnée. Les valeurs affichées restent la moyenne (ou somme pour
// population/superficie) de toutes les communes du département.
function renderDepartmentMap(statsFeatures){
  const cfg=VARS[curVar];
  const byDept = {};
  statsFeatures.forEach(f => {
    const dep = (f.properties.code || '').substring(0, 2);
    if (!byDept[dep]) byDept[dep] = { ab: [], temp: [], hum: [], rain: [], trend: [], pop: 0, sup: 0 };
    const p = f.properties, g = byDept[dep];
    const push = (arr, v) => { const n = parseFloat(v); if (!isNaN(n)) arr.push(n); };
    push(g.ab, p.mean_abundance_albopictus);
    push(g.temp, p.mean_temperature);
    push(g.hum, p.mean_humidity);
    push(g.rain, p.mean_rainfall);
    push(g.trend, p.mean_trend);
    g.pop += parseFloat(p.population) || 0;
    g.sup += parseFloat(p.superficie_km2) || 0;
  });

  const dissolved = ensureDepartmentGeoJSON();
  const mapData = {
    type: 'FeatureCollection',
    features: (dissolved ? dissolved.features : []).map(f => {
      const dep = f.properties.dep;
      const g = byDept[dep] || { ab: [], temp: [], hum: [], rain: [], trend: [], pop: 0, sup: 0 };
      return {
        type: 'Feature',
        geometry: f.geometry,
        properties: {
          code: dep, dep,
          nom: `Département ${dep} (moyenne)`,
          mean_abundance_albopictus: g.ab.length ? mean(g.ab) : null,
          mean_temperature: g.temp.length ? mean(g.temp) : null,
          mean_humidity: g.hum.length ? mean(g.hum) : null,
          mean_rainfall: g.rain.length ? mean(g.rain) : null,
          mean_trend: g.trend.length ? mean(g.trend) : null,
          population: g.pop || null,
          superficie_km2: g.sup || null
        }
      };
    })
  };

  const isTrend = curVar==='trend';
  const col = cfg.col==='abund'?'mean_abundance_albopictus':'mean_trend';
  document.getElementById('legend-bar').style.background=cfg.grad;
  const fmt3=v=>typeof v==='number'?v<0.001?v.toExponential(1):v<10?v.toFixed(3):v.toFixed(1):v;
  if (isTrend) {
    setEl('leg-min',cfg.catLabels[0]);setEl('leg-mid',cfg.catLabels[1]);setEl('leg-max',cfg.catLabels[2]);
  } else {
    setEl('leg-min',fmt3(cfg.breaks[0]));setEl('leg-mid',fmt3(cfg.breaks[2]));setEl('leg-max','>'+fmt3(cfg.breaks[4]));
  }
  setEl('legend-label',cfg.label);

  if (geoLayer) map.removeLayer(geoLayer);
  if (!dissolved) {
    // Turf indisponible/erreur : pas de fond à afficher plutôt qu'un rendu
    // à moitié fusionné/à moitié pas (mieux vaut un vide visible qu'un bug
    // silencieux). L'utilisateur peut revenir en mode "Commune".
    return;
  }
  geoLayer = L.geoJSON(mapData, {
    style: f => {
      const v = parseFloat(f.properties[col]);
      return { fillColor: gk(v, cfg), weight: 0.8, color: '#fff', opacity: 1, fillOpacity: isNaN(v) ? 0.15 : 0.8 };
    },
    onEachFeature: (f, layer) => {
      layer.on({
        mouseover: e => { e.target.setStyle({ weight: 2, color: '#1a5490', fillOpacity: .9 }); showTooltip(f, e.latlng); },
        mouseout: e => { geoLayer.resetStyle(e.target); map.closePopup(); },
        click: () => openDeptWeeklyModal(f.properties.dep)
      });
    }
  }).addTo(map);
}

 
function showTooltip(f,latlng){
  const p=f.properties,cfg=VARS[curVar];
  const isAbundTip=curVar==='abundance';
  const isTrendTip=curVar==='trend';
  const colMap={abund:'mean_abundance_albopictus',trend:'mean_trend'};
  const v=parseFloat(p[colMap[cfg.col]]);
  L.popup({closeButton:false,offset:[0,-5],maxWidth:240}).setLatLng(latlng).setContent(
    '<div class="info-popup"><div class="popup-title">'+( p.nom||p.code||'—')+'</div>'+
    '<div class="popup-row"><span>'+cfg.label+'</span><strong>'+(isNaN(v)?'N/D':(isTrendTip?trendCategory(v)+' ('+(v>=0?'+':'')+v.toFixed(1)+' '+cfg.unit+')':v.toFixed(3)+' '+cfg.unit))+'</strong></div>'+
    (!isAbundTip?'<div class="popup-row"><span>Activity Index</span><strong>'+fmt(p.mean_abundance_albopictus,'')+'</strong></div>':'')+
    (!isTrendTip?'<div class="popup-row"><span>Tendance</span><strong>'+fmt(p.mean_trend,'%')+'</strong></div>':'')+
    '</div>'
  ).openOn(map);
}


function zoomToCode(code){
  if(!baseGeoJSON||!code)return;
  const feat=baseGeoJSON.features.find(f=>f.properties.code===code);
  if(!feat)return;
  map.fitBounds(L.geoJSON(feat).getBounds(),{padding:[40,40],maxZoom:13});
}


// ── RECHERCHE DE COMMUNE ─────────────────────────────────────────────────────
// Filtre baseGeoJSON par nom (contient, insensible à la casse) — pas besoin
// d'attendre cachedWfsData/une semaine chargée, baseGeoJSON est dispo dès le
// tout début du chargement (frontières communales).
function onCommuneSearchInput(query){
  const resultsEl = document.getElementById('commune-search-results');
  if (!resultsEl) return;
  const q = (query || '').trim().toLowerCase();
  if (!q || !baseGeoJSON) { resultsEl.style.display = 'none'; resultsEl.innerHTML = ''; return; }
  const matches = baseGeoJSON.features
    .filter(f => (f.properties.nom || '').toLowerCase().includes(q))
    .sort((a, b) => (a.properties.nom || '').localeCompare(b.properties.nom || ''))
    .slice(0, 8);
  if (!matches.length) {
    resultsEl.innerHTML = '<div style="padding:6px 10px;font-size:11px;color:var(--text-muted)">Aucune commune trouvée.</div>';
    resultsEl.style.display = 'block';
    return;
  }
  resultsEl.innerHTML = matches.map(f => {
    const code = f.properties.code, nom = f.properties.nom || code;
    // dataset (pas un argument onclick avec le nom en dur) : évite tout souci
    // d'échappement de guillemets/apostrophes dans les noms de commune (ex.
    // "L'Île" — courant dans les noms de communes françaises).
    return `<div class="commune-search-result" data-code="${code}" data-nom="${nom.replace(/"/g,'&quot;')}" onclick="selectCommuneFromSearch(this.dataset.code,this.dataset.nom)" style="padding:6px 10px;font-size:12px;cursor:pointer;border-bottom:1px solid var(--border)" onmouseover="this.style.background='rgba(26,84,144,.08)'" onmouseout="this.style.background=''">${nom}</div>`;
  }).join('');
  resultsEl.style.display = 'block';
}

function selectCommuneFromSearch(code, nom){
  const input = document.getElementById('commune-search-input');
  const resultsEl = document.getElementById('commune-search-results');
  if (input) input.value = nom;
  if (resultsEl) { resultsEl.style.display = 'none'; resultsEl.innerHTML = ''; }
  if (mapScope !== 'commune') setMapScope('commune', document.getElementById('scope-btn-commune'));
  zoomToCode(code);
  openCommuneWeeklyModal(code, nom);
}

// Ferme la liste de résultats si on clique ailleurs sur la page (pas juste en
// perdant le focus de l'input, sinon un clic sur un résultat lui-même ne
// marcherait jamais — le blur de l'input se déclenche AVANT le click).
document.addEventListener('click', e => {
  const wrap = document.getElementById('commune-search-input');
  const resultsEl = document.getElementById('commune-search-results');
  if (!wrap || !resultsEl) return;
  if (e.target !== wrap && !resultsEl.contains(e.target)) resultsEl.style.display = 'none';
});
