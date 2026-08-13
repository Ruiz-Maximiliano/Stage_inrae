// ─────────────────────────────────────────────────────────────────────────
// RIGHT-PANEL — panneau de droite : stats de la semaine courante, graphique
// LIME (importance des variables), explication de tendance TM_0_4/TM_0_8.
// ─────────────────────────────────────────────────────────────────────────

 
function renderStats(statsFeatures){
  const valsAb = statsFeatures.map(f=>parseFloat(f.properties.mean_abundance_albopictus)).filter(v=>!isNaN(v));

  // Diagnostic nulls — plus de cadre visuel dédié (retiré, voir "coin inférieur
  // gauche" simplifié), mais gardé en console : utile pour repérer côté
  // pipeline les communes/semaines sans donnée exploitable.
  const totalCommunes = baseGeoJSON ? baseGeoJSON.features.length : 0;
  const missing = Math.max(totalCommunes - valsAb.length, 0);
  if (missing > 0) {
    console.warn(`[stats] ${missing} commune(s) sans donnée d'abondance sur ${totalCommunes} pour cette période.`);
  }

  if(valsAb.length){
    const av=mean(valsAb);
    updateRisk(av/(VARS.abundance.breaks[VARS.abundance.breaks.length-1]||20));
    setEl('h-risk',Math.round(av/(VARS.abundance.breaks[VARS.abundance.breaks.length-1]||20)*100)+'%');
    setEl('h-alerts',valsAb.filter(v=>v>VARS.abundance.breaks[3]).length+' comm.');

    const tps=statsFeatures.map(f=>parseFloat(f.properties.mean_temperature)).filter(v=>!isNaN(v));
    if(valsAb.length===tps.length&&valsAb.length>2){
      const rv=pearson(valsAb,tps); const el=document.getElementById('h-corr');
      // 'h-corr' vivait dans les KPIs du header, retirés du HTML — l'élément
      // n'existe plus, donc null-check obligatoire (contrairement à setEl(),
      // cet accès direct n'en avait pas, d'où le crash qui coupait tout
      // renderStats() en plein milieu et empêchait renderRightPanel/LIME de
      // s'exécuter juste après dans loadWeek()).
      if (el) { el.textContent='r='+rv.toFixed(2);el.className='val'+(Math.abs(rv)>.5?' '+(rv>0?'danger':'warn'):''); }
    }
    prevWeekVals=valsAb;
  }
}

 
function updateRisk(score){
  const bars=document.querySelectorAll('.risk-meter .risk-bar');
  const cls=['active-low','active-med','active-high','active-vhigh'],lbl=['Faible','Modéré','Élevé','Très élevé'];
  const lvl=score<.25?0:score<.5?1:score<.75?2:3;
  bars.forEach((b,i)=>{b.className='risk-bar '+(i<=lvl?cls[i]:'');});
  setEl('risk-label',lbl[lvl]+' ('+Math.round(score*100)+'%)');
}


// Rend le graphique LIME + l'explication de tendance (TM_0_4/TM_0_8) pour les
// features actuellement affichées — appelé après chaque chargement de
// semaine/mois/saison, ET après chaque sélection/désélection de commune sur
// la carte (pour rafraîchir sans refetch).
function renderRightPanel(features){
  lastStatsFeatures = features;
  buildLimeChart(features);
  renderTmExplanation(features);
}


function buildLimeChart(features){
  // Contribution LIME RÉELLE (colonnes lime_TM/lime_UM/lime_RR calculées par
  // le pipeline R — cf. add_lime_explanations() / 00_functions_models.R —
  // et qui ont remplacé le SHAP spatial). Par défaut on moyenne sur toutes
  // les communes affichées ; si une commune est sélectionnée sur la carte
  // (currentModalCodgeo), on montre SA contribution seule.
  const predVars=[
    {col:'lime_TM', label:'Température'},
    {col:'lime_RR', label:'Précipitations'},
    {col:'lime_UM', label:'Humidité'}
  ];

  let subset = features;
  let scopeLabel = `Moyenne régionale (${features.length} communes)`;
  if (currentModalCodgeo) {
    const one = features.find(f => f.properties.code === currentModalCodgeo);
    if (one) {
      subset = [one];
      scopeLabel = `Commune : ${one.properties.nom || currentModalCodgeo}`;
    }
  }
  const scopeEl = document.getElementById('lime-scope-label');
  if (scopeEl) scopeEl.textContent = scopeLabel;

  const items=predVars.map(v=>{
    const vals=subset.map(f=>parseFloat(f.properties[v.col])).filter(x=>!isNaN(x));
    return{label:v.label, lime: vals.length?mean(vals):0, n:vals.length};
  });

  // Semaine "actuelle" (données réelles/observées) vs semaine prédite : LIME
  // explique une PRÉDICTION du modèle, donc n'existe que sur les semaines de
  // forecast — pas sur les semaines avec une vraie observation terrain. Sans
  // ce garde-fou, le chart se "affichait" quand même avec 3 barres à ~0 (donc
  // invisibles) au lieu de prévenir clairement que ce n'est juste pas
  // disponible pour cette période précise.
  const canvasWrap = document.querySelector('#chart-shap')?.closest('.chart-wrap');
  const emptyMsg = document.getElementById('lime-empty-msg');
  const hasData = items.some(i => i.n > 0);
  if (!hasData) {
    destroyChart('chart-shap');
    if (canvasWrap) canvasWrap.style.display = 'none';
    if (emptyMsg) emptyMsg.style.display = 'flex';
    return;
  }
  if (canvasWrap) canvasWrap.style.display = '';
  if (emptyMsg) emptyMsg.style.display = 'none';

  // PAS de tri par |valeur| — ordre volontairement toujours identique
  // (Température / Précipitations / Humidité) d'un rendu à l'autre, pour que
  // les barres ne "sautent" pas de place à chaque changement de semaine/
  // commune (ça rendait le graphique difficile à lire d'un coup d'œil).
  //
  // Échelle FIXE (limeAxisMax, voir api/lime_range.php) plutôt que recalculée
  // à chaque rendu : sinon la même longueur de barre représente une influence
  // différente d'une semaine/commune à l'autre, impossible à comparer d'un
  // coup d'œil. Repli sur l'ancien calcul dynamique si pas encore chargé.
  const maxAbs = limeAxisMax || arrMax([...items.map(i=>Math.abs(i.lime)),0.01]);

  destroyChart('chart-shap');
  charts['chart-shap']=new Chart(document.getElementById('chart-shap'),{
    type:'bar',
    data:{
      labels:items.map(i=>i.label),
      datasets:[{
        data:items.map(i=>i.lime),
        backgroundColor:items.map(i=>i.lime>=0?'rgba(239,68,68,0.75)':'rgba(59,125,216,0.75)'),
        borderColor:items.map(i=>i.lime>=0?'#ef4444':'#3b7dd8'),
        borderWidth:1,
        borderRadius:4,
        barThickness:22
      }]
    },
    options:{
      indexAxis:'y',
      responsive:true,maintainAspectRatio:false,
      plugins:{
        legend:{display:false},
        tooltip:{callbacks:{label:c=>`Influence : ${c.raw.toFixed(3)}  (n=${items[c.dataIndex].n} communes)`}}
      },
      scales:{
        x:{min:-maxAbs*1.1,max:maxAbs*1.1,
           ticks:{color:'#64748b',font:{size:9},maxTicksLimit:5},
           grid:{color:'#e8eef5'},
           title:{display:true,text:'Influence moyenne sur la prédiction',color:'#64748b',font:{size:9}}},
        y:{ticks:{color:'#1e293b',font:{size:11,weight:'600'}},grid:{display:false}}
      }
    }
  });
}


// Explication de la tendance — basée sur les mêmes variables de lag
// température du modèle (tm_0_4/tm_0_8), mais présentées en langage naturel :
// tm_0_4 est une moyenne glissante sur ~4 semaines (≈ "le dernier mois"),
// tm_0_8 sur ~8 semaines (≈ "les 2 derniers mois") — mêmes valeurs, juste
// renommées pour ne plus exposer les noms techniques de colonnes. Seuils
// biologiques du moustique tigre (Aedes albopictus) : sous ~15°C de moyenne,
// le développement larvaire tourne au ralenti (peu/pas d'activité) ; entre
// 15°C et 25°C, conditions favorables (activité en hausse) ; au-delà de
// 25°C, la chaleur/le dessèchement des gîtes larvaires devient défavorable
// (activité qui plafonne ou redescend). Même logique de "scope" que
// buildLimeChart (commune sélectionnée sur la carte, sinon moyenne régionale).
function renderTmExplanation(features){
  const el = document.getElementById('tm-explanation');
  if (!el) return;

  let subset = features;
  let scopeLabel = `Moyenne régionale (${features.length} communes)`;
  if (currentModalCodgeo) {
    const one = features.find(f => f.properties.code === currentModalCodgeo);
    if (one) { subset = [one]; scopeLabel = `Commune : ${one.properties.nom || currentModalCodgeo}`; }
  }

  const moisVals = subset.map(f => parseFloat(f.properties.tm_0_4)).filter(v => !isNaN(v));
  const deuxMoisVals = subset.map(f => parseFloat(f.properties.tm_0_8)).filter(v => !isNaN(v));
  if (!moisVals.length) {
    el.innerHTML = `<div style="opacity:.6">${scopeLabel} — donnée de température non disponible pour cette période.</div>`;
    return;
  }
  const moisTemp = mean(moisVals);
  const deuxMoisTemp = deuxMoisVals.length ? mean(deuxMoisVals) : null;
  const deuxMoisStr = deuxMoisTemp !== null ? deuxMoisTemp.toFixed(1) + '°C' : '—';

  let msg;
  if (moisTemp < 15) {
    msg = `Avec ${moisTemp.toFixed(1)}°C de moyenne sur le dernier mois (et ${deuxMoisStr} sur les 2 derniers mois), <b>les températures sont défavorables</b> au moustique tigre — son développement ralentit fortement sous 15°C, donc peu ou pas de moustiques attendus.`;
  } else if (moisTemp <= 25) {
    msg = `Avec ${moisTemp.toFixed(1)}°C de moyenne sur le dernier mois, les conditions sont <b style="color:var(--danger)">favorables</b> au développement du moustique tigre (zone optimale 15-25°C) — l'activité tend à augmenter.`;
  } else {
    msg = `Avec ${moisTemp.toFixed(1)}°C de moyenne sur le dernier mois, l'activité reste élevée mais la chaleur devient <b style="color:var(--accent2)">défavorable</b> (dessèchement des gîtes larvaires, stress thermique au-delà de 25°C) — l'activité tend à plafonner ou diminuer malgré la chaleur.`;
  }

  el.innerHTML = `<div style="opacity:.6;margin-bottom:4px">${scopeLabel}</div><div>${msg}</div>`;
}
