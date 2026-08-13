// ─────────────────────────────────────────────────────────────────────────
// UTILS — fonctions pures/génériques réutilisées partout : formatage
// (fmt/fmtDate/fmtMonth/fmtShortNum/fmtNum), maths (mean/arrMax/arrMin/
// percentile/pearson), dates (isoDate/weekDateRange/calendarWeekJS), cache
// navigateur (cacheGet/cacheSet), couleurs (gk/hexToRgba/withAlpha), et
// petits helpers DOM (setEl/setLoading/hideLoading/showError).
// ─────────────────────────────────────────────────────────────────────────

// Catégorie texte (Baisse/Stable/Hausse) pour une valeur de tendance donnée —
// mêmes seuils que VARS.trend.breaks, utilisé par la légende et les popups.
// BUG CORRIGÉ — "seuil 20 : en hausse alors que c'est stable". gk() (couleur
// de la carte, plus bas) teste v>=breaks[i] (INCLUSIF, comportement standard
// d'un choroplèthe : une valeur pile sur un seuil bascule dans la tranche du
// dessus). Ce seuil "Hausse" testait avant v>TREND_STABLE_THRESHOLD (EXCLUSIF)
// — à v exactement égal à 20, la carte se coloriait en Hausse (rouge) via
// gk() pendant que ce texte affichait "Stable". Passé en >= pour que les deux
// s'accordent pile sur le seuil (le seuil "Baisse", lui, était déjà cohérent
// avec gk() des deux côtés — v<-20 exclusif ↔ gk() range aussi -20 pile dans
// Stable — donc pas touché).
function trendCategory(v){
  if(v===null||v===undefined||isNaN(v)) return null;
  if(v < -TREND_STABLE_THRESHOLD) return 'Baisse';
  if(v >= TREND_STABLE_THRESHOLD) return 'Hausse';
  return 'Stable';
}

// pct/calcRegBreaks/getActiveCfg/cycleAbundScale (toggle "Échelle: Brute/km²/
// hab.") RETIRÉS — demande utilisateur, une seule échelle désormais.


const fmt=(v,u)=>{const f=parseFloat(v);return isNaN(f)?'N/D':f.toFixed(1)+u;};

const fmtDate=d=>{if(!d)return'—';return new Date(d+'T00:00:00').toLocaleDateString('fr-FR',{day:'numeric',month:'short',year:'numeric'});};

const fmtMonth=m=>{if(!m)return'—';const [y,mo]=m.split('-');return new Date(Date.UTC(+y,+mo-1,1)).toLocaleDateString('fr-FR',{month:'long',year:'numeric'});};

const gk=(v,c)=>{if(isNaN(v)||v===null)return'#1a2235';for(let i=c.breaks.length-1;i>=0;i--)if(v>=c.breaks[i])return c.colors[i];return c.colors[0];};

const setEl=(id,v)=>{const e=document.getElementById(id);if(e)e.textContent=v;};

const mean=a=>a.length?a.reduce((s,b)=>s+b,0)/a.length:0;

// Math.max(...array)/Math.min(...array) plantent ("Maximum call stack size
// exceeded") sur de très grands tableaux (ex. cachedWfsData avec plusieurs
// années d'historique, dizaines de milliers de lignes) — reduce() est sûr
// quelle que soit la taille.
const arrMax=a=>a.reduce((m,v)=>v>m?v:m,-Infinity);

const arrMin=a=>a.reduce((m,v)=>v<m?v:m,Infinity);

const percentile=(a,p)=>{const s=[...a].sort((x,y)=>x-y);return s[Math.floor(s.length*p/100)]||0;};

function pearson(xs,ys){const n=xs.length;if(n<2)return 0;const mx=mean(xs),my=mean(ys);let num=0,dx2=0,dy2=0;for(let i=0;i<n;i++){const dx=xs[i]-mx,dy=ys[i]-my;num+=dx*dy;dx2+=dx*dx;dy2+=dy*dy;}return Math.sqrt(dx2*dy2)<1e-10?0:num/Math.sqrt(dx2*dy2);}

function destroyChart(id){if(charts[id]){charts[id].destroy();delete charts[id];}}

function setLoading(t){const el=document.getElementById('loading');el.style.display='flex';el.innerHTML=`<div class="loader"></div><div class="loading-text">${t}</div>`;}

function hideLoading(){document.getElementById('loading').style.display='none';}

function showError(m){document.getElementById('loading').innerHTML=`<div class="error-box">&#9888; Erreur<br><br>${m}<br><br><button class="btn" style="flex:none;padding:8px 20px" onclick="initDashboard()">Réessayer</button></div>`;}

// Plafond de la navigation temporelle (slider semaine/mois) + de la carte —
// 1er janvier de l'année EN COURS (sys. date), pas une fenêtre glissante de N
// années en arrière (ancien HISTORY_YEARS_BACK, retiré) : demande utilisateur,
// pour ne montrer que l'année en cours partout sur le mapa/panneau gauche.
// Recalculé depuis Date.now() à chaque chargement (pas une année en dur),
// donc cette limite avance automatiquement au 1er janvier suivant le 31/12.
const HISTORY_FLOOR = isoDate(new Date(Date.UTC(new Date().getFullYear(), 0, 1)));


function isoDate(d){ return d.toISOString().slice(0,10); }

function cacheGet(key){
  try {
    const raw = localStorage.getItem(key);
    if (!raw) return null;
    const { t, data } = JSON.parse(raw);
    if (!t || Date.now() - t > CLIENT_CACHE_TTL_MS) return null;
    return data;
  } catch (e) { return null; }
}

function cacheSet(key, data){
  try { localStorage.setItem(key, JSON.stringify({ t: Date.now(), data })); }
  catch (e) { console.warn('Cache navigateur indisponible/plein, on continue sans (', key, ') :', e.message); }
}


function hexToRgba(hex, alpha) {
  const h = hex.replace('#', '');
  const r = parseInt(h.substring(0, 2), 16);
  const g = parseInt(h.substring(2, 4), 16);
  const b = parseInt(h.substring(4, 6), 16);
  return `rgba(${r},${g},${b},${alpha})`;
}


// Remplace juste l'alpha d'une couleur "rgba(r,g,b,a)" déjà construite
// (utilisé pour le survol des lignes du graphique commune, voir onHover).
function withAlpha(rgbaStr, alpha) {
  const m = /rgba?\(([^)]+)\)/.exec(rgbaStr || '');
  if (!m) return rgbaStr;
  const [r, g, b] = m[1].split(',').map(s => s.trim());
  return `rgba(${r},${g},${b},${alpha})`;
}


// Seuils de risque — mêmes seuils que riskLevel() côté api/zone_report.php,
// appliqués ici sur la valeur brute (ind/piège) de l'année en cours.
function riskLevelFromValue(v) {
  if (v === null || v === undefined || isNaN(v)) return null;
  if (v < 3)  return 'Faible';
  if (v < 10) return 'Modéré';
  return 'Élevé';
}


// Format court jj/mm (pas de nom de mois — juste des chiffres).
function fmtShortNum(d) {
  const dd = String(d.getUTCDate()).padStart(2, '0');
  const mm = String(d.getUTCMonth() + 1).padStart(2, '0');
  return `${dd}/${mm}`;
}


// Bornes (lundi→dimanche) de la semaine calendaire "week" pour l'année donnée
// — même ancrage (1er janvier) que calendarWeek() côté PHP.
function weekDateRange(year, week) {
  const start = new Date(Date.UTC(year, 0, 1));
  start.setUTCDate(start.getUTCDate() + (week - 1) * 7);
  const end = new Date(start);
  end.setUTCDate(end.getUTCDate() + 6);
  return { start, end };
}


// Semaine calendaire pour une date "YYYY-MM-DD" — même formule que
// calendarWeek() côté PHP (FLOOR((DOY-1)/7)+1), utilisée pour aligner les
// dates de la prévision saisonnière sur l'axe des semaines du graphique.
function calendarWeekJS(dateStr) {
  const d = new Date(dateStr + 'T00:00:00Z');
  const start = Date.UTC(d.getUTCFullYear(), 0, 1);
  const doy = Math.floor((d.getTime() - start) / 86400000) + 1;
  return Math.floor((doy - 1) / 7) + 1;
}


function downloadMarkdown(text, filename) {
  const blob = new Blob([text], { type: 'text/markdown;charset=utf-8;' });
  const a = document.createElement('a');
  a.href = URL.createObjectURL(blob);
  a.download = filename;
  a.click();
  URL.revokeObjectURL(a.href);
}


function fmtNum(v, decimals) {
  if (v === null || v === undefined) return '—';
  const n = Number(v);
  if (isNaN(n)) return '—';
  return typeof decimals === 'number' ? n.toFixed(decimals) : n.toFixed(1);
}
