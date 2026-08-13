// ─────────────────────────────────────────────────────────────────────────
// COMMUNE-MODAL — ouverture/fermeture du modal profil (1 commune ou moyenne
// département), toggle de la prévision saisonnière, statut du rapport.
// ─────────────────────────────────────────────────────────────────────────


// Ouvre/ferme la "place" réservée au panneau dans la grille de la page (voir
// body.cw-modal-open dans styles.css — le panneau n'est plus un overlay
// flottant par-dessus la carte, il RÉTRÉCIT la carte). Leaflet ne détecte pas
// tout seul un changement de taille de son conteneur déclenché par du CSS
// pur : sans invalidateSize(), la carte garde ses anciennes dimensions
// internes (tuiles mal alignées / zone morte en bas). setTimeout(...,0) : le
// temps que le navigateur applique le nouveau grid-template-rows avant de
// mesurer le conteneur.
function setCwModalOpen(open) {
  document.body.classList.toggle('cw-modal-open', open);
  setTimeout(() => { if (typeof map !== 'undefined' && map) map.invalidateSize(); }, 0);
}


async function openCommuneWeeklyModal(codgeo, nom) {
  currentModalCodgeo = codgeo;
  currentProfile = null;
  refreshMapSelectionHighlight();
  renderRightPanel(lastStatsFeatures); // rafraîchit LIME + explication tendance sur la nouvelle sélection, sans refetch
  seasonalData = null;
  seasonalEnabled = false;
  cwDatasetVisibility = [true, false, false, false]; // nouvelle commune = repart sur "Année en cours" seule (moyennes + température off par défaut)
  cwForecastVisible = true;
  const forecastCheckbox = document.getElementById('cw-forecast-toggle');
  if (forecastCheckbox) forecastCheckbox.checked = true;
  const seasonalCheckbox = document.getElementById('cw-seasonal-toggle');
  if (seasonalCheckbox) {
    seasonalCheckbox.checked = false;
    seasonalCheckbox.disabled = false; // ré-active (peut avoir été désactivé par openDeptWeeklyModal)
    seasonalCheckbox.parentElement.style.opacity = 1;
  }
  const seasonalNote = document.getElementById('cw-seasonal-note');
  if (seasonalNote) seasonalNote.style.display = 'none';

  document.getElementById('cw-commune-name').textContent = nom || codgeo;
  document.getElementById('cw-status').textContent = 'Chargement...';
  document.getElementById('commune-weekly-modal').classList.remove('hidden');
  setCwModalOpen(true);
  updateZoneReportStatus();

  try {
    const r = await fetch(`api/commune_weekly_profile.php?codgeo=${codgeo}`);
    if (!r.ok) {
      let msg = 'HTTP ' + r.status;
      try { msg = (await r.json()).error || msg; } catch (_) {}
      throw new Error(msg);
    }
    const profile = await r.json();
    currentProfile = profile;
    document.getElementById('cw-status').textContent =
      `Année ${profile.current_year} — semaine actuelle : ${profile.today_week}`;
    // Diagnostic : semaines sans valeur "current" (ni réelle, ni prévision)
    // entre aujourd'hui et la 1ère semaine de prévision trouvée — signale un
    // trou dans les données de prédiction (pas un bug d'affichage).
    const gapWeeks = profile.weeks.filter(w => w.week >= profile.today_week && (w.current === null || w.current === undefined));
    if (gapWeeks.length) {
      console.warn(`[commune ${codgeo}] semaines sans donnée de prévision (trou possible dans predictions) :`, gapWeeks.map(w => w.week));
    }
    console.log(`[commune ${codgeo}] profile reçu :`, profile);
    renderCommuneWeeklyChart(profile);
  } catch (e) {
    document.getElementById('cw-status').textContent = '⚠ ' + e.message;
    console.error(e);
  }
}


// Même panneau/graphique que openCommuneWeeklyModal(), mais pour la moyenne
// d'un DÉPARTEMENT entier (mode carte "Département", voir setMapScope) —
// réutilise api/zone_report.php?dept=XX, qui renvoie déjà exactement la même
// forme (current_year, today_week, weeks[{week, current, mean_2y, mean_10y,
// is_forecast}]) que commune_weekly_profile.php, donc renderCommuneWeeklyChart
// fonctionne sans modification. Pas de prévision saisonnière ici : la table
// test_seasonal_ruiz n'a que des lignes par commune, pas de moyenne dept.
async function openDeptWeeklyModal(deptCode) {
  currentModalCodgeo = null;
  currentProfile = null;
  updateZoneReportStatus(); // pas de commune sélectionnée pour le rapport en mode département
  renderRightPanel(lastStatsFeatures); // LIME/tendance repassent en moyenne régionale
  seasonalData = null;
  seasonalEnabled = false;
  cwDatasetVisibility = [true, false, false, false]; // nouveau département = repart sur "Année en cours" seule (température incluse — zone_report.php la renvoie aussi)
  cwForecastVisible = true;
  const forecastCheckbox = document.getElementById('cw-forecast-toggle');
  if (forecastCheckbox) forecastCheckbox.checked = true;
  const seasonalCheckbox = document.getElementById('cw-seasonal-toggle');
  if (seasonalCheckbox) {
    seasonalCheckbox.checked = false;
    seasonalCheckbox.disabled = true;
    seasonalCheckbox.parentElement.style.opacity = .4;
  }
  const seasonalNote = document.getElementById('cw-seasonal-note');
  if (seasonalNote) { seasonalNote.style.display = 'block'; seasonalNote.textContent = 'Non disponible pour une moyenne de département (prévision saisonnière calculée par commune uniquement).'; }

  document.getElementById('cw-commune-name').textContent = `Département ${deptCode} (moyenne)`;
  document.getElementById('cw-status').textContent = 'Chargement...';
  document.getElementById('commune-weekly-modal').classList.remove('hidden');
  setCwModalOpen(true);

  try {
    const r = await fetch(`api/zone_report.php?dept=${deptCode}`);
    if (!r.ok) {
      let msg = 'HTTP ' + r.status;
      try { msg = (await r.json()).error || msg; } catch (_) {}
      throw new Error(msg);
    }
    const data = await r.json();
    const profile = { current_year: data.current_year, today_week: data.today_week, weeks: data.weeks };
    currentProfile = profile;
    document.getElementById('cw-status').textContent =
      `Année ${profile.current_year} — semaine actuelle : ${profile.today_week} — moyenne sur ${data.zone.n_communes} communes`;
    renderCommuneWeeklyChart(profile);
  } catch (e) {
    document.getElementById('cw-status').textContent = '⚠ ' + e.message;
    console.error(e);
  }
}


// Charge (avec cache) et bascule l'affichage de la prévision saisonnière
// (test_seasonal_ruiz, ~6 mois) en continuation de la ligne de l'année en
// cours, au-delà du forecast court terme réel.
async function onSeasonalToggle() {
  const checkbox = document.getElementById('cw-seasonal-toggle');
  seasonalEnabled = checkbox.checked;
  const noteEl = document.getElementById('cw-seasonal-note');

  if (seasonalEnabled && seasonalData === null) {
    noteEl.style.display = 'block';
    noteEl.textContent = 'Chargement de la prévision saisonnière...';
    try {
      const r = await fetch(`api/seasonal.php?codgeo=${currentModalCodgeo}`);
      seasonalData = await r.json();
    } catch (e) {
      seasonalData = { available: false };
      console.error('Erreur chargement seasonal.php :', e);
    }
  }

  if (seasonalEnabled && seasonalData && seasonalData.available) {
    noteEl.style.display = 'none';
    noteEl.textContent = '';
  } else if (seasonalEnabled) {
    noteEl.style.display = 'block';
    noteEl.textContent = 'Aucune prévision saisonnière disponible pour cette commune.';
  } else {
    noteEl.style.display = 'none';
  }

  // inPlace:true — met à jour le graphique existant (ajoute/retire juste le
  // tronçon saisonnier) au lieu de le détruire/recréer, voir renderCommuneWeeklyChart.
  if (currentProfile) renderCommuneWeeklyChart(currentProfile, { inPlace: true });
}


// Le modal commune est un tiroir en bas d'écran (pas un overlay plein écran,
// voir #commune-weekly-modal en CSS — pointer-events:none sauf sur le tiroir
// lui-même) : le panneau gauche/slider reste utilisable PENDANT qu'il est
// ouvert. Appelée à chaque changement de semaine/mois/saisonnier (voir
// loadWeek/loadMonth/loadSeason dans data-loader.js) pour que la ligne rouge
// "Aujourd'hui"/le badge du graphique suivent en direct la date affichée,
// sans devoir fermer/rouvrir le modal.
function refreshCommuneChartMarker() {
  // inPlace:true — sans ça, chaque tick du slider (semaine/mois/saisonnier)
  // détruisait et recréait tout le graphique juste pour déplacer la ligne
  // rouge, très visible pendant "Play" (défilement automatique).
  if (currentProfile) renderCommuneWeeklyChart(currentProfile, { inPlace: true });
}


function closeCommuneWeeklyModal() {
  document.getElementById('commune-weekly-modal').classList.add('hidden');
  setCwModalOpen(false);
  if (communeWeeklyChart) { communeWeeklyChart.destroy(); communeWeeklyChart = null; }
  const badge = document.getElementById('cw-today-badge');
  if (badge) badge.style.display = 'none';
}


document.getElementById('commune-weekly-modal').addEventListener('click', e => {
  if (e.target === document.getElementById('commune-weekly-modal')) closeCommuneWeeklyModal();
});


// ── COMMUNE SÉLECTIONNÉE (rapport) ────────────────────────────────────────────
// Plus de "mode sélection" à activer ni de multi-sélection : un click normal
// sur une commune (celui qui ouvre déjà son profil hebdomadaire, voir
// openCommuneWeeklyModal) la sélectionne aussi pour le rapport — contour
// jaune sur la carte, remplace toute sélection précédente. currentModalCodgeo
// (déjà utilisé pour le profil/saisonnier) sert de source unique de vérité.
function updateZoneReportStatus() {
  const el = document.getElementById('zr-status');
  const btn = document.getElementById('zr-report-pdf-btn');
  if (!el || !btn) return;
  if (currentModalCodgeo) {
    const nom = document.getElementById('cw-commune-name')?.textContent || currentModalCodgeo;
    el.textContent = nom;
    btn.disabled = false;
  } else {
    el.textContent = 'Aucune commune sélectionnée — cliquez une commune sur la carte';
    btn.disabled = true;
  }
}
