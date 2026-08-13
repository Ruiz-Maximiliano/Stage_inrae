// ─────────────────────────────────────────────────────────────────────────
// MAIN — point d'entrée : doit être chargé EN DERNIER (après tous les
// autres js/*.js), car il démarre effectivement le dashboard.
// ─────────────────────────────────────────────────────────────────────────


// Injecter les boutons ⤢ sur tous les chart-box après le chargement
function injectExpandButtons() {
  document.querySelectorAll('.chart-box').forEach(box => {
    const titleEl = box.querySelector('.chart-title');
    const canvas  = box.querySelector('canvas');
    if (!titleEl || !canvas) return;
    const btn = document.createElement('button');
    btn.className = 'chart-expand-btn';
    btn.title = 'Agrandir';
    btn.textContent = '⤢';
    btn.addEventListener('click', () => expandChart(canvas.id, titleEl.textContent.trim()));
    titleEl.appendChild(btn);
  });
}

 
initMap();initDashboard().finally(()=>injectExpandButtons());

