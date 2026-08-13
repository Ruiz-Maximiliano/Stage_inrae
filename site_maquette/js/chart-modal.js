// ─────────────────────────────────────────────────────────────────────────
// CHART-MODAL — petit modal générique pour agrandir n'importe quel
// chart-box (bouton ⤢ injecté par injectExpandButtons, voir main.js).
// ─────────────────────────────────────────────────────────────────────────

 
function expandChart(canvasId, title) {
  const src = charts[canvasId];
  if (!src) return;
  document.getElementById('chart-modal-title').textContent = title || canvasId;
  document.getElementById('chart-modal').classList.remove('hidden');
 
  if (modalChart) { modalChart.destroy(); modalChart = null; }
  const destCanvas = document.getElementById('chart-modal-canvas');
  // Copier la config Chart.js source dans le canvas modal
  const cfg = src.config;
  modalChart = new Chart(destCanvas, {
    type: cfg.type,
    data: JSON.parse(JSON.stringify(cfg.data)),
    options: Object.assign({}, cfg.options, {
      responsive: true,
      maintainAspectRatio: false,
      animation: false,
      plugins: Object.assign({}, cfg.options?.plugins, {
        legend: Object.assign({}, cfg.options?.plugins?.legend, { display: true })
      })
    })
  });
}

 
function closeChartModal() {
  document.getElementById('chart-modal').classList.add('hidden');
  if (modalChart) { modalChart.destroy(); modalChart = null; }
}


// Fermer sur clic backdrop
document.getElementById('chart-modal').addEventListener('click', e => {
  if (e.target === document.getElementById('chart-modal')) closeChartModal();
});
