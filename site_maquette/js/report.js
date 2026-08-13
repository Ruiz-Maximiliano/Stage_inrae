// ─────────────────────────────────────────────────────────────────────────
// REPORT — génération du rapport (Markdown et PDF via jsPDF) pour la
// commune sélectionnée : buildMarkdownReport/buildPdfReport + le graphique
// hors-DOM utilisé comme image dans le PDF (buildZoneTimelineImage).
// ─────────────────────────────────────────────────────────────────────────


// Rapport pour UNE SEULE commune, déclenché depuis le modal ouvert au clic
// sur la carte — même backend (zone_report.php avec un seul codgeo[]) et
// mêmes builders (buildMarkdownReport/buildPdfReport) que le rapport de zone,
// juste un point d'entrée plus direct (pas besoin du mode sélection).
async function generateCommuneReport(format) {
  if (!currentModalCodgeo) return;
  const qs = `codgeo[]=${encodeURIComponent(currentModalCodgeo)}`;
  const btn = document.getElementById('zr-report-pdf-btn');
  const original = btn ? btn.textContent : null;
  if (btn) { btn.textContent = '⏳ Génération...'; btn.disabled = true; }

  try {
    if (format === 'pdf' && (!window.jspdf || !window.jspdf.jsPDF)) {
      throw new Error("La librairie jsPDF n'a pas pu se charger (vérifier la connexion internet / le CDN).");
    }
    const data = await fetchZoneReportJSON(qs);
    console.log('zone_report.php response (commune unique):', data);
    if (format === 'pdf') {
      buildPdfReport(data);
    } else {
      const md = buildMarkdownReport(data);
      const nomFichier = (data.zone.communes_included[0]?.libgeo || currentModalCodgeo).replace(/\s+/g, '_');
      downloadMarkdown(md, `rapport_${nomFichier}_${data.current_year}_S${data.today_week}.md`);
    }
  } catch (e) {
    alert('Erreur lors de la génération du rapport : ' + e.message + '\n\n(voir la console pour le détail complet)');
    console.error('Erreur génération rapport commune —', e.message, '\nStack:', e.stack);
  } finally {
    if (btn) { btn.textContent = original; btn.disabled = false; }
  }
}


function buildMarkdownReport(data) {
  try {
  const z = data.zone;
  const cw = data.current_week;
  const todayYear = new Date().getFullYear();
  const communesList = z.communes_included.map(c => `${c.libgeo} (${c.codgeo})`).join(', ');

  const lines = [];
  lines.push(`# Rapport de zone d'intérêt — ${z.label}`);
  lines.push('');
  lines.push(`Généré en ${todayYear} — OMEES / Marathon du Web`);
  lines.push('');
  lines.push('| Champ | Valeur |');
  lines.push('|---|---|');
  lines.push(`| Zone sélectionnée | ${z.label} |`);
  lines.push(`| Communes incluses (${z.n_communes}) | ${communesList} |`);
  if (typeof z.population_total === 'number') lines.push(`| Population totale | ${Math.round(z.population_total)} hab. |`);
  if (typeof z.superficie_totale_km2 === 'number') lines.push(`| Superficie totale | ${z.superficie_totale_km2} km² |`);
  lines.push(`| Année de référence | ${data.current_year} |`);
  lines.push(`| Semaine actuelle | Semaine ${data.today_week} |`);
  lines.push('');

  lines.push('## Situation actuelle');
  lines.push('');
  if (cw) {
    lines.push(`- **Activity Index moyen** : ${fmtNum(cw.abundance_q50)} ind/piège (intervalle 90% : ${fmtNum(cw.abundance_q05)} – ${fmtNum(cw.abundance_q95)})`);
    lines.push(`- **Probabilité de présence** : ${cw.presence_prob !== null ? Math.round(cw.presence_prob * 100) + ' %' : '—'}`);
    lines.push(`- **Tendance vs semaine précédente** : ${cw.trend !== null ? (cw.trend >= 0 ? '+' : '') + cw.trend + ' %' : '—'}`);
    lines.push(`- **Climat** : ${fmtNum(cw.temperature)} °C · ${fmtNum(cw.rainfall)} mm de pluie · ${fmtNum(cw.humidity)} % d'humidité`);
  } else {
    lines.push('_Aucune donnée disponible pour la semaine actuelle sur cette zone._');
  }
  lines.push('');

  if (cw && (cw.lime_TM !== null || cw.lime_UM !== null || cw.lime_RR !== null)) {
    lines.push('## Facteurs explicatifs (LIME)');
    lines.push('');
    lines.push('Contribution moyenne de chaque variable climatique à la prédiction de la semaine actuelle (valeurs positives = pousse la prédiction à la hausse) :');
    lines.push('');
    lines.push('| Variable | Contribution (LIME) |');
    lines.push('|---|---|');
    lines.push(`| Température | ${fmtNum(cw.lime_TM, 4)} |`);
    lines.push(`| Humidité | ${fmtNum(cw.lime_UM, 4)} |`);
    lines.push(`| Précipitations | ${fmtNum(cw.lime_RR, 4)} |`);
    lines.push('');
  }

  lines.push('## Comparaison à la normale saisonnière');
  lines.push('');
  lines.push(`Prévision ${data.current_year} vs moyennes historiques (2 et 10 dernières années), semaine calendaire par semaine calendaire, pour la zone sélectionnée. La semaine actuelle est marquée **en gras**.`);
  lines.push('');
  lines.push('| Semaine | Mois | Activity Index ' + data.current_year + ' | Moyenne 2 ans | Moyenne 10 ans | Niveau |');
  lines.push('|---|---|---|---|---|---|');
  data.weeks.forEach(w => {
    if (w.current === null && w.mean_2y === null && w.mean_10y === null) return; // ligne vide, on saute
    const isNow = w.week === data.today_week;
    const cell = v => isNow ? `**${v}**` : v;
    lines.push(`| ${cell(w.week)} | ${cell(w.month)} | ${cell(fmtNum(w.current))}${w.is_forecast ? ' *(prévision)*' : ''} | ${fmtNum(w.mean_2y)} | ${fmtNum(w.mean_10y)} | ${w.level || '—'} |`);
  });
  lines.push('');

  lines.push('## Modèle & méthodologie');
  lines.push('');
  lines.push("**Modèle** — Random Forest deux temps (présence × abondance) sur *Aedes albopictus* (moustique tigre), validation croisée spatiale (LOSO). Prévision météo court terme ~2 semaines ; au-delà, prévision saisonnière (test, moins fiable). Source : base PostgreSQL `taconet_albopictus`, table `albopictus_ruiz_test`.");
  lines.push('');
  lines.push('**Niveau de risque** — Faible < 3 ind/piège · Modéré 3–10 · Élevé ≥ 10.');
  lines.push('');
  lines.push('**Tendance** — Baisse < -20% · Stable entre -20% et +20% · Hausse > +20% (vs semaine précédente).');
  lines.push('');
  lines.push('**Facteurs explicatifs (LIME)** — contribution locale de chaque variable climatique à la prédiction (positif = pousse à la hausse, négatif = pousse à la baisse).');
  lines.push('');
  lines.push('---');
  lines.push('_Rapport généré automatiquement depuis le tableau de bord régional OMEES — projet Marathon du Web._');

  return lines.join('\n');
  } catch (innerErr) {
    console.error('buildMarkdownReport a échoué, data reçue :', data);
    throw new Error('buildMarkdownReport: ' + innerErr.message);
  }
}


// Rend le graphique "année en cours vs moyennes historiques" (mêmes données
// que data.weeks) dans un canvas hors-DOM, et renvoie une image PNG (data
// URL) prête à insérer dans le PDF via doc.addImage(). Le Markdown n'a pas
// d'équivalent (pas de rendu d'image simple en texte) — image PDF only.
function buildZoneTimelineImage(data) {
  const canvas = document.createElement('canvas');
  canvas.width = 900;
  canvas.height = 320;
  const labels = data.weeks.map(w => w.week);
  const currentData = data.weeks.map(w => w.current);
  const mean2Data = data.weeks.map(w => w.mean_2y);
  const mean10Data = data.weeks.map(w => w.mean_10y);
  const todayWeek = data.today_week;

  const chart = new Chart(canvas.getContext('2d'), {
    type: 'line',
    data: {
      labels,
      datasets: [
        { label: `Année ${data.current_year}`, data: currentData, borderColor: '#1a5490', backgroundColor: '#1a5490', pointRadius: 2, borderWidth: 2, spanGaps: true, tension: 0.2 },
        { label: 'Moyenne 2 ans', data: mean2Data, borderColor: '#0891b2', backgroundColor: '#0891b2', pointRadius: 2, borderWidth: 1.5, borderDash: [2, 3], spanGaps: true, tension: 0 },
        { label: 'Moyenne 10 ans', data: mean10Data, borderColor: '#7c3aed', backgroundColor: '#7c3aed', pointRadius: 2, borderWidth: 1.5, borderDash: [1, 3], spanGaps: true, tension: 0 }
      ]
    },
    options: {
      responsive: false,
      animation: false,
      plugins: {
        legend: { display: true, position: 'bottom', labels: { boxWidth: 10, font: { size: 10 } } },
        annotation: { annotations: { todayLine: {
          type: 'line', xMin: todayWeek - 1, xMax: todayWeek - 1, // axe catégoriel : index 0-based
          borderColor: '#e53935', borderWidth: 2, borderDash: [4, 4]
        } } }
      },
      scales: {
        x: { title: { display: true, text: 'Semaine calendaire' } },
        y: { title: { display: true, text: 'Activity Index (ind/piège)' }, beginAtZero: true }
      }
    }
  });

  const imgData = canvas.toDataURL('image/png');
  chart.destroy();
  return imgData;
}


function buildPdfReport(data) {
  try {
    const { jsPDF } = window.jspdf;
    const doc = new jsPDF({ unit: 'mm', format: 'a4' });
    const z = data.zone;
    const cw = data.current_week;
    const todayYear = new Date().getFullYear();
    const pageWidth = doc.internal.pageSize.getWidth();
    const margin = 14;
    let y = 18;

    doc.setFont('helvetica', 'bold');
    doc.setFontSize(15);
    doc.text(`Rapport de zone d'intérêt — ${z.label}`, margin, y);
    y += 6;
    doc.setFont('helvetica', 'normal');
    doc.setFontSize(9);
    doc.setTextColor(120);
    doc.text(`Généré en ${todayYear} — OMEES / Marathon du Web`, margin, y);
    doc.setTextColor(0);
    y += 6;

    const communesList = z.communes_included.map(c => `${c.libgeo} (${c.codgeo})`).join(', ');
    const infoRows = [
      ['Zone sélectionnée', z.label],
      [`Communes incluses (${z.n_communes})`, communesList],
    ];
    if (typeof z.population_total === 'number') infoRows.push(['Population totale', `${Math.round(z.population_total)} hab.`]);
    if (typeof z.superficie_totale_km2 === 'number') infoRows.push(['Superficie totale', `${z.superficie_totale_km2} km²`]);
    infoRows.push(['Année de référence', String(data.current_year)]);
    infoRows.push(['Semaine actuelle', `Semaine ${data.today_week}`]);

    doc.autoTable({
      startY: y,
      margin: { left: margin, right: margin },
      theme: 'plain',
      styles: { fontSize: 8.5, cellPadding: 1.2 },
      columnStyles: { 0: { fontStyle: 'bold', cellWidth: 45 } },
      body: infoRows
    });
    y = doc.lastAutoTable.finalY + 8;

    doc.setFont('helvetica', 'bold'); doc.setFontSize(12);
    doc.text('Situation actuelle', margin, y);
    y += 6;
    doc.setFont('helvetica', 'normal'); doc.setFontSize(9.5);
    const situLines = [];
    if (cw) {
      situLines.push(`Activity Index moyen : ${fmtNum(cw.abundance_q50)} ind/piège (intervalle 90% : ${fmtNum(cw.abundance_q05)} – ${fmtNum(cw.abundance_q95)})`);
      situLines.push(`Probabilité de présence : ${cw.presence_prob !== null ? Math.round(cw.presence_prob * 100) + ' %' : '—'}`);
      situLines.push(`Tendance vs semaine précédente : ${cw.trend !== null ? (cw.trend >= 0 ? '+' : '') + cw.trend + ' %' : '—'}`);
      situLines.push(`Climat : ${fmtNum(cw.temperature)} °C · ${fmtNum(cw.rainfall)} mm de pluie · ${fmtNum(cw.humidity)} % d'humidité`);
    } else {
      situLines.push('Aucune donnée disponible pour la semaine actuelle sur cette zone.');
    }
    situLines.forEach(line => {
      const wrapped = doc.splitTextToSize('• ' + line, pageWidth - margin * 2);
      doc.text(wrapped, margin, y);
      y += wrapped.length * 4.6;
    });
    y += 4;

    if (cw && (cw.lime_TM !== null || cw.lime_UM !== null || cw.lime_RR !== null)) {
      doc.setFont('helvetica', 'bold'); doc.setFontSize(12);
      doc.text('Facteurs explicatifs (LIME)', margin, y);
      y += 5;
      doc.autoTable({
        startY: y,
        margin: { left: margin, right: margin },
        head: [['Variable', 'Contribution (LIME)']],
        body: [
          ['Température', fmtNum(cw.lime_TM, 4)],
          ['Humidité', fmtNum(cw.lime_UM, 4)],
          ['Précipitations', fmtNum(cw.lime_RR, 4)],
        ],
        styles: { fontSize: 8.5 },
        headStyles: { fillColor: [26, 84, 144] }
      });
      y = doc.lastAutoTable.finalY + 8;
    }

    doc.setFont('helvetica', 'bold'); doc.setFontSize(12);
    doc.text('Comparaison à la normale saisonnière', margin, y);
    y += 5;
    doc.setFont('helvetica', 'normal'); doc.setFontSize(8.5);
    const introWrapped = doc.splitTextToSize(
      `Prévision ${data.current_year} vs moyennes historiques (2 et 10 dernières années), semaine calendaire par semaine calendaire. La semaine actuelle est surlignée.`,
      pageWidth - margin * 2
    );
    doc.text(introWrapped, margin, y);
    y += introWrapped.length * 4 + 3;

    // Timeline en image — ajoutée pour donner un visuel d'ensemble avant le
    // détail semaine par semaine ci-dessous.
    try {
      const imgData = buildZoneTimelineImage(data);
      const imgWidth = pageWidth - margin * 2;
      const imgHeight = imgWidth * (320 / 900);
      if (y + imgHeight > doc.internal.pageSize.getHeight() - 20) { doc.addPage(); y = 18; }
      doc.addImage(imgData, 'PNG', margin, y, imgWidth, imgHeight);
      y += imgHeight + 8;
    } catch (imgErr) {
      console.warn('Impossible de générer le graphique pour le PDF :', imgErr);
    }

    const weekRows = data.weeks
      .filter(w => !(w.current === null && w.mean_2y === null && w.mean_10y === null))
      .map(w => [
        String(w.week),
        w.month,
        fmtNum(w.current) + (w.is_forecast ? ' (prév.)' : ''),
        fmtNum(w.mean_2y),
        fmtNum(w.mean_10y),
        w.level || '—'
      ]);

    doc.autoTable({
      startY: y,
      margin: { left: margin, right: margin },
      head: [['Sem.', 'Mois', `Activity Index ${data.current_year}`, 'Moy. 2 ans', 'Moy. 10 ans', 'Niveau']],
      body: weekRows,
      styles: { fontSize: 7.5, cellPadding: 1.3 },
      headStyles: { fillColor: [26, 84, 144] },
      didParseCell: (hookData) => {
        if (hookData.section === 'body') {
          const weekNum = parseInt(hookData.row.raw[0], 10);
          if (weekNum === data.today_week) {
            hookData.cell.styles.fontStyle = 'bold';
            hookData.cell.styles.fillColor = [255, 243, 205];
          }
        }
      }
    });
    y = doc.lastAutoTable.finalY + 8;

    // Bloc descriptif "Modèle & méthodologie" — même contenu que l'encart
    // "ℹ️ Modèle & seuils" du tableau de bord (voir index.html), pour que le
    // rapport PDF reste interprétable de façon autonome (sans avoir le
    // dashboard sous les yeux), une fois les champs Espèce/Modèle/Source/
    // Statut retirés de la table d'en-tête compacte.
    if (y > doc.internal.pageSize.getHeight() - 40) { doc.addPage(); y = 18; }
    doc.setFont('helvetica', 'bold'); doc.setFontSize(12);
    doc.text('Modèle & méthodologie', margin, y);
    y += 6;
    doc.setFont('helvetica', 'normal'); doc.setFontSize(8.5);
    const methodoParas = [
      "Modèle — Random Forest deux temps (présence x abondance) sur Aedes albopictus (moustique tigre), validation croisée spatiale (LOSO). Prévision météo court terme ~2 semaines ; au-delà, prévision saisonnière (test, moins fiable). Source : base PostgreSQL taconet_albopictus, table albopictus_ruiz_test.",
      "Niveau de risque — Faible < 3 ind/piège, Modéré 3-10, Élevé >= 10.",
      "Tendance — Baisse < -20%, Stable entre -20% et +20%, Hausse > +20% (vs semaine précédente).",
      "Facteurs explicatifs (LIME) — contribution locale de chaque variable climatique à la prédiction (positif = pousse à la hausse, négatif = pousse à la baisse)."
    ];
    methodoParas.forEach(p => {
      const wrapped = doc.splitTextToSize(p, pageWidth - margin * 2);
      if (y + wrapped.length * 4.2 > doc.internal.pageSize.getHeight() - 20) { doc.addPage(); y = 18; }
      doc.text(wrapped, margin, y);
      y += wrapped.length * 4.2 + 2.5;
    });

    const pageCount = doc.internal.getNumberOfPages();
    for (let i = 1; i <= pageCount; i++) {
      doc.setPage(i);
      doc.setFontSize(7.5);
      doc.setTextColor(150);
      doc.text(
        'Rapport généré automatiquement — tableau de bord régional OMEES / Marathon du Web',
        margin, doc.internal.pageSize.getHeight() - 8
      );
      doc.text(String(i) + ' / ' + pageCount, pageWidth - margin - 10, doc.internal.pageSize.getHeight() - 8);
    }

    doc.save(`rapport_zone_${data.current_year}_S${data.today_week}.pdf`);
  } catch (innerErr) {
    console.error('buildPdfReport a échoué, data reçue :', data);
    alert('Erreur lors de la construction du PDF : ' + innerErr.message);
  }
}
