/* Bolsillo — app. Depende de BolsilloParser (parser.js). */
(function () {
  'use strict';
  const P = window.BolsilloParser;
  const $ = (s, r) => (r || document).querySelector(s);
  const $$ = (s, r) => Array.from((r || document).querySelectorAll(s));

  // ---------------------------------------------------------------- iconos
  const ICON = {
    comida: '<path d="M3 4h2.2l2.3 10.3a1 1 0 0 0 1 .7h8.7a1 1 0 0 0 1-.8L20 7.5H6.1"/><circle cx="9" cy="19.2" r="1.3"/><circle cx="17" cy="19.2" r="1.3"/>',
    resto: '<path d="M4.5 9.5h11v3.2A5.3 5.3 0 0 1 10.2 18h-.4a5.3 5.3 0 0 1-5.3-5.3z"/><path d="M15.5 10.5h1.6a2.4 2.4 0 0 1 0 4.8h-2"/><path d="M8 3.5v2.5M12 3.5v2.5"/><path d="M4 21h12"/>',
    transporte: '<path d="M4 16.5v-4.3L6.3 7a1.6 1.6 0 0 1 1.5-1h8.4a1.6 1.6 0 0 1 1.5 1l2.3 5.2v4.3"/><path d="M3.5 12.5h17v4h-17z"/><path d="M6.5 16.5v2M17.5 16.5v2"/><circle cx="7.5" cy="14.5" r=".6"/><circle cx="16.5" cy="14.5" r=".6"/>',
    personal: '<circle cx="12" cy="7.8" r="3.4"/><path d="M5 20a7 7 0 0 1 14 0"/>',
    casa: '<path d="M3.5 11 12 4l8.5 7"/><path d="M6 9.5V20h12V9.5"/><path d="M10 20v-5h4v5"/>',
    salud: '<path d="M12 20s-7.5-4.5-7.5-10.2A4.1 4.1 0 0 1 12 7.4a4.1 4.1 0 0 1 7.5 2.4C19.5 15.5 12 20 12 20z"/>',
    ocio: '<path d="m12 3.6 2.6 5.3 5.8.8-4.2 4.1 1 5.8L12 16.9l-5.2 2.7 1-5.8-4.2-4.1 5.8-.8z"/>',
    noah: '<circle cx="12" cy="12.5" r="7.5"/><path d="M9 15a3.6 3.6 0 0 0 6 0"/><path d="M12 5c-1 0-1.8-.8-1.8-1.6"/><circle cx="9.4" cy="11.2" r=".5"/><circle cx="14.6" cy="11.2" r=".5"/>',
    compras: '<path d="M5.5 8h13l-1.1 12H6.6z"/><path d="M9 10V7a3 3 0 0 1 6 0v3"/>',
    otros: '<circle cx="6" cy="12" r="1.2"/><circle cx="12" cy="12" r="1.2"/><circle cx="18" cy="12" r="1.2"/>',
    custom: '<path d="M3.5 12.2V4.5a1 1 0 0 1 1-1h7.7l8.3 8.3-8.7 8.7z"/><circle cx="8" cy="8" r="1.3"/>',
    atm: '<rect x="2.8" y="6.5" width="18.4" height="11" rx="2.2"/><circle cx="12" cy="12" r="2.4"/><path d="M6 9.5v5M18 9.5v5"/>',
    exchange: '<path d="M5 8h13.5M15 4.5 18.5 8 15 11.5"/><path d="M19 16H5.5M9 12.5 5.5 16 9 19.5"/>',
    incoming: '<path d="M12 4v12M6.5 10.5 12 16l5.5-5.5"/><path d="M5 20h14"/>',
    adjust: '<path d="M4 8h10M18 8h2M4 16h2M10 16h10"/><circle cx="16" cy="8" r="2"/><circle cx="8" cy="16" r="2"/>',
    mic: '<rect x="9" y="3" width="6" height="11.5" rx="3"/><path d="M5.5 11.5a6.5 6.5 0 0 0 13 0"/><path d="M12 18v3"/>',
    stop: '<rect x="7" y="7" width="10" height="10" rx="2.2" fill="currentColor"/>',
    gear: '<circle cx="12" cy="12" r="3"/><path d="M19.4 13.5a7.6 7.6 0 0 0 0-3l2-1.6-2-3.4-2.4.9a7.6 7.6 0 0 0-2.6-1.5L14 2.5h-4l-.4 2.4A7.6 7.6 0 0 0 7 6.4l-2.4-.9-2 3.4 2 1.6a7.6 7.6 0 0 0 0 3l-2 1.6 2 3.4 2.4-.9a7.6 7.6 0 0 0 2.6 1.5l.4 2.4h4l.4-2.4a7.6 7.6 0 0 0 2.6-1.5l2.4.9 2-3.4z"/>',
    send: '<path d="M12 19V5M6 11l6-6 6 6"/>',
    plus: '<path d="M12 5v14M5 12h14"/>',
    home: '<path d="M3.5 11 12 4l8.5 7"/><path d="M6 9.5V20h4.5v-5h3v5H18V9.5"/>',
    list: '<path d="M9 6.5h11M9 12h11M9 17.5h11"/><circle cx="4.8" cy="6.5" r="1.1"/><circle cx="4.8" cy="12" r="1.1"/><circle cx="4.8" cy="17.5" r="1.1"/>',
    chart: '<path d="M4 20h16"/><rect x="5.5" y="11" width="3" height="6.5" rx="1"/><rect x="10.5" y="6" width="3" height="11.5" rx="1"/><rect x="15.5" y="13.5" width="3" height="4" rx="1"/>',
    left: '<path d="m14.5 5.5-6.5 6.5 6.5 6.5"/>',
    right: '<path d="m9.5 5.5 6.5 6.5-6.5 6.5"/>',
    search: '<circle cx="10.5" cy="10.5" r="6"/><path d="m15 15 5 5"/>',
    x: '<path d="M7 7l10 10M17 7 7 17"/>',
    info: '<circle cx="12" cy="12" r="8.5"/><path d="M12 11v5.5M12 7.6v.4"/>',
    quote: '<path d="M5 11.5h4v5H5zM5 11.5C5 8.5 6.5 7 9 6.5M14 11.5h4v5h-4zM14 11.5c0-3 1.5-4.5 4-5"/>',
    keyboard: '<rect x="2.5" y="6" width="19" height="12" rx="2"/><path d="M6 10h.01M9.5 10h.01M13 10h.01M16.5 10h.01M7.5 14h9"/>'
  };
  function svg(name, extra) {
    return '<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.9" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"' + (extra || '') + '>' + (ICON[name] || ICON.otros) + '</svg>';
  }

  // ---------------------------------------------------------------- catálogo
  const BUILTIN = [
    { id: 'comida', name: 'Comida', hint: 'Súper, mercado, feria', color: '#2f9e44' },
    { id: 'resto', name: 'Restaurantes', hint: 'Restaurantes y cafés', color: '#e8590c' },
    { id: 'transporte', name: 'Transporte', hint: 'Taxis, trufis, viajes a Santa Cruz', color: '#0a72e8' },
    { id: 'personal', name: 'Personal del hogar', short: 'Personal', hint: 'Limpieza, niñera', color: '#8e44c9' },
    { id: 'casa', name: 'Casa', hint: 'Alquiler, luz, agua, gas, internet', color: '#9c6b3c' },
    { id: 'salud', name: 'Salud', hint: 'Farmacia, médico', color: '#e03131' },
    { id: 'ocio', name: 'Ocio', hint: 'Excursiones, actividades, viajes', color: '#d6336c' },
    { id: 'noah', name: 'Noah', hint: 'Pañales, juguetes, cole', color: '#1689ad' },
    { id: 'compras', name: 'Compras', hint: 'Ropa, regalos, peluquería', color: '#b07d00' },
    { id: 'otros', name: 'Otros', hint: 'Todo lo demás', color: '#7c7c85' }
  ];
  const CUSTOM_COLORS = ['#5856d6', '#00897b', '#c2255c', '#6d5d45', '#1c7ed6', '#d9480f', '#495057', '#7048e8'];
  const WHO = [
    { id: 'familia', name: 'Familia' },
    { id: 'pareja', name: 'Sol y Pol' },
    { id: 'pol', name: 'Pol' },
    { id: 'sol', name: 'Sol' },
    { id: 'noah', name: 'Noah' }
  ];
  const SOURCES = [
    { id: 'atm', name: 'Cajero', icon: 'atm', color: '#157a55' },
    { id: 'exchange', name: 'Cambio de divisas', short: 'Cambio', icon: 'exchange', color: '#0b7f8a' },
    { id: 'other', name: 'Otra entrada', short: 'Otra', icon: 'incoming', color: '#3b7d3b' }
  ];
  const PEOPLE = ['Pol', 'Sol'];
  const DEFAULT_RATES = { USD: 6.96, EUR: 7.5 };

  // ---------------------------------------------------------------- utilidades
  const esc = s => String(s == null ? '' : s).replace(/[&<>"']/g, c => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c]));
  let NF0, NF2;
  try {
    NF0 = new Intl.NumberFormat('es-BO', { maximumFractionDigits: 0, useGrouping: 'always' });
    NF2 = new Intl.NumberFormat('es-BO', { minimumFractionDigits: 2, maximumFractionDigits: 2, useGrouping: 'always' });
    if (NF0.format(1234).indexOf('1') !== 0 || NF0.format(1234) === '1234') throw 0;
  } catch (e) {
    NF0 = new Intl.NumberFormat('de-DE', { maximumFractionDigits: 0 });
    NF2 = new Intl.NumberFormat('de-DE', { minimumFractionDigits: 2, maximumFractionDigits: 2 });
  }
  const r2 = v => Math.round(v * 100) / 100;
  function num(v) { v = r2(Math.abs(v || 0)); return Number.isInteger(v) ? NF0.format(v) : NF2.format(v); }
  function money(v) { return 'Bs ' + num(v); }
  function moneyHTML(v) { return '<span class="cur">Bs</span>' + (v < 0 ? '−' : '') + num(v); }
  const ymd = P.ymd;
  const todayStr = () => ymd(new Date());
  const monthOf = d => d.slice(0, 7);
  function parseYmd(s) { const [y, m, d] = s.split('-').map(Number); return new Date(y, m - 1, d); }
  const DF_LONG = new Intl.DateTimeFormat('es', { weekday: 'long', day: 'numeric', month: 'long' });
  const DF_DAY = new Intl.DateTimeFormat('es', { weekday: 'long', day: 'numeric', month: 'short' });
  const DF_SHORT = new Intl.DateTimeFormat('es', { day: 'numeric', month: 'short' });
  const DF_MONTH = new Intl.DateTimeFormat('es', { month: 'long', year: 'numeric' });
  const DF_MONTH_ONLY = new Intl.DateTimeFormat('es', { month: 'long' });
  const DF_TIME = new Intl.DateTimeFormat('es', { hour: '2-digit', minute: '2-digit' });
  function dayLabel(s) {
    const t = todayStr();
    if (s === t) return 'Hoy';
    const y = new Date(); y.setDate(y.getDate() - 1);
    if (s === ymd(y)) return 'Ayer';
    const d = parseYmd(s);
    const str = DF_DAY.format(d);
    return d.getFullYear() !== new Date().getFullYear() ? str + ' ' + d.getFullYear() : str;
  }
  const cap = s => s ? s.charAt(0).toUpperCase() + s.slice(1) : s;
  const newId = p => (p || 't') + Date.now().toString(36) + Math.random().toString(36).slice(2, 7);
  function parseAmount(s) {
    if (s == null) return null;
    const t = String(s).replace(/[^\d.,]/g, '');
    if (!t) return null;
    const v = P.digitValue(t);
    return v && v > 0 ? r2(v) : null;
  }
  function amountStr(v) { return v == null ? '' : (Number.isInteger(v) ? String(v) : String(r2(v)).replace('.', ',')); }
  const ls = {
    get(k) { try { return window.localStorage.getItem(k); } catch (e) { return null; } },
    set(k, v) { try { window.localStorage.setItem(k, v); return true; } catch (e) { return false; } }
  };

  // ---------------------------------------------------------------- estado
  const S = {
    mode: 'loading', // loading | shared | local
    txns: new Map(), config: {}, configExists: false,
    loaded: false, stale: false, canWrite: true, uid: null,
    me: null,
    tab: 'home', month: monthOf(todayStr()),
    hist: { f: 'all', q: '', cat: null, month: null, limit: 80 },
    dict: null, dictState: 'idle', dictLang: true, finals: '', partial: '', wantProcess: false,
    downloads: null,
    sheet: null
  };

  function cats() {
    const ov = S.config.catOverrides || {};
    const list = BUILTIN.map(c => Object.assign({}, c, {
      name: (ov[c.id] && ov[c.id].name) || c.name,
      short: (ov[c.id] && ov[c.id].name) ? null : c.short,
      keywords: (ov[c.id] && ov[c.id].keywords) || [], builtin: true, icon: c.id
    }));
    const cu = S.config.customCats || {};
    const custom = Object.keys(cu).map(id => Object.assign({ id, icon: 'custom', keywords: [] }, cu[id]))
      .filter(c => !c.deleted && c.name).sort((a, b) => (a.createdAt || 0) - (b.createdAt || 0));
    const otros = list.pop();
    return list.concat(custom, [otros]);
  }
  function catById(id) { const all = cats(); return all.find(c => c.id === id) || all[all.length - 1]; }
  function catShort(c) { return c.short || c.name; }
  function rates() { return Object.assign({}, DEFAULT_RATES, S.config.rates || {}); }
  function whoName(id) { const w = WHO.find(x => x.id === id); return w ? w.name : 'Familia'; }
  function srcOf(id) { return SOURCES.find(s => s.id === id) || SOURCES[2]; }

  function allTxns() {
    return Array.from(S.txns.entries()).map(([id, d]) => Object.assign({ id }, d))
      .filter(t => t && typeof t.amount === 'number' && isFinite(t.amount) && typeof t.date === 'string')
      .sort((a, b) => (a.date < b.date ? 1 : a.date > b.date ? -1 : (b.createdAt || 0) - (a.createdAt || 0)));
  }
  function balance(list) {
    let b = 0;
    for (const t of list) {
      if (t.type === 'cash_in') b += t.amount;
      else if (t.type === 'adjust') b += t.amount;
      else if (t.type === 'expense' && t.method !== 'qr') b -= t.amount;
    }
    return r2(b);
  }

  // ---------------------------------------------------------------- almacenamiento
  const Store = {
    db: null,
    configQueue: Promise.resolve(),
    initLocal(reason) {
      S.mode = 'local'; S.localReason = reason || 'standalone';
      let data = null;
      try { data = JSON.parse(ls.get('bolsillo.data.v1') || 'null'); } catch (e) { data = null; }
      data = data || { txns: {}, config: {} };
      S.txns = new Map(Object.entries(data.txns || {}));
      S.config = data.config || {};
      S.configExists = true; S.loaded = true;
      renderAll();
    },
    saveLocal() {
      const obj = { txns: Object.fromEntries(S.txns), config: S.config };
      if (!ls.set('bolsillo.data.v1', JSON.stringify(obj))) toast('No se pudo guardar en este dispositivo.');
    },
    initShared(db) {
      this.db = db; S.mode = 'shared';
      const timer = setTimeout(() => { if (!S.confirmed) { S.stale = true; renderBanners(); } }, 10000);
      db.collection('txns').onSnapshot(snap => {
        const m = new Map();
        snap.docs.forEach(d => { if (d.exists) m.set(d.id, d.data()); });
        S.txns = m; S.loaded = true;
        if (!snap.metadata.fromCache) { S.confirmed = true; S.stale = false; clearTimeout(timer); }
        renderAll();
      }, err => { S.loaded = true; S.stale = true; S.dbError = err && err.code; clearTimeout(timer); renderAll(); });
      db.doc('config/main').onSnapshot(d => {
        S.config = d.exists ? (d.data() || {}) : {};
        S.configExists = !!d.exists;
        if (S.uid && S.config.members && S.config.members[S.uid]) { S.me = S.config.members[S.uid]; ls.set(meKey(), S.me); }
        renderAll();
      }, () => {});
      renderAll();
    },
    async put(id, doc) {
      if (S.mode === 'local') { S.txns.set(id, doc); this.saveLocal(); renderAll(); return; }
      return this.retry(() => this.db.collection('txns').doc(id).set(doc));
    },
    async del(id) {
      if (S.mode === 'local') { S.txns.delete(id); this.saveLocal(); renderAll(); return; }
      return this.retry(() => this.db.collection('txns').doc(id).delete());
    },
    patchConfig(patch) {
      if (S.mode === 'local') {
        S.config = deepMerge(JSON.parse(JSON.stringify(S.config || {})), patch);
        this.saveLocal(); renderAll(); return Promise.resolve();
      }
      const run = () => {
        const ref = this.db.doc('config/main');
        return this.retry(() => S.configExists ? ref.update(patch) : ref.set(deepMerge(JSON.parse(JSON.stringify(S.config || {})), patch)))
          .then(() => { S.configExists = true; });
      };
      this.configQueue = this.configQueue.then(run, run);
      return this.configQueue;
    },
    async retry(fn) {
      try { return await fn(); }
      catch (e) {
        if (e && e.code === 'unavailable') {
          await new Promise(r => setTimeout(r, 400 + Math.random() * 800));
          try { return await fn(); } catch (e2) { throw writeError(e2); }
        }
        throw writeError(e);
      }
    }
  };
  function writeError(e) {
    const code = e && e.code;
    if (code === 'invalid_argument' || code === 'not_granted') { S.canWrite = false; renderBanners(); }
    return e || { code: 'unavailable' };
  }
  function errMsg(e) {
    const c = e && e.code;
    if (c === 'invalid_argument' || c === 'not_granted') return 'No tienes permiso para guardar. Pide acceso de Editor.';
    if (c === 'quota_exceeded') return 'El almacenamiento está lleno. Borra movimientos antiguos.';
    if (c === 'revoked') return 'Se ha retirado el acceso a los datos.';
    return 'No se pudo guardar. Revisa la conexión e inténtalo de nuevo.';
  }
  function deepMerge(a, b) {
    for (const k of Object.keys(b)) {
      if (b[k] && typeof b[k] === 'object' && !Array.isArray(b[k])) a[k] = deepMerge(a[k] && typeof a[k] === 'object' ? a[k] : {}, b[k]);
      else a[k] = b[k];
    }
    return a;
  }

  // ---------------------------------------------------------------- render: comunes
  function tileHTML(t, small) {
    let color, icon;
    if (t.type === 'cash_in') { const s = srcOf(t.source); color = s.color; icon = s.icon; }
    else if (t.type === 'adjust') { color = '#6b6b73'; icon = 'adjust'; }
    else { const c = catById(t.category); color = c.color; icon = c.icon; }
    return '<span class="tile' + (small ? ' sm' : '') + '" style="background:' + esc(color) + '">' + svg(icon) + '</span>';
  }
  function rowHTML(t, withDay) {
    let title, sub = [], amt, cls = '';
    if (t.type === 'cash_in') {
      const s = srcOf(t.source);
      title = t.note || s.name;
      sub.push(s.short || s.name);
      if (t.currency && t.currency !== 'BOB' && t.origAmount) sub.push(num(t.origAmount) + ' ' + t.currency);
      amt = '+' + money(t.amount); cls = 'in';
    } else if (t.type === 'adjust') {
      title = t.note || 'Ajuste de efectivo';
      sub.push('Ajuste');
      amt = (t.amount < 0 ? '−' : '+') + money(t.amount); cls = 'adj';
    } else {
      const c = catById(t.category);
      title = t.note || c.name;
      sub.push(catShort(c), whoName(t.who));
      if (t.method === 'qr') sub.push('QR / tarjeta');
      amt = money(t.amount);
    }
    if (withDay) sub.unshift(dayLabel(t.date));
    return '<li><button class="row" type="button" data-edit="' + esc(t.id) + '">' + tileHTML(t) +
      '<span class="txt"><span class="t1 t-note"></span><span class="t2">' + esc(sub.join(' · ')) + '</span></span>' +
      '<span class="amt ' + cls + '">' + esc(amt) + '</span></button></li>';
  }
  // Texto escrito por personas se inserta con textContent
  function fillNotes(container, list) {
    const byId = new Map(list.map(t => [t.id, t]));
    $$('[data-edit]', container).forEach(b => {
      const t = byId.get(b.getAttribute('data-edit'));
      const el = $('.t-note', b);
      if (!t || !el) return;
      let title;
      if (t.type === 'cash_in') title = t.note || srcOf(t.source).name;
      else if (t.type === 'adjust') title = t.note || 'Ajuste de efectivo';
      else title = t.note || catById(t.category).name;
      el.textContent = title;
    });
  }

  function renderAll() {
    renderBanners();
    renderHome();
    if (S.tab === 'hist') renderHist();
    if (S.tab === 'sum') renderSum();
    if (S.sheet && S.sheet.kind === 'settings') { /* los ajustes se re-renderizan sólo al abrir para no perder el foco */ }
  }

  function renderBanners() {
    const el = $('#banners');
    const parts = [];
    if (S.mode === 'local' && S.localReason !== 'standalone-quiet') {
      parts.push('<div class="banner">' + svg('info') + '<span><strong>Solo en este dispositivo.</strong> ' +
        (S.localReason === 'nodb' ? 'Inicia sesión en Claude para compartir los datos con tu familia.' : 'Los datos se guardan en este navegador.') + '</span></div>');
    }
    if (S.mode === 'shared' && !S.canWrite) {
      parts.push('<div class="banner">' + svg('info') + '<span><strong>Solo lectura.</strong> Pide a quien creó Bolsillo que te invite como Editor para poder anotar gastos.</span></div>');
    }
    if (S.mode === 'shared' && S.stale) {
      parts.push('<div class="banner">' + svg('info') + '<span><strong>Sin conexión con los datos.</strong> ' +
        (S.txns.size ? 'Lo que ves puede no estar al día.' : 'Revisa la conexión; se cargarán solos en cuanto vuelva.') + '</span></div>');
    }
    el.innerHTML = parts.join('');
  }

  // ---------------------------------------------------------------- render: inicio
  function renderHome() {
    $('#today-label').textContent = cap(DF_LONG.format(new Date()));
    if (!S.loaded) {
      $('#cash-total').innerHTML = '<span class="cur">Bs</span>—';
      $('#recent').innerHTML = '<li class="loading">Cargando…</li>';
      return;
    }
    const list = allTxns();
    const bal = balance(list);
    const ct = $('#cash-total');
    ct.innerHTML = moneyHTML(bal);
    ct.classList.toggle('neg', bal < 0);
    ct.setAttribute('aria-label', 'Efectivo en mano: ' + (bal < 0 ? 'menos ' : '') + money(bal));
    const t = todayStr(), m = monthOf(t);
    let sToday = 0, sMonth = 0;
    list.forEach(x => { if (x.type === 'expense') { if (x.date === t) sToday += x.amount; if (monthOf(x.date) === m) sMonth += x.amount; } });
    $('#spent-today').textContent = money(sToday);
    $('#month-label').textContent = 'Gastado en ' + DF_MONTH_ONLY.format(new Date());
    $('#spent-month').textContent = money(sMonth);

    // quién soy
    const who = $('#whoami');
    if (!S.me) {
      who.innerHTML = '<div class="whoami"><p>¿Quién usa este móvil?<span>Así entiendo «para mí» cuando dictes.</span></p><div class="pills">' +
        PEOPLE.map(p => '<button type="button" class="pill-btn" data-me="' + p + '">' + p + '</button>').join('') + '</div></div>';
    } else who.innerHTML = '';

    const rec = list.slice(0, 5);
    const ul = $('#recent');
    if (!rec.length) {
      ul.innerHTML = '<li class="empty"><strong>Aún no hay movimientos</strong>Toca el micrófono y di, por ejemplo, «saqué 1000 del cajero» o «35 en taxi».</li>';
      $('#see-all').hidden = true;
    } else {
      ul.innerHTML = rec.map(x => rowHTML(x, true)).join('');
      fillNotes(ul, rec);
      $('#see-all').hidden = false;
    }
    if (bal < 0) {
      const b = $('#banners');
      if (!$('#neg-hint')) b.insertAdjacentHTML('beforeend', '<div class="banner" id="neg-hint">' + svg('info') + '<span>El efectivo sale negativo: quizá falta anotar un retiro del cajero. Puedes corregirlo en <strong>Ajustes</strong>.</span></div>');
    }
    renderMic();
  }

  function renderMic() {
    const mic = $('#mic'), label = $('#mic-label'), sub = $('#mic-sub'), ex = $('#examples');
    const st = S.dictState;
    const on = st === 'listening' || st === 'starting';
    mic.classList.toggle('on', on);
    mic.classList.toggle('busy', st === 'finishing');
    mic.innerHTML = svg(on ? 'stop' : 'mic');
    if (S.dict) {
      mic.setAttribute('aria-label', on ? 'Terminar dictado' : 'Dictar un gasto');
      if (st === 'starting') { label.textContent = 'Preparando…'; sub.textContent = 'Toca para cancelar'; }
      else if (st === 'listening') { label.textContent = 'Te escucho…'; sub.textContent = 'Toca el botón cuando termines'; }
      else if (st === 'finishing') { label.textContent = 'Un momento…'; sub.textContent = ''; }
      else { label.textContent = 'Toca y di tu gasto'; sub.textContent = 'Por ejemplo: «25 bolivianos en un café para Noah»'; }
      ex.innerHTML = 'También: <q>saqué 1000 del cajero</q> · <q>40 de taxi y 120 en el súper</q>';
    } else {
      mic.setAttribute('aria-label', 'Dictar con el teclado');
      label.textContent = 'Toca y dicta';
      sub.textContent = 'Usa el micrófono de tu teclado y luego envía.';
      ex.innerHTML = 'Prueba: <q>25 en un café para Noah</q> · <q>saqué 1000 del cajero</q> · <q>cambié 100 dólares por 950</q>';
    }
  }

  // ---------------------------------------------------------------- render: movimientos
  function renderHist() {
    const box = $('#hist-list');
    $$('#hist-seg button').forEach(b => b.setAttribute('aria-pressed', String(b.dataset.f === S.hist.f)));
    const chip = $('#hist-chip');
    if (S.hist.cat) {
      const c = catById(S.hist.cat);
      chip.innerHTML = '<button type="button" class="chip-filter" id="clear-cat" aria-label="Quitar filtro"><span class="cf-name"></span>' + svg('x') + '</button>';
      $('.cf-name', chip).textContent = c.name + (S.hist.month ? ' · ' + DF_MONTH.format(parseYmd(S.hist.month + '-01')) : '');
    } else chip.innerHTML = '';
    if (!S.loaded) { box.innerHTML = '<p class="loading">Cargando…</p>'; return; }
    const q = P.strip(S.hist.q.trim());
    let list = allTxns().filter(t => {
      if (S.hist.f === 'expense' && t.type !== 'expense') return false;
      if (S.hist.f === 'cash' && t.type === 'expense' && t.method === 'qr') return false;
      if (S.hist.f === 'cash' && t.type === 'expense') return false;
      if (S.hist.cat && (t.type !== 'expense' || catById(t.category).id !== S.hist.cat)) return false;
      if (S.hist.month && monthOf(t.date) !== S.hist.month) return false;
      if (q) {
        const hay = P.strip([t.note, t.text, t.type === 'expense' ? catById(t.category).name : srcOf(t.source).name, t.type === 'expense' ? whoName(t.who) : ''].join(' '));
        if (!hay.includes(q)) return false;
      }
      return true;
    });
    if (!list.length) {
      box.innerHTML = '<div class="group" style="margin-top:14px"><div class="empty"><strong>' + (S.txns.size ? 'Nada por aquí' : 'Aún no hay movimientos') + '</strong>' +
        (S.txns.size ? 'Prueba con otro filtro o búsqueda.' : 'Lo que anotes aparecerá aquí, agrupado por día.') + '</div></div>';
      return;
    }
    const total = list.length;
    const shown = list.slice(0, S.hist.limit);
    const groups = [];
    shown.forEach(t => { const g = groups[groups.length - 1]; if (g && g.date === t.date) g.items.push(t); else groups.push({ date: t.date, items: [t] }); });
    const dayTotals = {};
    list.forEach(t => { if (t.type === 'expense') dayTotals[t.date] = (dayTotals[t.date] || 0) + t.amount; });
    box.innerHTML = groups.map(g => '<div class="day-h"><span>' + esc(dayLabel(g.date)) + '</span><span>' + (dayTotals[g.date] ? esc(money(dayTotals[g.date])) : '') + '</span></div>' +
      '<div class="group"><ul class="list">' + g.items.map(t => rowHTML(t, false)).join('') + '</ul></div>').join('') +
      (total > shown.length ? '<button type="button" class="more" id="more">Mostrar más (' + (total - shown.length) + ')</button>' : '');
    fillNotes(box, shown);
  }

  // ---------------------------------------------------------------- render: resumen
  function renderSum() {
    const cur = monthOf(todayStr());
    if (S.month > cur) S.month = cur;
    $('#m-label').textContent = cap(DF_MONTH.format(parseYmd(S.month + '-01')));
    $('#m-next').disabled = S.month >= cur;
    const body = $('#sum-body');
    if (!S.loaded) { body.innerHTML = '<p class="loading">Cargando…</p>'; return; }
    const all = allTxns();
    const mlist = all.filter(t => monthOf(t.date) === S.month);
    const exp = mlist.filter(t => t.type === 'expense');
    const total = r2(exp.reduce((s, t) => s + t.amount, 0));
    const [yy, mm] = S.month.split('-').map(Number);
    const daysInMonth = new Date(yy, mm, 0).getDate();
    const isCur = S.month === cur;
    const daysElapsed = isCur ? new Date().getDate() : daysInMonth;
    const prevM = ymd(new Date(yy, mm - 2, 1)).slice(0, 7);
    const prevTotal = all.filter(t => t.type === 'expense' && monthOf(t.date) === prevM).reduce((s, t) => s + t.amount, 0);
    const monthName = DF_MONTH_ONLY.format(new Date(yy, mm - 1, 1));
    const prevName = DF_MONTH_ONLY.format(new Date(yy, mm - 2, 1));

    let h = '<div class="hero"><p class="label">Gastado en ' + esc(monthName) + '</p><p class="big">' + moneyHTML(total) + '</p>' +
      '<p class="sub">' + (exp.length ? 'Media diaria ' + esc(money(total / daysElapsed)) + ' · ' + exp.length + (exp.length === 1 ? ' gasto' : ' gastos') : 'Sin gastos este mes') +
      (prevTotal ? '<br>' + esc(cap(prevName)) + ': ' + esc(money(prevTotal)) : '') + '</p></div>';

    if (!exp.length) {
      const ins = mlist.filter(t => t.type === 'cash_in');
      h += '<div class="group" style="margin-top:16px"><div class="empty"><strong>Nada que resumir todavía</strong>' +
        (ins.length ? 'Este mes solo hay entradas de efectivo.' : 'Cuando anotes gastos verás aquí en qué se va el dinero.') + '</div></div>';
      body.innerHTML = h; return;
    }

    // por categoría
    const byCat = {};
    exp.forEach(t => { const id = catById(t.category).id; byCat[id] = (byCat[id] || 0) + t.amount; });
    const catRows = Object.keys(byCat).map(id => ({ c: catById(id), v: r2(byCat[id]) })).sort((a, b) => b.v - a.v);
    const maxC = catRows[0].v;
    h += '<section class="sec"><div class="sec-h"><h2>Por categoría</h2><span class="meta">Toca para ver</span></div><div class="group"><ul class="list bars">' +
      catRows.map(r => '<li><button type="button" class="bar-row" data-cat="' + esc(r.c.id) + '" aria-label="' + esc(r.c.name + ': ' + money(r.v)) + '"><div class="bar-top">' +
        '<span class="tile sm" style="background:' + esc(r.c.color) + '">' + svg(r.c.icon) + '</span><span class="name">' + esc(r.c.name) + '</span>' +
        '<span class="val">' + esc(money(r.v)) + '</span><span class="pct">' + Math.round(r.v / total * 100) + '%</span></div>' +
        '<div class="track"><i style="width:' + (r.v / maxC * 100).toFixed(1) + '%"></i></div></button></li>').join('') + '</ul></div></section>';

    // para quién
    const byWho = {};
    exp.forEach(t => { const w = WHO.find(x => x.id === t.who) ? t.who : 'familia'; byWho[w] = (byWho[w] || 0) + t.amount; });
    const whoRows = WHO.filter(w => byWho[w.id]).map(w => ({ w, v: r2(byWho[w.id]) })).sort((a, b) => b.v - a.v);
    const maxW = whoRows[0].v;
    h += '<section class="sec"><div class="sec-h"><h2>Para quién</h2></div><div class="group"><ul class="list bars">' +
      whoRows.map(r => '<li><div class="bar-row"><div class="bar-top"><span class="tile sm" style="background:var(--fill-strong);color:var(--label)">' + svg(r.w.id === 'noah' ? 'noah' : 'personal') + '</span>' +
        '<span class="name">' + esc(r.w.name) + '</span><span class="val">' + esc(money(r.v)) + '</span><span class="pct">' + Math.round(r.v / total * 100) + '%</span></div>' +
        '<div class="track"><i style="width:' + (r.v / maxW * 100).toFixed(1) + '%"></i></div></div></li>').join('') + '</ul></div></section>';

    // día a día
    const perDay = new Array(daysInMonth).fill(0);
    exp.forEach(t => { const d = +t.date.slice(8, 10); if (d >= 1 && d <= daysInMonth) perDay[d - 1] += t.amount; });
    h += '<section class="sec"><div class="sec-h"><h2>Día a día</h2><span class="meta">Máx. ' + esc(money(Math.max.apply(null, perDay))) + '</span></div><div class="chart-card" id="daychart">' +
      dayChart(perDay, isCur ? new Date().getDate() : 0) + '</div></section>';

    // pago y efectivo
    const cash = exp.filter(t => t.method !== 'qr').reduce((s, t) => s + t.amount, 0);
    const qr = total - cash;
    const ins = mlist.filter(t => t.type === 'cash_in');
    const inBy = id => ins.filter(t => t.source === id).reduce((s, t) => s + t.amount, 0);
    h += '<section class="sec"><div class="sec-h"><h2>Cómo pagamos</h2></div><div class="group kv">' +
      '<div>Efectivo</div><div>' + esc(money(cash)) + '</div><div>QR / tarjeta</div><div>' + esc(money(qr)) + '</div></div></section>';
    h += '<section class="sec"><div class="sec-h"><h2>Entradas de efectivo</h2></div><div class="group kv">' +
      SOURCES.map(s => '<div>' + esc(s.name) + '</div><div' + (inBy(s.id) ? '' : ' class="muted"') + '>' + esc(money(inBy(s.id))) + '</div>').join('') + '</div></section>';
    body.innerHTML = h;
    wireChart();
  }

  function niceMax(v) {
    if (v <= 0) return 10;
    const p = Math.pow(10, Math.floor(Math.log10(v)));
    const n = v / p;
    const m = n <= 1 ? 1 : n <= 2 ? 2 : n <= 2.5 ? 2.5 : n <= 5 ? 5 : 10;
    return m * p;
  }
  function dayChart(perDay, today) {
    const W = 340, H = 150, L = 38, R = 6, T = 10, B = 22;
    const n = perDay.length, max = niceMax(Math.max.apply(null, perDay));
    const pw = W - L - R, ph = H - T - B, band = pw / n, bw = Math.max(3, band * 0.64);
    const y = v => T + ph - (v / max) * ph;
    let s = '<svg viewBox="0 0 ' + W + ' ' + H + '" role="img" aria-label="Gasto por día del mes">';
    [0, max / 2, max].forEach((g, i) => {
      s += '<line class="' + (i ? 'grid' : 'base') + '" x1="' + L + '" x2="' + (W - R) + '" y1="' + y(g) + '" y2="' + y(g) + '"/>';
      s += '<text class="axis" x="' + (L - 6) + '" y="' + (y(g) + 3.5) + '" text-anchor="end">' + esc(num(g)) + '</text>';
    });
    perDay.forEach((v, i) => {
      const x = L + i * band + (band - bw) / 2;
      if (v > 0) {
        const top = y(v), r = Math.min(2, bw / 2, (T + ph - top));
        const base = T + ph;
        s += '<path class="col' + (today && i + 1 > today ? ' dim' : '') + '" d="M' + x + ',' + base + 'V' + (top + r) + 'Q' + x + ',' + top + ' ' + (x + r) + ',' + top +
          'H' + (x + bw - r) + 'Q' + (x + bw) + ',' + top + ' ' + (x + bw) + ',' + (top + r) + 'V' + base + 'Z"/>';
      }
      s += '<rect class="hit" data-d="' + (i + 1) + '" data-v="' + v + '" x="' + (L + i * band) + '" y="' + T + '" width="' + band + '" height="' + ph + '"/>';
    });
    [1, 8, 15, 22, n].forEach(d => {
      if (d === 29 && n === 29) return;
      s += '<text class="axis" x="' + (L + (d - 0.5) * band) + '" y="' + (H - 6) + '" text-anchor="middle">' + d + '</text>';
    });
    return s + '</svg><div class="tip" id="tip" hidden></div>';
  }
  function wireChart() {
    const card = $('#daychart'); if (!card) return;
    const tip = $('#tip', card), svgEl = $('svg', card);
    const [yy, mm] = S.month.split('-').map(Number);
    const show = e => {
      const r = e.target.closest && e.target.closest('.hit'); if (!r) return;
      const d = +r.dataset.d, v = +r.dataset.v;
      const box = svgEl.getBoundingClientRect(), cb = card.getBoundingClientRect();
      const scale = box.width / 340;
      const x = (parseFloat(r.getAttribute('x')) + parseFloat(r.getAttribute('width')) / 2) * scale + (box.left - cb.left);
      tip.textContent = DF_SHORT.format(new Date(yy, mm - 1, d)) + ' · ' + money(v);
      tip.hidden = false;
      const tw = tip.offsetWidth;
      const cx = Math.min(Math.max(x, tw / 2 + 4), cb.width - tw / 2 - 4);
      tip.style.left = cx + 'px'; tip.style.top = (box.top - cb.top + 8) + 'px';
    };
    svgEl.addEventListener('pointermove', show);
    svgEl.addEventListener('pointerdown', show);
    svgEl.addEventListener('pointerleave', e => { if (e.pointerType === 'mouse') tip.hidden = true; });
  }

  // ---------------------------------------------------------------- pestañas
  function setTab(tab) {
    S.tab = tab;
    $$('.tabbar button').forEach(b => { if (b.dataset.tab === tab) b.setAttribute('aria-current', 'page'); else b.removeAttribute('aria-current'); });
    $('#scr-home').hidden = tab !== 'home';
    $('#scr-hist').hidden = tab !== 'hist';
    $('#scr-sum').hidden = tab !== 'sum';
    if (tab === 'hist') renderHist();
    if (tab === 'sum') renderSum();
    window.scrollTo(0, 0);
  }

  // ---------------------------------------------------------------- toast
  let toastTimer = null;
  function toast(msg, actLabel, act) {
    const el = $('#toast'), b = $('#toast-act');
    $('#toast-msg').textContent = msg;
    if (actLabel) { b.textContent = actLabel; b.hidden = false; b.onclick = () => { hideToast(); act(); }; }
    else { b.hidden = true; b.onclick = null; }
    el.classList.add('show');
    clearTimeout(toastTimer);
    toastTimer = setTimeout(hideToast, actLabel ? 5000 : 3200);
  }
  function hideToast() { $('#toast').classList.remove('show'); }

  // ---------------------------------------------------------------- hoja
  let lastFocus = null;
  function openSheet(state) {
    const wasOpen = !!S.sheet;
    S.sheet = state;
    if (!wasOpen) lastFocus = document.activeElement;
    renderSheet();
    const sh = $('#sheet'), sc = $('#scrim');
    if (!wasOpen) {
      sh.hidden = false; sc.hidden = false;
      void sh.offsetWidth;
      sh.classList.add('show'); sc.classList.add('show');
      document.body.classList.add('sheet-open');
      $$('#app, .tabbar').forEach(e => e.setAttribute('aria-hidden', 'true'));
      setTimeout(() => {
        const f = state.focus ? $(state.focus, sh) : null;
        (f || $('#sheet-body')).focus({ preventScroll: true });
      }, 60);
    }
  }
  function closeSheet() {
    if (!S.sheet) return;
    S.sheet = null;
    const sh = $('#sheet'), sc = $('#scrim');
    sh.classList.remove('show'); sc.classList.remove('show');
    document.body.classList.remove('sheet-open');
    $$('#app, .tabbar').forEach(e => e.removeAttribute('aria-hidden'));
    setTimeout(() => { if (!S.sheet) { sh.hidden = true; sc.hidden = true; $('#sheet-body').innerHTML = ''; } }, 300);
    if (lastFocus && document.contains(lastFocus)) { try { lastFocus.focus({ preventScroll: true }); } catch (e) { /* */ } }
  }
  function renderSheet() {
    const st = S.sheet; if (!st) return;
    const body = $('#sheet-body');
    const scroll = body.scrollTop;
    const ok = $('#sheet-ok'), cancel = $('#sheet-cancel');
    if (st.kind === 'entry') renderEntry(st, body, ok, cancel);
    else if (st.kind === 'settings') renderSettings(st, body, ok, cancel);
    else if (st.kind === 'cat') renderCatEditor(st, body, ok, cancel);
    body.scrollTop = scroll;
  }

  // ---- hoja: movimiento(s)
  function draftFromParsed(it) {
    return {
      type: it.type, amount: amountStr(it.amountBs), category: it.category || 'otros', who: it.who || 'familia',
      method: it.method || 'cash', source: it.source || 'atm', date: it.date || todayStr(), note: it.note || '',
      currency: it.currency || 'BOB', origAmount: it.amount, rateUsed: it.rateUsed, estimated: !!(it.flags && it.flags.estimated),
      guessed: !!(it.flags && it.flags.categoryGuessed)
    };
  }
  function blankDraft(type) {
    return { type, amount: '', category: 'otros', who: 'familia', method: 'cash', source: 'atm', date: todayStr(), note: '', currency: 'BOB', guessed: false };
  }
  function draftFromTxn(t) {
    return {
      type: t.type === 'adjust' ? 'adjust' : t.type, amount: amountStr(Math.abs(t.amount)), sign: t.amount < 0 ? -1 : 1,
      category: t.category || 'otros', who: t.who || 'familia', method: t.method || 'cash', source: t.source || 'other',
      date: t.date, note: t.note || '', currency: t.currency || 'BOB', origAmount: t.origAmount, guessed: false
    };
  }

  function renderEntry(st, body, ok, cancel) {
    const n = st.items.length;
    $('#sheet-title').textContent = st.editId ? 'Editar' : n > 1 ? n + ' movimientos' : st.items[0].type === 'cash_in' ? 'Entrada de efectivo' : st.items[0].type === 'adjust' ? 'Ajuste' : 'Nuevo gasto';
    ok.textContent = 'Guardar'; ok.disabled = !S.canWrite || st.saving; cancel.textContent = 'Cancelar'; cancel.classList.remove('ghost'); cancel.tabIndex = 0;
    let h = '';
    if (st.text) {
      h += '<div class="quote">' + svg('quote') + '<q class="q-text"></q><button type="button" data-act="edit-text">Corregir</button></div>';
    }
    const allCats = cats();
    st.items.forEach((d, i) => {
      h += '<div class="item" data-i="' + i + '">';
      if (n > 1) h += '<div class="item-h"><span>' + (i + 1) + ' de ' + n + '</span><button type="button" data-act="remove" data-i="' + i + '">Quitar</button></div>';
      h += '<div class="form">';
      if (d.type !== 'adjust') {
        h += '<div class="seg" role="group" aria-label="Tipo de movimiento">' +
          '<button type="button" data-k="type" data-v="expense" data-i="' + i + '" aria-pressed="' + (d.type === 'expense') + '">Gasto</button>' +
          '<button type="button" data-k="type" data-v="cash_in" data-i="' + i + '" aria-pressed="' + (d.type === 'cash_in') + '">Entrada de efectivo</button></div>';
      }
      const aid = 'amt-' + i;
      h += '<div><label class="sr" for="' + aid + '">Importe en bolivianos</label><div class="amount-in' + (d.err ? ' err' : '') + '"><span>Bs</span>' +
        (d.type === 'adjust' ? '<span aria-hidden="true">' + (d.sign < 0 ? '−' : '+') + '</span>' : '') +
        '<input id="' + aid + '" data-k="amount" data-i="' + i + '" type="text" inputmode="decimal" placeholder="0" autocomplete="off" value="' + esc(d.amount) + '"/></div>';
      if (d.type === 'cash_in' && d.currency !== 'BOB' && d.origAmount) {
        h += '<p class="note' + (d.estimated ? ' warn' : '') + '">' + esc(num(d.origAmount) + ' ' + d.currency) + (d.estimated ? ' × ' + esc(String(d.rateUsed).replace('.', ',')) + ' (tipo de cambio de Ajustes). Revisa lo que te dieron.' : (d.rateUsed ? ' × ' + esc(String(d.rateUsed).replace('.', ',')) : '')) + '</p>';
      } else if (d.type === 'expense' && d.currency && d.currency !== 'BOB' && d.origAmount) {
        h += '<p class="note warn">' + esc(num(d.origAmount) + ' ' + d.currency) + ' convertidos con el tipo de cambio de Ajustes. Revísalo.</p>';
      }
      if (d.err) h += '<p class="note" style="color:var(--danger)">Escribe el importe.</p>';
      h += '</div>';
      h += '<div class="field"><label for="note-' + i + '">Concepto</label><input id="note-' + i + '" type="text" data-k="note" data-i="' + i + '" maxlength="80" placeholder="' +
        (d.type === 'expense' ? 'Ej.: Café en la plaza' : d.type === 'adjust' ? 'Ajuste de efectivo' : 'Ej.: Cajero BNB') + '" value="' + esc(d.note) + '"/></div>';
      if (d.type === 'expense') {
        h += '<div class="field"><span class="flabel" id="cl-' + i + '">Categoría' + (d.guessed ? ' · <span style="font-weight:500">no la he reconocido, elige una</span>' : '') + '</span><div class="cats" role="group" aria-labelledby="cl-' + i + '">' +
          allCats.map(c => '<button type="button" class="cat" data-k="category" data-v="' + esc(c.id) + '" data-i="' + i + '" aria-pressed="' + (d.category === c.id) + '" title="' + esc(c.name) + '">' +
            '<span class="tile" style="background:' + esc(c.color) + '">' + svg(c.icon) + '</span><span>' + esc(catShort(c)) + '</span></button>').join('') + '</div></div>';
        h += '<div class="field"><span class="flabel" id="wl-' + i + '">Para</span><div class="chips" role="group" aria-labelledby="wl-' + i + '">' +
          WHO.map(w => '<button type="button" data-k="who" data-v="' + w.id + '" data-i="' + i + '" aria-pressed="' + (d.who === w.id) + '">' + esc(w.name) + '</button>').join('') + '</div></div>';
        h += '<div class="field"><span class="flabel" id="pl-' + i + '">Pago</span><div class="seg" role="group" aria-labelledby="pl-' + i + '">' +
          '<button type="button" data-k="method" data-v="cash" data-i="' + i + '" aria-pressed="' + (d.method !== 'qr') + '">Efectivo</button>' +
          '<button type="button" data-k="method" data-v="qr" data-i="' + i + '" aria-pressed="' + (d.method === 'qr') + '">QR / tarjeta</button></div></div>';
      } else if (d.type === 'cash_in') {
        h += '<div class="field"><span class="flabel" id="sl-' + i + '">Origen</span><div class="seg" role="group" aria-labelledby="sl-' + i + '">' +
          SOURCES.map(s => '<button type="button" data-k="source" data-v="' + s.id + '" data-i="' + i + '" aria-pressed="' + (d.source === s.id) + '">' + esc(s.short || s.name) + '</button>').join('') + '</div></div>';
      }
      h += '<div class="field"><label for="date-' + i + '">Fecha</label><input id="date-' + i + '" type="date" data-k="date" data-i="' + i + '" value="' + esc(d.date) + '" max="' + todayStr() + '"/></div>';
      h += '</div></div>';
    });
    if (st.editId) {
      const t = S.txns.get(st.editId);
      h += '<button type="button" class="danger-btn' + (st.armed ? ' armed' : '') + '" data-act="delete">' + (st.armed ? 'Toca otra vez para eliminar' : 'Eliminar movimiento') + '</button>';
      if (t) {
        const bits = [];
        if (t.by) bits.push('Añadido por ' + t.by);
        if (t.createdAt) bits.push(DF_SHORT.format(new Date(t.createdAt)) + ', ' + DF_TIME.format(new Date(t.createdAt)));
        if (bits.length) h += '<p class="meta-line">' + esc(bits.join(' · ')) + '</p>';
      }
    } else if (st.items.length > 0 && st.text && st.items.every(d => d.type === 'expense')) {
      h += '<p class="meta-line">Revisa y toca <strong>Guardar</strong>. Puedes cambiar cualquier dato.</p>';
    }
    body.innerHTML = h;
    if (st.text) $('.q-text', body).textContent = st.text;
  }

  function onEntryClick(e) {
    const st = S.sheet; if (!st || st.kind !== 'entry') return;
    const b = e.target.closest('button'); if (!b) return;
    const act = b.dataset.act;
    if (act === 'edit-text') {
      const t = st.text; closeSheet();
      const inp = $('#say'); inp.value = t; updateSend(); setTab('home');
      setTimeout(() => { inp.focus(); inp.setSelectionRange(t.length, t.length); }, 320);
      return;
    }
    if (act === 'remove') { st.items.splice(+b.dataset.i, 1); if (!st.items.length) closeSheet(); else renderSheet(); return; }
    if (act === 'delete') {
      if (!st.armed) { st.armed = true; renderSheet(); setTimeout(() => { if (S.sheet === st && st.armed) { st.armed = false; renderSheet(); } }, 4000); return; }
      const id = st.editId, prev = S.txns.get(id);
      closeSheet();
      Store.del(id).then(() => toast('Movimiento eliminado', 'Deshacer', () => Store.put(id, prev).catch(e2 => toast(errMsg(e2)))))
        .catch(err => toast(errMsg(err)));
      return;
    }
    const k = b.dataset.k; if (!k) return;
    const d = st.items[+b.dataset.i];
    d[k] = b.dataset.v;
    if (k === 'category') d.guessed = false;
    if (k === 'type' && d.type === 'cash_in' && !d.source) d.source = 'atm';
    renderSheet();
  }
  function onEntryInput(e) {
    const st = S.sheet; if (!st || st.kind !== 'entry') return;
    const el = e.target; const k = el.dataset && el.dataset.k; if (!k) return;
    const d = st.items[+el.dataset.i];
    d[k] = el.value;
    if (k === 'amount' && d.err && parseAmount(el.value)) {
      d.err = false;
      const wrap = el.closest('.amount-in'); if (wrap) wrap.classList.remove('err');
      const nt = wrap && wrap.parentNode.querySelector('.note[style]'); if (nt) nt.remove();
    }
  }

  async function saveEntry() {
    const st = S.sheet; if (!st || st.saving) return;
    let bad = false;
    st.items.forEach(d => { d.err = !parseAmount(d.amount); if (d.err) bad = true; if (!/^\d{4}-\d{2}-\d{2}$/.test(d.date)) d.date = todayStr(); });
    if (bad) { renderSheet(); const f = $('.amount-in.err input'); if (f) f.focus(); return; }
    st.saving = true; $('#sheet-ok').disabled = true;
    const now = Date.now();
    const writes = st.items.map((d, i) => {
      const amount = parseAmount(d.amount);
      const base = {
        type: d.type, amount: d.type === 'adjust' ? amount * (d.sign || 1) : amount, date: d.date, note: String(d.note || '').trim().slice(0, 80),
        updatedAt: now
      };
      if (d.type === 'expense') Object.assign(base, { category: d.category, who: d.who, method: d.method === 'qr' ? 'qr' : 'cash' });
      if (d.type === 'cash_in') Object.assign(base, { source: d.source || 'other' });
      if (d.currency && d.currency !== 'BOB' && d.origAmount && d.type !== 'adjust') Object.assign(base, { currency: d.currency, origAmount: d.origAmount });
      if (st.editId) {
        const prev = S.txns.get(st.editId) || {};
        const keep = { createdAt: prev.createdAt || now, by: prev.by || S.me || null };
        if (prev.text) keep.text = prev.text;
        if (prev.byId) keep.byId = prev.byId;
        return { id: st.editId, doc: Object.assign(keep, base), prev };
      }
      const doc = Object.assign(base, { createdAt: now + i, by: S.me || null });
      if (S.uid) doc.byId = S.uid;
      if (st.text) doc.text = st.text.slice(0, 300);
      return { id: newId(), doc, prev: null };
    });
    try {
      for (const w of writes) await Store.put(w.id, w.doc);
      closeSheet();
      $('#say').value = ''; updateSend(); $('#live').innerHTML = '';
      const msg = st.editId ? 'Cambios guardados' : writes.length > 1 ? writes.length + ' movimientos guardados' : writes[0].doc.type === 'cash_in' ? 'Entrada guardada' : 'Gasto guardado';
      toast(msg, 'Deshacer', async () => {
        try {
          for (const w of writes) { if (w.prev) await Store.put(w.id, w.prev); else await Store.del(w.id); }
        } catch (e2) { toast(errMsg(e2)); }
      });
    } catch (err) {
      st.saving = false; renderSheet(); toast(errMsg(err));
    }
  }

  // ---- hoja: ajustes
  function renderSettings(st, body, ok, cancel) {
    $('#sheet-title').textContent = 'Ajustes';
    ok.textContent = 'Listo'; ok.disabled = false; cancel.classList.add('ghost'); cancel.tabIndex = -1;
    const rt = rates();
    const bal = balance(allTxns());
    const ro = !S.canWrite;
    let h = '';
    h += '<div class="set-sec"><h4>Este móvil</h4><div class="set-card" style="padding:12px 14px"><div class="seg" role="group" aria-label="Quién usa este móvil">' +
      PEOPLE.map(p => '<button type="button" data-set-me="' + p + '" aria-pressed="' + (S.me === p) + '">Soy ' + p + '</button>').join('') +
      '</div></div><p class="foot">Sirve para entender «para mí» y para saber quién anotó cada gasto.</p></div>';
    h += '<div class="set-sec"><h4>Efectivo en mano</h4><div class="set-card"><div class="set-row"><label class="grow" for="adj-in">Tengo ahora<small>Bolsillo calcula ' + esc(money(bal)) + '</small></label>' +
      '<input class="inline" id="adj-in" type="text" inputmode="decimal" placeholder="' + esc(amountStr(bal < 0 ? 0 : bal)) + '"' + (ro ? ' disabled' : '') + '/>' +
      '<button type="button" class="act" data-act="adjust"' + (ro ? ' disabled' : '') + '>Ajustar</button></div></div>' +
      '<p class="foot">Cuenta los billetes y escribe lo que tenéis. Se anota un ajuste por la diferencia.</p></div>';
    h += '<div class="set-sec"><h4>Tipo de cambio</h4><div class="set-card">' +
      ['USD', 'EUR'].map(c => '<div class="set-row"><label class="grow" for="rate-' + c + '">1 ' + (c === 'USD' ? 'dólar' : 'euro') + ' =</label><input class="inline" id="rate-' + c + '" data-rate="' + c + '" type="text" inputmode="decimal" value="' + esc(String(rt[c]).replace('.', ',')) + '"' + (ro ? ' disabled' : '') + '/><span>Bs</span></div>').join('') +
      '</div><p class="foot">Solo se usa si dictas un cambio sin decir cuántos bolivianos te dieron.</p></div>';
    h += '<div class="set-sec"><h4>Categorías</h4><ul class="set-card list">' +
      cats().map(c => '<li><button type="button" class="set-row" data-cat-edit="' + esc(c.id) + '"' + (ro ? ' disabled' : '') + '><span class="tile sm" style="background:' + esc(c.color) + '">' + svg(c.icon) + '</span>' +
        '<span class="grow"><span class="cn"></span><small class="ck"></small></span>' + svg('right', ' class="chev"') + '</button></li>').join('') +
      (ro ? '' : '<li><button type="button" class="set-row link-row" data-cat-edit="__new">' + svg('plus', ' style="width:20px;height:20px"') + '<span class="grow">Añadir categoría</span></button></li>') +
      '</ul><p class="foot">Añade palabras clave (por ejemplo, el nombre de la niñera) para que la categoría se elija sola al dictar.</p></div>';
    h += '<div class="set-sec"><h4>Datos</h4><div class="set-card">' +
      '<button type="button" class="set-row link-row" data-act="csv">' + (S.downloads ? 'Descargar movimientos (CSV)' : 'Copiar movimientos (CSV)') + '</button></div>' +
      '<div id="csv-box"></div><p class="foot">' + (S.mode === 'shared' ? 'Los datos se guardan en Bolsillo y se comparten con quien tenga acceso. Se actualizan solos en todos los móviles.' : 'Los datos se guardan solo en este navegador.') + ' ' + S.txns.size + ' movimientos.</p></div>';
    h += '<div class="set-sec"><h4>Cómo dictar</h4><div class="set-card" style="padding:12px 14px;font-size:15px;line-height:1.55">' +
      '<q>Me acabo de gastar 25 bolivianos en un café para Sol y Pol</q><br><q>120 en el súper para toda la familia</q><br><q>Taxi a Santa Cruz 350 con QR</q><br><q>Ayer pagamos 150 a la señora de la limpieza</q><br><q>Saqué 1000 del cajero</q><br><q>Cambié 100 dólares y me dieron 950</q><br><q>Pan 10, leche 12 y taxi 15</q></div></div>';
    body.innerHTML = h;
    $$('[data-cat-edit]', body).forEach(b => {
      const c = cats().find(x => x.id === b.dataset.catEdit); if (!c) return;
      $('.cn', b).textContent = c.name;
      $('.ck', b).textContent = c.keywords.length ? c.keywords.join(', ') : (c.hint || 'Sin palabras clave');
    });
  }
  async function onSettingsClick(e) {
    const st = S.sheet; if (!st || st.kind !== 'settings') return;
    const b = e.target.closest('button'); if (!b) return;
    if (b.dataset.setMe) { setMe(b.dataset.setMe); renderSheet(); return; }
    if (b.dataset.catEdit) {
      const id = b.dataset.catEdit;
      const c = id === '__new' ? null : cats().find(x => x.id === id);
      const used = cats().map(x => x.color);
      openSheet({
        kind: 'cat', id: c ? c.id : null, builtin: c ? !!c.builtin : false,
        name: c ? c.name : '', keywords: c ? c.keywords.join(', ') : '',
        color: c ? c.color : (CUSTOM_COLORS.find(x => used.indexOf(x) < 0) || CUSTOM_COLORS[0]), focus: '#cat-name'
      });
      return;
    }
    if (b.dataset.act === 'adjust') {
      const v = parseAmount($('#adj-in').value);
      const raw = $('#adj-in').value.trim();
      const target = v === null && /^0+([.,]0+)?$/.test(raw) ? 0 : v;
      if (target === null) { toast('Escribe cuánto efectivo tenéis.'); $('#adj-in').focus(); return; }
      const diff = r2(target - balance(allTxns()));
      if (!diff) { toast('El efectivo ya cuadra.'); return; }
      const id = newId();
      const doc = { type: 'adjust', amount: diff, date: todayStr(), note: 'Ajuste de efectivo', createdAt: Date.now(), updatedAt: Date.now(), by: S.me || null };
      try {
        await Store.put(id, doc);
        $('#adj-in').value = '';
        renderSheet();
        toast('Efectivo ajustado a ' + money(target), 'Deshacer', () => Store.del(id).catch(e2 => toast(errMsg(e2))));
      } catch (err) { toast(errMsg(err)); }
      return;
    }
    if (b.dataset.act === 'csv') { exportCsv(); return; }
  }
  function onSettingsChange(e) {
    const el = e.target;
    if (el.dataset && el.dataset.rate) {
      const v = parseAmount(el.value);
      if (!v || v > 1000) { toast('Tipo de cambio no válido.'); el.value = String(rates()[el.dataset.rate]).replace('.', ','); return; }
      Store.patchConfig({ rates: { [el.dataset.rate]: v } }).then(() => toast('Tipo de cambio guardado')).catch(err => toast(errMsg(err)));
    }
  }

  function meKey() { return 'bolsillo.me.' + (S.uid || 'local'); }
  function setMe(name) {
    S.me = name; ls.set(meKey(), name);
    if (S.mode === 'shared' && S.uid && S.canWrite) Store.patchConfig({ members: { [S.uid]: name } }).catch(() => {});
    renderHome();
  }

  // ---- hoja: editor de categoría
  function renderCatEditor(st, body, ok, cancel) {
    $('#sheet-title').textContent = st.id ? 'Categoría' : 'Nueva categoría';
    ok.textContent = 'Guardar'; ok.disabled = false; cancel.classList.remove('ghost'); cancel.tabIndex = 0; cancel.textContent = 'Atrás';
    let h = '<div class="form">';
    h += '<div class="field"><label for="cat-name">Nombre</label><input id="cat-name" type="text" maxlength="24" value="' + esc(st.name) + '" placeholder="Ej.: Deporte"/></div>';
    if (!st.builtin) {
      h += '<div class="field"><span class="flabel">Color</span><div class="colors" role="group" aria-label="Color">' +
        CUSTOM_COLORS.map(c => '<button type="button" data-color="' + c + '" aria-label="Color ' + c + '" aria-pressed="' + (st.color === c) + '" style="background:' + c + '"></button>').join('') + '</div></div>';
    }
    h += '<div class="field"><label for="cat-kw">Palabras clave</label><textarea id="cat-kw" placeholder="Separadas por comas: gimnasio, yoga, piscina">' + esc(st.keywords) + '</textarea>' +
      '<p class="help">Si dictas alguna de estas palabras, el gasto irá a esta categoría.</p></div>';
    h += '</div>';
    if (st.id && !st.builtin) h += '<button type="button" class="danger-btn' + (st.armed ? ' armed' : '') + '" data-act="cat-del" style="margin-top:18px">' + (st.armed ? 'Toca otra vez para eliminar' : 'Eliminar categoría') + '</button><p class="meta-line">Los gastos de esta categoría pasarán a «Otros».</p>';
    body.innerHTML = h;
  }
  function onCatClick(e) {
    const st = S.sheet; if (!st || st.kind !== 'cat') return;
    const b = e.target.closest('button'); if (!b) return;
    if (b.dataset.color) { st.name = $('#cat-name').value; st.keywords = $('#cat-kw').value; st.color = b.dataset.color; renderSheet(); return; }
    if (b.dataset.act === 'cat-del') {
      if (!st.armed) { st.name = $('#cat-name').value; st.keywords = $('#cat-kw').value; st.armed = true; renderSheet(); return; }
      Store.patchConfig({ customCats: { [st.id]: { deleted: true } } }).then(() => toast('Categoría eliminada')).catch(err => toast(errMsg(err)));
      openSheet({ kind: 'settings' });
    }
  }
  function saveCat() {
    const st = S.sheet;
    const name = $('#cat-name').value.trim().slice(0, 24);
    const keywords = $('#cat-kw').value.split(/[,;\n]+/).map(s => s.trim()).filter(Boolean).slice(0, 60);
    if (!name) { toast('Ponle un nombre.'); $('#cat-name').focus(); return; }
    let patch;
    if (st.builtin) patch = { catOverrides: { [st.id]: { name, keywords } } };
    else {
      const id = st.id || newId('c');
      const prev = (S.config.customCats || {})[id] || {};
      patch = { customCats: { [id]: { name, keywords, color: st.color, createdAt: prev.createdAt || Date.now(), deleted: false } } };
    }
    Store.patchConfig(patch).then(() => toast('Categoría guardada')).catch(err => toast(errMsg(err)));
    openSheet({ kind: 'settings' });
  }

  // ---- CSV
  function csvText() {
    const q = v => { const s = String(v == null ? '' : v); return /[",\n;]/.test(s) ? '"' + s.replace(/"/g, '""') + '"' : s; };
    const rows = [['fecha', 'tipo', 'importe_bs', 'categoria', 'para', 'pago', 'concepto', 'anotado_por', 'texto_dictado']];
    allTxns().forEach(t => rows.push([
      t.date, t.type === 'expense' ? 'gasto' : t.type === 'cash_in' ? 'entrada' : 'ajuste',
      String(t.type === 'expense' ? -t.amount : t.amount),
      t.type === 'expense' ? catById(t.category).name : t.type === 'cash_in' ? srcOf(t.source).name : '',
      t.type === 'expense' ? whoName(t.who) : '', t.type === 'expense' ? (t.method === 'qr' ? 'QR/tarjeta' : 'efectivo') : '',
      t.note || '', t.by || '', t.text || ''
    ]));
    return rows.map(r => r.map(q).join(',')).join('\n');
  }
  function exportCsv() {
    const text = csvText();
    if (S.downloads) {
      S.downloads.save({ filename: 'bolsillo-' + todayStr() + '.csv', data: text })
        .catch(e => { if (e && e.code !== 'declined') copyCsv(text); });
      return;
    }
    copyCsv(text);
  }
  function copyCsv(text) {
    const fallback = () => {
      const box = $('#csv-box'); if (!box) return;
      box.innerHTML = '<textarea class="copybox" readonly aria-label="Movimientos en CSV"></textarea>';
      const ta = $('textarea', box); ta.value = text; ta.focus(); ta.select();
      toast('Selecciona el texto y cópialo.');
    };
    try {
      navigator.clipboard.writeText(text).then(() => toast('CSV copiado. Pégalo en una hoja de cálculo.'), fallback);
    } catch (e) { fallback(); }
  }

  // ---------------------------------------------------------------- interpretar
  function processText(text) {
    text = String(text || '').trim();
    if (!text) return;
    const extra = cats().filter(c => c.keywords.length || !c.builtin).map(c => ({ id: c.id, name: c.builtin ? '' : c.name, keywords: c.keywords }));
    const res = P.parseUtterance(text, { me: S.me, rates: rates(), extraCats: extra, now: new Date() });
    let items = res.items.map(draftFromParsed);
    if (!items.length) items = [blankDraft('expense')];
    const firstEmpty = items.findIndex(d => !d.amount);
    openSheet({ kind: 'entry', items, text, focus: firstEmpty >= 0 ? '#amt-' + firstEmpty : null });
  }

  // ---------------------------------------------------------------- dictado
  function wireDictation(d) {
    S.dict = d;
    d.onPartial(({ text }) => { S.partial = text || ''; renderLive(); });
    d.onFinal(({ text }) => { S.finals = S.finals ? S.finals + text : String(text || '').replace(/^\s+/, ''); S.partial = ''; renderLive(); });
    d.onState(({ state, reason }) => {
      S.dictState = state; renderMic();
      if (state === 'idle') {
        const text = (S.finals || S.partial || '').trim();
        const want = S.wantProcess; S.wantProcess = false;
        if (want && text) { $('#live').innerHTML = ''; processText(text); }
        else if (reason === 'not_granted') { S.dict = null; renderMic(); toast('Dictado desactivado. Usa el micrófono del teclado.'); }
        else if (reason === 'unavailable') { /* lo gestiona start() */ }
        else if (reason === 'rate_limited') toast('El micrófono está ocupado. Prueba en unos segundos.');
        else if (reason === 'upstream_error') toast('No se pudo usar el micrófono. Inténtalo otra vez.');
        else if (want && !text && (reason === 'stopped' || reason === 'viewer_stopped')) toast('No te he oído. Prueba otra vez, más cerca del móvil.');
        if (!text) $('#live').innerHTML = '';
      }
    });
    renderMic();
  }
  function renderLive() {
    const el = $('#live');
    el.innerHTML = '';
    if (S.finals) el.appendChild(document.createTextNode(S.finals.trim() + ' '));
    if (S.partial) { const s = document.createElement('span'); s.className = 'partial'; s.textContent = S.partial; el.appendChild(s); }
  }
  function onMic() {
    const d = S.dict;
    if (!d) {
      const inp = $('#say'); inp.focus();
      toast('Toca el micrófono de tu teclado y habla.');
      return;
    }
    if (S.dictState !== 'idle') { d.stop().catch(() => {}); return; }
    S.finals = ''; S.partial = ''; S.wantProcess = true; renderLive();
    const opts = S.dictLang ? { language: 'es' } : undefined;
    d.start(opts).catch(err => {
      S.wantProcess = false;
      const c = err && err.code;
      if (c === 'unavailable' && opts) { S.dictLang = false; toast('Toca otra vez el micrófono para dictar.'); }
      else if (c === 'unavailable' || c === 'not_granted') { S.dict = null; renderMic(); toast('El dictado no está disponible aquí. Usa el micrófono del teclado.'); }
      else if (c === 'rate_limited') toast('El micrófono está ocupado. Prueba en unos segundos.');
      else if (c === 'upstream_error') toast('No se pudo usar el micrófono. Inténtalo otra vez.');
      else if (c === 'no_gesture') toast('Toca el micrófono otra vez.');
    });
  }

  // ---------------------------------------------------------------- eventos
  function updateSend() { $('#send').disabled = !$('#say').value.trim(); }
  function wire() {
    $$('[data-icon]').forEach(el => { el.outerHTML = svg(el.dataset.icon); });
    $('#btn-settings').innerHTML = svg('gear');
    $('#send').innerHTML = svg('send');
    $('#m-prev').innerHTML = svg('left');
    $('#m-next').innerHTML = svg('right');

    $('#mic').addEventListener('click', onMic);
    $('#say').addEventListener('input', updateSend);
    $('#typebar').addEventListener('submit', e => { e.preventDefault(); const v = $('#say').value; if (v.trim()) { $('#say').blur(); processText(v); } });
    $('#add-expense').addEventListener('click', () => openSheet({ kind: 'entry', items: [blankDraft('expense')], focus: '#amt-0' }));
    $('#add-cash').addEventListener('click', () => { const d = blankDraft('cash_in'); d.source = 'atm'; openSheet({ kind: 'entry', items: [d], focus: '#amt-0' }); });
    $('#btn-settings').addEventListener('click', () => openSheet({ kind: 'settings' }));
    $('#see-all').addEventListener('click', () => setTab('hist'));
    $$('.tabbar button').forEach(b => b.addEventListener('click', () => setTab(b.dataset.tab)));

    document.addEventListener('click', e => {
      const ed = e.target.closest && e.target.closest('[data-edit]');
      if (ed && !S.sheet) {
        const t = S.txns.get(ed.dataset.edit); if (!t) return;
        openSheet({ kind: 'entry', items: [draftFromTxn(t)], editId: ed.dataset.edit });
        return;
      }
      const me = e.target.closest && e.target.closest('[data-me]');
      if (me) { setMe(me.dataset.me); return; }
    });

    $('#hist-seg').addEventListener('click', e => { const b = e.target.closest('button'); if (!b) return; S.hist.f = b.dataset.f; S.hist.limit = 80; renderHist(); });
    $('#q').addEventListener('input', e => { S.hist.q = e.target.value; S.hist.limit = 80; renderHist(); });
    $('#scr-hist').addEventListener('click', e => {
      if (e.target.closest('#clear-cat')) { S.hist.cat = null; S.hist.month = null; renderHist(); }
      if (e.target.closest('#more')) { S.hist.limit += 120; renderHist(); }
    });
    $('#m-prev').addEventListener('click', () => { const [y, m] = S.month.split('-').map(Number); S.month = ymd(new Date(y, m - 2, 1)).slice(0, 7); renderSum(); });
    $('#m-next').addEventListener('click', () => { const [y, m] = S.month.split('-').map(Number); S.month = ymd(new Date(y, m, 1)).slice(0, 7); renderSum(); });
    $('#sum-body').addEventListener('click', e => {
      const b = e.target.closest('[data-cat]'); if (!b) return;
      S.hist.cat = b.dataset.cat; S.hist.month = S.month; S.hist.f = 'expense'; S.hist.q = ''; $('#q').value = ''; S.hist.limit = 80;
      setTab('hist');
    });

    const sb = $('#sheet-body');
    sb.addEventListener('click', e => { onEntryClick(e); onSettingsClick(e); onCatClick(e); });
    sb.addEventListener('input', onEntryInput);
    sb.addEventListener('change', onSettingsChange);
    sb.addEventListener('keydown', e => {
      if (e.key === 'Enter' && S.sheet && S.sheet.kind === 'entry' && e.target.tagName === 'INPUT' && e.target.type !== 'date') { e.preventDefault(); saveEntry(); }
    });
    $('#sheet-ok').addEventListener('click', () => {
      const st = S.sheet; if (!st) return;
      if (st.kind === 'entry') saveEntry();
      else if (st.kind === 'settings') closeSheet();
      else if (st.kind === 'cat') saveCat();
    });
    $('#sheet-cancel').addEventListener('click', () => { if (S.sheet && S.sheet.kind === 'cat') openSheet({ kind: 'settings' }); else closeSheet(); });
    $('#scrim').addEventListener('click', closeSheet);
    document.addEventListener('keydown', e => { if (e.key === 'Escape' && S.sheet) { e.preventDefault(); if (S.sheet.kind === 'cat') openSheet({ kind: 'settings' }); else closeSheet(); } });
    // focus trap básico
    $('#sheet').addEventListener('keydown', e => {
      if (e.key !== 'Tab') return;
      const f = $$('#sheet button:not([disabled]):not([hidden]), #sheet input:not([disabled]), #sheet textarea').filter(x => x.offsetParent !== null);
      if (!f.length) return;
      const first = f[0], last = f[f.length - 1];
      if (e.shiftKey && document.activeElement === first) { e.preventDefault(); last.focus(); }
      else if (!e.shiftKey && document.activeElement === last) { e.preventDefault(); first.focus(); }
    });
    // cambio de día con la app abierta
    document.addEventListener('visibilitychange', () => { if (!document.hidden) renderAll(); });
  }

  // ---------------------------------------------------------------- arranque
  async function boot() {
    wire();
    renderMic();
    const c = window.claude;
    if (!c || typeof c.use !== 'function') { S.me = ls.get(meKey()); Store.initLocal('standalone'); return; }
    renderAll();
    const use = name => c.use(name).catch(() => null);
    use('dictation').then(d => { if (d) wireDictation(d); });
    use('downloads').then(d => { S.downloads = d; });
    const user = await use('user');
    if (user) {
      try { S.uid = await user.id(); } catch (e) { S.uid = null; }
      try { const cw = await user.can('data.write'); if (cw === false) S.canWrite = false; } catch (e) { /* */ }
    }
    S.me = ls.get(meKey());
    const db = await use('db');
    if (db) Store.initShared(db); else Store.initLocal('nodb');
  }
  window.__bolsillo = { S, Store, processText, balance, allTxns };
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', boot); else boot();
})();
