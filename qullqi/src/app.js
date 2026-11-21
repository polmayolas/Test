/* Qullqi — «plata, dinero» en quechua y aimara. App de gastos en efectivo por voz.
 * Depende de QullqiParser (parser.js). */
(function () {
  'use strict';
  const P = window.QullqiParser;
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
  const LOGO = '<svg class="logo" viewBox="0 0 48 48" aria-hidden="true"><rect width="48" height="48" rx="12" fill="#b0145a"/>' +
    '<path fill="#fff" fill-rule="evenodd" d="M18.9 8.7L29.1 8.7L29.1 12.1L32.5 12.1L32.5 15.5L35.9 15.5L35.9 18.9L39.3 18.9L39.3 29.1L35.9 29.1L35.9 32.5L32.5 32.5L32.5 35.9L29.1 35.9L29.1 39.3L18.9 39.3L18.9 35.9L15.5 35.9L15.5 32.5L12.1 32.5L12.1 29.1L8.7 29.1L8.7 18.9L12.1 18.9L12.1 15.5L15.5 15.5L15.5 12.1L18.9 12.1ZM18.7 24a5.3 5.3 0 1 0 10.6 0a5.3 5.3 0 1 0 -10.6 0Z"/></svg>';
  function svg(name, extra) {
    return '<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.9" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"' + (extra || '') + '>' + (ICON[name] || ICON.otros) + '</svg>';
  }

  // ---------------------------------------------------------------- catálogo
  const BUILTIN = [
    { id: 'comida', name: 'Comida', hint: 'Súper, mercado, feria', color: '#2f9e44' },
    { id: 'resto', name: 'Restaurantes', hint: 'Restaurantes y cafés', color: '#e8590c' },
    { id: 'transporte', name: 'Transporte', hint: 'Taxis, trufis, viajes a Santa Cruz', color: '#0a72e8' },
    { id: 'personal', name: 'Ayuda en casa', hint: 'Limpieza, niñera, personal del hogar', color: '#8e44c9' },
    { id: 'casa', name: 'Casa', hint: 'Alquiler, luz, agua, gas, internet', color: '#9c6b3c' },
    { id: 'salud', name: 'Salud', hint: 'Farmacia, médico', color: '#e03131' },
    { id: 'ocio', name: 'Ocio', hint: 'Excursiones, actividades, viajes', color: '#6741d9' },
    { id: 'noah', name: 'Noah', hint: 'Pañales, juguetes, cole', color: '#1689ad' },
    { id: 'compras', name: 'Compras', hint: 'Ropa, regalos, peluquería', color: '#b07d00' },
    { id: 'otros', name: 'Otros', hint: 'Todo lo demás', color: '#7c7c85' }
  ];
  const CUSTOM_COLORS = ['#5856d6', '#00897b', '#c2255c', '#6d5d45', '#1c7ed6', '#d9480f', '#495057', '#2b8a3e'];
  const COLOR_NAMES = { '#5856d6': 'Índigo', '#00897b': 'Turquesa', '#c2255c': 'Frambuesa', '#6d5d45': 'Tierra', '#1c7ed6': 'Azul', '#d9480f': 'Naranja', '#495057': 'Pizarra', '#2b8a3e': 'Verde' };
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
    const raw = String(s).trim();
    if (!raw || /[^\d.,\s]/.test(raw)) return null;
    const t = raw.replace(/\s+/g, '');
    if (!t) return null;
    const v = P.digitValue(t);
    return v && v > 0 && v < 1e8 ? r2(v) : null;
  }
  function pct(v, total) { const x = v / total * 100; return x > 0 && x < 1 ? '<1 %' : Math.round(x) + ' %'; }
  function withTimeout(pr, ms) {
    return new Promise((res, rej) => {
      const t = setTimeout(() => rej({ code: 'timeout' }), ms || 15000);
      Promise.resolve(pr).then(v => { clearTimeout(t); res(v); }, e => { clearTimeout(t); rej(e); });
    });
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
    loaded: false, stale: false, canWrite: true, uid: null, configLoaded: false, storageOk: true, txv: 0,
    me: null,
    tab: 'home', month: monthOf(todayStr()),
    hist: { f: 'all', q: '', cat: null, who: null, month: null, limit: 80 },
    dict: null, dictState: 'idle', dictLang: true, finals: '', partial: '', wantProcess: false, heard: false,
    adjusting: false,
    downloads: null,
    sheet: null
  };

  let catCache = { cfg: null, list: null };
  function cats() {
    if (catCache.cfg === S.config && catCache.list) return catCache.list;
    const list = buildCats();
    catCache = { cfg: S.config, list };
    return list;
  }
  function buildCats() {
    const ov = S.config.catOverrides || {};
    const kw = v => Array.isArray(v) ? v.filter(x => typeof x === 'string' && x.trim()).slice(0, 60) : [];
    const str = v => typeof v === 'string' ? v.trim().slice(0, 24) : '';
    const list = BUILTIN.map(c => {
      const o = ov[c.id] && typeof ov[c.id] === 'object' ? ov[c.id] : {};
      return Object.assign({}, c, { name: str(o.name) || c.name, short: str(o.name) ? null : c.short, keywords: kw(o.keywords), builtin: true, icon: c.id });
    });
    const cu = S.config.customCats && typeof S.config.customCats === 'object' ? S.config.customCats : {};
    const custom = Object.keys(cu).filter(id => cu[id] && typeof cu[id] === 'object' && !cu[id].deleted && str(cu[id].name)).map(id => ({
      id, icon: 'custom', name: str(cu[id].name), keywords: kw(cu[id].keywords),
      color: /^#[0-9a-f]{6}$/i.test(cu[id].color || '') ? cu[id].color : CUSTOM_COLORS[0], createdAt: +cu[id].createdAt || 0
    })).sort((a, b) => a.createdAt - b.createdAt);
    const otros = list.pop();
    return list.concat(custom, [otros]);
  }
  function catById(id) { const all = cats(); return all.find(c => c.id === id) || all[all.length - 1]; }
  function catShort(c) { return c.short || c.name; }
  function rates() {
    const r = Object.assign({}, DEFAULT_RATES);
    const c = S.config.rates || {};
    ['USD', 'EUR'].forEach(k => { if (typeof c[k] === 'number' && c[k] > 0 && c[k] < 1000) r[k] = c[k]; });
    return r;
  }
  function whoName(id) { const w = WHO.find(x => x.id === id); return w ? w.name : 'Familia'; }
  function srcOf(id) { return SOURCES.find(s => s.id === id) || SOURCES[2]; }

  let txCache = { v: -1, list: [] };
  function allTxns() {
    if (txCache.v !== S.txv) txCache = { v: S.txv, list: validTxns(S.txns) };
    return txCache.list;
  }
  function validTxns(map) {
    const out = [];
    map.forEach((d, id) => {
      if (!d || typeof d !== 'object' || typeof d.amount !== 'number' || !isFinite(d.amount) || Math.abs(d.amount) > 1e9) return;
      if (typeof d.date !== 'string' || !/^\d{4}-\d{2}-\d{2}$/.test(d.date) || ymd(parseYmd(d.date)) !== d.date) return;
      if (d.type !== 'expense' && d.type !== 'cash_in' && d.type !== 'adjust') return;
      const t = Object.assign({ id }, d);
      if (t.type !== 'adjust') t.amount = Math.abs(t.amount);
      if (typeof t.note !== 'string') t.note = '';
      out.push(t);
    });
    return out
      .sort((a, b) => (a.date < b.date ? 1 : a.date > b.date ? -1 : (b.createdAt || 0) - (a.createdAt || 0)));
  }
  // ¿Los movimientos que vemos están confirmados por el servidor? (sin ellos no se puede cuadrar el efectivo)
  function dataReady() { return S.mode === 'local' || (S.mode === 'shared' && S.confirmed && !S.stale); }
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
  const LOCAL_KEY = 'qullqi.data.v1';
  const Store = {
    db: null,
    configQueue: Promise.resolve(),
    initLocal(reason) {
      S.mode = 'local'; S.localReason = reason || 'standalone'; S.canWrite = true;
      S.storageOk = ls.set('qullqi.probe', '1');
      this.loadLocal();
      S.configExists = true; S.configLoaded = true; S.loaded = true;
      window.addEventListener('storage', e => { if (e.key === LOCAL_KEY) { this.loadLocal(); renderAll(); } });
      renderAll();
    },
    loadLocal() {
      let data = null;
      try { data = JSON.parse(ls.get(LOCAL_KEY) || 'null'); } catch (e) { data = null; }
      data = data && typeof data === 'object' ? data : {};
      S.txns = new Map(Object.entries(data.txns && typeof data.txns === 'object' ? data.txns : {})); S.txv++;
      S.config = data.config && typeof data.config === 'object' ? data.config : {};
    },
    saveLocal() {
      const obj = { txns: Object.fromEntries(S.txns), config: S.config };
      const ok = ls.set(LOCAL_KEY, JSON.stringify(obj));
      if (!ok) { S.storageOk = false; renderBanners(); }
      return ok;
    },
    initShared(db) {
      this.db = db; S.mode = 'shared';
      const timer = setTimeout(() => { if (!S.confirmed) { S.stale = true; renderBanners(); } }, 10000);
      const subTxns = () => db.collection('txns').onSnapshot(snap => {
        const m = new Map();
        snap.docs.forEach(d => { if (d.exists) m.set(d.id, d.data()); });
        S.txns = m; S.loaded = true; S.txv++;
        S.confirmed = !snap.metadata.fromCache;
        if (S.confirmed) { S.stale = false; clearTimeout(timer); }
        renderAll();
      }, err => {
        S.loaded = true; S.stale = true; S.confirmed = false; clearTimeout(timer); renderAll();
        const code = err && err.code;
        if (code === 'unavailable') setTimeout(subTxns, 5000);
        else if (code === 'resource_exhausted' && (S.txnRetries = (S.txnRetries || 0) + 1) <= 3) setTimeout(subTxns, 30000 * S.txnRetries);
      });
      const subConfig = () => db.doc('config/main').onSnapshot(d => {
        S.config = d.exists ? (d.data() || {}) : {};
        S.configExists = !!d.exists;
        if (!d.metadata || !d.metadata.fromCache) S.configLoaded = true;
        if (S.uid && S.config.members && typeof S.config.members[S.uid] === 'string' && PEOPLE.indexOf(S.config.members[S.uid]) >= 0) {
          S.me = S.config.members[S.uid]; ls.set(meKey(), S.me);
        }
        renderAll();
      }, err => {
        const code = err && err.code;
        if (code === 'unavailable') setTimeout(subConfig, 5000);
        else if (code === 'resource_exhausted' && (S.cfgRetries = (S.cfgRetries || 0) + 1) <= 3) setTimeout(subConfig, 30000 * S.cfgRetries);
      });
      subTxns(); subConfig();
      renderAll();
    },
    async put(id, doc) {
      if (S.mode === 'local') {
        const had = S.txns.has(id), prev = S.txns.get(id);
        S.txns.set(id, doc); S.txv++;
        if (!this.saveLocal()) { if (had) S.txns.set(id, prev); else S.txns.delete(id); S.txv++; throw { code: 'local_storage' }; }
        renderAll(); return;
      }
      return this.retry(() => this.db.collection('txns').doc(id).set(doc));
    },
    async del(id) {
      if (S.mode === 'local') {
        const prev = S.txns.get(id);
        S.txns.delete(id); S.txv++;
        if (!this.saveLocal()) { if (prev) S.txns.set(id, prev); S.txv++; throw { code: 'local_storage' }; }
        renderAll(); return;
      }
      return this.retry(() => this.db.collection('txns').doc(id).delete());
    },
    patchConfig(patch) {
      if (S.mode === 'local') {
        const prev = S.config;
        S.config = deepMerge(JSON.parse(JSON.stringify(S.config || {})), patch);
        if (!this.saveLocal()) { S.config = prev; return Promise.reject({ code: 'local_storage' }); }
        renderAll(); return Promise.resolve();
      }
      // Nunca se reescribe la configuración entera: update() fusiona; set() solo si el documento no existe.
      const run = async () => {
        const ref = this.db.doc('config/main');
        try { await withTimeout(ref.update(patch)); }
        catch (e) {
          if (!e || e.code !== 'invalid_argument') {
            if (e && e.code === 'unavailable') { await new Promise(r => setTimeout(r, 600)); return this.retry(() => ref.update(patch)); }
            throw writeError(e);
          }
          const snap = await withTimeout(ref.get());
          if (snap.exists) await this.retry(() => ref.update(patch));
          else {
            await this.retry(() => ref.set(patch));
            // si otro móvil creó el documento a la vez, su set() pudo pisar el nuestro: se vuelve a aplicar
            await new Promise(r => setTimeout(r, 500));
            const after = await withTimeout(ref.get());
            if (!after.exists || !containsPatch(after.data(), patch)) await this.retry(() => ref.update(patch));
          }
        }
        S.configExists = true;
      };
      const next = this.configQueue.then(run, run);
      this.configQueue = next.catch(() => {});
      return next;
    },
    async retry(fn) {
      try { return await withTimeout(fn()); }
      catch (e) {
        if (e && e.code === 'unavailable') {
          await new Promise(r => setTimeout(r, 400 + Math.random() * 800));
          try { return await withTimeout(fn()); } catch (e2) { throw writeError(e2); }
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
  function errMsg(e, retryLabel) {
    const c = e && e.code;
    if (c === 'invalid_argument' || c === 'not_granted') return 'No tienes permiso para guardar. Pide acceso de Editor.';
    if (c === 'quota_exceeded') return 'El almacenamiento está lleno. Borra movimientos antiguos.';
    if (c === 'revoked') return 'Se ha retirado el acceso a los datos.';
    if (c === 'local_storage') return 'Este navegador no deja guardar datos. No se ha guardado nada.';
    if (c === 'timeout') return 'Sin conexión: no se pudo confirmar. ' + (retryLabel || 'Vuelve a tocar Guardar') + ' cuando tengas señal.';
    return 'No se pudo guardar. Revisa la conexión e inténtalo de nuevo.';
  }
  function containsPatch(obj, patch) {
    if (!obj || typeof obj !== 'object') return false;
    return Object.keys(patch).every(k => {
      const v = patch[k];
      if (v && typeof v === 'object' && !Array.isArray(v)) return containsPatch(obj[k], v);
      return JSON.stringify(obj[k]) === JSON.stringify(v);
    });
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
      sub.push(title === (s.short || s.name) || title === s.name ? 'Entrada de efectivo' : (s.short || s.name));
      if (t.currency && t.currency !== 'BOB' && t.origAmount) sub.push(num(t.origAmount) + ' ' + t.currency);
      amt = '+' + money(t.amount); cls = 'in';
    } else if (t.type === 'adjust') {
      title = t.note || 'Ajuste de efectivo';
      sub.push('Ajuste');
      amt = (t.amount < 0 ? '−' : '+') + money(t.amount); cls = 'adj';
    } else {
      const c = catById(t.category);
      title = t.note || c.name;
      sub.push(catShort(c));
      if (whoName(t.who) !== catShort(c)) sub.push(whoName(t.who));
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
    if (S.sheet && S.sheet.kind === 'settings') {
      const a = document.activeElement;
      if (!(a && $('#sheet-body').contains(a) && (a.tagName === 'INPUT' || a.tagName === 'TEXTAREA'))) renderSheet();
    }
  }

  function renderBanners() {
    const el = $('#banners');
    const parts = [];
    if (S.mode === 'local' && !S.storageOk) {
      parts.push('<div class="banner">' + svg('info') + '<span><strong>No se puede guardar.</strong> Este navegador bloquea el almacenamiento: lo que anotes se perderá al cerrar.</span></div>');
    } else if (S.mode === 'local') {
      parts.push('<div class="banner">' + svg('info') + '<span><strong>Solo en este dispositivo.</strong> ' +
        (S.localReason === 'nodb' ? 'Inicia sesión en Claude para compartir los datos con tu familia.' : 'Los datos se guardan en este navegador.') + '</span></div>');
    }
    if (S.mode === 'shared' && !S.canWrite) {
      parts.push('<div class="banner">' + svg('info') + '<span><strong>Solo lectura.</strong> Pide a quien creó Qullqi que te invite como Editor para poder anotar gastos.</span></div>');
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
    if (!S.loaded || (S.mode === 'shared' && !S.confirmed && !S.txns.size)) {
      $('#cash-total').innerHTML = '<span class="cur">Bs</span>—';
      $('#recent').innerHTML = '<li class="loading">Cargando…</li>';
      return;
    }
    const list = allTxns();
    const bal = balance(list);
    const ct = $('#cash-total');
    ct.innerHTML = moneyHTML(bal);
    ct.classList.toggle('neg', bal < 0);
    const startBox = $('#start-box');
    const showStart = !list.length && S.canWrite && dataReady() && ls.get('qullqi.started') !== '1';
    if (showStart && !startBox.firstChild) {
      startBox.innerHTML = '<p class="start-q"><label for="start-in">¿Cuánto efectivo tenéis ahora?</label></p><form class="start" id="start-form" autocomplete="off">' +
        '<div class="amt-box"><span>Bs</span><input id="start-in" type="text" inputmode="decimal" placeholder="0"/></div>' +
        '<button class="btn-primary" type="button" id="start-go">Empezar</button></form>' +
        '<p class="start-help">Cuenta los billetes para partir de la cantidad real, o <button type="button" class="inline-link" id="start-zero">empieza desde 0</button>.</p>';
    } else if (!showStart && startBox.firstChild) startBox.innerHTML = '';
    $('#cash-stats').hidden = showStart;
    const t = todayStr(), m = monthOf(t);
    let sToday = 0, sMonth = 0;
    list.forEach(x => { if (x.type === 'expense') { if (x.date === t) sToday += x.amount; if (monthOf(x.date) === m) sMonth += x.amount; } });
    $('#spent-today').textContent = money(sToday);
    $('#month-label').textContent = 'Gastado en ' + DF_MONTH_ONLY.format(new Date());
    $('#spent-month').textContent = money(sMonth);

    // quién soy
    const who = $('#whoami');
    if (!S.me && (S.mode === 'local' || S.configLoaded)) {
      who.innerHTML = '<div class="whoami"><p>¿Quién usa este móvil?<span>Así entiendo «para mí» cuando dictes.</span></p><div class="pills">' +
        PEOPLE.map(p => '<button type="button" class="pill-btn" data-me="' + p + '">' + p + '</button>').join('') + '</div></div>';
    } else who.innerHTML = '';

    const rec = list.slice(0, 5);
    const ul = $('#recent');
    if (!rec.length) {
      ul.innerHTML = '<li class="empty"><strong>Aún no hay movimientos</strong>' + (S.dict ? 'Toca el micrófono y di, por ejemplo, «saqué 1000 del cajero» o «35 en taxi».' : 'Escribe o dicta con el teclado, por ejemplo, «saqué 1000 del cajero» o «35 en taxi».') + '</li>';
      $('#see-all').hidden = true;
    } else {
      ul.innerHTML = rec.map(x => rowHTML(x, true)).join('');
      fillNotes(ul, rec);
      $('#see-all').hidden = false;
    }
    if (bal < 0) {
      const b = $('#banners');
      if (!$('#neg-hint')) b.insertAdjacentHTML('beforeend', '<div class="banner" id="neg-hint">' + svg('info') + '<span>El efectivo sale negativo: quizá falta anotar un retiro del cajero. <button type="button" class="inline-link" id="fix-cash">Corregir el efectivo</button></span></div>');
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
    if (S.hist.cat || S.hist.who) {
      const label = S.hist.cat ? catById(S.hist.cat).name : 'Para ' + whoName(S.hist.who);
      chip.innerHTML = '<button type="button" class="chip-filter" id="clear-cat"><span class="cf-name"></span>' + svg('x') + '<span class="sr">Quitar filtro</span></button>';
      $('.cf-name', chip).textContent = label + (S.hist.month ? ' · ' + DF_MONTH.format(parseYmd(S.hist.month + '-01')) : '');
    } else chip.innerHTML = '';
    if (!S.loaded) { box.innerHTML = '<p class="loading">Cargando…</p>'; return; }
    const q = P.strip(S.hist.q.trim());
    let list = allTxns().filter(t => {
      if (S.hist.f === 'expense' && t.type !== 'expense') return false;
      if (S.hist.f === 'cash' && t.type === 'expense') return false;
      if (S.hist.cat && (t.type !== 'expense' || catById(t.category).id !== S.hist.cat)) return false;
      if (S.hist.who && (t.type !== 'expense' || (WHO.some(w => w.id === t.who) ? t.who : 'familia') !== S.hist.who)) return false;
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
    box.innerHTML = groups.map(g => '<div class="day-h"><span>' + esc(cap(dayLabel(g.date))) + '</span><span>' + (dayTotals[g.date] ? esc(money(dayTotals[g.date])) : '') + '</span></div>' +
      '<div class="group"><ul class="list">' + g.items.map(t => rowHTML(t, false)).join('') + '</ul></div>').join('') +
      (total > shown.length ? '<button type="button" class="more" id="more">Mostrar más (' + (total - shown.length) + ')</button>' : '');
    fillNotes(box, shown);
  }

  // ---------------------------------------------------------------- render: resumen
  function renderSum() {
    const cur = monthOf(todayStr());
    if (S.month > cur) S.month = cur;
    let first = cur;
    S.txns.forEach(d => { if (d && typeof d.date === 'string' && /^\d{4}-\d{2}/.test(d.date) && d.date.slice(0, 7) < first) first = d.date.slice(0, 7); });
    if (S.month < first) S.month = first;
    $('#m-label').textContent = cap(DF_MONTH.format(parseYmd(S.month + '-01')));
    $('#m-next').disabled = S.month >= cur;
    $('#m-prev').disabled = S.month <= first;
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
      '<p class="sub">' + (exp.length ? 'Media diaria ' + esc(money(Math.round(total / daysElapsed))) + ' · ' + exp.length + (exp.length === 1 ? ' gasto' : ' gastos') : 'Sin gastos este mes') +
      (prevTotal ? '<br>' + esc(cap(prevName)) + ': ' + esc(money(prevTotal)) : '') + '</p></div>';

    if (!exp.length) {
      const ins = mlist.filter(t => t.type === 'cash_in');
      h += '<div class="group" style="margin-top:16px"><div class="empty"><strong>Nada que resumir todavía</strong>' +
        (ins.length ? 'Este mes solo hay entradas de efectivo.' : all.length ? 'No hay gastos anotados en ' + esc(monthName) + '.' : 'Cuando anotes gastos verás aquí en qué se va el dinero.') + '</div></div>';
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
        '<span class="val">' + esc(money(r.v)) + '</span><span class="pct">' + pct(r.v, total) + '</span></div>' +
        '<div class="track"><i style="width:' + (r.v / maxC * 100).toFixed(1) + '%"></i></div></button></li>').join('') + '</ul></div></section>';

    // para quién
    const byWho = {};
    exp.forEach(t => { const w = WHO.find(x => x.id === t.who) ? t.who : 'familia'; byWho[w] = (byWho[w] || 0) + t.amount; });
    const whoRows = WHO.filter(w => byWho[w.id]).map(w => ({ w, v: r2(byWho[w.id]) })).sort((a, b) => b.v - a.v);
    const maxW = whoRows[0].v;
    h += '<section class="sec"><div class="sec-h"><h2>Para quién</h2><span class="meta">Toca para ver</span></div><div class="group"><ul class="list bars">' +
      whoRows.map(r => '<li><button type="button" class="bar-row" data-who="' + r.w.id + '" aria-label="' + esc(r.w.name + ': ' + money(r.v)) + '"><div class="bar-top"><span class="tile sm" style="background:var(--fill-strong);color:var(--label)">' + svg(r.w.id === 'noah' ? 'noah' : 'personal') + '</span>' +
        '<span class="name">' + esc(r.w.name) + '</span><span class="val">' + esc(money(r.v)) + '</span><span class="pct">' + pct(r.v, total) + '</span></div>' +
        '<div class="track"><i style="width:' + (r.v / maxW * 100).toFixed(1) + '%"></i></div></button></li>').join('') + '</ul></div></section>';

    // día a día
    const perDay = new Array(daysInMonth).fill(0);
    exp.forEach(t => { const d = +t.date.slice(8, 10); if (d >= 1 && d <= daysInMonth) perDay[d - 1] += t.amount; });
    h += '<section class="sec"><div class="sec-h"><h2>Día a día</h2><span class="meta">Máx. ' + esc(money(Math.max.apply(null, perDay))) + '</span></div><div class="chart-card" id="daychart">' +
      dayChart(perDay, isCur ? new Date().getDate() : 0, yy, mm) + '</div></section>';

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
  function dayChart(perDay, today, yy, mm) {
    const W = 340, H = 150, L = 38, R = 6, T = 10, B = 22;
    const n = perDay.length, max = niceMax(Math.max.apply(null, perDay));
    const pw = W - L - R, ph = H - T - B, band = pw / n, bw = Math.max(3, band * 0.64);
    const y = v => T + ph - (v / max) * ph;
    let s = '<svg viewBox="0 0 ' + W + ' ' + H + '" role="group" aria-label="Gasto por día del mes. Recorre las barras con el tabulador.">';
    [0, max / 2, max].forEach((g, i) => {
      s += '<line class="' + (i ? 'grid' : 'base') + '" x1="' + L + '" x2="' + (W - R) + '" y1="' + y(g) + '" y2="' + y(g) + '"/>';
      s += '<text class="axis" x="' + (L - 6) + '" y="' + (y(g) + 3.5) + '" text-anchor="end">' + esc(num(g)) + '</text>';
    });
    perDay.forEach((v, i) => {
      const x = L + i * band + (band - bw) / 2;
      if (v > 0) {
        const top = y(v), r = Math.min(2, bw / 2, (T + ph - top));
        const base = T + ph;
        s += '<path class="col' + (today && i + 1 > today ? ' dim' : '') + '" data-c="' + (i + 1) + '" d="M' + x + ',' + base + 'V' + (top + r) + 'Q' + x + ',' + top + ' ' + (x + r) + ',' + top +
          'H' + (x + bw - r) + 'Q' + (x + bw) + ',' + top + ' ' + (x + bw) + ',' + (top + r) + 'V' + base + 'Z"/>';
      }
      s += '<rect class="hit" data-d="' + (i + 1) + '" data-v="' + v + '" x="' + (L + i * band) + '" y="' + T + '" width="' + band + '" height="' + ph + '"' +
        (v > 0 ? ' tabindex="0" role="img" aria-label="' + esc(DF_SHORT.format(new Date(yy, mm - 1, i + 1)) + ': ' + money(v)) + '"' : '') + '/>';
    });
    [1, 8, 15, 22, n].forEach(d => {
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
      $$('.col.sel', svgEl).forEach(c => c.classList.remove('sel'));
      const col = $('.col[data-c="' + d + '"]', svgEl); if (col) col.classList.add('sel');
      const box = svgEl.getBoundingClientRect(), cb = card.getBoundingClientRect();
      const scale = box.width / 340;
      const x = (parseFloat(r.getAttribute('x')) + parseFloat(r.getAttribute('width')) / 2) * scale + (box.left - cb.left);
      tip.textContent = DF_SHORT.format(new Date(yy, mm - 1, d)) + ' · ' + money(v);
      tip.hidden = false;
      const tw = tip.offsetWidth, th = tip.offsetHeight;
      const cx = Math.min(Math.max(x, tw / 2 + 6), cb.width - tw / 2 - 6);
      tip.style.left = cx + 'px'; tip.style.top = Math.max(th + 6, box.top - cb.top + 10) + 'px';
    };
    svgEl.addEventListener('pointermove', show);
    svgEl.addEventListener('pointerdown', show);
    svgEl.addEventListener('focusin', show);
    svgEl.addEventListener('focusout', () => { tip.hidden = true; });
    svgEl.addEventListener('pointerleave', e => { if (e.pointerType === 'mouse') { tip.hidden = true; $$('.col.sel', svgEl).forEach(c => c.classList.remove('sel')); } });
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
    if (S.sheet) actLabel = null; // con una hoja abierta, sin acciones que se puedan tocar por error
    const el = $('#toast'), b = $('#toast-act');
    $('#toast-msg').textContent = msg;
    if (actLabel) { b.textContent = actLabel; b.hidden = false; b.onclick = () => { hideToast(); act(); }; }
    else { b.hidden = true; b.onclick = null; }
    el.classList.add('show');
    clearTimeout(toastTimer);
    toastTimer = setTimeout(hideToast, actLabel ? 7000 : 3200);
  }
  function hideToast(silent) {
    if (!silent && $('#toast').contains(document.activeElement)) restoreFocus(null);
    clearTimeout(toastTimer); $('#toast').classList.remove('show'); $('#toast-act').hidden = true; $('#toast-act').onclick = null; }

  // ---------------------------------------------------------------- hoja
  let lastFocus = null;
  function openSheet(state) {
    const wasOpen = !!S.sheet;
    const prevKind = wasOpen ? S.sheet.kind : null;
    S.sheet = state;
    if (!wasOpen) lastFocus = document.activeElement;
    hideToast(true);
    // 2) cada hoja nueva empieza arriba (se ve lo que se ha oído)
    const resetScroll = !wasOpen || prevKind !== state.kind;
    renderSheet();
    const sh = $('#sheet'), sc = $('#scrim');
    if (!wasOpen) {
      sh.hidden = false; sc.hidden = false;
      void sh.offsetWidth;
      $('#sheet-body').scrollTop = 0;
      sh.classList.add('show'); sc.classList.add('show');
      document.body.classList.add('sheet-open');
      $$('#app, .tabbar').forEach(e => { e.setAttribute('aria-hidden', 'true'); e.inert = true; });
      setTimeout(() => {
        const f = state.focus ? $(state.focus, sh) : null;
        (f || $('#sheet-body')).focus({ preventScroll: true });
      }, 60);
    }
    if (resetScroll) $('#sheet-body').scrollTop = 0;
    if (wasOpen && prevKind !== state.kind) {
      const f = state.focus ? $(state.focus, $('#sheet')) : null;
      (f || $('#sheet-title')).focus({ preventScroll: true });
    }
  }
  // Devuelve el foco a algo visible de la pantalla actual
  function restoreFocus(el) {
    const visible = x => x && document.contains(x) && x !== document.body && x.getClientRects().length > 0 && !x.closest('[hidden]') && !x.disabled;
    let target = visible(el) ? el : null;
    if (!target) target = S.tab === 'home' ? $('#mic') : $('#scr-' + S.tab + ' h1');
    try { target.focus({ preventScroll: true }); } catch (e) { /* */ }
  }
  function closeSheet() {
    if (!S.sheet) return;
    S.sheet = null;
    const sh = $('#sheet'), sc = $('#scrim');
    sh.classList.remove('show'); sc.classList.remove('show');
    document.body.classList.remove('sheet-open');
    $$('#app, .tabbar').forEach(e => { e.removeAttribute('aria-hidden'); e.inert = false; });
    setTimeout(() => { if (!S.sheet) { sh.hidden = true; sc.hidden = true; $('#sheet-body').innerHTML = ''; } }, 300);
    restoreFocus(lastFocus);
  }
  function focusKey(el, root) {
    if (!el || !root.contains(el) || el === root) return null;
    if (el.id) return '#' + CSS.escape(el.id);
    const parts = ['data-k', 'data-v', 'data-i', 'data-set-me', 'data-color', 'data-act', 'data-cat-edit']
      .filter(a => el.hasAttribute(a)).map(a => '[' + a + '="' + CSS.escape(el.getAttribute(a)) + '"]');
    return parts.length ? parts.join('') : null;
  }
  // Cancelar una revisión dictada no pierde la frase: queda en el campo de texto para corregirla
  function cancelSheet() {
    const st = S.sheet;
    if (st && st.kind === 'entry' && st.text && !st.editId) { $('#say').value = st.text; updateSend(); }
    closeSheet();
  }
  function renderSheet() {
    const st = S.sheet; if (!st) return;
    const body = $('#sheet-body');
    const scroll = body.scrollTop;
    const fk = focusKey(document.activeElement, body);
    const ok = $('#sheet-ok'), cancel = $('#sheet-cancel');
    if (st.kind === 'entry') renderEntry(st, body, ok, cancel);
    else if (st.kind === 'settings') renderSettings(st, body, ok, cancel);
    else if (st.kind === 'cat') renderCatEditor(st, body, ok, cancel);
    body.scrollTop = scroll;
    if (fk) { const el = body.querySelector(fk); if (el) el.focus({ preventScroll: true }); }
  }

  // ---- hoja: movimiento(s)
  function draftFromParsed(it) {
    return {
      type: it.type, amount: amountStr(it.amountBs), category: it.category || 'otros', who: it.who || 'familia',
      method: it.method || 'cash', source: it.source || 'atm', date: it.date || todayStr(), note: it.note || '',
      currency: it.currency || 'BOB', origAmount: it.amount, rateUsed: it.rateUsed, estimated: !!(it.flags && it.flags.estimated),
      guessed: !!(it.flags && it.flags.categoryGuessed),
      huge: !!(it.flags && it.flags.huge), origBs: it.amountBs,
      open: !!(it.flags && (it.flags.noAmount || it.flags.categoryGuessed || it.flags.estimated || it.flags.huge))
    };
  }
  function blankDraft(type) {
    return { type, amount: '', category: 'otros', who: 'familia', method: 'cash', source: 'atm', date: todayStr(), note: '', currency: 'BOB', guessed: false, open: true };
  }
  function draftFromTxn(t) {
    return {
      type: t.type === 'adjust' ? 'adjust' : t.type, amount: amountStr(Math.abs(t.amount)), sign: t.amount < 0 ? -1 : 1,
      category: t.category || 'otros', who: t.who || 'familia', method: t.method || 'cash', source: t.source || 'other',
      date: t.date, note: typeof t.note === 'string' ? t.note : '', currency: t.currency || 'BOB', origAmount: t.origAmount, origBs: Math.abs(t.amount), guessed: false, open: true
    };
  }

  function draftLine(d) {
    if (d.type === 'cash_in') return srcOf(d.source).name;
    if (d.type === 'adjust') return 'Ajuste de efectivo';
    const c = catById(d.category);
    const bits = [catShort(c)];
    if (whoName(d.who) !== catShort(c)) bits.push(whoName(d.who));
    if (d.method === 'qr') bits.push('QR / tarjeta');
    return bits.join(' · ');
  }
  function draftTile(d) {
    return tileHTML({ type: d.type, source: d.source, category: d.category });
  }

  function formHTML(d, i, allCats) {
    let h = '<div class="form" id="form-' + i + '">';
    if (d.type !== 'adjust') {
      h += '<div class="seg" role="group" aria-label="Tipo de movimiento">' +
        '<button type="button" data-k="type" data-v="expense" data-i="' + i + '" aria-pressed="' + (d.type === 'expense') + '">Gasto</button>' +
        '<button type="button" data-k="type" data-v="cash_in" data-i="' + i + '" aria-pressed="' + (d.type === 'cash_in') + '">Entrada de efectivo</button></div>';
    }
    const aid = 'amt-' + i;
    h += '<div><label class="sr" for="' + aid + '">Importe en bolivianos</label><div class="amount-in' + (d.err ? ' err' : '') + '"><span>Bs</span>' +
      (d.type === 'adjust' ? '<span aria-hidden="true">' + (d.sign < 0 ? '−' : '+') + '</span>' : '') +
      '<input id="' + aid + '" data-k="amount" data-i="' + i + '" type="text" inputmode="decimal" placeholder="0" autocomplete="off" value="' + esc(d.amount) + '"' +
      (d.err ? ' aria-invalid="true" aria-describedby="amt-err-' + i + '"' : '') + '/></div>';
    if (d.huge && !d.err) h += '<p class="note warn">Es un importe muy alto. Revísalo antes de guardar.</p>';
    if (d.err) h += '<p class="note err-msg" id="amt-err-' + i + '">Escribe el importe (solo números, por ejemplo 25 o 12,50).</p>';
    const convValid = parseAmount(d.amount) === d.origBs;
    if (d.type === 'cash_in' && d.currency !== 'BOB' && d.origAmount && convValid) {
      h += '<p class="note' + (d.estimated ? ' warn' : '') + '">' + esc(num(d.origAmount) + ' ' + d.currency) + (d.estimated ? ' × ' + esc(String(d.rateUsed).replace('.', ',')) + ' (tipo de cambio de Ajustes). Revisa lo que te dieron.' : (d.rateUsed ? ' × ' + esc(String(d.rateUsed).replace('.', ',')) : '')) + '</p>';
    } else if (d.type === 'expense' && d.currency && d.currency !== 'BOB' && d.origAmount && convValid) {
      h += '<p class="note warn">' + esc(num(d.origAmount) + ' ' + d.currency) + ' convertidos con el tipo de cambio de Ajustes. Revísalo.</p>';
    }
    h += '</div>';
    h += '<div class="field"><label for="note-' + i + '">Concepto</label><input id="note-' + i + '" type="text" data-k="note" data-i="' + i + '" maxlength="80" placeholder="' +
      (d.type === 'expense' ? 'Ej.: Café en la plaza' : d.type === 'adjust' ? 'Ajuste de efectivo' : 'Ej.: Cajero BNB') + '" value="' + esc(d.note) + '"/></div>';
    if (d.type === 'expense') {
      h += '<div class="field"><span class="flabel" id="cl-' + i + '">Categoría' + (d.guessed ? ' · <span style="font-weight:500">no la he reconocido, elige una</span>' : '') + '</span><div class="cats" role="group" aria-labelledby="cl-' + i + '">' +
        allCats.map(c => '<button type="button" class="cat" data-k="category" data-v="' + esc(c.id) + '" data-i="' + i + '" aria-pressed="' + (d.category === c.id) + '">' +
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
    h += '<div class="field"><label for="date-' + i + '">Fecha</label><input id="date-' + i + '" type="date" data-k="date" data-i="' + i + '" value="' + esc(d.date) + '" max="' + todayStr() + '"' +
      (d.dateErr ? ' aria-invalid="true" aria-describedby="date-err-' + i + '"' : '') + '/>' +
      (d.dateErr ? '<p class="note err-msg" id="date-err-' + i + '">La fecha no puede ser posterior a hoy.</p>' : '') + '</div>';
    return h + '</div>';
  }

  function renderEntry(st, body, ok, cancel) {
    const n = st.items.length;
    const total = st.items.reduce((s, d) => s + (d.type === 'expense' ? (parseAmount(d.amount) || 0) : 0), 0);
    $('#sheet-title').textContent = st.editId ? 'Editar' : n > 1 ? n + ' movimientos' : st.items[0].type === 'cash_in' ? 'Entrada de efectivo' : st.items[0].type === 'adjust' ? 'Ajuste' : 'Nuevo gasto';
    ok.textContent = st.saving ? 'Guardando…' : 'Guardar'; ok.disabled = !S.canWrite || st.saving; cancel.textContent = 'Cancelar'; cancel.disabled = !!st.saving; cancel.classList.remove('ghost'); cancel.tabIndex = 0;
    let h = '';
    if (!S.canWrite) h += '<div class="banner">' + svg('info') + '<span><strong>Solo lectura.</strong> Necesitas acceso de Editor para guardar.</span></div>';
    if (st.text) h += '<div class="quote">' + svg('quote') + '<q class="q-text"></q><button type="button" data-act="edit-text">Corregir</button></div>';
    const allCats = cats();
    if (n > 1) {
      h += '<div class="total-line"><span>Revisa cada uno; toca para cambiarlo</span>' + (total ? '<strong>Total ' + esc(money(total)) + '</strong>' : '') + '</div>';
    }
    st.items.forEach((d, i) => {
      h += '<div class="item" data-i="' + i + '">';
      if (n > 1) {
        const amt = parseAmount(d.amount);
        const problem = !amt ? 'Falta el importe' : d.guessed ? 'Elige la categoría' : '';
        if (d.open) h += '<div class="item-h"><span>' + (i + 1) + ' de ' + n + '</span><button type="button" data-act="remove" data-i="' + i + '">Quitar</button></div>';
        h += '<button type="button" class="mini" data-act="toggle" data-i="' + i + '" aria-expanded="' + !!d.open + '" aria-controls="form-' + i + '">' + draftTile(d) +
          '<span class="txt"><span class="t1 mini-note" data-i="' + i + '"></span><span class="t2' + (problem ? ' warn' : '') + '">' + esc(problem || draftLine(d)) + '</span></span>' +
          '<span class="amt">' + (amt ? esc((d.type === 'cash_in' ? '+' : '') + money(amt)) : '—') + '</span>' + svg('right', ' class="chev"') + '</button>';
        if (d.open) h += formHTML(d, i, allCats);
      } else {
        h += formHTML(d, i, allCats);
      }
      h += '</div>';
    });
    if (st.editId) {
      const t = S.txns.get(st.editId);
      h += '<button type="button" class="danger-btn' + (st.armed ? ' armed' : '') + '" data-act="delete"' + (S.canWrite ? '' : ' disabled') + '>' + (st.armed ? 'Toca otra vez para eliminar' : 'Eliminar movimiento') + '</button>';
      if (t) {
        const bits = [];
        if (typeof t.by === 'string' && t.by) bits.push('Añadido por ' + t.by);
        if (typeof t.createdAt === 'number' && isFinite(t.createdAt)) bits.push(DF_SHORT.format(new Date(t.createdAt)) + ', ' + DF_TIME.format(new Date(t.createdAt)));
        if (bits.length) h += '<p class="meta-line">' + esc(bits.join(' · ')) + '</p>';
      }
    } else if (st.text && S.canWrite) {
      h += '<p class="meta-line">Revisa y toca <strong>Guardar</strong>. Puedes cambiar cualquier dato.</p>';
    }
    body.innerHTML = h;
    if (st.saving) $$('input, button, textarea', body).forEach(el => { el.disabled = true; });
    if (st.text) $('.q-text', body).textContent = st.text;
    $$('.mini-note', body).forEach(el => {
      const d = st.items[+el.dataset.i];
      el.textContent = d.note || (d.type === 'expense' ? catById(d.category).name : d.type === 'cash_in' ? srcOf(d.source).name : 'Ajuste');
    });
  }

  function onEntryClick(e) {
    const st = S.sheet; if (!st || st.kind !== 'entry' || st.saving) return;
    const b = e.target.closest('button'); if (!b) return;
    const act = b.dataset.act;
    if (act === 'edit-text') {
      const t = st.text; closeSheet();
      const inp = $('#say'); inp.value = t; updateSend(); setTab('home');
      setTimeout(() => { inp.focus(); inp.setSelectionRange(t.length, t.length); }, 320);
      return;
    }
    if (act === 'toggle') { const d = st.items[+b.dataset.i]; d.open = !d.open; renderSheet(); const m = $('.mini[data-i="' + b.dataset.i + '"]'); if (m) m.focus({ preventScroll: true }); return; }
    if (act === 'remove') {
      const i = +b.dataset.i;
      st.items.splice(i, 1);
      if (!st.items.length) { closeSheet(); return; }
      if (st.items.length === 1) st.items[0].open = true;
      renderSheet();
      const next = $('.mini[data-i="' + Math.min(i, st.items.length - 1) + '"]') || $('#amt-0') || $('#sheet-ok');
      if (next) next.focus({ preventScroll: true });
      return;
    }
    if (act === 'delete') {
      if (!S.canWrite) return;
      if (!st.armed) { st.armed = true; renderSheet(); setTimeout(() => { if (S.sheet === st && st.armed) { st.armed = false; renderSheet(); } }, 4000); return; }
      const id = st.editId, prev = S.txns.get(id);
      closeSheet();
      if (!prev) { toast('Este movimiento ya no existe.'); return; }
      Store.del(id).then(() => {
        toast('Movimiento eliminado', 'Deshacer', () => Store.put(id, prev).then(() => toast('Movimiento restaurado')).catch(e2 => toast(errMsg(e2))));
        setTimeout(() => { const b2 = $('#toast-act'); if (!b2.hidden) b2.focus({ preventScroll: true }); }, 320);
      })
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
    const st = S.sheet; if (!st || st.kind !== 'entry' || st.saving) return;
    const el = e.target; const k = el.dataset && el.dataset.k; if (!k) return;
    const d = st.items[+el.dataset.i];
    d[k] = el.value;
    if (k === 'amount') d.huge = false;
    if (k === 'amount' && d.err && parseAmount(el.value)) {
      d.err = false;
      const wrap = el.closest('.amount-in'); if (wrap) wrap.classList.remove('err');
      el.removeAttribute('aria-invalid'); el.removeAttribute('aria-describedby');
      const nt = $('#amt-err-' + el.dataset.i); if (nt) nt.remove();
    }
  }

  async function saveEntry() {
    const st = S.sheet; if (!st || st.kind !== 'entry' || st.saving || !S.canWrite) return;
    const today = todayStr();
    let bad = -1;
    st.items.forEach((d, i) => {
      d.err = !parseAmount(d.amount);
      if (!/^\d{4}-\d{2}-\d{2}$/.test(d.date) || isNaN(parseYmd(d.date).getTime())) d.date = today;
      d.dateErr = d.date > today;
      if ((d.err || d.dateErr) && bad < 0) bad = i;
      if (d.err || d.dateErr) d.open = true;
    });
    if (bad >= 0) {
      renderSheet();
      const f = $('#form-' + bad + ' [aria-invalid="true"]') || $('[aria-invalid="true"]');
      if (f) f.focus();
      return;
    }
    if (st.editId && !S.txns.has(st.editId)) { closeSheet(); toast('Este movimiento se eliminó en otro móvil.'); return; }
    st.saving = true; renderSheet();
    const now = Date.now();
    // Cada borrador conserva su id: si se reintenta tras un fallo, se reescribe el mismo documento (sin duplicados).
    const writes = st.items.map((d, i) => {
      const amount = parseAmount(d.amount);
      const base = {
        type: d.type, amount: d.type === 'adjust' ? amount * (d.sign || 1) : amount, date: d.date, note: String(d.note || '').trim().slice(0, 80),
        updatedAt: now
      };
      if (d.type === 'expense') Object.assign(base, { category: d.category, who: d.who, method: d.method === 'qr' ? 'qr' : 'cash' });
      if (d.type === 'cash_in') Object.assign(base, { source: d.source || 'other' });
      if (d.currency && d.currency !== 'BOB' && d.origAmount && d.type !== 'adjust' && amount === d.origBs) Object.assign(base, { currency: d.currency, origAmount: d.origAmount });
      if (st.editId) {
        const prev = S.txns.get(st.editId) || {};
        const keep = { createdAt: typeof prev.createdAt === 'number' ? prev.createdAt : now, by: prev.by || S.me || null };
        if (prev.text) keep.text = prev.text;
        if (prev.byId) keep.byId = prev.byId;
        return { d, id: st.editId, doc: Object.assign(keep, base), prev };
      }
      if (!d.id) d.id = newId();
      const doc = Object.assign(base, { createdAt: d.createdAt || (d.createdAt = now + i), by: S.me || null });
      if (S.uid) doc.byId = S.uid;
      if (st.text) doc.text = st.text.slice(0, 300);
      return { d, id: d.id, doc, prev: null };
    });
    const done = [];
    try {
      for (const w of writes) { await Store.put(w.id, w.doc); done.push(w); }
    } catch (err) {
      st.saving = false;
      if (S.sheet === st) {
        if (done.length && !st.editId) {
          st.items = st.items.filter(d => !done.some(w => w.d === d));
          st.items.forEach(d => { d.open = st.items.length === 1 || d.open; });
          toast('Se guardaron ' + done.length + ' de ' + writes.length + '. ' + errMsg(err));
        } else toast(errMsg(err));
        renderSheet();
      } else toast(errMsg(err));
      return;
    }
    if (S.sheet === st) {
      closeSheet();
      $('#say').value = ''; updateSend(); $('#live').innerHTML = '';
    }
    const msg = st.editId ? 'Cambios guardados' : writes.length > 1 ? writes.length + ' movimientos guardados' : writes[0].doc.type === 'cash_in' ? 'Entrada guardada' : 'Gasto guardado';
    toast(msg, 'Deshacer', async () => {
      try {
        for (const w of writes) { if (w.prev) await Store.put(w.id, w.prev); else await Store.del(w.id); }
        toast('Deshecho');
      } catch (e2) { toast(errMsg(e2)); }
    });
  }

  // ---- hoja: ajustes
  function renderSettings(st, body, ok, cancel) {
    $('#sheet-title').textContent = 'Ajustes';
    ok.textContent = 'Listo'; ok.disabled = false; cancel.classList.add('ghost'); cancel.tabIndex = -1;
    const rt = rates();
    const bal = balance(allTxns());
    const ro = !S.canWrite;
    const waiting = !dataReady();
    const cfgWait = S.mode === 'shared' && !S.configLoaded;
    let h = '';
    h += '<div class="set-sec"><h3>Este móvil</h3><div class="set-card" style="padding:12px 14px"><div class="seg" role="group" aria-label="Quién usa este móvil">' +
      PEOPLE.map(p => '<button type="button" data-set-me="' + p + '" aria-pressed="' + (S.me === p) + '">Soy ' + p + '</button>').join('') +
      '</div></div><p class="foot">Sirve para entender «para mí» y para saber quién anotó cada gasto.</p></div>';
    h += '<div class="set-sec"><h3>Efectivo en mano</h3><div class="set-card"><div class="set-row"><label class="grow" for="adj-in">Tengo ahora<small>' + (waiting ? 'Esperando datos…' : 'Qullqi calcula <span class="nowrap">' + (bal < 0 ? '−' : '') + esc(money(bal)) + '</span>') + '</small></label>' +
      '<input class="inline" id="adj-in" type="text" inputmode="decimal" placeholder="Bs"' + (ro || S.adjusting || waiting ? ' disabled' : '') + '/>' +
      '<button type="button" class="act" data-act="adjust"' + (ro || S.adjusting || waiting ? ' disabled' : '') + '>Ajustar</button></div></div>' +
      '<p class="foot">Cuenta los billetes y escribe lo que tenéis. Se anota un ajuste por la diferencia.</p></div>';
    h += '<div class="set-sec"><h3>Tipo de cambio</h3><div class="set-card">' +
      ['USD', 'EUR'].map(c => '<div class="set-row"><label class="grow" for="rate-' + c + '">1 ' + (c === 'USD' ? 'dólar' : 'euro') + ' =</label><input class="inline" id="rate-' + c + '" data-rate="' + c + '" type="text" inputmode="decimal" value="' + esc(String(rt[c]).replace('.', ',')) + '"' + (ro || cfgWait ? ' disabled' : '') + '/><span>Bs</span></div>').join('') +
      '</div><p class="foot">Solo se usa si dictas un cambio sin decir cuántos bolivianos te dieron.</p></div>';
    h += '<div class="set-sec"><h3>Categorías</h3><ul class="set-card list">' +
      cats().map(c => '<li><button type="button" class="set-row" data-cat-edit="' + esc(c.id) + '"' + (ro || cfgWait ? ' disabled' : '') + '><span class="tile sm" style="background:' + esc(c.color) + '">' + svg(c.icon) + '</span>' +
        '<span class="grow"><span class="cn"></span><small class="ck"></small></span>' + svg('right', ' class="chev"') + '</button></li>').join('') +
      (ro || cfgWait ? '' : '<li><button type="button" class="set-row link-row" data-cat-edit="__new">' + svg('plus', ' style="width:20px;height:20px"') + '<span class="grow">Añadir categoría</span></button></li>') +
      '</ul><p class="foot">Añade palabras clave (por ejemplo, el nombre de la niñera) para que la categoría se elija sola al dictar.</p></div>';
    h += '<div class="set-sec"><h3>Datos</h3><div class="set-card">' +
      '<button type="button" class="set-row link-row" data-act="csv">' + (S.downloads ? 'Descargar movimientos (CSV)' : 'Copiar movimientos (CSV)') + '</button></div>' +
      '<div id="csv-box"></div><p class="foot">' + (S.mode === 'shared' ? 'Los datos se guardan en Qullqi y se comparten con quien tenga acceso. Se actualizan solos en todos los móviles.' : 'Los datos se guardan solo en este navegador.') + ' ' + S.txns.size + ' movimientos.</p></div>';
    h += '<div class="set-sec"><h3>Cómo dictar</h3><div class="set-card" style="padding:12px 14px;font-size:15px;line-height:1.55">' +
      '<q>Me acabo de gastar 25 bolivianos en un café para Sol y Pol</q><br><q>120 en el súper para toda la familia</q><br><q>Taxi a Santa Cruz 350 con QR</q><br><q>Ayer pagamos 150 a la señora de la limpieza</q><br><q>Saqué 1000 del cajero</q><br><q>Cambié 100 dólares y me dieron 950</q><br><q>Pan 10, leche 12 y taxi 15</q></div></div>';
    h += '<div class="set-sec"><h3>Acerca de</h3><div class="set-card about">' + LOGO +
      '<p><strong>Qullqi</strong>«Plata, dinero» en quechua y aimara. La plata de la familia, anotada en un momento.</p></div></div>';
    body.innerHTML = h;
    $$('[data-cat-edit]', body).forEach(b => {
      const c = cats().find(x => x.id === b.dataset.catEdit); if (!c) return;
      $('.cn', b).textContent = c.name;
      $('.ck', b).textContent = c.keywords.length ? 'Tus palabras: ' + c.keywords.join(', ') : c.id === 'otros' ? 'Lo que no encaja en otra categoría' : (c.hint ? 'Reconoce: ' + c.hint : 'Sin palabras clave');
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
      const inp = $('#adj-in');
      const target = parseCashInput(inp.value);
      if (target === null) { toast(/^\s*[-−]/.test(inp.value) ? 'El efectivo no puede ser negativo.' : 'Escribe cuánto efectivo tenéis.'); inp.focus(); return; }
      inp.value = '';
      const okSet = await setCashTo(target);
      if (S.sheet === st) renderSheet();
      if (!okSet) { const i2 = $('#adj-in'); if (i2) i2.value = amountStr(target); }
      return;
    }
    if (b.dataset.act === 'csv') { exportCsv(); return; }
  }
  function parseCashInput(v) {
    const raw = String(v || '').trim();
    if (/^0+([.,]0+)?$/.test(raw)) return 0;
    return parseAmount(raw);
  }
  // Ajusta el efectivo a una cantidad: anota la diferencia. Protegido contra dobles toques.
  async function setCashTo(target, note) {
    if (S.adjusting || !S.canWrite) return false;
    if (!dataReady()) { toast('Esperando a que carguen los datos. Inténtalo en un momento.'); return false; }
    S.adjusting = true;
    try {
      const initial = note === 'Efectivo inicial';
      // el efectivo inicial usa un id fijo (dos móviles a la vez = uno solo) y nunca pisa uno existente
      if (initial && S.mode === 'shared') {
        const ex = await withTimeout(Store.db.collection('txns').doc('efectivo-inicial').get());
        if (ex.exists) { toast('Ya se indicó el efectivo inicial. Si no cuadra, usa Ajustes.'); return true; }
      }
      // si un intento anterior no se confirmó, se reutiliza su id para no duplicar el ajuste
      const pend = S.pendingAdjust && S.pendingAdjust.target === target ? S.pendingAdjust.id : null;
      const id = initial ? 'efectivo-inicial' : (pend || newId());
      // el saldo se calcula con una lectura fresca del servidor, nunca con una copia en caché
      let base = allTxns();
      if (S.mode === 'shared') {
        let snap;
        try { snap = await withTimeout(Store.db.collection('txns').get(), 10000); } catch (e) { snap = null; }
        if (!snap || (snap.metadata && snap.metadata.fromCache)) { toast('Sin conexión: no se puede comprobar el efectivo ahora. Inténtalo con señal.'); return false; }
        const m = new Map(); snap.docs.forEach(d => { if (d.exists) m.set(d.id, d.data()); });
        base = validTxns(m);
      }
      const diff = r2(target - balance(base.filter(t => t.id !== id)));
      if (!diff) { toast('El efectivo ya cuadra.'); S.pendingAdjust = null; return true; }
      const doc = { type: 'adjust', amount: diff, date: todayStr(), note: note || 'Ajuste de efectivo', createdAt: Date.now(), updatedAt: Date.now(), by: S.me || null };
      S.pendingAdjust = { id, target };
      await Store.put(id, doc);
      S.pendingAdjust = null;
      toast('Efectivo: ' + money(target), 'Deshacer', () => Store.del(id).then(() => {
        if (initial) ls.set('qullqi.started', '0');
        toast('Deshecho');
      }).catch(e2 => toast(errMsg(e2))));
      return true;
    } catch (err) { toast(errMsg(err, 'Inténtalo otra vez')); return false; }
    finally { S.adjusting = false; }
  }
  function onSettingsChange(e) {
    const el = e.target;
    if (el.dataset && el.dataset.rate) {
      const v = parseAmount(el.value);
      if (!v || v > 1000) { toast('Tipo de cambio no válido.'); el.value = String(rates()[el.dataset.rate]).replace('.', ','); return; }
      Store.patchConfig({ rates: { [el.dataset.rate]: v } }).then(() => toast('Tipo de cambio guardado')).catch(err => toast(errMsg(err)));
    }
  }

  function meKey() { return 'qullqi.me.' + (S.uid || 'local'); }
  function setMe(name) {
    S.me = name; ls.set(meKey(), name);
    if (S.mode === 'shared' && S.uid && S.canWrite) Store.patchConfig({ members: { [S.uid]: name } }).catch(() => {});
    if (S.sheet && S.sheet.kind === 'settings') renderSheet();
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
        CUSTOM_COLORS.map(c => '<button type="button" data-color="' + c + '" aria-label="' + (COLOR_NAMES[c] || 'Color') + '" aria-pressed="' + (st.color === c) + '" style="--sw:' + c + '"></button>').join('') + '</div></div>';
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
    const q = (v, isNum) => {
      let s = String(v == null ? '' : v);
      if (isNum) s = s.replace('.', ',');
      else if (/^\s*[=+\-@\t\r]/.test(s)) s = "'" + s; // evita fórmulas al abrir en una hoja de cálculo
      return /[",\n;\r]/.test(s) ? '"' + s.replace(/"/g, '""') + '"' : s;
    };
    const rows = [['fecha', 'tipo', 'importe_bs', 'categoria', 'para', 'pago', 'concepto', 'anotado_por', 'texto_dictado']];
    allTxns().forEach(t => rows.push([
      t.date, t.type === 'expense' ? 'gasto' : t.type === 'cash_in' ? 'entrada' : 'ajuste',
      String(t.type === 'expense' ? -t.amount : t.amount),
      t.type === 'expense' ? catById(t.category).name : t.type === 'cash_in' ? srcOf(t.source).name : '',
      t.type === 'expense' ? whoName(t.who) : '', t.type === 'expense' ? (t.method === 'qr' ? 'QR/tarjeta' : 'efectivo') : '',
      t.note || '', t.by || '', t.text || ''
    ]));
    // separador «;» y coma decimal, como espera Excel en español; BOM para que respete las tildes
    return '\ufeff' + rows.map((r, ri) => r.map((v, ci) => q(v, ri > 0 && ci === 2)).join(';')).join('\r\n');
  }
  function exportCsv() {
    const text = csvText();
    if (S.downloads) {
      S.downloads.save({ filename: 'qullqi-' + todayStr() + '.csv', data: text })
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
    d.onFinal(({ text }) => {
      text = String(text || '');
      if (!S.finals) S.finals = text.replace(/^\s+/, '');
      else S.finals += (/^\s/.test(text) || /\s$/.test(S.finals) ? '' : ' ') + text;
      S.partial = ''; renderLive();
    });
    d.onState(({ state, reason }) => {
      S.dictState = state; renderMic();
      if (state === 'listening') S.heard = true;
      if (state === 'idle') {
        const text = (S.finals || S.partial || '').trim();
        const want = S.wantProcess; S.wantProcess = false;
        if (want && text && S.sheet) { $('#live').innerHTML = ''; $('#say').value = text; updateSend(); toast('He dejado lo dictado en el campo de texto.'); }
        else if (want && text) { $('#live').innerHTML = ''; processText(text); }
        else if (reason === 'not_granted') { S.dict = null; renderMic(); toast('Dictado desactivado. Usa el micrófono del teclado.'); }
        else if (reason === 'unavailable' && S.heard) toast('El dictado no está disponible ahora. Usa el micrófono del teclado.');
        else if (reason === 'rate_limited') toast('El micrófono está ocupado. Prueba en unos segundos.');
        else if (reason === 'upstream_error') toast('No se pudo usar el micrófono. Inténtalo otra vez.');
        else if (reason === 'dismissed') toast('Para dictar, permite el micrófono cuando te lo pida.');
        else if (want && !text && S.heard && (reason === 'stopped' || reason === 'viewer_stopped')) toast('No te he oído. Prueba otra vez, más cerca del móvil.');
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
    S.finals = ''; S.partial = ''; S.wantProcess = true; S.heard = false; renderLive();
    const opts = S.dictLang ? { language: 'es' } : undefined;
    d.start(opts).catch(err => {
      S.wantProcess = false;
      const c = err && err.code;
      if (c === 'unavailable' && opts) { S.dictLang = false; toast('Toca otra vez el micrófono para dictar.'); }
      else if (c === 'unavailable' || c === 'not_granted') { S.dict = null; renderMic(); toast('El dictado no está disponible aquí. Usa el micrófono del teclado.'); }
      else if (c === 'rate_limited') toast('El micrófono está ocupado. Prueba en unos segundos.');
      else if (c === 'upstream_error') toast('No se pudo usar el micrófono. Inténtalo otra vez.');
      else if (c === 'no_gesture') toast('Toca el micrófono otra vez.');
      else if (c === 'dismissed') toast('Para dictar, permite el micrófono cuando te lo pida.');
    });
  }

  // ---------------------------------------------------------------- eventos
  function updateSend() { $('#send').disabled = !$('#say').value.trim(); }
  function wire() {
    $$('[data-icon]').forEach(el => { el.outerHTML = svg(el.dataset.icon); });
    $$('[data-logo]').forEach(el => { el.outerHTML = LOGO; });
    $('#btn-settings').innerHTML = svg('adjust');
    $('#send').innerHTML = svg('send');
    $('#m-prev').innerHTML = svg('left');
    $('#m-next').innerHTML = svg('right');

    $('#mic').addEventListener('click', onMic);
    $('#say').addEventListener('input', updateSend);
    const sendTyped = () => { const v = $('#say').value; if (v.trim()) { processText(v); lastFocus = $('#say'); } };
    $('#typebar').addEventListener('submit', e => { e.preventDefault(); sendTyped(); });
    $('#say').addEventListener('keydown', e => { if (e.key === 'Enter' && !e.isComposing) { e.preventDefault(); sendTyped(); } });
    $('#send').addEventListener('click', e => { e.preventDefault(); sendTyped(); });
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
      if (e.target.closest && e.target.closest('#fix-cash')) { openSheet({ kind: 'settings', focus: '#adj-in' }); return; }
      if (e.target.closest && e.target.closest('#start-zero')) { ls.set('qullqi.started', '1'); renderHome(); return; }
    });
    const startNow = async () => {
      if (S.starting) return;
      const inp = $('#start-in'); if (!inp) return;
      const v = parseCashInput(inp.value);
      if (v === null) { toast('Escribe cuánto efectivo tenéis, o empieza desde 0.'); inp.focus(); return; }
      if (v === 0) { ls.set('qullqi.started', '1'); renderHome(); return; }
      S.starting = true;
      try { if (await setCashTo(v, 'Efectivo inicial')) { ls.set('qullqi.started', '1'); restoreFocus($('#mic')); } } finally { S.starting = false; }
    };
    document.addEventListener('submit', e => { if (e.target.id === 'start-form') { e.preventDefault(); startNow(); } });
    document.addEventListener('keydown', e => { if (e.target && e.target.id === 'start-in' && e.key === 'Enter' && !e.isComposing) { e.preventDefault(); startNow(); } });
    document.addEventListener('click', e => { if (e.target.closest && e.target.closest('#start-go')) { e.preventDefault(); startNow(); } });

    $('#hist-seg').addEventListener('click', e => {
      const b = e.target.closest('button'); if (!b) return;
      S.hist.f = b.dataset.f; S.hist.limit = 80;
      if (b.dataset.f !== 'expense') { S.hist.cat = null; S.hist.who = null; S.hist.month = null; }
      renderHist();
    });
    let qTimer = null;
    $('#q').addEventListener('input', e => { clearTimeout(qTimer); qTimer = setTimeout(() => { S.hist.q = e.target.value; S.hist.limit = 80; renderHist(); }, 150); });
    $('#scr-hist').addEventListener('click', e => {
      if (e.target.closest('#clear-cat')) { S.hist.cat = null; S.hist.who = null; S.hist.month = null; S.hist.f = 'all'; renderHist(); }
      if (e.target.closest('#more')) { S.hist.limit += 120; renderHist(); }
    });
    $('#m-prev').addEventListener('click', () => { const [y, m] = S.month.split('-').map(Number); S.month = ymd(new Date(y, m - 2, 1)).slice(0, 7); renderSum(); });
    $('#m-next').addEventListener('click', () => { const [y, m] = S.month.split('-').map(Number); S.month = ymd(new Date(y, m, 1)).slice(0, 7); renderSum(); });
    $('#sum-body').addEventListener('click', e => {
      const b = e.target.closest('[data-cat], [data-who]'); if (!b) return;
      S.hist.cat = b.dataset.cat || null; S.hist.who = b.dataset.who || null;
      S.hist.month = S.month; S.hist.f = 'expense'; S.hist.q = ''; $('#q').value = ''; S.hist.limit = 80;
      setTab('hist');
    });

    const sb = $('#sheet-body');
    sb.addEventListener('click', e => { onEntryClick(e); onSettingsClick(e); onCatClick(e); });
    sb.addEventListener('input', onEntryInput);
    sb.addEventListener('change', onSettingsChange);
    sb.addEventListener('keydown', e => {
      if (e.key === 'Enter' && S.sheet && S.sheet.kind === 'cat' && e.target.id === 'cat-name') { e.preventDefault(); saveCat(); return; }
      if (e.key === 'Enter' && S.sheet && S.sheet.kind === 'entry' && e.target.tagName === 'INPUT' && e.target.type !== 'date') { e.preventDefault(); saveEntry(); }
    });
    $('#sheet-ok').addEventListener('click', () => {
      const st = S.sheet; if (!st) return;
      if (st.kind === 'entry') saveEntry();
      else if (st.kind === 'settings') closeSheet();
      else if (st.kind === 'cat') saveCat();
    });
    $('#sheet-cancel').addEventListener('click', () => { if (S.sheet && S.sheet.saving) return; if (S.sheet && S.sheet.kind === 'cat') openSheet({ kind: 'settings' }); else cancelSheet(); });
    // Tocar fuera no descarta un gasto a medio revisar (hay que usar Cancelar)
    $('#scrim').addEventListener('click', () => { if (S.sheet && S.sheet.kind !== 'entry') closeSheet(); });
    document.addEventListener('keydown', e => { if (e.key === 'Escape' && S.sheet && !S.sheet.saving) { e.preventDefault(); if (S.sheet.kind === 'cat') openSheet({ kind: 'settings' }); else cancelSheet(); } });
    // focus trap básico
    $('#sheet').addEventListener('keydown', e => {
      if (e.key !== 'Tab' || !S.sheet) return;
      const f = $$('#sheet button:not([disabled]):not([hidden]), #sheet input:not([disabled]), #sheet textarea:not([disabled]), #sheet [tabindex="0"]')
        .filter(x => x.offsetParent !== null && x.tabIndex >= 0 && getComputedStyle(x).visibility !== 'hidden');
      if (!f.length) return;
      const first = f[0], last = f[f.length - 1];
      if (e.shiftKey && document.activeElement === first) { e.preventDefault(); last.focus(); }
      else if (!e.shiftKey && document.activeElement === last) { e.preventDefault(); first.focus(); }
    });
    // cambio de día con la app abierta
    document.addEventListener('visibilitychange', () => { if (!document.hidden) renderAll(); });
    let lastDay = todayStr();
    setInterval(() => {
      const d = todayStr();
      if (d === lastDay) return;
      if (S.month === monthOf(lastDay)) S.month = monthOf(d);
      lastDay = d; renderAll();
    }, 60000);
  }

  // ---------------------------------------------------------------- arranque
  async function boot() {
    try { document.documentElement.lang = 'es'; } catch (e) { /* */ }
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
  window.__qullqi = { S, Store, processText, balance, allTxns };
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', boot); else boot();
})();
