// Simulador mínimo del runtime de artifacts (db compartida, user, dictation, downloads)
// para pruebas locales con Playwright. Se inyecta con page.addInitScript.
// Config opcional antes de cargar: window.__MOCK = { uid, canWrite, noDictation, noDb, failWrites }
(function () {
  const cfg = Object.assign({ uid: 'u1', canWrite: true }, window.__MOCK || {});
  const KEY = '__mockdb';
  const load = () => { try { return JSON.parse(localStorage.getItem(KEY) || '{}'); } catch (e) { return {}; } };
  const save = o => localStorage.setItem(KEY, JSON.stringify(o));
  const listeners = new Set();
  const fire = () => listeners.forEach(fn => fn());
  window.addEventListener('storage', e => { if (e.key === KEY) fire(); });
  const snapDoc = (id, data) => ({ id, exists: data !== undefined, data: () => data && JSON.parse(JSON.stringify(data)), metadata: { fromCache: false, hasPendingWrites: false } });
  const err = (code) => Promise.reject({ code, message: code });
  function merge(a, b) { for (const k in b) { if (b[k] && typeof b[k] === 'object' && !Array.isArray(b[k])) a[k] = merge(a[k] && typeof a[k] === 'object' ? a[k] : {}, b[k]); else a[k] = b[k]; } return a; }
  function docRef(path) {
    return {
      id: path.split('/').pop(), path,
      get: async () => snapDoc(path.split('/').pop(), load()[path]),
      set: async d => { if (!cfg.canWrite || cfg.failWrites) return err(cfg.failWrites || 'invalid_argument'); const o = load(); o[path] = JSON.parse(JSON.stringify(d)); save(o); fire(); },
      update: async d => { if (!cfg.canWrite) return err('invalid_argument'); const o = load(); if (!o[path]) return err('invalid_argument'); o[path] = merge(o[path], JSON.parse(JSON.stringify(d))); save(o); fire(); },
      delete: async () => { if (!cfg.canWrite) return err('invalid_argument'); const o = load(); delete o[path]; save(o); fire(); },
      onSnapshot(next) { const fn = () => next(snapDoc(path.split('/').pop(), load()[path])); listeners.add(fn); setTimeout(fn, 30); return () => listeners.delete(fn); }
    };
  }
  function colRef(path) {
    return {
      path,
      doc: id => docRef(path + '/' + (id || Math.random().toString(36).slice(2))),
      onSnapshot(next) {
        const fn = () => {
          const o = load();
          const docs = Object.keys(o).filter(k => k.startsWith(path + '/') && k.split('/').length === path.split('/').length + 1).map(k => snapDoc(k.split('/').pop(), o[k]));
          next({ docs, size: docs.length, empty: !docs.length, docChanges: () => [], metadata: { fromCache: false, hasPendingWrites: false } });
        };
        listeners.add(fn); setTimeout(fn, 30); return () => listeners.delete(fn);
      }
    };
  }
  const db = Object.freeze({ doc: docRef, collection: colRef });
  const user = Object.freeze({ id: async () => cfg.uid, can: async () => cfg.canWrite, isOwner: async () => true, canEdit: async () => true });

  // dictado
  let state = 'idle';
  const h = { partial: new Set(), final: new Set(), state: new Set() };
  const emit = (k, v) => h[k].forEach(fn => fn(v));
  const setState = (s, reason) => { state = s; emit('state', reason ? { state: s, reason } : { state: s }); };
  const dictation = Object.freeze({
    start: () => { if (state !== 'idle') return Promise.resolve(); setState('starting'); return new Promise(r => setTimeout(() => { setState('listening'); r(); }, 40)); },
    stop: () => new Promise(r => { if (state === 'idle') return r(); setState('finishing'); setTimeout(() => { setState('idle', 'stopped'); r(); }, 40); }),
    active: () => state !== 'idle',
    onPartial: fn => { h.partial.add(fn); return () => h.partial.delete(fn); },
    onFinal: fn => { h.final.add(fn); return () => h.final.delete(fn); },
    onState: fn => { h.state.add(fn); return () => h.state.delete(fn); }
  });
  window.__mockSpeak = (text) => { emit('partial', { text: text.slice(0, 8) }); emit('partial', { text: '' }); emit('final', { text }); };
  window.__downloads = [];
  const downloads = Object.freeze({ save: async (o) => { window.__downloads.push(o); } });

  window.claude = Object.freeze({
    use: name => new Promise(r => setTimeout(() => r(
      name === 'db' ? (cfg.noDb ? null : db) : name === 'user' ? user : name === 'dictation' ? (cfg.noDictation ? null : dictation) : name === 'downloads' ? downloads : null
    ), 20))
  });
})();
