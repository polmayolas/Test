/* Bolsillo — intérprete de gastos dictados en español (sin IA, coste cero).
 * parseUtterance(text, ctx) -> { items: [...], text }
 * Cada item: { type:'expense'|'cash_in', amount, currency, amountBs, category,
 *   who, method, source, date, note, flags:{...} }
 */
(function (root) {
  'use strict';

  // ---------- normalización (misma longitud que el original) ----------
  function strip(s) {
    let out = '';
    for (const c of s) {
      let x = c.normalize('NFD').replace(/[̀-ͯ]/g, '');
      if (x.length !== c.length) x = x.length ? (x + '  ').slice(0, c.length) : ' '.repeat(c.length);
      out += x;
    }
    return out.toLowerCase();
  }

  // ---------- números ----------
  const UNITS = {
    cero: 0, un: 1, uno: 1, una: 1, dos: 2, tres: 3, cuatro: 4, cinco: 5, seis: 6, siete: 7,
    ocho: 8, nueve: 9, diez: 10, once: 11, doce: 12, trece: 13, catorce: 14, quince: 15,
    dieciseis: 16, diecisiete: 17, dieciocho: 18, diecinueve: 19, veinte: 20, veintiun: 21,
    veintiuno: 21, veintiuna: 21, veintidos: 22, veintitres: 23, veinticuatro: 24,
    veinticinco: 25, veintiseis: 26, veintisiete: 27, veintiocho: 28, veintinueve: 29,
    treinta: 30, cuarenta: 40, cincuenta: 50, sesenta: 60, setenta: 70, ochenta: 80,
    noventa: 90, cien: 100, ciento: 100, doscientos: 200, doscientas: 200, trescientos: 300,
    trescientas: 300, cuatrocientos: 400, cuatrocientas: 400, quinientos: 500, quinientas: 500,
    seiscientos: 600, seiscientas: 600, setecientos: 700, setecientas: 700, ochocientos: 800,
    ochocientas: 800, novecientos: 900, novecientas: 900
  };
  const isTens = v => v >= 20 && v <= 90 && v % 10 === 0;
  const isHundreds = v => v >= 100 && v < 1000 && v % 100 === 0;

  function digitValue(raw) {
    let s = raw.replace(/[.,]+$/, '');
    const hasDot = s.includes('.'), hasComma = s.includes(',');
    if (hasDot && hasComma) {
      const dec = s.lastIndexOf('.') > s.lastIndexOf(',') ? '.' : ',';
      const th = dec === '.' ? ',' : '.';
      s = s.split(th).join('').replace(dec, '.');
    } else if (hasDot || hasComma) {
      const sep = hasDot ? '.' : ',';
      const parts = s.split(sep);
      if (parts.length > 2 || (parts.length === 2 && parts[1].length === 3)) s = parts.join('');
      else s = parts.join('.');
    }
    const v = parseFloat(s);
    return isFinite(v) ? v : null;
  }

  // ---------- tokenizador ----------
  function tokenize(norm) {
    const re = /\d(?:[\d.,]*\d)?|[a-z]+|\$|€|[,;.!?¿¡:]/g;
    const toks = [];
    let m;
    while ((m = re.exec(norm))) {
      const t = m[0];
      toks.push({
        t, start: m.index, end: m.index + t.length,
        kind: /^\d/.test(t) ? 'num' : /^[a-z]/.test(t) ? 'word' : t === '$' || t === '€' ? 'sym' : 'punct'
      });
    }
    return toks;
  }

  const CUR = {
    bs: 'BOB', bob: 'BOB', boliviano: 'BOB', bolivianos: 'BOB', peso: 'BOB', pesos: 'BOB',
    pesitos: 'BOB', bolis: 'BOB', 'bs.': 'BOB',
    usd: 'USD', dolar: 'USD', dolares: 'USD', dollar: 'USD', dollars: 'USD', verdes: 'USD',
    eur: 'EUR', euro: 'EUR', euros: 'EUR', '$': 'USD', '€': 'EUR'
  };
  const curOf = tok => tok && (tok.kind === 'word' || tok.kind === 'sym') ? CUR[tok.t] || null : null;

  const VERBS = new Set(('gaste gastado gastamos gastar gasto gastaste gastaron pague pagado pagamos pagar ' +
    'pagaste pagaron costo cuesta costaron cuestan salio salieron son fueron compre compramos ' +
    'saque sacamos sacado sacar retire retiramos retirado retirar cambie cambiamos cambiado ' +
    'recibi recibimos cobre cobramos ingrese ingresamos dieron dio total precio vale valio').split(' '));

  const AFTER_DEC = new Set(['y', 'en', 'para', 'de', 'del', 'por', 'el', 'la', 'al', 'a', 'con']);
  const QTY_OK = new Set(['y', 'e', 'en', 'para', 'de', 'del', 'por', 'con', 'el', 'la', 'al', 'a', 'que', 'mas', 'menos', 'hoy', 'ayer', 'o', 'centavos', 'mil', 'medio', 'media']);

  // Busca candidatos de importe. dropMask: índices de tokens que no pueden ser importe.
  function findAmounts(toks, dropMask) {
    const out = [];
    for (let i = 0; i < toks.length; i++) {
      if (dropMask[i]) continue;
      const tk = toks[i];
      let value = null, j = i, fromWords = false;
      if (tk.kind === 'num') {
        value = digitValue(tk.t); j = i + 1;
        if (toks[j] && toks[j].t === 'mil') { value *= 1000; j++; }
      } else if (tk.kind === 'word' && (tk.t in UNITS || tk.t === 'mil')) {
        // un/una/uno sólo cuentan si van seguidos de "mil" o de moneda
        if ((tk.t === 'un' || tk.t === 'una' || tk.t === 'uno') &&
            !(toks[i + 1] && (toks[i + 1].t === 'mil' || curOf(toks[i + 1])))) continue;
        let total = 0, cur = 0, last = null, k = i, afterY = false;
        while (k < toks.length && !dropMask[k]) {
          const w = toks[k].t;
          if (toks[k].kind === 'word' && w in UNITS) {
            const v = UNITS[w];
            if (last === null) cur += v;
            else if (last === 'mil' && v < 1000) cur += v;
            else if (typeof last === 'number' && isHundreds(last) && v < 100) cur += v;
            else if (typeof last === 'number' && isTens(last) && afterY && v >= 1 && v <= 9) cur += v;
            else break;
            last = v; afterY = false; k++;
          } else if (w === 'mil') {
            cur = (cur || 1) * 1000; total += cur; cur = 0; last = 'mil'; afterY = false; k++;
          } else if (w === 'y' && typeof last === 'number' && isTens(last) && toks[k + 1] &&
                     UNITS[toks[k + 1].t] >= 1 && UNITS[toks[k + 1].t] <= 9) {
            afterY = true; k++;
          } else break;
        }
        if (k === i) continue;
        value = total + cur; j = k; fromWords = true;
      } else continue;
      if (value === null || !(value > 0)) { i = Math.max(i, j - 1); continue; }

      // "y medio"
      if (toks[j] && toks[j].t === 'y' && toks[j + 1] && (toks[j + 1].t === 'medio' || toks[j + 1].t === 'media')) {
        value += 0.5; j += 2;
      }
      // moneda después ("25 bs", "25 bs.", "25 de bolivianos")
      let currency = null, curEnd = j;
      if (curOf(toks[j])) { currency = curOf(toks[j]); curEnd = j + 1; if (toks[curEnd] && toks[curEnd].t === '.' ) curEnd++; }
      // moneda antes ("bs 25", "$20", "usd 20")
      let curStart = i;
      if (!currency && i > 0 && curOf(toks[i - 1]) && !dropMask[i - 1]) { currency = curOf(toks[i - 1]); curStart = i - 1; }
      // decimales "con 50 (centavos)"
      let end = curEnd;
      if (toks[end] && toks[end].t === 'con' && toks[end + 1]) {
        const nt = toks[end + 1];
        const dv = nt.kind === 'num' ? digitValue(nt.t) : (nt.t in UNITS ? UNITS[nt.t] : null);
        let after = end + 2;
        if (nt.kind === 'word' && isTens(dv || 0) && toks[after] && toks[after].t === 'y' && toks[after + 1] && UNITS[toks[after + 1].t] <= 9) {
          after += 2;
        }
        const dv2 = nt.kind === 'word' && after > end + 2 ? dv + UNITS[toks[after - 1].t] : dv;
        if (dv2 !== null && dv2 > 0 && dv2 < 100 && Number.isInteger(value) &&
            (!toks[after] || toks[after].t === 'centavos' || toks[after].kind === 'punct' || curOf(toks[after]) || AFTER_DEC.has(toks[after].t))) {
          value = Math.round((value + dv2 / 100) * 100) / 100;
          end = toks[after] && toks[after].t === 'centavos' ? after + 1 : after;
          if (!currency && curOf(toks[end])) { currency = curOf(toks[end]); end++; }
        }
      }
      // puntuación
      let score = 0;
      if (currency) score += 3;
      if (!fromWords) score += 1;
      else if (value > 12) score += 1;
      for (let b = curStart - 1; b >= Math.max(0, curStart - 3); b--) {
        if (toks[b].kind === 'punct' || toks[b].kind === 'num') break;
        if (VERBS.has(toks[b].t)) { score += 2; break; }
      }
      const prev = toks[curStart - 1];
      if (prev && (prev.t === 'de' || prev.t === 'por' || prev.t === 'a')) score += 1;
      if (fromWords && value <= 12 && !currency) score -= 1;
      // "2 cafés": número pequeño seguido de un sustantivo = cantidad, no importe
      const nx = toks[Math.max(end, j)];
      if (!currency && value <= 12 && Number.isInteger(value) && nx && nx.kind === 'word' && !QTY_OK.has(nx.t)) score -= 2;
      out.push({ value, currency, fromWords, score, i0: curStart, i1: Math.max(end, j) - 1, numIdx: i });
      i = Math.max(end, j) - 1;
    }
    return out;
  }

  // ---------- diccionarios ----------
  // peso 3 = lugar / señal fuerte; 1 = artículo
  const CATS = [
    { id: 'comida', kw: { 3: 'supermercado super súper mercado feria tienda almacen almacén despensa verduleria verdulería carniceria carnicería panaderia panadería fruteria frutería minimarket abarrotes compra del mes compras de comida comida para casa ketal hipermaxi fidalga', 1: 'comida verduras verdura fruta frutas pan leche huevos huevo carne pollo queso arroz aceite fideos azucar azúcar harina yogur cereal papas papa tomates tomate cebolla platano plátano manzanas palta paltas mantequilla jamon jamón agua botellon botellón garrafon garrafón cafe molido galletas snacks lentejas frijoles pescado verdurita abarrote' } },
    { id: 'resto', kw: { 3: 'restaurante restaurant resto cafeteria cafetería bar pizzeria pizzería heladeria heladería pub salimos a comer comer fuera comimos fuera cenamos fuera almorzamos almuerzo cena cenar almorzar menu menú brunch pasteleria pastelería', 1: 'cafe café cafecito capuchino cappuccino latte te té pizza helado helados cerveza cervezas chela vino copa salteña salteñas saltena saltenas empanada empanadas hamburguesa sandwich sándwich tortas torta pastel postre desayuno jugo jugos comida rapida comida rápida propina pique cunape cuñapé' } },
    { id: 'transporte', kw: { 3: 'taxi taxis trufi micro bus buses colectivo flota mototaxi uber gasolina combustible diesel diésel peaje transfer traslado pasaje pasajes parqueo transporte surtidor nafta', 1: 'moto viaje a santa cruz a santa cruz de santa cruz aeropuerto viru viru' } },
    { id: 'personal', kw: { 3: 'limpieza señora de la limpieza senora de la limpieza limpiadora empleada empleada domestica nana niñera ninera cuidadora canguro jardinero jardinera chica que cuida cuida a noah cuidar a noah cuidado de noah sueldo planchado lavado de ropa lavandera', 1: 'ayuda' } },
    { id: 'casa', kw: { 4: 'productos de limpieza articulos de limpieza', 3: 'alquiler renta luz electricidad agua potable factura del agua gas garrafa internet wifi telefono teléfono recarga credito para el celular crédito para el celular ferreteria ferretería reparacion reparación arreglo plomero electricista cerrajero muebles mueble lavanderia lavandería tigo entel viva', 1: 'casa hogar bombilla foco detergente lavandina escoba papel higienico papel higiénico' } },
    { id: 'salud', kw: { 3: 'farmacia medico médico doctor doctora medicina medicinas medicamento medicamentos dentista hospital clinica clínica analisis análisis laboratorio consulta pediatra seguro medico seguro médico vacuna vacunas psicologo psicólogo fisioterapia', 1: 'pastillas ibuprofeno paracetamol vitaminas' } },
    { id: 'ocio', kw: { 3: 'ocio excursion excursión tour cine fiesta piscina museo paseo cabalgata entrada entradas concierto amboro amboró el fuerte cuevas pajcha la pajcha vinedo viñedo bodega yoga clase de clases de actividad hotel hostal cabaña cabana alojamiento vacaciones', 1: 'juego juegos libro libros regalo' } },
    { id: 'noah', kw: { 3: 'pañales panales toallitas guarderia guardería juguete juguetes kinder colegio escuela', 1: 'biberon biberón chupete papilla leche de formula leche de fórmula ropa de noah' } },
    { id: 'compras', kw: { 3: 'ropa zapatos zapatillas peluqueria peluquería corte de pelo barberia barbería cosmetica cosmética maquillaje electronica electrónica cargador celular', 1: 'camiseta pantalon pantalón vestido chaqueta mochila bolso perfume crema champu champú' } },
    { id: 'otros', kw: { 3: 'comision comisión impuesto multa tramite trámite donacion donación', 1: '' } }
  ];

  // Construye índice de frases: [{phrase:'senora de la limpieza', w:3, cat:'personal'}]
  // Las frases multi-palabra se escriben separadas por espacios en el diccionario,
  // así que usamos una lista explícita de multi-palabras.
  const MULTI = [
    'compra del mes', 'compras de comida', 'comida para casa', 'cafe molido', 'salimos a comer', 'comer fuera',
    'comimos fuera', 'cenamos fuera', 'comida rapida', 'viaje a santa cruz', 'a santa cruz', 'de santa cruz',
    'viru viru', 'senora de la limpieza', 'empleada domestica', 'chica que cuida', 'cuida a noah', 'cuidar a noah',
    'cuidado de noah', 'lavado de ropa', 'productos de limpieza', 'articulos de limpieza', 'agua potable',
    'factura del agua', 'credito para el celular', 'papel higienico', 'seguro medico', 'el fuerte', 'la pajcha',
    'clase de', 'clases de', 'leche de formula', 'ropa de noah', 'corte de pelo', 'mercado campesino'
  ];

  function buildIndex(extra) {
    const idx = [];
    for (const c of CATS) {
      for (const w of Object.keys(c.kw)) {
        let s = strip(c.kw[w]);
        // extrae multi-palabras conocidas primero
        for (const mp of MULTI) {
          const re = new RegExp('(^|\\s)' + mp.replace(/ /g, '\\s') + '(?=\\s|$)', 'g');
          if (re.test(s)) { idx.push({ p: mp, w: +w, cat: c.id }); s = s.replace(re, ' '); }
        }
        s.split(/\s+/).filter(Boolean).forEach(p => idx.push({ p, w: +w, cat: c.id }));
      }
    }
    // palabras clave del usuario (peso alto, ganan a las de serie)
    (extra || []).forEach(e => {
      (e.keywords || []).forEach(k => {
        const p = strip(String(k)).trim().replace(/\s+/g, ' ');
        if (p) idx.push({ p, w: 5, cat: e.id });
      });
      if (e.name) {
        const p = strip(String(e.name)).trim().replace(/\s+/g, ' ');
        if (p && p.length > 2) idx.push({ p, w: 3, cat: e.id });
      }
    });
    // dedupe & compila
    const seen = new Set();
    return idx.filter(e => { const k = e.p + '|' + e.cat + '|' + e.w; if (seen.has(k)) return false; seen.add(k); return true; })
      .map(e => ({ ...e, re: new RegExp('(?:^|[^a-z0-9])' + escapeRe(e.p).replace(/ /g, '\\s+') + '(?:s|es)?(?=$|[^a-z0-9])') }));
  }
  function escapeRe(s) { return s.replace(/[.*+?^${}()|[\]\\]/g, '\\$&'); }

  let cachedIdx = null, cachedKey = '';
  function getIndex(extra) {
    const key = JSON.stringify(extra || []);
    if (!cachedIdx || key !== cachedKey) { cachedIdx = buildIndex(extra); cachedKey = key; }
    return cachedIdx;
  }

  function categorize(norm, extra) {
    const idx = getIndex(extra);
    const score = {};
    for (const e of idx) {
      if (e.re.test(norm)) score[e.cat] = (score[e.cat] || 0) + e.w + e.p.length / 100;
    }
    let best = null, bv = 0;
    for (const k in score) if (score[k] > bv) { bv = score[k]; best = k; }
    return best;
  }

  // ---------- quién ----------
  function detectWho(norm, me) {
    const has = re => re.test(norm);
    const noah = has(/\b(noah|noa|noe|el nene|el bebe|el peque|el nino|nuestro hijo|mi hijo)\b/);
    const fam = has(/\b(toda la familia|la familia|familia|familiar|todos|los tres)\b/);
    const pareja = has(/\b(los dos|nosotros dos|ambos|la pareja|pareja)\b/);
    let sol = has(/\bsol\b/);
    let pol = has(/\b(pol|paul|pool)\b/);
    const forMe = has(/\b(para mi|para mi mismo|para mi misma|mio|mia)\b/);
    const forYou = has(/\b(para ti|para vos)\b/);
    if (forMe && me) { if (me === 'Sol') sol = true; else if (me === 'Pol') pol = true; }
    if (forYou && me) { if (me === 'Sol') pol = true; else if (me === 'Pol') sol = true; }
    if (fam) return 'familia';
    if (noah && (sol || pol || pareja)) return 'familia';
    if (pareja || (sol && pol)) return 'pareja';
    if (noah) return 'noah';
    if (sol) return 'sol';
    if (pol) return 'pol';
    return null;
  }

  // ---------- fecha ----------
  const DAYS = ['domingo', 'lunes', 'martes', 'miercoles', 'jueves', 'viernes', 'sabado'];
  const DATE_RE = /\b(hoy|ayer|anteayer|antier|antes de ayer|anoche|esta manana|esta tarde|esta noche|(?:el )?(?:lunes|martes|miercoles|jueves|viernes|sabado|domingo)(?: pasado)?|hace (?:\d+|un|una|dos|tres|cuatro|cinco|seis|siete) dias?)\b/g;
  function detectDate(norm, now) {
    const d = new Date(now.getFullYear(), now.getMonth(), now.getDate());
    let m;
    const re = new RegExp(DATE_RE.source, 'g');
    let off = null;
    while ((m = re.exec(norm))) {
      const s = m[1];
      if (s === 'hoy' || s.startsWith('esta ')) off = 0;
      else if (s === 'ayer' || s === 'anoche') off = 1;
      else if (s === 'anteayer' || s === 'antier' || s === 'antes de ayer') off = 2;
      else if (s.startsWith('hace')) {
        const n = s.split(' ')[1];
        off = /\d/.test(n) ? parseInt(n, 10) : (UNITS[n] || 1);
      } else {
        const dn = DAYS.findIndex(x => s.includes(x));
        if (dn >= 0) { off = (d.getDay() - dn + 7) % 7; if (s.includes('pasado') && off === 0) off = 7; }
      }
    }
    if (off === null) return null;
    d.setDate(d.getDate() - Math.min(off, 60));
    return d;
  }

  // ---------- tipo ----------
  const CASHIN_RE = /\b(cajero|atm|saque|sacamos|sacado|sacar|saco|retire|retiramos|retirado|retirar|recibi|recibimos|cobre|cobramos|me pagaron|nos pagaron|me devolvieron|nos devolvieron|me dieron|nos dieron|casa de cambio|ingreso|ingrese|ingresamos)\b/;
  const EXCHANGE_RE = /\b(cambie|cambiamos|cambiado|cambiar|cambio|casa de cambio)\b/;
  const FOREIGN_RE = /\b(dolares|dolar|usd|euros|euro|eur|verdes)\b|[$€]/;
  const NOT_CASHIN_RE = /\b(pague|pagamos|gaste|gastamos|compre|compramos|comision|propina)\b/;

  function detectType(norm) {
    const exch = EXCHANGE_RE.test(norm) && (FOREIGN_RE.test(norm) || /casa de cambio/.test(norm));
    if (exch) return { type: 'cash_in', source: 'exchange' };
    if (CASHIN_RE.test(norm) && !NOT_CASHIN_RE.test(norm)) {
      const atm = /\b(cajero|atm|saque|sacamos|sacado|sacar|saco|retire|retiramos|retirado|retirar)\b/.test(norm);
      return { type: 'cash_in', source: atm ? 'atm' : 'other' };
    }
    return { type: 'expense', source: null };
  }

  const METHOD_RE = /\b(?:(?:con|por|en|via|mediante|pague con|pagamos con) )?(qr|tarjeta|transferencia|debito|credito|tigo money)\b/;

  // ---------- descripción ----------
  const FILLER = [
    /\b(?:me |nos |te )?(?:acabo|acabamos|acabas|acaba) de (?:gastar|pagar|comprar|sacar|retirar|cambiar|recibir|cobrar)\b/g,
    /\b(?:he|hemos|ha|has|han) (?:gastado|pagado|comprado|sacado|retirado|cambiado|recibido|cobrado)\b/g,
    /\b(?:me|nos|le|les) (?:costo|costaron|cobraron|cobro|salio|salieron|dieron|dio)\b/g,
    /\b(?:y )?(?:el |un )?gasto (?:es|fue|ha sido|seria|sería)\b/g,
    /\b(?:anota|anotar|apunta|apuntar|registra|registrar|anade|agrega|pon)(?:me)?\b/g,
    /\b(?:gaste|gastamos|gastaste|pague|pagamos|pagaste|compre|compramos|saque|sacamos|retire|retiramos|cambie|cambiamos|recibi|recibimos|cobre|cobramos|son|fueron|costo|cuesta|total|en total|vale|valio)\b/g,
    /\b(?:centavos)\b/g,
    /\b(?:en efectivo|efectivo|en cash|cash)\b/g
  ];
  const WHO_PHRASE = /\b(?:y )?(?:el gasto )?(?:es |fue |va )?(?:para|de|a|por) (?:mi mismo|mi misma|mi|ti|vos|noah|noa|noe|sol|pol|paul|pool|los dos|nosotros dos|nosotros|ambos|la pareja|toda la familia|la familia|familia|todos|los tres|el nene|el bebe|el peque|nuestro hijo|mi hijo)(?: y (?:para )?(?:mi|ti|noah|noa|sol|pol|paul|el nene|el bebe))*\b/g;
  const LEAD = new Set('en de del por un una unos unas el la los las a al y e que para lo le mi con o es ha'.split(' '));
  const TRAIL = new Set('y e de en por para el la los las con que a al del o un una'.split(' '));

  function describe(orig, norm, toks, dropSpans) {
    const mask = new Array(orig.length).fill(false);
    const markSpan = (a, b) => { for (let i = a; i < b && i < mask.length; i++) mask[i] = true; };
    dropSpans.forEach(([a, b]) => markSpan(a, b));
    const res = [...FILLER, WHO_PHRASE, new RegExp(METHOD_RE.source, 'g'), new RegExp(DATE_RE.source, 'g'),
      /\b(?:del|en el|de un|en un|el|un) cajero(?: automatico)?\b/g, /\bcajero\b/g];
    for (const re of res) {
      let m; re.lastIndex = 0;
      while ((m = re.exec(norm))) { markSpan(m.index, m.index + m[0].length); if (!m[0].length) re.lastIndex++; }
    }
    let s = '';
    for (let i = 0; i < orig.length; i++) s += mask[i] ? ' ' : orig[i];
    s = s.replace(/[¿¡!?;:"“”«»]/g, ' ').replace(/\s*[,.]\s*/g, ' ').replace(/\s+/g, ' ').trim();
    let words = s.split(' ').filter(Boolean);
    const n = w => strip(w);
    while (words.length && LEAD.has(n(words[0]))) words.shift();
    while (words.length && TRAIL.has(n(words[words.length - 1]))) words.pop();
    s = words.join(' ');
    if (s.length > 48) s = s.slice(0, 48).replace(/\s+\S*$/, '') + '…';
    return s ? s.charAt(0).toUpperCase() + s.slice(1) : '';
  }

  // ---------- separar en cláusulas ----------
  const SPLIT_WORDS = new Set(['y', 'e', 'tambien', 'ademas', 'luego', 'despues']);

  function round2(v) { return Math.round(v * 100) / 100; }

  function parseUtterance(text, ctx) {
    ctx = ctx || {};
    const now = ctx.now || new Date();
    const me = ctx.me || null;
    const rates = Object.assign({ USD: 6.96, EUR: 7.5 }, ctx.rates || {});
    const extra = ctx.extraCats || [];
    const orig = String(text || '').replace(/\s+/g, ' ').trim();
    if (!orig) return { items: [], text: '' };
    const norm = strip(orig);
    const toks = tokenize(norm);

    // tokens dentro de expresiones de fecha no son importes
    const dropMask = new Array(toks.length).fill(false);
    const dateRe = new RegExp(DATE_RE.source, 'g');
    let m;
    while ((m = dateRe.exec(norm))) {
      toks.forEach((t, i) => { if (t.start >= m.index && t.end <= m.index + m[0].length) dropMask[i] = true; });
    }
    // "cada uno", "x2" etc. no se tratan
    const amounts = findAmounts(toks, dropMask);

    const globalType = detectType(norm);
    const globalDate = detectDate(norm, now);
    const globalWho = detectWho(norm, me);

    // cláusulas
    let clauses = [];
    const noSplit = globalType.source === 'exchange';
    let cur = { a: 0, b: 0 };
    for (let i = 0; i < toks.length; i++) {
      const t = toks[i];
      const isSep = !noSplit && ((t.kind === 'punct' && /[,;.!?]/.test(t.t)) || (t.kind === 'word' && SPLIT_WORDS.has(t.t)));
      // no cortar dentro de un importe
      const inAmount = amounts.some(a => i > a.i0 && i <= a.i1);
      if (isSep && !inAmount) { cur.b = i; clauses.push(cur); cur = { a: i + 1, b: i + 1 }; }
    }
    cur.b = toks.length; clauses.push(cur);
    clauses = clauses.filter(c => c.b > c.a);

    const qualified = a => a.score >= 1;
    const anyQualified = amounts.some(qualified);
    const amountsIn = c => amounts.filter(a => a.i0 >= c.a && a.i1 < c.b && (anyQualified ? qualified(a) : true));

    // fusiona cláusulas sin importe con la anterior (o la siguiente)
    const merged = [];
    let pending = null;
    for (const c of clauses) {
      const has = amountsIn(c).length > 0;
      if (has) {
        const nc = { a: pending ? pending.a : c.a, b: c.b };
        pending = null; merged.push(nc);
      } else if (merged.length) {
        merged[merged.length - 1].b = c.b;
      } else {
        pending = pending ? { a: pending.a, b: c.b } : { a: c.a, b: c.b };
      }
    }
    if (!merged.length) merged.push({ a: 0, b: toks.length });
    else if (pending) merged[merged.length - 1].b = pending.b;

    const spanOf = c => {
      const s = toks[c.a] ? toks[c.a].start : 0;
      const e = toks[c.b - 1] ? toks[c.b - 1].end : norm.length;
      return [s, e];
    };

    const whoMentions = merged.map(c => { const [s, e] = spanOf(c); return detectWho(norm.slice(s, e), me); });
    const distinctWho = new Set(whoMentions.filter(Boolean));

    const items = merged.map((c, ci) => {
      const [s, e] = spanOf(c);
      const cNorm = norm.slice(s, e);
      const cOrig = orig.slice(s, e);
      const cAm = amountsIn(c);
      const tinfo = noSplit ? globalType : detectType(cNorm);
      let pick = null;
      let amountBs = null, currency = 'BOB', amount = null, estimated = false, rateUsed = null;

      if (tinfo.source === 'exchange') {
        const foreign = cAm.filter(a => a.currency === 'USD' || a.currency === 'EUR');
        const bob = cAm.filter(a => a.currency === 'BOB');
        const plain = cAm.filter(a => !a.currency);
        const f = foreign[0] || null;
        if (f) {
          currency = f.currency; amount = f.value;
          const b = bob.find(x => x.value >= f.value) || bob[0];
          const r = plain.find(x => x.value > 1 && x.value < 30 && !Number.isInteger(x.value)) ||
                    plain.find(x => x.value > 1 && x.value < 30);
          const big = plain.find(x => x.value > f.value);
          if (b) amountBs = b.value;
          else if (r) { amountBs = round2(f.value * r.value); rateUsed = r.value; }
          else if (big) amountBs = big.value;
          else { amountBs = round2(f.value * rates[f.currency]); estimated = true; rateUsed = rates[f.currency]; }
          pick = f;
        } else {
          const b = bob[0] || plain.sort((x, y) => y.value - x.value)[0];
          if (b) { amountBs = b.value; amount = b.value; pick = b; }
        }
      } else {
        const sorted = cAm.slice().sort((x, y) => (y.score - x.score) || (y.value - x.value));
        pick = sorted[0] || null;
        if (pick) {
          amount = pick.value; currency = pick.currency || 'BOB';
          if (currency !== 'BOB') { amountBs = round2(amount * rates[currency]); estimated = true; rateUsed = rates[currency]; }
          else amountBs = amount;
        }
      }

      // spans a quitar para la descripción: todos los importes/monedas de la cláusula
      const drop = amounts.filter(a => a.i0 >= c.a && a.i1 < c.b).map(a => [toks[a.i0].start, toks[a.i1].end]);
      const methodM = METHOD_RE.exec(cNorm);
      const method = methodM ? 'qr' : 'cash';
      let who = whoMentions[ci] || (distinctWho.size === 1 ? [...distinctWho][0] : null);
      let category = null, guessed = false;
      if (tinfo.type === 'expense') {
        category = categorize(cNorm, extra) || (merged.length === 1 ? null : categorize(norm, extra));
        if (!category && who === 'noah') category = 'noah';
        if (!category) { category = 'otros'; guessed = true; }
        if (!who) who = category === 'noah' ? 'noah' : 'familia';
      }
      let note = describe(cOrig, cNorm, toks, drop.map(([a, b]) => [a - s, b - s]));
      if (tinfo.type === 'cash_in') {
        if (tinfo.source === 'atm') note = note && !/^(dinero|plata|efectivo|bs)$/i.test(note) && !/cajero/i.test(note) ? note : 'Cajero';
        if (tinfo.source === 'exchange') note = amount && currency !== 'BOB' ? 'Cambio de ' + fmtPlain(amount) + ' ' + currency : 'Cambio de divisas';
        if (tinfo.source === 'other' && !note) note = 'Entrada de efectivo';
        if (/^(dinero|plata|efectivo)( del)?$/i.test(note)) note = tinfo.source === 'atm' ? 'Cajero' : 'Entrada de efectivo';
      }
      const date = globalDate || new Date(now.getFullYear(), now.getMonth(), now.getDate());
      return {
        type: tinfo.type,
        source: tinfo.type === 'cash_in' ? tinfo.source : null,
        amount, currency, amountBs: amountBs === null ? null : round2(amountBs), rateUsed,
        category: tinfo.type === 'expense' ? category : null,
        who: tinfo.type === 'expense' ? who : null,
        method: tinfo.type === 'expense' ? method : 'cash',
        date: ymd(date),
        note,
        flags: { noAmount: amountBs === null, estimated, categoryGuessed: guessed }
      };
    });
    return { items, text: orig };
  }

  function fmtPlain(v) { return Number.isInteger(v) ? String(v) : v.toFixed(2).replace('.', ','); }
  function ymd(d) {
    const p = n => String(n).padStart(2, '0');
    return d.getFullYear() + '-' + p(d.getMonth() + 1) + '-' + p(d.getDate());
  }

  const api = { parseUtterance, strip, digitValue, categorize, detectWho, ymd, CATEGORY_IDS: CATS.map(c => c.id) };
  if (typeof module !== 'undefined' && module.exports) module.exports = api;
  root.BolsilloParser = api;
})(typeof globalThis !== 'undefined' ? globalThis : this);
