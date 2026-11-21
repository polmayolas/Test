/* Qullqi — intérprete de gastos dictados en español (sin IA, coste cero).
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
      // "1.500" = mil quinientos; "0,500" = medio
      if (parts.length > 2 || (parts.length === 2 && parts[1].length === 3 && !/^0+$/.test(parts[0]))) s = parts.join('');
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
  const QTY_OK = new Set(['y', 'e', 'en', 'para', 'de', 'del', 'por', 'con', 'el', 'la', 'al', 'a', 'que', 'mas', 'menos', 'hoy', 'ayer', 'o', 'centavos', 'mil', 'medio', 'media', 'tambien', 'ademas', 'luego', 'despues', 'no', 'perdon', 'digo', 'cada']);

  const UNIT_NOUNS = new Set('ano anos dia dias mes meses semana semanas persona personas hora horas minuto minutos kilo kilos kg litro litros unidad unidades paquete paquetes vez veces docena metro metros'.split(' '));
  const STOPW = new Set('y e en de del por un una unos unas el la los las a al que para lo le les me nos te se mi con o es ha mas menos tambien ademas luego despues no si ya hoy ayer anoche cada'.split(' '));
  const isContent = w => !STOPW.has(w) && !VERBS.has(w) && !(w in UNITS) && !CUR[w] && w !== 'mil' && w.length > 1;
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
        // "doce cincuenta", "nueve ochenta" -> 12,50 / 9,80
        if (!dropMask[k] && value >= 1 && value <= 30 && toks[k] && toks[k].kind === 'word' && isTens(UNITS[toks[k].t] || 0) &&
            !(toks[k + 1] && (toks[k + 1].t === 'mil' || toks[k + 1].t === 'y'))) { value += UNITS[toks[k].t] / 100; j = k + 1; }
      } else continue;
      if (value === null || !(value > 0)) { i = Math.max(i, j - 1); continue; }

      // "y medio"
      if (toks[j] && toks[j].t === 'y' && toks[j + 1] && (toks[j + 1].t === 'medio' || toks[j + 1].t === 'media')) {
        value += 0.5; j += 2;
      }
      // moneda después ("25 bs", "25 bs.", "25 de bolivianos")
      let currency = null, curEnd = j, symIdx = -1;
      if (curOf(toks[j])) {
        currency = curOf(toks[j]); if (toks[j].t === '$') symIdx = j; curEnd = j + 1;
        if (toks[curEnd] && toks[curEnd].t === '.') curEnd++;
        // "20 bolivianos 50 centavos"
        const cn = toks[curEnd], cc = toks[curEnd + 1];
        if (cn && cn.kind === 'num' && cc && cc.t === 'centavos') { const cv = digitValue(cn.t); if (cv > 0 && cv < 100 && Number.isInteger(value)) { value = value + cv / 100; curEnd += 2; } }
      }
      // moneda antes ("bs 25", "$20", "usd 20")
      let curStart = i;
      if (!currency && i > 0 && curOf(toks[i - 1]) && !dropMask[i - 1]) { currency = curOf(toks[i - 1]); curStart = i - 1; if (toks[i - 1].t === '$') symIdx = i - 1; }
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
      // número justo después de un concepto ("pan 8", "taxi 10 hasta...") = precio
      const prevContent = prev && prev.kind === 'word' && isContent(prev.t);
      if (fromWords && value <= 12 && !currency && !prevContent) score -= 1;
      // "2 cafés", "3 kilos": número pequeño seguido de un sustantivo = cantidad, no importe
      const nx = toks[Math.max(end, j)];
      if (!currency && value <= 12 && Number.isInteger(value) && nx && nx.kind === 'word' && !QTY_OK.has(nx.t)) {
        if (UNIT_NOUNS.has(nx.t) || (!prevContent && value >= 2 && /s$/.test(nx.t))) score = Math.min(score - 2, -1);
        else if (!prevContent) score -= 2;
      }
      out.push({ value, currency, fromWords, score, i0: curStart, i1: Math.max(end, j) - 1, numIdx: i, symIdx });
      i = Math.max(end, j) - 1;
    }
    return out;
  }

  // ---------- diccionarios ----------
  // peso 3 = lugar / señal fuerte; 1 = artículo
  const CATS = [
    { id: 'comida', kw: { 3: 'supermercado super súper mercado feria tienda almacen almacén despensa verduleria verdulería carniceria carnicería panaderia panadería fruteria frutería minimarket abarrotes compra del mes compras de comida comida para casa ketal hipermaxi fidalga', 1: 'kilo kilos naranja naranjas mandarina mandarinas yuca choclo botellon de agua agua mineral comida verduras verdura fruta frutas pan leche huevos huevo carne pollo queso arroz aceite fideos azucar azúcar harina yogur cereal papas papa tomates tomate cebolla platano plátano manzanas palta paltas mantequilla jamon jamón botellon botellón garrafon garrafón cafe molido galletas snacks lentejas frijoles pescado verdurita abarrote' } },
    { id: 'resto', kw: { 4: 'cafe con leche café con leche', 3: 'api sonso zonzo tucumana tucumanas silpancho majadito anticucho anticuchos salteña salteñas saltena saltenas pique desayuno desayunamos comimos cenamos almorzamos para llevar restaurante restaurant resto cafeteria cafetería bar pizzeria pizzería heladeria heladería pub salimos a comer comer fuera comimos fuera cenamos fuera almorzamos almuerzo cena cenar almorzar menu menú brunch pasteleria pastelería pension pensión comedor', 1: 'mani maní chicharron chicharrón api sonso zonzo tucumana tucumanas silpancho majadito saice sopa anticucho anticuchos broaster pollo frito mocochinchi chicha refresco masaco cafe café cafecito capuchino cappuccino latte te té pizza helado helados cerveza cervezas chela vino copa salteña salteñas saltena saltenas empanada empanadas hamburguesa sandwich sándwich tortas torta pastel postre desayuno jugo jugos comida rapida comida rápida propina pique cunape cuñapé' } },
    { id: 'transporte', kw: { 3: 'taxi taxis trufi micro bus buses colectivo flota mototaxi uber gasolina combustible diesel diésel peaje transfer traslado pasaje pasajes parqueo transporte surtidor nafta taxista mecanico mecánico taller llanta llantas gomeria gomería surubi vagoneta', 4: 'cambio de aceite aceite del auto', 1: 'auto coche camioneta moto viaje a santa cruz a santa cruz de santa cruz aeropuerto viru viru' } },
    { id: 'personal', kw: { 3: 'limpieza señora de la limpieza senora de la limpieza limpiadora empleada empleada domestica nana niñera ninera cuidadora canguro jardinero jardinera chica que cuida cuida a noah cuidar a noah cuidado de noah sueldo planchado lavado de ropa lavandera que limpia nos limpia', 1: 'ayuda' } },
    { id: 'casa', kw: { 4: 'productos de limpieza articulos de limpieza', 3: 'alquiler renta luz electricidad agua potable factura del agua gas garrafa internet wifi telefono teléfono agua cre cotas cooperativa de agua saguapac megas recarga credito para el celular crédito para el celular ferreteria ferretería reparacion reparación arreglo plomero electricista cerrajero muebles mueble lavanderia lavandería tigo entel viva', 1: 'pintura cemento clavos casa hogar bombilla foco detergente lavandina escoba papel higienico papel higiénico' } },
    { id: 'salud', kw: { 3: 'farmacia medico médico doctor doctora medicina medicinas medicamento medicamentos dentista hospital clinica clínica analisis análisis laboratorio consulta pediatra medica médica posta centro de salud seguro medico seguro médico vacuna vacunas psicologo psicólogo fisioterapia', 1: 'jarabe antibiotico antibiótico curitas termometro termómetro pastillas ibuprofeno paracetamol vitaminas' } },
    { id: 'ocio', kw: { 3: 'ocio excursion excursión tour cine fiesta piscina museo paseo cabalgata entrada entradas concierto amboro amboró el fuerte cuevas pajcha la pajcha vinedo viñedo bodega yoga clase de clases de actividad hotel hostal cabaña cabana alojamiento vacaciones', 1: 'juego juegos libro libros' } },
    { id: 'noah', kw: { 3: 'pañales panales toallitas guarderia guardería juguete juguetes kinder colegio escuela', 1: 'biberon biberón chupete papilla leche de formula leche de fórmula ropa de noah' } },
    { id: 'compras', kw: { 3: 'ropa zapatos zapatillas peluqueria peluquería corte de pelo barberia barbería cosmetica cosmética maquillaje electronica electrónica cargador regalo regalos', 1: 'gafas lentes anteojos bicicleta tela celular camisa camisas polera poleras chamarra sandalias chinelas bloqueador protector solar camiseta pantalon pantalón vestido chaqueta mochila bolso perfume crema champu champú' } },
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
    'clase de', 'clases de', 'cafe con leche', 'para llevar', 'cambio de aceite', 'aceite del auto', 'botellon de agua', 'agua mineral', 'pollo frito', 'cooperativa de agua', 'centro de salud', 'que limpia', 'nos limpia', 'protector solar', 'leche de formula', 'ropa de noah', 'corte de pelo', 'mercado campesino'
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
  const WHO_SUBJ_RE = /\b(sol|pol|paul|pool|noah|noa) (?:pago|pagaron|compro|gasto|dice|dijo|fue|trajo|pidio|saco|cambio|tiene|quiere|me dijo|nos dijo|ya)\b/g;
  const WHO_WITH_RE = /\bcon (?:sol|pol|paul|pool)\b/g;
  const SUN_RE = /\b(?:gafas|lentes|anteojos|crema|protector|bloqueador|sombrero|gorra) (?:de|del|para el|contra el) sol\b/g;
  function detectWho(norm, me) {
    norm = norm.replace(WHO_SUBJ_RE, ' ').replace(WHO_WITH_RE, ' ').replace(SUN_RE, ' ');
    const has = re => re.test(norm);
    const noah = has(/\b(noah|noa|noe|(?:el|del|al) (?:nene|bebe|peque|nino)|nuestro hijo|mi hijo)\b/);
    const fam = has(/\b(toda la familia|la familia|familia|familiar|todos|los tres)\b/);
    const pareja = has(/\b(los dos|nosotros dos|ambos|la pareja|pareja)\b/);
    // "sol" es Sol salvo "el/del/al sol" (bloqueador para el sol)
    let sol = false;
    { const re = /(^|[^a-z])(el |del |al )?sol\b/g; let m; while ((m = re.exec(norm))) { if (!m[2]) { sol = true; break; } } }
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
  const MONTHS = ['enero', 'febrero', 'marzo', 'abril', 'mayo', 'junio', 'julio', 'agosto', 'septiembre', 'setiembre', 'octubre', 'noviembre', 'diciembre'];
  const MONTH_RE = '(?:' + MONTHS.join('|') + ')';
  const DATE_RE = /\b(\d{1,2}\/\d{1,2}(?:\/\d{2,4})?|(?:el )?(?:dia )?\d{1,2} de (?:enero|febrero|marzo|abril|mayo|junio|julio|agosto|septiembre|setiembre|octubre|noviembre|diciembre)|el dia \d{1,2}|hoy|ayer|anteayer|antier|antes de ayer|anoche|esta manana|esta tarde|esta noche|(?:el )?(?:lunes|martes|miercoles|jueves|viernes|sabado|domingo)(?: pasado)?|hace (?:\d+|un|una|dos|tres|cuatro|cinco|seis|siete) (?:dias?|semanas?))\b/g;
  function detectDate(norm, now) {
    const d = new Date(now.getFullYear(), now.getMonth(), now.getDate());
    let m;
    const re = new RegExp(DATE_RE.source, 'g');
    let off = null;
    while ((m = re.exec(norm))) {
      const s = m[1];
      const before = norm.slice(Math.max(0, m.index - 12), m.index);
      if (/\b(?:calle|avenida|av|barrio|plaza|colegio|calle el|avenida el) $/.test(before) && /\d/.test(s)) continue;
      const abs = absDate(s, d);
      if (abs) { off = Math.round((d - abs) / 864e5); continue; }
      if (s === 'hoy' || s.startsWith('esta ')) off = 0;
      else if (s === 'ayer' || s === 'anoche') off = 1;
      else if (s === 'anteayer' || s === 'antier' || s === 'antes de ayer') off = 2;
      else if (s.startsWith('hace')) {
        const n = s.split(' ')[1];
        off = /\d/.test(n) ? parseInt(n, 10) : (UNITS[n] || 1);
        if (/semana/.test(s)) off *= 7;
      } else {
        const dn = DAYS.findIndex(x => s.includes(x));
        if (dn >= 0) { off = (d.getDay() - dn + 7) % 7; if (s.includes('pasado') && off === 0) off = 7; }
      }
    }
    if (off === null) return null;
    d.setDate(d.getDate() - Math.min(off, 400));
    return d;
  }

  // "15 de noviembre", "el día 18", "18/11": nunca en el futuro (se toma el mes o año anterior)
  function absDate(s, today) {
    let m = /^(\d{1,2})\/(\d{1,2})(?:\/(\d{2,4}))?$/.exec(s);
    let day, mon, year = null;
    if (m) { day = +m[1]; mon = +m[2] - 1; if (m[3]) year = m[3].length === 2 ? 2000 + +m[3] : +m[3]; }
    else if ((m = /(\d{1,2}) de ([a-z]+)$/.exec(s))) { day = +m[1]; mon = MONTHS.indexOf(m[2]); if (mon > 8) mon = m[2] === 'setiembre' ? 8 : mon - 1; }
    else if ((m = /^el dia (\d{1,2})$/.exec(s))) { day = +m[1]; mon = today.getMonth(); }
    else return null;
    if (!(day >= 1 && day <= 31) || !(mon >= 0 && mon <= 11)) return null;
    let y = year || today.getFullYear();
    let r = new Date(y, mon, day);
    if (r.getDate() !== day) return null;
    if (!year && r > today) {
      if (/^el dia/.test(s)) { r = new Date(y, mon - 1, day); if (r.getDate() !== day) return null; }
      else r = new Date(y - 1, mon, day);
    }
    if (r > today || today - r > 400 * 864e5) return null;
    return r;
  }

  // ---------- tipo ----------
  // Verbos de entrada que sólo cuentan si su objeto es dinero ("saqué 500", "saqué plata"),
  // no "sacamos entradas", "retiré el pedido", "recibí la factura", "nos dieron la cuenta".
  const NUMW = '(?:\\d|un |una |mil|cien|ciento|doscient|trescient|cuatrocient|quinient|seiscient|setecient|ochocient|novecient|dos|tres|cuatro|cinco|seis|siete|ocho|nueve|diez|once|doce|trece|catorce|quince|dieci|veint|treinta|cuarenta|cincuenta|sesenta|setenta|ochenta|noventa)';
  const MONEY_OBJ = '(?:(?:de |del |el |la |los |las |en |un |una |otros? |mas |ya )*(?:' + NUMW + '|plata|dinero|efectivo|bs|bolivianos|pesos|bob|cash|\\$|\\d))';
  const ATM_HARD = /\b(cajero|atm)\b/;
  const ATM_VERB_RE = new RegExp('\\b(?:saque|sacamos|sacado|sacar|saco|retire|retiramos|retirado|retirar|retiro) ' + MONEY_OBJ);
  const ATM_BANK_RE = /\b(?:saque|sacamos|retire|retiramos)\b.*\bdel banco\b/;
  const OTHER_OBJ_RE = new RegExp('\\b(?:recibi|recibimos|me dieron|nos dieron|me dio|nos dio|me pasaron|nos pasaron|me prestaron|nos prestaron|ingrese|ingresamos) ' + MONEY_OBJ);
  const OTHER_MORE_RE = new RegExp('\\b(?:(?:le|les|me|nos) (?:regalo|regalaron|dieron|dio|mando|mandaron|enviaron|envio)|(?:me|nos) (?:llego|llegaron|reembolsaron)|ingresos? de|gane|ganamos|premio de|encontre|encontramos) ' + MONEY_OBJ);
  const OTHER_ANY_RE = /\b(cobre|cobramos|me pagaron|nos pagaron|me devolvieron|nos devolvieron|me devolvio|nos devolvio|me regalaron|nos regalaron|devolucion|reembolso|reembolsaron|vendi|vendimos|casa de cambio)\b/;
  const EXCHANGE_RE = /\b(cambie|cambiamos|cambiado|cambiar|cambio|cambiaron|vendi|vendimos|casa de cambio)\b/;
  const FOREIGN_RE = /\b(dolares|dolar|usd|euros|euro|eur|verdes)\b|[$€]/;
  const NOT_CASHIN_RE = /\b(pague|pagamos|gaste|gastamos|compre|compramos|comision|propina)\b/;
  // "cambié 700 bolivianos a dólares": salen bolivianos -> gasto
  const BUY_FOREIGN_RE = /\b(?:bs|bolivianos|pesos)\b.*\b(?:a|por|en)\s+(?:dolares|euros)\b|\bcompr\w* (?:\d\S* )?(?:dolares|euros)\b/;

  function detectType(norm) {
    const foreign = FOREIGN_RE.test(norm);
    if ((EXCHANGE_RE.test(norm) && (foreign || /casa de cambio/.test(norm))) || /\b(?:me|nos) dieron\b.*\bpor\b.*\b(?:dolares|euros|usd)\b/.test(norm)) {
      if (BUY_FOREIGN_RE.test(norm)) return { type: 'expense', source: null };
      return { type: 'cash_in', source: 'exchange' };
    }
    if (NOT_CASHIN_RE.test(norm)) return { type: 'expense', source: null };
    if (ATM_HARD.test(norm) || ATM_VERB_RE.test(norm) || ATM_BANK_RE.test(norm)) return { type: 'cash_in', source: 'atm' };
    if (OTHER_OBJ_RE.test(norm) || OTHER_ANY_RE.test(norm) || OTHER_MORE_RE.test(norm)) return { type: 'cash_in', source: 'other' };
    return { type: 'expense', source: null };
  }

  const METHOD_RE = /\b(?:(?:con|por|en|via|mediante|pague con|pagamos con) )?(qr|q r|cu ?erre|codigo qr|tarjeta|transferencia|tigo money|(?:con|por|en) (?:debito|credito))\b/;

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
    // límite de longitud: un dictado real nunca pasa de unas frases; evita bloqueos con textos enormes
    const orig = String(text || '').replace(/\s+/g, ' ').trim().slice(0, 600);
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
    const NT = '(\\d{1,2}(?:[:.]\\d{2})?|una|uno|dos|tres|cuatro|cinco|seis|siete|ocho|nueve|diez|once|doce)';
    const NOT_AMOUNT = [
      /\b\d+(?:[.,]\d+)? ?(?:km|kms|kilometros?|metros?|mts?)\b/g,
      /\b\d{1,2}\/\d{1,2}(?:\/\d{2,4})?\b/g,
      new RegExp('\\b(?:a|de|desde|hasta|para) las? ' + NT + '(?: y (?:media|cuarto|\\d+))?\\b', 'g'),
      new RegExp('\\b' + NT + ' (?:de la (?:manana|tarde|noche)|en punto|horas?|hrs?|minutos?|mins?)\\b', 'g'),
      /\b(?:calle|avenida|av|km|kilometro|numero|nro|habitacion|talla|piso|linea|puesto|mesa para) \d+\b/g,
      new RegExp('\\bde ' + NT + ' a ' + NT + '(?=\\s+(?:\\d|horas|hrs))', 'g'),
      new RegExp('\\b\\d{1,2} de (?:' + MONTHS.join('|') + ')\\b', 'g')
    ];
    // calle/avenida con nombre: "calle Bolívar 25" -> ese número no es (casi seguro) un importe
    const demote = new Array(toks.length).fill(false);
    { const re = /\b(?:calle|avenida|av|barrio)\s+(?:[a-z]+\s+){1,3}?(?:n(?:umero|ro)?\s+)?(\d+)\b/g; let mm;
      while ((mm = re.exec(norm))) { const b = mm.index + mm[0].length; toks.forEach((t, i) => { if (t.kind === 'num' && t.end === b) demote[i] = true; }); } }
    // horas, calles, km, fechas: sus números no son importes
    for (const re of NOT_AMOUNT) {
      let mm; re.lastIndex = 0;
      while ((mm = re.exec(norm))) {
        const a = mm.index, b = a + mm[0].length;
        toks.forEach((t, i) => { if (t.kind === 'num' || t.kind === 'word') { if (t.start >= a && t.end <= b && (t.kind === 'num' || t.t in UNITS)) dropMask[i] = true; } });
      }
    }
    let amounts = findAmounts(toks, dropMask);
    amounts.forEach(a => { if (demote[a.numIdx] && !a.currency) a.score -= 3; });
    // (d) correcciones habladas: "25, no, 35" / "40, digo 45" -> vale el último
    const CORR_KEY = new Set(['no', 'perdon', 'digo', 'dicho', 'decir', 'sea', 'corrijo', 'error', 'rectifico']);
    const CORR_ALL = new Set([...CORR_KEY, 'mejor', 'quiero', 'o', 'es', 'son', 'eran', 'fueron', 'mas', 'bien', 'perdona', 'uy', 'ay', 'eh', 'este', 'que', 'en', 'realidad']);
    const corrSpans = [];
    amounts = amounts.filter((a, k) => {
      const b = amounts[k + 1]; if (!b) return true;
      const between = toks.slice(a.i1 + 1, b.i0);
      if (between.length && between.every(t => t.kind === 'punct' || CORR_ALL.has(t.t)) && between.some(t => CORR_KEY.has(t.t))) {
        corrSpans.push([toks[a.i0].start, toks[b.i0 - 1].end]); return false;
      }
      return true;
    });

    const globalType = detectType(norm);
    const globalDate = detectDate(norm, now);
    // "$" suelto: en Bolivia el dólar se escribe "$us"; el dictado puede convertir "pesos" en "$".
    // Fuera de un cambio de divisa se trata como bolivianos (la hoja de revisión lo muestra).
    amounts.forEach(a => {
      if (a.symIdx < 0) return;
      const nx = toks[a.symIdx + 1];
      a.currency = (nx && (nx.t === 'us' || nx.t === 'usd')) || globalType.source === 'exchange' ? 'USD' : 'BOB';
    });

    // cláusulas
    let clauses = [];
    const noSplit = false;
    const isExch = globalType.source === 'exchange';
    const EXP_VERB = /^(gaste|gastamos|pague|pagamos|compre|compramos)$/;
    const ATM_VERB_ONLY = /^(saque|sacamos|retire|retiramos|cajero)$/;
    let cur = { a: 0, b: 0 };
    for (let i = 0; i < toks.length; i++) {
      const t = toks[i];
      let isSep = (t.kind === 'punct' && /[,;.!?]/.test(t.t)) || (t.kind === 'word' && SPLIT_WORDS.has(t.t));
      if (isSep && isExch) {
        // en un cambio solo se corta ante otra operación: un gasto, un cajero u otro cambio
        const seg = toks.slice(cur.a, i).map(x => x.t);
        const hadExch = seg.some(w => /^(cambie|cambiamos|cambiaron|cambiado|cambiar)$/.test(w));
        isSep = [1, 2, 3].some(d => toks[i + d] && (EXP_VERB.test(toks[i + d].t) || ATM_VERB_ONLY.test(toks[i + d].t) ||
          (!hadExch && /^(cambie|cambiamos)$/.test(toks[i + d].t))));
      }
      // no cortar dentro de un importe
      const inAmount = amounts.some(a => i > a.i0 && i <= a.i1);
      if (isSep && !inAmount) { cur.b = i; clauses.push(cur); cur = { a: i + 1, b: i + 1 }; }
    }
    cur.b = toks.length; clauses.push(cur);
    clauses = clauses.filter(c => c.b > c.a);

    const qualified = a => a.score >= 1;
    const anyQualified = amounts.some(qualified);
    const amountsIn = c => amounts.filter(a => a.i0 >= c.a && a.i1 < c.b && (anyQualified ? qualified(a) : a.score >= 0));

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
    // "gasolina para 120 km, 100 bolivianos": una cláusula que solo trae un número (sin concepto)
    // completa la anterior en vez de crear un movimiento fantasma
    for (let k = merged.length - 1; k > 0; k--) {
      const c = merged[k];
      const words = toks.slice(c.a, c.b).filter(t => t.kind === 'word' && isContent(t.t) && !/^(qr|bs|pesos|bolivianos|centavos|efectivo|cash|tarjeta|eh|bueno|pues|vale)$/.test(t.t));
      const ownSignal = toks.slice(c.a, c.b).some(t => t.kind === 'word' && (VERBS.has(t.t) || /^(cambie|cambiamos|saque|sacamos|retire|retiramos)$/.test(t.t)));
      const cs = toks[c.a] ? toks[c.a].start : 0, ce = toks[c.b - 1] ? toks[c.b - 1].end : 0;
      if (!words.length && !ownSignal && !detectWho(norm.slice(cs, ce), me)) { merged[k - 1].b = c.b; merged.splice(k, 1); }
    }

    if (!isExch) {
      const out = [];
      for (const c of merged) {
        const q = amountsIn(c).sort((x, y) => x.i0 - y.i0);
        const conceptFirst = q.length && toks.slice(c.a, q[0].i0).some(t => t.kind === 'word');
        let start = c.a;
        for (let k = 0; k + 1 < q.length; k++) {
          const x = q[k], y = q[k + 1];
          const bt = toks.slice(x.i1 + 1, y.i0);
          if (!bt.length) continue;
          const btNorm = norm.slice(bt[0].start, bt[bt.length - 1].end);
          if (!categorize(btNorm, extra)) continue;
          const cut = conceptFirst ? x.i1 + 1 : y.i0;
          if (cut > start) { out.push({ a: start, b: cut }); start = cut; }
        }
        out.push({ a: start, b: c.b });
      }
      merged.length = 0; out.forEach(c => merged.push(c));
    }
    const spanOf = c => {
      const s = toks[c.a] ? toks[c.a].start : 0;
      const e = toks[c.b - 1] ? toks[c.b - 1].end : norm.length;
      return [s, e];
    };

    const whoMentions = merged.map(c => { const [s, e] = spanOf(c); return detectWho(norm.slice(s, e), me); });
    const looseWho = clauses.filter(c => !amountsIn(c).length).map(c => { const [s, e] = spanOf(c); return detectWho(norm.slice(s, e), me); });
    const distinctWho = new Set(looseWho.filter(Boolean));

    // fechas: si se nombra una sola, vale para todo; si hay varias, cada cláusula la suya (o la anterior)
    const clauseDates = merged.map(c => { const [s, e] = spanOf(c); return detectDate(norm.slice(s, e), now); });
    const distinctDates = new Set(clauseDates.filter(Boolean).map(ymd));
    let carryDate = null;
    const today0 = new Date(now.getFullYear(), now.getMonth(), now.getDate());

    const items = merged.slice(0, 20).map((c, ci) => {
      const [s, e] = spanOf(c);
      const cNorm = norm.slice(s, e);
      const cOrig = orig.slice(s, e);
      const cAm0 = amountsIn(c);
      const own = detectType(cNorm);
      const tinfo = !isExch || own.type === 'cash_in' || NOT_CASHIN_RE.test(cNorm) ? own : globalType;
      const cAm = tinfo.source === 'exchange' ? amounts.filter(a => a.i0 >= c.a && a.i1 < c.b) : cAm0;
      let pick = null;
      let amountBs = null, currency = 'BOB', amount = null, estimated = false, rateUsed = null;

      if (tinfo.source === 'exchange') {
        const foreign = cAm.filter(a => a.currency === 'USD' || a.currency === 'EUR');
        const bob = cAm.filter(a => a.currency === 'BOB');
        const plain = cAm.filter(a => !a.currency);
        let f = foreign[0] || null;
        // "cambiamos dólares, 200 a 9,70": moneda nombrada sin pegar al número
        if (!f && plain.length) {
          const fw = /\b(euros?|eur)\b|€/.test(cNorm) ? 'EUR' : 'USD';
          const big = plain.slice().sort((x, y) => y.value - x.value)[0];
          if (big.value >= 10) { f = Object.assign({}, big, { currency: fw }); plain.splice(plain.indexOf(big), 1); }
        }
        // "a 10 bolivianos" con 100 dólares = tasa, no total
        const rateBob = f && bob.find(x => x.value <= 30 && x.value < f.value);
        if (rateBob && !bob.some(x => x.value >= f.value)) { bob.splice(bob.indexOf(rateBob), 1); plain.push(rateBob); }
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
          const aft = toks[pick.i1 + 1], aft2 = toks[pick.i1 + 2];
          if (aft && aft.t === 'cada' && aft2 && /^(uno|una)$/.test(aft2.t)) {
            const q = amounts.find(a => a.i0 >= c.a && a.i1 < pick.i0 && a.score < 1 && Number.isInteger(a.value) && a.value >= 2 && a.value <= 50);
            if (q) amount = round2(q.value * pick.value);
          }
          if (currency !== 'BOB') { amountBs = round2(amount * rates[currency]); estimated = true; rateUsed = rates[currency]; }
          else amountBs = amount;
        }
      }

      // spans a quitar para la descripción: todos los importes/monedas de la cláusula
      const drop = amounts.filter(a => a.i0 >= c.a && a.i1 < c.b).map(a => [toks[a.i0].start, toks[a.i1].end]).concat(corrSpans.filter(([a, b]) => a >= s && b <= e));
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
      let date;
      if (distinctDates.size <= 1) date = globalDate || today0;
      else { if (clauseDates[ci]) carryDate = clauseDates[ci]; date = carryDate || today0; }
      if (amountBs !== null && amountBs > 1e8) amountBs = null;
      return {
        type: tinfo.type,
        source: tinfo.type === 'cash_in' ? tinfo.source : null,
        amount, currency, amountBs: amountBs === null ? null : round2(amountBs), rateUsed,
        category: tinfo.type === 'expense' ? category : null,
        who: tinfo.type === 'expense' ? who : null,
        method: tinfo.type === 'expense' ? method : 'cash',
        date: ymd(date),
        note,
        flags: { noAmount: amountBs === null, estimated, categoryGuessed: guessed, huge: amountBs !== null && amountBs > 50000 }
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
  root.QullqiParser = api;
})(typeof globalThis !== 'undefined' ? globalThis : this);
