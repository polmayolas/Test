// node tests/e2e.js [outdir]  — pruebas de extremo a extremo con Playwright (Chromium)
const { chromium } = require('playwright');
const http = require('http');
const fs = require('fs');
const path = require('path');

const OUT = process.argv[2] || path.join(__dirname, '..', 'shots');
fs.mkdirSync(OUT, { recursive: true });
const html = fs.readFileSync(path.join(__dirname, '..', 'dist', 'index.html'), 'utf8');
const mock = fs.readFileSync(path.join(__dirname, 'mock-claude.js'), 'utf8');

let failures = 0;
const ok = (cond, msg) => { if (cond) console.log('  ✓', msg); else { failures++; console.log('  ✗', msg); } };

(async () => {
  const server = http.createServer((req, res) => { res.writeHead(200, { 'content-type': 'text/html; charset=utf-8' }); res.end(html); }).listen(0);
  const url = 'http://127.0.0.1:' + server.address().port + '/';
  const browser = await chromium.launch();
  const errors = [];
  async function newPage(ctx, mockCfg) {
    const p = await ctx.newPage();
    p.on('pageerror', e => errors.push(String(e)));
    p.on('console', m => { if (m.type() === 'error') errors.push(m.text()); });
    if (mockCfg !== null) await p.addInitScript(`window.__MOCK = ${JSON.stringify(mockCfg || {})};\n` + mock);
    await p.goto(url);
    return p;
  }
  const cashText = p => p.locator('#cash-total').innerText();

  console.log('1) Modo compartido con dictado');
  const ctx = await browser.newContext({ viewport: { width: 390, height: 844 }, deviceScaleFactor: 1, locale: 'es-BO' });
  const a = await newPage(ctx, { uid: 'u1' });
  await a.waitForSelector('.whoami');
  await a.screenshot({ path: path.join(OUT, '01-home-empty.png') });
  await a.click('[data-me="Pol"]');
  ok(await a.locator('.whoami').count() === 0, 'elige Pol y desaparece la tarjeta');
  await a.click('#mic');
  await a.waitForTimeout(120);
  ok((await a.locator('#mic-label').innerText()).includes('escucho'), 'estado escuchando');
  await a.evaluate(() => window.__mockSpeak('Me acabo de gastar 25 bolivianos en un café y el gasto es para Sol y Pol'));
  await a.screenshot({ path: path.join(OUT, '02-listening.png') });
  await a.click('#mic');
  await a.waitForSelector('#sheet.show');
  await a.waitForTimeout(350);
  ok(await a.inputValue('#amt-0') === '25', 'importe 25');
  ok(await a.getAttribute('[data-k="category"][data-v="resto"]', 'aria-pressed') === 'true', 'categoría restaurantes');
  ok(await a.getAttribute('[data-k="who"][data-v="pareja"]', 'aria-pressed') === 'true', 'para Sol y Pol');
  ok(await a.inputValue('#note-0') === 'Café', 'concepto Café');
  await a.screenshot({ path: path.join(OUT, '03-review-sheet.png') });
  await a.click('#sheet-ok');
  await a.waitForSelector('#sheet', { state: 'hidden' });
  ok((await cashText(a)).includes('25'), 'efectivo −25: ' + (await cashText(a)).replace(/\s+/g, ' '));

  console.log('2) Escribir: cajero');
  await a.fill('#say', 'saqué 1000 del cajero');
  await a.press('#say', 'Enter');
  await a.waitForSelector('#sheet.show');
  await a.waitForTimeout(300);
  ok(await a.getAttribute('[data-k="type"][data-v="cash_in"]', 'aria-pressed') === 'true', 'tipo entrada');
  await a.click('#sheet-ok');
  await a.waitForSelector('#sheet', { state: 'hidden' });
  ok((await cashText(a)).replace(/\s+/g, ' ').includes('975'), 'efectivo 975');

  console.log('3) Varios en una frase');
  await a.fill('#say', 'pan 10, leche 12 y taxi 15 con QR');
  await a.press('#say', 'Enter');
  await a.waitForSelector('#sheet.show'); await a.waitForTimeout(300);
  ok(await a.locator('.item').count() === 3, '3 movimientos');
  await a.screenshot({ path: path.join(OUT, '04-multi.png'), fullPage: false });
  await a.click('#sheet-ok');
  await a.waitForSelector('#sheet', { state: 'hidden' });
  ok((await cashText(a)).replace(/\s+/g, ' ').includes('953'), 'efectivo 953 (QR no descuenta)');
  await a.screenshot({ path: path.join(OUT, '05-home-filled.png') });

  console.log('4) Sincronización con otro móvil');
  const b = await newPage(ctx, { uid: 'u2' });
  await b.waitForTimeout(400);
  ok((await cashText(b)).replace(/\s+/g, ' ').includes('953'), 'el otro móvil ve 953');
  await b.click('[data-me="Sol"]');
  await b.fill('#say', 'un helado para mí 15 bs');
  await b.press('#say', 'Enter');
  await b.waitForSelector('#sheet.show'); await b.waitForTimeout(300);
  ok(await b.getAttribute('[data-k="who"][data-v="sol"]', 'aria-pressed') === 'true', '«para mí» = Sol en su móvil');
  await b.click('#sheet-ok'); await b.waitForSelector('#sheet', { state: 'hidden' });
  await a.waitForTimeout(300);
  ok((await cashText(a)).replace(/\s+/g, ' ').includes('938'), 'Pol ve el gasto de Sol (938)');

  console.log('5) Validación de importe vacío');
  await a.click('#add-expense');
  await a.waitForSelector('#sheet.show'); await a.waitForTimeout(300);
  await a.click('#sheet-ok');
  ok(await a.locator('.amount-in.err').count() === 1, 'marca importe vacío');
  await a.click('#sheet-cancel'); await a.waitForSelector('#sheet', { state: 'hidden' });

  console.log('6) Ajustes: corregir efectivo');
  await a.click('#btn-settings'); await a.waitForSelector('#sheet.show'); await a.waitForTimeout(300);
  await a.screenshot({ path: path.join(OUT, '06-settings.png') });
  await a.fill('#adj-in', '900');
  await a.click('[data-act="adjust"]');
  await a.waitForTimeout(200);
  await a.click('#sheet-ok'); await a.waitForSelector('#sheet', { state: 'hidden' });
  ok((await cashText(a)).replace(/\s+/g, ' ').includes('900'), 'efectivo ajustado a 900');

  console.log('7) Palabra clave personalizada');
  await a.click('#btn-settings'); await a.waitForSelector('#sheet.show'); await a.waitForTimeout(300);
  await a.click('[data-cat-edit="personal"]'); await a.waitForTimeout(150);
  await a.fill('#cat-kw', 'María, Rosa');
  await a.click('#sheet-ok'); await a.waitForTimeout(200);
  await a.click('#sheet-ok'); await a.waitForSelector('#sheet', { state: 'hidden' });
  await a.fill('#say', 'pagué 300 a María'); await a.press('#say', 'Enter');
  await a.waitForSelector('#sheet.show'); await a.waitForTimeout(300);
  ok(await a.getAttribute('[data-k="category"][data-v="personal"]', 'aria-pressed') === 'true', 'María → Personal del hogar');
  await a.click('#sheet-ok'); await a.waitForSelector('#sheet', { state: 'hidden' });

  console.log('8) Movimientos, editar y eliminar');
  await a.click('[data-tab="hist"]'); await a.waitForTimeout(150);
  await a.screenshot({ path: path.join(OUT, '07-history.png') });
  const rows = await a.locator('#hist-list [data-edit]').count();
  ok(rows === 8, 'historial con 8 filas (' + rows + ')');
  await a.locator('#hist-list [data-edit]').first().click();
  await a.waitForSelector('#sheet.show'); await a.waitForTimeout(300);
  await a.click('[data-act="delete"]'); await a.click('[data-act="delete"]');
  await a.waitForSelector('#sheet', { state: 'hidden' });
  ok(await a.locator('#hist-list [data-edit]').count() === rows - 1, 'eliminado');
  await a.click('#toast-act');
  await a.waitForTimeout(200);
  ok(await a.locator('#hist-list [data-edit]').count() === rows, 'deshacer restaura');

  console.log('9) Resumen');
  await a.click('[data-tab="sum"]'); await a.waitForTimeout(200);
  await a.screenshot({ path: path.join(OUT, '08-summary.png'), fullPage: true });
  ok(await a.locator('.bar-row[data-cat]').count() >= 3, 'barras por categoría');
  await a.locator('.bar-row[data-cat]').first().click(); await a.waitForTimeout(150);
  ok(await a.locator('#clear-cat').count() === 1, 'filtro por categoría desde resumen');

  console.log('10) Modo oscuro');
  const dctx = await browser.newContext({ viewport: { width: 390, height: 844 }, colorScheme: 'dark', locale: 'es-BO' });
  const d = await newPage(dctx, { uid: 'u9' });
  await d.evaluate(() => { localStorage.setItem('__mockdb', localStorage.getItem('__mockdb') || '{}'); });
  await d.waitForTimeout(300);
  await d.screenshot({ path: path.join(OUT, '09-dark-home.png') });
  await d.fill('#say', 'cambié 100 dólares'); await d.press('#say', 'Enter');
  await d.waitForSelector('#sheet.show'); await d.waitForTimeout(300);
  await d.screenshot({ path: path.join(OUT, '10-dark-sheet.png') });

  console.log('11) Sin plataforma (independiente, localStorage)');
  const sctx = await browser.newContext({ viewport: { width: 360, height: 740 }, locale: 'es-BO' });
  const s = await newPage(sctx, null);
  await s.waitForTimeout(200);
  ok(await s.locator('.banner').count() >= 1, 'aviso de modo local');
  await s.fill('#say', '40 de taxi'); await s.press('#say', 'Enter');
  await s.waitForSelector('#sheet.show'); await s.waitForTimeout(300);
  await s.click('#sheet-ok'); await s.waitForSelector('#sheet', { state: 'hidden' });
  await s.reload(); await s.waitForTimeout(200);
  ok((await cashText(s)).includes('40'), 'persiste tras recargar');
  await s.screenshot({ path: path.join(OUT, '11-standalone-360.png') });

  console.log('12) Solo lectura');
  const rctx = await browser.newContext({ viewport: { width: 390, height: 844 } });
  const r = await newPage(rctx, { uid: 'u3', canWrite: false });
  await r.waitForTimeout(300);
  ok((await r.locator('.banner').innerText()).includes('Solo lectura'), 'aviso de solo lectura');

  console.log('13) Regresiones de robustez');
  {
    // a) fallo a mitad de un guardado múltiple: reintento sin duplicados
    const c1 = await browser.newContext({ viewport: { width: 390, height: 844 } });
    const p1 = await newPage(c1, { uid: 'r1', failAfterSets: 1 });
    await p1.waitForTimeout(200);
    await p1.fill('#say', 'pan 10, leche 12 y taxi 15'); await p1.press('#say', 'Enter');
    await p1.waitForSelector('#sheet.show'); await p1.waitForTimeout(300);
    await p1.click('#sheet-ok'); await p1.waitForTimeout(300);
    ok(await p1.locator('.item').count() === 2, 'tras fallo quedan 2 por guardar');
    await p1.evaluate(() => { window.__mockCfg.failAfterSets = null; });
    await p1.click('#sheet-ok'); await p1.waitForSelector('#sheet', { state: 'hidden' });
    const n1 = await p1.evaluate(() => window.__qullqi.allTxns().length);
    ok(n1 === 3, 'sin duplicados al reintentar (' + n1 + ')');
    // b) configuración lenta: elegir quién soy no borra categorías de otros
    const c2 = await browser.newContext({ viewport: { width: 390, height: 844 } });
    const seed = await newPage(c2, { uid: 's1' });
    await seed.waitForTimeout(200);
    await seed.evaluate(() => window.__qullqi.Store.patchConfig({ customCats: { cx: { name: 'Deporte', keywords: ['gimnasio'], color: '#5856d6', createdAt: 1 } }, rates: { USD: 7.1 } }));
    const slow = await newPage(c2, { uid: 's2', configDelay: 1200 });
    await slow.waitForTimeout(300);
    ok(await slow.locator('.whoami').count() === 0, 'no pregunta quién soy antes de cargar la configuración');
    await slow.waitForTimeout(1200);
    await slow.click('[data-me="Sol"]'); await slow.waitForTimeout(200);
    const cfg = await slow.evaluate(() => JSON.parse(localStorage.getItem('__mockdb'))['config/main']);
    ok(cfg.customCats && cfg.customCats.cx && cfg.rates.USD === 7.1 && cfg.members.s2 === 'Sol', 'la configuración compartida se conserva');
    // c) doble toque en Ajustar con latencia
    const c3 = await browser.newContext({ viewport: { width: 390, height: 844 } });
    const p3 = await newPage(c3, { uid: 'a1', writeDelay: 300 });
    await p3.waitForTimeout(200);
    await p3.click('#btn-settings'); await p3.waitForSelector('#sheet.show'); await p3.waitForTimeout(300);
    await p3.fill('#adj-in', '500');
    await p3.evaluate(() => { const b = document.querySelector('[data-act="adjust"]'); b.click(); b.click(); });
    await p3.waitForTimeout(900);
    const bal3 = await p3.evaluate(() => window.__qullqi.balance(window.__qullqi.allTxns()));
    ok(bal3 === 500, 'doble toque en Ajustar no duplica (' + bal3 + ')');
    // d) almacenamiento local roto: no anuncia «guardado»
    const c4 = await browser.newContext({ viewport: { width: 390, height: 844 } });
    const p4 = await c4.newPage();
    await p4.addInitScript(() => { Storage.prototype.setItem = function () { throw new Error('QuotaExceeded'); }; });
    await p4.goto(url); await p4.waitForTimeout(200);
    ok((await p4.locator('.banner').innerText()).includes('No se puede guardar'), 'aviso si el navegador no deja guardar');
    await p4.fill('#say', '40 de taxi'); await p4.press('#say', 'Enter');
    await p4.waitForSelector('#sheet.show'); await p4.waitForTimeout(300);
    await p4.click('#sheet-ok'); await p4.waitForTimeout(200);
    ok((await p4.locator('#toast-msg').innerText()).includes('No se ha guardado'), 'avisa de que no se guardó');
    ok(await p4.locator('#sheet.show').count() === 1, 'la hoja sigue abierta para no perder el gasto');
    // e) datos compartidos mal formados no rompen la UI
    const c5 = await browser.newContext({ viewport: { width: 390, height: 844 } });
    const p5 = await newPage(c5, { uid: 'm1' });
    await p5.evaluate(() => localStorage.setItem('__mockdb', JSON.stringify({
      'txns/x1': { type: 'expense', amount: 10, date: 'ayer', category: 'resto' },
      'txns/x2': { type: 'expense', amount: '10', date: '2026-11-20' },
      'txns/x3': { type: 'expense', amount: 7, date: '2026-11-20', category: 'resto', note: 5 },
      'config/main': { customCats: { k: { name: 'Raro', keywords: 'no-array' } } }
    })));
    await p5.reload(); await p5.waitForTimeout(300);
    await p5.click('[data-tab="hist"]'); await p5.waitForTimeout(100);
    ok(await p5.locator('#hist-list [data-edit]').count() === 1, 'ignora movimientos mal formados');
    await p5.click('[data-tab="home"]'); await p5.click('#btn-settings'); await p5.waitForTimeout(300);
    ok(await p5.locator('[data-cat-edit]').count() > 5, 'Ajustes abre con palabras clave mal formadas');
  }

  console.log('14) Regresiones de diseño');
  {
    const c6 = await browser.newContext({ viewport: { width: 390, height: 844 } });
    const p6 = await newPage(c6, { uid: 'x1' });
    await p6.waitForTimeout(200);
    await p6.fill('#say', 'pan 10'); await p6.press('#say', 'Enter');
    await p6.waitForSelector('#sheet.show'); await p6.waitForTimeout(300);
    await p6.click('#sheet-ok'); await p6.waitForSelector('#sheet', { state: 'hidden' });
    ok(await p6.locator('#toast.show #toast-act:not([hidden])').count() === 1, 'aviso con Deshacer tras guardar');
    await p6.fill('#say', 'taxi 15'); await p6.press('#say', 'Enter');
    await p6.waitForSelector('#sheet.show'); await p6.waitForTimeout(300);
    ok(await p6.locator('#toast.show').count() === 0, 'el aviso se oculta al abrir la hoja siguiente');
    await p6.click('#sheet-cancel'); await p6.waitForSelector('#sheet', { state: 'hidden' });
    await p6.click('#btn-settings'); await p6.waitForSelector('#sheet.show'); await p6.waitForTimeout(300);
    await p6.evaluate(() => { document.querySelector('#sheet-body').scrollTop = 400; });
    await p6.click('#sheet-ok'); await p6.waitForSelector('#sheet', { state: 'hidden' });
    await p6.fill('#say', 'café 12'); await p6.press('#say', 'Enter');
    await p6.waitForSelector('#sheet.show'); await p6.waitForTimeout(300);
    ok(await p6.evaluate(() => document.querySelector('#sheet-body').scrollTop) === 0, 'la hoja nueva empieza arriba');
    const broken = await p6.evaluate(() => [...document.querySelectorAll('.cat > span:last-child')].filter(e => e.getClientRects().length > 1 || e.scrollWidth > e.clientWidth + 1).map(e => e.textContent));
    ok(broken.length === 0, 'categorías sin cortes a mitad de palabra ' + JSON.stringify(broken));
    ok(await p6.evaluate(() => document.documentElement.lang) === 'es', 'idioma de la página: es');
  }

  // overflow horizontal
  for (const p of [a, s]) {
    const ov = await p.evaluate(() => document.documentElement.scrollWidth - document.documentElement.clientWidth);
    ok(ov <= 0, 'sin scroll horizontal (' + ov + ')');
  }
  ok(errors.length === 0, 'sin errores de consola ' + (errors.length ? JSON.stringify(errors.slice(0, 5)) : ''));
  await browser.close(); server.close();
  console.log(failures ? `\n${failures} FALLOS` : '\nTODO OK');
  process.exit(failures ? 1 : 0);
})().catch(e => { console.error(e); process.exit(1); });
