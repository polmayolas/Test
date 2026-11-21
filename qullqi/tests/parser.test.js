// node qullqi/tests/parser.test.js
const P = require('../src/parser.js');
const now = new Date(2026, 10, 21, 12, 0); // sábado 21 nov 2026
const ctx = { now, me: 'Pol', rates: { USD: 6.96, EUR: 7.5 } };
let fail = 0, pass = 0;

function check(text, exp, c) {
  const r = P.parseUtterance(text, Object.assign({}, ctx, c || {}));
  const items = r.items;
  const exps = Array.isArray(exp) ? exp : [exp];
  const errs = [];
  if (items.length !== exps.length) errs.push(`items ${items.length} != ${exps.length}`);
  exps.forEach((e, i) => {
    const it = items[i] || {};
    for (const k of Object.keys(e)) {
      const v = k === 'noteMatch' ? it.note : it[k];
      if (k === 'noteMatch') { if (!e[k].test(v || '')) errs.push(`[${i}] note "${v}" !~ ${e[k]}`); }
      else if (v !== e[k]) errs.push(`[${i}] ${k}=${JSON.stringify(v)} expected ${JSON.stringify(e[k])}`);
    }
  });
  if (errs.length) { fail++; console.log('FAIL', JSON.stringify(text), '\n   ', errs.join('\n    '), '\n    got', JSON.stringify(items)); }
  else pass++;
}

// --- básicos
check('Me acabo de gastar 25 bolivianos en un café y el gasto es para Sol y Pol',
  { type: 'expense', amountBs: 25, category: 'resto', who: 'pareja', method: 'cash', note: 'Café' });
check('25 bs café', { amountBs: 25, category: 'resto', who: 'familia' });
check('Gasté 120 en el supermercado para toda la familia', { amountBs: 120, category: 'comida', who: 'familia', noteMatch: /supermercado/i });
check('compramos verduras en la feria 85 bolivianos', { amountBs: 85, category: 'comida' });
check('Taxi a Santa Cruz 350', { amountBs: 350, category: 'transporte', noteMatch: /Taxi a Santa Cruz/ });
check('pagué 150 a la señora de la limpieza', { amountBs: 150, category: 'personal', noteMatch: /señora de la limpieza/i });
check('200 bolivianos a la niñera de Noah', { amountBs: 200, category: 'personal' });
check('productos de limpieza 45 bs', { amountBs: 45, category: 'casa' });
check('farmacia 38,50 para Noah', { amountBs: 38.5, category: 'salud', who: 'noah' });
check('pañales 90', { amountBs: 90, category: 'noah', who: 'noah' });
check('veinticinco bolivianos en un café', { amountBs: 25, category: 'resto', note: 'Café' });
check('me gasté treinta y cinco en salteñas', { amountBs: 35, category: 'resto' });
check('ciento cincuenta en la cena del restaurante', { amountBs: 150, category: 'resto' });
check('mil quinientos de alquiler', { amountBs: 1500, category: 'casa' });
check('dos mil quinientos bolivianos de alquiler', { amountBs: 2500, category: 'casa' });
check('alquiler 1.500 bs', { amountBs: 1500, category: 'casa' });
check('20 con 50 de pan', { amountBs: 20.5, category: 'comida' });
check('veinte con cincuenta de pan', { amountBs: 20.5, category: 'comida' });
check('compré dos panes por 10', { amountBs: 10, category: 'comida' });
check('pagué 50 por 2 cafés', { amountBs: 50, category: 'resto' });
check('un helado para mí 15 bs', { amountBs: 15, category: 'resto', who: 'pol' });
check('un helado para mí 15 bs', { who: 'sol' }, { me: 'Sol' });
check('Bs 30 gasolina', { amountBs: 30, category: 'transporte' });
check('trufi 10 pesos', { amountBs: 10, category: 'transporte' });
check('pagamos 60 con QR en el restaurante', { amountBs: 60, method: 'qr', category: 'resto' });
check('ayer gasté 40 en el mercado', { amountBs: 40, date: '2026-11-20', category: 'comida' });
check('anteayer 40 de taxi', { amountBs: 40, date: '2026-11-19' });
check('hace 3 días 40 de taxi', { amountBs: 40, date: '2026-11-18' });
check('el lunes pagamos 100 de internet', { amountBs: 100, date: '2026-11-16', category: 'casa' });
check('entrada al museo 20 para Sol', { amountBs: 20, category: 'ocio', who: 'sol' });
check('excursión a Amboró 400 para los tres', { amountBs: 400, category: 'ocio', who: 'familia' });
check('algo raro 33', { amountBs: 33, category: 'otros', who: 'familia' });

// --- efectivo
check('Acabo de sacar 1000 bolivianos del cajero', { type: 'cash_in', source: 'atm', amountBs: 1000, note: 'Cajero' });
check('saqué dos mil del cajero', { type: 'cash_in', source: 'atm', amountBs: 2000 });
check('retiré 500', { type: 'cash_in', source: 'atm', amountBs: 500 });
check('cambié 100 dólares y me dieron 690 bolivianos', { type: 'cash_in', source: 'exchange', amountBs: 690 });
check('cambié 100 dólares a 9,80', { type: 'cash_in', source: 'exchange', amountBs: 980 });
check('cambiamos 200 dólares', { type: 'cash_in', source: 'exchange', amountBs: 1392 });
check('cambié 50 euros por 400 bolivianos', { type: 'cash_in', source: 'exchange', amountBs: 400, currency: 'EUR' });

// --- múltiples
check('25 en café y 40 en taxi', [{ amountBs: 25, category: 'resto' }, { amountBs: 40, category: 'transporte' }]);
check('pan 10, leche 12 y taxi 15', [{ amountBs: 10, category: 'comida' }, { amountBs: 12, category: 'comida' }, { amountBs: 15, category: 'transporte' }]);
check('saqué 1000 del cajero y pagué 20 de comisión', [{ type: 'cash_in', amountBs: 1000 }, { type: 'expense', amountBs: 20, category: 'otros' }]);
check('café 25 y taxi 40, todo para la familia', [{ who: 'familia' }, { who: 'familia' }]);

// --- números escritos
check('cuatrocientos cincuenta en la compra del mes', { amountBs: 450, category: 'comida' });
check('setenta y ocho bolivianos de farmacia', { amountBs: 78, category: 'salud' });
check('doce bolivianos de pan', { amountBs: 12, category: 'comida' });
check('gasté cinco en pan', { amountBs: 5, category: 'comida' });

// --- sin importe
check('café para Noah', { amountBs: null, category: 'resto', who: 'noah' });

// --- palabras clave personalizadas
check('pagué 300 a María', { category: 'personal' }, { extraCats: [{ id: 'personal', keywords: ['María'] }] });
check('20 en el gimnasio', { category: 'deporte' }, { extraCats: [{ id: 'deporte', name: 'Deporte', keywords: ['gimnasio'] }] });

// --- segunda ronda (árbitro del intérprete)
check('pan 8 leche 12 taxi 15', [{ amountBs: 8 }, { amountBs: 12 }, { amountBs: 15, category: 'transporte' }]);
check('taxi 25, no, 35', { amountBs: 35, category: 'transporte' });
check('compré 4 empanadas', { amountBs: null });
check('taxi a las 5 de la tarde 20', { amountBs: 20 });
check('mesa para 6, cena 300', { amountBs: 300, category: 'resto' });
check('sacamos entradas para el cine 80', { type: 'expense', category: 'ocio', amountBs: 80 });
check('mi suegra nos dio 500', { type: 'cash_in', amountBs: 500 });
check('cambié cien euros a ocho con veinte', { type: 'cash_in', source: 'exchange', amountBs: 820 });
check('saqué 500 y cambié 100 dólares', [{ type: 'cash_in', source: 'atm', amountBs: 500 }, { type: 'cash_in', source: 'exchange', amountBs: 696 }]);
check('café 15, taxi 20 y pañales 90 para Noah', [{ who: 'familia' }, { who: 'familia' }, { who: 'noah' }]);
check('bloqueador para el sol 60', { who: 'familia' });
check('ayer cena 120 y hoy taxi 20', [{ date: '2026-11-20' }, { date: '2026-11-21' }]);
check('ayer pan 10 y leche 12', [{ date: '2026-11-20' }, { date: '2026-11-20' }]);
check('pañales $ 90', { amountBs: 90, currency: 'BOB' });
check('cambié $100 por 950', { type: 'cash_in', amountBs: 950, currency: 'USD' });
check('20 bolivianos 50 centavos de pan', { amountBs: 20.5 });
check('café con leche 14', { category: 'resto' });
check('megas para el celular 20', { category: 'casa' });
check('x'.repeat(5000) + ' 20', { amountBs: null });

// --- tercera ronda
check('gasolina para 120 km, 100 bolivianos', { amountBs: 100, category: 'transporte' });
check('taxi a la calle Bolívar 25, 10 pesos', { amountBs: 10 });
check('taxi de 15 km, 30', { amountBs: 30 });
check('pasaje en flota 70 para Sol', { amountBs: 70, who: 'sol' });
check('café en la plaza 15', { amountBs: 15 });
check('la abuela le regaló 200 a Noah', { type: 'cash_in', amountBs: 200 });
check('regalo para la profesora 60', { type: 'expense', amountBs: 60 });
check('nos llegaron 2000 de España', { type: 'cash_in', amountBs: 2000 });
check('Paul pagó el taxi, 20', { who: 'familia', category: 'transporte' });
check('gafas de sol 100', { who: 'familia', category: 'compras' });
check('el 15 de noviembre farmacia 40', { date: '2026-11-15', amountBs: 40 });
check('el 18/11 pan 10', { date: '2026-11-18', amountBs: 10 });
check('taxi a la calle 24 de septiembre 15', { amountBs: 15, date: '2026-11-21' });
check('Almuerzo 30 para Sol y 30 para mí', [{ who: 'sol' }, { who: 'pol' }]);
check('comimos en la pensión 45', { category: 'resto' });
check('me dieron 980 bolivianos por 100 dólares', { type: 'cash_in', source: 'exchange', amountBs: 980 });

// --- cuarta ronda
check('el 15 pagué el alquiler 2500', { amountBs: 2500, date: '2026-11-15', category: 'casa' });
check('el martes 17 taxi 20', { amountBs: 20, date: '2026-11-17' });
check('el quince de noviembre luz 85', { amountBs: 85, date: '2026-11-15' });
check('pagué 30 en el taxi, bueno, en realidad fueron 35', { amountBs: 35 });
check('no eran 50, eran 60 del mercado', { amountBs: 60 });
check('pan 8 y leche 10, no, leche 12', [{ amountBs: 8 }, { amountBs: 12 }]);
check('100 gramos de queso 12', { amountBs: 12 });
check('mecánico, cambiaron 4 llantas, 1600', { amountBs: 1600 });
check('gasolina para 300 km 250 bolivianos', { amountBs: 250 });
check('dos almuerzos a 30', { amountBs: 60 });
check('alquiler 2 mil 500', { amountBs: 2500 });
check('alquiler dosmil quinientos', { amountBs: 2500 });
check('chicle cincuenta centavos', { amountBs: 0.5 });
check('almuerzo cuarenta bolivianos y cincuenta centavos', { amountBs: 40.5 });
check('el cajero del súper me cobró 150', { type: 'expense', amountBs: 150 });
check('trufi 6 ida y 6 vuelta', [{ amountBs: 6 }, { amountBs: 6 }]);
check('pan 8 leche 12 huevos 25', [{ amountBs: 8 }, { amountBs: 12 }, { amountBs: 25 }]);
check('taxi 20 y gaseosa 8', [{ category: 'transporte' }, { category: 'comida' }]);
check('taxi 20 y unas jarritas 10', [{ category: 'transporte' }, { category: 'otros' }]);
check('me compré una camisa 100', { who: 'pol', category: 'compras' });
check('taxi al hospital 10', { category: 'transporte' });

console.log(`\n${pass} ok, ${fail} fallos`);
process.exit(fail ? 1 : 0);
