# Qullqi

*Qullqi* significa «plata, dinero» en quechua y aimara. Es una web app para el móvil que sirve para llevar el control del dinero en efectivo de la familia: dices el gasto en voz alta y Qullqi lo anota.

- **Por voz:** «me acabo de gastar 25 bolivianos en un café para Sol y Pol». Qullqi entiende el importe, la categoría, para quién es, la forma de pago y la fecha, y te enseña una hoja para revisarlo antes de guardar.
- **Efectivo en mano:** suma lo que sacas del cajero o cambias de divisas y resta los gastos en efectivo. Los pagos con QR o tarjeta no lo descuentan.
- **Resumen:** por mes, por categoría, para quién y día a día.
- **Coste cero:** la interpretación funciona con reglas propias en español, sin IA ni tokens.

## Estructura

| Archivo | Qué es |
|---|---|
| `src/parser.js` | Intérprete del texto dictado: importes en cifras o en palabras, varias compras en una frase, correcciones, cajero y cambio de divisas, fechas relativas |
| `src/app.js`, `src/app.html`, `src/app.css` | La app (JavaScript sin frameworks) |
| `build.js` | Genera `dist/artifact.html` (la página que se publica) y `dist/index.html` (un documento completo para usarla por separado o hacer pruebas) |
| `tests/` | Pruebas del intérprete (`parser.test.js`), pruebas de extremo a extremo con Playwright (`e2e.js`) y un simulador de la plataforma (`mock-claude.js`) |

```sh
node build.js
node tests/parser.test.js
node tests/e2e.js
```

## Datos

Publicada como Artifact de claude.ai, la app guarda los movimientos en la base de datos compartida del artifact: colección `txns` y documento `config/main`. Así, quienes tienen acceso de Editor ven lo mismo en tiempo real.

Si se abre fuera de la plataforma, por ejemplo con `dist/index.html`, guarda los datos en el propio navegador.
