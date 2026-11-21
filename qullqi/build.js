// node build.js  ->  dist/artifact.html (para publicar)  +  dist/index.html (documento completo, uso local/independiente)
const fs = require('fs');
const path = require('path');
const src = f => fs.readFileSync(path.join(__dirname, 'src', f), 'utf8');
const head = '<title>Qullqi</title>\n<meta name="claude:viewport-fit-sides" content="cover">\n' +
  '<meta name="description" content="Control de gastos en efectivo por voz">\n<meta name="theme-color" content="#f2f2f7">\n';
const content = head + '<style>\n' + src('app.css') + '</style>\n' + src('app.html') +
  '<script>\n' + src('parser.js') + '</script>\n<script>\n' + src('app.js') + '</script>\n';
fs.mkdirSync(path.join(__dirname, 'dist'), { recursive: true });
fs.writeFileSync(path.join(__dirname, 'dist', 'artifact.html'), content);
const full = '<!doctype html><html lang="es"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1,viewport-fit=cover">' +
  '<style>:root{color-scheme:light;padding-top:env(safe-area-inset-top,0px);padding-bottom:env(safe-area-inset-bottom,0px)}body{margin:0;font:14px system-ui,sans-serif}img{max-width:100%}[hidden]{display:none!important}</style>' +
  '</head><body>' + content + '</body></html>\n';
fs.writeFileSync(path.join(__dirname, 'dist', 'index.html'), full);
console.log('built', (content.length / 1024).toFixed(1) + ' KB');
