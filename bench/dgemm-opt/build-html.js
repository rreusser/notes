'use strict';

/* eslint-disable max-len */

// Compiles reports/dgemm-optimization.md to a single self-contained HTML file
// (reports/dgemm-optimization.html). Figures referenced as `![alt](figN.svg)`
// are inlined directly from the .svg sources, so the result is one portable
// file with no external asset dependencies. Uses the repo's markdown-it.

var fs = require( 'fs' );
var path = require( 'path' );
var MarkdownIt = require( 'markdown-it' );

var DIR = path.join( __dirname, 'reports' );
var SRC = path.join( DIR, 'dgemm-optimization.md' );
var OUT = path.join( DIR, 'dgemm-optimization.html' );

var md = new MarkdownIt({ 'html': true, 'linkify': true, 'typographer': true });

// Pull figure references out before markdown rendering and replace them with a
// placeholder, so we can inject the raw <svg> (markdown-it would otherwise emit
// an <img src="figN.svg"> that needs the file alongside it).
var source = fs.readFileSync( SRC, 'utf8' );
var figRe = /!\[([^\]]*)\]\((fig[^)]+\.svg)\)/g;
var figures = [];
source = source.replace( figRe, function ( _, alt, file ) {
	var token = '@@FIGURE' + figures.length + '@@';
	figures.push({ 'alt': alt, 'file': file });
	return token;
});

var body = md.render( source );

// Inject inlined figures:
figures.forEach( function ( fig, i ) {
	var svgPath = path.join( DIR, fig.file );
	var svg = fs.existsSync( svgPath ) ? fs.readFileSync( svgPath, 'utf8' ) : '<em>missing ' + fig.file + '</em>';
	var fignum = i + 1;
	var html = '<figure><div class="chart">' + svg + '</div><figcaption>Figure ' + fignum + '. ' + fig.alt + '</figcaption></figure>';
	body = body.replace( '<p>@@FIGURE' + i + '@@</p>', html ).replace( '@@FIGURE' + i + '@@', html );
});

var css = [
	':root{--fg:#1a1a1a;--muted:#666;--line:#e2e2e2;--accent:#1f77b4;--bg:#fff;--code-bg:#f4f5f7}',
	'*{box-sizing:border-box}',
	'body{margin:0;background:#fafafa;color:var(--fg);font:16px/1.65 -apple-system,BlinkMacSystemFont,"Segoe UI",Helvetica,Arial,sans-serif}',
	'main{max-width:880px;margin:0 auto;padding:48px 28px 96px;background:var(--bg);box-shadow:0 0 0 1px var(--line)}',
	'h1{font-size:2.1rem;line-height:1.2;margin:0 0 .2em;border-bottom:3px solid var(--accent);padding-bottom:.3em}',
	'h2{font-size:1.5rem;margin:2.2em 0 .6em;padding-bottom:.25em;border-bottom:1px solid var(--line)}',
	'h3{font-size:1.18rem;margin:1.8em 0 .5em}',
	'p,li{color:var(--fg)}',
	'em{color:var(--muted)}',
	'a{color:var(--accent);text-decoration:none}a:hover{text-decoration:underline}',
	'code{background:var(--code-bg);padding:.12em .38em;border-radius:4px;font:0.86em "SF Mono",ui-monospace,Menlo,Consolas,monospace}',
	'pre{background:var(--code-bg);padding:14px 16px;border-radius:8px;overflow:auto;border:1px solid var(--line)}',
	'pre code{background:none;padding:0}',
	'blockquote{margin:1.2em 0;padding:.6em 1.1em;border-left:4px solid var(--accent);background:#f0f6fc;border-radius:0 6px 6px 0}',
	'blockquote p{margin:.3em 0}',
	'table{border-collapse:collapse;width:100%;margin:1.2em 0;font-size:.86rem;display:block;overflow-x:auto}',
	'th,td{border:1px solid var(--line);padding:6px 10px;text-align:right;white-space:nowrap}',
	'th:first-child,td:first-child{text-align:left}',
	'thead th{background:#f4f5f7;position:sticky;top:0}',
	'tbody tr:nth-child(even){background:#fafbfc}',
	'figure{margin:1.6em 0;text-align:center}',
	'figure .chart{border:1px solid var(--line);border-radius:8px;padding:8px;background:#fff;display:inline-block;max-width:100%}',
	'figure svg{max-width:100%;height:auto}',
	'figcaption{color:var(--muted);font-size:.85rem;margin-top:.6em}',
	'hr{border:none;border-top:1px solid var(--line);margin:2.4em 0}',
	'strong{font-weight:650}'
].join( '\n' );

var doc = '<!doctype html>\n<html lang="en">\n<head>\n<meta charset="utf-8">\n' +
	'<meta name="viewport" content="width=device-width,initial-scale=1">\n' +
	'<title>dgemm optimization report</title>\n<style>\n' + css + '\n</style>\n</head>\n<body>\n<main>\n' +
	body + '\n</main>\n</body>\n</html>\n';

fs.writeFileSync( OUT, doc );
console.log( 'wrote ' + path.relative( process.cwd(), OUT ) + ' (' + ( doc.length/1024 ).toFixed(1) + ' KB, ' + figures.length + ' figures inlined)' );
