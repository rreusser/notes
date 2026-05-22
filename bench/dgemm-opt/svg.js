'use strict';

/* eslint-disable max-len */

// Minimal dependency-free SVG charting for the dgemm report. Produces
// standalone <svg> documents (line charts + grouped bar charts) saved as .svg
// files and referenced from the markdown report. Kept deliberately small and
// data-driven so the plots regenerate from the same numbers the tables use.

var PALETTE = [ '#1f77b4', '#d62728', '#2ca02c', '#9467bd', '#ff7f0e', '#17becf' ];

function esc( s ) { return String( s ).replace( /&/g, '&amp;' ).replace( /</g, '&lt;' ).replace( />/g, '&gt;' ); }

function niceCeil( x ) {
	var pow = Math.pow( 10, Math.floor( Math.log10( x ) ) );
	var f = x / pow;
	var nf = ( f <= 1 ) ? 1 : ( f <= 2 ) ? 2 : ( f <= 5 ) ? 5 : 10;
	return nf * pow;
}

// Line chart. series: [{name, color, points:[[x,y],...]}]. xLog: log10 x-axis.
function lineChart( opts ) {
	var W = opts.width || 720;
	var H = opts.height || 420;
	var m = { t: 50, r: 24, b: 64, l: 64 };
	var pw = W - m.l - m.r;
	var ph = H - m.t - m.b;
	var series = opts.series;
	var xs = [];
	var ys = [];
	series.forEach( function ( s ) { s.points.forEach( function ( p ) { xs.push( p[0] ); ys.push( p[1] ); } ); } );
	var xmin = opts.xLog ? Math.log10( Math.min.apply( null, xs ) ) : Math.min.apply( null, xs );
	var xmax = opts.xLog ? Math.log10( Math.max.apply( null, xs ) ) : Math.max.apply( null, xs );
	var ymin = ( opts.ymin !== undefined ) ? opts.ymin : 0;
	var ymax = opts.ymax || niceCeil( Math.max.apply( null, ys ) * 1.05 );
	function X( x ) { var v = opts.xLog ? Math.log10( x ) : x; return m.l + ( ( v - xmin ) / ( xmax - xmin ) ) * pw; }
	function Y( y ) { return m.t + ph - ( ( y - ymin ) / ( ymax - ymin ) ) * ph; }

	var s = [];
	s.push( '<svg xmlns="http://www.w3.org/2000/svg" width="'+W+'" height="'+H+'" viewBox="0 0 '+W+' '+H+'" font-family="-apple-system,Segoe UI,Helvetica,Arial,sans-serif">' );
	s.push( '<rect width="'+W+'" height="'+H+'" fill="#ffffff"/>' );
	s.push( '<text x="'+(W/2)+'" y="26" text-anchor="middle" font-size="17" font-weight="600">'+esc(opts.title||'')+'</text>' );

	// y gridlines + labels:
	var yticks = opts.yticks || 5;
	var i, yt, xt, gx, gy;
	for ( i = 0; i <= yticks; i++ ) {
		yt = ymin + ( ymax - ymin ) * i / yticks;
		gy = Y( yt );
		s.push( '<line x1="'+m.l+'" y1="'+gy.toFixed(1)+'" x2="'+(m.l+pw)+'" y2="'+gy.toFixed(1)+'" stroke="#e6e6e6"/>' );
		s.push( '<text x="'+(m.l-8)+'" y="'+(gy+4).toFixed(1)+'" text-anchor="end" font-size="12" fill="#444">'+yt.toFixed( opts.yprec===undefined?1:opts.yprec )+'</text>' );
	}
	// baseline (y=1) emphasis if requested:
	if ( opts.baseline !== undefined ) {
		gy = Y( opts.baseline );
		s.push( '<line x1="'+m.l+'" y1="'+gy.toFixed(1)+'" x2="'+(m.l+pw)+'" y2="'+gy.toFixed(1)+'" stroke="#999" stroke-dasharray="5,4"/>' );
		s.push( '<text x="'+(m.l+pw-4)+'" y="'+(gy-6).toFixed(1)+'" text-anchor="end" font-size="11" fill="#777">'+esc(opts.baselineLabel||'baseline')+'</text>' );
	}
	// x ticks:
	( opts.xticks || xs.filter( function ( v, idx, a ) { return a.indexOf(v)===idx; } ).sort( function(a,b){return a-b;} ) ).forEach( function ( xv ) {
		gx = X( xv );
		s.push( '<line x1="'+gx.toFixed(1)+'" y1="'+(m.t+ph)+'" x2="'+gx.toFixed(1)+'" y2="'+(m.t+ph+5)+'" stroke="#444"/>' );
		s.push( '<text x="'+gx.toFixed(1)+'" y="'+(m.t+ph+20)+'" text-anchor="middle" font-size="12" fill="#444">'+esc(xv)+'</text>' );
	} );
	// axes:
	s.push( '<line x1="'+m.l+'" y1="'+(m.t+ph)+'" x2="'+(m.l+pw)+'" y2="'+(m.t+ph)+'" stroke="#444"/>' );
	s.push( '<line x1="'+m.l+'" y1="'+m.t+'" x2="'+m.l+'" y2="'+(m.t+ph)+'" stroke="#444"/>' );
	s.push( '<text x="'+(m.l+pw/2)+'" y="'+(H-18)+'" text-anchor="middle" font-size="13" fill="#222">'+esc(opts.xlabel||'')+'</text>' );
	s.push( '<text transform="translate(16,'+(m.t+ph/2)+') rotate(-90)" text-anchor="middle" font-size="13" fill="#222">'+esc(opts.ylabel||'')+'</text>' );

	// series:
	series.forEach( function ( ser, k ) {
		var col = ser.color || PALETTE[ k % PALETTE.length ];
		var d = ser.points.map( function ( p, idx ) { return ( idx?'L':'M' ) + X(p[0]).toFixed(1) + ' ' + Y(p[1]).toFixed(1); } ).join( ' ' );
		s.push( '<path d="'+d+'" fill="none" stroke="'+col+'" stroke-width="2.4"/>' );
		ser.points.forEach( function ( p ) { s.push( '<circle cx="'+X(p[0]).toFixed(1)+'" cy="'+Y(p[1]).toFixed(1)+'" r="3.2" fill="'+col+'"/>' ); } );
	} );
	// legend:
	var lx = m.l + 12;
	var ly = m.t + 6;
	series.forEach( function ( ser, k ) {
		var col = ser.color || PALETTE[ k % PALETTE.length ];
		s.push( '<rect x="'+lx+'" y="'+(ly-9+k*18)+'" width="14" height="4" rx="2" fill="'+col+'"/>' );
		s.push( '<text x="'+(lx+20)+'" y="'+(ly-4+k*18)+'" font-size="12" fill="#222">'+esc(ser.name)+'</text>' );
	} );
	s.push( '</svg>' );
	return s.join( '\n' );
}

// Grouped bar chart. groups:[{label, bars:[{name,value,color}]}]
function barChart( opts ) {
	var W = opts.width || 720;
	var H = opts.height || 420;
	var m = { t: 50, r: 24, b: 70, l: 64 };
	var pw = W - m.l - m.r;
	var ph = H - m.t - m.b;
	var groups = opts.groups;
	var names = groups[0].bars.map( function ( b ) { return b.name; } );
	var maxv = 0;
	groups.forEach( function ( g ) { g.bars.forEach( function ( b ) { if ( b.value > maxv ) maxv = b.value; } ); } );
	var ymax = opts.ymax || niceCeil( maxv * 1.08 );
	function Y( y ) { return m.t + ph - ( y / ymax ) * ph; }

	var s = [];
	s.push( '<svg xmlns="http://www.w3.org/2000/svg" width="'+W+'" height="'+H+'" viewBox="0 0 '+W+' '+H+'" font-family="-apple-system,Segoe UI,Helvetica,Arial,sans-serif">' );
	s.push( '<rect width="'+W+'" height="'+H+'" fill="#ffffff"/>' );
	s.push( '<text x="'+(W/2)+'" y="26" text-anchor="middle" font-size="17" font-weight="600">'+esc(opts.title||'')+'</text>' );

	var i, yt, gy;
	for ( i = 0; i <= 5; i++ ) {
		yt = ymax * i / 5; gy = Y( yt );
		s.push( '<line x1="'+m.l+'" y1="'+gy.toFixed(1)+'" x2="'+(m.l+pw)+'" y2="'+gy.toFixed(1)+'" stroke="#e6e6e6"/>' );
		s.push( '<text x="'+(m.l-8)+'" y="'+(gy+4).toFixed(1)+'" text-anchor="end" font-size="12" fill="#444">'+yt.toFixed(1)+'</text>' );
	}
	if ( opts.baseline !== undefined ) {
		gy = Y( opts.baseline );
		s.push( '<line x1="'+m.l+'" y1="'+gy.toFixed(1)+'" x2="'+(m.l+pw)+'" y2="'+gy.toFixed(1)+'" stroke="#999" stroke-dasharray="5,4"/>' );
	}
	s.push( '<text transform="translate(16,'+(m.t+ph/2)+') rotate(-90)" text-anchor="middle" font-size="13" fill="#222">'+esc(opts.ylabel||'')+'</text>' );

	var gw = pw / groups.length;
	var nb = names.length;
	var bw = ( gw * 0.78 ) / nb;
	groups.forEach( function ( g, gi ) {
		var gx0 = m.l + gi*gw + gw*0.11;
		g.bars.forEach( function ( b, bi ) {
			var col = b.color || PALETTE[ bi % PALETTE.length ];
			var bx = gx0 + bi*bw;
			var by = Y( b.value );
			s.push( '<rect x="'+bx.toFixed(1)+'" y="'+by.toFixed(1)+'" width="'+(bw*0.9).toFixed(1)+'" height="'+(m.t+ph-by).toFixed(1)+'" fill="'+col+'"/>' );
			s.push( '<text x="'+(bx+bw*0.45).toFixed(1)+'" y="'+(by-4).toFixed(1)+'" text-anchor="middle" font-size="10" fill="#333">'+b.value.toFixed(1)+'</text>' );
		} );
		s.push( '<text x="'+(m.l+gi*gw+gw/2).toFixed(1)+'" y="'+(m.t+ph+18)+'" text-anchor="middle" font-size="11" fill="#222">'+esc(g.label)+'</text>' );
	} );
	// axis line + legend:
	s.push( '<line x1="'+m.l+'" y1="'+(m.t+ph)+'" x2="'+(m.l+pw)+'" y2="'+(m.t+ph)+'" stroke="#444"/>' );
	var lx = m.l + 12, ly = m.t + 6;
	names.forEach( function ( nm, k ) {
		var col = ( groups[0].bars[k].color ) || PALETTE[ k % PALETTE.length ];
		s.push( '<rect x="'+lx+'" y="'+(ly-9+k*18)+'" width="12" height="12" rx="2" fill="'+col+'"/>' );
		s.push( '<text x="'+(lx+18)+'" y="'+(ly+1+k*18)+'" font-size="12" fill="#222">'+esc(nm)+'</text>' );
	} );
	s.push( '</svg>' );
	return s.join( '\n' );
}

module.exports = { lineChart, barChart, PALETTE };
