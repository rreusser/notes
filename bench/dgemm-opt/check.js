'use strict';

/* eslint-disable max-len */

// Correctness gate: validate each variant against the v0 reference across a
// matrix of shapes, layouts (col-major / row-major / padded / offset),
// transpose combinations, and alpha/beta values. A variant that disagrees
// anywhere is disqualified before it can be benchmarked.

var ref = require( './variants/v0-reference.js' );

var TOL = 1e-9; // relative tolerance for f64 reorderings

function randArray( n ) {
	var a = new Float64Array( n );
	var i;
	for ( i = 0; i < n; i++ ) {
		a[ i ] = ( Math.random() * 20 ) - 10;
	}
	return a;
}

function maxRelErr( x, y ) {
	var e = 0;
	var d;
	var s;
	var i;
	for ( i = 0; i < x.length; i++ ) {
		d = Math.abs( x[i] - y[i] );
		s = Math.abs( x[i] ) + Math.abs( y[i] ) + 1e-300;
		if ( d/s > e ) {
			e = d/s;
		}
	}
	return e;
}

// Build a strided-layout descriptor for an op(X) with logical dims rows x cols.
// layout: 'col' (s1=1,s2=ld), 'row' (s1=ld,s2=1). pad adds leading-dim padding,
// off adds an offset (sub-view).
function makeMat( rows, cols, layout, pad, off ) {
	var s1;
	var s2;
	var ld;
	var n;
	if ( layout === 'col' ) {
		ld = rows + pad;
		s1 = 1;
		s2 = ld;
		n = off + ( ld * cols );
	} else {
		ld = cols + pad;
		s1 = ld;
		s2 = 1;
		n = off + ( ld * rows );
	}
	return { 'data': randArray( n ), 's1': s1, 's2': s2, 'off': off };
}

function runCase( fn, c ) {
	// op(A) is M x K, op(B) is K x N, C is M x N.
	var nota = ( c.transa === 'no-transpose' );
	var notb = ( c.transb === 'no-transpose' );
	var aRows = nota ? c.M : c.K;
	var aCols = nota ? c.K : c.M;
	var bRows = notb ? c.K : c.N;
	var bCols = notb ? c.N : c.K;
	var A = makeMat( aRows, aCols, c.la, c.pad, c.off );
	var B = makeMat( bRows, bCols, c.lb, c.pad, c.off );
	var C = makeMat( c.M, c.N, c.lc, c.pad, c.off );
	var Cv = { 'data': C.data.slice(), 's1': C.s1, 's2': C.s2, 'off': C.off };

	ref( c.transa, c.transb, c.M, c.N, c.K, c.alpha, A.data, A.s1, A.s2, A.off, B.data, B.s1, B.s2, B.off, c.beta, C.data, C.s1, C.s2, C.off );
	fn( c.transa, c.transb, c.M, c.N, c.K, c.alpha, A.data, A.s1, A.s2, A.off, B.data, B.s1, B.s2, B.off, c.beta, Cv.data, Cv.s1, Cv.s2, Cv.off );
	return maxRelErr( C.data, Cv.data );
}

function check( fn ) {
	var transposes = [ 'no-transpose', 'transpose' ];
	var layouts = [ 'col', 'row' ];
	var shapes = [
		{ 'M': 1, 'N': 1, 'K': 1 },
		{ 'M': 2, 'N': 3, 'K': 4 },
		{ 'M': 7, 'N': 5, 'K': 6 },
		{ 'M': 9, 'N': 9, 'K': 9 },
		{ 'M': 13, 'N': 11, 'K': 17 },
		{ 'M': 1, 'N': 8, 'K': 5 },
		{ 'M': 8, 'N': 1, 'K': 5 },
		{ 'M': 32, 'N': 32, 'K': 32 }
	];
	var scalars = [
		{ 'alpha': 1.0, 'beta': 0.0 },
		{ 'alpha': 1.0, 'beta': 1.0 },
		{ 'alpha': 2.5, 'beta': -0.5 },
		{ 'alpha': 0.0, 'beta': 2.0 },
		{ 'alpha': -1.0, 'beta': 0.0 }
	];
	var worst = 0;
	var ncase = 0;
	var ta, tb, la, lb, lc, s, sh, pad, off;
	var e;
	for ( ta = 0; ta < transposes.length; ta++ ) {
	for ( tb = 0; tb < transposes.length; tb++ ) {
	for ( la = 0; la < layouts.length; la++ ) {
	for ( lb = 0; lb < layouts.length; lb++ ) {
	for ( lc = 0; lc < layouts.length; lc++ ) {
	for ( s = 0; s < scalars.length; s++ ) {
	for ( sh = 0; sh < shapes.length; sh++ ) {
		for ( pad = 0; pad <= 2; pad += 2 ) {
			off = ( pad === 0 ) ? 0 : 3;
			e = runCase( fn, {
				'transa': transposes[ta], 'transb': transposes[tb],
				'la': layouts[la], 'lb': layouts[lb], 'lc': layouts[lc],
				'M': shapes[sh].M, 'N': shapes[sh].N, 'K': shapes[sh].K,
				'alpha': scalars[s].alpha, 'beta': scalars[s].beta,
				'pad': pad, 'off': off
			} );
			ncase++;
			if ( e > worst ) {
				worst = e;
			}
		}
	}}}}}}}
	return { 'worst': worst, 'ncase': ncase, 'pass': worst <= TOL };
}

module.exports = { check, TOL };

// CLI: node check.js <variant-file ...>
if ( require.main === module ) {
	var files = process.argv.slice( 2 );
	if ( files.length === 0 ) {
		files = require( 'fs' ).readdirSync( __dirname + '/variants' )
			.filter( function ( f ) { return /\.js$/.test( f ) && f !== 'v0-reference.js'; } )
			.map( function ( f ) { return './variants/' + f; } );
	}
	files.forEach( function ( f ) {
		var fn = require( f.charAt(0) === '.' ? f : './' + f );
		var r = check( fn );
		var status = r.pass ? 'PASS' : 'FAIL';
		console.log( status + '  ' + f + '  worstRelErr=' + r.worst.toExponential(3) + '  cases=' + r.ncase );
	} );
}
