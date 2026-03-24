

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dtrsen = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dtrsen.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}

/**
* Convert column-major Fortran fixture array (4x4 with padding) to column-major Float64Array for N-by-N.
*/
function fromFortranColMajor( arr, N, LDA ) {
	var out = new Float64Array( N * N );
	var i;
	var j;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < N; i++ ) {
			out[ i + j * N ] = arr[ i + j * LDA ];
		}
	}
	return out;
}


// TESTS //

test( 'dtrsen: N V select scalar swap', function t() {
	// Select second eigenvalue to move it to position 0
	var N = 3;
	var T = new Float64Array([
		1.0, 0.0, 0.0,
		2.0, 3.0, 0.0,
		0.5, 1.0, 5.0
	]);
	var Q = new Float64Array( N * N );
	Q[ 0 ] = 1; Q[ 4 ] = 1; Q[ 8 ] = 1;
	var SELECT = new Uint8Array([ 0, 1, 0 ]);
	var WR = new Float64Array( N );
	var WI = new Float64Array( N );
	var M = new Float64Array( 1 );
	var s = new Float64Array( 1 );
	var sep = new Float64Array( 1 );
	var WORK = new Float64Array( 100 );
	var IWORK = new Int32Array( 100 );

	var info = dtrsen( 'N', 'V', SELECT, 1, 0, N, T, 1, N, 0, Q, 1, N, 0, WR, 1, 0, WI, 1, 0, M, s, sep, WORK, 1, 0, 100, IWORK, 1, 0, 100 );

	assert.equal( info, 0 );
	assert.equal( M[ 0 ], 1 );
	// Eigenvalue 3.0 should now be in leading position
	assertClose( WR[ 0 ], 3.0, 1e-13, 'WR[0]' );
	assertClose( WR[ 1 ], 1.0, 1e-13, 'WR[1]' );
	assertClose( WR[ 2 ], 5.0, 1e-13, 'WR[2]' );
});

test( 'dtrsen: all selected 2x2', function t() {
	var tc = findCase( 'all selected 2x2' );
	var N = 2;
	var T = fromFortranColMajor( [
		1.0, 0.0, 0.0, 0.0,
		2.0, 3.0, 0.0, 0.0,
		0.0, 0.0, 0.0, 0.0,
		0.0, 0.0, 0.0, 0.0
	], N, 4 );
	var Q = new Float64Array( N * N );
	Q[ 0 ] = 1; Q[ 3 ] = 1;
	var SELECT = new Uint8Array([ 1, 1 ]);
	var WR = new Float64Array( N );
	var WI = new Float64Array( N );
	var M = new Float64Array( 1 );
	var s = new Float64Array( 1 );
	var sep = new Float64Array( 1 );
	var WORK = new Float64Array( 100 );
	var IWORK = new Int32Array( 100 );

	var info = dtrsen( 'N', 'V', SELECT, 1, 0, N, T, 1, N, 0, Q, 1, N, 0, WR, 1, 0, WI, 1, 0, M, s, sep, WORK, 1, 0, 100, IWORK, 1, 0, 100 );

	assert.equal( info, tc.info );
	assert.equal( M[ 0 ], tc.M );
	assertArrayClose( Array.from( WR ), tc.WR.slice( 0, N ), 1e-13, 'WR' );
	assertArrayClose( Array.from( WI ), tc.WI.slice( 0, N ), 1e-13, 'WI' );
});

test( 'dtrsen: none selected', function t() {
	var tc = findCase( 'none selected' );
	var N = 2;
	var T = new Float64Array([ 1.0, 0.0, 2.0, 3.0 ]);
	var Q = new Float64Array([ 1.0, 0.0, 0.0, 1.0 ]);
	var SELECT = new Uint8Array([ 0, 0 ]);
	var WR = new Float64Array( N );
	var WI = new Float64Array( N );
	var M = new Float64Array( 1 );
	var s = new Float64Array( 1 );
	var sep = new Float64Array( 1 );
	var WORK = new Float64Array( 100 );
	var IWORK = new Int32Array( 100 );

	var info = dtrsen( 'N', 'V', SELECT, 1, 0, N, T, 1, N, 0, Q, 1, N, 0, WR, 1, 0, WI, 1, 0, M, s, sep, WORK, 1, 0, 100, IWORK, 1, 0, 100 );

	assert.equal( info, tc.info );
	assert.equal( M[ 0 ], tc.M );
});

test( 'dtrsen: E compute S', function t() {
	var tc = findCase( 'E compute S' );
	var N = 3;
	var T = fromFortranColMajor( [
		1.0, 0.0, 0.0, 0.0,
		2.0, 3.0, 0.0, 0.0,
		0.5, 1.0, 5.0, 0.0,
		0.0, 0.0, 0.0, 0.0
	], N, 4 );
	var Q = new Float64Array( N * N );
	Q[ 0 ] = 1; Q[ 4 ] = 1; Q[ 8 ] = 1;
	var SELECT = new Uint8Array([ 1, 0, 0 ]);
	var WR = new Float64Array( N );
	var WI = new Float64Array( N );
	var M = new Float64Array( 1 );
	var s = new Float64Array( 1 );
	var sep = new Float64Array( 1 );
	var WORK = new Float64Array( 100 );
	var IWORK = new Int32Array( 100 );

	var info = dtrsen( 'E', 'V', SELECT, 1, 0, N, T, 1, N, 0, Q, 1, N, 0, WR, 1, 0, WI, 1, 0, M, s, sep, WORK, 1, 0, 100, IWORK, 1, 0, 100 );

	assert.equal( info, tc.info );
	assert.equal( M[ 0 ], tc.M );
	assertClose( s[ 0 ], tc.S, 1e-13, 'S' );
});

test( 'dtrsen: V compute SEP', function t() {
	var tc = findCase( 'V compute SEP' );
	var N = 3;
	var T = fromFortranColMajor( [
		1.0, 0.0, 0.0, 0.0,
		2.0, 3.0, 0.0, 0.0,
		0.5, 1.0, 5.0, 0.0,
		0.0, 0.0, 0.0, 0.0
	], N, 4 );
	var Q = new Float64Array( N * N );
	Q[ 0 ] = 1; Q[ 4 ] = 1; Q[ 8 ] = 1;
	var SELECT = new Uint8Array([ 1, 0, 0 ]);
	var WR = new Float64Array( N );
	var WI = new Float64Array( N );
	var M = new Float64Array( 1 );
	var s = new Float64Array( 1 );
	var sep = new Float64Array( 1 );
	var WORK = new Float64Array( 100 );
	var IWORK = new Int32Array( 100 );

	var info = dtrsen( 'V', 'V', SELECT, 1, 0, N, T, 1, N, 0, Q, 1, N, 0, WR, 1, 0, WI, 1, 0, M, s, sep, WORK, 1, 0, 100, IWORK, 1, 0, 100 );

	assert.equal( info, tc.info );
	assert.equal( M[ 0 ], tc.M );
	assertClose( sep[ 0 ], tc.SEP, 1e-13, 'SEP' );
});

test( 'dtrsen: B compute both', function t() {
	var tc = findCase( 'B compute both' );
	var N = 3;
	var T = fromFortranColMajor( [
		1.0, 0.0, 0.0, 0.0,
		2.0, 3.0, 0.0, 0.0,
		0.5, 1.0, 5.0, 0.0,
		0.0, 0.0, 0.0, 0.0
	], N, 4 );
	var Q = new Float64Array( N * N );
	Q[ 0 ] = 1; Q[ 4 ] = 1; Q[ 8 ] = 1;
	var SELECT = new Uint8Array([ 1, 0, 0 ]);
	var WR = new Float64Array( N );
	var WI = new Float64Array( N );
	var M = new Float64Array( 1 );
	var s = new Float64Array( 1 );
	var sep = new Float64Array( 1 );
	var WORK = new Float64Array( 100 );
	var IWORK = new Int32Array( 100 );

	var info = dtrsen( 'B', 'V', SELECT, 1, 0, N, T, 1, N, 0, Q, 1, N, 0, WR, 1, 0, WI, 1, 0, M, s, sep, WORK, 1, 0, 100, IWORK, 1, 0, 100 );

	assert.equal( info, tc.info );
	assert.equal( M[ 0 ], tc.M );
	assertClose( s[ 0 ], tc.S, 1e-13, 'S' );
	assertClose( sep[ 0 ], tc.SEP, 1e-13, 'SEP' );
});

test( 'dtrsen: N=0', function t() {
	var tc = findCase( 'N=0' );
	var T = new Float64Array( 0 );
	var Q = new Float64Array( 0 );
	var SELECT = new Uint8Array( 0 );
	var WR = new Float64Array( 0 );
	var WI = new Float64Array( 0 );
	var M = new Float64Array( 1 );
	var s = new Float64Array( 1 );
	var sep = new Float64Array( 1 );
	var WORK = new Float64Array( 100 );
	var IWORK = new Int32Array( 100 );

	var info = dtrsen( 'N', 'V', SELECT, 1, 0, 0, T, 1, 0, 0, Q, 1, 0, 0, WR, 1, 0, WI, 1, 0, M, s, sep, WORK, 1, 0, 100, IWORK, 1, 0, 100 );

	assert.equal( info, tc.info );
	assert.equal( M[ 0 ], tc.M );
});

test( 'dtrsen: select complex pair (already in leading position)', function t() {
	// Test with complex pair already in leading position (no swap needed)
	var N = 4;
	var T = fromFortranColMajor( [
		4.0, -1.5, 0.0, 0.0,
		1.5, 4.0, 0.0, 0.0,
		0.3, 0.4, 1.0, 0.0,
		0.2, 0.1, 0.5, 2.0
	], N, N );
	var Q = new Float64Array( N * N );
	Q[ 0 ] = 1; Q[ 5 ] = 1; Q[ 10 ] = 1; Q[ 15 ] = 1;
	var SELECT = new Uint8Array([ 1, 1, 0, 0 ]);
	var WR = new Float64Array( N );
	var WI = new Float64Array( N );
	var M = new Float64Array( 1 );
	var s = new Float64Array( 1 );
	var sep = new Float64Array( 1 );
	var WORK = new Float64Array( 100 );
	var IWORK = new Int32Array( 100 );

	var info = dtrsen( 'N', 'V', SELECT, 1, 0, N, T, 1, N, 0, Q, 1, N, 0, WR, 1, 0, WI, 1, 0, M, s, sep, WORK, 1, 0, 100, IWORK, 1, 0, 100 );

	assert.equal( info, 0 );
	assert.equal( M[ 0 ], 2 );
	// Complex pair eigenvalues: 4 +/- 1.5i
	assertClose( WR[ 0 ], 4.0, 1e-13, 'WR[0]' );
	assertClose( WR[ 1 ], 4.0, 1e-13, 'WR[1]' );
	assertClose( Math.abs( WI[ 0 ] ), 1.5, 1e-13, 'WI[0]' );
	assertClose( Math.abs( WI[ 1 ] ), 1.5, 1e-13, 'WI[1]' );
});
