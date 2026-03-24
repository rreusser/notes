

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zgesvx = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zgesvx.jsonl' ), 'utf8' ).trim().split( '\n' );
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
* Complex matrix-vector multiply: b = A*x (col-major, interleaved re/im).
*/
function zmatvec( Adata, xdata, N ) {
	var b = new Float64Array( 2 * N );
	var are;
	var aim;
	var xre;
	var xim;
	var i;
	var j;
	for ( i = 0; i < N; i++ ) {
		for ( j = 0; j < N; j++ ) {
			are = Adata[ 2 * ( i + j * N ) ];
			aim = Adata[ 2 * ( i + j * N ) + 1 ];
			xre = xdata[ 2 * j ];
			xim = xdata[ 2 * j + 1 ];
			b[ 2 * i ] += are * xre - aim * xim;
			b[ 2 * i + 1 ] += are * xim + aim * xre;
		}
	}
	return b;
}

/**
* Complex matrix-conjugate-transpose-vector multiply: b = A^H * x.
*/
function zmatvecH( Adata, xdata, N ) {
	var b = new Float64Array( 2 * N );
	var are;
	var aim;
	var xre;
	var xim;
	var i;
	var j;
	for ( i = 0; i < N; i++ ) {
		for ( j = 0; j < N; j++ ) {
			are = Adata[ 2 * ( j + i * N ) ];
			aim = -Adata[ 2 * ( j + i * N ) + 1 ];
			xre = xdata[ 2 * j ];
			xim = xdata[ 2 * j + 1 ];
			b[ 2 * i ] += are * xre - aim * xim;
			b[ 2 * i + 1 ] += are * xim + aim * xre;
		}
	}
	return b;
}

// Map Fortran equed chars to long-form strings
var EQUED_MAP = { 'N': 'none', 'R': 'row', 'C': 'column', 'B': 'both' };

// 3x3 test matrix data (interleaved re/im, col-major)
var A_DATA = [
	4, 1, 1, -1, 0.5, 0.2,
	1, 0.5, 3, 2, 1, -0.5,
	0.5, 0.1, 1, 0.3, 2, 1
];

/**
* Create workspace arrays for zgesvx.
*/
function makeWork( n ) {
	return {
		WORK: new Complex128Array( 2 * Math.max( 1, n ) ),
		RWORK: new Float64Array( 2 * Math.max( 1, n ) )
	};
}


// TESTS //

test( 'zgesvx: fact_N_trans_N', function t() {
	var tc = findCase( 'fact_N_trans_N' );
	var n = 3;
	var nrhs = 1;
	var A = new Complex128Array( A_DATA.slice() );
	var AF = new Complex128Array( n * n );
	var IPIV = new Int32Array( n );
	var r = new Float64Array( n );
	var c = new Float64Array( n );
	var FERR = new Float64Array( nrhs );
	var BERR = new Float64Array( nrhs );
	var w = makeWork( n );

	// b = A * [1;1;1]
	var bData = zmatvec( A_DATA, [ 1, 0, 1, 0, 1, 0 ], n );
	var B = new Complex128Array( bData );
	var X = new Complex128Array( n * nrhs );

	var result = zgesvx( 'not-factored', 'no-transpose', n, nrhs, A, 1, n, 0, AF, 1, n, 0, IPIV, 1, 0, 'none', r, 1, 0, c, 1, 0, B, 1, n, 0, X, 1, n, 0, FERR, 1, 0, BERR, 1, 0, w.WORK, 1, 0, w.RWORK, 1, 0 );

	assert.equal( result.info, tc.info );
	assert.equal( result.equed, EQUED_MAP[ tc.equed ] );
	var Xv = reinterpret( X, 0 );
	assertArrayClose( Array.from( Xv ), tc.x, 1e-12, 'x' );
	assertClose( result.rcond, tc.rcond, 1e-6, 'rcond' );
	assertClose( result.rpvgrw, tc.rpvgrw, 1e-12, 'rpvgrw' );
});

test( 'zgesvx: fact_N_trans_C', function t() {
	var tc = findCase( 'fact_N_trans_C' );
	var n = 3;
	var nrhs = 1;
	var A = new Complex128Array( A_DATA.slice() );
	var AF = new Complex128Array( n * n );
	var IPIV = new Int32Array( n );
	var r = new Float64Array( n );
	var c = new Float64Array( n );
	var FERR = new Float64Array( nrhs );
	var BERR = new Float64Array( nrhs );
	var w = makeWork( n );

	// b = A^H * [1;1;1]
	var bData = zmatvecH( A_DATA, [ 1, 0, 1, 0, 1, 0 ], n );
	var B = new Complex128Array( bData );
	var X = new Complex128Array( n * nrhs );

	var result = zgesvx( 'not-factored', 'conjugate-transpose', n, nrhs, A, 1, n, 0, AF, 1, n, 0, IPIV, 1, 0, 'none', r, 1, 0, c, 1, 0, B, 1, n, 0, X, 1, n, 0, FERR, 1, 0, BERR, 1, 0, w.WORK, 1, 0, w.RWORK, 1, 0 );

	assert.equal( result.info, tc.info );
	assert.equal( result.equed, EQUED_MAP[ tc.equed ] );
	var Xv = reinterpret( X, 0 );
	assertArrayClose( Array.from( Xv ), tc.x, 1e-12, 'x' );
	assertClose( result.rcond, tc.rcond, 1e-6, 'rcond' );
});

test( 'zgesvx: fact_E (equilibrate)', function t() {
	var tc = findCase( 'fact_E' );
	var n = 3;
	var nrhs = 1;
	var A = new Complex128Array( [
		1e6, 0, 1, 0, 1, 0,
		1, 0, 1e-3, 0, 1, 0,
		1, 0, 1, 0, 1e3, 0
	] );
	var AF = new Complex128Array( n * n );
	var IPIV = new Int32Array( n );
	var r = new Float64Array( n );
	var c = new Float64Array( n );
	var FERR = new Float64Array( nrhs );
	var BERR = new Float64Array( nrhs );
	var w = makeWork( n );

	var B = new Complex128Array( [ 1e6 + 2, 0, 2.001, 0, 1.002e3, 0 ] );
	var X = new Complex128Array( n * nrhs );

	var result = zgesvx( 'equilibrate', 'no-transpose', n, nrhs, A, 1, n, 0, AF, 1, n, 0, IPIV, 1, 0, 'none', r, 1, 0, c, 1, 0, B, 1, n, 0, X, 1, n, 0, FERR, 1, 0, BERR, 1, 0, w.WORK, 1, 0, w.RWORK, 1, 0 );

	assert.equal( result.info, tc.info );
	assert.equal( result.equed, EQUED_MAP[ tc.equed ] );
	var Xv = reinterpret( X, 0 );
	assertArrayClose( Array.from( Xv ), tc.x, 1e-6, 'x' );
	assertClose( result.rcond, tc.rcond, 0.5, 'rcond' );
});

test( 'zgesvx: fact_F (pre-factored)', function t() {
	var tc = findCase( 'fact_F' );
	var n = 3;
	var nrhs = 1;
	var A = new Complex128Array( A_DATA.slice() );
	var AF = new Complex128Array( n * n );
	var IPIV = new Int32Array( n );
	var r = new Float64Array( n );
	var c = new Float64Array( n );
	var FERR = new Float64Array( nrhs );
	var BERR = new Float64Array( nrhs );
	var w = makeWork( n );

	// First factorize with FACT='N'
	var bData = zmatvec( A_DATA, [ 1, 0, 1, 0, 1, 0 ], n );
	var B = new Complex128Array( bData );
	var X = new Complex128Array( n * nrhs );

	zgesvx( 'not-factored', 'no-transpose', n, nrhs, A, 1, n, 0, AF, 1, n, 0, IPIV, 1, 0, 'none', r, 1, 0, c, 1, 0, B, 1, n, 0, X, 1, n, 0, FERR, 1, 0, BERR, 1, 0, w.WORK, 1, 0, w.RWORK, 1, 0 );

	// Now use FACT='F' with new RHS
	// Restore A
	A = new Complex128Array( A_DATA.slice() );
	// b = A * [2+1i; -1+1i; 0.5-0.5i]
	bData = zmatvec( A_DATA, [ 2, 1, -1, 1, 0.5, -0.5 ], n );
	B = new Complex128Array( bData );
	X = new Complex128Array( n * nrhs );

	var result = zgesvx( 'factored', 'no-transpose', n, nrhs, A, 1, n, 0, AF, 1, n, 0, IPIV, 1, 0, 'none', r, 1, 0, c, 1, 0, B, 1, n, 0, X, 1, n, 0, FERR, 1, 0, BERR, 1, 0, w.WORK, 1, 0, w.RWORK, 1, 0 );

	assert.equal( result.info, tc.info );
	assert.equal( result.equed, EQUED_MAP[ tc.equed ] );
	var Xv = reinterpret( X, 0 );
	assertArrayClose( Array.from( Xv ), tc.x, 1e-12, 'x' );
	assertClose( result.rcond, tc.rcond, 1e-6, 'rcond' );
});

test( 'zgesvx: singular', function t() {
	var tc = findCase( 'singular' );
	var n = 3;
	var nrhs = 1;
	var A = new Complex128Array( [
		1, 0, 2, 0, 3, 0,
		1, 0, 2, 0, 3, 0,
		1, 0, 2, 0, 3, 0
	] );
	var AF = new Complex128Array( n * n );
	var IPIV = new Int32Array( n );
	var r = new Float64Array( n );
	var c = new Float64Array( n );
	var FERR = new Float64Array( nrhs );
	var BERR = new Float64Array( nrhs );
	var w = makeWork( n );
	var B = new Complex128Array( [ 1, 0, 2, 0, 3, 0 ] );
	var X = new Complex128Array( n * nrhs );

	var result = zgesvx( 'not-factored', 'no-transpose', n, nrhs, A, 1, n, 0, AF, 1, n, 0, IPIV, 1, 0, 'none', r, 1, 0, c, 1, 0, B, 1, n, 0, X, 1, n, 0, FERR, 1, 0, BERR, 1, 0, w.WORK, 1, 0, w.RWORK, 1, 0 );

	assert.ok( result.info > 0, 'info > 0 for singular matrix' );
	assert.equal( result.rcond, tc.rcond );
});

test( 'zgesvx: n_zero', function t() {
	var A = new Complex128Array( 1 );
	var AF = new Complex128Array( 1 );
	var IPIV = new Int32Array( 1 );
	var r = new Float64Array( 1 );
	var c = new Float64Array( 1 );
	var B = new Complex128Array( 1 );
	var X = new Complex128Array( 1 );
	var FERR = new Float64Array( 1 );
	var BERR = new Float64Array( 1 );
	var WORK = new Complex128Array( 2 );
	var RWORK = new Float64Array( 2 );

	var result = zgesvx( 'not-factored', 'no-transpose', 0, 1, A, 1, 1, 0, AF, 1, 1, 0, IPIV, 1, 0, 'none', r, 1, 0, c, 1, 0, B, 1, 1, 0, X, 1, 1, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 );

	assert.equal( result.info, 0 );
	assert.equal( result.rcond, 1.0 );
	assert.equal( result.rpvgrw, 1.0 );
});

test( 'zgesvx: multi_rhs', function t() {
	var tc = findCase( 'multi_rhs' );
	var n = 3;
	var nrhs = 2;
	var A = new Complex128Array( A_DATA.slice() );
	var AF = new Complex128Array( n * n );
	var IPIV = new Int32Array( n );
	var r = new Float64Array( n );
	var c = new Float64Array( n );
	var FERR = new Float64Array( nrhs );
	var BERR = new Float64Array( nrhs );
	var w = makeWork( n );

	// RHS 1: b = A * [1;1;1]
	var b1 = zmatvec( A_DATA, [ 1, 0, 1, 0, 1, 0 ], n );
	// RHS 2: b = A * [2+1i; -1+1i; 0.5-0.5i]
	var b2 = zmatvec( A_DATA, [ 2, 1, -1, 1, 0.5, -0.5 ], n );

	var Bdata = new Float64Array( 2 * n * nrhs );
	var i;
	for ( i = 0; i < 2 * n; i++ ) {
		Bdata[ i ] = b1[ i ];
		Bdata[ 2 * n + i ] = b2[ i ];
	}
	var B = new Complex128Array( Bdata );
	var X = new Complex128Array( n * nrhs );

	var result = zgesvx( 'not-factored', 'no-transpose', n, nrhs, A, 1, n, 0, AF, 1, n, 0, IPIV, 1, 0, 'none', r, 1, 0, c, 1, 0, B, 1, n, 0, X, 1, n, 0, FERR, 1, 0, BERR, 1, 0, w.WORK, 1, 0, w.RWORK, 1, 0 );

	assert.equal( result.info, tc.info );
	var Xv = reinterpret( X, 0 );
	assertArrayClose( Array.from( Xv ), tc.x, 1e-12, 'x' );
	assertClose( result.rcond, tc.rcond, 1e-6, 'rcond' );
});

test( 'zgesvx: fact_E_trans_C', function t() {
	var tc = findCase( 'fact_E_trans_C' );
	var n = 3;
	var nrhs = 1;
	var A = new Complex128Array( [
		1e6, 0, 1, 0, 1, 0,
		1, 0, 1e-3, 0, 1, 0,
		1, 0, 1, 0, 1e3, 0
	] );
	var AF = new Complex128Array( n * n );
	var IPIV = new Int32Array( n );
	var r = new Float64Array( n );
	var c = new Float64Array( n );
	var FERR = new Float64Array( nrhs );
	var BERR = new Float64Array( nrhs );
	var w = makeWork( n );

	var B = new Complex128Array( [ 1e6 + 2, 0, 2.001, 0, 1.002e3, 0 ] );
	var X = new Complex128Array( n * nrhs );

	var result = zgesvx( 'equilibrate', 'conjugate-transpose', n, nrhs, A, 1, n, 0, AF, 1, n, 0, IPIV, 1, 0, 'none', r, 1, 0, c, 1, 0, B, 1, n, 0, X, 1, n, 0, FERR, 1, 0, BERR, 1, 0, w.WORK, 1, 0, w.RWORK, 1, 0 );

	assert.equal( result.info, tc.info );
	assert.equal( result.equed, EQUED_MAP[ tc.equed ] );
	var Xv = reinterpret( X, 0 );
	assertArrayClose( Array.from( Xv ), tc.x, 1e-6, 'x' );
	assertClose( result.rcond, tc.rcond, 0.5, 'rcond' );
});
