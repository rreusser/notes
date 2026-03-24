'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dtrevc3 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dtrevc3.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual + ' (relErr=' + relErr + ')' );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch (' + actual.length + ' vs ' + expected.length + ')' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}

/**
* Pack an MxN submatrix from a column-major LDA array.
*/
function packMatrix( A, LDA, M, N ) {
	var out = [];
	var i;
	var j;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			out.push( A[ i + j * LDA ] );
		}
	}
	return out;
}

/**
* Build the 4x4 quasi-triangular test matrix T.
* T = [ 1   0.5  0.2  0.1  ]
*     [ 0   2    0.3  0.15 ]
*     [ 0   0    3   -0.5  ]
*     [ 0   0    0.8  3    ]
*/
function buildT4() {
	var N = 4;
	var T = new Float64Array( N * N );
	T[ 0 + 0*N ] = 1.0; T[ 0 + 1*N ] = 0.5; T[ 0 + 2*N ] = 0.2; T[ 0 + 3*N ] = 0.1;
	T[ 1 + 1*N ] = 2.0; T[ 1 + 2*N ] = 0.3; T[ 1 + 3*N ] = 0.15;
	T[ 2 + 2*N ] = 3.0; T[ 2 + 3*N ] = -0.5;
	T[ 3 + 2*N ] = 0.8; T[ 3 + 3*N ] = 3.0;
	return T;
}

/**
* Build the 6x6 quasi-triangular test matrix T.
* 2x2 block at (0,0)-(1,1): eigenvalues ~ 1 +/- 0.6i
* Real eigenvalue at (2,2): 4.0
* Real eigenvalue at (3,3): 5.0
* 2x2 block at (4,4)-(5,5): eigenvalues ~ 7 +/- 0.5i
*/
function buildT6() {
	var N = 6;
	var T = new Float64Array( N * N );
	// 2x2 block at (0,0)-(1,1)
	T[ 0 + 0*N ] = 1.0;  T[ 0 + 1*N ] = 0.6;
	T[ 1 + 0*N ] = -0.6; T[ 1 + 1*N ] = 1.0;
	// Upper part from rows 0-1
	T[ 0 + 2*N ] = 0.2; T[ 0 + 3*N ] = 0.1; T[ 0 + 4*N ] = 0.3; T[ 0 + 5*N ] = 0.05;
	T[ 1 + 2*N ] = 0.15; T[ 1 + 3*N ] = 0.08; T[ 1 + 4*N ] = 0.12; T[ 1 + 5*N ] = 0.04;
	// Real eigenvalue at (2,2)
	T[ 2 + 2*N ] = 4.0;
	T[ 2 + 3*N ] = 0.5; T[ 2 + 4*N ] = 0.2; T[ 2 + 5*N ] = 0.1;
	// Real eigenvalue at (3,3)
	T[ 3 + 3*N ] = 5.0;
	T[ 3 + 4*N ] = 0.3; T[ 3 + 5*N ] = 0.15;
	// 2x2 block at (4,4)-(5,5)
	T[ 4 + 4*N ] = 7.0;  T[ 4 + 5*N ] = 0.5;
	T[ 5 + 4*N ] = -0.5; T[ 5 + 5*N ] = 7.0;
	return T;
}


// TESTS //

test( 'dtrevc3: right eigenvectors, all, 4x4', function t() {
	var tc = findCase( 'right all 4x4' );
	var N = 4;
	var T = buildT4();
	var VL = new Float64Array( N * N );
	var VR = new Float64Array( N * N );
	var SELECT = new Uint8Array( N );
	var WORK = new Float64Array( 3 * N );
	var M = 0;

	var info = dtrevc3( 'R', 'A', SELECT, 1, 0, N, T, 1, N, 0, VL, 1, N, 0, VR, 1, N, 0, N, M, WORK, 1, 0, 3 * N );

	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( Array.from( VR ), tc.VR, 1e-12, 'VR' );
});

test( 'dtrevc3: left eigenvectors, all, 4x4', function t() {
	var tc = findCase( 'left all 4x4' );
	var N = 4;
	var T = buildT4();
	var VL = new Float64Array( N * N );
	var VR = new Float64Array( N * N );
	var SELECT = new Uint8Array( N );
	var WORK = new Float64Array( 3 * N );
	var M = 0;

	var info = dtrevc3( 'L', 'A', SELECT, 1, 0, N, T, 1, N, 0, VL, 1, N, 0, VR, 1, N, 0, N, M, WORK, 1, 0, 3 * N );

	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( Array.from( VL ), tc.VL, 1e-12, 'VL' );
});

test( 'dtrevc3: both eigenvectors, all, 4x4', function t() {
	var tc = findCase( 'both all 4x4' );
	var N = 4;
	var T = buildT4();
	var VL = new Float64Array( N * N );
	var VR = new Float64Array( N * N );
	var SELECT = new Uint8Array( N );
	var WORK = new Float64Array( 3 * N );
	var M = 0;

	var info = dtrevc3( 'B', 'A', SELECT, 1, 0, N, T, 1, N, 0, VL, 1, N, 0, VR, 1, N, 0, N, M, WORK, 1, 0, 3 * N );

	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( Array.from( VR ), tc.VR, 1e-12, 'VR' );
	assertArrayClose( Array.from( VL ), tc.VL, 1e-12, 'VL' );
});

test( 'dtrevc3: right eigenvectors, selected, 4x4', function t() {
	var tc = findCase( 'right selected 4x4' );
	var N = 4;
	var M_out = tc.M;
	var T = buildT4();
	var VL = new Float64Array( N * N );
	var VR = new Float64Array( N * M_out );
	var SELECT = new Uint8Array( N );
	SELECT[ 0 ] = 1;  // eigenvalue 1
	SELECT[ 1 ] = 0;  // skip eigenvalue 2
	SELECT[ 2 ] = 1;  // complex pair
	SELECT[ 3 ] = 1;  // complex pair
	var WORK = new Float64Array( 3 * N );
	var M = 0;

	var info = dtrevc3( 'R', 'S', SELECT, 1, 0, N, T, 1, N, 0, VL, 1, N, 0, VR, 1, N, 0, N, M, WORK, 1, 0, 3 * N );

	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( Array.from( VR ), tc.VR, 1e-12, 'VR' );
});

test( 'dtrevc3: right backtransform, 4x4', function t() {
	var tc = findCase( 'right backtransform 4x4' );
	var N = 4;
	var T = buildT4();
	var VL = new Float64Array( N * N );
	var VR = new Float64Array( N * N );
	// VR starts as identity (Q)
	var i;
	for ( i = 0; i < N; i++ ) {
		VR[ i + i*N ] = 1.0;
	}
	var SELECT = new Uint8Array( N );
	var WORK = new Float64Array( 3 * N );
	var M = 0;

	var info = dtrevc3( 'R', 'B', SELECT, 1, 0, N, T, 1, N, 0, VL, 1, N, 0, VR, 1, N, 0, N, M, WORK, 1, 0, 3 * N );

	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( Array.from( VR ), tc.VR, 1e-12, 'VR' );
});

test( 'dtrevc3: right all real eigenvalues, 4x4', function t() {
	var tc = findCase( 'right all real 4x4' );
	var N = 4;
	var T = new Float64Array( N * N );
	T[ 0 + 0*N ] = 5.0; T[ 0 + 1*N ] = 1.0; T[ 0 + 2*N ] = 0.5; T[ 0 + 3*N ] = 0.2;
	T[ 1 + 1*N ] = 3.0; T[ 1 + 2*N ] = 0.8; T[ 1 + 3*N ] = 0.3;
	T[ 2 + 2*N ] = 1.0; T[ 2 + 3*N ] = 0.6;
	T[ 3 + 3*N ] = -1.0;
	var VL = new Float64Array( N * N );
	var VR = new Float64Array( N * N );
	var SELECT = new Uint8Array( N );
	var WORK = new Float64Array( 3 * N );
	var M = 0;

	var info = dtrevc3( 'R', 'A', SELECT, 1, 0, N, T, 1, N, 0, VL, 1, N, 0, VR, 1, N, 0, N, M, WORK, 1, 0, 3 * N );

	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( Array.from( VR ), tc.VR, 1e-12, 'VR' );
});

test( 'dtrevc3: right N=1', function t() {
	var tc = findCase( 'right N=1' );
	var N = 1;
	var T = new Float64Array([ 7.0 ]);
	var VL = new Float64Array( 1 );
	var VR = new Float64Array( 1 );
	var SELECT = new Uint8Array( 1 );
	var WORK = new Float64Array( 3 );
	var M = 0;

	var info = dtrevc3( 'R', 'A', SELECT, 1, 0, N, T, 1, N, 0, VL, 1, N, 0, VR, 1, N, 0, 1, M, WORK, 1, 0, 3 );

	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( Array.from( VR ), tc.VR, 1e-12, 'VR' );
});

test( 'dtrevc3: left selected complex 4x4', function t() {
	var tc = findCase( 'left selected complex 4x4' );
	var N = 4;
	var M_out = tc.M;
	var T = buildT4();
	var VL = new Float64Array( N * M_out );
	var VR = new Float64Array( N * N );
	var SELECT = new Uint8Array( N );
	SELECT[ 0 ] = 0;
	SELECT[ 1 ] = 0;
	SELECT[ 2 ] = 1;  // complex pair
	SELECT[ 3 ] = 1;
	var WORK = new Float64Array( 3 * N );
	var M = 0;

	var info = dtrevc3( 'L', 'S', SELECT, 1, 0, N, T, 1, N, 0, VL, 1, N, 0, VR, 1, N, 0, N, M, WORK, 1, 0, 3 * N );

	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( Array.from( VL ), tc.VL, 1e-12, 'VL' );
});

test( 'dtrevc3: left backtransform, 4x4', function t() {
	var tc = findCase( 'left backtransform 4x4' );
	var N = 4;
	var T = buildT4();
	var VL = new Float64Array( N * N );
	var VR = new Float64Array( N * N );
	var i;
	for ( i = 0; i < N; i++ ) {
		VL[ i + i*N ] = 1.0;
	}
	var SELECT = new Uint8Array( N );
	var WORK = new Float64Array( 3 * N );
	var M = 0;

	var info = dtrevc3( 'L', 'B', SELECT, 1, 0, N, T, 1, N, 0, VL, 1, N, 0, VR, 1, N, 0, N, M, WORK, 1, 0, 3 * N );

	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( Array.from( VL ), tc.VL, 1e-12, 'VL' );
});

test( 'dtrevc3: right all 6x6 mixed', function t() {
	var tc = findCase( 'right all 6x6 mixed' );
	var N = 6;
	var T = buildT6();
	var VL = new Float64Array( N * N );
	var VR = new Float64Array( N * N );
	var SELECT = new Uint8Array( N );
	var WORK = new Float64Array( 3 * N );
	var M = 0;

	var info = dtrevc3( 'R', 'A', SELECT, 1, 0, N, T, 1, N, 0, VL, 1, N, 0, VR, 1, N, 0, N, M, WORK, 1, 0, 3 * N );

	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( Array.from( VR ), tc.VR, 1e-12, 'VR' );
});

test( 'dtrevc3: left all 6x6 mixed', function t() {
	var tc = findCase( 'left all 6x6 mixed' );
	var N = 6;
	var T = buildT6();
	var VL = new Float64Array( N * N );
	var VR = new Float64Array( N * N );
	var SELECT = new Uint8Array( N );
	var WORK = new Float64Array( 3 * N );
	var M = 0;

	var info = dtrevc3( 'L', 'A', SELECT, 1, 0, N, T, 1, N, 0, VL, 1, N, 0, VR, 1, N, 0, N, M, WORK, 1, 0, 3 * N );

	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( Array.from( VL ), tc.VL, 1e-12, 'VL' );
});

test( 'dtrevc3: both backtransform 6x6', function t() {
	var tc = findCase( 'both backtransform 6x6' );
	var N = 6;
	var T = buildT6();
	var VL = new Float64Array( N * N );
	var VR = new Float64Array( N * N );
	var i;
	for ( i = 0; i < N; i++ ) {
		VR[ i + i*N ] = 1.0;
		VL[ i + i*N ] = 1.0;
	}
	var SELECT = new Uint8Array( N );
	var WORK = new Float64Array( 3 * N );
	var M = 0;

	var info = dtrevc3( 'B', 'B', SELECT, 1, 0, N, T, 1, N, 0, VL, 1, N, 0, VR, 1, N, 0, N, M, WORK, 1, 0, 3 * N );

	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( Array.from( VR ), tc.VR, 1e-12, 'VR' );
	assertArrayClose( Array.from( VL ), tc.VL, 1e-12, 'VL' );
});

test( 'dtrevc3: right selected real 6x6', function t() {
	var tc = findCase( 'right selected real 6x6' );
	var N = 6;
	var M_out = tc.M;
	var T = buildT6();
	var VL = new Float64Array( N * N );
	var VR = new Float64Array( N * M_out );
	var SELECT = new Uint8Array( N );
	SELECT[ 2 ] = 1;  // real eigenvalue at pos 3
	SELECT[ 3 ] = 1;  // real eigenvalue at pos 4
	var WORK = new Float64Array( 3 * N );
	var M = 0;

	var info = dtrevc3( 'R', 'S', SELECT, 1, 0, N, T, 1, N, 0, VL, 1, N, 0, VR, 1, N, 0, N, M, WORK, 1, 0, 3 * N );

	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( Array.from( VR ), tc.VR, 1e-12, 'VR' );
});

test( 'dtrevc3: left selected complex first 6x6', function t() {
	var tc = findCase( 'left selected complex first 6x6' );
	var N = 6;
	var M_out = tc.M;
	var T = buildT6();
	var VL = new Float64Array( N * M_out );
	var VR = new Float64Array( N * N );
	var SELECT = new Uint8Array( N );
	SELECT[ 0 ] = 1;  // first of complex pair
	SELECT[ 1 ] = 1;  // second of complex pair
	var WORK = new Float64Array( 3 * N );
	var M = 0;

	var info = dtrevc3( 'L', 'S', SELECT, 1, 0, N, T, 1, N, 0, VL, 1, N, 0, VR, 1, N, 0, N, M, WORK, 1, 0, 3 * N );

	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( Array.from( VL ), tc.VL, 1e-12, 'VL' );
});
