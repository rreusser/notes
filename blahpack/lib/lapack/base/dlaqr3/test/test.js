/* eslint-disable max-len */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dlaqr3 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlaqr3.jsonl' ), 'utf8' ).trim().split( '\n' );
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
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}

/**
* Extract NxN submatrix from column-major array with leading dimension LDA,
* returning a column-major Float64Array with leading dimension N.
*/
function extractMatrix( arr, N, LDA ) {
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

/**
* Run dlaqr3 with test parameters.
* Matrices are passed in column-major order. Returns { ns, nd, H, Z, SR, SI }.
*/
function runDlaqr3( wantt, wantz, N, ktop, kbot, nw, Hin, iloz, ihiz ) {
	var MAXN = 10;
	var H = new Float64Array( MAXN * N );
	var Z = new Float64Array( MAXN * N );
	var SR = new Float64Array( N );
	var SI = new Float64Array( N );
	var V = new Float64Array( MAXN * MAXN );
	var T = new Float64Array( MAXN * MAXN );
	var WV = new Float64Array( MAXN * MAXN );
	var WORK = new Float64Array( 200 );
	var nh = N;
	var nv = N;
	var lwork = 200;
	var result;
	var i;

	// Copy input H (column-major with LDH=MAXN -> stride1=1, stride2=MAXN -> but we use N as LDH)
	// We store H with LDH=N (stride1=1, stride2=N) for simplicity
	for ( i = 0; i < Hin.length && i < N * N; i++ ) {
		H[ i ] = Hin[ i ];
	}

	// Z = I
	for ( i = 0; i < N; i++ ) {
		Z[ i + i * N ] = 1.0;
	}

	result = dlaqr3( wantt, wantz, N, ktop, kbot, nw, H, 1, N, 0, iloz, ihiz, Z, 1, N, 0, SR, 1, 0, SI, 1, 0, V, 1, N, 0, nh, T, 1, N, 0, nv, WV, 1, N, 0, WORK, 1, 0, lwork );

	return {
		'ns': result.ns,
		'nd': result.nd,
		'H': H,
		'Z': Z,
		'SR': SR,
		'SI': SI
	};
}


// TESTS //

test( 'dlaqr3: 6x6 hessenberg NW=3', function t() {
	var tc = findCase( '6x6 hessenberg NW=3' );
	var N = 6;
	var MAXN = 10;
	// Extract N-by-N submatrix from fixture (stored with LDA=MAXN)
	var Hin = extractMatrix( tc.H, N, MAXN );
	var result = runDlaqr3( true, true, N, 1, 6, 3, Hin, 1, 6 );

	assert.equal( result.ns, tc.ns, 'ns' );
	assert.equal( result.nd, tc.nd, 'nd' );

	// Compare SR and SI (the eigenvalue arrays)
	var Hexp = extractMatrix( tc.H, N, MAXN );
	assertArrayClose( result.SR, new Float64Array( tc.SR ), 1e-10, 'SR' );
	assertArrayClose( result.SI, new Float64Array( tc.SI ), 1e-10, 'SI' );
});

test( 'dlaqr3: 4x4 hessenberg NW=2', function t() {
	var tc = findCase( '4x4 hessenberg NW=2' );
	var N = 4;
	var MAXN = 10;
	var Hin = extractMatrix( tc.H, N, MAXN );
	var result = runDlaqr3( true, true, N, 1, 4, 2, Hin, 1, 4 );

	assert.equal( result.ns, tc.ns, 'ns' );
	assert.equal( result.nd, tc.nd, 'nd' );
	assertArrayClose( result.SR, new Float64Array( tc.SR ), 1e-10, 'SR' );
	assertArrayClose( result.SI, new Float64Array( tc.SI ), 1e-10, 'SI' );
});

test( 'dlaqr3: 4x4 hessenberg NW=1', function t() {
	var tc = findCase( '4x4 hessenberg NW=1' );
	var N = 4;
	var MAXN = 10;
	// The input H is the same as test 2 (rebuilt from original values)
	var H0 = new Float64Array( N * N );
	H0[ 0 + 0 * N ] = 5.0; H0[ 0 + 1 * N ] = 2.0; H0[ 0 + 2 * N ] = 0.3; H0[ 0 + 3 * N ] = 0.1;
	H0[ 1 + 0 * N ] = 1.0; H0[ 1 + 1 * N ] = 4.0; H0[ 1 + 2 * N ] = 1.5; H0[ 1 + 3 * N ] = 0.2;
	H0[ 2 + 1 * N ] = 0.8; H0[ 2 + 2 * N ] = 3.0; H0[ 2 + 3 * N ] = 1.0;
	H0[ 3 + 2 * N ] = 0.5; H0[ 3 + 3 * N ] = 2.0;

	var result = runDlaqr3( true, false, N, 1, 4, 1, H0, 1, 4 );

	assert.equal( result.ns, tc.ns, 'ns' );
	assert.equal( result.nd, tc.nd, 'nd' );
	assertArrayClose( result.SR, new Float64Array( tc.SR ), 1e-10, 'SR' );
	assertArrayClose( result.SI, new Float64Array( tc.SI ), 1e-10, 'SI' );
});

test( 'dlaqr3: empty active block (ktop > kbot)', function t() {
	var tc = findCase( 'empty active block' );
	var N = 4;
	var H = new Float64Array( N * N );
	var Z = new Float64Array( N * N );
	var SR = new Float64Array( N );
	var SI = new Float64Array( N );
	var V = new Float64Array( N * N );
	var T = new Float64Array( N * N );
	var WV = new Float64Array( N * N );
	var WORK = new Float64Array( 200 );

	var result = dlaqr3( true, false, N, 3, 2, 2, H, 1, N, 0, 1, 4, Z, 1, N, 0, SR, 1, 0, SI, 1, 0, V, 1, N, 0, N, T, 1, N, 0, N, WV, 1, N, 0, WORK, 1, 0, 200 );

	assert.equal( result.ns, tc.ns, 'ns' );
	assert.equal( result.nd, tc.nd, 'nd' );
});

test( 'dlaqr3: empty deflation window (nw < 1)', function t() {
	var tc = findCase( 'empty deflation window' );
	var N = 4;
	var H = new Float64Array( N * N );
	var Z = new Float64Array( N * N );
	var SR = new Float64Array( N );
	var SI = new Float64Array( N );
	var V = new Float64Array( N * N );
	var T = new Float64Array( N * N );
	var WV = new Float64Array( N * N );
	var WORK = new Float64Array( 200 );

	var result = dlaqr3( true, false, N, 1, 4, 0, H, 1, N, 0, 1, 4, Z, 1, N, 0, SR, 1, 0, SI, 1, 0, V, 1, N, 0, N, T, 1, N, 0, N, WV, 1, N, 0, WORK, 1, 0, 200 );

	assert.equal( result.ns, tc.ns, 'ns' );
	assert.equal( result.nd, tc.nd, 'nd' );
});
