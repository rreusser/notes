/* eslint-disable max-len, stdlib/first-unit-test, no-restricted-syntax, max-statements-per-line, require-jsdoc, stdlib/jsdoc-private-annotation, stdlib/require-globals */

'use strict';

// MODULES //

var readFileSync = require( 'fs' ).readFileSync; // eslint-disable-line node/no-sync
var path = require( 'path' );
var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zhbgst = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zhbgst.jsonl' ), 'utf8' ).trim().split( '\n' );
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
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}

/**
* Runs a zhbgst test case from fixture data.
*
* @private
* @param {string} name - fixture case name
* @param {string} vect - 'none' or 'update'
* @param {string} uplo - 'upper' or 'lower'
* @param {integer} N - matrix order
* @param {integer} ka - half-bandwidth of A
* @param {integer} kb - half-bandwidth of B
* @param {integer} ldab - leading dimension of AB
* @param {integer} ldbb - leading dimension of BB
* @param {integer} ldx - leading dimension of X
* @param {number} tol - tolerance for comparison
*/
function runTest( name, vect, uplo, N, ka, kb, ldab, ldbb, ldx, tol ) {
	var RWORK;
	var WORK;
	var info;
	var tc;
	var AB;
	var BB;
	var Xm;
	var av;
	var xv;

	tc = findCase( name );

	AB = new Complex128Array( tc.AB_in );
	BB = new Complex128Array( tc.BB );
	Xm = new Complex128Array( N * ldx );
	WORK = new Complex128Array( N );
	RWORK = new Float64Array( N );

	info = zhbgst( vect, uplo, N, ka, kb, AB, 1, ldab, 0, BB, 1, ldbb, 0, Xm, 1, ldx, 0, WORK, 1, 0, RWORK, 1, 0 );

	assert.equal( info, tc.info, 'info' );

	av = new Float64Array( reinterpret( AB, 0 ) );
	assertArrayClose( av, tc.AB, tol, 'AB' );

	if ( tc.X ) {
		xv = new Float64Array( reinterpret( Xm, 0 ) );
		assertArrayClose( xv, tc.X, tol, 'X' );
	}
}


// TESTS //

test( 'zhbgst: main export is a function', function t() {
	assert.strictEqual( typeof zhbgst, 'function' );
});

test( 'zhbgst: upper_n5_ka2_kb1_none', function t() {
	runTest( 'upper_n5_ka2_kb1_none', 'none', 'upper', 5, 2, 1, 3, 2, 1, 1e-13 );
});

test( 'zhbgst: lower_n5_ka2_kb1_none', function t() {
	runTest( 'lower_n5_ka2_kb1_none', 'none', 'lower', 5, 2, 1, 3, 2, 1, 1e-13 );
});

test( 'zhbgst: upper_n5_ka2_kb1_vect', function t() {
	runTest( 'upper_n5_ka2_kb1_vect', 'update', 'upper', 5, 2, 1, 3, 2, 5, 1e-13 );
});

test( 'zhbgst: lower_n5_ka2_kb1_vect', function t() {
	runTest( 'lower_n5_ka2_kb1_vect', 'update', 'lower', 5, 2, 1, 3, 2, 5, 1e-13 );
});

test( 'zhbgst: n_zero quick return', function t() {
	var RWORK;
	var WORK;
	var info;
	var AB;
	var BB;
	var X;

	RWORK = new Float64Array( 1 );
	WORK = new Complex128Array( 1 );
	AB = new Complex128Array( 2 );
	BB = new Complex128Array( 1 );
	X = new Complex128Array( 1 );

	info = zhbgst( 'none', 'upper', 0, 1, 0, AB, 1, 2, 0, BB, 1, 1, 0, X, 1, 1, 0, WORK, 1, 0, RWORK, 1, 0 );
	assert.equal( info, 0, 'info' );
});

test( 'zhbgst: upper_n4_ka1_kb1', function t() {
	runTest( 'upper_n4_ka1_kb1', 'none', 'upper', 4, 1, 1, 2, 2, 1, 1e-13 );
});

test( 'zhbgst: upper_n3_ka0_kb0 (diagonal)', function t() {
	runTest( 'upper_n3_ka0_kb0', 'none', 'upper', 3, 0, 0, 1, 1, 1, 1e-13 );
});

test( 'zhbgst: upper_n8_ka3_kb2_vect', function t() {
	runTest( 'upper_n8_ka3_kb2_vect', 'update', 'upper', 8, 3, 2, 4, 3, 8, 1e-12 );
});

test( 'zhbgst: lower_n8_ka3_kb2_vect', function t() {
	runTest( 'lower_n8_ka3_kb2_vect', 'update', 'lower', 8, 3, 2, 4, 3, 8, 1e-12 );
});

test( 'zhbgst: ndarray export exists', function t() {
	var main = require( './../lib' );
	assert.strictEqual( typeof main, 'function' );
	assert.strictEqual( typeof main.ndarray, 'function' );
});
