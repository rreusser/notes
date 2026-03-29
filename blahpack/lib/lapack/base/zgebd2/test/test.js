/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';


// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var zgebd2 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'zgebd2.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
var fixture = lines.map( function parse( line ) {
	return JSON.parse( line );
} );


// FUNCTIONS //

/**
* Returns a test case from the fixture data.
*
* @private
* @param {string} name - test case name
* @returns {*} result
*/
function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name;
	} );
}

/**
* Asserts that two numbers are approximately equal.
*
* @private
* @param {*} actual - actual value
* @param {*} expected - expected value
* @param {number} tol - tolerance
* @param {string} msg - assertion message
*/
function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 ); // eslint-disable-line max-len
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

/**
* Asserts that two arrays are element-wise approximately equal.
*
* @private
* @param {*} actual - actual value
* @param {*} expected - expected value
* @param {number} tol - tolerance
* @param {string} msg - assertion message
*/
function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}

/**
* Converts a typed array to a plain array.
*
* @private
* @param {TypedArray} arr - input array
* @returns {Array} output array
*/
function toArray( arr ) {
	var out = [];
	var i;
	for ( i = 0; i < arr.length; i++ ) {
		out.push( arr[ i ] );
	}
	return out;
}


// TESTS //

test( 'zgebd2: upper_4x3 (M > N, upper bidiagonal)', function t() {
	var TAUQ;
	var TAUP;
	var WORK;
	var info;
	var tc;
	var M;
	var N;
	var A;
	var d;
	var e;

	tc = findCase( 'upper_4x3' );
	M = 4;
	N = 3;
	A = new Complex128Array([
		1,
		2,
		3,
		4,
		5,
		6,
		7,
		8,
		9,
		1,
		2,
		3,
		4,
		5,
		6,
		7,
		8,
		9,
		1,
		2,
		3,
		4,
		5,
		6
	]);
	d = new Float64Array( 3 );
	e = new Float64Array( 2 );
	TAUQ = new Complex128Array( 3 );
	TAUP = new Complex128Array( 3 );
	WORK = new Complex128Array( Math.max( M, N ) );
	info = zgebd2( M, N, A, 1, M, 0, d, 1, 0, e, 1, 0, TAUQ, 1, 0, TAUP, 1, 0, WORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
	assertArrayClose( toArray( reinterpret( A, 0 ) ), tc.a, 1e-14, 'a' );
	assertArrayClose( toArray( d ), tc.d, 1e-14, 'd' );
	assertArrayClose( toArray( e ), tc.e, 1e-14, 'e' );
	assertArrayClose( toArray( reinterpret( TAUQ, 0 ) ), tc.tauq, 1e-14, 'tauq' );
	assertArrayClose( toArray( reinterpret( TAUP, 0 ) ), tc.taup, 1e-14, 'taup' );
});

test( 'zgebd2: square_3x3 (M >= N, upper bidiagonal)', function t() {
	var TAUQ;
	var TAUP;
	var WORK;
	var info;
	var tc;
	var M;
	var N;
	var A;
	var d;
	var e;

	tc = findCase( 'square_3x3' );
	M = 3;
	N = 3;
	A = new Complex128Array([
		1,
		1,
		2,
		-1,
		0,
		3,
		4,
		0,
		5,
		2,
		1,
		-1,
		3,
		1,
		0,
		4,
		2,
		2
	]);
	d = new Float64Array( 3 );
	e = new Float64Array( 2 );
	TAUQ = new Complex128Array( 3 );
	TAUP = new Complex128Array( 3 );
	WORK = new Complex128Array( Math.max( M, N ) );
	info = zgebd2( M, N, A, 1, M, 0, d, 1, 0, e, 1, 0, TAUQ, 1, 0, TAUP, 1, 0, WORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
	assertArrayClose( toArray( reinterpret( A, 0 ) ), tc.a, 1e-14, 'a' );
	assertArrayClose( toArray( d ), tc.d, 1e-14, 'd' );
	assertArrayClose( toArray( e ), tc.e, 1e-14, 'e' );
	assertArrayClose( toArray( reinterpret( TAUQ, 0 ) ), tc.tauq, 1e-14, 'tauq' );
	assertArrayClose( toArray( reinterpret( TAUP, 0 ) ), tc.taup, 1e-14, 'taup' );
});

test( 'zgebd2: lower_3x4 (M < N, lower bidiagonal)', function t() {
	var TAUQ;
	var TAUP;
	var WORK;
	var info;
	var tc;
	var M;
	var N;
	var A;
	var d;
	var e;

	tc = findCase( 'lower_3x4' );
	M = 3;
	N = 4;
	A = new Complex128Array([
		1,
		2,
		3,
		4,
		5,
		6,
		7,
		8,
		9,
		1,
		2,
		3,
		4,
		5,
		6,
		7,
		8,
		9,
		1,
		2,
		3,
		4,
		5,
		6
	]);
	d = new Float64Array( 3 );
	e = new Float64Array( 2 );
	TAUQ = new Complex128Array( 3 );
	TAUP = new Complex128Array( 3 );
	WORK = new Complex128Array( Math.max( M, N ) );
	info = zgebd2( M, N, A, 1, M, 0, d, 1, 0, e, 1, 0, TAUQ, 1, 0, TAUP, 1, 0, WORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
	assertArrayClose( toArray( reinterpret( A, 0 ) ), tc.a, 1e-14, 'a' );
	assertArrayClose( toArray( d ), tc.d, 1e-14, 'd' );
	assertArrayClose( toArray( e ), tc.e, 1e-14, 'e' );
	assertArrayClose( toArray( reinterpret( TAUQ, 0 ) ), tc.tauq, 1e-14, 'tauq' );
	assertArrayClose( toArray( reinterpret( TAUP, 0 ) ), tc.taup, 1e-14, 'taup' );
});

test( 'zgebd2: m_zero (quick return)', function t() {
	var TAUQ;
	var TAUP;
	var WORK;
	var info;
	var tc;
	var A;
	var d;
	var e;

	tc = findCase( 'm_zero' );
	A = new Complex128Array( 1 );
	d = new Float64Array( 1 );
	e = new Float64Array( 1 );
	TAUQ = new Complex128Array( 1 );
	TAUP = new Complex128Array( 1 );
	WORK = new Complex128Array( 3 );
	info = zgebd2( 0, 3, A, 1, 1, 0, d, 1, 0, e, 1, 0, TAUQ, 1, 0, TAUP, 1, 0, WORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
});

test( 'zgebd2: n_zero (quick return)', function t() {
	var TAUQ;
	var TAUP;
	var WORK;
	var info;
	var tc;
	var A;
	var d;
	var e;

	tc = findCase( 'n_zero' );
	A = new Complex128Array( 1 );
	d = new Float64Array( 1 );
	e = new Float64Array( 1 );
	TAUQ = new Complex128Array( 1 );
	TAUP = new Complex128Array( 1 );
	WORK = new Complex128Array( 3 );
	info = zgebd2( 3, 0, A, 1, 3, 0, d, 1, 0, e, 1, 0, TAUQ, 1, 0, TAUP, 1, 0, WORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
});

test( 'zgebd2: one_by_one', function t() {
	var TAUQ;
	var TAUP;
	var WORK;
	var info;
	var tc;
	var A;
	var d;
	var e;

	tc = findCase( 'one_by_one' );
	A = new Complex128Array([ 5, 3 ]);
	d = new Float64Array( 1 );
	e = new Float64Array( 1 );
	TAUQ = new Complex128Array( 1 );
	TAUP = new Complex128Array( 1 );
	WORK = new Complex128Array( 1 );
	info = zgebd2( 1, 1, A, 1, 1, 0, d, 1, 0, e, 1, 0, TAUQ, 1, 0, TAUP, 1, 0, WORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
	assertArrayClose( toArray( reinterpret( A, 0 ) ), tc.a, 1e-14, 'a' );
	assertArrayClose( toArray( d ), tc.d, 1e-14, 'd' );
	assertArrayClose( toArray( reinterpret( TAUQ, 0 ) ), tc.tauq, 1e-14, 'tauq' );
	assertArrayClose( toArray( reinterpret( TAUP, 0 ) ), tc.taup, 1e-14, 'taup' );
});

test( 'zgebd2: lower_2x3 (M < N, lower bidiagonal, small)', function t() {
	var TAUQ;
	var TAUP;
	var WORK;
	var info;
	var tc;
	var M;
	var N;
	var A;
	var d;
	var e;

	tc = findCase( 'lower_2x3' );
	M = 2;
	N = 3;
	A = new Complex128Array([
		1,
		0,
		0,
		1,
		2,
		1,
		1,
		-1,
		3,
		0,
		0,
		2
	]);
	d = new Float64Array( 2 );
	e = new Float64Array( 1 );
	TAUQ = new Complex128Array( 2 );
	TAUP = new Complex128Array( 2 );
	WORK = new Complex128Array( Math.max( M, N ) );
	info = zgebd2( M, N, A, 1, M, 0, d, 1, 0, e, 1, 0, TAUQ, 1, 0, TAUP, 1, 0, WORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
	assertArrayClose( toArray( reinterpret( A, 0 ) ), tc.a, 1e-14, 'a' );
	assertArrayClose( toArray( d ), tc.d, 1e-14, 'd' );
	assertArrayClose( toArray( e ), tc.e, 1e-14, 'e' );
	assertArrayClose( toArray( reinterpret( TAUQ, 0 ) ), tc.tauq, 1e-14, 'tauq' );
	assertArrayClose( toArray( reinterpret( TAUP, 0 ) ), tc.taup, 1e-14, 'taup' );
});
