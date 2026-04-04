/* eslint-disable no-restricted-syntax, stdlib/first-unit-test, max-lines */

'use strict';

// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync; // eslint-disable-line node/no-sync
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zhpgvx = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'zhpgvx.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line max-len
var fixture = lines.map( function parse( line ) {
	return JSON.parse( line );
} );


// FUNCTIONS //

/**
* Returns a test case from the fixture data.
*
* @private
* @param {string} name - test case name
* @returns {Object} test case data
*/
function findCase( name ) {
	return fixture.find( function find( t ) {
		return t.name === name;
	} );
}

/**
* Asserts that two numbers are approximately equal.
*
* @private
* @param {number} actual - actual value
* @param {number} expected - expected value
* @param {number} tol - tolerance
* @param {string} msg - assertion message
*/
function assertClose( actual, expected, tol, msg ) {
	var relErr;
	var denom;

	denom = Math.max( Math.abs( expected ), 1.0 );
	relErr = Math.abs( actual - expected ) / denom;
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual ); // eslint-disable-line max-len
}

/**
* Asserts that two arrays are element-wise approximately equal.
*
* @private
* @param {Array} actual - actual value
* @param {Array} expected - expected value
* @param {number} tol - tolerance
* @param {string} msg - assertion message
*/
function assertArrayClose( actual, expected, tol, msg ) {
	var i;

	assert.equal( actual.length, expected.length, msg + ' length' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}

/**
* Converts a typed array slice to a plain array.
*
* @private
* @param {TypedArray} arr - input array
* @param {NonNegativeInteger} start - start index
* @param {NonNegativeInteger} end - end index
* @returns {Array} output array
*/
function toArray( arr, start, end ) {
	var out = [];
	var i;

	for ( i = start; i < end; i++ ) {
		out.push( arr[ i ] );
	}
	return out;
}

/**
* Extracts complex magnitudes from interleaved re/im float64 array.
*
* @private
* @param {Array} arr - interleaved re/im values
* @returns {Array} array of magnitudes
*/
function complexMagnitudes( arr ) {
	var out = [];
	var i;

	for ( i = 0; i < arr.length; i += 2 ) {
		out.push( Math.sqrt( ( arr[ i ] * arr[ i ] ) + ( arr[ i + 1 ] * arr[ i + 1 ] ) ) ); // eslint-disable-line max-len
	}
	return out;
}

/**
* Runs zhpgvx with standard workspace allocation.
*
* @private
* @param {integer} itype - problem type
* @param {string} jobz - job type
* @param {string} range - range type
* @param {string} uplo - triangle type
* @param {NonNegativeInteger} N - matrix order
* @param {Complex128Array} AP - packed matrix A
* @param {Complex128Array} BP - packed matrix B
* @param {number} vl - lower value bound
* @param {number} vu - upper value bound
* @param {integer} il - lower index bound
* @param {integer} iu - upper index bound
* @param {number} abstol - convergence tolerance
* @returns {Object} result with info, M, w, Z, Zv, IFAIL
*/
function runZhpgvx( itype, jobz, range, uplo, N, AP, BP, vl, vu, il, iu, abstol ) { // eslint-disable-line max-len, max-params
	var IWORK;
	var IFAIL;
	var RWORK;
	var WORK;
	var info;
	var out;
	var Zv;
	var w;
	var Z;

	WORK = new Complex128Array( Math.max( 256, (2 * N) + 100 ) );
	RWORK = new Float64Array( Math.max( 256, (7 * N) + 100 ) );
	IWORK = new Int32Array( (5 * N) + 10 );
	IFAIL = new Int32Array( N + 1 );
	w = new Float64Array( N );
	Z = new Complex128Array( N * N );
	Zv = reinterpret( Z, 0 );
	out = {
		'M': 0
	};

	info = zhpgvx( itype, jobz, range, uplo, N, AP, 1, 0, BP, 1, 0, vl, vu, il, iu, abstol, out, w, 1, 0, Z, 1, N, 0, WORK, 1, 0, RWORK, 1, 0, IWORK, 1, 0, IFAIL, 1, 0 ); // eslint-disable-line max-len
	return {
		'info': info,
		'M': out.M,
		'w': w,
		'Z': Z,
		'Zv': Zv,
		'IFAIL': IFAIL
	};
}

// Hermitian 3x3:
// A = [4     1-i    2   ]
//     [1+i   5     3-i  ]
//     [2     3+i    6   ]

// B = [4     1-i    0   ]
//     [1+i   5      i   ]
//     [0     -i     3   ]

/**
* Returns A in upper packed format (3x3, interleaved re/im).
*
* @private
* @returns {Complex128Array} packed upper A
*/
function makeAPUpper3( ) {
	return new Complex128Array( [ 4, 0, 1, -1, 5, 0, 2, 0, 3, -1, 6, 0 ] );
}

/**
* Returns A in lower packed format (3x3, interleaved re/im).
*
* @private
* @returns {Complex128Array} packed lower A
*/
function makeAPLower3( ) {
	return new Complex128Array( [ 4, 0, 1, 1, 2, 0, 5, 0, 3, 1, 6, 0 ] );
}

/**
* Returns B in upper packed format (3x3, interleaved re/im).
*
* @private
* @returns {Complex128Array} packed upper B
*/
function makeBPUpper3( ) {
	return new Complex128Array( [ 4, 0, 1, -1, 5, 0, 0, 0, 0, 1, 3, 0 ] );
}

/**
* Returns B in lower packed format (3x3, interleaved re/im).
*
* @private
* @returns {Complex128Array} packed lower B
*/
function makeBPLower3( ) {
	return new Complex128Array( [ 4, 0, 1, 1, 0, 0, 5, 0, 0, -1, 3, 0 ] );
}


// TESTS //

test( 'zhpgvx: itype1, V, A, U (all eigenvalues + vectors, upper)', function t() { // eslint-disable-line max-len
	var tc;
	var r;

	tc = findCase( 'itype1_V_A_U' );
	r = runZhpgvx( 1, 'compute-vectors', 'all', 'upper', 3, makeAPUpper3(), makeBPUpper3(), 0, 0, 0, 0, 0 ); // eslint-disable-line max-len
	assert.equal( r.info, tc.info );
	assert.equal( r.M, tc.M );
	assertArrayClose( toArray( r.w, 0, r.M ), tc.w, 1e-13, 'w' );
	assertArrayClose( complexMagnitudes( toArray( r.Zv, 0, 2 * 3 * r.M ) ), complexMagnitudes( tc.Z ), 1e-12, 'Z' ); // eslint-disable-line max-len
});

test( 'zhpgvx: itype1, V, A, L (all eigenvalues + vectors, lower)', function t() { // eslint-disable-line max-len
	var tc;
	var r;

	tc = findCase( 'itype1_V_A_L' );
	r = runZhpgvx( 1, 'compute-vectors', 'all', 'lower', 3, makeAPLower3(), makeBPLower3(), 0, 0, 0, 0, 0 ); // eslint-disable-line max-len
	assert.equal( r.info, tc.info );
	assert.equal( r.M, tc.M );
	assertArrayClose( toArray( r.w, 0, r.M ), tc.w, 1e-13, 'w' );
	assertArrayClose( complexMagnitudes( toArray( r.Zv, 0, 2 * 3 * r.M ) ), complexMagnitudes( tc.Z ), 1e-12, 'Z' ); // eslint-disable-line max-len
});

test( 'zhpgvx: itype1, N, A, L (eigenvalues only)', function t() {
	var tc;
	var r;

	tc = findCase( 'itype1_N_A_L' );
	r = runZhpgvx( 1, 'no-vectors', 'all', 'lower', 3, makeAPLower3(), makeBPLower3(), 0, 0, 0, 0, 0 ); // eslint-disable-line max-len
	assert.equal( r.info, tc.info );
	assert.equal( r.M, tc.M );
	assertArrayClose( toArray( r.w, 0, r.M ), tc.w, 1e-13, 'w' );
});

test( 'zhpgvx: itype1, V, V, U (value range)', function t() {
	var tc;
	var r;

	tc = findCase( 'itype1_V_V_U' );
	r = runZhpgvx( 1, 'compute-vectors', 'value', 'upper', 3, makeAPUpper3(), makeBPUpper3(), 0.5, 1.5, 0, 0, 0 ); // eslint-disable-line max-len
	assert.equal( r.info, tc.info );
	assert.equal( r.M, tc.M );
	assertArrayClose( toArray( r.w, 0, r.M ), tc.w, 1e-13, 'w' );
	assertArrayClose( complexMagnitudes( toArray( r.Zv, 0, 2 * 3 * r.M ) ), complexMagnitudes( tc.Z ), 1e-12, 'Z' ); // eslint-disable-line max-len
});

test( 'zhpgvx: itype1, V, I, L (index range)', function t() {
	var tc;
	var r;

	tc = findCase( 'itype1_V_I_L' );
	r = runZhpgvx( 1, 'compute-vectors', 'index', 'lower', 3, makeAPLower3(), makeBPLower3(), 0, 0, 1, 2, 0 ); // eslint-disable-line max-len
	assert.equal( r.info, tc.info );
	assert.equal( r.M, tc.M );
	assertArrayClose( toArray( r.w, 0, r.M ), tc.w, 1e-13, 'w' );
	assertArrayClose( complexMagnitudes( toArray( r.Zv, 0, 2 * 3 * r.M ) ), complexMagnitudes( tc.Z ), 1e-12, 'Z' ); // eslint-disable-line max-len
});

test( 'zhpgvx: itype2, V, A, U', function t() {
	var tc;
	var r;

	tc = findCase( 'itype2_V_A_U' );
	r = runZhpgvx( 2, 'compute-vectors', 'all', 'upper', 3, makeAPUpper3(), makeBPUpper3(), 0, 0, 0, 0, 0 ); // eslint-disable-line max-len
	assert.equal( r.info, tc.info );
	assert.equal( r.M, tc.M );
	assertArrayClose( toArray( r.w, 0, r.M ), tc.w, 1e-13, 'w' );
	assertArrayClose( complexMagnitudes( toArray( r.Zv, 0, 2 * 3 * r.M ) ), complexMagnitudes( tc.Z ), 1e-12, 'Z' ); // eslint-disable-line max-len
});

test( 'zhpgvx: itype3, V, A, L', function t() {
	var tc;
	var r;

	tc = findCase( 'itype3_V_A_L' );
	r = runZhpgvx( 3, 'compute-vectors', 'all', 'lower', 3, makeAPLower3(), makeBPLower3(), 0, 0, 0, 0, 0 ); // eslint-disable-line max-len
	assert.equal( r.info, tc.info );
	assert.equal( r.M, tc.M );
	assertArrayClose( toArray( r.w, 0, r.M ), tc.w, 1e-13, 'w' );
	assertArrayClose( complexMagnitudes( toArray( r.Zv, 0, 2 * 3 * r.M ) ), complexMagnitudes( tc.Z ), 1e-12, 'Z' ); // eslint-disable-line max-len
});

test( 'zhpgvx: itype3, V, I, U (index range)', function t() {
	var tc;
	var r;

	tc = findCase( 'itype3_V_I_U' );
	r = runZhpgvx( 3, 'compute-vectors', 'index', 'upper', 3, makeAPUpper3(), makeBPUpper3(), 0, 0, 2, 3, 0 ); // eslint-disable-line max-len
	assert.equal( r.info, tc.info );
	assert.equal( r.M, tc.M );
	assertArrayClose( toArray( r.w, 0, r.M ), tc.w, 1e-13, 'w' );
	assertArrayClose( complexMagnitudes( toArray( r.Zv, 0, 2 * 3 * r.M ) ), complexMagnitudes( tc.Z ), 1e-12, 'Z' ); // eslint-disable-line max-len
});

test( 'zhpgvx: itype1, N, V, U (eigenvalues only, value range)', function t() {
	var tc;
	var r;

	tc = findCase( 'itype1_N_V_U' );
	r = runZhpgvx( 1, 'no-vectors', 'value', 'upper', 3, makeAPUpper3(), makeBPUpper3(), 0.5, 1.5, 0, 0, 0 ); // eslint-disable-line max-len
	assert.equal( r.info, tc.info );
	assert.equal( r.M, tc.M );
	assertArrayClose( toArray( r.w, 0, r.M ), tc.w, 1e-13, 'w' );
});

test( 'zhpgvx: N=0 quick return', function t() {
	var IWORK;
	var IFAIL;
	var RWORK;
	var WORK;
	var info;
	var out;
	var AP;
	var BP;
	var w;
	var Z;

	AP = new Complex128Array( 1 );
	BP = new Complex128Array( 1 );
	w = new Float64Array( 1 );
	Z = new Complex128Array( 1 );
	WORK = new Complex128Array( 1 );
	RWORK = new Float64Array( 1 );
	IWORK = new Int32Array( 1 );
	IFAIL = new Int32Array( 1 );
	out = {
		'M': 0
	};
	info = zhpgvx( 1, 'compute-vectors', 'all', 'upper', 0, AP, 1, 0, BP, 1, 0, 0, 0, 0, 0, 0, out, w, 1, 0, Z, 1, 1, 0, WORK, 1, 0, RWORK, 1, 0, IWORK, 1, 0, IFAIL, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, 0 );
	assert.equal( out.M, 0 );
});

test( 'zhpgvx: N=1', function t() {
	var tc;
	var r;

	tc = findCase( 'n_one' );
	r = runZhpgvx( 1, 'compute-vectors', 'all', 'upper', 1, new Complex128Array( [ 6.0, 0.0 ] ), new Complex128Array( [ 2.0, 0.0 ] ), 0, 0, 0, 0, 0 ); // eslint-disable-line max-len
	assert.equal( r.info, tc.info );
	assert.equal( r.M, tc.M );
	assertArrayClose( toArray( r.w, 0, r.M ), tc.w, 1e-13, 'w' );
	assertArrayClose( complexMagnitudes( toArray( r.Zv, 0, 2 ) ), complexMagnitudes( tc.Z ), 1e-13, 'Z' ); // eslint-disable-line max-len
});

test( 'zhpgvx: non-positive definite B', function t() {
	var tc;
	var r;

	tc = findCase( 'not_posdef' );
	r = runZhpgvx( 1, 'compute-vectors', 'all', 'lower', 2, new Complex128Array( [ 1, 0, 0, 0, 1, 0 ] ), new Complex128Array( [ -1, 0, 0, 0, 1, 0 ] ), 0, 0, 0, 0, 0 ); // eslint-disable-line max-len
	assert.equal( r.info, tc.info );
});

test( 'zhpgvx: itype2, V, I, L (index range)', function t() {
	var tc;
	var r;

	tc = findCase( 'itype2_V_I_L' );
	r = runZhpgvx( 2, 'compute-vectors', 'index', 'lower', 3, makeAPLower3(), makeBPLower3(), 0, 0, 2, 3, 0 ); // eslint-disable-line max-len
	assert.equal( r.info, tc.info );
	assert.equal( r.M, tc.M );
	assertArrayClose( toArray( r.w, 0, r.M ), tc.w, 1e-13, 'w' );
	assertArrayClose( complexMagnitudes( toArray( r.Zv, 0, 2 * 3 * r.M ) ), complexMagnitudes( tc.Z ), 1e-12, 'Z' ); // eslint-disable-line max-len
});
