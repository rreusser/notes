/* eslint-disable no-restricted-syntax, stdlib/first-unit-test, no-mixed-operators, max-lines, max-params, node/no-sync */

'use strict';

// MODULES //

var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dsbgvx = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dsbgvx.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line max-len
var fixture = lines.map( function parse( line ) { // eslint-disable-line max-len
	return JSON.parse( line );
});


// FUNCTIONS //

/**
* Returns a test case from the fixture data.
*
* @private
* @param {string} name - test case name
* @returns {(Object|void)} test case or undefined
*/
function findCase( name ) {
	return fixture.find( function find( t ) {
		return t.name === name;
	});
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
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual + ' (relErr=' + relErr + ')' ); // eslint-disable-line max-len
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
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}

/**
* Returns the upper-band-storage A matrix (N=5, KA=2, LDAB=3).
*
* @private
* @returns {Float64Array} band matrix A
*/
function bandAUpper5() {
	// Band A upper storage, Fortran column-major with LDAB=3
	return new Float64Array([
		0, 0, 10, 0, 1, 8, 0.5, 2, 6, 0.3, 1.5, 9, 0.4, 1, 7
	]);
}

/**
* Returns the lower-band-storage A matrix (N=5, KA=2, LDAB=3).
*
* @private
* @returns {Float64Array} band matrix A
*/
function bandALower5() {
	// Band A lower storage:
	return new Float64Array([
		10, 1, 0.5, 8, 2, 0.3, 6, 1.5, 0.4, 9, 1, 0, 7, 0, 0
	]);
}

/**
* Returns the upper-band-storage B matrix (N=5, KB=1, LDBB=2).
*
* @private
* @returns {Float64Array} band matrix B
*/
function bandBUpper5() {
	// Band B upper storage:
	return new Float64Array([
		0, 4, 0.2, 5, 0.3, 3, 0.1, 6, 0.2, 4
	]);
}

/**
* Returns the lower-band-storage B matrix (N=5, KB=1, LDBB=2).
*
* @private
* @returns {Float64Array} band matrix B
*/
function bandBLower5() {
	// Band B lower storage:
	return new Float64Array([
		4, 0.2, 5, 0.3, 3, 0.1, 6, 0.2, 4, 0
	]);
}

/**
* Runs dsbgvx with standard workspace allocation.
*
* @private
* @param {string} jobz - job type
* @param {string} range - range type
* @param {string} uplo - triangle
* @param {NonNegativeInteger} N - order
* @param {NonNegativeInteger} ka - bandwidth of A
* @param {NonNegativeInteger} kb - bandwidth of B
* @param {Float64Array} AB - band matrix A
* @param {Float64Array} BB - band matrix B
* @param {number} vl - lower bound
* @param {number} vu - upper bound
* @param {integer} il - lower index
* @param {integer} iu - upper index
* @param {number} abstol - tolerance
* @returns {Object} results
*/
function runDsbgvx( jobz, range, uplo, N, ka, kb, AB, BB, vl, vu, il, iu, abstol ) { // eslint-disable-line max-len
	var IWORK = new Int32Array( Math.max( (5 * N) + 10, 1 ) );
	var IFAIL = new Int32Array( Math.max( N, 1 ) );
	var WORK = new Float64Array( Math.max( (7 * N) + 100, 1 ) );
	var LDAB = ka + 1;
	var LDBB = kb + 1;
	var info;
	var out;
	var w = new Float64Array( Math.max( N, 1 ) );
	var Q = new Float64Array( Math.max( N * N, 1 ) );
	var Z = new Float64Array( Math.max( N * N, 1 ) );

	out = {
		'M': 0
	};
	info = dsbgvx( jobz, range, uplo, N, ka, kb, AB, 1, LDAB, 0, BB, 1, LDBB, 0, Q, 1, N, 0, vl, vu, il, iu, abstol, out, w, 1, 0, Z, 1, N, 0, WORK, 1, 0, IWORK, 1, 0, IFAIL, 1, 0 ); // eslint-disable-line max-len
	return {
		'info': info,
		'M': out.M,
		'w': w,
		'Z': Z,
		'Q': Q,
		'IFAIL': IFAIL
	};
}


// TESTS //

test( 'dsbgvx is a function', function t() {
	assert.equal( typeof dsbgvx, 'function' );
});

test( 'dsbgvx: V, A, U, N=5, KA=2, KB=1 (all eigenvalues + vectors)', function t() {
	var tc = findCase( 'V_A_U_n5_ka2_kb1' );
	var AB = bandAUpper5();
	var BB = bandBUpper5();
	var r = runDsbgvx( 'compute-vectors', 'all', 'upper', 5, 2, 1, AB, BB, 0, 0, 0, 0, 0 ); // eslint-disable-line max-len

	assert.equal( r.info, tc.info );
	assert.equal( r.M, tc.M );
	assertArrayClose( Array.prototype.slice.call( r.w, 0, r.M ), tc.W, 1e-13, 'W' ); // eslint-disable-line max-len
	assertArrayClose( Array.prototype.slice.call( r.Z, 0, 5 * r.M ), tc.Z, 1e-13, 'Z' ); // eslint-disable-line max-len
});

test( 'dsbgvx: N, A, U, N=5, KA=2, KB=1 (all eigenvalues, no vectors)', function t() {
	var tc = findCase( 'N_A_U_n5_ka2_kb1' );
	var AB = bandAUpper5();
	var BB = bandBUpper5();
	var r = runDsbgvx( 'no-vectors', 'all', 'upper', 5, 2, 1, AB, BB, 0, 0, 0, 0, 0 ); // eslint-disable-line max-len

	assert.equal( r.info, tc.info );
	assert.equal( r.M, tc.M );
	assertArrayClose( Array.prototype.slice.call( r.w, 0, r.M ), tc.W, 1e-13, 'W' ); // eslint-disable-line max-len
});

test( 'dsbgvx: V, A, L, N=5, KA=2, KB=1 (lower, all eigenvalues + vectors)', function t() {
	var tc = findCase( 'V_A_L_n5_ka2_kb1' );
	var AB = bandALower5();
	var BB = bandBLower5();
	var r = runDsbgvx( 'compute-vectors', 'all', 'lower', 5, 2, 1, AB, BB, 0, 0, 0, 0, 0 ); // eslint-disable-line max-len

	assert.equal( r.info, tc.info );
	assert.equal( r.M, tc.M );
	assertArrayClose( Array.prototype.slice.call( r.w, 0, r.M ), tc.W, 1e-13, 'W' ); // eslint-disable-line max-len
	assertArrayClose( Array.prototype.slice.call( r.Z, 0, 5 * r.M ), tc.Z, 1e-13, 'Z' ); // eslint-disable-line max-len
});

test( 'dsbgvx: V, V, U, N=5, KA=2, KB=1 (value range [1.0, 2.5])', function t() {
	var tc = findCase( 'V_V_U_n5_ka2_kb1' );
	var AB = bandAUpper5();
	var BB = bandBUpper5();
	var r = runDsbgvx( 'compute-vectors', 'value', 'upper', 5, 2, 1, AB, BB, 1.0, 2.5, 0, 0, 0 ); // eslint-disable-line max-len

	assert.equal( r.info, tc.info );
	assert.equal( r.M, tc.M );
	assertArrayClose( Array.prototype.slice.call( r.w, 0, r.M ), tc.W, 1e-13, 'W' ); // eslint-disable-line max-len
	assertArrayClose( Array.prototype.slice.call( r.Z, 0, 5 * r.M ), tc.Z, 1e-13, 'Z' ); // eslint-disable-line max-len
});

test( 'dsbgvx: V, I, U, N=5, KA=2, KB=1 (index range 2..4)', function t() {
	var tc = findCase( 'V_I_U_n5_ka2_kb1' );
	var AB = bandAUpper5();
	var BB = bandBUpper5();
	var r = runDsbgvx( 'compute-vectors', 'index', 'upper', 5, 2, 1, AB, BB, 0, 0, 2, 4, 0 ); // eslint-disable-line max-len

	assert.equal( r.info, tc.info );
	assert.equal( r.M, tc.M );
	assertArrayClose( Array.prototype.slice.call( r.w, 0, r.M ), tc.W, 1e-13, 'W' ); // eslint-disable-line max-len
	assertArrayClose( Array.prototype.slice.call( r.Z, 0, 5 * r.M ), tc.Z, 1e-13, 'Z' ); // eslint-disable-line max-len
});

test( 'dsbgvx: N, V, L, N=5, KA=2, KB=1 (value range, lower, no vectors)', function t() {
	var tc = findCase( 'N_V_L_n5_ka2_kb1' );
	var AB = bandALower5();
	var BB = bandBLower5();
	var r = runDsbgvx( 'no-vectors', 'value', 'lower', 5, 2, 1, AB, BB, 1.0, 2.5, 0, 0, 0 ); // eslint-disable-line max-len

	assert.equal( r.info, tc.info );
	assert.equal( r.M, tc.M );
	assertArrayClose( Array.prototype.slice.call( r.w, 0, r.M ), tc.W, 1e-13, 'W' ); // eslint-disable-line max-len
});

test( 'dsbgvx: N, I, L, N=5, KA=2, KB=1 (single eigenvalue, index 3)', function t() {
	var tc = findCase( 'N_I_L_n5_ka2_kb1' );
	var AB = bandALower5();
	var BB = bandBLower5();
	var r = runDsbgvx( 'no-vectors', 'index', 'lower', 5, 2, 1, AB, BB, 0, 0, 3, 3, 0 ); // eslint-disable-line max-len

	assert.equal( r.info, tc.info );
	assert.equal( r.M, tc.M );
	assertArrayClose( Array.prototype.slice.call( r.w, 0, r.M ), tc.W, 1e-13, 'W' ); // eslint-disable-line max-len
});

test( 'dsbgvx: N=0 (quick return)', function t() {
	var AB = new Float64Array( 1 );
	var BB = new Float64Array( 1 );
	var r = runDsbgvx( 'no-vectors', 'all', 'upper', 0, 1, 0, AB, BB, 0, 0, 0, 0, 0 ); // eslint-disable-line max-len

	assert.equal( r.info, 0 );
	assert.equal( r.M, 0 );
});

test( 'dsbgvx: N=1, V, A (trivial)', function t() {
	var tc = findCase( 'n1_V_A' );
	var AB = new Float64Array( [ 3.0 ] );
	var BB = new Float64Array( [ 2.0 ] );
	var r = runDsbgvx( 'compute-vectors', 'all', 'upper', 1, 0, 0, AB, BB, 0, 0, 0, 0, 0 ); // eslint-disable-line max-len

	assert.equal( r.info, tc.info );
	assert.equal( r.M, tc.M );
	assertArrayClose( Array.prototype.slice.call( r.w, 0, r.M ), tc.W, 1e-13, 'W' ); // eslint-disable-line max-len
	assertArrayClose( Array.prototype.slice.call( r.Z, 0, r.M ), tc.Z, 1e-13, 'Z' ); // eslint-disable-line max-len
});

test( 'dsbgvx: KA=KB=0, N=3 (diagonal matrices)', function t() {
	var tc = findCase( 'diag_n3' );
	var AB = new Float64Array( [ 5, 6, 7 ] );
	var BB = new Float64Array( [ 2, 3, 4 ] );
	var r = runDsbgvx( 'no-vectors', 'all', 'upper', 3, 0, 0, AB, BB, 0, 0, 0, 0, 0 ); // eslint-disable-line max-len

	assert.equal( r.info, tc.info );
	assert.equal( r.M, tc.M );
	assertArrayClose( Array.prototype.slice.call( r.w, 0, r.M ), tc.W, 1e-13, 'W' ); // eslint-disable-line max-len
});

test( 'dsbgvx: V, I, U, N=5, fast path (IL=1, IU=N)', function t() {
	var tc = findCase( 'V_I_U_n5_fast' );
	var AB = bandAUpper5();
	var BB = bandBUpper5();
	var r = runDsbgvx( 'compute-vectors', 'index', 'upper', 5, 2, 1, AB, BB, 0, 0, 1, 5, 0 ); // eslint-disable-line max-len

	assert.equal( r.info, tc.info );
	assert.equal( r.M, tc.M );
	assertArrayClose( Array.prototype.slice.call( r.w, 0, r.M ), tc.W, 1e-13, 'W' ); // eslint-disable-line max-len
	assertArrayClose( Array.prototype.slice.call( r.Z, 0, 5 * r.M ), tc.Z, 1e-13, 'Z' ); // eslint-disable-line max-len
});

test( 'dsbgvx: N, I, U, N=5, fast path (IL=1, IU=N, no vectors)', function t() {
	var tc = findCase( 'N_I_U_n5_fast' );
	var AB = bandAUpper5();
	var BB = bandBUpper5();
	var r = runDsbgvx( 'no-vectors', 'index', 'upper', 5, 2, 1, AB, BB, 0, 0, 1, 5, 0 ); // eslint-disable-line max-len

	assert.equal( r.info, tc.info );
	assert.equal( r.M, tc.M );
	assertArrayClose( Array.prototype.slice.call( r.w, 0, r.M ), tc.W, 1e-13, 'W' ); // eslint-disable-line max-len
});

test( 'dsbgvx: V, I, U, N=5, single eigenvalue (IL=IU=1)', function t() {
	var tc = findCase( 'V_I_U_n5_single' );
	var AB = bandAUpper5();
	var BB = bandBUpper5();
	var r = runDsbgvx( 'compute-vectors', 'index', 'upper', 5, 2, 1, AB, BB, 0, 0, 1, 1, 0 ); // eslint-disable-line max-len

	assert.equal( r.info, tc.info );
	assert.equal( r.M, tc.M );
	assertArrayClose( Array.prototype.slice.call( r.w, 0, r.M ), tc.W, 1e-13, 'W' ); // eslint-disable-line max-len
	assertArrayClose( Array.prototype.slice.call( r.Z, 0, 5 * r.M ), tc.Z, 1e-13, 'Z' ); // eslint-disable-line max-len
});
