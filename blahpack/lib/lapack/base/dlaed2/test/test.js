/* eslint-disable max-len, max-statements, function-call-argument-newline, function-paren-newline, max-statements-per-line, no-restricted-syntax, node/no-unsupported-features/es-builtins, no-mixed-operators, stdlib/first-unit-test, require-jsdoc, stdlib/jsdoc-private-annotation */

'use strict';

// MODULES //

var readFileSync = require( 'fs' ).readFileSync; // eslint-disable-line node/no-sync
var path = require( 'path' );
var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dlaed2 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlaed2.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

/**
* Find a test case by name.
*
* @private
* @param {string} name - test case name
* @returns {Object} test case fixture data
*/
function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

/**
* Assert that two numbers are close.
*
* @private
* @param {number} actual - actual value
* @param {number} expected - expected value
* @param {number} tol - relative tolerance
* @param {string} msg - assertion message
*/
function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

/**
* Assert that two arrays are element-wise close.
*
* @private
* @param {Array} actual - actual array
* @param {Array} expected - expected array
* @param {number} tol - relative tolerance
* @param {string} msg - assertion message
*/
function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i += 1 ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}

/**
* Calls dlaed2 with simple column-major layout.
*
* @private
* @param {integer} N - problem size
* @param {integer} n1 - first sub-problem size
* @param {Float64Array} d - eigenvalues
* @param {Float64Array} Q - eigenvectors (N*N column-major)
* @param {Int32Array} INDXQ - sorting permutation (1-based)
* @param {number} rho - rank-1 cut weight
* @param {Float64Array} z - updating vector
* @returns {Object} result with all output arrays and info/K/rho
*/
function callDlaed2( N, n1, d, Q, INDXQ, rho, z ) {
	var DLAMBDA = new Float64Array( N );
	var COLTYP = new Int32Array( Math.max( N, 4 ) );
	var result;
	var INDXP = new Int32Array( N );
	var INDXC = new Int32Array( N );
	var INDX = new Int32Array( N );
	var Q2 = new Float64Array( N * N );
	var w = new Float64Array( N );

	result = dlaed2( N, n1, d, 1, 0, Q, 1, N, 0, INDXQ, 1, 0, rho, z, 1, 0, DLAMBDA, 1, 0, w, 1, 0, Q2, 1, 0, INDX, 1, 0, INDXC, 1, 0, INDXP, 1, 0, COLTYP, 1, 0 );

	return {
		'info': result.info,
		'K': result.K,
		'rho': result.rho,
		'd': d,
		'Q': Q,
		'z': z,
		'DLAMBDA': DLAMBDA,
		'w': w,
		'Q2': Q2,
		'INDX': INDX,
		'INDXC': INDXC,
		'INDXP': INDXP,
		'COLTYP': COLTYP
	};
}


// TESTS //

test( 'main export is a function', function t() {
	assert.equal( typeof dlaed2, 'function' );
});

test( 'dlaed2: basic_n6 - no deflation', function t() {
	var INDXQ;
	var tc;
	var Q;
	var d;
	var z;
	var N;
	var r;

	tc = findCase( 'basic_n6' );
	N = tc.N;

	d = new Float64Array( [ 1.0, 3.0, 5.0, 2.0, 4.0, 6.0 ] );
	Q = new Float64Array( N * N );
	Q[ 0 ] = 1.0; Q[ 7 ] = 1.0; Q[ 14 ] = 1.0; Q[ 21 ] = 1.0; Q[ 28 ] = 1.0; Q[ 35 ] = 1.0;
	INDXQ = new Int32Array( [ 1, 2, 3, 1, 2, 3 ] );
	z = new Float64Array( [ 0.5, 0.6, 0.7, 0.4, 0.3, 0.2 ] );

	r = callDlaed2( N, 3, d, Q, INDXQ, 1.0, z );

	assert.equal( r.info, tc.INFO, 'INFO' );
	assert.equal( r.K, tc.K, 'K' );
	assertClose( r.rho, tc.RHO, 1e-14, 'RHO' );
	assertArrayClose( Array.from( r.DLAMBDA ), tc.DLAMBDA, 1e-14, 'DLAMBDA' );
	assertArrayClose( Array.from( r.w ), tc.W, 1e-14, 'W' );
	assertArrayClose( Array.from( r.Q.slice( 0, N * N ) ), tc.Q, 1e-14, 'Q' );

	// COLTYP on exit contains counts of each column type
	assert.deepEqual( Array.from( r.COLTYP.slice( 0, 4 ) ), tc.COLTYP, 'COLTYP' );
});

test( 'dlaed2: n0 - quick return for empty matrix', function t() {
	var DLAMBDA = new Float64Array( 0 );
	var COLTYP = new Int32Array( 0 );
	var result;
	var INDXQ = new Int32Array( 0 );
	var INDXP = new Int32Array( 0 );
	var INDXC = new Int32Array( 0 );
	var INDX = new Int32Array( 0 );
	var Q2 = new Float64Array( 0 );
	var tc;
	var d = new Float64Array( 0 );
	var Q = new Float64Array( 0 );
	var w = new Float64Array( 0 );
	var z = new Float64Array( 0 );

	tc = findCase( 'n0' );

	result = dlaed2( 0, 0, d, 1, 0, Q, 1, 1, 0, INDXQ, 1, 0, 1.0, z, 1, 0, DLAMBDA, 1, 0, w, 1, 0, Q2, 1, 0, INDX, 1, 0, INDXC, 1, 0, INDXP, 1, 0, COLTYP, 1, 0 );

	assert.equal( result.info, tc.INFO, 'INFO' );
	assert.equal( result.K, 0, 'K' );
});

test( 'dlaed2: neg_rho - negative rho negates second half of z', function t() {
	var INDXQ;
	var tc;
	var Q;
	var d;
	var z;
	var N;
	var r;

	tc = findCase( 'neg_rho' );
	N = tc.N;

	d = new Float64Array( [ 1.0, 4.0, 2.0, 5.0 ] );
	Q = new Float64Array( N * N );
	Q[ 0 ] = 1.0; Q[ 5 ] = 1.0; Q[ 10 ] = 1.0; Q[ 15 ] = 1.0;
	INDXQ = new Int32Array( [ 1, 2, 1, 2 ] );
	z = new Float64Array( [ 0.3, 0.5, 0.4, 0.6 ] );

	r = callDlaed2( N, 2, d, Q, INDXQ, -2.0, z );

	assert.equal( r.info, tc.INFO, 'INFO' );
	assert.equal( r.K, tc.K, 'K' );
	assertClose( r.rho, tc.RHO, 1e-14, 'RHO' );
	assertArrayClose( Array.from( r.DLAMBDA ), tc.DLAMBDA, 1e-14, 'DLAMBDA' );
	assertArrayClose( Array.from( r.w ), tc.W, 1e-14, 'W' );
	assertArrayClose( Array.from( r.Q.slice( 0, N * N ) ), tc.Q, 1e-14, 'Q' );
	assert.deepEqual( Array.from( r.COLTYP.slice( 0, 4 ) ), tc.COLTYP, 'COLTYP' );
});

test( 'dlaed2: small_z - deflation via small z component', function t() {
	var INDXQ;
	var tc;
	var Q;
	var d;
	var z;
	var N;
	var r;

	tc = findCase( 'small_z' );
	N = tc.N;

	d = new Float64Array( [ 1.0, 3.0, 2.0, 5.0 ] );
	Q = new Float64Array( N * N );
	Q[ 0 ] = 1.0; Q[ 5 ] = 1.0; Q[ 10 ] = 1.0; Q[ 15 ] = 1.0;
	INDXQ = new Int32Array( [ 1, 2, 1, 2 ] );
	z = new Float64Array( [ 0.5, 1.0e-20, 0.4, 0.3 ] );

	r = callDlaed2( N, 2, d, Q, INDXQ, 1.0, z );

	assert.equal( r.info, tc.INFO, 'INFO' );
	assert.equal( r.K, tc.K, 'K' );
	assertClose( r.rho, tc.RHO, 1e-14, 'RHO' );
	assertArrayClose( Array.from( r.DLAMBDA.slice( 0, r.K ) ), tc.DLAMBDA.slice( 0, r.K ), 1e-14, 'DLAMBDA' );
	assertArrayClose( Array.from( r.w.slice( 0, r.K ) ), tc.W.slice( 0, r.K ), 1e-14, 'W' );
	assert.deepEqual( Array.from( r.COLTYP.slice( 0, 4 ) ), tc.COLTYP, 'COLTYP' );
});

test( 'dlaed2: close_eigenvalues - deflation via close eigenvalues', function t() {
	var INDXQ;
	var tc;
	var Q;
	var d;
	var z;
	var N;
	var r;

	tc = findCase( 'close_eigenvalues' );
	N = tc.N;

	d = new Float64Array( [ 1.0, 3.0, 1.0 + 1.0e-16, 5.0 ] );
	Q = new Float64Array( N * N );
	Q[ 0 ] = 1.0; Q[ 5 ] = 1.0; Q[ 10 ] = 1.0; Q[ 15 ] = 1.0;
	INDXQ = new Int32Array( [ 1, 2, 1, 2 ] );
	z = new Float64Array( [ 0.5, 0.6, 0.4, 0.3 ] );

	r = callDlaed2( N, 2, d, Q, INDXQ, 1.0, z );

	assert.equal( r.info, tc.INFO, 'INFO' );
	assert.equal( r.K, tc.K, 'K' );
	assertClose( r.rho, tc.RHO, 1e-14, 'RHO' );
	assertArrayClose( Array.from( r.DLAMBDA.slice( 0, r.K ) ), tc.DLAMBDA.slice( 0, r.K ), 1e-14, 'DLAMBDA' );
	assertArrayClose( Array.from( r.w.slice( 0, r.K ) ), tc.W.slice( 0, r.K ), 1e-14, 'W' );
	assert.deepEqual( Array.from( r.COLTYP.slice( 0, 4 ) ), tc.COLTYP, 'COLTYP' );
});

test( 'dlaed2: all_deflated - all z components tiny', function t() {
	var INDXQ;
	var tc;
	var Q;
	var d;
	var z;
	var N;
	var r;

	tc = findCase( 'all_deflated' );
	N = tc.N;

	d = new Float64Array( [ 1.0, 3.0, 2.0, 5.0 ] );
	Q = new Float64Array( N * N );
	Q[ 0 ] = 1.0; Q[ 5 ] = 1.0; Q[ 10 ] = 1.0; Q[ 15 ] = 1.0;
	INDXQ = new Int32Array( [ 1, 2, 1, 2 ] );
	z = new Float64Array( [ 1.0e-20, 1.0e-20, 1.0e-20, 1.0e-20 ] );

	r = callDlaed2( N, 2, d, Q, INDXQ, 1.0, z );

	assert.equal( r.info, tc.INFO, 'INFO' );
	assert.equal( r.K, tc.K, 'K = 0 (all deflated)' );
	assertClose( r.rho, tc.RHO, 1e-14, 'RHO' );

	// On exit, D should contain sorted eigenvalues and Q should be reordered
	assertArrayClose( Array.from( r.d ), tc.D, 1e-14, 'D' );
	assertArrayClose( Array.from( r.Q.slice( 0, N * N ) ), tc.Q, 1e-14, 'Q' );
});

test( 'dlaed2: n2 - minimal case N=2, N1=1', function t() {
	var INDXQ;
	var tc;
	var Q;
	var d;
	var z;
	var r;

	tc = findCase( 'n2' );

	d = new Float64Array( [ 3.0, 1.0 ] );
	Q = new Float64Array( 4 );
	Q[ 0 ] = 1.0; Q[ 3 ] = 1.0;
	INDXQ = new Int32Array( [ 1, 1 ] );
	z = new Float64Array( [ 0.7, 0.7 ] );

	r = callDlaed2( 2, 1, d, Q, INDXQ, 0.5, z );

	assert.equal( r.info, tc.INFO, 'INFO' );
	assert.equal( r.K, tc.K, 'K' );
	assertClose( r.rho, tc.RHO, 1e-14, 'RHO' );
	assertArrayClose( Array.from( r.DLAMBDA.slice( 0, r.K ) ), tc.DLAMBDA.slice( 0, r.K ), 1e-14, 'DLAMBDA' );
	assertArrayClose( Array.from( r.w.slice( 0, r.K ) ), tc.W.slice( 0, r.K ), 1e-14, 'W' );
	assert.deepEqual( Array.from( r.COLTYP.slice( 0, 4 ) ), tc.COLTYP, 'COLTYP' );
});

test( 'dlaed2: n8_mixed - larger case with mixed deflation', function t() {
	var INDXQ;
	var tc;
	var Q;
	var d;
	var z;
	var N;
	var r;
	var i;

	tc = findCase( 'n8_mixed' );
	N = tc.N;

	d = new Float64Array( [ 1.0, 3.0, 5.5, 7.0, 2.0, 4.0, 5.5 + 1.0e-16, 9.0 ] );
	Q = new Float64Array( N * N );
	for ( i = 0; i < N; i += 1 ) {
		Q[ ( i * N ) + i ] = 1.0;
	}
	INDXQ = new Int32Array( [ 1, 2, 3, 4, 1, 2, 3, 4 ] );
	z = new Float64Array( [ 0.3, 0.4, 0.5, 0.2, 0.35, 0.45, 0.25, 0.15 ] );

	r = callDlaed2( N, 4, d, Q, INDXQ, 1.5, z );

	assert.equal( r.info, tc.INFO, 'INFO' );
	assert.equal( r.K, tc.K, 'K' );
	assertClose( r.rho, tc.RHO, 1e-14, 'RHO' );
	assertArrayClose( Array.from( r.DLAMBDA.slice( 0, r.K ) ), tc.DLAMBDA.slice( 0, r.K ), 1e-14, 'DLAMBDA' );
	assertArrayClose( Array.from( r.w.slice( 0, r.K ) ), tc.W.slice( 0, r.K ), 1e-14, 'W' );
	assert.deepEqual( Array.from( r.COLTYP.slice( 0, 4 ) ), tc.COLTYP, 'COLTYP' );
});

test( 'dlaed2: returns rho = ABS(2*rho)', function t() {
	var result;

	// Rho = 3.5 => returns ABS(2*3.5) = 7.0
	result = callDlaed2( 2, 1, new Float64Array( [ 1.0, 2.0 ] ), new Float64Array( [ 1.0, 0.0, 0.0, 1.0 ] ), new Int32Array( [ 1, 1 ] ), 3.5, new Float64Array( [ 0.5, 0.5 ] ) );
	assertClose( result.rho, 7.0, 1e-14, 'rho=ABS(2*3.5)' );

	// Rho = -2.0 => returns ABS(2*(-2)) = 4.0
	result = callDlaed2( 2, 1, new Float64Array( [ 1.0, 2.0 ] ), new Float64Array( [ 1.0, 0.0, 0.0, 1.0 ] ), new Int32Array( [ 1, 1 ] ), -2.0, new Float64Array( [ 0.5, 0.5 ] ) );
	assertClose( result.rho, 4.0, 1e-14, 'rho=ABS(2*(-2))' );
});

test( 'dlaed2: deflated D values sorted in increasing order', function t() {
	var r;
	var i;

	r = callDlaed2( 4, 2, new Float64Array( [ 1.0, 3.0, 2.0, 5.0 ] ), new Float64Array( [ 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0 ] ), new Int32Array( [ 1, 2, 1, 2 ] ), 1.0, new Float64Array( [ 0.5, 1.0e-20, 0.4, 0.3 ] ) );

	// Verify deflated D entries (from K+1 to N) are in increasing order
	for ( i = r.K; i < 3; i += 1 ) {
		assert.ok( r.d[ i ] <= r.d[ i + 1 ], 'D[' + i + '] <= D[' + ( i + 1 ) + ']: ' + r.d[ i ] + ' <= ' + r.d[ i + 1 ] );
	}
});
