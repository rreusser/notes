/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var zppcon = require( './../lib/base.js' );
var ndarray = require( './../lib/ndarray.js' );


// FIXTURES //

var fixtures = {
	'upper_3x3': require( './fixtures/upper_3x3.json' ),
	'lower_3x3': require( './fixtures/lower_3x3.json' ),
	'identity': require( './fixtures/identity.json' ),
	'4x4_upper': require( './fixtures/4x4_upper.json' ),
	'4x4_lower': require( './fixtures/4x4_lower.json' ),
	'n_one': require( './fixtures/n_one.json' )
};


// FUNCTIONS //

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
* Runs a zppcon test case using fixture data.
*
* The fixture stores the factored AP (output of ZPPTRF) in ap_r.
*
* @private
* @param {string} uplo - 'upper' or 'lower'
* @param {NonNegativeInteger} N - order of the matrix
* @param {string} caseName - fixture case name
*/
function runCase( uplo, N, caseName ) {
	var RWORK;
	var rcond;
	var WORK;
	var info;
	var tc;
	var AP;

	tc = fixtures[ caseName ];

	AP = new Complex128Array( new Float64Array( tc.ap_r ).buffer );
	WORK = new Complex128Array( 2 * N );
	RWORK = new Float64Array( N );
	rcond = new Float64Array( 1 );

	info = zppcon( uplo, N, AP, 1, 0, tc.anorm, rcond, WORK, 1, 0, RWORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( rcond[ 0 ], tc.rcond, 1e-14, 'rcond' );
}


// TESTS //

test( 'zppcon: upper 3x3 HPD matrix', function t() {
	runCase( 'upper', 3, 'upper_3x3' );
});

test( 'zppcon: lower 3x3 HPD matrix', function t() {
	runCase( 'lower', 3, 'lower_3x3' );
});

test( 'zppcon: identity matrix (rcond=1)', function t() {
	var rcond;
	var RWORK;
	var WORK;
	var info;
	var tc;
	var AP;

	rcond = new Float64Array( 1 );
	WORK = new Complex128Array( 6 );
	RWORK = new Float64Array( 3 );
	tc = fixtures[ 'identity' ];
	AP = new Complex128Array( [ 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0 ] );
	info = zppcon( 'upper', 3, AP, 1, 0, tc.anorm, rcond, WORK, 1, 0, RWORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( rcond[ 0 ], tc.rcond, 1e-14, 'rcond' );
});

test( 'zppcon: N=0 returns rcond=1', function t() {
	var rcond;
	var RWORK;
	var WORK;
	var info;
	var AP;

	rcond = new Float64Array( 1 );
	WORK = new Complex128Array( 1 );
	RWORK = new Float64Array( 1 );
	AP = new Complex128Array( 1 );
	info = zppcon( 'upper', 0, AP, 1, 0, 0.0, rcond, WORK, 1, 0, RWORK, 1, 0 );
	assert.equal( info, 0, 'info' );
	assert.equal( rcond[ 0 ], 1.0, 'rcond' );
});

test( 'zppcon: anorm=0 returns rcond=0', function t() {
	var rcond;
	var RWORK;
	var WORK;
	var info;
	var AP;

	rcond = new Float64Array( 1 );
	WORK = new Complex128Array( 2 );
	RWORK = new Float64Array( 1 );
	AP = new Complex128Array( [ 1.0, 0.0 ] );
	info = zppcon( 'upper', 1, AP, 1, 0, 0.0, rcond, WORK, 1, 0, RWORK, 1, 0 );
	assert.equal( info, 0, 'info' );
	assert.equal( rcond[ 0 ], 0.0, 'rcond' );
});

test( 'zppcon: upper 4x4 HPD matrix', function t() {
	runCase( 'upper', 4, '4x4_upper' );
});

test( 'zppcon: lower 4x4 HPD matrix', function t() {
	runCase( 'lower', 4, '4x4_lower' );
});

test( 'zppcon: N=1 edge case', function t() {
	var rcond;
	var RWORK;
	var WORK;
	var info;
	var tc;
	var AP;

	rcond = new Float64Array( 1 );
	WORK = new Complex128Array( 2 );
	RWORK = new Float64Array( 1 );
	tc = fixtures[ 'n_one' ];
	AP = new Complex128Array( [ Math.sqrt( 3.0 ), 0.0 ] );
	info = zppcon( 'upper', 1, AP, 1, 0, tc.anorm, rcond, WORK, 1, 0, RWORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( rcond[ 0 ], tc.rcond, 1e-14, 'rcond' );
});

test( 'ndarray: validates uplo', function t() {
	var RWORK;
	var rcond;
	var WORK;
	var AP;

	RWORK = new Float64Array( 1 );
	rcond = new Float64Array( 1 );
	WORK = new Complex128Array( 2 );
	AP = new Complex128Array( 1 );
	assert.throws( function badUplo() {
		ndarray( 'invalid', 1, AP, 1, 0, 1.0, rcond, WORK, 1, 0, RWORK, 1, 0 ); // eslint-disable-line max-len
	}, /invalid argument/ );
});

test( 'ndarray: upper 3x3 HPD matrix', function t() {
	var RWORK;
	var rcond;
	var WORK;
	var info;
	var tc;
	var AP;

	tc = fixtures[ 'upper_3x3' ];
	AP = new Complex128Array( new Float64Array( tc.ap_r ).buffer );
	WORK = new Complex128Array( 6 );
	RWORK = new Float64Array( 3 );
	rcond = new Float64Array( 1 );
	info = ndarray( 'upper', 3, AP, 1, 0, tc.anorm, rcond, WORK, 1, 0, RWORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( rcond[ 0 ], tc.rcond, 1e-14, 'rcond' );
});

test( 'ndarray: lower 4x4 HPD matrix', function t() {
	var RWORK;
	var rcond;
	var WORK;
	var info;
	var tc;
	var AP;

	tc = fixtures[ '4x4_lower' ];
	AP = new Complex128Array( new Float64Array( tc.ap_r ).buffer );
	WORK = new Complex128Array( 8 );
	RWORK = new Float64Array( 4 );
	rcond = new Float64Array( 1 );
	info = ndarray( 'lower', 4, AP, 1, 0, tc.anorm, rcond, WORK, 1, 0, RWORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( rcond[ 0 ], tc.rcond, 1e-14, 'rcond' );
});
