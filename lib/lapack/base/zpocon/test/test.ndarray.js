'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var zpocon = require( './../lib/ndarray.js' );
var zpotrf = require( '../../zpotrf/lib/base.js' );
var zlanhe = require( '../../zlanhe/lib/base.js' );

// FIXTURES //

var upper_1norm = require( './fixtures/upper_1norm.json' );
var lower_1norm = require( './fixtures/lower_1norm.json' );
var identity = require( './fixtures/identity.json' );
var n_zero = require( './fixtures/n_zero.json' );
var anorm_zero = require( './fixtures/anorm_zero.json' );
var _4x4_upper = require( './fixtures/4x4_upper.json' );
var _4x4_lower = require( './fixtures/4x4_lower.json' );

// FUNCTIONS //

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual + ' (relErr=' + relErr + ')' );
}

// Helper to compute condition number for HPD matrix
function computeRcond( uploStr, N, Aflat ) {
	var A = new Complex128Array( Aflat );
	var work = new Complex128Array( 2 * N );
	var rwork = new Float64Array( N );
	var rcond = new Float64Array( 1 );
	var anorm = zlanhe( 'one-norm', uploStr, N, A, 1, N, 0, rwork, 1, 0 );
	zpotrf( uploStr, N, A, 1, N, 0 );
	var info = zpocon( uploStr, N, A, 1, N, 0, anorm, rcond, work, 1, 0, rwork, 1, 0 );
	return { rcond: rcond[ 0 ], anorm: anorm, info: info };
}

// TESTS //

test( 'zpocon: main export is a function', function t() {
	assert.strictEqual( typeof zpocon, 'function' );
});

test( 'zpocon: 3x3 HPD, upper, 1-norm', function t() {
	var tc = upper_1norm;
	// A = [[4, 1+i, 0], [1-i, 3, 1], [0, 1, 2]]
	var result = computeRcond( 'upper', 3, [
		4, 0,   1, -1,  0, 0,
		1, 1,   3, 0,   1, 0,
		0, 0,   1, 0,   2, 0
	] );
	assert.strictEqual( result.info, 0 );
	assertClose( result.anorm, tc.anorm, 1e-14, 'anorm' );
	assertClose( result.rcond, tc.rcond, 1e-10, 'rcond' );
});

test( 'zpocon: 3x3 HPD, lower, 1-norm', function t() {
	var tc = lower_1norm;
	var result = computeRcond( 'lower', 3, [
		4, 0,   1, -1,  0, 0,
		1, 1,   3, 0,   1, 0,
		0, 0,   1, 0,   2, 0
	] );
	assert.strictEqual( result.info, 0 );
	assertClose( result.anorm, tc.anorm, 1e-14, 'anorm' );
	assertClose( result.rcond, tc.rcond, 1e-10, 'rcond' );
});

test( 'zpocon: identity (rcond=1)', function t() {
	var tc = identity;
	var result = computeRcond( 'upper', 3, [
		1, 0,  0, 0,  0, 0,
		0, 0,  1, 0,  0, 0,
		0, 0,  0, 0,  1, 0
	] );
	assert.strictEqual( result.info, 0 );
	assertClose( result.anorm, tc.anorm, 1e-14, 'anorm' );
	assertClose( result.rcond, tc.rcond, 1e-10, 'rcond' );
});

test( 'zpocon: N=0 (rcond=1)', function t() {
	var tc = n_zero;
	var A = new Complex128Array( 1 );
	var work = new Complex128Array( 1 );
	var rwork = new Float64Array( 1 );
	var rcond = new Float64Array( 1 );
	var info = zpocon( 'upper', 0, A, 1, 1, 0, 0.0, rcond, work, 1, 0, rwork, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertClose( rcond[ 0 ], tc.rcond, 1e-14, 'rcond' );
});

test( 'zpocon: anorm=0 (rcond=0)', function t() {
	var tc = anorm_zero;
	var A = new Complex128Array( [ 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0 ] );
	var work = new Complex128Array( 6 );
	var rwork = new Float64Array( 3 );
	var rcond = new Float64Array( 1 );
	zpotrf( 'upper', 3, A, 1, 3, 0 );
	var info = zpocon( 'upper', 3, A, 1, 3, 0, 0.0, rcond, work, 1, 0, rwork, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertClose( rcond[ 0 ], tc.rcond, 1e-14, 'rcond' );
});

test( 'zpocon: 4x4 HPD, upper', function t() {
	var tc = _4x4_upper;
	// A = [[5, 1+i, 0, 0], [1-i, 4, 1+i, 0], [0, 1-i, 3, 1], [0, 0, 1, 2]]
	var result = computeRcond( 'upper', 4, [
		5, 0,    1, -1,   0, 0,    0, 0,
		1, 1,    4, 0,    1, -1,   0, 0,
		0, 0,    1, 1,    3, 0,    1, 0,
		0, 0,    0, 0,    1, 0,    2, 0
	] );
	assert.strictEqual( result.info, 0 );
	assertClose( result.anorm, tc.anorm, 1e-14, 'anorm' );
	assertClose( result.rcond, tc.rcond, 1e-10, 'rcond' );
});

test( 'zpocon: 4x4 HPD, lower', function t() {
	var tc = _4x4_lower;
	var result = computeRcond( 'lower', 4, [
		5, 0,    1, -1,   0, 0,    0, 0,
		1, 1,    4, 0,    1, -1,   0, 0,
		0, 0,    1, 1,    3, 0,    1, 0,
		0, 0,    0, 0,    1, 0,    2, 0
	] );
	assert.strictEqual( result.info, 0 );
	assertClose( result.anorm, tc.anorm, 1e-14, 'anorm' );
	assertClose( result.rcond, tc.rcond, 1e-10, 'rcond' );
});
