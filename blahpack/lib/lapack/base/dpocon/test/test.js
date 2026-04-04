'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var dpocon = require( './../lib/base.js' );
var dpotrf = require( '../../dpotrf/lib/base.js' );
var dlansy = require( '../../dlansy/lib/base.js' );

// FIXTURES //

var identity_upper = require( './fixtures/identity_upper.json' );
var identity_lower = require( './fixtures/identity_lower.json' );
var well_cond_upper = require( './fixtures/well_cond_upper.json' );
var well_cond_lower = require( './fixtures/well_cond_lower.json' );
var ill_cond = require( './fixtures/ill_cond.json' );
var _4x4_upper = require( './fixtures/4x4_upper.json' );

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual + ' (relErr=' + relErr + ')' );
}

// Helper to compute condition number of SPD matrix
function computeRcond( uploStr, N, Aflat ) {
	var A = new Float64Array( Aflat );
	var work = new Float64Array( 3 * N );
	var iwork = new Int32Array( N );
	var rcond = new Float64Array( 1 );
	var anorm = dlansy( 'one-norm', uploStr, N, A, 1, N, 0, work, 1, 0 );
	dpotrf( uploStr, N, A, 1, N, 0 );
	var info = dpocon( uploStr, N, A, 1, N, 0, anorm, rcond, work, 1, 0, iwork, 1, 0 );
	return { rcond: rcond[ 0 ], anorm: anorm, info: info };
}

test( 'dpocon: main export is a function', function t() {
	assert.strictEqual( typeof dpocon, 'function' );
});

test( 'dpocon: 3x3 identity, upper (rcond = 1)', function t() {
	var tc = identity_upper;
	var result = computeRcond( 'upper', 3, [
		1, 0, 0,
		0, 1, 0,
		0, 0, 1
	] );
	assert.strictEqual( result.info, 0 );
	assertClose( result.anorm, tc.anorm, 1e-14, 'anorm' );
	assertClose( result.rcond, tc.rcond, 1e-10, 'rcond' );
});

test( 'dpocon: 3x3 identity, lower (rcond = 1)', function t() {
	var tc = identity_lower;
	var result = computeRcond( 'lower', 3, [
		1, 0, 0,
		0, 1, 0,
		0, 0, 1
	] );
	assert.strictEqual( result.info, 0 );
	assertClose( result.anorm, tc.anorm, 1e-14, 'anorm' );
	assertClose( result.rcond, tc.rcond, 1e-10, 'rcond' );
});

test( 'dpocon: well-conditioned SPD 3x3, upper', function t() {
	var tc = well_cond_upper;
	var result = computeRcond( 'upper', 3, [
		4, 2, 1,
		2, 5, 2,
		1, 2, 6
	] );
	assert.strictEqual( result.info, 0 );
	assertClose( result.anorm, tc.anorm, 1e-14, 'anorm' );
	assertClose( result.rcond, tc.rcond, 1e-10, 'rcond' );
});

test( 'dpocon: well-conditioned SPD 3x3, lower', function t() {
	var tc = well_cond_lower;
	var result = computeRcond( 'lower', 3, [
		4, 2, 1,
		2, 5, 2,
		1, 2, 6
	] );
	assert.strictEqual( result.info, 0 );
	assertClose( result.anorm, tc.anorm, 1e-14, 'anorm' );
	assertClose( result.rcond, tc.rcond, 1e-10, 'rcond' );
});

test( 'dpocon: ill-conditioned SPD (rcond near 0)', function t() {
	var tc = ill_cond;
	var result = computeRcond( 'upper', 3, [
		1, 0, 0,
		0, 1, 0,
		0, 0, 1e-15
	] );
	assert.strictEqual( result.info, 0 );
	assertClose( result.rcond, tc.rcond, 1e-5, 'rcond' );
});

test( 'dpocon: N=0 (rcond = 1)', function t() {
	var A = new Float64Array( 0 );
	var work = new Float64Array( 0 );
	var iwork = new Int32Array( 0 );
	var rcond = new Float64Array( 1 );
	var info = dpocon( 'upper', 0, A, 1, 1, 0, 0.0, rcond, work, 1, 0, iwork, 1, 0 );
	assert.strictEqual( info, 0 );
	assert.strictEqual( rcond[ 0 ], 1.0 );
});

test( 'dpocon: anorm = 0 (rcond = 0)', function t() {
	var A = new Float64Array( [ 1, 0, 0, 0, 1, 0, 0, 0, 1 ] );
	var work = new Float64Array( 9 );
	var iwork = new Int32Array( 3 );
	var rcond = new Float64Array( 1 );
	dpotrf( 'upper', 3, A, 1, 3, 0 );
	var info = dpocon( 'upper', 3, A, 1, 3, 0, 0.0, rcond, work, 1, 0, iwork, 1, 0 );
	assert.strictEqual( info, 0 );
	assert.strictEqual( rcond[ 0 ], 0.0 );
});

test( 'dpocon: 4x4 SPD matrix, upper', function t() {
	var tc = _4x4_upper;
	var result = computeRcond( 'upper', 4, [
		10, 1, 0, 0,
		1, 8, 1, 0,
		0, 1, 6, 1,
		0, 0, 1, 4
	] );
	assert.strictEqual( result.info, 0 );
	assertClose( result.anorm, tc.anorm, 1e-14, 'anorm' );
	assertClose( result.rcond, tc.rcond, 1e-10, 'rcond' );
});

test( 'dpocon: rcond is between 0 and 1 for well-conditioned SPD', function t() {
	var result = computeRcond( 'upper', 3, [
		10, 1, 0,
		1, 10, 1,
		0, 1, 10
	] );
	assert.strictEqual( result.info, 0 );
	assert.ok( result.rcond > 0, 'rcond > 0' );
	assert.ok( result.rcond <= 1, 'rcond <= 1' );
});

test( 'dpocon: 2x2 SPD, lower', function t() {
	var result = computeRcond( 'lower', 2, [
		4, 1,
		1, 3
	] );
	assert.strictEqual( result.info, 0 );
	assert.ok( result.rcond > 0, 'rcond > 0' );
	assert.ok( result.rcond <= 1, 'rcond <= 1' );
});

test( 'dpocon: 1x1 SPD', function t() {
	var A = new Float64Array( [ 5.0 ] );
	var work = new Float64Array( 3 );
	var iwork = new Int32Array( 1 );
	var rcond = new Float64Array( 1 );
	var anorm = 5.0;
	dpotrf( 'upper', 1, A, 1, 1, 0 );
	var info = dpocon( 'upper', 1, A, 1, 1, 0, anorm, rcond, work, 1, 0, iwork, 1, 0 );
	assert.strictEqual( info, 0 );
	assertClose( rcond[ 0 ], 1.0, 1e-10, 'rcond for scalar' );
});
