'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var dgecon = require( './../lib/base.js' );
var dgetrf = require( '../../dgetrf/lib/base.js' );
var dlange = require( '../../dlange/lib/base.js' );

// FIXTURES //

var well_cond_1norm = require( './fixtures/well_cond_1norm.json' );
var well_cond_inorm = require( './fixtures/well_cond_inorm.json' );
var identity = require( './fixtures/identity.json' );
var ill_cond = require( './fixtures/ill_cond.json' );
var singular = require( './fixtures/singular.json' );
var _4x4_1norm = require( './fixtures/4x4_1norm.json' );

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual + ' (relErr=' + relErr + ')' );
}

// Helper to compute condition number
function computeRcond( normStr, N, Aflat ) {
	var A = new Float64Array( Aflat );
	var work = new Float64Array( 4 * N );
	var iwork = new Int32Array( N );
	var ipiv = new Int32Array( N );
	var rcond = new Float64Array( 1 );
	var anorm = dlange( normStr, N, N, A, 1, N, 0, work, 1, 0 );
	dgetrf( N, N, A, 1, N, 0, ipiv, 1, 0 );
	var info = dgecon( normStr, N, A, 1, N, 0, anorm, rcond, work, 1, 0, iwork, 1, 0 );
	return { rcond: rcond[ 0 ], anorm: anorm, info: info };
}

test( 'dgecon: main export is a function', function t() {
	assert.strictEqual( typeof dgecon, 'function' );
});

test( 'dgecon: well-conditioned 3x3 (1-norm)', function t() {
	var tc = well_cond_1norm;
	var result = computeRcond( 'one-norm', 3, [
		4, 1, 1,
		1, 3, 1,
		1, 1, 2
	] );
	assert.strictEqual( result.info, 0 );
	assertClose( result.anorm, tc.anorm, 1e-14, 'anorm' );
	assertClose( result.rcond, tc.rcond, 1e-10, 'rcond' );
});

test( 'dgecon: well-conditioned 3x3 (infinity-norm)', function t() {
	var tc = well_cond_inorm;
	var result = computeRcond( 'inf-norm', 3, [
		4, 1, 1,
		1, 3, 1,
		1, 1, 2
	] );
	assert.strictEqual( result.info, 0 );
	assertClose( result.anorm, tc.anorm, 1e-14, 'anorm' );
	assertClose( result.rcond, tc.rcond, 1e-10, 'rcond' );
});

test( 'dgecon: 3x3 identity (rcond = 1)', function t() {
	var tc = identity;
	var result = computeRcond( 'one-norm', 3, [
		1, 0, 0,
		0, 1, 0,
		0, 0, 1
	] );
	assert.strictEqual( result.info, 0 );
	assertClose( result.anorm, tc.anorm, 1e-14, 'anorm' );
	assertClose( result.rcond, tc.rcond, 1e-10, 'rcond' );
});

test( 'dgecon: ill-conditioned 3x3 (rcond near 0)', function t() {
	var tc = ill_cond;
	var result = computeRcond( 'one-norm', 3, [
		1, 0, 0,
		0, 1, 0,
		0, 0, 1e-15
	] );
	assert.strictEqual( result.info, 0 );
	assertClose( result.rcond, tc.rcond, 1e-5, 'rcond' );
});

test( 'dgecon: singular 3x3 (rcond = 0)', function t() {
	var tc = singular;
	var result = computeRcond( 'one-norm', 3, [
		1, 2, 3,
		2, 4, 6,
		3, 6, 9
	] );
	assert.strictEqual( result.info, 0 );
	assert.strictEqual( result.rcond, 0.0 );
});

test( 'dgecon: N=0 (rcond = 1)', function t() {
	var A = new Float64Array( 0 );
	var work = new Float64Array( 0 );
	var iwork = new Int32Array( 0 );
	var rcond = new Float64Array( 1 );
	var info = dgecon( 'one-norm', 0, A, 1, 1, 0, 0.0, rcond, work, 1, 0, iwork, 1, 0 );
	assert.strictEqual( info, 0 );
	assert.strictEqual( rcond[ 0 ], 1.0 );
});

test( 'dgecon: anorm = 0 (rcond = 0)', function t() {
	var A = new Float64Array( [ 1, 0, 0, 0, 1, 0, 0, 0, 1 ] );
	var work = new Float64Array( 12 );
	var iwork = new Int32Array( 3 );
	var ipiv = new Int32Array( 3 );
	var rcond = new Float64Array( 1 );
	dgetrf( 3, 3, A, 1, 3, 0, ipiv, 1, 0 );
	var info = dgecon( 'one-norm', 3, A, 1, 3, 0, 0.0, rcond, work, 1, 0, iwork, 1, 0 );
	assert.strictEqual( info, 0 );
	assert.strictEqual( rcond[ 0 ], 0.0 );
});

test( 'dgecon: 4x4 tridiagonal-like matrix (1-norm)', function t() {
	var tc = _4x4_1norm;
	var result = computeRcond( 'one-norm', 4, [
		5, 1, 0, 0,
		1, 4, 1, 0,
		0, 1, 3, 1,
		0, 0, 1, 2
	] );
	assert.strictEqual( result.info, 0 );
	assertClose( result.anorm, tc.anorm, 1e-14, 'anorm' );
	assertClose( result.rcond, tc.rcond, 1e-10, 'rcond' );
});

test( 'dgecon: NaN anorm returns info=-5', function t() {
	var A = new Float64Array( [ 1 ] );
	var work = new Float64Array( 4 );
	var iwork = new Int32Array( 1 );
	var rcond = new Float64Array( 1 );
	var info = dgecon( 'one-norm', 1, A, 1, 1, 0, NaN, rcond, work, 1, 0, iwork, 1, 0 );
	assert.strictEqual( info, -5 );
});

test( 'dgecon: Inf anorm returns info=-5', function t() {
	var A = new Float64Array( [ 1 ] );
	var work = new Float64Array( 4 );
	var iwork = new Int32Array( 1 );
	var rcond = new Float64Array( 1 );
	var info = dgecon( 'one-norm', 1, A, 1, 1, 0, Infinity, rcond, work, 1, 0, iwork, 1, 0 );
	assert.strictEqual( info, -5 );
});

test( 'dgecon: rcond is between 0 and 1 for well-conditioned matrices', function t() {
	var result = computeRcond( 'one-norm', 3, [
		10, 1, 1,
		1, 10, 1,
		1, 1, 10
	] );
	assert.strictEqual( result.info, 0 );
	assert.ok( result.rcond > 0, 'rcond > 0' );
	assert.ok( result.rcond <= 1, 'rcond <= 1' );
});
