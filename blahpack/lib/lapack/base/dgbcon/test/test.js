/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dgbtrf = require( './../../dgbtrf/lib/base.js' );
var dgbcon = require( './../lib/base.js' );

// FIXTURES //

var tridiag_1norm = require( './fixtures/tridiag_1norm.json' );
var tridiag_inorm = require( './fixtures/tridiag_inorm.json' );
var kl2_ku1_1norm = require( './fixtures/kl2_ku1_1norm.json' );
var n_zero = require( './fixtures/n_zero.json' );
var n_one = require( './fixtures/n_one.json' );
var anorm_zero = require( './fixtures/anorm_zero.json' );

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
* Creates banded matrix in column-major from entries.
* entries is array of [row0based, col, value].
*/
function bandedMatrix( ldab, n, entries ) {
	var ab = new Float64Array( ldab * n );
	var i;
	for ( i = 0; i < entries.length; i++ ) {
		ab[ (entries[i][1] * ldab) + entries[i][0] ] = entries[i][2];
	}
	return ab;
}

// TESTS //

test( 'dgbcon: tridiag 1-norm (KL=1, KU=1, N=4)', function t() {
	var rcond;
	var iwork;
	var work;
	var ipiv;
	var info;
	var ldab;
	var tc;
	var ab;
	var n;

	tc = tridiag_1norm;
	n = 4;
	ldab = 4;
	ab = bandedMatrix( ldab, n, [
		[2, 0, 4.0],
		[3, 0, 1.0],
		[1, 1, 1.0],
		[2, 1, 5.0],
		[3, 1, 1.0],
		[1, 2, 2.0],
		[2, 2, 6.0],
		[3, 2, 2.0],
		[1, 3, 1.0],
		[2, 3, 7.0]
	]);
	ipiv = new Int32Array( n );
	work = new Float64Array( 3 * n );
	iwork = new Int32Array( n );
	rcond = new Float64Array( 1 );
	info = dgbtrf( n, n, 1, 1, ab, 1, ldab, 0, ipiv, 1, 0 );
	assert.equal( info, 0, 'dgbtrf info' );
	info = dgbcon( 'one-norm', n, 1, 1, ab, 1, ldab, 0, ipiv, 1, 0, tc.anorm, rcond, work, 1, 0, iwork, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( rcond[0], tc.rcond, 1e-10, 'rcond' );
});

test( 'dgbcon: tridiag infinity-norm (KL=1, KU=1, N=4)', function t() {
	var rcond;
	var iwork;
	var work;
	var ipiv;
	var info;
	var ldab;
	var tc;
	var ab;
	var n;

	tc = tridiag_inorm;
	n = 4;
	ldab = 4;
	ab = bandedMatrix( ldab, n, [
		[2, 0, 4.0],
		[3, 0, 1.0],
		[1, 1, 1.0],
		[2, 1, 5.0],
		[3, 1, 1.0],
		[1, 2, 2.0],
		[2, 2, 6.0],
		[3, 2, 2.0],
		[1, 3, 1.0],
		[2, 3, 7.0]
	]);
	ipiv = new Int32Array( n );
	work = new Float64Array( 3 * n );
	iwork = new Int32Array( n );
	rcond = new Float64Array( 1 );
	info = dgbtrf( n, n, 1, 1, ab, 1, ldab, 0, ipiv, 1, 0 );
	assert.equal( info, 0, 'dgbtrf info' );
	info = dgbcon( 'infinity-norm', n, 1, 1, ab, 1, ldab, 0, ipiv, 1, 0, tc.anorm, rcond, work, 1, 0, iwork, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( rcond[0], tc.rcond, 1e-10, 'rcond' );
});

test( 'dgbcon: KL=2, KU=1, N=4, 1-norm', function t() {
	var rcond;
	var iwork;
	var work;
	var ipiv;
	var info;
	var ldab;
	var tc;
	var ab;
	var n;

	tc = kl2_ku1_1norm;
	n = 4;
	ldab = 6;
	ab = bandedMatrix( ldab, n, [
		[3, 0, 5.0],
		[4, 0, 2.0],
		[5, 0, 1.0],
		[2, 1, 1.0],
		[3, 1, 6.0],
		[4, 1, 1.0],
		[5, 1, 2.0],
		[2, 2, 2.0],
		[3, 2, 7.0],
		[4, 2, 3.0],
		[2, 3, 1.0],
		[3, 3, 8.0]
	]);
	ipiv = new Int32Array( n );
	work = new Float64Array( 3 * n );
	iwork = new Int32Array( n );
	rcond = new Float64Array( 1 );
	info = dgbtrf( n, n, 2, 1, ab, 1, ldab, 0, ipiv, 1, 0 );
	assert.equal( info, 0, 'dgbtrf info' );
	info = dgbcon( 'one-norm', n, 2, 1, ab, 1, ldab, 0, ipiv, 1, 0, tc.anorm, rcond, work, 1, 0, iwork, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( rcond[0], tc.rcond, 1e-10, 'rcond' );
});

test( 'dgbcon: N=0', function t() {
	var rcond;
	var iwork;
	var work;
	var ipiv;
	var info;
	var tc;
	var ab;

	tc = n_zero;
	ab = new Float64Array( 1 );
	ipiv = new Int32Array( 1 );
	work = new Float64Array( 3 );
	iwork = new Int32Array( 1 );
	rcond = new Float64Array( 1 );
	info = dgbcon( 'one-norm', 0, 0, 0, ab, 1, 1, 0, ipiv, 1, 0, 0.0, rcond, work, 1, 0, iwork, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( rcond[0], tc.rcond, 1e-14, 'rcond' );
});

test( 'dgbcon: N=1', function t() {
	var rcond;
	var iwork;
	var work;
	var ipiv;
	var info;
	var tc;
	var ab;

	tc = n_one;
	ab = new Float64Array([ 3.0 ]);
	ipiv = new Int32Array( 1 );
	work = new Float64Array( 3 );
	iwork = new Int32Array( 1 );
	rcond = new Float64Array( 1 );
	info = dgbtrf( 1, 1, 0, 0, ab, 1, 1, 0, ipiv, 1, 0 );
	assert.equal( info, 0, 'dgbtrf info' );
	info = dgbcon( 'one-norm', 1, 0, 0, ab, 1, 1, 0, ipiv, 1, 0, 3.0, rcond, work, 1, 0, iwork, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( rcond[0], tc.rcond, 1e-14, 'rcond' );
});

test( 'dgbcon: anorm=0', function t() {
	var rcond;
	var iwork;
	var work;
	var ipiv;
	var info;
	var ldab;
	var tc;
	var ab;
	var n;

	tc = anorm_zero;
	n = 4;
	ldab = 4;
	ab = bandedMatrix( ldab, n, [
		[2, 0, 4.0],
		[3, 0, 1.0],
		[1, 1, 1.0],
		[2, 1, 5.0],
		[3, 1, 1.0],
		[1, 2, 2.0],
		[2, 2, 6.0],
		[3, 2, 2.0],
		[1, 3, 1.0],
		[2, 3, 7.0]
	]);
	ipiv = new Int32Array( n );
	work = new Float64Array( 3 * n );
	iwork = new Int32Array( n );
	rcond = new Float64Array( 1 );
	dgbtrf( n, n, 1, 1, ab, 1, ldab, 0, ipiv, 1, 0 );
	info = dgbcon( 'one-norm', n, 1, 1, ab, 1, ldab, 0, ipiv, 1, 0, 0.0, rcond, work, 1, 0, iwork, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( rcond[0], tc.rcond, 1e-14, 'rcond' );
});
