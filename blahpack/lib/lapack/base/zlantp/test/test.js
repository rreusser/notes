/* eslint-disable no-restricted-syntax, stdlib/first-unit-test, max-lines */

'use strict';

// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var zlantp = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'zlantp.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
var fixture = lines.map( function parse( line ) {
	return JSON.parse( line );
} );

// Upper packed 3x3 triangular (column-major interleaved re/im):
var AP3U = [ 1.0, 2.0, 3.0, 4.0, 7.0, 8.0, 5.0, 6.0, 9.0, 1.0, 2.0, 3.0 ]; // eslint-disable-line max-len

// Lower packed 3x3 triangular (column-major interleaved re/im):
var AP3L = [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 1.0, 2.0, 3.0 ]; // eslint-disable-line max-len

// Upper packed 4x4 triangular (column-major interleaved re/im):
var AP4U = [ 1.0, 1.0, 2.0, -1.0, 5.0, 1.0, 3.0, 2.0, 1.0, -2.0, 6.0, -1.0, 4.0, -3.0, 2.0, 4.0, 3.0, 1.0, 7.0, 2.0 ]; // eslint-disable-line max-len

// Lower packed 4x4 triangular (column-major interleaved re/im):
var AP4L = [ 1.0, 1.0, 2.0, -1.0, 3.0, 2.0, 4.0, -3.0, 5.0, 1.0, 1.0, -2.0, 2.0, 4.0, 6.0, -1.0, 3.0, 1.0, 7.0, 2.0 ]; // eslint-disable-line max-len


// FUNCTIONS //

/**
* Creates a Complex128Array from interleaved real/imaginary pairs.
*
* @private
* @param {Array} arr - interleaved values
* @returns {Complex128Array} complex array
*/
function c128( arr ) {
	return new Complex128Array( arr );
}

/**
* Returns a test case from the fixture data.
*
* @private
* @param {string} name - test case name
* @returns {*} result
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
* @param {*} actual - actual value
* @param {*} expected - expected value
* @param {number} tol - tolerance
* @param {string} msg - assertion message
*/
function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 ); // eslint-disable-line max-len
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}


// TESTS //

test( 'zlantp is a function', function t() {
	assert.strictEqual( typeof zlantp, 'function' );
});

// 3x3 upper, non-unit

test( 'zlantp: max_upper_nonunit', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( 'max_upper_nonunit' );
	ap = c128( AP3U );
	work = new Float64Array( 3 );
	result = zlantp( 'max', 'upper', 'non-unit', 3, ap, 1, 0, work, 1, 0 );
	assert.strictEqual( typeof result, 'number', 'returns a number' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantp: one_upper_nonunit', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( 'one_upper_nonunit' );
	ap = c128( AP3U );
	work = new Float64Array( 3 );
	result = zlantp( 'one-norm', 'upper', 'non-unit', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantp: inf_upper_nonunit', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( 'inf_upper_nonunit' );
	ap = c128( AP3U );
	work = new Float64Array( 3 );
	result = zlantp( 'inf-norm', 'upper', 'non-unit', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantp: frob_upper_nonunit', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( 'frob_upper_nonunit' );
	ap = c128( AP3U );
	work = new Float64Array( 3 );
	result = zlantp( 'frobenius', 'upper', 'non-unit', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

// 3x3 upper, unit

test( 'zlantp: max_upper_unit', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( 'max_upper_unit' );
	ap = c128( AP3U );
	work = new Float64Array( 3 );
	result = zlantp( 'max', 'upper', 'unit', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantp: one_upper_unit', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( 'one_upper_unit' );
	ap = c128( AP3U );
	work = new Float64Array( 3 );
	result = zlantp( 'one-norm', 'upper', 'unit', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantp: inf_upper_unit', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( 'inf_upper_unit' );
	ap = c128( AP3U );
	work = new Float64Array( 3 );
	result = zlantp( 'inf-norm', 'upper', 'unit', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantp: frob_upper_unit', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( 'frob_upper_unit' );
	ap = c128( AP3U );
	work = new Float64Array( 3 );
	result = zlantp( 'frobenius', 'upper', 'unit', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

// 3x3 lower, non-unit

test( 'zlantp: max_lower_nonunit', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( 'max_lower_nonunit' );
	ap = c128( AP3L );
	work = new Float64Array( 3 );
	result = zlantp( 'max', 'lower', 'non-unit', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantp: one_lower_nonunit', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( 'one_lower_nonunit' );
	ap = c128( AP3L );
	work = new Float64Array( 3 );
	result = zlantp( 'one-norm', 'lower', 'non-unit', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantp: inf_lower_nonunit', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( 'inf_lower_nonunit' );
	ap = c128( AP3L );
	work = new Float64Array( 3 );
	result = zlantp( 'inf-norm', 'lower', 'non-unit', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantp: frob_lower_nonunit', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( 'frob_lower_nonunit' );
	ap = c128( AP3L );
	work = new Float64Array( 3 );
	result = zlantp( 'frobenius', 'lower', 'non-unit', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

// 3x3 lower, unit

test( 'zlantp: max_lower_unit', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( 'max_lower_unit' );
	ap = c128( AP3L );
	work = new Float64Array( 3 );
	result = zlantp( 'max', 'lower', 'unit', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantp: one_lower_unit', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( 'one_lower_unit' );
	ap = c128( AP3L );
	work = new Float64Array( 3 );
	result = zlantp( 'one-norm', 'lower', 'unit', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantp: inf_lower_unit', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( 'inf_lower_unit' );
	ap = c128( AP3L );
	work = new Float64Array( 3 );
	result = zlantp( 'inf-norm', 'lower', 'unit', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantp: frob_lower_unit', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( 'frob_lower_unit' );
	ap = c128( AP3L );
	work = new Float64Array( 3 );
	result = zlantp( 'frobenius', 'lower', 'unit', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

// 4x4 upper, non-unit

test( 'zlantp: 4x4_max_upper_nonunit', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( '4x4_max_upper_nonunit' );
	ap = c128( AP4U );
	work = new Float64Array( 4 );
	result = zlantp( 'max', 'upper', 'non-unit', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantp: 4x4_one_upper_nonunit', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( '4x4_one_upper_nonunit' );
	ap = c128( AP4U );
	work = new Float64Array( 4 );
	result = zlantp( 'one-norm', 'upper', 'non-unit', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantp: 4x4_inf_upper_nonunit', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( '4x4_inf_upper_nonunit' );
	ap = c128( AP4U );
	work = new Float64Array( 4 );
	result = zlantp( 'inf-norm', 'upper', 'non-unit', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantp: 4x4_frob_upper_nonunit', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( '4x4_frob_upper_nonunit' );
	ap = c128( AP4U );
	work = new Float64Array( 4 );
	result = zlantp( 'frobenius', 'upper', 'non-unit', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

// 4x4 upper, unit

test( 'zlantp: 4x4_max_upper_unit', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( '4x4_max_upper_unit' );
	ap = c128( AP4U );
	work = new Float64Array( 4 );
	result = zlantp( 'max', 'upper', 'unit', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantp: 4x4_one_upper_unit', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( '4x4_one_upper_unit' );
	ap = c128( AP4U );
	work = new Float64Array( 4 );
	result = zlantp( 'one-norm', 'upper', 'unit', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantp: 4x4_inf_upper_unit', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( '4x4_inf_upper_unit' );
	ap = c128( AP4U );
	work = new Float64Array( 4 );
	result = zlantp( 'inf-norm', 'upper', 'unit', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantp: 4x4_frob_upper_unit', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( '4x4_frob_upper_unit' );
	ap = c128( AP4U );
	work = new Float64Array( 4 );
	result = zlantp( 'frobenius', 'upper', 'unit', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

// 4x4 lower, non-unit

test( 'zlantp: 4x4_max_lower_nonunit', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( '4x4_max_lower_nonunit' );
	ap = c128( AP4L );
	work = new Float64Array( 4 );
	result = zlantp( 'max', 'lower', 'non-unit', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantp: 4x4_one_lower_nonunit', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( '4x4_one_lower_nonunit' );
	ap = c128( AP4L );
	work = new Float64Array( 4 );
	result = zlantp( 'one-norm', 'lower', 'non-unit', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantp: 4x4_inf_lower_nonunit', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( '4x4_inf_lower_nonunit' );
	ap = c128( AP4L );
	work = new Float64Array( 4 );
	result = zlantp( 'inf-norm', 'lower', 'non-unit', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantp: 4x4_frob_lower_nonunit', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( '4x4_frob_lower_nonunit' );
	ap = c128( AP4L );
	work = new Float64Array( 4 );
	result = zlantp( 'frobenius', 'lower', 'non-unit', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

// 4x4 lower, unit

test( 'zlantp: 4x4_max_lower_unit', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( '4x4_max_lower_unit' );
	ap = c128( AP4L );
	work = new Float64Array( 4 );
	result = zlantp( 'max', 'lower', 'unit', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantp: 4x4_one_lower_unit', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( '4x4_one_lower_unit' );
	ap = c128( AP4L );
	work = new Float64Array( 4 );
	result = zlantp( 'one-norm', 'lower', 'unit', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantp: 4x4_inf_lower_unit', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( '4x4_inf_lower_unit' );
	ap = c128( AP4L );
	work = new Float64Array( 4 );
	result = zlantp( 'inf-norm', 'lower', 'unit', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantp: 4x4_frob_lower_unit', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( '4x4_frob_lower_unit' );
	ap = c128( AP4L );
	work = new Float64Array( 4 );
	result = zlantp( 'frobenius', 'lower', 'unit', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

// N=0 quick return

test( 'zlantp: n_zero', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( 'n_zero' );
	ap = c128( [] );
	work = new Float64Array( 0 );
	result = zlantp( 'max', 'upper', 'non-unit', 0, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

// N=1, non-unit

test( 'zlantp: 1x1_max_nonunit', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( '1x1_max_nonunit' );
	ap = c128( [ 3.0, 4.0 ] );
	work = new Float64Array( 1 );
	result = zlantp( 'max', 'upper', 'non-unit', 1, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantp: 1x1_one_nonunit', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( '1x1_one_nonunit' );
	ap = c128( [ 3.0, 4.0 ] );
	work = new Float64Array( 1 );
	result = zlantp( 'one-norm', 'upper', 'non-unit', 1, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantp: 1x1_inf_nonunit', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( '1x1_inf_nonunit' );
	ap = c128( [ 3.0, 4.0 ] );
	work = new Float64Array( 1 );
	result = zlantp( 'inf-norm', 'upper', 'non-unit', 1, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantp: 1x1_frob_nonunit', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( '1x1_frob_nonunit' );
	ap = c128( [ 3.0, 4.0 ] );
	work = new Float64Array( 1 );
	result = zlantp( 'frobenius', 'upper', 'non-unit', 1, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

// N=1, unit diagonal

test( 'zlantp: 1x1_max_unit', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( '1x1_max_unit' );
	ap = c128( [ 3.0, 4.0 ] );
	work = new Float64Array( 1 );
	result = zlantp( 'max', 'upper', 'unit', 1, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantp: 1x1_frob_unit', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = findCase( '1x1_frob_unit' );
	ap = c128( [ 3.0, 4.0 ] );
	work = new Float64Array( 1 );
	result = zlantp( 'frobenius', 'upper', 'unit', 1, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});
