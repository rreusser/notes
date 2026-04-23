/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var dznrm2 = require( './../lib/base.js' );

// FIXTURES //

var single = require( './fixtures/single.json' );
var two_elements = require( './fixtures/two_elements.json' );
var n_zero = require( './fixtures/n_zero.json' );
var three_elements = require( './fixtures/three_elements.json' );
var stride_2 = require( './fixtures/stride_2.json' );
var large_values = require( './fixtures/large_values.json' );
var small_values = require( './fixtures/small_values.json' );
var large_and_medium = require( './fixtures/large_and_medium.json' );
var small_and_medium = require( './fixtures/small_and_medium.json' );
var small_dominant = require( './fixtures/small_dominant.json' );

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
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual + ' (relErr=' + relErr + ')' ); // eslint-disable-line max-len
}

test( 'dznrm2: single complex element (3+4i) has norm 5', function t() {
	var tc = single;
	var zx = new Complex128Array( [ 3, 4 ] );
	assertClose( dznrm2( 1, zx, 1, 0 ), tc.result, 1e-14, 'single' );
});

test( 'dznrm2: two elements (1+0i), (0+1i) has norm sqrt(2)', function t() {
	var tc = two_elements;
	var zx = new Complex128Array( [ 1, 0, 0, 1 ] );
	assertClose( dznrm2( 2, zx, 1, 0 ), tc.result, 1e-14, 'two_elements' );
});

test( 'dznrm2: n=0 returns 0', function t() {
	var tc = n_zero;
	assert.equal( dznrm2( 0, new Complex128Array( 1 ), 1, 0 ), tc.result );
});

test( 'dznrm2: three elements', function t() {
	var tc = three_elements;
	var zx = new Complex128Array( [ 1, 2, 3, 4, 5, 6 ] );
	assertClose( dznrm2( 3, zx, 1, 0 ), tc.result, 1e-14, 'three_elements' );
});

test( 'dznrm2: stride=2', function t() {
	var tc = stride_2;

	// x(1) = (1,0), x(2) = (99,99), x(3) = (0,1) — stride 2 skips x(2)
	var zx = new Complex128Array( [ 1, 0, 99, 99, 0, 1 ] );
	assertClose( dznrm2( 2, zx, 2, 0 ), tc.result, 1e-14, 'stride_2' );
});

test( 'dznrm2: large values trigger overflow-safe (abig) path', function t() {
	var tc = large_values;
	var zx = new Complex128Array( [ 1e300, 1e300, 1e300, 1e300 ] );
	assertClose( dznrm2( 2, zx, 1, 0 ), tc.result, 1e-14, 'large_values' );
});

test( 'dznrm2: small values trigger underflow-safe (asml) path', function t() {
	var tc = small_values;
	var zx = new Complex128Array( [ 1e-300, 1e-300, 1e-300, 1e-300 ] );
	assertClose( dznrm2( 2, zx, 1, 0 ), tc.result, 1e-14, 'small_values' );
});

test( 'dznrm2: large and medium values (abig > 0, amed > 0)', function t() {
	var tc = large_and_medium;
	var zx = new Complex128Array( [ 1e300, 1e300, 1, 1 ] );
	assertClose( dznrm2( 2, zx, 1, 0 ), tc.result, 1e-14, 'large_and_medium' );
});

test( 'dznrm2: small and medium values (asml > 0, amed > 0, asml < amed)', function t() { // eslint-disable-line max-len
	var tc = small_and_medium;
	var zx = new Complex128Array( [ 1e-300, 1e-300, 1, 1 ] );
	assertClose( dznrm2( 2, zx, 1, 0 ), tc.result, 1e-14, 'small_and_medium' );
});

test( 'dznrm2: small dominant with medium (asml > amed branch)', function t() {
	var tc = small_dominant;

	// Many small complex values (just below TSML ~1.49e-154) plus one tiny medium value // eslint-disable-line max-len

	// (just above TSML). After scaling, small contribution dominates medium.
	var zx = new Complex128Array([
		1e-154,
		1e-154,  // small
		1e-154,
		1e-154,  // small
		1e-154,
		1e-154,  // small
		1e-154,
		1e-154,  // small
		1e-154,
		1e-154,  // small
		1e-154,
		1e-154,  // small
		1e-154,
		1e-154,  // small
		1e-154,
		1e-154,  // small
		1e-154,
		1e-154,  // small
		1.5e-154,
		0      // medium (just above TSML)
	]);
	assertClose( dznrm2( 10, zx, 1, 0 ), tc.result, 1e-14, 'small_dominant' );
});
