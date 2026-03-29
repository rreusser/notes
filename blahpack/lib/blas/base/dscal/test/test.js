/* eslint-disable no-restricted-syntax, stdlib/require-globals, stdlib/first-unit-test */

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dscal = require( './../lib/base.js' );

test( 'dscal: main export is a function', function t() {
	assert.strictEqual( typeof dscal, 'function' );
});

test( 'dscal: N=0 returns x unchanged', function t() {
	var out = dscal( 0, 2.0, x, 1, 0 );
	var x = new Float64Array( [ 1, 2, 3 ] );
	assert.strictEqual( out, x );
	assert.deepStrictEqual( Array.from( x ), [ 1, 2, 3 ] );
});

test( 'dscal: N<0 returns x unchanged', function t() {
	var x = new Float64Array( [ 1, 2, 3 ] );
	dscal( -1, 2.0, x, 1, 0 );
	assert.deepStrictEqual( Array.from( x ), [ 1, 2, 3 ] );
});

test( 'dscal: basic scaling (N=3, da=2, unit stride)', function t() {
	var out = dscal( 3, 2.0, x, 1, 0 );
	var x = new Float64Array( [ 1, 2, 3 ] );
	assert.strictEqual( out, x, 'returns x' );
	assert.deepStrictEqual( Array.from( x ), [ 2, 4, 6 ] );
});

test( 'dscal: N=1', function t() {
	var x = new Float64Array( [ 5 ] );
	dscal( 1, 3.0, x, 1, 0 );
	assert.strictEqual( x[ 0 ], 15.0 );
});

test( 'dscal: da=0 zeros the vector', function t() {
	var x = new Float64Array( [ 1, 2, 3 ] );
	dscal( 3, 0.0, x, 1, 0 );
	assert.deepStrictEqual( Array.from( x ), [ 0, 0, 0 ] );
});

test( 'dscal: da=1 is identity', function t() {
	var x = new Float64Array( [ 1, 2, 3 ] );
	dscal( 3, 1.0, x, 1, 0 );
	assert.deepStrictEqual( Array.from( x ), [ 1, 2, 3 ] );
});

test( 'dscal: unrolled path (N=7, unit stride)', function t() {
	// N=7: m=2 remainder, then one block of 5
	var x = new Float64Array( [ 1, 2, 3, 4, 5, 6, 7 ] );
	dscal( 7, 2.0, x, 1, 0 );
	assert.deepStrictEqual( Array.from( x ), [ 2, 4, 6, 8, 10, 12, 14 ] );
});

test( 'dscal: unrolled path (N=5, exact multiple)', function t() {
	// M = 5%5 = 0, remainder loop skipped
	var x = new Float64Array( [ 1, 2, 3, 4, 5 ] );
	dscal( 5, 3.0, x, 1, 0 );
	assert.deepStrictEqual( Array.from( x ), [ 3, 6, 9, 12, 15 ] );
});

test( 'dscal: unrolled path (N=10, two blocks)', function t() {
	var x = new Float64Array( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ] );
	dscal( 10, -1.0, x, 1, 0 );
	assert.deepStrictEqual( Array.from( x ), [ -1, -2, -3, -4, -5, -6, -7, -8, -9, -10 ] );
});

test( 'dscal: non-unit stride (strideX=2)', function t() {
	// x = [1, _, 3, _, 5], strideX=2 → scales x[0], x[2], x[4]
	var x = new Float64Array( [ 1, 99, 3, 99, 5 ] );
	dscal( 3, 2.0, x, 2, 0 );
	assert.deepStrictEqual( Array.from( x ), [ 2, 99, 6, 99, 10 ] );
});

test( 'dscal: negative stride', function t() {
	var x = new Float64Array( [ 1, 2, 3 ] );
	dscal( 3, 2.0, x, -1, 2 );
	assert.deepStrictEqual( Array.from( x ), [ 2, 4, 6 ] );
});

test( 'dscal: offsetX non-zero', function t() {
	var x = new Float64Array( [ 99, 1, 2, 3 ] );
	dscal( 3, 10.0, x, 1, 1 );
	assert.deepStrictEqual( Array.from( x ), [ 99, 10, 20, 30 ] );
});

test( 'dscal: negative da', function t() {
	var x = new Float64Array( [ 1, -2, 3 ] );
	dscal( 3, -2.0, x, 1, 0 );
	assert.deepStrictEqual( Array.from( x ), [ -2, 4, -6 ] );
});
