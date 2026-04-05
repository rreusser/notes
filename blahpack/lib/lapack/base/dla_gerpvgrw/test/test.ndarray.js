/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';


// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dla_gerpvgrw = require( './../lib' );


// FUNCTIONS //

/**
* Creates a column-major matrix from a flat array of values.
*
* @private
* @param {number} M - number of rows
* @param {number} N - number of columns
* @param {Array} vals - values in row-major order (for readability)
* @returns {Float64Array} column-major array
*/
function colMajor( M, N, vals ) {
	var out = new Float64Array( M * N );
	var i;
	var j;
	for ( i = 0; i < M; i++ ) {
		for ( j = 0; j < N; j++ ) {
			out[ j * M + i ] = vals[ i * N + j ];
		}
	}
	return out;
}


// TESTS //

test( 'dla_gerpvgrw: main export is a function', function t() {
	assert.strictEqual( typeof dla_gerpvgrw, 'function' );
});

test( 'dla_gerpvgrw: attached to the main export is an `ndarray` method', function t() { // eslint-disable-line max-len
	assert.strictEqual( typeof dla_gerpvgrw.ndarray, 'function' );
});

test( 'dla_gerpvgrw: basic 3x3 matrix', function t() {
	var result;
	var AF;
	var A;

	A = colMajor( 3, 3, [
		6.0,
		2.0,
		1.0,
		2.0,
		5.0,
		3.0,
		1.0,
		3.0,
		4.0
	]);
	AF = colMajor( 3, 3, [
		6.0,
		2.0,
		1.0,
		0.0,
		4.0,
		2.5,
		0.0,
		0.0,
		2.0
	]);
	result = dla_gerpvgrw.ndarray( 3, 3, A, 1, 3, 0, AF, 1, 3, 0 );
	assert.strictEqual( result, 1.0 );
});

test( 'dla_gerpvgrw: ncols=0 returns 1.0', function t() {
	var result;
	var AF;
	var A;

	A = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
	AF = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
	result = dla_gerpvgrw.ndarray( 3, 0, A, 1, 3, 0, AF, 1, 3, 0 );
	assert.strictEqual( result, 1.0 );
});

test( 'dla_gerpvgrw: 1x1 matrix with equal A and AF', function t() {
	var result;
	var AF;
	var A;

	A = new Float64Array( [ 5.0 ] );
	AF = new Float64Array( [ 5.0 ] );
	result = dla_gerpvgrw.ndarray( 1, 1, A, 1, 1, 0, AF, 1, 1, 0 );
	assert.strictEqual( result, 1.0 );
});

test( 'dla_gerpvgrw: 1x1 with pivot growth (ratio=0.5)', function t() {
	var result;
	var AF;
	var A;

	A = new Float64Array( [ 3.0 ] );
	AF = new Float64Array( [ 6.0 ] );
	result = dla_gerpvgrw.ndarray( 1, 1, A, 1, 1, 0, AF, 1, 1, 0 );
	assert.strictEqual( result, 0.5 );
});

test( 'dla_gerpvgrw: zero U column (umax=0) is skipped', function t() {
	var result;
	var AF;
	var A;

	A = colMajor( 3, 3, [
		4.0,
		0.0,
		3.0,
		2.0,
		0.0,
		1.0,
		1.0,
		0.0,
		2.0
	]);
	AF = colMajor( 3, 3, [
		4.0,
		0.0,
		3.0,
		0.0,
		0.0,
		1.0,
		0.0,
		0.0,
		2.0
	]);
	result = dla_gerpvgrw.ndarray( 3, 3, A, 1, 3, 0, AF, 1, 3, 0 );
	assert.strictEqual( result, 1.0 );
});

test( 'dla_gerpvgrw: 2x2 pivot growth', function t() {
	var result;
	var AF;
	var A;

	A = colMajor( 2, 2, [
		10.0,
		8.0,
		7.0,
		9.0
	]);
	AF = colMajor( 2, 2, [
		10.0,
		8.0,
		0.0,
		1.0
	]);
	result = dla_gerpvgrw.ndarray( 2, 2, A, 1, 2, 0, AF, 1, 2, 0 );
	assert.strictEqual( result, 1.0 );
});

test( 'dla_gerpvgrw: ratio less than one', function t() {
	var result;
	var AF;
	var A;

	A = colMajor( 2, 2, [
		1.0,
		2.0,
		3.0,
		4.0
	]);
	AF = colMajor( 2, 2, [
		5.0,
		6.0,
		0.0,
		8.0
	]);
	result = dla_gerpvgrw.ndarray( 2, 2, A, 1, 2, 0, AF, 1, 2, 0 );
	assert.strictEqual( result, 0.5 );
});

test( 'dla_gerpvgrw: ncols < N processes only first ncols columns', function t() { // eslint-disable-line max-len
	var result;
	var AF;
	var A;

	A = colMajor( 3, 3, [
		4.0,
		2.0,
		99.0,
		1.0,
		5.0,
		99.0,
		3.0,
		6.0,
		99.0
	]);
	AF = colMajor( 3, 3, [
		4.0,
		2.0,
		99.0,
		0.0,
		5.0,
		99.0,
		0.0,
		0.0,
		99.0
	]);
	result = dla_gerpvgrw.ndarray( 3, 2, A, 1, 3, 0, AF, 1, 3, 0 );
	assert.strictEqual( result, 1.0 );
});

test( 'dla_gerpvgrw: large values', function t() {
	var result;
	var AF;
	var A;

	A = colMajor( 2, 2, [
		1.0e100,
		2.0e100,
		3.0e100,
		4.0e100
	]);
	AF = colMajor( 2, 2, [
		6.0e100,
		5.0e100,
		0.0,
		8.0e100
	]);
	result = dla_gerpvgrw.ndarray( 2, 2, A, 1, 2, 0, AF, 1, 2, 0 );
	assert.strictEqual( result, 0.5 );
});

test( 'dla_gerpvgrw: works with non-unit strides and offsets', function t() {
	var result;
	var AF;
	var A;

	A = new Float64Array([
		1.0,
		999.0,
		3.0,
		999.0,
		2.0,
		999.0,
		4.0,
		999.0
	]);
	AF = new Float64Array([
		5.0,
		999.0,
		0.0,
		999.0,
		6.0,
		999.0,
		8.0,
		999.0
	]);
	result = dla_gerpvgrw.ndarray( 2, 2, A, 2, 4, 0, AF, 2, 4, 0 );
	assert.strictEqual( result, 0.5 );
});

test( 'dla_gerpvgrw: works with offset', function t() {
	var result;
	var AF;
	var A;

	A = new Float64Array( [ 0.0, 0.0, 0.0, 7.0 ] );
	AF = new Float64Array( [ 0.0, 0.0, 0.0, 14.0 ] );
	result = dla_gerpvgrw.ndarray( 1, 1, A, 1, 1, 3, AF, 1, 1, 3 );
	assert.strictEqual( result, 0.5 );
});

test( 'dla_gerpvgrw: BLAS-style entry via main export', function t() {
	var result;
	var AF;
	var A;

	A = colMajor( 2, 2, [
		1.0,
		2.0,
		3.0,
		4.0
	]);
	AF = colMajor( 2, 2, [
		5.0,
		6.0,
		0.0,
		8.0
	]);
	result = dla_gerpvgrw( 2, 2, A, 2, AF, 2 );
	assert.strictEqual( result, 0.5 );
});

test( 'dla_gerpvgrw: negative values in A', function t() {
	var result;
	var AF;
	var A;

	A = colMajor( 2, 2, [
		-3.0,
		-7.0,
		-5.0,
		-2.0
	]);
	AF = colMajor( 2, 2, [
		10.0,
		7.0,
		0.0,
		4.0
	]);
	result = dla_gerpvgrw.ndarray( 2, 2, A, 1, 2, 0, AF, 1, 2, 0 );
	assert.strictEqual( result, 0.5 );
});
