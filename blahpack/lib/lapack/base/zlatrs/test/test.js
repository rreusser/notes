/**
* @license Apache-2.0
*
* Copyright (c) 2025 The Stdlib Authors.
*/

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zlatrs = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zlatrs.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var diff = Math.abs( actual - expected );
	var denom = Math.max( Math.abs( expected ), 1.0 );
	var relErr = diff / denom;
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual + ' (relErr=' + relErr + ')' );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}


// HELPERS //

/**
* Creates an NxN Complex128Array in column-major layout from a flat Float64 interleaved array.
*
* @private
* @param {Array} vals - interleaved [re,im,...] for column-major elements
* @param {integer} N - matrix dimension
* @returns {Complex128Array} matrix
*/
function makeMatrix( vals, N ) {
	var buf = new Complex128Array( N * N );
	var v = reinterpret( buf, 0 );
	var i;
	for ( i = 0; i < vals.length; i++ ) {
		v[ i ] = vals[ i ];
	}
	return buf;
}

/**
* Creates a Complex128Array vector from flat Float64 interleaved array.
*
* @private
* @param {Array} vals - interleaved [re,im,...]
* @returns {Complex128Array} vector
*/
function makeVector( vals ) {
	var buf = new Complex128Array( vals.length / 2 );
	var v = reinterpret( buf, 0 );
	var i;
	for ( i = 0; i < vals.length; i++ ) {
		v[ i ] = vals[ i ];
	}
	return buf;
}


// TESTS //

test( 'zlatrs is a function', function t() {
	assert.strictEqual( typeof zlatrs, 'function' );
});

test( 'zlatrs: N=0 returns immediately', function t() {
	var tc = findCase( 'n_zero' );
	var A = new Complex128Array( 1 );
	var x = new Complex128Array( 1 );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 1 );
	var info;

	info = zlatrs( 'upper', 'no-transpose', 'non-unit', 'N', 0, A, 1, 1, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
});

test( 'zlatrs: N=1 upper, no-transpose, non-unit', function t() {
	var tc = findCase( 'n_one' );
	var A = makeMatrix( [ 5.0, 2.0 ], 1 );
	var x = makeVector( [ 10.0, -3.0 ] );
	var xv = reinterpret( x, 0 );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 1 );
	var info;

	info = zlatrs( 'upper', 'no-transpose', 'non-unit', 'N', 1, A, 1, 1, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( xv, tc.x, 1e-13, 'x' );
});

test( 'zlatrs: upper, no-transpose, non-unit, 3x3', function t() {
	var tc = findCase( 'upper_N_nonunit' );
	// A upper triangular 3x3 column-major in a 4x4 leading dim = 4
	// In JS: strideA1=1, strideA2=N=3, leading dim matches N
	var A = makeMatrix( [
		2.0, 1.0,  0.0, 0.0,  0.0, 0.0,
		1.0, 1.0,  3.0, 0.5,  0.0, 0.0,
		0.5, 0.0,  1.0, -1.0, 4.0, -1.0
	], 3 );
	var x = makeVector( [ 1.0, 0.0, 2.0, 1.0, 3.0, -1.0 ] );
	var xv = reinterpret( x, 0 );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info;

	info = zlatrs( 'upper', 'no-transpose', 'non-unit', 'N', 3, A, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( xv, tc.x, 1e-13, 'x' );
	assertArrayClose( cnorm, tc.cnorm, 1e-14, 'cnorm' );
});

test( 'zlatrs: lower, no-transpose, non-unit, 3x3', function t() {
	var tc = findCase( 'lower_N_nonunit' );
	var A = makeMatrix( [
		2.0, 1.0,  1.0, 1.0,  0.5, 0.0,
		0.0, 0.0,  3.0, 0.5,  1.0, -1.0,
		0.0, 0.0,  0.0, 0.0,  4.0, -1.0
	], 3 );
	var x = makeVector( [ 1.0, 0.0, 2.0, 1.0, 3.0, -1.0 ] );
	var xv = reinterpret( x, 0 );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info;

	info = zlatrs( 'lower', 'no-transpose', 'non-unit', 'N', 3, A, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( xv, tc.x, 1e-13, 'x' );
	assertArrayClose( cnorm, tc.cnorm, 1e-14, 'cnorm' );
});

test( 'zlatrs: upper, transpose, non-unit, 3x3', function t() {
	var tc = findCase( 'upper_T_nonunit' );
	var A = makeMatrix( [
		2.0, 1.0,  0.0, 0.0,  0.0, 0.0,
		1.0, 1.0,  3.0, 0.5,  0.0, 0.0,
		0.5, 0.0,  1.0, -1.0, 4.0, -1.0
	], 3 );
	var x = makeVector( [ 1.0, 0.0, 2.0, 1.0, 3.0, -1.0 ] );
	var xv = reinterpret( x, 0 );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info;

	info = zlatrs( 'upper', 'transpose', 'non-unit', 'N', 3, A, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( xv, tc.x, 1e-13, 'x' );
});

test( 'zlatrs: upper, conjugate-transpose, non-unit, 3x3', function t() {
	var tc = findCase( 'upper_C_nonunit' );
	var A = makeMatrix( [
		2.0, 1.0,  0.0, 0.0,  0.0, 0.0,
		1.0, 1.0,  3.0, 0.5,  0.0, 0.0,
		0.5, 0.0,  1.0, -1.0, 4.0, -1.0
	], 3 );
	var x = makeVector( [ 1.0, 0.0, 2.0, 1.0, 3.0, -1.0 ] );
	var xv = reinterpret( x, 0 );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info;

	info = zlatrs( 'upper', 'conjugate-transpose', 'non-unit', 'N', 3, A, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( xv, tc.x, 1e-13, 'x' );
});

test( 'zlatrs: lower, transpose, non-unit, 3x3', function t() {
	var tc = findCase( 'lower_T_nonunit' );
	var A = makeMatrix( [
		2.0, 1.0,  1.0, 1.0,  0.5, 0.0,
		0.0, 0.0,  3.0, 0.5,  1.0, -1.0,
		0.0, 0.0,  0.0, 0.0,  4.0, -1.0
	], 3 );
	var x = makeVector( [ 1.0, 0.0, 2.0, 1.0, 3.0, -1.0 ] );
	var xv = reinterpret( x, 0 );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info;

	info = zlatrs( 'lower', 'transpose', 'non-unit', 'N', 3, A, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( xv, tc.x, 1e-13, 'x' );
});

test( 'zlatrs: lower, conjugate-transpose, non-unit, 3x3', function t() {
	var tc = findCase( 'lower_C_nonunit' );
	var A = makeMatrix( [
		2.0, 1.0,  1.0, 1.0,  0.5, 0.0,
		0.0, 0.0,  3.0, 0.5,  1.0, -1.0,
		0.0, 0.0,  0.0, 0.0,  4.0, -1.0
	], 3 );
	var x = makeVector( [ 1.0, 0.0, 2.0, 1.0, 3.0, -1.0 ] );
	var xv = reinterpret( x, 0 );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info;

	info = zlatrs( 'lower', 'conjugate-transpose', 'non-unit', 'N', 3, A, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( xv, tc.x, 1e-13, 'x' );
});

test( 'zlatrs: upper, no-transpose, unit diagonal, 3x3', function t() {
	var tc = findCase( 'upper_N_unit' );
	var A = makeMatrix( [
		99.0, 99.0,  0.0, 0.0,   0.0, 0.0,
		1.0, 1.0,    99.0, 99.0, 0.0, 0.0,
		0.5, 0.0,    1.0, -1.0,  99.0, 99.0
	], 3 );
	var x = makeVector( [ 1.0, 0.0, 2.0, 1.0, 3.0, -1.0 ] );
	var xv = reinterpret( x, 0 );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info;

	info = zlatrs( 'upper', 'no-transpose', 'unit', 'N', 3, A, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( xv, tc.x, 1e-13, 'x' );
});

test( 'zlatrs: lower, no-transpose, unit diagonal, 3x3', function t() {
	var tc = findCase( 'lower_N_unit' );
	var A = makeMatrix( [
		99.0, 99.0, 1.0, 1.0,   0.5, 0.0,
		0.0, 0.0,   99.0, 99.0, 1.0, -1.0,
		0.0, 0.0,   0.0, 0.0,   99.0, 99.0
	], 3 );
	var x = makeVector( [ 1.0, 0.0, 2.0, 1.0, 3.0, -1.0 ] );
	var xv = reinterpret( x, 0 );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info;

	info = zlatrs( 'lower', 'no-transpose', 'unit', 'N', 3, A, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( xv, tc.x, 1e-13, 'x' );
});

test( 'zlatrs: upper, no-transpose, normin=Y, 3x3', function t() {
	var tc = findCase( 'upper_N_normin_Y' );
	var A = makeMatrix( [
		2.0, 1.0,  0.0, 0.0,  0.0, 0.0,
		1.0, 1.0,  3.0, 0.5,  0.0, 0.0,
		0.5, 0.0,  1.0, -1.0, 4.0, -1.0
	], 3 );
	var x = makeVector( [ 1.0, 0.0, 2.0, 1.0, 3.0, -1.0 ] );
	var xv = reinterpret( x, 0 );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( [ 0.0, 2.0, 2.5 ] );
	var info;

	info = zlatrs( 'upper', 'no-transpose', 'non-unit', 'Y', 3, A, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( xv, tc.x, 1e-13, 'x' );
});

test( 'zlatrs: upper, conjugate-transpose, unit diagonal, 3x3', function t() {
	var tc = findCase( 'upper_C_unit' );
	var A = makeMatrix( [
		99.0, 99.0,  0.0, 0.0,   0.0, 0.0,
		1.0, 1.0,    99.0, 99.0, 0.0, 0.0,
		0.5, 0.0,    1.0, -1.0,  99.0, 99.0
	], 3 );
	var x = makeVector( [ 1.0, 0.0, 2.0, 1.0, 3.0, -1.0 ] );
	var xv = reinterpret( x, 0 );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info;

	info = zlatrs( 'upper', 'conjugate-transpose', 'unit', 'N', 3, A, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( xv, tc.x, 1e-13, 'x' );
});

test( 'zlatrs: lower, conjugate-transpose, unit diagonal, 3x3', function t() {
	var tc = findCase( 'lower_C_unit' );
	var A = makeMatrix( [
		99.0, 99.0, 1.0, 1.0,   0.5, 0.0,
		0.0, 0.0,   99.0, 99.0, 1.0, -1.0,
		0.0, 0.0,   0.0, 0.0,   99.0, 99.0
	], 3 );
	var x = makeVector( [ 1.0, 0.0, 2.0, 1.0, 3.0, -1.0 ] );
	var xv = reinterpret( x, 0 );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info;

	info = zlatrs( 'lower', 'conjugate-transpose', 'unit', 'N', 3, A, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( xv, tc.x, 1e-13, 'x' );
});

test( 'zlatrs: upper, no-transpose, 4x4', function t() {
	var tc = findCase( 'upper_N_4x4' );
	// 4x4 upper triangular, column-major
	var A = makeMatrix( [
		3.0, 0.0,   0.0, 0.0,   0.0, 0.0,  0.0, 0.0,
		1.0, 0.5,   4.0, 1.0,   0.0, 0.0,  0.0, 0.0,
		0.0, 1.0,   1.0, 0.0,   2.0, -1.0, 0.0, 0.0,
		0.5, 0.0,   0.0, 0.5,   1.0, 1.0,  5.0, 0.0
	], 4 );
	var x = makeVector( [ 1.0, 1.0, 2.0, 0.0, 0.0, 3.0, 1.0, -2.0 ] );
	var xv = reinterpret( x, 0 );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 4 );
	var info;

	info = zlatrs( 'upper', 'no-transpose', 'non-unit', 'N', 4, A, 1, 4, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( xv, tc.x, 1e-12, 'x' );
});

test( 'zlatrs: lower, transpose, unit, normin=Y, 3x3', function t() {
	var tc = findCase( 'lower_T_unit_norminY' );
	var A = makeMatrix( [
		99.0, 99.0, 1.0, 1.0,   0.5, 0.0,
		0.0, 0.0,   99.0, 99.0, 1.0, -1.0,
		0.0, 0.0,   0.0, 0.0,   99.0, 99.0
	], 3 );
	var x = makeVector( [ 5.0, 1.0, 3.0, -2.0, 1.0, 0.0 ] );
	var xv = reinterpret( x, 0 );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( [ 2.5, 2.0, 0.0 ] );
	var info;

	info = zlatrs( 'lower', 'transpose', 'unit', 'Y', 3, A, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( xv, tc.x, 1e-13, 'x' );
});

test( 'zlatrs: upper, no-transpose, careful solve (near-singular diag)', function t() {
	// Near-singular diagonal forces the careful (non-ztrsv) solve path.
	// scale=0 means singular - x is a null-space direction, so only check
	// that scale is tiny and info=0, not exact x values.
	var A = makeMatrix( [
		1e-300, 0.0,  0.0, 0.0,  0.0, 0.0,
		1.0, 1.0,     1e-300, 0.0, 0.0, 0.0,
		0.5, 0.0,     1.0, -1.0, 1e-300, 0.0
	], 3 );
	var x = makeVector( [ 1.0, 0.0, 2.0, 1.0, 3.0, -1.0 ] );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info;

	info = zlatrs( 'upper', 'no-transpose', 'non-unit', 'N', 3, A, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, 0 );
	assert.ok( scale[ 0 ] <= 1e-100, 'scale should be tiny: ' + scale[ 0 ] );
});

test( 'zlatrs: lower, no-transpose, careful solve', function t() {
	var A = makeMatrix( [
		1e-300, 0.0, 1.0, 1.0,  0.5, 0.0,
		0.0, 0.0,    1e-300, 0.0, 1.0, -1.0,
		0.0, 0.0,    0.0, 0.0,  1e-300, 0.0
	], 3 );
	var x = makeVector( [ 1.0, 0.0, 2.0, 1.0, 3.0, -1.0 ] );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info;

	info = zlatrs( 'lower', 'no-transpose', 'non-unit', 'N', 3, A, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, 0 );
	assert.ok( scale[ 0 ] <= 1e-100, 'scale should be tiny: ' + scale[ 0 ] );
});

test( 'zlatrs: upper, transpose, careful solve', function t() {
	var A = makeMatrix( [
		1e-300, 0.0,  0.0, 0.0,  0.0, 0.0,
		1.0, 1.0,     1e-300, 0.0, 0.0, 0.0,
		0.5, 0.0,     1.0, -1.0, 1e-300, 0.0
	], 3 );
	var x = makeVector( [ 1.0, 0.0, 2.0, 1.0, 3.0, -1.0 ] );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info;

	info = zlatrs( 'upper', 'transpose', 'non-unit', 'N', 3, A, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, 0 );
	assert.ok( scale[ 0 ] <= 1e-100, 'scale should be tiny: ' + scale[ 0 ] );
});

test( 'zlatrs: upper, conjugate-transpose, careful solve', function t() {
	var A = makeMatrix( [
		1e-300, 0.0,  0.0, 0.0,  0.0, 0.0,
		1.0, 1.0,     1e-300, 0.0, 0.0, 0.0,
		0.5, 0.0,     1.0, -1.0, 1e-300, 0.0
	], 3 );
	var x = makeVector( [ 1.0, 0.0, 2.0, 1.0, 3.0, -1.0 ] );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info;

	info = zlatrs( 'upper', 'conjugate-transpose', 'non-unit', 'N', 3, A, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, 0 );
	assert.ok( scale[ 0 ] <= 1e-100, 'scale should be tiny: ' + scale[ 0 ] );
});

test( 'zlatrs: upper, no-transpose, unit, careful solve (large off-diag)', function t() {
	var tc = findCase( 'upper_N_unit_careful' );
	var A = makeMatrix( [
		99.0, 99.0,  0.0, 0.0,      0.0, 0.0,
		1e+150, 1e+150, 99.0, 99.0, 0.0, 0.0,
		1e+150, 0.0, 1e+150, -1e+150, 99.0, 99.0
	], 3 );
	var x = makeVector( [ 1.0, 0.0, 2.0, 1.0, 3.0, -1.0 ] );
	var xv = reinterpret( x, 0 );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info;

	info = zlatrs( 'upper', 'no-transpose', 'unit', 'N', 3, A, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertClose( scale[ 0 ], tc.scale, 1e-10, 'scale' );
	assertArrayClose( xv, tc.x, 1e-10, 'x' );
});

test( 'zlatrs: lower, conjugate-transpose, careful solve', function t() {
	var A = makeMatrix( [
		1e-300, 0.0, 1.0, 1.0,  0.5, 0.0,
		0.0, 0.0,    1e-300, 0.0, 1.0, -1.0,
		0.0, 0.0,    0.0, 0.0,  1e-300, 0.0
	], 3 );
	var x = makeVector( [ 1.0, 0.0, 2.0, 1.0, 3.0, -1.0 ] );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info;

	info = zlatrs( 'lower', 'conjugate-transpose', 'non-unit', 'N', 3, A, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, 0 );
	assert.ok( scale[ 0 ] <= 1e-100, 'scale should be tiny: ' + scale[ 0 ] );
});
