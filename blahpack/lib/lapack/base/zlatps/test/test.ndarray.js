/**
* @license Apache-2.0
*
* Copyright (c) 2025 The Stdlib Authors.
*/

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zlatps = require( './../lib/base.js' );

// FIXTURES //

var n_zero = require( './fixtures/n_zero.json' );
var n_one = require( './fixtures/n_one.json' );
var upper_n_nonunit = require( './fixtures/upper_n_nonunit.json' );
var lower_n_nonunit = require( './fixtures/lower_n_nonunit.json' );
var upper_c_nonunit = require( './fixtures/upper_c_nonunit.json' );
var lower_c_nonunit = require( './fixtures/lower_c_nonunit.json' );
var upper_n_unit = require( './fixtures/upper_n_unit.json' );
var lower_n_unit = require( './fixtures/lower_n_unit.json' );
var upper_n_normin_y = require( './fixtures/upper_n_normin_y.json' );
var upper_c_unit = require( './fixtures/upper_c_unit.json' );
var lower_c_unit = require( './fixtures/lower_c_unit.json' );
var upper_n_4x4 = require( './fixtures/upper_n_4x4.json' );
var lower_t_unit_norminy = require( './fixtures/lower_t_unit_norminy.json' );
var upper_n_careful = require( './fixtures/upper_n_careful.json' );
var lower_n_careful = require( './fixtures/lower_n_careful.json' );
var upper_c_careful = require( './fixtures/upper_c_careful.json' );
var lower_c_careful = require( './fixtures/lower_c_careful.json' );
var lower_c_4x4 = require( './fixtures/lower_c_4x4.json' );
var upper_n_unit_careful = require( './fixtures/upper_n_unit_careful.json' );

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
* Creates a Complex128Array from flat Float64 interleaved array.
*
* @private
* @param {Array} vals - interleaved [re,im,...]
* @returns {Complex128Array} packed array
*/
function makePacked( vals ) {
	var buf = new Complex128Array( vals.length / 2 );
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

test( 'zlatps is a function', function t() {
	assert.strictEqual( typeof zlatps, 'function' );
});

test( 'zlatps: N=0 returns immediately', function t() {
	var tc = n_zero;
	var AP = new Complex128Array( 1 );
	var x = new Complex128Array( 1 );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 1 );
	var info;

	info = zlatps( 'upper', 'no-transpose', 'non-unit', 'no', 0, AP, 1, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, tc.info );
});

test( 'zlatps: N=1 upper, no-transpose, non-unit', function t() {
	var tc = n_one;
	var AP = makePacked( [ 5.0, 2.0 ] );
	var x = makeVector( [ 10.0, -3.0 ] );
	var xv = reinterpret( x, 0 );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 1 );
	var info;

	info = zlatps( 'upper', 'no-transpose', 'non-unit', 'no', 1, AP, 1, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( xv, tc.x, 1e-13, 'x' );
});

test( 'zlatps: upper, no-transpose, non-unit, 3x3', function t() {
	var tc = upper_n_nonunit;
	var AP = makePacked( [
		2.0, 1.0, 1.0, 1.0, 3.0, 0.5, 0.5, 0.0, 1.0, -1.0, 4.0, -1.0
	]);
	var x = makeVector( [ 1.0, 0.0, 2.0, 1.0, 3.0, -1.0 ] );
	var xv = reinterpret( x, 0 );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info;

	info = zlatps( 'upper', 'no-transpose', 'non-unit', 'no', 3, AP, 1, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( xv, tc.x, 1e-13, 'x' );
	assertArrayClose( cnorm, tc.cnorm, 1e-14, 'cnorm' );
});

test( 'zlatps: lower, no-transpose, non-unit, 3x3', function t() {
	var tc = lower_n_nonunit;
	var AP = makePacked( [
		2.0, 1.0, 1.0, 1.0, 0.5, 0.0, 3.0, 0.5, 1.0, -1.0, 4.0, -1.0
	]);
	var x = makeVector( [ 1.0, 0.0, 2.0, 1.0, 3.0, -1.0 ] );
	var xv = reinterpret( x, 0 );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info;

	info = zlatps( 'lower', 'no-transpose', 'non-unit', 'no', 3, AP, 1, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( xv, tc.x, 1e-13, 'x' );
	assertArrayClose( cnorm, tc.cnorm, 1e-14, 'cnorm' );
});

test( 'zlatps: upper, conjugate-transpose, non-unit, 3x3', function t() {
	var tc = upper_c_nonunit;
	var AP = makePacked( [
		2.0, 1.0, 1.0, 1.0, 3.0, 0.5, 0.5, 0.0, 1.0, -1.0, 4.0, -1.0
	]);
	var x = makeVector( [ 1.0, 0.0, 2.0, 1.0, 3.0, -1.0 ] );
	var xv = reinterpret( x, 0 );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info;

	info = zlatps( 'upper', 'conjugate-transpose', 'non-unit', 'no', 3, AP, 1, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( xv, tc.x, 1e-13, 'x' );
});

test( 'zlatps: lower, conjugate-transpose, non-unit, 3x3', function t() {
	var tc = lower_c_nonunit;
	var AP = makePacked( [
		2.0, 1.0, 1.0, 1.0, 0.5, 0.0, 3.0, 0.5, 1.0, -1.0, 4.0, -1.0
	]);
	var x = makeVector( [ 1.0, 0.0, 2.0, 1.0, 3.0, -1.0 ] );
	var xv = reinterpret( x, 0 );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info;

	info = zlatps( 'lower', 'conjugate-transpose', 'non-unit', 'no', 3, AP, 1, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( xv, tc.x, 1e-13, 'x' );
});

test( 'zlatps: upper, no-transpose, unit, 3x3', function t() {
	var tc = upper_n_unit;
	var AP = makePacked( [
		99.0, 99.0, 1.0, 1.0, 99.0, 99.0, 0.5, 0.0, 1.0, -1.0, 99.0, 99.0
	]);
	var x = makeVector( [ 1.0, 0.0, 2.0, 1.0, 3.0, -1.0 ] );
	var xv = reinterpret( x, 0 );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info;

	info = zlatps( 'upper', 'no-transpose', 'unit', 'no', 3, AP, 1, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( xv, tc.x, 1e-13, 'x' );
});

test( 'zlatps: lower, no-transpose, unit, 3x3', function t() {
	var tc = lower_n_unit;
	var AP = makePacked( [
		99.0, 99.0, 1.0, 1.0, 0.5, 0.0, 99.0, 99.0, 1.0, -1.0, 99.0, 99.0
	]);
	var x = makeVector( [ 1.0, 0.0, 2.0, 1.0, 3.0, -1.0 ] );
	var xv = reinterpret( x, 0 );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info;

	info = zlatps( 'lower', 'no-transpose', 'unit', 'no', 3, AP, 1, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( xv, tc.x, 1e-13, 'x' );
});

test( 'zlatps: upper, no-transpose, non-unit, normin=yes', function t() {
	var tc = upper_n_normin_y;
	var AP = makePacked( [
		2.0, 1.0, 1.0, 1.0, 3.0, 0.5, 0.5, 0.0, 1.0, -1.0, 4.0, -1.0
	]);
	var x = makeVector( [ 1.0, 0.0, 2.0, 1.0, 3.0, -1.0 ] );
	var xv = reinterpret( x, 0 );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( [ 0.0, 2.0, 2.5 ] );
	var info;

	info = zlatps( 'upper', 'no-transpose', 'non-unit', 'yes', 3, AP, 1, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( xv, tc.x, 1e-13, 'x' );
});

test( 'zlatps: upper, conjugate-transpose, unit', function t() {
	var tc = upper_c_unit;
	var AP = makePacked( [
		99.0, 99.0, 1.0, 1.0, 99.0, 99.0, 0.5, 0.0, 1.0, -1.0, 99.0, 99.0
	]);
	var x = makeVector( [ 1.0, 0.0, 2.0, 1.0, 3.0, -1.0 ] );
	var xv = reinterpret( x, 0 );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info;

	info = zlatps( 'upper', 'conjugate-transpose', 'unit', 'no', 3, AP, 1, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( xv, tc.x, 1e-13, 'x' );
});

test( 'zlatps: lower, conjugate-transpose, unit', function t() {
	var tc = lower_c_unit;
	var AP = makePacked( [
		99.0, 99.0, 1.0, 1.0, 0.5, 0.0, 99.0, 99.0, 1.0, -1.0, 99.0, 99.0
	]);
	var x = makeVector( [ 1.0, 0.0, 2.0, 1.0, 3.0, -1.0 ] );
	var xv = reinterpret( x, 0 );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info;

	info = zlatps( 'lower', 'conjugate-transpose', 'unit', 'no', 3, AP, 1, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( xv, tc.x, 1e-13, 'x' );
});

test( 'zlatps: upper, no-transpose, non-unit, 4x4', function t() {
	var tc = upper_n_4x4;
	var AP = makePacked( [
		3.0, 0.0, 1.0, 0.5, 4.0, 1.0, 0.0, 1.0, 1.0, 0.0,
		2.0, -1.0, 0.5, 0.0, 0.0, 0.5, 1.0, 1.0, 5.0, 0.0
	]);
	var x = makeVector( [ 1.0, 1.0, 2.0, 0.0, 0.0, 3.0, 1.0, -2.0 ] );
	var xv = reinterpret( x, 0 );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 4 );
	var info;

	info = zlatps( 'upper', 'no-transpose', 'non-unit', 'no', 4, AP, 1, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( xv, tc.x, 1e-13, 'x' );
});

test( 'zlatps: lower, transpose, unit, normin=yes', function t() {
	var tc = lower_t_unit_norminy;
	var AP = makePacked( [
		99.0, 99.0, 1.0, 1.0, 0.5, 0.0, 99.0, 99.0, 1.0, -1.0, 99.0, 99.0
	]);
	var x = makeVector( [ 5.0, 1.0, 3.0, -2.0, 1.0, 0.0 ] );
	var xv = reinterpret( x, 0 );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( [ 2.5, 2.0, 0.0 ] );
	var info;

	info = zlatps( 'lower', 'transpose', 'unit', 'yes', 3, AP, 1, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( xv, tc.x, 1e-13, 'x' );
});

test( 'zlatps: upper, no-transpose, non-unit, careful (tiny diagonal)', function t() {
	var tc = upper_n_careful;
	var AP = makePacked( [
		1e-300, 0.0, 1.0, 1.0, 1e-300, 0.0, 0.5, 0.0, 1.0, -1.0, 1e-300, 0.0
	]);
	var x = makeVector( [ 1.0, 0.0, 2.0, 1.0, 3.0, -1.0 ] );
	var xv = reinterpret( x, 0 );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info;

	info = zlatps( 'upper', 'no-transpose', 'non-unit', 'no', 3, AP, 1, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( xv, tc.x, 1e-10, 'x' );
});

test( 'zlatps: lower, no-transpose, non-unit, careful', function t() {
	var tc = lower_n_careful;
	var AP = makePacked( [
		1e-300, 0.0, 1.0, 1.0, 0.5, 0.0, 1e-300, 0.0, 1.0, -1.0, 1e-300, 0.0
	]);
	var x = makeVector( [ 1.0, 0.0, 2.0, 1.0, 3.0, -1.0 ] );
	var xv = reinterpret( x, 0 );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info;

	info = zlatps( 'lower', 'no-transpose', 'non-unit', 'no', 3, AP, 1, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( xv, tc.x, 1e-10, 'x' );
});

test( 'zlatps: upper, conjugate-transpose, non-unit, careful', function t() {
	var tc = upper_c_careful;
	var AP = makePacked( [
		1e-300, 0.0, 1.0, 1.0, 1e-300, 0.0, 0.5, 0.0, 1.0, -1.0, 1e-300, 0.0
	]);
	var x = makeVector( [ 1.0, 0.0, 2.0, 1.0, 3.0, -1.0 ] );
	var xv = reinterpret( x, 0 );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info;

	info = zlatps( 'upper', 'conjugate-transpose', 'non-unit', 'no', 3, AP, 1, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( xv, tc.x, 1e-10, 'x' );
});

test( 'zlatps: lower, conjugate-transpose, non-unit, careful', function t() {
	var tc = lower_c_careful;
	var AP = makePacked( [
		1e-300, 0.0, 1.0, 1.0, 0.5, 0.0, 1e-300, 0.0, 1.0, -1.0, 1e-300, 0.0
	]);
	var x = makeVector( [ 1.0, 0.0, 2.0, 1.0, 3.0, -1.0 ] );
	var xv = reinterpret( x, 0 );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info;

	info = zlatps( 'lower', 'conjugate-transpose', 'non-unit', 'no', 3, AP, 1, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( xv, tc.x, 1e-10, 'x' );
});

test( 'zlatps: lower, conjugate-transpose, non-unit, 4x4', function t() {
	var tc = lower_c_4x4;
	var AP = makePacked( [
		3.0, 0.0, 1.0, 0.5, 0.0, 1.0, 0.5, 0.0, 4.0, 1.0,
		1.0, 0.0, 0.0, 0.5, 2.0, -1.0, 1.0, 1.0, 5.0, 0.0
	]);
	var x = makeVector( [ 1.0, 1.0, 2.0, 0.0, 0.0, 3.0, 1.0, -2.0 ] );
	var xv = reinterpret( x, 0 );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 4 );
	var info;

	info = zlatps( 'lower', 'conjugate-transpose', 'non-unit', 'no', 4, AP, 1, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( xv, tc.x, 1e-13, 'x' );
});

test( 'zlatps: upper, no-transpose, unit, careful (large off-diag)', function t() {
	var tc = upper_n_unit_careful;
	var AP = makePacked( [
		99.0, 99.0, 1e150, 1e150, 99.0, 99.0, 1e150, 0.0, 1e150, -1e150, 99.0, 99.0
	]);
	var x = makeVector( [ 1.0, 0.0, 2.0, 1.0, 3.0, -1.0 ] );
	var xv = reinterpret( x, 0 );
	var scale = new Float64Array( 1 );
	var cnorm = new Float64Array( 3 );
	var info;

	info = zlatps( 'upper', 'no-transpose', 'unit', 'no', 3, AP, 1, 0, x, 1, 0, scale, cnorm, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertClose( scale[ 0 ], tc.scale, 1e-10, 'scale' );
	assertArrayClose( xv, tc.x, 1e-10, 'x' );
});
