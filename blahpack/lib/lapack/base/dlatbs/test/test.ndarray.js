/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlatbs = require( './../lib/base.js' );

// FIXTURES //

var upper_n_nonunit = require( './fixtures/upper_n_nonunit.json' );
var lower_n_nonunit = require( './fixtures/lower_n_nonunit.json' );
var upper_t_nonunit = require( './fixtures/upper_t_nonunit.json' );
var lower_t_nonunit = require( './fixtures/lower_t_nonunit.json' );
var upper_n_unit = require( './fixtures/upper_n_unit.json' );
var lower_n_unit = require( './fixtures/lower_n_unit.json' );
var n_zero = require( './fixtures/n_zero.json' );
var n_one = require( './fixtures/n_one.json' );
var normin_y = require( './fixtures/normin_y.json' );
var upper_kd1 = require( './fixtures/upper_kd1.json' );
var upper_t_unit = require( './fixtures/upper_t_unit.json' );
var lower_t_unit = require( './fixtures/lower_t_unit.json' );
var lower_t_nonunit_normin_y = require( './fixtures/lower_t_nonunit_normin_y.json' );
var upper_t_nonunit_normin_y = require( './fixtures/upper_t_nonunit_normin_y.json' );
var lower_n_unit_normin_y = require( './fixtures/lower_n_unit_normin_y.json' );
var lower_t_kd1 = require( './fixtures/lower_t_kd1.json' );
var singular_upper = require( './fixtures/singular_upper.json' );
var near_singular_upper = require( './fixtures/near_singular_upper.json' );

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
* Asserts that two arrays are element-wise approximately equal.
*
* @private
* @param {*} actual - actual value
* @param {*} expected - expected value
* @param {number} tol - tolerance
* @param {string} msg - assertion message
*/
function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}

/**
* Creates banded storage (KD+1 x N) in column-major from entries.
* entries is array of [row0based, col, value].
*/
function bandedMatrix( kdp1, n, entries ) {
	var ab = new Float64Array( kdp1 * n );
	var i;
	for ( i = 0; i < entries.length; i++ ) {
		ab[ (entries[i][1] * kdp1) + entries[i][0] ] = entries[i][2];
	}
	return ab;
}

/**
* Converts a typed array to a plain array.
*
* @private
* @param {TypedArray} arr - input array
* @returns {Array} output array
*/
function toArray( arr ) {
	var out = [];
	var i;
	for ( i = 0; i < arr.length; i++ ) {
		out.push( arr[ i ] );
	}
	return out;
}

// TESTS //

test( 'dlatbs: upper_N_nonunit', function t() {
	var scale;
	var cnorm;
	var info;
	var tc;
	var ab;
	var x;

	tc = upper_n_nonunit;
	ab = bandedMatrix( 3, 4, [
		[2, 0, 4.0],
		[1, 1, 2.0],
		[2, 1, 3.0],
		[0, 2, 1.0],
		[1, 2, 1.0],
		[2, 2, 5.0],
		[0, 3, 2.0],
		[1, 3, 3.0],
		[2, 3, 6.0]
	]);
	x = new Float64Array([ 1.0, 2.0, 3.0, 4.0 ]);
	scale = new Float64Array( 1 );
	cnorm = new Float64Array( 4 );
	info = dlatbs( 'upper', 'no-transpose', 'non-unit', 'no', 4, 2, ab, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[0], tc.scale, 1e-14, 'scale' );
	assertArrayClose( toArray( x), tc.x, 1e-14, 'x' );
	assertArrayClose( toArray( cnorm), tc.cnorm, 1e-14, 'cnorm' );
});

test( 'dlatbs: lower_N_nonunit', function t() {
	var scale;
	var cnorm;
	var info;
	var tc;
	var ab;
	var x;

	tc = lower_n_nonunit;
	ab = bandedMatrix( 3, 4, [
		[0, 0, 4.0],
		[1, 0, 2.0],
		[2, 0, 1.0],
		[0, 1, 3.0],
		[1, 1, 1.0],
		[2, 1, 2.0],
		[0, 2, 5.0],
		[1, 2, 3.0],
		[0, 3, 6.0]
	]);
	x = new Float64Array([ 1.0, 2.0, 3.0, 4.0 ]);
	scale = new Float64Array( 1 );
	cnorm = new Float64Array( 4 );
	info = dlatbs( 'lower', 'no-transpose', 'non-unit', 'no', 4, 2, ab, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[0], tc.scale, 1e-14, 'scale' );
	assertArrayClose( toArray( x), tc.x, 1e-14, 'x' );
	assertArrayClose( toArray( cnorm), tc.cnorm, 1e-14, 'cnorm' );
});

test( 'dlatbs: upper_T_nonunit', function t() {
	var scale;
	var cnorm;
	var info;
	var tc;
	var ab;
	var x;

	tc = upper_t_nonunit;
	ab = bandedMatrix( 3, 4, [
		[2, 0, 4.0],
		[1, 1, 2.0],
		[2, 1, 3.0],
		[0, 2, 1.0],
		[1, 2, 1.0],
		[2, 2, 5.0],
		[0, 3, 2.0],
		[1, 3, 3.0],
		[2, 3, 6.0]
	]);
	x = new Float64Array([ 1.0, 2.0, 3.0, 4.0 ]);
	scale = new Float64Array( 1 );
	cnorm = new Float64Array( 4 );
	info = dlatbs( 'upper', 'transpose', 'non-unit', 'no', 4, 2, ab, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[0], tc.scale, 1e-14, 'scale' );
	assertArrayClose( toArray( x), tc.x, 1e-14, 'x' );
	assertArrayClose( toArray( cnorm), tc.cnorm, 1e-14, 'cnorm' );
});

test( 'dlatbs: lower_T_nonunit', function t() {
	var scale;
	var cnorm;
	var info;
	var tc;
	var ab;
	var x;

	tc = lower_t_nonunit;
	ab = bandedMatrix( 3, 4, [
		[0, 0, 4.0],
		[1, 0, 2.0],
		[2, 0, 1.0],
		[0, 1, 3.0],
		[1, 1, 1.0],
		[2, 1, 2.0],
		[0, 2, 5.0],
		[1, 2, 3.0],
		[0, 3, 6.0]
	]);
	x = new Float64Array([ 1.0, 2.0, 3.0, 4.0 ]);
	scale = new Float64Array( 1 );
	cnorm = new Float64Array( 4 );
	info = dlatbs( 'lower', 'transpose', 'non-unit', 'no', 4, 2, ab, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[0], tc.scale, 1e-14, 'scale' );
	assertArrayClose( toArray( x), tc.x, 1e-14, 'x' );
	assertArrayClose( toArray( cnorm), tc.cnorm, 1e-14, 'cnorm' );
});

test( 'dlatbs: upper_N_unit', function t() {
	var scale;
	var cnorm;
	var info;
	var tc;
	var ab;
	var x;

	tc = upper_n_unit;
	ab = bandedMatrix( 3, 4, [
		[2, 0, 99.0],
		[1, 1, 2.0],
		[2, 1, 99.0],
		[0, 2, 1.0],
		[1, 2, 1.0],
		[2, 2, 99.0],
		[0, 3, 2.0],
		[1, 3, 3.0],
		[2, 3, 99.0]
	]);
	x = new Float64Array([ 1.0, 2.0, 3.0, 4.0 ]);
	scale = new Float64Array( 1 );
	cnorm = new Float64Array( 4 );
	info = dlatbs( 'upper', 'no-transpose', 'unit', 'no', 4, 2, ab, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[0], tc.scale, 1e-14, 'scale' );
	assertArrayClose( toArray( x), tc.x, 1e-14, 'x' );
	assertArrayClose( toArray( cnorm), tc.cnorm, 1e-14, 'cnorm' );
});

test( 'dlatbs: lower_N_unit', function t() {
	var scale;
	var cnorm;
	var info;
	var tc;
	var ab;
	var x;

	tc = lower_n_unit;
	ab = bandedMatrix( 3, 4, [
		[0, 0, 99.0],
		[1, 0, 2.0],
		[2, 0, 1.0],
		[0, 1, 99.0],
		[1, 1, 1.0],
		[2, 1, 2.0],
		[0, 2, 99.0],
		[1, 2, 3.0],
		[0, 3, 99.0]
	]);
	x = new Float64Array([ 1.0, 2.0, 3.0, 4.0 ]);
	scale = new Float64Array( 1 );
	cnorm = new Float64Array( 4 );
	info = dlatbs( 'lower', 'no-transpose', 'unit', 'no', 4, 2, ab, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[0], tc.scale, 1e-14, 'scale' );
	assertArrayClose( toArray( x), tc.x, 1e-14, 'x' );
	assertArrayClose( toArray( cnorm), tc.cnorm, 1e-14, 'cnorm' );
});

test( 'dlatbs: n_zero', function t() {
	var scale;
	var cnorm;
	var info;
	var tc;
	var ab;
	var x;

	tc = n_zero;
	ab = new Float64Array( 1 );
	x = new Float64Array( 1 );
	scale = new Float64Array( 1 );
	cnorm = new Float64Array( 1 );
	info = dlatbs( 'upper', 'no-transpose', 'non-unit', 'no', 0, 0, ab, 1, 1, 0, x, 1, 0, scale, cnorm, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[0], tc.scale, 1e-14, 'scale' );
});

test( 'dlatbs: n_one', function t() {
	var scale;
	var cnorm;
	var info;
	var tc;
	var ab;
	var x;

	tc = n_one;
	ab = new Float64Array([ 5.0 ]);
	x = new Float64Array([ 10.0 ]);
	scale = new Float64Array( 1 );
	cnorm = new Float64Array( 1 );
	info = dlatbs( 'upper', 'no-transpose', 'non-unit', 'no', 1, 0, ab, 1, 1, 0, x, 1, 0, scale, cnorm, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[0], tc.scale, 1e-14, 'scale' );
	assertArrayClose( toArray( x), tc.x, 1e-14, 'x' );
});

test( 'dlatbs: normin_Y', function t() {
	var scale;
	var cnorm;
	var info;
	var tc;
	var ab;
	var x;

	tc = normin_y;
	ab = bandedMatrix( 3, 4, [
		[2, 0, 4.0],
		[1, 1, 2.0],
		[2, 1, 3.0],
		[0, 2, 1.0],
		[1, 2, 1.0],
		[2, 2, 5.0],
		[0, 3, 2.0],
		[1, 3, 3.0],
		[2, 3, 6.0]
	]);
	x = new Float64Array([ 1.0, 2.0, 3.0, 4.0 ]);
	scale = new Float64Array( 1 );
	cnorm = new Float64Array([ 0.0, 2.0, 2.0, 5.0 ]);
	info = dlatbs( 'upper', 'no-transpose', 'non-unit', 'yes', 4, 2, ab, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[0], tc.scale, 1e-14, 'scale' );
	assertArrayClose( toArray( x), tc.x, 1e-14, 'x' );
	assertArrayClose( toArray( cnorm), tc.cnorm, 1e-14, 'cnorm' );
});

test( 'dlatbs: upper_kd1', function t() {
	var scale;
	var cnorm;
	var info;
	var tc;
	var ab;
	var x;

	tc = upper_kd1;
	ab = bandedMatrix( 2, 4, [
		[1, 0, 3.0],
		[0, 1, 1.0],
		[1, 1, 4.0],
		[0, 2, 2.0],
		[1, 2, 5.0],
		[0, 3, 1.0],
		[1, 3, 6.0]
	]);
	x = new Float64Array([ 2.0, 3.0, 1.0, 5.0 ]);
	scale = new Float64Array( 1 );
	cnorm = new Float64Array( 4 );
	info = dlatbs( 'upper', 'no-transpose', 'non-unit', 'no', 4, 1, ab, 1, 2, 0, x, 1, 0, scale, cnorm, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[0], tc.scale, 1e-14, 'scale' );
	assertArrayClose( toArray( x), tc.x, 1e-14, 'x' );
	assertArrayClose( toArray( cnorm), tc.cnorm, 1e-14, 'cnorm' );
});

test( 'dlatbs: upper_T_unit', function t() {
	var scale;
	var cnorm;
	var info;
	var tc;
	var ab;
	var x;

	tc = upper_t_unit;
	ab = bandedMatrix( 3, 4, [
		[2, 0, 99.0],
		[1, 1, 2.0],
		[2, 1, 99.0],
		[0, 2, 1.0],
		[1, 2, 1.0],
		[2, 2, 99.0],
		[0, 3, 2.0],
		[1, 3, 3.0],
		[2, 3, 99.0]
	]);
	x = new Float64Array([ 1.0, 2.0, 3.0, 4.0 ]);
	scale = new Float64Array( 1 );
	cnorm = new Float64Array( 4 );
	info = dlatbs( 'upper', 'transpose', 'unit', 'no', 4, 2, ab, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[0], tc.scale, 1e-14, 'scale' );
	assertArrayClose( toArray( x), tc.x, 1e-14, 'x' );
	assertArrayClose( toArray( cnorm), tc.cnorm, 1e-14, 'cnorm' );
});

test( 'dlatbs: lower_T_unit', function t() {
	var scale;
	var cnorm;
	var info;
	var tc;
	var ab;
	var x;

	tc = lower_t_unit;
	ab = bandedMatrix( 3, 4, [
		[0, 0, 99.0],
		[1, 0, 2.0],
		[2, 0, 1.0],
		[0, 1, 99.0],
		[1, 1, 1.0],
		[2, 1, 2.0],
		[0, 2, 99.0],
		[1, 2, 3.0],
		[0, 3, 99.0]
	]);
	x = new Float64Array([ 1.0, 2.0, 3.0, 4.0 ]);
	scale = new Float64Array( 1 );
	cnorm = new Float64Array( 4 );
	info = dlatbs( 'lower', 'transpose', 'unit', 'no', 4, 2, ab, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[0], tc.scale, 1e-14, 'scale' );
	assertArrayClose( toArray( x), tc.x, 1e-14, 'x' );
	assertArrayClose( toArray( cnorm), tc.cnorm, 1e-14, 'cnorm' );
});

test( 'dlatbs: lower_T_nonunit_normin_Y', function t() {
	var scale;
	var cnorm;
	var info;
	var tc;
	var ab;
	var x;

	tc = lower_t_nonunit_normin_y;
	ab = bandedMatrix( 3, 4, [
		[0, 0, 4.0],
		[1, 0, 2.0],
		[2, 0, 1.0],
		[0, 1, 3.0],
		[1, 1, 1.0],
		[2, 1, 2.0],
		[0, 2, 5.0],
		[1, 2, 3.0],
		[0, 3, 6.0]
	]);
	x = new Float64Array([ 1.0, 2.0, 3.0, 4.0 ]);
	scale = new Float64Array( 1 );
	cnorm = new Float64Array([ 3.0, 3.0, 3.0, 0.0 ]);
	info = dlatbs( 'lower', 'transpose', 'non-unit', 'yes', 4, 2, ab, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[0], tc.scale, 1e-14, 'scale' );
	assertArrayClose( toArray( x), tc.x, 1e-14, 'x' );
	assertArrayClose( toArray( cnorm), tc.cnorm, 1e-14, 'cnorm' );
});

test( 'dlatbs: upper_T_nonunit_normin_Y', function t() {
	var scale;
	var cnorm;
	var info;
	var tc;
	var ab;
	var x;

	tc = upper_t_nonunit_normin_y;
	ab = bandedMatrix( 3, 4, [
		[2, 0, 4.0],
		[1, 1, 2.0],
		[2, 1, 3.0],
		[0, 2, 1.0],
		[1, 2, 1.0],
		[2, 2, 5.0],
		[0, 3, 2.0],
		[1, 3, 3.0],
		[2, 3, 6.0]
	]);
	x = new Float64Array([ 1.0, 2.0, 3.0, 4.0 ]);
	scale = new Float64Array( 1 );
	cnorm = new Float64Array([ 0.0, 2.0, 2.0, 5.0 ]);
	info = dlatbs( 'upper', 'transpose', 'non-unit', 'yes', 4, 2, ab, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[0], tc.scale, 1e-14, 'scale' );
	assertArrayClose( toArray( x), tc.x, 1e-14, 'x' );
	assertArrayClose( toArray( cnorm), tc.cnorm, 1e-14, 'cnorm' );
});

test( 'dlatbs: lower_N_unit_normin_Y', function t() {
	var scale;
	var cnorm;
	var info;
	var tc;
	var ab;
	var x;

	tc = lower_n_unit_normin_y;
	ab = bandedMatrix( 3, 4, [
		[0, 0, 99.0],
		[1, 0, 2.0],
		[2, 0, 1.0],
		[0, 1, 99.0],
		[1, 1, 1.0],
		[2, 1, 2.0],
		[0, 2, 99.0],
		[1, 2, 3.0],
		[0, 3, 99.0]
	]);
	x = new Float64Array([ 1.0, 2.0, 3.0, 4.0 ]);
	scale = new Float64Array( 1 );
	cnorm = new Float64Array([ 3.0, 3.0, 3.0, 0.0 ]);
	info = dlatbs( 'lower', 'no-transpose', 'unit', 'yes', 4, 2, ab, 1, 3, 0, x, 1, 0, scale, cnorm, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[0], tc.scale, 1e-14, 'scale' );
	assertArrayClose( toArray( x), tc.x, 1e-14, 'x' );
	assertArrayClose( toArray( cnorm), tc.cnorm, 1e-14, 'cnorm' );
});

test( 'dlatbs: lower_T_kd1', function t() {
	var scale;
	var cnorm;
	var info;
	var tc;
	var ab;
	var x;

	tc = lower_t_kd1;
	ab = bandedMatrix( 2, 4, [
		[0, 0, 3.0],
		[1, 0, 1.0],
		[0, 1, 4.0],
		[1, 1, 2.0],
		[0, 2, 5.0],
		[1, 2, 1.0],
		[0, 3, 6.0]
	]);
	x = new Float64Array([ 2.0, 3.0, 1.0, 5.0 ]);
	scale = new Float64Array( 1 );
	cnorm = new Float64Array( 4 );
	info = dlatbs( 'lower', 'transpose', 'non-unit', 'no', 4, 1, ab, 1, 2, 0, x, 1, 0, scale, cnorm, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[0], tc.scale, 1e-14, 'scale' );
	assertArrayClose( toArray( x), tc.x, 1e-14, 'x' );
	assertArrayClose( toArray( cnorm), tc.cnorm, 1e-14, 'cnorm' );
});

test( 'dlatbs: singular_upper (zero diagonal)', function t() {
	var scale;
	var cnorm;
	var info;
	var tc;
	var ab;
	var x;

	tc = singular_upper;
	ab = bandedMatrix( 2, 3, [
		[1, 0, 2.0],
		[0, 1, 1.0],
		[1, 1, 0.0],
		[0, 2, 1.0],
		[1, 2, 3.0]
	]);
	x = new Float64Array([ 1.0, 1.0, 1.0 ]);
	scale = new Float64Array( 1 );
	cnorm = new Float64Array( 3 );
	info = dlatbs( 'upper', 'no-transpose', 'non-unit', 'no', 3, 1, ab, 1, 2, 0, x, 1, 0, scale, cnorm, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[0], tc.scale, 1e-14, 'scale' );
	assertArrayClose( toArray( x), tc.x, 1e-14, 'x' );
	assertArrayClose( toArray( cnorm), tc.cnorm, 1e-14, 'cnorm' );
});

test( 'dlatbs: near_singular_upper (very small diagonal)', function t() {
	var scale;
	var cnorm;
	var info;
	var tc;
	var ab;
	var x;

	tc = near_singular_upper;
	ab = bandedMatrix( 2, 3, [
		[1, 0, 1.0],
		[0, 1, 1.0],
		[1, 1, 1e-300],
		[0, 2, 1.0],
		[1, 2, 1.0]
	]);
	x = new Float64Array([ 1.0, 1.0, 1.0 ]);
	scale = new Float64Array( 1 );
	cnorm = new Float64Array( 3 );
	info = dlatbs( 'upper', 'no-transpose', 'non-unit', 'no', 3, 1, ab, 1, 2, 0, x, 1, 0, scale, cnorm, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[0], tc.scale, 1e-14, 'scale' );
	assertArrayClose( toArray( x), tc.x, 1e-14, 'x' );
	assertArrayClose( toArray( cnorm), tc.cnorm, 1e-14, 'cnorm' );
});
