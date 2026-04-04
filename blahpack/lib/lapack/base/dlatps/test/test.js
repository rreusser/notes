/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlatps = require( './../lib/base.js' );

// FIXTURES //

var upper_n_nonunit = require( './fixtures/upper_n_nonunit.json' );
var lower_n_nonunit = require( './fixtures/lower_n_nonunit.json' );
var upper_t_nonunit = require( './fixtures/upper_t_nonunit.json' );
var lower_t_nonunit = require( './fixtures/lower_t_nonunit.json' );
var upper_n_unit = require( './fixtures/upper_n_unit.json' );
var n_one = require( './fixtures/n_one.json' );
var upper_n_normin_y = require( './fixtures/upper_n_normin_y.json' );
var lower_t_unit = require( './fixtures/lower_t_unit.json' );
var identity = require( './fixtures/identity.json' );

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
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}

test( 'dlatps: main export is a function', function t() {
	assert.strictEqual( typeof dlatps, 'function' );
});

test( 'dlatps: upper, no-transpose, non-unit diagonal, 3x3', function t() {
	var scale;
	var cnorm;
	var info;
	var tc;
	var ap;
	var x;

	tc = upper_n_nonunit;

	// A = [[2, 1, 1], [0, 3, 2], [0, 0, 4]], packed upper: [2, 1, 3, 1, 2, 4]
	ap = new Float64Array( [ 2, 1, 3, 1, 2, 4 ] );
	x = new Float64Array( [ 1, 2, 3 ] );
	scale = new Float64Array( 1 );
	cnorm = new Float64Array( 3 );
	info = dlatps( 'upper', 'no-transpose', 'non-unit', 'no', 3, ap, 1, 0, x, 1, 0, scale, cnorm, 1, 0 ); // eslint-disable-line max-len
	assert.strictEqual( info, 0 );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
	assertArrayClose( cnorm, tc.cnorm, 1e-14, 'cnorm' );
});

test( 'dlatps: lower, no-transpose, non-unit diagonal, 3x3', function t() {
	var scale;
	var cnorm;
	var info;
	var tc;
	var ap;
	var x;

	tc = lower_n_nonunit;

	// A = [[2, 0, 0], [1, 3, 0], [1, 2, 4]], packed lower: [2, 1, 1, 3, 2, 4]
	ap = new Float64Array( [ 2, 1, 1, 3, 2, 4 ] );
	x = new Float64Array( [ 1, 2, 3 ] );
	scale = new Float64Array( 1 );
	cnorm = new Float64Array( 3 );
	info = dlatps( 'lower', 'no-transpose', 'non-unit', 'no', 3, ap, 1, 0, x, 1, 0, scale, cnorm, 1, 0 ); // eslint-disable-line max-len
	assert.strictEqual( info, 0 );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
	assertArrayClose( cnorm, tc.cnorm, 1e-14, 'cnorm' );
});

test( 'dlatps: upper, transpose, non-unit diagonal, 3x3', function t() {
	var scale;
	var cnorm;
	var info;
	var tc;
	var ap;
	var x;

	tc = upper_t_nonunit;
	ap = new Float64Array( [ 2, 1, 3, 1, 2, 4 ] );
	x = new Float64Array( [ 1, 2, 3 ] );
	scale = new Float64Array( 1 );
	cnorm = new Float64Array( 3 );
	info = dlatps( 'upper', 'transpose', 'non-unit', 'no', 3, ap, 1, 0, x, 1, 0, scale, cnorm, 1, 0 ); // eslint-disable-line max-len
	assert.strictEqual( info, 0 );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'dlatps: lower, transpose, non-unit diagonal, 3x3', function t() {
	var scale;
	var cnorm;
	var info;
	var tc;
	var ap;
	var x;

	tc = lower_t_nonunit;
	ap = new Float64Array( [ 2, 1, 1, 3, 2, 4 ] );
	x = new Float64Array( [ 1, 2, 3 ] );
	scale = new Float64Array( 1 );
	cnorm = new Float64Array( 3 );
	info = dlatps( 'lower', 'transpose', 'non-unit', 'no', 3, ap, 1, 0, x, 1, 0, scale, cnorm, 1, 0 ); // eslint-disable-line max-len
	assert.strictEqual( info, 0 );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'dlatps: upper, no-transpose, unit diagonal, 3x3', function t() {
	var scale;
	var cnorm;
	var info;
	var tc;
	var ap;
	var x;

	tc = upper_n_unit;
	ap = new Float64Array( [ 99, 1, 99, 1, 2, 99 ] );
	x = new Float64Array( [ 1, 2, 3 ] );
	scale = new Float64Array( 1 );
	cnorm = new Float64Array( 3 );
	info = dlatps( 'upper', 'no-transpose', 'unit', 'no', 3, ap, 1, 0, x, 1, 0, scale, cnorm, 1, 0 ); // eslint-disable-line max-len
	assert.strictEqual( info, 0 );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'dlatps: N=0', function t() {
	var scale;
	var cnorm;
	var info;
	var ap;
	var x;

	ap = new Float64Array( 0 );
	x = new Float64Array( 0 );
	scale = new Float64Array( 1 );
	cnorm = new Float64Array( 0 );
	info = dlatps( 'upper', 'no-transpose', 'non-unit', 'no', 0, ap, 1, 0, x, 1, 0, scale, cnorm, 1, 0 ); // eslint-disable-line max-len
	assert.strictEqual( info, 0 );
	assert.strictEqual( scale[ 0 ], 1.0 );
});

test( 'dlatps: N=1', function t() {
	var scale;
	var cnorm;
	var info;
	var tc;
	var ap;
	var x;

	tc = n_one;
	ap = new Float64Array( [ 5 ] );
	x = new Float64Array( [ 10 ] );
	scale = new Float64Array( 1 );
	cnorm = new Float64Array( 1 );
	info = dlatps( 'upper', 'no-transpose', 'non-unit', 'no', 1, ap, 1, 0, x, 1, 0, scale, cnorm, 1, 0 ); // eslint-disable-line max-len
	assert.strictEqual( info, 0 );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertClose( x[ 0 ], tc.x[ 0 ], 1e-14, 'x[0]' );
});

test( 'dlatps: upper, normin=yes, 4x4', function t() {
	var scale;
	var cnorm;
	var info;
	var tc;
	var ap;
	var x;

	tc = upper_n_normin_y;

	// A = [[3,1,2,1],[0,4,1,2],[0,0,2,1],[0,0,0,5]], packed upper: [3, 1, 4, 2, 1, 2, 1, 2, 1, 5]
	ap = new Float64Array( [ 3, 1, 4, 2, 1, 2, 1, 2, 1, 5 ] );
	x = new Float64Array( [ 1, 1, 1, 1 ] );
	scale = new Float64Array( 1 );
	cnorm = new Float64Array( [ 0, 1, 3, 4 ] );
	info = dlatps( 'upper', 'no-transpose', 'non-unit', 'yes', 4, ap, 1, 0, x, 1, 0, scale, cnorm, 1, 0 ); // eslint-disable-line max-len
	assert.strictEqual( info, 0 );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'dlatps: lower, transpose, unit diagonal, 3x3', function t() {
	var scale;
	var cnorm;
	var info;
	var tc;
	var ap;
	var x;

	tc = lower_t_unit;

	// Lower packed: [99, 1, 2, 99, 3, 99]
	ap = new Float64Array( [ 99, 1, 2, 99, 3, 99 ] );
	x = new Float64Array( [ 6, 5, 4 ] );
	scale = new Float64Array( 1 );
	cnorm = new Float64Array( 3 );
	info = dlatps( 'lower', 'transpose', 'unit', 'no', 3, ap, 1, 0, x, 1, 0, scale, cnorm, 1, 0 ); // eslint-disable-line max-len
	assert.strictEqual( info, 0 );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'dlatps: identity matrix (upper packed)', function t() {
	var scale;
	var cnorm;
	var info;
	var tc;
	var ap;
	var x;

	tc = identity;

	// Upper packed identity: [1, 0, 1, 0, 0, 1]
	ap = new Float64Array( [ 1, 0, 1, 0, 0, 1 ] );
	x = new Float64Array( [ 7, 8, 9 ] );
	scale = new Float64Array( 1 );
	cnorm = new Float64Array( 3 );
	info = dlatps( 'upper', 'no-transpose', 'non-unit', 'no', 3, ap, 1, 0, x, 1, 0, scale, cnorm, 1, 0 ); // eslint-disable-line max-len
	assert.strictEqual( info, 0 );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'dlatps: near-singular upper triggers careful solve (non-transpose)', function t() { // eslint-disable-line max-len
	var scale;
	var cnorm;
	var tiny;
	var info;
	var ap;
	var x;

	tiny = 1e-300;

	// Diagonal-only packed upper: [tiny, 0, tiny, 0, 0, tiny]
	ap = new Float64Array( [ tiny, 0, tiny, 0, 0, tiny ] );
	x = new Float64Array( [ 1, 1, 1 ] );
	scale = new Float64Array( 1 );
	cnorm = new Float64Array( 3 );
	info = dlatps( 'upper', 'no-transpose', 'non-unit', 'no', 3, ap, 1, 0, x, 1, 0, scale, cnorm, 1, 0 ); // eslint-disable-line max-len
	assert.strictEqual( info, 0 );
	assert.ok( scale[ 0 ] >= 0.0, 'scale >= 0' );
});

test( 'dlatps: near-singular lower triggers careful solve (non-transpose)', function t() { // eslint-disable-line max-len
	var scale;
	var cnorm;
	var tiny;
	var info;
	var ap;
	var x;

	tiny = 1e-300;

	// Diagonal-only packed lower: [tiny, 0, 0, tiny, 0, tiny]
	ap = new Float64Array( [ tiny, 0, 0, tiny, 0, tiny ] );
	x = new Float64Array( [ 1, 1, 1 ] );
	scale = new Float64Array( 1 );
	cnorm = new Float64Array( 3 );
	info = dlatps( 'lower', 'no-transpose', 'non-unit', 'no', 3, ap, 1, 0, x, 1, 0, scale, cnorm, 1, 0 ); // eslint-disable-line max-len
	assert.strictEqual( info, 0 );
	assert.ok( scale[ 0 ] >= 0.0, 'scale >= 0' );
});

test( 'dlatps: near-singular upper, transpose triggers careful solve', function t() { // eslint-disable-line max-len
	var scale;
	var cnorm;
	var tiny;
	var info;
	var ap;
	var x;

	tiny = 1e-300;
	ap = new Float64Array( [ tiny, 0, tiny, 0, 0, tiny ] );
	x = new Float64Array( [ 1, 1, 1 ] );
	scale = new Float64Array( 1 );
	cnorm = new Float64Array( 3 );
	info = dlatps( 'upper', 'transpose', 'non-unit', 'no', 3, ap, 1, 0, x, 1, 0, scale, cnorm, 1, 0 ); // eslint-disable-line max-len
	assert.strictEqual( info, 0 );
	assert.ok( scale[ 0 ] >= 0.0, 'scale >= 0' );
});

test( 'dlatps: zero diagonal triggers singular path', function t() {
	var scale;
	var cnorm;
	var info;
	var ap;
	var x;

	// Upper packed with zero diagonals: [0, 1, 0, 1, 1, 0]
	ap = new Float64Array( [ 0, 1, 0, 1, 1, 0 ] );
	x = new Float64Array( [ 1, 1, 1 ] );
	scale = new Float64Array( 1 );
	cnorm = new Float64Array( 3 );
	info = dlatps( 'upper', 'no-transpose', 'non-unit', 'no', 3, ap, 1, 0, x, 1, 0, scale, cnorm, 1, 0 ); // eslint-disable-line max-len
	assert.strictEqual( info, 0 );
	assert.strictEqual( scale[ 0 ], 0.0 );
});
