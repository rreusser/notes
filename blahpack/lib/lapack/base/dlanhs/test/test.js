/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';


// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlanhs = require( './../lib' );
var base = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dlanhs.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
var fixture = lines.map( function parse( line ) {
	return JSON.parse( line );
} );


// FUNCTIONS //

/**
* Returns a test case from the fixture data.
*
* @private
* @param {string} name - test case name
* @returns {*} result
*/
function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name;
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

// 3x3 upper Hessenberg matrix (column-major, LDA=3):
// [1  2  3]
// [4  5  6]
// [0  7  8]
var A3 = new Float64Array([
	1.0,
	4.0,
	0.0,   // col 0
	2.0,
	5.0,
	7.0,   // col 1
	3.0,
	6.0,
	8.0    // col 2
]);

// 1x1 matrix
var A1 = new Float64Array([ -5.5 ]);

// 4x4 upper Hessenberg matrix (column-major, LDA=4):

// [ 2  4  -7  1]

// [-1 -6   2  0]

// [ 0  1   8 -3]

// [ 0  0  -4  5]
var A4 = new Float64Array([
	2.0,
	-1.0,
	0.0,
	0.0,     // col 0
	4.0,
	-6.0,
	1.0,
	0.0,     // col 1
	-7.0,
	2.0,
	8.0,
	-4.0,    // col 2
	1.0,
	0.0,
	-3.0,
	5.0      // col 3
]);


// TESTS //

test( 'dlanhs: main export is a function', function t() {
	assert.strictEqual( typeof dlanhs, 'function' );
});

test( 'dlanhs: attached to the main export is an `ndarray` method', function t() { // eslint-disable-line max-len
	assert.strictEqual( typeof dlanhs.ndarray, 'function' );
});

test( 'dlanhs: N=0 returns 0', function t() {
	var result = base( 'max', 0, A3, 1, 3, 0, new Float64Array( 3 ), 1, 0 );
	var tc = findCase( 'n_zero' );
	assert.strictEqual( result, tc.result );
});

test( 'dlanhs: max norm (3x3)', function t() {
	var result = base( 'max', 3, A3, 1, 3, 0, new Float64Array( 3 ), 1, 0 );
	var tc = findCase( 'max_norm' );
	assertClose( result, tc.result, 1e-14, 'max_norm' );
});

test( 'dlanhs: one-norm (3x3)', function t() {
	var result = base( 'one-norm', 3, A3, 1, 3, 0, new Float64Array( 3 ), 1, 0 );
	var tc = findCase( 'one_norm' );
	assertClose( result, tc.result, 1e-14, 'one_norm' );
});

test( 'dlanhs: inf-norm (3x3)', function t() {
	var result = base( 'inf-norm', 3, A3, 1, 3, 0, new Float64Array( 3 ), 1, 0 );
	var tc = findCase( 'inf_norm' );
	assertClose( result, tc.result, 1e-14, 'inf_norm' );
});

test( 'dlanhs: Frobenius norm (3x3)', function t() {
	var result = base( 'frobenius', 3, A3, 1, 3, 0, new Float64Array( 3 ), 1, 0 );
	var tc = findCase( 'frob_norm' );
	assertClose( result, tc.result, 1e-14, 'frob_norm' );
});

test( 'dlanhs: 1x1 max', function t() {
	var result = base( 'max', 1, A1, 1, 1, 0, new Float64Array( 1 ), 1, 0 );
	var tc = findCase( 'one_by_one_max' );
	assertClose( result, tc.result, 1e-14, 'one_by_one_max' );
});

test( 'dlanhs: 1x1 one-norm', function t() {
	var result = base( 'one-norm', 1, A1, 1, 1, 0, new Float64Array( 1 ), 1, 0 );
	var tc = findCase( 'one_by_one_one' );
	assertClose( result, tc.result, 1e-14, 'one_by_one_one' );
});

test( 'dlanhs: 1x1 inf-norm', function t() {
	var result = base( 'inf-norm', 1, A1, 1, 1, 0, new Float64Array( 1 ), 1, 0 );
	var tc = findCase( 'one_by_one_inf' );
	assertClose( result, tc.result, 1e-14, 'one_by_one_inf' );
});

test( 'dlanhs: 1x1 Frobenius', function t() {
	var result = base( 'frobenius', 1, A1, 1, 1, 0, new Float64Array( 1 ), 1, 0 );
	var tc = findCase( 'one_by_one_frob' );
	assertClose( result, tc.result, 1e-14, 'one_by_one_frob' );
});

test( 'dlanhs: 4x4 max', function t() {
	var result = base( 'max', 4, A4, 1, 4, 0, new Float64Array( 4 ), 1, 0 );
	var tc = findCase( 'big_max' );
	assertClose( result, tc.result, 1e-14, 'big_max' );
});

test( 'dlanhs: 4x4 one-norm', function t() {
	var result = base( 'one-norm', 4, A4, 1, 4, 0, new Float64Array( 4 ), 1, 0 );
	var tc = findCase( 'big_one' );
	assertClose( result, tc.result, 1e-14, 'big_one' );
});

test( 'dlanhs: 4x4 inf-norm', function t() {
	var result = base( 'inf-norm', 4, A4, 1, 4, 0, new Float64Array( 4 ), 1, 0 );
	var tc = findCase( 'big_inf' );
	assertClose( result, tc.result, 1e-14, 'big_inf' );
});

test( 'dlanhs: 4x4 Frobenius', function t() {
	var result = base( 'frobenius', 4, A4, 1, 4, 0, new Float64Array( 4 ), 1, 0 );
	var tc = findCase( 'big_frob' );
	assertClose( result, tc.result, 1e-14, 'big_frob' );
});

test( 'dlanhs: unrecognized norm returns 0', function t() {
	var result = base( 'X', 3, A3, 1, 3, 0, new Float64Array( 3 ), 1, 0 );
	assert.strictEqual( result, 0.0 );
});

test( 'dlanhs: NaN propagation through max norm', function t() {
	var result;
	var B;

	B = new Float64Array([ NaN, 1.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 1.0 ]);
	result = base( 'max', 3, B, 1, 3, 0, new Float64Array( 3 ), 1, 0 );
	assert.ok( result !== result, 'expected NaN' );
});

test( 'dlanhs: NaN propagation through one-norm', function t() {
	var result;
	var B;

	B = new Float64Array([ NaN, 1.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 1.0 ]);
	result = base( 'one-norm', 3, B, 1, 3, 0, new Float64Array( 3 ), 1, 0 );
	assert.ok( result !== result, 'expected NaN' );
});

test( 'dlanhs: NaN propagation through inf-norm', function t() {
	var result;
	var B;

	B = new Float64Array([ NaN, 1.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 1.0 ]);
	result = base( 'inf-norm', 3, B, 1, 3, 0, new Float64Array( 3 ), 1, 0 );
	assert.ok( result !== result, 'expected NaN' );
});

test( 'dlanhs: non-unit strides', function t() {
	var result;
	var tc;
	var B;

	B = new Float64Array( 18 );
	B[ 0 ] = 1.0;
	B[ 2 ] = 4.0;
	B[ 4 ] = 0.0;
	B[ 6 ] = 2.0;
	B[ 8 ] = 5.0;
	B[ 10 ] = 7.0;
	B[ 12 ] = 3.0;
	B[ 14 ] = 6.0;
	B[ 16 ] = 8.0;
	tc = findCase( 'max_norm' );
	result = base( 'max', 3, B, 2, 6, 0, new Float64Array( 3 ), 1, 0 );
	assertClose( result, tc.result, 1e-14, 'non-unit stride max' );
	tc = findCase( 'one_norm' );
	result = base( 'one-norm', 3, B, 2, 6, 0, new Float64Array( 3 ), 1, 0 );
	assertClose( result, tc.result, 1e-14, 'non-unit stride one' );
	tc = findCase( 'inf_norm' );
	result = base( 'inf-norm', 3, B, 2, 6, 0, new Float64Array( 3 ), 1, 0 );
	assertClose( result, tc.result, 1e-14, 'non-unit stride inf' );
	tc = findCase( 'frob_norm' );
	result = base( 'frobenius', 3, B, 2, 6, 0, new Float64Array( 3 ), 1, 0 );
	assertClose( result, tc.result, 1e-14, 'non-unit stride frob' );
});

test( 'dlanhs: offset support', function t() {
	var result;
	var tc;
	var B;

	B = new Float64Array( 14 );
	B[ 5 ] = 1.0;
	B[ 6 ] = 4.0;
	B[ 7 ] = 0.0;
	B[ 8 ] = 2.0;
	B[ 9 ] = 5.0;
	B[ 10 ] = 7.0;
	B[ 11 ] = 3.0;
	B[ 12 ] = 6.0;
	B[ 13 ] = 8.0;
	tc = findCase( 'max_norm' );
	result = base( 'max', 3, B, 1, 3, 5, new Float64Array( 3 ), 1, 0 );
	assertClose( result, tc.result, 1e-14, 'offset max' );
});

test( 'dlanhs: ndarray validates norm type', function t() {
	assert.throws( function invalid() {
		dlanhs.ndarray( 'bad', 3, A3, 1, 3, 0, new Float64Array( 3 ), 1, 0 );
	}, TypeError );
});

test( 'dlanhs: ndarray validates N >= 0', function t() {
	assert.throws( function invalid() {
		dlanhs.ndarray( 'max', -1, A3, 1, 3, 0, new Float64Array( 3 ), 1, 0 );
	}, RangeError );
});

test( 'dlanhs: ndarray returns 0 for N=0', function t() {
	var result = dlanhs.ndarray( 'max', 0, A3, 1, 3, 0, new Float64Array( 3 ), 1, 0 ); // eslint-disable-line max-len
	assert.strictEqual( result, 0.0 );
});

test( 'dlanhs: LAPACK-style API (column-major)', function t() {
	var result = dlanhs( 'column-major', 'max', 3, A3, 3, new Float64Array( 3 ), 1 );
	var tc = findCase( 'max_norm' ); // eslint-disable-line max-len
	assertClose( result, tc.result, 1e-14, 'LAPACK-style column-major max' );
});

test( 'dlanhs: LAPACK-style API validates order', function t() {
	assert.throws( function invalid() {
		dlanhs( 'bad-order', 'max', 3, A3, 3, new Float64Array( 3 ), 1 );
	}, TypeError );
});
