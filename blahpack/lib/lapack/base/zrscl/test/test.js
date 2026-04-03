/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var readFileSync = require( 'fs' ).readFileSync;
var test = require( 'node:test' );
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Complex128 = require( '@stdlib/complex/float64/ctor' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zrscl = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'zrscl.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line max-len, node/no-sync
var fixture = lines.map( function parse( line ) {
	// Handle non-standard JSON tokens (Infinity, -Infinity, NaN) from Fortran output: // eslint-disable-line max-len
	var cleaned = line.replace( /\bInfinity\b/g, '1e999' ).replace( /\b-Infinity\b/g, '-1e999' ).replace( /\bNaN\b/g, 'null' ); // eslint-disable-line max-len
	return JSON.parse( cleaned );
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
	return fixture.find( function find( t ) {
		return t.name === name;
	} );
}

/**
* Asserts that two numbers are approximately equal.
*
* @private
* @param {number} actual - actual value
* @param {number} expected - expected value
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
* @param {Array} actual - actual value
* @param {Array} expected - expected value
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
* Converts a Complex128Array to a plain array of doubles.
*
* @private
* @param {Complex128Array} arr - complex array
* @returns {Array} plain array
*/
function toArray( arr ) {
	var view = reinterpret( arr, 0 );
	var out = [];
	var i;
	for ( i = 0; i < view.length; i++ ) {
		out.push( view[ i ] );
	}
	return out;
}


// TESTS //

test( 'zrscl: main export is a function', function t() {
	assert.strictEqual( typeof zrscl, 'function' );
});

test( 'zrscl: purely real scalar a=(2,0) delegates to zdrscl', function t() {
	var tc = findCase( 'real_scalar' );
	var x = new Complex128Array( [ 2.0, 4.0, 6.0, 8.0, 10.0, 12.0 ] );
	zrscl( 3, new Complex128( 2.0, 0.0 ), x, 1, 0 );
	assertArrayClose( toArray( x ), tc.x, 1e-14, 'x' );
});

test( 'zrscl: purely imaginary scalar a=(0,2)', function t() {
	var tc = findCase( 'imag_scalar' );
	var x = new Complex128Array( [ 2.0, 4.0, 6.0, 8.0, 10.0, 12.0 ] );
	zrscl( 3, new Complex128( 0.0, 2.0 ), x, 1, 0 );
	assertArrayClose( toArray( x ), tc.x, 1e-14, 'x' );
});

test( 'zrscl: general complex scalar a=(3,4)', function t() {
	var tc = findCase( 'general_complex' );
	var x = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 ] );
	zrscl( 3, new Complex128( 3.0, 4.0 ), x, 1, 0 );
	assertArrayClose( toArray( x ), tc.x, 1e-14, 'x' );
});

test( 'zrscl: N=0 quick return', function t() {
	var tc = findCase( 'n_zero' );
	var x = new Complex128Array( [ 99.0, 88.0 ] );
	zrscl( 0, new Complex128( 1.0, 1.0 ), x, 1, 0 );
	assertArrayClose( toArray( x ), tc.x, 1e-14, 'x' );
});

test( 'zrscl: N=1', function t() {
	var tc = findCase( 'n_one' );
	var x = new Complex128Array( [ 4.0, -6.0 ] );
	zrscl( 1, new Complex128( 2.0, 1.0 ), x, 1, 0 );
	assertArrayClose( toArray( x ), tc.x, 1e-14, 'x' );
});

test( 'zrscl: non-unit stride (incx=2)', function t() {
	var tc = findCase( 'stride_2' );
	var x = new Complex128Array( [ 1.0, 2.0, 99.0, 99.0, 3.0, 4.0, 99.0, 99.0 ] ); // eslint-disable-line max-len
	zrscl( 2, new Complex128( 2.0, 3.0 ), x, 2, 0 );
	assertArrayClose( toArray( x ), tc.x, 1e-14, 'x' );
});

test( 'zrscl: purely imaginary, moderate |AI| (normal path)', function t() {
	var tc = findCase( 'imag_moderate' );
	var x = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
	zrscl( 2, new Complex128( 0.0, 1.0e300 ), x, 1, 0 );
	assertArrayClose( toArray( x ), tc.x, 1e-10, 'x' );
});

test( 'zrscl: purely imaginary, truly large |AI| > SAFMAX', function t() {
	var tc = findCase( 'imag_very_large' );
	var x = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
	zrscl( 2, new Complex128( 0.0, 1.0e308 ), x, 1, 0 );
	assertArrayClose( toArray( x ), tc.x, 1e-10, 'x' );
});

test( 'zrscl: purely imaginary, small |AI| < SAFMIN', function t() {
	var view;
	var x;

	x = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
	zrscl( 2, new Complex128( 0.0, 1.0e-310 ), x, 1, 0 );
	view = reinterpret( x, 0 );
	assert.strictEqual( view[ 0 ], Infinity );
	assert.strictEqual( view[ 1 ], -Infinity );
	assert.strictEqual( view[ 2 ], Infinity );
	assert.strictEqual( view[ 3 ], -Infinity );
});

test( 'zrscl: general complex, very small components', function t() {
	var view;
	var x;

	x = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
	zrscl( 2, new Complex128( 1.0e-310, 1.0e-310 ), x, 1, 0 );
	view = reinterpret( x, 0 );
	assert.strictEqual( view[ 0 ], Infinity );
	assert.strictEqual( view[ 1 ], Infinity );
	assert.strictEqual( view[ 2 ], Infinity );
	assert.strictEqual( view[ 3 ], Infinity );
});

test( 'zrscl: general complex, very large components', function t() {
	var tc = findCase( 'general_large' );
	var x = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
	zrscl( 2, new Complex128( 1.0e300, 1.0e300 ), x, 1, 0 );
	assertArrayClose( toArray( x ), tc.x, 1e-10, 'x' );
});

test( 'zrscl: identity scalar a=(1,0)', function t() {
	var tc = findCase( 'identity' );
	var x = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 ] );
	zrscl( 3, new Complex128( 1.0, 0.0 ), x, 1, 0 );
	assertArrayClose( toArray( x ), tc.x, 1e-14, 'x' );
});

test( 'zrscl: divide by i gives x * (-i)', function t() {
	var tc = findCase( 'div_by_i' );
	var x = new Complex128Array( [ 1.0, 0.0, 0.0, 1.0 ] );
	zrscl( 2, new Complex128( 0.0, 1.0 ), x, 1, 0 );
	assertArrayClose( toArray( x ), tc.x, 1e-14, 'x' );
});

test( 'zrscl: divide by -i gives x * i', function t() {
	var tc = findCase( 'div_by_neg_i' );
	var x = new Complex128Array( [ 1.0, 0.0, 0.0, 1.0 ] );
	zrscl( 2, new Complex128( 0.0, -1.0 ), x, 1, 0 );
	assertArrayClose( toArray( x ), tc.x, 1e-14, 'x' );
});

test( 'zrscl: negative real scalar a=(-3,0)', function t() {
	var tc = findCase( 'neg_real_scalar' );
	var x = new Complex128Array( [ 6.0, 9.0, -3.0, 12.0 ] );
	zrscl( 2, new Complex128( -3.0, 0.0 ), x, 1, 0 );
	assertArrayClose( toArray( x ), tc.x, 1e-14, 'x' );
});

test( 'zrscl: general complex, UR/UI > SAFMAX but <= OV (SAFMAX/UR scaling)', function t() { // eslint-disable-line max-len
	var tc = findCase( 'general_safmax_not_ov' );
	var x = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
	zrscl( 2, new Complex128( 3.0e307, 3.0e307 ), x, 1, 0 );
	assertArrayClose( toArray( x ), tc.x, 1e-10, 'x' );
});

test( 'zrscl: general complex overflow, absr < absi (UR/UI overflow from large AI)', function t() { // eslint-disable-line max-len
	var tc = findCase( 'general_overflow_absr_lt_absi' );
	var x = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
	zrscl( 2, new Complex128( 1.0, 1.0e200 ), x, 1, 0 );
	assertArrayClose( toArray( x ), tc.x, 1e-10, 'x' );
});

test( 'zrscl: general complex overflow, absr >= absi (UR/UI overflow from large AR)', function t() { // eslint-disable-line max-len
	var tc = findCase( 'general_overflow_absr_ge_absi' );
	var x = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
	zrscl( 2, new Complex128( 1.0e200, 1.0 ), x, 1, 0 );
	assertArrayClose( toArray( x ), tc.x, 1e-10, 'x' );
});
