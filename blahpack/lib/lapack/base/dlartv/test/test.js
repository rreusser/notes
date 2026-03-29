/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';


// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlartv = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dlartv.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
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
* Asserts that two arrays are element-wise approximately equal.
*
* @private
* @param {*} actual - actual value
* @param {*} expected - expected value
* @param {number} tol - tolerance
* @param {string} msg - assertion message
*/
function assertArrayClose( actual, expected, tol, msg ) {
	var relErr;
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		relErr = Math.abs( actual[ i ] - expected[ i ] ) / Math.max( Math.abs( expected[ i ] ), 1.0 ); // eslint-disable-line max-len
		assert.ok( relErr <= tol, msg + '[' + i + ']: expected ' + expected[ i ] + ', got ' + actual[ i ] ); // eslint-disable-line max-len
	}
}


// TESTS //

test( 'dlartv is a function', function t() {
	assert.equal( typeof dlartv, 'function' );
});

test( 'dlartv: basic (N=4, unit strides)', function t() {
	var tc = findCase( 'basic' );
	var x = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
	var y = new Float64Array( [ 5.0, 6.0, 7.0, 8.0 ] );
	var c = new Float64Array( [ 0.8660254037844387, 0.7071067811865476, 0.5, 0.0 ] ); // eslint-disable-line max-len
	var s = new Float64Array( [ 0.5, 0.7071067811865476, 0.8660254037844387, 1.0 ] ); // eslint-disable-line max-len

	dlartv( 4, x, 1, 0, y, 1, 0, c, 1, 0, s, 1, 0 );

	assertArrayClose( x, tc.x, 1e-14, 'x' );
	assertArrayClose( y, tc.y, 1e-14, 'y' );
});

test( 'dlartv: n_zero (quick return)', function t() {
	var x = new Float64Array( [ 1.0 ] );
	var y = new Float64Array( [ 2.0 ] );
	var c = new Float64Array( [ 0.5 ] );
	var s = new Float64Array( [ 0.8660254037844387 ] );

	dlartv( 0, x, 1, 0, y, 1, 0, c, 1, 0, s, 1, 0 );

	// Arrays should be unchanged
	assert.equal( x[ 0 ], 1.0 );
	assert.equal( y[ 0 ], 2.0 );
});

test( 'dlartv: n_one (single element)', function t() {
	var tc = findCase( 'n_one' );
	var x = new Float64Array( [ 3.0 ] );
	var y = new Float64Array( [ 4.0 ] );
	var c = new Float64Array( [ 0.6 ] );
	var s = new Float64Array( [ 0.8 ] );

	dlartv( 1, x, 1, 0, y, 1, 0, c, 1, 0, s, 1, 0 );

	assertArrayClose( x, tc.x, 1e-14, 'x' );
	assertArrayClose( y, tc.y, 1e-14, 'y' );
});

test( 'dlartv: non-unit strides (INCX=2, INCY=2, INCC=2)', function t() {
	var tc = findCase( 'non_unit_stride' );
	var x = new Float64Array( [ 1.0, 0.0, 2.0, 0.0, 3.0 ] );
	var y = new Float64Array( [ 4.0, 0.0, 5.0, 0.0, 6.0 ] );
	var c = new Float64Array( [ 0.8, 0.0, 0.6, 0.0, 0.0 ] );
	var s = new Float64Array( [ 0.6, 0.0, 0.8, 0.0, 0.0 ] );

	dlartv( 3, x, 2, 0, y, 2, 0, c, 2, 0, s, 2, 0 );

	assertArrayClose( x, tc.x, 1e-14, 'x' );
	assertArrayClose( y, tc.y, 1e-14, 'y' );
});

test( 'dlartv: identity rotation (c=1, s=0)', function t() {
	var tc = findCase( 'identity' );
	var x = new Float64Array( [ 10.0, 20.0, 30.0 ] );
	var y = new Float64Array( [ 40.0, 50.0, 60.0 ] );
	var c = new Float64Array( [ 1.0, 1.0, 1.0 ] );
	var s = new Float64Array( [ 0.0, 0.0, 0.0 ] );

	dlartv( 3, x, 1, 0, y, 1, 0, c, 1, 0, s, 1, 0 );

	assertArrayClose( x, tc.x, 1e-14, 'x' );
	assertArrayClose( y, tc.y, 1e-14, 'y' );
});

test( 'dlartv: swap rotation (c=0, s=1)', function t() {
	var tc = findCase( 'swap' );
	var x = new Float64Array( [ 1.0, 2.0 ] );
	var y = new Float64Array( [ 3.0, 4.0 ] );
	var c = new Float64Array( [ 0.0, 0.0 ] );
	var s = new Float64Array( [ 1.0, 1.0 ] );

	dlartv( 2, x, 1, 0, y, 1, 0, c, 1, 0, s, 1, 0 );

	assertArrayClose( x, tc.x, 1e-14, 'x' );
	assertArrayClose( y, tc.y, 1e-14, 'y' );
});

test( 'dlartv: mixed strides (INCX=1, INCY=3, INCC=2)', function t() {
	var tc = findCase( 'mixed_strides' );
	var x = new Float64Array( [ 2.0, 4.0 ] );
	var y = new Float64Array( [ 6.0, 0.0, 0.0, 8.0 ] );
	var c = new Float64Array( [ 0.8, 0.0, 0.6 ] );
	var s = new Float64Array( [ 0.6, 0.0, 0.8 ] );

	dlartv( 2, x, 1, 0, y, 3, 0, c, 2, 0, s, 2, 0 );

	assertArrayClose( x, tc.x, 1e-14, 'x' );
	assertArrayClose( y, tc.y, 1e-14, 'y' );
});

test( 'dlartv: offset support', function t() {
	var x = new Float64Array( [ 999.0, 3.0, 4.0 ] );
	var y = new Float64Array( [ 999.0, 5.0, 6.0 ] );
	var c = new Float64Array( [ 999.0, 0.6, 0.8 ] );
	var s = new Float64Array( [ 999.0, 0.8, 0.6 ] );

	dlartv( 2, x, 1, 1, y, 1, 1, c, 1, 1, s, 1, 1 );

	// x[1] = 0.6*3 + 0.8*5 = 1.8 + 4.0 = 5.8

	// y[1] = 0.6*5 - 0.8*3 = 3.0 - 2.4 = 0.6

	// x[2] = 0.8*4 + 0.6*6 = 3.2 + 3.6 = 6.8

	// y[2] = 0.8*6 - 0.6*4 = 4.8 - 2.4 = 2.4
	assert.ok( Math.abs( x[ 0 ] - 999.0 ) < 1e-14, 'x[0] unchanged' );
	assert.ok( Math.abs( x[ 1 ] - 5.8 ) < 1e-14, 'x[1]' );
	assert.ok( Math.abs( x[ 2 ] - 6.8 ) < 1e-14, 'x[2]' );
	assert.ok( Math.abs( y[ 0 ] - 999.0 ) < 1e-14, 'y[0] unchanged' );
	assert.ok( Math.abs( y[ 1 ] - 0.6 ) < 1e-14, 'y[1]' );
	assert.ok( Math.abs( y[ 2 ] - 2.4 ) < 1e-14, 'y[2]' );
});
