/* eslint-disable no-restricted-syntax, stdlib/first-unit-test, max-len */

'use strict';


// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync; // eslint-disable-line node/no-sync
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zpptrf = require( '../../zpptrf/lib/base.js' );
var zhpgst = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'zhpgst.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
var fixture = lines.map( function parse( line ) {
	return JSON.parse( line );
} );


// VARIABLES //

// 3x3 Hermitian matrices for tests 1-6:
var B3U = [ 10, 0, 2, 1, 8, 0, 3, -2, 1, 1, 6, 0 ];
var B3L = [ 10, 0, 2, -1, 3, 2, 8, 0, 1, -1, 6, 0 ];
var A3U = [ 12, 0, 3, 2, 9, 0, 1, -1, 2, 3, 7, 0 ];
var A3L = [ 12, 0, 3, -2, 1, 1, 9, 0, 2, -3, 7, 0 ];

// 4x4 Hermitian matrices for tests 7-12:
var B4U = [ 20, 0, 3, 1, 15, 0, 1, -2, 2, 3, 12, 0, 4, 1, 1, -1, 3, 2, 10, 0 ];
var B4L = [ 20, 0, 3, -1, 1, 2, 4, -1, 15, 0, 2, -3, 1, 1, 12, 0, 3, -2, 10, 0 ]; // eslint-disable-line max-len
var A4U = [ 18, 0, 5, 2, 14, 0, 2, -1, 4, 3, 11, 0, 3, 1, 1, -2, 2, 1, 8, 0 ];
var A4L = [ 18, 0, 5, -2, 2, 1, 3, -1, 14, 0, 4, -3, 1, 2, 11, 0, 2, -1, 8, 0 ];


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
* Asserts that two arrays are element-wise approximately equal.
*
* @private
* @param {*} actual - actual value
* @param {*} expected - expected value
* @param {number} tol - tolerance
* @param {string} msg - assertion message
*/
function assertArrayClose( actual, expected, tol, msg ) {
	var absErr;
	var relErr;
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i += 1 ) {
		absErr = Math.abs( actual[ i ] - expected[ i ] );

		// Use absolute tolerance for near-zero values (e.g. imaginary parts of Hermitian diagonal): // eslint-disable-line max-len
		if ( absErr < 1e-13 ) {
			continue;
		}
		relErr = absErr / Math.max( Math.abs( expected[ i ] ), 1e-14 );
		assert.ok( relErr <= tol, msg + '[' + i + ']: expected ' + expected[ i ] + ', got ' + actual[ i ] ); // eslint-disable-line max-len
	}
}

/**
* Creates a Complex128Array from interleaved re/im values.
*
* @private
* @param {Array<number>} arr - interleaved values
* @returns {Complex128Array} complex array
*/
function c128( arr ) {
	return new Complex128Array( new Float64Array( arr ) );
}


// TESTS //

test( 'zhpgst is a function', function t() {
	assert.equal( typeof zhpgst, 'function' );
});

test( 'zhpgst: itype1_upper_3', function t() {
	var info;
	var tc;
	var ap;
	var bp;

	tc = findCase( 'itype1_upper_3' );
	bp = c128( B3U );
	zpptrf( 'upper', 3, bp, 1, 0 );
	ap = c128( A3U );
	info = zhpgst( 1, 'upper', 3, ap, 1, 0, bp, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( reinterpret( ap, 0 ), tc.AP, 1e-13, 'AP' );
});

test( 'zhpgst: itype1_lower_3', function t() {
	var info;
	var tc;
	var ap;
	var bp;

	tc = findCase( 'itype1_lower_3' );
	bp = c128( B3L );
	zpptrf( 'lower', 3, bp, 1, 0 );
	ap = c128( A3L );
	info = zhpgst( 1, 'lower', 3, ap, 1, 0, bp, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( reinterpret( ap, 0 ), tc.AP, 1e-13, 'AP' );
});

test( 'zhpgst: itype2_upper_3', function t() {
	var info;
	var tc;
	var ap;
	var bp;

	tc = findCase( 'itype2_upper_3' );
	bp = c128( B3U );
	zpptrf( 'upper', 3, bp, 1, 0 );
	ap = c128( A3U );
	info = zhpgst( 2, 'upper', 3, ap, 1, 0, bp, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( reinterpret( ap, 0 ), tc.AP, 1e-13, 'AP' );
});

test( 'zhpgst: itype2_lower_3', function t() {
	var info;
	var tc;
	var ap;
	var bp;

	tc = findCase( 'itype2_lower_3' );
	bp = c128( B3L );
	zpptrf( 'lower', 3, bp, 1, 0 );
	ap = c128( A3L );
	info = zhpgst( 2, 'lower', 3, ap, 1, 0, bp, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( reinterpret( ap, 0 ), tc.AP, 1e-13, 'AP' );
});

test( 'zhpgst: itype3_upper_3', function t() {
	var info;
	var tc;
	var ap;
	var bp;

	tc = findCase( 'itype3_upper_3' );
	bp = c128( B3U );
	zpptrf( 'upper', 3, bp, 1, 0 );
	ap = c128( A3U );
	info = zhpgst( 3, 'upper', 3, ap, 1, 0, bp, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( reinterpret( ap, 0 ), tc.AP, 1e-13, 'AP' );
});

test( 'zhpgst: itype3_lower_3', function t() {
	var info;
	var tc;
	var ap;
	var bp;

	tc = findCase( 'itype3_lower_3' );
	bp = c128( B3L );
	zpptrf( 'lower', 3, bp, 1, 0 );
	ap = c128( A3L );
	info = zhpgst( 3, 'lower', 3, ap, 1, 0, bp, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( reinterpret( ap, 0 ), tc.AP, 1e-13, 'AP' );
});

test( 'zhpgst: itype1_upper_4', function t() {
	var info;
	var tc;
	var ap;
	var bp;

	tc = findCase( 'itype1_upper_4' );
	bp = c128( B4U );
	zpptrf( 'upper', 4, bp, 1, 0 );
	ap = c128( A4U );
	info = zhpgst( 1, 'upper', 4, ap, 1, 0, bp, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( reinterpret( ap, 0 ), tc.AP, 1e-13, 'AP' );
});

test( 'zhpgst: itype1_lower_4', function t() {
	var info;
	var tc;
	var ap;
	var bp;

	tc = findCase( 'itype1_lower_4' );
	bp = c128( B4L );
	zpptrf( 'lower', 4, bp, 1, 0 );
	ap = c128( A4L );
	info = zhpgst( 1, 'lower', 4, ap, 1, 0, bp, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( reinterpret( ap, 0 ), tc.AP, 1e-13, 'AP' );
});

test( 'zhpgst: itype2_upper_4', function t() {
	var info;
	var tc;
	var ap;
	var bp;

	tc = findCase( 'itype2_upper_4' );
	bp = c128( B4U );
	zpptrf( 'upper', 4, bp, 1, 0 );
	ap = c128( A4U );
	info = zhpgst( 2, 'upper', 4, ap, 1, 0, bp, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( reinterpret( ap, 0 ), tc.AP, 1e-13, 'AP' );
});

test( 'zhpgst: itype2_lower_4', function t() {
	var info;
	var tc;
	var ap;
	var bp;

	tc = findCase( 'itype2_lower_4' );
	bp = c128( B4L );
	zpptrf( 'lower', 4, bp, 1, 0 );
	ap = c128( A4L );
	info = zhpgst( 2, 'lower', 4, ap, 1, 0, bp, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( reinterpret( ap, 0 ), tc.AP, 1e-13, 'AP' );
});

test( 'zhpgst: itype3_upper_4', function t() {
	var info;
	var tc;
	var ap;
	var bp;

	tc = findCase( 'itype3_upper_4' );
	bp = c128( B4U );
	zpptrf( 'upper', 4, bp, 1, 0 );
	ap = c128( A4U );
	info = zhpgst( 3, 'upper', 4, ap, 1, 0, bp, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( reinterpret( ap, 0 ), tc.AP, 1e-13, 'AP' );
});

test( 'zhpgst: itype3_lower_4', function t() {
	var info;
	var tc;
	var ap;
	var bp;

	tc = findCase( 'itype3_lower_4' );
	bp = c128( B4L );
	zpptrf( 'lower', 4, bp, 1, 0 );
	ap = c128( A4L );
	info = zhpgst( 3, 'lower', 4, ap, 1, 0, bp, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( reinterpret( ap, 0 ), tc.AP, 1e-13, 'AP' );
});

test( 'zhpgst: n_zero (quick return)', function t() {
	var info;
	var tc;
	var ap;
	var bp;

	tc = findCase( 'n_zero' );
	ap = c128( [ 0, 0 ] );
	bp = c128( [ 0, 0 ] );
	info = zhpgst( 1, 'upper', 0, ap, 1, 0, bp, 1, 0 );
	assert.equal( info, tc.info );
});

test( 'zhpgst: n_one_itype1_upper', function t() {
	var info;
	var tc;
	var ap;
	var bp;

	tc = findCase( 'n_one_itype1_upper' );
	bp = c128( [ 9, 0 ] );
	zpptrf( 'upper', 1, bp, 1, 0 );
	ap = c128( [ 12, 0 ] );
	info = zhpgst( 1, 'upper', 1, ap, 1, 0, bp, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( reinterpret( ap, 0 ), tc.AP, 1e-14, 'AP' );
});

test( 'zhpgst: n_one_itype1_lower', function t() {
	var info;
	var tc;
	var ap;
	var bp;

	tc = findCase( 'n_one_itype1_lower' );
	bp = c128( [ 16, 0 ] );
	zpptrf( 'lower', 1, bp, 1, 0 );
	ap = c128( [ 25, 0 ] );
	info = zhpgst( 1, 'lower', 1, ap, 1, 0, bp, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( reinterpret( ap, 0 ), tc.AP, 1e-14, 'AP' );
});

test( 'zhpgst: n_one_itype2', function t() {
	var info;
	var tc;
	var ap;
	var bp;

	tc = findCase( 'n_one_itype2' );
	bp = c128( [ 4, 0 ] );
	zpptrf( 'upper', 1, bp, 1, 0 );
	ap = c128( [ 5, 0 ] );
	info = zhpgst( 2, 'upper', 1, ap, 1, 0, bp, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( reinterpret( ap, 0 ), tc.AP, 1e-14, 'AP' );
});

test( 'zhpgst: n_one_itype3', function t() {
	var info;
	var tc;
	var ap;
	var bp;

	tc = findCase( 'n_one_itype3' );
	bp = c128( [ 4, 0 ] );
	zpptrf( 'lower', 1, bp, 1, 0 );
	ap = c128( [ 5, 0 ] );
	info = zhpgst( 3, 'lower', 1, ap, 1, 0, bp, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( reinterpret( ap, 0 ), tc.AP, 1e-14, 'AP' );
});
