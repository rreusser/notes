/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';


// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var zgtsv = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'zgtsv.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
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
* Creates a Complex128Array from interleaved re/im data.
*/
function c128( data ) {
	return new Complex128Array( new Float64Array( data ) );
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

test( 'zgtsv: basic_5x5_single_rhs', function t() {
	var nrhs;
	var info;
	var tc;
	var DL;
	var DU;
	var bv;
	var dv;
	var N;
	var d;
	var B;

	tc = findCase( 'basic_5x5_single_rhs' );
	N = 5;
	nrhs = 1;
	DL = c128( [ -1, 0, -1, 0, -1, 0, -1, 0 ] );
	d = c128( [ 2, 0, 2, 0, 2, 0, 2, 0, 2, 0 ] );
	DU = c128( [ -1, 0, -1, 0, -1, 0, -1, 0 ] );
	B = c128( [ 1, 0, 2, 0, 3, 0, 4, 0, 5, 0 ] );
	info = zgtsv( N, nrhs, DL, 1, 0, d, 1, 0, DU, 1, 0, B, 1, N, 0 );
	assert.equal( info, tc.info );
	dv = reinterpret( d, 0 );
	assertArrayClose( toArray( dv ), tc.d, 1e-14, 'd' );
	bv = reinterpret( B, 0 );
	assertArrayClose( toArray( bv ), tc.b, 1e-14, 'b' );
});

test( 'zgtsv: multi_rhs_complex', function t() {
	var bCol1;
	var bCol2;
	var nrhs;
	var info;
	var tc;
	var DL;
	var DU;
	var bv;
	var dv;
	var N;
	var d;
	var B;

	tc = findCase( 'multi_rhs_complex' );
	N = 4;
	nrhs = 2;
	DL = c128( [ 1, 1, 1, -1, 2, 0 ] );
	d = c128( [ 4, 0, 4, 1, 4, -1, 4, 0 ] );
	DU = c128( [ 1, -1, 1, 1, 1, 0 ] );
	B = c128([
		6,
		-1,
		10,
		1,
		10,
		-1,
		7,
		0,
		2,
		0,
		3,
		1,
		3,
		-1,
		2,
		0
	]);
	info = zgtsv( N, nrhs, DL, 1, 0, d, 1, 0, DU, 1, 0, B, 1, N, 0 );
	assert.equal( info, tc.info );
	dv = reinterpret( d, 0 );
	assertArrayClose( toArray( dv ), tc.d, 1e-14, 'd' );
	bv = reinterpret( B, 0 );
	bCol1 = toArray( bv ).slice( 0, 2 * N );
	bCol2 = toArray( bv ).slice( 2 * N, 4 * N );
	assertArrayClose( bCol1, tc.b.slice( 0, 2 * N ), 1e-13, 'b col1' );
	assertArrayClose( bCol2, tc.b.slice( 2 * N, 4 * N ), 1e-13, 'b col2' );
});

test( 'zgtsv: n_one', function t() {
	var info;
	var tc;
	var DL;
	var DU;
	var bv;
	var d;
	var B;

	tc = findCase( 'n_one' );
	d = c128( [ 5, 2 ] );
	DL = c128( [] );
	DU = c128( [] );
	B = c128( [ 10, 4 ] );
	info = zgtsv( 1, 1, DL, 1, 0, d, 1, 0, DU, 1, 0, B, 1, 1, 0 );
	assert.equal( info, tc.info );
	bv = reinterpret( B, 0 );
	assertArrayClose( toArray( bv ), tc.b, 1e-14, 'b' );
});

test( 'zgtsv: n_zero', function t() {
	var info;
	var tc;
	var DL;
	var DU;
	var d;
	var B;

	tc = findCase( 'n_zero' );
	d = c128( [] );
	DL = c128( [] );
	DU = c128( [] );
	B = c128( [] );
	info = zgtsv( 0, 1, DL, 1, 0, d, 1, 0, DU, 1, 0, B, 1, 0, 0 );
	assert.equal( info, tc.info );
});

test( 'zgtsv: singular', function t() {
	var info;
	var tc;
	var DL;
	var DU;
	var d;
	var B;

	tc = findCase( 'singular' );
	DL = c128( [ 0, 0, 0, 0 ] );
	d = c128( [ 0, 0, 2, 0, 3, 0 ] );
	DU = c128( [ 1, 0, 1, 0 ] );
	B = c128( [ 1, 0, 2, 0, 3, 0 ] );
	info = zgtsv( 3, 1, DL, 1, 0, d, 1, 0, DU, 1, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
});

test( 'zgtsv: pivoting', function t() {
	var info;
	var dlv;
	var duv;
	var tc;
	var DL;
	var DU;
	var bv;
	var dv;
	var N;
	var d;
	var B;

	tc = findCase( 'pivoting' );
	N = 4;
	DL = c128( [ 5, 0, 7, 0, 9, 0 ] );
	d = c128( [ 1, 0, 3, 0, 2, 0, 1, 0 ] );
	DU = c128( [ 2, 0, 4, 0, 6, 0 ] );
	B = c128( [ 5, 0, 12, 0, 15, 0, 10, 0 ] );
	info = zgtsv( N, 1, DL, 1, 0, d, 1, 0, DU, 1, 0, B, 1, N, 0 );
	assert.equal( info, tc.info );
	dv = reinterpret( d, 0 );
	assertArrayClose( toArray( dv ), tc.d, 1e-14, 'd' );
	dlv = reinterpret( DL, 0 );
	assertArrayClose( toArray( dlv ), tc.dl, 1e-14, 'dl' );
	duv = reinterpret( DU, 0 );
	assertArrayClose( toArray( duv ), tc.du, 1e-14, 'du' );
	bv = reinterpret( B, 0 );
	assertArrayClose( toArray( bv ), tc.b, 1e-13, 'b' );
});
