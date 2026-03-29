/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';


// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Complex128 = require( '@stdlib/complex/float64/ctor' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zsymm = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'zsymm.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
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

test( 'zsymm: left_upper_basic', function t() {
	var tc = findCase( 'left_upper_basic' );

	// A is 3x3 symmetric upper: A = [2+i 1+i 3-2i; * 4-i 2+i; * * 5+2i]
	var A = new Complex128Array([
		2,
		1,
		0,
		0,
		0,
		0,       // col 1: A(1,1)=2+i, A(2,1)=0, A(3,1)=0
		1,
		1,
		4,
		-1,
		0,
		0,      // col 2: A(1,2)=1+i, A(2,2)=4-i, A(3,2)=0
		3,
		-2,
		2,
		1,
		5,
		2       // col 3: A(1,3)=3-2i, A(2,3)=2+i, A(3,3)=5+2i
	]);
	var B = new Complex128Array([
		1,
		0.5,
		2,
		-1,
		3,
		1,    // col 1
		4,
		2,
		5,
		0,
		6,
		-0.5     // col 2
	]);
	var C = new Complex128Array( 6 );

	zsymm( 'left', 'upper', 3, 2, new Complex128( 1, 0 ), A, 1, 3, 0, B, 1, 3, 0, new Complex128( 0, 0 ), C, 1, 3, 0 ); // eslint-disable-line max-len
	assertArrayClose( toArray( reinterpret( C, 0 ) ), tc.C, 1e-14, 'C' );
});

test( 'zsymm: left_lower_basic', function t() {
	var tc = findCase( 'left_lower_basic' );

	// A is 3x3 symmetric lower (same matrix, NO conjugation in off-diag)
	var A = new Complex128Array([
		2,
		1,
		1,
		1,
		3,
		-2,      // col 1: A(1,1)=2+i, A(2,1)=1+i, A(3,1)=3-2i
		0,
		0,
		4,
		-1,
		2,
		1,      // col 2: A(1,2)=0, A(2,2)=4-i, A(3,2)=2+i
		0,
		0,
		0,
		0,
		5,
		2        // col 3: A(1,3)=0, A(2,3)=0, A(3,3)=5+2i
	]);
	var B = new Complex128Array([
		1,
		0.5,
		2,
		-1,
		3,
		1,
		4,
		2,
		5,
		0,
		6,
		-0.5
	]);
	var C = new Complex128Array( 6 );

	zsymm( 'left', 'lower', 3, 2, new Complex128( 1, 0 ), A, 1, 3, 0, B, 1, 3, 0, new Complex128( 0, 0 ), C, 1, 3, 0 ); // eslint-disable-line max-len
	assertArrayClose( toArray( reinterpret( C, 0 ) ), tc.C, 1e-14, 'C' );
});

test( 'zsymm: right_upper_basic', function t() {
	var tc = findCase( 'right_upper_basic' );
	var A = new Complex128Array([
		2,
		1,
		0,
		0,
		0,
		0,
		1,
		1,
		4,
		-1,
		0,
		0,
		3,
		-2,
		2,
		1,
		5,
		2
	]);

	// B is 2x3, LDB=2
	var B = new Complex128Array([
		1,
		0.5,
		2,
		-1,
		3,
		1,
		4,
		2,
		5,
		0,
		6,
		-0.5
	]);
	var C = new Complex128Array( 6 );

	zsymm( 'right', 'upper', 2, 3, new Complex128( 1, 0 ), A, 1, 3, 0, B, 1, 2, 0, new Complex128( 0, 0 ), C, 1, 2, 0 ); // eslint-disable-line max-len
	assertArrayClose( toArray( reinterpret( C, 0 ) ), tc.C, 1e-14, 'C' );
});

test( 'zsymm: right_lower_basic', function t() {
	var tc = findCase( 'right_lower_basic' );
	var A = new Complex128Array([
		2,
		1,
		1,
		1,
		3,
		-2,
		0,
		0,
		4,
		-1,
		2,
		1,
		0,
		0,
		0,
		0,
		5,
		2
	]);
	var B = new Complex128Array([
		1,
		0.5,
		2,
		-1,
		3,
		1,
		4,
		2,
		5,
		0,
		6,
		-0.5
	]);
	var C = new Complex128Array( 6 );

	zsymm( 'right', 'lower', 2, 3, new Complex128( 1, 0 ), A, 1, 3, 0, B, 1, 2, 0, new Complex128( 0, 0 ), C, 1, 2, 0 ); // eslint-disable-line max-len
	assertArrayClose( toArray( reinterpret( C, 0 ) ), tc.C, 1e-14, 'C' );
});

test( 'zsymm: complex_alpha_beta', function t() {
	var tc = findCase( 'complex_alpha_beta' );
	var A = new Complex128Array([
		2,
		1,
		0,
		0,
		0,
		0,
		1,
		1,
		4,
		-1,
		0,
		0,
		3,
		-2,
		2,
		1,
		5,
		2
	]);
	var B = new Complex128Array([
		1,
		0.5,
		2,
		-1,
		3,
		1,
		4,
		2,
		5,
		0,
		6,
		-0.5
	]);
	var C = new Complex128Array([
		1,
		1,
		2,
		-1,
		0.5,
		0.5,
		1,
		0,
		0,
		2,
		3,
		-1
	]);

	zsymm( 'left', 'upper', 3, 2, new Complex128( 2, 1 ), A, 1, 3, 0, B, 1, 3, 0, new Complex128( 0.5, -0.5 ), C, 1, 3, 0 ); // eslint-disable-line max-len
	assertArrayClose( toArray( reinterpret( C, 0 ) ), tc.C, 1e-14, 'C' );
});

test( 'zsymm: alpha_zero', function t() {
	var tc = findCase( 'alpha_zero' );
	var A = new Complex128Array( 9 );
	var B = new Complex128Array( 4 );
	var C = new Complex128Array([
		1,
		2,
		3,
		4,
		5,
		6,
		7,
		8
	]);

	zsymm( 'left', 'upper', 2, 2, new Complex128( 0, 0 ), A, 1, 3, 0, B, 1, 2, 0, new Complex128( 2, 0 ), C, 1, 2, 0 ); // eslint-disable-line max-len
	assertArrayClose( toArray( reinterpret( C, 0 ) ), tc.C, 1e-14, 'C' );
});

test( 'zsymm: m_zero', function t() {
	var tc;
	var C;
	var v;

	tc = findCase( 'm_zero' );
	C = new Complex128Array( [ 99, 0 ] );
	zsymm( 'left', 'upper', 0, 2, new Complex128( 1, 0 ), new Complex128Array( 1 ), 1, 1, 0, new Complex128Array( 1 ), 1, 1, 0, new Complex128( 0, 0 ), C, 1, 1, 0 ); // eslint-disable-line max-len
	v = reinterpret( C, 0 );
	assertClose( v[ 0 ], tc.C[ 0 ], 1e-14, 'C[0]' );
	assertClose( v[ 1 ], tc.C[ 1 ], 1e-14, 'C[1]' );
});

test( 'zsymm: n_zero', function t() {
	var tc;
	var C;
	var v;

	tc = findCase( 'n_zero' );
	C = new Complex128Array( [ 99, 0 ] );
	zsymm( 'left', 'upper', 2, 0, new Complex128( 1, 0 ), new Complex128Array( 4 ), 1, 2, 0, new Complex128Array( 4 ), 1, 2, 0, new Complex128( 0, 0 ), C, 1, 2, 0 ); // eslint-disable-line max-len
	v = reinterpret( C, 0 );
	assertClose( v[ 0 ], tc.C[ 0 ], 1e-14, 'C[0]' );
	assertClose( v[ 1 ], tc.C[ 1 ], 1e-14, 'C[1]' );
});

test( 'zsymm: scalar', function t() {
	var tc = findCase( 'scalar' );

	// A(1,1) = 3+i (complex diagonal), B(1,1) = 5+2i, alpha = 2+i
	var A = new Complex128Array( [ 3, 1 ] );
	var B = new Complex128Array( [ 5, 2 ] );
	var C = new Complex128Array( 1 );

	zsymm( 'left', 'upper', 1, 1, new Complex128( 2, 1 ), A, 1, 1, 0, B, 1, 1, 0, new Complex128( 0, 0 ), C, 1, 1, 0 ); // eslint-disable-line max-len
	assertArrayClose( toArray( reinterpret( C, 0 ) ), tc.C, 1e-14, 'C' );
});

test( 'zsymm: beta_zero', function t() {
	var tc = findCase( 'beta_zero' );

	// A is 2x2 symmetric lower: A(1,1)=1+0.5i, A(2,1)=0, A(2,2)=1-0.5i
	var A = new Complex128Array([
		1,
		0.5,
		0,
		0,
		0,
		0,
		1,
		-0.5
	]);
	var B = new Complex128Array([
		2,
		1,
		3,
		-1,
		4,
		0.5,
		5,
		2
	]);
	var C = new Complex128Array([
		999,
		999,
		999,
		999,
		999,
		999,
		999,
		999
	]);

	zsymm( 'left', 'lower', 2, 2, new Complex128( 1, 0 ), A, 1, 2, 0, B, 1, 2, 0, new Complex128( 0, 0 ), C, 1, 2, 0 ); // eslint-disable-line max-len
	assertArrayClose( toArray( reinterpret( C, 0 ) ), tc.C, 1e-14, 'C' );
});

test( 'zsymm: alpha_zero_beta_zero (zeros C)', function t() {
	var tc = findCase( 'alpha_zero_beta_zero' );
	var A = new Complex128Array( 4 );
	var B = new Complex128Array( 4 );
	var C = new Complex128Array([
		99,
		88,
		77,
		66,
		55,
		44,
		33,
		22
	]);

	zsymm( 'left', 'upper', 2, 2, new Complex128( 0, 0 ), A, 1, 2, 0, B, 1, 2, 0, new Complex128( 0, 0 ), C, 1, 2, 0 ); // eslint-disable-line max-len
	assertArrayClose( toArray( reinterpret( C, 0 ) ), tc.C, 1e-14, 'C' );
});

test( 'zsymm: left_lower_nonzero_beta', function t() {
	var tc = findCase( 'left_lower_nonzero_beta' );
	var A = new Complex128Array([
		2,
		1,
		1,
		1,
		3,
		-2,
		0,
		0,
		4,
		-1,
		2,
		1,
		0,
		0,
		0,
		0,
		5,
		2
	]);
	var B = new Complex128Array([
		1,
		0.5,
		2,
		-1,
		3,
		1,
		4,
		2,
		5,
		0,
		6,
		-0.5
	]);
	var C = new Complex128Array([
		1,
		1,
		2,
		-1,
		0.5,
		0.5,
		1,
		0,
		0,
		2,
		3,
		-1
	]);

	zsymm( 'left', 'lower', 3, 2, new Complex128( 1, 0 ), A, 1, 3, 0, B, 1, 3, 0, new Complex128( 0.5, 0 ), C, 1, 3, 0 ); // eslint-disable-line max-len
	assertArrayClose( toArray( reinterpret( C, 0 ) ), tc.C, 1e-14, 'C' );
});

test( 'zsymm: right_upper_nonzero_beta', function t() {
	var tc = findCase( 'right_upper_nonzero_beta' );
	var A = new Complex128Array([
		2,
		1,
		0,
		0,
		0,
		0,
		1,
		1,
		4,
		-1,
		0,
		0,
		3,
		-2,
		2,
		1,
		5,
		2
	]);
	var B = new Complex128Array([
		1,
		0.5,
		2,
		-1,
		3,
		1,
		4,
		2,
		5,
		0,
		6,
		-0.5
	]);
	var C = new Complex128Array([
		1,
		1,
		2,
		-1,
		0.5,
		0.5,
		1,
		0,
		0,
		2,
		3,
		-1
	]);

	zsymm( 'right', 'upper', 2, 3, new Complex128( 1, 0 ), A, 1, 3, 0, B, 1, 2, 0, new Complex128( 0.5, 0.5 ), C, 1, 2, 0 ); // eslint-disable-line max-len
	assertArrayClose( toArray( reinterpret( C, 0 ) ), tc.C, 1e-14, 'C' );
});
