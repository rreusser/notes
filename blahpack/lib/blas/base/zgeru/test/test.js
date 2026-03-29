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
var zgeru = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'zgeru.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
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

test( 'zgeru: main export is a function', function t() {
	assert.strictEqual( typeof zgeru, 'function' );
});

test( 'zgeru: basic 2x3 rank-1 update', function t() {
	var tc = findCase( 'basic_2x3' );

	// A = 2x3 col-major, x = 2-elem, y = 3-elem, alpha = (1,0)
	var A = new Complex128Array([
		1,
		2,
		7,
		8,    // col 1: A(1,1)=1+2i, A(2,1)=7+8i
		3,
		4,
		9,
		10,   // col 2: A(1,2)=3+4i, A(2,2)=9+10i
		5,
		6,
		11,
		12   // col 3: A(1,3)=5+6i, A(2,3)=11+12i
	]);
	var x = new Complex128Array( [ 1, 1, 2, 2 ] );
	var y = new Complex128Array( [ 1, 0, 0, 1, 1, 1 ] );

	zgeru( 2, 3, new Complex128( 1, 0 ), x, 1, 0, y, 1, 0, A, 1, 2, 0 );
	assertArrayClose( toArray( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zgeru: alpha = (2, -1)', function t() {
	var tc = findCase( 'alpha_2_neg1' );

	// A = 2x2 identity, x = [(1,2), (3,4)], y = [(2,1), (1,-1)]
	var A = new Complex128Array([
		1,
		0,
		0,
		0,   // col 1
		0,
		0,
		1,
		0    // col 2
	]);
	var x = new Complex128Array( [ 1, 2, 3, 4 ] );
	var y = new Complex128Array( [ 2, 1, 1, -1 ] );

	zgeru( 2, 2, new Complex128( 2, -1 ), x, 1, 0, y, 1, 0, A, 1, 2, 0 );
	assertArrayClose( toArray( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zgeru: alpha = 0 (no update)', function t() {
	var tc = findCase( 'alpha_zero' );
	var A = new Complex128Array([
		5,
		6,
		7,
		8,

		// Note: the fixture has 8 doubles for a 2x2 matrix, but the alpha_zero test

		// In Fortran only prints the first column since M=2 N=2. Let me check.
		7,
		-1,
		16,
		-5
	]);
	var x = new Complex128Array( [ 1, 1, 2, 2 ] );
	var y = new Complex128Array( [ 3, 3, 4, 4 ] );

	zgeru( 2, 2, new Complex128( 0, 0 ), x, 1, 0, y, 1, 0, A, 1, 2, 0 );
	assertArrayClose( toArray( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zgeru: N=0 quick return', function t() {
	var tc = findCase( 'n_zero' );
	var A = new Complex128Array( [ 5, 6 ] );
	var x = new Complex128Array( [ 1, 1, 2, 2 ] );
	var y = new Complex128Array( [ 3, 3 ] );

	zgeru( 2, 0, new Complex128( 1, 0 ), x, 1, 0, y, 1, 0, A, 1, 2, 0 );
	assertArrayClose( toArray( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zgeru: M=0 quick return', function t() {
	var tc = findCase( 'm_zero' );
	var A = new Complex128Array( [ 5, 6 ] );
	var x = new Complex128Array( [ 1, 1 ] );
	var y = new Complex128Array( [ 3, 3, 4, 4 ] );

	zgeru( 0, 2, new Complex128( 1, 0 ), x, 1, 0, y, 1, 0, A, 1, 2, 0 );
	assertArrayClose( toArray( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zgeru: non-unit strides (incx=2, incy=2)', function t() {
	var tc = findCase( 'stride_2' );

	// x stored: [(1,0), (99,99), (0,1)], stride 2 picks elements 0 and 2

	// y stored: [(2,0), (88,88), (0,2)], stride 2 picks elements 0 and 2
	var A = new Complex128Array([
		0,
		0,
		0,
		0,
		0,
		0,
		0,
		0
	]);
	var x = new Complex128Array( [ 1, 0, 99, 99, 0, 1 ] );
	var y = new Complex128Array( [ 2, 0, 88, 88, 0, 2 ] );

	zgeru( 2, 2, new Complex128( 1, 0 ), x, 2, 0, y, 2, 0, A, 1, 2, 0 );
	assertArrayClose( toArray( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zgeru: negative stride incy=-1', function t() {
	var tc = findCase( 'neg_incy' );

	// x = [(1,1), (2,0)], y = [(3,0), (0,3)], incy=-1 reads y in reverse
	var A = new Complex128Array([
		0,
		0,
		0,
		0,
		0,
		0,
		0,
		0
	]);
	var x = new Complex128Array( [ 1, 1, 2, 0 ] );
	var y = new Complex128Array( [ 3, 0, 0, 3 ] );

	// In Fortran incy=-1, so y is read backward.

	// In our base.js convention, we pass offset = N-1 = 1, stride = -1
	zgeru( 2, 2, new Complex128( 1, 0 ), x, 1, 0, y, -1, 1, A, 1, 2, 0 );
	assertArrayClose( toArray( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zgeru: 1x1 matrix', function t() {
	var tc = findCase( 'one_by_one' );
	var A = new Complex128Array( [ 1, 1 ] );
	var x = new Complex128Array( [ 2, 3 ] );
	var y = new Complex128Array( [ 4, 5 ] );

	zgeru( 1, 1, new Complex128( 1, 0 ), x, 1, 0, y, 1, 0, A, 1, 1, 0 );
	assertArrayClose( toArray( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zgeru: y element zero skips column update', function t() {
	var Av;
	var A;
	var x;
	var y;

	A = new Complex128Array([
		1,
		2,
		3,
		4,   // col 1
		5,
		6,
		7,
		8    // col 2
	]);
	x = new Complex128Array( [ 10, 20, 30, 40 ] );
	y = new Complex128Array( [ 0, 0, 1, 0 ] );
	zgeru( 2, 2, new Complex128( 1, 0 ), x, 1, 0, y, 1, 0, A, 1, 2, 0 );
	Av = toArray( reinterpret( A, 0 ) );
	assertClose( Av[ 0 ], 1, 1e-14, 'A[0,0] re' );
	assertClose( Av[ 1 ], 2, 1e-14, 'A[0,0] im' );
	assertClose( Av[ 2 ], 3, 1e-14, 'A[1,0] re' );
	assertClose( Av[ 3 ], 4, 1e-14, 'A[1,0] im' );
	assertClose( Av[ 4 ], 15, 1e-14, 'A[0,1] re' );
	assertClose( Av[ 5 ], 26, 1e-14, 'A[0,1] im' );
	assertClose( Av[ 6 ], 37, 1e-14, 'A[1,1] re' );
	assertClose( Av[ 7 ], 48, 1e-14, 'A[1,1] im' );
});

test( 'zgeru: offset support', function t() {
	var Av;
	var A;
	var x;
	var y;

	A = new Complex128Array([
		99,
		99,        // garbage at index 0
		0,
		0,
		0,
		0,   // actual 1x2 col-major matrix starts at index 1
		0,
		0
	]);
	x = new Complex128Array( [ 88, 88, 2, 3 ] );
	y = new Complex128Array( [ 77, 77, 4, 5, 6, 7 ] );
	zgeru( 1, 2, new Complex128( 1, 0 ), x, 1, 1, y, 1, 1, A, 1, 1, 1 );
	Av = toArray( reinterpret( A, 0 ) );
	assertClose( Av[ 0 ], 99, 1e-14, 'garbage re' );
	assertClose( Av[ 1 ], 99, 1e-14, 'garbage im' );
	assertClose( Av[ 2 ], -7, 1e-14, 'A[0,0] re' );
	assertClose( Av[ 3 ], 22, 1e-14, 'A[0,0] im' );
	assertClose( Av[ 4 ], -9, 1e-14, 'A[0,1] re' );
	assertClose( Av[ 5 ], 32, 1e-14, 'A[0,1] im' );
});
