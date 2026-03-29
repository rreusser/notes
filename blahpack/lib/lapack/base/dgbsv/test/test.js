/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';


// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dgbsv = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dgbsv.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
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
* Creates a band matrix in column-major flat array.
*
* @param {number} ldab - leading dimension (2*KL+KU+1)
* @param {number} n - number of columns
* @param {Array} entries - array of [row, col, value] (0-based row/col in band storage)
* @returns {Float64Array} flat band matrix
*/
function bandMatrix( ldab, n, entries ) {
	var AB = new Float64Array( ldab * n );
	var i;
	for ( i = 0; i < entries.length; i++ ) {
		AB[ entries[i][0] + entries[i][1] * ldab ] = entries[i][2];
	}
	return AB;
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

test( 'dgbsv: 4x4 tridiagonal, single RHS', function t() {
	var IPIV;
	var info;
	var tc;
	var AB;
	var B;

	tc = findCase( 'tridiag_4x4_1rhs' );
	AB = bandMatrix( 4, 4, [
		// Col 0
		[ 2, 0, 4.0 ],
		[ 3, 0, -1.0 ],

		// Col 1
		[ 1, 1, -1.0 ],
		[ 2, 1, 4.0 ],
		[ 3, 1, -1.0 ],

		// Col 2
		[ 1, 2, -1.0 ],
		[ 2, 2, 4.0 ],
		[ 3, 2, -1.0 ],

		// Col 3
		[ 1, 3, -1.0 ],
		[ 2, 3, 4.0 ]
	]);
	B = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
	IPIV = new Int32Array( 4 );
	info = dgbsv( 4, 1, 1, 1, AB, 1, 4, 0, IPIV, 1, 0, B, 1, 4, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( toArray( B ), tc.x, 1e-14, 'x' );
});

test( 'dgbsv: 4x4 tridiagonal, 2 RHS', function t() {
	var IPIV;
	var info;
	var tc;
	var AB;
	var B;

	tc = findCase( 'tridiag_4x4_2rhs' );
	AB = bandMatrix( 4, 4, [
		[ 2, 0, 4.0 ],
		[ 3, 0, -1.0 ],
		[ 1, 1, -1.0 ],
		[ 2, 1, 4.0 ],
		[ 3, 1, -1.0 ],
		[ 1, 2, -1.0 ],
		[ 2, 2, 4.0 ],
		[ 3, 2, -1.0 ],
		[ 1, 3, -1.0 ],
		[ 2, 3, 4.0 ]
	]);
	B = new Float64Array( 8 );
	B[ 0 ] = 1.0;
	B[ 1 ] = 2.0;
	B[ 2 ] = 3.0;
	B[ 3 ] = 4.0;
	B[ 4 ] = 4.0;
	B[ 5 ] = 3.0;
	B[ 6 ] = 2.0;
	B[ 7 ] = 1.0;
	IPIV = new Int32Array( 4 );
	info = dgbsv( 4, 1, 1, 2, AB, 1, 4, 0, IPIV, 1, 0, B, 1, 4, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( toArray( B ), tc.x, 1e-14, 'x' );
});

test( 'dgbsv: 5x5 pentadiagonal, single RHS', function t() {
	var IPIV;
	var info;
	var tc;
	var AB;
	var B;

	tc = findCase( 'pentadiag_5x5_1rhs' );
	AB = bandMatrix( 7, 5, [
		// Col 0: diag at row KL+KU=4, subdiags at row 5,6
		[ 4, 0, 6.0 ],
		[ 5, 0, -2.0 ],
		[ 6, 0, 1.0 ],

		// Col 1: superdiag at row 3, diag at row 4, sub at 5,6
		[ 3, 1, -2.0 ],
		[ 4, 1, 6.0 ],
		[ 5, 1, -2.0 ],
		[ 6, 1, 1.0 ],

		// Col 2: super at row 2,3, diag at 4, sub at 5,6
		[ 2, 2, 1.0 ],
		[ 3, 2, -2.0 ],
		[ 4, 2, 6.0 ],
		[ 5, 2, -2.0 ],
		[ 6, 2, 1.0 ],

		// Col 3
		[ 2, 3, 1.0 ],
		[ 3, 3, -2.0 ],
		[ 4, 3, 6.0 ],
		[ 5, 3, -2.0 ],

		// Col 4
		[ 2, 4, 1.0 ],
		[ 3, 4, -2.0 ],
		[ 4, 4, 6.0 ]
	]);
	B = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0 ] );
	IPIV = new Int32Array( 5 );
	info = dgbsv( 5, 2, 2, 1, AB, 1, 7, 0, IPIV, 1, 0, B, 1, 5, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( toArray( B ), tc.x, 1e-14, 'x' );
});

test( 'dgbsv: N=0 quick return', function t() {
	var IPIV;
	var info;
	var tc;
	var AB;
	var B;

	tc = findCase( 'n_zero' );
	AB = new Float64Array( 4 );
	B = new Float64Array( 1 );
	IPIV = new Int32Array( 1 );
	info = dgbsv( 0, 1, 1, 1, AB, 1, 4, 0, IPIV, 1, 0, B, 1, 1, 0 );
	assert.equal( info, tc.info, 'info' );
});

test( 'dgbsv: NRHS=0 still factorizes AB', function t() {
	var anyNonZero;
	var IPIV;
	var info;
	var tc;
	var AB;
	var i;

	tc = findCase( 'nrhs_zero' );
	AB = bandMatrix( 4, 4, [
		[ 2, 0, 4.0 ],
		[ 3, 0, -1.0 ],
		[ 1, 1, -1.0 ],
		[ 2, 1, 4.0 ],
		[ 3, 1, -1.0 ],
		[ 1, 2, -1.0 ],
		[ 2, 2, 4.0 ],
		[ 3, 2, -1.0 ],
		[ 1, 3, -1.0 ],
		[ 2, 3, 4.0 ]
	]);
	IPIV = new Int32Array( 4 );
	info = dgbsv( 4, 1, 1, 0, AB, 1, 4, 0, IPIV, 1, 0, new Float64Array( 1 ), 1, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	anyNonZero = false;
	for ( i = 0; i < 4; i++ ) {
		if ( IPIV[ i ] !== 0 ) {
			anyNonZero = true;
		}
	}
	assert.equal( IPIV[ 0 ], 0, 'IPIV[0]' );
	assert.equal( IPIV[ 1 ], 1, 'IPIV[1]' );
	assert.equal( IPIV[ 2 ], 2, 'IPIV[2]' );
	assert.equal( IPIV[ 3 ], 3, 'IPIV[3]' );
});

test( 'dgbsv: singular matrix returns info > 0', function t() {
	var IPIV;
	var info;
	var tc;
	var AB;
	var B;

	tc = findCase( 'singular' );
	AB = bandMatrix( 4, 2, [
		[ 2, 0, 1.0 ]  // diag(0) = 1, diag(1) = 0 (default)
	]);
	B = new Float64Array( [ 1.0, 2.0 ] );
	IPIV = new Int32Array( 2 );
	info = dgbsv( 2, 1, 1, 1, AB, 1, 4, 0, IPIV, 1, 0, B, 1, 2, 0 );
	assert.equal( info, tc.info, 'info (singular at position 2)' );
});

test( 'dgbsv: pivoting 2x2', function t() {
	var IPIV;
	var info;
	var tc;
	var AB;
	var B;

	tc = findCase( 'pivot_2x2' );
	AB = bandMatrix( 4, 2, [
		[ 2, 0, 1.0 ],
		[ 3, 0, 3.0 ],  // col 0: diag=1, sub=3
		[ 1, 1, 2.0 ],
		[ 2, 1, 4.0 ]    // col 1: super=2, diag=4
	]);
	B = new Float64Array( [ 5.0, 11.0 ] );
	IPIV = new Int32Array( 2 );
	info = dgbsv( 2, 1, 1, 1, AB, 1, 4, 0, IPIV, 1, 0, B, 1, 2, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( toArray( B ), tc.x, 1e-14, 'x' );
});

test( 'dgbsv: 5x5 pentadiagonal, 3 RHS', function t() {
	var IPIV;
	var info;
	var tc;
	var AB;
	var B;

	tc = findCase( 'pentadiag_5x5_3rhs' );
	AB = bandMatrix( 7, 5, [
		[ 4, 0, 6.0 ],
		[ 5, 0, -2.0 ],
		[ 6, 0, 1.0 ],
		[ 3, 1, -2.0 ],
		[ 4, 1, 6.0 ],
		[ 5, 1, -2.0 ],
		[ 6, 1, 1.0 ],
		[ 2, 2, 1.0 ],
		[ 3, 2, -2.0 ],
		[ 4, 2, 6.0 ],
		[ 5, 2, -2.0 ],
		[ 6, 2, 1.0 ],
		[ 2, 3, 1.0 ],
		[ 3, 3, -2.0 ],
		[ 4, 3, 6.0 ],
		[ 5, 3, -2.0 ],
		[ 2, 4, 1.0 ],
		[ 3, 4, -2.0 ],
		[ 4, 4, 6.0 ]
	]);
	B = new Float64Array( 15 );
	B[ 0 ] = 1.0;
	B[ 5 + 2 ] = 1.0;
	B[ 10 ] = 1.0;
	B[ 11 ] = 2.0;
	B[ 12 ] = 3.0;
	B[ 13 ] = 4.0;
	B[ 14 ] = 5.0;
	IPIV = new Int32Array( 5 );
	info = dgbsv( 5, 2, 2, 3, AB, 1, 7, 0, IPIV, 1, 0, B, 1, 5, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( toArray( B ), tc.x, 1e-14, 'x' );
});
