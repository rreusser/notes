/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';


// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dsbmv = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dsbmv.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
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


// TESTS //

// 4x4 symmetric matrix with K=1 (tridiagonal):
//   [ 1  2  0  0 ]
//   [ 2  3  4  0 ]
//   [ 0  4  5  6 ]
//   [ 0  0  6  7 ]

// Upper band storage (LDA=2): row 0 = superdiag, row 1 = diag
// Col 0: row1=1
// Col 1: row0=2, row1=3
// Col 2: row0=4, row1=5
// Col 3: row0=6, row1=7
/**
* UpperBandA.
*
* @private
* @returns {*} result
*/
function upperBandA() {
	var a = new Float64Array( 8 );

	// Column-major: a[ row + col*2 ]
	a[ 1 + 0 * 2 ] = 1.0; // col0: diag

	a[ 0 + 1 * 2 ] = 2.0; // col1: super
	a[ 1 + 1 * 2 ] = 3.0; // col1: diag

	a[ 0 + 2 * 2 ] = 4.0; // col2: super
	a[ 1 + 2 * 2 ] = 5.0; // col2: diag

	a[ 0 + 3 * 2 ] = 6.0; // col3: super
	a[ 1 + 3 * 2 ] = 7.0; // col3: diag

	return a;
}

// Lower band storage (LDA=2): row 0 = diag, row 1 = subdiag
// Col 0: row0=1, row1=2
// Col 1: row0=3, row1=4
// Col 2: row0=5, row1=6
// Col 3: row0=7
/**
* LowerBandA.
*
* @private
* @returns {*} result
*/
function lowerBandA() {
	var a = new Float64Array( 8 );

	// Column-major: a[ row + col*2 ]
	a[ 0 + 0 * 2 ] = 1.0; // col0: diag
	a[ 1 + 0 * 2 ] = 2.0; // col0: sub

	a[ 0 + 1 * 2 ] = 3.0; // col1: diag
	a[ 1 + 1 * 2 ] = 4.0; // col1: sub

	a[ 0 + 2 * 2 ] = 5.0; // col2: diag
	a[ 1 + 2 * 2 ] = 6.0; // col2: sub

	a[ 0 + 3 * 2 ] = 7.0; // col3: diag

	return a;
}

test( 'dsbmv: upper_basic', function t() {
	var tc = findCase( 'upper_basic' );
	var a = upperBandA();
	var x = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
	var y = new Float64Array( 4 );

	dsbmv( 'upper', 4, 1, 1.0, a, 1, 2, 0, x, 1, 0, 0.0, y, 1, 0 );
	assertArrayClose( y, tc.y, 1e-14, 'y' );
});

test( 'dsbmv: lower_basic', function t() {
	var tc = findCase( 'lower_basic' );
	var a = lowerBandA();
	var x = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
	var y = new Float64Array( 4 );

	dsbmv( 'lower', 4, 1, 1.0, a, 1, 2, 0, x, 1, 0, 0.0, y, 1, 0 );
	assertArrayClose( y, tc.y, 1e-14, 'y' );
});

test( 'dsbmv: alpha_beta', function t() {
	var tc = findCase( 'alpha_beta' );
	var a = upperBandA();
	var x = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
	var y = new Float64Array( [ 10.0, 20.0, 30.0, 40.0 ] );

	dsbmv( 'upper', 4, 1, 2.0, a, 1, 2, 0, x, 1, 0, 0.5, y, 1, 0 );
	assertArrayClose( y, tc.y, 1e-14, 'y' );
});

test( 'dsbmv: n_zero', function t() {
	var tc = findCase( 'n_zero' );
	var y = new Float64Array( [ 99.0 ] );
	var a = new Float64Array( 2 );
	var x = new Float64Array( 1 );

	dsbmv( 'upper', 0, 1, 1.0, a, 1, 2, 0, x, 1, 0, 0.0, y, 1, 0 );
	assertArrayClose( y, tc.y, 1e-14, 'y' );
});

test( 'dsbmv: alpha_zero', function t() {
	var a = upperBandA();
	var x = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
	var y = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );

	dsbmv( 'upper', 4, 1, 0.0, a, 1, 2, 0, x, 1, 0, 2.0, y, 1, 0 );
	assertArrayClose( y, [ 2.0, 4.0, 6.0, 8.0 ], 1e-14, 'y' );
});

test( 'dsbmv: stride', function t() {
	var tc = findCase( 'stride' );
	var a = upperBandA();
	var x = new Float64Array( [ 1.0, 0.0, 2.0, 0.0, 3.0, 0.0, 4.0, 0.0 ] );
	var y = new Float64Array( 8 );

	dsbmv( 'upper', 4, 1, 1.0, a, 1, 2, 0, x, 2, 0, 0.0, y, 2, 0 );
	assertArrayClose( y, tc.y, 1e-14, 'y' );
});
