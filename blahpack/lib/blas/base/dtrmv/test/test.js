/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';


// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dtrmv = require( './../lib/base.js' );
var ndarray = require( './../lib/ndarray.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dtrmv.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
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

// Upper triangular 3x3 (col-major): [2 3 4; 0 5 6; 0 0 7]
var AU = new Float64Array( [ 2, 0, 0, 3, 5, 0, 4, 6, 7 ] );

// Lower triangular 3x3 (col-major): [2 0 0; 3 5 0; 4 6 7]
var AL = new Float64Array( [ 2, 3, 4, 0, 5, 6, 0, 0, 7 ] );


// TESTS //

test( 'dtrmv: upper, no-transpose, non-unit diag', function t() {
	var tc = findCase( 'upper_n_nonunit' );
	var x = new Float64Array( [ 1, 2, 3 ] );
	dtrmv( 'upper', 'no-transpose', 'non-unit', 3, AU, 1, 3, 0, x, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'dtrmv: lower, no-transpose, non-unit diag', function t() {
	var tc = findCase( 'lower_n_nonunit' );
	var x = new Float64Array( [ 1, 2, 3 ] );
	dtrmv( 'lower', 'no-transpose', 'non-unit', 3, AL, 1, 3, 0, x, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'dtrmv: upper, transpose, non-unit diag', function t() {
	var tc = findCase( 'upper_t_nonunit' );
	var x = new Float64Array( [ 1, 2, 3 ] );
	dtrmv( 'upper', 'transpose', 'non-unit', 3, AU, 1, 3, 0, x, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'dtrmv: lower, transpose, non-unit diag', function t() {
	var tc = findCase( 'lower_t_nonunit' );
	var x = new Float64Array( [ 1, 2, 3 ] );
	dtrmv( 'lower', 'transpose', 'non-unit', 3, AL, 1, 3, 0, x, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'dtrmv: upper, no-transpose, unit diag', function t() {
	var Aunit = new Float64Array( [ 99, 0, 0, 3, 99, 0, 4, 6, 99 ] );
	var tc = findCase( 'upper_n_unit' );
	var x = new Float64Array( [ 1, 2, 3 ] );
	dtrmv( 'upper', 'no-transpose', 'unit', 3, Aunit, 1, 3, 0, x, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'dtrmv: lower, transpose, unit diag', function t() {
	var Aunit = new Float64Array( [ 99, 3, 4, 0, 99, 6, 0, 0, 99 ] );
	var tc = findCase( 'lower_t_unit' );
	var x = new Float64Array( [ 1, 2, 3 ] );
	dtrmv( 'lower', 'transpose', 'unit', 3, Aunit, 1, 3, 0, x, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'dtrmv: N=0 quick return', function t() {
	var x = new Float64Array( [ 99 ] );
	dtrmv( 'upper', 'no-transpose', 'non-unit', 0, new Float64Array( 0 ), 1, 1, 0, x, 1, 0 ); // eslint-disable-line max-len
	assertArrayClose( x, new Float64Array( [ 99 ] ), 1e-14, 'x' );
});

test( 'dtrmv: N=1', function t() {
	var tc = findCase( 'n_one' );
	var x = new Float64Array( [ 3 ] );
	dtrmv( 'upper', 'no-transpose', 'non-unit', 1, new Float64Array( [ 5 ] ), 1, 1, 0, x, 1, 0 ); // eslint-disable-line max-len
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'dtrmv: non-unit stride incx=2', function t() {
	var tc = findCase( 'stride' );
	var x = new Float64Array( [ 1, 0, 2, 0, 3, 0 ] );
	dtrmv( 'upper', 'no-transpose', 'non-unit', 3, AU, 1, 3, 0, x, 2, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'dtrmv: negative stride incx=-1', function t() {
	var tc = findCase( 'neg_stride' );
	var x = new Float64Array( [ 1, 2, 3 ] );

	// With negative stride, start from last element
	dtrmv( 'lower', 'no-transpose', 'non-unit', 3, AL, 1, 3, 0, x, -1, 2 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});


// NDARRAY VALIDATION TESTS //

test( 'ndarray: throws TypeError for invalid uplo', function t() {
	var x = new Float64Array( [ 1, 2, 3 ] );
	assert.throws( function f() {
		ndarray( 'foo', 'no-transpose', 'non-unit', 3, AU, 1, 3, 0, x, 1, 0 );
	}, TypeError );
});

test( 'ndarray: throws TypeError for invalid trans', function t() {
	var x = new Float64Array( [ 1, 2, 3 ] );
	assert.throws( function f() {
		ndarray( 'upper', 'foo', 'non-unit', 3, AU, 1, 3, 0, x, 1, 0 );
	}, TypeError );
});

test( 'ndarray: throws TypeError for invalid diag', function t() {
	var x = new Float64Array( [ 1, 2, 3 ] );
	assert.throws( function f() {
		ndarray( 'upper', 'no-transpose', 'foo', 3, AU, 1, 3, 0, x, 1, 0 );
	}, TypeError );
});

test( 'ndarray: throws RangeError for negative N', function t() {
	var x = new Float64Array( [ 1, 2, 3 ] );
	assert.throws( function f() {
		ndarray( 'upper', 'no-transpose', 'non-unit', -1, AU, 1, 3, 0, x, 1, 0 );
	}, RangeError );
});

test( 'ndarray: throws RangeError for strideX=0', function t() {
	var x = new Float64Array( [ 1, 2, 3 ] );
	assert.throws( function f() {
		ndarray( 'upper', 'no-transpose', 'non-unit', 3, AU, 1, 3, 0, x, 0, 0 );
	}, RangeError );
});

test( 'ndarray: N=0 early return', function t() {
	var out;
	var x;

	x = new Float64Array( [ 99 ] );
	out = ndarray( 'upper', 'no-transpose', 'non-unit', 0, new Float64Array( 0 ), 1, 1, 0, x, 1, 0 ); // eslint-disable-line max-len
	assert.equal( out, x );
	assert.equal( x[ 0 ], 99 );
});
