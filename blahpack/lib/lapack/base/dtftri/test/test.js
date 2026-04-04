/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dtftri = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dtftri.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
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
	return fixture.find( function find( t ) {
		return t.name === name;
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
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual ); // eslint-disable-line max-len
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
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' ); // eslint-disable-line max-len
	}
}

/**
* Runs a standard test case.
*
* @private
* @param {string} name - test case name
* @param {string} transr - transpose operation
* @param {string} uplo - matrix triangle
* @param {string} diag - diagonal type
* @param {NonNegativeInteger} N - matrix order
*/
function runTest( name, transr, uplo, diag, N ) {
	var expected;
	var actual;
	var info;
	var tc;
	var a;
	var i;

	tc = findCase( name );
	a = new Float64Array( tc.input.length );
	for ( i = 0; i < tc.input.length; i++ ) {
		a[ i ] = tc.input[ i ];
	}

	info = dtftri( transr, uplo, diag, N, a, 1, 0 );
	assert.equal( info, tc.info, 'info' );

	if ( tc.a ) {
		expected = tc.a;
		actual = [];
		for ( i = 0; i < expected.length; i++ ) {
			actual.push( a[ i ] );
		}
		assertArrayClose( actual, expected, 1e-14, 'a' );
	}
}

/**
* Runs a singular matrix test case.
*
* @private
* @param {string} name - test case name
* @param {string} transr - transpose operation
* @param {string} uplo - matrix triangle
* @param {string} diag - diagonal type
* @param {NonNegativeInteger} N - matrix order
*/
function runSingularTest( name, transr, uplo, diag, N ) {
	var info;
	var tc;
	var a;
	var i;

	tc = findCase( name );
	a = new Float64Array( tc.input.length );
	for ( i = 0; i < tc.input.length; i++ ) {
		a[ i ] = tc.input[ i ];
	}

	info = dtftri( transr, uplo, diag, N, a, 1, 0 );
	assert.equal( info, tc.info, 'info should indicate singularity' ); // eslint-disable-line max-len
}


// TESTS //

test( 'dtftri: lower_odd_normal_nonunit', function t() {
	runTest( 'lower_odd_normal_nonunit', 'no-transpose', 'lower', 'non-unit', 3 ); // eslint-disable-line max-len
});

test( 'dtftri: upper_odd_normal_nonunit', function t() {
	runTest( 'upper_odd_normal_nonunit', 'no-transpose', 'upper', 'non-unit', 3 ); // eslint-disable-line max-len
});

test( 'dtftri: lower_odd_trans_nonunit', function t() {
	runTest( 'lower_odd_trans_nonunit', 'transpose', 'lower', 'non-unit', 3 ); // eslint-disable-line max-len
});

test( 'dtftri: upper_odd_trans_nonunit', function t() {
	runTest( 'upper_odd_trans_nonunit', 'transpose', 'upper', 'non-unit', 3 ); // eslint-disable-line max-len
});

test( 'dtftri: lower_even_normal_nonunit', function t() {
	runTest( 'lower_even_normal_nonunit', 'no-transpose', 'lower', 'non-unit', 4 ); // eslint-disable-line max-len
});

test( 'dtftri: upper_even_normal_nonunit', function t() {
	runTest( 'upper_even_normal_nonunit', 'no-transpose', 'upper', 'non-unit', 4 ); // eslint-disable-line max-len
});

test( 'dtftri: lower_even_trans_nonunit', function t() {
	runTest( 'lower_even_trans_nonunit', 'transpose', 'lower', 'non-unit', 4 ); // eslint-disable-line max-len
});

test( 'dtftri: upper_even_trans_nonunit', function t() {
	runTest( 'upper_even_trans_nonunit', 'transpose', 'upper', 'non-unit', 4 ); // eslint-disable-line max-len
});

test( 'dtftri: lower_odd_normal_unit', function t() {
	runTest( 'lower_odd_normal_unit', 'no-transpose', 'lower', 'unit', 3 ); // eslint-disable-line max-len
});

test( 'dtftri: upper_odd_normal_unit', function t() {
	runTest( 'upper_odd_normal_unit', 'no-transpose', 'upper', 'unit', 3 ); // eslint-disable-line max-len
});

test( 'dtftri: lower_odd_trans_unit', function t() {
	runTest( 'lower_odd_trans_unit', 'transpose', 'lower', 'unit', 3 );
});

test( 'dtftri: upper_odd_trans_unit', function t() {
	runTest( 'upper_odd_trans_unit', 'transpose', 'upper', 'unit', 3 );
});

test( 'dtftri: lower_even_normal_unit', function t() {
	runTest( 'lower_even_normal_unit', 'no-transpose', 'lower', 'unit', 4 ); // eslint-disable-line max-len
});

test( 'dtftri: upper_even_normal_unit', function t() {
	runTest( 'upper_even_normal_unit', 'no-transpose', 'upper', 'unit', 4 ); // eslint-disable-line max-len
});

test( 'dtftri: lower_even_trans_unit', function t() {
	runTest( 'lower_even_trans_unit', 'transpose', 'lower', 'unit', 4 );
});

test( 'dtftri: upper_even_trans_unit', function t() {
	runTest( 'upper_even_trans_unit', 'transpose', 'upper', 'unit', 4 );
});

test( 'dtftri: n_zero', function t() {
	var info;
	var a;

	a = new Float64Array( 0 );
	info = dtftri( 'no-transpose', 'lower', 'non-unit', 0, a, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, 0, 'info should be 0 for N=0' );
});

test( 'dtftri: n_one_nonunit', function t() {
	runTest( 'n_one_nonunit', 'no-transpose', 'lower', 'non-unit', 1 ); // eslint-disable-line max-len
});

test( 'dtftri: n_one_unit', function t() {
	runTest( 'n_one_unit', 'no-transpose', 'lower', 'unit', 1 );
});

test( 'dtftri: lower_5_normal_nonunit', function t() {
	runTest( 'lower_5_normal_nonunit', 'no-transpose', 'lower', 'non-unit', 5 ); // eslint-disable-line max-len
});

test( 'dtftri: upper_5_trans_nonunit', function t() {
	runTest( 'upper_5_trans_nonunit', 'transpose', 'upper', 'non-unit', 5 ); // eslint-disable-line max-len
});

test( 'dtftri: singular', function t() {
	runSingularTest( 'singular', 'no-transpose', 'lower', 'non-unit', 3 ); // eslint-disable-line max-len
});

test( 'dtftri: ndarray offset and stride', function t() {
	var expected;
	var actual;
	var info;
	var tc;
	var a;
	var i;

	tc = findCase( 'lower_odd_normal_nonunit' );
	a = new Float64Array( tc.input.length + 2 );
	a[ 0 ] = 999.0;
	for ( i = 0; i < tc.input.length; i++ ) {
		a[ i + 1 ] = tc.input[ i ];
	}
	a[ tc.input.length + 1 ] = 999.0;

	info = dtftri( 'no-transpose', 'lower', 'non-unit', 3, a, 1, 1 ); // eslint-disable-line max-len
	assert.equal( info, 0, 'info' );

	expected = tc.a;
	actual = [];
	for ( i = 0; i < expected.length; i++ ) {
		actual.push( a[ i + 1 ] );
	}
	assertArrayClose( actual, expected, 1e-14, 'a with offset' );
	assert.equal( a[ 0 ], 999.0, 'prefix unchanged' );
	assert.equal( a[ tc.input.length + 1 ], 999.0, 'suffix unchanged' ); // eslint-disable-line max-len
});
