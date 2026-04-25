/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dtftri = require( './../lib/ndarray.js' );


// FIXTURES //

var lowerOddNormalNonunit = require( './fixtures/lower_odd_normal_nonunit.json' );
var upperOddNormalNonunit = require( './fixtures/upper_odd_normal_nonunit.json' );
var lowerOddTransNonunit = require( './fixtures/lower_odd_trans_nonunit.json' );
var upperOddTransNonunit = require( './fixtures/upper_odd_trans_nonunit.json' );
var lowerEvenNormalNonunit = require( './fixtures/lower_even_normal_nonunit.json' );
var upperEvenNormalNonunit = require( './fixtures/upper_even_normal_nonunit.json' );
var lowerEvenTransNonunit = require( './fixtures/lower_even_trans_nonunit.json' );
var upperEvenTransNonunit = require( './fixtures/upper_even_trans_nonunit.json' );
var lowerOddNormalUnit = require( './fixtures/lower_odd_normal_unit.json' );
var upperOddNormalUnit = require( './fixtures/upper_odd_normal_unit.json' );
var lowerOddTransUnit = require( './fixtures/lower_odd_trans_unit.json' );
var upperOddTransUnit = require( './fixtures/upper_odd_trans_unit.json' );
var lowerEvenNormalUnit = require( './fixtures/lower_even_normal_unit.json' );
var upperEvenNormalUnit = require( './fixtures/upper_even_normal_unit.json' );
var lowerEvenTransUnit = require( './fixtures/lower_even_trans_unit.json' );
var upperEvenTransUnit = require( './fixtures/upper_even_trans_unit.json' );
var nOneNonunit = require( './fixtures/n_one_nonunit.json' );
var nOneUnit = require( './fixtures/n_one_unit.json' );
var lower5NormalNonunit = require( './fixtures/lower_5_normal_nonunit.json' );
var upper5TransNonunit = require( './fixtures/upper_5_trans_nonunit.json' );
var singular = require( './fixtures/singular.json' );

var fixtures = {
	'lower_odd_normal_nonunit': lowerOddNormalNonunit,
	'upper_odd_normal_nonunit': upperOddNormalNonunit,
	'lower_odd_trans_nonunit': lowerOddTransNonunit,
	'upper_odd_trans_nonunit': upperOddTransNonunit,
	'lower_even_normal_nonunit': lowerEvenNormalNonunit,
	'upper_even_normal_nonunit': upperEvenNormalNonunit,
	'lower_even_trans_nonunit': lowerEvenTransNonunit,
	'upper_even_trans_nonunit': upperEvenTransNonunit,
	'lower_odd_normal_unit': lowerOddNormalUnit,
	'upper_odd_normal_unit': upperOddNormalUnit,
	'lower_odd_trans_unit': lowerOddTransUnit,
	'upper_odd_trans_unit': upperOddTransUnit,
	'lower_even_normal_unit': lowerEvenNormalUnit,
	'upper_even_normal_unit': upperEvenNormalUnit,
	'lower_even_trans_unit': lowerEvenTransUnit,
	'upper_even_trans_unit': upperEvenTransUnit,
	'n_one_nonunit': nOneNonunit,
	'n_one_unit': nOneUnit,
	'lower_5_normal_nonunit': lower5NormalNonunit,
	'upper_5_trans_nonunit': upper5TransNonunit,
	'singular': singular
};


// FUNCTIONS //

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

	tc = fixtures[ name ];
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

	tc = fixtures[ name ];
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

	tc = lowerOddNormalNonunit;
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
