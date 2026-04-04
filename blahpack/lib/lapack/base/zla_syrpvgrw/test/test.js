
'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );
var zla_syrpvgrw = require( './../lib/base.js' );


// FIXTURES //

var upper4x4Info0 = require( './fixtures/upper_4x4_info0.json' );
var lower4x4Info0 = require( './fixtures/lower_4x4_info0.json' );
var upper1x1 = require( './fixtures/upper_1x1.json' );
var lower1x1 = require( './fixtures/lower_1x1.json' );
var upperSingular = require( './fixtures/upper_singular.json' );
var lowerSingular = require( './fixtures/lower_singular.json' );
var upper2x2Pivot = require( './fixtures/upper_2x2_pivot.json' );
var lower2x2Pivot = require( './fixtures/lower_2x2_pivot.json' );
var upper6x6Mixed = require( './fixtures/upper_6x6_mixed.json' );
var lower6x6Mixed = require( './fixtures/lower_6x6_mixed.json' );
var lower1x1Swap = require( './fixtures/lower_1x1_swap.json' );
var lower1x1Swap5x5 = require( './fixtures/lower_1x1_swap_5x5.json' );

var fixtures = {
	'upper_4x4_info0': upper4x4Info0,
	'lower_4x4_info0': lower4x4Info0,
	'upper_1x1': upper1x1,
	'lower_1x1': lower1x1,
	'upper_singular': upperSingular,
	'lower_singular': lowerSingular,
	'upper_2x2_pivot': upper2x2Pivot,
	'lower_2x2_pivot': lower2x2Pivot,
	'upper_6x6_mixed': upper6x6Mixed,
	'lower_6x6_mixed': lower6x6Mixed,
	'lower_1x1_swap': lower1x1Swap,
	'lower_1x1_swap_5x5': lower1x1Swap5x5
};


// FUNCTIONS //

/**
* Convert Fortran 1-based IPIV to JS 0-based IPIV.
* Positive values: subtract 1 (1-based to 0-based).
* Negative values: stay the same (Fortran -p maps to JS ~(p-1) = -p).
*
* @private
* @param {Array} ipiv - Fortran 1-based IPIV array
* @returns {Int32Array} JS 0-based IPIV array
*/
function convertIPIV( ipiv ) {
	var out = new Int32Array( ipiv.length );
	var i;
	for ( i = 0; i < ipiv.length; i += 1 ) {
		if ( ipiv[ i ] >= 0 ) {
			out[ i ] = ipiv[ i ] - 1;
		} else {
			out[ i ] = ipiv[ i ];
		}
	}
	return out;
}

/**
* Run a single test case from the fixture.
*
* @private
* @param {string} name - test case name
* @param {string} uplo - 'upper' or 'lower'
*/
function runCase( name, uplo ) {
	var result;
	var WORK;
	var IPIV;
	var tc;
	var AF;
	var N;
	var A;

	tc = fixtures[ name ];
	N = tc.N;
	A = new Complex128Array( tc.A );
	AF = new Complex128Array( tc.AF );
	IPIV = convertIPIV( tc.IPIV );
	WORK = new Float64Array( 2 * N );

	result = zla_syrpvgrw( uplo, N, tc.INFO, A, 1, N, 0, AF, 1, N, 0, IPIV, 1, 0, WORK, 1, 0 );
	assert.ok(Math.abs( result - tc.rpvgrw ) < 1e-10, name + ': expected rpvgrw=' + tc.rpvgrw + ', got ' + result);
}

/**
* Run a test case and also verify WORK array contents.
*
* @private
* @param {string} name - test case name
* @param {string} uplo - 'upper' or 'lower'
*/
function runCaseWithWORK( name, uplo ) {
	var result;
	var WORK;
	var IPIV;
	var tc;
	var AF;
	var N;
	var A;
	var i;

	tc = fixtures[ name ];
	N = tc.N;
	A = new Complex128Array( tc.A );
	AF = new Complex128Array( tc.AF );
	IPIV = convertIPIV( tc.IPIV );
	WORK = new Float64Array( 2 * N );

	result = zla_syrpvgrw( uplo, N, tc.INFO, A, 1, N, 0, AF, 1, N, 0, IPIV, 1, 0, WORK, 1, 0 );
	assert.ok(Math.abs( result - tc.rpvgrw ) < 1e-10, name + ': expected rpvgrw=' + tc.rpvgrw + ', got ' + result);

	// Compare WORK contents to fixture
	for ( i = 0; i < 2 * N; i += 1 ) {
		assert.ok(Math.abs( WORK[ i ] - tc.WORK[ i ] ) < 1e-10, 'WORK[' + i + ']: expected ' + tc.WORK[ i ] + ', got ' + WORK[ i ]);
	}
}


// TESTS //

test( 'zla_syrpvgrw is a function', function t() {
	assert.equal( typeof zla_syrpvgrw, 'function' );
});

test( 'zla_syrpvgrw: upper 4x4 with INFO=0', function t() {
	runCase( 'upper_4x4_info0', 'upper' );
});

test( 'zla_syrpvgrw: lower 4x4 with INFO=0', function t() {
	runCase( 'lower_4x4_info0', 'lower' );
});

test( 'zla_syrpvgrw: upper 1x1', function t() {
	var result;
	var WORK;
	var IPIV;
	var tc;
	var AF;
	var A;

	tc = upper1x1;
	A = new Complex128Array( tc.A );
	AF = new Complex128Array( tc.AF );
	IPIV = new Int32Array( [ 0 ] );
	WORK = new Float64Array( 2 );

	result = zla_syrpvgrw( 'upper', 1, tc.INFO, A, 1, 1, 0, AF, 1, 1, 0, IPIV, 1, 0, WORK, 1, 0 );
	assert.equal( result, tc.rpvgrw );
});

test( 'zla_syrpvgrw: lower 1x1', function t() {
	var result;
	var WORK;
	var IPIV;
	var tc;
	var AF;
	var A;

	tc = lower1x1;
	A = new Complex128Array( tc.A );
	AF = new Complex128Array( tc.AF );
	IPIV = new Int32Array( [ 0 ] );
	WORK = new Float64Array( 2 );

	result = zla_syrpvgrw( 'lower', 1, tc.INFO, A, 1, 1, 0, AF, 1, 1, 0, IPIV, 1, 0, WORK, 1, 0 );
	assert.equal( result, tc.rpvgrw );
});

test( 'zla_syrpvgrw: upper singular (INFO=0, non-singular factorization)', function t() {
	runCase( 'upper_singular', 'upper' );
});

test( 'zla_syrpvgrw: lower singular (INFO=0, non-singular factorization)', function t() {
	runCase( 'lower_singular', 'lower' );
});

test( 'zla_syrpvgrw: upper with 2x2 pivots', function t() {
	runCase( 'upper_2x2_pivot', 'upper' );
});

test( 'zla_syrpvgrw: lower with 2x2 pivots', function t() {
	runCase( 'lower_2x2_pivot', 'lower' );
});

test( 'zla_syrpvgrw: upper 6x6 mixed pivots', function t() {
	runCase( 'upper_6x6_mixed', 'upper' );
});

test( 'zla_syrpvgrw: lower 6x6 mixed pivots', function t() {
	runCase( 'lower_6x6_mixed', 'lower' );
});

test( 'zla_syrpvgrw: lower with nontrivial 1x1 pivot swaps', function t() {
	runCase( 'lower_1x1_swap', 'lower' );
});

test( 'zla_syrpvgrw: lower with nontrivial 1x1 pivot swaps (5x5)', function t() {
	runCase( 'lower_1x1_swap_5x5', 'lower' );
});

test( 'zla_syrpvgrw: N=0 returns 1.0', function t() {
	var result;
	var WORK;
	var IPIV;
	var AF;
	var A;

	A = new Complex128Array( 0 );
	AF = new Complex128Array( 0 );
	IPIV = new Int32Array( 0 );
	WORK = new Float64Array( 0 );

	result = zla_syrpvgrw( 'upper', 0, 0, A, 1, 1, 0, AF, 1, 1, 0, IPIV, 1, 0, WORK, 1, 0 );
	assert.equal( result, 1.0, 'N=0 upper returns 1.0' );

	result = zla_syrpvgrw( 'lower', 0, 0, A, 1, 1, 0, AF, 1, 1, 0, IPIV, 1, 0, WORK, 1, 0 );
	assert.equal( result, 1.0, 'N=0 lower returns 1.0' );
});

test( 'zla_syrpvgrw: identity matrix returns 1.0', function t() {
	var result;
	var WORK;
	var IPIV;
	var AF;
	var A;

	// 3x3 complex identity matrix (upper storage): A = AF = I, IPIV = [0,1,2]
	A = new Complex128Array([
		1,
		0,
		0,
		0,
		0,
		0,
		0,
		0,
		1,
		0,
		0,
		0,
		0,
		0,
		0,
		0,
		1,
		0
	]);
	AF = new Complex128Array([
		1,
		0,
		0,
		0,
		0,
		0,
		0,
		0,
		1,
		0,
		0,
		0,
		0,
		0,
		0,
		0,
		1,
		0
	]);
	IPIV = new Int32Array( [ 0, 1, 2 ] );
	WORK = new Float64Array( 6 );

	result = zla_syrpvgrw( 'upper', 3, 0, A, 1, 3, 0, AF, 1, 3, 0, IPIV, 1, 0, WORK, 1, 0 );
	assert.equal( result, 1.0, 'identity upper' );

	WORK = new Float64Array( 6 );
	result = zla_syrpvgrw( 'lower', 3, 0, A, 1, 3, 0, AF, 1, 3, 0, IPIV, 1, 0, WORK, 1, 0 );
	assert.equal( result, 1.0, 'identity lower' );
});

test( 'zla_syrpvgrw: verifies WORK array contents (upper)', function t() {
	runCaseWithWORK( 'upper_4x4_info0', 'upper' );
});

test( 'zla_syrpvgrw: verifies WORK array contents (lower)', function t() {
	runCaseWithWORK( 'lower_4x4_info0', 'lower' );
});

test( 'zla_syrpvgrw: verifies WORK array contents (upper 2x2 pivot)', function t() {
	runCaseWithWORK( 'upper_2x2_pivot', 'upper' );
});

test( 'zla_syrpvgrw: verifies WORK array contents (lower 2x2 pivot)', function t() {
	runCaseWithWORK( 'lower_2x2_pivot', 'lower' );
});

test( 'zla_syrpvgrw: lower 1x1 swap WORK contents', function t() {
	runCaseWithWORK( 'lower_1x1_swap', 'lower' );
});

test( 'zla_syrpvgrw: lower 1x1 swap 5x5 WORK contents', function t() {
	runCaseWithWORK( 'lower_1x1_swap_5x5', 'lower' );
});

test( 'zla_syrpvgrw: lower with INFO > 0 (singular factorization)', function t() {
	var result;
	var WORK;
	var IPIV;
	var AF;
	var A;

	// 3x3 lower matrix, simulate INFO=2 (singular at column 2)

	// A: complex symmetric lower
	A = new Complex128Array([
		4,
		1,
		0,
		0,
		0,
		0,
		2,
		0.5,
		1,
		0.3,
		0,
		0,
		1,
		0.2,
		0.5,
		0.1,
		3,
		0.8
	]);

	// AF: factored form (just use A for simplicity; the routine only reads magnitudes)
	AF = new Complex128Array([
		4,
		1,
		0,
		0,
		0,
		0,
		0.5,
		0.125,
		0,
		0,
		0,
		0,
		0.25,
		0.05,
		0.375,
		0.075,
		3,
		0.8
	]);
	IPIV = new Int32Array( [ 0, 1, 2 ] );
	WORK = new Float64Array( 6 );

	// INFO=2 means ncols=2, so only columns 0..1 are used in the rpvgrw computation
	result = zla_syrpvgrw( 'lower', 3, 2, A, 1, 3, 0, AF, 1, 3, 0, IPIV, 1, 0, WORK, 1, 0 );
	assert.ok( typeof result === 'number', 'returns a number' );
	assert.ok( result > 0.0, 'rpvgrw is positive' );
	assert.ok( result <= 1.0, 'rpvgrw is at most 1.0' );
});

test( 'zla_syrpvgrw: upper with INFO > 0', function t() {
	var result;
	var WORK;
	var IPIV;
	var AF;
	var A;

	// 3x3 upper matrix, INFO=2
	A = new Complex128Array([
		4,
		1,
		2,
		0.5,
		1,
		0.2,
		0,
		0,
		1,
		0.3,
		0.5,
		0.1,
		0,
		0,
		0,
		0,
		3,
		0.8
	]);
	AF = new Complex128Array([
		4,
		1,
		0.5,
		0.125,
		0.25,
		0.05,
		0,
		0,
		0,
		0,
		0.375,
		0.075,
		0,
		0,
		0,
		0,
		3,
		0.8
	]);
	IPIV = new Int32Array( [ 0, 1, 2 ] );
	WORK = new Float64Array( 6 );

	// INFO=2 means ncols=2 in the upper branch
	result = zla_syrpvgrw( 'upper', 3, 2, A, 1, 3, 0, AF, 1, 3, 0, IPIV, 1, 0, WORK, 1, 0 );
	assert.ok( typeof result === 'number', 'returns a number' );
	assert.ok( result > 0.0, 'rpvgrw is positive' );
	assert.ok( result <= 1.0, 'rpvgrw is at most 1.0' );
});
