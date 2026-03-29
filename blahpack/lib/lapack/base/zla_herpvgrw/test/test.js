
'use strict';

// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var zla_herpvgrw = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zla_herpvgrw.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

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
	for ( i = 0; i < ipiv.length; i++ ) {
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
* @param {string} uplo - `upper` or `lower`
*/
function runCase( name, uplo ) {
	var result;
	var WORK;
	var IPIV;
	var tc;
	var AF;
	var N;
	var A;

	tc = findCase( name );
	N = tc.N;
	A = new Complex128Array( new Float64Array( tc.A ) );
	AF = new Complex128Array( new Float64Array( tc.AF ) );
	IPIV = convertIPIV( tc.IPIV );
	WORK = new Float64Array( 2 * N );

	result = zla_herpvgrw( uplo, N, tc.INFO, A, 1, N, 0, AF, 1, N, 0, IPIV, 1, 0, WORK, 1, 0 );
	assert.ok(Math.abs( result - tc.rpvgrw ) < 1e-14, name + ': rpvgrw expected ' + tc.rpvgrw + ', got ' + result);
}


// TESTS //

test( 'zla_herpvgrw is a function', function t() {
	assert.equal( typeof zla_herpvgrw, 'function' );
});

test( 'zla_herpvgrw: upper 4x4 with INFO=0', function t() {
	runCase( 'upper_4x4_info0', 'upper' );
});

test( 'zla_herpvgrw: lower 4x4 with INFO=0', function t() {
	runCase( 'lower_4x4_info0', 'lower' );
});

test( 'zla_herpvgrw: upper 1x1', function t() {
	var result;
	var WORK;
	var IPIV;
	var tc;
	var AF;
	var A;

	tc = findCase( 'upper_1x1' );
	A = new Complex128Array( [ 7.0, 0.0 ] );
	AF = new Complex128Array( [ 7.0, 0.0 ] );
	IPIV = new Int32Array( [ 0 ] );
	WORK = new Float64Array( 2 );

	result = zla_herpvgrw( 'upper', 1, tc.INFO, A, 1, 1, 0, AF, 1, 1, 0, IPIV, 1, 0, WORK, 1, 0 );
	assert.equal( result, tc.rpvgrw );
});

test( 'zla_herpvgrw: lower 1x1', function t() {
	var result;
	var WORK;
	var IPIV;
	var tc;
	var AF;
	var A;

	tc = findCase( 'lower_1x1' );
	A = new Complex128Array( [ 3.0, 0.0 ] );
	AF = new Complex128Array( [ 3.0, 0.0 ] );
	IPIV = new Int32Array( [ 0 ] );
	WORK = new Float64Array( 2 );

	result = zla_herpvgrw( 'lower', 1, tc.INFO, A, 1, 1, 0, AF, 1, 1, 0, IPIV, 1, 0, WORK, 1, 0 );
	assert.equal( result, tc.rpvgrw );
});

test( 'zla_herpvgrw: upper singular (INFO > 0)', function t() {
	runCase( 'upper_singular', 'upper' );
});

test( 'zla_herpvgrw: lower singular (INFO > 0)', function t() {
	runCase( 'lower_singular', 'lower' );
});

test( 'zla_herpvgrw: upper with 2x2 pivots', function t() {
	runCase( 'upper_2x2_pivot', 'upper' );
});

test( 'zla_herpvgrw: lower with 2x2 pivots', function t() {
	runCase( 'lower_2x2_pivot', 'lower' );
});

test( 'zla_herpvgrw: upper 6x6 mixed pivots', function t() {
	runCase( 'upper_6x6_mixed', 'upper' );
});

test( 'zla_herpvgrw: lower 6x6 mixed pivots', function t() {
	runCase( 'lower_6x6_mixed', 'lower' );
});

test( 'zla_herpvgrw: lower with nontrivial 1x1 pivot swaps', function t() {
	runCase( 'lower_1x1_swap', 'lower' );
});

test( 'zla_herpvgrw: lower with nontrivial 1x1 pivot swaps (5x5)', function t() {
	runCase( 'lower_1x1_swap_5x5', 'lower' );
});

test( 'zla_herpvgrw: N=0 returns 1.0', function t() {
	var result;
	var WORK;
	var IPIV;
	var AF;
	var A;

	A = new Complex128Array( 0 );
	AF = new Complex128Array( 0 );
	IPIV = new Int32Array( 0 );
	WORK = new Float64Array( 0 );

	result = zla_herpvgrw( 'upper', 0, 0, A, 1, 1, 0, AF, 1, 1, 0, IPIV, 1, 0, WORK, 1, 0 );
	assert.equal( result, 1.0, 'N=0 upper returns 1.0' );

	result = zla_herpvgrw( 'lower', 0, 0, A, 1, 1, 0, AF, 1, 1, 0, IPIV, 1, 0, WORK, 1, 0 );
	assert.equal( result, 1.0, 'N=0 lower returns 1.0' );
});

test( 'zla_herpvgrw: identity matrix returns 1.0', function t() {
	var result;
	var WORK;
	var IPIV;
	var AF;
	var A;

	// 3x3 identity matrix (upper storage): A = AF = I, IPIV = [0,1,2]
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

	result = zla_herpvgrw( 'upper', 3, 0, A, 1, 3, 0, AF, 1, 3, 0, IPIV, 1, 0, WORK, 1, 0 );
	assert.equal( result, 1.0, 'identity upper' );

	result = zla_herpvgrw( 'lower', 3, 0, A, 1, 3, 0, AF, 1, 3, 0, IPIV, 1, 0, WORK, 1, 0 );
	assert.equal( result, 1.0, 'identity lower' );
});

test( 'zla_herpvgrw: verifies WORK array contents (upper)', function t() {
	var WORK;
	var IPIV;
	var tc;
	var AF;
	var N;
	var A;
	var i;

	tc = findCase( 'upper_4x4_info0' );
	N = tc.N;
	A = new Complex128Array( new Float64Array( tc.A ) );
	AF = new Complex128Array( new Float64Array( tc.AF ) );
	IPIV = convertIPIV( tc.IPIV );
	WORK = new Float64Array( 2 * N );

	zla_herpvgrw( 'upper', N, tc.INFO, A, 1, N, 0, AF, 1, N, 0, IPIV, 1, 0, WORK, 1, 0 );

	// Compare WORK contents to fixture
	for ( i = 0; i < 2 * N; i++ ) {
		assert.ok(Math.abs( WORK[ i ] - tc.WORK[ i ] ) < 1e-10, 'WORK[' + i + ']: expected ' + tc.WORK[ i ] + ', got ' + WORK[ i ]);
	}
});

test( 'zla_herpvgrw: verifies WORK array contents (lower)', function t() {
	var WORK;
	var IPIV;
	var tc;
	var AF;
	var N;
	var A;
	var i;

	tc = findCase( 'lower_4x4_info0' );
	N = tc.N;
	A = new Complex128Array( new Float64Array( tc.A ) );
	AF = new Complex128Array( new Float64Array( tc.AF ) );
	IPIV = convertIPIV( tc.IPIV );
	WORK = new Float64Array( 2 * N );

	zla_herpvgrw( 'lower', N, tc.INFO, A, 1, N, 0, AF, 1, N, 0, IPIV, 1, 0, WORK, 1, 0 );

	for ( i = 0; i < 2 * N; i++ ) {
		assert.ok(Math.abs( WORK[ i ] - tc.WORK[ i ] ) < 1e-10, 'WORK[' + i + ']: expected ' + tc.WORK[ i ] + ', got ' + WORK[ i ]);
	}
});

test( 'zla_herpvgrw: verifies WORK array contents (upper 2x2 pivot)', function t() {
	var WORK;
	var IPIV;
	var tc;
	var AF;
	var N;
	var A;
	var i;

	tc = findCase( 'upper_2x2_pivot' );
	N = tc.N;
	A = new Complex128Array( new Float64Array( tc.A ) );
	AF = new Complex128Array( new Float64Array( tc.AF ) );
	IPIV = convertIPIV( tc.IPIV );
	WORK = new Float64Array( 2 * N );

	zla_herpvgrw( 'upper', N, tc.INFO, A, 1, N, 0, AF, 1, N, 0, IPIV, 1, 0, WORK, 1, 0 );

	for ( i = 0; i < 2 * N; i++ ) {
		assert.ok(Math.abs( WORK[ i ] - tc.WORK[ i ] ) < 1e-10, 'WORK[' + i + ']: expected ' + tc.WORK[ i ] + ', got ' + WORK[ i ]);
	}
});

test( 'zla_herpvgrw: verifies WORK array contents (lower 2x2 pivot)', function t() {
	var WORK;
	var IPIV;
	var tc;
	var AF;
	var N;
	var A;
	var i;

	tc = findCase( 'lower_2x2_pivot' );
	N = tc.N;
	A = new Complex128Array( new Float64Array( tc.A ) );
	AF = new Complex128Array( new Float64Array( tc.AF ) );
	IPIV = convertIPIV( tc.IPIV );
	WORK = new Float64Array( 2 * N );

	zla_herpvgrw( 'lower', N, tc.INFO, A, 1, N, 0, AF, 1, N, 0, IPIV, 1, 0, WORK, 1, 0 );

	for ( i = 0; i < 2 * N; i++ ) {
		assert.ok(Math.abs( WORK[ i ] - tc.WORK[ i ] ) < 1e-10, 'WORK[' + i + ']: expected ' + tc.WORK[ i ] + ', got ' + WORK[ i ]);
	}
});

test( 'zla_herpvgrw: lower 1x1 swap WORK contents', function t() {
	var WORK;
	var IPIV;
	var tc;
	var AF;
	var N;
	var A;
	var i;

	tc = findCase( 'lower_1x1_swap' );
	N = tc.N;
	A = new Complex128Array( new Float64Array( tc.A ) );
	AF = new Complex128Array( new Float64Array( tc.AF ) );
	IPIV = convertIPIV( tc.IPIV );
	WORK = new Float64Array( 2 * N );

	zla_herpvgrw( 'lower', N, tc.INFO, A, 1, N, 0, AF, 1, N, 0, IPIV, 1, 0, WORK, 1, 0 );

	for ( i = 0; i < 2 * N; i++ ) {
		assert.ok(Math.abs( WORK[ i ] - tc.WORK[ i ] ) < 1e-10, 'WORK[' + i + ']: expected ' + tc.WORK[ i ] + ', got ' + WORK[ i ]);
	}
});
