

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dtfttr = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dtfttr.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

/**
* Runs a standard dtfttr fixture test with LDA = N.
*
* @private
* @param {string} name - fixture case name
* @param {string} transr - `'no-transpose'` or `'transpose'`
* @param {string} uplo - `'lower'` or `'upper'`
*/
function runFixtureTest( name, transr, uplo ) {
	var expected;
	var actual;
	var info;
	var arf;
	var tc;
	var n;
	var A;
	var i;

	tc = findCase( name );
	n = tc.n;
	arf = new Float64Array( tc.ARF );
	A = new Float64Array( n * n );
	info = dtfttr( transr, uplo, n, arf, 1, 0, A, 1, n, 0, n );

	assert.equal( info, tc.info, name + ' info' );

	expected = tc.A;
	actual = Array.from( A );
	assert.equal( actual.length, expected.length, name + ': length mismatch' );
	for ( i = 0; i < expected.length; i += 1 ) {
		assert.equal( actual[ i ], expected[ i ], name + ': A[' + i + '] expected ' + expected[ i ] + ', got ' + actual[ i ] );
	}
}


// TESTS //

test( 'dtfttr is a function', function t() {
	assert.equal( typeof dtfttr, 'function' );
});

test( 'dtfttr: N=0 quick return', function t() {
	var A = new Float64Array( [ -1.0 ] );
	var arf = new Float64Array( 1 );
	var info = dtfttr( 'no-transpose', 'lower', 0, arf, 1, 0, A, 1, 1, 0, 1 );
	assert.equal( info, 0, 'info' );
	// A should be unchanged
	assert.equal( A[ 0 ], -1.0, 'A unchanged' );
});

test( 'dtfttr: N=1 quick return', function t() {
	var tc = findCase( 'n1' );
	var arf = new Float64Array( [ 42.0 ] );
	var A = new Float64Array( 1 );
	var info = dtfttr( 'no-transpose', 'lower', 1, arf, 1, 0, A, 1, 1, 0, 1 );
	assert.equal( info, 0, 'info' );
	assert.equal( A[ 0 ], tc.a00, 'A[0,0]' );
});

// N=5 (odd) tests
test( 'dtfttr: N=5, normal, lower', function t() {
	runFixtureTest( 'n5_N_L', 'no-transpose', 'lower' );
});

test( 'dtfttr: N=5, normal, upper', function t() {
	runFixtureTest( 'n5_N_U', 'no-transpose', 'upper' );
});

test( 'dtfttr: N=5, transpose, lower', function t() {
	runFixtureTest( 'n5_T_L', 'transpose', 'lower' );
});

test( 'dtfttr: N=5, transpose, upper', function t() {
	runFixtureTest( 'n5_T_U', 'transpose', 'upper' );
});

// N=6 (even) tests
test( 'dtfttr: N=6, normal, lower', function t() {
	runFixtureTest( 'n6_N_L', 'no-transpose', 'lower' );
});

test( 'dtfttr: N=6, normal, upper', function t() {
	runFixtureTest( 'n6_N_U', 'no-transpose', 'upper' );
});

test( 'dtfttr: N=6, transpose, lower', function t() {
	runFixtureTest( 'n6_T_L', 'transpose', 'lower' );
});

test( 'dtfttr: N=6, transpose, upper', function t() {
	runFixtureTest( 'n6_T_U', 'transpose', 'upper' );
});

// N=7 (odd, larger) tests
test( 'dtfttr: N=7, normal, lower', function t() {
	runFixtureTest( 'n7_N_L', 'no-transpose', 'lower' );
});

test( 'dtfttr: N=7, normal, upper', function t() {
	runFixtureTest( 'n7_N_U', 'no-transpose', 'upper' );
});

test( 'dtfttr: N=7, transpose, lower', function t() {
	runFixtureTest( 'n7_T_L', 'transpose', 'lower' );
});

test( 'dtfttr: N=7, transpose, upper', function t() {
	runFixtureTest( 'n7_T_U', 'transpose', 'upper' );
});

// N=8 (even, larger) tests
test( 'dtfttr: N=8, normal, lower', function t() {
	runFixtureTest( 'n8_N_L', 'no-transpose', 'lower' );
});

test( 'dtfttr: N=8, normal, upper', function t() {
	runFixtureTest( 'n8_N_U', 'no-transpose', 'upper' );
});

test( 'dtfttr: N=8, transpose, lower', function t() {
	runFixtureTest( 'n8_T_L', 'transpose', 'lower' );
});

test( 'dtfttr: N=8, transpose, upper', function t() {
	runFixtureTest( 'n8_T_U', 'transpose', 'upper' );
});

// LDA > N test
test( 'dtfttr: N=5, normal, lower, LDA=8', function t() {
	var expected;
	var info;
	var arf;
	var lda;
	var tc;
	var ei;
	var n;
	var A;
	var i;
	var j;

	tc = findCase( 'n5_N_L_lda8' );
	n = tc.n;
	lda = tc.lda;
	arf = new Float64Array( tc.ARF );
	A = new Float64Array( lda * n );
	info = dtfttr( 'no-transpose', 'lower', n, arf, 1, 0, A, 1, lda, 0, lda );

	assert.equal( info, tc.info, 'info' );

	// The fixture uses print_matrix(A, LDA=8, M=5, N=5) which outputs
	// 5 rows from each of 5 columns: A(1:5,1) then A(1:5,2) etc.
	// In JS with strideA1=1, strideA2=lda=8, element (i,j) is at j*lda+i.
	expected = tc.A;
	ei = 0;
	for ( j = 0; j < n; j += 1 ) {
		for ( i = 0; i < n; i += 1 ) {
			assert.equal( A[ ( j * lda ) + i ], expected[ ei ], 'A(' + i + ',' + j + ')' );
			ei += 1;
		}
	}
});

// Offset test for ARF
test( 'dtfttr: ARF offset', function t() {
	var expected;
	var info;
	var arf;
	var tc;
	var n;
	var A;
	var i;

	tc = findCase( 'n5_N_L' );
	n = tc.n;

	// Prepend 3 dummy values to ARF
	arf = new Float64Array( [ 99.0, 88.0, 77.0 ].concat( Array.from( tc.ARF ) ) );
	A = new Float64Array( n * n );
	info = dtfttr( 'no-transpose', 'lower', n, arf, 1, 3, A, 1, n, 0, n );

	assert.equal( info, 0, 'info' );

	expected = tc.A;
	for ( i = 0; i < expected.length; i += 1 ) {
		assert.equal( A[ i ], expected[ i ], 'A[' + i + ']' );
	}
});

// Offset test for A
test( 'dtfttr: A offset', function t() {
	var expected;
	var info;
	var arf;
	var tc;
	var n;
	var A;
	var i;

	tc = findCase( 'n5_N_L' );
	n = tc.n;

	arf = new Float64Array( tc.ARF );
	// Prepend 2 dummy values to A
	A = new Float64Array( 2 + ( n * n ) );
	info = dtfttr( 'no-transpose', 'lower', n, arf, 1, 0, A, 1, n, 2, n );

	assert.equal( info, 0, 'info' );

	expected = tc.A;
	for ( i = 0; i < expected.length; i += 1 ) {
		assert.equal( A[ 2 + i ], expected[ i ], 'A[' + i + ']' );
	}
});
