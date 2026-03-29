/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';


// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dtpttf = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dtpttf.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
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
* RunTest.
*
* @private
* @param {*} transr - transr
* @param {*} uplo - uplo
* @param {*} N - N
* @param {*} tc - tc
*/
function runTest( transr, uplo, N, tc ) {
	var expected;
	var actual;
	var info;
	var nt;
	var AP;
	var i;

	nt = ( N * ( N + 1 ) ) / 2;
	AP = new Float64Array( tc.AP );
	actual = new Float64Array( nt );
	expected = new Float64Array( tc.ARF );

	info = dtpttf( transr, uplo, N, AP, 1, 0, actual, 1, 0 );
	assert.equal( info, 0, 'info should be 0' );
	for ( i = 0; i < nt; i++ ) {
		assert.equal( actual[ i ], expected[ i ], 'ARF[' + i + '] mismatch' );
	}
}


// TESTS //

test( 'dtpttf is a function', function t() {
	assert.equal( typeof dtpttf, 'function' );
});

test( 'dtpttf: N=0 quick return', function t() {
	var info;
	var ARF;
	var AP;

	AP = new Float64Array( 0 );
	ARF = new Float64Array( 0 );
	info = dtpttf( 'no-transpose', 'lower', 0, AP, 1, 0, ARF, 1, 0 );
	assert.equal( info, 0, 'info should be 0 for N=0' );
});

test( 'dtpttf: N=1, no-transpose, lower', function t() {
	var info;
	var ARF;
	var tc;
	var AP;

	tc = findCase( 'n1_N_L' );
	AP = new Float64Array( [ 42.0 ] );
	ARF = new Float64Array( 1 );
	info = dtpttf( 'no-transpose', 'lower', 1, AP, 1, 0, ARF, 1, 0 );
	assert.equal( info, 0, 'info should be 0' );
	assert.equal( ARF[ 0 ], tc.arf0, 'ARF[0] should be 42' );
});

test( 'dtpttf: N=1, transpose, upper', function t() {
	var info;
	var ARF;
	var tc;
	var AP;

	tc = findCase( 'n1_T_U' );
	AP = new Float64Array( [ 99.0 ] );
	ARF = new Float64Array( 1 );
	info = dtpttf( 'transpose', 'upper', 1, AP, 1, 0, ARF, 1, 0 );
	assert.equal( info, 0, 'info should be 0' );
	assert.equal( ARF[ 0 ], tc.arf0, 'ARF[0] should be 99' );
});

// N=5 (odd) — all 4 combinations

test( 'dtpttf: N=5, no-transpose, lower (odd, normal, lower)', function t() {
	runTest( 'no-transpose', 'lower', 5, findCase( 'n5_N_L' ) );
});

test( 'dtpttf: N=5, no-transpose, upper (odd, normal, upper)', function t() {
	runTest( 'no-transpose', 'upper', 5, findCase( 'n5_N_U' ) );
});

test( 'dtpttf: N=5, transpose, lower (odd, transpose, lower)', function t() {
	runTest( 'transpose', 'lower', 5, findCase( 'n5_T_L' ) );
});

test( 'dtpttf: N=5, transpose, upper (odd, transpose, upper)', function t() {
	runTest( 'transpose', 'upper', 5, findCase( 'n5_T_U' ) );
});

// N=6 (even) — all 4 combinations

test( 'dtpttf: N=6, no-transpose, lower (even, normal, lower)', function t() {
	runTest( 'no-transpose', 'lower', 6, findCase( 'n6_N_L' ) );
});

test( 'dtpttf: N=6, no-transpose, upper (even, normal, upper)', function t() {
	runTest( 'no-transpose', 'upper', 6, findCase( 'n6_N_U' ) );
});

test( 'dtpttf: N=6, transpose, lower (even, transpose, lower)', function t() {
	runTest( 'transpose', 'lower', 6, findCase( 'n6_T_L' ) );
});

test( 'dtpttf: N=6, transpose, upper (even, transpose, upper)', function t() {
	runTest( 'transpose', 'upper', 6, findCase( 'n6_T_U' ) );
});

// N=7 (odd, larger) — all 4 combinations

test( 'dtpttf: N=7, no-transpose, lower (odd, normal, lower)', function t() {
	runTest( 'no-transpose', 'lower', 7, findCase( 'n7_N_L' ) );
});

test( 'dtpttf: N=7, no-transpose, upper (odd, normal, upper)', function t() {
	runTest( 'no-transpose', 'upper', 7, findCase( 'n7_N_U' ) );
});

test( 'dtpttf: N=7, transpose, lower (odd, transpose, lower)', function t() {
	runTest( 'transpose', 'lower', 7, findCase( 'n7_T_L' ) );
});

test( 'dtpttf: N=7, transpose, upper (odd, transpose, upper)', function t() {
	runTest( 'transpose', 'upper', 7, findCase( 'n7_T_U' ) );
});

// N=8 (even, larger) — all 4 combinations

test( 'dtpttf: N=8, no-transpose, lower (even, normal, lower)', function t() {
	runTest( 'no-transpose', 'lower', 8, findCase( 'n8_N_L' ) );
});

test( 'dtpttf: N=8, no-transpose, upper (even, normal, upper)', function t() {
	runTest( 'no-transpose', 'upper', 8, findCase( 'n8_N_U' ) );
});

test( 'dtpttf: N=8, transpose, lower (even, transpose, lower)', function t() {
	runTest( 'transpose', 'lower', 8, findCase( 'n8_T_L' ) );
});

test( 'dtpttf: N=8, transpose, upper (even, transpose, upper)', function t() {
	runTest( 'transpose', 'upper', 8, findCase( 'n8_T_U' ) );
});

// Test with non-unit stride and offset

test( 'dtpttf: N=5, no-transpose, lower with stride=2 and offset', function t() { // eslint-disable-line max-len
	var expected;
	var actual;
	var info;
	var tc;
	var AP;
	var nt;
	var i;

	tc = findCase( 'n5_N_L' );
	nt = 15;
	AP = new Float64Array( 2 * nt );
	for ( i = 0; i < nt; i++ ) {
		AP[ 2 * i ] = tc.AP[ i ];
	}
	expected = new Float64Array( tc.ARF );
	actual = new Float64Array( 2 * nt );
	info = dtpttf( 'no-transpose', 'lower', 5, AP, 2, 0, actual, 2, 0 );
	assert.equal( info, 0, 'info should be 0' );
	for ( i = 0; i < nt; i++ ) {
		assert.equal( actual[ 2 * i ], expected[ i ], 'ARF[' + i + '] mismatch with stride=2' ); // eslint-disable-line max-len
	}
});

test( 'dtpttf: N=6, transpose, upper with offset', function t() {
	var expected;
	var actual;
	var info;
	var tc;
	var AP;
	var nt;
	var i;

	tc = findCase( 'n6_T_U' );
	nt = 21;
	AP = new Float64Array( nt + 3 );
	for ( i = 0; i < nt; i++ ) {
		AP[ 3 + i ] = tc.AP[ i ];
	}
	expected = new Float64Array( tc.ARF );
	actual = new Float64Array( nt + 5 );
	info = dtpttf( 'transpose', 'upper', 6, AP, 1, 3, actual, 1, 5 );
	assert.equal( info, 0, 'info should be 0' );
	for ( i = 0; i < nt; i++ ) {
		assert.equal( actual[ 5 + i ], expected[ i ], 'ARF[' + i + '] mismatch with offset' ); // eslint-disable-line max-len
	}
});
