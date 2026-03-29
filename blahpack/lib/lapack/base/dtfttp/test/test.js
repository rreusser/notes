/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';


// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dtfttp = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dtfttp.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
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
* RunCase.
*
* @private
* @param {string} name - test case name
* @param {*} transr - transr
* @param {*} uplo - uplo
*/
function runCase( name, transr, uplo ) {
	var expected;
	var info;
	var ARF;
	var tc;
	var AP;
	var N;

	tc = findCase( name );
	N = tc.n;
	ARF = new Float64Array( tc.ARF );
	AP = new Float64Array( tc.AP.length );
	expected = new Float64Array( tc.AP );
	info = dtfttp( transr, uplo, N, ARF, 1, 0, AP, 1, 0 );
	assert.equal( info, 0, name + ': info' );
	assert.deepStrictEqual( AP, expected, name + ': AP' );
}


// TESTS //

test( 'dtfttp is a function', function t() {
	assert.equal( typeof dtfttp, 'function' );
});

test( 'dtfttp: N=0 quick return', function t() {
	var info;
	var AP;

	AP = new Float64Array( [ -1.0, -1.0, -1.0 ] );
	info = dtfttp( 'no-transpose', 'lower', 0, new Float64Array( 0 ), 1, 0, AP, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, 0, 'info should be 0' );
	assert.deepStrictEqual( AP, new Float64Array( [ -1.0, -1.0, -1.0 ] ), 'AP unchanged' ); // eslint-disable-line max-len
});

test( 'dtfttp: N=1, normal', function t() {
	var info;
	var ARF;
	var tc;
	var AP;

	tc = findCase( 'n1_N' );
	ARF = new Float64Array( [ 42.0 ] );
	AP = new Float64Array( 1 );
	info = dtfttp( 'no-transpose', 'lower', 1, ARF, 1, 0, AP, 1, 0 );
	assert.equal( info, 0, 'info' );
	assert.deepStrictEqual( AP, new Float64Array( tc.AP ), 'AP' );
});

test( 'dtfttp: N=1, transpose', function t() {
	var info;
	var ARF;
	var tc;
	var AP;

	tc = findCase( 'n1_T' );
	ARF = new Float64Array( [ 99.0 ] );
	AP = new Float64Array( 1 );
	info = dtfttp( 'transpose', 'upper', 1, ARF, 1, 0, AP, 1, 0 );
	assert.equal( info, 0, 'info' );
	assert.deepStrictEqual( AP, new Float64Array( tc.AP ), 'AP' );
});

// N=5 (odd) — all 4 combinations:
test( 'dtfttp: N=5, no-transpose, lower', function t() {
	runCase( 'n5_N_L', 'no-transpose', 'lower' );
});

test( 'dtfttp: N=5, no-transpose, upper', function t() {
	runCase( 'n5_N_U', 'no-transpose', 'upper' );
});

test( 'dtfttp: N=5, transpose, lower', function t() {
	runCase( 'n5_T_L', 'transpose', 'lower' );
});

test( 'dtfttp: N=5, transpose, upper', function t() {
	runCase( 'n5_T_U', 'transpose', 'upper' );
});

// N=6 (even) — all 4 combinations:
test( 'dtfttp: N=6, no-transpose, lower', function t() {
	runCase( 'n6_N_L', 'no-transpose', 'lower' );
});

test( 'dtfttp: N=6, no-transpose, upper', function t() {
	runCase( 'n6_N_U', 'no-transpose', 'upper' );
});

test( 'dtfttp: N=6, transpose, lower', function t() {
	runCase( 'n6_T_L', 'transpose', 'lower' );
});

test( 'dtfttp: N=6, transpose, upper', function t() {
	runCase( 'n6_T_U', 'transpose', 'upper' );
});

// N=7 (odd, larger) — all 4 combinations:
test( 'dtfttp: N=7, no-transpose, lower', function t() {
	runCase( 'n7_N_L', 'no-transpose', 'lower' );
});

test( 'dtfttp: N=7, no-transpose, upper', function t() {
	runCase( 'n7_N_U', 'no-transpose', 'upper' );
});

test( 'dtfttp: N=7, transpose, lower', function t() {
	runCase( 'n7_T_L', 'transpose', 'lower' );
});

test( 'dtfttp: N=7, transpose, upper', function t() {
	runCase( 'n7_T_U', 'transpose', 'upper' );
});

// N=8 (even, larger) — all 4 combinations:
test( 'dtfttp: N=8, no-transpose, lower', function t() {
	runCase( 'n8_N_L', 'no-transpose', 'lower' );
});

test( 'dtfttp: N=8, no-transpose, upper', function t() {
	runCase( 'n8_N_U', 'no-transpose', 'upper' );
});

test( 'dtfttp: N=8, transpose, lower', function t() {
	runCase( 'n8_T_L', 'transpose', 'lower' );
});

test( 'dtfttp: N=8, transpose, upper', function t() {
	runCase( 'n8_T_U', 'transpose', 'upper' );
});

// Test with non-unit strides:
test( 'dtfttp: N=5, no-transpose, lower, strideARF=2', function t() {
	var expected;
	var info;
	var ARF;
	var tc;
	var AP;
	var N;
	var i;

	tc = findCase( 'n5_N_L' );
	expected = new Float64Array( tc.AP );
	N = tc.n;
	ARF = new Float64Array( tc.ARF.length * 2 );
	for ( i = 0; i < tc.ARF.length; i += 1 ) {
		ARF[ i * 2 ] = tc.ARF[ i ];
	}
	AP = new Float64Array( tc.AP.length );
	info = dtfttp( 'no-transpose', 'lower', N, ARF, 2, 0, AP, 1, 0 );
	assert.equal( info, 0, 'info' );
	assert.deepStrictEqual( AP, expected, 'AP with strideARF=2' );
});

test( 'dtfttp: N=5, no-transpose, lower, strideAP=2', function t() {
	var info;
	var ARF;
	var tc;
	var AP;
	var N;
	var i;

	tc = findCase( 'n5_N_L' );
	ARF = new Float64Array( tc.ARF );
	N = tc.n;
	AP = new Float64Array( tc.AP.length * 2 );
	info = dtfttp( 'no-transpose', 'lower', N, ARF, 1, 0, AP, 2, 0 );
	assert.equal( info, 0, 'info' );
	for ( i = 0; i < tc.AP.length; i += 1 ) {
		assert.equal( AP[ i * 2 ], tc.AP[ i ], 'AP[' + ( i * 2 ) + ']' );
	}
});

test( 'dtfttp: N=6, transpose, upper, with offset', function t() {
	var expected;
	var info;
	var ARF;
	var tc;
	var AP;
	var N;
	var i;

	tc = findCase( 'n6_T_U' );
	N = tc.n;
	ARF = new Float64Array( tc.ARF.length + 3 );
	for ( i = 0; i < tc.ARF.length; i += 1 ) {
		ARF[ i + 3 ] = tc.ARF[ i ];
	}
	AP = new Float64Array( tc.AP.length + 5 );
	expected = new Float64Array( tc.AP );
	info = dtfttp( 'transpose', 'upper', N, ARF, 1, 3, AP, 1, 5 );
	assert.equal( info, 0, 'info' );
	for ( i = 0; i < tc.AP.length; i += 1 ) {
		assert.equal( AP[ i + 5 ], expected[ i ], 'AP[' + ( i + 5 ) + ']' );
	}
});
