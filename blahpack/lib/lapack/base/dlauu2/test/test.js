'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dlauu2 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlauu2.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}


// TESTS //

test( 'dlauu2: upper_3x3', function t() {
	var tc = findCase( 'upper_3x3' );
	// A = [2 1 3; 0 4 5; 0 0 6] (column-major)
	var A = new Float64Array([
		2.0, 0.0, 0.0,
		1.0, 4.0, 0.0,
		3.0, 5.0, 6.0
	]);
	var info = dlauu2( 'U', 3, A, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( A, tc.a, 1e-14, 'a' );
});

test( 'dlauu2: lower_3x3', function t() {
	var tc = findCase( 'lower_3x3' );
	// A = [2 0 0; 1 4 0; 3 5 6] (column-major)
	var A = new Float64Array([
		2.0, 1.0, 3.0,
		0.0, 4.0, 5.0,
		0.0, 0.0, 6.0
	]);
	var info = dlauu2( 'L', 3, A, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( A, tc.a, 1e-14, 'a' );
});

test( 'dlauu2: n_one', function t() {
	var tc = findCase( 'n_one' );
	var A = new Float64Array([ 5.0 ]);
	var info = dlauu2( 'U', 1, A, 1, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( A, tc.a, 1e-14, 'a' );
});

test( 'dlauu2: n_zero', function t() {
	var tc = findCase( 'n_zero' );
	var A = new Float64Array([ 99.0 ]);
	var info = dlauu2( 'U', 0, A, 1, 1, 0 );
	assert.equal( info, tc.info );
	// A should be unchanged
	assert.equal( A[ 0 ], 99.0 );
});

test( 'dlauu2: lower_n_one', function t() {
	var tc = findCase( 'lower_n_one' );
	var A = new Float64Array([ 3.0 ]);
	var info = dlauu2( 'L', 1, A, 1, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( A, tc.a, 1e-14, 'a' );
});

test( 'dlauu2: upper_4x4', function t() {
	var tc = findCase( 'upper_4x4' );
	// A = [1 2 3 4; 0 5 6 7; 0 0 8 9; 0 0 0 10] (column-major)
	var A = new Float64Array([
		1.0, 0.0, 0.0, 0.0,
		2.0, 5.0, 0.0, 0.0,
		3.0, 6.0, 8.0, 0.0,
		4.0, 7.0, 9.0, 10.0
	]);
	var info = dlauu2( 'U', 4, A, 1, 4, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( A, tc.a, 1e-14, 'a' );
});

test( 'dlauu2: identity_upper', function t() {
	var tc = findCase( 'identity_upper' );
	var A = new Float64Array([
		1.0, 0.0, 0.0,
		0.0, 1.0, 0.0,
		0.0, 0.0, 1.0
	]);
	var info = dlauu2( 'U', 3, A, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( A, tc.a, 1e-14, 'a' );
});

test( 'dlauu2: identity_lower', function t() {
	var tc = findCase( 'identity_lower' );
	var A = new Float64Array([
		1.0, 0.0, 0.0,
		0.0, 1.0, 0.0,
		0.0, 0.0, 1.0
	]);
	var info = dlauu2( 'L', 3, A, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( A, tc.a, 1e-14, 'a' );
});

test( 'dlauu2: supports offset', function t() {
	// Test with non-zero offset: embed a 2x2 upper triangular in a larger array
	// A = [3 2; 0 4] => U*U^T = [13 8; 0 16]
	var A = new Float64Array([
		999.0, 999.0, 999.0,  // padding
		3.0, 0.0,
		2.0, 4.0
	]);
	var info = dlauu2( 'U', 2, A, 1, 2, 3 );
	assert.equal( info, 0 );
	assertClose( A[ 3 ], 13.0, 1e-14, 'A(0,0)' );
	assertClose( A[ 4 ], 0.0, 1e-14, 'A(1,0)' );
	assertClose( A[ 5 ], 8.0, 1e-14, 'A(0,1)' );
	assertClose( A[ 6 ], 16.0, 1e-14, 'A(1,1)' );
});

test( 'dlauu2: supports row-major via strides', function t() {
	// Row-major 3x3 upper: A = [2 1 3; 0 4 5; 0 0 6]
	// In row-major storage: [2, 1, 3, 0, 4, 5, 0, 0, 6]
	// strideA1 = 3 (row stride), strideA2 = 1 (col stride)
	var tc = findCase( 'upper_3x3' );
	var A = new Float64Array([
		2.0, 1.0, 3.0,
		0.0, 4.0, 5.0,
		0.0, 0.0, 6.0
	]);
	var info = dlauu2( 'U', 3, A, 3, 1, 0 );
	assert.equal( info, 0 );
	// Result in row-major should be same values but in row-major order
	// Expected column-major: [14, 0, 0, 19, 41, 0, 18, 30, 36]
	// In row-major: [14, 19, 18, 0, 41, 30, 0, 0, 36]
	assertClose( A[ 0 ], 14.0, 1e-14, 'A(0,0)' );
	assertClose( A[ 1 ], 19.0, 1e-14, 'A(0,1)' );
	assertClose( A[ 2 ], 18.0, 1e-14, 'A(0,2)' );
	assertClose( A[ 4 ], 41.0, 1e-14, 'A(1,1)' );
	assertClose( A[ 5 ], 30.0, 1e-14, 'A(1,2)' );
	assertClose( A[ 8 ], 36.0, 1e-14, 'A(2,2)' );
});
