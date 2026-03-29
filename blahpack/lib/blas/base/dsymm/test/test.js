/* eslint-disable no-restricted-syntax, stdlib/require-globals, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dsymm = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dsymm.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'dsymm: left_upper_basic', function t() {
	var tc = findCase( 'left_upper_basic' );

	// A is 3x3 symmetric (upper stored), col-major
	var A = new Float64Array([
		2.0,
		0.0,
		0.0, // col 1 (only A(1,1)=2 matters for upper)
		1.0,
		4.0,
		0.0, // col 2 (A(1,2)=1, A(2,2)=4)
		3.0,
		2.0,
		5.0  // col 3 (A(1,3)=3, A(2,3)=2, A(3,3)=5)
	]);
	var B = new Float64Array([
		1.0,
		2.0,
		3.0, // col 1
		4.0,
		5.0,
		6.0  // col 2
	]);
	var C = new Float64Array( 6 );

	dsymm( 'left', 'upper', 3, 2, 1.0, A, 1, 3, 0, B, 1, 3, 0, 0.0, C, 1, 3, 0 );
	assertArrayClose( Array.from( C ), tc.C, 1e-14, 'C' );
});

test( 'dsymm: left_lower_basic', function t() {
	var tc = findCase( 'left_lower_basic' );

	// Same symmetric matrix, lower stored
	var A = new Float64Array([
		2.0,
		1.0,
		3.0, // col 1 (A(1,1)=2, A(2,1)=1, A(3,1)=3)
		0.0,
		4.0,
		2.0, // col 2 (A(2,2)=4, A(3,2)=2)
		0.0,
		0.0,
		5.0  // col 3 (A(3,3)=5)
	]);
	var B = new Float64Array([
		1.0,
		2.0,
		3.0,
		4.0,
		5.0,
		6.0
	]);
	var C = new Float64Array( 6 );

	dsymm( 'left', 'lower', 3, 2, 1.0, A, 1, 3, 0, B, 1, 3, 0, 0.0, C, 1, 3, 0 );
	assertArrayClose( Array.from( C ), tc.C, 1e-14, 'C' );
});

test( 'dsymm: right_upper_basic', function t() {
	var tc = findCase( 'right_upper_basic' );

	// A is 3x3 symmetric (upper), B is 2x3, C is 2x3
	var A = new Float64Array([
		2.0,
		0.0,
		0.0,
		1.0,
		4.0,
		0.0,
		3.0,
		2.0,
		5.0
	]);
	var B = new Float64Array([
		1.0,
		2.0, // col 1
		3.0,
		4.0, // col 2
		5.0,
		6.0  // col 3
	]);
	var C = new Float64Array( 6 );

	dsymm( 'right', 'upper', 2, 3, 1.0, A, 1, 3, 0, B, 1, 2, 0, 0.0, C, 1, 2, 0 );
	assertArrayClose( Array.from( C ), tc.C, 1e-14, 'C' );
});

test( 'dsymm: right_lower_basic', function t() {
	var tc = findCase( 'right_lower_basic' );
	var A = new Float64Array([
		2.0,
		1.0,
		3.0,
		0.0,
		4.0,
		2.0,
		0.0,
		0.0,
		5.0
	]);
	var B = new Float64Array([
		1.0,
		2.0,
		3.0,
		4.0,
		5.0,
		6.0
	]);
	var C = new Float64Array( 6 );

	dsymm( 'right', 'lower', 2, 3, 1.0, A, 1, 3, 0, B, 1, 2, 0, 0.0, C, 1, 2, 0 );
	assertArrayClose( Array.from( C ), tc.C, 1e-14, 'C' );
});

test( 'dsymm: alpha_beta_scaling', function t() {
	var tc = findCase( 'alpha_beta_scaling' );
	var A = new Float64Array([
		2.0,
		0.0,
		0.0,
		1.0,
		4.0,
		0.0,
		3.0,
		2.0,
		5.0
	]);
	var B = new Float64Array([
		1.0,
		2.0,
		3.0,
		4.0,
		5.0,
		6.0
	]);
	var C = new Float64Array([
		1.0,
		1.0,
		1.0,
		1.0,
		1.0,
		1.0
	]);

	dsymm( 'left', 'upper', 3, 2, 2.0, A, 1, 3, 0, B, 1, 3, 0, 3.0, C, 1, 3, 0 );
	assertArrayClose( Array.from( C ), tc.C, 1e-14, 'C' );
});

test( 'dsymm: alpha_zero', function t() {
	var tc = findCase( 'alpha_zero' );
	var A = new Float64Array( 9 );
	var B = new Float64Array( 4 );
	var C = new Float64Array([ 1.0, 2.0, 3.0, 4.0 ]);

	dsymm( 'left', 'upper', 2, 2, 0.0, A, 1, 2, 0, B, 1, 2, 0, 2.0, C, 1, 2, 0 );
	assertArrayClose( Array.from( C ), tc.C, 1e-14, 'C' );
});

test( 'dsymm: m_zero', function t() {
	var tc = findCase( 'm_zero' );
	var A = new Float64Array( 1 );
	var B = new Float64Array( 1 );
	var C = new Float64Array([ 99.0 ]);

	dsymm( 'left', 'upper', 0, 2, 1.0, A, 1, 1, 0, B, 1, 1, 0, 0.0, C, 1, 1, 0 );
	assertClose( C[ 0 ], tc.C1, 1e-14, 'C1' );
});

test( 'dsymm: n_zero', function t() {
	var tc = findCase( 'n_zero' );
	var A = new Float64Array( 4 );
	var B = new Float64Array( 4 );
	var C = new Float64Array([ 99.0 ]);

	dsymm( 'left', 'upper', 2, 0, 1.0, A, 1, 2, 0, B, 1, 2, 0, 0.0, C, 1, 2, 0 );
	assertClose( C[ 0 ], tc.C1, 1e-14, 'C1' );
});

test( 'dsymm: scalar', function t() {
	var tc = findCase( 'scalar' );
	var A = new Float64Array([ 3.0 ]);
	var B = new Float64Array([ 5.0 ]);
	var C = new Float64Array( 1 );

	dsymm( 'left', 'upper', 1, 1, 2.0, A, 1, 1, 0, B, 1, 1, 0, 0.0, C, 1, 1, 0 );
	assertClose( C[ 0 ], tc.C1, 1e-14, 'C1' );
});

test( 'dsymm: beta_zero', function t() {
	var tc = findCase( 'beta_zero' );

	// A = I (2x2), B = [2 4; 3 5]
	var A = new Float64Array([ 1.0, 0.0, 0.0, 1.0 ]);
	var B = new Float64Array([ 2.0, 3.0, 4.0, 5.0 ]);
	var C = new Float64Array([ 999.0, 999.0, 999.0, 999.0 ]);

	dsymm( 'left', 'lower', 2, 2, 1.0, A, 1, 2, 0, B, 1, 2, 0, 0.0, C, 1, 2, 0 );
	assertArrayClose( Array.from( C ), tc.C, 1e-14, 'C' );
});

test( 'dsymm: alpha_zero_beta_zero (zeros C)', function t() {
	var tc = findCase( 'alpha_zero_beta_zero' );
	var A = new Float64Array( 4 );
	var B = new Float64Array( 4 );
	var C = new Float64Array([ 99.0, 88.0, 77.0, 66.0 ]);

	dsymm( 'left', 'upper', 2, 2, 0.0, A, 1, 2, 0, B, 1, 2, 0, 0.0, C, 1, 2, 0 );
	assertArrayClose( Array.from( C ), tc.C, 1e-14, 'C' );
});

test( 'dsymm: left_lower_nonzero_beta', function t() {
	var tc = findCase( 'left_lower_nonzero_beta' );
	var A = new Float64Array([
		2.0,
		1.0,
		3.0,
		0.0,
		4.0,
		2.0,
		0.0,
		0.0,
		5.0
	]);
	var B = new Float64Array([
		1.0,
		2.0,
		3.0,
		4.0,
		5.0,
		6.0
	]);
	var C = new Float64Array([
		1.0,
		1.0,
		1.0,
		1.0,
		1.0,
		1.0
	]);

	dsymm( 'left', 'lower', 3, 2, 2.0, A, 1, 3, 0, B, 1, 3, 0, 0.5, C, 1, 3, 0 );
	assertArrayClose( Array.from( C ), tc.C, 1e-14, 'C' );
});

test( 'dsymm: right_upper_nonzero_beta', function t() {
	var tc = findCase( 'right_upper_nonzero_beta' );
	var A = new Float64Array([
		2.0,
		0.0,
		0.0,
		1.0,
		4.0,
		0.0,
		3.0,
		2.0,
		5.0
	]);
	var B = new Float64Array([
		1.0,
		2.0,
		3.0,
		4.0,
		5.0,
		6.0
	]);
	var C = new Float64Array([
		1.0,
		2.0,
		3.0,
		4.0,
		5.0,
		6.0
	]);

	dsymm( 'right', 'upper', 2, 3, 1.0, A, 1, 3, 0, B, 1, 2, 0, 0.5, C, 1, 2, 0 );
	assertArrayClose( Array.from( C ), tc.C, 1e-14, 'C' );
});
