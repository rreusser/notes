

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dsyr2k = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dsyr2k.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'dsyr2k: upper_N', function t() {
	var tc = findCase( 'upper_N' );
	// A is 3x2 col-major, B is 3x2 col-major, C is 3x3
	var A = new Float64Array( [ 1, 2, 3, 4, 5, 6 ] );
	var B = new Float64Array( [ 0.5, 1.5, 2.5, 3.5, 4.5, 5.5 ] );
	var C = new Float64Array( [ 1, 0, 0, 0, 1, 0, 0, 0, 1 ] );
	dsyr2k( 'upper', 'no-transpose', 3, 2, 2.0, A, 1, 3, 0, B, 1, 3, 0, 1.0, C, 1, 3, 0 );
	assertArrayClose( Array.from( C ), tc.c, 1e-14, 'c' );
});

test( 'dsyr2k: lower_N', function t() {
	var tc = findCase( 'lower_N' );
	var A = new Float64Array( [ 1, 2, 3, 4, 5, 6 ] );
	var B = new Float64Array( [ 0.5, 1.5, 2.5, 3.5, 4.5, 5.5 ] );
	var C = new Float64Array( [ 1, 0, 0, 0, 1, 0, 0, 0, 1 ] );
	dsyr2k( 'lower', 'no-transpose', 3, 2, 2.0, A, 1, 3, 0, B, 1, 3, 0, 1.0, C, 1, 3, 0 );
	assertArrayClose( Array.from( C ), tc.c, 1e-14, 'c' );
});

test( 'dsyr2k: upper_T', function t() {
	var tc = findCase( 'upper_T' );
	// A is 2x3 col-major (K=2, N=3), B is 2x3 col-major
	var A = new Float64Array( [ 1, 2, 3, 4, 5, 6 ] );
	var B = new Float64Array( [ 0.5, 1.5, 2.5, 3.5, 4.5, 5.5 ] );
	var C = new Float64Array( [ 1, 0, 0, 0, 1, 0, 0, 0, 1 ] );
	dsyr2k( 'upper', 'transpose', 3, 2, 2.0, A, 1, 2, 0, B, 1, 2, 0, 1.0, C, 1, 3, 0 );
	assertArrayClose( Array.from( C ), tc.c, 1e-14, 'c' );
});

test( 'dsyr2k: lower_T', function t() {
	var tc = findCase( 'lower_T' );
	var A = new Float64Array( [ 1, 2, 3, 4, 5, 6 ] );
	var B = new Float64Array( [ 0.5, 1.5, 2.5, 3.5, 4.5, 5.5 ] );
	var C = new Float64Array( [ 1, 0, 0, 0, 1, 0, 0, 0, 1 ] );
	dsyr2k( 'lower', 'transpose', 3, 2, 2.0, A, 1, 2, 0, B, 1, 2, 0, 1.0, C, 1, 3, 0 );
	assertArrayClose( Array.from( C ), tc.c, 1e-14, 'c' );
});

test( 'dsyr2k: alpha_zero', function t() {
	var tc = findCase( 'alpha_zero' );
	var A = new Float64Array( 6 );
	var B = new Float64Array( 6 );
	var C = new Float64Array( [ 2, 0, 0, 3, 4, 0, 5, 6, 7 ] );
	dsyr2k( 'upper', 'no-transpose', 3, 2, 0.0, A, 1, 3, 0, B, 1, 3, 0, 2.0, C, 1, 3, 0 );
	assertArrayClose( Array.from( C ), tc.c, 1e-14, 'c' );
});

test( 'dsyr2k: beta_zero', function t() {
	var tc = findCase( 'beta_zero' );
	var A = new Float64Array( [ 1, 2, 3, 4, 5, 6 ] );
	var B = new Float64Array( [ 0.5, 1.5, 2.5, 3.5, 4.5, 5.5 ] );
	var C = new Float64Array( [ 99, 0, 0, 0, 99, 0, 0, 0, 99 ] );
	dsyr2k( 'upper', 'no-transpose', 3, 2, 1.0, A, 1, 3, 0, B, 1, 3, 0, 0.0, C, 1, 3, 0 );
	assertArrayClose( Array.from( C ), tc.c, 1e-14, 'c' );
});

test( 'dsyr2k: n_zero', function t() {
	var A = new Float64Array( 1 );
	var B = new Float64Array( 1 );
	var C = new Float64Array( 1 );
	var result = dsyr2k( 'upper', 'no-transpose', 0, 2, 1.0, A, 1, 1, 0, B, 1, 1, 0, 1.0, C, 1, 1, 0 );
	assert.ok( result === C );
});

test( 'dsyr2k: alpha_zero_beta_zero', function t() {
	var tc = findCase( 'alpha_zero_beta_zero' );
	var A = new Float64Array( 6 );
	var B = new Float64Array( 6 );
	var C = new Float64Array( [ 5, 0, 0, 6, 7, 0, 8, 9, 10 ] );
	dsyr2k( 'upper', 'no-transpose', 3, 2, 0.0, A, 1, 3, 0, B, 1, 3, 0, 0.0, C, 1, 3, 0 );
	assertArrayClose( Array.from( C ), tc.c, 1e-14, 'c' );
});

test( 'dsyr2k: alpha_zero_beta_zero_lower', function t() {
	var tc = findCase( 'alpha_zero_beta_zero_lower' );
	var A = new Float64Array( 6 );
	var B = new Float64Array( 6 );
	var C = new Float64Array( [ 5, 6, 7, 0, 8, 9, 0, 0, 10 ] );
	dsyr2k( 'lower', 'no-transpose', 3, 2, 0.0, A, 1, 3, 0, B, 1, 3, 0, 0.0, C, 1, 3, 0 );
	assertArrayClose( Array.from( C ), tc.c, 1e-14, 'c' );
});

test( 'dsyr2k: alpha_zero_beta_scale_upper', function t() {
	var tc = findCase( 'alpha_zero_beta_scale_upper' );
	var A = new Float64Array( 6 );
	var B = new Float64Array( 6 );
	var C = new Float64Array( [ 2, 0, 0, 3, 4, 0, 5, 6, 7 ] );
	dsyr2k( 'upper', 'no-transpose', 3, 2, 0.0, A, 1, 3, 0, B, 1, 3, 0, 3.0, C, 1, 3, 0 );
	assertArrayClose( Array.from( C ), tc.c, 1e-14, 'c' );
});

test( 'dsyr2k: alpha_zero_beta_scale_lower', function t() {
	var tc = findCase( 'alpha_zero_beta_scale_lower' );
	var A = new Float64Array( 6 );
	var B = new Float64Array( 6 );
	var C = new Float64Array( [ 2, 3, 5, 0, 4, 6, 0, 0, 7 ] );
	dsyr2k( 'lower', 'no-transpose', 3, 2, 0.0, A, 1, 3, 0, B, 1, 3, 0, 3.0, C, 1, 3, 0 );
	assertArrayClose( Array.from( C ), tc.c, 1e-14, 'c' );
});

test( 'dsyr2k: upper_N_beta_half', function t() {
	var tc = findCase( 'upper_N_beta_half' );
	var A = new Float64Array( [ 1, 2, 3, 4, 5, 6 ] );
	var B = new Float64Array( [ 0.5, 1.5, 2.5, 3.5, 4.5, 5.5 ] );
	var C = new Float64Array( [ 1, 0, 0, 0, 1, 0, 0, 0, 1 ] );
	dsyr2k( 'upper', 'no-transpose', 3, 2, 1.0, A, 1, 3, 0, B, 1, 3, 0, 0.5, C, 1, 3, 0 );
	assertArrayClose( Array.from( C ), tc.c, 1e-14, 'c' );
});

test( 'dsyr2k: lower_N_beta_zero', function t() {
	var tc = findCase( 'lower_N_beta_zero' );
	var A = new Float64Array( [ 1, 2, 3, 4, 5, 6 ] );
	var B = new Float64Array( [ 0.5, 1.5, 2.5, 3.5, 4.5, 5.5 ] );
	var C = new Float64Array( [ 99, 0, 0, 0, 99, 0, 0, 0, 99 ] );
	dsyr2k( 'lower', 'no-transpose', 3, 2, 1.0, A, 1, 3, 0, B, 1, 3, 0, 0.0, C, 1, 3, 0 );
	assertArrayClose( Array.from( C ), tc.c, 1e-14, 'c' );
});

test( 'dsyr2k: lower_N_beta_half', function t() {
	var tc = findCase( 'lower_N_beta_half' );
	var A = new Float64Array( [ 1, 2, 3, 4, 5, 6 ] );
	var B = new Float64Array( [ 0.5, 1.5, 2.5, 3.5, 4.5, 5.5 ] );
	var C = new Float64Array( [ 1, 0, 0, 0, 1, 0, 0, 0, 1 ] );
	dsyr2k( 'lower', 'no-transpose', 3, 2, 1.0, A, 1, 3, 0, B, 1, 3, 0, 0.5, C, 1, 3, 0 );
	assertArrayClose( Array.from( C ), tc.c, 1e-14, 'c' );
});

test( 'dsyr2k: k_zero_beta_scale', function t() {
	var tc = findCase( 'k_zero_beta_scale' );
	var A = new Float64Array( 6 );
	var B = new Float64Array( 6 );
	var C = new Float64Array( [ 2, 0, 0, 3, 4, 0, 5, 6, 7 ] );
	dsyr2k( 'upper', 'no-transpose', 3, 0, 1.0, A, 1, 3, 0, B, 1, 3, 0, 2.0, C, 1, 3, 0 );
	assertArrayClose( Array.from( C ), tc.c, 1e-14, 'c' );
});

test( 'dsyr2k: upper_T_beta_zero', function t() {
	var tc = findCase( 'upper_T_beta_zero' );
	var A = new Float64Array( [ 1, 2, 3, 4, 5, 6 ] );
	var B = new Float64Array( [ 0.5, 1.5, 2.5, 3.5, 4.5, 5.5 ] );
	var C = new Float64Array( [ 99, 0, 0, 0, 99, 0, 0, 0, 99 ] );
	dsyr2k( 'upper', 'transpose', 3, 2, 1.0, A, 1, 2, 0, B, 1, 2, 0, 0.0, C, 1, 3, 0 );
	assertArrayClose( Array.from( C ), tc.c, 1e-14, 'c' );
});

test( 'dsyr2k: lower_T_beta_zero', function t() {
	var tc = findCase( 'lower_T_beta_zero' );
	var A = new Float64Array( [ 1, 2, 3, 4, 5, 6 ] );
	var B = new Float64Array( [ 0.5, 1.5, 2.5, 3.5, 4.5, 5.5 ] );
	var C = new Float64Array( [ 99, 0, 0, 0, 99, 0, 0, 0, 99 ] );
	dsyr2k( 'lower', 'transpose', 3, 2, 1.0, A, 1, 2, 0, B, 1, 2, 0, 0.0, C, 1, 3, 0 );
	assertArrayClose( Array.from( C ), tc.c, 1e-14, 'c' );
});
