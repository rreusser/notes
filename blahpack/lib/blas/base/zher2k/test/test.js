

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Complex128 = require( '@stdlib/complex/float64/ctor' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zher2k = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zher2k.jsonl' ), 'utf8' ).trim().split( '\n' );
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

// Common input data: A is 3x2 (trans='N') or 2x3 (trans='C'), B likewise
// A col-major: A(1,1)=(1,2) A(2,1)=(3,4) A(3,1)=(5,6) A(1,2)=(7,8) A(2,2)=(9,10) A(3,2)=(11,12)
var A_N_data = [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ]; // interleaved re/im for 3x2
var B_N_data = [ 0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11.5 ];

// For trans='C': A is 2x3, same values laid out as 2x3 col-major
// A(1,1)=(1,2) A(2,1)=(3,4) A(1,2)=(5,6) A(2,2)=(7,8) A(1,3)=(9,10) A(2,3)=(11,12)
var A_C_data = [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ]; // same raw data, different shape
var B_C_data = [ 0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11.5 ];

function makeA_N() { return new Complex128Array( A_N_data ); }
function makeB_N() { return new Complex128Array( B_N_data ); }
function makeA_C() { return new Complex128Array( A_C_data ); }
function makeB_C() { return new Complex128Array( B_C_data ); }

function makeC_identity() {
	// 3x3 col-major: C(1,1)=1, C(2,2)=1, C(3,3)=1, rest=0
	return new Complex128Array( [ 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0 ] );
}

function makeC_junk() {
	// 3x3 with diagonal = 99
	return new Complex128Array( [ 99, 0, 0, 0, 0, 0, 0, 0, 99, 0, 0, 0, 0, 0, 0, 0, 99, 0 ] );
}


// TESTS //

test( 'zher2k: upper_N', function t() {
	var tc = findCase( 'upper_N' );
	var A = makeA_N();
	var B = makeB_N();
	var C = makeC_identity();
	var alpha = new Complex128( 2.0, 1.0 );
	zher2k( 'U', 'N', 3, 2, alpha, A, 1, 3, 0, B, 1, 3, 0, 1.0, C, 1, 3, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.c, 1e-14, 'c' );
});

test( 'zher2k: lower_N', function t() {
	var tc = findCase( 'lower_N' );
	var A = makeA_N();
	var B = makeB_N();
	var C = makeC_identity();
	var alpha = new Complex128( 2.0, 1.0 );
	zher2k( 'L', 'N', 3, 2, alpha, A, 1, 3, 0, B, 1, 3, 0, 1.0, C, 1, 3, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.c, 1e-14, 'c' );
});

test( 'zher2k: upper_C', function t() {
	var tc = findCase( 'upper_C' );
	var A = makeA_C();
	var B = makeB_C();
	var C = makeC_identity();
	var alpha = new Complex128( 2.0, 1.0 );
	zher2k( 'U', 'C', 3, 2, alpha, A, 1, 2, 0, B, 1, 2, 0, 1.0, C, 1, 3, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.c, 1e-14, 'c' );
});

test( 'zher2k: lower_C', function t() {
	var tc = findCase( 'lower_C' );
	var A = makeA_C();
	var B = makeB_C();
	var C = makeC_identity();
	var alpha = new Complex128( 2.0, 1.0 );
	zher2k( 'L', 'C', 3, 2, alpha, A, 1, 2, 0, B, 1, 2, 0, 1.0, C, 1, 3, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.c, 1e-14, 'c' );
});

test( 'zher2k: alpha_zero', function t() {
	var tc = findCase( 'alpha_zero' );
	var A = makeA_N();
	var B = makeB_N();
	// Upper Hermitian C with off-diagonal values
	var C = new Complex128Array( [ 2, 0, 0, 0, 0, 0, 3, 1, 4, 0, 0, 0, 5, 2, 6, 3, 7, 0 ] );
	var alpha = new Complex128( 0.0, 0.0 );
	zher2k( 'U', 'N', 3, 2, alpha, A, 1, 3, 0, B, 1, 3, 0, 2.0, C, 1, 3, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.c, 1e-14, 'c' );
});

test( 'zher2k: alpha_zero_beta_zero', function t() {
	var tc = findCase( 'alpha_zero_beta_zero' );
	var A = makeA_N();
	var B = makeB_N();
	var C = new Complex128Array( [ 5, 0, 0, 0, 0, 0, 6, 1, 7, 0, 0, 0, 8, 2, 9, 3, 10, 0 ] );
	var alpha = new Complex128( 0.0, 0.0 );
	zher2k( 'U', 'N', 3, 2, alpha, A, 1, 3, 0, B, 1, 3, 0, 0.0, C, 1, 3, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.c, 1e-14, 'c' );
});

test( 'zher2k: alpha_zero_beta_zero_lower', function t() {
	var tc = findCase( 'alpha_zero_beta_zero_lower' );
	var A = makeA_N();
	var B = makeB_N();
	var C = new Complex128Array( [ 5, 0, 6, 1, 7, 2, 0, 0, 8, 0, 9, 3, 0, 0, 0, 0, 10, 0 ] );
	var alpha = new Complex128( 0.0, 0.0 );
	zher2k( 'L', 'N', 3, 2, alpha, A, 1, 3, 0, B, 1, 3, 0, 0.0, C, 1, 3, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.c, 1e-14, 'c' );
});

test( 'zher2k: alpha_zero_beta_scale_lower', function t() {
	var tc = findCase( 'alpha_zero_beta_scale_lower' );
	var A = makeA_N();
	var B = makeB_N();
	var C = new Complex128Array( [ 2, 0, 3, 1, 5, 2, 0, 0, 4, 0, 6, 3, 0, 0, 0, 0, 7, 0 ] );
	var alpha = new Complex128( 0.0, 0.0 );
	zher2k( 'L', 'N', 3, 2, alpha, A, 1, 3, 0, B, 1, 3, 0, 3.0, C, 1, 3, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.c, 1e-14, 'c' );
});

test( 'zher2k: beta_zero', function t() {
	var tc = findCase( 'beta_zero' );
	var A = makeA_N();
	var B = makeB_N();
	var C = makeC_junk();
	var alpha = new Complex128( 2.0, 1.0 );
	zher2k( 'U', 'N', 3, 2, alpha, A, 1, 3, 0, B, 1, 3, 0, 0.0, C, 1, 3, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.c, 1e-14, 'c' );
});

test( 'zher2k: upper_N_beta_half', function t() {
	var tc = findCase( 'upper_N_beta_half' );
	var A = makeA_N();
	var B = makeB_N();
	var C = makeC_identity();
	var alpha = new Complex128( 2.0, 1.0 );
	zher2k( 'U', 'N', 3, 2, alpha, A, 1, 3, 0, B, 1, 3, 0, 0.5, C, 1, 3, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.c, 1e-14, 'c' );
});

test( 'zher2k: lower_N_beta_zero', function t() {
	var tc = findCase( 'lower_N_beta_zero' );
	var A = makeA_N();
	var B = makeB_N();
	var C = makeC_junk();
	var alpha = new Complex128( 2.0, 1.0 );
	zher2k( 'L', 'N', 3, 2, alpha, A, 1, 3, 0, B, 1, 3, 0, 0.0, C, 1, 3, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.c, 1e-14, 'c' );
});

test( 'zher2k: lower_N_beta_half', function t() {
	var tc = findCase( 'lower_N_beta_half' );
	var A = makeA_N();
	var B = makeB_N();
	var C = makeC_identity();
	var alpha = new Complex128( 2.0, 1.0 );
	zher2k( 'L', 'N', 3, 2, alpha, A, 1, 3, 0, B, 1, 3, 0, 0.5, C, 1, 3, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.c, 1e-14, 'c' );
});

test( 'zher2k: upper_C_beta_zero', function t() {
	var tc = findCase( 'upper_C_beta_zero' );
	var A = makeA_C();
	var B = makeB_C();
	var C = makeC_junk();
	var alpha = new Complex128( 2.0, 1.0 );
	zher2k( 'U', 'C', 3, 2, alpha, A, 1, 2, 0, B, 1, 2, 0, 0.0, C, 1, 3, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.c, 1e-14, 'c' );
});

test( 'zher2k: lower_C_beta_zero', function t() {
	var tc = findCase( 'lower_C_beta_zero' );
	var A = makeA_C();
	var B = makeB_C();
	var C = makeC_junk();
	var alpha = new Complex128( 2.0, 1.0 );
	zher2k( 'L', 'C', 3, 2, alpha, A, 1, 2, 0, B, 1, 2, 0, 0.0, C, 1, 3, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.c, 1e-14, 'c' );
});

test( 'zher2k: n_zero', function t() {
	var C = makeC_identity();
	var A = makeA_N();
	var B = makeB_N();
	var alpha = new Complex128( 1.0, 0.0 );
	var result = zher2k( 'U', 'N', 0, 2, alpha, A, 1, 1, 0, B, 1, 1, 0, 1.0, C, 1, 3, 0 );
	// Should be a quick return, C unchanged
	assert.ok( result === C );
});

test( 'zher2k: k_zero_beta_scale', function t() {
	var tc = findCase( 'k_zero_beta_scale' );
	var A = makeA_N();
	var B = makeB_N();
	// Upper C with off-diagonal
	var C = new Complex128Array( [ 2, 0, 0, 0, 0, 0, 3, 1, 4, 0, 0, 0, 5, 2, 6, 3, 7, 0 ] );
	var alpha = new Complex128( 1.0, 0.0 );
	zher2k( 'U', 'N', 3, 0, alpha, A, 1, 3, 0, B, 1, 3, 0, 2.0, C, 1, 3, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.c, 1e-14, 'c' );
});

test( 'zher2k: k_zero_beta_one', function t() {
	var tc = findCase( 'k_zero_beta_one' );
	var A = makeA_N();
	var B = makeB_N();
	var C = new Complex128Array( [ 2, 0, 0, 0, 0, 0, 3, 1, 4, 0, 0, 0, 5, 2, 6, 3, 7, 0 ] );
	var alpha = new Complex128( 1.0, 0.0 );
	// K=0 and beta=1 → quick return
	var result = zher2k( 'U', 'N', 3, 0, alpha, A, 1, 3, 0, B, 1, 3, 0, 1.0, C, 1, 3, 0 );
	assertArrayClose( Array.from( reinterpret( result, 0 ) ), tc.c, 1e-14, 'c' );
});

test( 'zher2k: upper_C_beta_half', function t() {
	var tc = findCase( 'upper_C_beta_half' );
	var A = makeA_C();
	var B = makeB_C();
	var C = makeC_identity();
	var alpha = new Complex128( 2.0, 1.0 );
	zher2k( 'U', 'C', 3, 2, alpha, A, 1, 2, 0, B, 1, 2, 0, 0.5, C, 1, 3, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.c, 1e-14, 'c' );
});

test( 'zher2k: lower_C_beta_half', function t() {
	var tc = findCase( 'lower_C_beta_half' );
	var A = makeA_C();
	var B = makeB_C();
	var C = makeC_identity();
	var alpha = new Complex128( 2.0, 1.0 );
	zher2k( 'L', 'C', 3, 2, alpha, A, 1, 2, 0, B, 1, 2, 0, 0.5, C, 1, 3, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.c, 1e-14, 'c' );
});

test( 'zher2k: upper_N_real_alpha', function t() {
	var tc = findCase( 'upper_N_real_alpha' );
	var A = makeA_N();
	var B = makeB_N();
	var C = makeC_identity();
	var alpha = new Complex128( 1.0, 0.0 );
	zher2k( 'U', 'N', 3, 2, alpha, A, 1, 3, 0, B, 1, 3, 0, 1.0, C, 1, 3, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.c, 1e-14, 'c' );
});
