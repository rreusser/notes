'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Complex128 = require( '@stdlib/complex/float64/ctor' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zsyr2k = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zsyr2k.jsonl' ), 'utf8' ).trim().split( '\n' );
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
	for ( i = 0; i < expected.length; i += 1 ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}

// Common input data matching Fortran test
// A: 3x2 column-major (for trans='no-transpose') or 3x2 as K=3, N=2 (for trans='transpose')
// a(1)=(1,0.5), a(2)=(2,-1), a(3)=(3,1), a(4)=(4,2), a(5)=(5,0), a(6)=(6,-0.5)
var A_data = [ 1, 0.5, 2, -1, 3, 1, 4, 2, 5, 0, 6, -0.5 ];
var B_data = [ 0.5, 1, 1.5, -0.5, 2.5, 0, 3, 1.5, 4, -1, 5, 0.5 ];

function makeA() { return new Complex128Array( A_data ); }
function makeB() { return new Complex128Array( B_data ); }

function makeC( n ) {
	return new Complex128Array( n * n );
}


// TESTS //

test( 'zsyr2k: main export is a function', function t() {
	assert.strictEqual( typeof zsyr2k, 'function' );
});

test( 'zsyr2k: upper, no-transpose', function t() {
	var tc = findCase( 'upper_no_trans' );
	var A = makeA();
	var B = makeB();
	var C = makeC( 3 );
	var alpha = new Complex128( 1.0, 0.0 );
	var beta = new Complex128( 0.0, 0.0 );
	zsyr2k( 'upper', 'no-transpose', 3, 2, alpha, A, 1, 3, 0, B, 1, 3, 0, beta, C, 1, 3, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.C, 1e-14, 'C' );
});

test( 'zsyr2k: lower, no-transpose', function t() {
	var tc = findCase( 'lower_no_trans' );
	var A = makeA();
	var B = makeB();
	var C = makeC( 3 );
	var alpha = new Complex128( 1.0, 0.0 );
	var beta = new Complex128( 0.0, 0.0 );
	zsyr2k( 'lower', 'no-transpose', 3, 2, alpha, A, 1, 3, 0, B, 1, 3, 0, beta, C, 1, 3, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.C, 1e-14, 'C' );
});

test( 'zsyr2k: upper, transpose', function t() {
	var tc = findCase( 'upper_trans' );
	var A = makeA();
	var B = makeB();
	var C = makeC( 2 );
	var alpha = new Complex128( 1.0, 0.0 );
	var beta = new Complex128( 0.0, 0.0 );
	// A is 3x2 col-major (LDA=3), trans='T' means C = alpha*A^T*B + alpha*B^T*A, N=2, K=3
	zsyr2k( 'upper', 'transpose', 2, 3, alpha, A, 1, 3, 0, B, 1, 3, 0, beta, C, 1, 2, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.C, 1e-14, 'C' );
});

test( 'zsyr2k: lower, transpose', function t() {
	var tc = findCase( 'lower_trans' );
	var A = makeA();
	var B = makeB();
	var C = makeC( 2 );
	var alpha = new Complex128( 1.0, 0.0 );
	var beta = new Complex128( 0.0, 0.0 );
	zsyr2k( 'lower', 'transpose', 2, 3, alpha, A, 1, 3, 0, B, 1, 3, 0, beta, C, 1, 2, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.C, 1e-14, 'C' );
});

test( 'zsyr2k: complex alpha and beta', function t() {
	var tc = findCase( 'complex_alpha_beta' );
	var A = makeA();
	var B = makeB();
	// Set up C with specific initial values matching Fortran test
	// 3x3 col-major: c(1)=(1,1), c(4)=(2,-1), c(5)=(0.5,0.5), c(8)=(1,0), c(9)=(3,-1)
	var C = new Complex128Array( [
		1.0, 1.0, 0.0, 0.0, 0.0, 0.0,
		2.0, -1.0, 0.5, 0.5, 0.0, 0.0,
		0.0, 0.0, 1.0, 0.0, 3.0, -1.0
	] );
	var alpha = new Complex128( 2.0, 1.0 );
	var beta = new Complex128( 0.5, -0.5 );
	zsyr2k( 'upper', 'no-transpose', 3, 2, alpha, A, 1, 3, 0, B, 1, 3, 0, beta, C, 1, 3, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.C, 1e-14, 'C' );
});

test( 'zsyr2k: alpha=0 scales C by beta', function t() {
	var tc = findCase( 'alpha_zero' );
	var A = makeA();
	var B = makeB();
	// C is 2x2, upper, with specific values
	var C = new Complex128Array( [
		1.0, 2.0, 3.0, 4.0,
		5.0, 6.0, 7.0, 8.0
	] );
	var alpha = new Complex128( 0.0, 0.0 );
	var beta = new Complex128( 2.0, 0.0 );
	zsyr2k( 'upper', 'no-transpose', 2, 2, alpha, A, 1, 2, 0, B, 1, 2, 0, beta, C, 1, 2, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.C, 1e-14, 'C' );
});

test( 'zsyr2k: N=0 quick return', function t() {
	var C = new Complex128Array( [ 99.0, 0.0 ] );
	var A = makeA();
	var B = makeB();
	var alpha = new Complex128( 1.0, 0.0 );
	var beta = new Complex128( 0.0, 0.0 );
	var result = zsyr2k( 'upper', 'no-transpose', 0, 2, alpha, A, 1, 1, 0, B, 1, 1, 0, beta, C, 1, 1, 0 );
	// C should be unchanged
	assert.ok( result === C );
	var Cv = reinterpret( C, 0 );
	assert.strictEqual( Cv[ 0 ], 99.0 );
	assert.strictEqual( Cv[ 1 ], 0.0 );
});

test( 'zsyr2k: alpha=0, beta=0 zeros C', function t() {
	var tc = findCase( 'alpha_zero_beta_zero' );
	// Lower 2x2
	var C = new Complex128Array( [
		99.0, 88.0, 77.0, 66.0,
		55.0, 44.0, 33.0, 22.0
	] );
	var A = makeA();
	var B = makeB();
	var alpha = new Complex128( 0.0, 0.0 );
	var beta = new Complex128( 0.0, 0.0 );
	zsyr2k( 'lower', 'no-transpose', 2, 2, alpha, A, 1, 2, 0, B, 1, 2, 0, beta, C, 1, 2, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.C, 1e-14, 'C' );
});

test( 'zsyr2k: alpha=0, beta=1 (no-op)', function t() {
	var tc = findCase( 'alpha_zero_beta_one' );
	var C = new Complex128Array( [
		42.0, 13.0, 7.0, 8.0,
		9.0, 10.0, 11.0, 12.0
	] );
	var A = makeA();
	var B = makeB();
	var alpha = new Complex128( 0.0, 0.0 );
	var beta = new Complex128( 1.0, 0.0 );
	var result = zsyr2k( 'lower', 'no-transpose', 2, 2, alpha, A, 1, 2, 0, B, 1, 2, 0, beta, C, 1, 2, 0 );
	assert.ok( result === C );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.C, 1e-14, 'C' );
});

test( 'zsyr2k: lower with nonzero beta', function t() {
	var tc = findCase( 'lower_nonzero_beta' );
	var A = makeA();
	var B = makeB();
	// Set up C with specific values matching Fortran test
	var C = new Complex128Array( [
		1.0, 1.0, 2.0, -1.0, 0.5, 0.5,
		0.0, 0.0, 0.0, 2.0, 3.0, -1.0,
		0.0, 0.0, 0.0, 0.0, 1.0, 0.0
	] );
	var alpha = new Complex128( 1.0, 0.0 );
	var beta = new Complex128( 0.5, 0.0 );
	zsyr2k( 'lower', 'no-transpose', 3, 2, alpha, A, 1, 3, 0, B, 1, 3, 0, beta, C, 1, 3, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.C, 1e-14, 'C' );
});

test( 'zsyr2k: 1x1 scalar case', function t() {
	var tc = findCase( 'scalar' );
	var A = new Complex128Array( [ 3.0, 2.0 ] );
	var B = new Complex128Array( [ 1.0, -1.0 ] );
	var C = new Complex128Array( 1 );
	var alpha = new Complex128( 2.0, 1.0 );
	var beta = new Complex128( 0.0, 0.0 );
	zsyr2k( 'upper', 'no-transpose', 1, 1, alpha, A, 1, 1, 0, B, 1, 1, 0, beta, C, 1, 1, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.C, 1e-14, 'C' );
});

test( 'zsyr2k: K=0 with alpha!=0 and beta=1 is quick return', function t() {
	var C = new Complex128Array( [ 42.0, 13.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0 ] );
	var A = makeA();
	var B = makeB();
	var alpha = new Complex128( 1.0, 0.0 );
	var beta = new Complex128( 1.0, 0.0 );
	var result = zsyr2k( 'upper', 'no-transpose', 2, 0, alpha, A, 1, 2, 0, B, 1, 2, 0, beta, C, 1, 2, 0 );
	assert.ok( result === C );
	var Cv = reinterpret( C, 0 );
	assert.strictEqual( Cv[ 0 ], 42.0 );
	assert.strictEqual( Cv[ 1 ], 13.0 );
});

test( 'zsyr2k: K=0 with beta!=1 scales C', function t() {
	var C = new Complex128Array( [ 2.0, 4.0, 1.0, 2.0, 3.0, 6.0, 8.0, 10.0 ] );
	var A = makeA();
	var B = makeB();
	var alpha = new Complex128( 1.0, 0.0 );
	var beta = new Complex128( 0.5, 0.0 );
	zsyr2k( 'upper', 'no-transpose', 2, 0, alpha, A, 1, 2, 0, B, 1, 2, 0, beta, C, 1, 2, 0 );
	var Cv = reinterpret( C, 0 );
	// Upper triangle: (0,0) and (0,1) and (1,1) scaled by 0.5
	assertClose( Cv[ 0 ], 1.0, 1e-14, 'C[0,0] re' );
	assertClose( Cv[ 1 ], 2.0, 1e-14, 'C[0,0] im' );
	// C[1,0] is lower, not touched in upper case
	assertClose( Cv[ 4 ], 1.5, 1e-14, 'C[0,1] re' );
	assertClose( Cv[ 5 ], 3.0, 1e-14, 'C[0,1] im' );
	assertClose( Cv[ 6 ], 4.0, 1e-14, 'C[1,1] re' );
	assertClose( Cv[ 7 ], 5.0, 1e-14, 'C[1,1] im' );
});

test( 'zsyr2k: complex beta with alpha=0 (upper)', function t() {
	// C = beta*C where beta = (1, 1) and alpha = 0
	var C = new Complex128Array( [ 2.0, 3.0, 0.0, 0.0, 4.0, 5.0, 6.0, 7.0 ] );
	var A = makeA();
	var B = makeB();
	var alpha = new Complex128( 0.0, 0.0 );
	var beta = new Complex128( 1.0, 1.0 );
	zsyr2k( 'upper', 'no-transpose', 2, 2, alpha, A, 1, 2, 0, B, 1, 2, 0, beta, C, 1, 2, 0 );
	var Cv = reinterpret( C, 0 );
	// C[0,0]: (1+i)*(2+3i) = 2+3i+2i+3i^2 = (2-3)+(3+2)i = -1+5i
	assertClose( Cv[ 0 ], -1.0, 1e-14, 'C[0,0] re' );
	assertClose( Cv[ 1 ], 5.0, 1e-14, 'C[0,0] im' );
	// C[0,1]: (1+i)*(4+5i) = 4+5i+4i+5i^2 = (4-5)+(5+4)i = -1+9i
	assertClose( Cv[ 4 ], -1.0, 1e-14, 'C[0,1] re' );
	assertClose( Cv[ 5 ], 9.0, 1e-14, 'C[0,1] im' );
	// C[1,1]: (1+i)*(6+7i) = 6+7i+6i+7i^2 = (6-7)+(7+6)i = -1+13i
	assertClose( Cv[ 6 ], -1.0, 1e-14, 'C[1,1] re' );
	assertClose( Cv[ 7 ], 13.0, 1e-14, 'C[1,1] im' );
});

test( 'zsyr2k: complex beta with alpha=0 (lower)', function t() {
	// C = beta*C where beta = (0.5, -0.5) and alpha = 0
	var C = new Complex128Array( [ 4.0, 2.0, 6.0, 8.0, 0.0, 0.0, 10.0, 12.0 ] );
	var A = makeA();
	var B = makeB();
	var alpha = new Complex128( 0.0, 0.0 );
	var beta = new Complex128( 0.5, -0.5 );
	zsyr2k( 'lower', 'no-transpose', 2, 2, alpha, A, 1, 2, 0, B, 1, 2, 0, beta, C, 1, 2, 0 );
	var Cv = reinterpret( C, 0 );
	// C[0,0]: (0.5-0.5i)*(4+2i) = 2+i-2i-i^2 = 2+1 + (1-2)i = 3-i
	assertClose( Cv[ 0 ], 3.0, 1e-14, 'C[0,0] re' );
	assertClose( Cv[ 1 ], -1.0, 1e-14, 'C[0,0] im' );
	// C[1,0]: (0.5-0.5i)*(6+8i) = 3+4i-3i-4i^2 = 3+4+(4-3)i = 7+i
	assertClose( Cv[ 2 ], 7.0, 1e-14, 'C[1,0] re' );
	assertClose( Cv[ 3 ], 1.0, 1e-14, 'C[1,0] im' );
	// C[1,1]: (0.5-0.5i)*(10+12i) = 5+6i-5i-6i^2 = 5+6+(6-5)i = 11+i
	assertClose( Cv[ 6 ], 11.0, 1e-14, 'C[1,1] re' );
	assertClose( Cv[ 7 ], 1.0, 1e-14, 'C[1,1] im' );
});

test( 'zsyr2k: upper, transpose with nonzero beta', function t() {
	// C is 2x2 with initial values, trans='T' with beta=(0.5,0)
	var A = makeA();
	var B = makeB();
	var C = new Complex128Array( [ 10.0, 20.0, 0.0, 0.0, 30.0, 40.0, 50.0, 60.0 ] );
	var alpha = new Complex128( 1.0, 0.0 );
	var beta = new Complex128( 0.5, 0.0 );
	zsyr2k( 'upper', 'transpose', 2, 3, alpha, A, 1, 3, 0, B, 1, 3, 0, beta, C, 1, 2, 0 );
	var Cv = reinterpret( C, 0 );
	// Expected: 0.5*C_old + alpha*A^T*B + alpha*B^T*A
	// From fixture upper_trans: C_nobet = [20, 2.5, 0, 0, 46.25, 4.75, 118.5, 15]
	// C[0,0] = 0.5*(10+20i) + (20+2.5i) = 5+10i+20+2.5i = 25+12.5i
	assertClose( Cv[ 0 ], 25.0, 1e-14, 'C[0,0] re' );
	assertClose( Cv[ 1 ], 12.5, 1e-14, 'C[0,0] im' );
	// C[0,1] = 0.5*(30+40i) + (46.25+4.75i) = 15+20i+46.25+4.75i = 61.25+24.75i
	assertClose( Cv[ 4 ], 61.25, 1e-14, 'C[0,1] re' );
	assertClose( Cv[ 5 ], 24.75, 1e-14, 'C[0,1] im' );
	// C[1,1] = 0.5*(50+60i) + (118.5+15i) = 25+30i+118.5+15i = 143.5+45i
	assertClose( Cv[ 6 ], 143.5, 1e-14, 'C[1,1] re' );
	assertClose( Cv[ 7 ], 45.0, 1e-14, 'C[1,1] im' );
});

test( 'zsyr2k: lower, transpose with nonzero beta', function t() {
	var A = makeA();
	var B = makeB();
	var C = new Complex128Array( [ 10.0, 20.0, 30.0, 40.0, 0.0, 0.0, 50.0, 60.0 ] );
	var alpha = new Complex128( 1.0, 0.0 );
	var beta = new Complex128( 0.5, 0.0 );
	zsyr2k( 'lower', 'transpose', 2, 3, alpha, A, 1, 3, 0, B, 1, 3, 0, beta, C, 1, 2, 0 );
	var Cv = reinterpret( C, 0 );
	// From fixture lower_trans: C_nobet = [20, 2.5, 46.25, 4.75, 0, 0, 118.5, 15]
	// C[0,0] = 0.5*(10+20i) + (20+2.5i) = 25+12.5i
	assertClose( Cv[ 0 ], 25.0, 1e-14, 'C[0,0] re' );
	assertClose( Cv[ 1 ], 12.5, 1e-14, 'C[0,0] im' );
	// C[1,0] = 0.5*(30+40i) + (46.25+4.75i) = 61.25+24.75i
	assertClose( Cv[ 2 ], 61.25, 1e-14, 'C[1,0] re' );
	assertClose( Cv[ 3 ], 24.75, 1e-14, 'C[1,0] im' );
	// C[1,1] = 0.5*(50+60i) + (118.5+15i) = 143.5+45i
	assertClose( Cv[ 6 ], 143.5, 1e-14, 'C[1,1] re' );
	assertClose( Cv[ 7 ], 45.0, 1e-14, 'C[1,1] im' );
});

test( 'zsyr2k: alpha=0, beta=0 zeros upper C', function t() {
	var C = new Complex128Array( [
		99.0, 88.0, 77.0, 66.0,
		55.0, 44.0, 33.0, 22.0
	] );
	var A = makeA();
	var B = makeB();
	var alpha = new Complex128( 0.0, 0.0 );
	var beta = new Complex128( 0.0, 0.0 );
	zsyr2k( 'upper', 'no-transpose', 2, 2, alpha, A, 1, 2, 0, B, 1, 2, 0, beta, C, 1, 2, 0 );
	var Cv = reinterpret( C, 0 );
	// Upper triangle zeroed: C[0,0], C[0,1], C[1,1]
	assertClose( Cv[ 0 ], 0.0, 1e-14, 'C[0,0] re' );
	assertClose( Cv[ 1 ], 0.0, 1e-14, 'C[0,0] im' );
	// C[1,0] lower, not touched
	assertClose( Cv[ 4 ], 0.0, 1e-14, 'C[0,1] re' );
	assertClose( Cv[ 5 ], 0.0, 1e-14, 'C[0,1] im' );
	assertClose( Cv[ 6 ], 0.0, 1e-14, 'C[1,1] re' );
	assertClose( Cv[ 7 ], 0.0, 1e-14, 'C[1,1] im' );
});

test( 'zsyr2k: returns C', function t() {
	var A = makeA();
	var B = makeB();
	var C = makeC( 3 );
	var alpha = new Complex128( 1.0, 0.0 );
	var beta = new Complex128( 0.0, 0.0 );
	var result = zsyr2k( 'upper', 'no-transpose', 3, 2, alpha, A, 1, 3, 0, B, 1, 3, 0, beta, C, 1, 3, 0 );
	assert.ok( result === C );
});
