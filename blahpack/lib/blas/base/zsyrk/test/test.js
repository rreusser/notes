/* eslint-disable no-restricted-syntax, stdlib/require-globals, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Complex128 = require( '@stdlib/complex/float64/ctor' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zsyrk = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zsyrk.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'zsyrk: main export is a function', function t() {
	assert.strictEqual( typeof zsyrk, 'function' );
});

test( 'zsyrk: upper_no_trans', function t() {
	var tc = findCase( 'upper_no_trans' );

	// A is 3x2 complex (col-major: 3 rows, 2 cols), C is 3x3

	// a(1)=(1,0.5), a(2)=(2,-1), a(3)=(3,1), a(4)=(4,2), a(5)=(5,0), a(6)=(6,-0.5)
	var A = new Complex128Array( [ 1, 0.5, 2, -1, 3, 1, 4, 2, 5, 0, 6, -0.5 ] );
	var C = new Complex128Array( 9 ); // 3x3 zeroed
	zsyrk( 'upper', 'no-transpose', 3, 2, new Complex128( 1, 0 ), A, 1, 3, 0, new Complex128( 0, 0 ), C, 1, 3, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.C, 1e-14, 'C' );
});

test( 'zsyrk: lower_no_trans', function t() {
	var tc = findCase( 'lower_no_trans' );
	var A = new Complex128Array( [ 1, 0.5, 2, -1, 3, 1, 4, 2, 5, 0, 6, -0.5 ] );
	var C = new Complex128Array( 9 );
	zsyrk( 'lower', 'no-transpose', 3, 2, new Complex128( 1, 0 ), A, 1, 3, 0, new Complex128( 0, 0 ), C, 1, 3, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.C, 1e-14, 'C' );
});

test( 'zsyrk: upper_trans', function t() {
	var tc = findCase( 'upper_trans' );

	// A is 3x2 col-major, trans='T', so C is 2x2: C := alpha*A^T*A + beta*C
	var A = new Complex128Array( [ 1, 0.5, 2, -1, 3, 1, 4, 2, 5, 0, 6, -0.5 ] );
	var C = new Complex128Array( 4 ); // 2x2 zeroed
	zsyrk( 'upper', 'transpose', 2, 3, new Complex128( 1, 0 ), A, 1, 3, 0, new Complex128( 0, 0 ), C, 1, 2, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.C, 1e-14, 'C' );
});

test( 'zsyrk: lower_trans', function t() {
	var tc = findCase( 'lower_trans' );
	var A = new Complex128Array( [ 1, 0.5, 2, -1, 3, 1, 4, 2, 5, 0, 6, -0.5 ] );
	var C = new Complex128Array( 4 );
	zsyrk( 'lower', 'transpose', 2, 3, new Complex128( 1, 0 ), A, 1, 3, 0, new Complex128( 0, 0 ), C, 1, 2, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.C, 1e-14, 'C' );
});

test( 'zsyrk: complex_alpha_beta', function t() {
	var tc = findCase( 'complex_alpha_beta' );

	// alpha=(2,1), beta=(0.5,-0.5), uplo='U', trans='N', N=3, K=2
	var A = new Complex128Array( [ 1, 0.5, 2, -1, 3, 1, 4, 2, 5, 0, 6, -0.5 ] );

	// C initial: c(1)=(1,1), c(4)=(2,-1), c(5)=(0.5,0.5), c(8)=(1,0), c(9)=(3,-1), rest=0

	// Column-major 3x3:

	// Col 0: (1,1), (0,0), (0,0)

	// Col 1: (2,-1), (0.5,0.5), (0,0)

	// Col 2: (1,0), (0,0), (3,-1)   -- wait, c(8) is col 2 row 1, c(9) is col 2 row 2

	// Fortran 1-based: c(1)=col0,row0; c(2)=col0,row1; c(3)=col0,row2; c(4)=col1,row0; c(5)=col1,row1; c(6)=col1,row2; c(7)=col2,row0; c(8)=col2,row1; c(9)=col2,row2

	// Fortran 1-based col-major 3x3:

	// c(1)=(1,1)->C[0,0], c(4)=(2,-1)->C[0,1], c(5)=(0.5,0.5)->C[1,1],

	// c(8)=(1,0)->C[1,2], c(9)=(3,-1)->C[2,2], rest=0
	var C = new Complex128Array([
		1,
		1,
		0,
		0,
		0,
		0,
		2,
		-1,
		0.5,
		0.5,
		0,
		0,
		0,
		0,
		1,
		0,
		3,
		-1
	]);
	zsyrk( 'upper', 'no-transpose', 3, 2, new Complex128( 2, 1 ), A, 1, 3, 0, new Complex128( 0.5, -0.5 ), C, 1, 3, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.C, 1e-14, 'C' );
});

test( 'zsyrk: alpha_zero (scale C by beta only)', function t() {
	var tc = findCase( 'alpha_zero' );

	// alpha=0, beta=(2,0), uplo='U', trans='N', N=2, K=2

	// c(1)=(1,2), c(2)=(3,4), c(3)=(5,6), c(4)=(7,8)
	var A = new Complex128Array( 6 ); // doesn't matter
	var C = new Complex128Array( [ 1, 2, 3, 4, 5, 6, 7, 8 ] );
	zsyrk( 'upper', 'no-transpose', 2, 2, new Complex128( 0, 0 ), A, 1, 2, 0, new Complex128( 2, 0 ), C, 1, 2, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.C, 1e-14, 'C' );
});

test( 'zsyrk: n_zero (quick return)', function t() {
	var tc = findCase( 'n_zero' );
	var A = new Complex128Array( 2 );
	var C = new Complex128Array( [ 99, 0 ] );
	zsyrk( 'upper', 'no-transpose', 0, 2, new Complex128( 1, 0 ), A, 1, 1, 0, new Complex128( 0, 0 ), C, 1, 1, 0 );

	// C should be unchanged
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.C, 1e-14, 'C' );
});

test( 'zsyrk: alpha_zero_beta_zero (zero out upper)', function t() {
	var tc = findCase( 'alpha_zero_beta_zero' );

	// alpha=0, beta=0, uplo='U', N=2, K=2

	// c(1)=(99,88), c(2)=(77,66), c(3)=(55,44), c(4)=(33,22)
	var A = new Complex128Array( 4 );
	var C = new Complex128Array( [ 99, 88, 77, 66, 55, 44, 33, 22 ] );
	zsyrk( 'upper', 'no-transpose', 2, 2, new Complex128( 0, 0 ), A, 1, 2, 0, new Complex128( 0, 0 ), C, 1, 2, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.C, 1e-14, 'C' );
});

test( 'zsyrk: alpha_zero_beta_one (no-op, lower)', function t() {
	var tc = findCase( 'alpha_zero_beta_one' );

	// alpha=0, beta=1, uplo='L', N=2, K=2
	var A = new Complex128Array( 4 );
	var C = new Complex128Array( [ 42, 13, 7, 8, 9, 10, 11, 12 ] );
	zsyrk( 'lower', 'no-transpose', 2, 2, new Complex128( 0, 0 ), A, 1, 2, 0, new Complex128( 1, 0 ), C, 1, 2, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.C, 1e-14, 'C' );
});

test( 'zsyrk: lower_nonzero_beta', function t() {
	var tc = findCase( 'lower_nonzero_beta' );

	// alpha=1, beta=0.5, uplo='L', trans='N', N=3, K=2
	var A = new Complex128Array( [ 1, 0.5, 2, -1, 3, 1, 4, 2, 5, 0, 6, -0.5 ] );

	// c(1)=(1,1), c(2)=(2,-1), c(3)=(0.5,0.5), c(4)=0, c(5)=(0,2), c(6)=(3,-1), c(7)=0, c(8)=0, c(9)=(1,0)
	var C = new Complex128Array([
		1,
		1,
		2,
		-1,
		0.5,
		0.5,
		0,
		0,
		0,
		2,
		3,
		-1,
		0,
		0,
		0,
		0,
		1,
		0
	]);
	zsyrk( 'lower', 'no-transpose', 3, 2, new Complex128( 1, 0 ), A, 1, 3, 0, new Complex128( 0.5, 0 ), C, 1, 3, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.C, 1e-14, 'C' );
});

test( 'zsyrk: scalar (1x1 case)', function t() {
	var tc = findCase( 'scalar' );

	// a(1)=(3,2), alpha=(2,1), beta=0, N=1, K=1
	var A = new Complex128Array( [ 3, 2 ] );
	var C = new Complex128Array( 1 );
	zsyrk( 'upper', 'no-transpose', 1, 1, new Complex128( 2, 1 ), A, 1, 1, 0, new Complex128( 0, 0 ), C, 1, 1, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.C, 1e-14, 'C' );
});

test( 'zsyrk: returns C', function t() {
	var ret = zsyrk( 'upper', 'no-transpose', 1, 2, new Complex128( 1, 0 ), A, 1, 1, 0, new Complex128( 0, 0 ), C, 1, 1, 0 );
	var A = new Complex128Array( [ 1, 0, 2, 0 ] );
	var C = new Complex128Array( 1 );
	assert.strictEqual( ret, C );
});

test( 'zsyrk: K=0 with beta!=1 still scales C', function t() {
	// K=0, alpha=1, beta=(2,1): C := alpha*A*A^T + beta*C but A has 0 columns, so just beta*C
	var Cv = reinterpret( C, 0 );
	var A = new Complex128Array( 0 );
	var C = new Complex128Array( [ 3, 4 ] );
	assertClose( Cv[ 0 ], 2.0, 1e-14, 'real' );
	assertClose( Cv[ 1 ], 11.0, 1e-14, 'imag' );
});

test( 'zsyrk: lower alpha_zero scaling', function t() {
	// uplo='L', alpha=0, beta=(0.5,0.5), N=2
	var Cv = reinterpret( C, 0 );
	var A = new Complex128Array( 4 );
	var C = new Complex128Array( [ 2, 0, 4, 0, 6, 0, 8, 0 ] );

	// lower: (0,0) and (1,0) and (1,1)

	// C[0,0]=(2,0) -> beta*C = (0.5+0.5i)*(2+0i) = (1,1)
	assertClose( Cv[ 0 ], 1.0, 1e-14, 'C00r' );
	assertClose( Cv[ 1 ], 1.0, 1e-14, 'C00i' );

	// C[1,0]=(4,0) -> beta*C = (0.5+0.5i)*(4+0i) = (2,2)
	assertClose( Cv[ 2 ], 2.0, 1e-14, 'C10r' );
	assertClose( Cv[ 3 ], 2.0, 1e-14, 'C10i' );

	// C[1,1]=(8,0) -> beta*C = (0.5+0.5i)*(8+0i) = (4,4)
	assertClose( Cv[ 6 ], 4.0, 1e-14, 'C11r' );
	assertClose( Cv[ 7 ], 4.0, 1e-14, 'C11i' );
});

test( 'zsyrk: lower alpha_zero beta_zero zeroes lower triangle', function t() {
	var Cv = reinterpret( C, 0 );
	var A = new Complex128Array( 4 );
	var C = new Complex128Array( [ 1, 2, 3, 4, 5, 6, 7, 8 ] );

	// lower: C[0,0], C[1,0], C[1,1] zeroed; C[0,1] untouched
	assertClose( Cv[ 0 ], 0.0, 1e-14, 'C00r' );
	assertClose( Cv[ 1 ], 0.0, 1e-14, 'C00i' );
	assertClose( Cv[ 2 ], 0.0, 1e-14, 'C10r' );
	assertClose( Cv[ 3 ], 0.0, 1e-14, 'C10i' );

	// C[0,1] = col1,row0 = index 2 in complex = (5,6) untouched
	assertClose( Cv[ 4 ], 5.0, 1e-14, 'C01r' );
	assertClose( Cv[ 5 ], 6.0, 1e-14, 'C01i' );
	assertClose( Cv[ 6 ], 0.0, 1e-14, 'C11r' );
	assertClose( Cv[ 7 ], 0.0, 1e-14, 'C11i' );
});

test( 'zsyrk: upper trans with nonzero beta', function t() {
	// Test trans='T', upper, with nonzero beta
	var Cv = reinterpret( C, 0 );
	var A = new Complex128Array( [ 1, 0.5, 2, -1, 3, 1, 4, 2, 5, 0, 6, -0.5 ] );
	var C = new Complex128Array( [ 1, 1, 0, 0, 0, 0, 2, -1 ] );

	// C[0,0] = 0.5*(1+i) + A^T*A[0,0] = (0.5,0.5) + (11.75,3)
	assertClose( Cv[ 0 ], 12.25, 1e-14, 'C00r' );
	assertClose( Cv[ 1 ], 3.5, 1e-14, 'C00i' );

	// C[0,1] = 0.5*(0+0) + A^T*A[0,1] = (31.5,3.5)
	assertClose( Cv[ 4 ], 31.5, 1e-14, 'C01r' );
	assertClose( Cv[ 5 ], 3.5, 1e-14, 'C01i' );

	// C[1,1] = 0.5*(2-i) + A^T*A[1,1] = (1,-0.5) + (72.75,10)
	assertClose( Cv[ 6 ], 73.75, 1e-14, 'C11r' );
	assertClose( Cv[ 7 ], 9.5, 1e-14, 'C11i' );
});

test( 'zsyrk: lower trans with nonzero beta', function t() {
	// Test trans='T', lower, with nonzero beta
	var Cv = reinterpret( C, 0 );
	var A = new Complex128Array( [ 1, 0.5, 2, -1, 3, 1, 4, 2, 5, 0, 6, -0.5 ] );
	var C = new Complex128Array( [ 1, 1, 0, 0, 0, 0, 2, -1 ] );

	// C[0,0] = same as upper
	assertClose( Cv[ 0 ], 12.25, 1e-14, 'C00r' );
	assertClose( Cv[ 1 ], 3.5, 1e-14, 'C00i' );

	// C[1,0] = 0.5*(0+0) + A^T*A[1,0] = (31.5,3.5)
	assertClose( Cv[ 2 ], 31.5, 1e-14, 'C10r' );
	assertClose( Cv[ 3 ], 3.5, 1e-14, 'C10i' );

	// C[1,1] = same as upper
	assertClose( Cv[ 6 ], 73.75, 1e-14, 'C11r' );
	assertClose( Cv[ 7 ], 9.5, 1e-14, 'C11i' );
});

test( 'zsyrk: complex alpha with transpose', function t() {
	// Test complex alpha=(1,1) with trans='T', N=2, K=2
	// A is 2x2 col-major
	var Cv = reinterpret( C, 0 );
	var A = new Complex128Array( [ 1, 0, 0, 1, 2, 0, 0, 2 ] );
	var C = new Complex128Array( 4 );

	// temp[0,0] = A[0,0]*A[0,0] + A[1,0]*A[1,0] = (1)(1) + (0+i)(0+i) = 1 + (-1) = 0

	// C[0,0] = alpha*temp = (1+i)*0 = 0
	assertClose( Cv[ 0 ], 0.0, 1e-14, 'C00r' );
	assertClose( Cv[ 1 ], 0.0, 1e-14, 'C00i' );

	// temp[0,1] = A[0,0]*A[0,1] + A[1,0]*A[1,1] = (1)(2) + (i)(2i) = 2 + (-2) = 0

	// C[0,1] = alpha*temp = (1+i)*0 = 0
	assertClose( Cv[ 4 ], 0.0, 1e-14, 'C01r' );
	assertClose( Cv[ 5 ], 0.0, 1e-14, 'C01i' );

	// temp[1,1] = A[0,1]*A[0,1] + A[1,1]*A[1,1] = (2)(2) + (2i)(2i) = 4+(-4) = 0
	assertClose( Cv[ 6 ], 0.0, 1e-14, 'C11r' );
	assertClose( Cv[ 7 ], 0.0, 1e-14, 'C11i' );
});

test( 'zsyrk: K=0 alpha=1 beta=1 quick return', function t() {
	// When K=0 and beta=1, quick return
	var Cv = reinterpret( C, 0 );
	var A = new Complex128Array( 0 );
	var C = new Complex128Array( [ 5, 6 ] );
	assertClose( Cv[ 0 ], 5.0, 1e-14, 'real' );
	assertClose( Cv[ 1 ], 6.0, 1e-14, 'imag' );
});

test( 'zsyrk: complex beta scaling with alpha_zero lower', function t() {
	// alpha=0, beta=(1,2), uplo='L', N=2
	// Test complex beta scaling on lower triangle
	var Cv = reinterpret( C, 0 );
	var A = new Complex128Array( 4 );
	var C = new Complex128Array( [ 1, 0, 0, 1, 2, 0, 3, 0 ] );

	// C[0,0] = (1+2i)*(1+0i) = (1,2)
	assertClose( Cv[ 0 ], 1.0, 1e-14, 'C00r' );
	assertClose( Cv[ 1 ], 2.0, 1e-14, 'C00i' );

	// C[1,0] = (1+2i)*(0+i) = (0+i+0-2) = (-2,1)
	assertClose( Cv[ 2 ], -2.0, 1e-14, 'C10r' );
	assertClose( Cv[ 3 ], 1.0, 1e-14, 'C10i' );

	// C[1,1] = (1+2i)*(3+0i) = (3,6)
	assertClose( Cv[ 6 ], 3.0, 1e-14, 'C11r' );
	assertClose( Cv[ 7 ], 6.0, 1e-14, 'C11i' );
});
