

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var ztrsv = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'ztrsv.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'ztrsv: upper, no transpose, non-unit diagonal, N=2', function t() {
	var tc = findCase( 'ztrsv_upper_no_trans' );
	// A = [[2+1i, 3+1i], [0, 4+2i]]  col-major in 4x4
	var A = new Complex128Array( 4 * 4 );
	var Av = reinterpret( A, 0 );
	// a(1,1) = (2,1), a(1,2) = (3,1), a(2,2) = (4,2)
	Av[ 0 ] = 2; Av[ 1 ] = 1;   // A(0,0)
	Av[ 8 ] = 3; Av[ 9 ] = 1;   // A(0,1) at col 1 offset = 4*2 = 8
	Av[ 10 ] = 4; Av[ 11 ] = 2; // A(1,1) at col 1 row 1 = 4*2 + 1*2 = 10
	var x = new Complex128Array( [ 7, 6, 2, 6 ] );
	ztrsv( 'upper', 'no-transpose', 'non-unit', 2, A, 1, 4, 0, x, 1, 0 );
	var xv = reinterpret( x, 0 );
	assertArrayClose( Array.from( xv ), tc.x, 1e-14, 'x' );
});

test( 'ztrsv: lower, no transpose, non-unit diagonal, N=2', function t() {
	var tc = findCase( 'ztrsv_lower_no_trans' );
	// A = [[2+1i, 0], [3+1i, 4+2i]]  col-major in 4x4
	var A = new Complex128Array( 4 * 4 );
	var Av = reinterpret( A, 0 );
	Av[ 0 ] = 2; Av[ 1 ] = 1;   // A(0,0)
	Av[ 2 ] = 3; Av[ 3 ] = 1;   // A(1,0)
	Av[ 10 ] = 4; Av[ 11 ] = 2; // A(1,1)
	var x = new Complex128Array( [ 2, 1, 5, 7 ] );
	ztrsv( 'lower', 'no-transpose', 'non-unit', 2, A, 1, 4, 0, x, 1, 0 );
	var xv = reinterpret( x, 0 );
	assertArrayClose( Array.from( xv ), tc.x, 1e-14, 'x' );
});

test( 'ztrsv: upper, unit diagonal, N=2', function t() {
	var tc = findCase( 'ztrsv_unit_diag' );
	var A = new Complex128Array( 4 * 4 );
	var Av = reinterpret( A, 0 );
	Av[ 0 ] = 99; Av[ 1 ] = 99;  // ignored
	Av[ 8 ] = 3; Av[ 9 ] = 1;
	Av[ 10 ] = 99; Av[ 11 ] = 99; // ignored
	var x = new Complex128Array( [ 3, 4, 1, 1 ] );
	ztrsv( 'upper', 'no-transpose', 'unit', 2, A, 1, 4, 0, x, 1, 0 );
	var xv = reinterpret( x, 0 );
	assertArrayClose( Array.from( xv ), tc.x, 1e-14, 'x' );
});

test( 'ztrsv: upper, transpose (no conj), non-unit, N=2', function t() {
	var tc = findCase( 'ztrsv_upper_trans' );
	var A = new Complex128Array( 4 * 4 );
	var Av = reinterpret( A, 0 );
	Av[ 0 ] = 2; Av[ 1 ] = 1;
	Av[ 8 ] = 3; Av[ 9 ] = 1;
	Av[ 10 ] = 4; Av[ 11 ] = 2;
	var x = new Complex128Array( [ 2, 1, 5, 7 ] );
	ztrsv( 'upper', 'transpose', 'non-unit', 2, A, 1, 4, 0, x, 1, 0 );
	var xv = reinterpret( x, 0 );
	assertArrayClose( Array.from( xv ), tc.x, 1e-14, 'x' );
});

test( 'ztrsv: upper, conjugate transpose, non-unit, N=2', function t() {
	var tc = findCase( 'ztrsv_upper_conjtrans' );
	var A = new Complex128Array( 4 * 4 );
	var Av = reinterpret( A, 0 );
	Av[ 0 ] = 2; Av[ 1 ] = 1;
	Av[ 8 ] = 3; Av[ 9 ] = 1;
	Av[ 10 ] = 4; Av[ 11 ] = 2;
	var x = new Complex128Array( [ 2, -1, 9, 1 ] );
	ztrsv( 'upper', 'conjugate-transpose', 'non-unit', 2, A, 1, 4, 0, x, 1, 0 );
	var xv = reinterpret( x, 0 );
	assertArrayClose( Array.from( xv ), tc.x, 1e-14, 'x' );
});

test( 'ztrsv: N=0 quick return', function t() {
	var tc = findCase( 'ztrsv_n_zero' );
	var A = new Complex128Array( 4 * 4 );
	var x = new Complex128Array( [ 5, 5 ] );
	ztrsv( 'upper', 'no-transpose', 'non-unit', 0, A, 1, 4, 0, x, 1, 0 );
	var xv = reinterpret( x, 0 );
	assertArrayClose( Array.from( xv ), tc.x, 1e-14, 'x' );
});

test( 'ztrsv: N=1, upper, non-unit', function t() {
	var tc = findCase( 'ztrsv_n_one' );
	var A = new Complex128Array( 4 * 4 );
	var Av = reinterpret( A, 0 );
	Av[ 0 ] = 3; Av[ 1 ] = 2;
	var x = new Complex128Array( [ 4, 7 ] );
	ztrsv( 'upper', 'no-transpose', 'non-unit', 1, A, 1, 4, 0, x, 1, 0 );
	var xv = reinterpret( x, 0 );
	assertArrayClose( Array.from( xv ), tc.x, 1e-14, 'x' );
});

test( 'ztrsv: non-unit stride incx=2, upper, no transpose, N=2', function t() {
	var tc = findCase( 'ztrsv_stride' );
	var A = new Complex128Array( 4 * 4 );
	var Av = reinterpret( A, 0 );
	Av[ 0 ] = 2; Av[ 1 ] = 0;   // A(0,0)
	Av[ 8 ] = 1; Av[ 9 ] = 1;   // A(0,1)
	Av[ 10 ] = 3; Av[ 11 ] = 0; // A(1,1)
	var x = new Complex128Array( [ 3, 1, 99, 99, 0, 3 ] );
	ztrsv( 'upper', 'no-transpose', 'non-unit', 2, A, 1, 4, 0, x, 2, 0 );
	var xv = reinterpret( x, 0 );
	assertArrayClose( Array.from( xv ), tc.x, 1e-14, 'x' );
});

test( 'ztrsv: lower, conj-trans, non-unit, N=3', function t() {
	var tc = findCase( 'ztrsv_lower_conjtrans' );
	var A = new Complex128Array( 4 * 4 );
	var Av = reinterpret( A, 0 );
	// Column-major, LDA=4
	Av[ 0 ] = 1; Av[ 1 ] = 1;   // A(0,0)
	Av[ 2 ] = 2; Av[ 3 ] = 1;   // A(1,0)
	Av[ 4 ] = 3; Av[ 5 ] = 1;   // A(2,0)
	Av[ 10 ] = 4; Av[ 11 ] = 2; // A(1,1)
	Av[ 12 ] = 5; Av[ 13 ] = 2; // A(2,1)
	Av[ 20 ] = 6; Av[ 21 ] = 3; // A(2,2)
	var x = new Complex128Array( [ 1, 0, 2, 3, 4, 5 ] );
	ztrsv( 'lower', 'conjugate-transpose', 'non-unit', 3, A, 1, 4, 0, x, 1, 0 );
	var xv = reinterpret( x, 0 );
	assertArrayClose( Array.from( xv ), tc.x, 1e-14, 'x' );
});

test( 'ztrsv: lower, transpose (no conj), non-unit, N=3', function t() {
	var tc = findCase( 'ztrsv_lower_trans' );
	var A = new Complex128Array( 4 * 4 );
	var Av = reinterpret( A, 0 );
	Av[ 0 ] = 1; Av[ 1 ] = 1;
	Av[ 2 ] = 2; Av[ 3 ] = 1;
	Av[ 4 ] = 3; Av[ 5 ] = 1;
	Av[ 10 ] = 4; Av[ 11 ] = 2;
	Av[ 12 ] = 5; Av[ 13 ] = 2;
	Av[ 20 ] = 6; Av[ 21 ] = 3;
	var x = new Complex128Array( [ 1, 0, 2, 3, 4, 5 ] );
	ztrsv( 'lower', 'transpose', 'non-unit', 3, A, 1, 4, 0, x, 1, 0 );
	var xv = reinterpret( x, 0 );
	assertArrayClose( Array.from( xv ), tc.x, 1e-14, 'x' );
});

test( 'ztrsv: negative stride incx=-1, lower, no transpose, N=2', function t() {
	var tc = findCase( 'ztrsv_neg_stride' );
	var A = new Complex128Array( 4 * 4 );
	var Av = reinterpret( A, 0 );
	Av[ 0 ] = 2; Av[ 1 ] = 0;
	Av[ 2 ] = 1; Av[ 3 ] = 1;
	Av[ 10 ] = 3; Av[ 11 ] = 0;
	// Fortran INCX=-1: KX = 1 - (N-1)*INCX = 1 - (1)*(-1) = 2, so x starts at element 2.
	// In 0-based: offsetX = 1, strideX = -1
	var x = new Complex128Array( [ 4, 0, 5, 1 ] );
	ztrsv( 'lower', 'no-transpose', 'non-unit', 2, A, 1, 4, 0, x, -1, 1 );
	var xv = reinterpret( x, 0 );
	assertArrayClose( Array.from( xv ), tc.x, 1e-14, 'x' );
});

test( 'ztrsv: lower, unit diag, no transpose, N=3', function t() {
	var tc = findCase( 'ztrsv_lower_unit' );
	var A = new Complex128Array( 4 * 4 );
	var Av = reinterpret( A, 0 );
	Av[ 0 ] = 99; Av[ 1 ] = 99;  // ignored
	Av[ 2 ] = 1; Av[ 3 ] = 0;
	Av[ 4 ] = 2; Av[ 5 ] = 1;
	Av[ 10 ] = 99; Av[ 11 ] = 99; // ignored
	Av[ 12 ] = 3; Av[ 13 ] = 0;
	Av[ 20 ] = 99; Av[ 21 ] = 99; // ignored
	var x = new Complex128Array( [ 1, 0, 3, 1, 10, 5 ] );
	ztrsv( 'lower', 'no-transpose', 'unit', 3, A, 1, 4, 0, x, 1, 0 );
	var xv = reinterpret( x, 0 );
	assertArrayClose( Array.from( xv ), tc.x, 1e-14, 'x' );
});

test( 'ztrsv: upper, conj-trans, Smith else-branch (|imag|>|real| diagonal)', function t() {
	var tc = findCase( 'ztrsv_upper_conjtrans_smith' );
	var A = new Complex128Array( 4 * 4 );
	var Av = reinterpret( A, 0 );
	Av[ 0 ] = 1; Av[ 1 ] = 5;   // A(0,0) = 1+5i, |imag|>|real|
	Av[ 8 ] = 2; Av[ 9 ] = 1;   // A(0,1)
	Av[ 10 ] = 1; Av[ 11 ] = 4; // A(1,1) = 1+4i, |imag|>|real|
	var x = new Complex128Array( [ 3, 2, 1, 1 ] );
	ztrsv( 'upper', 'conjugate-transpose', 'non-unit', 2, A, 1, 4, 0, x, 1, 0 );
	var xv = reinterpret( x, 0 );
	assertArrayClose( Array.from( xv ), tc.x, 1e-14, 'x' );
});

test( 'ztrsv: lower, conj-trans, Smith else-branch (|imag|>|real| diagonal)', function t() {
	var tc = findCase( 'ztrsv_lower_conjtrans_smith' );
	var A = new Complex128Array( 4 * 4 );
	var Av = reinterpret( A, 0 );
	Av[ 0 ] = 1; Av[ 1 ] = 5;   // A(0,0) = 1+5i
	Av[ 2 ] = 2; Av[ 3 ] = 1;   // A(1,0)
	Av[ 10 ] = 1; Av[ 11 ] = 4; // A(1,1) = 1+4i
	var x = new Complex128Array( [ 3, 2, 1, 1 ] );
	ztrsv( 'lower', 'conjugate-transpose', 'non-unit', 2, A, 1, 4, 0, x, 1, 0 );
	var xv = reinterpret( x, 0 );
	assertArrayClose( Array.from( xv ), tc.x, 1e-14, 'x' );
});
