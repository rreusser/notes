/* eslint-disable no-restricted-syntax, stdlib/require-globals, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var ztpmv = require( './../../ztpmv/lib/base.js' );
var ztpsv = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'ztpsv.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// HELPERS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.strictEqual( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i += 1 ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}

// Upper packed AP (4x4) matching Fortran test:
// a11=(5,1), a12=(1,0.5), a22=(6,-1), a13=(0.5,0), a23=(1,-0.5), a33=(7,0.5)
// a14=(0,0), a24=(0.5,1), a34=(1,0), a44=(8,-1)
function upperAP() {
	return new Complex128Array( [
		5, 1, 1, 0.5, 6, -1, 0.5, 0, 1, -0.5, 7, 0.5,
		0, 0, 0.5, 1, 1, 0, 8, -1
	] );
}

// Lower packed AP (4x4) matching Fortran test:
// a11=(5,1), a21=(1,0.5), a31=(0.5,0), a41=(0,0)
// a22=(6,-1), a32=(1,-0.5), a42=(0.5,1)
// a33=(7,0.5), a43=(1,0)
// a44=(8,-1)
function lowerAP() {
	return new Complex128Array( [
		5, 1, 1, 0.5, 0.5, 0, 0, 0,
		6, -1, 1, -0.5, 0.5, 1,
		7, 0.5, 1, 0,
		8, -1
	] );
}

// Known solution vector: [1+0i, 2+i, 3-i, 4+0.5i]
function knownX() {
	return new Complex128Array( [ 1, 0, 2, 1, 3, -1, 4, 0.5 ] );
}


// TESTS //

test( 'ztpsv: base is a function', function t() {
	assert.strictEqual( typeof ztpsv, 'function' );
});

test( 'ztpsv: upper, no-transpose, non-unit (N=4)', function t() {
	var tc = findCase( 'upper_no_trans_nonunit' );
	var ap = upperAP();
	var x = knownX();
	// Compute b = A*x
	ztpmv( 'upper', 'no-transpose', 'non-unit', 4, ap, 1, 0, x, 1, 0 );
	// Solve A*x = b
	var result = ztpsv( 'upper', 'no-transpose', 'non-unit', 4, ap, 1, 0, x, 1, 0 );
	assert.strictEqual( result, x );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 1e-14, 'x' );
});

test( 'ztpsv: upper, transpose, non-unit (N=4)', function t() {
	var tc = findCase( 'upper_trans_nonunit' );
	var ap = upperAP();
	var x = knownX();
	ztpmv( 'upper', 'transpose', 'non-unit', 4, ap, 1, 0, x, 1, 0 );
	ztpsv( 'upper', 'transpose', 'non-unit', 4, ap, 1, 0, x, 1, 0 );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 1e-14, 'x' );
});

test( 'ztpsv: upper, conjugate-transpose, non-unit (N=4)', function t() {
	var tc = findCase( 'upper_conj_trans_nonunit' );
	var ap = upperAP();
	var x = knownX();
	ztpmv( 'upper', 'conjugate-transpose', 'non-unit', 4, ap, 1, 0, x, 1, 0 );
	ztpsv( 'upper', 'conjugate-transpose', 'non-unit', 4, ap, 1, 0, x, 1, 0 );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 1e-14, 'x' );
});

test( 'ztpsv: upper, no-transpose, unit diagonal (N=4)', function t() {
	var tc = findCase( 'upper_no_trans_unit' );
	var ap = upperAP();
	var x = knownX();
	ztpmv( 'upper', 'no-transpose', 'unit', 4, ap, 1, 0, x, 1, 0 );
	ztpsv( 'upper', 'no-transpose', 'unit', 4, ap, 1, 0, x, 1, 0 );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 1e-14, 'x' );
});

test( 'ztpsv: lower, no-transpose, non-unit (N=4)', function t() {
	var tc = findCase( 'lower_no_trans_nonunit' );
	var ap = lowerAP();
	var x = knownX();
	ztpmv( 'lower', 'no-transpose', 'non-unit', 4, ap, 1, 0, x, 1, 0 );
	ztpsv( 'lower', 'no-transpose', 'non-unit', 4, ap, 1, 0, x, 1, 0 );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 1e-14, 'x' );
});

test( 'ztpsv: lower, transpose, non-unit (N=4)', function t() {
	var tc = findCase( 'lower_trans_nonunit' );
	var ap = lowerAP();
	var x = knownX();
	ztpmv( 'lower', 'transpose', 'non-unit', 4, ap, 1, 0, x, 1, 0 );
	ztpsv( 'lower', 'transpose', 'non-unit', 4, ap, 1, 0, x, 1, 0 );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 1e-14, 'x' );
});

test( 'ztpsv: lower, conjugate-transpose, non-unit (N=4)', function t() {
	var tc = findCase( 'lower_conj_trans_nonunit' );
	var ap = lowerAP();
	var x = knownX();
	ztpmv( 'lower', 'conjugate-transpose', 'non-unit', 4, ap, 1, 0, x, 1, 0 );
	ztpsv( 'lower', 'conjugate-transpose', 'non-unit', 4, ap, 1, 0, x, 1, 0 );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 1e-14, 'x' );
});

test( 'ztpsv: lower, no-transpose, unit diagonal (N=4)', function t() {
	var tc = findCase( 'lower_no_trans_unit' );
	var ap = lowerAP();
	var x = knownX();
	ztpmv( 'lower', 'no-transpose', 'unit', 4, ap, 1, 0, x, 1, 0 );
	ztpsv( 'lower', 'no-transpose', 'unit', 4, ap, 1, 0, x, 1, 0 );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 1e-14, 'x' );
});

test( 'ztpsv: N=0 quick return', function t() {
	var tc = findCase( 'n_zero' );
	var ap = upperAP();
	var x = new Complex128Array( [ 99, 0 ] );
	var result = ztpsv( 'upper', 'no-transpose', 'non-unit', 0, ap, 1, 0, x, 1, 0 );
	assert.strictEqual( result, x );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 1e-14, 'x' );
});

test( 'ztpsv: upper, no-transpose, non-unit, strideX=2 (N=4)', function t() {
	var tc = findCase( 'upper_stride_2' );
	var ap = upperAP();
	// x with stride 2: [(1,0), (0,0), (2,1), (0,0), (3,-1), (0,0), (4,0.5), (0,0)]
	var x = new Complex128Array( [ 1, 0, 0, 0, 2, 1, 0, 0, 3, -1, 0, 0, 4, 0.5, 0, 0 ] );
	ztpmv( 'upper', 'no-transpose', 'non-unit', 4, ap, 1, 0, x, 2, 0 );
	ztpsv( 'upper', 'no-transpose', 'non-unit', 4, ap, 1, 0, x, 2, 0 );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 1e-14, 'x' );
});

test( 'ztpsv: N=1, scalar case', function t() {
	var tc = findCase( 'scalar' );
	var ap = new Complex128Array( [ 5, 2 ] );
	var x = new Complex128Array( [ 3, -1 ] );
	// For N=1: ztpsv just divides x[0] by A[0,0] = (5+2i)
	// x[0] = (3-i) / (5+2i)
	ztpsv( 'upper', 'no-transpose', 'non-unit', 1, ap, 1, 0, x, 1, 0 );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 1e-14, 'x' );
});

test( 'ztpsv: upper, transpose, unit diagonal (N=4)', function t() {
	var ap = upperAP();
	var x = knownX();
	ztpmv( 'upper', 'transpose', 'unit', 4, ap, 1, 0, x, 1, 0 );
	ztpsv( 'upper', 'transpose', 'unit', 4, ap, 1, 0, x, 1, 0 );
	var xv = reinterpret( x, 0 );
	assertClose( xv[ 0 ], 1.0, 1e-14, 'x[0]' );
	assertClose( xv[ 1 ], 0.0, 1e-14, 'x[1]' );
	assertClose( xv[ 2 ], 2.0, 1e-14, 'x[2]' );
	assertClose( xv[ 3 ], 1.0, 1e-14, 'x[3]' );
});

test( 'ztpsv: lower, transpose, unit diagonal (N=4)', function t() {
	var ap = lowerAP();
	var x = knownX();
	ztpmv( 'lower', 'transpose', 'unit', 4, ap, 1, 0, x, 1, 0 );
	ztpsv( 'lower', 'transpose', 'unit', 4, ap, 1, 0, x, 1, 0 );
	var xv = reinterpret( x, 0 );
	assertClose( xv[ 0 ], 1.0, 1e-14, 'x[0]' );
	assertClose( xv[ 1 ], 0.0, 1e-14, 'x[1]' );
	assertClose( xv[ 6 ], 4.0, 1e-14, 'x[6]' );
	assertClose( xv[ 7 ], 0.5, 1e-14, 'x[7]' );
});

test( 'ztpsv: upper, conjugate-transpose, unit diagonal (N=4)', function t() {
	var ap = upperAP();
	var x = knownX();
	ztpmv( 'upper', 'conjugate-transpose', 'unit', 4, ap, 1, 0, x, 1, 0 );
	ztpsv( 'upper', 'conjugate-transpose', 'unit', 4, ap, 1, 0, x, 1, 0 );
	var xv = reinterpret( x, 0 );
	assertClose( xv[ 0 ], 1.0, 1e-14, 'x[0]' );
	assertClose( xv[ 1 ], 0.0, 1e-14, 'x[1]' );
	assertClose( xv[ 4 ], 3.0, 1e-14, 'x[4]' );
	assertClose( xv[ 5 ], -1.0, 1e-14, 'x[5]' );
});

test( 'ztpsv: lower, conjugate-transpose, unit diagonal (N=4)', function t() {
	var ap = lowerAP();
	var x = knownX();
	ztpmv( 'lower', 'conjugate-transpose', 'unit', 4, ap, 1, 0, x, 1, 0 );
	ztpsv( 'lower', 'conjugate-transpose', 'unit', 4, ap, 1, 0, x, 1, 0 );
	var xv = reinterpret( x, 0 );
	assertClose( xv[ 0 ], 1.0, 1e-14, 'x[0]' );
	assertClose( xv[ 1 ], 0.0, 1e-14, 'x[1]' );
	assertClose( xv[ 6 ], 4.0, 1e-14, 'x[6]' );
	assertClose( xv[ 7 ], 0.5, 1e-14, 'x[7]' );
});

test( 'ztpsv: x with all zeros remains zeros (upper, no-transpose)', function t() {
	var ap = upperAP();
	var x = new Complex128Array( [ 0, 0, 0, 0, 0, 0, 0, 0 ] );
	ztpsv( 'upper', 'no-transpose', 'non-unit', 4, ap, 1, 0, x, 1, 0 );
	var xv = reinterpret( x, 0 );
	var i;
	for ( i = 0; i < 8; i += 1 ) {
		assert.strictEqual( xv[ i ], 0.0, 'x[' + i + '] should be zero' );
	}
});

test( 'ztpsv: x with all zeros remains zeros (lower, no-transpose)', function t() {
	var ap = lowerAP();
	var x = new Complex128Array( [ 0, 0, 0, 0, 0, 0, 0, 0 ] );
	ztpsv( 'lower', 'no-transpose', 'non-unit', 4, ap, 1, 0, x, 1, 0 );
	var xv = reinterpret( x, 0 );
	var i;
	for ( i = 0; i < 8; i += 1 ) {
		assert.strictEqual( xv[ i ], 0.0, 'x[' + i + '] should be zero' );
	}
});

test( 'ztpsv: lower, no-transpose, stride 2 (N=4)', function t() {
	var ap = lowerAP();
	var x = new Complex128Array( [ 1, 0, 0, 0, 2, 1, 0, 0, 3, -1, 0, 0, 4, 0.5, 0, 0 ] );
	ztpmv( 'lower', 'no-transpose', 'non-unit', 4, ap, 1, 0, x, 2, 0 );
	ztpsv( 'lower', 'no-transpose', 'non-unit', 4, ap, 1, 0, x, 2, 0 );
	var xv = reinterpret( x, 0 );
	assertClose( xv[ 0 ], 1.0, 1e-14, 'x[0]' );
	assertClose( xv[ 1 ], 0.0, 1e-14, 'x[1]' );
	assertClose( xv[ 4 ], 2.0, 1e-14, 'x[4]' );
	assertClose( xv[ 5 ], 1.0, 1e-14, 'x[5]' );
});

test( 'ztpsv: with non-zero offsetAP and offsetX', function t() {
	// Pad AP with 2 junk elements at the front, offset by 2
	var ap = new Complex128Array( [
		99, 99, 99, 99,
		5, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
	] );
	// Pad x with 1 junk element, offset by 1
	var x = new Complex128Array( [ 99, 99, 3, -1 ] );
	var result = ztpsv( 'upper', 'no-transpose', 'non-unit', 1, ap, 1, 2, x, 1, 1 );
	assert.strictEqual( result, x );
	var xv = reinterpret( x, 0 );
	// (3-i) / (5+2i) = (3-i)(5-2i) / (25+4) = (13-11i) / 29
	assertClose( xv[ 2 ], 13.0 / 29.0, 1e-14, 'x real' );
	assertClose( xv[ 3 ], -11.0 / 29.0, 1e-14, 'x imag' );
});

test( 'ztpsv: upper, transpose, stride 2 (N=4)', function t() {
	var ap = upperAP();
	var x = new Complex128Array( [ 1, 0, 0, 0, 2, 1, 0, 0, 3, -1, 0, 0, 4, 0.5, 0, 0 ] );
	ztpmv( 'upper', 'transpose', 'non-unit', 4, ap, 1, 0, x, 2, 0 );
	ztpsv( 'upper', 'transpose', 'non-unit', 4, ap, 1, 0, x, 2, 0 );
	var xv = reinterpret( x, 0 );
	assertClose( xv[ 0 ], 1.0, 1e-14, 'x[0]' );
	assertClose( xv[ 1 ], 0.0, 1e-14, 'x[1]' );
});

test( 'ztpsv: lower, conjugate-transpose, stride 2 (N=4)', function t() {
	var ap = lowerAP();
	var x = new Complex128Array( [ 1, 0, 0, 0, 2, 1, 0, 0, 3, -1, 0, 0, 4, 0.5, 0, 0 ] );
	ztpmv( 'lower', 'conjugate-transpose', 'non-unit', 4, ap, 1, 0, x, 2, 0 );
	ztpsv( 'lower', 'conjugate-transpose', 'non-unit', 4, ap, 1, 0, x, 2, 0 );
	var xv = reinterpret( x, 0 );
	assertClose( xv[ 0 ], 1.0, 1e-14, 'x[0]' );
	assertClose( xv[ 1 ], 0.0, 1e-14, 'x[1]' );
});
