'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var zlarf = require( './../lib/base.js' );

// Load fixture
var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zlarf.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) {
	return JSON.parse( line );
});

function assertClose( actual, expected, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1e-30 );
	assert.ok( relErr <= 1e-12, msg + ': expected ' + expected + ', got ' + actual + ' (relErr=' + relErr + ')' );
}

function assertArrayClose( actual, expected, label ) {
	var i;
	assert.strictEqual( actual.length, expected.length, label + ' length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], label + '[' + i + ']' );
	}
}

test( 'zlarf: left side, 3x2 matrix', function t() {
	var tc = fixture.find( function f( t ) { return t.name === 'zlarf_left_3x2'; });
	// v = [1+0i, 0.5+0.5i, -0.3+0.2i]
	var v = new Complex128Array( [ 1.0, 0.0, 0.5, 0.5, -0.3, 0.2 ] );
	var tau = new Complex128Array( [ 1.5, 0.3 ] );
	// C is 3x2 col-major: col1=[1+0i,3+1i,5+2i], col2=[2+1i,4-1i,6+0i]
	var C = new Complex128Array( [
		1.0, 0.0,  3.0, 1.0,  5.0, 2.0,   // col 1
		2.0, 1.0,  4.0, -1.0, 6.0, 0.0    // col 2
	]);
	var work = new Complex128Array( 20 );

	// strideC1=1 (row stride in complex elems), strideC2=3 (col stride = LDA=3)
	zlarf( 'L', 3, 2, v, 1, 0, tau, 0, C, 1, 3, 0, work, 1, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.C, 'C' );
});

test( 'zlarf: right side, 2x3 matrix', function t() {
	var tc = fixture.find( function f( t ) { return t.name === 'zlarf_right_2x3'; });
	var v = new Float64Array( [ 1.0, 0.0, 0.5, 0.5, -0.3, 0.2 ] );
	var tau = new Float64Array( [ 1.5, 0.3 ] );
	// C is 2x3 col-major
	var C = new Float64Array( [
		1.0, 0.0,  2.0, 1.0,   // col 1
		3.0, 1.0,  4.0, -1.0,  // col 2
		5.0, 2.0,  6.0, 0.0    // col 3
	]);
	var work = new Float64Array( 20 );

	zlarf( 'R', 2, 3, v, 1, 0, tau, 0, C, 1, 2, 0, work, 1, 0 );
	assertArrayClose( Array.from( C ), tc.C, 'C' );
});

test( 'zlarf: tau=0 (identity)', function t() {
	var tc = fixture.find( function f( t ) { return t.name === 'zlarf_tau_zero'; });
	var v = new Float64Array( [ 1.0, 0.0, 0.5, 0.5 ] );
	var tau = new Float64Array( [ 0.0, 0.0 ] );
	var C = new Float64Array( [
		1.0, 0.0,  2.0, 1.0,
		3.0, 2.0,  4.0, 3.0
	]);
	var work = new Float64Array( 20 );

	zlarf( 'L', 2, 2, v, 1, 0, tau, 0, C, 1, 2, 0, work, 1, 0 );
	assertArrayClose( Array.from( C ), tc.C, 'C' );
});

test( 'zlarf: n=0', function t() {
	var tc = fixture.find( function f( t ) { return t.name === 'zlarf_n_zero'; });
	var v = new Float64Array( [ 1.0, 0.0 ] );
	var tau = new Float64Array( [ 1.0, 0.0 ] );
	var C = new Float64Array( [ 1.0, 0.0 ] );
	var work = new Float64Array( 20 );

	zlarf( 'L', 1, 0, v, 1, 0, tau, 0, C, 1, 1, 0, work, 1, 0 );
	assertArrayClose( Array.from( C ), tc.C, 'C' );
});

test( 'zlarf: v with trailing zeros (lastv reduced)', function t() {
	// v = [1+0i, 0.5+0.5i, 0+0i] — trailing zero should cause lastv=2
	// Only first 2 rows of C should be modified; row 2 stays unchanged.
	var v = new Float64Array( [ 1.0, 0.0, 0.5, 0.5, 0.0, 0.0 ] );
	var tau = new Float64Array( [ 1.5, 0.3 ] );
	var C = new Float64Array( [
		1.0, 0.0,  3.0, 1.0,  5.0, 2.0,
		2.0, 1.0,  4.0, -1.0, 6.0, 0.0
	]);
	var origRow2re0 = C[ 4 ]; // row2, col0
	var origRow2im0 = C[ 5 ];
	var origRow2re1 = C[ 10 ]; // row2, col1
	var origRow2im1 = C[ 11 ];
	var work = new Float64Array( 20 );

	zlarf( 'L', 3, 2, v, 1, 0, tau, 0, C, 1, 3, 0, work, 1, 0 );
	// Row 2 should be unchanged because v[2]=0
	assert.strictEqual( C[ 4 ], origRow2re0 );
	assert.strictEqual( C[ 5 ], origRow2im0 );
	assert.strictEqual( C[ 10 ], origRow2re1 );
	assert.strictEqual( C[ 11 ], origRow2im1 );
});

test( 'zlarf: negative strideV', function t() {
	// Same logical v=[1+0i, 0.5+0.5i] stored reversed with strideV=-1
	// Physical: [-0.3, 0.2, 0.5, 0.5, 1.0, 0.0], offsetV=4, strideV=-1
	// maps to: v[0]=phys[4]=(1,0), v[1]=phys[2]=(0.5,0.5)
	//
	// Use same C as the positive-stride left_3x2 test for comparison.
	// v = [1, 0, 0.5, 0.5, -0.3, 0.2], tau = [1.5, 0.3]
	var vfwd = new Float64Array( [ 1.0, 0.0, 0.5, 0.5, -0.3, 0.2 ] );
	var tau = new Float64Array( [ 1.5, 0.3 ] );
	var C1 = new Float64Array( [
		1.0, 0.0,  3.0, 1.0,  5.0, 2.0,
		2.0, 1.0,  4.0, -1.0, 6.0, 0.0
	]);
	var C2 = new Float64Array( C1 );
	var work1 = new Float64Array( 20 );
	var work2 = new Float64Array( 20 );

	// Forward stride
	zlarf( 'L', 3, 2, vfwd, 1, 0, tau, 0, C1, 1, 3, 0, work1, 1, 0 );

	// Reversed: store v in reverse order, use strideV=-1, offsetV=4
	var vrev = new Float64Array( [ -0.3, 0.2, 0.5, 0.5, 1.0, 0.0 ] );
	zlarf( 'L', 3, 2, vrev, -1, 4, tau, 0, C2, 1, 3, 0, work2, 1, 0 );

	// Should produce same result
	for ( var i = 0; i < C1.length; i++ ) {
		assert.strictEqual( C2[ i ], C1[ i ], 'C[' + i + ']' );
	}
});

test( 'zlarf: left side, 4x3 matrix', function t() {
	var tc = fixture.find( function f( t ) { return t.name === 'zlarf_left_4x3'; });
	var v = new Float64Array( [
		1.0, 0.0,  0.2, 0.3,  -0.5, 0.1,  0.4, -0.6
	]);
	var tau = new Float64Array( [ 1.2, -0.4 ] );
	var C = new Float64Array( [
		1.0, 0.0,   0.0, 1.0,   2.0, -1.0,  3.0, 0.5,
		-1.0, 2.0,  0.5, 0.5,   1.5, -0.5, -2.0, 1.0,
		0.0, 0.0,   1.0, 1.0,  -0.5, 0.0,   2.0, -2.0
	]);
	var work = new Float64Array( 20 );

	zlarf( 'L', 4, 3, v, 1, 0, tau, 0, C, 1, 4, 0, work, 1, 0 );
	assertArrayClose( Array.from( C ), tc.C, 'C' );
});
