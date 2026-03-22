'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var zlarft = require( './../lib/base.js' );

// Load fixture
var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zlarft.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'zlarft: forward, columnwise, n=4, k=2', function t() {
	var tc = fixture.find( function f( t ) { return t.name === 'zlarft_fwd_col'; });
	// V is 4x2 (LDV=4), col-major interleaved
	var V = new Complex128Array( [
		// col 1: V(0,0)=1, V(1,0)=0.3+0.2i, V(2,0)=-0.5+0.1i, V(3,0)=0.4-0.3i
		1.0, 0.0,  0.3, 0.2,  -0.5, 0.1,  0.4, -0.3,
		// col 2: V(0,1)=0, V(1,1)=1, V(2,1)=0.6-0.4i, V(3,1)=-0.2+0.5i
		0.0, 0.0,  1.0, 0.0,  0.6, -0.4,  -0.2, 0.5
	]);
	var tau = new Complex128Array( [ 1.2, -0.3,  1.5, 0.4 ] );
	// T is 3x2 in Fortran (LDT=3), but only 2x2 is used. We store 3x2 col-major interleaved.
	var T = new Complex128Array( 6 );

	// strideV1=1 (rows), strideV2=4 (cols with LDV=4)
	// strideT1=1, strideT2=3 (LDT=3)
	// strideTAU=1
	zlarft( 'F', 'C', 4, 2, V, 1, 4, 0, tau, 1, 0, T, 1, 3, 0 );

	// Compare first 12 doubles (3x2 col-major = 6 complex values)
	assertArrayClose( Array.from( reinterpret( T, 0 ) ), tc.T, 'T' );
});

test( 'zlarft: forward, columnwise, n=5, k=3', function t() {
	var tc = fixture.find( function f( t ) { return t.name === 'zlarft_fwd_col_5x3'; });
	// V is 5x3, LDV=5
	var V = new Complex128Array( [
		// col 1
		1.0, 0.0,  0.2, 0.1,  -0.3, 0.4,  0.5, -0.2,  0.1, 0.6,
		// col 2
		0.0, 0.0,  1.0, 0.0,  0.4, -0.5,  -0.1, 0.3,  0.7, 0.2,
		// col 3
		0.0, 0.0,  0.0, 0.0,  1.0, 0.0,  0.3, 0.1,  -0.2, 0.8
	]);
	var tau = new Complex128Array( [ 1.1, 0.2,  1.3, -0.1,  1.6, 0.5 ] );
	// T is 3x3, LDT=3
	var T = new Complex128Array( 9 );

	zlarft( 'F', 'C', 5, 3, V, 1, 5, 0, tau, 1, 0, T, 1, 3, 0 );
	assertArrayClose( Array.from( reinterpret( T, 0 ) ), tc.T, 'T' );
});

test( 'zlarft: tau(0)=0 (first reflector is identity)', function t() {
	var tc = fixture.find( function f( t ) { return t.name === 'zlarft_tau_zero'; });
	var V = new Complex128Array( [
		1.0, 0.0,  0.3, 0.2,  -0.5, 0.1,  0.4, -0.3,
		0.0, 0.0,  1.0, 0.0,  0.6, -0.4,  -0.2, 0.5
	]);
	var tau = new Complex128Array( [ 0.0, 0.0,  1.5, 0.4 ] );
	var T = new Complex128Array( 6 );

	zlarft( 'F', 'C', 4, 2, V, 1, 4, 0, tau, 1, 0, T, 1, 3, 0 );
	assertArrayClose( Array.from( reinterpret( T, 0 ) ), tc.T, 'T' );
});

test( 'zlarft: backward, columnwise, n=4, k=2', function t() {
	var tc = fixture.find( function f( t ) { return t.name === 'zlarft_bwd_col'; });
	// V for backward: last K rows have unit upper triangular
	var V = new Complex128Array( [
		// col 1: V(0,0)=0.3+0.2i, V(1,0)=-0.5+0.1i, V(2,0)=1, V(3,0)=0
		0.3, 0.2,  -0.5, 0.1,  1.0, 0.0,  0.0, 0.0,
		// col 2: V(0,1)=0.6-0.4i, V(1,1)=-0.2+0.5i, V(2,1)=0.4-0.3i, V(3,1)=1
		0.6, -0.4,  -0.2, 0.5,  0.4, -0.3,  1.0, 0.0
	]);
	var tau = new Complex128Array( [ 1.2, -0.3,  1.5, 0.4 ] );
	var T = new Complex128Array( 6 );

	zlarft( 'B', 'C', 4, 2, V, 1, 4, 0, tau, 1, 0, T, 1, 3, 0 );
	assertArrayClose( Array.from( reinterpret( T, 0 ) ), tc.T, 'T' );
});

test( 'zlarft: N=0 quick return', function t() {
	var V = new Complex128Array( 8 );
	var tau = new Complex128Array( 2 );
	var T = new Complex128Array( 6 );
	var Tv = reinterpret( T, 0 );
	Tv[ 0 ] = 99.0; // sentinel value
	zlarft( 'F', 'C', 0, 2, V, 1, 4, 0, tau, 1, 0, T, 1, 3, 0 );
	// T should be unchanged
	Tv = reinterpret( T, 0 );
	assert.strictEqual( Tv[ 0 ], 99.0 );
});

test( 'zlarft: forward, rowwise, n=4, k=2', function t() {
	var tc = fixture.find( function f( t ) { return t.name === 'zlarft_fwd_row'; });
	// V is 2x4 (LDV=2), row-major reflectors (unit upper triangular in V1)
	var V = new Complex128Array( [
		1.0, 0.0,  0.0, 0.0,
		0.3, 0.2,  1.0, 0.0,
		-0.5, 0.1,  0.6, -0.4,
		0.4, -0.3,  -0.2, 0.5
	]);
	var tau = new Complex128Array( [ 1.2, -0.3,  1.5, 0.4 ] );
	var T = new Complex128Array( 6 );

	zlarft( 'F', 'R', 4, 2, V, 1, 2, 0, tau, 1, 0, T, 1, 3, 0 );
	assertArrayClose( Array.from( reinterpret( T, 0 ) ), tc.T, 'T' );
});

test( 'zlarft: backward, rowwise, n=4, k=2', function t() {
	var tc = fixture.find( function f( t ) { return t.name === 'zlarft_bwd_row'; });
	var V = new Complex128Array( [
		0.3, 0.2,  0.6, -0.4,
		-0.5, 0.1,  -0.2, 0.5,
		1.0, 0.0,  0.4, -0.3,
		0.0, 0.0,  1.0, 0.0
	]);
	var tau = new Complex128Array( [ 1.2, -0.3,  1.5, 0.4 ] );
	var T = new Complex128Array( 6 );

	zlarft( 'B', 'R', 4, 2, V, 1, 2, 0, tau, 1, 0, T, 1, 3, 0 );
	assertArrayClose( Array.from( reinterpret( T, 0 ) ), tc.T, 'T' );
});

test( 'zlarft: backward, tau(1)=0 (second reflector is identity)', function t() {
	// Same V as backward test, but tau(1)=0
	// Expected: T(:,1) column should be all zeros, T(0,0) = tau(0)
	var V = new Complex128Array( [
		0.3, 0.2,  -0.5, 0.1,  1.0, 0.0,  0.0, 0.0,
		0.6, -0.4,  -0.2, 0.5,  0.4, -0.3,  1.0, 0.0
	]);
	var tau = new Complex128Array( [ 1.2, -0.3,  0.0, 0.0 ] );
	var T = new Complex128Array( 6 );

	zlarft( 'B', 'C', 4, 2, V, 1, 4, 0, tau, 1, 0, T, 1, 3, 0 );

	var Tv = reinterpret( T, 0 );
	// T(1,1) column should be zero (tau=0 path)
	assert.strictEqual( Tv[ 8 ], 0.0, 'T(1,1) re' );  // T[1*st1 + 1*st2] = T[1*2 + 1*6] = T[8]
	assert.strictEqual( Tv[ 9 ], 0.0, 'T(1,1) im' );
	// T(0,0) should be tau(0)
	assert.strictEqual( Tv[ 0 ], 1.2, 'T(0,0) re' );
	assert.strictEqual( Tv[ 1 ], -0.3, 'T(0,0) im' );
});

test( 'zlarft: backward, K=3 with leading zeros in V', function t() {
	var V = new Complex128Array( [
		0.2, 0.1,  0.4, -0.2,  -0.3, 0.5,  1.0, 0.0,  0.0, 0.0,  0.0, 0.0,
		0.0, 0.0,  0.5, 0.3,  -0.1, 0.4,  0.3, -0.1,  1.0, 0.0,  0.0, 0.0,
		0.6, -0.2,  -0.2, 0.1,  0.4, 0.3,  -0.5, 0.2,  0.1, -0.3,  1.0, 0.0
	]);
	var tau = new Complex128Array( [ 1.1, 0.2,  1.3, -0.1,  1.6, 0.5 ] );
	// T is 3x3, LDT=3
	var T = new Complex128Array( 9 );

	zlarft( 'B', 'C', 6, 3, V, 1, 6, 0, tau, 1, 0, T, 1, 3, 0 );

	var Tv = reinterpret( T, 0 );
	// Verify T is lower triangular and diagonals are tau values
	// T(0,0) = tau(0) = 1.1+0.2i
	assert.strictEqual( Tv[ 0 ], 1.1, 'T(0,0) re = tau(0) re' );
	assert.strictEqual( Tv[ 1 ], 0.2, 'T(0,0) im = tau(0) im' );
	// T(1,1) = tau(1) = 1.3-0.1i
	var idx11 = 2 + 6; // 1*st1 + 1*st2 = 1*2 + 1*6 = 8
	assert.strictEqual( Tv[ idx11 ], 1.3, 'T(1,1) re = tau(1) re' );
	assert.strictEqual( Tv[ idx11 + 1 ], -0.1, 'T(1,1) im = tau(1) im' );
	// T(2,2) = tau(2) = 1.6+0.5i
	var idx22 = 4 + 12; // 2*st1 + 2*st2 = 2*2 + 2*6 = 16
	assert.strictEqual( Tv[ idx22 ], 1.6, 'T(2,2) re = tau(2) re' );
	assert.strictEqual( Tv[ idx22 + 1 ], 0.5, 'T(2,2) im = tau(2) im' );
	// Upper triangle should be zero (backward → lower triangular T)
	// T(0,1) should be 0
	var idx01 = 0 + 6; // 0*st1 + 1*st2 = 6
	assert.strictEqual( Tv[ idx01 ], 0.0, 'T(0,1) re = 0' );
	assert.strictEqual( Tv[ idx01 + 1 ], 0.0, 'T(0,1) im = 0' );
});

test( 'zlarft: forward with trailing zeros in V column', function t() {
	// V is 4x2 where column 0 has trailing zeros (V(3,0)=0)
	var V = new Complex128Array( [
		1.0, 0.0,  0.3, 0.2,  -0.5, 0.1,  0.0, 0.0,   // col 0: last elem = 0
		0.0, 0.0,  1.0, 0.0,  0.6, -0.4,  -0.2, 0.5    // col 1: normal
	]);
	var tau = new Complex128Array( [ 1.2, -0.3,  1.5, 0.4 ] );
	var T = new Complex128Array( 6 );

	zlarft( 'F', 'C', 4, 2, V, 1, 4, 0, tau, 1, 0, T, 1, 3, 0 );

	var Tv = reinterpret( T, 0 );
	// T(0,0) should be tau(0)
	assert.strictEqual( Tv[ 0 ], 1.2, 'T(0,0) re = tau(0) re' );
	assert.strictEqual( Tv[ 1 ], -0.3, 'T(0,0) im = tau(0) im' );
	// T(1,1) should be tau(1)
	var t11 = 2 + 6; // index: 1*st1 + 1*st2 = 1*2 + 1*6 = 8
	assert.strictEqual( Tv[ t11 ], 1.5, 'T(1,1) re = tau(1) re' );
	assert.strictEqual( Tv[ t11 + 1 ], 0.4, 'T(1,1) im = tau(1) im' );
});
