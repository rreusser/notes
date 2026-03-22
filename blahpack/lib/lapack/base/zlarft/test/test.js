'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
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
	var V = new Float64Array( [
		// col 1: V(0,0)=1, V(1,0)=0.3+0.2i, V(2,0)=-0.5+0.1i, V(3,0)=0.4-0.3i
		1.0, 0.0,  0.3, 0.2,  -0.5, 0.1,  0.4, -0.3,
		// col 2: V(0,1)=0, V(1,1)=1, V(2,1)=0.6-0.4i, V(3,1)=-0.2+0.5i
		0.0, 0.0,  1.0, 0.0,  0.6, -0.4,  -0.2, 0.5
	]);
	var tau = new Float64Array( [ 1.2, -0.3,  1.5, 0.4 ] );
	// T is 3x2 in Fortran (LDT=3), but only 2x2 is used. We store 3x2 col-major interleaved.
	var T = new Float64Array( 12 );

	// strideV1=1 (rows), strideV2=4 (cols with LDV=4)
	// strideT1=1, strideT2=3 (LDT=3)
	// strideTAU=1
	zlarft( 'F', 'C', 4, 2, V, 1, 4, 0, tau, 1, 0, T, 1, 3, 0 );

	// Compare first 12 doubles (3x2 col-major = 6 complex values)
	assertArrayClose( Array.from( T ), tc.T, 'T' );
});

test( 'zlarft: forward, columnwise, n=5, k=3', function t() {
	var tc = fixture.find( function f( t ) { return t.name === 'zlarft_fwd_col_5x3'; });
	// V is 5x3, LDV=5
	var V = new Float64Array( [
		// col 1
		1.0, 0.0,  0.2, 0.1,  -0.3, 0.4,  0.5, -0.2,  0.1, 0.6,
		// col 2
		0.0, 0.0,  1.0, 0.0,  0.4, -0.5,  -0.1, 0.3,  0.7, 0.2,
		// col 3
		0.0, 0.0,  0.0, 0.0,  1.0, 0.0,  0.3, 0.1,  -0.2, 0.8
	]);
	var tau = new Float64Array( [ 1.1, 0.2,  1.3, -0.1,  1.6, 0.5 ] );
	// T is 3x3, LDT=3
	var T = new Float64Array( 18 );

	zlarft( 'F', 'C', 5, 3, V, 1, 5, 0, tau, 1, 0, T, 1, 3, 0 );
	assertArrayClose( Array.from( T ), tc.T, 'T' );
});

test( 'zlarft: tau(0)=0 (first reflector is identity)', function t() {
	var tc = fixture.find( function f( t ) { return t.name === 'zlarft_tau_zero'; });
	var V = new Float64Array( [
		1.0, 0.0,  0.3, 0.2,  -0.5, 0.1,  0.4, -0.3,
		0.0, 0.0,  1.0, 0.0,  0.6, -0.4,  -0.2, 0.5
	]);
	var tau = new Float64Array( [ 0.0, 0.0,  1.5, 0.4 ] );
	var T = new Float64Array( 12 );

	zlarft( 'F', 'C', 4, 2, V, 1, 4, 0, tau, 1, 0, T, 1, 3, 0 );
	assertArrayClose( Array.from( T ), tc.T, 'T' );
});

test( 'zlarft: backward, columnwise, n=4, k=2', function t() {
	var tc = fixture.find( function f( t ) { return t.name === 'zlarft_bwd_col'; });
	// V for backward: last K rows have unit upper triangular
	var V = new Float64Array( [
		// col 1: V(0,0)=0.3+0.2i, V(1,0)=-0.5+0.1i, V(2,0)=1, V(3,0)=0
		0.3, 0.2,  -0.5, 0.1,  1.0, 0.0,  0.0, 0.0,
		// col 2: V(0,1)=0.6-0.4i, V(1,1)=-0.2+0.5i, V(2,1)=0.4-0.3i, V(3,1)=1
		0.6, -0.4,  -0.2, 0.5,  0.4, -0.3,  1.0, 0.0
	]);
	var tau = new Float64Array( [ 1.2, -0.3,  1.5, 0.4 ] );
	var T = new Float64Array( 12 );

	zlarft( 'B', 'C', 4, 2, V, 1, 4, 0, tau, 1, 0, T, 1, 3, 0 );
	assertArrayClose( Array.from( T ), tc.T, 'T' );
});
