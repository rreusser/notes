

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zgesv = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zgesv.jsonl' ), 'utf8' ).trim().split( '\n' );
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

/**
* Computes complex matrix-matrix product C = A*B (col-major, N x N times N x NRHS).
* A and B are Float64Array views of Complex128Arrays (interleaved re/im).
*/
function zmatmat( Av, Bv, N, nrhs ) {
	var cr;
	var ci;
	var ar;
	var ai;
	var br;
	var bi;
	var C;
	var i;
	var j;
	var k;
	C = new Float64Array( 2 * N * nrhs );
	for ( j = 0; j < nrhs; j++ ) {
		for ( i = 0; i < N; i++ ) {
			cr = 0.0;
			ci = 0.0;
			for ( k = 0; k < N; k++ ) {
				ar = Av[ 2 * ( i + k * N ) ];
				ai = Av[ 2 * ( i + k * N ) + 1 ];
				br = Bv[ 2 * ( k + j * N ) ];
				bi = Bv[ 2 * ( k + j * N ) + 1 ];
				cr += ar * br - ai * bi;
				ci += ar * bi + ai * br;
			}
			C[ 2 * ( i + j * N ) ] = cr;
			C[ 2 * ( i + j * N ) + 1 ] = ci;
		}
	}
	return C;
}


// TESTS //

test( 'zgesv: solve_3x3', function t() {
	var Aorig;
	var Borig;
	var IPIV;
	var info;
	var tc;
	var AB;
	var A;
	var B;

	tc = findCase( 'solve_3x3' );

	// A = [(2+1i) (1+0.5i) (0.5+0.1i);
	//      (1-1i) (4+2i)   (1+0.3i);
	//      (0.5+0.2i) (1-0.5i) (3+1i)] col-major
	Aorig = new Complex128Array( [
		2, 1, 1, -1, 0.5, 0.2,
		1, 0.5, 4, 2, 1, -0.5,
		0.5, 0.1, 1, 0.3, 3, 1
	] );
	A = new Complex128Array( Array.from( reinterpret( Aorig, 0 ) ) );
	IPIV = new Int32Array( 3 );

	// b = A * [1+0i; 1+0i; 1+0i]
	Borig = new Complex128Array( [ 3.5, 1.6, 6.0, 1.3, 4.5, 0.7 ] );
	B = new Complex128Array( Array.from( reinterpret( Borig, 0 ) ) );

	info = zgesv( 3, 1, A, 1, 3, 0, IPIV, 1, 0, B, 1, 3, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( Array.from( reinterpret( B, 0 ) ), tc.x, 1e-14, 'x' );

	// Verify A_orig * x = b_orig
	AB = zmatmat( reinterpret( Aorig, 0 ), reinterpret( B, 0 ), 3, 1 );
	assertArrayClose( Array.from( AB ), Array.from( reinterpret( Borig, 0 ) ), 1e-14, 'A*x=b' );
});

test( 'zgesv: multi_rhs', function t() {
	var Aorig;
	var Borig;
	var IPIV;
	var info;
	var tc;
	var AB;
	var A;
	var B;

	tc = findCase( 'multi_rhs' );

	// A = [(3+1i) (1-1i); (2+0.5i) (5+2i)] col-major
	Aorig = new Complex128Array( [
		3, 1, 2, 0.5,
		1, -1, 5, 2
	] );
	A = new Complex128Array( Array.from( reinterpret( Aorig, 0 ) ) );
	IPIV = new Int32Array( 2 );

	// B col-major: [(3+1i) (0+2i); (2+0.5i) (4.5+4i)]
	Borig = new Complex128Array( [
		3, 1, 2, 0.5,
		0, 2, 4.5, 4
	] );
	B = new Complex128Array( Array.from( reinterpret( Borig, 0 ) ) );

	info = zgesv( 2, 2, A, 1, 2, 0, IPIV, 1, 0, B, 1, 2, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( Array.from( reinterpret( B, 0 ) ), tc.x, 1e-14, 'x' );

	// Verify A_orig * X = B_orig
	AB = zmatmat( reinterpret( Aorig, 0 ), reinterpret( B, 0 ), 2, 2 );
	assertArrayClose( Array.from( AB ), Array.from( reinterpret( Borig, 0 ) ), 1e-14, 'A*X=B' );
});

test( 'zgesv: singular', function t() {
	var IPIV;
	var info;
	var tc;
	var A;
	var B;

	tc = findCase( 'singular' );

	// Singular 3x3: each column is a multiple of [1;2;3]
	A = new Complex128Array( [
		1, 0, 2, 0, 3, 0,
		2, 0, 4, 0, 6, 0,
		3, 0, 6, 0, 9, 0
	] );
	IPIV = new Int32Array( 3 );
	B = new Complex128Array( [ 1, 0, 2, 0, 3, 0 ] );

	info = zgesv( 3, 1, A, 1, 3, 0, IPIV, 1, 0, B, 1, 3, 0 );

	// Singular matrix: info > 0
	assert.ok( info > 0, 'info > 0 for singular matrix' );
});

test( 'zgesv: n_zero', function t() {
	var IPIV;
	var info;
	var tc;
	var A;
	var B;

	tc = findCase( 'n_zero' );

	A = new Complex128Array( 1 );
	IPIV = new Int32Array( 1 );
	B = new Complex128Array( 1 );

	info = zgesv( 0, 1, A, 1, 1, 0, IPIV, 1, 0, B, 1, 1, 0 );

	assert.equal( info, tc.info, 'info' );
});

test( 'zgesv: nrhs_zero', function t() {
	var IPIV;
	var info;
	var tc;
	var A;
	var B;

	tc = findCase( 'nrhs_zero' );

	A = new Complex128Array( [ 5, 1 ] );
	IPIV = new Int32Array( 1 );
	B = new Complex128Array( 1 );

	info = zgesv( 1, 0, A, 1, 1, 0, IPIV, 1, 0, B, 1, 1, 0 );

	assert.equal( info, tc.info, 'info' );
});

test( 'zgesv: 1x1', function t() {
	var IPIV;
	var info;
	var tc;
	var A;
	var B;

	tc = findCase( '1x1' );

	// (5+2i)*x = (10+4i) => x = 2+0i
	A = new Complex128Array( [ 5, 2 ] );
	IPIV = new Int32Array( 1 );
	B = new Complex128Array( [ 10, 4 ] );

	info = zgesv( 1, 1, A, 1, 1, 0, IPIV, 1, 0, B, 1, 1, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( Array.from( reinterpret( B, 0 ) ), tc.x, 1e-14, 'x' );
});

test( 'zgesv: 4x4', function t() {
	var Aorig;
	var Borig;
	var IPIV;
	var info;
	var tc;
	var AB;
	var A;
	var B;

	tc = findCase( '4x4' );

	// 4x4 diagonally dominant complex matrix, col-major
	Aorig = new Complex128Array( [
		10, 1, 1, 2, 2, -1, 3, 0.5,
		1, -1, 12, 2, 1, 3, 2, -0.5,
		2, 0.5, 3, -1, 15, 1, 1, 2,
		1, 1, 2, 0.5, 3, -2, 20, 3
	] );
	A = new Complex128Array( Array.from( reinterpret( Aorig, 0 ) ) );
	IPIV = new Int32Array( 4 );

	// b = A * [1+1i; 2-1i; -1+2i; 3+0i] (computed by Fortran)
	// Compute b from the known x and A
	var Av = reinterpret( Aorig, 0 );
	// x = [1+1i, 2-1i, -1+2i, 3+0i]
	var xv = new Float64Array( [ 1, 1, 2, -1, -1, 2, 3, 0 ] );
	var bv = zmatmat( Av, xv, 4, 1 );
	Borig = new Complex128Array( bv.buffer );
	B = new Complex128Array( Array.from( bv ) );

	info = zgesv( 4, 1, A, 1, 4, 0, IPIV, 1, 0, B, 1, 4, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( Array.from( reinterpret( B, 0 ) ), tc.x, 1e-13, 'x' );

	// Verify A_orig * x = b_orig
	AB = zmatmat( reinterpret( Aorig, 0 ), reinterpret( B, 0 ), 4, 1 );
	assertArrayClose( Array.from( AB ), Array.from( bv ), 1e-13, 'A*x=b' );
});
