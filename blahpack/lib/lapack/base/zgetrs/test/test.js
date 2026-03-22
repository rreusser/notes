

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zgetrf2 = require( './../../zgetrf2/lib/base.js' );
var zgetrs = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zgetrs.jsonl' ), 'utf8' ).trim().split( '\n' );
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
* Factorizes complex matrix A in place and returns info.
* Uses zgetrf2 (recursive LU factorization with partial pivoting).
*/
function factorize( N, A, IPIV ) {
	return zgetrf2( N, N, A, 1, N, 0, IPIV, 1, 0 );
}

/**
* Computes complex matrix-vector product y = A*x (col-major, N x N).
* A and x are Float64Array views of Complex128Arrays (interleaved re/im).
*/
function zmatvec( Av, xv, N ) {
	var yr;
	var yi;
	var ar;
	var ai;
	var xr;
	var xi;
	var y;
	var i;
	var j;
	y = new Float64Array( 2 * N );
	for ( i = 0; i < N; i++ ) {
		yr = 0.0;
		yi = 0.0;
		for ( j = 0; j < N; j++ ) {
			// A(i,j) in col-major: index = i + j*N
			ar = Av[ 2 * ( i + j * N ) ];
			ai = Av[ 2 * ( i + j * N ) + 1 ];
			xr = xv[ 2 * j ];
			xi = xv[ 2 * j + 1 ];
			yr += ar * xr - ai * xi;
			yi += ar * xi + ai * xr;
		}
		y[ 2 * i ] = yr;
		y[ 2 * i + 1 ] = yi;
	}
	return y;
}

/**
* Computes complex matrix-vector product y = A^T * x (col-major, N x N).
* A^T means plain transpose, no conjugation.
*/
function zmatvecT( Av, xv, N ) {
	var yr;
	var yi;
	var ar;
	var ai;
	var xr;
	var xi;
	var y;
	var i;
	var j;
	y = new Float64Array( 2 * N );
	for ( i = 0; i < N; i++ ) {
		yr = 0.0;
		yi = 0.0;
		for ( j = 0; j < N; j++ ) {
			// A^T(i,j) = A(j,i), col-major: A(j,i) at index j + i*N
			ar = Av[ 2 * ( j + i * N ) ];
			ai = Av[ 2 * ( j + i * N ) + 1 ];
			xr = xv[ 2 * j ];
			xi = xv[ 2 * j + 1 ];
			yr += ar * xr - ai * xi;
			yi += ar * xi + ai * xr;
		}
		y[ 2 * i ] = yr;
		y[ 2 * i + 1 ] = yi;
	}
	return y;
}

/**
* Computes complex matrix-vector product y = A^H * x (col-major, N x N).
* A^H means conjugate transpose: conj(A^T).
*/
function zmatvecH( Av, xv, N ) {
	var yr;
	var yi;
	var ar;
	var ai;
	var xr;
	var xi;
	var y;
	var i;
	var j;
	y = new Float64Array( 2 * N );
	for ( i = 0; i < N; i++ ) {
		yr = 0.0;
		yi = 0.0;
		for ( j = 0; j < N; j++ ) {
			// A^H(i,j) = conj(A(j,i)), col-major: A(j,i) at index j + i*N
			ar = Av[ 2 * ( j + i * N ) ];
			ai = -Av[ 2 * ( j + i * N ) + 1 ]; // conjugate
			xr = xv[ 2 * j ];
			xi = xv[ 2 * j + 1 ];
			yr += ar * xr - ai * xi;
			yi += ar * xi + ai * xr;
		}
		y[ 2 * i ] = yr;
		y[ 2 * i + 1 ] = yi;
	}
	return y;
}

/**
* Computes complex matrix-matrix product C = A*B (col-major, N x N times N x NRHS).
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

/**
* Computes complex matrix-matrix product C = A^H * B.
*/
function zmatmatH( Av, Bv, N, nrhs ) {
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
				// A^H(i,k) = conj(A(k,i))
				ar = Av[ 2 * ( k + i * N ) ];
				ai = -Av[ 2 * ( k + i * N ) + 1 ]; // conjugate
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

test( 'zgetrs: solve_3x3 (no-transpose)', function t() {
	var Aorig;
	var IPIV;
	var info;
	var tc;
	var Ax;
	var A;
	var B;

	tc = findCase( 'solve_3x3' );

	// A = [(2+1i) (1+0.5i) (1+0.1i); (4+2i) (3+1i) (3+0.5i); (8+3i) (7+2i) (9+1i)] col-major
	Aorig = new Complex128Array( [
		2, 1, 4, 2, 8, 3,
		1, 0.5, 3, 1, 7, 2,
		1, 0.1, 3, 0.5, 9, 1
	] );
	A = new Complex128Array( Array.from( reinterpret( Aorig, 0 ) ) );
	IPIV = new Int32Array( 3 );
	B = new Complex128Array( [ 1, 0.5, 2, 1, 3, 0 ] );

	factorize( 3, A, IPIV );
	info = zgetrs( 'N', 3, 1, A, 1, 3, 0, IPIV, 1, 0, B, 1, 3, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( Array.from( reinterpret( B, 0 ) ), tc.x, 1e-12, 'x' );

	// Verify A*x = b: multiply original A by solution x (now in B)
	Ax = zmatvec( reinterpret( Aorig, 0 ), reinterpret( B, 0 ), 3 );
	assertArrayClose( Array.from( Ax ), [ 1, 0.5, 2, 1, 3, 0 ], 1e-12, 'A*x=b' );
});

test( 'zgetrs: solve_3x3_trans (transpose)', function t() {
	var Aorig;
	var IPIV;
	var info;
	var tc;
	var ATx;
	var A;
	var B;

	tc = findCase( 'solve_3x3_trans' );

	Aorig = new Complex128Array( [
		2, 1, 4, 2, 8, 3,
		1, 0.5, 3, 1, 7, 2,
		1, 0.1, 3, 0.5, 9, 1
	] );
	A = new Complex128Array( Array.from( reinterpret( Aorig, 0 ) ) );
	IPIV = new Int32Array( 3 );
	B = new Complex128Array( [ 1, 0.5, 2, 1, 3, 0 ] );

	factorize( 3, A, IPIV );
	info = zgetrs( 'T', 3, 1, A, 1, 3, 0, IPIV, 1, 0, B, 1, 3, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( Array.from( reinterpret( B, 0 ) ), tc.x, 1e-12, 'x' );

	// Verify A^T * x = b
	ATx = zmatvecT( reinterpret( Aorig, 0 ), reinterpret( B, 0 ), 3 );
	assertArrayClose( Array.from( ATx ), [ 1, 0.5, 2, 1, 3, 0 ], 1e-12, 'A^T*x=b' );
});

test( 'zgetrs: solve_3x3_conj (conjugate transpose)', function t() {
	var Aorig;
	var IPIV;
	var info;
	var tc;
	var AHx;
	var A;
	var B;

	tc = findCase( 'solve_3x3_conj' );

	Aorig = new Complex128Array( [
		2, 1, 4, 2, 8, 3,
		1, 0.5, 3, 1, 7, 2,
		1, 0.1, 3, 0.5, 9, 1
	] );
	A = new Complex128Array( Array.from( reinterpret( Aorig, 0 ) ) );
	IPIV = new Int32Array( 3 );
	B = new Complex128Array( [ 1, 0.5, 2, 1, 3, 0 ] );

	factorize( 3, A, IPIV );
	info = zgetrs( 'C', 3, 1, A, 1, 3, 0, IPIV, 1, 0, B, 1, 3, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( Array.from( reinterpret( B, 0 ) ), tc.x, 1e-12, 'x' );

	// Verify A^H * x = b
	AHx = zmatvecH( reinterpret( Aorig, 0 ), reinterpret( B, 0 ), 3 );
	assertArrayClose( Array.from( AHx ), [ 1, 0.5, 2, 1, 3, 0 ], 1e-12, 'A^H*x=b' );
});

test( 'zgetrs: multi_rhs (NRHS=2, no-transpose)', function t() {
	var Borig;
	var Aorig;
	var IPIV;
	var info;
	var tc;
	var AB;
	var A;
	var B;

	tc = findCase( 'multi_rhs' );

	Aorig = new Complex128Array( [
		2, 1, 4, 2, 8, 3,
		1, 0.5, 3, 1, 7, 2,
		1, 0.1, 3, 0.5, 9, 1
	] );
	A = new Complex128Array( Array.from( reinterpret( Aorig, 0 ) ) );
	IPIV = new Int32Array( 3 );

	// B is 3x2 col-major: b1 = [1+0i; 0+0i; 0+0i], b2 = [0+0i; 1+0i; 0+0i]
	Borig = new Complex128Array( [ 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0 ] );
	B = new Complex128Array( Array.from( reinterpret( Borig, 0 ) ) );

	factorize( 3, A, IPIV );
	info = zgetrs( 'N', 3, 2, A, 1, 3, 0, IPIV, 1, 0, B, 1, 3, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( Array.from( reinterpret( B, 0 ) ), tc.x, 1e-12, 'x' );

	// Verify A*X = B_original
	AB = zmatmat( reinterpret( Aorig, 0 ), reinterpret( B, 0 ), 3, 2 );
	assertArrayClose( Array.from( AB ), Array.from( reinterpret( Borig, 0 ) ), 1e-12, 'A*X=B' );
});

test( 'zgetrs: n_zero (quick return)', function t() {
	var IPIV;
	var info;
	var tc;
	var A;
	var B;

	tc = findCase( 'n_zero' );

	A = new Complex128Array( 1 );
	IPIV = new Int32Array( 1 );
	B = new Complex128Array( 1 );

	info = zgetrs( 'N', 0, 1, A, 1, 1, 0, IPIV, 1, 0, B, 1, 1, 0 );

	assert.equal( info, tc.info, 'info' );
});

test( 'zgetrs: nrhs_zero (quick return)', function t() {
	var IPIV;
	var info;
	var tc;
	var A;
	var B;

	tc = findCase( 'nrhs_zero' );

	A = new Complex128Array( 9 );
	IPIV = new Int32Array( 3 );
	B = new Complex128Array( 3 );

	info = zgetrs( 'N', 3, 0, A, 1, 3, 0, IPIV, 1, 0, B, 1, 3, 0 );

	assert.equal( info, tc.info, 'info' );
});

test( 'zgetrs: 1x1', function t() {
	var IPIV;
	var info;
	var tc;
	var A;
	var B;

	tc = findCase( '1x1' );

	// (5+3i)*x = (10+6i) => x = 2+0i
	A = new Complex128Array( [ 5, 3 ] );
	IPIV = new Int32Array( 1 );
	B = new Complex128Array( [ 10, 6 ] );

	factorize( 1, A, IPIV );
	info = zgetrs( 'N', 1, 1, A, 1, 1, 0, IPIV, 1, 0, B, 1, 1, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( Array.from( reinterpret( B, 0 ) ), tc.x, 1e-14, 'x' );
});

test( 'zgetrs: identity', function t() {
	var IPIV;
	var info;
	var tc;
	var A;
	var B;

	tc = findCase( 'identity' );

	// 3x3 identity matrix, col-major
	A = new Complex128Array( [ 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0 ] );
	IPIV = new Int32Array( 3 );
	B = new Complex128Array( [ 3, 1, 5, 2, 7, 3 ] );

	factorize( 3, A, IPIV );
	info = zgetrs( 'N', 3, 1, A, 1, 3, 0, IPIV, 1, 0, B, 1, 3, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( Array.from( reinterpret( B, 0 ) ), tc.x, 1e-14, 'x' );
});

test( 'zgetrs: multi_rhs_conj (NRHS=2, conjugate transpose)', function t() {
	var Borig;
	var Aorig;
	var IPIV;
	var info;
	var tc;
	var AHB;
	var A;
	var B;

	tc = findCase( 'multi_rhs_conj' );

	Aorig = new Complex128Array( [
		2, 1, 4, 2, 8, 3,
		1, 0.5, 3, 1, 7, 2,
		1, 0.1, 3, 0.5, 9, 1
	] );
	A = new Complex128Array( Array.from( reinterpret( Aorig, 0 ) ) );
	IPIV = new Int32Array( 3 );

	// B is 3x2 col-major: b1 = [1+0i; 0+0i; 0+0i], b2 = [0+0i; 1+0i; 0+0i]
	Borig = new Complex128Array( [ 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0 ] );
	B = new Complex128Array( Array.from( reinterpret( Borig, 0 ) ) );

	factorize( 3, A, IPIV );
	info = zgetrs( 'C', 3, 2, A, 1, 3, 0, IPIV, 1, 0, B, 1, 3, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( Array.from( reinterpret( B, 0 ) ), tc.x, 1e-12, 'x' );

	// Verify A^H * X = B_original
	AHB = zmatmatH( reinterpret( Aorig, 0 ), reinterpret( B, 0 ), 3, 2 );
	assertArrayClose( Array.from( AHB ), Array.from( reinterpret( Borig, 0 ) ), 1e-12, 'A^H*X=B' );
});

test( 'zgetrs: lowercase trans argument', function t() {
	var Aorig;
	var IPIV;
	var info;
	var Ax;
	var A;
	var B;

	// Same as solve_3x3 but with lowercase 'n'
	Aorig = new Complex128Array( [
		2, 1, 4, 2, 8, 3,
		1, 0.5, 3, 1, 7, 2,
		1, 0.1, 3, 0.5, 9, 1
	] );
	A = new Complex128Array( Array.from( reinterpret( Aorig, 0 ) ) );
	IPIV = new Int32Array( 3 );
	B = new Complex128Array( [ 1, 0.5, 2, 1, 3, 0 ] );

	factorize( 3, A, IPIV );
	info = zgetrs( 'n', 3, 1, A, 1, 3, 0, IPIV, 1, 0, B, 1, 3, 0 );

	assert.equal( info, 0, 'info' );
	Ax = zmatvec( reinterpret( Aorig, 0 ), reinterpret( B, 0 ), 3 );
	assertArrayClose( Array.from( Ax ), [ 1, 0.5, 2, 1, 3, 0 ], 1e-12, 'A*x=b' );
});
