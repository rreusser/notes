

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
* Computes complex matrix-vector product b = A*x (col-major).
* All arrays are Float64 interleaved [re, im, re, im, ...].
*
* @param {Float64Array} A - N*N complex matrix (2*N*N doubles, col-major)
* @param {Float64Array} x - N complex vector (2*N doubles)
* @param {number} N - dimension
* @param {number} nrhs - number of right-hand sides
* @returns {Float64Array} b - N*nrhs complex result (2*N*nrhs doubles)
*/
function zmatmat( A, x, N, nrhs ) {
	var b = new Float64Array( 2 * N * nrhs );
	var are;
	var aim;
	var xre;
	var xim;
	var i;
	var j;
	var k;
	for ( j = 0; j < nrhs; j++ ) {
		for ( i = 0; i < N; i++ ) {
			for ( k = 0; k < N; k++ ) {
				// A[i,k] col-major: index i + k*N, each complex = 2 doubles
				are = A[ 2 * ( i + k * N ) ];
				aim = A[ 2 * ( i + k * N ) + 1 ];
				xre = x[ 2 * ( k + j * N ) ];
				xim = x[ 2 * ( k + j * N ) + 1 ];
				// (are + i*aim) * (xre + i*xim)
				b[ 2 * ( i + j * N ) ] += are * xre - aim * xim;
				b[ 2 * ( i + j * N ) + 1 ] += are * xim + aim * xre;
			}
		}
	}
	return b;
}


// TESTS //

test( 'zgesv: solve_3x3', function t() {
	var Aorig;
	var Borig;
	var IPIV;
	var info;
	var view;
	var tc;
	var AB;
	var A;
	var B;

	tc = findCase( 'solve_3x3' );

	// A = [(2+1i) (1+0.5i) (0.5+0.1i);
	//      (1-1i) (4+2i)   (1+0.3i);
	//      (0.5+0.2i) (1-0.5i) (3+1i)] col-major
	A = new Complex128Array( [
		2.0, 1.0, 1.0, -1.0, 0.5, 0.2,
		1.0, 0.5, 4.0, 2.0, 1.0, -0.5,
		0.5, 0.1, 1.0, 0.3, 3.0, 1.0
	] );
	Aorig = new Float64Array( reinterpret( A, 0 ) );

	// b = A * [1; 1; 1] = sum of each row
	B = new Complex128Array( 3 );
	Borig = reinterpret( B, 0 );

	// Compute b = A * [1+0i; 1+0i; 1+0i]
	var bview = zmatmat( Aorig, new Float64Array( [ 1, 0, 1, 0, 1, 0 ] ), 3, 1 );
	Borig.set( bview );
	Borig = new Float64Array( Borig );

	IPIV = new Int32Array( 3 );

	info = zgesv( 3, 1, A, 1, 3, 0, IPIV, 1, 0, B, 1, 3, 0 );
	view = reinterpret( B, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( Array.from( view ), tc.x, 1e-13, 'x' );

	// Verify A_orig * x ≈ b_orig
	AB = zmatmat( Aorig, Array.from( view ), 3, 1 );
	assertArrayClose( Array.from( AB ), Array.from( Borig ), 1e-13, 'A*x=b' );
});

test( 'zgesv: multi_rhs', function t() {
	var Aorig;
	var Borig;
	var IPIV;
	var info;
	var view;
	var tc;
	var AB;
	var A;
	var B;

	tc = findCase( 'multi_rhs' );

	// A = [(3+1i) (1-1i); (2+0.5i) (5+2i)] col-major
	A = new Complex128Array( [
		3.0, 1.0, 2.0, 0.5,
		1.0, -1.0, 5.0, 2.0
	] );
	Aorig = new Float64Array( reinterpret( A, 0 ) );
	IPIV = new Int32Array( 2 );

	// B col-major: b1 = [(3+1i); (2+0.5i)], b2 = [(0+2i); (4.5+4i)]
	B = new Complex128Array( [
		3.0, 1.0, 2.0, 0.5,
		0.0, 2.0, 4.5, 4.0
	] );
	Borig = new Float64Array( reinterpret( B, 0 ) );

	info = zgesv( 2, 2, A, 1, 2, 0, IPIV, 1, 0, B, 1, 2, 0 );
	view = reinterpret( B, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( Array.from( view ), tc.x, 1e-13, 'x' );

	// Verify A_orig * X ≈ B_orig
	AB = zmatmat( Aorig, Array.from( view ), 2, 2 );
	assertArrayClose( Array.from( AB ), Array.from( Borig ), 1e-13, 'A*X=B' );
});

test( 'zgesv: singular', function t() {
	var IPIV;
	var info;
	var tc;
	var A;
	var B;

	tc = findCase( 'singular' );

	// Singular: rows are multiples [1 2 3; 2 4 6; 3 6 9] (all real)
	A = new Complex128Array( [
		1.0, 0.0, 2.0, 0.0, 3.0, 0.0,
		2.0, 0.0, 4.0, 0.0, 6.0, 0.0,
		3.0, 0.0, 6.0, 0.0, 9.0, 0.0
	] );
	IPIV = new Int32Array( 3 );
	B = new Complex128Array( [
		1.0, 0.0, 2.0, 0.0, 3.0, 0.0
	] );

	info = zgesv( 3, 1, A, 1, 3, 0, IPIV, 1, 0, B, 1, 3, 0 );

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

	A = new Complex128Array( [ 5.0, 1.0 ] );
	IPIV = new Int32Array( 1 );
	B = new Complex128Array( 1 );

	info = zgesv( 1, 0, A, 1, 1, 0, IPIV, 1, 0, B, 1, 1, 0 );

	assert.equal( info, tc.info, 'info' );
});

test( 'zgesv: 1x1', function t() {
	var info;
	var IPIV;
	var view;
	var tc;
	var A;
	var B;

	tc = findCase( '1x1' );

	// (5+2i)*x = (10+4i) => x = 2+0i
	A = new Complex128Array( [ 5.0, 2.0 ] );
	IPIV = new Int32Array( 1 );
	B = new Complex128Array( [ 10.0, 4.0 ] );

	info = zgesv( 1, 1, A, 1, 1, 0, IPIV, 1, 0, B, 1, 1, 0 );
	view = reinterpret( B, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( Array.from( view ), tc.x, 1e-14, 'x' );
});

test( 'zgesv: 4x4', function t() {
	var Aorig;
	var Borig;
	var IPIV;
	var info;
	var view;
	var tc;
	var AB;
	var A;
	var B;

	tc = findCase( '4x4' );

	// A = diagonally dominant 4x4 complex matrix (col-major)
	A = new Complex128Array( [
		10.0, 1.0, 1.0, 2.0, 2.0, -1.0, 3.0, 0.5,
		1.0, -1.0, 12.0, 2.0, 1.0, 3.0, 2.0, -0.5,
		2.0, 0.5, 3.0, -1.0, 15.0, 1.0, 1.0, 2.0,
		1.0, 1.0, 2.0, 0.5, 3.0, -2.0, 20.0, 3.0
	] );
	Aorig = new Float64Array( reinterpret( A, 0 ) );
	IPIV = new Int32Array( 4 );

	// x = [1+1i; 2-1i; -1+2i; 3+0i]
	var xvec = new Float64Array( [
		1.0, 1.0, 2.0, -1.0, -1.0, 2.0, 3.0, 0.0
	] );

	// b = A * x
	var bvals = zmatmat( Aorig, xvec, 4, 1 );
	B = new Complex128Array( 4 );
	reinterpret( B, 0 ).set( bvals );
	Borig = new Float64Array( reinterpret( B, 0 ) );

	info = zgesv( 4, 1, A, 1, 4, 0, IPIV, 1, 0, B, 1, 4, 0 );
	view = reinterpret( B, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( Array.from( view ), tc.x, 1e-13, 'x' );

	// Verify A_orig * x ≈ b_orig
	AB = zmatmat( Aorig, Array.from( view ), 4, 1 );
	assertArrayClose( Array.from( AB ), Array.from( Borig ), 1e-13, 'A*x=b' );
});
