

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zgetrf = require( './../../zgetrf/lib/base.js' );
var zgetrs = require( './../../zgetrs/lib/base.js' );
var zgerfs = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zgerfs.jsonl' ), 'utf8' ).trim().split( '\n' );
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
* Complex matrix-vector multiply: b = A*x (col-major, interleaved re/im).
*/
function zmatvec( Adata, xdata, N ) {
	var b = new Float64Array( 2 * N );
	var are;
	var aim;
	var xre;
	var xim;
	var i;
	var j;
	for ( i = 0; i < N; i++ ) {
		for ( j = 0; j < N; j++ ) {
			are = Adata[ 2 * ( i + j * N ) ];
			aim = Adata[ 2 * ( i + j * N ) + 1 ];
			xre = xdata[ 2 * j ];
			xim = xdata[ 2 * j + 1 ];
			b[ 2 * i ] += are * xre - aim * xim;
			b[ 2 * i + 1 ] += are * xim + aim * xre;
		}
	}
	return b;
}

/**
* Complex matrix-conjugate-transpose-vector multiply: b = A^H * x.
*/
function zmatvecH( Adata, xdata, N ) {
	var b = new Float64Array( 2 * N );
	var are;
	var aim;
	var xre;
	var xim;
	var i;
	var j;
	for ( i = 0; i < N; i++ ) {
		for ( j = 0; j < N; j++ ) {
			// A^H(i,j) = conj(A(j,i)) = conj of col-major A[j + i*N]
			are = Adata[ 2 * ( j + i * N ) ];
			aim = -Adata[ 2 * ( j + i * N ) + 1 ]; // conjugate
			xre = xdata[ 2 * j ];
			xim = xdata[ 2 * j + 1 ];
			b[ 2 * i ] += are * xre - aim * xim;
			b[ 2 * i + 1 ] += are * xim + aim * xre;
		}
	}
	return b;
}

// 3x3 test matrix data (interleaved re/im, col-major)
var A_DATA = [
	4, 1, 1, -1, 0.5, 0.2,
	1, 0.5, 3, 2, 1, -0.5,
	0.5, 0.1, 1, 0.3, 2, 1
];


// TESTS //

test( 'zgerfs: trans_N', function t() {
	var tc = findCase( 'trans_N' );
	var n = 3;
	var nrhs = 1;
	var A = new Complex128Array( A_DATA.slice() );
	var AF = new Complex128Array( A_DATA.slice() );
	var IPIV = new Int32Array( n );
	var WORK = new Complex128Array( 2 * n );
	var RWORK = new Float64Array( n );
	var FERR = new Float64Array( nrhs );
	var BERR = new Float64Array( nrhs );

	// Compute b = A * [1;1;1]
	var xExact = [ 1, 0, 1, 0, 1, 0 ];
	var bData = zmatvec( A_DATA, xExact, n );
	var B = new Complex128Array( bData );

	// Factor
	zgetrf( n, n, AF, 1, n, 0, IPIV, 1, 0 );

	// Solve
	var X = new Complex128Array( bData.slice() );
	zgetrs( 'no-transpose', n, nrhs, AF, 1, n, 0, IPIV, 1, 0, X, 1, n, 0 );

	// Refine
	var info = zgerfs( 'no-transpose', n, nrhs, A, 1, n, 0, AF, 1, n, 0, IPIV, 1, 0, B, 1, n, 0, X, 1, n, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 );

	assert.equal( info, tc.info );
	var Xv = reinterpret( X, 0 );
	assertArrayClose( Array.from( Xv ), tc.x, 1e-12, 'x' );
	// berr should be very small for well-conditioned system
	assert.ok( BERR[ 0 ] < 1e-10, 'berr small' );
});

test( 'zgerfs: trans_C', function t() {
	var tc = findCase( 'trans_C' );
	var n = 3;
	var nrhs = 1;
	var A = new Complex128Array( A_DATA.slice() );
	var AF = new Complex128Array( A_DATA.slice() );
	var IPIV = new Int32Array( n );
	var WORK = new Complex128Array( 2 * n );
	var RWORK = new Float64Array( n );
	var FERR = new Float64Array( nrhs );
	var BERR = new Float64Array( nrhs );

	// Compute b = A^H * [1;1;1]
	var xExact = [ 1, 0, 1, 0, 1, 0 ];
	var bData = zmatvecH( A_DATA, xExact, n );
	var B = new Complex128Array( bData );

	// Factor
	zgetrf( n, n, AF, 1, n, 0, IPIV, 1, 0 );

	// Solve A^H * x = b
	var X = new Complex128Array( bData.slice() );
	zgetrs( 'conjugate-transpose', n, nrhs, AF, 1, n, 0, IPIV, 1, 0, X, 1, n, 0 );

	// Refine
	var info = zgerfs( 'conjugate-transpose', n, nrhs, A, 1, n, 0, AF, 1, n, 0, IPIV, 1, 0, B, 1, n, 0, X, 1, n, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 );

	assert.equal( info, tc.info );
	var Xv = reinterpret( X, 0 );
	assertArrayClose( Array.from( Xv ), tc.x, 1e-12, 'x' );
});

test( 'zgerfs: n_zero', function t() {
	var tc = findCase( 'n_zero' );
	var A = new Complex128Array( 1 );
	var AF = new Complex128Array( 1 );
	var IPIV = new Int32Array( 1 );
	var B = new Complex128Array( 1 );
	var X = new Complex128Array( 1 );
	var WORK = new Complex128Array( 2 );
	var RWORK = new Float64Array( 1 );
	var FERR = new Float64Array( 1 );
	var BERR = new Float64Array( 1 );
	var info = zgerfs( 'no-transpose', 0, 1, A, 1, 1, 0, AF, 1, 1, 0, IPIV, 1, 0, B, 1, 1, 0, X, 1, 1, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 );
	assert.equal( info, tc.info );
});

test( 'zgerfs: nrhs_zero', function t() {
	var tc = findCase( 'nrhs_zero' );
	var A = new Complex128Array( 9 );
	var AF = new Complex128Array( 9 );
	var IPIV = new Int32Array( 3 );
	var B = new Complex128Array( 3 );
	var X = new Complex128Array( 3 );
	var WORK = new Complex128Array( 6 );
	var RWORK = new Float64Array( 3 );
	var FERR = new Float64Array( 1 );
	var BERR = new Float64Array( 1 );
	var info = zgerfs( 'no-transpose', 3, 0, A, 1, 3, 0, AF, 1, 3, 0, IPIV, 1, 0, B, 1, 3, 0, X, 1, 3, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 );
	assert.equal( info, tc.info );
});

test( 'zgerfs: multi_rhs', function t() {
	var tc = findCase( 'multi_rhs' );
	var n = 3;
	var nrhs = 2;
	var A = new Complex128Array( A_DATA.slice() );
	var AF = new Complex128Array( A_DATA.slice() );
	var IPIV = new Int32Array( n );
	var WORK = new Complex128Array( 2 * n );
	var RWORK = new Float64Array( n );
	var FERR = new Float64Array( nrhs );
	var BERR = new Float64Array( nrhs );

	// RHS 1: b = A * [1;1;1]
	var x1 = [ 1, 0, 1, 0, 1, 0 ];
	var b1 = zmatvec( A_DATA, x1, n );
	// RHS 2: b = A * [1+i; 2-i; 0.5+0.5i]
	var x2 = [ 1, 1, 2, -1, 0.5, 0.5 ];
	var b2 = zmatvec( A_DATA, x2, n );

	// Pack B column-major: B(:,1)=b1, B(:,2)=b2
	var Bdata = new Float64Array( 2 * n * nrhs );
	var i;
	for ( i = 0; i < 2 * n; i++ ) {
		Bdata[ i ] = b1[ i ];
		Bdata[ 2 * n + i ] = b2[ i ];
	}
	var B = new Complex128Array( Bdata );

	// Factor
	zgetrf( n, n, AF, 1, n, 0, IPIV, 1, 0 );

	// Solve
	var X = new Complex128Array( Bdata.slice() );
	zgetrs( 'no-transpose', n, nrhs, AF, 1, n, 0, IPIV, 1, 0, X, 1, n, 0 );

	// Refine
	var info = zgerfs( 'no-transpose', n, nrhs, A, 1, n, 0, AF, 1, n, 0, IPIV, 1, 0, B, 1, n, 0, X, 1, n, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 );

	assert.equal( info, tc.info );
	var Xv = reinterpret( X, 0 );
	assertArrayClose( Array.from( Xv ), tc.x, 1e-12, 'x' );
});

test( 'zgerfs: trans_T', function t() {
	var tc = findCase( 'trans_T' );
	var n = 3;
	var nrhs = 1;
	var A = new Complex128Array( A_DATA.slice() );
	var AF = new Complex128Array( A_DATA.slice() );
	var IPIV = new Int32Array( n );
	var WORK = new Complex128Array( 2 * n );
	var RWORK = new Float64Array( n );
	var FERR = new Float64Array( nrhs );
	var BERR = new Float64Array( nrhs );

	// b = A^T * [1;1;1] -- transpose, NOT conjugate
	// A^T(i,j) = A(j,i): b(i) = sum_j A(j,i) * x(j) = sum of col i of A
	var bData = new Float64Array( 2 * n );
	var i;
	var j;
	for ( i = 0; i < n; i++ ) {
		for ( j = 0; j < n; j++ ) {
			// A^T: element (i,j) = A(j,i) in col-major = A_DATA[2*(j + i*n)]
			bData[ 2 * i ] += A_DATA[ 2 * ( j + i * n ) ];
			bData[ 2 * i + 1 ] += A_DATA[ 2 * ( j + i * n ) + 1 ];
		}
	}
	var B = new Complex128Array( bData );

	// Factor
	zgetrf( n, n, AF, 1, n, 0, IPIV, 1, 0 );

	// Solve A^T * x = b
	var X = new Complex128Array( bData.slice() );
	zgetrs( 'transpose', n, nrhs, AF, 1, n, 0, IPIV, 1, 0, X, 1, n, 0 );

	// Refine
	var info = zgerfs( 'transpose', n, nrhs, A, 1, n, 0, AF, 1, n, 0, IPIV, 1, 0, B, 1, n, 0, X, 1, n, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 );

	assert.equal( info, tc.info );
	var Xv = reinterpret( X, 0 );
	assertArrayClose( Array.from( Xv ), tc.x, 1e-12, 'x' );
});
