'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zgetrf = require( '../../zgetrf/lib/base.js' );
var zgetri = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zgetri.jsonl' ), 'utf8' ).trim().split( '\n' );
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
* Complex matrix multiply C = A * B (column-major, N-by-N).
*/
function zmatmul( N, Av, Bv ) {
	var Cv = new Float64Array( 2 * N * N );
	var ar;
	var ai;
	var br;
	var bi;
	var i;
	var j;
	var k;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < N; i++ ) {
			for ( k = 0; k < N; k++ ) {
				ar = Av[ 2 * ( i + k * N ) ];
				ai = Av[ 2 * ( i + k * N ) + 1 ];
				br = Bv[ 2 * ( k + j * N ) ];
				bi = Bv[ 2 * ( k + j * N ) + 1 ];
				// (ar + ai*i) * (br + bi*i) = (ar*br - ai*bi) + (ar*bi + ai*br)*i
				Cv[ 2 * ( i + j * N ) ] += ar * br - ai * bi;
				Cv[ 2 * ( i + j * N ) + 1 ] += ar * bi + ai * br;
			}
		}
	}
	return Cv;
}

/**
* Assert that a complex matrix is approximately the identity.
*/
function assertComplexIdentity( N, Cv, tol, msg ) {
	var expectedR;
	var expectedI;
	var errR;
	var errI;
	var i;
	var j;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < N; i++ ) {
			expectedR = ( i === j ) ? 1.0 : 0.0;
			expectedI = 0.0;
			errR = Math.abs( Cv[ 2 * ( i + j * N ) ] - expectedR );
			errI = Math.abs( Cv[ 2 * ( i + j * N ) + 1 ] - expectedI );
			assert.ok( errR <= tol, msg + ': C[' + i + ',' + j + '] real = ' + Cv[ 2 * ( i + j * N ) ] + ', expected ' + expectedR + ', err = ' + errR );
			assert.ok( errI <= tol, msg + ': C[' + i + ',' + j + '] imag = ' + Cv[ 2 * ( i + j * N ) + 1 ] + ', expected ' + expectedI + ', err = ' + errI );
		}
	}
}


// TESTS //

test( 'zgetri: 3x3 inverse', function t() {
	var tc = findCase( '3x3_inverse' );
	// A = [[2+1i, 1+0i, 1+0.5i], [4+2i, 3+1i, 3+0i], [8+0i, 7+1i, 9+2i]] col-major
	var Aorig = new Complex128Array( [
		2, 1, 4, 2, 8, 0,
		1, 0, 3, 1, 7, 1,
		1, 0.5, 3, 0, 9, 2
	] );
	var A = new Complex128Array( Array.from( reinterpret( Aorig, 0 ) ) );
	var IPIV = new Int32Array( 3 );
	var WORK = new Complex128Array( 64 );
	var info;

	// Factor
	info = zgetrf( 3, 3, A, 1, 3, 0, IPIV, 1, 0 );
	assert.equal( info, 0, 'zgetrf info' );

	// Invert
	info = zgetri( 3, A, 1, 3, 0, IPIV, 1, 0, WORK, 1, 0, 64 );
	assert.equal( info, tc.info, 'zgetri info' );

	var view = reinterpret( A, 0 );
	assertArrayClose( Array.from( view ), tc.a, 1e-13, 'a' );

	// Verify A_orig * A_inv ~= I
	var C = zmatmul( 3, reinterpret( Aorig, 0 ), view );
	assertComplexIdentity( 3, C, 1e-13, 'A * A_inv' );
});

test( 'zgetri: 4x4 inverse', function t() {
	var tc = findCase( '4x4_inverse' );
	var Aorig = new Complex128Array( [
		5, 1, 1, 0.5, 0.5, 0, 0, 0.5,
		1, -0.5, 5, 2, 1, 1, 0.5, 0,
		0.5, 0, 1, -1, 5, 0, 1, 0.5,
		0, -0.5, 0.5, 0, 1, -0.5, 5, 1
	] );
	var A = new Complex128Array( Array.from( reinterpret( Aorig, 0 ) ) );
	var IPIV = new Int32Array( 4 );
	var WORK = new Complex128Array( 128 );
	var info;

	info = zgetrf( 4, 4, A, 1, 4, 0, IPIV, 1, 0 );
	assert.equal( info, 0, 'zgetrf info' );

	info = zgetri( 4, A, 1, 4, 0, IPIV, 1, 0, WORK, 1, 0, 128 );
	assert.equal( info, tc.info, 'zgetri info' );

	var view = reinterpret( A, 0 );
	assertArrayClose( Array.from( view ), tc.a, 1e-13, 'a' );

	var C = zmatmul( 4, reinterpret( Aorig, 0 ), view );
	assertComplexIdentity( 4, C, 1e-13, 'A * A_inv' );
});

test( 'zgetri: N=1 edge case', function t() {
	var tc = findCase( 'n1' );
	var A = new Complex128Array( [ 3, 4 ] );
	var IPIV = new Int32Array( 1 );
	var WORK = new Complex128Array( 4 );
	var info;

	info = zgetrf( 1, 1, A, 1, 1, 0, IPIV, 1, 0 );
	assert.equal( info, 0, 'zgetrf info' );

	info = zgetri( 1, A, 1, 1, 0, IPIV, 1, 0, WORK, 1, 0, 4 );
	assert.equal( info, tc.info, 'zgetri info' );

	var view = reinterpret( A, 0 );
	assertArrayClose( Array.from( view ), tc.a, 1e-14, 'a' );
});

test( 'zgetri: N=0 quick return', function t() {
	var A = new Complex128Array( 1 );
	var IPIV = new Int32Array( 1 );
	var WORK = new Complex128Array( 1 );
	var info;

	info = zgetri( 0, A, 1, 1, 0, IPIV, 1, 0, WORK, 1, 0, 1 );
	assert.equal( info, 0, 'info should be 0' );
});

test( 'zgetri: 3x3 different pivots', function t() {
	var tc = findCase( '3x3_pivots_inverse' );
	var Aorig = new Complex128Array( [
		1, 0, 4, 1, 7, 2,
		2, 1, 5, 0, 8, 1,
		3, 0, 6, 2, 0, 1
	] );
	var A = new Complex128Array( Array.from( reinterpret( Aorig, 0 ) ) );
	var IPIV = new Int32Array( 3 );
	var WORK = new Complex128Array( 64 );
	var info;

	info = zgetrf( 3, 3, A, 1, 3, 0, IPIV, 1, 0 );
	assert.equal( info, 0, 'zgetrf info' );

	info = zgetri( 3, A, 1, 3, 0, IPIV, 1, 0, WORK, 1, 0, 64 );
	assert.equal( info, tc.info, 'zgetri info' );

	var view = reinterpret( A, 0 );
	assertArrayClose( Array.from( view ), tc.a, 1e-13, 'a' );

	var C = zmatmul( 3, reinterpret( Aorig, 0 ), view );
	assertComplexIdentity( 3, C, 1e-13, 'A * A_inv' );
});

test( 'zgetri: singular matrix returns info > 0', function t() {
	// A = [[1+0i, 2+0i], [2+0i, 4+0i]] (singular, column-major)
	var A = new Complex128Array( [ 1, 0, 2, 0, 2, 0, 4, 0 ] );
	var IPIV = new Int32Array( 2 );
	var WORK = new Complex128Array( 16 );
	var info;

	info = zgetrf( 2, 2, A, 1, 2, 0, IPIV, 1, 0 );
	assert.ok( info > 0, 'zgetrf should detect singular matrix, info=' + info );

	info = zgetri( 2, A, 1, 2, 0, IPIV, 1, 0, WORK, 1, 0, 16 );
	assert.ok( info > 0, 'zgetri should return info > 0 for singular matrix, info=' + info );
});

test( 'zgetri: 5x5 matrix inverse (diagonally dominant)', function t() {
	var N = 5;
	var data = new Float64Array( 2 * N * N );
	var i;
	var j;

	// Create diagonally dominant complex matrix
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < N; i++ ) {
			if ( i === j ) {
				data[ 2 * ( i + j * N ) ] = N + 1.0;
				data[ 2 * ( i + j * N ) + 1 ] = 1.0;
			} else {
				data[ 2 * ( i + j * N ) ] = 1.0 / ( 1.0 + Math.abs( i - j ) );
				data[ 2 * ( i + j * N ) + 1 ] = 0.5 / ( 1.0 + Math.abs( i - j ) );
			}
		}
	}

	var Aorig = new Complex128Array( data.buffer.slice( 0 ) );
	var A = new Complex128Array( Array.from( data ) );
	var IPIV = new Int32Array( N );
	var WORK = new Complex128Array( N * 64 );
	var info;

	info = zgetrf( N, N, A, 1, N, 0, IPIV, 1, 0 );
	assert.equal( info, 0, 'zgetrf info' );

	info = zgetri( N, A, 1, N, 0, IPIV, 1, 0, WORK, 1, 0, N * 64 );
	assert.equal( info, 0, 'zgetri info' );

	var C = zmatmul( N, reinterpret( Aorig, 0 ), reinterpret( A, 0 ) );
	assertComplexIdentity( N, C, 1e-12, 'A * A_inv' );
});

test( 'zgetri: blocked path (large matrix, N=35)', function t() {
	// N=35 > NB=32, exercises the blocked code path
	var N = 35;
	var data = new Float64Array( 2 * N * N );
	var i;
	var j;

	// Create diagonally dominant complex matrix
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < N; i++ ) {
			if ( i === j ) {
				data[ 2 * ( i + j * N ) ] = N + 1.0;
				data[ 2 * ( i + j * N ) + 1 ] = 1.0;
			} else {
				data[ 2 * ( i + j * N ) ] = 1.0 / ( 1.0 + Math.abs( i - j ) );
				data[ 2 * ( i + j * N ) + 1 ] = 0.5 / ( 1.0 + Math.abs( i - j ) );
			}
		}
	}

	var Aorig = new Complex128Array( data.buffer.slice( 0 ) );
	var A = new Complex128Array( Array.from( data ) );
	var IPIV = new Int32Array( N );
	var WORK = new Complex128Array( N * 64 );
	var info;

	info = zgetrf( N, N, A, 1, N, 0, IPIV, 1, 0 );
	assert.equal( info, 0, 'zgetrf info' );

	info = zgetri( N, A, 1, N, 0, IPIV, 1, 0, WORK, 1, 0, N * 64 );
	assert.equal( info, 0, 'zgetri info' );

	var C = zmatmul( N, reinterpret( Aorig, 0 ), reinterpret( A, 0 ) );
	assertComplexIdentity( N, C, 1e-10, 'A * A_inv' );
});

test( 'zgetri: blocked path with insufficient workspace', function t() {
	// N=35 > NB=32, lwork < N*NB triggers nb adjustment
	var N = 35;
	var data = new Float64Array( 2 * N * N );
	var i;
	var j;

	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < N; i++ ) {
			if ( i === j ) {
				data[ 2 * ( i + j * N ) ] = N + 1.0;
				data[ 2 * ( i + j * N ) + 1 ] = 1.0;
			} else {
				data[ 2 * ( i + j * N ) ] = 1.0 / ( 1.0 + Math.abs( i - j ) );
				data[ 2 * ( i + j * N ) + 1 ] = 0.5 / ( 1.0 + Math.abs( i - j ) );
			}
		}
	}

	var Aorig = new Complex128Array( data.buffer.slice( 0 ) );
	var A = new Complex128Array( Array.from( data ) );
	var IPIV = new Int32Array( N );
	// Provide workspace smaller than N*NB (35*32=1120) but >= N*2 (70)
	// This forces nb = floor(lwork/N) = floor(105/35) = 3
	var lwork = 105;
	var WORK = new Complex128Array( lwork );
	var info;

	info = zgetrf( N, N, A, 1, N, 0, IPIV, 1, 0 );
	assert.equal( info, 0, 'zgetrf info' );

	info = zgetri( N, A, 1, N, 0, IPIV, 1, 0, WORK, 1, 0, lwork );
	assert.equal( info, 0, 'zgetri info' );

	var C = zmatmul( N, reinterpret( Aorig, 0 ), reinterpret( A, 0 ) );
	assertComplexIdentity( N, C, 1e-10, 'A * A_inv' );
});
