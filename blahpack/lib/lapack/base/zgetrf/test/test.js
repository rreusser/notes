

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zgetrf = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zgetrf.jsonl' ), 'utf8' ).trim().split( '\n' );
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
* Converts 1-based Fortran IPIV to 0-based JS IPIV for comparison.
*/
function ipivTo0Based( arr ) {
	var out = [];
	var i;
	for ( i = 0; i < arr.length; i++ ) {
		out.push( arr[ i ] - 1 );
	}
	return out;
}

/**
* Verifies P*L*U = A_original for a complex factored M x N matrix.
*
* @param {Float64Array} AorigR - original matrix as interleaved re/im (col-major, M rows, N cols, LDA=M)
* @param {Float64Array} ALUR - factored matrix from zgetrf as interleaved re/im (col-major)
* @param {Int32Array} IPIV - 0-based pivot indices from zgetrf
* @param {number} M - number of rows
* @param {number} N - number of columns
* @param {number} tol - tolerance
* @param {string} msg - error message prefix
*/
function assertFactorizationCorrect( AorigR, ALUR, IPIV, M, N, tol, msg ) {
	var minMN = Math.min( M, N );
	var resultR;
	var resultI;
	var sumR;
	var sumI;
	var LikR;
	var LikI;
	var UkjR;
	var UkjI;
	var LUR;
	var LUI;
	var tmpR;
	var tmpI;
	var idxI;
	var idxJ;
	var idxK;
	var ia;
	var ib;
	var i;
	var j;
	var k;

	// Compute L*U (M x N) - interleaved real/imag stored separately for clarity
	LUR = new Float64Array( M * N );
	LUI = new Float64Array( M * N );
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			sumR = 0.0;
			sumI = 0.0;
			for ( k = 0; k < minMN; k++ ) {
				// L(i,k): if i===k, 1.0; if i>k, ALU[i+k*M]; else 0
				if ( i === k ) {
					LikR = 1.0;
					LikI = 0.0;
				} else if ( i > k ) {
					ia = ( i + k * M ) * 2;
					LikR = ALUR[ ia ];
					LikI = ALUR[ ia + 1 ];
				} else {
					LikR = 0.0;
					LikI = 0.0;
				}
				// U(k,j): if k<=j, ALU[k+j*M]; else 0
				if ( k <= j ) {
					ib = ( k + j * M ) * 2;
					UkjR = ALUR[ ib ];
					UkjI = ALUR[ ib + 1 ];
				} else {
					UkjR = 0.0;
					UkjI = 0.0;
				}
				// complex multiply-add: sum += Lik * Ukj
				sumR += LikR * UkjR - LikI * UkjI;
				sumI += LikR * UkjI + LikI * UkjR;
			}
			LUR[ i + j * M ] = sumR;
			LUI[ i + j * M ] = sumI;
		}
	}

	// Apply P^T (undo row interchanges in reverse) to get P*L*U
	resultR = new Float64Array( LUR );
	resultI = new Float64Array( LUI );
	for ( i = minMN - 1; i >= 0; i-- ) {
		if ( IPIV[ i ] !== i ) {
			for ( j = 0; j < N; j++ ) {
				idxI = i + j * M;
				idxJ = IPIV[ i ] + j * M;
				tmpR = resultR[ idxI ];
				tmpI = resultI[ idxI ];
				resultR[ idxI ] = resultR[ idxJ ];
				resultI[ idxI ] = resultI[ idxJ ];
				resultR[ idxJ ] = tmpR;
				resultI[ idxJ ] = tmpI;
			}
		}
	}

	// Compare against original (interleaved format)
	for ( i = 0; i < M; i++ ) {
		for ( j = 0; j < N; j++ ) {
			ia = ( i + j * M ) * 2;
			assertClose( resultR[ i + j * M ], AorigR[ ia ], tol, msg + ' PLU real[' + i + ',' + j + ']' );
			assertClose( resultI[ i + j * M ], AorigR[ ia + 1 ], tol, msg + ' PLU imag[' + i + ',' + j + ']' );
		}
	}
}


// TESTS //

test( 'zgetrf: 3x3', function t() {
	var IPIV;
	var info;
	var view;
	var tc;
	var A;

	tc = findCase( '3x3' );

	// A = [(2+1i) (1+0.5i) (1+0.1i); (4+2i) (3+1i) (3+0.5i); (8+3i) (7+2i) (9+1i)] col-major
	A = new Complex128Array( [
		2.0, 1.0, 4.0, 2.0, 8.0, 3.0,
		1.0, 0.5, 3.0, 1.0, 7.0, 2.0,
		1.0, 0.1, 3.0, 0.5, 9.0, 1.0
	] );
	IPIV = new Int32Array( 3 );

	info = zgetrf( 3, 3, A, 1, 3, 0, IPIV, 1, 0 );
	view = reinterpret( A, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( Array.from( view ), tc.a, 1e-14, 'a' );
	assert.deepStrictEqual( Array.from( IPIV ), ipivTo0Based( tc.ipiv ), 'ipiv' );
});

test( 'zgetrf: 4x3 tall matrix', function t() {
	var Aorig;
	var IPIV;
	var info;
	var view;
	var A;

	// 4x3 complex matrix, col-major, LDA=4
	A = new Complex128Array( [
		2.0, 1.0, 0.0, 0.5, 1.0, 0.2, 0.0, 0.1,
		1.0, 0.3, 3.0, 1.0, 0.0, 0.4, 1.0, 0.5,
		0.0, 0.1, 1.0, 0.6, 4.0, 2.0, 2.0, 1.0
	] );
	Aorig = new Float64Array( reinterpret( A, 0 ) );
	IPIV = new Int32Array( 3 );

	info = zgetrf( 4, 3, A, 1, 4, 0, IPIV, 1, 0 );
	view = reinterpret( A, 0 );

	assert.equal( info, 0, 'info' );
	assertFactorizationCorrect( Aorig, Array.from( view ), IPIV, 4, 3, 1e-13, '4x3' );
});

test( 'zgetrf: singular', function t() {
	var IPIV;
	var info;
	var view;
	var tc;
	var A;

	tc = findCase( 'singular' );

	// Singular 3x3: A = diag(1, 0, 1) in complex
	A = new Complex128Array( [
		1.0, 0.0, 0.0, 0.0, 0.0, 0.0,
		0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
		0.0, 0.0, 0.0, 0.0, 1.0, 0.0
	] );
	IPIV = new Int32Array( 3 );

	info = zgetrf( 3, 3, A, 1, 3, 0, IPIV, 1, 0 );
	view = reinterpret( A, 0 );

	assert.ok( info > 0, 'info > 0 for singular matrix' );
	assert.equal( info, tc.info, 'info matches fixture' );
	assertArrayClose( Array.from( view ), tc.a, 1e-14, 'a' );
	assert.deepStrictEqual( Array.from( IPIV ), ipivTo0Based( tc.ipiv ), 'ipiv' );
});

test( 'zgetrf: n_zero', function t() {
	var IPIV;
	var info;
	var tc;
	var A;

	tc = findCase( 'n_zero' );

	A = new Complex128Array( 9 );
	IPIV = new Int32Array( 3 );

	info = zgetrf( 3, 0, A, 1, 3, 0, IPIV, 1, 0 );

	assert.equal( info, tc.info, 'info' );
});

test( 'zgetrf: m_zero', function t() {
	var IPIV;
	var info;
	var tc;
	var A;

	tc = findCase( 'm_zero' );

	A = new Complex128Array( 1 );
	IPIV = new Int32Array( 1 );

	info = zgetrf( 0, 3, A, 1, 1, 0, IPIV, 1, 0 );

	assert.equal( info, tc.info, 'info' );
});

test( 'zgetrf: 1x1', function t() {
	var IPIV;
	var info;
	var view;
	var tc;
	var A;

	tc = findCase( '1x1' );

	A = new Complex128Array( [ 5.0, 3.0 ] );
	IPIV = new Int32Array( 1 );

	info = zgetrf( 1, 1, A, 1, 1, 0, IPIV, 1, 0 );
	view = reinterpret( A, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( Array.from( view ), tc.a, 1e-14, 'a' );
	assert.deepStrictEqual( Array.from( IPIV ), ipivTo0Based( tc.ipiv ), 'ipiv' );
});

test( 'zgetrf: 4x4', function t() {
	var Aorig;
	var IPIV;
	var info;
	var view;
	var tc;
	var A;

	tc = findCase( '4x4' );

	// 4x4 diagonally dominant complex, col-major
	A = new Complex128Array( [
		10.0, 1.0, 1.0, 2.0, 2.0, -1.0, 3.0, 0.5,
		1.0, -1.0, 12.0, 2.0, 1.0, 3.0, 2.0, -0.5,
		2.0, 0.5, 3.0, -1.0, 15.0, 1.0, 1.0, 2.0,
		1.0, 1.0, 2.0, 0.5, 3.0, -2.0, 20.0, 3.0
	] );
	Aorig = new Float64Array( reinterpret( A, 0 ) );
	IPIV = new Int32Array( 4 );

	info = zgetrf( 4, 4, A, 1, 4, 0, IPIV, 1, 0 );
	view = reinterpret( A, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( Array.from( view ), tc.a, 1e-14, 'a' );
	assert.deepStrictEqual( Array.from( IPIV ), ipivTo0Based( tc.ipiv ), 'ipiv' );
});

test( 'zgetrf: non-unit stride with offset', function t() {
	var Aorig;
	var subOrig;
	var subView;
	var subIPIV;
	var IPIV;
	var info;
	var view;
	var A;

	// Place a 2x2 complex matrix at offset 2 (complex elements) in a larger array
	// A = [(4+1i) (6+2i); (3+0.5i) (8+3i)]
	// Pad with 2 complex zeros, then the 2x2 matrix in col-major
	A = new Complex128Array( [
		0.0, 0.0, 0.0, 0.0,  // padding (2 complex elements)
		4.0, 1.0, 3.0, 0.5,  // col 0
		6.0, 2.0, 8.0, 3.0   // col 1
	] );
	IPIV = new Int32Array( [ 0, 0, 0, 0 ] );

	info = zgetrf( 2, 2, A, 1, 2, 2, IPIV, 1, 1 );
	view = reinterpret( A, 0 );

	assert.equal( info, 0, 'info' );

	// Verify factorization: reconstruct from A at offset 2 (complex elements)
	subOrig = new Float64Array( [ 4.0, 1.0, 3.0, 0.5, 6.0, 2.0, 8.0, 3.0 ] );
	subView = Array.from( view ).slice( 4, 12 ); // elements 4..11 in Float64
	subIPIV = new Int32Array( [ IPIV[ 1 ], IPIV[ 2 ] ] );
	assertFactorizationCorrect( subOrig, subView, subIPIV, 2, 2, 1e-14, 'offset' );
});

test( 'zgetrf: 70x70 blocked path (NB=64, min(M,N) > NB)', function t() {
	var AorigR;
	var IPIV;
	var info;
	var view;
	var seed;
	var N;
	var A;
	var i;
	var j;
	var idx;

	// Generate a 70x70 diagonally dominant complex matrix (well-conditioned, non-singular).
	N = 70;
	A = new Complex128Array( N * N );
	view = reinterpret( A, 0 );

	// Use a simple deterministic pseudo-random sequence for reproducibility
	seed = 12345;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < N; i++ ) {
			idx = ( i + j * N ) * 2;
			seed = ( seed * 1103515245 + 12345 ) & 0x7fffffff;
			view[ idx ] = ( seed / 0x7fffffff ) * 2.0 - 1.0;  // real part
			seed = ( seed * 1103515245 + 12345 ) & 0x7fffffff;
			view[ idx + 1 ] = ( seed / 0x7fffffff ) * 2.0 - 1.0;  // imag part
		}
	}
	// Make diagonally dominant
	for ( i = 0; i < N; i++ ) {
		idx = ( i + i * N ) * 2;
		view[ idx ] += 200.0;
	}

	AorigR = new Float64Array( view );
	IPIV = new Int32Array( N );

	info = zgetrf( N, N, A, 1, N, 0, IPIV, 1, 0 );
	view = reinterpret( A, 0 );

	assert.equal( info, 0, '70x70 info should be 0 for non-singular matrix' );

	// Verify P*L*U = A
	assertFactorizationCorrect( AorigR, Array.from( view ), IPIV, N, N, 1e-10, '70x70 blocked' );
});

test( 'zgetrf: 80x70 tall blocked path', function t() {
	var AorigR;
	var IPIV;
	var info;
	var minMN;
	var view;
	var seed;
	var M;
	var N;
	var A;
	var i;
	var j;
	var idx;

	M = 80;
	N = 70;
	minMN = Math.min( M, N );
	A = new Complex128Array( M * N );
	view = reinterpret( A, 0 );

	seed = 67890;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			idx = ( i + j * M ) * 2;
			seed = ( seed * 1103515245 + 12345 ) & 0x7fffffff;
			view[ idx ] = ( seed / 0x7fffffff ) * 2.0 - 1.0;
			seed = ( seed * 1103515245 + 12345 ) & 0x7fffffff;
			view[ idx + 1 ] = ( seed / 0x7fffffff ) * 2.0 - 1.0;
		}
	}
	for ( i = 0; i < minMN; i++ ) {
		idx = ( i + i * M ) * 2;
		view[ idx ] += 200.0;
	}

	AorigR = new Float64Array( view );
	IPIV = new Int32Array( minMN );

	info = zgetrf( M, N, A, 1, M, 0, IPIV, 1, 0 );
	view = reinterpret( A, 0 );

	assert.equal( info, 0, '80x70 info should be 0' );
	assertFactorizationCorrect( AorigR, Array.from( view ), IPIV, M, N, 1e-10, '80x70 blocked' );
});

test( 'zgetrf: 70x80 wide blocked path', function t() {
	var AorigR;
	var IPIV;
	var info;
	var minMN;
	var view;
	var seed;
	var M;
	var N;
	var A;
	var i;
	var j;
	var idx;

	M = 70;
	N = 80;
	minMN = Math.min( M, N );
	A = new Complex128Array( M * N );
	view = reinterpret( A, 0 );

	seed = 11111;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			idx = ( i + j * M ) * 2;
			seed = ( seed * 1103515245 + 12345 ) & 0x7fffffff;
			view[ idx ] = ( seed / 0x7fffffff ) * 2.0 - 1.0;
			seed = ( seed * 1103515245 + 12345 ) & 0x7fffffff;
			view[ idx + 1 ] = ( seed / 0x7fffffff ) * 2.0 - 1.0;
		}
	}
	for ( i = 0; i < minMN; i++ ) {
		idx = ( i + i * M ) * 2;
		view[ idx ] += 200.0;
	}

	AorigR = new Float64Array( view );
	IPIV = new Int32Array( minMN );

	info = zgetrf( M, N, A, 1, M, 0, IPIV, 1, 0 );
	view = reinterpret( A, 0 );

	assert.equal( info, 0, '70x80 info should be 0' );
	assertFactorizationCorrect( AorigR, Array.from( view ), IPIV, M, N, 1e-10, '70x80 blocked' );
});

test( 'zgetrf: 70x70 singular matrix in blocked path (iinfo > 0 branch)', function t() {
	var AorigR;
	var IPIV;
	var info;
	var view;
	var seed;
	var N;
	var A;
	var i;
	var j;
	var idx;

	// Create a 70x70 complex matrix that is singular: zero out column 65 (index 64)
	// entirely. This ensures U(64,64) = 0 after the first block panel (cols 0..63)
	// is factored and the trailing update is applied, triggering iinfo > 0 in the
	// second panel factorization.
	N = 70;
	A = new Complex128Array( N * N );
	view = reinterpret( A, 0 );

	seed = 99999;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < N; i++ ) {
			idx = ( i + j * N ) * 2;
			seed = ( seed * 1103515245 + 12345 ) & 0x7fffffff;
			view[ idx ] = ( seed / 0x7fffffff ) * 2.0 - 1.0;
			seed = ( seed * 1103515245 + 12345 ) & 0x7fffffff;
			view[ idx + 1 ] = ( seed / 0x7fffffff ) * 2.0 - 1.0;
		}
	}
	for ( i = 0; i < N; i++ ) {
		idx = ( i + i * N ) * 2;
		view[ idx ] += 200.0;
	}

	// Zero out column 64 entirely to create a guaranteed singularity
	for ( i = 0; i < N; i++ ) {
		idx = ( i + 64 * N ) * 2;
		view[ idx ] = 0.0;
		view[ idx + 1 ] = 0.0;
	}

	AorigR = new Float64Array( view );
	IPIV = new Int32Array( N );

	info = zgetrf( N, N, A, 1, N, 0, IPIV, 1, 0 );
	view = reinterpret( A, 0 );

	// The matrix is singular, so info should be > 0
	assert.ok( info > 0, '70x70 singular: info > 0 (got ' + info + ')' );

	// Verify P*L*U still reconstructs A
	assertFactorizationCorrect( AorigR, Array.from( view ), IPIV, N, N, 1e-8, '70x70 singular blocked' );
});
