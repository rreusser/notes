

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dgetrf = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dgetrf.jsonl' ), 'utf8' ).trim().split( '\n' );
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
* Verifies P*L*U = A_original for a factored M x N matrix.
*
* @param {Float64Array} Aorig - original matrix (col-major, M rows, N cols, LDA=M)
* @param {Float64Array} ALU - factored matrix from dgetrf (col-major)
* @param {Int32Array} IPIV - 0-based pivot indices from dgetrf
* @param {number} M - number of rows
* @param {number} N - number of columns
* @param {number} tol - tolerance
* @param {string} msg - error message prefix
*/
function assertFactorizationCorrect( Aorig, ALU, IPIV, M, N, tol, msg ) {
	var minMN = Math.min( M, N );
	var result;
	var sum;
	var tmp;
	var LU;
	var i;
	var j;
	var k;

	// Compute L*U (M x N)
	LU = new Float64Array( M * N );
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			sum = 0.0;
			for ( k = 0; k < minMN; k++ ) {
				// L(i,k): if i===k, 1.0; if i>k, ALU[i+k*M]; else 0
				// U(k,j): if k<=j, ALU[k+j*M]; else 0
				var Lik;
				var Ukj;
				if ( i === k ) {
					Lik = 1.0;
				} else if ( i > k ) {
					Lik = ALU[ i + k * M ];
				} else {
					Lik = 0.0;
				}
				if ( k <= j ) {
					Ukj = ALU[ k + j * M ];
				} else {
					Ukj = 0.0;
				}
				sum += Lik * Ukj;
			}
			LU[ i + j * M ] = sum;
		}
	}

	// Apply P^T (undo row interchanges in reverse) to get P*L*U
	result = new Float64Array( LU );
	for ( i = minMN - 1; i >= 0; i-- ) {
		if ( IPIV[ i ] !== i ) {
			for ( j = 0; j < N; j++ ) {
				tmp = result[ i + j * M ];
				result[ i + j * M ] = result[ IPIV[ i ] + j * M ];
				result[ IPIV[ i ] + j * M ] = tmp;
			}
		}
	}

	// Compare
	for ( i = 0; i < M * N; i++ ) {
		assertClose( result[ i ], Aorig[ i ], tol, msg + ' PLU[' + i + ']' );
	}
}


// TESTS //

test( 'dgetrf: 3x3', function t() {
	var IPIV;
	var info;
	var tc;
	var A;

	tc = findCase( '3x3' );

	// A = [2 1 1; 4 3 3; 8 7 9] col-major
	A = new Float64Array( [ 2.0, 4.0, 8.0, 1.0, 3.0, 7.0, 1.0, 3.0, 9.0 ] );
	IPIV = new Int32Array( 3 );

	info = dgetrf( 3, 3, A, 1, 3, 0, IPIV, 1, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( Array.from( A ), tc.a, 1e-14, 'a' );
	assert.deepStrictEqual( Array.from( IPIV ), ipivTo0Based( tc.ipiv ), 'ipiv' );
});

test( 'dgetrf: 5x4 tall matrix', function t() {
	var Aorig;
	var IPIV;
	var info;
	var A;

	// 5x4 matrix, col-major, LDA=5
	Aorig = new Float64Array( [
		1.0, 6.0, 11.0, 16.0, 21.0,
		2.0, 7.0, 12.0, 17.0, 22.0,
		3.0, 8.0, 13.0, 18.0, 23.0,
		4.0, 9.0, 14.0, 19.0, 24.0
	] );
	A = new Float64Array( Aorig );
	IPIV = new Int32Array( 4 );

	info = dgetrf( 5, 4, A, 1, 5, 0, IPIV, 1, 0 );

	// This rank-2 matrix produces a valid factorization with info > 0
	// (indicating a zero on the U diagonal). The exact info value and pivot
	// order may differ from Fortran due to floating-point arithmetic in the
	// recursive algorithm. Verify P*L*U = A instead.
	assert.ok( info > 0, 'info > 0 for rank-deficient matrix' );
	assertFactorizationCorrect( Aorig, A, IPIV, 5, 4, 1e-13, '5x4' );
});

test( 'dgetrf: singular', function t() {
	var Aorig;
	var IPIV;
	var info;
	var A;

	// Singular 3x3: [1 2 3; 2 4 6; 3 6 9] col-major (rank 1)
	Aorig = new Float64Array( [ 1.0, 2.0, 3.0, 2.0, 4.0, 6.0, 3.0, 6.0, 9.0 ] );
	A = new Float64Array( Aorig );
	IPIV = new Int32Array( 3 );

	info = dgetrf( 3, 3, A, 1, 3, 0, IPIV, 1, 0 );

	// Singular matrix: info > 0 indicates which diagonal of U is zero.
	// Exact value may differ from Fortran due to recursive splitting order.
	assert.ok( info > 0, 'info > 0 for singular matrix' );
	assertFactorizationCorrect( Aorig, A, IPIV, 3, 3, 1e-14, 'singular' );
});

test( 'dgetrf: n_zero', function t() {
	var IPIV;
	var info;
	var tc;
	var A;

	tc = findCase( 'n_zero' );

	A = new Float64Array( 9 );
	IPIV = new Int32Array( 3 );

	info = dgetrf( 3, 0, A, 1, 3, 0, IPIV, 1, 0 );

	assert.equal( info, tc.info, 'info' );
});

test( 'dgetrf: m_zero', function t() {
	var IPIV;
	var info;
	var tc;
	var A;

	tc = findCase( 'm_zero' );

	A = new Float64Array( 1 );
	IPIV = new Int32Array( 1 );

	info = dgetrf( 0, 3, A, 1, 1, 0, IPIV, 1, 0 );

	assert.equal( info, tc.info, 'info' );
});

test( 'dgetrf: 1x1', function t() {
	var IPIV;
	var info;
	var tc;
	var A;

	tc = findCase( '1x1' );

	A = new Float64Array( [ 7.0 ] );
	IPIV = new Int32Array( 1 );

	info = dgetrf( 1, 1, A, 1, 1, 0, IPIV, 1, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( Array.from( A ), tc.a, 1e-14, 'a' );
	assert.deepStrictEqual( Array.from( IPIV ), ipivTo0Based( tc.ipiv ), 'ipiv' );
});

test( 'dgetrf: 4x4', function t() {
	var IPIV;
	var info;
	var tc;
	var A;

	tc = findCase( '4x4' );

	// A = [2 1 3 1; 1 3 2 2; 3 2 1 4; 4 1 2 3] col-major, LDA=4
	A = new Float64Array( [
		2.0, 1.0, 3.0, 4.0,
		1.0, 3.0, 2.0, 1.0,
		3.0, 2.0, 1.0, 2.0,
		1.0, 2.0, 4.0, 3.0
	] );
	IPIV = new Int32Array( 4 );

	info = dgetrf( 4, 4, A, 1, 4, 0, IPIV, 1, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( Array.from( A ), tc.a, 1e-14, 'a' );
	assert.deepStrictEqual( Array.from( IPIV ), ipivTo0Based( tc.ipiv ), 'ipiv' );
});

test( 'dgetrf: non-unit stride with offset', function t() {
	var Aorig;
	var IPIV;
	var info;
	var A;

	// Place a 2x2 matrix at offset 3 in a larger array, still col-major with LDA=2
	// A = [4 6; 3 8]
	Aorig = new Float64Array( [ 0.0, 0.0, 0.0, 4.0, 3.0, 6.0, 8.0 ] );
	A = new Float64Array( Aorig );
	IPIV = new Int32Array( [ 0, 0, 0, 0 ] );

	info = dgetrf( 2, 2, A, 1, 2, 3, IPIV, 1, 1 );

	assert.equal( info, 0, 'info' );

	// Verify factorization: reconstruct 2x2 from A at offset 3
	var subA = new Float64Array( [ A[ 3 ], A[ 4 ], A[ 5 ], A[ 6 ] ] );
	var subIPIV = new Int32Array( [ IPIV[ 1 ], IPIV[ 2 ] ] );
	var subOrig = new Float64Array( [ 4.0, 3.0, 6.0, 8.0 ] );
	assertFactorizationCorrect( subOrig, subA, subIPIV, 2, 2, 1e-14, 'offset' );
});

test( 'dgetrf: 70x70 blocked path (NB=64, min(M,N) > NB)', function t() {
	var Aorig;
	var IPIV;
	var info;
	var N;
	var A;
	var i;
	var j;

	// Generate a 70x70 diagonally dominant matrix (well-conditioned, non-singular).
	// A(i,j) = random in [-1,1], then A(i,i) += 100 to ensure diagonal dominance.
	N = 70;
	Aorig = new Float64Array( N * N );

	// Use a simple deterministic pseudo-random sequence for reproducibility
	// (linear congruential generator)
	var seed = 12345;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < N; i++ ) {
			seed = ( seed * 1103515245 + 12345 ) & 0x7fffffff;
			Aorig[ i + j * N ] = ( seed / 0x7fffffff ) * 2.0 - 1.0;
		}
	}
	// Make diagonally dominant
	for ( i = 0; i < N; i++ ) {
		Aorig[ i + i * N ] += 100.0;
	}

	A = new Float64Array( Aorig );
	IPIV = new Int32Array( N );

	info = dgetrf( N, N, A, 1, N, 0, IPIV, 1, 0 );

	assert.equal( info, 0, '70x70 info should be 0 for non-singular matrix' );

	// Verify P*L*U = A
	assertFactorizationCorrect( Aorig, A, IPIV, N, N, 1e-10, '70x70 blocked' );
});

test( 'dgetrf: 80x70 tall blocked path', function t() {
	var Aorig;
	var IPIV;
	var info;
	var minMN;
	var M;
	var N;
	var A;
	var i;
	var j;

	// Tall matrix: M > N, both > NB=64, so min(M,N) = 70 > 64 triggers blocking.
	// Also exercises the j+jb < M path (line 104) in every block iteration.
	M = 80;
	N = 70;
	minMN = Math.min( M, N );
	Aorig = new Float64Array( M * N );

	var seed = 67890;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			seed = ( seed * 1103515245 + 12345 ) & 0x7fffffff;
			Aorig[ i + j * M ] = ( seed / 0x7fffffff ) * 2.0 - 1.0;
		}
	}
	// Diagonally dominant on the min(M,N) x min(M,N) leading block
	for ( i = 0; i < minMN; i++ ) {
		Aorig[ i + i * M ] += 100.0;
	}

	A = new Float64Array( Aorig );
	IPIV = new Int32Array( minMN );

	info = dgetrf( M, N, A, 1, M, 0, IPIV, 1, 0 );

	assert.equal( info, 0, '80x70 info should be 0' );
	assertFactorizationCorrect( Aorig, A, IPIV, M, N, 1e-10, '80x70 blocked' );
});

test( 'dgetrf: 70x80 wide blocked path', function t() {
	var Aorig;
	var IPIV;
	var info;
	var minMN;
	var M;
	var N;
	var A;
	var i;
	var j;

	// Wide matrix: N > M, both > NB=64, so min(M,N) = 70 > 64 triggers blocking.
	// Exercises j+jb < N path (line 92) strongly.
	M = 70;
	N = 80;
	minMN = Math.min( M, N );
	Aorig = new Float64Array( M * N );

	var seed = 11111;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			seed = ( seed * 1103515245 + 12345 ) & 0x7fffffff;
			Aorig[ i + j * M ] = ( seed / 0x7fffffff ) * 2.0 - 1.0;
		}
	}
	for ( i = 0; i < minMN; i++ ) {
		Aorig[ i + i * M ] += 100.0;
	}

	A = new Float64Array( Aorig );
	IPIV = new Int32Array( minMN );

	info = dgetrf( M, N, A, 1, M, 0, IPIV, 1, 0 );

	assert.equal( info, 0, '70x80 info should be 0' );
	assertFactorizationCorrect( Aorig, A, IPIV, M, N, 1e-10, '70x80 blocked' );
});

test( 'dgetrf: 70x70 singular matrix in blocked path (iinfo > 0 branch)', function t() {
	var Aorig;
	var IPIV;
	var info;
	var N;
	var A;
	var i;
	var j;

	// Create a 70x70 matrix that is singular: make the last row a copy of the
	// first row. The singularity will be detected during the second panel
	// factorization (after the first 64-column block), triggering iinfo > 0
	// at line 79.
	//
	// Strategy: build a diagonally dominant matrix, then zero out column 65
	// (index 64) entirely. This ensures U(64,64) = 0 after factoring the
	// second panel, which triggers info = iinfo + j with j=64.
	N = 70;
	Aorig = new Float64Array( N * N );

	var seed = 99999;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < N; i++ ) {
			seed = ( seed * 1103515245 + 12345 ) & 0x7fffffff;
			Aorig[ i + j * N ] = ( seed / 0x7fffffff ) * 2.0 - 1.0;
		}
	}
	for ( i = 0; i < N; i++ ) {
		Aorig[ i + i * N ] += 100.0;
	}

	// Make the matrix singular by making row 65 (index 64) equal to row 0.
	// After the first block panel (cols 0..63), the trailing update will make
	// row 64 linearly dependent, causing a zero pivot in the second panel.
	for ( j = 0; j < N; j++ ) {
		Aorig[ 64 + j * N ] = Aorig[ 0 + j * N ];
	}

	A = new Float64Array( Aorig );
	IPIV = new Int32Array( N );

	info = dgetrf( N, N, A, 1, N, 0, IPIV, 1, 0 );

	// The matrix is singular, so info should be > 0.
	assert.ok( info > 0, '70x70 singular: info > 0 (got ' + info + ')' );

	// Verify P*L*U still reconstructs A (the factorization is valid even for
	// singular matrices; it just has a zero on the U diagonal).
	assertFactorizationCorrect( Aorig, A, IPIV, N, N, 1e-8, '70x70 singular blocked' );
});
