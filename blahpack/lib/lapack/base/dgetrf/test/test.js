

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
