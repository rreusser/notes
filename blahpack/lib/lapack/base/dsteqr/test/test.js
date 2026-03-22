'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Float64Array = require( '@stdlib/array/float64' );
var dsteqr = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dsteqr.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual + ' (relErr=' + relErr + ')' );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}

/**
* Verifies Z^T * Z = I (orthogonality of eigenvectors).
*/
function assertOrthogonal( Z, N, tol, msg ) {
	var val;
	var i;
	var j;
	var k;
	for ( i = 0; i < N; i++ ) {
		for ( j = 0; j < N; j++ ) {
			val = 0.0;
			for ( k = 0; k < N; k++ ) {
				// Col-major: Z[k, i] = Z[k + i*N]
				val += Z[ k + i * N ] * Z[ k + j * N ];
			}
			if ( i === j ) {
				assertClose( val, 1.0, tol, msg + ': Z^T*Z[' + i + ',' + j + '] should be 1' );
			} else {
				assert.ok( Math.abs( val ) <= tol, msg + ': Z^T*Z[' + i + ',' + j + '] should be 0, got ' + val );
			}
		}
	}
}

/**
* Verify T * Z = Z * diag(d): for each eigenvector column j,
* check that T * z_j = d_j * z_j, where T is the original tridiagonal matrix.
*
* @param {Float64Array} Z - eigenvector matrix (col-major, N x N)
* @param {Float64Array} d - eigenvalues (length N)
* @param {Float64Array} origD - original diagonal (length N)
* @param {Float64Array} origE - original subdiagonal (length N-1)
* @param {integer} N - order
* @param {number} tol - tolerance
* @param {string} msg - message prefix
*/
function assertEigendecomp( Z, d, origD, origE, N, tol, msg ) {
	var Tz_k;
	var dz_k;
	var err;
	var i;
	var j;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < N; i++ ) {
			// (T * z_j)[i] = origD[i]*Z[i,j] + origE[i]*Z[i+1,j] + origE[i-1]*Z[i-1,j]
			Tz_k = origD[ i ] * Z[ i + j * N ];
			if ( i < N - 1 ) {
				Tz_k += origE[ i ] * Z[ ( i + 1 ) + j * N ];
			}
			if ( i > 0 ) {
				Tz_k += origE[ i - 1 ] * Z[ ( i - 1 ) + j * N ];
			}
			dz_k = d[ j ] * Z[ i + j * N ];
			err = Math.abs( Tz_k - dz_k );
			assert.ok( err <= tol, msg + ': T*z[' + i + ',' + j + '] - d[' + j + ']*z[' + i + ',' + j + '] = ' + err );
		}
	}
}


// TESTS //

test( 'dsteqr: COMPZ=I, 4x4 tridiagonal matrix', function t() {
	var tc = findCase( 'compz_I_4x4' );
	var origD = [ 2.0, 2.0, 2.0, 2.0 ];
	var origE = [ 1.0, 1.0, 1.0 ];
	var N = 4;
	var d = new Float64Array( origD );
	var e = new Float64Array( origE );
	var Z = new Float64Array( N * N );
	var WORK = new Float64Array( 2 * ( N - 1 ) );
	var info;

	info = dsteqr( 'I', N, d, 1, 0, e, 1, 0, Z, 1, N, 0, WORK, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( d ), tc.d, 1e-14, 'd' );
	assertOrthogonal( Z, N, 1e-14, 'orthogonality' );
	assertEigendecomp( Z, d, origD, origE, N, 1e-13, 'eigendecomp' );
});

test( 'dsteqr: COMPZ=V, 4x4 with identity Z', function t() {
	var tc = findCase( 'compz_V_4x4' );
	var origD = [ 2.0, 2.0, 2.0, 2.0 ];
	var origE = [ 1.0, 1.0, 1.0 ];
	var N = 4;
	var d = new Float64Array( origD );
	var e = new Float64Array( origE );
	var Z = new Float64Array( N * N );
	var WORK = new Float64Array( 2 * ( N - 1 ) );
	var info;
	var i;

	// Initialize Z to identity
	for ( i = 0; i < N; i++ ) {
		Z[ i + i * N ] = 1.0;
	}

	info = dsteqr( 'V', N, d, 1, 0, e, 1, 0, Z, 1, N, 0, WORK, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( d ), tc.d, 1e-14, 'd' );
	assertOrthogonal( Z, N, 1e-14, 'orthogonality' );
	assertEigendecomp( Z, d, origD, origE, N, 1e-13, 'eigendecomp' );
});

test( 'dsteqr: COMPZ=N, eigenvalues only', function t() {
	var tc = findCase( 'compz_N_4x4' );
	var N = 4;
	var d = new Float64Array( [ 2.0, 2.0, 2.0, 2.0 ] );
	var e = new Float64Array( [ 1.0, 1.0, 1.0 ] );
	var Z = new Float64Array( 1 ); // not referenced
	var WORK = new Float64Array( 1 ); // not used when COMPZ='N'
	var info;

	info = dsteqr( 'N', N, d, 1, 0, e, 1, 0, Z, 1, N, 0, WORK, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( d ), tc.d, 1e-14, 'd' );
});

test( 'dsteqr: N=1, COMPZ=I', function t() {
	var tc = findCase( 'n1_compz_I' );
	var N = 1;
	var d = new Float64Array( [ 5.0 ] );
	var e = new Float64Array( 0 );
	var Z = new Float64Array( 1 );
	var WORK = new Float64Array( 1 );
	var info;

	info = dsteqr( 'I', N, d, 1, 0, e, 1, 0, Z, 1, 1, 0, WORK, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( d ), tc.d, 1e-14, 'd' );
	assertArrayClose( Array.from( Z ), tc.z, 1e-14, 'z' );
});

test( 'dsteqr: N=2, COMPZ=I', function t() {
	var tc = findCase( 'n2_compz_I' );
	var origD = [ 3.0, 1.0 ];
	var origE = [ 2.0 ];
	var N = 2;
	var d = new Float64Array( origD );
	var e = new Float64Array( origE );
	var Z = new Float64Array( N * N );
	var WORK = new Float64Array( 2 );
	var info;

	info = dsteqr( 'I', N, d, 1, 0, e, 1, 0, Z, 1, N, 0, WORK, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( d ), tc.d, 1e-14, 'd' );
	assertOrthogonal( Z, N, 1e-14, 'orthogonality' );
	assertEigendecomp( Z, d, origD, origE, N, 1e-13, 'eigendecomp' );
});

test( 'dsteqr: N=0 edge case', function t() {
	var tc = findCase( 'n0' );
	var info;

	info = dsteqr( 'I', 0, new Float64Array( 0 ), 1, 0, new Float64Array( 0 ), 1, 0, new Float64Array( 0 ), 1, 1, 0, new Float64Array( 0 ), 1, 0 );
	assert.equal( info, tc.info );
});

test( 'dsteqr: already-diagonal matrix, COMPZ=I', function t() {
	var tc = findCase( 'diagonal_compz_I' );
	var origD = [ 4.0, 1.0, 3.0, 2.0 ];
	var origE = [ 0.0, 0.0, 0.0 ];
	var N = 4;
	var d = new Float64Array( origD );
	var e = new Float64Array( origE );
	var Z = new Float64Array( N * N );
	var WORK = new Float64Array( 2 * ( N - 1 ) );
	var info;

	info = dsteqr( 'I', N, d, 1, 0, e, 1, 0, Z, 1, N, 0, WORK, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( d ), tc.d, 1e-14, 'd' );

	// For diagonal matrix, eigenvalues are the diagonal sorted ascending:
	// d = [1, 2, 3, 4] and Z is a permutation matrix
	assertOrthogonal( Z, N, 1e-14, 'orthogonality' );
	assertEigendecomp( Z, d, origD, origE, N, 1e-13, 'eigendecomp' );
});

test( 'dsteqr: 6x6 matrix, COMPZ=I', function t() {
	var tc = findCase( 'n6_compz_I' );
	var origD = [ 4.0, 3.0, 2.0, 1.0, 5.0, 6.0 ];
	var origE = [ 1.0, 0.5, 0.25, 0.125, 2.0 ];
	var N = 6;
	var d = new Float64Array( origD );
	var e = new Float64Array( origE );
	var Z = new Float64Array( N * N );
	var WORK = new Float64Array( 2 * ( N - 1 ) );
	var info;

	info = dsteqr( 'I', N, d, 1, 0, e, 1, 0, Z, 1, N, 0, WORK, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( d ), tc.d, 1e-13, 'd' );
	assertOrthogonal( Z, N, 1e-13, 'orthogonality' );
	assertEigendecomp( Z, d, origD, origE, N, 1e-12, 'eigendecomp' );
});

test( 'dsteqr: COMPZ=V, 4x4 with permuted Z', function t() {
	var tc = findCase( 'compz_V_permuted' );
	var origD = [ 2.0, 2.0, 2.0, 2.0 ];
	var origE = [ 1.0, 1.0, 1.0 ];
	var N = 4;
	var d = new Float64Array( origD );
	var e = new Float64Array( origE );
	var Z = new Float64Array( N * N );
	var WORK = new Float64Array( 2 * ( N - 1 ) );
	var info;

	// Initialize Z to permutation (swap cols 1 and 2)
	// Col-major: Z[row + col*N]
	Z[ 0 + 1 * N ] = 1.0; // Z(1,2) = 1
	Z[ 1 + 0 * N ] = 1.0; // Z(2,1) = 1
	Z[ 2 + 2 * N ] = 1.0; // Z(3,3) = 1
	Z[ 3 + 3 * N ] = 1.0; // Z(4,4) = 1

	info = dsteqr( 'V', N, d, 1, 0, e, 1, 0, Z, 1, N, 0, WORK, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( d ), tc.d, 1e-14, 'd' );
	// For COMPZ='V', Z columns represent the eigenvectors in the original
	// basis. With the permutation Z0, the result is Z0 * Q where Q are the
	// tridiagonal eigenvectors. We check orthogonality of the result columns.
	assertOrthogonal( Z, N, 1e-14, 'orthogonality' );
});

test( 'dsteqr: N=2, COMPZ=N', function t() {
	var tc = findCase( 'n2_compz_N' );
	var N = 2;
	var d = new Float64Array( [ 3.0, 1.0 ] );
	var e = new Float64Array( [ 2.0 ] );
	var Z = new Float64Array( 1 );
	var WORK = new Float64Array( 1 );
	var info;

	info = dsteqr( 'N', N, d, 1, 0, e, 1, 0, Z, 1, N, 0, WORK, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( d ), tc.d, 1e-14, 'd' );
});

test( 'dsteqr: invalid COMPZ returns -1', function t() {
	var info;
	info = dsteqr( 'X', 2, new Float64Array( 2 ), 1, 0, new Float64Array( 1 ), 1, 0, new Float64Array( 4 ), 1, 2, 0, new Float64Array( 2 ), 1, 0 );
	assert.equal( info, -1 );
});

test( 'dsteqr: QR iteration path - descending diagonal, COMPZ=I', function t() {
	// This exercises the QR branch (lend < l) because abs(D[end]) < abs(D[start])
	// with a tridiagonal matrix where the last element is much smaller
	var origD = [ 10.0, 5.0, 1.0, 0.1 ];
	var origE = [ 1.0, 1.0, 1.0 ];
	var N = 4;
	var d = new Float64Array( origD );
	var e = new Float64Array( origE );
	var Z = new Float64Array( N * N );
	var WORK = new Float64Array( 2 * ( N - 1 ) );
	var info;

	info = dsteqr( 'I', N, d, 1, 0, e, 1, 0, Z, 1, N, 0, WORK, 1, 0 );
	assert.equal( info, 0 );
	assertOrthogonal( Z, N, 1e-13, 'orthogonality' );
	assertEigendecomp( Z, d, origD, origE, N, 1e-12, 'eigendecomp' );

	// Eigenvalues should be sorted ascending
	var i;
	for ( i = 0; i < N - 1; i++ ) {
		assert.ok( d[ i ] <= d[ i + 1 ], 'eigenvalues ascending: d[' + i + ']=' + d[ i ] + ' <= d[' + ( i + 1 ) + ']=' + d[ i + 1 ] );
	}
});

test( 'dsteqr: QR iteration path - descending diagonal, COMPZ=N', function t() {
	// Same as above but eigenvalues only
	var origD = [ 10.0, 5.0, 1.0, 0.1 ];
	var origE = [ 1.0, 1.0, 1.0 ];
	var N = 4;
	var d = new Float64Array( origD );
	var e = new Float64Array( origE );
	var Z = new Float64Array( 1 );
	var WORK = new Float64Array( 1 );
	var info;

	info = dsteqr( 'N', N, d, 1, 0, e, 1, 0, Z, 1, N, 0, WORK, 1, 0 );
	assert.equal( info, 0 );

	// Eigenvalues should be sorted ascending
	var i;
	for ( i = 0; i < N - 1; i++ ) {
		assert.ok( d[ i ] <= d[ i + 1 ], 'eigenvalues ascending' );
	}
});

test( 'dsteqr: QR path with 2x2 subproblem, COMPZ=I', function t() {
	// A 3x3 where the QR path encounters a 2x2 subproblem
	var origD = [ 100.0, 2.0, 0.01 ];
	var origE = [ 0.5, 0.5 ];
	var N = 3;
	var d = new Float64Array( origD );
	var e = new Float64Array( origE );
	var Z = new Float64Array( N * N );
	var WORK = new Float64Array( 2 * ( N - 1 ) );
	var info;

	info = dsteqr( 'I', N, d, 1, 0, e, 1, 0, Z, 1, N, 0, WORK, 1, 0 );
	assert.equal( info, 0 );
	assertOrthogonal( Z, N, 1e-13, 'orthogonality' );
	assertEigendecomp( Z, d, origD, origE, N, 1e-11, 'eigendecomp' );
});

test( 'dsteqr: QR path with 2x2 subproblem, COMPZ=N', function t() {
	var origD = [ 100.0, 2.0, 0.01 ];
	var origE = [ 0.5, 0.5 ];
	var N = 3;
	var d = new Float64Array( origD );
	var e = new Float64Array( origE );
	var Z = new Float64Array( 1 );
	var WORK = new Float64Array( 1 );
	var info;

	info = dsteqr( 'N', N, d, 1, 0, e, 1, 0, Z, 1, N, 0, WORK, 1, 0 );
	assert.equal( info, 0 );
	// Ascending order
	var i;
	for ( i = 0; i < N - 1; i++ ) {
		assert.ok( d[ i ] <= d[ i + 1 ], 'eigenvalues ascending' );
	}
});

test( 'dsteqr: matrix with very small off-diag elements triggers negligible E branch', function t() {
	// Very small off-diagonal elements that should be set to zero by the
	// convergence test e[m] <= sqrt(|d[m]|)*sqrt(|d[m+1]|)*eps
	var N = 3;
	var origD = [ 1.0, 1.0, 1.0 ];
	var origE = [ 1e-17, 1e-17 ];
	var d = new Float64Array( origD );
	var e = new Float64Array( origE );
	var Z = new Float64Array( N * N );
	var WORK = new Float64Array( 2 * ( N - 1 ) );
	var info;

	info = dsteqr( 'I', N, d, 1, 0, e, 1, 0, Z, 1, N, 0, WORK, 1, 0 );
	assert.equal( info, 0 );
	// All eigenvalues should be approximately 1.0
	var i;
	for ( i = 0; i < N; i++ ) {
		assertClose( d[ i ], 1.0, 1e-14, 'd[' + i + ']' );
	}
});

test( 'dsteqr: very large values trigger ssfmax scaling', function t() {
	// Values large enough to trigger iscale = 1 (anorm > ssfmax ~ 2.23e153)
	var N = 3;
	var bigVal = 1e154;
	var origD = [ 2.0 * bigVal, 2.0 * bigVal, 2.0 * bigVal ];
	var origE = [ 1.0 * bigVal, 1.0 * bigVal ];
	var d = new Float64Array( origD );
	var e = new Float64Array( origE );
	var Z = new Float64Array( N * N );
	var WORK = new Float64Array( 2 * ( N - 1 ) );
	var info;

	info = dsteqr( 'I', N, d, 1, 0, e, 1, 0, Z, 1, N, 0, WORK, 1, 0 );
	assert.equal( info, 0 );
	assertOrthogonal( Z, N, 1e-12, 'orthogonality' );

	// Eigenvalues should be proportional to the small case
	// The eigenvalues of [2,1;1,2,1;1,2] are 2-sqrt(2), 2, 2+sqrt(2)
	assertClose( d[ 0 ] / bigVal, 2.0 - Math.sqrt( 2.0 ), 1e-12, 'scaled d[0]' );
	assertClose( d[ 1 ] / bigVal, 2.0, 1e-12, 'scaled d[1]' );
	assertClose( d[ 2 ] / bigVal, 2.0 + Math.sqrt( 2.0 ), 1e-12, 'scaled d[2]' );
});

test( 'dsteqr: very small values trigger ssfmin scaling', function t() {
	// Values small enough to trigger iscale = 2 (anorm < ssfmin ~ 1.21e-122)
	var N = 3;
	var smallVal = 1e-200;
	var origD = [ 2.0 * smallVal, 2.0 * smallVal, 2.0 * smallVal ];
	var origE = [ 1.0 * smallVal, 1.0 * smallVal ];
	var d = new Float64Array( origD );
	var e = new Float64Array( origE );
	var Z = new Float64Array( N * N );
	var WORK = new Float64Array( 2 * ( N - 1 ) );
	var info;

	info = dsteqr( 'I', N, d, 1, 0, e, 1, 0, Z, 1, N, 0, WORK, 1, 0 );
	assert.equal( info, 0 );
	assertOrthogonal( Z, N, 1e-12, 'orthogonality' );

	// Same proportional eigenvalues
	assertClose( d[ 0 ] / smallVal, 2.0 - Math.sqrt( 2.0 ), 1e-10, 'scaled d[0]' );
	assertClose( d[ 1 ] / smallVal, 2.0, 1e-10, 'scaled d[1]' );
	assertClose( d[ 2 ] / smallVal, 2.0 + Math.sqrt( 2.0 ), 1e-10, 'scaled d[2]' );
});

test( 'dsteqr: very large values trigger ssfmax scaling in QR path', function t() {
	// Same as above but with descending diagonal to trigger QR path
	var N = 3;
	var bigVal = 1e154;
	var origD = [ 10.0 * bigVal, 2.0 * bigVal, 0.1 * bigVal ];
	var origE = [ 1.0 * bigVal, 1.0 * bigVal ];
	var d = new Float64Array( origD );
	var e = new Float64Array( origE );
	var Z = new Float64Array( N * N );
	var WORK = new Float64Array( 2 * ( N - 1 ) );
	var info;

	info = dsteqr( 'I', N, d, 1, 0, e, 1, 0, Z, 1, N, 0, WORK, 1, 0 );
	assert.equal( info, 0 );
	assertOrthogonal( Z, N, 1e-11, 'orthogonality' );
});

test( 'dsteqr: block splitting - matrix with zero off-diagonal element', function t() {
	// Matrix splits into two blocks at the zero off-diagonal element
	var N = 4;
	var origD = [ 3.0, 1.0, 5.0, 2.0 ];
	var origE = [ 0.5, 0.0, 0.5 ];
	var d = new Float64Array( origD );
	var e = new Float64Array( origE );
	var Z = new Float64Array( N * N );
	var WORK = new Float64Array( 2 * ( N - 1 ) );
	var info;

	info = dsteqr( 'I', N, d, 1, 0, e, 1, 0, Z, 1, N, 0, WORK, 1, 0 );
	assert.equal( info, 0 );

	// Eigenvalues should be sorted ascending
	var i;
	for ( i = 0; i < N - 1; i++ ) {
		assert.ok( d[ i ] <= d[ i + 1 ], 'eigenvalues ascending' );
	}
	assertOrthogonal( Z, N, 1e-14, 'orthogonality' );
	assertEigendecomp( Z, d, origD, origE, N, 1e-13, 'eigendecomp' );
});
