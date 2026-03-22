'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zggev = require( '../lib/base.js' );

// Load fixture
var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zggev.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) {
	return JSON.parse( line );
});

/**
* Assert that two floating-point numbers are close.
*/
function assertClose( actual, expected, tol, msg ) {
	var relErr;
	if ( expected === 0.0 ) {
		assert.ok( Math.abs( actual ) <= tol, msg + ': expected 0, got ' + actual );
		return;
	}
	relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual + ' (relErr=' + relErr + ')' );
}

/**
* Assert that two complex arrays are close, element by element.
* Operates on a Float64 view of the complex array.
*/
function assertComplexArrayClose( actual, offsetActual, expected, tol, msg ) {
	var i;
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ offsetActual + i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}

test( 'zggev: N=0 quick return', function t() {
	var ALPHA = new Complex128Array( 1 );
	var BETA = new Complex128Array( 1 );
	var VL = new Complex128Array( 1 );
	var VR = new Complex128Array( 1 );
	var A = new Complex128Array( 1 );
	var B = new Complex128Array( 1 );
	var info;

	info = zggev( 'N', 'N', 0,
		A, 1, 1, 0,
		B, 1, 1, 0,
		ALPHA, 1, 0,
		BETA, 1, 0,
		VL, 1, 1, 0,
		VR, 1, 1, 0
	);
	assert.strictEqual( info, 0 );
});

test( 'zggev: N=1 with eigenvectors', function t() {
	var tc = fixture.find( function find( t ) { return t.name === 'n_eq_1'; });
	var N = 1;
	var LDA = N;
	var A = new Complex128Array( LDA * N );
	var B = new Complex128Array( LDA * N );
	var ALPHA = new Complex128Array( N );
	var BETA = new Complex128Array( N );
	var VL = new Complex128Array( N * N );
	var VR = new Complex128Array( N * N );
	var Av = reinterpret( A, 0 );
	var Bv = reinterpret( B, 0 );
	var ALPHAv = reinterpret( ALPHA, 0 );
	var BETAv = reinterpret( BETA, 0 );
	var VLv = reinterpret( VL, 0 );
	var VRv = reinterpret( VR, 0 );
	var info;
	var tol = 1e-13;

	// A(1,1) = (3, 1)
	Av[ 0 ] = 3.0; Av[ 1 ] = 1.0;
	// B(1,1) = (2, 0.5)
	Bv[ 0 ] = 2.0; Bv[ 1 ] = 0.5;

	info = zggev( 'V', 'V', N,
		A, 1, LDA, 0,
		B, 1, LDA, 0,
		ALPHA, 1, 0,
		BETA, 1, 0,
		VL, 1, N, 0,
		VR, 1, N, 0
	);
	assert.strictEqual( info, tc.info );
	assertComplexArrayClose( ALPHAv, 0, tc.alpha, tol, 'alpha' );
	assertComplexArrayClose( BETAv, 0, tc.beta, tol, 'beta' );
	assertComplexArrayClose( VLv, 0, tc.VL, tol, 'VL' );
	assertComplexArrayClose( VRv, 0, tc.VR, tol, 'VR' );
});

test( 'zggev: 3x3 right eigenvectors only', function t() {
	var tc = fixture.find( function find( t ) { return t.name === 'right_evec_3x3'; });
	var N = 3;
	var LDA = N;
	var A = new Complex128Array( LDA * N );
	var B = new Complex128Array( LDA * N );
	var ALPHA = new Complex128Array( N );
	var BETA = new Complex128Array( N );
	var VL = new Complex128Array( N * N );
	var VR = new Complex128Array( N * N );
	var Av = reinterpret( A, 0 );
	var Bv = reinterpret( B, 0 );
	var ALPHAv = reinterpret( ALPHA, 0 );
	var BETAv = reinterpret( BETA, 0 );
	var VRv = reinterpret( VR, 0 );
	var info;
	var tol = 1e-12;

	// Column-major: A(i,j) at index 2*(i + j*LDA)
	// A matrix
	Av[ 0 ] = 2.0; Av[ 1 ] = 1.0;     // A(1,1)
	Av[ 2 ] = 1.0; Av[ 3 ] = -1.0;    // A(2,1)
	Av[ 4 ] = 0.5; Av[ 5 ] = 0.5;     // A(3,1)
	Av[ 6 ] = 1.0; Av[ 7 ] = 0.5;     // A(1,2)
	Av[ 8 ] = 3.0; Av[ 9 ] = 0.0;     // A(2,2)
	Av[ 10 ] = 0.5; Av[ 11 ] = -0.5;  // A(3,2)
	Av[ 12 ] = 0.5; Av[ 13 ] = -0.5;  // A(1,3)
	Av[ 14 ] = 1.0; Av[ 15 ] = 1.0;   // A(2,3)
	Av[ 16 ] = 4.0; Av[ 17 ] = -1.0;  // A(3,3)

	// B matrix
	Bv[ 0 ] = 3.0; Bv[ 1 ] = 0.0;
	Bv[ 2 ] = 0.5; Bv[ 3 ] = -0.5;
	Bv[ 4 ] = 0.0; Bv[ 5 ] = 0.5;
	Bv[ 6 ] = 1.0; Bv[ 7 ] = 0.5;
	Bv[ 8 ] = 2.0; Bv[ 9 ] = 1.0;
	Bv[ 10 ] = 0.5; Bv[ 11 ] = 0.0;
	Bv[ 12 ] = 0.5; Bv[ 13 ] = 0.5;
	Bv[ 14 ] = 1.0; Bv[ 15 ] = 0.0;
	Bv[ 16 ] = 1.0; Bv[ 17 ] = 0.5;

	info = zggev( 'N', 'V', N,
		A, 1, LDA, 0,
		B, 1, LDA, 0,
		ALPHA, 1, 0,
		BETA, 1, 0,
		VL, 1, N, 0,
		VR, 1, N, 0
	);
	assert.strictEqual( info, tc.info );
	assertComplexArrayClose( ALPHAv, 0, tc.alpha, tol, 'alpha' );
	assertComplexArrayClose( BETAv, 0, tc.beta, tol, 'beta' );

	// Check VR columns
	assertComplexArrayClose( VRv, 0, tc.VR_col1, tol, 'VR_col1' );
	assertComplexArrayClose( VRv, 2 * N, tc.VR_col2, tol, 'VR_col2' );
	assertComplexArrayClose( VRv, 4 * N, tc.VR_col3, tol, 'VR_col3' );
});

test( 'zggev: 3x3 both eigenvectors', function t() {
	var tc = fixture.find( function find( t ) { return t.name === 'both_evec_3x3'; });
	var N = 3;
	var LDA = N;
	var A = new Complex128Array( LDA * N );
	var B = new Complex128Array( LDA * N );
	var ALPHA = new Complex128Array( N );
	var BETA = new Complex128Array( N );
	var VL = new Complex128Array( N * N );
	var VR = new Complex128Array( N * N );
	var Av = reinterpret( A, 0 );
	var Bv = reinterpret( B, 0 );
	var ALPHAv = reinterpret( ALPHA, 0 );
	var BETAv = reinterpret( BETA, 0 );
	var VLv = reinterpret( VL, 0 );
	var VRv = reinterpret( VR, 0 );
	var info;
	var tol = 1e-12;

	// Same matrices as right_evec_3x3
	Av[ 0 ] = 2.0; Av[ 1 ] = 1.0;
	Av[ 2 ] = 1.0; Av[ 3 ] = -1.0;
	Av[ 4 ] = 0.5; Av[ 5 ] = 0.5;
	Av[ 6 ] = 1.0; Av[ 7 ] = 0.5;
	Av[ 8 ] = 3.0; Av[ 9 ] = 0.0;
	Av[ 10 ] = 0.5; Av[ 11 ] = -0.5;
	Av[ 12 ] = 0.5; Av[ 13 ] = -0.5;
	Av[ 14 ] = 1.0; Av[ 15 ] = 1.0;
	Av[ 16 ] = 4.0; Av[ 17 ] = -1.0;

	Bv[ 0 ] = 3.0; Bv[ 1 ] = 0.0;
	Bv[ 2 ] = 0.5; Bv[ 3 ] = -0.5;
	Bv[ 4 ] = 0.0; Bv[ 5 ] = 0.5;
	Bv[ 6 ] = 1.0; Bv[ 7 ] = 0.5;
	Bv[ 8 ] = 2.0; Bv[ 9 ] = 1.0;
	Bv[ 10 ] = 0.5; Bv[ 11 ] = 0.0;
	Bv[ 12 ] = 0.5; Bv[ 13 ] = 0.5;
	Bv[ 14 ] = 1.0; Bv[ 15 ] = 0.0;
	Bv[ 16 ] = 1.0; Bv[ 17 ] = 0.5;

	info = zggev( 'V', 'V', N,
		A, 1, LDA, 0,
		B, 1, LDA, 0,
		ALPHA, 1, 0,
		BETA, 1, 0,
		VL, 1, N, 0,
		VR, 1, N, 0
	);
	assert.strictEqual( info, tc.info );
	assertComplexArrayClose( ALPHAv, 0, tc.alpha, tol, 'alpha' );
	assertComplexArrayClose( BETAv, 0, tc.beta, tol, 'beta' );

	assertComplexArrayClose( VLv, 0, tc.VL_col1, tol, 'VL_col1' );
	assertComplexArrayClose( VLv, 2 * N, tc.VL_col2, tol, 'VL_col2' );
	assertComplexArrayClose( VLv, 4 * N, tc.VL_col3, tol, 'VL_col3' );
	assertComplexArrayClose( VRv, 0, tc.VR_col1, tol, 'VR_col1' );
	assertComplexArrayClose( VRv, 2 * N, tc.VR_col2, tol, 'VR_col2' );
	assertComplexArrayClose( VRv, 4 * N, tc.VR_col3, tol, 'VR_col3' );
});

test( 'zggev: 3x3 eigenvalues only', function t() {
	var tc = fixture.find( function find( t ) { return t.name === 'eig_only_3x3'; });
	var N = 3;
	var LDA = N;
	var A = new Complex128Array( LDA * N );
	var B = new Complex128Array( LDA * N );
	var ALPHA = new Complex128Array( N );
	var BETA = new Complex128Array( N );
	var VL = new Complex128Array( N * N );
	var VR = new Complex128Array( N * N );
	var Av = reinterpret( A, 0 );
	var Bv = reinterpret( B, 0 );
	var ALPHAv = reinterpret( ALPHA, 0 );
	var BETAv = reinterpret( BETA, 0 );
	var info;
	var tol = 1e-12;

	Av[ 0 ] = 2.0; Av[ 1 ] = 1.0;
	Av[ 2 ] = 1.0; Av[ 3 ] = -1.0;
	Av[ 4 ] = 0.5; Av[ 5 ] = 0.5;
	Av[ 6 ] = 1.0; Av[ 7 ] = 0.5;
	Av[ 8 ] = 3.0; Av[ 9 ] = 0.0;
	Av[ 10 ] = 0.5; Av[ 11 ] = -0.5;
	Av[ 12 ] = 0.5; Av[ 13 ] = -0.5;
	Av[ 14 ] = 1.0; Av[ 15 ] = 1.0;
	Av[ 16 ] = 4.0; Av[ 17 ] = -1.0;

	Bv[ 0 ] = 3.0; Bv[ 1 ] = 0.0;
	Bv[ 2 ] = 0.5; Bv[ 3 ] = -0.5;
	Bv[ 4 ] = 0.0; Bv[ 5 ] = 0.5;
	Bv[ 6 ] = 1.0; Bv[ 7 ] = 0.5;
	Bv[ 8 ] = 2.0; Bv[ 9 ] = 1.0;
	Bv[ 10 ] = 0.5; Bv[ 11 ] = 0.0;
	Bv[ 12 ] = 0.5; Bv[ 13 ] = 0.5;
	Bv[ 14 ] = 1.0; Bv[ 15 ] = 0.0;
	Bv[ 16 ] = 1.0; Bv[ 17 ] = 0.5;

	info = zggev( 'N', 'N', N,
		A, 1, LDA, 0,
		B, 1, LDA, 0,
		ALPHA, 1, 0,
		BETA, 1, 0,
		VL, 1, N, 0,
		VR, 1, N, 0
	);
	assert.strictEqual( info, tc.info );
	assertComplexArrayClose( ALPHAv, 0, tc.alpha, tol, 'alpha' );
	assertComplexArrayClose( BETAv, 0, tc.beta, tol, 'beta' );
});

test( 'zggev: 2x2 diagonal', function t() {
	var tc = fixture.find( function find( t ) { return t.name === 'diagonal_2x2'; });
	var N = 2;
	var LDA = N;
	var A = new Complex128Array( LDA * N );
	var B = new Complex128Array( LDA * N );
	var ALPHA = new Complex128Array( N );
	var BETA = new Complex128Array( N );
	var VL = new Complex128Array( N * N );
	var VR = new Complex128Array( N * N );
	var Av = reinterpret( A, 0 );
	var Bv = reinterpret( B, 0 );
	var ALPHAv = reinterpret( ALPHA, 0 );
	var BETAv = reinterpret( BETA, 0 );
	var VLv = reinterpret( VL, 0 );
	var VRv = reinterpret( VR, 0 );
	var info;
	var tol = 1e-13;

	Av[ 0 ] = 4.0; Av[ 1 ] = 0.0;
	Av[ 2 ] = 0.0; Av[ 3 ] = 0.0;
	Av[ 4 ] = 0.0; Av[ 5 ] = 0.0;
	Av[ 6 ] = 6.0; Av[ 7 ] = 0.0;

	Bv[ 0 ] = 2.0; Bv[ 1 ] = 0.0;
	Bv[ 2 ] = 0.0; Bv[ 3 ] = 0.0;
	Bv[ 4 ] = 0.0; Bv[ 5 ] = 0.0;
	Bv[ 6 ] = 3.0; Bv[ 7 ] = 0.0;

	info = zggev( 'V', 'V', N,
		A, 1, LDA, 0,
		B, 1, LDA, 0,
		ALPHA, 1, 0,
		BETA, 1, 0,
		VL, 1, N, 0,
		VR, 1, N, 0
	);
	assert.strictEqual( info, tc.info );
	assertComplexArrayClose( ALPHAv, 0, tc.alpha, tol, 'alpha' );
	assertComplexArrayClose( BETAv, 0, tc.beta, tol, 'beta' );
	assertComplexArrayClose( VLv, 0, tc.VL_col1, tol, 'VL_col1' );
	assertComplexArrayClose( VLv, 2 * N, tc.VL_col2, tol, 'VL_col2' );
	assertComplexArrayClose( VRv, 0, tc.VR_col1, tol, 'VR_col1' );
	assertComplexArrayClose( VRv, 2 * N, tc.VR_col2, tol, 'VR_col2' );
});

test( 'zggev: N=1 no eigenvectors', function t() {
	var tc = fixture.find( function find( t ) { return t.name === 'n_eq_1_noevec'; });
	var N = 1;
	var LDA = N;
	var A = new Complex128Array( LDA * N );
	var B = new Complex128Array( LDA * N );
	var ALPHA = new Complex128Array( N );
	var BETA = new Complex128Array( N );
	var VL = new Complex128Array( N * N );
	var VR = new Complex128Array( N * N );
	var Av = reinterpret( A, 0 );
	var Bv = reinterpret( B, 0 );
	var ALPHAv = reinterpret( ALPHA, 0 );
	var BETAv = reinterpret( BETA, 0 );
	var info;
	var tol = 1e-13;

	Av[ 0 ] = 5.0; Av[ 1 ] = 2.0;
	Bv[ 0 ] = 1.0; Bv[ 1 ] = 0.0;

	info = zggev( 'N', 'N', N,
		A, 1, LDA, 0,
		B, 1, LDA, 0,
		ALPHA, 1, 0,
		BETA, 1, 0,
		VL, 1, N, 0,
		VR, 1, N, 0
	);
	assert.strictEqual( info, tc.info );
	assertComplexArrayClose( ALPHAv, 0, tc.alpha, tol, 'alpha' );
	assertComplexArrayClose( BETAv, 0, tc.beta, tol, 'beta' );
});

test( 'zggev: 2x2 left eigenvectors only', function t() {
	var tc = fixture.find( function find( t ) { return t.name === 'left_evec_2x2'; });
	var N = 2;
	var LDA = N;
	var A = new Complex128Array( LDA * N );
	var B = new Complex128Array( LDA * N );
	var ALPHA = new Complex128Array( N );
	var BETA = new Complex128Array( N );
	var VL = new Complex128Array( N * N );
	var VR = new Complex128Array( N * N );
	var Av = reinterpret( A, 0 );
	var Bv = reinterpret( B, 0 );
	var ALPHAv = reinterpret( ALPHA, 0 );
	var BETAv = reinterpret( BETA, 0 );
	var VLv = reinterpret( VL, 0 );
	var info;
	var tol = 1e-12;

	// A = [[1+2i, 3-i], [0.5+0.5i, 4+i]]
	Av[ 0 ] = 1.0; Av[ 1 ] = 2.0;   // A(1,1)
	Av[ 2 ] = 0.5; Av[ 3 ] = 0.5;   // A(2,1)
	Av[ 4 ] = 3.0; Av[ 5 ] = -1.0;  // A(1,2)
	Av[ 6 ] = 4.0; Av[ 7 ] = 1.0;   // A(2,2)

	// B = [[2, 1+i], [0, 3-0.5i]]
	Bv[ 0 ] = 2.0; Bv[ 1 ] = 0.0;
	Bv[ 2 ] = 0.0; Bv[ 3 ] = 0.0;
	Bv[ 4 ] = 1.0; Bv[ 5 ] = 1.0;
	Bv[ 6 ] = 3.0; Bv[ 7 ] = -0.5;

	info = zggev( 'V', 'N', N,
		A, 1, LDA, 0,
		B, 1, LDA, 0,
		ALPHA, 1, 0,
		BETA, 1, 0,
		VL, 1, N, 0,
		VR, 1, N, 0
	);
	assert.strictEqual( info, tc.info );
	assertComplexArrayClose( ALPHAv, 0, tc.alpha, tol, 'alpha' );
	assertComplexArrayClose( BETAv, 0, tc.beta, tol, 'beta' );
	assertComplexArrayClose( VLv, 0, tc.VL_col1, tol, 'VL_col1' );
	assertComplexArrayClose( VLv, 2 * N, tc.VL_col2, tol, 'VL_col2' );
});
