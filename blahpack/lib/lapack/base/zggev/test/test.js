'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var zggev = require( '../lib/base.js' );

// Load fixture
var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zggev.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) {
	return JSON.parse( line );
});

/**
* Assert that two floating-point numbers are close.
*
* @private
* @param {number} actual - actual value
* @param {number} expected - expected value
* @param {number} tol - relative tolerance
* @param {string} msg - error message
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
*
* @private
* @param {Float64Array} actual - actual interleaved complex array
* @param {number} offsetActual - starting index
* @param {Array} expected - expected values array
* @param {number} tol - relative tolerance
* @param {string} msg - error message prefix
*/
function assertComplexArrayClose( actual, offsetActual, expected, tol, msg ) {
	var i;
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ offsetActual + i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}

test( 'zggev: N=0 quick return', function t() {
	var ALPHA = new Float64Array( 2 );
	var BETA = new Float64Array( 2 );
	var VL = new Float64Array( 2 );
	var VR = new Float64Array( 2 );
	var A = new Float64Array( 2 );
	var B = new Float64Array( 2 );
	var info;

	info = zggev( 'N', 'N', 0,
		A, 2, 2, 0,
		B, 2, 2, 0,
		ALPHA, 1, 0,
		BETA, 1, 0,
		VL, 2, 2, 0,
		VR, 2, 2, 0
	);
	assert.strictEqual( info, 0 );
});

test( 'zggev: N=1 with eigenvectors', function t() {
	var tc = fixture.find( function find( t ) { return t.name === 'n_eq_1'; });
	var N = 1;
	var LDA = N;
	var A = new Float64Array( 2 * LDA * N );
	var B = new Float64Array( 2 * LDA * N );
	var ALPHA = new Float64Array( 2 * N );
	var BETA = new Float64Array( 2 * N );
	var VL = new Float64Array( 2 * N * N );
	var VR = new Float64Array( 2 * N * N );
	var info;
	var tol = 1e-13;

	// A(1,1) = (3, 1)
	A[ 0 ] = 3.0; A[ 1 ] = 1.0;
	// B(1,1) = (2, 0.5)
	B[ 0 ] = 2.0; B[ 1 ] = 0.5;

	info = zggev( 'V', 'V', N,
		A, 2, 2 * LDA, 0,
		B, 2, 2 * LDA, 0,
		ALPHA, 1, 0,
		BETA, 1, 0,
		VL, 2, 2 * N, 0,
		VR, 2, 2 * N, 0
	);
	assert.strictEqual( info, tc.info );
	assertComplexArrayClose( ALPHA, 0, tc.alpha, tol, 'alpha' );
	assertComplexArrayClose( BETA, 0, tc.beta, tol, 'beta' );
	assertComplexArrayClose( VL, 0, tc.VL, tol, 'VL' );
	assertComplexArrayClose( VR, 0, tc.VR, tol, 'VR' );
});

test( 'zggev: 3x3 right eigenvectors only', function t() {
	var tc = fixture.find( function find( t ) { return t.name === 'right_evec_3x3'; });
	var N = 3;
	var LDA = N;
	var A = new Float64Array( 2 * LDA * N );
	var B = new Float64Array( 2 * LDA * N );
	var ALPHA = new Float64Array( 2 * N );
	var BETA = new Float64Array( 2 * N );
	var VL = new Float64Array( 2 * N * N );
	var VR = new Float64Array( 2 * N * N );
	var info;
	var tol = 1e-12;

	// Column-major: A(i,j) at index 2*(i + j*LDA)
	// A matrix
	A[ 0 ] = 2.0; A[ 1 ] = 1.0;     // A(1,1)
	A[ 2 ] = 1.0; A[ 3 ] = -1.0;    // A(2,1)
	A[ 4 ] = 0.5; A[ 5 ] = 0.5;     // A(3,1)
	A[ 6 ] = 1.0; A[ 7 ] = 0.5;     // A(1,2)
	A[ 8 ] = 3.0; A[ 9 ] = 0.0;     // A(2,2)
	A[ 10 ] = 0.5; A[ 11 ] = -0.5;  // A(3,2)
	A[ 12 ] = 0.5; A[ 13 ] = -0.5;  // A(1,3)
	A[ 14 ] = 1.0; A[ 15 ] = 1.0;   // A(2,3)
	A[ 16 ] = 4.0; A[ 17 ] = -1.0;  // A(3,3)

	// B matrix
	B[ 0 ] = 3.0; B[ 1 ] = 0.0;
	B[ 2 ] = 0.5; B[ 3 ] = -0.5;
	B[ 4 ] = 0.0; B[ 5 ] = 0.5;
	B[ 6 ] = 1.0; B[ 7 ] = 0.5;
	B[ 8 ] = 2.0; B[ 9 ] = 1.0;
	B[ 10 ] = 0.5; B[ 11 ] = 0.0;
	B[ 12 ] = 0.5; B[ 13 ] = 0.5;
	B[ 14 ] = 1.0; B[ 15 ] = 0.0;
	B[ 16 ] = 1.0; B[ 17 ] = 0.5;

	info = zggev( 'N', 'V', N,
		A, 2, 2 * LDA, 0,
		B, 2, 2 * LDA, 0,
		ALPHA, 1, 0,
		BETA, 1, 0,
		VL, 2, 2 * N, 0,
		VR, 2, 2 * N, 0
	);
	assert.strictEqual( info, tc.info );
	assertComplexArrayClose( ALPHA, 0, tc.alpha, tol, 'alpha' );
	assertComplexArrayClose( BETA, 0, tc.beta, tol, 'beta' );

	// Check VR columns
	assertComplexArrayClose( VR, 0, tc.VR_col1, tol, 'VR_col1' );
	assertComplexArrayClose( VR, 2 * N, tc.VR_col2, tol, 'VR_col2' );
	assertComplexArrayClose( VR, 4 * N, tc.VR_col3, tol, 'VR_col3' );
});

test( 'zggev: 3x3 both eigenvectors', function t() {
	var tc = fixture.find( function find( t ) { return t.name === 'both_evec_3x3'; });
	var N = 3;
	var LDA = N;
	var A = new Float64Array( 2 * LDA * N );
	var B = new Float64Array( 2 * LDA * N );
	var ALPHA = new Float64Array( 2 * N );
	var BETA = new Float64Array( 2 * N );
	var VL = new Float64Array( 2 * N * N );
	var VR = new Float64Array( 2 * N * N );
	var info;
	var tol = 1e-12;

	// Same matrices as right_evec_3x3
	A[ 0 ] = 2.0; A[ 1 ] = 1.0;
	A[ 2 ] = 1.0; A[ 3 ] = -1.0;
	A[ 4 ] = 0.5; A[ 5 ] = 0.5;
	A[ 6 ] = 1.0; A[ 7 ] = 0.5;
	A[ 8 ] = 3.0; A[ 9 ] = 0.0;
	A[ 10 ] = 0.5; A[ 11 ] = -0.5;
	A[ 12 ] = 0.5; A[ 13 ] = -0.5;
	A[ 14 ] = 1.0; A[ 15 ] = 1.0;
	A[ 16 ] = 4.0; A[ 17 ] = -1.0;

	B[ 0 ] = 3.0; B[ 1 ] = 0.0;
	B[ 2 ] = 0.5; B[ 3 ] = -0.5;
	B[ 4 ] = 0.0; B[ 5 ] = 0.5;
	B[ 6 ] = 1.0; B[ 7 ] = 0.5;
	B[ 8 ] = 2.0; B[ 9 ] = 1.0;
	B[ 10 ] = 0.5; B[ 11 ] = 0.0;
	B[ 12 ] = 0.5; B[ 13 ] = 0.5;
	B[ 14 ] = 1.0; B[ 15 ] = 0.0;
	B[ 16 ] = 1.0; B[ 17 ] = 0.5;

	info = zggev( 'V', 'V', N,
		A, 2, 2 * LDA, 0,
		B, 2, 2 * LDA, 0,
		ALPHA, 1, 0,
		BETA, 1, 0,
		VL, 2, 2 * N, 0,
		VR, 2, 2 * N, 0
	);
	assert.strictEqual( info, tc.info );
	assertComplexArrayClose( ALPHA, 0, tc.alpha, tol, 'alpha' );
	assertComplexArrayClose( BETA, 0, tc.beta, tol, 'beta' );

	assertComplexArrayClose( VL, 0, tc.VL_col1, tol, 'VL_col1' );
	assertComplexArrayClose( VL, 2 * N, tc.VL_col2, tol, 'VL_col2' );
	assertComplexArrayClose( VL, 4 * N, tc.VL_col3, tol, 'VL_col3' );
	assertComplexArrayClose( VR, 0, tc.VR_col1, tol, 'VR_col1' );
	assertComplexArrayClose( VR, 2 * N, tc.VR_col2, tol, 'VR_col2' );
	assertComplexArrayClose( VR, 4 * N, tc.VR_col3, tol, 'VR_col3' );
});

test( 'zggev: 3x3 eigenvalues only', function t() {
	var tc = fixture.find( function find( t ) { return t.name === 'eig_only_3x3'; });
	var N = 3;
	var LDA = N;
	var A = new Float64Array( 2 * LDA * N );
	var B = new Float64Array( 2 * LDA * N );
	var ALPHA = new Float64Array( 2 * N );
	var BETA = new Float64Array( 2 * N );
	var VL = new Float64Array( 2 * N * N );
	var VR = new Float64Array( 2 * N * N );
	var info;
	var tol = 1e-12;

	A[ 0 ] = 2.0; A[ 1 ] = 1.0;
	A[ 2 ] = 1.0; A[ 3 ] = -1.0;
	A[ 4 ] = 0.5; A[ 5 ] = 0.5;
	A[ 6 ] = 1.0; A[ 7 ] = 0.5;
	A[ 8 ] = 3.0; A[ 9 ] = 0.0;
	A[ 10 ] = 0.5; A[ 11 ] = -0.5;
	A[ 12 ] = 0.5; A[ 13 ] = -0.5;
	A[ 14 ] = 1.0; A[ 15 ] = 1.0;
	A[ 16 ] = 4.0; A[ 17 ] = -1.0;

	B[ 0 ] = 3.0; B[ 1 ] = 0.0;
	B[ 2 ] = 0.5; B[ 3 ] = -0.5;
	B[ 4 ] = 0.0; B[ 5 ] = 0.5;
	B[ 6 ] = 1.0; B[ 7 ] = 0.5;
	B[ 8 ] = 2.0; B[ 9 ] = 1.0;
	B[ 10 ] = 0.5; B[ 11 ] = 0.0;
	B[ 12 ] = 0.5; B[ 13 ] = 0.5;
	B[ 14 ] = 1.0; B[ 15 ] = 0.0;
	B[ 16 ] = 1.0; B[ 17 ] = 0.5;

	info = zggev( 'N', 'N', N,
		A, 2, 2 * LDA, 0,
		B, 2, 2 * LDA, 0,
		ALPHA, 1, 0,
		BETA, 1, 0,
		VL, 2, 2 * N, 0,
		VR, 2, 2 * N, 0
	);
	assert.strictEqual( info, tc.info );
	assertComplexArrayClose( ALPHA, 0, tc.alpha, tol, 'alpha' );
	assertComplexArrayClose( BETA, 0, tc.beta, tol, 'beta' );
});

test( 'zggev: 2x2 diagonal', function t() {
	var tc = fixture.find( function find( t ) { return t.name === 'diagonal_2x2'; });
	var N = 2;
	var LDA = N;
	var A = new Float64Array( 2 * LDA * N );
	var B = new Float64Array( 2 * LDA * N );
	var ALPHA = new Float64Array( 2 * N );
	var BETA = new Float64Array( 2 * N );
	var VL = new Float64Array( 2 * N * N );
	var VR = new Float64Array( 2 * N * N );
	var info;
	var tol = 1e-13;

	A[ 0 ] = 4.0; A[ 1 ] = 0.0;
	A[ 2 ] = 0.0; A[ 3 ] = 0.0;
	A[ 4 ] = 0.0; A[ 5 ] = 0.0;
	A[ 6 ] = 6.0; A[ 7 ] = 0.0;

	B[ 0 ] = 2.0; B[ 1 ] = 0.0;
	B[ 2 ] = 0.0; B[ 3 ] = 0.0;
	B[ 4 ] = 0.0; B[ 5 ] = 0.0;
	B[ 6 ] = 3.0; B[ 7 ] = 0.0;

	info = zggev( 'V', 'V', N,
		A, 2, 2 * LDA, 0,
		B, 2, 2 * LDA, 0,
		ALPHA, 1, 0,
		BETA, 1, 0,
		VL, 2, 2 * N, 0,
		VR, 2, 2 * N, 0
	);
	assert.strictEqual( info, tc.info );
	assertComplexArrayClose( ALPHA, 0, tc.alpha, tol, 'alpha' );
	assertComplexArrayClose( BETA, 0, tc.beta, tol, 'beta' );
	assertComplexArrayClose( VL, 0, tc.VL_col1, tol, 'VL_col1' );
	assertComplexArrayClose( VL, 2 * N, tc.VL_col2, tol, 'VL_col2' );
	assertComplexArrayClose( VR, 0, tc.VR_col1, tol, 'VR_col1' );
	assertComplexArrayClose( VR, 2 * N, tc.VR_col2, tol, 'VR_col2' );
});

test( 'zggev: N=1 no eigenvectors', function t() {
	var tc = fixture.find( function find( t ) { return t.name === 'n_eq_1_noevec'; });
	var N = 1;
	var LDA = N;
	var A = new Float64Array( 2 * LDA * N );
	var B = new Float64Array( 2 * LDA * N );
	var ALPHA = new Float64Array( 2 * N );
	var BETA = new Float64Array( 2 * N );
	var VL = new Float64Array( 2 * N * N );
	var VR = new Float64Array( 2 * N * N );
	var info;
	var tol = 1e-13;

	A[ 0 ] = 5.0; A[ 1 ] = 2.0;
	B[ 0 ] = 1.0; B[ 1 ] = 0.0;

	info = zggev( 'N', 'N', N,
		A, 2, 2 * LDA, 0,
		B, 2, 2 * LDA, 0,
		ALPHA, 1, 0,
		BETA, 1, 0,
		VL, 2, 2 * N, 0,
		VR, 2, 2 * N, 0
	);
	assert.strictEqual( info, tc.info );
	assertComplexArrayClose( ALPHA, 0, tc.alpha, tol, 'alpha' );
	assertComplexArrayClose( BETA, 0, tc.beta, tol, 'beta' );
});

test( 'zggev: 2x2 left eigenvectors only', function t() {
	var tc = fixture.find( function find( t ) { return t.name === 'left_evec_2x2'; });
	var N = 2;
	var LDA = N;
	var A = new Float64Array( 2 * LDA * N );
	var B = new Float64Array( 2 * LDA * N );
	var ALPHA = new Float64Array( 2 * N );
	var BETA = new Float64Array( 2 * N );
	var VL = new Float64Array( 2 * N * N );
	var VR = new Float64Array( 2 * N * N );
	var info;
	var tol = 1e-12;

	// A = [[1+2i, 3-i], [0.5+0.5i, 4+i]]
	A[ 0 ] = 1.0; A[ 1 ] = 2.0;   // A(1,1)
	A[ 2 ] = 0.5; A[ 3 ] = 0.5;   // A(2,1)
	A[ 4 ] = 3.0; A[ 5 ] = -1.0;  // A(1,2)
	A[ 6 ] = 4.0; A[ 7 ] = 1.0;   // A(2,2)

	// B = [[2, 1+i], [0, 3-0.5i]]
	B[ 0 ] = 2.0; B[ 1 ] = 0.0;
	B[ 2 ] = 0.0; B[ 3 ] = 0.0;
	B[ 4 ] = 1.0; B[ 5 ] = 1.0;
	B[ 6 ] = 3.0; B[ 7 ] = -0.5;

	info = zggev( 'V', 'N', N,
		A, 2, 2 * LDA, 0,
		B, 2, 2 * LDA, 0,
		ALPHA, 1, 0,
		BETA, 1, 0,
		VL, 2, 2 * N, 0,
		VR, 2, 2 * N, 0
	);
	assert.strictEqual( info, tc.info );
	assertComplexArrayClose( ALPHA, 0, tc.alpha, tol, 'alpha' );
	assertComplexArrayClose( BETA, 0, tc.beta, tol, 'beta' );
	assertComplexArrayClose( VL, 0, tc.VL_col1, tol, 'VL_col1' );
	assertComplexArrayClose( VL, 2 * N, tc.VL_col2, tol, 'VL_col2' );
});
