

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var FLOAT64_SMALLEST_NORMAL = require( '@stdlib/constants/float64/smallest-normal' );
var dlag2 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlag2.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual + ' (relErr=' + relErr + ')' );
}

/**
* Helper to call dlag2 with column-major 2x2 matrices.
*
* @private
* @param {Array} aVals - [a11, a21, a12, a22] column-major
* @param {Array} bVals - [b11, b21, b12, b22] column-major
* @param {number} safmin - safe minimum
* @returns {Object} result with scale1, scale2, wr1, wr2, wi
*/
function callDlag2( aVals, bVals, safmin ) {
	var A = new Float64Array( aVals );
	var B = new Float64Array( bVals );
	return dlag2( A, 1, 2, 0, B, 1, 2, 0, safmin );
}

var SAFMIN = FLOAT64_SMALLEST_NORMAL;


// TESTS //

test( 'dlag2 is a function', function t() {
	assert.equal( typeof dlag2, 'function' );
});

test( 'dlag2: real_eigenvalues_identity_B', function t() {
	var tc = findCase( 'real_eigenvalues_identity_B' );

	// A = [4 1; 2 3], B = I (column-major)
	var result = callDlag2(
		[ 4.0, 2.0, 1.0, 3.0 ],
		[ 1.0, 0.0, 0.0, 1.0 ],
		SAFMIN
	);
	assertClose( result.scale1, tc.scale1, 1e-14, 'scale1' );
	assertClose( result.scale2, tc.scale2, 1e-14, 'scale2' );
	assertClose( result.wr1, tc.wr1, 1e-14, 'wr1' );
	assertClose( result.wr2, tc.wr2, 1e-14, 'wr2' );
	assert.equal( result.wi, tc.wi, 'wi should be zero for real eigenvalues' );
});

test( 'dlag2: complex_eigenvalues', function t() {
	var tc = findCase( 'complex_eigenvalues' );

	// A = [1 -5; 2 1], B = I (column-major)
	var result = callDlag2(
		[ 1.0, 2.0, -5.0, 1.0 ],
		[ 1.0, 0.0, 0.0, 1.0 ],
		SAFMIN
	);
	assertClose( result.scale1, tc.scale1, 1e-14, 'scale1' );
	assertClose( result.scale2, tc.scale2, 1e-14, 'scale2' );
	assertClose( result.wr1, tc.wr1, 1e-14, 'wr1' );
	assertClose( result.wr2, tc.wr2, 1e-14, 'wr2' );
	assertClose( result.wi, tc.wi, 1e-14, 'wi' );
	assert.ok( result.wi > 0.0, 'wi should be positive for complex eigenvalues' );
});

test( 'dlag2: diagonal', function t() {
	var tc = findCase( 'diagonal' );

	// A = [5 0; 0 3], B = [2 0; 0 1] (column-major)
	var result = callDlag2(
		[ 5.0, 0.0, 0.0, 3.0 ],
		[ 2.0, 0.0, 0.0, 1.0 ],
		SAFMIN
	);
	assertClose( result.scale1, tc.scale1, 1e-14, 'scale1' );
	assertClose( result.scale2, tc.scale2, 1e-14, 'scale2' );
	assertClose( result.wr1, tc.wr1, 1e-14, 'wr1' );
	assertClose( result.wr2, tc.wr2, 1e-14, 'wr2' );
	assert.equal( result.wi, tc.wi, 'wi' );
});

test( 'dlag2: upper_tri_B', function t() {
	var tc = findCase( 'upper_tri_B' );

	// A = [3 1; 1 2], B = [2 1; 0 3] (column-major)
	var result = callDlag2(
		[ 3.0, 1.0, 1.0, 2.0 ],
		[ 2.0, 0.0, 1.0, 3.0 ],
		SAFMIN
	);
	assertClose( result.scale1, tc.scale1, 1e-14, 'scale1' );
	assertClose( result.scale2, tc.scale2, 1e-14, 'scale2' );
	assertClose( result.wr1, tc.wr1, 1e-14, 'wr1' );
	assertClose( result.wr2, tc.wr2, 1e-14, 'wr2' );
	assert.equal( result.wi, tc.wi, 'wi' );
});

test( 'dlag2: small_B_diagonal', function t() {
	var tc = findCase( 'small_B_diagonal' );

	// A = [1 1; 1 1], B = [1e-200 0; 0 1] (column-major)
	var result = callDlag2(
		[ 1.0, 1.0, 1.0, 1.0 ],
		[ 1.0e-200, 0.0, 0.0, 1.0 ],
		SAFMIN
	);
	assertClose( result.scale1, tc.scale1, 1e-14, 'scale1' );
	assertClose( result.scale2, tc.scale2, 1e-14, 'scale2' );
	assertClose( result.wr1, tc.wr1, 1e-14, 'wr1' );
	assertClose( result.wr2, tc.wr2, 1e-10, 'wr2' );
	assert.equal( result.wi, tc.wi, 'wi' );
});

test( 'dlag2: s1_leq_s2', function t() {
	var tc = findCase( 's1_leq_s2' );

	// A = [0.1 3; 2 5], B = I (column-major)
	var result = callDlag2(
		[ 0.1, 2.0, 3.0, 5.0 ],
		[ 1.0, 0.0, 0.0, 1.0 ],
		SAFMIN
	);
	assertClose( result.scale1, tc.scale1, 1e-14, 'scale1' );
	assertClose( result.scale2, tc.scale2, 1e-14, 'scale2' );
	assertClose( result.wr1, tc.wr1, 1e-14, 'wr1' );
	assertClose( result.wr2, tc.wr2, 1e-14, 'wr2' );
	assert.equal( result.wi, tc.wi, 'wi' );
});

test( 'dlag2: s1_gt_s2', function t() {
	var tc = findCase( 's1_gt_s2' );

	// A = [10 1; 1 0.5], B = I (column-major)
	var result = callDlag2(
		[ 10.0, 1.0, 1.0, 0.5 ],
		[ 1.0, 0.0, 0.0, 1.0 ],
		SAFMIN
	);
	assertClose( result.scale1, tc.scale1, 1e-14, 'scale1' );
	assertClose( result.scale2, tc.scale2, 1e-14, 'scale2' );
	assertClose( result.wr1, tc.wr1, 1e-14, 'wr1' );
	assertClose( result.wr2, tc.wr2, 1e-14, 'wr2' );
	assert.equal( result.wi, tc.wi, 'wi' );
});

test( 'dlag2: large_A', function t() {
	var tc = findCase( 'large_A' );

	// A = [1e100 3e100; 2e100 4e100], B = I (column-major)
	var result = callDlag2(
		[ 1.0e100, 2.0e100, 3.0e100, 4.0e100 ],
		[ 1.0, 0.0, 0.0, 1.0 ],
		SAFMIN
	);
	assertClose( result.scale1, tc.scale1, 1e-14, 'scale1' );
	assertClose( result.scale2, tc.scale2, 1e-14, 'scale2' );
	assertClose( result.wr1, tc.wr1, 1e-14, 'wr1' );
	assertClose( result.wr2, tc.wr2, 1e-14, 'wr2' );
	assert.equal( result.wi, tc.wi, 'wi' );
});

test( 'dlag2: negative_eigenvalues', function t() {
	var tc = findCase( 'negative_eigenvalues' );

	// A = [-2 1; 1 -3], B = I (column-major)
	var result = callDlag2(
		[ -2.0, 1.0, 1.0, -3.0 ],
		[ 1.0, 0.0, 0.0, 1.0 ],
		SAFMIN
	);
	assertClose( result.scale1, tc.scale1, 1e-14, 'scale1' );
	assertClose( result.scale2, tc.scale2, 1e-14, 'scale2' );
	assertClose( result.wr1, tc.wr1, 1e-14, 'wr1' );
	assertClose( result.wr2, tc.wr2, 1e-14, 'wr2' );
	assert.equal( result.wi, tc.wi, 'wi' );
});

test( 'dlag2: pp_gt_abi22', function t() {
	var tc = findCase( 'pp_gt_abi22' );

	// A = [6 1; 0.1 2], B = I (column-major)
	var result = callDlag2(
		[ 6.0, 0.1, 1.0, 2.0 ],
		[ 1.0, 0.0, 0.0, 1.0 ],
		SAFMIN
	);
	assertClose( result.scale1, tc.scale1, 1e-14, 'scale1' );
	assertClose( result.scale2, tc.scale2, 1e-14, 'scale2' );
	assertClose( result.wr1, tc.wr1, 1e-14, 'wr1' );
	assertClose( result.wr2, tc.wr2, 1e-14, 'wr2' );
	assert.equal( result.wi, tc.wi, 'wi' );
});

test( 'dlag2: small_A', function t() {
	var tc = findCase( 'small_A' );

	// A = [1e-200 3e-200; 2e-200 4e-200], B = I (column-major)
	var result = callDlag2(
		[ 1.0e-200, 2.0e-200, 3.0e-200, 4.0e-200 ],
		[ 1.0, 0.0, 0.0, 1.0 ],
		SAFMIN
	);
	assertClose( result.scale1, tc.scale1, 1e-14, 'scale1' );
	assertClose( result.scale2, tc.scale2, 1e-14, 'scale2' );
	assertClose( result.wr1, tc.wr1, 1e-14, 'wr1' );
	assertClose( result.wr2, tc.wr2, 1e-14, 'wr2' );
	assert.equal( result.wi, tc.wi, 'wi' );
});

test( 'dlag2: both_B_diag_small', function t() {
	var tc = findCase( 'both_B_diag_small' );

	// A = [1 0.5; 0.5 1], B = [1e-200 0; 0 1e-200] (column-major)
	var result = callDlag2(
		[ 1.0, 0.5, 0.5, 1.0 ],
		[ 1.0e-200, 0.0, 0.0, 1.0e-200 ],
		SAFMIN
	);
	assertClose( result.scale1, tc.scale1, 1e-14, 'scale1' );
	assertClose( result.scale2, tc.scale2, 1e-14, 'scale2' );
	assertClose( result.wr1, tc.wr1, 1e-14, 'wr1' );
	assertClose( result.wr2, tc.wr2, 1e-14, 'wr2' );
	assert.equal( result.wi, tc.wi, 'wi' );
});

test( 'dlag2: B_offdiag', function t() {
	var tc = findCase( 'B_offdiag' );

	// A = [2 0; 0 3], B = [1 0.5; 0 1] (column-major)
	var result = callDlag2(
		[ 2.0, 0.0, 0.0, 3.0 ],
		[ 1.0, 0.0, 0.5, 1.0 ],
		SAFMIN
	);
	assertClose( result.scale1, tc.scale1, 1e-14, 'scale1' );
	assertClose( result.scale2, tc.scale2, 1e-14, 'scale2' );
	assertClose( result.wr1, tc.wr1, 1e-14, 'wr1' );
	assertClose( result.wr2, tc.wr2, 1e-14, 'wr2' );
	assert.equal( result.wi, tc.wi, 'wi' );
});

test( 'dlag2: large_eigenvalue_scaling', function t() {
	var tc = findCase( 'large_eigenvalue_scaling' );

	// A = [1e150 0; 0 2e150], B = [1e-10 0; 0 1e-10] (column-major)
	var result = callDlag2(
		[ 1.0e150, 0.0, 0.0, 2.0e150 ],
		[ 1.0e-10, 0.0, 0.0, 1.0e-10 ],
		SAFMIN
	);
	assertClose( result.scale1, tc.scale1, 1e-14, 'scale1' );
	assertClose( result.scale2, tc.scale2, 1e-14, 'scale2' );
	assertClose( result.wr1, tc.wr1, 1e-14, 'wr1' );
	assertClose( result.wr2, tc.wr2, 1e-14, 'wr2' );
	assert.equal( result.wi, tc.wi, 'wi' );
});

test( 'dlag2: complex_nontrivial_B', function t() {
	var tc = findCase( 'complex_nontrivial_B' );

	// A = [1 -2; 3 1], B = [2 1; 0 2] (column-major)
	var result = callDlag2(
		[ 1.0, 3.0, -2.0, 1.0 ],
		[ 2.0, 0.0, 1.0, 2.0 ],
		SAFMIN
	);
	assertClose( result.scale1, tc.scale1, 1e-14, 'scale1' );
	assertClose( result.scale2, tc.scale2, 1e-14, 'scale2' );
	assertClose( result.wr1, tc.wr1, 1e-14, 'wr1' );
	assertClose( result.wr2, tc.wr2, 1e-14, 'wr2' );
	assertClose( result.wi, tc.wi, 1e-14, 'wi' );
});

test( 'dlag2: negative_B_diag', function t() {
	var tc = findCase( 'negative_B_diag' );

	// A = [3 1; 1 2], B = [-1 0; 0 1] (column-major)
	var result = callDlag2(
		[ 3.0, 1.0, 1.0, 2.0 ],
		[ -1.0, 0.0, 0.0, 1.0 ],
		SAFMIN
	);
	assertClose( result.scale1, tc.scale1, 1e-14, 'scale1' );
	assertClose( result.scale2, tc.scale2, 1e-14, 'scale2' );
	assertClose( result.wr1, tc.wr1, 1e-14, 'wr1' );
	assertClose( result.wr2, tc.wr2, 1e-14, 'wr2' );
	assert.equal( result.wi, tc.wi, 'wi' );
});

test( 'dlag2: b22_small', function t() {
	var tc = findCase( 'b22_small' );

	// A = [2 1; 1 3], B = [1 0; 0 1e-200] (column-major)
	var result = callDlag2(
		[ 2.0, 1.0, 1.0, 3.0 ],
		[ 1.0, 0.0, 0.0, 1.0e-200 ],
		SAFMIN
	);
	assertClose( result.scale1, tc.scale1, 1e-14, 'scale1' );
	assertClose( result.scale2, tc.scale2, 1e-14, 'scale2' );
	assertClose( result.wr1, tc.wr1, 1e-10, 'wr1' );
	assertClose( result.wr2, tc.wr2, 1e-14, 'wr2' );
	assert.equal( result.wi, tc.wi, 'wi' );
});

test( 'dlag2: large_pp', function t() {
	var tc = findCase( 'large_pp' );

	// A = I, B = [1e-155 0; 0 1e-155] (column-major)
	var result = callDlag2(
		[ 1.0, 0.0, 0.0, 1.0 ],
		[ 1.0e-155, 0.0, 0.0, 1.0e-155 ],
		SAFMIN
	);
	assertClose( result.scale1, tc.scale1, 1e-14, 'scale1' );
	assertClose( result.scale2, tc.scale2, 1e-14, 'scale2' );
	assertClose( result.wr1, tc.wr1, 1e-14, 'wr1' );
	assertClose( result.wr2, tc.wr2, 1e-14, 'wr2' );
	assert.equal( result.wi, tc.wi, 'wi' );
});

test( 'dlag2: tiny_pp_qq', function t() {
	var tc = findCase( 'tiny_pp_qq' );

	// A = [1e-200 1e-200; 1e-200 1e-200], B = [1 1; 0 1] (column-major)
	var result = callDlag2(
		[ 1.0e-200, 1.0e-200, 1.0e-200, 1.0e-200 ],
		[ 1.0, 0.0, 1.0, 1.0 ],
		SAFMIN
	);
	assertClose( result.scale1, tc.scale1, 1e-14, 'scale1' );
	assertClose( result.scale2, tc.scale2, 1e-14, 'scale2' );
	assertClose( result.wr1, tc.wr1, 1e-14, 'wr1' );
	assertClose( result.wr2, tc.wr2, 1e-14, 'wr2' );
	assert.equal( result.wi, tc.wi, 'wi' );
});

test( 'dlag2: identity', function t() {
	var tc = findCase( 'identity' );

	// A = I, B = I (column-major)
	var result = callDlag2(
		[ 1.0, 0.0, 0.0, 1.0 ],
		[ 1.0, 0.0, 0.0, 1.0 ],
		SAFMIN
	);
	assertClose( result.scale1, tc.scale1, 1e-14, 'scale1' );
	assertClose( result.scale2, tc.scale2, 1e-14, 'scale2' );
	assertClose( result.wr1, tc.wr1, 1e-14, 'wr1' );
	assertClose( result.wr2, tc.wr2, 1e-14, 'wr2' );
	assert.equal( result.wi, tc.wi, 'wi' );
});

test( 'dlag2: ascale_gt1_bsize_gt1', function t() {
	var tc = findCase( 'ascale_gt1_bsize_gt1' );

	// A = [0.4 0; 0 0.3], B = [2 0; 0 3] (column-major)
	var result = callDlag2(
		[ 0.4, 0.0, 0.0, 0.3 ],
		[ 2.0, 0.0, 0.0, 3.0 ],
		SAFMIN
	);
	assertClose( result.scale1, tc.scale1, 1e-14, 'scale1' );
	assertClose( result.scale2, tc.scale2, 1e-14, 'scale2' );
	assertClose( result.wr1, tc.wr1, 1e-14, 'wr1' );
	assertClose( result.wr2, tc.wr2, 1e-14, 'wr2' );
	assert.equal( result.wi, tc.wi, 'wi' );
});

test( 'dlag2: wsize_gt1_eigenvalue1', function t() {
	var tc = findCase( 'wsize_gt1_eigenvalue1' );

	// A = [1 0; 0 0.5], B = [1e-100 0; 0 1e-100] (column-major)
	var result = callDlag2(
		[ 1.0, 0.0, 0.0, 0.5 ],
		[ 1.0e-100, 0.0, 0.0, 1.0e-100 ],
		SAFMIN
	);
	assertClose( result.scale1, tc.scale1, 1e-14, 'scale1' );
	assertClose( result.scale2, tc.scale2, 1e-14, 'scale2' );
	assertClose( result.wr1, tc.wr1, 1e-14, 'wr1' );
	assertClose( result.wr2, tc.wr2, 1e-14, 'wr2' );
	assert.equal( result.wi, tc.wi, 'wi' );
});

test( 'dlag2: supports non-unit strides', function t() {
	// Test with strideA1=2, strideA2=4 (padded layout)
	// A = [4 1; 2 3] embedded in larger array
	var A = new Float64Array( [ 4.0, -1.0, 2.0, -1.0, 1.0, -1.0, 3.0, -1.0 ] );

	// strideA1=2, strideA2=4, offsetA=0
	// A[0] = 4, A[2] = 2, A[4] = 1, A[6] = 3
	var B = new Float64Array( [ 1.0, -1.0, 0.0, -1.0, 0.0, -1.0, 1.0, -1.0 ] );

	// Same strides for B
	var result = dlag2( A, 2, 4, 0, B, 2, 4, 0, SAFMIN );

	var tc = findCase( 'real_eigenvalues_identity_B' );
	assertClose( result.scale1, tc.scale1, 1e-14, 'scale1' );
	assertClose( result.scale2, tc.scale2, 1e-14, 'scale2' );
	assertClose( result.wr1, tc.wr1, 1e-14, 'wr1' );
	assertClose( result.wr2, tc.wr2, 1e-14, 'wr2' );
	assert.equal( result.wi, tc.wi, 'wi' );
});

test( 'dlag2: supports offsets', function t() {
	// A = [4 1; 2 3], but stored at offset 2
	var A = new Float64Array( [ -1.0, -1.0, 4.0, 2.0, 1.0, 3.0 ] );

	// strideA1=1, strideA2=2, offsetA=2
	var B = new Float64Array( [ -1.0, -1.0, 1.0, 0.0, 0.0, 1.0 ] );

	// strideB1=1, strideB2=2, offsetB=2
	var result = dlag2( A, 1, 2, 2, B, 1, 2, 2, SAFMIN );

	var tc = findCase( 'real_eigenvalues_identity_B' );
	assertClose( result.scale1, tc.scale1, 1e-14, 'scale1' );
	assertClose( result.scale2, tc.scale2, 1e-14, 'scale2' );
	assertClose( result.wr1, tc.wr1, 1e-14, 'wr1' );
	assertClose( result.wr2, tc.wr2, 1e-14, 'wr2' );
	assert.equal( result.wi, tc.wi, 'wi' );
});
