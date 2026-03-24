'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Float64Array = require( '@stdlib/array/float64' );
var dlantr = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlantr.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}


// FIXTURES DATA //

// Upper triangular 4x4 matrix (column-major, LDA=4):
//   [ 1  -4   7  -2 ]
//   [ 0   5  -8   6 ]
//   [ 0   0   9  -3 ]
//   [ 0   0   0   4 ]
var upperA = new Float64Array([
	1, 0, 0, 0,   // col 0
	-4, 5, 0, 0,  // col 1
	7, -8, 9, 0,  // col 2
	-2, 6, -3, 4  // col 3
]);

// Lower triangular 4x4 matrix (column-major, LDA=4):
//   [ 2   0   0   0 ]
//   [-3   6   0   0 ]
//   [ 1  -5   8   0 ]
//   [-4   7  -2   3 ]
var lowerA = new Float64Array([
	2, -3, 1, -4,  // col 0
	0, 6, -5, 7,   // col 1
	0, 0, 8, -2,   // col 2
	0, 0, 0, 3     // col 3
]);

// Rectangular upper M>N: 3x2 (LDA=3)
//   [ 1  -4 ]
//   [ 0   5 ]
//   [ 0   0 ]
var rectUpperMN = new Float64Array([
	1, 0, 0,    // col 0
	-4, 5, 0    // col 1
]);

// Rectangular upper M<N: 2x4 (LDA=2)
//   [ 1  -4   7  -2 ]
//   [ 0   5  -8   6 ]
var rectUpperNM = new Float64Array([
	1, 0,       // col 0
	-4, 5,      // col 1
	7, -8,      // col 2
	-2, 6       // col 3
]);

// Rectangular lower M>N: 4x2 (LDA=4)
//   [ 2   0 ]
//   [-3   6 ]
//   [ 1  -5 ]
//   [-4   7 ]
var rectLowerMN = new Float64Array([
	2, -3, 1, -4,  // col 0
	0, 6, -5, 7    // col 1
]);

// 1x1 matrix
var oneByOne = new Float64Array([ 5.0 ]);

var work = new Float64Array( 10 );


// TESTS //

// --- Upper triangular, non-unit diagonal ---

test( 'dlantr: upper_nonunit_max', function t() {
	var tc = findCase( 'upper_nonunit_max' );
	var result = dlantr( 'max', 'upper', 'non-unit', 4, 4, upperA, 1, 4, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantr: upper_nonunit_one', function t() {
	var tc = findCase( 'upper_nonunit_one' );
	var result = dlantr( 'one-norm', 'upper', 'non-unit', 4, 4, upperA, 1, 4, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantr: upper_nonunit_inf', function t() {
	var tc = findCase( 'upper_nonunit_inf' );
	var result = dlantr( 'inf-norm', 'upper', 'non-unit', 4, 4, upperA, 1, 4, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantr: upper_nonunit_frob', function t() {
	var tc = findCase( 'upper_nonunit_frob' );
	var result = dlantr( 'frobenius', 'upper', 'non-unit', 4, 4, upperA, 1, 4, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

// --- Upper triangular, unit diagonal ---

test( 'dlantr: upper_unit_max', function t() {
	var tc = findCase( 'upper_unit_max' );
	var result = dlantr( 'max', 'upper', 'unit', 4, 4, upperA, 1, 4, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantr: upper_unit_one', function t() {
	var tc = findCase( 'upper_unit_one' );
	var result = dlantr( 'one-norm', 'upper', 'unit', 4, 4, upperA, 1, 4, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantr: upper_unit_inf', function t() {
	var tc = findCase( 'upper_unit_inf' );
	var result = dlantr( 'inf-norm', 'upper', 'unit', 4, 4, upperA, 1, 4, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantr: upper_unit_frob', function t() {
	var tc = findCase( 'upper_unit_frob' );
	var result = dlantr( 'frobenius', 'upper', 'unit', 4, 4, upperA, 1, 4, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

// --- Lower triangular, non-unit diagonal ---

test( 'dlantr: lower_nonunit_max', function t() {
	var tc = findCase( 'lower_nonunit_max' );
	var result = dlantr( 'max', 'lower', 'non-unit', 4, 4, lowerA, 1, 4, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantr: lower_nonunit_one', function t() {
	var tc = findCase( 'lower_nonunit_one' );
	var result = dlantr( 'one-norm', 'lower', 'non-unit', 4, 4, lowerA, 1, 4, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantr: lower_nonunit_inf', function t() {
	var tc = findCase( 'lower_nonunit_inf' );
	var result = dlantr( 'inf-norm', 'lower', 'non-unit', 4, 4, lowerA, 1, 4, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantr: lower_nonunit_frob', function t() {
	var tc = findCase( 'lower_nonunit_frob' );
	var result = dlantr( 'frobenius', 'lower', 'non-unit', 4, 4, lowerA, 1, 4, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

// --- Lower triangular, unit diagonal ---

test( 'dlantr: lower_unit_max', function t() {
	var tc = findCase( 'lower_unit_max' );
	var result = dlantr( 'max', 'lower', 'unit', 4, 4, lowerA, 1, 4, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantr: lower_unit_one', function t() {
	var tc = findCase( 'lower_unit_one' );
	var result = dlantr( 'one-norm', 'lower', 'unit', 4, 4, lowerA, 1, 4, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantr: lower_unit_inf', function t() {
	var tc = findCase( 'lower_unit_inf' );
	var result = dlantr( 'inf-norm', 'lower', 'unit', 4, 4, lowerA, 1, 4, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantr: lower_unit_frob', function t() {
	var tc = findCase( 'lower_unit_frob' );
	var result = dlantr( 'frobenius', 'lower', 'unit', 4, 4, lowerA, 1, 4, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

// --- Edge cases ---

test( 'dlantr: edge_m0', function t() {
	var result = dlantr( 'max', 'upper', 'non-unit', 0, 4, upperA, 1, 4, 0, work, 1, 0 );
	assert.equal( result, 0.0 );
});

test( 'dlantr: edge_n0', function t() {
	var result = dlantr( 'max', 'upper', 'non-unit', 4, 0, upperA, 1, 4, 0, work, 1, 0 );
	assert.equal( result, 0.0 );
});

// --- Rectangular upper, M > N (3x2) ---

test( 'dlantr: rect_upper_mn_max', function t() {
	var tc = findCase( 'rect_upper_mn_max' );
	var result = dlantr( 'max', 'upper', 'non-unit', 3, 2, rectUpperMN, 1, 3, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantr: rect_upper_mn_one', function t() {
	var tc = findCase( 'rect_upper_mn_one' );
	var result = dlantr( 'one-norm', 'upper', 'non-unit', 3, 2, rectUpperMN, 1, 3, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantr: rect_upper_mn_inf', function t() {
	var tc = findCase( 'rect_upper_mn_inf' );
	var result = dlantr( 'inf-norm', 'upper', 'non-unit', 3, 2, rectUpperMN, 1, 3, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantr: rect_upper_mn_frob', function t() {
	var tc = findCase( 'rect_upper_mn_frob' );
	var result = dlantr( 'frobenius', 'upper', 'non-unit', 3, 2, rectUpperMN, 1, 3, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

// --- Rectangular upper, M < N (2x4) ---

test( 'dlantr: rect_upper_nm_max', function t() {
	var tc = findCase( 'rect_upper_nm_max' );
	var result = dlantr( 'max', 'upper', 'non-unit', 2, 4, rectUpperNM, 1, 2, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantr: rect_upper_nm_one', function t() {
	var tc = findCase( 'rect_upper_nm_one' );
	var result = dlantr( 'one-norm', 'upper', 'non-unit', 2, 4, rectUpperNM, 1, 2, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantr: rect_upper_nm_inf', function t() {
	var tc = findCase( 'rect_upper_nm_inf' );
	var result = dlantr( 'inf-norm', 'upper', 'non-unit', 2, 4, rectUpperNM, 1, 2, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantr: rect_upper_nm_frob', function t() {
	var tc = findCase( 'rect_upper_nm_frob' );
	var result = dlantr( 'frobenius', 'upper', 'non-unit', 2, 4, rectUpperNM, 1, 2, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

// --- Rectangular lower, M > N (4x2) ---

test( 'dlantr: rect_lower_mn_max', function t() {
	var tc = findCase( 'rect_lower_mn_max' );
	var result = dlantr( 'max', 'lower', 'non-unit', 4, 2, rectLowerMN, 1, 4, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantr: rect_lower_mn_one', function t() {
	var tc = findCase( 'rect_lower_mn_one' );
	var result = dlantr( 'one-norm', 'lower', 'non-unit', 4, 2, rectLowerMN, 1, 4, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantr: rect_lower_mn_inf', function t() {
	var tc = findCase( 'rect_lower_mn_inf' );
	var result = dlantr( 'inf-norm', 'lower', 'non-unit', 4, 2, rectLowerMN, 1, 4, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantr: rect_lower_mn_frob', function t() {
	var tc = findCase( 'rect_lower_mn_frob' );
	var result = dlantr( 'frobenius', 'lower', 'non-unit', 4, 2, rectLowerMN, 1, 4, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

// --- 1x1 matrix ---

test( 'dlantr: edge_1x1_max', function t() {
	var tc = findCase( 'edge_1x1_max' );
	var result = dlantr( 'max', 'upper', 'non-unit', 1, 1, oneByOne, 1, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantr: edge_1x1_one', function t() {
	var tc = findCase( 'edge_1x1_one' );
	var result = dlantr( 'one-norm', 'upper', 'non-unit', 1, 1, oneByOne, 1, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantr: edge_1x1_inf', function t() {
	var tc = findCase( 'edge_1x1_inf' );
	var result = dlantr( 'inf-norm', 'upper', 'non-unit', 1, 1, oneByOne, 1, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantr: edge_1x1_frob', function t() {
	var tc = findCase( 'edge_1x1_frob' );
	var result = dlantr( 'frobenius', 'upper', 'non-unit', 1, 1, oneByOne, 1, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantr: edge_1x1_unit_max', function t() {
	var tc = findCase( 'edge_1x1_unit_max' );
	var result = dlantr( 'max', 'upper', 'unit', 1, 1, oneByOne, 1, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});
