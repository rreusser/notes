

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var zlantr = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zlantr.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

/**
* Creates the 3x3 upper triangular test matrix (column-major, LDA=3):
*   A = [ (1+2i)  (3+4i)  (5+6i) ]
*       [     0   (7+8i)  (9+1i) ]
*       [     0       0   (2+3i) ]
*/
function makeUpperA() {
	return new Complex128Array( [
		1, 2, 0, 0, 0, 0,    // col 0: (1+2i), 0, 0
		3, 4, 7, 8, 0, 0,    // col 1: (3+4i), (7+8i), 0
		5, 6, 9, 1, 2, 3     // col 2: (5+6i), (9+1i), (2+3i)
	] );
}

/**
* Creates the 3x3 lower triangular test matrix (column-major, LDA=3):
*   A = [ (1+2i)     0       0   ]
*       [ (3+4i)  (7+8i)     0   ]
*       [ (5+6i)  (9+1i)  (2+3i) ]
*/
function makeLowerA() {
	return new Complex128Array( [
		1, 2, 3, 4, 5, 6,    // col 0: (1+2i), (3+4i), (5+6i)
		0, 0, 7, 8, 9, 1,    // col 1: 0, (7+8i), (9+1i)
		0, 0, 0, 0, 2, 3     // col 2: 0, 0, (2+3i)
	] );
}


// TESTS //

// --- Upper triangular, non-unit diagonal ---

test( 'zlantr: max_upper_nonunit', function t() {
	var tc = findCase( 'max_upper_nonunit' );
	var A = makeUpperA();
	var WORK = new Float64Array( 10 );
	var result = zlantr( 'max', 'upper', 'non-unit', 3, 3, A, 1, 3, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantr: max_upper_unit', function t() {
	var tc = findCase( 'max_upper_unit' );
	var A = makeUpperA();
	var WORK = new Float64Array( 10 );
	var result = zlantr( 'max', 'upper', 'unit', 3, 3, A, 1, 3, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantr: one_upper_nonunit', function t() {
	var tc = findCase( 'one_upper_nonunit' );
	var A = makeUpperA();
	var WORK = new Float64Array( 10 );
	var result = zlantr( 'one-norm', 'upper', 'non-unit', 3, 3, A, 1, 3, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantr: one_upper_unit', function t() {
	var tc = findCase( 'one_upper_unit' );
	var A = makeUpperA();
	var WORK = new Float64Array( 10 );
	var result = zlantr( 'one-norm', 'upper', 'unit', 3, 3, A, 1, 3, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantr: inf_upper_nonunit', function t() {
	var tc = findCase( 'inf_upper_nonunit' );
	var A = makeUpperA();
	var WORK = new Float64Array( 10 );
	var result = zlantr( 'inf-norm', 'upper', 'non-unit', 3, 3, A, 1, 3, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantr: inf_upper_unit', function t() {
	var tc = findCase( 'inf_upper_unit' );
	var A = makeUpperA();
	var WORK = new Float64Array( 10 );
	var result = zlantr( 'inf-norm', 'upper', 'unit', 3, 3, A, 1, 3, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantr: frob_upper_nonunit', function t() {
	var tc = findCase( 'frob_upper_nonunit' );
	var A = makeUpperA();
	var WORK = new Float64Array( 10 );
	var result = zlantr( 'frobenius', 'upper', 'non-unit', 3, 3, A, 1, 3, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantr: frob_upper_unit', function t() {
	var tc = findCase( 'frob_upper_unit' );
	var A = makeUpperA();
	var WORK = new Float64Array( 10 );
	var result = zlantr( 'frobenius', 'upper', 'unit', 3, 3, A, 1, 3, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

// --- Lower triangular, non-unit diagonal ---

test( 'zlantr: max_lower_nonunit', function t() {
	var tc = findCase( 'max_lower_nonunit' );
	var A = makeLowerA();
	var WORK = new Float64Array( 10 );
	var result = zlantr( 'max', 'lower', 'non-unit', 3, 3, A, 1, 3, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantr: max_lower_unit', function t() {
	var tc = findCase( 'max_lower_unit' );
	var A = makeLowerA();
	var WORK = new Float64Array( 10 );
	var result = zlantr( 'max', 'lower', 'unit', 3, 3, A, 1, 3, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantr: one_lower_nonunit', function t() {
	var tc = findCase( 'one_lower_nonunit' );
	var A = makeLowerA();
	var WORK = new Float64Array( 10 );
	var result = zlantr( 'one-norm', 'lower', 'non-unit', 3, 3, A, 1, 3, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantr: one_lower_unit', function t() {
	var tc = findCase( 'one_lower_unit' );
	var A = makeLowerA();
	var WORK = new Float64Array( 10 );
	var result = zlantr( 'one-norm', 'lower', 'unit', 3, 3, A, 1, 3, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantr: inf_lower_nonunit', function t() {
	var tc = findCase( 'inf_lower_nonunit' );
	var A = makeLowerA();
	var WORK = new Float64Array( 10 );
	var result = zlantr( 'inf-norm', 'lower', 'non-unit', 3, 3, A, 1, 3, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantr: inf_lower_unit', function t() {
	var tc = findCase( 'inf_lower_unit' );
	var A = makeLowerA();
	var WORK = new Float64Array( 10 );
	var result = zlantr( 'inf-norm', 'lower', 'unit', 3, 3, A, 1, 3, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantr: frob_lower_nonunit', function t() {
	var tc = findCase( 'frob_lower_nonunit' );
	var A = makeLowerA();
	var WORK = new Float64Array( 10 );
	var result = zlantr( 'frobenius', 'lower', 'non-unit', 3, 3, A, 1, 3, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantr: frob_lower_unit', function t() {
	var tc = findCase( 'frob_lower_unit' );
	var A = makeLowerA();
	var WORK = new Float64Array( 10 );
	var result = zlantr( 'frobenius', 'lower', 'unit', 3, 3, A, 1, 3, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

// --- Edge cases ---

test( 'zlantr: m_zero', function t() {
	var A = new Complex128Array( 1 );
	var WORK = new Float64Array( 1 );
	var result = zlantr( 'max', 'upper', 'non-unit', 0, 3, A, 1, 1, 0, WORK, 1, 0 );
	assert.equal( result, 0.0 );
});

test( 'zlantr: n_zero', function t() {
	var A = new Complex128Array( 1 );
	var WORK = new Float64Array( 1 );
	var result = zlantr( 'max', 'upper', 'non-unit', 3, 0, A, 1, 3, 0, WORK, 1, 0 );
	assert.equal( result, 0.0 );
});

test( 'zlantr: frob_1x1', function t() {
	var tc = findCase( 'frob_1x1' );
	var A = new Complex128Array( [ 3, 4 ] );
	var WORK = new Float64Array( 1 );
	var result = zlantr( 'frobenius', 'upper', 'non-unit', 1, 1, A, 1, 1, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantr: max_1x1_unit', function t() {
	var tc = findCase( 'max_1x1_unit' );
	var A = new Complex128Array( [ 3, 4 ] );
	var WORK = new Float64Array( 1 );
	var result = zlantr( 'max', 'upper', 'unit', 1, 1, A, 1, 1, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantr: one_O_upper_nonunit (O norm = one-norm)', function t() {
	var tc = findCase( 'one_O_upper_nonunit' );
	var A = makeUpperA();
	var WORK = new Float64Array( 10 );
	// 'O' norm in Fortran maps to 'one-norm' in JS
	var result = zlantr( 'one-norm', 'upper', 'non-unit', 3, 3, A, 1, 3, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

// --- Trapezoidal cases ---

test( 'zlantr: one_lower_trap (4x2 lower trapezoidal)', function t() {
	var tc = findCase( 'one_lower_trap' );
	// 4x2 lower trapezoidal, column-major, LDA=4
	var A = new Complex128Array( [
		1, 1, 2, 2, 3, 3, 4, 4,   // col 0: (1+i), (2+2i), (3+3i), (4+4i)
		0, 0, 5, 5, 6, 6, 7, 7    // col 1: 0, (5+5i), (6+6i), (7+7i)
	] );
	var WORK = new Float64Array( 10 );
	var result = zlantr( 'one-norm', 'lower', 'non-unit', 4, 2, A, 1, 4, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantr: inf_upper_trap (2x4 upper trapezoidal)', function t() {
	var tc = findCase( 'inf_upper_trap' );
	// 2x4 upper trapezoidal, column-major, LDA=2
	var A = new Complex128Array( [
		1, 1, 0, 0,    // col 0: (1+i), 0
		2, 2, 3, 3,    // col 1: (2+2i), (3+3i)
		4, 4, 5, 5,    // col 2: (4+4i), (5+5i)
		6, 6, 7, 7     // col 3: (6+6i), (7+7i)
	] );
	var WORK = new Float64Array( 10 );
	var result = zlantr( 'inf-norm', 'upper', 'non-unit', 2, 4, A, 1, 2, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

// --- Additional trapezoidal: inf-norm, lower, unit, M > N ---

test( 'zlantr: inf_lower_unit_trap (4x2 lower trapezoidal)', function t() {
	// 4x2 lower trapezoidal, unit diag, column-major, LDA=4
	// A = [ 1      0    ]  (unit diag)
	//     [ (2+2i) 1    ]  (unit diag)
	//     [ (3+3i) (6+6i) ]
	//     [ (4+4i) (7+7i) ]
	var A = new Complex128Array( [
		1, 1, 2, 2, 3, 3, 4, 4,   // col 0
		0, 0, 5, 5, 6, 6, 7, 7    // col 1
	] );
	var WORK = new Float64Array( 10 );
	var result = zlantr( 'inf-norm', 'lower', 'unit', 4, 2, A, 1, 4, 0, WORK, 1, 0 );
	// Row 0: 1.0 (unit diag only, j=0 has no off-diag below)
	// Row 1: 1.0 (unit diag) -- no off-diagonal elements below j=1 diagonal (j+1..M but j=1 is last col)
	// Row 2: |3+3i| + |6+6i| = 3*sqrt(2) + 6*sqrt(2) = 9*sqrt(2) ≈ 12.727
	// Row 3: |4+4i| + |7+7i| = 4*sqrt(2) + 7*sqrt(2) = 11*sqrt(2) ≈ 15.556
	// Wait - rows 2,3 have WORK init to 0 (beyond N), then accumulate
	// Actually for unit diag: rows 0..min(M,N)-1 get WORK=1, rows N..M-1 get WORK=0
	// Then for each col j, rows j+1..M-1 accumulate
	// Row 2: WORK=0 + |3+3i| from col0 + |6+6i| from col1 = 3*sqrt(2) + 6*sqrt(2)
	// Row 3: WORK=0 + |4+4i| from col0 + |7+7i| from col1 = 4*sqrt(2) + 7*sqrt(2)
	// Max is row 3: 11*sqrt(2) ≈ 15.556
	var expected = 11.0 * Math.sqrt( 2.0 );
	assertClose( result, expected, 1e-14, 'result' );
});

// --- Unknown norm returns 0 ---

test( 'zlantr: unknown norm returns 0', function t() {
	var A = makeUpperA();
	var WORK = new Float64Array( 10 );
	var result = zlantr( 'unknown', 'upper', 'non-unit', 3, 3, A, 1, 3, 0, WORK, 1, 0 );
	assert.equal( result, 0.0 );
});
