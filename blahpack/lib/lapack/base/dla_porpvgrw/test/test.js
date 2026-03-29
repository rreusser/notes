

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dla_porpvgrw = require( './../lib/base.js' );
var ndarray = require( './../lib/ndarray.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dla_porpvgrw.jsonl' ), 'utf8' ).trim().split( '\n' );
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
* Creates a column-major 2D matrix (NxN) as a Float64Array from a flat column-major list.
*
* @private
* @param {Array} vals - flat column-major values
* @returns {Float64Array} matrix
*/
function mat( vals ) {
	return new Float64Array( vals );
}


// TESTS //

test( 'dla_porpvgrw is a function', function t() {
	assert.equal( typeof dla_porpvgrw, 'function' );
});

test( 'ndarray is a function', function t() {
	assert.equal( typeof ndarray, 'function' );
});

test( 'ndarray throws if uplo is invalid', function t() {
	assert.throws( function invalid() {
		ndarray( 'foo', 1, new Float64Array( 1 ), 1, 1, 0, new Float64Array( 1 ), 1, 1, 0, new Float64Array( 2 ), 1, 0 );
	}, /TypeError/ );
});

test( 'dla_porpvgrw: upper_3x3', function t() {
	var tc = findCase( 'upper_3x3' );
	var ncols = 3;
	var lda = 4;

	// Column-major, LDA=4, upper triangle of 3x3
	// A(1,1)=4, A(1,2)=2, A(1,3)=-2, A(2,2)=5, A(2,3)=1, A(3,3)=14
	var A = mat([
		4.0, 0.0, 0.0, 0.0,  // col 1
		2.0, 5.0, 0.0, 0.0,  // col 2
		-2.0, 1.0, 14.0, 0.0 // col 3
	]);
	// AF = Cholesky factor (upper), from dpotrf:
	// AF(1,1)=2, AF(1,2)=1, AF(1,3)=-1, AF(2,2)=2, AF(2,3)=1, AF(3,3)=sqrt(12)
	var AF = mat([
		2.0, 0.0, 0.0, 0.0,
		1.0, 2.0, 0.0, 0.0,
		-1.0, 1.0, 3.46410161513775439, 0.0
	]);
	var WORK = new Float64Array( 2 * ncols );
	var result = dla_porpvgrw( 'upper', ncols, A, 1, lda, 0, AF, 1, lda, 0, WORK, 1, 0 );

	assertClose( result, tc.result, 1e-14, 'result' );
	assertArrayClose( Array.from( WORK ), tc.work, 1e-14, 'work' );
});

test( 'dla_porpvgrw: lower_3x3', function t() {
	var tc = findCase( 'lower_3x3' );
	var ncols = 3;
	var lda = 4;

	// Lower triangle of same 3x3 matrix
	var A = mat([
		4.0, 2.0, -2.0, 0.0,
		0.0, 5.0, 1.0, 0.0,
		0.0, 0.0, 14.0, 0.0
	]);
	// AF = Cholesky factor (lower), from dpotrf
	var AF = mat([
		2.0, 1.0, -1.0, 0.0,
		0.0, 2.0, 1.0, 0.0,
		0.0, 0.0, 3.46410161513775439, 0.0
	]);
	var WORK = new Float64Array( 2 * ncols );
	var result = dla_porpvgrw( 'lower', ncols, A, 1, lda, 0, AF, 1, lda, 0, WORK, 1, 0 );

	assertClose( result, tc.result, 1e-14, 'result' );
	assertArrayClose( Array.from( WORK ), tc.work, 1e-14, 'work' );
});

test( 'dla_porpvgrw: ncols_0', function t() {
	var tc = findCase( 'ncols_0' );
	var A = new Float64Array( 1 );
	var AF = new Float64Array( 1 );
	var WORK = new Float64Array( 0 );
	var result = dla_porpvgrw( 'upper', 0, A, 1, 1, 0, AF, 1, 1, 0, WORK, 1, 0 );

	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dla_porpvgrw: ncols_1_upper', function t() {
	var tc = findCase( 'ncols_1_upper' );
	var A = mat([ 9.0 ]);
	var AF = mat([ 3.0 ]);
	var WORK = new Float64Array( 2 );
	var result = dla_porpvgrw( 'upper', 1, A, 1, 1, 0, AF, 1, 1, 0, WORK, 1, 0 );

	assertClose( result, tc.result, 1e-14, 'result' );
	assertArrayClose( Array.from( WORK ), tc.work, 1e-14, 'work' );
});

test( 'dla_porpvgrw: ncols_1_lower', function t() {
	var tc = findCase( 'ncols_1_lower' );
	var A = mat([ 9.0 ]);
	var AF = mat([ 3.0 ]);
	var WORK = new Float64Array( 2 );
	var result = dla_porpvgrw( 'lower', 1, A, 1, 1, 0, AF, 1, 1, 0, WORK, 1, 0 );

	assertClose( result, tc.result, 1e-14, 'result' );
	assertArrayClose( Array.from( WORK ), tc.work, 1e-14, 'work' );
});

test( 'dla_porpvgrw: upper_4x4', function t() {
	var tc = findCase( 'upper_4x4' );
	var ncols = 4;
	var lda = 4;

	// Upper triangle of 4x4 diagonally dominant matrix
	var A = mat([
		10.0, 0.0, 0.0, 0.0,
		1.0, 10.0, 0.0, 0.0,
		2.0, 1.0, 10.0, 0.0,
		0.5, 1.5, 2.0, 10.0
	]);
	// AF from dpotrf('U', ...)
	// We need the exact Cholesky factor. Let me compute it from the fixture:
	// work[0..3] = max |AF col| = [3.16228..., 3.14643..., 3.08794..., 3.07071...]
	// work[4..7] = max |A col| = [10, 10, 10, 10]
	// The diagonal of AF for a diag-dominant matrix is close to sqrt(diag).
	// For test matching, we compute AF using the known Cholesky:
	// sqrt(10) ~ 3.16228
	var s10 = Math.sqrt( 10.0 );
	var AF = new Float64Array( 16 );
	// Column 1
	AF[ 0 ] = s10;
	// Column 2: AF(1,2) = A(1,2)/AF(1,1) = 1/sqrt(10)
	AF[ 4 ] = 1.0 / s10;
	AF[ 5 ] = Math.sqrt( 10.0 - ( 1.0 / s10 ) * ( 1.0 / s10 ) );
	// Column 3
	AF[ 8 ] = 2.0 / s10;
	AF[ 9 ] = ( 1.0 - ( 1.0 / s10 ) * ( 2.0 / s10 ) ) / AF[ 5 ];
	AF[ 10 ] = Math.sqrt( 10.0 - ( 2.0 / s10 ) * ( 2.0 / s10 ) - AF[ 9 ] * AF[ 9 ] );
	// Column 4
	AF[ 12 ] = 0.5 / s10;
	AF[ 13 ] = ( 1.5 - AF[ 4 ] * AF[ 12 ] ) / AF[ 5 ];
	AF[ 14 ] = ( 2.0 - AF[ 8 ] * AF[ 12 ] - AF[ 9 ] * AF[ 13 ] ) / AF[ 10 ];
	AF[ 15 ] = Math.sqrt( 10.0 - AF[ 12 ] * AF[ 12 ] - AF[ 13 ] * AF[ 13 ] - AF[ 14 ] * AF[ 14 ] );

	var WORK = new Float64Array( 2 * ncols );
	var result = dla_porpvgrw( 'upper', ncols, A, 1, lda, 0, AF, 1, lda, 0, WORK, 1, 0 );

	assertClose( result, tc.result, 1e-14, 'result' );
	assertArrayClose( Array.from( WORK ), tc.work, 1e-14, 'work' );
});

test( 'dla_porpvgrw: lower_4x4', function t() {
	var tc = findCase( 'lower_4x4' );
	var ncols = 4;
	var lda = 4;

	// Lower triangle of same 4x4 matrix
	var A = mat([
		10.0, 1.0, 2.0, 0.5,
		0.0, 10.0, 1.0, 1.5,
		0.0, 0.0, 10.0, 2.0,
		0.0, 0.0, 0.0, 10.0
	]);
	// AF from dpotrf('L', ...) — same numeric factor, transposed storage
	var s10 = Math.sqrt( 10.0 );
	var AF = new Float64Array( 16 );
	// Column 1 (lower tri): L(1,1), L(2,1), L(3,1), L(4,1)
	AF[ 0 ] = s10;
	AF[ 1 ] = 1.0 / s10;
	AF[ 2 ] = 2.0 / s10;
	AF[ 3 ] = 0.5 / s10;
	// Column 2: L(2,2), L(3,2), L(4,2)
	var l22 = Math.sqrt( 10.0 - AF[ 1 ] * AF[ 1 ] );
	AF[ 5 ] = l22;
	AF[ 6 ] = ( 1.0 - AF[ 1 ] * AF[ 2 ] ) / l22;
	AF[ 7 ] = ( 1.5 - AF[ 1 ] * AF[ 3 ] ) / l22;
	// Column 3: L(3,3), L(4,3)
	var l33 = Math.sqrt( 10.0 - AF[ 2 ] * AF[ 2 ] - AF[ 6 ] * AF[ 6 ] );
	AF[ 10 ] = l33;
	AF[ 11 ] = ( 2.0 - AF[ 2 ] * AF[ 3 ] - AF[ 6 ] * AF[ 7 ] ) / l33;
	// Column 4: L(4,4)
	AF[ 15 ] = Math.sqrt( 10.0 - AF[ 3 ] * AF[ 3 ] - AF[ 7 ] * AF[ 7 ] - AF[ 11 ] * AF[ 11 ] );

	var WORK = new Float64Array( 2 * ncols );
	var result = dla_porpvgrw( 'lower', ncols, A, 1, lda, 0, AF, 1, lda, 0, WORK, 1, 0 );

	assertClose( result, tc.result, 1e-14, 'result' );
	assertArrayClose( Array.from( WORK ), tc.work, 1e-14, 'work' );
});

test( 'dla_porpvgrw: upper_zero_col', function t() {
	var tc = findCase( 'upper_zero_col' );
	var ncols = 3;
	var lda = 4;

	// Upper triangle: A(1,1)=4, A(1,2)=2, A(1,3)=1, A(2,2)=5, A(2,3)=3, A(3,3)=7
	var A = mat([
		4.0, 0.0, 0.0, 0.0,
		2.0, 5.0, 0.0, 0.0,
		1.0, 3.0, 7.0, 0.0
	]);
	// AF with zero column 2 (upper tri)
	var AF = mat([
		2.0, 0.0, 0.0, 0.0,
		1.0, 0.0, 0.0, 0.0,
		0.5, 0.0, 2.5, 0.0
	]);
	var WORK = new Float64Array( 2 * ncols );
	var result = dla_porpvgrw( 'upper', ncols, A, 1, lda, 0, AF, 1, lda, 0, WORK, 1, 0 );

	assertClose( result, tc.result, 1e-14, 'result' );
	assertArrayClose( Array.from( WORK ), tc.work, 1e-14, 'work' );
});

test( 'dla_porpvgrw: upper_rpvgrw_lt1', function t() {
	var tc = findCase( 'upper_rpvgrw_lt1' );
	var ncols = 3;
	var lda = 4;

	// A: upper triangle with small elements
	var A = mat([
		2.0, 0.0, 0.0, 0.0,
		1.0, 3.0, 0.0, 0.0,
		0.5, 1.0, 4.0, 0.0
	]);
	// AF: upper triangle with larger elements
	var AF = mat([
		4.0, 0.0, 0.0, 0.0,
		2.0, 6.0, 0.0, 0.0,
		1.0, 3.0, 8.0, 0.0
	]);
	var WORK = new Float64Array( 2 * ncols );
	var result = dla_porpvgrw( 'upper', ncols, A, 1, lda, 0, AF, 1, lda, 0, WORK, 1, 0 );

	assertClose( result, tc.result, 1e-14, 'result' );
	assertArrayClose( Array.from( WORK ), tc.work, 1e-14, 'work' );
});

test( 'dla_porpvgrw: lower_rpvgrw_lt1', function t() {
	var tc = findCase( 'lower_rpvgrw_lt1' );
	var ncols = 3;
	var lda = 4;

	// A: lower triangle with small elements
	var A = mat([
		2.0, 1.0, 0.5, 0.0,
		0.0, 3.0, 1.0, 0.0,
		0.0, 0.0, 4.0, 0.0
	]);
	// AF: lower triangle with larger elements
	var AF = mat([
		4.0, 2.0, 1.0, 0.0,
		0.0, 6.0, 3.0, 0.0,
		0.0, 0.0, 8.0, 0.0
	]);
	var WORK = new Float64Array( 2 * ncols );
	var result = dla_porpvgrw( 'lower', ncols, A, 1, lda, 0, AF, 1, lda, 0, WORK, 1, 0 );

	assertClose( result, tc.result, 1e-14, 'result' );
	assertArrayClose( Array.from( WORK ), tc.work, 1e-14, 'work' );
});

test( 'dla_porpvgrw: lower_zero_col', function t() {
	var tc = findCase( 'lower_zero_col' );
	var ncols = 3;
	var lda = 4;

	// Lower triangle: A(1,1)=4, A(2,1)=2, A(3,1)=1, A(2,2)=5, A(3,2)=3, A(3,3)=7
	var A = mat([
		4.0, 2.0, 1.0, 0.0,
		0.0, 5.0, 3.0, 0.0,
		0.0, 0.0, 7.0, 0.0
	]);
	// AF with zero column 2 (lower tri)
	var AF = mat([
		2.0, 1.0, 0.5, 0.0,
		0.0, 0.0, 0.0, 0.0,
		0.0, 0.0, 2.5, 0.0
	]);
	var WORK = new Float64Array( 2 * ncols );
	var result = dla_porpvgrw( 'lower', ncols, A, 1, lda, 0, AF, 1, lda, 0, WORK, 1, 0 );

	assertClose( result, tc.result, 1e-14, 'result' );
	assertArrayClose( Array.from( WORK ), tc.work, 1e-14, 'work' );
});
