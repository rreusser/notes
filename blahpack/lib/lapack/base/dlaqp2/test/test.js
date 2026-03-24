

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dlaqp2 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlaqp2.jsonl' ), 'utf8' ).trim().split( '\n' );
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
* Creates a column-major matrix from flat column-major data.
* Fortran fixture outputs column-major flat arrays using print_matrix which
* outputs column by column, m elements per column.
*
* @param {Array} data - flat column-major data
* @param {number} M - number of rows
* @param {number} N - number of columns
* @param {number} LDA - leading dimension (>= M)
* @returns {Float64Array} - column-major array with stride=(1, LDA)
*/
function makeMatrix( data, M, N, LDA ) {
	var A = new Float64Array( LDA * N );
	var col;
	var row;
	var k;
	k = 0;
	for ( col = 0; col < N; col++ ) {
		for ( row = 0; row < M; row++ ) {
			A[ row + ( col * LDA ) ] = data[ k ];
			k += 1;
		}
	}
	return A;
}

/**
* Extracts column-major flat array from a matrix stored with LDA stride.
*
* @param {Float64Array} A - matrix
* @param {number} M - rows
* @param {number} N - cols
* @param {number} LDA - leading dimension
* @returns {Array} flat column-major array (M*N elements)
*/
function extractMatrix( A, M, N, LDA ) {
	var out = [];
	var col;
	var row;
	for ( col = 0; col < N; col++ ) {
		for ( row = 0; row < M; row++ ) {
			out.push( A[ row + ( col * LDA ) ] );
		}
	}
	return out;
}

/**
* Compute column norms of A(startRow:M-1, 0:N-1).
*/
function computeNorms( A, M, N, LDA, startRow ) {
	var VN1 = new Float64Array( N );
	var VN2 = new Float64Array( N );
	var sum;
	var val;
	var j;
	var i;
	for ( j = 0; j < N; j++ ) {
		sum = 0.0;
		for ( i = startRow; i < M; i++ ) {
			val = A[ i + ( j * LDA ) ];
			sum += val * val;
		}
		VN1[ j ] = Math.sqrt( sum );
		VN2[ j ] = VN1[ j ];
	}
	return { VN1: VN1, VN2: VN2 };
}


// TESTS //

test( 'dlaqp2: basic_4x3', function t() {
	var norms;
	var JPVT;
	var WORK;
	var TAU;
	var tc;
	var A;

	tc = findCase( 'basic_4x3' );

	// 4x3 matrix, column-major with LDA=4
	A = makeMatrix( [
		1, 2, 0, 1,
		0, 1, 3, 2,
		3, 0, 1, 2
	], 4, 3, 4 );

	JPVT = new Int32Array( [ 0, 1, 2 ] ); // 0-based
	TAU = new Float64Array( 3 );
	WORK = new Float64Array( 3 );
	norms = computeNorms( A, 4, 3, 4, 0 );

	dlaqp2( 4, 3, 0, A, 1, 4, 0, JPVT, 1, 0, TAU, 1, 0, norms.VN1, 1, 0, norms.VN2, 1, 0, WORK, 1, 0 );

	assertArrayClose( extractMatrix( A, 4, 3, 4 ), tc.a, 1e-14, 'a' );
	assertArrayClose( Array.from( TAU ), tc.tau, 1e-14, 'tau' );

	// Fixture JPVT is 1-based, ours is 0-based
	assert.deepStrictEqual( Array.from( JPVT ), tc.jpvt.map( function( v ) { return v - 1; } ) );
});

test( 'dlaqp2: square_3x3', function t() {
	var norms;
	var JPVT;
	var WORK;
	var TAU;
	var tc;
	var A;

	tc = findCase( 'square_3x3' );

	A = makeMatrix( [
		1, 0, 1,
		2, 1, 0,
		3, 2, 1
	], 3, 3, 3 );

	JPVT = new Int32Array( [ 0, 1, 2 ] );
	TAU = new Float64Array( 3 );
	WORK = new Float64Array( 3 );
	norms = computeNorms( A, 3, 3, 3, 0 );

	dlaqp2( 3, 3, 0, A, 1, 3, 0, JPVT, 1, 0, TAU, 1, 0, norms.VN1, 1, 0, norms.VN2, 1, 0, WORK, 1, 0 );

	assertArrayClose( extractMatrix( A, 3, 3, 3 ), tc.a, 1e-14, 'a' );
	assertArrayClose( Array.from( TAU ), tc.tau, 1e-14, 'tau' );
	assert.deepStrictEqual( Array.from( JPVT ), tc.jpvt.map( function( v ) { return v - 1; } ) );
});

test( 'dlaqp2: n_one', function t() {
	var norms;
	var JPVT;
	var WORK;
	var TAU;
	var tc;
	var A;

	tc = findCase( 'n_one' );

	A = makeMatrix( [
		3, 4, 0
	], 3, 1, 3 );

	JPVT = new Int32Array( [ 0 ] );
	TAU = new Float64Array( 1 );
	WORK = new Float64Array( 1 );
	norms = computeNorms( A, 3, 1, 3, 0 );

	dlaqp2( 3, 1, 0, A, 1, 3, 0, JPVT, 1, 0, TAU, 1, 0, norms.VN1, 1, 0, norms.VN2, 1, 0, WORK, 1, 0 );

	assertArrayClose( extractMatrix( A, 3, 1, 3 ), tc.a, 1e-14, 'a' );
	assertArrayClose( Array.from( TAU ), tc.tau, 1e-14, 'tau' );
	assert.deepStrictEqual( Array.from( JPVT ), tc.jpvt.map( function( v ) { return v - 1; } ) );
});

test( 'dlaqp2: one_by_one', function t() {
	var JPVT;
	var WORK;
	var VN1;
	var VN2;
	var TAU;
	var tc;
	var A;

	tc = findCase( 'one_by_one' );

	A = new Float64Array( [ 7.0 ] );
	JPVT = new Int32Array( [ 0 ] );
	TAU = new Float64Array( 1 );
	WORK = new Float64Array( 1 );
	VN1 = new Float64Array( [ 7.0 ] );
	VN2 = new Float64Array( [ 7.0 ] );

	dlaqp2( 1, 1, 0, A, 1, 1, 0, JPVT, 1, 0, TAU, 1, 0, VN1, 1, 0, VN2, 1, 0, WORK, 1, 0 );

	assertArrayClose( Array.from( A ), tc.a, 1e-14, 'a' );
	assertArrayClose( Array.from( TAU ), tc.tau, 1e-14, 'tau' );
	assert.deepStrictEqual( Array.from( JPVT ), tc.jpvt.map( function( v ) { return v - 1; } ) );
});

test( 'dlaqp2: n_zero', function t() {
	var JPVT;
	var WORK;
	var VN1;
	var VN2;
	var TAU;
	var A;

	// N=0 means mn=0, so the loop does nothing
	A = new Float64Array( 3 );
	JPVT = new Int32Array( 1 );
	TAU = new Float64Array( 1 );
	WORK = new Float64Array( 1 );
	VN1 = new Float64Array( 1 );
	VN2 = new Float64Array( 1 );

	// Should not throw
	dlaqp2( 3, 0, 0, A, 1, 3, 0, JPVT, 1, 0, TAU, 1, 0, VN1, 1, 0, VN2, 1, 0, WORK, 1, 0 );
});

test( 'dlaqp2: offset_1', function t() {
	var norms;
	var JPVT;
	var WORK;
	var TAU;
	var tc;
	var A;

	tc = findCase( 'offset_1' );

	// 4x3 matrix with offset=1, LDA=4
	A = makeMatrix( [
		5, 0, 0, 0,
		1, 1, 3, 2,
		2, 2, 0, 1
	], 4, 3, 4 );

	JPVT = new Int32Array( [ 0, 1, 2 ] );
	TAU = new Float64Array( 3 );
	WORK = new Float64Array( 3 );
	// Norms computed from rows 1..3 (0-based), i.e. rows 2..4 in Fortran
	norms = computeNorms( A, 4, 3, 4, 1 );

	dlaqp2( 4, 3, 1, A, 1, 4, 0, JPVT, 1, 0, TAU, 1, 0, norms.VN1, 1, 0, norms.VN2, 1, 0, WORK, 1, 0 );

	assertArrayClose( extractMatrix( A, 4, 3, 4 ), tc.a, 1e-14, 'a' );
	assertArrayClose( Array.from( TAU ), tc.tau, 1e-14, 'tau' );
	assert.deepStrictEqual( Array.from( JPVT ), tc.jpvt.map( function( v ) { return v - 1; } ) );
});

test( 'dlaqp2: pivot_reorder', function t() {
	var norms;
	var JPVT;
	var WORK;
	var TAU;
	var tc;
	var A;

	tc = findCase( 'pivot_reorder' );

	A = makeMatrix( [
		0.1, 0.1, 0.1,
		1, 1, 0,
		5, 3, 4
	], 3, 3, 3 );

	JPVT = new Int32Array( [ 0, 1, 2 ] );
	TAU = new Float64Array( 3 );
	WORK = new Float64Array( 3 );
	norms = computeNorms( A, 3, 3, 3, 0 );

	dlaqp2( 3, 3, 0, A, 1, 3, 0, JPVT, 1, 0, TAU, 1, 0, norms.VN1, 1, 0, norms.VN2, 1, 0, WORK, 1, 0 );

	assertArrayClose( extractMatrix( A, 3, 3, 3 ), tc.a, 1e-14, 'a' );
	assertArrayClose( Array.from( TAU ), tc.tau, 1e-14, 'tau' );
	assert.deepStrictEqual( Array.from( JPVT ), tc.jpvt.map( function( v ) { return v - 1; } ) );
});

test( 'dlaqp2: collinear_recomp', function t() {
	var norms;
	var JPVT;
	var WORK;
	var TAU;
	var tc;
	var A;

	tc = findCase( 'collinear_recomp' );

	A = makeMatrix( [
		1, 2, 3, 4, 5, 6,
		1, 2, 3, 4, 5, 6 + 1e-10,
		1, 2, 3, 4, 5, 6 + 1e-15
	], 6, 3, 6 );

	JPVT = new Int32Array( [ 0, 1, 2 ] );
	TAU = new Float64Array( 3 );
	WORK = new Float64Array( 3 );
	norms = computeNorms( A, 6, 3, 6, 0 );

	dlaqp2( 6, 3, 0, A, 1, 6, 0, JPVT, 1, 0, TAU, 1, 0, norms.VN1, 1, 0, norms.VN2, 1, 0, WORK, 1, 0 );

	assertArrayClose( extractMatrix( A, 6, 3, 6 ), tc.a, 1e-4, 'a' );
	assertArrayClose( Array.from( TAU ), tc.tau, 1e-4, 'tau' );
	assert.deepStrictEqual( Array.from( JPVT ), tc.jpvt.map( function( v ) { return v - 1; } ) );
});
