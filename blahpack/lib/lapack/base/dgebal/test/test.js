'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dgebal = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dgebal.jsonl' ), 'utf8' ).trim().split( '\n' );
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
* Build a column-major Float64Array from column arrays.
*
* @param {number} N - matrix order
* @param {Array<Array<number>>} cols - array of column arrays (each length N)
* @returns {Float64Array} column-major matrix
*/
function buildMatrix( N, cols ) {
	var out = new Float64Array( N * N );
	var i;
	var j;
	for ( j = 0; j < cols.length; j++ ) {
		for ( i = 0; i < N; i++ ) {
			out[ i + ( j * N ) ] = cols[ j ][ i ];
		}
	}
	return out;
}

/**
* Extract column j (0-based) from a column-major N x N matrix.
*
* @param {Float64Array} A - matrix
* @param {number} N - order
* @param {number} j - column index (0-based)
* @returns {Array<number>} column values
*/
function getCol( A, N, j ) {
	var out = [];
	var i;
	for ( i = 0; i < N; i++ ) {
		out.push( A[ i + ( j * N ) ] );
	}
	return out;
}


// TESTS //

test( 'dgebal: n0 (N=0 quick return)', function t() {
	var result;
	var SCALE = new Float64Array( 1 );
	var A = new Float64Array( 1 );
	result = dgebal( 'both', 0, A, 1, 0, 0, SCALE, 1, 0 );
	assert.equal( result.info, 0, 'info' );
	assert.equal( result.ilo, 1, 'ilo' );
	assert.equal( result.ihi, 0, 'ihi' );
});

test( 'dgebal: n1 (N=1)', function t() {
	var tc = findCase( 'n1' );
	var result;
	var A = new Float64Array( [ 5.0 ] );
	var SCALE = new Float64Array( 1 );
	result = dgebal( 'both', 1, A, 1, 1, 0, SCALE, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
	assert.equal( result.ilo, tc.ilo, 'ilo' );
	assert.equal( result.ihi, tc.ihi, 'ihi' );
	assertArrayClose( Array.from( SCALE ), tc.scale, 1e-14, 'scale' );
});

test( 'dgebal: job_n (JOB=N, no balancing)', function t() {
	var tc = findCase( 'job_n' );
	var N = 3;
	var result;
	var A = buildMatrix( N, [
		[ 1, 4, 7 ],
		[ 2, 5, 8 ],
		[ 3, 6, 9 ]
	]);
	var SCALE = new Float64Array( N );
	result = dgebal( 'none', N, A, 1, N, 0, SCALE, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
	assert.equal( result.ilo, tc.ilo, 'ilo' );
	assert.equal( result.ihi, tc.ihi, 'ihi' );
	assertArrayClose( Array.from( SCALE ), tc.scale, 1e-14, 'scale' );
});

test( 'dgebal: job_p (JOB=P, permute only)', function t() {
	var tc = findCase( 'job_p' );
	var N = 4;
	var result;
	// Row 1: [1, 2, 3, 0]
	// Row 2: [0, 5, 6, 0]
	// Row 3: [0, 8, 9, 0]
	// Row 4: [0, 0, 0, 4]
	var A = buildMatrix( N, [
		[ 1, 0, 0, 0 ],  // col 1
		[ 2, 5, 8, 0 ],  // col 2
		[ 3, 6, 9, 0 ],  // col 3
		[ 0, 0, 0, 4 ]   // col 4
	]);
	var SCALE = new Float64Array( N );
	result = dgebal( 'permute', N, A, 1, N, 0, SCALE, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
	assert.equal( result.ilo, tc.ilo, 'ilo' );
	assert.equal( result.ihi, tc.ihi, 'ihi' );
	assertArrayClose( Array.from( SCALE ), tc.scale, 1e-14, 'scale' );
	assertArrayClose( getCol( A, N, 0 ), tc.A, 1e-14, 'A col1' );
	assertArrayClose( getCol( A, N, 1 ), tc.A2, 1e-14, 'A col2' );
	assertArrayClose( getCol( A, N, 2 ), tc.A3, 1e-14, 'A col3' );
	assertArrayClose( getCol( A, N, 3 ), tc.A4, 1e-14, 'A col4' );
});

test( 'dgebal: job_s (JOB=S, scale only)', function t() {
	var tc = findCase( 'job_s' );
	var N = 3;
	var result;
	var A = buildMatrix( N, [
		[ 1, 1000, 0 ],      // col 1
		[ 0, 1, 1000 ],      // col 2
		[ 0, 0, 1 ]          // col 3
	]);
	var SCALE = new Float64Array( N );
	result = dgebal( 'scale', N, A, 1, N, 0, SCALE, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
	assert.equal( result.ilo, tc.ilo, 'ilo' );
	assert.equal( result.ihi, tc.ihi, 'ihi' );
	assertArrayClose( Array.from( SCALE ), tc.scale, 1e-14, 'scale' );
	assertArrayClose( getCol( A, N, 0 ), tc.A1, 1e-14, 'A col1' );
	assertArrayClose( getCol( A, N, 1 ), tc.A2, 1e-14, 'A col2' );
	assertArrayClose( getCol( A, N, 2 ), tc.A3, 1e-14, 'A col3' );
});

test( 'dgebal: job_b (JOB=B, both permute and scale)', function t() {
	var tc = findCase( 'job_b' );
	var N = 4;
	var result;
	// Row 1: [1,    0,     0,   0]
	// Row 2: [100,  2,     300, 0]
	// Row 3: [0,    0.01,  3,   0]
	// Row 4: [0,    0,     0,   4]
	var A = buildMatrix( N, [
		[ 1, 100, 0, 0 ],       // col 1
		[ 0, 2, 0.01, 0 ],      // col 2
		[ 0, 300, 3, 0 ],       // col 3
		[ 0, 0, 0, 4 ]          // col 4
	]);
	var SCALE = new Float64Array( N );
	result = dgebal( 'both', N, A, 1, N, 0, SCALE, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
	assert.equal( result.ilo, tc.ilo, 'ilo' );
	assert.equal( result.ihi, tc.ihi, 'ihi' );
	assertArrayClose( Array.from( SCALE ), tc.scale, 1e-14, 'scale' );
	assertArrayClose( getCol( A, N, 0 ), tc.A1, 1e-14, 'A col1' );
	assertArrayClose( getCol( A, N, 1 ), tc.A2, 1e-14, 'A col2' );
	assertArrayClose( getCol( A, N, 2 ), tc.A3, 1e-14, 'A col3' );
	assertArrayClose( getCol( A, N, 3 ), tc.A4, 1e-14, 'A col4' );
});

test( 'dgebal: diagonal (already balanced diagonal matrix)', function t() {
	var tc = findCase( 'diagonal' );
	var N = 3;
	var result;
	var A = buildMatrix( N, [
		[ 2, 0, 0 ],
		[ 0, 3, 0 ],
		[ 0, 0, 5 ]
	]);
	var SCALE = new Float64Array( N );
	result = dgebal( 'both', N, A, 1, N, 0, SCALE, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
	assert.equal( result.ilo, tc.ilo, 'ilo' );
	assert.equal( result.ihi, tc.ihi, 'ihi' );
	assertArrayClose( Array.from( SCALE ), tc.scale, 1e-14, 'scale' );
});

test( 'dgebal: perm_and_scale (5x5 with permutations on both ends)', function t() {
	var tc = findCase( 'perm_and_scale' );
	var N = 5;
	var result;
	// Row 1: [1,     0,       0,     0,  0]
	// Row 2: [0,     2,    1000,     0,  0]
	// Row 3: [0,   0.001,    3,    500,  0]
	// Row 4: [0,     0,    0.002,    4,  0]
	// Row 5: [0,     0,       0,     0,  5]
	var A = buildMatrix( N, [
		[ 1, 0, 0, 0, 0 ],         // col 1
		[ 0, 2, 0.001, 0, 0 ],     // col 2
		[ 0, 1000, 3, 0.002, 0 ],  // col 3
		[ 0, 0, 500, 4, 0 ],       // col 4
		[ 0, 0, 0, 0, 5 ]          // col 5
	]);
	var SCALE = new Float64Array( N );
	result = dgebal( 'both', N, A, 1, N, 0, SCALE, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
	assert.equal( result.ilo, tc.ilo, 'ilo' );
	assert.equal( result.ihi, tc.ihi, 'ihi' );
	assertArrayClose( Array.from( SCALE ), tc.scale, 1e-14, 'scale' );
	assertArrayClose( getCol( A, N, 0 ), tc.A1, 1e-14, 'A col1' );
	assertArrayClose( getCol( A, N, 1 ), tc.A2, 1e-14, 'A col2' );
	assertArrayClose( getCol( A, N, 2 ), tc.A3, 1e-14, 'A col3' );
	assertArrayClose( getCol( A, N, 3 ), tc.A4, 1e-14, 'A col4' );
	assertArrayClose( getCol( A, N, 4 ), tc.A5, 1e-14, 'A col5' );
});

test( 'dgebal: col_isolation (JOB=P with column isolation)', function t() {
	var tc = findCase( 'col_isolation' );
	var N = 4;
	var result;
	// Row 1: [7, 1, 2, 3]
	// Row 2: [0, 4, 5, 0]
	// Row 3: [0, 6, 8, 0]
	// Row 4: [0, 0, 0, 9]
	var A = buildMatrix( N, [
		[ 7, 0, 0, 0 ],  // col 1
		[ 1, 4, 6, 0 ],  // col 2
		[ 2, 5, 8, 0 ],  // col 3
		[ 3, 0, 0, 9 ]   // col 4
	]);
	var SCALE = new Float64Array( N );
	result = dgebal( 'permute', N, A, 1, N, 0, SCALE, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
	assert.equal( result.ilo, tc.ilo, 'ilo' );
	assert.equal( result.ihi, tc.ihi, 'ihi' );
	assertArrayClose( Array.from( SCALE ), tc.scale, 1e-14, 'scale' );
	assertArrayClose( getCol( A, N, 0 ), tc.A1, 1e-14, 'A col1' );
	assertArrayClose( getCol( A, N, 1 ), tc.A2, 1e-14, 'A col2' );
	assertArrayClose( getCol( A, N, 2 ), tc.A3, 1e-14, 'A col3' );
	assertArrayClose( getCol( A, N, 3 ), tc.A4, 1e-14, 'A col4' );
});
