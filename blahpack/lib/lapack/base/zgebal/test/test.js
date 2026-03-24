'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var zgebal = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zgebal.jsonl' ), 'utf8' ).trim().split( '\n' );
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
* Build a column-major Complex128Array from interleaved re/im column data.
*
* @param {number} N - matrix order
* @param {number} LDA - leading dimension (in complex elements)
* @param {Array<Array<number>>} cols - array of column data, each is interleaved [re0,im0,re1,im1,...] of length 2*N
* @returns {Complex128Array} column-major matrix with LDA rows
*/
function buildComplexMatrix( N, LDA, cols ) {
	var out = new Complex128Array( LDA * cols.length );
	var ov = reinterpret( out, 0 );
	var col;
	var i;
	var j;
	for ( j = 0; j < cols.length; j++ ) {
		col = cols[ j ];
		for ( i = 0; i < N; i++ ) {
			ov[ ( j * LDA + i ) * 2 ] = col[ i * 2 ];
			ov[ ( j * LDA + i ) * 2 + 1 ] = col[ i * 2 + 1 ];
		}
	}
	return out;
}

/**
* Extract column j (0-based) as interleaved Float64 array of length 2*N.
*/
function extractColumn( A, N, LDA, j ) {
	var av = reinterpret( A, 0 );
	var out = [];
	var i;
	for ( i = 0; i < N; i++ ) {
		out.push( av[ ( j * LDA + i ) * 2 ] );
		out.push( av[ ( j * LDA + i ) * 2 + 1 ] );
	}
	return out;
}


// TESTS //

test( 'zgebal: n0 - N=0 quick return', function t() {
	var result;
	var SCALE;
	var A;

	A = new Complex128Array( 0 );
	SCALE = new Float64Array( 0 );
	result = zgebal( 'B', 0, A, 1, 1, 0, SCALE, 1, 0 );
	assert.equal( result.info, 0, 'info' );
	assert.equal( result.ilo, 1, 'ilo' );
	assert.equal( result.ihi, 0, 'ihi' );
});

test( 'zgebal: n1 - N=1', function t() {
	var result;
	var SCALE;
	var tc;
	var av;
	var A;

	tc = findCase( 'n1' );
	// A = [ (5, 3) ]
	A = new Complex128Array( 1 );
	av = reinterpret( A, 0 );
	av[ 0 ] = 5.0;
	av[ 1 ] = 3.0;
	SCALE = new Float64Array( 1 );

	result = zgebal( 'B', 1, A, 1, 1, 0, SCALE, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
	assert.equal( result.ilo, tc.ilo, 'ilo' );
	assert.equal( result.ihi, tc.ihi, 'ihi' );
	assertArrayClose( Array.from( SCALE ), tc.scale, 1e-14, 'scale' );
	assertArrayClose( Array.from( av ).slice( 0, 2 ), tc.A, 1e-14, 'A' );
});

test( 'zgebal: job_n - JOB=N no balancing', function t() {
	var result;
	var SCALE;
	var tc;
	var A;

	tc = findCase( 'job_n' );
	// 3x3 complex matrix, LDA=3
	A = buildComplexMatrix( 3, 3, [
		[ 1.0, 0.5, 4.0, 2.0, 7.0, 0.0 ],
		[ 2.0, 1.0, 5.0, 0.0, 8.0, 3.0 ],
		[ 3.0, 0.0, 6.0, 1.5, 9.0, 0.5 ]
	]);
	SCALE = new Float64Array( 3 );

	result = zgebal( 'N', 3, A, 1, 3, 0, SCALE, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
	assert.equal( result.ilo, tc.ilo, 'ilo' );
	assert.equal( result.ihi, tc.ihi, 'ihi' );
	assertArrayClose( Array.from( SCALE ), tc.scale, 1e-14, 'scale' );
});

test( 'zgebal: job_p - JOB=P permute only', function t() {
	var result;
	var SCALE;
	var tc;
	var A;

	tc = findCase( 'job_p' );
	// 4x4 complex matrix, LDA=4 (use N as LDA for simplicity)
	A = buildComplexMatrix( 4, 4, [
		[ 1.0, 0.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 ],
		[ 2.0, 1.0, 5.0, 2.0, 8.0, 1.0, 0.0, 0.0 ],
		[ 3.0, 0.0, 6.0, 0.0, 9.0, 0.5, 0.0, 0.0 ],
		[ 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 4.0, 3.0 ]
	]);
	SCALE = new Float64Array( 4 );

	result = zgebal( 'P', 4, A, 1, 4, 0, SCALE, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
	assert.equal( result.ilo, tc.ilo, 'ilo' );
	assert.equal( result.ihi, tc.ihi, 'ihi' );
	assertArrayClose( Array.from( SCALE ), tc.scale, 1e-14, 'scale' );
	assertArrayClose( extractColumn( A, 4, 4, 0 ), tc.A1, 1e-14, 'A col 1' );
	assertArrayClose( extractColumn( A, 4, 4, 1 ), tc.A2, 1e-14, 'A col 2' );
	assertArrayClose( extractColumn( A, 4, 4, 2 ), tc.A3, 1e-14, 'A col 3' );
	assertArrayClose( extractColumn( A, 4, 4, 3 ), tc.A4, 1e-14, 'A col 4' );
});

test( 'zgebal: job_s - JOB=S scale only', function t() {
	var result;
	var SCALE;
	var tc;
	var A;

	tc = findCase( 'job_s' );
	// 3x3, LDA=3
	A = buildComplexMatrix( 3, 3, [
		[ 1.0, 0.5, 1000.0, 500.0, 0.0, 0.0 ],
		[ 0.0, 0.0, 1.0, 0.5, 1000.0, 200.0 ],
		[ 0.0, 0.0, 0.0, 0.0, 1.0, 0.5 ]
	]);
	SCALE = new Float64Array( 3 );

	result = zgebal( 'S', 3, A, 1, 3, 0, SCALE, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
	assert.equal( result.ilo, tc.ilo, 'ilo' );
	assert.equal( result.ihi, tc.ihi, 'ihi' );
	assertArrayClose( Array.from( SCALE ), tc.scale, 1e-14, 'scale' );
	assertArrayClose( extractColumn( A, 3, 3, 0 ), tc.A1, 1e-14, 'A col 1' );
	assertArrayClose( extractColumn( A, 3, 3, 1 ), tc.A2, 1e-14, 'A col 2' );
	assertArrayClose( extractColumn( A, 3, 3, 2 ), tc.A3, 1e-14, 'A col 3' );
});

test( 'zgebal: job_b - JOB=B both permute and scale', function t() {
	var result;
	var SCALE;
	var tc;
	var A;

	tc = findCase( 'job_b' );
	// 4x4, LDA=4
	A = buildComplexMatrix( 4, 4, [
		[ 1.0, 0.0, 100.0, 50.0, 0.0, 0.0, 0.0, 0.0 ],
		[ 0.0, 0.0, 2.0, 1.0, 0.01, 0.005, 0.0, 0.0 ],
		[ 0.0, 0.0, 300.0, 100.0, 3.0, 0.5, 0.0, 0.0 ],
		[ 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 4.0, 2.0 ]
	]);
	SCALE = new Float64Array( 4 );

	result = zgebal( 'B', 4, A, 1, 4, 0, SCALE, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
	assert.equal( result.ilo, tc.ilo, 'ilo' );
	assert.equal( result.ihi, tc.ihi, 'ihi' );
	assertArrayClose( Array.from( SCALE ), tc.scale, 1e-14, 'scale' );
	assertArrayClose( extractColumn( A, 4, 4, 0 ), tc.A1, 1e-14, 'A col 1' );
	assertArrayClose( extractColumn( A, 4, 4, 1 ), tc.A2, 1e-14, 'A col 2' );
	assertArrayClose( extractColumn( A, 4, 4, 2 ), tc.A3, 1e-14, 'A col 3' );
	assertArrayClose( extractColumn( A, 4, 4, 3 ), tc.A4, 1e-14, 'A col 4' );
});

test( 'zgebal: diagonal - already balanced matrix', function t() {
	var result;
	var SCALE;
	var tc;
	var A;

	tc = findCase( 'diagonal' );
	// 3x3 diagonal
	A = buildComplexMatrix( 3, 3, [
		[ 2.0, 1.0, 0.0, 0.0, 0.0, 0.0 ],
		[ 0.0, 0.0, 3.0, 2.0, 0.0, 0.0 ],
		[ 0.0, 0.0, 0.0, 0.0, 5.0, 0.5 ]
	]);
	SCALE = new Float64Array( 3 );

	result = zgebal( 'B', 3, A, 1, 3, 0, SCALE, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
	assert.equal( result.ilo, tc.ilo, 'ilo' );
	assert.equal( result.ihi, tc.ihi, 'ihi' );
	assertArrayClose( Array.from( SCALE ), tc.scale, 1e-14, 'scale' );
});

test( 'zgebal: perm_and_scale - 5x5 with permutations on both ends', function t() {
	var result;
	var SCALE;
	var tc;
	var A;

	tc = findCase( 'perm_and_scale' );
	// 5x5, LDA=5
	A = buildComplexMatrix( 5, 5, [
		[ 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 ],
		[ 0.0, 0.0, 2.0, 1.0, 0.001, 0.0005, 0.0, 0.0, 0.0, 0.0 ],
		[ 0.0, 0.0, 1000.0, 500.0, 3.0, 1.0, 0.002, 0.001, 0.0, 0.0 ],
		[ 0.0, 0.0, 0.0, 0.0, 500.0, 250.0, 4.0, 2.0, 0.0, 0.0 ],
		[ 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 5.0, 0.0 ]
	]);
	SCALE = new Float64Array( 5 );

	result = zgebal( 'B', 5, A, 1, 5, 0, SCALE, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
	assert.equal( result.ilo, tc.ilo, 'ilo' );
	assert.equal( result.ihi, tc.ihi, 'ihi' );
	assertArrayClose( Array.from( SCALE ), tc.scale, 1e-14, 'scale' );
	assertArrayClose( extractColumn( A, 5, 5, 0 ), tc.A1, 1e-14, 'A col 1' );
	assertArrayClose( extractColumn( A, 5, 5, 1 ), tc.A2, 1e-14, 'A col 2' );
	assertArrayClose( extractColumn( A, 5, 5, 2 ), tc.A3, 1e-14, 'A col 3' );
	assertArrayClose( extractColumn( A, 5, 5, 3 ), tc.A4, 1e-14, 'A col 4' );
	assertArrayClose( extractColumn( A, 5, 5, 4 ), tc.A5, 1e-14, 'A col 5' );
});

test( 'zgebal: col_isolation - JOB=P with column isolation', function t() {
	var result;
	var SCALE;
	var tc;
	var A;

	tc = findCase( 'col_isolation' );
	// 4x4, LDA=4
	A = buildComplexMatrix( 4, 4, [
		[ 7.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 ],
		[ 1.0, 0.5, 4.0, 2.0, 6.0, 1.0, 0.0, 0.0 ],
		[ 2.0, 0.0, 5.0, 0.0, 8.0, 0.5, 0.0, 0.0 ],
		[ 3.0, 1.0, 0.0, 0.0, 0.0, 0.0, 9.0, 3.0 ]
	]);
	SCALE = new Float64Array( 4 );

	result = zgebal( 'P', 4, A, 1, 4, 0, SCALE, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
	assert.equal( result.ilo, tc.ilo, 'ilo' );
	assert.equal( result.ihi, tc.ihi, 'ihi' );
	assertArrayClose( Array.from( SCALE ), tc.scale, 1e-14, 'scale' );
	assertArrayClose( extractColumn( A, 4, 4, 0 ), tc.A1, 1e-14, 'A col 1' );
	assertArrayClose( extractColumn( A, 4, 4, 1 ), tc.A2, 1e-14, 'A col 2' );
	assertArrayClose( extractColumn( A, 4, 4, 2 ), tc.A3, 1e-14, 'A col 3' );
	assertArrayClose( extractColumn( A, 4, 4, 3 ), tc.A4, 1e-14, 'A col 4' );
});
