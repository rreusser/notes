/* eslint-disable no-restricted-syntax, stdlib/first-unit-test, max-lines */

'use strict';

// MODULES //

var readFileSync = require( 'fs' ).readFileSync; // eslint-disable-line node/no-sync
var path = require( 'path' );
var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zggqrf = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'zggqrf.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line max-len, node/no-sync
var fixture = lines.map( function parse( line ) {
	return JSON.parse( line );
});


// FUNCTIONS //

/**
* Finds a named test case in the fixture data.
*
* @private
* @param {string} name - test case name
* @returns {Object} test case data
*/
function findCase( name ) {
	return fixture.find( function find( t ) {
		return t.name === name;
	});
}

/**
* Asserts that two numbers are close within a relative tolerance.
*
* @private
* @param {number} actual - actual value
* @param {number} expected - expected value
* @param {number} tol - tolerance
* @param {string} msg - assertion message
*/
function assertClose( actual, expected, tol, msg ) {
	var relErr;

	relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 ); // eslint-disable-line max-len
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual ); // eslint-disable-line max-len
}

/**
* Asserts that two arrays are element-wise close.
*
* @private
* @param {Array} actual - actual array
* @param {Array} expected - expected array
* @param {number} tol - tolerance
* @param {string} msg - assertion message
*/
function assertArrayClose( actual, expected, tol, msg ) {
	var i;

	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i += 1 ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' ); // eslint-disable-line max-len
	}
}

/**
* Converts a typed array to a plain Array.
*
* @private
* @param {TypedArray} arr - input array
* @returns {Array} plain array
*/
function toArray( arr ) {
	var out;
	var i;

	out = [];
	for ( i = 0; i < arr.length; i += 1 ) {
		out.push( arr[ i ] );
	}
	return out;
}

/**
* Builds a column-major interleaved array for a complex N-by-M matrix.
*
* @private
* @param {NonNegativeInteger} N - number of rows
* @param {NonNegativeInteger} M - number of columns
* @param {Array} vals - flat array of re/im pairs in row-major order
* @returns {Array} interleaved re/im in column-major layout
*/
function colMajorComplex( N, M, vals ) {
	var out;
	var idx;
	var i;
	var j;

	out = [];
	out.length = 2 * N * M;
	for ( j = 0; j < M; j += 1 ) {
		for ( i = 0; i < N; i += 1 ) {
			idx = ((i * M) + j) * 2;
			out[ ((j * N) + i) * 2 ] = vals[ idx ];
			out[ (((j * N) + i) * 2) + 1 ] = vals[ idx + 1 ];
		}
	}
	return out;
}

/**
* Calls zggqrf with the given parameters.
*
* @private
* @param {NonNegativeInteger} N - number of rows
* @param {NonNegativeInteger} M - number of columns of A
* @param {NonNegativeInteger} P - number of columns of B
* @param {Array} aFlat - interleaved re/im values for A (column-major)
* @param {Array} bFlat - interleaved re/im values for B (column-major)
* @returns {Object} results containing info, A, TAUA, B, TAUB
*/
function callZggqrf( N, M, P, aFlat, bFlat ) {
	var lwork;
	var TAUA;
	var TAUB;
	var WORK;
	var info;
	var A;
	var B;

	WORK = new Complex128Array( Math.max( 1, Math.max( N, M, P ) * 64 ) );
	TAUA = new Complex128Array( Math.min( N, M ) );
	TAUB = new Complex128Array( Math.min( N, P ) );
	lwork = WORK.length;
	A = new Complex128Array( new Float64Array( aFlat ) );
	B = new Complex128Array( new Float64Array( bFlat ) );

	// Column-major: strideA1=1, strideA2=N (in complex elements)
	info = zggqrf( N, M, P, A, 1, N, 0, TAUA, 1, 0, B, 1, N, 0, TAUB, 1, 0, WORK, 1, 0, lwork ); // eslint-disable-line max-len
	return {
		'info': info,
		'A': toArray( reinterpret( A, 0 ) ),
		'TAUA': toArray( reinterpret( TAUA, 0 ) ),
		'B': toArray( reinterpret( B, 0 ) ),
		'TAUB': toArray( reinterpret( TAUB, 0 ) )
	};
}


// TESTS //

test( 'zggqrf is a function', function t() {
	assert.equal( typeof zggqrf, 'function' );
});

test( 'zggqrf: basic_3x3', function t() {
	var res;
	var tc;
	var A;
	var B;

	tc = findCase( 'basic_3x3' );

	// Row-major re/im pairs: each row is [re0,im0, re1,im1, ...]
	A = colMajorComplex( 3, 3, [
		2,
		1,
		1,
		2,
		3,
		0,
		1,
		0,
		4,
		1,
		2,
		-1,
		3,
		-1,
		2,
		0,
		5,
		2
	]);
	B = colMajorComplex( 3, 3, [
		1,
		0.5,
		2,
		1,
		1,
		-1,
		3,
		0,
		1,
		-1,
		2,
		0.5,
		2,
		1,
		3,
		0,
		1,
		1
	]);
	res = callZggqrf( 3, 3, 3, A, B );
	assert.equal( res.info, tc.info, 'info' );
	assertArrayClose( res.A, tc.A, 1e-14, 'A' );
	assertArrayClose( res.TAUA, tc.TAUA, 1e-14, 'TAUA' );
	assertArrayClose( res.B, tc.B, 1e-14, 'B' );
	assertArrayClose( res.TAUB, tc.TAUB, 1e-14, 'TAUB' );
});

test( 'zggqrf: m_gt_n', function t() {
	var res;
	var tc;
	var A;
	var B;

	tc = findCase( 'm_gt_n' );
	A = colMajorComplex( 3, 4, [
		2,
		1,
		1,
		2,
		3,
		0,
		1,
		1,
		1,
		0,
		4,
		1,
		2,
		-1,
		3,
		0,
		3,
		-1,
		2,
		0,
		5,
		2,
		2,
		-2
	]);
	B = colMajorComplex( 3, 3, [
		1,
		0.5,
		2,
		1,
		1,
		-1,
		3,
		0,
		1,
		-1,
		2,
		0.5,
		2,
		1,
		3,
		0,
		1,
		1
	]);
	res = callZggqrf( 3, 4, 3, A, B );
	assert.equal( res.info, tc.info, 'info' );
	assertArrayClose( res.A, tc.A, 1e-14, 'A' );
	assertArrayClose( res.TAUA, tc.TAUA, 1e-14, 'TAUA' );
	assertArrayClose( res.B, tc.B, 1e-14, 'B' );
	assertArrayClose( res.TAUB, tc.TAUB, 1e-14, 'TAUB' );
});

test( 'zggqrf: m_lt_n', function t() {
	var res;
	var tc;
	var A;
	var B;

	tc = findCase( 'm_lt_n' );
	A = colMajorComplex( 4, 3, [
		2,
		1,
		1,
		2,
		3,
		0,
		1,
		0,
		4,
		1,
		2,
		-1,
		3,
		-1,
		2,
		0,
		5,
		2,
		1,
		1,
		3,
		-1,
		1,
		0.5
	]);
	B = colMajorComplex( 4, 4, [
		1,
		0.5,
		2,
		1,
		1,
		-1,
		3,
		0,
		3,
		0,
		1,
		-1,
		2,
		0.5,
		1,
		1,
		2,
		1,
		3,
		0,
		1,
		1,
		2,
		-1,
		1,
		-0.5,
		2,
		1,
		3,
		0,
		1,
		2
	]);
	res = callZggqrf( 4, 3, 4, A, B );
	assert.equal( res.info, tc.info, 'info' );
	assertArrayClose( res.A, tc.A, 1e-14, 'A' );
	assertArrayClose( res.TAUA, tc.TAUA, 1e-14, 'TAUA' );
	assertArrayClose( res.B, tc.B, 1e-14, 'B' );
	assertArrayClose( res.TAUB, tc.TAUB, 1e-14, 'TAUB' );
});

test( 'zggqrf: n_zero (quick return)', function t() {
	var TAUA;
	var TAUB;
	var WORK;
	var info;
	var tc;
	var A;
	var B;

	tc = findCase( 'n_zero' );
	WORK = new Complex128Array( 64 );
	TAUA = new Complex128Array( 0 );
	TAUB = new Complex128Array( 0 );
	A = new Complex128Array( 0 );
	B = new Complex128Array( 0 );
	info = zggqrf( 0, 3, 3, A, 1, 0, 0, TAUA, 1, 0, B, 1, 0, 0, TAUB, 1, 0, WORK, 1, 0, 64 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
});

test( 'zggqrf: n_one', function t() {
	var res;
	var tc;
	var A;
	var B;

	tc = findCase( 'n_one' );
	A = [
		5,
		2
	];
	B = [
		3,
		-1
	];
	res = callZggqrf( 1, 1, 1, A, B );
	assert.equal( res.info, tc.info, 'info' );
	assertArrayClose( res.A, tc.A, 1e-14, 'A' );
	assertArrayClose( res.TAUA, tc.TAUA, 1e-14, 'TAUA' );
	assertArrayClose( res.B, tc.B, 1e-14, 'B' );
	assertArrayClose( res.TAUB, tc.TAUB, 1e-14, 'TAUB' );
});

test( 'zggqrf: tall_skinny', function t() {
	var res;
	var tc;
	var A;
	var B;

	tc = findCase( 'tall_skinny' );
	A = colMajorComplex( 5, 2, [
		1,
		0.5,
		2,
		1,
		3,
		0,
		1,
		-1,
		2,
		1,
		3,
		0,
		1,
		-0.5,
		1,
		1,
		2,
		0,
		2,
		-1
	]);
	B = colMajorComplex( 5, 3, [
		1,
		0.5,
		0.5,
		1,
		2,
		0,
		0.5,
		0,
		3,
		-1,
		1,
		0.5,
		2,
		1,
		1,
		0,
		1,
		-1,
		1,
		-1,
		2,
		0.5,
		0.5,
		0,
		3,
		0,
		1,
		1,
		2,
		-0.5
	]);
	res = callZggqrf( 5, 2, 3, A, B );
	assert.equal( res.info, tc.info, 'info' );
	assertArrayClose( res.A, tc.A, 1e-14, 'A' );
	assertArrayClose( res.TAUA, tc.TAUA, 1e-14, 'TAUA' );
	assertArrayClose( res.B, tc.B, 1e-14, 'B' );
	assertArrayClose( res.TAUB, tc.TAUB, 1e-14, 'TAUB' );
});
