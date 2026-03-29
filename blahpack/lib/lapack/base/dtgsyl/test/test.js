/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';


// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dtgsyl = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dtgsyl.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
var fixture = lines.map( function parse( line ) {
	return JSON.parse( line );
} );


// FUNCTIONS //

/**
* Returns a test case from the fixture data.
*
* @private
* @param {string} name - test case name
* @returns {*} result
*/
function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name;
	} );
}

/**
* Asserts that two numbers are approximately equal.
*
* @private
* @param {*} actual - actual value
* @param {*} expected - expected value
* @param {number} tol - tolerance
* @param {string} msg - assertion message
*/
function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 ); // eslint-disable-line max-len
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

/**
* Asserts that two arrays are element-wise approximately equal.
*
* @private
* @param {*} actual - actual value
* @param {*} expected - expected value
* @param {number} tol - tolerance
* @param {string} msg - assertion message
*/
function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}

/**
* PackMatrix.
*
* @private
* @param {*} entries - entries
* @param {*} M - M
* @param {*} N - N
* @returns {*} result
*/
function packMatrix( entries, M, N ) {
	var A = new Float64Array( M * N );
	var i;
	for ( i = 0; i < entries.length; i += 3 ) {
		A[ entries[ i + 1 ] * M + entries[ i ] ] = entries[ i + 2 ];
	}
	return A;
}

/**
* ExtractMatrix.
*
* @private
* @param {*} A - A
* @param {*} LDA - LDA
* @param {*} M - M
* @param {*} N - N
* @returns {*} result
*/
function extractMatrix( A, LDA, M, N ) {
	var out = [];
	var i;
	var j;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			out.push( A[ j * LDA + i ] );
		}
	}
	return out;
}


// TESTS //

test( 'dtgsyl: notrans_2x2', function t() {
	var scale;
	var IWORK;
	var WORK;
	var info;
	var dif;
	var tc;
	var M;
	var N;
	var A;
	var B;
	var C;
	var D;
	var E;
	var F;

	tc = findCase( 'notrans_2x2' );
	M = 2;
	N = 2;
	A = packMatrix([ 0, 0, 1.0, 0, 1, 0.5, 1, 1, 2.0 ], M, M);
	B = packMatrix([ 0, 0, 3.0, 0, 1, 0.3, 1, 1, 4.0 ], N, N);
	C = new Float64Array([ 1.0, 3.0, 2.0, 4.0 ]);
	D = packMatrix([ 0, 0, 1.0, 0, 1, 0.2, 1, 1, 1.5 ], M, M);
	E = packMatrix([ 0, 0, 1.0, 0, 1, 0.1, 1, 1, 2.0 ], N, N);
	F = new Float64Array([ 5.0, 7.0, 6.0, 8.0 ]);
	scale = new Float64Array( 1 );
	dif = new Float64Array( 1 );
	WORK = new Float64Array( 400 );
	IWORK = new Int32Array( M + N + 6 );
	info = dtgsyl( 'no-transpose', 0, M, N, A, 1, M, 0, B, 1, N, 0, C, 1, M, 0, D, 1, M, 0, E, 1, N, 0, F, 1, M, 0, scale, dif, WORK, 1, 0, 400, IWORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( extractMatrix( C, M, M, N ), tc.C, 1e-14, 'C' );
	assertArrayClose( extractMatrix( F, M, M, N ), tc.F, 1e-14, 'F' );
});

test( 'dtgsyl: notrans_3x2_quasi', function t() {
	var scale;
	var IWORK;
	var WORK;
	var info;
	var dif;
	var tc;
	var M;
	var N;
	var A;
	var B;
	var C;
	var D;
	var E;
	var F;

	tc = findCase( 'notrans_3x2_quasi' );
	M = 3;
	N = 2;
	A = packMatrix([ 0, 0, 1.0, 0, 1, 0.5, 0, 2, 0.3, 1, 0, -0.5, 1, 1, 1.0, 1, 2, 0.2, 2, 2, 3.0 ], M, M); // eslint-disable-line max-len
	B = packMatrix([ 0, 0, 2.0, 0, 1, 0.4, 1, 1, 5.0 ], N, N);
	C = new Float64Array([ 1.0, 3.0, 5.0, 2.0, 4.0, 6.0 ]);
	D = packMatrix([ 0, 0, 1.0, 0, 1, 0.1, 0, 2, 0.2, 1, 1, 1.5, 1, 2, 0.3, 2, 2, 2.0 ], M, M);
	E = packMatrix([ 0, 0, 1.0, 0, 1, 0.2, 1, 1, 3.0 ], N, N);
	F = new Float64Array([ 7.0, 9.0, 11.0, 8.0, 10.0, 12.0 ]);
	scale = new Float64Array( 1 );
	dif = new Float64Array( 1 );
	WORK = new Float64Array( 400 );
	IWORK = new Int32Array( M + N + 6 );
	info = dtgsyl( 'no-transpose', 0, M, N, A, 1, M, 0, B, 1, N, 0, C, 1, M, 0, D, 1, M, 0, E, 1, N, 0, F, 1, M, 0, scale, dif, WORK, 1, 0, 400, IWORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( extractMatrix( C, M, M, N ), tc.C, 1e-14, 'C' );
	assertArrayClose( extractMatrix( F, M, M, N ), tc.F, 1e-14, 'F' );
});

test( 'dtgsyl: trans_2x2', function t() {
	var scale;
	var IWORK;
	var WORK;
	var info;
	var dif;
	var tc;
	var M;
	var N;
	var A;
	var B;
	var C;
	var D;
	var E;
	var F;

	tc = findCase( 'trans_2x2' );
	M = 2;
	N = 2;
	A = packMatrix([ 0, 0, 1.0, 0, 1, 0.5, 1, 1, 2.0 ], M, M);
	B = packMatrix([ 0, 0, 3.0, 0, 1, 0.3, 1, 1, 4.0 ], N, N);
	C = new Float64Array([ 1.0, 3.0, 2.0, 4.0 ]);
	D = packMatrix([ 0, 0, 1.0, 0, 1, 0.2, 1, 1, 1.5 ], M, M);
	E = packMatrix([ 0, 0, 1.0, 0, 1, 0.1, 1, 1, 2.0 ], N, N);
	F = new Float64Array([ 5.0, 7.0, 6.0, 8.0 ]);
	scale = new Float64Array( 1 );
	dif = new Float64Array( 1 );
	WORK = new Float64Array( 400 );
	IWORK = new Int32Array( M + N + 6 );
	info = dtgsyl( 'transpose', 0, M, N, A, 1, M, 0, B, 1, N, 0, C, 1, M, 0, D, 1, M, 0, E, 1, N, 0, F, 1, M, 0, scale, dif, WORK, 1, 0, 400, IWORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( extractMatrix( C, M, M, N ), tc.C, 1e-14, 'C' );
	assertArrayClose( extractMatrix( F, M, M, N ), tc.F, 1e-14, 'F' );
});

test( 'dtgsyl: trans_3x2_quasi', function t() {
	var scale;
	var IWORK;
	var WORK;
	var info;
	var dif;
	var tc;
	var M;
	var N;
	var A;
	var B;
	var C;
	var D;
	var E;
	var F;

	tc = findCase( 'trans_3x2_quasi' );
	M = 3;
	N = 2;
	A = packMatrix([ 0, 0, 1.0, 0, 1, 0.5, 0, 2, 0.3, 1, 0, -0.5, 1, 1, 1.0, 1, 2, 0.2, 2, 2, 3.0 ], M, M); // eslint-disable-line max-len
	B = packMatrix([ 0, 0, 2.0, 0, 1, 0.4, 1, 1, 5.0 ], N, N);
	C = new Float64Array([ 1.0, 3.0, 5.0, 2.0, 4.0, 6.0 ]);
	D = packMatrix([ 0, 0, 1.0, 0, 1, 0.1, 0, 2, 0.2, 1, 1, 1.5, 1, 2, 0.3, 2, 2, 2.0 ], M, M);
	E = packMatrix([ 0, 0, 1.0, 0, 1, 0.2, 1, 1, 3.0 ], N, N);
	F = new Float64Array([ 7.0, 9.0, 11.0, 8.0, 10.0, 12.0 ]);
	scale = new Float64Array( 1 );
	dif = new Float64Array( 1 );
	WORK = new Float64Array( 400 );
	IWORK = new Int32Array( M + N + 6 );
	info = dtgsyl( 'transpose', 0, M, N, A, 1, M, 0, B, 1, N, 0, C, 1, M, 0, D, 1, M, 0, E, 1, N, 0, F, 1, M, 0, scale, dif, WORK, 1, 0, 400, IWORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( extractMatrix( C, M, M, N ), tc.C, 1e-14, 'C' );
	assertArrayClose( extractMatrix( F, M, M, N ), tc.F, 1e-14, 'F' );
});

test( 'dtgsyl: m0_n0 (quick return)', function t() {
	var scale;
	var IWORK;
	var WORK;
	var info;
	var dif;
	var A;
	var B;
	var C;
	var D;
	var E;
	var F;

	A = new Float64Array( 1 );
	B = new Float64Array( 1 );
	C = new Float64Array( 1 );
	D = new Float64Array( 1 );
	E = new Float64Array( 1 );
	F = new Float64Array( 1 );
	scale = new Float64Array( 1 );
	dif = new Float64Array( 1 );
	WORK = new Float64Array( 1 );
	IWORK = new Int32Array( 10 );
	info = dtgsyl( 'no-transpose', 0, 0, 0, A, 1, 1, 0, B, 1, 1, 0, C, 1, 1, 0, D, 1, 1, 0, E, 1, 1, 0, F, 1, 1, 0, scale, dif, WORK, 1, 0, 1, IWORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, 0, 'info' );
	assert.equal( scale[ 0 ], 1.0, 'scale' );
});

test( 'dtgsyl: trans_3x3_quasi', function t() {
	var scale;
	var IWORK;
	var WORK;
	var info;
	var dif;
	var tc;
	var M;
	var N;
	var A;
	var B;
	var C;
	var D;
	var E;
	var F;

	tc = findCase( 'trans_3x3_quasi' );
	M = 3;
	N = 3;
	A = packMatrix([ 0, 0, 1.0, 0, 1, 0.4, 0, 2, 0.1, 1, 0, -0.4, 1, 1, 1.0, 1, 2, 0.2, 2, 2, 5.0 ], M, M); // eslint-disable-line max-len
	B = packMatrix([ 0, 0, 2.0, 0, 1, 0.3, 0, 2, 0.1, 1, 0, -0.3, 1, 1, 2.0, 1, 2, 0.2, 2, 2, 6.0 ], N, N); // eslint-disable-line max-len
	C = new Float64Array([ 1.0, 4.0, 7.0, 2.0, 5.0, 8.0, 3.0, 6.0, 9.0 ]);
	D = packMatrix([ 0, 0, 1.0, 0, 1, 0.1, 0, 2, 0.05, 1, 1, 1.5, 1, 2, 0.2, 2, 2, 2.0 ], M, M); // eslint-disable-line max-len
	E = packMatrix([ 0, 0, 1.0, 0, 1, 0.2, 0, 2, 0.1, 1, 1, 2.5, 1, 2, 0.15, 2, 2, 3.0 ], N, N); // eslint-disable-line max-len
	F = new Float64Array([ 10.0, 13.0, 16.0, 11.0, 14.0, 17.0, 12.0, 15.0, 18.0 ]);
	scale = new Float64Array( 1 );
	dif = new Float64Array( 1 );
	WORK = new Float64Array( 400 );
	IWORK = new Int32Array( M + N + 6 );
	info = dtgsyl( 'transpose', 0, M, N, A, 1, M, 0, B, 1, N, 0, C, 1, M, 0, D, 1, M, 0, E, 1, N, 0, F, 1, M, 0, scale, dif, WORK, 1, 0, 400, IWORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( extractMatrix( C, M, M, N ), tc.C, 1e-14, 'C' );
	assertArrayClose( extractMatrix( F, M, M, N ), tc.F, 1e-14, 'F' );
});

test( 'dtgsyl: notrans_3x3_quasi', function t() {
	var scale;
	var IWORK;
	var WORK;
	var info;
	var dif;
	var tc;
	var M;
	var N;
	var A;
	var B;
	var C;
	var D;
	var E;
	var F;

	tc = findCase( 'notrans_3x3_quasi' );
	M = 3;
	N = 3;
	A = packMatrix([ 0, 0, 1.0, 0, 1, 0.4, 0, 2, 0.1, 1, 0, -0.4, 1, 1, 1.0, 1, 2, 0.2, 2, 2, 5.0 ], M, M); // eslint-disable-line max-len
	B = packMatrix([ 0, 0, 2.0, 0, 1, 0.3, 0, 2, 0.1, 1, 0, -0.3, 1, 1, 2.0, 1, 2, 0.2, 2, 2, 6.0 ], N, N); // eslint-disable-line max-len
	C = new Float64Array([ 1.0, 4.0, 7.0, 2.0, 5.0, 8.0, 3.0, 6.0, 9.0 ]);
	D = packMatrix([ 0, 0, 1.0, 0, 1, 0.1, 0, 2, 0.05, 1, 1, 1.5, 1, 2, 0.2, 2, 2, 2.0 ], M, M); // eslint-disable-line max-len
	E = packMatrix([ 0, 0, 1.0, 0, 1, 0.2, 0, 2, 0.1, 1, 1, 2.5, 1, 2, 0.15, 2, 2, 3.0 ], N, N); // eslint-disable-line max-len
	F = new Float64Array([ 10.0, 13.0, 16.0, 11.0, 14.0, 17.0, 12.0, 15.0, 18.0 ]);
	scale = new Float64Array( 1 );
	dif = new Float64Array( 1 );
	WORK = new Float64Array( 400 );
	IWORK = new Int32Array( M + N + 6 );
	info = dtgsyl( 'no-transpose', 0, M, N, A, 1, M, 0, B, 1, N, 0, C, 1, M, 0, D, 1, M, 0, E, 1, N, 0, F, 1, M, 0, scale, dif, WORK, 1, 0, 400, IWORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( scale[ 0 ], tc.scale, 1e-14, 'scale' );
	assertArrayClose( extractMatrix( C, M, M, N ), tc.C, 1e-14, 'C' );
	assertArrayClose( extractMatrix( F, M, M, N ), tc.F, 1e-14, 'F' );
});
