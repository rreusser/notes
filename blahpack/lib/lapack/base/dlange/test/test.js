/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';


// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlange = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dlange.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
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
* Creates a column-major matrix from a row-major 2D array.
*
* @private
* @param {Array<Array<number>>} rows - row-major data
* @returns {Float64Array} column-major flat array
*/
function colMajor( rows ) {
	var M = rows.length;
	var N = rows[ 0 ].length;
	var A = new Float64Array( M * N );
	var i;
	var j;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			A[ i + j * M ] = rows[ i ][ j ];
		}
	}
	return A;
}


// TESTS //

// 3x4 matrix:
// A = [  1.0  -4.0   7.0  -2.0 ]
//     [ -3.0   5.0  -8.0   6.0 ]
//     [  2.0  -1.0   9.0  -3.0 ]
var A3x4 = colMajor([
	[ 1.0, -4.0, 7.0, -2.0 ],
	[-3.0, 5.0, -8.0, 6.0 ],
	[ 2.0, -1.0, 9.0, -3.0 ]
]);

// 4x5 matrix:
var A4x5 = colMajor([
	[ 2.0, 4.0, -7.0, 1.0, 0.0 ],
	[-1.0, -6.0, 2.0, 0.0, 9.0 ],
	[ 0.0, 1.0, 8.0, -3.0, -2.0 ],
	[ 3.0, 0.5, -4.0, 5.0, 1.0 ]
]);

// 1x1 matrix:
var A1x1 = new Float64Array([ -5.5 ]);

test( 'dlange: max norm (M) on 3x4 matrix', function t() {
	var result;
	var work;
	var tc;

	tc = findCase( 'dlange_max' );
	work = new Float64Array( 10 );
	result = dlange( 'max', 3, 4, A3x4, 1, 3, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlange: one norm (1) on 3x4 matrix', function t() {
	var result;
	var work;
	var tc;

	tc = findCase( 'dlange_one' );
	work = new Float64Array( 10 );
	result = dlange( 'one-norm', 3, 4, A3x4, 1, 3, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlange: one norm (O) on 3x4 matrix', function t() {
	var result;
	var work;
	var tc;

	tc = findCase( 'dlange_one_O' );
	work = new Float64Array( 10 );
	result = dlange( 'one-norm', 3, 4, A3x4, 1, 3, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlange: infinity norm (I) on 3x4 matrix', function t() {
	var result;
	var work;
	var tc;

	tc = findCase( 'dlange_inf' );
	work = new Float64Array( 10 );
	result = dlange( 'inf-norm', 3, 4, A3x4, 1, 3, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlange: Frobenius norm (F) on 3x4 matrix', function t() {
	var result;
	var work;
	var tc;

	tc = findCase( 'dlange_frob' );
	work = new Float64Array( 10 );
	result = dlange( 'frobenius', 3, 4, A3x4, 1, 3, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlange: Frobenius norm (E) on 3x4 matrix', function t() {
	var result;
	var work;
	var tc;

	tc = findCase( 'dlange_frob_E' );
	work = new Float64Array( 10 );
	result = dlange( 'frobenius', 3, 4, A3x4, 1, 3, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlange: M=0 quick return', function t() {
	var result;
	var work;
	var tc;

	tc = findCase( 'dlange_m_zero' );
	work = new Float64Array( 10 );
	result = dlange( 'max', 0, 4, A3x4, 1, 3, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlange: N=0 quick return', function t() {
	var result;
	var work;
	var tc;

	tc = findCase( 'dlange_n_zero' );
	work = new Float64Array( 10 );
	result = dlange( 'one-norm', 3, 0, A3x4, 1, 3, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlange: 1x1 max norm', function t() {
	var result;
	var work;
	var tc;

	tc = findCase( 'dlange_1x1_max' );
	work = new Float64Array( 10 );
	result = dlange( 'max', 1, 1, A1x1, 1, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlange: 1x1 Frobenius norm', function t() {
	var result;
	var work;
	var tc;

	tc = findCase( 'dlange_1x1_frob' );
	work = new Float64Array( 10 );
	result = dlange( 'frobenius', 1, 1, A1x1, 1, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlange: 1x1 one norm', function t() {
	var result;
	var work;
	var tc;

	tc = findCase( 'dlange_1x1_one' );
	work = new Float64Array( 10 );
	result = dlange( 'one-norm', 1, 1, A1x1, 1, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlange: 1x1 infinity norm', function t() {
	var result;
	var work;
	var tc;

	tc = findCase( 'dlange_1x1_inf' );
	work = new Float64Array( 10 );
	result = dlange( 'inf-norm', 1, 1, A1x1, 1, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlange: 4x5 max norm', function t() {
	var result;
	var work;
	var tc;

	tc = findCase( 'dlange_4x5_max' );
	work = new Float64Array( 10 );
	result = dlange( 'max', 4, 5, A4x5, 1, 4, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlange: 4x5 one norm', function t() {
	var result;
	var work;
	var tc;

	tc = findCase( 'dlange_4x5_one' );
	work = new Float64Array( 10 );
	result = dlange( 'one-norm', 4, 5, A4x5, 1, 4, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlange: 4x5 infinity norm', function t() {
	var result;
	var work;
	var tc;

	tc = findCase( 'dlange_4x5_inf' );
	work = new Float64Array( 10 );
	result = dlange( 'inf-norm', 4, 5, A4x5, 1, 4, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlange: 4x5 Frobenius norm', function t() {
	var result;
	var work;
	var tc;

	tc = findCase( 'dlange_4x5_frob' );
	work = new Float64Array( 10 );
	result = dlange( 'frobenius', 4, 5, A4x5, 1, 4, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlange: lowercase norm chars work', function t() {
	var resultM;
	var resultO;
	var resultI;
	var resultF;
	var resultE;
	var work;

	work = new Float64Array( 10 );
	resultM = dlange( 'max', 3, 4, A3x4, 1, 3, 0, work, 1, 0 );
	resultO = dlange( 'one-norm', 3, 4, A3x4, 1, 3, 0, work, 1, 0 );
	resultI = dlange( 'inf-norm', 3, 4, A3x4, 1, 3, 0, work, 1, 0 );
	resultF = dlange( 'frobenius', 3, 4, A3x4, 1, 3, 0, work, 1, 0 );
	resultE = dlange( 'frobenius', 3, 4, A3x4, 1, 3, 0, work, 1, 0 );
	assertClose( resultM, 9.0, 1e-14, 'lowercase m' );
	assertClose( resultO, 24.0, 1e-14, 'lowercase o' );
	assertClose( resultI, 22.0, 1e-14, 'lowercase i' );
	assertClose( resultF, Math.sqrt( 299 ), 1e-14, 'lowercase f' );
	assertClose( resultE, Math.sqrt( 299 ), 1e-14, 'lowercase e' );
});

test( 'dlange: non-unit strides with offset', function t() {
	var offset;
	var result;
	var rows;
	var work;
	var sa1;
	var sa2;
	var A;
	var i;
	var j;

	A = new Float64Array( 50 );
	offset = 3;
	sa1 = 2;
	sa2 = 8;
	rows = [
		[ 1.0, -4.0, 7.0, -2.0 ],
		[-3.0, 5.0, -8.0, 6.0 ],
		[ 2.0, -1.0, 9.0, -3.0 ]
	];
	for ( j = 0; j < 4; j++ ) {
		for ( i = 0; i < 3; i++ ) {
			A[ offset + i * sa1 + j * sa2 ] = rows[ i ][ j ];
		}
	}
	work = new Float64Array( 10 );
	result = dlange( 'max', 3, 4, A, sa1, sa2, offset, work, 1, 0 );
	assertClose( result, 9.0, 1e-14, 'max norm with strides' );
	result = dlange( 'one-norm', 3, 4, A, sa1, sa2, offset, work, 1, 0 );
	assertClose( result, 24.0, 1e-14, 'one norm with strides' );
	result = dlange( 'inf-norm', 3, 4, A, sa1, sa2, offset, work, 1, 0 );
	assertClose( result, 22.0, 1e-14, 'inf norm with strides' );
	result = dlange( 'frobenius', 3, 4, A, sa1, sa2, offset, work, 1, 0 );
	assertClose( result, Math.sqrt( 299 ), 1e-14, 'frob norm with strides' );
});

test( 'dlange: work array with offset for infinity norm', function t() {
	var workOffset;
	var result;
	var work;

	work = new Float64Array( 20 );
	workOffset = 5;
	result = dlange( 'inf-norm', 3, 4, A3x4, 1, 3, 0, work, 1, workOffset );
	assertClose( result, 22.0, 1e-14, 'inf norm with work offset' );
});

test( 'dlange: unrecognized norm returns 0', function t() {
	var result;
	var work;

	work = new Float64Array( 10 );
	result = dlange( 'X', 3, 4, A3x4, 1, 3, 0, work, 1, 0 );
	assert.equal( result, 0.0 );
});
