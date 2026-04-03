/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

/**
* @license Apache-2.0
*
* Copyright (c) 2025 The Stdlib Authors.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*    http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/

'use strict';

// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlange = require( './../lib/base.js' );


// VARIABLES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dlange.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync, max-len
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
	return fixture.find( function find( t ) {
		return t.name === name;
	} );
}

/**
* Asserts that two numbers are approximately equal.
*
* @private
* @param {number} actual - actual value
* @param {number} expected - expected value
* @param {number} tol - tolerance
* @param {string} msg - assertion message
*/
function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 ); // eslint-disable-line max-len
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual ); // eslint-disable-line max-len
}

/**
* Creates a column-major matrix from a row-major 2D array.
*
* @private
* @param {Array<Array<number>>} rows - row-major data
* @returns {Float64Array} column-major flat array
*/
function colMajor( rows ) {
	var out;
	var M;
	var N;
	var i;
	var j;

	M = rows.length;
	N = rows[ 0 ].length;
	out = new Float64Array( M * N );
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			out[ i + (j * M) ] = rows[ i ][ j ];
		}
	}
	return out;
}


// TESTS //

test( 'dlange is a function', function t() {
	assert.strictEqual( typeof dlange, 'function' );
});

test( 'dlange: max norm on 3x4 matrix', function t() {
	var result;
	var work;
	var A;

	// 3x4 matrix:

	// A = [  1.0  -4.0   7.0  -2.0 ]

	//     [ -3.0   5.0  -8.0   6.0 ]

	//     [  2.0  -1.0   9.0  -3.0 ]
	A = colMajor([
		[ 1.0, -4.0, 7.0, -2.0 ],
		[ -3.0, 5.0, -8.0, 6.0 ],
		[ 2.0, -1.0, 9.0, -3.0 ]
	]);
	work = new Float64Array( 10 );
	result = dlange( 'max', 3, 4, A, 1, 3, 0, work, 1, 0 );
	assertClose( result, findCase( 'dlange_max' ).result, 1e-14, 'max norm' );
});

test( 'dlange: one norm on 3x4 matrix', function t() {
	var result;
	var work;
	var A;

	A = colMajor([
		[ 1.0, -4.0, 7.0, -2.0 ],
		[ -3.0, 5.0, -8.0, 6.0 ],
		[ 2.0, -1.0, 9.0, -3.0 ]
	]);
	work = new Float64Array( 10 );
	result = dlange( 'one-norm', 3, 4, A, 1, 3, 0, work, 1, 0 );
	assertClose( result, findCase( 'dlange_one' ).result, 1e-14, 'one norm' );
});

test( 'dlange: infinity norm on 3x4 matrix', function t() {
	var result;
	var work;
	var A;

	A = colMajor([
		[ 1.0, -4.0, 7.0, -2.0 ],
		[ -3.0, 5.0, -8.0, 6.0 ],
		[ 2.0, -1.0, 9.0, -3.0 ]
	]);
	work = new Float64Array( 10 );
	result = dlange( 'inf-norm', 3, 4, A, 1, 3, 0, work, 1, 0 );
	assertClose( result, findCase( 'dlange_inf' ).result, 1e-14, 'inf norm' );
});

test( 'dlange: Frobenius norm on 3x4 matrix', function t() {
	var result;
	var work;
	var A;

	A = colMajor([
		[ 1.0, -4.0, 7.0, -2.0 ],
		[ -3.0, 5.0, -8.0, 6.0 ],
		[ 2.0, -1.0, 9.0, -3.0 ]
	]);
	work = new Float64Array( 10 );
	result = dlange( 'frobenius', 3, 4, A, 1, 3, 0, work, 1, 0 );
	assertClose( result, findCase( 'dlange_frob' ).result, 1e-14, 'frobenius' );
});

test( 'dlange: M=0 quick return', function t() {
	var result;
	var work;
	var A;

	A = new Float64Array( 12 );
	work = new Float64Array( 10 );
	result = dlange( 'max', 0, 4, A, 1, 3, 0, work, 1, 0 );
	assert.strictEqual( result, 0.0, 'M=0 returns 0' );
});

test( 'dlange: N=0 quick return', function t() {
	var result;
	var work;
	var A;

	A = new Float64Array( 12 );
	work = new Float64Array( 10 );
	result = dlange( 'one-norm', 3, 0, A, 1, 3, 0, work, 1, 0 );
	assert.strictEqual( result, 0.0, 'N=0 returns 0' );
});

test( 'dlange: 1x1 matrix, all norms', function t() {
	var result;
	var work;
	var A;

	A = new Float64Array( [ -5.5 ] );
	work = new Float64Array( 10 );

	result = dlange( 'max', 1, 1, A, 1, 1, 0, work, 1, 0 );
	assertClose( result, findCase( 'dlange_1x1_max' ).result, 1e-14, 'max' );

	result = dlange( 'one-norm', 1, 1, A, 1, 1, 0, work, 1, 0 );
	assertClose( result, findCase( 'dlange_1x1_one' ).result, 1e-14, 'one' );

	result = dlange( 'inf-norm', 1, 1, A, 1, 1, 0, work, 1, 0 );
	assertClose( result, findCase( 'dlange_1x1_inf' ).result, 1e-14, 'inf' );

	result = dlange( 'frobenius', 1, 1, A, 1, 1, 0, work, 1, 0 );
	assertClose( result, findCase( 'dlange_1x1_frob' ).result, 1e-14, 'frob' );
});

test( 'dlange: 4x5 matrix, all norms', function t() {
	var result;
	var work;
	var A;

	A = colMajor([
		[ 2.0, 4.0, -7.0, 1.0, 0.0 ],
		[ -1.0, -6.0, 2.0, 0.0, 9.0 ],
		[ 0.0, 1.0, 8.0, -3.0, -2.0 ],
		[ 3.0, 0.5, -4.0, 5.0, 1.0 ]
	]);
	work = new Float64Array( 10 );

	result = dlange( 'max', 4, 5, A, 1, 4, 0, work, 1, 0 );
	assertClose( result, findCase( 'dlange_4x5_max' ).result, 1e-14, 'max' );

	result = dlange( 'one-norm', 4, 5, A, 1, 4, 0, work, 1, 0 );
	assertClose( result, findCase( 'dlange_4x5_one' ).result, 1e-14, 'one' );

	result = dlange( 'inf-norm', 4, 5, A, 1, 4, 0, work, 1, 0 );
	assertClose( result, findCase( 'dlange_4x5_inf' ).result, 1e-14, 'inf' );

	result = dlange( 'frobenius', 4, 5, A, 1, 4, 0, work, 1, 0 );
	assertClose( result, findCase( 'dlange_4x5_frob' ).result, 1e-14, 'frob' );
});

test( 'dlange: all norms produce expected results', function t() {
	var result;
	var work;
	var A;

	A = colMajor([
		[ 1.0, -4.0, 7.0, -2.0 ],
		[ -3.0, 5.0, -8.0, 6.0 ],
		[ 2.0, -1.0, 9.0, -3.0 ]
	]);
	work = new Float64Array( 10 );

	result = dlange( 'max', 3, 4, A, 1, 3, 0, work, 1, 0 );
	assert.strictEqual( result, 9.0, 'max norm = 9' );

	result = dlange( 'one-norm', 3, 4, A, 1, 3, 0, work, 1, 0 );
	assert.strictEqual( result, 24.0, 'one norm = 24' );

	result = dlange( 'inf-norm', 3, 4, A, 1, 3, 0, work, 1, 0 );
	assert.strictEqual( result, 22.0, 'inf norm = 22' );

	result = dlange( 'frobenius', 3, 4, A, 1, 3, 0, work, 1, 0 );
	assertClose( result, Math.sqrt( 299 ), 1e-14, 'frobenius norm' );
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
		[ -3.0, 5.0, -8.0, 6.0 ],
		[ 2.0, -1.0, 9.0, -3.0 ]
	];
	for ( j = 0; j < 4; j++ ) {
		for ( i = 0; i < 3; i++ ) {
			A[ offset + (i * sa1) + (j * sa2) ] = rows[ i ][ j ]; // eslint-disable-line max-len
		}
	}
	work = new Float64Array( 10 );

	result = dlange( 'max', 3, 4, A, sa1, sa2, offset, work, 1, 0 );
	assert.strictEqual( result, 9.0, 'max norm with strides' );

	result = dlange( 'one-norm', 3, 4, A, sa1, sa2, offset, work, 1, 0 );
	assert.strictEqual( result, 24.0, 'one norm with strides' );

	result = dlange( 'inf-norm', 3, 4, A, sa1, sa2, offset, work, 1, 0 );
	assert.strictEqual( result, 22.0, 'inf norm with strides' );

	result = dlange( 'frobenius', 3, 4, A, sa1, sa2, offset, work, 1, 0 );
	assertClose( result, Math.sqrt( 299 ), 1e-14, 'frob norm with strides' );
});

test( 'dlange: work array with offset for infinity norm', function t() {
	var workOffset;
	var result;
	var work;
	var A;

	A = colMajor([
		[ 1.0, -4.0, 7.0, -2.0 ],
		[ -3.0, 5.0, -8.0, 6.0 ],
		[ 2.0, -1.0, 9.0, -3.0 ]
	]);
	work = new Float64Array( 20 );
	workOffset = 5;
	result = dlange( 'inf-norm', 3, 4, A, 1, 3, 0, work, 1, workOffset );
	assert.strictEqual( result, 22.0, 'inf norm with work offset' );
});

test( 'dlange: unrecognized norm returns 0', function t() {
	var result;
	var work;
	var A;

	A = colMajor([
		[ 1.0, -4.0, 7.0, -2.0 ],
		[ -3.0, 5.0, -8.0, 6.0 ],
		[ 2.0, -1.0, 9.0, -3.0 ]
	]);
	work = new Float64Array( 10 );
	result = dlange( 'X', 3, 4, A, 1, 3, 0, work, 1, 0 );
	assert.strictEqual( result, 0.0, 'unrecognized norm returns 0' );
});

test( 'dlange: NaN propagation in max norm', function t() {
	var result;
	var work;
	var A;

	A = new Float64Array( [ 1.0, NaN, 3.0, 4.0 ] );
	work = new Float64Array( 10 );
	result = dlange( 'max', 2, 2, A, 1, 2, 0, work, 1, 0 );
	assert.ok( result !== result, 'NaN propagates in max norm' );
});

test( 'dlange: NaN propagation in one norm', function t() {
	var result;
	var work;
	var A;

	A = new Float64Array( [ 1.0, NaN, 3.0, 4.0 ] );
	work = new Float64Array( 10 );
	result = dlange( 'one-norm', 2, 2, A, 1, 2, 0, work, 1, 0 );
	assert.ok( result !== result, 'NaN propagates in one norm' );
});

test( 'dlange: NaN propagation in infinity norm', function t() {
	var result;
	var work;
	var A;

	A = new Float64Array( [ 1.0, NaN, 3.0, 4.0 ] );
	work = new Float64Array( 10 );
	result = dlange( 'inf-norm', 2, 2, A, 1, 2, 0, work, 1, 0 );
	assert.ok( result !== result, 'NaN propagates in inf norm' );
});
