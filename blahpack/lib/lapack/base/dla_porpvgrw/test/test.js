/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';


// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dla_porpvgrw = require( './../lib/base.js' );
var ndarray = require( './../lib/ndarray.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dla_porpvgrw.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
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
* Creates a column-major 2D matrix (NxN) as a Float64Array from a flat column-major list.
*
* @private
* @param {Array} vals - flat column-major values
* @returns {Float64Array} matrix
*/
function mat( vals ) {
	return new Float64Array( vals );
}

/**
* Converts a typed array to a plain array.
*
* @private
* @param {TypedArray} arr - input array
* @returns {Array} output array
*/
function toArray( arr ) {
	var out = [];
	var i;
	for ( i = 0; i < arr.length; i++ ) {
		out.push( arr[ i ] );
	}
	return out;
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
		ndarray( 'foo', 1, new Float64Array( 1 ), 1, 1, 0, new Float64Array( 1 ), 1, 1, 0, new Float64Array( 2 ), 1, 0 ); // eslint-disable-line max-len
	}, /TypeError/ );
});

test( 'dla_porpvgrw: upper_3x3', function t() {
	var result;
	var ncols;
	var WORK;
	var lda;
	var tc;
	var AF;
	var A;

	tc = findCase( 'upper_3x3' );
	ncols = 3;
	lda = 4;
	A = mat([
		4.0,
		0.0,
		0.0,
		0.0,  // col 1
		2.0,
		5.0,
		0.0,
		0.0,  // col 2
		-2.0,
		1.0,
		14.0,
		0.0 // col 3
	]);
	AF = mat([
		2.0,
		0.0,
		0.0,
		0.0,
		1.0,
		2.0,
		0.0,
		0.0,
		-1.0,
		1.0,
		3.46410161513775439,
		0.0
	]);
	WORK = new Float64Array( 2 * ncols );
	result = dla_porpvgrw( 'upper', ncols, A, 1, lda, 0, AF, 1, lda, 0, WORK, 1, 0 ); // eslint-disable-line max-len
	assertClose( result, tc.result, 1e-14, 'result' );
	assertArrayClose( toArray( WORK ), tc.work, 1e-14, 'work' );
});

test( 'dla_porpvgrw: lower_3x3', function t() {
	var result;
	var ncols;
	var WORK;
	var lda;
	var tc;
	var AF;
	var A;

	tc = findCase( 'lower_3x3' );
	ncols = 3;
	lda = 4;
	A = mat([
		4.0,
		2.0,
		-2.0,
		0.0,
		0.0,
		5.0,
		1.0,
		0.0,
		0.0,
		0.0,
		14.0,
		0.0
	]);
	AF = mat([
		2.0,
		1.0,
		-1.0,
		0.0,
		0.0,
		2.0,
		1.0,
		0.0,
		0.0,
		0.0,
		3.46410161513775439,
		0.0
	]);
	WORK = new Float64Array( 2 * ncols );
	result = dla_porpvgrw( 'lower', ncols, A, 1, lda, 0, AF, 1, lda, 0, WORK, 1, 0 ); // eslint-disable-line max-len
	assertClose( result, tc.result, 1e-14, 'result' );
	assertArrayClose( toArray( WORK ), tc.work, 1e-14, 'work' );
});

test( 'dla_porpvgrw: ncols_0', function t() {
	var result;
	var WORK;
	var tc;
	var AF;
	var A;

	tc = findCase( 'ncols_0' );
	A = new Float64Array( 1 );
	AF = new Float64Array( 1 );
	WORK = new Float64Array( 0 );
	result = dla_porpvgrw( 'upper', 0, A, 1, 1, 0, AF, 1, 1, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dla_porpvgrw: ncols_1_upper', function t() {
	var result;
	var WORK;
	var tc;
	var AF;
	var A;

	tc = findCase( 'ncols_1_upper' );
	A = mat([ 9.0 ]);
	AF = mat([ 3.0 ]);
	WORK = new Float64Array( 2 );
	result = dla_porpvgrw( 'upper', 1, A, 1, 1, 0, AF, 1, 1, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
	assertArrayClose( toArray( WORK ), tc.work, 1e-14, 'work' );
});

test( 'dla_porpvgrw: ncols_1_lower', function t() {
	var result;
	var WORK;
	var tc;
	var AF;
	var A;

	tc = findCase( 'ncols_1_lower' );
	A = mat([ 9.0 ]);
	AF = mat([ 3.0 ]);
	WORK = new Float64Array( 2 );
	result = dla_porpvgrw( 'lower', 1, A, 1, 1, 0, AF, 1, 1, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
	assertArrayClose( toArray( WORK ), tc.work, 1e-14, 'work' );
});

test( 'dla_porpvgrw: upper_4x4', function t() {
	var result;
	var ncols;
	var WORK;
	var lda;
	var s10;
	var tc;
	var AF;
	var A;

	tc = findCase( 'upper_4x4' );
	ncols = 4;
	lda = 4;
	A = mat([
		10.0,
		0.0,
		0.0,
		0.0,
		1.0,
		10.0,
		0.0,
		0.0,
		2.0,
		1.0,
		10.0,
		0.0,
		0.5,
		1.5,
		2.0,
		10.0
	]);
	s10 = Math.sqrt( 10.0 );
	AF = new Float64Array( 16 );
	AF[ 0 ] = s10;
	AF[ 4 ] = 1.0 / s10;
	AF[ 5 ] = Math.sqrt( 10.0 - ( 1.0 / s10 ) * ( 1.0 / s10 ) );
	AF[ 8 ] = 2.0 / s10;
	AF[ 9 ] = ( 1.0 - ( 1.0 / s10 ) * ( 2.0 / s10 ) ) / AF[ 5 ];
	AF[ 10 ] = Math.sqrt( 10.0 - ( 2.0 / s10 ) * ( 2.0 / s10 ) - AF[ 9 ] * AF[ 9 ] ); // eslint-disable-line max-len
	AF[ 12 ] = 0.5 / s10;
	AF[ 13 ] = ( 1.5 - AF[ 4 ] * AF[ 12 ] ) / AF[ 5 ];
	AF[ 14 ] = ( 2.0 - AF[ 8 ] * AF[ 12 ] - AF[ 9 ] * AF[ 13 ] ) / AF[ 10 ];
	AF[ 15 ] = Math.sqrt( 10.0 - AF[ 12 ] * AF[ 12 ] - AF[ 13 ] * AF[ 13 ] - AF[ 14 ] * AF[ 14 ] ); // eslint-disable-line max-len
	WORK = new Float64Array( 2 * ncols );
	result = dla_porpvgrw( 'upper', ncols, A, 1, lda, 0, AF, 1, lda, 0, WORK, 1, 0 ); // eslint-disable-line max-len
	assertClose( result, tc.result, 1e-14, 'result' );
	assertArrayClose( toArray( WORK ), tc.work, 1e-14, 'work' );
});

test( 'dla_porpvgrw: lower_4x4', function t() {
	var result;
	var ncols;
	var WORK;
	var lda;
	var s10;
	var l22;
	var l33;
	var tc;
	var AF;
	var A;

	tc = findCase( 'lower_4x4' );
	ncols = 4;
	lda = 4;
	A = mat([
		10.0,
		1.0,
		2.0,
		0.5,
		0.0,
		10.0,
		1.0,
		1.5,
		0.0,
		0.0,
		10.0,
		2.0,
		0.0,
		0.0,
		0.0,
		10.0
	]);
	s10 = Math.sqrt( 10.0 );
	AF = new Float64Array( 16 );
	AF[ 0 ] = s10;
	AF[ 1 ] = 1.0 / s10;
	AF[ 2 ] = 2.0 / s10;
	AF[ 3 ] = 0.5 / s10;
	l22 = Math.sqrt( 10.0 - AF[ 1 ] * AF[ 1 ] );
	AF[ 5 ] = l22;
	AF[ 6 ] = ( 1.0 - AF[ 1 ] * AF[ 2 ] ) / l22;
	AF[ 7 ] = ( 1.5 - AF[ 1 ] * AF[ 3 ] ) / l22;
	l33 = Math.sqrt( 10.0 - AF[ 2 ] * AF[ 2 ] - AF[ 6 ] * AF[ 6 ] );
	AF[ 10 ] = l33;
	AF[ 11 ] = ( 2.0 - AF[ 2 ] * AF[ 3 ] - AF[ 6 ] * AF[ 7 ] ) / l33;
	AF[ 15 ] = Math.sqrt( 10.0 - AF[ 3 ] * AF[ 3 ] - AF[ 7 ] * AF[ 7 ] - AF[ 11 ] * AF[ 11 ] ); // eslint-disable-line max-len
	WORK = new Float64Array( 2 * ncols );
	result = dla_porpvgrw( 'lower', ncols, A, 1, lda, 0, AF, 1, lda, 0, WORK, 1, 0 ); // eslint-disable-line max-len
	assertClose( result, tc.result, 1e-14, 'result' );
	assertArrayClose( toArray( WORK ), tc.work, 1e-14, 'work' );
});

test( 'dla_porpvgrw: upper_zero_col', function t() {
	var result;
	var ncols;
	var WORK;
	var lda;
	var tc;
	var AF;
	var A;

	tc = findCase( 'upper_zero_col' );
	ncols = 3;
	lda = 4;
	A = mat([
		4.0,
		0.0,
		0.0,
		0.0,
		2.0,
		5.0,
		0.0,
		0.0,
		1.0,
		3.0,
		7.0,
		0.0
	]);
	AF = mat([
		2.0,
		0.0,
		0.0,
		0.0,
		1.0,
		0.0,
		0.0,
		0.0,
		0.5,
		0.0,
		2.5,
		0.0
	]);
	WORK = new Float64Array( 2 * ncols );
	result = dla_porpvgrw( 'upper', ncols, A, 1, lda, 0, AF, 1, lda, 0, WORK, 1, 0 ); // eslint-disable-line max-len
	assertClose( result, tc.result, 1e-14, 'result' );
	assertArrayClose( toArray( WORK ), tc.work, 1e-14, 'work' );
});

test( 'dla_porpvgrw: upper_rpvgrw_lt1', function t() {
	var result;
	var ncols;
	var WORK;
	var lda;
	var tc;
	var AF;
	var A;

	tc = findCase( 'upper_rpvgrw_lt1' );
	ncols = 3;
	lda = 4;
	A = mat([
		2.0,
		0.0,
		0.0,
		0.0,
		1.0,
		3.0,
		0.0,
		0.0,
		0.5,
		1.0,
		4.0,
		0.0
	]);
	AF = mat([
		4.0,
		0.0,
		0.0,
		0.0,
		2.0,
		6.0,
		0.0,
		0.0,
		1.0,
		3.0,
		8.0,
		0.0
	]);
	WORK = new Float64Array( 2 * ncols );
	result = dla_porpvgrw( 'upper', ncols, A, 1, lda, 0, AF, 1, lda, 0, WORK, 1, 0 ); // eslint-disable-line max-len
	assertClose( result, tc.result, 1e-14, 'result' );
	assertArrayClose( toArray( WORK ), tc.work, 1e-14, 'work' );
});

test( 'dla_porpvgrw: lower_rpvgrw_lt1', function t() {
	var result;
	var ncols;
	var WORK;
	var lda;
	var tc;
	var AF;
	var A;

	tc = findCase( 'lower_rpvgrw_lt1' );
	ncols = 3;
	lda = 4;
	A = mat([
		2.0,
		1.0,
		0.5,
		0.0,
		0.0,
		3.0,
		1.0,
		0.0,
		0.0,
		0.0,
		4.0,
		0.0
	]);
	AF = mat([
		4.0,
		2.0,
		1.0,
		0.0,
		0.0,
		6.0,
		3.0,
		0.0,
		0.0,
		0.0,
		8.0,
		0.0
	]);
	WORK = new Float64Array( 2 * ncols );
	result = dla_porpvgrw( 'lower', ncols, A, 1, lda, 0, AF, 1, lda, 0, WORK, 1, 0 ); // eslint-disable-line max-len
	assertClose( result, tc.result, 1e-14, 'result' );
	assertArrayClose( toArray( WORK ), tc.work, 1e-14, 'work' );
});

test( 'dla_porpvgrw: lower_zero_col', function t() {
	var result;
	var ncols;
	var WORK;
	var lda;
	var tc;
	var AF;
	var A;

	tc = findCase( 'lower_zero_col' );
	ncols = 3;
	lda = 4;
	A = mat([
		4.0,
		2.0,
		1.0,
		0.0,
		0.0,
		5.0,
		3.0,
		0.0,
		0.0,
		0.0,
		7.0,
		0.0
	]);
	AF = mat([
		2.0,
		1.0,
		0.5,
		0.0,
		0.0,
		0.0,
		0.0,
		0.0,
		0.0,
		0.0,
		2.5,
		0.0
	]);
	WORK = new Float64Array( 2 * ncols );
	result = dla_porpvgrw( 'lower', ncols, A, 1, lda, 0, AF, 1, lda, 0, WORK, 1, 0 ); // eslint-disable-line max-len
	assertClose( result, tc.result, 1e-14, 'result' );
	assertArrayClose( toArray( WORK ), tc.work, 1e-14, 'work' );
});
