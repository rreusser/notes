/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';


// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dsytrd = require( '../../dsytrd/lib/base.js' );
var dormtr = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dormtr.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
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
* Asserts that two arrays are element-wise approximately equal.
*
* @private
* @param {*} actual - actual value
* @param {*} expected - expected value
* @param {number} tol - tolerance
* @param {string} msg - assertion message
*/
function assertArrayClose( actual, expected, tol, msg ) {
	var relErr;
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		relErr = Math.abs( actual[ i ] - expected[ i ] ) / Math.max( Math.abs( expected[ i ] ), 1.0 ); // eslint-disable-line max-len
		assert.ok( relErr <= tol, msg + '[' + i + ']: expected ' + expected[ i ] + ', got ' + actual[ i ] ); // eslint-disable-line max-len
	}
}

/**
* Setup: symmetric 4x4 matrix.
*   [ 4  1 -2  2 ]
*   [ 1  2  0  1 ]
*   [-2  0  3 -2 ]
*   [ 2  1 -2 -1 ]
*/
function symMatrix4() {
	// Column-major: col0=[4,1,-2,2], col1=[1,2,0,1], col2=[-2,0,3,-2], col3=[2,1,-2,-1] // eslint-disable-line max-len
	return new Float64Array([
		4,
		1,
		-2,
		2,
		1,
		2,
		0,
		1,
		-2,
		0,
		3,
		-2,
		2,
		1,
		-2,
		-1
	]);
}

/**
* Identity4.
*
* @private
* @returns {*} result
*/
function identity4( ) {
	return new Float64Array([
		1,
		0,
		0,
		0,
		0,
		1,
		0,
		0,
		0,
		0,
		1,
		0,
		0,
		0,
		0,
		1
	]);
}

/**
* ReduceUpper.
*
* @private
* @returns {*} result
*/
function reduceUpper() {
	var TAU = new Float64Array( 4 );
	var A = symMatrix4();
	var D = new Float64Array( 4 );
	var E = new Float64Array( 4 );
	dsytrd( 'upper', 4, A, 1, 4, 0, D, 1, 0, E, 1, 0, TAU, 1, 0 );
	return {
		'A': A,
		'TAU': TAU
	};
}

/**
* ReduceLower.
*
* @private
* @returns {*} result
*/
function reduceLower() {
	var TAU = new Float64Array( 4 );
	var A = symMatrix4();
	var D = new Float64Array( 4 );
	var E = new Float64Array( 4 );
	dsytrd( 'lower', 4, A, 1, 4, 0, D, 1, 0, E, 1, 0, TAU, 1, 0 );
	return {
		'A': A,
		'TAU': TAU
	};
}

/**
* FlattenColMajor.
*
* @private
* @param {*} C - C
* @param {*} M - M
* @param {*} N - N
* @returns {*} result
*/
function flattenColMajor( C, M, N ) {
	var out = [];
	var i;
	var j;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			out.push( C[ i + j * M ] );
		}
	}
	return out;
}


// TESTS //

test( 'dormtr: left_notrans_upper', function t() {
	var WORK;
	var info;
	var tc;
	var r;
	var C;

	tc = findCase( 'left_notrans_upper' );
	r = reduceUpper();
	C = identity4();
	WORK = new Float64Array( 256 );
	info = dormtr( 'left', 'upper', 'no-transpose', 4, 4, r.A, 1, 4, 0, r.TAU, 1, 0, C, 1, 4, 0, WORK, 1, 0, 256 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
	assertArrayClose( flattenColMajor( C, 4, 4 ), tc.C, 1e-14, 'C' );
});

test( 'dormtr: left_trans_upper', function t() {
	var WORK;
	var info;
	var tc;
	var r;
	var C;

	tc = findCase( 'left_trans_upper' );
	r = reduceUpper();
	C = identity4();
	WORK = new Float64Array( 256 );
	info = dormtr( 'left', 'upper', 'transpose', 4, 4, r.A, 1, 4, 0, r.TAU, 1, 0, C, 1, 4, 0, WORK, 1, 0, 256 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
	assertArrayClose( flattenColMajor( C, 4, 4 ), tc.C, 1e-14, 'C' );
});

test( 'dormtr: right_notrans_upper', function t() {
	var WORK;
	var info;
	var tc;
	var r;
	var C;

	tc = findCase( 'right_notrans_upper' );
	r = reduceUpper();
	C = identity4();
	WORK = new Float64Array( 256 );
	info = dormtr( 'right', 'upper', 'no-transpose', 4, 4, r.A, 1, 4, 0, r.TAU, 1, 0, C, 1, 4, 0, WORK, 1, 0, 256 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
	assertArrayClose( flattenColMajor( C, 4, 4 ), tc.C, 1e-14, 'C' );
});

test( 'dormtr: right_trans_upper', function t() {
	var WORK;
	var info;
	var tc;
	var r;
	var C;

	tc = findCase( 'right_trans_upper' );
	r = reduceUpper();
	C = identity4();
	WORK = new Float64Array( 256 );
	info = dormtr( 'right', 'upper', 'transpose', 4, 4, r.A, 1, 4, 0, r.TAU, 1, 0, C, 1, 4, 0, WORK, 1, 0, 256 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
	assertArrayClose( flattenColMajor( C, 4, 4 ), tc.C, 1e-14, 'C' );
});

test( 'dormtr: left_notrans_lower', function t() {
	var WORK;
	var info;
	var tc;
	var r;
	var C;

	tc = findCase( 'left_notrans_lower' );
	r = reduceLower();
	C = identity4();
	WORK = new Float64Array( 256 );
	info = dormtr( 'left', 'lower', 'no-transpose', 4, 4, r.A, 1, 4, 0, r.TAU, 1, 0, C, 1, 4, 0, WORK, 1, 0, 256 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
	assertArrayClose( flattenColMajor( C, 4, 4 ), tc.C, 1e-14, 'C' );
});

test( 'dormtr: left_trans_lower', function t() {
	var WORK;
	var info;
	var tc;
	var r;
	var C;

	tc = findCase( 'left_trans_lower' );
	r = reduceLower();
	C = identity4();
	WORK = new Float64Array( 256 );
	info = dormtr( 'left', 'lower', 'transpose', 4, 4, r.A, 1, 4, 0, r.TAU, 1, 0, C, 1, 4, 0, WORK, 1, 0, 256 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
	assertArrayClose( flattenColMajor( C, 4, 4 ), tc.C, 1e-14, 'C' );
});

test( 'dormtr: right_notrans_lower', function t() {
	var WORK;
	var info;
	var tc;
	var r;
	var C;

	tc = findCase( 'right_notrans_lower' );
	r = reduceLower();
	C = identity4();
	WORK = new Float64Array( 256 );
	info = dormtr( 'right', 'lower', 'no-transpose', 4, 4, r.A, 1, 4, 0, r.TAU, 1, 0, C, 1, 4, 0, WORK, 1, 0, 256 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
	assertArrayClose( flattenColMajor( C, 4, 4 ), tc.C, 1e-14, 'C' );
});

test( 'dormtr: right_trans_lower', function t() {
	var WORK;
	var info;
	var tc;
	var r;
	var C;

	tc = findCase( 'right_trans_lower' );
	r = reduceLower();
	C = identity4();
	WORK = new Float64Array( 256 );
	info = dormtr( 'right', 'lower', 'transpose', 4, 4, r.A, 1, 4, 0, r.TAU, 1, 0, C, 1, 4, 0, WORK, 1, 0, 256 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
	assertArrayClose( flattenColMajor( C, 4, 4 ), tc.C, 1e-14, 'C' );
});

test( 'dormtr: m_zero', function t() {
	var WORK;
	var info;
	var r;
	var C;

	r = reduceUpper();
	C = identity4();
	WORK = new Float64Array( 256 );
	info = dormtr( 'left', 'upper', 'no-transpose', 0, 4, r.A, 1, 4, 0, r.TAU, 1, 0, C, 1, 4, 0, WORK, 1, 0, 256 ); // eslint-disable-line max-len
	assert.equal( info, 0 );
});

test( 'dormtr: n_zero', function t() {
	var WORK;
	var info;
	var r;
	var C;

	r = reduceUpper();
	C = identity4();
	WORK = new Float64Array( 256 );
	info = dormtr( 'left', 'upper', 'no-transpose', 4, 0, r.A, 1, 4, 0, r.TAU, 1, 0, C, 1, 4, 0, WORK, 1, 0, 256 ); // eslint-disable-line max-len
	assert.equal( info, 0 );
});

test( 'dormtr: nq_one_left', function t() {
	var WORK;
	var info;
	var r;
	var C;

	r = reduceUpper();
	C = new Float64Array([ 7, 3, -1, 2 ]);
	WORK = new Float64Array( 256 );
	info = dormtr( 'left', 'upper', 'no-transpose', 1, 4, r.A, 1, 4, 0, r.TAU, 1, 0, C, 1, 1, 0, WORK, 1, 0, 256 ); // eslint-disable-line max-len
	assert.equal( info, 0 );
});

test( 'dormtr: left_notrans_upper_rect', function t() {
	var WORK;
	var info;
	var tc;
	var r;
	var C;

	tc = findCase( 'left_notrans_upper_rect' );
	r = reduceUpper();
	C = new Float64Array([
		1,
		2,
		3,
		4,
		5,
		6,
		7,
		8
	]);
	WORK = new Float64Array( 256 );
	info = dormtr( 'left', 'upper', 'no-transpose', 4, 2, r.A, 1, 4, 0, r.TAU, 1, 0, C, 1, 4, 0, WORK, 1, 0, 256 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
	assertArrayClose( flattenColMajor( C, 4, 2 ), tc.C, 1e-14, 'C' );
});

test( 'dormtr: right_notrans_lower_rect', function t() {
	var WORK;
	var info;
	var tc;
	var r;
	var C;

	tc = findCase( 'right_notrans_lower_rect' );
	r = reduceLower();
	C = new Float64Array([
		1,
		2,
		3,
		4,
		5,
		6,
		7,
		8
	]);
	WORK = new Float64Array( 256 );
	info = dormtr( 'right', 'lower', 'no-transpose', 2, 4, r.A, 1, 4, 0, r.TAU, 1, 0, C, 1, 2, 0, WORK, 1, 0, 256 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
	assertArrayClose( flattenColMajor( C, 2, 4 ), tc.C, 1e-14, 'C' );
});
