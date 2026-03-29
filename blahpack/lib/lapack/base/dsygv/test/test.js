/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';


// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dsygv = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dsygv.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
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

// A = [4 2 1; 2 5 3; 1 3 6], B = [4 2 0; 2 5 1; 0 1 3]

/**
* MakeAUpper.
*
* @private
* @returns {*} result
*/
function makeAUpper( ) {
	return new Float64Array([
		4.0,
		0.0,
		0.0,
		2.0,
		5.0,
		0.0,
		1.0,
		3.0,
		6.0
	]);
}

/**
* MakeALower.
*
* @private
* @returns {*} result
*/
function makeALower( ) {
	return new Float64Array([
		4.0,
		2.0,
		1.0,
		0.0,
		5.0,
		3.0,
		0.0,
		0.0,
		6.0
	]);
}

/**
* MakeBUpper.
*
* @private
* @returns {*} result
*/
function makeBUpper( ) {
	return new Float64Array([
		4.0,
		0.0,
		0.0,
		2.0,
		5.0,
		0.0,
		0.0,
		1.0,
		3.0
	]);
}

/**
* MakeBLower.
*
* @private
* @returns {*} result
*/
function makeBLower( ) {
	return new Float64Array([
		4.0,
		2.0,
		0.0,
		0.0,
		5.0,
		1.0,
		0.0,
		0.0,
		3.0
	]);
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

test( 'dsygv: itype1_v_upper', function t() {
	var absExpected;
	var WORK;
	var info;
	var absA;
	var tc;
	var A;
	var B;
	var w;

	tc = findCase( 'itype1_v_upper' );
	A = makeAUpper();
	B = makeBUpper();
	w = new Float64Array( 3 );
	WORK = new Float64Array( 100 );
	info = dsygv( 1, 'compute', 'upper', 3, A, 1, 3, 0, B, 1, 3, 0, w, 1, 0, WORK, 1, 0, 100 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
	assertArrayClose( toArray( w ), tc.w, 1e-14, 'w' );
	absA = toArray( A ).map( Math.abs );
	absExpected = tc.A.map( Math.abs );
	assertArrayClose( absA, absExpected, 1e-12, 'A' );
});

test( 'dsygv: itype1_v_lower', function t() {
	var absExpected;
	var WORK;
	var info;
	var absA;
	var tc;
	var A;
	var B;
	var w;

	tc = findCase( 'itype1_v_lower' );
	A = makeALower();
	B = makeBLower();
	w = new Float64Array( 3 );
	WORK = new Float64Array( 100 );
	info = dsygv( 1, 'compute', 'lower', 3, A, 1, 3, 0, B, 1, 3, 0, w, 1, 0, WORK, 1, 0, 100 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
	assertArrayClose( toArray( w ), tc.w, 1e-14, 'w' );
	absA = toArray( A ).map( Math.abs );
	absExpected = tc.A.map( Math.abs );
	assertArrayClose( absA, absExpected, 1e-12, 'A' );
});

test( 'dsygv: itype1_n_lower (eigenvalues only)', function t() {
	var WORK;
	var info;
	var tc;
	var A;
	var B;
	var w;

	tc = findCase( 'itype1_n_lower' );
	A = makeALower();
	B = makeBLower();
	w = new Float64Array( 3 );
	WORK = new Float64Array( 100 );
	info = dsygv( 1, 'none', 'lower', 3, A, 1, 3, 0, B, 1, 3, 0, w, 1, 0, WORK, 1, 0, 100 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
	assertArrayClose( toArray( w ), tc.w, 1e-14, 'w' );
});

test( 'dsygv: itype2_v_upper', function t() {
	var WORK;
	var info;
	var tc;
	var A;
	var B;
	var w;

	tc = findCase( 'itype2_v_upper' );
	A = makeAUpper();
	B = makeBUpper();
	w = new Float64Array( 3 );
	WORK = new Float64Array( 100 );
	info = dsygv( 2, 'compute', 'upper', 3, A, 1, 3, 0, B, 1, 3, 0, w, 1, 0, WORK, 1, 0, 100 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
	assertArrayClose( toArray( w ), tc.w, 1e-13, 'w' );
});

test( 'dsygv: itype3_v_lower', function t() {
	var WORK;
	var info;
	var tc;
	var A;
	var B;
	var w;

	tc = findCase( 'itype3_v_lower' );
	A = makeALower();
	B = makeBLower();
	w = new Float64Array( 3 );
	WORK = new Float64Array( 100 );
	info = dsygv( 3, 'compute', 'lower', 3, A, 1, 3, 0, B, 1, 3, 0, w, 1, 0, WORK, 1, 0, 100 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
	assertArrayClose( toArray( w ), tc.w, 1e-13, 'w' );
});

test( 'dsygv: n_zero', function t() {
	var WORK;
	var info;
	var tc;
	var A;
	var B;
	var w;

	tc = findCase( 'n_zero' );
	A = new Float64Array( 1 );
	B = new Float64Array( 1 );
	w = new Float64Array( 1 );
	WORK = new Float64Array( 1 );
	info = dsygv( 1, 'compute', 'upper', 0, A, 1, 1, 0, B, 1, 1, 0, w, 1, 0, WORK, 1, 0, 1 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
});

test( 'dsygv: n_one', function t() {
	var WORK;
	var info;
	var tc;
	var A;
	var B;
	var w;

	tc = findCase( 'n_one' );
	A = new Float64Array([ 6.0 ]);
	B = new Float64Array([ 2.0 ]);
	w = new Float64Array( 1 );
	WORK = new Float64Array( 100 );
	info = dsygv( 1, 'compute', 'upper', 1, A, 1, 1, 0, B, 1, 1, 0, w, 1, 0, WORK, 1, 0, 100 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
	assertClose( w[ 0 ], tc.w1, 1e-14, 'w1' );
	assertClose( A[ 0 ], tc.A1, 1e-14, 'A1' );
});

test( 'dsygv: not_posdef', function t() {
	var WORK;
	var info;
	var tc;
	var A;
	var B;
	var w;

	tc = findCase( 'not_posdef' );
	A = new Float64Array([ 1.0, 0.0, 0.0, 1.0 ]);
	B = new Float64Array([ -1.0, 0.0, 0.0, 1.0 ]);
	w = new Float64Array( 2 );
	WORK = new Float64Array( 100 );
	info = dsygv( 1, 'compute', 'lower', 2, A, 1, 2, 0, B, 1, 2, 0, w, 1, 0, WORK, 1, 0, 100 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
});
