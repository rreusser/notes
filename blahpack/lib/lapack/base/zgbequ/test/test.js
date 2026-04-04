/* eslint-disable no-restricted-syntax, stdlib/first-unit-test, max-lines */

'use strict';

// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var zgbequ = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'zgbequ.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
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

test( 'zgbequ: basic 3x3 tridiagonal band (KL=1, KU=1)', function t() {
	var result;
	var ldab;
	var tc;
	var kl;
	var ku;
	var AB;
	var r;
	var c;

	tc = findCase( 'basic' );
	kl = 1;
	ku = 1;
	ldab = kl + ku + 1;
	AB = new Complex128Array([
		0,
		0,
		2,
		1,
		3,
		0,    // column 1
		1,
		2,
		4,
		1,
		2,
		3,    // column 2
		1,
		1,
		5,
		0,
		0,
		0     // column 3
	]);
	r = new Float64Array( 3 );
	c = new Float64Array( 3 );
	result = zgbequ( 3, 3, kl, ku, AB, 1, ldab, 0, r, 1, 0, c, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
	assertArrayClose( toArray( r ), tc.r, 1e-14, 'r' );
	assertArrayClose( toArray( c ), tc.c, 1e-14, 'c' );
	assertClose( result.rowcnd, tc.rowcnd, 1e-14, 'rowcnd' );
	assertClose( result.colcnd, tc.colcnd, 1e-14, 'colcnd' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
});

test( 'zgbequ: larger 4x4 band (KL=2, KU=1)', function t() {
	var result;
	var ldab;
	var tc;
	var kl;
	var ku;
	var AB;
	var r;
	var c;

	tc = findCase( 'larger' );
	kl = 2;
	ku = 1;
	ldab = kl + ku + 1;
	AB = new Complex128Array([
		0,
		0,
		1,
		1,
		3,
		2,
		0,
		5,    // column 1
		2,
		0,
		4,
		0,
		6,
		1,
		1,
		0,    // column 2
		1,
		3,
		2,
		2,
		7,
		0,
		0,
		0,    // column 3
		3,
		0,
		4,
		4,
		0,
		0,
		0,
		0     // column 4
	]);
	r = new Float64Array( 4 );
	c = new Float64Array( 4 );
	result = zgbequ( 4, 4, kl, ku, AB, 1, ldab, 0, r, 1, 0, c, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
	assertArrayClose( toArray( r ), tc.r, 1e-14, 'r' );
	assertArrayClose( toArray( c ), tc.c, 1e-14, 'c' );
	assertClose( result.rowcnd, tc.rowcnd, 1e-14, 'rowcnd' );
	assertClose( result.colcnd, tc.colcnd, 1e-14, 'colcnd' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
});

test( 'zgbequ: zero row returns info=i', function t() {
	var result;
	var ldab;
	var tc;
	var kl;
	var ku;
	var AB;
	var r;
	var c;

	tc = findCase( 'zero_row' );
	kl = 2;
	ku = 1;
	ldab = kl + ku + 1;
	AB = new Complex128Array([
		0,
		0,
		1,
		0,
		0,
		0,
		0,
		0,    // column 1
		2,
		0,
		0,
		0,
		3,
		0,
		0,
		0     // column 2
	]);
	r = new Float64Array( 3 );
	c = new Float64Array( 2 );
	result = zgbequ( 3, 2, kl, ku, AB, 1, ldab, 0, r, 1, 0, c, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
});

test( 'zgbequ: zero column returns info=M+j', function t() {
	var result;
	var ldab;
	var tc;
	var kl;
	var ku;
	var AB;
	var r;
	var c;

	tc = findCase( 'zero_col' );
	kl = 1;
	ku = 0;
	ldab = kl + ku + 1;
	AB = new Complex128Array([
		1,
		0,
		2,
		0,    // column 1
		0,
		0,
		0,
		0     // column 2
	]);
	r = new Float64Array( 2 );
	c = new Float64Array( 2 );
	result = zgbequ( 2, 2, kl, ku, AB, 1, ldab, 0, r, 1, 0, c, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
	assertArrayClose( toArray( r ), tc.r, 1e-14, 'r' );
});

test( 'zgbequ: quick return M=0', function t() {
	var result;
	var tc;
	var r;
	var c;

	tc = findCase( 'm_zero' );
	r = new Float64Array( 0 );
	c = new Float64Array( 3 );
	result = zgbequ( 0, 3, 0, 0, new Complex128Array( 0 ), 1, 1, 0, r, 1, 0, c, 1, 0 ); // eslint-disable-line max-len
	assert.equal( result.info, 0, 'info' );
	assertClose( result.rowcnd, tc.rowcnd, 1e-14, 'rowcnd' );
	assertClose( result.colcnd, tc.colcnd, 1e-14, 'colcnd' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
});

test( 'zgbequ: quick return N=0', function t() {
	var result;
	var tc;
	var r;
	var c;

	tc = findCase( 'n_zero' );
	r = new Float64Array( 3 );
	c = new Float64Array( 0 );
	result = zgbequ( 3, 0, 0, 0, new Complex128Array( 0 ), 1, 1, 0, r, 1, 0, c, 1, 0 ); // eslint-disable-line max-len
	assert.equal( result.info, 0, 'info' );
	assertClose( result.rowcnd, tc.rowcnd, 1e-14, 'rowcnd' );
	assertClose( result.colcnd, tc.colcnd, 1e-14, 'colcnd' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
});

test( 'zgbequ: diagonal only (KL=0, KU=0)', function t() {
	var result;
	var tc;
	var AB;
	var r;
	var c;

	tc = findCase( 'diagonal' );
	AB = new Complex128Array([
		3,
		4,    // A(1,1)
		1,
		0,    // A(2,2)
		0,
		2     // A(3,3)
	]);
	r = new Float64Array( 3 );
	c = new Float64Array( 3 );
	result = zgbequ( 3, 3, 0, 0, AB, 1, 1, 0, r, 1, 0, c, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
	assertArrayClose( toArray( r ), tc.r, 1e-14, 'r' );
	assertArrayClose( toArray( c ), tc.c, 1e-14, 'c' );
	assertClose( result.rowcnd, tc.rowcnd, 1e-14, 'rowcnd' );
	assertClose( result.colcnd, tc.colcnd, 1e-14, 'colcnd' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
});

test( 'zgbequ: non-square 2x4 band (KL=0, KU=1)', function t() {
	var result;
	var ldab;
	var tc;
	var kl;
	var ku;
	var AB;
	var r;
	var c;

	tc = findCase( 'nonsquare' );
	kl = 0;
	ku = 1;
	ldab = kl + ku + 1;
	AB = new Complex128Array([
		0,
		0,
		1,
		1,    // column 1
		2,
		3,
		4,
		0,    // column 2
		5,
		1,
		0,
		0,    // column 3
		0,
		0,
		0,
		0     // column 4
	]);
	r = new Float64Array( 2 );
	c = new Float64Array( 4 );
	result = zgbequ( 2, 4, kl, ku, AB, 1, ldab, 0, r, 1, 0, c, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
	assertArrayClose( toArray( r ), tc.r, 1e-14, 'r' );
	assertArrayClose( toArray( c ).slice( 0, 3 ), tc.c.slice( 0, 3 ), 1e-14, 'c (first 3)' ); // eslint-disable-line max-len
	assertClose( result.rowcnd, tc.rowcnd, 1e-14, 'rowcnd' );
});

test( 'zgbequ: 1x1 matrix', function t() {
	var result;
	var tc;
	var AB;
	var r;
	var c;

	tc = findCase( 'one_by_one' );
	AB = new Complex128Array( [ 7, 3 ] );
	r = new Float64Array( 1 );
	c = new Float64Array( 1 );
	result = zgbequ( 1, 1, 0, 0, AB, 1, 1, 0, r, 1, 0, c, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
	assertArrayClose( toArray( r ), tc.r, 1e-14, 'r' );
	assertArrayClose( toArray( c ), tc.c, 1e-14, 'c' );
	assertClose( result.rowcnd, tc.rowcnd, 1e-14, 'rowcnd' );
	assertClose( result.colcnd, tc.colcnd, 1e-14, 'colcnd' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
});
