/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var ztptri = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'ztptri.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync, max-len
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
* Converts a Float64Array to an array.
*
* @private
* @param {Float64Array} arr - input array
* @returns {Array} output array
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
* Asserts that two arrays are element-wise approximately equal.
*
* @private
* @param {Array} actual - actual value
* @param {Array} expected - expected value
* @param {number} tol - tolerance
* @param {string} msg - assertion message
*/
function assertArrayClose( actual, expected, tol, msg ) {
	var relErr;
	var i;

	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i += 1 ) {
		relErr = Math.abs( actual[ i ] - expected[ i ] ) / Math.max( Math.abs( expected[ i ] ), 1.0 ); // eslint-disable-line max-len
		assert.ok( relErr <= tol, msg + '[' + i + ']: expected ' + expected[ i ] + ', got ' + actual[ i ] ); // eslint-disable-line max-len
	}
}

/**
* Creates a Complex128Array from interleaved real/imaginary doubles.
*
* @private
* @param {Array} arr - interleaved real/imaginary values
* @returns {Complex128Array} complex array
*/
function c128( arr ) {
	return new Complex128Array( new Float64Array( arr ) );
}


// TESTS //

test( 'ztptri is a function', function t() {
	assert.equal( typeof ztptri, 'function' );
});

test( 'ztptri: upper, non-unit (3x3)', function t() {
	var info;
	var tc;
	var ap;
	var av;

	tc = findCase( 'upper_nonunit_3' );
	ap = c128( [ 2.0, 1.0, 1.0, 2.0, 4.0, 1.0, 3.0, 0.0, 5.0, -1.0, 6.0, 2.0 ] );
	info = ztptri( 'upper', 'non-unit', 3, ap, 1, 0 );
	assert.equal( info, 0 );
	av = toArray( new Float64Array( ap.buffer, ap.byteOffset, 12 ) );
	assertArrayClose( av, tc.ap, 1e-14, 'ap' );
});

test( 'ztptri: lower, non-unit (3x3)', function t() {
	var info;
	var tc;
	var ap;
	var av;

	tc = findCase( 'lower_nonunit_3' );
	ap = c128( [ 2.0, 1.0, 1.0, 2.0, 3.0, 0.0, 4.0, 1.0, 5.0, -1.0, 6.0, 2.0 ] );
	info = ztptri( 'lower', 'non-unit', 3, ap, 1, 0 );
	assert.equal( info, 0 );
	av = toArray( new Float64Array( ap.buffer, ap.byteOffset, 12 ) );
	assertArrayClose( av, tc.ap, 1e-14, 'ap' );
});

test( 'ztptri: upper, unit (3x3)', function t() {
	var info;
	var tc;
	var ap;
	var av;

	tc = findCase( 'upper_unit_3' );
	ap = c128( [ 99.0, 99.0, 1.0, 2.0, 99.0, 99.0, 3.0, 0.0, 5.0, -1.0, 99.0, 99.0 ] ); // eslint-disable-line max-len
	info = ztptri( 'upper', 'unit', 3, ap, 1, 0 );
	assert.equal( info, 0 );
	av = toArray( new Float64Array( ap.buffer, ap.byteOffset, 12 ) );
	assertArrayClose( av, tc.ap, 1e-14, 'ap' );
});

test( 'ztptri: lower, unit (3x3)', function t() {
	var info;
	var tc;
	var ap;
	var av;

	tc = findCase( 'lower_unit_3' );
	ap = c128( [ 99.0, 99.0, 1.0, 2.0, 3.0, 0.0, 99.0, 99.0, 5.0, -1.0, 99.0, 99.0 ] ); // eslint-disable-line max-len
	info = ztptri( 'lower', 'unit', 3, ap, 1, 0 );
	assert.equal( info, 0 );
	av = toArray( new Float64Array( ap.buffer, ap.byteOffset, 12 ) );
	assertArrayClose( av, tc.ap, 1e-14, 'ap' );
});

test( 'ztptri: N=0 quick return', function t() {
	var info;
	var ap;

	ap = c128( [ 99.0, 99.0 ] );
	info = ztptri( 'upper', 'non-unit', 0, ap, 1, 0 );
	assert.equal( info, 0 );
});

test( 'ztptri: N=1 edge case', function t() {
	var info;
	var tc;
	var ap;
	var av;

	tc = findCase( 'n1' );
	ap = c128( [ 3.0, 4.0 ] );
	info = ztptri( 'upper', 'non-unit', 1, ap, 1, 0 );
	assert.equal( info, 0 );
	av = toArray( new Float64Array( ap.buffer, ap.byteOffset, 2 ) );
	assertArrayClose( av, tc.ap, 1e-14, 'ap' );
});

test( 'ztptri: upper, non-unit (4x4)', function t() {
	var info;
	var tc;
	var ap;
	var av;

	tc = findCase( 'upper_nonunit_4' );
	ap = c128([
		1.0,
		1.0,
		2.0,
		0.0,
		5.0,
		1.0,
		3.0,
		1.0,
		6.0,
		0.0,
		8.0,
		2.0,
		4.0,
		2.0,
		7.0,
		3.0,
		9.0,
		1.0,
		10.0,
		0.0
	]);
	info = ztptri( 'upper', 'non-unit', 4, ap, 1, 0 );
	assert.equal( info, 0 );
	av = toArray( new Float64Array( ap.buffer, ap.byteOffset, 20 ) );
	assertArrayClose( av, tc.ap, 1e-14, 'ap' );
});

test( 'ztptri: lower, non-unit (4x4)', function t() {
	var info;
	var tc;
	var ap;
	var av;

	tc = findCase( 'lower_nonunit_4' );
	ap = c128([
		1.0,
		1.0,
		2.0,
		0.0,
		3.0,
		1.0,
		4.0,
		2.0,
		5.0,
		1.0,
		6.0,
		0.0,
		7.0,
		3.0,
		8.0,
		2.0,
		9.0,
		1.0,
		10.0,
		0.0
	]);
	info = ztptri( 'lower', 'non-unit', 4, ap, 1, 0 );
	assert.equal( info, 0 );
	av = toArray( new Float64Array( ap.buffer, ap.byteOffset, 20 ) );
	assertArrayClose( av, tc.ap, 1e-14, 'ap' );
});

test( 'ztptri: singular upper (info=2)', function t() {
	var info;
	var ap;

	ap = c128( [ 2.0, 1.0, 1.0, 2.0, 0.0, 0.0, 3.0, 0.0, 5.0, -1.0, 6.0, 2.0 ] );
	info = ztptri( 'upper', 'non-unit', 3, ap, 1, 0 );
	assert.equal( info, 2 );
});

test( 'ztptri: singular lower (info=3)', function t() {
	var info;
	var ap;

	ap = c128( [ 2.0, 1.0, 1.0, 2.0, 3.0, 0.0, 4.0, 1.0, 5.0, -1.0, 0.0, 0.0 ] );
	info = ztptri( 'lower', 'non-unit', 3, ap, 1, 0 );
	assert.equal( info, 3 );
});

test( 'ztptri: works with non-zero offset', function t() {
	var info;
	var tc;
	var ap;
	var av;

	tc = findCase( 'upper_nonunit_3' );
	ap = c128( [ 99.0, 99.0, 77.0, 77.0, 2.0, 1.0, 1.0, 2.0, 4.0, 1.0, 3.0, 0.0, 5.0, -1.0, 6.0, 2.0 ] ); // eslint-disable-line max-len
	info = ztptri( 'upper', 'non-unit', 3, ap, 1, 2 );
	assert.equal( info, 0 );
	av = toArray( new Float64Array( ap.buffer, ap.byteOffset, 16 ) );
	assert.equal( av[ 0 ], 99.0 );
	assert.equal( av[ 1 ], 99.0 );
	assert.equal( av[ 2 ], 77.0 );
	assert.equal( av[ 3 ], 77.0 );
	assertArrayClose( av.slice( 4 ), tc.ap, 1e-14, 'ap' );
});
