/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dnrm2 = require( './../lib/base.js' );

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dnrm2.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
var fixture = lines.map( function parse( line ) {
	return JSON.parse( line );
} );

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

test( 'dnrm2: basic [3,4] -> 5', function t() {
	var tc = findCase( 'basic' );
	var x = new Float64Array( [ 3.0, 4.0 ] );
	assertClose( dnrm2( 2, x, 1, 0 ), tc.result, 1e-14, 'result' );
});

test( 'dnrm2: n=0 -> 0', function t() {
	var tc = findCase( 'n_zero' );
	var x = new Float64Array( [ 3.0, 4.0 ] );
	assert.strictEqual( dnrm2( 0, x, 1, 0 ), tc.result );
});

test( 'dnrm2: n=1', function t() {
	var tc = findCase( 'n_one' );
	var x = new Float64Array( [ 7.0 ] );
	assertClose( dnrm2( 1, x, 1, 0 ), tc.result, 1e-14, 'result' );
});

test( 'dnrm2: stride=2', function t() {
	var tc = findCase( 'stride2' );
	var x = new Float64Array( [ 3.0, 999.0, 4.0 ] );
	assertClose( dnrm2( 2, x, 2, 0 ), tc.result, 1e-14, 'result' );
});

test( 'dnrm2: large values', function t() {
	var tc = findCase( 'large_values' );
	var x = new Float64Array( [ 1e+154, 1e+154 ] );
	assertClose( dnrm2( 2, x, 1, 0 ), tc.result, 1e-14, 'result' );
});

test( 'dnrm2: small values', function t() {
	var tc = findCase( 'small_values' );
	var x = new Float64Array( [ 1e-160, 1e-160 ] );
	assertClose( dnrm2( 2, x, 1, 0 ), tc.result, 1e-14, 'result' );
});

test( 'dnrm2: five elements', function t() {
	var tc = findCase( 'five_elements' );
	var x = new Float64Array( [ 1, 2, 3, 4, 5 ] );
	assertClose( dnrm2( 5, x, 1, 0 ), tc.result, 1e-14, 'result' );
});

test( 'dnrm2: all zeros', function t() {
	var tc = findCase( 'all_zeros' );
	var x = new Float64Array( [ 0, 0, 0 ] );
	assert.strictEqual( dnrm2( 3, x, 1, 0 ), tc.result );
});

test( 'dnrm2: overflow path - very large values', function t() {
	// Values above TBIG (~1.998e+146) trigger the abig accumulator (overflow path)
	var expected = Math.sqrt( 3.0 ) * 1e+300;
	var x = new Float64Array( [ 1e+300, 1e+300, 1e+300 ] );
	assertClose( dnrm2( 3, x, 1, 0 ), expected, 1e-14, 'overflow result' );
});

test( 'dnrm2: overflow path with medium values (lines 69-70)', function t() {
	// Mix of one value above TBIG (~2e+146) and medium-range values in [TSML, TBIG]. // eslint-disable-line max-len
	// This exercises lines 69-70: abig > 0 and amed > 0 branch.
	var expected = Math.sqrt( 9e+292 + 1.0 + 4.0 );
	var x = new Float64Array( [ 3e+146, 1.0, 2.0 ] );
	assertClose( dnrm2( 3, x, 1, 0 ), expected, 1e-14, 'overflow+medium result' );
});

test( 'dnrm2: underflow path - very small values only', function t() {
	// Values below TSML (~1.492e-154) with NO medium values.
	// This exercises the asml > 0 path where amed === 0 (lines 87-88).
	var expected = Math.sqrt( 3.0 ) * 1e-200;
	var x = new Float64Array( [ 1e-200, 1e-200, 1e-200 ] );
	assertClose( dnrm2( 3, x, 1, 0 ), expected, 1e-14, 'underflow-only result' );
});

test( 'dnrm2: underflow path with medium values (lines 75-85)', function t() {
	// Mix of values below TSML and medium-range values.
	// This exercises lines 75-85: asml > 0 and amed > 0 branch.
	var expected = Math.sqrt( 1e-400 + 1.0 + 4.0 );
	var x = new Float64Array( [ 1e-200, 1.0, 2.0 ] );
	assertClose( dnrm2( 3, x, 1, 0 ), expected, 1e-14, 'underflow+medium result' );
});

test( 'dnrm2: underflow path, asml > amed (lines 78-79)', function t() {
	var expected;
	var TSML;
	var x;

	TSML = 1.4916681462400413e-154;
	x = new Float64Array( [ 1.4e-154, 1.4e-154, TSML + 1e-168 ] );
	expected = Math.sqrt( 2.0 * 1.96e-308 + TSML * TSML );
	assertClose( dnrm2( 3, x, 1, 0 ), expected, 1e-10, 'asml > amed branch' );
});
