/* eslint-disable no-restricted-syntax, stdlib/require-globals, stdlib/first-unit-test */

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dnrm2 = require( './../lib/base.js' );

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dnrm2.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
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
	var x = new Float64Array( [ 1e+300, 1e+300, 1e+300 ] );
	var expected = Math.sqrt( 3.0 ) * 1e+300;
	assertClose( dnrm2( 3, x, 1, 0 ), expected, 1e-14, 'overflow result' );
});

test( 'dnrm2: overflow path with medium values (lines 69-70)', function t() {
	// Mix of one value above TBIG (~2e+146) and medium-range values in [TSML, TBIG].
	// This exercises lines 69-70: abig > 0 and amed > 0 branch.
	var x = new Float64Array( [ 3e+146, 1.0, 2.0 ] );
	// The large value dominates; the blue algorithm safely handles this
	var expected = Math.sqrt( 9e+292 + 1.0 + 4.0 );
	assertClose( dnrm2( 3, x, 1, 0 ), expected, 1e-14, 'overflow+medium result' );
});

test( 'dnrm2: underflow path - very small values only', function t() {
	// Values below TSML (~1.492e-154) with NO medium values.
	// This exercises the asml > 0 path where amed === 0 (lines 87-88).
	var x = new Float64Array( [ 1e-200, 1e-200, 1e-200 ] );
	var expected = Math.sqrt( 3.0 ) * 1e-200;
	assertClose( dnrm2( 3, x, 1, 0 ), expected, 1e-14, 'underflow-only result' );
});

test( 'dnrm2: underflow path with medium values (lines 75-85)', function t() {
	// Mix of values below TSML and medium-range values.
	// This exercises lines 75-85: asml > 0 and amed > 0 branch.
	var x = new Float64Array( [ 1e-200, 1.0, 2.0 ] );
	// norm = sqrt(1e-400 + 1 + 4) ~ sqrt(5)
	var expected = Math.sqrt( 1e-400 + 1.0 + 4.0 );
	assertClose( dnrm2( 3, x, 1, 0 ), expected, 1e-14, 'underflow+medium result' );
});

test( 'dnrm2: underflow path, asml > amed (lines 78-79)', function t() {
	// Ensure we cover the branch where asml > amed in lines 77-79.
	// Use 2 values just below TSML (~1.49e-154) and 1 value just barely above TSML.
	// asml = sqrt(2) * 1.4e-154 ~ 1.98e-154 > amed = 1.4917e-154
	var TSML = 1.4916681462400413e-154;
	var x = new Float64Array( [ 1.4e-154, 1.4e-154, TSML + 1e-168 ] );
	var expected = Math.sqrt( 2.0 * 1.96e-308 + TSML * TSML );
	assertClose( dnrm2( 3, x, 1, 0 ), expected, 1e-10, 'asml > amed branch' );
});
