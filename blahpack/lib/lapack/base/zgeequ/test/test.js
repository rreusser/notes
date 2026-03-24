

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var zgeequ = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zgeequ.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}


// TESTS //

test( 'zgeequ: basic_3x3', function t() {
	var tc = findCase( 'basic_3x3' );
	var A = new Complex128Array( [
		4, 1, 1, -1, 0.5, 0.2,
		1, 0.5, 3, 2, 1, -0.5,
		0.5, 0.1, 1, 0.3, 2, 1
	] );
	var r = new Float64Array( 3 );
	var c = new Float64Array( 3 );
	var result = zgeequ( 3, 3, A, 1, 3, 0, r, 1, 0, c, 1, 0 );
	assert.equal( result.info, tc.info );
	assertArrayClose( Array.from( r ), tc.r, 1e-14, 'r' );
	assertArrayClose( Array.from( c ), tc.c, 1e-14, 'c' );
	assertClose( result.rowcnd, tc.rowcnd, 1e-14, 'rowcnd' );
	assertClose( result.colcnd, tc.colcnd, 1e-14, 'colcnd' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
});

test( 'zgeequ: m_zero', function t() {
	var tc = findCase( 'm_zero' );
	var A = new Complex128Array( 1 );
	var r = new Float64Array( 1 );
	var c = new Float64Array( 1 );
	var result = zgeequ( 0, 3, A, 1, 1, 0, r, 1, 0, c, 1, 0 );
	assert.equal( result.info, tc.info );
	assertClose( result.rowcnd, tc.rowcnd, 1e-14, 'rowcnd' );
	assertClose( result.colcnd, tc.colcnd, 1e-14, 'colcnd' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
});

test( 'zgeequ: n_zero', function t() {
	var tc = findCase( 'n_zero' );
	var A = new Complex128Array( 1 );
	var r = new Float64Array( 1 );
	var c = new Float64Array( 1 );
	var result = zgeequ( 3, 0, A, 1, 3, 0, r, 1, 0, c, 1, 0 );
	assert.equal( result.info, tc.info );
	assertClose( result.rowcnd, tc.rowcnd, 1e-14, 'rowcnd' );
	assertClose( result.colcnd, tc.colcnd, 1e-14, 'colcnd' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
});

test( 'zgeequ: zero_row', function t() {
	var tc = findCase( 'zero_row' );
	var A = new Complex128Array( [
		4, 1, 0, 0, 1, 0,
		1, 0.5, 0, 0, 2, 0,
		0.5, 0.1, 0, 0, 3, 0
	] );
	var r = new Float64Array( 3 );
	var c = new Float64Array( 3 );
	var result = zgeequ( 3, 3, A, 1, 3, 0, r, 1, 0, c, 1, 0 );
	assert.equal( result.info, tc.info );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
});

test( 'zgeequ: 1x1', function t() {
	var tc = findCase( '1x1' );
	var A = new Complex128Array( [ 5, 3 ] );
	var r = new Float64Array( 1 );
	var c = new Float64Array( 1 );
	var result = zgeequ( 1, 1, A, 1, 1, 0, r, 1, 0, c, 1, 0 );
	assert.equal( result.info, tc.info );
	assertArrayClose( Array.from( r ), tc.r, 1e-14, 'r' );
	assertArrayClose( Array.from( c ), tc.c, 1e-14, 'c' );
	assertClose( result.rowcnd, tc.rowcnd, 1e-14, 'rowcnd' );
	assertClose( result.colcnd, tc.colcnd, 1e-14, 'colcnd' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
});

test( 'zgeequ: poorly_scaled', function t() {
	var tc = findCase( 'poorly_scaled' );
	var A = new Complex128Array( [
		1e6, 0, 1, 0, 1, 0,
		1, 0, 1e-3, 0, 1, 0,
		1, 0, 1, 0, 1e3, 0
	] );
	var r = new Float64Array( 3 );
	var c = new Float64Array( 3 );
	var result = zgeequ( 3, 3, A, 1, 3, 0, r, 1, 0, c, 1, 0 );
	assert.equal( result.info, tc.info );
	assertArrayClose( Array.from( r ), tc.r, 1e-14, 'r' );
	assertArrayClose( Array.from( c ), tc.c, 1e-14, 'c' );
	assertClose( result.rowcnd, tc.rowcnd, 1e-14, 'rowcnd' );
	assertClose( result.colcnd, tc.colcnd, 1e-14, 'colcnd' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
});

test( 'zgeequ: rect_2x3', function t() {
	var tc = findCase( 'rect_2x3' );
	var A = new Complex128Array( [
		2, 1, 1, -1,
		3, 0, 0.5, 0.5,
		1, 1, 4, 2
	] );
	var r = new Float64Array( 2 );
	var c = new Float64Array( 3 );
	var result = zgeequ( 2, 3, A, 1, 2, 0, r, 1, 0, c, 1, 0 );
	assert.equal( result.info, tc.info );
	assertArrayClose( Array.from( r ), tc.r, 1e-14, 'r' );
	assertArrayClose( Array.from( c ), tc.c, 1e-14, 'c' );
	assertClose( result.rowcnd, tc.rowcnd, 1e-14, 'rowcnd' );
	assertClose( result.colcnd, tc.colcnd, 1e-14, 'colcnd' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
});
