/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var iladlc = require( './../lib/base.js' );

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'iladlc.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
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

// Fortran returns 1-based; JS returns 0-based:
/**
* Expected.
*
* @private
* @param {*} tc - tc
* @returns {*} result
*/
function expected( tc ) {
	return tc.result - 1;
}

test( 'iladlc: basic 3x4 diagonal', function t() {
	var tc = findCase( 'basic_3x4' );

	// 3x4 col-major, LDA=4, strideA1=1, strideA2=4
	var A = new Float64Array( 4 * 4 );
	A[ 0 + 0 * 4 ] = 1.0;
	A[ 1 + 1 * 4 ] = 2.0;
	A[ 2 + 2 * 4 ] = 3.0;
	assert.strictEqual( iladlc( 3, 4, A, 1, 4, 0 ), expected( tc ) );
});

test( 'iladlc: all zeros', function t() {
	var tc = findCase( 'all_zeros' );
	var A = new Float64Array( 4 * 4 );
	assert.strictEqual( iladlc( 3, 4, A, 1, 4, 0 ), expected( tc ) );
});

test( 'iladlc: last column non-zero', function t() {
	var tc = findCase( 'last_col_nonzero' );
	var A = new Float64Array( 4 * 4 );
	A[ 0 + 3 * 4 ] = 5.0;
	assert.strictEqual( iladlc( 3, 4, A, 1, 4, 0 ), expected( tc ) );
});

test( 'iladlc: first column only', function t() {
	var tc = findCase( 'first_col_only' );
	var A = new Float64Array( 4 * 4 );
	A[ 0 + 0 * 4 ] = 1.0;
	A[ 1 + 0 * 4 ] = 2.0;
	assert.strictEqual( iladlc( 3, 4, A, 1, 4, 0 ), expected( tc ) );
});

test( 'iladlc: N=0', function t() {
	var tc = findCase( 'n_zero' );
	var A = new Float64Array( 4 * 4 );
	assert.strictEqual( iladlc( 3, 0, A, 1, 4, 0 ), expected( tc ) );
});

test( 'iladlc: bottom right corner', function t() {
	var tc = findCase( 'bottom_right' );
	var A = new Float64Array( 4 * 4 );
	A[ 2 + 3 * 4 ] = 9.0;
	assert.strictEqual( iladlc( 3, 4, A, 1, 4, 0 ), expected( tc ) );
});
