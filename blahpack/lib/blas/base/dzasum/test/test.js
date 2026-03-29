/* eslint-disable no-restricted-syntax, stdlib/require-globals, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var dzasum = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dzasum.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}


// TESTS //

test( 'dzasum: basic', function t() {
	var result = dzasum( 3, zx, 1, 0 );
	var tc = findCase( 'basic' );
	var zx = new Complex128Array( [ 1.0, 2.0, 3.0, -4.0, -5.0, 6.0 ] );

	// |1|+|2| + |3|+|-4| + |-5|+|6| = 3 + 7 + 11 = 21
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dzasum: n_zero', function t() {
	var result = dzasum( 0, zx, 1, 0 );
	var tc = findCase( 'n_zero' );
	var zx = new Complex128Array( [ 1.0, 2.0, 3.0, -4.0, -5.0, 6.0 ] );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dzasum: n_one', function t() {
	var result = dzasum( 1, zx, 1, 0 );
	var tc = findCase( 'n_one' );
	var zx = new Complex128Array( [ 3.0, 4.0 ] );

	// |3| + |4| = 7
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dzasum: stride2', function t() {
	var result = dzasum( 2, zx, 2, 0 );
	var tc = findCase( 'stride2' );
	var zx = new Complex128Array( [ 1.0, 1.0, 99.0, 99.0, 2.0, 3.0 ] );

	// (|1|+|1|) + (|2|+|3|) = 2 + 5 = 7
	assertClose( result, tc.result, 1e-14, 'result' );
});
