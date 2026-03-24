'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var izmax1 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'izmax1.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}


// TESTS //

test( 'izmax1: main export is a function', function t() {
	assert.strictEqual( typeof izmax1, 'function' );
});

test( 'izmax1: basic 3-element vector', function t() {
	var tc = findCase( 'basic_3' );
	// zx = [(1,0), (3,4), (2,0)] => max at Fortran index 2 = JS index 1
	var zx = new Complex128Array( [ 1, 0, 3, 4, 2, 0 ] );
	var result = izmax1( 3, zx, 1, 0 );
	assert.strictEqual( result, tc.result - 1, 'result (0-based)' );
});

test( 'izmax1: max at first element', function t() {
	var tc = findCase( 'max_first' );
	var zx = new Complex128Array( [ 5, 12, 3, 4, 1, 0 ] );
	var result = izmax1( 3, zx, 1, 0 );
	assert.strictEqual( result, tc.result - 1, 'result (0-based)' );
});

test( 'izmax1: max at last element', function t() {
	var tc = findCase( 'max_last' );
	var zx = new Complex128Array( [ 1, 0, 2, 0, 3, 0, 0, 10 ] );
	var result = izmax1( 4, zx, 1, 0 );
	assert.strictEqual( result, tc.result - 1, 'result (0-based)' );
});

test( 'izmax1: N=1', function t() {
	var tc = findCase( 'n_one' );
	var zx = new Complex128Array( [ 7, 3 ] );
	var result = izmax1( 1, zx, 1, 0 );
	assert.strictEqual( result, tc.result - 1, 'result (0-based)' );
});

test( 'izmax1: stride=2', function t() {
	var tc = findCase( 'stride2' );
	var zx = new Complex128Array( [ 1, 0, 99, 99, 3, 4, 99, 99, 2, 0 ] );
	var result = izmax1( 3, zx, 2, 0 );
	assert.strictEqual( result, tc.result - 1, 'result (0-based)' );
});

test( 'izmax1: all equal magnitudes returns first', function t() {
	var tc = findCase( 'equal_magnitudes' );
	// All have |z| = 5, Fortran returns first (1-based), JS returns 0-based
	var zx = new Complex128Array( [ 3, 4, 0, 5, 5, 0 ] );
	var result = izmax1( 3, zx, 1, 0 );
	assert.strictEqual( result, tc.result - 1, 'result (0-based)' );
});

test( 'izmax1: N < 1 returns -1', function t() {
	var zx = new Complex128Array( [ 1, 0 ] );
	var result = izmax1( 0, zx, 1, 0 );
	assert.strictEqual( result, -1 );
});

test( 'izmax1: nonzero offset', function t() {
	// Skip first element, search starting at index 1
	var zx = new Complex128Array( [ 99, 99, 1, 0, 3, 4, 2, 0 ] );
	var result = izmax1( 3, zx, 1, 1 );
	// Max is at element index 1 relative to offset start
	assert.strictEqual( result, 1 );
});
