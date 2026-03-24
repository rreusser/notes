'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dlaruv = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlaruv.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'dlaruv: basic_5', function t() {
	var tc = findCase( 'basic_5' );
	var iseed = new Int32Array( [ 1, 1, 1, 1 ] );
	var x = new Float64Array( 5 );

	dlaruv( iseed, 1, 0, 5, x, 1, 0 );

	assertArrayClose( x, tc.x, 1e-14, 'x' );
	assert.deepEqual( Array.from( iseed ), tc.iseed_out, 'iseed_out' );
});

test( 'dlaruv: basic_10', function t() {
	var tc = findCase( 'basic_10' );
	var iseed = new Int32Array( [ 123, 456, 789, 1011 ] );
	var x = new Float64Array( 10 );

	dlaruv( iseed, 1, 0, 10, x, 1, 0 );

	assertArrayClose( x, tc.x, 1e-14, 'x' );
	assert.deepEqual( Array.from( iseed ), tc.iseed_out, 'iseed_out' );
});

test( 'dlaruv: n_equals_0', function t() {
	var iseed = new Int32Array( [ 1, 1, 1, 1 ] );
	var x = new Float64Array( 1 );

	dlaruv( iseed, 1, 0, 0, x, 1, 0 );

	// Seed should be unchanged
	assert.deepEqual( Array.from( iseed ), [ 1, 1, 1, 1 ], 'iseed unchanged' );
});
