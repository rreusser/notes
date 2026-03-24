'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dlarnv = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlarnv.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'dlarnv: uniform (0,1)', function t() {
	var tc = findCase( 'uniform_01' );
	var iseed = new Int32Array( [ 1, 1, 1, 1 ] );
	var x = new Float64Array( 5 );

	dlarnv( 1, iseed, 1, 0, 5, x, 1, 0 );

	assertArrayClose( x, tc.x, 1e-14, 'x' );
	assert.deepEqual( Array.from( iseed ), tc.iseed_out, 'iseed_out' );
});

test( 'dlarnv: uniform (-1,1)', function t() {
	var tc = findCase( 'uniform_m1_1' );
	var iseed = new Int32Array( [ 1, 1, 1, 1 ] );
	var x = new Float64Array( 5 );

	dlarnv( 2, iseed, 1, 0, 5, x, 1, 0 );

	assertArrayClose( x, tc.x, 1e-14, 'x' );
	assert.deepEqual( Array.from( iseed ), tc.iseed_out, 'iseed_out' );
});

test( 'dlarnv: normal (0,1)', function t() {
	var tc = findCase( 'normal_01' );
	var iseed = new Int32Array( [ 1, 1, 1, 1 ] );
	var x = new Float64Array( 5 );

	dlarnv( 3, iseed, 1, 0, 5, x, 1, 0 );

	assertArrayClose( x, tc.x, 1e-14, 'x' );
	assert.deepEqual( Array.from( iseed ), tc.iseed_out, 'iseed_out' );
});
