'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dlagtf = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlagtf.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'dlagtf: basic 5x5', function t() {
	var tc = findCase( 'basic_5x5' );
	var a = new Float64Array( [ 2.0, 2.0, 2.0, 2.0, 2.0 ] );
	var b = new Float64Array( [ 1.0, 1.0, 1.0, 1.0 ] );
	var c = new Float64Array( [ 1.0, 1.0, 1.0, 1.0 ] );
	var d = new Float64Array( 3 );
	var IN = new Int32Array( 5 );

	var info = dlagtf( 5, a, 1, 0, 2.0, b, 1, 0, c, 1, 0, 0.0, d, 1, 0, IN, 1, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( a, tc.a, 1e-14, 'a' );
	assertArrayClose( b, tc.b, 1e-14, 'b' );
	assertArrayClose( c, tc.c, 1e-14, 'c' );
	assertArrayClose( d, tc.d, 1e-14, 'd' );
	assert.deepEqual( Array.from( IN ), tc.in, 'IN' );
});

test( 'dlagtf: N=1', function t() {
	var tc = findCase( 'n_equals_1' );
	var a = new Float64Array( [ 5.0 ] );
	var b = new Float64Array( 1 );
	var c = new Float64Array( 1 );
	var d = new Float64Array( 1 );
	var IN = new Int32Array( 1 );

	var info = dlagtf( 1, a, 1, 0, 3.0, b, 1, 0, c, 1, 0, 0.0, d, 1, 0, IN, 1, 0 );

	assert.equal( info, tc.info, 'info' );
	assertClose( a[ 0 ], tc.a[ 0 ], 1e-14, 'a[0]' );
	assert.equal( IN[ 0 ], tc.in[ 0 ], 'IN[0]' );
});

test( 'dlagtf: N=0', function t() {
	var a = new Float64Array( 1 );
	var b = new Float64Array( 1 );
	var c = new Float64Array( 1 );
	var d = new Float64Array( 1 );
	var IN = new Int32Array( 1 );

	var info = dlagtf( 0, a, 1, 0, 0.0, b, 1, 0, c, 1, 0, 0.0, d, 1, 0, IN, 1, 0 );

	assert.equal( info, 0, 'info' );
});
