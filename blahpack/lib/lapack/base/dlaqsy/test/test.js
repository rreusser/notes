

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dlaqsy = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlaqsy.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'dlaqsy: upper_equilibrate', function t() {
	var tc = findCase( 'upper_equilibrate' );
	var A = new Float64Array([ 4.0, 0.0, 0.0, 1.0, 9.0, 0.0, 0.5, 2.0, 16.0 ]);
	var s = new Float64Array([ 0.5, 1.0/3.0, 0.25 ]);
	var equed = dlaqsy( 'upper', 3, A, 1, 3, 0, s, 1, 0, 0.05, 16.0 );
	assert.equal( equed, tc.equed, 'equed' );
	assertArrayClose( Array.from( A ), tc.a, 1e-14, 'a' );
});

test( 'dlaqsy: lower_equilibrate', function t() {
	var tc = findCase( 'lower_equilibrate' );
	var A = new Float64Array([ 4.0, 1.0, 0.5, 0.0, 9.0, 2.0, 0.0, 0.0, 16.0 ]);
	var s = new Float64Array([ 0.5, 1.0/3.0, 0.25 ]);
	var equed = dlaqsy( 'lower', 3, A, 1, 3, 0, s, 1, 0, 0.05, 16.0 );
	assert.equal( equed, tc.equed, 'equed' );
	assertArrayClose( Array.from( A ), tc.a, 1e-14, 'a' );
});

test( 'dlaqsy: no_equilibrate', function t() {
	var tc = findCase( 'no_equilibrate' );
	var A = new Float64Array([ 4.0, 1.0, 0.5, 1.0, 9.0, 2.0, 0.5, 2.0, 16.0 ]);
	var s = new Float64Array([ 1.0, 1.0, 1.0 ]);
	var equed = dlaqsy( 'upper', 3, A, 1, 3, 0, s, 1, 0, 0.5, 16.0 );
	assert.equal( equed, tc.equed, 'equed' );
	assertArrayClose( Array.from( A ), tc.a, 1e-14, 'a' );
});

test( 'dlaqsy: n_zero', function t() {
	var tc = findCase( 'n_zero' );
	var A = new Float64Array( 1 );
	var s = new Float64Array( 1 );
	var equed = dlaqsy( 'upper', 0, A, 1, 1, 0, s, 1, 0, 1.0, 1.0 );
	assert.equal( equed, tc.equed, 'equed' );
});

test( 'dlaqsy: n_one_upper', function t() {
	var tc = findCase( 'n_one_upper' );
	var A = new Float64Array([ 100.0 ]);
	var s = new Float64Array([ 0.1 ]);
	var equed = dlaqsy( 'upper', 1, A, 1, 1, 0, s, 1, 0, 0.01, 100.0 );
	assert.equal( equed, tc.equed, 'equed' );
	assertArrayClose( Array.from( A ), tc.a, 1e-14, 'a' );
});

test( 'dlaqsy: small_amax', function t() {
	var tc = findCase( 'small_amax' );
	var A = new Float64Array([ 1e-300, 0.0, 0.0, 1e-300 ]);
	var s = new Float64Array([ 1e150, 1e150 ]);
	var equed = dlaqsy( 'upper', 2, A, 1, 2, 0, s, 1, 0, 1.0, 1e-300 );
	assert.equal( equed, tc.equed, 'equed' );
	assertArrayClose( Array.from( A ), tc.a, 1e-14, 'a' );
});
