

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dsdot = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dsdot.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'dsdot: basic', function t() {
	var tc = findCase( 'basic' );
	var x = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0 ] );
	var y = new Float64Array( [ 2.0, 3.0, 4.0, 5.0, 6.0 ] );
	var result = dsdot( 5, x, 1, 0, y, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dsdot: n_zero', function t() {
	var tc = findCase( 'n_zero' );
	var x = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0 ] );
	var y = new Float64Array( [ 2.0, 3.0, 4.0, 5.0, 6.0 ] );
	var result = dsdot( 0, x, 1, 0, y, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dsdot: n_one', function t() {
	var tc = findCase( 'n_one' );
	var x = new Float64Array( [ 3.0 ] );
	var y = new Float64Array( [ 7.0 ] );
	var result = dsdot( 1, x, 1, 0, y, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dsdot: stride', function t() {
	var tc = findCase( 'stride' );
	var x = new Float64Array( [ 1.0, 0.0, 2.0, 0.0, 3.0 ] );
	var y = new Float64Array( [ 4.0, 0.0, 5.0, 0.0, 6.0 ] );
	var result = dsdot( 3, x, 2, 0, y, 2, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dsdot: neg_inc', function t() {
	var tc = findCase( 'neg_inc' );
	var x = new Float64Array( [ 1.0, 2.0, 3.0 ] );
	var y = new Float64Array( [ 4.0, 5.0, 6.0 ] );

	// Negative stride: start from end of x, walk backward
	var result = dsdot( 3, x, -1, 2, y, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});
