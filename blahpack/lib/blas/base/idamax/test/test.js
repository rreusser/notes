'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var idamax = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'idamax.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}


// TESTS //

test( 'idamax: basic (n=5, stride=1)', function t() {
	var tc = findCase( 'basic' );
	var x = new Float64Array( [ 1.0, -3.0, 2.0, 5.0, -4.0 ] );
	var result = idamax( 5, x, 1, 0 );
	// Fortran returns 1-based index 4, JS should return 0-based index 3
	assert.strictEqual( result, tc.result - 1 );
});

test( 'idamax: n=0 returns -1', function t() {
	var x = new Float64Array( [ 1.0 ] );
	var result = idamax( 0, x, 1, 0 );
	assert.strictEqual( result, -1 );
});

test( 'idamax: n=1 returns 0', function t() {
	var x = new Float64Array( [ 42.0 ] );
	var result = idamax( 1, x, 1, 0 );
	assert.strictEqual( result, 0 );
});

test( 'idamax: negative values', function t() {
	var tc = findCase( 'negative' );
	var x = new Float64Array( [ -2.0, -7.0, -1.0, -3.0 ] );
	var result = idamax( 4, x, 1, 0 );
	assert.strictEqual( result, tc.result - 1 );
});

test( 'idamax: non-unit stride=2', function t() {
	var tc = findCase( 'stride' );
	var x = new Float64Array( [ 1.0, 99.0, 10.0, 99.0, 2.0 ] );
	var result = idamax( 3, x, 2, 0 );
	assert.strictEqual( result, tc.result - 1 );
});

test( 'idamax: first element is max', function t() {
	var tc = findCase( 'first_max' );
	var x = new Float64Array( [ 100.0, 1.0, 2.0 ] );
	var result = idamax( 3, x, 1, 0 );
	assert.strictEqual( result, tc.result - 1 );
});

test( 'idamax: last element is max', function t() {
	var tc = findCase( 'last_max' );
	var x = new Float64Array( [ 1.0, 2.0, 100.0 ] );
	var result = idamax( 3, x, 1, 0 );
	assert.strictEqual( result, tc.result - 1 );
});

test( 'idamax: offset parameter', function t() {
	// Array: [99, 99, 1.0, -3.0, 2.0, 5.0, -4.0], offset=2
	var x = new Float64Array( [ 99.0, 99.0, 1.0, -3.0, 2.0, 5.0, -4.0 ] );
	var result = idamax( 5, x, 1, 2 );
	// Max abs is 5.0 at index 3 (0-based within the view)
	assert.strictEqual( result, 3 );
});

test( 'idamax: negative stride returns -1', function t() {
	var x = new Float64Array( [ 1.0, 2.0, 3.0 ] );
	var result = idamax( 3, x, -1, 2 );
	assert.strictEqual( result, -1 );
});
