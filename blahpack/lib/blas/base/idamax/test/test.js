/* eslint-disable no-restricted-syntax, stdlib/require-globals, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
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

test( 'idamax: main export is a function', function t() {
	assert.strictEqual( typeof idamax, 'function' );
});

test( 'idamax: basic (N=5, stride=1)', function t() {
	// Fortran test: dx = [1, -3, 2, 5, -4], max abs at index 4 (1-based)
	// JS returns 0-based, fixture returns 1-based Fortran result
	var tc = findCase( 'basic' );
	var x = new Float64Array( [ 1.0, -3.0, 2.0, 5.0, -4.0 ] );
	var result = idamax( 5, x, 1, 0 );
	// Fortran returns 4 (1-based), JS returns 3 (0-based)
	assert.strictEqual( result, tc.result - 1 );
});

test( 'idamax: n_zero returns -1', function t() {
	var x = new Float64Array( [ 1.0, 2.0, 3.0 ] );
	var result = idamax( 0, x, 1, 0 );
	assert.strictEqual( result, -1 );
});

test( 'idamax: n_one returns 0', function t() {
	var x = new Float64Array( [ 42.0 ] );
	var result = idamax( 1, x, 1, 0 );
	assert.strictEqual( result, 0 );
});

test( 'idamax: negative values (N=4, stride=1)', function t() {
	// dx = [-2, -7, -1, -3], max abs at index 2 (1-based)
	var tc = findCase( 'negative' );
	var x = new Float64Array( [ -2.0, -7.0, -1.0, -3.0 ] );
	var result = idamax( 4, x, 1, 0 );
	assert.strictEqual( result, tc.result - 1 );
});

test( 'idamax: non-unit stride (N=3, stride=2)', function t() {
	// dx = [1, 99, 10, 99, 2], stride=2, elements accessed: 1, 10, 2
	// max abs at index 2 (1-based)
	var tc = findCase( 'stride' );
	var x = new Float64Array( [ 1.0, 99.0, 10.0, 99.0, 2.0 ] );
	var result = idamax( 3, x, 2, 0 );
	assert.strictEqual( result, tc.result - 1 );
});

test( 'idamax: first element is max', function t() {
	// dx = [100, 1, 2], max at index 1 (1-based)
	var tc = findCase( 'first_max' );
	var x = new Float64Array( [ 100.0, 1.0, 2.0 ] );
	var result = idamax( 3, x, 1, 0 );
	assert.strictEqual( result, tc.result - 1 );
});

test( 'idamax: last element is max', function t() {
	// dx = [1, 2, 100], max at index 3 (1-based)
	var tc = findCase( 'last_max' );
	var x = new Float64Array( [ 1.0, 2.0, 100.0 ] );
	var result = idamax( 3, x, 1, 0 );
	assert.strictEqual( result, tc.result - 1 );
});

test( 'idamax: negative N returns -1', function t() {
	var x = new Float64Array( [ 1.0, 2.0, 3.0 ] );
	var result = idamax( -1, x, 1, 0 );
	assert.strictEqual( result, -1 );
});

test( 'idamax: stride <= 0 returns -1', function t() {
	var x = new Float64Array( [ 1.0, 2.0, 3.0 ] );
	assert.strictEqual( idamax( 3, x, 0, 0 ), -1 );
	assert.strictEqual( idamax( 3, x, -1, 0 ), -1 );
});

test( 'idamax: offset parameter works', function t() {
	// x = [99, 1, 10, 2], offset=1, N=3, stride=1 => elements 1, 10, 2
	var x = new Float64Array( [ 99.0, 1.0, 10.0, 2.0 ] );
	var result = idamax( 3, x, 1, 1 );
	assert.strictEqual( result, 1 ); // 0-based index within the N elements
});

test( 'idamax: all equal elements returns 0', function t() {
	var x = new Float64Array( [ 5.0, 5.0, 5.0, 5.0 ] );
	var result = idamax( 4, x, 1, 0 );
	assert.strictEqual( result, 0 );
});
