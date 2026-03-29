/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';


// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var idamax = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'idamax.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
var fixture = lines.map( function parse( line ) {
	return JSON.parse( line );
} );


// FUNCTIONS //

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


// TESTS //

test( 'idamax: main export is a function', function t() {
	assert.strictEqual( typeof idamax, 'function' );
});

test( 'idamax: basic (N=5, stride=1)', function t() {
	var result;
	var tc;
	var x;

	tc = findCase( 'basic' );
	x = new Float64Array( [ 1.0, -3.0, 2.0, 5.0, -4.0 ] );
	result = idamax( 5, x, 1, 0 );
	assert.strictEqual( result, tc.result - 1 );
});

test( 'idamax: n_zero returns -1', function t() {
	var result;
	var x;

	x = new Float64Array( [ 1.0, 2.0, 3.0 ] );
	result = idamax( 0, x, 1, 0 );
	assert.strictEqual( result, -1 );
});

test( 'idamax: n_one returns 0', function t() {
	var result;
	var x;

	x = new Float64Array( [ 42.0 ] );
	result = idamax( 1, x, 1, 0 );
	assert.strictEqual( result, 0 );
});

test( 'idamax: negative values (N=4, stride=1)', function t() {
	var result;
	var tc;
	var x;

	tc = findCase( 'negative' );
	x = new Float64Array( [ -2.0, -7.0, -1.0, -3.0 ] );
	result = idamax( 4, x, 1, 0 );
	assert.strictEqual( result, tc.result - 1 );
});

test( 'idamax: non-unit stride (N=3, stride=2)', function t() {
	var result;
	var tc;
	var x;

	tc = findCase( 'stride' );
	x = new Float64Array( [ 1.0, 99.0, 10.0, 99.0, 2.0 ] );
	result = idamax( 3, x, 2, 0 );
	assert.strictEqual( result, tc.result - 1 );
});

test( 'idamax: first element is max', function t() {
	var result;
	var tc;
	var x;

	tc = findCase( 'first_max' );
	x = new Float64Array( [ 100.0, 1.0, 2.0 ] );
	result = idamax( 3, x, 1, 0 );
	assert.strictEqual( result, tc.result - 1 );
});

test( 'idamax: last element is max', function t() {
	var result;
	var tc;
	var x;

	tc = findCase( 'last_max' );
	x = new Float64Array( [ 1.0, 2.0, 100.0 ] );
	result = idamax( 3, x, 1, 0 );
	assert.strictEqual( result, tc.result - 1 );
});

test( 'idamax: negative N returns -1', function t() {
	var result;
	var x;

	x = new Float64Array( [ 1.0, 2.0, 3.0 ] );
	result = idamax( -1, x, 1, 0 );
	assert.strictEqual( result, -1 );
});

test( 'idamax: stride <= 0 returns -1', function t() {
	var x = new Float64Array( [ 1.0, 2.0, 3.0 ] );
	assert.strictEqual( idamax( 3, x, 0, 0 ), -1 );
	assert.strictEqual( idamax( 3, x, -1, 0 ), -1 );
});

test( 'idamax: offset parameter works', function t() {
	var result;
	var x;

	x = new Float64Array( [ 99.0, 1.0, 10.0, 2.0 ] );
	result = idamax( 3, x, 1, 1 );
	assert.strictEqual( result, 1 );
});

test( 'idamax: all equal elements returns 0', function t() {
	var result;
	var x;

	x = new Float64Array( [ 5.0, 5.0, 5.0, 5.0 ] );
	result = idamax( 4, x, 1, 0 );
	assert.strictEqual( result, 0 );
});
