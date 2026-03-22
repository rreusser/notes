'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var iladlr = require( './../lib/base.js' );

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'iladlr.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function expected( tc ) {
	return tc.result - 1;
}

test( 'iladlr: basic 3x4 diagonal', function t() {
	var tc = findCase( 'basic_3x4' );
	var A = new Float64Array( 4 * 4 );
	A[ 0 + 0 * 4 ] = 1.0;
	A[ 1 + 1 * 4 ] = 2.0;
	A[ 2 + 2 * 4 ] = 3.0;
	assert.strictEqual( iladlr( 3, 4, A, 1, 4, 0 ), expected( tc ) );
});

test( 'iladlr: all zeros', function t() {
	var tc = findCase( 'all_zeros' );
	var A = new Float64Array( 4 * 4 );
	assert.strictEqual( iladlr( 3, 4, A, 1, 4, 0 ), expected( tc ) );
});

test( 'iladlr: last row non-zero', function t() {
	var tc = findCase( 'last_row_nonzero' );
	var A = new Float64Array( 4 * 4 );
	A[ 2 + 0 * 4 ] = 5.0;
	assert.strictEqual( iladlr( 3, 4, A, 1, 4, 0 ), expected( tc ) );
});

test( 'iladlr: first row only', function t() {
	var tc = findCase( 'first_row_only' );
	var A = new Float64Array( 4 * 4 );
	A[ 0 + 0 * 4 ] = 1.0;
	A[ 0 + 2 * 4 ] = 2.0;
	assert.strictEqual( iladlr( 3, 4, A, 1, 4, 0 ), expected( tc ) );
});

test( 'iladlr: M=0', function t() {
	var tc = findCase( 'm_zero' );
	var A = new Float64Array( 4 * 4 );
	assert.strictEqual( iladlr( 0, 4, A, 1, 4, 0 ), expected( tc ) );
});

test( 'iladlr: bottom right corner', function t() {
	var tc = findCase( 'bottom_right' );
	var A = new Float64Array( 4 * 4 );
	A[ 2 + 3 * 4 ] = 9.0;
	assert.strictEqual( iladlr( 3, 4, A, 1, 4, 0 ), expected( tc ) );
});
