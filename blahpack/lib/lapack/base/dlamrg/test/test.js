'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dlamrg = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlamrg.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}


// TESTS //

test( 'dlamrg: both forward, 3+3', function t() {
	var tc = findCase( 'fwd_fwd_3_3' );
	var a = new Float64Array( [ 1.0, 3.0, 5.0, 2.0, 4.0, 6.0 ] );
	var idx = new Int32Array( 6 );
	dlamrg( 3, 3, a, 1, 0, 1, 1, idx, 1, 0 );
	var i;
	for ( i = 0; i < 6; i++ ) {
		assert.strictEqual( idx[ i ], tc.index[ i ], 'index[' + i + ']' );
	}
});

test( 'dlamrg: first forward, second backward, 3+3', function t() {
	var tc = findCase( 'fwd_bwd_3_3' );
	var a = new Float64Array( [ 1.0, 3.0, 5.0, 6.0, 4.0, 2.0 ] );
	var idx = new Int32Array( 6 );
	dlamrg( 3, 3, a, 1, 0, 1, -1, idx, 1, 0 );
	var i;
	for ( i = 0; i < 6; i++ ) {
		assert.strictEqual( idx[ i ], tc.index[ i ], 'index[' + i + ']' );
	}
});

test( 'dlamrg: first backward, second forward, 3+2', function t() {
	var tc = findCase( 'bwd_fwd_3_2' );
	var a = new Float64Array( [ 5.0, 3.0, 1.0, 2.0, 4.0 ] );
	var idx = new Int32Array( 5 );
	dlamrg( 3, 2, a, 1, 0, -1, 1, idx, 1, 0 );
	var i;
	for ( i = 0; i < 5; i++ ) {
		assert.strictEqual( idx[ i ], tc.index[ i ], 'index[' + i + ']' );
	}
});

test( 'dlamrg: both backward, 2+3', function t() {
	var tc = findCase( 'bwd_bwd_2_3' );
	var a = new Float64Array( [ 4.0, 2.0, 6.0, 3.0, 1.0 ] );
	var idx = new Int32Array( 5 );
	dlamrg( 2, 3, a, 1, 0, -1, -1, idx, 1, 0 );
	var i;
	for ( i = 0; i < 5; i++ ) {
		assert.strictEqual( idx[ i ], tc.index[ i ], 'index[' + i + ']' );
	}
});

test( 'dlamrg: n1=1, n1=1', function t() {
	var tc = findCase( 'n1_n1' );
	var a = new Float64Array( [ 3.0, 1.0 ] );
	var idx = new Int32Array( 2 );
	dlamrg( 1, 1, a, 1, 0, 1, 1, idx, 1, 0 );
	var i;
	for ( i = 0; i < 2; i++ ) {
		assert.strictEqual( idx[ i ], tc.index[ i ], 'index[' + i + ']' );
	}
});

test( 'dlamrg: equal elements', function t() {
	var tc = findCase( 'equal' );
	var a = new Float64Array( [ 2.0, 2.0, 2.0, 2.0 ] );
	var idx = new Int32Array( 4 );
	dlamrg( 2, 2, a, 1, 0, 1, 1, idx, 1, 0 );
	var i;
	for ( i = 0; i < 4; i++ ) {
		assert.strictEqual( idx[ i ], tc.index[ i ], 'index[' + i + ']' );
	}
});

test( 'dlamrg: n1=4, n2=1', function t() {
	var tc = findCase( 'n4_n1' );
	var a = new Float64Array( [ 1.0, 3.0, 5.0, 7.0, 4.0 ] );
	var idx = new Int32Array( 5 );
	dlamrg( 4, 1, a, 1, 0, 1, 1, idx, 1, 0 );
	var i;
	for ( i = 0; i < 5; i++ ) {
		assert.strictEqual( idx[ i ], tc.index[ i ], 'index[' + i + ']' );
	}
});
