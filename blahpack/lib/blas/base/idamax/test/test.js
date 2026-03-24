

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

test( 'idamax: basic', function t() {
	var tc = findCase( 'basic' );
	// TODO: set up inputs and call idamax(...)
	// assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'idamax: n_zero', function t() {
	var tc = findCase( 'n_zero' );
	// TODO: set up inputs and call idamax(...)
	// assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'idamax: n_one', function t() {
	var tc = findCase( 'n_one' );
	// TODO: set up inputs and call idamax(...)
	// assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'idamax: negative', function t() {
	var tc = findCase( 'negative' );
	// TODO: set up inputs and call idamax(...)
	// assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'idamax: stride', function t() {
	var tc = findCase( 'stride' );
	// TODO: set up inputs and call idamax(...)
	// assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'idamax: first_max', function t() {
	var tc = findCase( 'first_max' );
	// TODO: set up inputs and call idamax(...)
	// assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'idamax: last_max', function t() {
	var tc = findCase( 'last_max' );
	// TODO: set up inputs and call idamax(...)
	// assertClose( result, tc.result, 1e-14, 'result' );
});

