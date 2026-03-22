

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var ztgevc = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'ztgevc.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'ztgevc: right_all', function t() {
	var tc = findCase( 'right_all' );
	// TODO: set up inputs and call ztgevc(...)
	// assertClose( result, tc.info, 1e-14, 'info' );
	// assertClose( result, tc.m, 1e-14, 'm' );
	// assertArrayClose( result, tc.vr, 1e-14, 'vr' );
});

test( 'ztgevc: left_all', function t() {
	var tc = findCase( 'left_all' );
	// TODO: set up inputs and call ztgevc(...)
	// assertClose( result, tc.info, 1e-14, 'info' );
	// assertClose( result, tc.m, 1e-14, 'm' );
	// assertArrayClose( result, tc.vl, 1e-14, 'vl' );
});

test( 'ztgevc: both_all', function t() {
	var tc = findCase( 'both_all' );
	// TODO: set up inputs and call ztgevc(...)
	// assertClose( result, tc.info, 1e-14, 'info' );
	// assertClose( result, tc.m, 1e-14, 'm' );
	// assertArrayClose( result, tc.vl, 1e-14, 'vl' );
	// assertArrayClose( result, tc.vr, 1e-14, 'vr' );
});

test( 'ztgevc: right_selected', function t() {
	var tc = findCase( 'right_selected' );
	// TODO: set up inputs and call ztgevc(...)
	// assertClose( result, tc.info, 1e-14, 'info' );
	// assertClose( result, tc.m, 1e-14, 'm' );
	// assertArrayClose( result, tc.vr, 1e-14, 'vr' );
});

test( 'ztgevc: right_backtransform', function t() {
	var tc = findCase( 'right_backtransform' );
	// TODO: set up inputs and call ztgevc(...)
	// assertClose( result, tc.info, 1e-14, 'info' );
	// assertClose( result, tc.m, 1e-14, 'm' );
	// assertArrayClose( result, tc.vr, 1e-14, 'vr' );
});

test( 'ztgevc: n_zero', function t() {
	var tc = findCase( 'n_zero' );
	// TODO: set up inputs and call ztgevc(...)
	// assertClose( result, tc.info, 1e-14, 'info' );
	// assertClose( result, tc.m, 1e-14, 'm' );
});

test( 'ztgevc: both_2x2', function t() {
	var tc = findCase( 'both_2x2' );
	// TODO: set up inputs and call ztgevc(...)
	// assertClose( result, tc.info, 1e-14, 'info' );
	// assertClose( result, tc.m, 1e-14, 'm' );
	// assertArrayClose( result, tc.vl, 1e-14, 'vl' );
	// assertArrayClose( result, tc.vr, 1e-14, 'vr' );
});

