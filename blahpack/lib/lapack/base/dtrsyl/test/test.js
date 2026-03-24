

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dtrsyl = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dtrsyl.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'dtrsyl: NN basic 2x2', function t() {
	var tc = findCase( 'NN basic 2x2' );
	// TODO: set up inputs and call dtrsyl(...)
	// assertArrayClose( result, tc.C, 1e-14, 'C' );
	// assertClose( result, tc.info, 1e-14, 'info' );
	// assertClose( result, tc.scale, 1e-14, 'scale' );
});

test( 'dtrsyl: NN isgn=-1', function t() {
	var tc = findCase( 'NN isgn=-1' );
	// TODO: set up inputs and call dtrsyl(...)
	// assertArrayClose( result, tc.C, 1e-14, 'C' );
	// assertClose( result, tc.info, 1e-14, 'info' );
	// assertClose( result, tc.scale, 1e-14, 'scale' );
});

test( 'dtrsyl: TN basic', function t() {
	var tc = findCase( 'TN basic' );
	// TODO: set up inputs and call dtrsyl(...)
	// assertArrayClose( result, tc.C, 1e-14, 'C' );
	// assertClose( result, tc.info, 1e-14, 'info' );
	// assertClose( result, tc.scale, 1e-14, 'scale' );
});

test( 'dtrsyl: TT basic', function t() {
	var tc = findCase( 'TT basic' );
	// TODO: set up inputs and call dtrsyl(...)
	// assertArrayClose( result, tc.C, 1e-14, 'C' );
	// assertClose( result, tc.info, 1e-14, 'info' );
	// assertClose( result, tc.scale, 1e-14, 'scale' );
});

test( 'dtrsyl: NT basic', function t() {
	var tc = findCase( 'NT basic' );
	// TODO: set up inputs and call dtrsyl(...)
	// assertArrayClose( result, tc.C, 1e-14, 'C' );
	// assertClose( result, tc.info, 1e-14, 'info' );
	// assertClose( result, tc.scale, 1e-14, 'scale' );
});

test( 'dtrsyl: M=0', function t() {
	var tc = findCase( 'M=0' );
	// TODO: set up inputs and call dtrsyl(...)
	// assertClose( result, tc.info, 1e-14, 'info' );
	// assertClose( result, tc.scale, 1e-14, 'scale' );
});

test( 'dtrsyl: N=0', function t() {
	var tc = findCase( 'N=0' );
	// TODO: set up inputs and call dtrsyl(...)
	// assertClose( result, tc.info, 1e-14, 'info' );
	// assertClose( result, tc.scale, 1e-14, 'scale' );
});

test( 'dtrsyl: NN 3x3 quasi-tri', function t() {
	var tc = findCase( 'NN 3x3 quasi-tri' );
	// TODO: set up inputs and call dtrsyl(...)
	// assertArrayClose( result, tc.C, 1e-14, 'C' );
	// assertClose( result, tc.info, 1e-14, 'info' );
	// assertClose( result, tc.scale, 1e-14, 'scale' );
});

test( 'dtrsyl: M=1 N=1', function t() {
	var tc = findCase( 'M=1 N=1' );
	// TODO: set up inputs and call dtrsyl(...)
	// assertArrayClose( result, tc.C, 1e-14, 'C' );
	// assertClose( result, tc.info, 1e-14, 'info' );
	// assertClose( result, tc.scale, 1e-14, 'scale' );
});

test( 'dtrsyl: TN 3x3 quasi-tri', function t() {
	var tc = findCase( 'TN 3x3 quasi-tri' );
	// TODO: set up inputs and call dtrsyl(...)
	// assertArrayClose( result, tc.C, 1e-14, 'C' );
	// assertClose( result, tc.info, 1e-14, 'info' );
	// assertClose( result, tc.scale, 1e-14, 'scale' );
});

test( 'dtrsyl: TT 3x3 quasi-tri', function t() {
	var tc = findCase( 'TT 3x3 quasi-tri' );
	// TODO: set up inputs and call dtrsyl(...)
	// assertArrayClose( result, tc.C, 1e-14, 'C' );
	// assertClose( result, tc.info, 1e-14, 'info' );
	// assertClose( result, tc.scale, 1e-14, 'scale' );
});

test( 'dtrsyl: NT 3x3 quasi-tri', function t() {
	var tc = findCase( 'NT 3x3 quasi-tri' );
	// TODO: set up inputs and call dtrsyl(...)
	// assertArrayClose( result, tc.C, 1e-14, 'C' );
	// assertClose( result, tc.info, 1e-14, 'info' );
	// assertClose( result, tc.scale, 1e-14, 'scale' );
});

