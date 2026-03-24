

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dpoequ = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dpoequ.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'dpoequ: basic', function t() {
	var tc = findCase( 'basic' );
	// 3x3 SPD matrix, column-major
	var A = new Float64Array([ 4.0, 1.0, 0.5, 1.0, 9.0, 1.0, 0.5, 1.0, 16.0 ]);
	var s = new Float64Array( 3 );
	var result = dpoequ( 3, A, 1, 3, 0, s, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
	assertClose( result.scond, tc.scond, 1e-14, 'scond' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
	assertArrayClose( Array.from( s ), tc.s, 1e-14, 's' );
});

test( 'dpoequ: diagonal_varied', function t() {
	var tc = findCase( 'diagonal_varied' );
	// 3x3 diagonal matrix, column-major
	var A = new Float64Array([ 100.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.25 ]);
	var s = new Float64Array( 3 );
	var result = dpoequ( 3, A, 1, 3, 0, s, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
	assertClose( result.scond, tc.scond, 1e-14, 'scond' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
	assertArrayClose( Array.from( s ), tc.s, 1e-14, 's' );
});

test( 'dpoequ: non_positive_diag', function t() {
	var tc = findCase( 'non_positive_diag' );
	// 3x3 diagonal with negative element at position 2
	var A = new Float64Array([ 4.0, 0.0, 0.0, 0.0, -1.0, 0.0, 0.0, 0.0, 9.0 ]);
	var s = new Float64Array( 3 );
	var result = dpoequ( 3, A, 1, 3, 0, s, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
});

test( 'dpoequ: zero_diag', function t() {
	var tc = findCase( 'zero_diag' );
	// 3x3 diagonal with zero element at position 2
	var A = new Float64Array([ 4.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 9.0 ]);
	var s = new Float64Array( 3 );
	var result = dpoequ( 3, A, 1, 3, 0, s, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
});

test( 'dpoequ: n_zero', function t() {
	var tc = findCase( 'n_zero' );
	var A = new Float64Array( 1 );
	var s = new Float64Array( 1 );
	var result = dpoequ( 0, A, 1, 1, 0, s, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
	assertClose( result.scond, tc.scond, 1e-14, 'scond' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
});

test( 'dpoequ: n_one', function t() {
	var tc = findCase( 'n_one' );
	var A = new Float64Array([ 25.0 ]);
	var s = new Float64Array( 1 );
	var result = dpoequ( 1, A, 1, 1, 0, s, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
	assertClose( result.scond, tc.scond, 1e-14, 'scond' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
	assertArrayClose( Array.from( s ), tc.s, 1e-14, 's' );
});

test( 'dpoequ: identity', function t() {
	var tc = findCase( 'identity' );
	var A = new Float64Array([ 1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0 ]);
	var s = new Float64Array( 3 );
	var result = dpoequ( 3, A, 1, 3, 0, s, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
	assertClose( result.scond, tc.scond, 1e-14, 'scond' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
	assertArrayClose( Array.from( s ), tc.s, 1e-14, 's' );
});
