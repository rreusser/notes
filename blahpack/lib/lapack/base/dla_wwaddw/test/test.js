

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dla_wwaddw = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dla_wwaddw.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'dla_wwaddw is a function', function t() {
	assert.equal( typeof dla_wwaddw, 'function' );
});

test( 'dla_wwaddw: basic', function t() {
	var tc = findCase( 'basic' );
	var x = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0 ] );
	var y = new Float64Array( [ 0.1, 0.2, 0.3, 0.4, 0.5 ] );
	var w = new Float64Array( [ 10.0, 20.0, 30.0, 40.0, 50.0 ] );
	dla_wwaddw( 5, x, 1, 0, y, 1, 0, w, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
	assertArrayClose( y, tc.y, 1e-14, 'y' );
});

test( 'dla_wwaddw: n_zero', function t() {
	var x = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0 ] );
	var y = new Float64Array( [ 0.1, 0.2, 0.3, 0.4, 0.5 ] );
	var w = new Float64Array( [ 10.0, 20.0, 30.0, 40.0, 50.0 ] );
	dla_wwaddw( 0, x, 1, 0, y, 1, 0, w, 1, 0 );

	// Arrays unchanged when N=0
	var tc = findCase( 'n_zero' );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
	assertArrayClose( y, tc.y, 1e-14, 'y' );
});

test( 'dla_wwaddw: n_one', function t() {
	var tc = findCase( 'n_one' );
	var x = new Float64Array( [ 1.0 ] );
	var y = new Float64Array( [ 0.1 ] );
	var w = new Float64Array( [ 10.0 ] );
	dla_wwaddw( 1, x, 1, 0, y, 1, 0, w, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
	assertArrayClose( y, tc.y, 1e-14, 'y' );
});

test( 'dla_wwaddw: negative', function t() {
	var tc = findCase( 'negative' );
	var x = new Float64Array( [ -1.0, -2.0, -3.0, -4.0 ] );
	var y = new Float64Array( [ 0.01, -0.02, 0.03, -0.04 ] );
	var w = new Float64Array( [ 0.5, -0.5, 1.5, -1.5 ] );
	dla_wwaddw( 4, x, 1, 0, y, 1, 0, w, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
	assertArrayClose( y, tc.y, 1e-14, 'y' );
});

test( 'dla_wwaddw: large_values', function t() {
	var tc = findCase( 'large_values' );
	var x = new Float64Array( [ 1.0e15, 2.0e15, 3.0e15 ] );
	var y = new Float64Array( [ 1.0, 2.0, 3.0 ] );
	var w = new Float64Array( [ 1.0, 2.0, 3.0 ] );
	dla_wwaddw( 3, x, 1, 0, y, 1, 0, w, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
	assertArrayClose( y, tc.y, 1e-14, 'y' );
});

test( 'dla_wwaddw: zeros', function t() {
	var tc = findCase( 'zeros' );
	var x = new Float64Array( [ 0.0, 0.0, 0.0 ] );
	var y = new Float64Array( [ 0.0, 0.0, 0.0 ] );
	var w = new Float64Array( [ 0.0, 0.0, 0.0 ] );
	dla_wwaddw( 3, x, 1, 0, y, 1, 0, w, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
	assertArrayClose( y, tc.y, 1e-14, 'y' );
});

test( 'dla_wwaddw: non-unit strides', function t() {
	// Test with stride=2 and offset=1
	var x = new Float64Array( [ 999.0, 1.0, 999.0, 2.0, 999.0, 3.0 ] );
	var y = new Float64Array( [ 999.0, 0.1, 999.0, 0.2, 999.0, 0.3 ] );
	var w = new Float64Array( [ 999.0, 10.0, 999.0, 20.0, 999.0, 30.0 ] );

	// Same inputs as basic[0:3], should produce same results
	var tcBasic = findCase( 'basic' );
	dla_wwaddw( 3, x, 2, 1, y, 2, 1, w, 2, 1 );

	assertClose( x[ 1 ], tcBasic.x[ 0 ], 1e-14, 'x[1]' );
	assertClose( x[ 3 ], tcBasic.x[ 1 ], 1e-14, 'x[3]' );
	assertClose( x[ 5 ], tcBasic.x[ 2 ], 1e-14, 'x[5]' );
	assertClose( y[ 1 ], tcBasic.y[ 0 ], 1e-14, 'y[1]' );
	assertClose( y[ 3 ], tcBasic.y[ 1 ], 1e-14, 'y[3]' );
	assertClose( y[ 5 ], tcBasic.y[ 2 ], 1e-14, 'y[5]' );

	// Untouched elements should remain
	assert.equal( x[ 0 ], 999.0 );
	assert.equal( x[ 2 ], 999.0 );
	assert.equal( x[ 4 ], 999.0 );
});

test( 'dla_wwaddw: negative strides', function t() {
	// Test with negative stride
	var x = new Float64Array( [ 3.0, 2.0, 1.0 ] );
	var y = new Float64Array( [ 0.3, 0.2, 0.1 ] );
	var w = new Float64Array( [ 30.0, 20.0, 10.0 ] );

	var tcBasic = findCase( 'basic' );
	dla_wwaddw( 3, x, -1, 2, y, -1, 2, w, -1, 2 );

	// Negative stride means we start at offset=2 and go backwards
	// Element at offset 2 is x[2]=1.0, w[2]=10.0 => same as basic[0]
	assertClose( x[ 2 ], tcBasic.x[ 0 ], 1e-14, 'x[2]' );
	assertClose( x[ 1 ], tcBasic.x[ 1 ], 1e-14, 'x[1]' );
	assertClose( x[ 0 ], tcBasic.x[ 2 ], 1e-14, 'x[0]' );
	assertClose( y[ 2 ], tcBasic.y[ 0 ], 1e-14, 'y[2]' );
	assertClose( y[ 1 ], tcBasic.y[ 1 ], 1e-14, 'y[1]' );
	assertClose( y[ 0 ], tcBasic.y[ 2 ], 1e-14, 'y[0]' );
});
