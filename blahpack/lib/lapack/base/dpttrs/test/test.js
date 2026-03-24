

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Float64Array = require( '@stdlib/array/float64' );
var dpttrs = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dpttrs.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'dpttrs: basic_5x5_single_rhs', function t() {
	var tc = findCase( 'basic_5x5_single_rhs' );
	var d = new Float64Array( [ 4.0, 3.0, 2.0, 3.0, 4.0 ] );
	var e = new Float64Array( [ 0.5, -0.5, 0.25, -0.25 ] );
	var b = new Float64Array( [ 8.0, 5.5, 7.25, 10.25, 17.9375 ] );
	var info;

	info = dpttrs( 5, 1, d, 1, 0, e, 1, 0, b, 1, 5, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( b, new Float64Array( tc.x ), 1e-14, 'x' );
});

test( 'dpttrs: multi_rhs_3', function t() {
	var tc = findCase( 'multi_rhs_3' );
	var d = new Float64Array( [ 4.0, 3.0, 2.0, 3.0, 4.0 ] );
	var e = new Float64Array( [ 0.5, -0.5, 0.25, -0.25 ] );
	// Column-major: LDB=5, NRHS=3
	var b = new Float64Array( [
		8.0, 5.5, 7.25, 10.25, 17.9375,
		28.0, 21.5, 3.25, 7.0, 2.6875,
		8.0, 5.5, -1.25, 8.125, 1.9375
	] );
	var info;

	info = dpttrs( 5, 3, d, 1, 0, e, 1, 0, b, 1, 5, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( b, new Float64Array( tc.x ), 1e-14, 'x' );
});

test( 'dpttrs: n_eq_1', function t() {
	var tc = findCase( 'n_eq_1' );
	var d = new Float64Array( [ 3.0 ] );
	var e = new Float64Array( [] );
	var b = new Float64Array( [ 9.0 ] );
	var info;

	info = dpttrs( 1, 1, d, 1, 0, e, 1, 0, b, 1, 1, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( b, new Float64Array( tc.x ), 1e-14, 'x' );
});

test( 'dpttrs: n_eq_0', function t() {
	var tc = findCase( 'n_eq_0' );
	var d = new Float64Array( [] );
	var e = new Float64Array( [] );
	var b = new Float64Array( [ 42.0 ] );
	var info;

	info = dpttrs( 0, 1, d, 1, 0, e, 1, 0, b, 1, 1, 0 );

	assert.equal( info, tc.info, 'info' );
	// b should be unchanged
	assert.equal( b[ 0 ], 42.0, 'b unchanged' );
});

test( 'dpttrs: nrhs_eq_0', function t() {
	var tc = findCase( 'nrhs_eq_0' );
	var d = new Float64Array( [ 4.0, 3.0 ] );
	var e = new Float64Array( [ 0.5 ] );
	var b = new Float64Array( [ 42.0 ] );
	var info;

	info = dpttrs( 2, 0, d, 1, 0, e, 1, 0, b, 1, 2, 0 );

	assert.equal( info, tc.info, 'info' );
	// b should be unchanged
	assert.equal( b[ 0 ], 42.0, 'b unchanged' );
});

test( 'dpttrs: non-unit strides and offsets', function t() {
	// Same as basic 5x5 single RHS but with offset=1 on d and e
	var d = new Float64Array( [ 0.0, 4.0, 3.0, 2.0, 3.0, 4.0 ] );
	var e = new Float64Array( [ 0.0, 0.5, -0.5, 0.25, -0.25 ] );
	var b = new Float64Array( [ 8.0, 5.5, 7.25, 10.25, 17.9375 ] );
	var expected = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0 ] );
	var info;

	info = dpttrs( 5, 1, d, 1, 1, e, 1, 1, b, 1, 5, 0 );

	assert.equal( info, 0, 'info' );
	assertArrayClose( b, expected, 1e-14, 'x' );
});
