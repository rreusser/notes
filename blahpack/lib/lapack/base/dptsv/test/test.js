'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dptsv = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dptsv.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'dptsv: basic_5x5_single_rhs', function t() {
	var tc = findCase( 'basic_5x5_single_rhs' );
	var d = new Float64Array( [ 4.0, 4.0, 4.0, 4.0, 4.0 ] );
	var e = new Float64Array( [ -1.0, -1.0, -1.0, -1.0 ] );
	var B = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0 ] );
	var info = dptsv( 5, 1, d, 1, 0, e, 1, 0, B, 1, 5, 0 );

	assert.equal( info, tc.info );
	assertArrayClose( Array.from( d ), tc.d, 1e-14, 'd' );
	assertArrayClose( Array.from( e ), tc.e, 1e-14, 'e' );
	assertArrayClose( Array.from( B ), tc.b, 1e-14, 'b' );
});

test( 'dptsv: multi_rhs', function t() {
	var tc = findCase( 'multi_rhs' );
	// Column-major: B is 4x2, stored column-major with stride1=1, stride2=4
	var d = new Float64Array( [ 3.0, 3.0, 3.0, 3.0 ] );
	var e = new Float64Array( [ 1.0, 1.0, 1.0 ] );
	var B = new Float64Array( [
		1.0, 0.0, 0.0, 1.0,  // column 1
		2.0, 1.0, 1.0, 2.0   // column 2
	] );
	var info = dptsv( 4, 2, d, 1, 0, e, 1, 0, B, 1, 4, 0 );

	assert.equal( info, tc.info );
	assertArrayClose( Array.from( d ), tc.d, 1e-14, 'd' );
	assertArrayClose( Array.from( e ), tc.e, 1e-14, 'e' );
	assertArrayClose( Array.from( B.subarray( 0, 4 ) ), tc.b1, 1e-14, 'b1' );
	assertArrayClose( Array.from( B.subarray( 4, 8 ) ), tc.b2, 1e-14, 'b2' );
});

test( 'dptsv: n_one', function t() {
	var tc = findCase( 'n_one' );
	var d = new Float64Array( [ 5.0 ] );
	var e = new Float64Array( 0 );
	var B = new Float64Array( [ 10.0 ] );
	var info = dptsv( 1, 1, d, 1, 0, e, 1, 0, B, 1, 1, 0 );

	assert.equal( info, tc.info );
	assertArrayClose( Array.from( d ), tc.d, 1e-14, 'd' );
	assertArrayClose( Array.from( B ), tc.b, 1e-14, 'b' );
});

test( 'dptsv: n_zero (quick return)', function t() {
	var tc = findCase( 'n_zero' );
	var d = new Float64Array( 0 );
	var e = new Float64Array( 0 );
	var B = new Float64Array( 0 );
	var info = dptsv( 0, 1, d, 1, 0, e, 1, 0, B, 1, 1, 0 );

	assert.equal( info, tc.info );
});

test( 'dptsv: not_posdef (returns INFO > 0)', function t() {
	var tc = findCase( 'not_posdef' );
	var d = new Float64Array( [ -1.0, 4.0, 4.0 ] );
	var e = new Float64Array( [ 1.0, 1.0 ] );
	var B = new Float64Array( [ 1.0, 2.0, 3.0 ] );
	var info = dptsv( 3, 1, d, 1, 0, e, 1, 0, B, 1, 3, 0 );

	assert.equal( info, tc.info );
});

test( 'dptsv: nrhs=0 (quick return)', function t() {
	var d = new Float64Array( [ 4.0, 4.0 ] );
	var e = new Float64Array( [ -1.0 ] );
	var B = new Float64Array( 0 );
	var info = dptsv( 2, 0, d, 1, 0, e, 1, 0, B, 1, 2, 0 );

	assert.equal( info, 0 );
	// d and e should be unchanged since we returned early
	assert.equal( d[ 0 ], 4.0 );
	assert.equal( d[ 1 ], 4.0 );
	assert.equal( e[ 0 ], -1.0 );
});

test( 'dptsv: non-unit strides with offset', function t() {
	// Use stride=2 and offset=1 to test non-contiguous access
	var d = new Float64Array( [ 0.0, 4.0, 0.0, 4.0, 0.0, 4.0 ] );
	var e = new Float64Array( [ 0.0, -1.0, 0.0, -1.0 ] );
	var B = new Float64Array( [ 0.0, 1.0, 0.0, 2.0, 0.0, 3.0 ] );
	var info = dptsv( 3, 1, d, 2, 1, e, 2, 1, B, 2, 6, 1 );

	assert.equal( info, 0 );
	// Verify solution matches unit-stride result
	var dRef = new Float64Array( [ 4.0, 4.0, 4.0 ] );
	var eRef = new Float64Array( [ -1.0, -1.0 ] );
	var BRef = new Float64Array( [ 1.0, 2.0, 3.0 ] );
	dptsv( 3, 1, dRef, 1, 0, eRef, 1, 0, BRef, 1, 3, 0 );
	assertClose( B[ 1 ], BRef[ 0 ], 1e-14, 'b[0]' );
	assertClose( B[ 3 ], BRef[ 1 ], 1e-14, 'b[1]' );
	assertClose( B[ 5 ], BRef[ 2 ], 1e-14, 'b[2]' );
});
