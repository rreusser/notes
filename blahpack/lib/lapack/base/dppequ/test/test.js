'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var Float64Array = require( '@stdlib/array/float64' );
var path = require( 'path' );
var dppequ = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dppequ.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'dppequ is a function', function t() {
	assert.equal( typeof dppequ, 'function' );
});

test( 'dppequ: upper_basic', function t() {
	var result;
	var tc = findCase( 'upper_basic' );

	// Upper packed: col 1 = [4], col 2 = [1, 9], col 3 = [0.5, 2, 16], col 4 = [0.3, 1.5, 3, 25]
	var ap = new Float64Array( [ 4.0, 1.0, 9.0, 0.5, 2.0, 16.0, 0.3, 1.5, 3.0, 25.0 ] );
	var s = new Float64Array( 4 );

	result = dppequ( 'upper', 4, ap, 1, 0, s, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
	assertClose( result.scond, tc.scond, 1e-14, 'scond' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
	assertArrayClose( Array.from( s ), tc.s, 1e-14, 's' );
});

test( 'dppequ: lower_basic', function t() {
	var result;
	var tc = findCase( 'lower_basic' );

	// Lower packed: col 1 = [4, 1, 0.5, 0.3], col 2 = [9, 2, 1.5], col 3 = [16, 3], col 4 = [25]
	var ap = new Float64Array( [ 4.0, 1.0, 0.5, 0.3, 9.0, 2.0, 1.5, 16.0, 3.0, 25.0 ] );
	var s = new Float64Array( 4 );

	result = dppequ( 'lower', 4, ap, 1, 0, s, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
	assertClose( result.scond, tc.scond, 1e-14, 'scond' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
	assertArrayClose( Array.from( s ), tc.s, 1e-14, 's' );
});

test( 'dppequ: n_zero', function t() {
	var result;
	var tc = findCase( 'n_zero' );
	var ap = new Float64Array( 1 );
	var s = new Float64Array( 1 );

	result = dppequ( 'upper', 0, ap, 1, 0, s, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
	assertClose( result.scond, tc.scond, 1e-14, 'scond' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
});

test( 'dppequ: n_one', function t() {
	var result;
	var tc = findCase( 'n_one' );
	var ap = new Float64Array( [ 49.0 ] );
	var s = new Float64Array( 1 );

	result = dppequ( 'upper', 1, ap, 1, 0, s, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
	assertClose( result.scond, tc.scond, 1e-14, 'scond' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
	assertArrayClose( Array.from( s ), tc.s, 1e-14, 's' );
});

test( 'dppequ: non_positive_upper', function t() {
	var result;
	var tc = findCase( 'non_positive_upper' );

	// Upper packed N=3: diag at 0, 2, 5
	// Values: 4.0, -1.0, 9.0 at diagonal positions
	var ap = new Float64Array( [ 4.0, 1.0, -1.0, 0.5, 2.0, 9.0 ] );
	var s = new Float64Array( 3 );

	result = dppequ( 'upper', 3, ap, 1, 0, s, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
});

test( 'dppequ: zero_diag_lower', function t() {
	var result;
	var tc = findCase( 'zero_diag_lower' );

	// Lower packed N=3: diag at 0, 3, 5
	// Diagonal: 4.0, 0.0, 9.0
	var ap = new Float64Array( [ 4.0, 1.0, 0.5, 0.0, 2.0, 9.0 ] );
	var s = new Float64Array( 3 );

	result = dppequ( 'lower', 3, ap, 1, 0, s, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
});

test( 'dppequ: identity_upper', function t() {
	var result;
	var tc = findCase( 'identity_upper' );

	// Upper packed identity N=3: diag at 0, 2, 5, all 1.0
	var ap = new Float64Array( [ 1.0, 0.0, 1.0, 0.0, 0.0, 1.0 ] );
	var s = new Float64Array( 3 );

	result = dppequ( 'upper', 3, ap, 1, 0, s, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
	assertClose( result.scond, tc.scond, 1e-14, 'scond' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
	assertArrayClose( Array.from( s ), tc.s, 1e-14, 's' );
});

test( 'dppequ: diagonal_varied_lower', function t() {
	var result;
	var tc = findCase( 'diagonal_varied_lower' );

	// Lower packed N=3: diag at 0, 3, 5
	// Diagonal: 100.0, 1.0, 0.25
	var ap = new Float64Array( [ 100.0, 5.0, 2.0, 1.0, 0.1, 0.25 ] );
	var s = new Float64Array( 3 );

	result = dppequ( 'lower', 3, ap, 1, 0, s, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
	assertClose( result.scond, tc.scond, 1e-14, 'scond' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
	assertArrayClose( Array.from( s ), tc.s, 1e-14, 's' );
});

test( 'dppequ: non_positive_first', function t() {
	var result;
	var tc = findCase( 'non_positive_first' );

	// Lower packed N=3: diag at 0, 3, 5
	// Diagonal: -2.0, 4.0, 9.0
	var ap = new Float64Array( [ -2.0, 1.0, 0.5, 4.0, 2.0, 9.0 ] );
	var s = new Float64Array( 3 );

	result = dppequ( 'lower', 3, ap, 1, 0, s, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
});

test( 'dppequ: non_positive_last', function t() {
	var result;
	var tc = findCase( 'non_positive_last' );

	// Upper packed N=3: diag at 0, 2, 5
	// Diagonal: 4.0, 9.0, -3.0
	var ap = new Float64Array( [ 4.0, 1.0, 9.0, 0.5, 2.0, -3.0 ] );
	var s = new Float64Array( 3 );

	result = dppequ( 'upper', 3, ap, 1, 0, s, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
});

test( 'dppequ: non-unit stride for s', function t() {
	// Upper packed N=3: diag 4.0, 9.0, 16.0
	var ap = new Float64Array( [ 4.0, 1.0, 9.0, 0.5, 2.0, 16.0 ] );
	var s = new Float64Array( 6 );
	var result;

	result = dppequ( 'upper', 3, ap, 1, 0, s, 2, 0 );
	assert.equal( result.info, 0, 'info' );
	assertClose( s[ 0 ], 1.0 / Math.sqrt( 4.0 ), 1e-14, 's[0]' );
	assertClose( s[ 2 ], 1.0 / Math.sqrt( 9.0 ), 1e-14, 's[2]' );
	assertClose( s[ 4 ], 1.0 / Math.sqrt( 16.0 ), 1e-14, 's[4]' );
});

test( 'dppequ: offset for AP', function t() {
	// Prepend 3 elements, then upper packed N=2: diag 25.0, 36.0
	var ap = new Float64Array( [ 999.0, 999.0, 999.0, 25.0, 7.0, 36.0 ] );
	var s = new Float64Array( 2 );
	var result;

	result = dppequ( 'upper', 2, ap, 1, 3, s, 1, 0 );
	assert.equal( result.info, 0, 'info' );
	assertClose( s[ 0 ], 1.0 / Math.sqrt( 25.0 ), 1e-14, 's[0]' );
	assertClose( s[ 1 ], 1.0 / Math.sqrt( 36.0 ), 1e-14, 's[1]' );
	assertClose( result.amax, 36.0, 1e-14, 'amax' );
	assertClose( result.scond, Math.sqrt( 25.0 ) / Math.sqrt( 36.0 ), 1e-14, 'scond' );
});

test( 'dppequ: offset for s', function t() {
	// Lower packed N=2: diag 9.0, 16.0
	var ap = new Float64Array( [ 9.0, 3.0, 16.0 ] );
	var s = new Float64Array( [ 0.0, 0.0, 0.0, 0.0 ] );
	var result;

	result = dppequ( 'lower', 2, ap, 1, 0, s, 1, 2 );
	assert.equal( result.info, 0, 'info' );
	assertClose( s[ 2 ], 1.0 / Math.sqrt( 9.0 ), 1e-14, 's[2]' );
	assertClose( s[ 3 ], 1.0 / Math.sqrt( 16.0 ), 1e-14, 's[3]' );
});

test( 'dppequ: N=1 lower', function t() {
	var ap = new Float64Array( [ 25.0 ] );
	var s = new Float64Array( 1 );
	var result;

	result = dppequ( 'lower', 1, ap, 1, 0, s, 1, 0 );
	assert.equal( result.info, 0, 'info' );
	assertClose( s[ 0 ], 1.0 / Math.sqrt( 25.0 ), 1e-14, 's[0]' );
	assertClose( result.scond, 1.0, 1e-14, 'scond' );
	assertClose( result.amax, 25.0, 1e-14, 'amax' );
});
