'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dpbequ = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dpbequ.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'dpbequ is a function', function t() {
	assert.equal( typeof dpbequ, 'function' );
});

test( 'dpbequ: upper_basic', function t() {
	var result;
	var tc;
	var AB;
	var s;

	tc = findCase( 'upper_basic' );

	// UPLO='U', N=4, KD=2, LDAB=3 (column-major band storage)
	// Diagonal at row KD=2 (0-based), diags: 4, 9, 16, 25
	AB = new Float64Array([
		0.0, 0.0, 4.0,   // col 1
		0.0, 1.0, 9.0,   // col 2
		0.5, 2.0, 16.0,  // col 3
		0.0, 1.5, 25.0   // col 4
	]);
	s = new Float64Array( 4 );

	// strideAB1=1 (row), strideAB2=3 (LDAB=3), offsetAB=0
	result = dpbequ( 'upper', 4, 2, AB, 1, 3, 0, s, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
	assertClose( result.scond, tc.scond, 1e-14, 'scond' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
	assertArrayClose( Array.from( s ), tc.s, 1e-14, 's' );
});

test( 'dpbequ: lower_basic', function t() {
	var result;
	var tc;
	var AB;
	var s;

	tc = findCase( 'lower_basic' );

	// UPLO='L', N=4, KD=2, LDAB=3 (column-major band storage)
	// Diagonal at row 0 (0-based), diags: 4, 9, 16, 25
	AB = new Float64Array([
		4.0, 1.0, 0.5,   // col 1
		9.0, 2.0, 0.0,   // col 2
		16.0, 1.5, 0.0,  // col 3
		25.0, 0.0, 0.0   // col 4
	]);
	s = new Float64Array( 4 );

	result = dpbequ( 'lower', 4, 2, AB, 1, 3, 0, s, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
	assertClose( result.scond, tc.scond, 1e-14, 'scond' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
	assertArrayClose( Array.from( s ), tc.s, 1e-14, 's' );
});

test( 'dpbequ: n_zero', function t() {
	var result;
	var tc;
	var AB;
	var s;

	tc = findCase( 'n_zero' );
	AB = new Float64Array( 1 );
	s = new Float64Array( 1 );

	result = dpbequ( 'upper', 0, 0, AB, 1, 1, 0, s, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
	assertClose( result.scond, tc.scond, 1e-14, 'scond' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
});

test( 'dpbequ: n_one', function t() {
	var result;
	var tc;
	var AB;
	var s;

	tc = findCase( 'n_one' );

	// N=1, KD=0, LDAB=1
	AB = new Float64Array([ 49.0 ]);
	s = new Float64Array( 1 );

	result = dpbequ( 'upper', 1, 0, AB, 1, 1, 0, s, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
	assertClose( result.scond, tc.scond, 1e-14, 'scond' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
	assertArrayClose( Array.from( s ), tc.s, 1e-14, 's' );
});

test( 'dpbequ: non_positive_upper', function t() {
	var result;
	var tc;
	var AB;
	var s;

	tc = findCase( 'non_positive_upper' );

	// UPLO='U', N=3, KD=1, LDAB=2
	// Diagonal at row KD=1 (0-based): 4, -1, 9
	AB = new Float64Array([
		0.0, 4.0,    // col 1
		1.0, -1.0,   // col 2
		0.5, 9.0     // col 3
	]);
	s = new Float64Array( 3 );

	result = dpbequ( 'upper', 3, 1, AB, 1, 2, 0, s, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
});

test( 'dpbequ: zero_diag_lower', function t() {
	var result;
	var tc;
	var AB;
	var s;

	tc = findCase( 'zero_diag_lower' );

	// UPLO='L', N=3, KD=1, LDAB=2
	// Diagonal at row 0 (0-based): 4, 0, 9
	AB = new Float64Array([
		4.0, 1.0,   // col 1
		0.0, 0.5,   // col 2
		9.0, 0.0    // col 3
	]);
	s = new Float64Array( 3 );

	result = dpbequ( 'lower', 3, 1, AB, 1, 2, 0, s, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
});

test( 'dpbequ: identity_upper', function t() {
	var result;
	var tc;
	var AB;
	var s;

	tc = findCase( 'identity_upper' );

	// UPLO='U', N=3, KD=1, LDAB=2
	// Diagonal at row 1 (0-based): 1, 1, 1
	AB = new Float64Array([
		0.0, 1.0,   // col 1
		0.0, 1.0,   // col 2
		0.0, 1.0    // col 3
	]);
	s = new Float64Array( 3 );

	result = dpbequ( 'upper', 3, 1, AB, 1, 2, 0, s, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
	assertClose( result.scond, tc.scond, 1e-14, 'scond' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
	assertArrayClose( Array.from( s ), tc.s, 1e-14, 's' );
});

test( 'dpbequ: diagonal_varied_lower', function t() {
	var result;
	var tc;
	var AB;
	var s;

	tc = findCase( 'diagonal_varied_lower' );

	// UPLO='L', N=3, KD=0, LDAB=1
	// Diagonal at row 0 (0-based): 100, 1, 0.25
	AB = new Float64Array([ 100.0, 1.0, 0.25 ]);
	s = new Float64Array( 3 );

	result = dpbequ( 'lower', 3, 0, AB, 1, 1, 0, s, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
	assertClose( result.scond, tc.scond, 1e-14, 'scond' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
	assertArrayClose( Array.from( s ), tc.s, 1e-14, 's' );
});

test( 'dpbequ: non_positive_first', function t() {
	var result;
	var tc;
	var AB;
	var s;

	tc = findCase( 'non_positive_first' );

	// UPLO='L', N=3, KD=0, LDAB=1
	// Diagonal: -2, 4, 9
	AB = new Float64Array([ -2.0, 4.0, 9.0 ]);
	s = new Float64Array( 3 );

	result = dpbequ( 'lower', 3, 0, AB, 1, 1, 0, s, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
});

test( 'dpbequ: non_positive_last', function t() {
	var result;
	var tc;
	var AB;
	var s;

	tc = findCase( 'non_positive_last' );

	// UPLO='U', N=3, KD=0, LDAB=1
	// Diagonal: 4, 9, -3
	AB = new Float64Array([ 4.0, 9.0, -3.0 ]);
	s = new Float64Array( 3 );

	result = dpbequ( 'upper', 3, 0, AB, 1, 1, 0, s, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
});
