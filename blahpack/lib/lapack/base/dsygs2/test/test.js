

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dsygs2 = require( './../lib/base.js' );
var dpotrf = require( '../../dpotrf/lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dsygs2.jsonl' ), 'utf8' ).trim().split( '\n' );
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

// Build the symmetric positive definite B matrix and factorize it
// B = [4 2 0; 2 5 1; 0 1 3]
function makeBUpper() {
	var B = new Float64Array([
		4.0, 0.0, 0.0,
		2.0, 5.0, 0.0,
		0.0, 1.0, 3.0
	]);
	dpotrf( 'upper', 3, B, 1, 3, 0 );
	return B;
}

function makeBLower() {
	var B = new Float64Array([
		4.0, 2.0, 0.0,
		0.0, 5.0, 1.0,
		0.0, 0.0, 3.0
	]);
	dpotrf( 'lower', 3, B, 1, 3, 0 );
	return B;
}

// A = [4 2 1; 2 5 3; 1 3 6]
function makeAUpper() {
	return new Float64Array([
		4.0, 0.0, 0.0,
		2.0, 5.0, 0.0,
		1.0, 3.0, 6.0
	]);
}

function makeALower() {
	return new Float64Array([
		4.0, 2.0, 1.0,
		0.0, 5.0, 3.0,
		0.0, 0.0, 6.0
	]);
}


// TESTS //

test( 'dsygs2: itype1_upper', function t() {
	var tc = findCase( 'itype1_upper' );
	var A = makeAUpper();
	var B = makeBUpper();
	var info = dsygs2( 1, 'upper', 3, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( A ), tc.A, 1e-14, 'A' );
});

test( 'dsygs2: itype1_lower', function t() {
	var tc = findCase( 'itype1_lower' );
	var A = makeALower();
	var B = makeBLower();
	var info = dsygs2( 1, 'lower', 3, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( A ), tc.A, 1e-14, 'A' );
});

test( 'dsygs2: itype2_upper', function t() {
	var tc = findCase( 'itype2_upper' );
	var A = makeAUpper();
	var B = makeBUpper();
	var info = dsygs2( 2, 'upper', 3, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( A ), tc.A, 1e-14, 'A' );
});

test( 'dsygs2: itype2_lower', function t() {
	var tc = findCase( 'itype2_lower' );
	var A = makeALower();
	var B = makeBLower();
	var info = dsygs2( 2, 'lower', 3, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( A ), tc.A, 1e-14, 'A' );
});

test( 'dsygs2: itype3_upper', function t() {
	var tc = findCase( 'itype3_upper' );
	var A = makeAUpper();
	var B = makeBUpper();
	var info = dsygs2( 3, 'upper', 3, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( A ), tc.A, 1e-14, 'A' );
});

test( 'dsygs2: n_zero', function t() {
	var tc = findCase( 'n_zero' );
	var A = new Float64Array( 1 );
	var B = new Float64Array( 1 );
	var info = dsygs2( 1, 'upper', 0, A, 1, 1, 0, B, 1, 1, 0 );
	assert.equal( info, tc.info );
});

test( 'dsygs2: n_one', function t() {
	var tc = findCase( 'n_one' );
	var A = new Float64Array([ 9.0 ]);
	var B = new Float64Array([ 3.0 ]);
	var info = dsygs2( 1, 'upper', 1, A, 1, 1, 0, B, 1, 1, 0 );
	assert.equal( info, tc.info );
	assertClose( A[ 0 ], tc.A11, 1e-14, 'A11' );
});
