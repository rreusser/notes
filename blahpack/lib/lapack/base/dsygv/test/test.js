

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dsygv = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dsygv.jsonl' ), 'utf8' ).trim().split( '\n' );
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

// A = [4 2 1; 2 5 3; 1 3 6], B = [4 2 0; 2 5 1; 0 1 3]

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

function makeBUpper() {
	return new Float64Array([
		4.0, 0.0, 0.0,
		2.0, 5.0, 0.0,
		0.0, 1.0, 3.0
	]);
}

function makeBLower() {
	return new Float64Array([
		4.0, 2.0, 0.0,
		0.0, 5.0, 1.0,
		0.0, 0.0, 3.0
	]);
}


// TESTS //

test( 'dsygv: itype1_v_upper', function t() {
	var tc = findCase( 'itype1_v_upper' );
	var A = makeAUpper();
	var B = makeBUpper();
	var w = new Float64Array( 3 );
	var WORK = new Float64Array( 100 );

	var info = dsygv( 1, 'compute', 'upper', 3, A, 1, 3, 0, B, 1, 3, 0, w, 1, 0, WORK, 1, 0, 100 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( w ), tc.w, 1e-14, 'w' );
	// Eigenvectors may differ by sign — compare absolute values
	var absA = Array.from( A ).map( Math.abs );
	var absExpected = tc.A.map( Math.abs );
	assertArrayClose( absA, absExpected, 1e-12, 'A' );
});

test( 'dsygv: itype1_v_lower', function t() {
	var tc = findCase( 'itype1_v_lower' );
	var A = makeALower();
	var B = makeBLower();
	var w = new Float64Array( 3 );
	var WORK = new Float64Array( 100 );

	var info = dsygv( 1, 'compute', 'lower', 3, A, 1, 3, 0, B, 1, 3, 0, w, 1, 0, WORK, 1, 0, 100 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( w ), tc.w, 1e-14, 'w' );
	var absA = Array.from( A ).map( Math.abs );
	var absExpected = tc.A.map( Math.abs );
	assertArrayClose( absA, absExpected, 1e-12, 'A' );
});

test( 'dsygv: itype1_n_lower (eigenvalues only)', function t() {
	var tc = findCase( 'itype1_n_lower' );
	var A = makeALower();
	var B = makeBLower();
	var w = new Float64Array( 3 );
	var WORK = new Float64Array( 100 );

	var info = dsygv( 1, 'none', 'lower', 3, A, 1, 3, 0, B, 1, 3, 0, w, 1, 0, WORK, 1, 0, 100 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( w ), tc.w, 1e-14, 'w' );
});

test( 'dsygv: itype2_v_upper', function t() {
	var tc = findCase( 'itype2_v_upper' );
	var A = makeAUpper();
	var B = makeBUpper();
	var w = new Float64Array( 3 );
	var WORK = new Float64Array( 100 );

	var info = dsygv( 2, 'compute', 'upper', 3, A, 1, 3, 0, B, 1, 3, 0, w, 1, 0, WORK, 1, 0, 100 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( w ), tc.w, 1e-13, 'w' );
});

test( 'dsygv: itype3_v_lower', function t() {
	var tc = findCase( 'itype3_v_lower' );
	var A = makeALower();
	var B = makeBLower();
	var w = new Float64Array( 3 );
	var WORK = new Float64Array( 100 );

	var info = dsygv( 3, 'compute', 'lower', 3, A, 1, 3, 0, B, 1, 3, 0, w, 1, 0, WORK, 1, 0, 100 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( w ), tc.w, 1e-13, 'w' );
});

test( 'dsygv: n_zero', function t() {
	var tc = findCase( 'n_zero' );
	var A = new Float64Array( 1 );
	var B = new Float64Array( 1 );
	var w = new Float64Array( 1 );
	var WORK = new Float64Array( 1 );

	var info = dsygv( 1, 'compute', 'upper', 0, A, 1, 1, 0, B, 1, 1, 0, w, 1, 0, WORK, 1, 0, 1 );
	assert.equal( info, tc.info );
});

test( 'dsygv: n_one', function t() {
	var tc = findCase( 'n_one' );
	var A = new Float64Array([ 6.0 ]);
	var B = new Float64Array([ 2.0 ]);
	var w = new Float64Array( 1 );
	var WORK = new Float64Array( 100 );

	var info = dsygv( 1, 'compute', 'upper', 1, A, 1, 1, 0, B, 1, 1, 0, w, 1, 0, WORK, 1, 0, 100 );
	assert.equal( info, tc.info );
	assertClose( w[ 0 ], tc.w1, 1e-14, 'w1' );
	assertClose( A[ 0 ], tc.A1, 1e-14, 'A1' );
});

test( 'dsygv: not_posdef', function t() {
	var tc = findCase( 'not_posdef' );
	var A = new Float64Array([ 1.0, 0.0, 0.0, 1.0 ]);
	var B = new Float64Array([ -1.0, 0.0, 0.0, 1.0 ]);
	var w = new Float64Array( 2 );
	var WORK = new Float64Array( 100 );

	var info = dsygv( 1, 'compute', 'lower', 2, A, 1, 2, 0, B, 1, 2, 0, w, 1, 0, WORK, 1, 0, 100 );
	assert.equal( info, tc.info );
});
