'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dsysv = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dsysv.jsonl' ), 'utf8' ).trim().split( '\n' );
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

/**
* Convert Fortran 1-based IPIV to 0-based JS convention.
* Positive values: subtract 1 (1-based to 0-based).
* Negative values: stay the same (Fortran -k maps to JS ~(k-1) = -k).
*/
function ipivTo0Based( ipiv ) {
	var i;
	var out = [];
	for ( i = 0; i < ipiv.length; i++ ) {
		if ( ipiv[ i ] > 0 ) {
			out.push( ipiv[ i ] - 1 );
		} else {
			out.push( ipiv[ i ] );
		}
	}
	return out;
}


// TESTS //

test( 'dsysv: upper_4x4 - solves symmetric system with upper storage', function t() {
	var tc = findCase( 'upper_4x4' );
	var expectedIPIV = ipivTo0Based( tc.ipiv );
	var IPIV = new Int32Array( 4 );
	var info;
	var A;
	var B;

	// A = [4 1 2 3; 1 5 1 2; 2 1 6 1; 3 2 1 7] column-major
	A = new Float64Array([
		4, 1, 2, 3,
		1, 5, 1, 2,
		2, 1, 6, 1,
		3, 2, 1, 7
	]);
	// b = A*[1;2;3;4] = [24;22;26;38]
	B = new Float64Array([ 24, 22, 26, 38 ]);

	info = dsysv( 'U', 4, 1, A, 1, 4, 0, IPIV, 1, 0, B, 1, 4, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( Array.from( B ), tc.x, 1e-14, 'x' );
	assert.deepStrictEqual( Array.from( IPIV ), expectedIPIV, 'ipiv' );
});

test( 'dsysv: lower_4x4 - solves symmetric system with lower storage', function t() {
	var tc = findCase( 'lower_4x4' );
	var expectedIPIV = ipivTo0Based( tc.ipiv );
	var IPIV = new Int32Array( 4 );
	var info;
	var A;
	var B;

	A = new Float64Array([
		4, 1, 2, 3,
		1, 5, 1, 2,
		2, 1, 6, 1,
		3, 2, 1, 7
	]);
	B = new Float64Array([ 24, 22, 26, 38 ]);

	info = dsysv( 'L', 4, 1, A, 1, 4, 0, IPIV, 1, 0, B, 1, 4, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( Array.from( B ), tc.x, 1e-14, 'x' );
	assert.deepStrictEqual( Array.from( IPIV ), expectedIPIV, 'ipiv' );
});

test( 'dsysv: multi_rhs - multiple right-hand sides', function t() {
	var tc = findCase( 'multi_rhs' );
	var expectedIPIV = ipivTo0Based( tc.ipiv );
	var IPIV = new Int32Array( 2 );
	var info;
	var A;
	var B;

	// A = [2 -1; -1 3]
	A = new Float64Array([
		2, -1,
		-1, 3
	]);
	// B = [1 5; 4 7] (2x2 column-major: col1=[1,5], col2=[4,7])
	B = new Float64Array([ 1, 5, 4, 7 ]);

	info = dsysv( 'U', 2, 2, A, 1, 2, 0, IPIV, 1, 0, B, 1, 2, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( Array.from( B ), tc.x, 1e-14, 'x' );
	assert.deepStrictEqual( Array.from( IPIV ), expectedIPIV, 'ipiv' );
});

test( 'dsysv: singular - returns info > 0 for singular matrix', function t() {
	var IPIV = new Int32Array( 2 );
	var info;
	var A;
	var B;

	// A = [1 2; 2 4] singular
	A = new Float64Array([ 1, 2, 2, 4 ]);
	B = new Float64Array([ 1, 2 ]);

	info = dsysv( 'U', 2, 1, A, 1, 2, 0, IPIV, 1, 0, B, 1, 2, 0 );

	assert.ok( info > 0, 'info should be > 0 for singular matrix' );
});

test( 'dsysv: n1 - N=1 edge case', function t() {
	var tc = findCase( 'n1' );
	var expectedIPIV = ipivTo0Based( tc.ipiv );
	var IPIV = new Int32Array( 1 );
	var info;
	var A;
	var B;

	// A = [3], b = [9] => x = 3
	A = new Float64Array([ 3 ]);
	B = new Float64Array([ 9 ]);

	info = dsysv( 'U', 1, 1, A, 1, 1, 0, IPIV, 1, 0, B, 1, 1, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( Array.from( B ), tc.x, 1e-14, 'x' );
	assert.deepStrictEqual( Array.from( IPIV ), expectedIPIV, 'ipiv' );
});

test( 'dsysv: pivot_2x2_upper - matrix triggering 2x2 pivots (upper)', function t() {
	var tc = findCase( 'pivot_2x2_upper' );
	var expectedIPIV = ipivTo0Based( tc.ipiv );
	var IPIV = new Int32Array( 4 );
	var info;
	var A;
	var B;

	// A = [0 1 0 0; 1 0 0 0; 0 0 4 1; 0 0 1 4]
	A = new Float64Array([
		0, 1, 0, 0,
		1, 0, 0, 0,
		0, 0, 4, 1,
		0, 0, 1, 4
	]);
	B = new Float64Array([ 1, 1, 5, 5 ]);

	info = dsysv( 'U', 4, 1, A, 1, 4, 0, IPIV, 1, 0, B, 1, 4, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( Array.from( B ), tc.x, 1e-14, 'x' );
	assert.deepStrictEqual( Array.from( IPIV ), expectedIPIV, 'ipiv' );
});

test( 'dsysv: pivot_2x2_lower - matrix triggering 2x2 pivots (lower)', function t() {
	var tc = findCase( 'pivot_2x2_lower' );
	var expectedIPIV = ipivTo0Based( tc.ipiv );
	var IPIV = new Int32Array( 4 );
	var info;
	var A;
	var B;

	// A = [0 1 0 0; 1 0 0 0; 0 0 4 1; 0 0 1 4]
	A = new Float64Array([
		0, 1, 0, 0,
		1, 0, 0, 0,
		0, 0, 4, 1,
		0, 0, 1, 4
	]);
	B = new Float64Array([ 1, 1, 5, 5 ]);

	info = dsysv( 'L', 4, 1, A, 1, 4, 0, IPIV, 1, 0, B, 1, 4, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( Array.from( B ), tc.x, 1e-14, 'x' );
	assert.deepStrictEqual( Array.from( IPIV ), expectedIPIV, 'ipiv' );
});

test( 'dsysv: n_zero - N=0 quick return', function t() {
	var IPIV = new Int32Array( 1 );
	var info;
	var A;
	var B;

	A = new Float64Array([ 1 ]);
	B = new Float64Array([ 1 ]);

	info = dsysv( 'U', 0, 1, A, 1, 1, 0, IPIV, 1, 0, B, 1, 1, 0 );
	assert.equal( info, 0, 'info' );
});

test( 'dsysv: nrhs_zero - NRHS=0 quick return', function t() {
	var IPIV = new Int32Array( 1 );
	var info;
	var A;
	var B;

	A = new Float64Array([ 5 ]);
	B = new Float64Array([ 10 ]);

	info = dsysv( 'U', 1, 0, A, 1, 1, 0, IPIV, 1, 0, B, 1, 1, 0 );
	assert.equal( info, 0, 'info' );
});
