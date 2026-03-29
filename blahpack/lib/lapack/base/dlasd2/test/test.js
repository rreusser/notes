
/* eslint-disable max-len, max-lines */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dlasd2 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlasd2.jsonl' ), 'utf8' ).trim().split( '\n' );
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
* Runs a dlasd2 test case given a named fixture.
*
* @private
* @param {Object} tc - test case from fixture
* @param {integer} nl - nl
* @param {integer} nr - nr
* @param {integer} sqre - sqre
* @param {number} alpha - alpha
* @param {number} beta - beta
* @param {Float64Array} din - initial D values
* @param {Array<integer>} idxqIn - initial IDXQ values (1-based)
*/
function runCase( tc, nl, nr, sqre, alpha, beta, din, idxqIn ) {
	var COLTYP;
	var DSIGMA;
	var info;
	var IDXP;
	var IDXC;
	var IDXQ;
	var tol;
	var IDX;
	var VT2;
	var U2;
	var VT;
	var K2;
	var d;
	var z;
	var U;
	var n;
	var m;
	var i;
	var j;

	n = nl + nr + 1;
	m = n + sqre;
	tol = 1e-14;

	d = new Float64Array( n );
	for ( i = 0; i < n; i++ ) {
		d[ i ] = din[ i ];
	}
	z = new Float64Array( m );
	U = new Float64Array( n * n );
	for ( i = 0; i < n; i++ ) {
		U[ i * n + i ] = 1.0; // identity, column-major: U(i,i) = 1
	}
	VT = new Float64Array( m * m );
	for ( i = 0; i < m; i++ ) {
		VT[ i * m + i ] = 1.0; // identity, column-major
	}
	DSIGMA = new Float64Array( n );
	U2 = new Float64Array( n * n );
	VT2 = new Float64Array( m * m );
	IDXP = new Int32Array( n );
	IDX = new Int32Array( n );
	IDXC = new Int32Array( n );
	IDXQ = new Int32Array( m );
	for ( i = 0; i < idxqIn.length; i++ ) {
		IDXQ[ i ] = idxqIn[ i ];
	}
	COLTYP = new Int32Array( Math.max( n, 4 ) );
	K2 = new Int32Array( 1 );

	// Call: column-major strides: strideU1=1, strideU2=n, etc.
	info = dlasd2(
		nl, nr, sqre, K2,
		d, 1, 0,
		z, 1, 0,
		alpha, beta,
		U, 1, n, 0,
		VT, 1, m, 0,
		DSIGMA, 1, 0,
		U2, 1, n, 0,
		VT2, 1, m, 0,
		IDXP, 1, 0,
		IDX, 1, 0,
		IDXC, 1, 0,
		IDXQ, 1, 0,
		COLTYP, 1, 0
	);

	assert.equal( info, tc.info, 'info' );
	assert.equal( K2[ 0 ], tc.K, 'K' );

	assertArrayClose( Array.from( d ), tc.D, tol, 'D' );
	assertArrayClose( Array.from( z.subarray( 0, n ) ), tc.Z, tol, 'Z' );
	assertArrayClose( Array.from( DSIGMA ), tc.DSIGMA, tol, 'DSIGMA' );

	// U is N x N column-major (packed)
	assertArrayClose( Array.from( U ), tc.U, tol, 'U' );

	// VT is M x M column-major (packed)
	assertArrayClose( Array.from( VT ), tc.VT, tol, 'VT' );

	// U2 is N x N column-major (packed)
	assertArrayClose( Array.from( U2 ), tc.U2, tol, 'U2' );

	// VT2 is M x M column-major (packed)
	assertArrayClose( Array.from( VT2 ), tc.VT2, tol, 'VT2' );

	// Integer arrays: compare against 1-based fixture values
	for ( i = 0; i < n; i++ ) {
		assert.equal( IDXP[ i ], tc.IDXP[ i ], 'IDXP[' + i + ']' );
	}
	for ( i = 0; i < n; i++ ) {
		assert.equal( IDX[ i ], tc.IDX[ i ], 'IDX[' + i + ']' );
	}
	for ( i = 0; i < n; i++ ) {
		assert.equal( IDXC[ i ], tc.IDXC[ i ], 'IDXC[' + i + ']' );
	}
	for ( i = 0; i < n; i++ ) {
		assert.equal( IDXQ[ i ], tc.IDXQ[ i ], 'IDXQ[' + i + ']' );
	}
	for ( i = 0; i < 4; i++ ) {
		assert.equal( COLTYP[ i ], tc.COLTYP[ i ], 'COLTYP[' + i + ']' );
	}
}


// TESTS //

test( 'dlasd2 is a function', function t() {
	assert.equal( typeof dlasd2, 'function' );
});

test( 'dlasd2: basic_nl2_nr2_sqre0', function t() {
	var tc = findCase( 'basic_nl2_nr2_sqre0' );
	runCase( tc, 2, 2, 0, 0.5, 0.7,
		[ 1.0, 3.0, 0.0, 2.0, 4.0 ],
		[ 1, 2, 0, 1, 2 ]
	);
});

test( 'dlasd2: sqre1_nl2_nr2', function t() {
	var tc = findCase( 'sqre1_nl2_nr2' );
	runCase( tc, 2, 2, 1, 0.3, 0.4,
		[ 1.5, 3.5, 0.0, 2.5, 5.0 ],
		[ 1, 2, 0, 1, 2 ]
	);
});

test( 'dlasd2: nl3_nr3_sqre0', function t() {
	var tc = findCase( 'nl3_nr3_sqre0' );
	runCase( tc, 3, 3, 0, 0.6, 0.8,
		[ 0.5, 1.5, 2.5, 0.0, 1.0, 2.0, 3.0 ],
		[ 1, 2, 3, 0, 1, 2, 3 ]
	);
});

test( 'dlasd2: deflation_close_values', function t() {
	var tc = findCase( 'deflation_close_values' );
	runCase( tc, 2, 2, 0, 0.5, 0.5,
		[ 1.0, 2.0, 0.0, 1.0, 3.0 ],
		[ 1, 2, 0, 1, 2 ]
	);
});

test( 'dlasd2: minimal_nl1_nr1', function t() {
	var tc = findCase( 'minimal_nl1_nr1' );
	runCase( tc, 1, 1, 0, 0.8, 0.6,
		[ 2.0, 0.0, 4.0 ],
		[ 1, 0, 1 ]
	);
});

test( 'dlasd2: sqre1_nl1_nr1', function t() {
	var tc = findCase( 'sqre1_nl1_nr1' );
	runCase( tc, 1, 1, 1, 0.4, 0.9,
		[ 3.0, 0.0, 5.0 ],
		[ 1, 0, 1 ]
	);
});

test( 'dlasd2: givens_deflation (close values with non-tiny Z)', function t() {
	var COLTYP;
	var DSIGMA;
	var info;
	var IDXP;
	var IDXC;
	var IDXQ;
	var tol;
	var IDX;
	var VT2;
	var tc;
	var U2;
	var VT;
	var K2;
	var d;
	var z;
	var U;
	var n;
	var m;
	var i;

	tc = findCase( 'givens_deflation' );
	n = 5;
	m = 5;
	tol = 1e-14;

	d = new Float64Array( [ 5.0, 10.0, 0.0, 5.0, 15.0 ] );
	z = new Float64Array( m );

	// U = identity 5x5
	U = new Float64Array( n * n );
	for ( i = 0; i < n; i++ ) {
		U[ i * n + i ] = 1.0;
	}

	// VT = identity 5x5 with non-zero entries in key columns
	VT = new Float64Array( m * m );
	for ( i = 0; i < m; i++ ) {
		VT[ i * m + i ] = 1.0;
	}
	// Column 3 (0-based col 2) = NLP1: set VT(1,3), VT(2,3)
	VT[ 0 + 2 * m ] = 0.3;  // VT(1,3) in col-major
	VT[ 1 + 2 * m ] = 0.4;  // VT(2,3)
	VT[ 2 + 2 * m ] = 0.5;  // VT(3,3)
	// Column 4 (0-based col 3) = NLP2:
	VT[ 3 + 3 * m ] = 0.6;  // VT(4,4)
	VT[ 4 + 3 * m ] = 0.7;  // VT(5,4)

	DSIGMA = new Float64Array( n );
	U2 = new Float64Array( n * n );
	VT2 = new Float64Array( m * m );
	IDXP = new Int32Array( n );
	IDX = new Int32Array( n );
	IDXC = new Int32Array( n );
	IDXQ = new Int32Array( [ 1, 2, 0, 1, 2 ] );
	COLTYP = new Int32Array( Math.max( n, 4 ) );
	K2 = new Int32Array( 1 );

	info = dlasd2(
		2, 2, 0, K2,
		d, 1, 0,
		z, 1, 0,
		2.0, 3.0,
		U, 1, n, 0,
		VT, 1, m, 0,
		DSIGMA, 1, 0,
		U2, 1, n, 0,
		VT2, 1, m, 0,
		IDXP, 1, 0,
		IDX, 1, 0,
		IDXC, 1, 0,
		IDXQ, 1, 0,
		COLTYP, 1, 0
	);

	assert.equal( info, tc.info, 'info' );
	assert.equal( K2[ 0 ], tc.K, 'K' );
	assertArrayClose( Array.from( d ), tc.D, tol, 'D' );
	assertArrayClose( Array.from( z.subarray( 0, n ) ), tc.Z, tol, 'Z' );
	assertArrayClose( Array.from( DSIGMA ), tc.DSIGMA, tol, 'DSIGMA' );
	assertArrayClose( Array.from( U ), tc.U, tol, 'U' );
	assertArrayClose( Array.from( VT ), tc.VT, tol, 'VT' );
	assertArrayClose( Array.from( U2 ), tc.U2, tol, 'U2' );
	assertArrayClose( Array.from( VT2 ), tc.VT2, tol, 'VT2' );
	for ( i = 0; i < n; i++ ) {
		assert.equal( IDXP[ i ], tc.IDXP[ i ], 'IDXP[' + i + ']' );
		assert.equal( IDX[ i ], tc.IDX[ i ], 'IDX[' + i + ']' );
		assert.equal( IDXC[ i ], tc.IDXC[ i ], 'IDXC[' + i + ']' );
		assert.equal( IDXQ[ i ], tc.IDXQ[ i ], 'IDXQ[' + i + ']' );
	}
	for ( i = 0; i < 4; i++ ) {
		assert.equal( COLTYP[ i ], tc.COLTYP[ i ], 'COLTYP[' + i + ']' );
	}
});

test( 'dlasd2: all_deflated (tiny alpha/beta)', function t() {
	var tc = findCase( 'all_deflated' );
	runCase( tc, 2, 1, 0, 1.0e-20, 1.0e-20,
		[ 1.0, 2.0, 0.0, 3.0 ],
		[ 1, 2, 0, 1 ]
	);
});

test( 'dlasd2: tiny_dsigma2 (DSIGMA(2) replacement)', function t() {
	var tc = findCase( 'tiny_dsigma2' );
	runCase( tc, 2, 1, 0, 1.0, 1.0,
		[ 1.0e-20, 2.0, 0.0, 3.0 ],
		[ 1, 2, 0, 1 ]
	);
});
