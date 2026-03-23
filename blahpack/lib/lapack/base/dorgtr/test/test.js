/**
* @license Apache-2.0
*
* Copyright (c) 2025 The Stdlib Authors.
*/

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Float64Array = require( '@stdlib/array/float64' );
var dorgtr = require( './../lib/base.js' );
var dsytrd = require( '../../dsytrd/lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dorgtr.jsonl' ), 'utf8' ).trim().split( '\n' );
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
* Helper: performs dsytrd then dorgtr and returns the Q matrix (column-major flat).
*
* @private
* @param {string} uplo - 'U' or 'L'
* @param {NonNegativeInteger} N - matrix order
* @param {Float64Array} Asym - symmetric matrix (column-major, N*N)
* @returns {Object} { Q, D, E, TAU, info }
*/
function dsytrdThenDorgtr( uplo, N, Asym ) {
	var WORK = new Float64Array( 256 );
	var TAU = new Float64Array( Math.max( N - 1, 1 ) );
	var D = new Float64Array( N );
	var E = new Float64Array( Math.max( N - 1, 1 ) );
	var A = new Float64Array( Asym );
	var info;
	var Q;
	var i;

	// Call dsytrd to reduce to tridiagonal form
	dsytrd( uplo, N, A, 1, N, 0, D, 1, 0, E, 1, 0, TAU, 1, 0, WORK, 1, 0, 256 );

	// Copy A to Q (A now contains reflectors)
	Q = new Float64Array( A );

	// Allocate fresh WORK for dorgtr
	WORK = new Float64Array( 256 );

	// Call dorgtr to generate Q
	info = dorgtr( uplo, N, Q, 1, N, 0, TAU, 1, 0, WORK, 1, 0, 256 );

	return {
		Q: Q,
		D: D,
		E: E,
		TAU: TAU,
		info: info
	};
}

/**
* Extract a flat column-major subarray as a regular Array.
*/
function toArray( arr, offset, len ) {
	var result = [];
	var i;
	for ( i = 0; i < len; i++ ) {
		result.push( arr[ offset + i ] );
	}
	return result;
}


// TESTS //

test( 'dorgtr: uplo_U_4x4', function t() {
	var tc = findCase( 'uplo_U_4x4' );
	var N = 4;
	// Symmetric matrix (column-major):
	// [ 4  1 -2  2 ]
	// [ 1  2  0  1 ]
	// [-2  0  3 -2 ]
	// [ 2  1 -2 -1 ]
	var A = new Float64Array([
		4, 1, -2, 2,
		1, 2, 0, 1,
		-2, 0, 3, -2,
		2, 1, -2, -1
	]);

	var result = dsytrdThenDorgtr( 'upper', N, A );

	assert.equal( result.info, 0, 'info' );
	assertArrayClose( toArray( result.Q, 0, N * N ), tc.Q, 1e-13, 'Q' );
});

test( 'dorgtr: uplo_L_4x4', function t() {
	var tc = findCase( 'uplo_L_4x4' );
	var N = 4;
	var A = new Float64Array([
		4, 1, -2, 2,
		1, 2, 0, 1,
		-2, 0, 3, -2,
		2, 1, -2, -1
	]);

	var result = dsytrdThenDorgtr( 'lower', N, A );

	assert.equal( result.info, 0, 'info' );
	assertArrayClose( toArray( result.Q, 0, N * N ), tc.Q, 1e-13, 'Q' );
});

test( 'dorgtr: N1_uplo_U', function t() {
	var tc = findCase( 'N1_uplo_U' );
	var N = 1;
	var A = new Float64Array([ 5.0 ]);

	var result = dsytrdThenDorgtr( 'upper', N, A );

	assert.equal( result.info, 0, 'info' );
	assertArrayClose( toArray( result.Q, 0, 1 ), tc.Q, 1e-14, 'Q' );
});

test( 'dorgtr: N1_uplo_L', function t() {
	var tc = findCase( 'N1_uplo_L' );
	var N = 1;
	var A = new Float64Array([ 5.0 ]);

	var result = dsytrdThenDorgtr( 'lower', N, A );

	assert.equal( result.info, 0, 'info' );
	assertArrayClose( toArray( result.Q, 0, 1 ), tc.Q, 1e-14, 'Q' );
});

test( 'dorgtr: N0_uplo_U', function t() {
	var WORK = new Float64Array( 256 );
	var TAU = new Float64Array( 1 );
	var A = new Float64Array( 1 );
	var info;

	info = dorgtr( 'upper', 0, A, 1, 1, 0, TAU, 1, 0, WORK, 1, 0, 256 );
	assert.equal( info, 0, 'info' );
});

test( 'dorgtr: N0_uplo_L', function t() {
	var WORK = new Float64Array( 256 );
	var TAU = new Float64Array( 1 );
	var A = new Float64Array( 1 );
	var info;

	info = dorgtr( 'lower', 0, A, 1, 1, 0, TAU, 1, 0, WORK, 1, 0, 256 );
	assert.equal( info, 0, 'info' );
});

test( 'dorgtr: uplo_U_3x3', function t() {
	var tc = findCase( 'uplo_U_3x3' );
	var N = 3;
	// [ 2  1  3 ]
	// [ 1  5 -1 ]
	// [ 3 -1  4 ]
	var A = new Float64Array([
		2, 1, 3,
		1, 5, -1,
		3, -1, 4
	]);

	var result = dsytrdThenDorgtr( 'upper', N, A );

	assert.equal( result.info, 0, 'info' );
	assertArrayClose( toArray( result.Q, 0, N * N ), tc.Q, 1e-13, 'Q' );
});

test( 'dorgtr: uplo_L_3x3', function t() {
	var tc = findCase( 'uplo_L_3x3' );
	var N = 3;
	var A = new Float64Array([
		2, 1, 3,
		1, 5, -1,
		3, -1, 4
	]);

	var result = dsytrdThenDorgtr( 'lower', N, A );

	assert.equal( result.info, 0, 'info' );
	assertArrayClose( toArray( result.Q, 0, N * N ), tc.Q, 1e-13, 'Q' );
});

test( 'dorgtr: Q is orthogonal (uplo_U_4x4)', function t() {
	var N = 4;
	var A = new Float64Array([
		4, 1, -2, 2,
		1, 2, 0, 1,
		-2, 0, 3, -2,
		2, 1, -2, -1
	]);

	var result = dsytrdThenDorgtr( 'upper', N, A );
	var Q = result.Q;
	var i;
	var j;
	var k;
	var sum;

	// Verify Q^T * Q = I
	for ( i = 0; i < N; i++ ) {
		for ( j = 0; j < N; j++ ) {
			sum = 0.0;
			for ( k = 0; k < N; k++ ) {
				sum += Q[ k + i * N ] * Q[ k + j * N ]; // Q^T * Q, column-major
			}
			if ( i === j ) {
				assertClose( sum, 1.0, 1e-13, 'QTQ[' + i + ',' + j + ']' );
			} else {
				assertClose( sum, 0.0, 1e-13, 'QTQ[' + i + ',' + j + ']' );
			}
		}
	}
});

test( 'dorgtr: Q is orthogonal (uplo_L_4x4)', function t() {
	var N = 4;
	var A = new Float64Array([
		4, 1, -2, 2,
		1, 2, 0, 1,
		-2, 0, 3, -2,
		2, 1, -2, -1
	]);

	var result = dsytrdThenDorgtr( 'lower', N, A );
	var Q = result.Q;
	var i;
	var j;
	var k;
	var sum;

	// Verify Q^T * Q = I
	for ( i = 0; i < N; i++ ) {
		for ( j = 0; j < N; j++ ) {
			sum = 0.0;
			for ( k = 0; k < N; k++ ) {
				sum += Q[ k + i * N ] * Q[ k + j * N ];
			}
			if ( i === j ) {
				assertClose( sum, 1.0, 1e-13, 'QTQ[' + i + ',' + j + ']' );
			} else {
				assertClose( sum, 0.0, 1e-13, 'QTQ[' + i + ',' + j + ']' );
			}
		}
	}
});

test( 'dorgtr: Q^T*A*Q is tridiagonal (uplo_U_4x4)', function t() {
	var N = 4;
	var Aorig = new Float64Array([
		4, 1, -2, 2,
		1, 2, 0, 1,
		-2, 0, 3, -2,
		2, 1, -2, -1
	]);

	var result = dsytrdThenDorgtr( 'upper', N, Aorig );
	var Q = result.Q;
	var i;
	var j;
	var k;
	var l;

	// Compute QTAQ = Q^T * Aorig * Q
	var tmp = new Float64Array( N * N ); // Aorig * Q
	var QTAQ = new Float64Array( N * N );

	// tmp = Aorig * Q (column-major)
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < N; i++ ) {
			var s = 0.0;
			for ( k = 0; k < N; k++ ) {
				s += Aorig[ i + k * N ] * Q[ k + j * N ];
			}
			tmp[ i + j * N ] = s;
		}
	}

	// QTAQ = Q^T * tmp (column-major)
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < N; i++ ) {
			s = 0.0;
			for ( k = 0; k < N; k++ ) {
				s += Q[ k + i * N ] * tmp[ k + j * N ];
			}
			QTAQ[ i + j * N ] = s;
		}
	}

	// Verify QTAQ is tridiagonal: elements more than 1 away from diagonal should be ~0
	for ( i = 0; i < N; i++ ) {
		for ( j = 0; j < N; j++ ) {
			if ( Math.abs( i - j ) > 1 ) {
				assertClose( QTAQ[ i + j * N ], 0.0, 1e-13, 'QTAQ[' + i + ',' + j + '] should be zero' );
			}
		}
	}

	// Verify diagonal matches D from dsytrd
	for ( i = 0; i < N; i++ ) {
		assertClose( QTAQ[ i + i * N ], result.D[ i ], 1e-13, 'diagonal[' + i + ']' );
	}

	// Verify off-diagonal matches E from dsytrd
	for ( i = 0; i < N - 1; i++ ) {
		assertClose( QTAQ[ i + ( i + 1 ) * N ], result.E[ i ], 1e-13, 'superdiag[' + i + ']' );
		assertClose( QTAQ[ ( i + 1 ) + i * N ], result.E[ i ], 1e-13, 'subdiag[' + i + ']' );
	}
});

test( 'dorgtr: Q^T*A*Q is tridiagonal (uplo_L_4x4)', function t() {
	var N = 4;
	var Aorig = new Float64Array([
		4, 1, -2, 2,
		1, 2, 0, 1,
		-2, 0, 3, -2,
		2, 1, -2, -1
	]);

	var result = dsytrdThenDorgtr( 'lower', N, Aorig );
	var Q = result.Q;
	var i;
	var j;
	var k;

	// Compute QTAQ = Q^T * Aorig * Q
	var tmp = new Float64Array( N * N );
	var QTAQ = new Float64Array( N * N );
	var s;

	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < N; i++ ) {
			s = 0.0;
			for ( k = 0; k < N; k++ ) {
				s += Aorig[ i + k * N ] * Q[ k + j * N ];
			}
			tmp[ i + j * N ] = s;
		}
	}
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < N; i++ ) {
			s = 0.0;
			for ( k = 0; k < N; k++ ) {
				s += Q[ k + i * N ] * tmp[ k + j * N ];
			}
			QTAQ[ i + j * N ] = s;
		}
	}

	for ( i = 0; i < N; i++ ) {
		for ( j = 0; j < N; j++ ) {
			if ( Math.abs( i - j ) > 1 ) {
				assertClose( QTAQ[ i + j * N ], 0.0, 1e-13, 'QTAQ[' + i + ',' + j + '] should be zero' );
			}
		}
	}
	for ( i = 0; i < N; i++ ) {
		assertClose( QTAQ[ i + i * N ], result.D[ i ], 1e-13, 'diagonal[' + i + ']' );
	}
	for ( i = 0; i < N - 1; i++ ) {
		assertClose( QTAQ[ i + ( i + 1 ) * N ], result.E[ i ], 1e-13, 'superdiag[' + i + ']' );
		assertClose( QTAQ[ ( i + 1 ) + i * N ], result.E[ i ], 1e-13, 'subdiag[' + i + ']' );
	}
});

test( 'dorgtr: N=2 edge case (uplo_U)', function t() {
	var N = 2;
	var A = new Float64Array([
		3, 1,
		1, 5
	]);
	var WORK = new Float64Array( 256 );
	var TAU = new Float64Array( 1 );
	var D = new Float64Array( 2 );
	var E = new Float64Array( 1 );
	var Q;
	var info;
	var sum;
	var i;
	var j;
	var k;

	dsytrd( 'upper', N, A, 1, N, 0, D, 1, 0, E, 1, 0, TAU, 1, 0, WORK, 1, 0, 256 );
	Q = new Float64Array( A );
	WORK = new Float64Array( 256 );
	info = dorgtr( 'upper', N, Q, 1, N, 0, TAU, 1, 0, WORK, 1, 0, 256 );

	assert.equal( info, 0, 'info' );

	// Verify orthogonality
	for ( i = 0; i < N; i++ ) {
		for ( j = 0; j < N; j++ ) {
			sum = 0.0;
			for ( k = 0; k < N; k++ ) {
				sum += Q[ k + i * N ] * Q[ k + j * N ];
			}
			if ( i === j ) {
				assertClose( sum, 1.0, 1e-14, 'QTQ[' + i + ',' + j + ']' );
			} else {
				assertClose( sum, 0.0, 1e-14, 'QTQ[' + i + ',' + j + ']' );
			}
		}
	}
});

test( 'dorgtr: N=2 edge case (uplo_L)', function t() {
	var N = 2;
	var A = new Float64Array([
		3, 1,
		1, 5
	]);
	var WORK = new Float64Array( 256 );
	var TAU = new Float64Array( 1 );
	var D = new Float64Array( 2 );
	var E = new Float64Array( 1 );
	var Q;
	var info;
	var sum;
	var i;
	var j;
	var k;

	dsytrd( 'lower', N, A, 1, N, 0, D, 1, 0, E, 1, 0, TAU, 1, 0, WORK, 1, 0, 256 );
	Q = new Float64Array( A );
	WORK = new Float64Array( 256 );
	info = dorgtr( 'lower', N, Q, 1, N, 0, TAU, 1, 0, WORK, 1, 0, 256 );

	assert.equal( info, 0, 'info' );

	for ( i = 0; i < N; i++ ) {
		for ( j = 0; j < N; j++ ) {
			sum = 0.0;
			for ( k = 0; k < N; k++ ) {
				sum += Q[ k + i * N ] * Q[ k + j * N ];
			}
			if ( i === j ) {
				assertClose( sum, 1.0, 1e-14, 'QTQ[' + i + ',' + j + ']' );
			} else {
				assertClose( sum, 0.0, 1e-14, 'QTQ[' + i + ',' + j + ']' );
			}
		}
	}
});
