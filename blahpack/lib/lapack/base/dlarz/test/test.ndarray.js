/* eslint-disable no-restricted-syntax, stdlib/first-unit-test, max-len */

'use strict';

// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync; // eslint-disable-line node/no-sync
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var format = require( '@stdlib/string/format' );
var Float64Array = require( '@stdlib/array/float64' );
var ndarray = require( './../lib/ndarray.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlarz.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
var fixture = lines.map( function parse( line ) {
	return JSON.parse( line );
});

var TOL = 1e-12;


// FUNCTIONS //

/**
* Returns a fixture entry by name.
*
* @private
* @param {string} name - fixture name
* @throws {Error} if the fixture name is unknown
* @returns {Object} fixture entry
*/
function findCase( name ) {
	var tc = fixture.find( function find( t ) {
		return t.name === name;
	});
	if ( !tc ) {
		throw new Error( format( 'unknown fixture: `%s`.', name ) );
	}
	return tc;
}

/**
* Specs for each fixture case, matching test/fortran/test_dlarz.f90.
*
* @private
* @param {string} name - case name
* @throws {Error} if the case name is unknown
* @returns {Object} case spec
*/
function buildCase( name ) {
	switch ( name ) {
	case 'left_4x4_l2':
		return {
			'side': 'left',
			'M': 4,
			'N': 4,
			'l': 2,
			'v': [ 0.5, 0.25 ],
			'tau': 1.5,
			'A': [
				[ 1.0, 2.0, 3.0, 4.0 ],
				[ 5.0, 6.0, 7.0, 8.0 ],
				[ 9.0, 10.0, 11.0, 12.0 ],
				[ 13.0, 14.0, 15.0, 16.0 ]
			]
		};
	case 'right_4x4_l2':
		return {
			'side': 'right',
			'M': 4,
			'N': 4,
			'l': 2,
			'v': [ 0.5, 0.25 ],
			'tau': 1.5,
			'A': [
				[ 1.0, 2.0, 3.0, 4.0 ],
				[ 5.0, 6.0, 7.0, 8.0 ],
				[ 9.0, 10.0, 11.0, 12.0 ],
				[ 13.0, 14.0, 15.0, 16.0 ]
			]
		};
	case 'tau_zero':
		return {
			'side': 'left',
			'M': 3,
			'N': 3,
			'l': 1,
			'v': [ 1.0 ],
			'tau': 0.0,
			'A': [
				[ 1.0, 2.0, 3.0 ],
				[ 4.0, 5.0, 6.0 ],
				[ 7.0, 8.0, 9.0 ]
			]
		};
	case 'left_l0':
		return {
			'side': 'left',
			'M': 3,
			'N': 3,
			'l': 0,
			'v': [ 0.0 ],
			'tau': 0.5,
			'A': [
				[ 1.0, 2.0, 3.0 ],
				[ 4.0, 5.0, 6.0 ],
				[ 7.0, 8.0, 9.0 ]
			]
		};
	case 'left_5x3_l3':
		return {
			'side': 'left',
			'M': 5,
			'N': 3,
			'l': 3,
			'v': [ 0.1, 0.2, 0.3 ],
			'tau': 2.0,
			'A': [
				[ 1.0, 2.0, 3.0 ],
				[ 4.0, 5.0, 6.0 ],
				[ 7.0, 8.0, 9.0 ],
				[ 10.0, 11.0, 12.0 ],
				[ 13.0, 14.0, 15.0 ]
			]
		};
	case 'right_3x5_l3':
		return {
			'side': 'right',
			'M': 3,
			'N': 5,
			'l': 3,
			'v': [ 0.3, 0.2, 0.1 ],
			'tau': -0.5,
			'A': [
				[ 1.0, 2.0, 3.0, 4.0, 5.0 ],
				[ 6.0, 7.0, 8.0, 9.0, 10.0 ],
				[ 11.0, 12.0, 13.0, 14.0, 15.0 ]
			]
		};
	default:
		throw new Error( format( 'unknown case: `%s`.', name ) );
	}
}

/**
* Packs a 2D JS array into a column-major Float64Array.
*
* @private
* @param {Array<Array>} A - rows of values
* @param {integer} M - number of rows
* @param {integer} N - number of columns
* @returns {Float64Array} col-major packed M*N
*/
function packColMajor( A, M, N ) {
	var out = new Float64Array( M * N );
	var i;
	var j;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			out[ ( j * M ) + i ] = A[ i ][ j ];
		}
	}
	return out;
}

/**
* Asserts two numeric arrays agree within TOL.
*
* @private
* @param {ArrayLikeObject} actual - computed values
* @param {ArrayLikeObject} expected - expected values
* @param {string} label - label
*/
function expectClose( actual, expected, label ) {
	var diff;
	var i;
	assert.equal( actual.length, expected.length, format( '%s: length mismatch', label ) );
	for ( i = 0; i < actual.length; i++ ) {
		diff = Math.abs( actual[ i ] - expected[ i ] );
		assert.ok( diff <= TOL, format( '%s[%d]: expected %s, got %s (diff %s)', label, i, expected[ i ], actual[ i ], diff ) );
	}
}


// TESTS //

test( 'main export is a function', function t() {
	assert.strictEqual( typeof ndarray, 'function', 'is a function' );
});

test( 'ndarray throws TypeError for invalid side', function t() {
	assert.throws( function throws() {
		ndarray( 'invalid', 2, 2, 1, new Float64Array( 2 ), 1, 0, 1.0, new Float64Array( 4 ), 1, 2, 0, new Float64Array( 4 ), 1, 0 );
	}, TypeError );
});

// Fixture-driven column-major (strideC1=1, strideC2=M)
fixture.forEach( function each( tc ) {
	test( 'ndarray col-major: ' + tc.name, function t() {
		var inputs = buildCase( tc.name );
		var WORK = new Float64Array( Math.max( inputs.M, inputs.N ) );
		var C = packColMajor( inputs.A, inputs.M, inputs.N );
		var V = new Float64Array( inputs.v );
		ndarray( inputs.side, inputs.M, inputs.N, inputs.l, V, 1, 0, inputs.tau, C, 1, inputs.M, 0, WORK, 1, 0 );
		expectClose( C, tc.C, tc.name + ' col-major' );
	});
});

// Fixture-driven row-major (strideC1=N, strideC2=1)
fixture.forEach( function each( tc ) {
	test( 'ndarray row-major: ' + tc.name, function t() {
		var inputs = buildCase( tc.name );
		var WORK = new Float64Array( Math.max( inputs.M, inputs.N ) );
		var C = new Float64Array( inputs.M * inputs.N );
		var V = new Float64Array( inputs.v );
		var e;
		var a;
		var i;
		var j;
		for ( i = 0; i < inputs.M; i++ ) {
			for ( j = 0; j < inputs.N; j++ ) {
				C[ ( i * inputs.N ) + j ] = inputs.A[ i ][ j ];
			}
		}
		ndarray( inputs.side, inputs.M, inputs.N, inputs.l, V, 1, 0, inputs.tau, C, inputs.N, 1, 0, WORK, 1, 0 );

		// Expected fixture is col-major packed (M*N) with index (j*M+i); row-major result index is (i*N+j), so compare element-wise:
		for ( i = 0; i < inputs.M; i++ ) {
			for ( j = 0; j < inputs.N; j++ ) {
				e = tc.C[ ( j * inputs.M ) + i ];
				a = C[ ( i * inputs.N ) + j ];
				assert.ok( Math.abs( a - e ) <= TOL, format( '%s row-major [%d,%d]: expected %s, got %s', tc.name, i, j, e, a ) );
			}
		}
	});
});

// Strided v input: v laid out with stride 3, offset 1
test( 'ndarray: strided v with non-zero offset', function t() {
	var inputs = buildCase( 'left_5x3_l3' );
	var WORK = new Float64Array( Math.max( inputs.M, inputs.N ) );
	var tc = findCase( 'left_5x3_l3' );
	var C = packColMajor( inputs.A, inputs.M, inputs.N );
	var V = new Float64Array([ 9.0, 0.1, 9.0, 9.0, 0.2, 9.0, 9.0, 0.3, 9.0 ]);
	ndarray( inputs.side, inputs.M, inputs.N, inputs.l, V, 3, 1, inputs.tau, C, 1, inputs.M, 0, WORK, 1, 0 );
	expectClose( C, tc.C, 'left_5x3_l3 strided-v' );
});

// Strided v with negative stride (reverse direction)
test( 'ndarray: negative strideV', function t() {
	var inputs = buildCase( 'right_3x5_l3' );
	var WORK = new Float64Array( Math.max( inputs.M, inputs.N ) );
	var tc = findCase( 'right_3x5_l3' );
	var C = packColMajor( inputs.A, inputs.M, inputs.N );
	var V = new Float64Array([ 0.1, 0.2, 0.3 ]);
	ndarray( inputs.side, inputs.M, inputs.N, inputs.l, V, -1, 2, inputs.tau, C, 1, inputs.M, 0, WORK, 1, 0 );
	expectClose( C, tc.C, 'right_3x5_l3 negative-strideV' );
});

// Non-zero offsetC and offsetWORK
test( 'ndarray: non-zero offsetC and offsetWORK', function t() {
	var packed;
	var inputs;
	var WORK;
	var tc;
	var C;
	var e;
	var a;
	var i;
	inputs = buildCase( 'left_4x4_l2' );
	tc = findCase( 'left_4x4_l2' );
	packed = packColMajor( inputs.A, inputs.M, inputs.N );
	C = new Float64Array( packed.length + 5 );
	WORK = new Float64Array( 4 + 2 );
	for ( i = 0; i < 5; i++ ) {
		C[ i ] = 7.0; // sentinel
	}
	for ( i = 0; i < packed.length; i++ ) {
		C[ i + 5 ] = packed[ i ];
	}
	WORK[ 0 ] = 99.0;
	WORK[ 1 ] = 99.0;
	ndarray( inputs.side, inputs.M, inputs.N, inputs.l, new Float64Array( inputs.v ), 1, 0, inputs.tau, C, 1, inputs.M, 5, WORK, 1, 2 );

	// Sentinels preserved:
	for ( i = 0; i < 5; i++ ) {
		assert.equal( C[ i ], 7.0, 'sentinel preserved at index ' + i );
	}

	// Computed region matches fixture:
	for ( i = 0; i < tc.C.length; i++ ) {
		a = C[ i + 5 ];
		e = tc.C[ i ];
		assert.ok( Math.abs( a - e ) <= TOL, 'offsetC region matches fixture at index ' + i );
	}

	// Sentinel before offsetWORK preserved:
	assert.equal( WORK[ 0 ], 99.0, 'WORK sentinel preserved' );
	assert.equal( WORK[ 1 ], 99.0, 'WORK sentinel preserved' );
});

// Non-unit strideC1 with col-major ordering via "interleaved rows" (strideC1=2)
test( 'ndarray: non-unit strideC1 (interleaved rows)', function t() {
	var inputs;
	var WORK;
	var tc;
	var C;
	var M;
	var N;
	var e;
	var a;
	var i;
	var j;
	inputs = buildCase( 'left_4x4_l2' );
	tc = findCase( 'left_4x4_l2' );
	M = inputs.M;
	N = inputs.N;
	C = new Float64Array( 2 * M * N );
	WORK = new Float64Array( M );
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			C[ ( j * 2 * M ) + ( i * 2 ) ] = inputs.A[ i ][ j ];
		}
	}
	ndarray( inputs.side, M, N, inputs.l, new Float64Array( inputs.v ), 1, 0, inputs.tau, C, 2, 2 * M, 0, WORK, 1, 0 );
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			e = tc.C[ ( j * M ) + i ];
			a = C[ ( j * 2 * M ) + ( i * 2 ) ];
			assert.ok( Math.abs( a - e ) <= TOL, format( 'strideC1=2 [%d,%d]: expected %s, got %s', i, j, e, a ) );
		}
	}
});
